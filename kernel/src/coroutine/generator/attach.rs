//! Temporary attachment of unbound generators to borrowed request channels.

use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll, ready};

use super::{BoundGenerator, Generator, GeneratorStep, generator_fn};
use crate::KernelResult;
use crate::coroutine::channel::Forward;
use crate::coroutine::{Channel, CoroutineStep, StaticCoroutine};

/// An unbound generator temporarily attached to a borrowed request channel.
///
/// Created by [`GeneratorExt::attach_to`](super::GeneratorExt::attach_to). The generator owns a
/// private request channel. While attached, it yields items like a bound generator and forwards
/// its connector requests to the borrowed channel. [`Self::detach`] releases the borrowed channel
/// and returns an unbound generator that resumes where this one stopped, including after
/// completion.
// This struct's manual `impl Unpin` is valid because it does not use structural field pinning.
pub struct Attached<'c, P: Send + 'static, Y: Send + 'static, O: Send + 'static> {
    coroutine: StaticCoroutine<P, Y, O>,
    output: Option<O>,
    channel: &'c Channel<P>,
    // The coroutine is not polled while a forward awaits acceptance, so it cannot abandon a
    // request whose forward still sits in the borrowed channel's queue.
    forward: Option<Forward<'c, P>>,
}

impl<P: Send + 'static, Y: Send + 'static, O: Send + 'static> Unpin for Attached<'_, P, Y, O> {}

impl<'c, P: Send + 'static, Y: Send + 'static, O: Send + 'static> Attached<'c, P, Y, O> {
    pub(super) fn new(
        generator: impl Generator<P, Item = Y, Output = O> + 'static,
        channel: &'c Channel<P>,
    ) -> Self {
        Self {
            coroutine: StaticCoroutine::from_generator(generator),
            output: None,
            channel,
            forward: None,
        }
    }

    /// Releases the borrowed channel once its driver has accepted any forwarded request.
    ///
    /// The returned generator resumes this one once bound. Requests the driver already accepted
    /// stay with that driver, and their replies still reach the resumed generator.
    pub async fn detach(
        mut self,
    ) -> KernelResult<impl Generator<P, Item = Y, Output = O> + use<P, Y, O>> {
        if let Some(forward) = self.forward.take() {
            forward.await?;
        }
        Ok(generator_fn!(async move |channel| {
            if let Some(output) = self.output {
                return Ok(output);
            }
            loop {
                match self.coroutine.advance().await? {
                    CoroutineStep::Yield(item) => channel.yield_item(item).await?,
                    CoroutineStep::Request(request) => channel.forward(request).await?,
                    CoroutineStep::Done(output) => return Ok(output),
                }
            }
        }))
    }
}

impl<P: Send + 'static, Y: Send + 'static, O: Send + 'static> BoundGenerator
    for Attached<'_, P, Y, O>
{
    type Item = Y;
    type Output = ();

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<KernelResult<GeneratorStep<Y, ()>>> {
        let this = self.get_mut();
        loop {
            if let Some(forward) = this.forward.as_mut() {
                let accepted = ready!(Pin::new(forward).poll(context));
                this.forward = None;
                accepted?;
            }
            match ready!(this.coroutine.poll_step(context))? {
                CoroutineStep::Yield(item) => return Poll::Ready(Ok(GeneratorStep::Yield(item))),
                CoroutineStep::Request(request) => {
                    this.forward = Some(this.channel.forward(request));
                }
                CoroutineStep::Done(output) => {
                    this.output = Some(output);
                    return Poll::Ready(Ok(GeneratorStep::Done(())));
                }
            }
        }
    }
}
