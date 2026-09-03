//! Kernel-side coroutine support.
//!
//! On the kernel side, [`Workflow`] and [`Generator`] coroutines are just async functions that
//! happen to use a [`Channel`] that models requests to the connector as ordinary futures; kernel
//! code can `await` to receive the connector's response, and standard async infrastructure takes
//! over from there (polling, compiler-generated stack ripping and state machines, etc).
//!
//! Crucially, all this async magic happens entirely on the calling thread, coordinated by
//! [`Workflow::start`] and [`Generator::start`], which form the boundary between kernel's internal
//! async-for-stack-ripping code and kernel's public (sync) entry points. All polling of
//! coroutine-related futures uses a no-op waker, so no external events can influence the
//! coroutine. A [`Future::poll`](std::future::Future::poll) call only ever returns
//! [`Poll::Ready`](std::task::Poll::Ready) if the connector invoked the corresponding
//! [`Resume`](super::Resume).
//!
//! Generators are special in that they use a [`Yielder`] to separate the entity that consumes a
//! yielded item from the entity that processes a delegated [`Request`](super::Request). The kernel
//! code that implements a generator sends requests to the normal channel while routing yielded
//! items to separate futures the caller can `await`. This allows kernel to create and consume
//! generators internally while still forwarding all requests to the connector. The
//! [`GeneratorState`] wrapper provides an iterator-like surface for kernel-side generator
//! consumption, used for operations such as log replay that must manipulate and consume multiple
//! streams of connector-provided data.
use std::ops::Deref;
use std::sync::{Arc, Weak};

use delta_kernel_derive::internal_api;
use tracing::{error, Instrument as _, Span};

use super::{Generator, Workflow, YieldResume};
#[internal_api]
pub(crate) use crate::coroutine::core::DeltaFuture;
use crate::coroutine::core::{Exchange, Mailbox, PendingRequest, Wait, YieldExchange};
use crate::DeltaResult;

/// Kernel-side handle for typed connector operations.
///
/// It shares a mailbox with the coroutine driver and admits one live request at a time.
#[internal_api]
pub(crate) struct Channel(Arc<Mailbox<PendingRequest>>);

impl Channel {
    /// Initiate a request/response exchange with the connector
    pub(super) async fn exchange<Out: Send + 'static, In: Send + 'static>(
        &self,
        outbound: Out,
        pending: impl FnOnce(Weak<Exchange<Out, In>>) -> PendingRequest + Send,
    ) -> DeltaResult<In> {
        let exchange = Arc::new(Exchange::new(outbound));
        self.0.publish(pending(Arc::downgrade(&exchange)))?;
        Wait(exchange).await
    }
}

/// Kernel-side handle passed to a generator body.
///
/// Forward all requests to the underlying [`Channel`] so connector can handle them, while
/// delivering the input of [`Self::yield_item`] calls to the generator's immediate consumer.
#[internal_api]
pub(crate) struct Yielder<Y> {
    channel: Channel,
    mailbox: Arc<Mailbox<Weak<YieldExchange<Y>>>>,
}

impl<Y> Deref for Yielder<Y> {
    type Target = Channel;

    fn deref(&self) -> &Self::Target {
        &self.channel
    }
}

impl<Y: Send + 'static> Yielder<Y> {
    /// Yield one item and suspend until the consumer resumes the generator.
    ///
    /// An error supplied by the consumer is returned at this await point.
    #[internal_api]
    pub(crate) async fn yield_item(&self, item: Y) -> DeltaResult<()> {
        let emission = Arc::new(Exchange::new(item));
        self.mailbox.publish(Arc::downgrade(&emission))?;
        Wait(emission).await
    }
}

/// Kernel-side state for consuming a child generator.
#[internal_api]
pub(crate) enum GeneratorState<W> {
    Start(W),
    Continue(YieldResume<W>),
    Exhausted,
}

impl<Y: Send + 'static> GeneratorState<Generator<(), Y>> {
    /// Return the next yielded item, forwarding connector requests through `parent`.
    ///
    /// Returns `None` after the child generator completes.
    #[internal_api]
    pub(crate) async fn next(&mut self, parent: &Channel) -> DeltaResult<Option<Y>> {
        let state = std::mem::replace(self, Self::Exhausted);
        let mut generator = match state {
            Self::Start(generator) => Ok(generator),
            Self::Continue(resume) => resume(Ok(())),
            Self::Exhausted => return Ok(None),
        };

        loop {
            generator = match generator? {
                Generator::Request(request) => request.forward_to(parent).await,
                Generator::Done(()) => return Ok(None),
                Generator::Yield(item, resume) => {
                    *self = Self::Continue(resume);
                    return Ok(Some(item));
                }
            };
        }
    }
}

impl<O: Send + 'static> Workflow<O> {
    /// Start a workflow and run it until completion or its first connector request.
    #[internal_api]
    pub(crate) fn start<Fut>(workflow: impl FnOnce(Channel) -> Fut) -> DeltaResult<Self>
    where
        Fut: DeltaFuture<O> + 'static,
    {
        let mailbox = Arc::new(Mailbox::default());
        let future = workflow(Channel(Arc::clone(&mailbox)));
        let future = async move {
            future
                .await
                .inspect_err(|err| error!(error = %err, "kernel workflow failed"))
        };
        let task = Box::pin(future.instrument(Span::current()));
        Self::advance(task, mailbox)
    }
}

impl<O: Send + 'static, Y: Send + 'static> Generator<O, Y> {
    /// Start a generator and run it until completion, its first yield, or a connector request.
    #[internal_api]
    pub(crate) fn start<Fut>(generator: impl FnOnce(Yielder<Y>) -> Fut) -> DeltaResult<Self>
    where
        Fut: DeltaFuture<O> + 'static,
    {
        let mailbox = Arc::new(Mailbox::default());
        let yields = Arc::new(Mailbox::default());
        let yielder = Yielder {
            channel: Channel(Arc::clone(&mailbox)),
            mailbox: Arc::clone(&yields),
        };
        let future = generator(yielder);
        let future = async move {
            future
                .await
                .inspect_err(|err| error!(error = %err, "kernel generator failed"))
        };
        let task = Box::pin(future.instrument(Span::current()));
        Self::advance(task, mailbox, yields)
    }
}
