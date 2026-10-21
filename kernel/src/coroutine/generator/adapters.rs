//! Generator adapters and synchronous iterator sources.

use std::future::Future;
use std::ops::{AsyncFnMut, ControlFlow};
use std::pin::Pin;
use std::task::{Context, Poll};

use super::{
    BoundGenerator, BoxedBoundGenerator, ChannelBinding, Generator, GeneratorExt, GeneratorStep,
};
use crate::{KernelError, KernelResult};

/// Extract a yielded item from the provided poll, propagating pending, failure, or completion.
macro_rules! ready_yield {
    ($poll:expr) => {
        match std::task::ready!($poll) {
            Ok(GeneratorStep::Yield(item)) => item,
            Ok(GeneratorStep::Done(output)) => {
                return Poll::Ready(Ok(GeneratorStep::Done(output)));
            }
            Err(err) => return Poll::Ready(Err(err)),
        }
    };
}

/// Generator returned by [`GeneratorExt::flatten`].
// This struct's manual `impl Unpin` is conditional on `G: Unpin` and valid because it does not use
// structural field pinning.
pub struct Flatten<G, I> {
    inner: G,
    rest: Option<I>,
}

impl<G, I> Flatten<G, I> {
    pub(super) fn new(inner: G) -> Self {
        Self { inner, rest: None }
    }
}

impl<G: Unpin, I> Unpin for Flatten<G, I> {}

impl<G, I> GeneratorExt for Flatten<G, I> {}

impl<P, G, I> Generator<P> for Flatten<G, I>
where
    P: Send + 'static,
    G: Generator<P>,
    G::Item: IntoIterator<IntoIter = I>,
    I: Iterator + Send + 'static,
    I::Item: Send + 'static,
{
    type Item = I::Item;
    type Output = G::Output;

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl BoundGenerator<Item = I::Item, Output = G::Output> + Unpin + 'a
    where
        Self: 'a,
    {
        Flatten {
            inner: self.inner.bind_to(channel),
            rest: None,
        }
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedBoundGenerator<'a, I::Item, G::Output>
    where
        Self: 'a,
    {
        Box::new((*self).bind_to(channel))
    }
}

impl<G, I> BoundGenerator for Flatten<G, I>
where
    G: BoundGenerator + Unpin,
    G::Item: IntoIterator<IntoIter = I>,
    I: Iterator + Send,
    I::Item: Send + 'static,
{
    type Item = I::Item;
    type Output = G::Output;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<KernelResult<GeneratorStep<I::Item, G::Output>>> {
        let this = self.get_mut();
        loop {
            if let Some(item) = this.rest.as_mut().and_then(Iterator::next) {
                return Poll::Ready(Ok(GeneratorStep::Yield(item)));
            }
            this.rest = None;
            let page = ready_yield!(Pin::new(&mut this.inner).poll_step(context));
            this.rest = Some(page.into_iter());
        }
    }
}

/// Counterpart of [`std::iter::empty`]: a generator that yields nothing.
pub fn empty<Y: Send>() -> Iter<std::iter::Empty<Y>> {
    Iter(std::iter::empty())
}

/// Converts synchronous iterables into generators.
pub trait AsyncIntoIterator: IntoIterator + Sized {
    /// Convert this iterable into a generator.
    fn into_generator(self) -> Iter<Self::IntoIter>
    where
        Self::IntoIter: Send,
        Self::Item: Send,
    {
        Iter(self.into_iter())
    }
}

impl<T: IntoIterator> AsyncIntoIterator for T {}

/// Generator returned by [`AsyncIntoIterator::into_generator`].
// This struct's manual `impl Unpin` is valid because it does not use structural field pinning.
pub struct Iter<I>(I);

impl<I> Unpin for Iter<I> {}
impl<I> GeneratorExt for Iter<I> {}

impl<I: Iterator + Send, P: Send + 'static> Generator<P> for Iter<I>
where
    I::Item: Send,
{
    type Item = I::Item;
    type Output = ();

    fn bind_to<'a>(
        self,
        _channel: ChannelBinding<'a, P>,
    ) -> impl BoundGenerator<Item = I::Item, Output = ()> + Unpin + 'a
    where
        Self: 'a,
        I::Item: 'a,
    {
        self
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        _channel: ChannelBinding<'a, P>,
    ) -> BoxedBoundGenerator<'a, I::Item>
    where
        Self: 'a,
        I::Item: 'a,
    {
        self
    }
}

impl<I: Iterator + Send> BoundGenerator for Iter<I>
where
    I::Item: Send,
{
    type Item = I::Item;
    type Output = ();

    fn poll_step(
        self: Pin<&mut Self>,
        _context: &mut Context<'_>,
    ) -> Poll<KernelResult<GeneratorStep<I::Item, ()>>> {
        match self.get_mut().0.next() {
            Some(item) => Poll::Ready(Ok(GeneratorStep::Yield(item))),
            None => Poll::Ready(Ok(GeneratorStep::Done(()))),
        }
    }
}

/// Builds a [`BoundGenerator`] by repeatedly passing owned state through `step`.
///
/// `step` consumes the current state and returns a future that either yields
/// `Continue((next_state, item))` or completes with `Break(output)`. The future owns the state
/// while pending. Each step future is pinned in a box so this adapter is [`Unpin`] even when the
/// step future is not.
pub fn unfold<S, I, O, F, Fut>(state: S, step: F) -> Unfold<S, F, Fut>
where
    F: FnMut(S) -> Fut,
    Fut: Future<Output = KernelResult<ControlFlow<O, (S, I)>>>,
{
    Unfold {
        idle: Some(state),
        pending: None,
        step,
    }
}

/// Builds a [`BoundGenerator`] from owned state and an async `next(&mut state)` callback.
pub(crate) fn unfold_mut_with<'s, S, I, F, Fut>(
    state: S,
    next: F,
    mut witness: impl FnMut(S, F) -> Fut + Send + 's,
) -> impl BoundGenerator<Item = I, Output = ()> + Unpin + 's
where
    S: Send + 's,
    I: Send + 'static,
    F: Send + 's,
    Fut: Future<Output = KernelResult<ControlFlow<(), ((S, F), I)>>> + Send + 's,
{
    unfold((state, next), move |(state, next)| witness(state, next))
}

/// Witness for [`unfold_mut`](super::unfold_mut): exposes the concrete future produced by `next`.
pub(crate) async fn unfold_mut_witness<S, I, F>(
    mut state: S,
    mut next: F,
) -> KernelResult<ControlFlow<(), ((S, F), I)>>
where
    F: for<'s> AsyncFnMut(&'s mut S) -> KernelResult<Option<I>>,
{
    let item = next(&mut state).await?;
    Ok(match item {
        Some(item) => ControlFlow::Continue(((state, next), item)),
        None => ControlFlow::Break(()),
    })
}

/// Generator returned by [`unfold`].
// This struct's manual `impl Unpin` is valid because it does not use structural field pinning;
// `pending` holds its future in a pin-box.
pub struct Unfold<S, F, Fut> {
    idle: Option<S>,
    pending: Option<Pin<Box<Fut>>>,
    step: F,
}

impl<S, F, Fut> Unpin for Unfold<S, F, Fut> {}

impl<S, I, O, F, Fut> BoundGenerator for Unfold<S, F, Fut>
where
    S: Send,
    I: Send + 'static,
    O: Send + 'static,
    F: FnMut(S) -> Fut + Send,
    Fut: Future<Output = KernelResult<ControlFlow<O, (S, I)>>> + Send,
{
    type Item = I;
    type Output = O;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<KernelResult<GeneratorStep<I, O>>> {
        let this = self.get_mut();
        let mut pending = match this.pending.take() {
            Some(pending) => pending,
            None => {
                let Some(state) = this.idle.take() else {
                    return Poll::Ready(Err(KernelError::internal_error(
                        "unfold generator advanced after completion",
                    )));
                };
                Box::pin((this.step)(state))
            }
        };
        let Poll::Ready(result) = pending.as_mut().poll(context) else {
            this.pending = Some(pending);
            return Poll::Pending;
        };
        match result {
            Ok(ControlFlow::Continue((state, item))) => {
                this.idle = Some(state);
                Poll::Ready(Ok(GeneratorStep::Yield(item)))
            }
            Ok(ControlFlow::Break(output)) => Poll::Ready(Ok(GeneratorStep::Done(output))),
            Err(err) => Poll::Ready(Err(err)),
        }
    }
}
