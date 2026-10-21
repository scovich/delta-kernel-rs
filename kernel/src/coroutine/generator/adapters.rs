//! Generator adapters and synchronous iterator sources.

use std::future::{poll_fn, Future};
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

#[doc(hidden)]
pub trait Step<I> {
    type Out;

    fn step(&mut self, item: I) -> Option<Self::Out>;
}

/// Mapping policy used by [`GeneratorExt::map`].
pub struct Map<F>(F);

impl<F> Map<F> {
    pub(super) fn new(map: F) -> Self {
        Self(map)
    }
}

impl<F, T, U> Step<T> for Map<F>
where
    F: FnMut(T) -> U,
{
    type Out = U;

    fn step(&mut self, item: T) -> Option<U> {
        Some((self.0)(item))
    }
}

/// Mapping policy used by [`GeneratorExt::map_ok`].
pub struct MapOk<F>(F);

impl<F> MapOk<F> {
    pub(super) fn new(map: F) -> Self {
        Self(map)
    }
}

impl<F, T, U> Step<KernelResult<T>> for MapOk<F>
where
    F: FnMut(T) -> U,
{
    type Out = KernelResult<U>;

    fn step(&mut self, item: KernelResult<T>) -> Option<KernelResult<U>> {
        Some(item.map(&mut self.0))
    }
}

/// Generator returned by [`GeneratorExt::map`] and [`GeneratorExt::map_ok`].
// This struct's manual `impl Unpin` is conditional on `G: Unpin` and valid because it does not use
// structural field pinning.
pub struct FilterMap<G, S>(G, S);

impl<G, S> FilterMap<G, S> {
    pub(super) fn new(inner: G, step: S) -> Self {
        Self(inner, step)
    }
}

impl<G: Unpin, S> Unpin for FilterMap<G, S> {}

impl<G, S> GeneratorExt for FilterMap<G, S> {}

impl<P, G, S> Generator<P> for FilterMap<G, S>
where
    P: Send + 'static,
    G: Generator<P>,
    S: Step<G::Item> + Send,
    S::Out: Send + 'static,
{
    type Item = S::Out;
    type Output = G::Output;

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl BoundGenerator<Item = S::Out, Output = G::Output> + Unpin + 'a
    where
        Self: 'a,
    {
        FilterMap(self.0.bind_to(channel), self.1)
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedBoundGenerator<'a, S::Out, G::Output>
    where
        Self: 'a,
    {
        Box::new((*self).bind_to(channel))
    }
}

impl<G, S> BoundGenerator for FilterMap<G, S>
where
    G: BoundGenerator + Unpin,
    S: Step<G::Item> + Send,
    S::Out: Send + 'static,
{
    type Item = S::Out;
    type Output = G::Output;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<KernelResult<GeneratorStep<S::Out, G::Output>>> {
        let FilterMap(inner, step) = self.get_mut();
        loop {
            let item = ready_yield!(Pin::new(&mut *inner).poll_step(context));
            if let Some(item) = step.step(item) {
                return Poll::Ready(Ok(GeneratorStep::Yield(item)));
            }
        }
    }
}

/// Generator returned by [`BoundGenerator::enumerate`].
// This struct's manual `impl Unpin` is conditional on `G: Unpin` and valid because it does not use
// structural field pinning.
pub struct Enumerate<G: BoundGenerator> {
    inner: G,
    next: usize,
}

impl<G: BoundGenerator> Enumerate<G> {
    pub(super) fn new(inner: G) -> Self {
        Self { inner, next: 0 }
    }
}

impl<G: BoundGenerator + Unpin> Unpin for Enumerate<G> {}

impl<G> BoundGenerator for Enumerate<G>
where
    G: BoundGenerator + Unpin,
{
    type Item = (usize, G::Item);
    type Output = G::Output;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<KernelResult<GeneratorStep<Self::Item, Self::Output>>> {
        let this = self.get_mut();
        let item = ready_yield!(Pin::new(&mut this.inner).poll_step(context));
        let index = this.next;
        this.next += 1;
        Poll::Ready(Ok(GeneratorStep::Yield((index, item))))
    }
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

/// Bound generator returned by [`BoundGenerator::peekable`].
// This struct's manual `impl Unpin` is conditional on `G: Unpin` and valid because it does not use
// structural field pinning.
pub struct Peekable<G: BoundGenerator> {
    inner: G,
    peeked: Option<G::Item>,
    done: Option<G::Output>,
}

impl<G: BoundGenerator + Unpin> Peekable<G> {
    pub(super) fn new(inner: G) -> Self {
        Self {
            inner,
            peeked: None,
            done: None,
        }
    }

    fn poll_inner(
        &mut self,
        context: &mut Context<'_>,
    ) -> Poll<KernelResult<GeneratorStep<G::Item, G::Output>>> {
        Pin::new(&mut self.inner).poll_step(context)
    }

    /// Returns the next item without consuming it, or `None` at completion.
    pub async fn peek(&mut self) -> KernelResult<Option<&G::Item>> {
        if self.peeked.is_none() && self.done.is_none() {
            match poll_fn(|context| self.poll_inner(context)).await? {
                GeneratorStep::Yield(item) => self.peeked = Some(item),
                GeneratorStep::Done(output) => self.done = Some(output),
            }
        }
        Ok(self.peeked.as_ref())
    }
}

impl<G: BoundGenerator + Unpin> Unpin for Peekable<G> {}

impl<G: BoundGenerator + Unpin> BoundGenerator for Peekable<G> {
    type Item = G::Item;
    type Output = G::Output;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<KernelResult<GeneratorStep<Self::Item, Self::Output>>> {
        let this = self.get_mut();
        if let Some(item) = this.peeked.take() {
            return Poll::Ready(Ok(GeneratorStep::Yield(item)));
        }
        if let Some(output) = this.done.take() {
            return Poll::Ready(Ok(GeneratorStep::Done(output)));
        }
        this.poll_inner(context)
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
