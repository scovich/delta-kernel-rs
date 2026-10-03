//! Generator traits, combinators, and constructors.
//!
//! Bound generators are polled for the next yield or completion. Unbound pipelines apply
//! combinators first, then attach a request channel.

use std::future::{poll_fn, Future};
use std::ops::AsyncFnMut;
use std::pin::Pin;
use std::task::{Context, Poll};

use super::{BoundGenerator, ChannelBinding};
use crate::coroutine::Channel;
use crate::DeltaResult;

/// Yield or completion produced by polling a bound [`Generator`].
pub enum GeneratorStep<Y, O> {
    /// A yielded item.
    Yield(Y),
    /// The generator completed.
    Done(O),
}

/// Consume helpers and combinators for [`IntoGenerator`] and [`Generator`].
pub trait GeneratorExt {
    /// Binds this generator to `channel`, which receives its connector requests.
    fn bind<'a, P: Send + 'static>(
        self,
        channel: &'a Channel<P>,
    ) -> impl Generator<Item = Self::Item, Output = Self::Output> + Unpin + 'a
    where
        Self: IntoGenerator<P> + Sized,
    {
        self.bind_to(ChannelBinding::Borrowed(channel))
    }

    /// Waits for the next item, returning `Ok(None)` when the generator is exhausted.
    ///
    /// Restricted to [`Generator::Output`] `()` so a real completion value cannot vanish in a
    /// `while let Some` loop. Callers that need `Output` poll [`Generator::poll_step`] or drive a
    /// [`super::GeneratorCoroutine`].
    fn next(&mut self) -> impl Future<Output = DeltaResult<Option<Self::Item>>> + Send + '_
    where
        Self: Generator<Output = ()> + Sized + Unpin,
    {
        poll_fn(
            move |context| match Pin::new(&mut *self).poll_step(context) {
                Poll::Ready(Ok(GeneratorStep::Yield(item))) => Poll::Ready(Ok(Some(item))),
                Poll::Ready(Ok(GeneratorStep::Done(()))) => Poll::Ready(Ok(None)),
                Poll::Ready(Err(err)) => Poll::Ready(Err(err)),
                Poll::Pending => Poll::Pending,
            },
        )
    }

    /// Counterpart of [`Iterator::map`]: transforms each yielded item.
    fn map<F>(self, map: F) -> Map<Self, F>
    where
        Self: Sized,
    {
        Map { inner: self, map }
    }

    /// Counterpart of `Itertools::map_ok`: transforms each successful yielded item.
    fn map_ok<F>(self, map: F) -> MapOk<Self, F>
    where
        Self: Sized,
    {
        MapOk { inner: self, map }
    }

    /// Counterpart of [`Iterator::peekable`]: allows inspecting the next item before consuming it.
    fn peekable<I>(self) -> Peekable<Self, I>
    where
        Self: Sized,
    {
        Peekable {
            inner: self,
            peeked: None,
            done: false,
        }
    }

    /// Counterpart of [`Iterator::chain`]: yields every item of `self`, then every item of `other`.
    fn chain<B>(self, other: B) -> Chain<Self, B>
    where
        Self: Sized,
    {
        Chain {
            first: Some(self),
            second: other,
        }
    }

    /// Counterpart of [`Iterator::flatten`]: yields the elements of each collection-valued item.
    ///
    /// Page-request (or other inner-generator) failures remain control-plane `Err`. Collection
    /// elements are yielded as-is, including when the element type is [`DeltaResult`]. Inner
    /// completion is preserved.
    fn flatten<I>(self) -> Flatten<Self, I>
    where
        Self: Sized,
    {
        Flatten {
            inner: self,
            rest: None,
        }
    }
}

/// Lazy generator pipeline.
///
/// Functions return `impl IntoGenerator<P, Item = ...>`. Call [`GeneratorExt`] combinators before
/// [`GeneratorExt::bind`].
pub trait IntoGenerator<P: Send + 'static>: GeneratorExt + Send + 'static {
    /// Type of the yielded items.
    type Item: Send + 'static;

    /// Type of the completion value.
    type Output: Send + 'static;

    /// Type-erases this generator as `Box<dyn IntoGenerator<...>>`.
    fn into_boxed(self) -> Box<dyn IntoGenerator<P, Item = Self::Item, Output = Self::Output>>
    where
        Self: Sized,
    {
        Box::new(self)
    }

    /// Binds this generator to a borrowed or owned request channel.
    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = Self::Item, Output = Self::Output> + Unpin + 'a
    where
        Self: Sized;

    /// Object-safe binding used after type erasure.
    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedGenerator<'a, Self::Item, Self::Output>;
}

/// Pollable generator bound to a request channel.
pub trait Generator: GeneratorExt + Send {
    /// Type of the yielded items.
    type Item: Send + 'static;

    /// Type of the completion value.
    type Output: Send + 'static;

    /// Polls for the next yield or completion.
    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<Self::Item, Self::Output>>>;
}

/// Type-erased generator with a pinned address and a caller-selected lifetime.
pub type BoxedGenerator<'a, Y, O = ()> = Box<dyn Generator<Item = Y, Output = O> + Unpin + 'a>;

/// Type-erased generator that yields independently fallible items.
///
/// Polling the generator can fail before an item is yielded; each yielded item can independently
/// be an [`Err`](Result::Err).
pub type BoxedDeltaResultGenerator<'a, T, O = ()> = BoxedGenerator<'a, DeltaResult<T>, O>;

impl<Y: Send + 'static, O: Send + 'static> GeneratorExt for BoundGenerator<'_, Y, O> {}
impl<P: Send + 'static, Y: Send + 'static, O: Send + 'static> GeneratorExt
    for Box<dyn IntoGenerator<P, Item = Y, Output = O>>
{
}
impl<'a, Y: Send + 'static, O: Send + 'static> GeneratorExt for BoxedGenerator<'a, Y, O> {}
impl<G, F> GeneratorExt for Map<G, F> {}
impl<G, F> GeneratorExt for MapOk<G, F> {}
impl<G, I> GeneratorExt for Peekable<G, I> {}
impl<A, B> GeneratorExt for Chain<A, B> {}
impl<I> GeneratorExt for Iter<I> {}
impl<S, F, Fut> GeneratorExt for Unfold<S, F, Fut> {}

impl<Y: Send + 'static, O: Send + 'static> Generator for BoundGenerator<'_, Y, O> {
    type Item = Y;
    type Output = O;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<Y, O>>> {
        self.get_mut()
            .poll_step(context, GeneratorStep::Yield, GeneratorStep::Done)
    }
}

impl<'a, Y: Send + 'static, O: Send + 'static> Generator for BoxedGenerator<'a, Y, O> {
    type Item = Y;
    type Output = O;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<Y, O>>> {
        Pin::new(&mut **self.get_mut()).poll_step(context)
    }
}

impl<P: Send + 'static, Y: Send + 'static, O: Send + 'static> IntoGenerator<P>
    for Box<dyn IntoGenerator<P, Item = Y, Output = O>>
{
    type Item = Y;
    type Output = O;

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = Y, Output = O> + Unpin + 'a {
        IntoGenerator::bind_boxed(self, channel)
    }

    fn into_boxed(self) -> Box<dyn IntoGenerator<P, Item = Y, Output = O>> {
        self
    }

    fn bind_boxed<'a>(self: Box<Self>, channel: ChannelBinding<'a, P>) -> BoxedGenerator<'a, Y, O> {
        (*self).bind_boxed(channel)
    }
}

/// Generator returned by [`GeneratorExt::map`].
pub struct Map<G, F> {
    inner: G,
    map: F,
}

impl<P, G, U, F> IntoGenerator<P> for Map<G, F>
where
    P: Send + 'static,
    G: IntoGenerator<P, Output = ()>,
    U: Send + 'static,
    F: FnMut(G::Item) -> U + Send + Unpin + 'static,
{
    type Item = U;
    type Output = ();

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = U, Output = ()> + Unpin + 'a {
        Map {
            inner: self.inner.bind_to(channel),
            map: self.map,
        }
    }

    fn bind_boxed<'a>(self: Box<Self>, channel: ChannelBinding<'a, P>) -> BoxedGenerator<'a, U> {
        Box::new((*self).bind_to(channel))
    }
}

impl<G, U, F> Generator for Map<G, F>
where
    G: Generator<Output = ()> + Unpin,
    U: Send + 'static,
    F: FnMut(G::Item) -> U + Send + Unpin,
{
    type Item = U;
    type Output = ();

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<U, ()>>> {
        let this = self.get_mut();
        match Pin::new(&mut this.inner).poll_step(context) {
            Poll::Ready(Ok(GeneratorStep::Yield(item))) => {
                Poll::Ready(Ok(GeneratorStep::Yield((this.map)(item))))
            }
            Poll::Ready(Ok(GeneratorStep::Done(()))) => Poll::Ready(Ok(GeneratorStep::Done(()))),
            Poll::Ready(Err(err)) => Poll::Ready(Err(err)),
            Poll::Pending => Poll::Pending,
        }
    }
}

/// Generator returned by [`GeneratorExt::map_ok`].
pub struct MapOk<G, F> {
    inner: G,
    map: F,
}

impl<P, G, T, U, F> IntoGenerator<P> for MapOk<G, F>
where
    P: Send + 'static,
    G: IntoGenerator<P, Item = DeltaResult<T>, Output = ()>,
    T: Send + 'static,
    U: Send + 'static,
    F: FnMut(T) -> U + Send + Unpin + 'static,
{
    type Item = DeltaResult<U>;
    type Output = ();

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = DeltaResult<U>, Output = ()> + Unpin + 'a {
        MapOk {
            inner: self.inner.bind_to(channel),
            map: self.map,
        }
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedGenerator<'a, DeltaResult<U>> {
        Box::new((*self).bind_to(channel))
    }
}

impl<G, T, U, F> Generator for MapOk<G, F>
where
    G: Generator<Item = DeltaResult<T>, Output = ()> + Unpin,
    T: Send + 'static,
    U: Send + 'static,
    F: FnMut(T) -> U + Send + Unpin,
{
    type Item = DeltaResult<U>;
    type Output = ();

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<DeltaResult<U>, ()>>> {
        let this = self.get_mut();
        match Pin::new(&mut this.inner).poll_step(context) {
            Poll::Ready(Ok(GeneratorStep::Yield(item))) => {
                let item = item.map(|item| (this.map)(item));
                Poll::Ready(Ok(GeneratorStep::Yield(item)))
            }
            Poll::Ready(Ok(GeneratorStep::Done(()))) => Poll::Ready(Ok(GeneratorStep::Done(()))),
            Poll::Ready(Err(err)) => Poll::Ready(Err(err)),
            Poll::Pending => Poll::Pending,
        }
    }
}

/// Generator returned by [`GeneratorExt::peekable`].
pub struct Peekable<G, I> {
    inner: G,
    peeked: Option<I>,
    done: bool,
}

impl<G, I> Peekable<G, I>
where
    G: Generator<Item = I, Output = ()> + Unpin,
    I: Send + 'static,
{
    fn poll_inner(&mut self, context: &mut Context<'_>) -> Poll<DeltaResult<GeneratorStep<I, ()>>> {
        Pin::new(&mut self.inner).poll_step(context)
    }

    /// Return the next item without consuming it, or `None` at completion.
    pub async fn peek(&mut self) -> DeltaResult<Option<&I>> {
        if self.peeked.is_none() && !self.done {
            match poll_fn(|context| self.poll_inner(context)).await? {
                GeneratorStep::Yield(item) => self.peeked = Some(item),
                GeneratorStep::Done(()) => self.done = true,
            }
        }
        Ok(self.peeked.as_ref())
    }
}

impl<P, G, I> IntoGenerator<P> for Peekable<G, I>
where
    P: Send + 'static,
    G: IntoGenerator<P, Item = I, Output = ()>,
    I: Send + Unpin + 'static,
{
    type Item = I;
    type Output = ();

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = I, Output = ()> + Unpin + 'a {
        Peekable {
            inner: self.inner.bind_to(channel),
            peeked: self.peeked,
            done: self.done,
        }
    }

    fn bind_boxed<'a>(self: Box<Self>, channel: ChannelBinding<'a, P>) -> BoxedGenerator<'a, I> {
        Box::new((*self).bind_to(channel))
    }
}

impl<G, I> Generator for Peekable<G, I>
where
    G: Generator<Item = I, Output = ()> + Unpin,
    I: Send + Unpin + 'static,
{
    type Item = I;
    type Output = ();

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<I, ()>>> {
        let this = self.get_mut();
        if let Some(item) = this.peeked.take() {
            return Poll::Ready(Ok(GeneratorStep::Yield(item)));
        }
        if this.done {
            return Poll::Ready(Ok(GeneratorStep::Done(())));
        }
        match this.poll_inner(context) {
            Poll::Ready(Ok(GeneratorStep::Done(()))) => {
                this.done = true;
                Poll::Ready(Ok(GeneratorStep::Done(())))
            }
            step => step,
        }
    }
}

/// Generator returned by [`GeneratorExt::chain`].
pub struct Chain<A, B> {
    // `None` once exhausted
    first: Option<A>,
    second: B,
}

impl<P, A, B> IntoGenerator<P> for Chain<A, B>
where
    P: Send + 'static,
    A: IntoGenerator<P, Output = ()>,
    B: IntoGenerator<P, Item = A::Item, Output = ()>,
{
    type Item = A::Item;
    type Output = ();

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = A::Item, Output = ()> + Unpin + 'a {
        Chain {
            first: self.first.map(|first| first.bind_to(channel.clone())),
            second: self.second.bind_to(channel),
        }
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedGenerator<'a, A::Item> {
        Box::new((*self).bind_to(channel))
    }
}

impl<A, B> Generator for Chain<A, B>
where
    A: Generator<Output = ()> + Unpin,
    B: Generator<Item = A::Item, Output = ()> + Unpin,
{
    type Item = A::Item;
    type Output = ();

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<A::Item, ()>>> {
        let this = self.get_mut();
        if let Some(first) = &mut this.first {
            match Pin::new(first).poll_step(context) {
                Poll::Ready(Ok(GeneratorStep::Done(()))) => this.first = None,
                poll => return poll,
            }
        }
        Pin::new(&mut this.second).poll_step(context)
    }
}

/// Generator returned by [`GeneratorExt::flatten`].
pub struct Flatten<G, I> {
    inner: G,
    rest: Option<I>,
}

impl<G, I> GeneratorExt for Flatten<G, I> {}

impl<P, G, I> IntoGenerator<P> for Flatten<G, I>
where
    P: Send + 'static,
    G: IntoGenerator<P>,
    G::Item: IntoIterator<IntoIter = I>,
    I: Iterator + Send + Unpin + 'static,
    I::Item: Send + 'static,
{
    type Item = I::Item;
    type Output = G::Output;

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = I::Item, Output = G::Output> + Unpin + 'a {
        Flatten {
            inner: self.inner.bind_to(channel),
            rest: None,
        }
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedGenerator<'a, I::Item, G::Output> {
        Box::new((*self).bind_to(channel))
    }
}

impl<G, I> Generator for Flatten<G, I>
where
    G: Generator + Unpin,
    G::Item: IntoIterator<IntoIter = I>,
    I: Iterator + Send + Unpin,
    I::Item: Send + 'static,
{
    type Item = I::Item;
    type Output = G::Output;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<I::Item, G::Output>>> {
        let this = self.get_mut();
        loop {
            if let Some(item) = this.rest.as_mut().and_then(Iterator::next) {
                return Poll::Ready(Ok(GeneratorStep::Yield(item)));
            }
            this.rest = None;
            match Pin::new(&mut this.inner).poll_step(context) {
                Poll::Ready(Ok(GeneratorStep::Yield(page))) => {
                    this.rest = Some(page.into_iter());
                }
                Poll::Ready(Ok(GeneratorStep::Done(output))) => {
                    return Poll::Ready(Ok(GeneratorStep::Done(output)));
                }
                Poll::Ready(Err(err)) => return Poll::Ready(Err(err)),
                Poll::Pending => return Poll::Pending,
            }
        }
    }
}

/// Counterpart of [`std::iter::empty`]: a generator that yields nothing.
pub fn empty<Y: Send + 'static>() -> Iter<std::iter::Empty<Y>> {
    from_iter(std::iter::empty())
}

/// Wrap `iter` as a [`Generator`] that yields each item synchronously.
pub fn from_iter<T>(iter: T) -> Iter<T::IntoIter>
where
    T: IntoIterator,
    T::IntoIter: Send + Unpin,
    T::Item: Send + 'static,
{
    Iter(iter.into_iter())
}

/// Generator returned by [`from_iter`].
pub struct Iter<I>(I);

impl<I: Iterator + Send + Unpin + 'static, P: Send + 'static> IntoGenerator<P> for Iter<I>
where
    I::Item: Send + 'static,
{
    type Item = I::Item;
    type Output = ();

    fn bind_to<'a>(
        self,
        _channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = I::Item, Output = ()> + Unpin + 'a {
        self
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedGenerator<'a, I::Item> {
        self
    }
}

impl<I: Iterator + Send + Unpin> Generator for Iter<I>
where
    I::Item: Send + 'static,
{
    type Item = I::Item;
    type Output = ();

    fn poll_step(
        self: Pin<&mut Self>,
        _context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<I::Item, ()>>> {
        match self.get_mut().0.next() {
            Some(item) => Poll::Ready(Ok(GeneratorStep::Yield(item))),
            None => Poll::Ready(Ok(GeneratorStep::Done(()))),
        }
    }
}

/// Builds a [`Generator`] by repeatedly passing owned state through `step`.
///
/// `step` consumes the current state and returns a future that yields `Some((next_state, item))`
/// or `None` at exhaustion. The future owns the state while pending. Each step future is pinned
/// in a box so this adapter is [`Unpin`] even when the step future is not.
pub fn unfold<S, I, F, Fut>(state: S, step: F) -> Unfold<S, F, Fut>
where
    F: FnMut(S) -> Fut,
    Fut: Future<Output = DeltaResult<Option<(S, I)>>>,
{
    Unfold {
        idle: Some(state),
        pending: None,
        step,
    }
}

/// Builds a [`Generator`] from owned state and an async `next(&mut state)` callback.
///
/// Used by [`unfold_mut`]. Pass [`unfold_mut_witness`] so type inference can prove the step
/// future is [`Send`]. Each step boxes that future so the returned generator is [`Unpin`].
///
/// [`unfold_mut`]: crate::coroutine::generator::unfold_mut
pub(crate) fn unfold_mut_with<'s, S, I, F, Fut>(
    state: S,
    next: F,
    mut witness: impl FnMut(S, F) -> Fut + Send + Unpin + 's,
) -> impl Generator<Item = I, Output = ()> + Unpin + 's
where
    S: Send + Unpin + 's,
    I: Send + 'static,
    F: Send + Unpin + 's,
    Fut: Future<Output = DeltaResult<Option<((S, F), I)>>> + Send + 's,
{
    unfold((state, next), move |(state, next)| witness(state, next))
}

/// Witness for [`unfold_mut`]: exposes the concrete future produced by `next` without
/// constraining it beyond its return type.
///
/// [`unfold_mut`]: crate::coroutine::generator::unfold_mut
pub(crate) async fn unfold_mut_witness<S, I, F>(
    mut state: S,
    mut next: F,
) -> DeltaResult<Option<((S, F), I)>>
where
    F: for<'s> AsyncFnMut(&'s mut S) -> DeltaResult<Option<I>>,
{
    let item = next(&mut state).await?;
    Ok(item.map(|item| ((state, next), item)))
}

/// Generator returned by [`unfold`].
pub struct Unfold<S, F, Fut> {
    idle: Option<S>,
    pending: Option<Pin<Box<Fut>>>,
    step: F,
}

impl<S, I, F, Fut> Generator for Unfold<S, F, Fut>
where
    S: Send + Unpin,
    I: Send + 'static,
    F: FnMut(S) -> Fut + Send + Unpin,
    Fut: Future<Output = DeltaResult<Option<(S, I)>>> + Send,
{
    type Item = I;
    type Output = ();

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<I, ()>>> {
        let this = self.get_mut();
        let mut pending = match this.pending.take() {
            Some(pending) => pending,
            None => {
                let Some(state) = this.idle.take() else {
                    return Poll::Ready(Ok(GeneratorStep::Done(())));
                };
                Box::pin((this.step)(state))
            }
        };
        let Poll::Ready(result) = pending.as_mut().poll(context) else {
            this.pending = Some(pending);
            return Poll::Pending;
        };
        match result {
            Ok(Some((state, item))) => {
                this.idle = Some(state);
                Poll::Ready(Ok(GeneratorStep::Yield(item)))
            }
            Ok(None) => Poll::Ready(Ok(GeneratorStep::Done(()))),
            Err(err) => Poll::Ready(Err(err)),
        }
    }
}
