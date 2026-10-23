//! Generator adapters and synchronous iterator sources.

use std::future::{poll_fn, Future};
use std::ops::{AsyncFnMut, ControlFlow};
use std::pin::Pin;
use std::task::{Context, Poll};

use super::{
    BoundGenerator, BoxedBoundGenerator, ChannelBinding, Generator, GeneratorExt, GeneratorStep,
};
use crate::coroutine::Channel;
use crate::utils::PhantomType;
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

/// Filtering policy used by [`GeneratorExt::filter_ok`].
pub struct FilterOk<P>(P);

impl<P> FilterOk<P> {
    pub(super) fn new(predicate: P) -> Self {
        Self(predicate)
    }
}

impl<P, T> Step<KernelResult<T>> for FilterOk<P>
where
    P: FnMut(&T) -> bool,
{
    type Out = KernelResult<T>;

    fn step(&mut self, item: KernelResult<T>) -> Option<Self::Out> {
        match item {
            Ok(item) if (self.0)(&item) => Some(Ok(item)),
            Ok(_) => None,
            Err(err) => Some(Err(err)),
        }
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

/// Witnesses one call to an asynchronous mapper.
pub async fn map_async_witness<'a, P, F, I, U>(
    mapper: &'a mut F,
    (channel, item): (&'a Channel<P>, I),
) -> U
where
    P: Send + 'static,
    F: for<'b> AsyncFnMut(&'b Channel<P>, I) -> U,
{
    mapper(channel, item).await
}

/// Exposes one concrete asynchronous mapper call future for a channel lifetime.
#[doc(hidden)]
pub trait MapAsyncWitness<'a, P: 'static, F, I, U> {
    /// Future returned by this witness invocation.
    type Future: Future<Output = U> + Send + 'a;

    /// Invokes the witness for `mapper` and its channel-bound item.
    fn invoke(&self, mapper: &'a mut F, argument: (&'a Channel<P>, I)) -> Self::Future;
}

impl<'a, P, F: 'a, I, U, W, Fut> MapAsyncWitness<'a, P, F, I, U> for W
where
    P: 'static,
    W: Fn(&'a mut F, (&'a Channel<P>, I)) -> Fut,
    Fut: Future<Output = U> + Send + 'a,
{
    type Future = Fut;

    fn invoke(&self, mapper: &'a mut F, argument: (&'a Channel<P>, I)) -> Self::Future {
        self(mapper, argument)
    }
}

/// Generator returned by [`GeneratorExt::map_async`].
pub struct MapAsync<G, P, F, W, U> {
    inner: G,
    mapper: F,
    witness: W,
    _types: PhantomType<(P, U)>,
}

impl<G, P, F, W, U> MapAsync<G, P, F, W, U> {
    pub(super) fn new(inner: G, mapper: F, witness: W) -> Self {
        Self {
            inner,
            mapper,
            witness,
            _types: PhantomType::default(),
        }
    }
}

impl<G, P, F, W, U> GeneratorExt for MapAsync<G, P, F, W, U> {}

impl<P, G, F, W, U> Generator<P> for MapAsync<G, P, F, W, U>
where
    P: Send + 'static,
    G: Generator<P>,
    F: for<'a> AsyncFnMut(&'a Channel<P>, G::Item) -> U + Send,
    W: Send,
    for<'a> W: MapAsyncWitness<'a, P, F, G::Item, U>,
    U: Send + 'static,
{
    type Item = U;
    type Output = G::Output;

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl BoundGenerator<Item = U, Output = G::Output> + Unpin + 'a
    where
        Self: 'a,
    {
        let state = BoundMapAsync {
            inner: self.inner.bind_to(channel.clone()),
            channel,
            mapper: self.mapper,
            witness: self.witness,
            _item_type: PhantomType::default(),
        };
        unfold(state, BoundMapAsync::<'a, P, _, _, _, U>::next)
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedBoundGenerator<'a, U, G::Output>
    where
        Self: 'a,
    {
        Box::new((*self).bind_to(channel))
    }
}

struct BoundMapAsync<'a, P: 'static, G, F, W, U> {
    inner: G,
    channel: ChannelBinding<'a, P>,
    mapper: F,
    witness: W,
    _item_type: PhantomType<U>,
}

impl<'channel, P, G, F, W, U> BoundMapAsync<'channel, P, G, F, W, U>
where
    P: Send + 'static,
    G: BoundGenerator + Unpin,
    F: for<'a> AsyncFnMut(&'a Channel<P>, G::Item) -> U + Send,
    W: for<'a> MapAsyncWitness<'a, P, F, G::Item, U> + Send,
    U: Send + 'static,
{
    async fn next(mut self) -> KernelResult<ControlFlow<G::Output, (Self, U)>> {
        match poll_fn(|context| Pin::new(&mut self.inner).poll_step(context)).await? {
            GeneratorStep::Yield(item) => {
                let argument = (&*self.channel, item);
                let item = self.witness.invoke(&mut self.mapper, argument).await;
                Ok(ControlFlow::Continue((self, item)))
            }
            GeneratorStep::Done(output) => Ok(ControlFlow::Break(output)),
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

/// Generator returned by [`GeneratorExt::chain`].
pub struct Chain<A, B> {
    first: Option<A>,
    second: B,
}

impl<A, B> Chain<A, B> {
    pub(super) fn new(first: A, second: B) -> Self {
        Self {
            first: Some(first),
            second,
        }
    }
}

impl<A, B> GeneratorExt for Chain<A, B> {}

impl<P, A, B> Generator<P> for Chain<A, B>
where
    P: Send + 'static,
    A: Generator<P, Output = ()>,
    B: Generator<P, Item = A::Item>,
{
    type Item = A::Item;
    type Output = B::Output;

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl BoundGenerator<Item = A::Item, Output = B::Output> + Unpin + 'a
    where
        Self: 'a,
        A::Item: 'a,
    {
        Chain {
            first: self.first.map(|first| first.bind_to(channel.clone())),
            second: self.second.bind_to(channel),
        }
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedBoundGenerator<'a, A::Item, B::Output>
    where
        Self: 'a,
        A::Item: 'a,
    {
        Box::new((*self).bind_to(channel))
    }
}

impl<A, B> BoundGenerator for Chain<A, B>
where
    A: BoundGenerator<Output = ()> + Unpin,
    B: BoundGenerator<Item = A::Item> + Unpin,
{
    type Item = A::Item;
    type Output = B::Output;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<KernelResult<GeneratorStep<A::Item, B::Output>>> {
        let this = self.get_mut();
        if let Some(first) = &mut this.first {
            match std::task::ready!(Pin::new(first).poll_step(context)) {
                Ok(GeneratorStep::Done(())) => this.first = None,
                Ok(GeneratorStep::Yield(item)) => {
                    return Poll::Ready(Ok(GeneratorStep::Yield(item)));
                }
                Err(err) => return Poll::Ready(Err(err)),
            }
        }
        Pin::new(&mut this.second).poll_step(context)
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
