//! Generator traits, combinators, and constructors.
//!
//! Unbound pipelines apply combinators first, then attach a request channel. Bound generators are
//! polled for the next yield or completion.

use std::future::{poll_fn, Future};
use std::ops::{AsyncFnMut, ControlFlow};
use std::pin::Pin;
use std::task::{Context, Poll};

use super::{ChannelBinding, GeneratorBody};
use crate::coroutine::{Channel, DeltaFuture};
use crate::utils::PhantomType;
use crate::{DeltaResult, Error};

/// Yield or completion produced by polling a bound [`BoundGenerator`].
pub enum GeneratorStep<Y, O> {
    /// A yielded item.
    Yield(Y),
    /// The generator completed.
    Done(O),
}

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

/// Combinators for unbound [`Generator`] pipelines.
///
/// These methods cannot live on [`Generator`] itself: that trait is parameterized by the request
/// type `P`, and naming `P` on a combinator such as [`Self::map`] would force it to be inferred
/// before [`Generator::bind`]. Bound consume helpers live on [`BoundGenerator`].
pub trait GeneratorExt {
    /// Counterpart of [`Iterator::map`]: transforms each yielded item.
    fn map<F>(self, map: F) -> FilterMap<Self, Map<F>>
    where
        Self: Sized,
    {
        FilterMap(self, Map(map))
    }

    /// Counterpart of the iterator `on_complete` adapter: invokes `on_complete` after successful
    /// generator completion.
    fn on_complete<F>(self, on_complete: F) -> OnComplete<Self, F>
    where
        Self: Sized,
    {
        OnComplete {
            inner: self,
            on_complete,
        }
    }

    /// Maps items asynchronously using the supplied call-future witness.
    fn map_async<P, U, F, W>(self, witness: W, mapper: F) -> MapAsync<Self, P, F, W, U>
    where
        Self: Generator<P> + Sized,
        P: Send + 'static,
        U: Send + 'static,
        F: for<'a> AsyncFnMut(&'a Channel<P>, Self::Item) -> U + Send,
        W: Send,
        W: for<'a> MapAsyncWitness<'a, P, F, Self::Item, U>,
    {
        MapAsync {
            inner: self,
            mapper,
            witness,
            _types: PhantomType::default(),
        }
    }

    /// Counterpart of `Itertools::map_ok`: transforms each successful yielded item.
    fn map_ok<F>(self, map: F) -> FilterMap<Self, MapOk<F>>
    where
        Self: Sized,
    {
        FilterMap(self, MapOk(map))
    }

    /// Counterpart of `Itertools::filter_ok`: retains successful yielded items matching
    /// `predicate`.
    fn filter_ok<F>(self, predicate: F) -> FilterMap<Self, FilterOk<F>>
    where
        Self: Sized,
    {
        FilterMap(self, FilterOk(predicate))
    }

    /// Flattens inner generators, preserving this generator's completion value.
    fn flatten_async<I, T>(self) -> FlattenBy<Self, FlattenIdent<I, T>>
    where
        Self: Sized,
    {
        FlattenBy::new(self)
    }

    /// Counterpart of `Itertools::flatten_ok`: flattens successful inner generators.
    fn flatten_ok<I, T>(self) -> FlattenBy<Self, FlattenResult<I, T>>
    where
        Self: Sized,
    {
        FlattenBy::new(self)
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
/// Functions return `impl Generator<P, Item = ...>`. Call [`GeneratorExt`] combinators before
/// [`Self::bind`].
pub trait Generator<P: Send + 'static>: GeneratorExt + Send {
    /// Type of the yielded items.
    type Item: Send;

    /// Type of the completion value.
    type Output: Send + 'static;

    /// Type-erases this generator as `Box<dyn Generator<...>>`.
    fn into_boxed<'a>(self) -> Box<dyn Generator<P, Item = Self::Item, Output = Self::Output> + 'a>
    where
        Self: Sized + 'a,
        Self::Item: 'a,
    {
        Box::new(self)
    }

    /// Binds this generator to `channel`, which receives its connector requests.
    fn bind<'a>(
        self,
        channel: &'a Channel<P>,
    ) -> impl BoundGenerator<Item = Self::Item, Output = Self::Output> + Unpin + 'a
    where
        Self: Sized + 'a,
        Self::Item: 'a,
    {
        self.bind_to(ChannelBinding::Borrowed(channel))
    }

    /// Binds this generator to a borrowed or owned request channel.
    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl BoundGenerator<Item = Self::Item, Output = Self::Output> + Unpin + 'a
    where
        Self: Sized + 'a,
        Self::Item: 'a;

    /// Object-safe binding used after type erasure.
    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedBoundGenerator<'a, Self::Item, Self::Output>
    where
        Self: 'a,
        Self::Item: 'a;
}

/// Pollable generator bound to a request channel.
pub trait BoundGenerator: Send {
    /// Type of the yielded items.
    type Item: Send;

    /// Type of the completion value.
    type Output: Send + 'static;

    /// Polls for the next yield or completion.
    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<Self::Item, Self::Output>>>;

    /// Waits for the next item, returning `Ok(None)` when the generator is exhausted.
    ///
    /// Restricted to [`Self::Output`] `()` so a real completion value cannot vanish in a
    /// `while let Some` loop. Callers that need `Output` poll [`Self::poll_step`] or drive a
    /// [`super::GeneratorCoroutine`].
    fn next(&mut self) -> impl DeltaFuture<Option<Self::Item>> + '_
    where
        Self: Sized + Unpin + BoundGenerator<Output = ()>,
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

    /// Counterpart of `Itertools::try_collect`: collects successful yielded items, stopping at the
    /// first item or generator error.
    fn try_collect<T, C>(mut self) -> impl DeltaFuture<C>
    where
        Self: Sized + Unpin + BoundGenerator<Item = DeltaResult<T>, Output = ()>,
        T: Send + 'static,
        C: Default + Extend<T> + Send,
    {
        async move {
            let mut output = C::default();
            while let Some(item) = self.next().await? {
                output.extend([item?]);
            }
            Ok(output)
        }
    }

    /// Counterpart of [`Iterator::collect`]: collects every yielded item.
    fn collect<C>(mut self) -> impl DeltaFuture<C>
    where
        Self: Sized + Unpin + BoundGenerator<Output = ()>,
        C: Default + Extend<Self::Item> + Send,
    {
        async move {
            let mut output = C::default();
            while let Some(item) = self.next().await? {
                output.extend([item]);
            }
            Ok(output)
        }
    }

    /// Counterpart of [`Iterator::peekable`]: allows inspecting the next item without consuming it.
    fn peekable(self) -> Peekable<Self>
    where
        Self: Sized + Unpin,
    {
        Peekable {
            inner: self,
            peeked: None,
            done: None,
        }
    }

    /// Type-erases this bound generator as [`BoxedBoundGenerator`].
    fn into_boxed<'a>(self) -> BoxedBoundGenerator<'a, Self::Item, Self::Output>
    where
        Self: Sized + Unpin + 'a,
    {
        Box::new(self)
    }
}

/// Type-erased generator with a pinned address and a caller-selected lifetime.
pub type BoxedBoundGenerator<'a, Y, O = ()> =
    Box<dyn BoundGenerator<Item = Y, Output = O> + Unpin + 'a>;

impl<'g, P: Send + 'static, Y: Send + 'g, O: Send + 'static> GeneratorExt
    for Box<dyn Generator<P, Item = Y, Output = O> + 'g>
{
}
impl<G, S> GeneratorExt for FilterMap<G, S> {}
impl<G, F> GeneratorExt for OnComplete<G, F> {}
impl<G, P, F, W, U> GeneratorExt for MapAsync<G, P, F, W, U> {}
impl<G, Policy> GeneratorExt for FlattenBy<G, Policy> {}
impl<A, B> GeneratorExt for Chain<A, B> {}
impl<I> GeneratorExt for Iter<I> {}

impl<'a, Y: Send + 'a, O: Send + 'static> BoundGenerator for GeneratorBody<'a, Y, O> {
    type Item = Y;
    type Output = O;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<Y, O>>> {
        self.get_mut().poll_step(context)
    }
}

impl<G> BoundGenerator for Box<G>
where
    G: BoundGenerator + Unpin + ?Sized,
{
    type Item = G::Item;
    type Output = G::Output;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<G::Item, G::Output>>> {
        Pin::new(&mut **self.get_mut()).poll_step(context)
    }
}

impl<'g, P: Send + 'static, Y: Send + 'g, O: Send + 'static> Generator<P>
    for Box<dyn Generator<P, Item = Y, Output = O> + 'g>
{
    type Item = Y;
    type Output = O;

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl BoundGenerator<Item = Y, Output = O> + Unpin + 'a
    where
        Self: 'a,
        Y: 'a,
    {
        self.bind_boxed(channel)
    }

    fn into_boxed<'a>(self) -> Box<dyn Generator<P, Item = Y, Output = O> + 'a>
    where
        Self: 'a,
        Y: 'a,
    {
        self
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedBoundGenerator<'a, Y, O>
    where
        Self: 'a,
        Y: 'a,
    {
        (*self).bind_boxed(channel)
    }
}

/// Generator returned by [`GeneratorExt::on_complete`].
pub struct OnComplete<G, F> {
    inner: G,
    on_complete: F,
}

struct BoundOnComplete<G, F: FnOnce()> {
    inner: G,
    on_complete: Option<F>,
}

// The callback is never structurally pinned; it is only taken after `inner` completes.
impl<G: Unpin, F: FnOnce()> Unpin for BoundOnComplete<G, F> {}

impl<P, G, F> Generator<P> for OnComplete<G, F>
where
    P: Send + 'static,
    G: Generator<P>,
    F: FnOnce() + Send,
{
    type Item = G::Item;
    type Output = G::Output;

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl BoundGenerator<Item = G::Item, Output = G::Output> + Unpin + 'a
    where
        Self: 'a,
    {
        BoundOnComplete {
            inner: self.inner.bind_to(channel),
            on_complete: Some(self.on_complete),
        }
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedBoundGenerator<'a, G::Item, G::Output>
    where
        Self: 'a,
    {
        Box::new((*self).bind_to(channel))
    }
}

impl<G, F> BoundGenerator for BoundOnComplete<G, F>
where
    G: BoundGenerator + Unpin,
    F: FnOnce() + Send,
{
    type Item = G::Item;
    type Output = G::Output;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<G::Item, G::Output>>> {
        let this = self.get_mut();
        match Pin::new(&mut this.inner).poll_step(context) {
            Poll::Ready(Ok(GeneratorStep::Done(output))) => {
                if let Some(on_complete) = this.on_complete.take() {
                    on_complete();
                }
                Poll::Ready(Ok(GeneratorStep::Done(output)))
            }
            other => other,
        }
    }
}

impl<G, F: FnOnce()> Drop for BoundOnComplete<G, F> {
    fn drop(&mut self) {
        if self.on_complete.is_some() {
            tracing::debug!(
                "OnComplete generator dropped before completion; completion callback not called"
            );
        }
    }
}

#[doc(hidden)]
pub trait Step<I> {
    type Out;

    fn step(&mut self, item: I) -> Option<Self::Out>;
}

#[doc(hidden)]
pub struct Map<F>(F);

impl<F, T, U> Step<T> for Map<F>
where
    F: FnMut(T) -> U,
{
    type Out = U;

    fn step(&mut self, item: T) -> Option<U> {
        Some((self.0)(item))
    }
}

#[doc(hidden)]
pub struct MapOk<F>(F);

impl<F, T, U> Step<DeltaResult<T>> for MapOk<F>
where
    F: FnMut(T) -> U,
{
    type Out = DeltaResult<U>;

    fn step(&mut self, item: DeltaResult<T>) -> Option<DeltaResult<U>> {
        Some(item.map(&mut self.0))
    }
}

#[doc(hidden)]
pub struct FilterOk<F>(F);

impl<F, T> Step<DeltaResult<T>> for FilterOk<F>
where
    F: FnMut(&T) -> bool,
{
    type Out = DeltaResult<T>;

    fn step(&mut self, item: DeltaResult<T>) -> Option<DeltaResult<T>> {
        match item {
            Err(err) => Some(Err(err)),
            Ok(item) if (self.0)(&item) => Some(Ok(item)),
            Ok(_) => None,
        }
    }
}

/// Generator returned by [`GeneratorExt::map`], [`GeneratorExt::map_ok`], and
/// [`GeneratorExt::filter_ok`].
// WARNING: We manually impl `Unpin` for `FilterMap` so `S` need not be `Unpin`. This is sound only
// because `FilterMap` does not structurally pin the step; it is accessed only through `&mut S`.
pub struct FilterMap<G, S>(G, S);

impl<G: Unpin, S> Unpin for FilterMap<G, S> {}

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
    ) -> Poll<DeltaResult<GeneratorStep<S::Out, G::Output>>> {
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

    /// Invoke the witness for `mapper` and its channel-bound item.
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
            _output: PhantomType::default(),
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
    _output: PhantomType<U>,
}

impl<'channel, P, G, F, W, U> BoundMapAsync<'channel, P, G, F, W, U>
where
    P: Send + 'static,
    G: BoundGenerator + Unpin,
    F: for<'a> AsyncFnMut(&'a Channel<P>, G::Item) -> U + Send,
    W: for<'a> MapAsyncWitness<'a, P, F, G::Item, U> + Send,
    U: Send + 'static,
{
    async fn next(mut self) -> DeltaResult<ControlFlow<G::Output, (Self, U)>> {
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

#[doc(hidden)]
pub enum FlattenAction<Inner, Yield> {
    Enter(Inner),
    Yield(Yield),
}

#[doc(hidden)]
pub trait FlattenPolicy {
    type OuterItem: Send;
    type Inner: Send;
    type InnerItem: Send;
    type Yield: Send;

    fn classify(item: Self::OuterItem) -> FlattenAction<Self::Inner, Self::Yield>;
    fn wrap_yield(item: Self::InnerItem) -> Self::Yield;
}

/// Policy for [`GeneratorExt::flatten_async`].
pub struct FlattenIdent<I, T>(PhantomType<(I, T)>);

impl<I: Send, T: Send> FlattenPolicy for FlattenIdent<I, T> {
    type OuterItem = I;
    type Inner = I;
    type InnerItem = T;
    type Yield = T;

    fn classify(item: I) -> FlattenAction<I, T> {
        FlattenAction::Enter(item)
    }

    fn wrap_yield(item: T) -> T {
        item
    }
}

/// Policy for [`GeneratorExt::flatten_ok`].
pub struct FlattenResult<I, T>(PhantomType<(I, T)>);

impl<I: Send, T: Send> FlattenPolicy for FlattenResult<I, T> {
    type OuterItem = DeltaResult<I>;
    type Inner = I;
    type InnerItem = T;
    type Yield = DeltaResult<T>;

    fn classify(item: DeltaResult<I>) -> FlattenAction<I, DeltaResult<T>> {
        match item {
            Ok(inner) => FlattenAction::Enter(inner),
            Err(err) => FlattenAction::Yield(Err(err)),
        }
    }

    fn wrap_yield(item: T) -> DeltaResult<T> {
        Ok(item)
    }
}

/// Generator returned by [`GeneratorExt::flatten_async`] and [`GeneratorExt::flatten_ok`].
pub struct FlattenBy<G, Policy> {
    outer: G,
    _policy: PhantomType<Policy>,
}

impl<G, Policy> FlattenBy<G, Policy> {
    fn new(outer: G) -> Self {
        Self {
            outer,
            _policy: PhantomType::default(),
        }
    }
}

impl<P, G, Policy> Generator<P> for FlattenBy<G, Policy>
where
    P: Send + 'static,
    G: Generator<P, Item = Policy::OuterItem>,
    Policy: FlattenPolicy,
    Policy::Inner: Generator<P, Item = Policy::InnerItem, Output = ()>,
{
    type Item = Policy::Yield;
    type Output = G::Output;

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl BoundGenerator<Item = Policy::Yield, Output = G::Output> + Unpin + 'a
    where
        Self: 'a,
    {
        BoundFlatten::<'_, P, _, Policy, _, _> {
            outer: self.outer.bind_to(channel.clone()),
            channel,
            bind_inner: |inner: Policy::Inner, channel| inner.bind_to(channel),
            inner: None,
            _policy: PhantomType::default(),
        }
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedBoundGenerator<'a, Policy::Yield, G::Output>
    where
        Self: 'a,
    {
        Box::new((*self).bind_to(channel))
    }
}

struct BoundFlatten<'a, P: 'static, G, Policy, F, B> {
    outer: G,
    channel: ChannelBinding<'a, P>,
    bind_inner: F,
    inner: Option<B>,
    _policy: PhantomType<Policy>,
}

// Only `outer` and `inner` are polled; `channel` is cloned and `bind_inner` is called through &mut.
impl<P, G: Unpin, Policy, F, B: Unpin> Unpin for BoundFlatten<'_, P, G, Policy, F, B> {}

impl<'a, P, G, Policy, F, B> BoundGenerator for BoundFlatten<'a, P, G, Policy, F, B>
where
    P: Send + 'static,
    G: BoundGenerator<Item = Policy::OuterItem> + Unpin,
    Policy: FlattenPolicy,
    Policy::OuterItem: Send + 'a,
    Policy::Inner: Send + 'a,
    F: FnMut(Policy::Inner, ChannelBinding<'a, P>) -> B + Send,
    B: BoundGenerator<Item = Policy::InnerItem, Output = ()> + Unpin,
{
    type Item = Policy::Yield;
    type Output = G::Output;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<Policy::Yield, G::Output>>> {
        let this = self.get_mut();
        loop {
            if let Some(inner) = &mut this.inner {
                match Pin::new(inner).poll_step(context) {
                    Poll::Ready(Ok(GeneratorStep::Yield(item))) => {
                        return Poll::Ready(Ok(GeneratorStep::Yield(Policy::wrap_yield(item))));
                    }
                    Poll::Ready(Ok(GeneratorStep::Done(()))) => this.inner = None,
                    Poll::Ready(Err(err)) => return Poll::Ready(Err(err)),
                    Poll::Pending => return Poll::Pending,
                }
            }

            match Policy::classify(ready_yield!(Pin::new(&mut this.outer).poll_step(context))) {
                FlattenAction::Enter(inner) => {
                    this.inner = Some((this.bind_inner)(inner, this.channel.clone()));
                }
                FlattenAction::Yield(item) => {
                    return Poll::Ready(Ok(GeneratorStep::Yield(item)));
                }
            }
        }
    }
}

/// Bound generator returned by [`BoundGenerator::peekable`].
// WARNING: We manually impl `Unpin` for `Peekable` so `G::Item` need not be `Unpin`. This is sound
// only because `Peekable` does not structurally pin `peeked`; it is only moved through ordinary
// `Option` operations.
pub struct Peekable<G: BoundGenerator> {
    inner: G,
    peeked: Option<G::Item>,
    done: Option<G::Output>,
}

impl<G: BoundGenerator + Unpin> Unpin for Peekable<G> {}

impl<G: BoundGenerator + Unpin> Peekable<G> {
    fn poll_inner(
        &mut self,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<G::Item, G::Output>>> {
        Pin::new(&mut self.inner).poll_step(context)
    }

    /// Return the next item without consuming it, or `None` at completion.
    pub async fn peek(&mut self) -> DeltaResult<Option<&G::Item>> {
        if self.peeked.is_none() && self.done.is_none() {
            match poll_fn(|context| self.poll_inner(context)).await? {
                GeneratorStep::Yield(item) => self.peeked = Some(item),
                GeneratorStep::Done(output) => self.done = Some(output),
            }
        }
        Ok(self.peeked.as_ref())
    }
}

impl<G: BoundGenerator + Unpin> BoundGenerator for Peekable<G> {
    type Item = G::Item;
    type Output = G::Output;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<G::Item, G::Output>>> {
        let this = self.get_mut();
        if let Some(item) = this.peeked.take() {
            return Poll::Ready(Ok(GeneratorStep::Yield(item)));
        }
        if let Some(output) = this.done.take() {
            return Poll::Ready(Ok(GeneratorStep::Done(output)));
        }
        match this.poll_inner(context) {
            Poll::Ready(Ok(GeneratorStep::Done(output))) => {
                Poll::Ready(Ok(GeneratorStep::Done(output)))
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
    ) -> Poll<DeltaResult<GeneratorStep<A::Item, B::Output>>> {
        let this = self.get_mut();
        if let Some(first) = &mut this.first {
            match Pin::new(first).poll_step(context) {
                Poll::Ready(Ok(GeneratorStep::Done(()))) => this.first = None,
                Poll::Ready(Ok(GeneratorStep::Yield(item))) => {
                    return Poll::Ready(Ok(GeneratorStep::Yield(item)));
                }
                Poll::Ready(Err(err)) => return Poll::Ready(Err(err)),
                Poll::Pending => return Poll::Pending,
            }
        }
        Pin::new(&mut this.second).poll_step(context)
    }
}

/// Generator returned by [`GeneratorExt::flatten`].
// WARNING: We manually impl `Unpin` for `Flatten` so `I` need not be `Unpin`. This is sound only
// because `Flatten` does not structurally pin `rest`; it is accessed only through
// `Iterator::next(&mut self)` and moved by ordinary `Option` operations.
pub struct Flatten<G, I> {
    inner: G,
    rest: Option<I>,
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
    ) -> Poll<DeltaResult<GeneratorStep<I::Item, G::Output>>> {
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
// WARNING: We manually impl `Unpin` for `Iter`, so pinning the generator will not structurally pin
// `I`, even if `I: !Unpin`. This is sound only because `Iter` does not structurally pin its inner
// value; it is accessed only through `Iterator::next(&mut self)`.
pub struct Iter<I>(I);

impl<I> Unpin for Iter<I> {}

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
    ) -> Poll<DeltaResult<GeneratorStep<I::Item, ()>>> {
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
    Fut: Future<Output = DeltaResult<ControlFlow<O, (S, I)>>>,
{
    Unfold {
        idle: Some(state),
        pending: None,
        step,
    }
}

/// Builds a [`BoundGenerator`] from owned state and an async `next(&mut state)` callback.
///
/// Used by [`unfold_mut`]. Pass [`unfold_mut_witness`] so type inference can prove the step
/// future is [`Send`]. Each step boxes that future so the returned generator is [`Unpin`].
///
/// [`unfold_mut`]: crate::coroutine::generator::unfold_mut
pub(crate) fn unfold_mut_with<'s, S, I, F, Fut>(
    state: S,
    next: F,
    mut witness: impl FnMut(S, F) -> Fut + Send + 's,
) -> impl BoundGenerator<Item = I, Output = ()> + Unpin + 's
where
    S: Send + 's,
    I: Send + 'static,
    F: Send + 's,
    Fut: Future<Output = DeltaResult<ControlFlow<(), ((S, F), I)>>> + Send + 's,
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
) -> DeltaResult<ControlFlow<(), ((S, F), I)>>
where
    F: for<'s> AsyncFnMut(&'s mut S) -> DeltaResult<Option<I>>,
{
    let item = next(&mut state).await?;
    Ok(match item {
        Some(item) => ControlFlow::Continue(((state, next), item)),
        None => ControlFlow::Break(()),
    })
}

/// Generator returned by [`unfold`].
// WARNING: We manually impl `Unpin` for `Unfold`, even when its inputs are not `Unpin`. This is
// sound only because `idle` and `step` are not structurally pinned; the pending future remains
// pinned by its `Pin<Box<Fut>>` when `Unfold` moves.
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
    Fut: Future<Output = DeltaResult<ControlFlow<O, (S, I)>>> + Send,
{
    type Item = I;
    type Output = O;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<I, O>>> {
        let this = self.get_mut();
        let mut pending = match this.pending.take() {
            Some(pending) => pending,
            None => {
                let Some(state) = this.idle.take() else {
                    return Poll::Ready(Err(Error::internal_error(
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
