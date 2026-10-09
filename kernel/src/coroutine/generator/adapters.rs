//! Generator traits, combinators, and constructors.
//!
//! Bound generators are polled for the next yield or completion. Unbound pipelines apply
//! combinators first, then attach a request channel.

use std::future::{poll_fn, Future};
use std::ops::{AsyncFnMut, ControlFlow};
use std::pin::Pin;
use std::task::{Context, Poll};

use super::{BoundGenerator, ChannelBinding};
use crate::coroutine::Channel;
use crate::utils::PhantomType;
use crate::{DeltaResult, Error};

/// Yield or completion produced by polling a bound [`Generator`].
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

/// Consume helpers and combinators for [`IntoGenerator`] and [`Generator`].
pub trait GeneratorExt {
    /// Binds this generator to `channel`, which receives its connector requests.
    fn bind<'a, P: Send + 'static>(
        self,
        channel: &'a Channel<P>,
    ) -> impl Generator<Item = Self::Item, Output = Self::Output> + Unpin + 'a
    where
        Self: IntoGenerator<P> + Sized + 'a,
        Self::Item: 'a,
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

    /// Counterpart of `Itertools::try_collect`: collects successful yielded items, stopping at the
    /// first item or generator error.
    fn try_collect<T, C>(mut self) -> impl Future<Output = DeltaResult<C>> + Send
    where
        Self: Generator<Item = DeltaResult<T>, Output = ()> + Sized + Unpin,
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
    fn collect<C>(mut self) -> impl Future<Output = DeltaResult<C>> + Send
    where
        Self: Generator<Output = ()> + Sized + Unpin,
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

    /// Counterpart of [`Iterator::map`]: transforms each yielded item.
    fn map<F>(self, map: F) -> Map<Self, F>
    where
        Self: Sized,
    {
        Map { inner: self, map }
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
        Self: IntoGenerator<P> + Sized,
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
    fn map_ok<F>(self, map: F) -> MapOk<Self, F>
    where
        Self: Sized,
    {
        MapOk { inner: self, map }
    }

    /// Counterpart of `Itertools::filter_ok`: retains successful yielded items matching
    /// `predicate`.
    fn filter_ok<F>(self, predicate: F) -> FilterOk<Self, F>
    where
        Self: Sized,
    {
        FilterOk {
            inner: self,
            predicate,
        }
    }

    /// Flattens inner generators, preserving this generator's completion value.
    fn flatten_async<P, I, T>(self) -> FlattenAsync<Self, P, I, T>
    where
        Self: Sized,
    {
        FlattenAsync {
            outer: self,
            _types: PhantomType::default(),
        }
    }

    /// Counterpart of `Itertools::flatten_ok`: flattens successful inner generators.
    fn flatten_ok<P, I, T>(self) -> FlattenOk<Self, P, I, T>
    where
        Self: Sized,
    {
        FlattenOk {
            outer: self,
            _types: PhantomType::default(),
        }
    }

    /// Counterpart of [`Iterator::peekable`]: allows inspecting the next item before consuming it.
    fn peekable<I, O>(self) -> Peekable<Self, I, O>
    where
        Self: Sized,
    {
        Peekable {
            inner: self,
            peeked: None,
            done: None,
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
pub trait IntoGenerator<P: Send + 'static>: GeneratorExt + Send {
    /// Type of the yielded items.
    type Item: Send;

    /// Type of the completion value.
    type Output: Send + 'static;

    /// Type-erases this generator as `Box<dyn IntoGenerator<...>>`.
    fn into_boxed<'a>(
        self,
    ) -> Box<dyn IntoGenerator<P, Item = Self::Item, Output = Self::Output> + 'a>
    where
        Self: Sized + 'a,
        Self::Item: 'a,
    {
        Box::new(self)
    }

    /// Binds this generator to a borrowed or owned request channel.
    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = Self::Item, Output = Self::Output> + Unpin + 'a
    where
        Self: Sized + 'a,
        Self::Item: 'a;

    /// Object-safe binding used after type erasure.
    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedGenerator<'a, Self::Item, Self::Output>
    where
        Self: 'a,
        Self::Item: 'a;
}

/// Pollable generator bound to a request channel.
pub trait Generator: GeneratorExt + Send {
    /// Type of the yielded items.
    type Item: Send;

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

impl<'a, Y: Send + 'a, O: Send + 'static> GeneratorExt for BoundGenerator<'a, Y, O> {}
impl<'g, P: Send + 'static, Y: Send + 'g, O: Send + 'static> GeneratorExt
    for Box<dyn IntoGenerator<P, Item = Y, Output = O> + 'g>
{
}
impl<'a, Y: Send + 'a, O: Send + 'static> GeneratorExt for BoxedGenerator<'a, Y, O> {}
impl<G, F> GeneratorExt for Map<G, F> {}
impl<G, F> GeneratorExt for OnComplete<G, F> {}
impl<G, F: FnOnce()> GeneratorExt for BoundOnComplete<G, F> {}
impl<G, P, F, W, U> GeneratorExt for MapAsync<G, P, F, W, U> {}
impl<G, F> GeneratorExt for MapOk<G, F> {}
impl<G, F> GeneratorExt for FilterOk<G, F> {}
impl<G, P, I, T> GeneratorExt for FlattenAsync<G, P, I, T> {}
impl<G, P, I, T> GeneratorExt for FlattenOk<G, P, I, T> {}
impl<G, I, O> GeneratorExt for Peekable<G, I, O> {}
impl<A, B> GeneratorExt for Chain<A, B> {}
impl<I> GeneratorExt for Iter<I> {}
impl<S, F, Fut> GeneratorExt for Unfold<S, F, Fut> {}

impl<'a, Y: Send + 'a, O: Send + 'static> Generator for BoundGenerator<'a, Y, O> {
    type Item = Y;
    type Output = O;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<Y, O>>> {
        self.get_mut().poll_step(context)
    }
}

impl<'a, Y: Send + 'a, O: Send + 'static> Generator for BoxedGenerator<'a, Y, O> {
    type Item = Y;
    type Output = O;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<Y, O>>> {
        Pin::new(&mut **self.get_mut()).poll_step(context)
    }
}

impl<'g, P: Send + 'static, Y: Send + 'g, O: Send + 'static> IntoGenerator<P>
    for Box<dyn IntoGenerator<P, Item = Y, Output = O> + 'g>
{
    type Item = Y;
    type Output = O;

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = Y, Output = O> + Unpin + 'a
    where
        Self: 'a,
        Y: 'a,
    {
        self.bind_boxed(channel)
    }

    fn into_boxed<'a>(self) -> Box<dyn IntoGenerator<P, Item = Y, Output = O> + 'a>
    where
        Self: 'a,
        Y: 'a,
    {
        self
    }

    fn bind_boxed<'a>(self: Box<Self>, channel: ChannelBinding<'a, P>) -> BoxedGenerator<'a, Y, O>
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

impl<P, G, F> IntoGenerator<P> for OnComplete<G, F>
where
    P: Send + 'static,
    G: IntoGenerator<P>,
    F: FnOnce() + Send,
{
    type Item = G::Item;
    type Output = G::Output;

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = G::Item, Output = G::Output> + Unpin + 'a
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
    ) -> BoxedGenerator<'a, G::Item, G::Output>
    where
        Self: 'a,
    {
        Box::new((*self).bind_to(channel))
    }
}

impl<G, F> Generator for BoundOnComplete<G, F>
where
    G: Generator + Unpin,
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

/// Generator returned by [`GeneratorExt::map`].
// WARNING: We manually impl `Unpin` for `Map` so `F` need not be `Unpin`. This is sound only
// because `Map` does not structurally pin `map`; it is accessed only through `&mut F`.
pub struct Map<G, F> {
    inner: G,
    map: F,
}

impl<G: Unpin, F> Unpin for Map<G, F> {}

impl<P, G, U, F> IntoGenerator<P> for Map<G, F>
where
    P: Send + 'static,
    G: IntoGenerator<P>,
    U: Send + 'static,
    F: FnMut(G::Item) -> U + Send + 'static,
{
    type Item = U;
    type Output = G::Output;

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = U, Output = G::Output> + Unpin + 'a
    where
        Self: 'a,
    {
        Map {
            inner: self.inner.bind_to(channel),
            map: self.map,
        }
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedGenerator<'a, U, G::Output>
    where
        Self: 'a,
    {
        Box::new((*self).bind_to(channel))
    }
}

impl<G, U, F> Generator for Map<G, F>
where
    G: Generator + Unpin,
    U: Send + 'static,
    F: FnMut(G::Item) -> U + Send,
{
    type Item = U;
    type Output = G::Output;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<U, G::Output>>> {
        let this = self.get_mut();
        let item = ready_yield!(Pin::new(&mut this.inner).poll_step(context));
        Poll::Ready(Ok(GeneratorStep::Yield((this.map)(item))))
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

impl<P, G, F, W, U> IntoGenerator<P> for MapAsync<G, P, F, W, U>
where
    P: Send + 'static,
    G: IntoGenerator<P>,
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
    ) -> impl Generator<Item = U, Output = G::Output> + Unpin + 'a
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
    ) -> BoxedGenerator<'a, U, G::Output>
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
    G: Generator + Unpin,
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

/// Generator returned by [`GeneratorExt::map_ok`].
// WARNING: We manually impl `Unpin` for `MapOk` so `F` need not be `Unpin`. This is sound only
// because `MapOk` does not structurally pin `map`; it is accessed only through `&mut F`.
pub struct MapOk<G, F> {
    inner: G,
    map: F,
}

impl<G: Unpin, F> Unpin for MapOk<G, F> {}

impl<P, G, T, U, F> IntoGenerator<P> for MapOk<G, F>
where
    P: Send + 'static,
    G: IntoGenerator<P, Item = DeltaResult<T>>,
    T: Send + 'static,
    U: Send + 'static,
    F: FnMut(T) -> U + Send + 'static,
{
    type Item = DeltaResult<U>;
    type Output = G::Output;

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = DeltaResult<U>, Output = G::Output> + Unpin + 'a
    where
        Self: 'a,
    {
        MapOk {
            inner: self.inner.bind_to(channel),
            map: self.map,
        }
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedGenerator<'a, DeltaResult<U>, G::Output>
    where
        Self: 'a,
    {
        Box::new((*self).bind_to(channel))
    }
}

impl<G, T, U, F> Generator for MapOk<G, F>
where
    G: Generator<Item = DeltaResult<T>> + Unpin,
    T: Send + 'static,
    U: Send + 'static,
    F: FnMut(T) -> U + Send,
{
    type Item = DeltaResult<U>;
    type Output = G::Output;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<DeltaResult<U>, G::Output>>> {
        let this = self.get_mut();
        let item = ready_yield!(Pin::new(&mut this.inner).poll_step(context));
        let item = item.map(|item| (this.map)(item));
        Poll::Ready(Ok(GeneratorStep::Yield(item)))
    }
}

/// Generator returned by [`GeneratorExt::filter_ok`].
// WARNING: We manually impl `Unpin` for `FilterOk` so `F` need not be `Unpin`. This is sound only
// because `FilterOk` does not structurally pin `predicate`; it is accessed only through `&mut F`.
pub struct FilterOk<G, F> {
    inner: G,
    predicate: F,
}

impl<G: Unpin, F> Unpin for FilterOk<G, F> {}

impl<P, G, T, F> IntoGenerator<P> for FilterOk<G, F>
where
    P: Send + 'static,
    G: IntoGenerator<P, Item = DeltaResult<T>>,
    T: Send + 'static,
    F: FnMut(&T) -> bool + Send + 'static,
{
    type Item = DeltaResult<T>;
    type Output = G::Output;

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = DeltaResult<T>, Output = G::Output> + Unpin + 'a
    where
        Self: 'a,
    {
        FilterOk {
            inner: self.inner.bind_to(channel),
            predicate: self.predicate,
        }
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedGenerator<'a, DeltaResult<T>, G::Output>
    where
        Self: 'a,
    {
        Box::new((*self).bind_to(channel))
    }
}

impl<G, T, F> Generator for FilterOk<G, F>
where
    G: Generator<Item = DeltaResult<T>> + Unpin,
    T: Send + 'static,
    F: FnMut(&T) -> bool + Send,
{
    type Item = DeltaResult<T>;
    type Output = G::Output;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<DeltaResult<T>, G::Output>>> {
        let this = self.get_mut();
        loop {
            let item = ready_yield!(Pin::new(&mut this.inner).poll_step(context));
            let item = match item {
                Err(err) => Err(err),
                Ok(item) if (this.predicate)(&item) => Ok(item),
                Ok(_) => continue,
            };
            return Poll::Ready(Ok(GeneratorStep::Yield(item)));
        }
    }
}

/// Generator returned by [`GeneratorExt::flatten_async`].
pub struct FlattenAsync<G, P, I, T> {
    outer: G,
    _types: PhantomType<(P, I, T)>,
}

impl<P, G, I, T> IntoGenerator<P> for FlattenAsync<G, P, I, T>
where
    P: Send + 'static,
    G: IntoGenerator<P, Item = I>,
    I: IntoGenerator<P, Item = T, Output = ()>,
    T: Send,
{
    type Item = T;
    type Output = G::Output;

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = T, Output = G::Output> + Unpin + 'a
    where
        Self: 'a,
    {
        BoundFlattenAsync {
            outer: self.outer.bind_to(channel.clone()),
            channel,
            bind_inner: |inner: I, channel| inner.bind_to(channel),
            inner: None,
        }
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedGenerator<'a, T, G::Output>
    where
        Self: 'a,
    {
        Box::new((*self).bind_to(channel))
    }
}

struct BoundFlattenAsync<'a, P: 'static, G, F, B> {
    outer: G,
    channel: ChannelBinding<'a, P>,
    bind_inner: F,
    inner: Option<B>,
}

// Only `outer` and `inner` are polled; `channel` is cloned and `bind_inner` is called through &mut.
impl<P, G: Unpin, F, B: Unpin> Unpin for BoundFlattenAsync<'_, P, G, F, B> {}
impl<P, G, F, B> GeneratorExt for BoundFlattenAsync<'_, P, G, F, B> {}

impl<'a, P, G, I, F, B> Generator for BoundFlattenAsync<'a, P, G, F, B>
where
    P: Send + 'static,
    G: Generator<Item = I> + Unpin,
    I: Send + 'a,
    F: FnMut(I, ChannelBinding<'a, P>) -> B + Send,
    B: Generator<Output = ()> + Unpin,
{
    type Item = B::Item;
    type Output = G::Output;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<B::Item, G::Output>>> {
        let this = self.get_mut();
        loop {
            if let Some(inner) = &mut this.inner {
                match Pin::new(inner).poll_step(context) {
                    Poll::Ready(Ok(GeneratorStep::Yield(item))) => {
                        return Poll::Ready(Ok(GeneratorStep::Yield(item)));
                    }
                    Poll::Ready(Ok(GeneratorStep::Done(()))) => this.inner = None,
                    Poll::Ready(Err(err)) => return Poll::Ready(Err(err)),
                    Poll::Pending => return Poll::Pending,
                }
            }

            let inner = ready_yield!(Pin::new(&mut this.outer).poll_step(context));
            this.inner = Some((this.bind_inner)(inner, this.channel.clone()));
        }
    }
}

/// Generator returned by [`GeneratorExt::flatten_ok`].
pub struct FlattenOk<G, P, I, T> {
    outer: G,
    _types: PhantomType<(P, I, T)>,
}

impl<P, G, I, T> IntoGenerator<P> for FlattenOk<G, P, I, T>
where
    P: Send + 'static,
    G: IntoGenerator<P, Item = DeltaResult<I>>,
    I: IntoGenerator<P, Item = T, Output = ()>,
    T: Send,
{
    type Item = DeltaResult<T>;
    type Output = G::Output;

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = DeltaResult<T>, Output = G::Output> + Unpin + 'a
    where
        Self: 'a,
    {
        BoundFlattenOk {
            outer: self.outer.bind_to(channel.clone()),
            channel,
            bind_inner: |inner: I, channel| inner.bind_to(channel),
            inner: None,
        }
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedGenerator<'a, DeltaResult<T>, G::Output>
    where
        Self: 'a,
    {
        Box::new((*self).bind_to(channel))
    }
}

struct BoundFlattenOk<'a, P: 'static, G, F, B> {
    outer: G,
    channel: ChannelBinding<'a, P>,
    bind_inner: F,
    inner: Option<B>,
}

// Only `outer` and `inner` are polled; `channel` is cloned and `bind_inner` is called through &mut.
impl<P, G: Unpin, F, B: Unpin> Unpin for BoundFlattenOk<'_, P, G, F, B> {}
impl<P, G, F, B> GeneratorExt for BoundFlattenOk<'_, P, G, F, B> {}

impl<'a, P, G, I, F, B> Generator for BoundFlattenOk<'a, P, G, F, B>
where
    P: Send + 'static,
    G: Generator<Item = DeltaResult<I>> + Unpin,
    I: Send + 'a,
    F: FnMut(I, ChannelBinding<'a, P>) -> B + Send,
    B: Generator<Output = ()> + Unpin,
{
    type Item = DeltaResult<B::Item>;
    type Output = G::Output;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<DeltaResult<B::Item>, G::Output>>> {
        let this = self.get_mut();
        loop {
            if let Some(inner) = &mut this.inner {
                match Pin::new(inner).poll_step(context) {
                    Poll::Ready(Ok(GeneratorStep::Yield(item))) => {
                        return Poll::Ready(Ok(GeneratorStep::Yield(Ok(item))));
                    }
                    Poll::Ready(Ok(GeneratorStep::Done(()))) => this.inner = None,
                    Poll::Ready(Err(err)) => return Poll::Ready(Err(err)),
                    Poll::Pending => return Poll::Pending,
                }
            }

            match ready_yield!(Pin::new(&mut this.outer).poll_step(context)) {
                Ok(inner) => {
                    this.inner = Some((this.bind_inner)(inner, this.channel.clone()));
                }
                Err(err) => return Poll::Ready(Ok(GeneratorStep::Yield(Err(err)))),
            }
        }
    }
}

/// Generator returned by [`GeneratorExt::peekable`].
// WARNING: We manually impl `Unpin` for `Peekable` so `I` need not be `Unpin`. This is sound only
// because `Peekable` does not structurally pin `peeked`; it is only moved through ordinary `Option`
// operations.
pub struct Peekable<G, I, O> {
    inner: G,
    peeked: Option<I>,
    done: Option<O>,
}

impl<G: Unpin, I, O> Unpin for Peekable<G, I, O> {}

impl<G, I, O> Peekable<G, I, O>
where
    G: Generator<Item = I, Output = O> + Unpin,
    I: Send + 'static,
    O: Send + 'static,
{
    fn poll_inner(&mut self, context: &mut Context<'_>) -> Poll<DeltaResult<GeneratorStep<I, O>>> {
        Pin::new(&mut self.inner).poll_step(context)
    }

    /// Return the next item without consuming it, or `None` at completion.
    pub async fn peek(&mut self) -> DeltaResult<Option<&I>> {
        if self.peeked.is_none() && self.done.is_none() {
            match poll_fn(|context| self.poll_inner(context)).await? {
                GeneratorStep::Yield(item) => self.peeked = Some(item),
                GeneratorStep::Done(output) => self.done = Some(output),
            }
        }
        Ok(self.peeked.as_ref())
    }
}

impl<P, G, I, O> IntoGenerator<P> for Peekable<G, I, O>
where
    P: Send + 'static,
    G: IntoGenerator<P, Item = I, Output = O>,
    I: Send + 'static,
    O: Send + 'static,
{
    type Item = I;
    type Output = O;

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = I, Output = O> + Unpin + 'a
    where
        Self: 'a,
    {
        Peekable {
            inner: self.inner.bind_to(channel),
            peeked: self.peeked,
            done: self.done,
        }
    }

    fn bind_boxed<'a>(self: Box<Self>, channel: ChannelBinding<'a, P>) -> BoxedGenerator<'a, I, O>
    where
        Self: 'a,
    {
        Box::new((*self).bind_to(channel))
    }
}

impl<G, I, O> Generator for Peekable<G, I, O>
where
    G: Generator<Item = I, Output = O> + Unpin,
    I: Send + 'static,
    O: Send + 'static,
{
    type Item = I;
    type Output = O;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<I, O>>> {
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

impl<P, A, B> IntoGenerator<P> for Chain<A, B>
where
    P: Send + 'static,
    A: IntoGenerator<P, Output = ()>,
    B: IntoGenerator<P, Item = A::Item>,
{
    type Item = A::Item;
    type Output = B::Output;

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = A::Item, Output = B::Output> + Unpin + 'a
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
    ) -> BoxedGenerator<'a, A::Item, B::Output>
    where
        Self: 'a,
    {
        Box::new((*self).bind_to(channel))
    }
}

impl<A, B> Generator for Chain<A, B>
where
    A: Generator<Output = ()> + Unpin,
    B: Generator<Item = A::Item> + Unpin,
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

impl<P, G, I> IntoGenerator<P> for Flatten<G, I>
where
    P: Send + 'static,
    G: IntoGenerator<P>,
    G::Item: IntoIterator<IntoIter = I>,
    I: Iterator + Send + 'static,
    I::Item: Send + 'static,
{
    type Item = I::Item;
    type Output = G::Output;

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = I::Item, Output = G::Output> + Unpin + 'a
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
    ) -> BoxedGenerator<'a, I::Item, G::Output>
    where
        Self: 'a,
    {
        Box::new((*self).bind_to(channel))
    }
}

impl<G, I> Generator for Flatten<G, I>
where
    G: Generator + Unpin,
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

impl<I: Iterator + Send, P: Send + 'static> IntoGenerator<P> for Iter<I>
where
    I::Item: Send,
{
    type Item = I::Item;
    type Output = ();

    fn bind_to<'a>(
        self,
        _channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = I::Item, Output = ()> + Unpin + 'a
    where
        Self: 'a,
        I::Item: 'a,
    {
        self
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedGenerator<'a, I::Item>
    where
        Self: 'a,
        I::Item: 'a,
    {
        self
    }
}

impl<I: Iterator + Send> Generator for Iter<I>
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

/// Builds a [`Generator`] by repeatedly passing owned state through `step`.
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

/// Builds a [`Generator`] from owned state and an async `next(&mut state)` callback.
///
/// Used by [`unfold_mut`]. Pass [`unfold_mut_witness`] so type inference can prove the step
/// future is [`Send`]. Each step boxes that future so the returned generator is [`Unpin`].
///
/// [`unfold_mut`]: crate::coroutine::generator::unfold_mut
pub(crate) fn unfold_mut_with<'s, S, I, F, Fut>(
    state: S,
    next: F,
    mut witness: impl FnMut(S, F) -> Fut + Send + 's,
) -> impl Generator<Item = I, Output = ()> + Unpin + 's
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

impl<S, I, O, F, Fut> Generator for Unfold<S, F, Fut>
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
