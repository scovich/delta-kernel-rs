//! [`MapOk`], [`Chain`], and [`Empty`] types returned by [`GeneratorExt`] combinators.

use std::future::{poll_fn, Future};
use std::pin::Pin;
use std::task::{Context, Poll};

use super::{BoundGenerator, ChannelBinding};
use crate::coroutine::Channel;
use crate::utils::PhantomType;
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
    ) -> impl Generator<Item = Self::Item, Output = Self::Output> + GeneratorExt + Unpin + 'a
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

    /// Counterpart of `Itertools::map_ok`: transforms each yielded item.
    fn map_ok<F>(self, map: F) -> MapOk<Self, F>
    where
        Self: Sized,
    {
        MapOk { inner: self, map }
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
    ) -> impl Generator<Item = Self::Item, Output = Self::Output> + GeneratorExt + Unpin + 'a
    where
        Self: Sized;

    /// Object-safe binding used after type erasure.
    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> Box<dyn Generator<Item = Self::Item, Output = Self::Output> + Unpin + 'a>;
}

/// Pollable generator bound to a request channel.
pub trait Generator: Send {
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

impl<Y: Send + 'static, O: Send + 'static> GeneratorExt for BoundGenerator<'_, Y, O> {}
impl<P: Send + 'static, Y: Send + 'static, O: Send + 'static> GeneratorExt
    for Box<dyn IntoGenerator<P, Item = Y, Output = O>>
{
}
impl<'a, Y: Send + 'static, O: Send + 'static> GeneratorExt
    for Box<dyn Generator<Item = Y, Output = O> + Unpin + 'a>
{
}
impl<G, F> GeneratorExt for MapOk<G, F> {}
impl<A, B> GeneratorExt for Chain<A, B> {}
impl<Y: Send + 'static> GeneratorExt for Empty<Y> {}

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

impl<'a, Y: Send + 'static, O: Send + 'static> Generator
    for Box<dyn Generator<Item = Y, Output = O> + Unpin + 'a>
{
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
    ) -> impl Generator<Item = Y, Output = O> + GeneratorExt + Unpin + 'a {
        IntoGenerator::bind_boxed(self, channel)
    }

    fn into_boxed(self) -> Box<dyn IntoGenerator<P, Item = Y, Output = O>> {
        self
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> Box<dyn Generator<Item = Y, Output = O> + Unpin + 'a> {
        (*self).bind_boxed(channel)
    }
}

/// Generator returned by [`GeneratorExt::map_ok`].
pub struct MapOk<G, F> {
    inner: G,
    map: F,
}

impl<P, G, U, F> IntoGenerator<P> for MapOk<G, F>
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
    ) -> impl Generator<Item = U, Output = ()> + GeneratorExt + Unpin + 'a {
        MapOk {
            inner: self.inner.bind_to(channel),
            map: self.map,
        }
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> Box<dyn Generator<Item = U, Output = ()> + Unpin + 'a> {
        Box::new((*self).bind_to(channel))
    }
}

impl<G, U, F> Generator for MapOk<G, F>
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
    ) -> impl Generator<Item = A::Item, Output = ()> + GeneratorExt + Unpin + 'a {
        Chain {
            first: self.first.map(|first| first.bind_to(channel.clone())),
            second: self.second.bind_to(channel),
        }
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> Box<dyn Generator<Item = A::Item, Output = ()> + Unpin + 'a> {
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

/// Counterpart of [`std::iter::empty`]: a generator that yields nothing.
pub fn empty<Y: Send + 'static>() -> Empty<Y> {
    Empty(PhantomType::default())
}

/// Generator returned by [`empty`].
pub struct Empty<Y>(PhantomType<Y>);

impl<P: Send + 'static, Y: Send + 'static> IntoGenerator<P> for Empty<Y> {
    type Item = Y;
    type Output = ();

    fn bind_to<'a>(
        self,
        _channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = Y, Output = ()> + GeneratorExt + Unpin + 'a {
        self
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> Box<dyn Generator<Item = Y, Output = ()> + Unpin + 'a> {
        Box::new((*self).bind_to(channel))
    }
}

impl<Y: Send + 'static> Generator for Empty<Y> {
    type Item = Y;
    type Output = ();

    fn poll_step(
        self: Pin<&mut Self>,
        _context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorStep<Y, ()>>> {
        Poll::Ready(Ok(GeneratorStep::Done(())))
    }
}
