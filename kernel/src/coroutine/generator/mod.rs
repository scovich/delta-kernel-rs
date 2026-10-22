//! Internal generator pipelines, channel binding, and yield machinery.
//!
//! # Unbound pipelines
//!
//! Kernel functions return [`Generator`] values before a request [`Channel`] exists. Combinators
//! from [`GeneratorExt`] transform that unbound pipeline without adding a per-item yield handshake.
//! The initial surface includes [`GeneratorExt::flatten`],
//! [`AsyncIntoIterator::into_generator`](crate::coroutine::generator::AsyncIntoIterator::into_generator),
//! [`empty`](crate::coroutine::generator::empty), and
//! [`unfold`](crate::coroutine::generator::unfold); other iterator counterparts are added with
//! their first use.
//!
//! # Binding
//!
//! [`Generator::bind`] attaches a borrowed parent channel and produces a [`BoundGenerator`].
//! [`Generator::bind_to`] can instead attach the owned channel held by a
//! [`Coroutine`](crate::coroutine::Coroutine). [`BoundGenerator::poll_step`] returns
//! [`GeneratorStep::Yield`] or [`GeneratorStep::Done`], while connector requests travel through
//! the bound channel. [`BoundGenerator::next`] treats `Output = ()` completion as exhaustion.
//!
//! `generator!` creates an already-bound [`GeneratorBody`]. `generator_fn!` creates an unbound,
//! type-erased pipeline whose channel is supplied later.

use std::collections::VecDeque;
use std::future::{poll_fn, Future};
use std::ops::{AsyncFnMut, AsyncFnOnce, Deref};
use std::pin::Pin;
use std::sync::{Arc, Mutex, MutexGuard};
use std::task::{ready, Context, Poll};

use delta_kernel_derive::internal_api;
use tracing::{error, Instrument as _, Span};

pub use self::adapters::{
    empty, map_async_witness, unfold, AsyncIntoIterator, Enumerate, FilterMap, FilterOk, Flatten,
    Iter, Map, MapAsync, MapAsyncWitness, MapOk, Peekable, Unfold,
};
pub(crate) use self::adapters::{unfold_mut_with, unfold_mut_witness};
use super::witness::invoke_witness;
use super::{Channel, KernelFuture};
use crate::utils::PhantomType;
use crate::{KernelError, KernelResult};

// === Core generator protocol ===

/// Yield or completion produced by polling a bound [`BoundGenerator`].
pub enum GeneratorStep<Y, O> {
    /// A yielded item.
    Yield(Y),
    /// The generator completed.
    Done(O),
}

/// Combinators for unbound [`Generator`] pipelines.
///
/// These methods cannot live on [`Generator`] itself: that trait is parameterized by the request
/// type `P`, and naming `P` on a combinator would force it to be inferred before
/// [`Generator::bind`]. Bound consume helpers live on [`BoundGenerator`].
pub trait GeneratorExt {
    /// Counterpart of [`Iterator::map`]: transforms each yielded item.
    fn map<F>(self, map: F) -> FilterMap<Self, Map<F>>
    where
        Self: Sized,
    {
        FilterMap::new(self, Map::new(map))
    }

    /// Counterpart of [`itertools::Itertools::map_ok`]: transforms each successful yielded item.
    fn map_ok<F>(self, map: F) -> FilterMap<Self, MapOk<F>>
    where
        Self: Sized,
    {
        FilterMap::new(self, MapOk::new(map))
    }

    /// Counterpart of [`itertools::Itertools::filter_ok`]: retains successful items matching
    /// `predicate` and preserves errors.
    fn filter_ok<P>(self, predicate: P) -> FilterMap<Self, FilterOk<P>>
    where
        Self: Sized,
    {
        FilterMap::new(self, FilterOk::new(predicate))
    }

    /// Maps items asynchronously through the bound request channel.
    fn map_async<P, U, F, W>(self, witness: W, mapper: F) -> MapAsync<Self, P, F, W, U>
    where
        Self: Generator<P> + Sized,
        P: Send + 'static,
        U: Send + 'static,
        F: for<'a> AsyncFnMut(&'a Channel<P>, Self::Item) -> U + Send,
        W: Send,
        W: for<'a> MapAsyncWitness<'a, P, F, Self::Item, U>,
    {
        MapAsync::new(self, mapper, witness)
    }

    /// Counterpart of [`Iterator::flatten`]: yields the elements of each collection-valued item.
    ///
    /// Page-request failures remain control-plane `Err`. Collection elements are yielded as-is,
    /// including when the element type is [`KernelResult`]. Inner completion value is preserved.
    fn flatten<I>(self) -> Flatten<Self, I>
    where
        Self: Sized,
    {
        Flatten::new(self)
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
    ) -> Poll<KernelResult<GeneratorStep<Self::Item, Self::Output>>>;

    /// Waits for the next item, returning `Ok(None)` when the generator is exhausted.
    ///
    /// Restricted to [`Self::Output`] `()` so a real completion value cannot vanish in a
    /// `while let Some` loop. Callers that need `Output` poll [`Self::poll_step`] or drive a
    /// [`Coroutine`](super::Coroutine).
    fn next(&mut self) -> impl KernelFuture<Option<Self::Item>> + '_
    where
        Self: Sized + Unpin + BoundGenerator<Output = ()>,
    {
        poll_fn(move |context| {
            let step = ready!(Pin::new(&mut *self).poll_step(context));
            Poll::Ready(step.map(|step| match step {
                GeneratorStep::Yield(item) => Some(item),
                GeneratorStep::Done(()) => None,
            }))
        })
    }

    /// Counterpart of [`itertools::Itertools::try_collect`]: collects successful yielded items,
    /// stopping at the first item or generator error.
    fn try_collect<T, C>(mut self) -> impl KernelFuture<C>
    where
        Self: Sized + Unpin + BoundGenerator<Item = KernelResult<T>, Output = ()>,
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

    /// Counterpart of [`Iterator::peekable`]: allows inspecting the next item without consuming it.
    fn peekable(self) -> Peekable<Self>
    where
        Self: Sized + Unpin,
    {
        Peekable::new(self)
    }

    /// Counterpart of [`Iterator::enumerate`]: pairs each yielded item with its index.
    fn enumerate(self) -> Enumerate<Self>
    where
        Self: Sized + Unpin,
    {
        Enumerate::new(self)
    }
}

/// Type-erased generator with a pinned address and a caller-selected lifetime.
pub type BoxedBoundGenerator<'a, Y, O = ()> =
    Box<dyn BoundGenerator<Item = Y, Output = O> + Unpin + 'a>;

impl<'g, P: Send + 'static, Y: Send + 'g, O: Send + 'static> GeneratorExt
    for Box<dyn Generator<P, Item = Y, Output = O> + 'g>
{
}

impl<G: BoundGenerator + Unpin + ?Sized> BoundGenerator for Box<G> {
    type Item = G::Item;
    type Output = G::Output;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<KernelResult<GeneratorStep<G::Item, G::Output>>> {
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

// === Bound generator body ===

/// Bound generator that yields zero or more items to its caller before producing a final value.
///
/// Connector requests surface through the driver that owns the supplied channel, while
/// [`Self::next`] returns yielded items and reports successful completion as exhaustion. The `'a`
/// lifetime covers the channel binding and any state borrowed by the operation.
// This struct's manual `impl Unpin` is valid because it does not use structural field pinning;
// `body` holds its future in a pin-box.
pub struct GeneratorBody<'a, Y: Send + 'a, O = ()> {
    body: Option<Pin<Box<dyn KernelFuture<O> + 'a>>>,
    yields: Arc<YieldQueue<Y>>,
    terminal: Option<KernelResult<O>>,
}

impl<'a, Y: Send + 'a, O> Unpin for GeneratorBody<'a, Y, O> {}

impl<'a, Y: Send + 'a> GeneratorBody<'a, Y> {
    /// Waits for the next item, returning `None` if the generator is exhausted.
    ///
    /// Connector requests made while producing the item flow through the generator's channel.
    ///
    /// Calling this method after receiving anything other than `Ok(Some(_))` is a logic error.
    pub async fn next(&mut self) -> KernelResult<Option<Y>> {
        BoundGenerator::next(self).await
    }
}

impl<'a, Y: Send + 'a, O> GeneratorBody<'a, Y, O> {
    fn new_with_witness<State, F, Fut>(state: State, witness: F) -> Self
    where
        F: FnOnce(State, Arc<YieldQueue<Y>>) -> Fut,
        Fut: KernelFuture<O> + 'a,
    {
        let yields = Arc::default();
        Self {
            body: Some(Box::pin(invoke_witness(witness, state, Arc::clone(&yields))) as _),
            yields,
            terminal: None,
        }
    }

    fn take_ready_step(&mut self) -> Option<KernelResult<GeneratorStep<Y, O>>> {
        if let Some(result) = self.yields.pop().transpose() {
            return Some(result.map(GeneratorStep::Yield));
        }
        Some(self.terminal.take()?.map(GeneratorStep::Done))
    }

    fn poll_step(&mut self, context: &mut Context<'_>) -> Poll<KernelResult<GeneratorStep<Y, O>>> {
        if let Some(result) = self.take_ready_step() {
            return Poll::Ready(result);
        }
        let Some(body) = self.body.as_mut() else {
            return Poll::Ready(Err(KernelError::internal_error(
                "generator advanced after completion",
            )));
        };
        if let Poll::Ready(result) = body.as_mut().poll(context) {
            self.body = None;
            self.terminal = Some(result);
        }
        self.take_ready_step().map_or(Poll::Pending, Poll::Ready)
    }
}

impl<'a, Y: Send + 'a, O: Send + 'static> BoundGenerator for GeneratorBody<'a, Y, O> {
    type Item = Y;
    type Output = O;

    fn poll_step(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<KernelResult<GeneratorStep<Y, O>>> {
        self.get_mut().poll_step(context)
    }
}

// === Construction macros ===

/// Creates a bound generator using an existing request channel.
#[internal_api]
macro_rules! generator {
    ($channel:expr, $body:expr) => {
        $crate::coroutine::generator::BodyFn::<_, _, _, _>::new($body)
            .into_generator_with($channel, $crate::coroutine::generator::BodyFn::witness)
    };
}

/// Creates a [`Generator`] from an async closure.
///
/// The request channel is supplied later by [`Generator::bind`].
#[internal_api]
macro_rules! generator_fn {
    ($body:expr) => {
        $crate::coroutine::generator::box_generator(move |channel| {
            $crate::coroutine::generator::generator!(channel, $body)
        })
    };
}

/// Maps generator items asynchronously through the bound request channel.
#[internal_api]
macro_rules! map_async {
    ($generator:expr, $body:expr $(,)?) => {
        $generator.map_async($crate::coroutine::generator::map_async_witness, $body)
    };
}

/// Builds a [`BoundGenerator`] from owned state and an async `next(&mut state)` callback.
#[internal_api]
macro_rules! unfold_mut {
    ($state:expr, $next:expr) => {
        $crate::coroutine::generator::unfold_mut_with(
            $state,
            $next,
            $crate::coroutine::generator::unfold_mut_witness,
        )
    };
}

// === Channel binding and type erasure ===

/// Request-channel ownership supplied when binding a generator.
pub enum ChannelBinding<'a, P: 'static> {
    /// Bind to a borrowed parent request channel.
    Borrowed(&'a Channel<P>),
    /// Bind to an independently driven request channel.
    Owned(Arc<Channel<P>>),
}

impl<P: 'static> Clone for ChannelBinding<'_, P> {
    fn clone(&self) -> Self {
        match self {
            Self::Borrowed(channel) => Self::Borrowed(channel),
            Self::Owned(channel) => Self::Owned(Arc::clone(channel)),
        }
    }
}

impl<P: 'static> Deref for ChannelBinding<'_, P> {
    type Target = Channel<P>;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Borrowed(channel) => channel,
            Self::Owned(channel) => channel,
        }
    }
}

/// Hidden expansion state for [`generator_fn!`].
///
/// Stores an uninvoked binder until the generator is bound or erased. This type is public only so
/// exported macros can name it.
#[doc(hidden)]
#[internal_api]
pub(crate) struct GeneratorFn<F, P, Y, O> {
    bind: F,
    _type_signature: PhantomType<(P, Y, O)>,
}

impl<F, P, Y, O> GeneratorFn<F, P, Y, O> {
    /// Store `bind` without invoking it.
    #[internal_api]
    pub(crate) fn new(bind: F) -> Self {
        Self {
            bind,
            _type_signature: PhantomType::default(),
        }
    }
}

/// Erase a channel binder as `Box<dyn Generator<...>>`.
#[internal_api]
pub(crate) fn box_generator<P, Y, O, F>(bind: F) -> Box<dyn Generator<P, Item = Y, Output = O>>
where
    F: for<'a> FnOnce(ChannelBinding<'a, P>) -> GeneratorBody<'a, Y, O> + Send + 'static,
    P: Send + 'static,
    Y: Send + 'static,
    O: Send + 'static,
{
    Box::new(GeneratorFn::new(bind))
}

impl<F, P, Y, O> GeneratorExt for GeneratorFn<F, P, Y, O> {}

impl<F, P, Y, O> Generator<P> for GeneratorFn<F, P, Y, O>
where
    F: for<'a> FnOnce(ChannelBinding<'a, P>) -> GeneratorBody<'a, Y, O> + Send + 'static,
    P: Send + 'static,
    Y: Send + 'static,
    O: Send + 'static,
{
    type Item = Y;
    type Output = O;

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl BoundGenerator<Item = Y, Output = O> + Unpin + 'a
    where
        Self: 'a,
    {
        (self.bind)(channel)
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedBoundGenerator<'a, Y, O>
    where
        Self: 'a,
    {
        Box::new((*self).bind_to(channel))
    }
}

// === Yield plumbing ===

/// Generator access to connector requests and yielded output.
#[derive(derive_more::Deref)]
#[internal_api]
pub(crate) struct GeneratorChannel<'a, P: Send + 'static, Y: Send> {
    #[deref]
    channel: &'a Channel<P>,
    yields: Arc<YieldQueue<Y>>,
}

#[doc(hidden)]
#[internal_api]
pub(crate) struct YieldQueue<Y>(Mutex<VecDeque<Y>>);

impl<Y> Default for YieldQueue<Y> {
    fn default() -> Self {
        Self(Mutex::default())
    }
}

impl<Y> YieldQueue<Y> {
    fn push(&self, item: Y) -> KernelResult<()> {
        self.lock()?.push_back(item);
        Ok(())
    }

    fn pop(&self) -> KernelResult<Option<Y>> {
        Ok(self.lock()?.pop_front())
    }

    fn lock(&self) -> KernelResult<MutexGuard<'_, VecDeque<Y>>> {
        self.0
            .lock()
            .map_err(|_| KernelError::internal_error("generator yield queue lock poisoned"))
    }
}

impl<'a, P: Send + 'static, Y: Send> GeneratorChannel<'a, P, Y> {
    fn new(channel: &'a Channel<P>, yields: Arc<YieldQueue<Y>>) -> Self {
        Self { channel, yields }
    }

    /// Commit one item and suspend until the consumer drains all currently pending yields.
    #[internal_api]
    pub(crate) async fn yield_item(&self, item: Y) -> KernelResult<()> {
        let mut item = Some(item);
        poll_fn(|_| {
            let Some(item) = item.take() else {
                return Poll::Ready(Ok(()));
            };
            match self.yields.push(item) {
                Ok(()) => Poll::Pending,
                Err(err) => Poll::Ready(Err(err)),
            }
        })
        .await
    }

    /// Forward every item from `generator` and return its completion.
    #[internal_api]
    pub(crate) async fn yield_from<G, O: Send + 'static>(&self, mut generator: G) -> KernelResult<O>
    where
        G: BoundGenerator<Item = Y, Output = O> + Unpin,
    {
        loop {
            match poll_fn(|context| Pin::new(&mut generator).poll_step(context)).await? {
                GeneratorStep::Yield(item) => self.yield_item(item).await?,
                GeneratorStep::Done(output) => return Ok(output),
            }
        }
    }
}

/// Hidden expansion state for [`generator!`].
#[doc(hidden)]
#[internal_api]
pub(crate) struct BodyFn<F, P, Y, O> {
    body: F,
    span: Span,
    _type_signature: PhantomType<(P, Y, O)>,
}

impl<F, P: Send + 'static, Y: Send, O> BodyFn<F, P, Y, O> {
    /// Store `body` without choosing its call-future proof path.
    #[internal_api]
    pub(crate) fn new(body: F) -> Self
    where
        F: for<'channel> AsyncFnOnce(GeneratorChannel<'channel, P, Y>) -> KernelResult<O>,
    {
        Self {
            body,
            span: Span::current(),
            _type_signature: PhantomType::default(),
        }
    }

    /// Create a generator using the concrete call-future proof supplied by `witness`.
    #[internal_api]
    pub(crate) fn into_generator_with<'generator, C, Fut>(
        self,
        channel: C,
        witness: impl FnOnce(Self, C, Arc<YieldQueue<Y>>) -> Fut,
    ) -> GeneratorBody<'generator, Y, O>
    where
        C: Deref<Target = Channel<P>> + Send + 'generator,
        Fut: KernelFuture<O> + 'generator,
        Y: 'generator,
        O: 'generator,
    {
        let witness = move |(this, channel), yields| witness(this, channel, yields);
        GeneratorBody::new_with_witness((self, channel), witness)
    }

    /// Run the body while exposing its concrete future to [`Self::into_generator_with`].
    #[internal_api]
    pub(crate) fn witness(
        self,
        channel: impl Deref<Target = Channel<P>> + Send,
        yields: Arc<YieldQueue<Y>>,
    ) -> impl Future<Output = KernelResult<O>>
    where
        F: for<'channel> AsyncFnOnce(GeneratorChannel<'channel, P, Y>) -> KernelResult<O>,
    {
        let future = async move {
            let channel = GeneratorChannel::new(&channel, yields);
            let output = (self.body)(channel).await;
            output.inspect_err(|err| error!(error = %err, "generator failed"))
        };
        future.instrument(self.span)
    }
}

mod adapters;
