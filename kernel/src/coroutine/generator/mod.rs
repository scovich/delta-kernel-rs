//! Streaming generators: unbound pipelines, bound consumers, and independently driven wrappers.
//!
//! # Unbound pipeline
//!
//! Kernel functions return [`Generator`]. Combinators on [`GeneratorExt`] run before a request
//! [`Channel`] exists: [`GeneratorExt::map`] transforms items, [`GeneratorExt::map_ok`] transforms
//! successful result items, [`GeneratorExt::chain`] concatenates two generators, [`empty`] yields
//! nothing, and [`AsyncIntoIterator::into_generator`] wraps an iterable. [`unfold`] and
//! [`unfold_mut`] build a bound generator from owned state and a step future. Item-transforming
//! combinators preserve terminal output; consuming helpers such as [`BoundGenerator::next`] require
//! `Output = ()`.
//! [`generator_fn!`] builds an erased pipeline from an async body that later receives its channel.
//!
//! # Binding
//!
//! [`Generator::bind`] attaches a borrowed parent channel and produces a [`BoundGenerator`].
//! [`BoundGenerator::poll_step`] returns [`GeneratorStep::Yield`] or [`GeneratorStep::Done`].
//! Connector requests posted by the body surface on that parent channel, not in `GeneratorStep`.
//! [`BoundGenerator::next`] awaits items until `Output = ()` completion looks like exhaustion.
//!
//! [`generator!`] binds immediately: it takes a channel and an async body and returns a
//! [`GeneratorBody`], the [`BoundGenerator`] used inside a parent coroutine. Await
//! [`GeneratorBody::next`] (or [`BoundGenerator::next`]) for items; requests stay on the supplied
//! channel.
//!
//! # Independent driver
//!
//! With no parent channel, [`generator_coroutine!`] (or [`static_generator_coroutine!`]) constructs
//! a [`GeneratorCoroutine`] that owns its channel. [`GeneratorCoroutine::advance`] and
//! [`GeneratorCoroutine::try_advance`] return [`GeneratorCoroutineStep`]: a yield, a request, or
//! completion. [`StaticGeneratorCoroutine`] is the `'static` alias. A
//! `Box<dyn Generator<...>>` converts into that wrapper via [`From`].
//!
//! Adapter chains bind in place: no extra allocation or yield handshake per item. Polling after an
//! error or completion is a logic error.
//!
//! [`GeneratorCoroutine`]: generator::GeneratorCoroutine
//! [`GeneratorCoroutine::new`]: generator::GeneratorCoroutine::new
//! [`GeneratorCoroutine::advance`]: generator::GeneratorCoroutine::advance
//! [`GeneratorCoroutine::try_advance`]: generator::GeneratorCoroutine::try_advance
//! [`GeneratorCoroutineStep`]: generator::GeneratorCoroutineStep
//! [`StaticGeneratorCoroutine`]: generator::StaticGeneratorCoroutine
//! [`AsyncIntoIterator::into_generator`]: generator::AsyncIntoIterator::into_generator
//! [`empty`]: generator::empty
//! [`unfold`]: generator::unfold
//! [`unfold_mut`]: generator::unfold_mut
//! [`generator_fn!`]: generator::generator_fn
//! [`generator!`]: generator::generator
//! [`generator_coroutine!`]: generator::generator_coroutine
//! [`static_generator_coroutine!`]: generator::static_generator_coroutine

use std::collections::VecDeque;
use std::future::{poll_fn, Future};
use std::ops::{AsyncFnOnce, Deref};
use std::pin::Pin;
use std::sync::{Arc, Mutex, MutexGuard};
use std::task::{Context, Poll};

use delta_kernel_derive::internal_api;
use tracing::{error, Instrument as _, Span};

pub use self::adapters::{
    empty, map_async_witness, unfold, AsyncIntoIterator, BoundGenerator, BoxedBoundGenerator,
    BoxedDeltaResultGenerator, Chain, FilterMap, FilterOk, Flatten, FlattenBy, Generator,
    GeneratorExt, GeneratorStep, Iter, Map, MapAsync, MapAsyncWitness, MapOk, OnComplete, Peekable,
    Unfold,
};
pub(crate) use self::adapters::{unfold_mut_with, unfold_mut_witness};
use super::core::{noop_context, Channel, DeltaFuture};
use super::invoke_witness;
use crate::utils::PhantomType;
use crate::{DeltaResult, DeltaResultIteratorStatic, Error};

mod adapters;

/// Streaming coroutine that yields zero or more items to its caller before producing a final value.
///
/// Connector requests surface through the driver that owns the supplied channel, while
/// [`Self::next`] returns yielded items and reports successful completion as exhaustion. The `'a`
/// lifetime covers the channel binding and any state borrowed by the operation.
///
/// Use [`GeneratorCoroutine`] for an independently driven generator that exposes requests, yielded
/// items, and completion to its driver.
pub struct GeneratorBody<'a, Y: Send + 'a, O = ()> {
    body: Option<Pin<Box<dyn DeltaFuture<O> + 'a>>>,
    yields: Arc<YieldQueue<Y>>,
    terminal: Option<DeltaResult<O>>,
}

// The body has its own stable heap address; no other field is structurally pinned.
impl<'a, Y: Send + 'a, O> Unpin for GeneratorBody<'a, Y, O> {}

impl<'a, Y: Send + 'a> GeneratorBody<'a, Y> {
    /// Waits for the next item, returning `None` if the generator is exhausted.
    ///
    /// Connector requests made while producing the item flow through the generator's channel.
    ///
    /// Calling this method after receiving anything other than `Ok(Some(_))` is a logic error.
    pub async fn next(&mut self) -> DeltaResult<Option<Y>> {
        BoundGenerator::next(self).await
    }
}

impl<'a, Y: Send + 'a, O> GeneratorBody<'a, Y, O> {
    fn new_with_witness<State, F, Fut>(state: State, witness: F) -> Self
    where
        F: FnOnce(State, Arc<YieldQueue<Y>>) -> Fut,
        Fut: DeltaFuture<O> + 'a,
    {
        let yields = Arc::default();
        Self {
            body: Some(Box::pin(invoke_witness(witness, state, Arc::clone(&yields))) as _),
            yields,
            terminal: None,
        }
    }

    fn take_ready_step(&mut self) -> Option<DeltaResult<GeneratorStep<Y, O>>> {
        if let Some(result) = self.yields.pop().transpose() {
            return Some(result.map(GeneratorStep::Yield));
        }
        Some(self.terminal.take()?.map(GeneratorStep::Done))
    }

    fn poll_step(&mut self, context: &mut Context<'_>) -> Poll<DeltaResult<GeneratorStep<Y, O>>> {
        if let Some(result) = self.take_ready_step() {
            return Poll::Ready(result);
        }

        let Some(body) = self.body.as_mut() else {
            return Poll::Ready(Err(Error::internal_error(
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

/// Yield, request, or completion produced by advancing a [`GeneratorCoroutine`].
///
/// Yielded items are removed from the pending queue before this step returns. Requests carry their
/// own replies.
pub enum GeneratorCoroutineStep<P: Send + 'static, Y: Send, O: Send + 'static = ()> {
    /// A yielded item.
    Yield(Y),
    /// Connector work offered by the generator.
    Request(P),
    /// The generator completed.
    Done(O),
}

/// Independently driven generator with owned request state.
///
/// The generator owns its request channel and may borrow captured operation state for `'a`.
pub struct GeneratorCoroutine<'a, P: Send + 'static, Y: Send + 'a, O: Send + 'static = ()> {
    requests: Arc<Channel<P>>,
    generator: BoxedBoundGenerator<'a, Y, O>,
}

/// Independently driven generator that owns all captured state.
pub type StaticGeneratorCoroutine<P, Y, O = ()> = GeneratorCoroutine<'static, P, Y, O>;

impl<'a, P: Send + 'static, Y: Send + 'a, O: Send + 'static> GeneratorCoroutine<'a, P, Y, O> {
    /// Constructs a generator around an infallible channel-taking operation.
    ///
    /// `make_generator` runs immediately with the owned request channel. The returned generator
    /// remains unpolled until [`Self::advance`] or [`Self::try_advance`] is called.
    pub fn new(make_generator: impl FnOnce(Arc<Channel<P>>) -> GeneratorBody<'a, Y, O>) -> Self {
        let channel = Arc::new(Channel::default());
        Self {
            requests: Arc::clone(&channel),
            generator: Box::new(make_generator(channel)),
        }
    }

    /// Tries to advance to the next yielded item, request or completion without waiting.
    ///
    /// Polls the generator once and returns with [`Error::WouldBlock`] if no next step is
    /// immediately available. This happens if the generator is blocked on one or more outstanding
    /// [`GeneratorCoroutineStep::Request`]; callers can unblock the generator by responding to
    /// previously-received requests by invoking their [`Reply::send`](super::Reply::send) (or
    /// dropping them) before calling this method again.
    ///
    /// Calling this method after receiving [`GeneratorCoroutineStep::Done`] is a logic error.
    pub fn try_advance(&mut self) -> DeltaResult<GeneratorCoroutineStep<P, Y, O>> {
        match self.poll_step(&mut noop_context()) {
            Poll::Ready(step) => step,
            Poll::Pending => Err(Error::WouldBlock),
        }
    }

    /// Advances to the next yielded item, request or completion, waiting asynchronously if the
    /// generator is blocked on one or more outstanding [`GeneratorCoroutineStep::Request`]. Invoke
    /// the requests' [`Reply::send`](super::Reply::send) (or drop them) to unblock the
    /// generator.
    ///
    /// Calling this method after receiving [`GeneratorCoroutineStep::Done`] is a logic error.
    pub async fn advance(&mut self) -> DeltaResult<GeneratorCoroutineStep<P, Y, O>> {
        poll_fn(|context| self.poll_step(context)).await
    }

    // Poll the generator, falling back to the request channel if pending
    fn poll_step(
        &mut self,
        context: &mut Context<'_>,
    ) -> Poll<DeltaResult<GeneratorCoroutineStep<P, Y, O>>> {
        match Pin::new(&mut *self.generator).poll_step(context) {
            Poll::Ready(Ok(GeneratorStep::Yield(item))) => {
                Poll::Ready(Ok(GeneratorCoroutineStep::Yield(item)))
            }
            Poll::Ready(Ok(GeneratorStep::Done(output))) => {
                Poll::Ready(Ok(GeneratorCoroutineStep::Done(output)))
            }
            Poll::Ready(Err(err)) => Poll::Ready(Err(err)),
            Poll::Pending => match self.requests.pop() {
                Some(request) => Poll::Ready(Ok(GeneratorCoroutineStep::Request(request))),
                None => Poll::Pending,
            },
        }
    }
}

/// Creates a generator coroutine using an existing request channel.
///
/// The generator may borrow both its channel and caller state:
///
/// ```
/// use delta_kernel::coroutine::generator::{generator, GeneratorBody};
/// use delta_kernel::coroutine::Channel;
///
/// enum Request {}
///
/// fn lengths<'a>(
///     channel: &'a Channel<Request>,
///     value: &'a str,
/// ) -> GeneratorBody<'a, usize> {
///     generator!(channel, async move |channel| {
///         channel.yield_item(value.len()).await
///     })
/// }
/// ```
#[internal_api]
macro_rules! generator {
    ($channel:expr, $body:expr) => {
        $crate::coroutine::generator::BodyFn::<_, _, _, _>::new($body)
            .into_generator_with($channel, $crate::coroutine::generator::BodyFn::witness)
    };
}

/// Creates a [`GeneratorCoroutine`] for a specific request vocabulary.
///
/// The coroutine owns its request channel but may borrow captured state.
///
/// The request type determines the concrete [`Channel`] received by the body. A vocabulary can
/// add typed operations with an extension trait over that channel:
///
/// ```
/// use delta_kernel::coroutine::generator::{generator_coroutine, GeneratorCoroutineStep};
/// use delta_kernel::coroutine::{Channel, ChannelExchange, DeltaFuture, Reply};
///
/// enum Request {
///     Length(String, Reply<usize>),
/// }
///
/// trait ChannelExt: ChannelExchange<Request> {
///     fn length(&self, value: String) -> impl DeltaFuture<usize> {
///         self.exchange(value, Request::Length)
///     }
/// }
///
/// impl ChannelExt for Channel<Request> {}
///
/// let mut generator = generator_coroutine!(Request, async move |channel| {
///     let length = channel.length("delta".to_string()).await?;
///     channel.yield_item(length).await
/// });
/// let GeneratorCoroutineStep::Request(Request::Length(value, reply)) = generator.try_advance()? else {
///     unreachable!("generator did not request a length");
/// };
/// reply.send(Ok(value.len()));
/// let GeneratorCoroutineStep::Yield(length) = generator.try_advance()? else {
///     unreachable!("generator did not yield");
/// };
/// assert_eq!(length, 5);
/// assert!(matches!(generator.try_advance()?, GeneratorCoroutineStep::Done(())));
/// # Ok::<(), delta_kernel::Error>(())
/// ```
#[internal_api]
macro_rules! generator_coroutine {
    ($request:ty, $body:expr) => {
        $crate::coroutine::generator::GeneratorCoroutine::<$request, _, _>::new(move |channel| {
            $crate::coroutine::generator::generator!(channel, $body)
        })
    };
}

/// Creates a [`StaticGeneratorCoroutine`] that owns all captured state.
///
/// ```
/// use delta_kernel::coroutine::generator::{static_generator_coroutine, StaticGeneratorCoroutine};
///
/// enum Request {}
///
/// let generator: StaticGeneratorCoroutine<Request, ()> =
///     static_generator_coroutine!(Request, async move |_channel| Ok(()));
/// ```
#[internal_api]
macro_rules! static_generator_coroutine {
    ($request:ty, $body:expr) => {
        $crate::coroutine::generator::generator_coroutine!($request, $body)
    };
}

/// Creates an [`Generator`] from an async closure.
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
///
/// Supplies the [`Send`] witness the same way [`generator!`] does. Each step boxes the
/// compiler-generated future so the returned generator is [`Unpin`].
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

impl<'a, P: Send + 'static, Y: Send + 'a, O: Send + 'static> GeneratorCoroutine<'a, P, Y, O> {
    /// Bind an unbound generator to an owned request channel.
    pub fn from_generator(generator: impl Generator<P, Item = Y, Output = O> + 'a) -> Self {
        let requests = Arc::new(Channel::default());
        let generator = generator.bind_to(ChannelBinding::Owned(Arc::clone(&requests)));
        Self {
            generator: Box::new(generator),
            requests,
        }
    }
}

impl<P: Send + 'static, Y: Send + 'static> StaticGeneratorCoroutine<P, Y> {
    /// Adapt a sendable iterator into a lazy generator.
    ///
    /// Advancing the generator polls `items` inline and propagates item errors.
    pub fn from_iterator(items: DeltaResultIteratorStatic<Y>) -> Self {
        static_generator_coroutine!(P, async move |channel| {
            for item in items {
                channel.yield_item(item?).await?;
            }
            Ok(())
        })
    }
}

/// Request-channel ownership supplied when binding a generator.
pub enum ChannelBinding<'a, P: 'static> {
    /// Bind to a parent coroutine's request channel.
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
    /// Stores `bind` without invoking it.
    #[internal_api]
    pub(crate) fn new(bind: F) -> Self {
        Self {
            bind,
            _type_signature: PhantomType::default(),
        }
    }
}

/// Erases a channel binder as `Box<dyn Generator<...>>`.
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

impl<'a, P: Send + 'static, Y: Send + 'a, O: Send + 'static>
    From<Box<dyn Generator<P, Item = Y, Output = O> + 'a>> for GeneratorCoroutine<'a, P, Y, O>
{
    fn from(generator: Box<dyn Generator<P, Item = Y, Output = O> + 'a>) -> Self {
        let requests = Arc::new(Channel::default());
        Self {
            generator: generator.bind_boxed(ChannelBinding::Owned(Arc::clone(&requests))),
            requests,
        }
    }
}

/// Generator access to connector requests and yielded output.
#[derive(derive_more::Deref)]
#[internal_api]
pub(crate) struct GeneratorChannel<'a, P: Send + 'static, Y: Send> {
    #[deref]
    channel: &'a Channel<P>,
    yields: Arc<YieldQueue<Y>>,
}

/// Queue of items committed by generator yield futures.
#[doc(hidden)]
#[internal_api]
pub(crate) struct YieldQueue<Y>(Mutex<VecDeque<Y>>);

impl<Y> Default for YieldQueue<Y> {
    fn default() -> Self {
        Self(Mutex::default())
    }
}

impl<Y> YieldQueue<Y> {
    fn push(&self, item: Y) -> DeltaResult<()> {
        self.lock()?.push_back(item);
        Ok(())
    }

    fn pop(&self) -> DeltaResult<Option<Y>> {
        Ok(self.lock()?.pop_front())
    }

    fn lock(&self) -> DeltaResult<MutexGuard<'_, VecDeque<Y>>> {
        self.0
            .lock()
            .map_err(|_| Error::internal_error("generator yield queue lock poisoned"))
    }
}

impl<'a, P: Send + 'static, Y: Send> GeneratorChannel<'a, P, Y> {
    fn new(channel: &'a Channel<P>, yields: Arc<YieldQueue<Y>>) -> Self {
        Self { channel, yields }
    }

    /// Commit one item and suspend until the consumer drains all currently pending yields.
    #[internal_api]
    pub(crate) async fn yield_item(&self, item: Y) -> DeltaResult<()> {
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

    /// Forward every item from a bound `generator` and return its completion.
    pub(crate) async fn yield_from<G, O: Send + 'static>(&self, mut generator: G) -> DeltaResult<O>
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

/// Hidden expansion state for [`generator!`], [`generator_coroutine!`], and
/// [`static_generator_coroutine!`].
///
/// Stores an uninvoked async function and the current tracing span until the macro constructs a
/// [`GeneratorBody`]. This type is public only so exported macros can name it.
#[doc(hidden)]
#[internal_api]
pub(crate) struct BodyFn<F, P, Y, O> {
    body: F,
    span: Span,
    _type_signature: PhantomType<(P, Y, O)>,
}

// `Self::new` and `Self::witness` constrain `F` to be callable with a `GeneratorChannel` of any
// `'channel` lifetime. That lifetime is local to the higher-ranked call; the `C: 'generator` bound
// keeps the request-channel binding valid, and the generator owns its yield queue and resulting
// future. The future can therefore retain both channel references while suspended without
// imposing another external lifetime requirement.
//
// `Self::into_generator_with` passes `self`, `channel`, and `Self::witness` to
// `GeneratorBody::new_with_witness`; [`invoke_witness`] explains why the witness must remain
// separate. The returned future is instrumented with the captured span.
impl<F, P: Send + 'static, Y: Send, O> BodyFn<F, P, Y, O> {
    /// Stores `body` without choosing its call-future proof path.
    #[internal_api]
    pub(crate) fn new(body: F) -> Self
    where
        F: for<'channel> AsyncFnOnce(GeneratorChannel<'channel, P, Y>) -> DeltaResult<O>,
    {
        Self {
            body,
            span: Span::current(),
            _type_signature: PhantomType::default(),
        }
    }

    /// Creates a generator using the concrete call-future proof supplied by `witness`.
    #[internal_api]
    pub(crate) fn into_generator_with<'generator, C, Fut>(
        self,
        channel: C,
        witness: impl FnOnce(Self, C, Arc<YieldQueue<Y>>) -> Fut,
    ) -> GeneratorBody<'generator, Y, O>
    where
        C: Deref<Target = Channel<P>> + Send + 'generator,
        Fut: DeltaFuture<O> + 'generator,
        Y: 'generator,
        O: 'generator,
    {
        let witness = move |(this, channel), yields| witness(this, channel, yields);
        GeneratorBody::new_with_witness((self, channel), witness)
    }

    /// Runs the body while exposing its concrete future to [`Self::into_generator_with`].
    #[internal_api]
    pub(crate) fn witness(
        self,
        channel: impl Deref<Target = Channel<P>> + Send,
        yields: Arc<YieldQueue<Y>>,
    ) -> impl Future<Output = DeltaResult<O>>
    where
        F: for<'channel> AsyncFnOnce(GeneratorChannel<'channel, P, Y>) -> DeltaResult<O>,
    {
        let future = async move {
            let channel = GeneratorChannel::new(&channel, yields);
            let output = (self.body)(channel).await;
            output.inspect_err(|err| error!(error = %err, "coroutine generator failed"))
        };
        future.instrument(self.span)
    }
}
