//! Streaming generators and independently driven static wrappers.
//!
//! A streaming operation receives a [`Channel`] and returns a [`Generator`]. Code within a parent
//! coroutine awaits [`Generator::next`]; connector requests flow through the supplied channel.
//!
//! A caller without a parent channel uses [`StaticGenerator::new`] with owned inputs. The resulting
//! wrapper owns the request channel and a static generator. [`StaticGenerator::advance`] waits
//! asynchronously for a [`GeneratorStep`], while [`StaticGenerator::try_advance`] attempts to
//! produce one without waiting.

use std::borrow::Borrow;
use std::future::{poll_fn, Future};
use std::ops::AsyncFnOnce;
use std::sync::Arc;
use std::task::{Context, Poll};

use delta_kernel_derive::internal_api;
use derive_more::Deref;
use tracing::{error, Instrument as _, Span};

use super::core::{noop_context, Channel, ChannelExchange, DeltaFuture, Reply, Step, Task};
use crate::utils::PhantomType;
use crate::{DeltaResult, DeltaResultIteratorStatic, Error};

#[doc(hidden)]
#[internal_api]
pub(crate) type YieldRequest<Y> = (Y, Reply<()>);

/// Streaming operation that uses an existing request channel and yields items to its caller.
///
/// Connector requests surface through the driver that owns the supplied channel, while
/// [`Self::next`] returns yielded items and reports successful completion as exhaustion. The `'a`
/// lifetime covers the channel binding and any state borrowed by the operation.
///
/// Use [`StaticGenerator`] for an independently driven generator that owns its request channel and
/// captured state. It exposes requests, yielded items, and completion to its driver.
pub struct Generator<'a, Y: Send + 'static>(Task<'a, YieldRequest<Y>, ()>);

impl<'a, Y: Send + 'static> Generator<'a, Y> {
    /// Waits for the next item, returning `None` if the generator is exhausted.
    ///
    /// Connector requests made while producing the item flow through the generator's channel.
    ///
    /// Calling this method after receiving anything other than `Ok(Some(_))` is a logic error.
    pub async fn next(&mut self) -> DeltaResult<Option<Y>> {
        poll_fn(|context| self.poll_step(context, Some, None)).await
    }

    // Receive and acknowlege yields
    fn poll_step<R>(
        &mut self,
        context: &mut Context<'_>,
        map_yield: impl FnOnce(Y) -> R,
        done: R,
    ) -> Poll<DeltaResult<R>> {
        match self.0.poll_step(context) {
            Poll::Ready(Ok(Step::Request((item, reply)))) => {
                reply.send(Ok(()));
                Poll::Ready(Ok(map_yield(item)))
            }
            Poll::Ready(Ok(Step::Done(()))) => Poll::Ready(Ok(done)),
            Poll::Ready(Err(err)) => Poll::Ready(Err(err)),
            Poll::Pending => Poll::Pending,
        }
    }
}

/// Yield, request, or completion produced by advancing a [`StaticGenerator`].
///
/// Yielded items are acknowledged before this step returns. Requests carry their own replies.
pub enum GeneratorStep<P: Send + 'static, Y: Send + 'static> {
    /// A yielded item.
    Yield(Y),
    /// Connector work offered by the generator.
    Request(P),
    /// The generator completed.
    Done,
}

/// Independently driven generator with owned request state.
///
/// A static generator owns its request channel and a [`Generator<'static, Y>`].
pub struct StaticGenerator<P: Send + 'static, Y: Send + 'static> {
    requests: Arc<Channel<P>>,
    generator: Generator<'static, Y>,
}

impl<P: Send + 'static, Y: Send + 'static> StaticGenerator<P, Y> {
    /// Constructs a static generator around an infallible channel-taking operation.
    ///
    /// `make_generator` runs immediately with the owned request channel. The returned generator
    /// remains unpolled until [`Self::advance`] or [`Self::try_advance`] is called and must own all
    /// captured state.
    pub fn new(make_generator: impl FnOnce(Arc<Channel<P>>) -> Generator<'static, Y>) -> Self {
        let channel = Arc::new(Channel::default());
        Self {
            requests: Arc::clone(&channel),
            generator: make_generator(channel),
        }
    }

    /// Tries to advance to the next yielded item, request or completion without waiting.
    ///
    /// Polls the generator once and returns with [`Error::WouldBlock`] if no next step is
    /// immediately available. This happens if the generator is blocked on one or more outstanding
    /// [`GeneratorStep::Request`]; callers can unblock the generator by responding to
    /// previously-received requests by invoking their [`Reply::send`](super::Reply::send) (or
    /// dropping them) before calling this method again.
    ///
    /// Calling this method after receiving [`GeneratorStep::Done`] is a logic error.
    pub fn try_advance(&mut self) -> DeltaResult<GeneratorStep<P, Y>> {
        match self.poll_step(&mut noop_context()) {
            Poll::Ready(step) => step,
            Poll::Pending => Err(Error::WouldBlock),
        }
    }

    /// Advances to the next yielded item, request or completion, waiting asynchronously if the
    /// generator is blocked on one or more outstanding [`GeneratorStep::Request`]. Invoke the
    /// requests' [`Reply::send`](super::Reply::send) (or drop them) to unblock the generator.
    ///pop
    /// Calling this method after receiving [`GeneratorStep::Done`] is a logic error.
    pub async fn advance(&mut self) -> DeltaResult<GeneratorStep<P, Y>> {
        poll_fn(|context| self.poll_step(context)).await
    }

    // Poll the generator, falling back to the request channel if pending
    fn poll_step(&mut self, context: &mut Context<'_>) -> Poll<DeltaResult<GeneratorStep<P, Y>>> {
        let inner = &mut self.generator;
        match inner.poll_step(context, GeneratorStep::Yield, GeneratorStep::Done) {
            Poll::Pending => match self.requests.pop() {
                Some(request) => Poll::Ready(Ok(GeneratorStep::Request(request))),
                None => Poll::Pending,
            },
            ready => ready,
        }
    }
}

/// Creates a generator using an existing request channel.
///
/// The generator may borrow both its channel and caller state:
///
/// ```
/// use delta_kernel::coroutine::generator::{generator, Generator};
/// use delta_kernel::coroutine::Channel;
///
/// enum Request {}
///
/// fn lengths<'a>(
///     channel: &'a Channel<Request>,
///     value: &'a str,
/// ) -> Generator<'a, usize> {
///     generator!(channel, async move |channel| {
///         channel.yield_item(value.len()).await
///     })
/// }
/// ```
#[internal_api]
macro_rules! generator {
    ($channel:expr, $body:expr) => {
        $crate::coroutine::generator::GeneratorFn::<_, _, _>::new($body)
            .into_generator_with($channel, $crate::coroutine::generator::GeneratorFn::witness)
    };
}

/// Creates a lazy generator that owns its request channel and captured state.
///
/// ```
/// use delta_kernel::coroutine::generator::{static_generator, GeneratorStep};
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
/// let mut generator = static_generator!(Request, async move |channel| {
///     let length = channel.length("delta".to_string()).await?;
///     channel.yield_item(length).await
/// });
/// let GeneratorStep::Request(Request::Length(value, reply)) = generator.try_advance()? else {
///     unreachable!("generator did not request a length");
/// };
/// reply.send(Ok(value.len()));
/// let GeneratorStep::Yield(length) = generator.try_advance()? else {
///     unreachable!("generator did not yield");
/// };
/// assert_eq!(length, 5);
/// assert!(matches!(generator.try_advance()?, GeneratorStep::Done));
/// # Ok::<(), delta_kernel::Error>(())
/// ```
#[internal_api]
macro_rules! static_generator {
    ($request:ty, $body:expr) => {
        $crate::coroutine::generator::StaticGenerator::<$request, _>::new(move |channel| {
            $crate::coroutine::generator::generator!(channel, $body)
        })
    };
}

/// Creates a single-use generator without choosing its request channel.
///
/// The async body receives a [`GeneratorChannel`] when the returned [`UnboundGenerator`] is bound.
/// Use [`UnboundGenerator::bind`] to compose it with a parent channel, or
/// [`StaticGenerator::from`] to give it a new root channel. The body and its captured state must be
/// sendable and `'static` because they are stored until binding.
#[internal_api]
macro_rules! unbound_generator {
    ($body:expr) => {
        $crate::coroutine::generator::UnboundGenerator::from_binder(move |channel| {
            $crate::coroutine::generator::generator!(channel, $body)
        })
    };
}

impl<P: Send + 'static, Y: Send + 'static> StaticGenerator<P, Y> {
    /// Adapt a sendable iterator into a lazy generator.
    ///
    /// Advancing the generator polls `items` inline and propagates item errors.
    pub fn from_iterator(items: DeltaResultIteratorStatic<Y>) -> Self {
        static_generator!(P, async move |channel| {
            for item in items {
                channel.yield_item(item?).await?;
            }
            Ok(())
        })
    }
}

/// Generator access to connector requests and yielded output.
#[derive(Deref)]
#[internal_api]
pub(crate) struct GeneratorChannel<'a, P: Send + 'static, Y: Send + 'static> {
    #[deref]
    channel: &'a Channel<P>,
    yields: &'a Channel<YieldRequest<Y>>,
}

impl<'a, P: Send + 'static, Y: Send + 'static> GeneratorChannel<'a, P, Y> {
    fn new(channel: &'a Channel<P>, yields: &'a Channel<YieldRequest<Y>>) -> Self {
        Self { channel, yields }
    }

    /// Yield one item and suspend until the consumer receives it.
    #[internal_api]
    pub(crate) async fn yield_item(&self, item: Y) -> DeltaResult<()> {
        self.yields
            .exchange(item, |item, reply| (item, reply))
            .await
    }

    /// Forward every item from `generator`.
    pub(crate) async fn yield_from(&self, generator: UnboundGenerator<P, Y>) -> DeltaResult<()> {
        let mut generator = generator.bind(self.channel);
        while let Some(item) = generator.next().await? {
            self.yield_item(item).await?;
        }
        Ok(())
    }
}

/// Request-channel ownership supplied when binding an unbound generator.
pub(crate) enum ChannelBinding<'a, P: 'static> {
    Borrowed(&'a Channel<P>),
    Owned(Arc<Channel<P>>),
}

impl<P: 'static> Borrow<Channel<P>> for ChannelBinding<'_, P> {
    fn borrow(&self) -> &Channel<P> {
        match self {
            Self::Borrowed(channel) => channel,
            Self::Owned(channel) => channel.as_ref(),
        }
    }
}

/// Erased binder contract for an unbound generator body.
pub(crate) trait BindGeneratorFn<P: 'static, Y: Send + 'static>:
    for<'a> FnOnce(ChannelBinding<'a, P>) -> Generator<'a, Y>
{
}
impl<F, P: 'static, Y: Send + 'static> BindGeneratorFn<P, Y> for F where
    F: for<'a> FnOnce(ChannelBinding<'a, P>) -> Generator<'a, Y>
{
}

/// Unbound generator body factory whose concrete type has been erased.
pub struct UnboundGenerator<P: Send + 'static, Y: Send + 'static>(
    Box<dyn BindGeneratorFn<P, Y> + Send + 'static>,
);

impl<P: Send + 'static, Y: Send + 'static> UnboundGenerator<P, Y> {
    /// Erase a binder created while its generator body type is still concrete.
    pub(crate) fn from_binder(bind: impl BindGeneratorFn<P, Y> + Send + 'static) -> Self {
        Self(Box::new(bind))
    }

    /// Bind this unbound generator to `channel`.
    pub fn bind<'a>(self, channel: &'a Channel<P>) -> Generator<'a, Y> {
        (self.0)(ChannelBinding::Borrowed(channel))
    }
}

impl<P: Send + 'static, Y: Send + 'static> From<UnboundGenerator<P, Y>> for StaticGenerator<P, Y> {
    fn from(generator: UnboundGenerator<P, Y>) -> Self {
        StaticGenerator::new(move |channel| (generator.0)(ChannelBinding::Owned(channel)))
    }
}

/// Hidden expansion state for [`generator!`] and [`static_generator!`].
///
/// Stores an uninvoked async function and the current tracing span until the macro constructs a
/// [`Generator`]. This type is public only so exported macros can name it.
#[doc(hidden)]
#[internal_api]
pub(crate) struct GeneratorFn<F, P, Y> {
    body: F,
    span: Span,
    _type_signature: PhantomType<(P, Y)>,
}

// `Self::new` and `Self::witness` constrain `F` to be callable with a `GeneratorChannel` of any
// `'channel` lifetime. That lifetime is local to the higher-ranked call; the `C: 'generator` bound
// keeps the request-channel binding valid, and the generator owns its yield channel and resulting
// future. The future can therefore retain both channel references while suspended without
// imposing another external lifetime requirement.
//
// `Self::into_generator_with` passes `self`, `channel`, and `Self::witness` to
// `Task::new_with_witness`; the latter's doc comment explains why we must pass the witness
// separately. The returned future is instrumented with the captured span. The `unbound_generator!`
// macro erases a higher-ranked binder built with the witness instead of erasing `F` directly.
impl<F, P: Send + 'static, Y: Send + 'static> GeneratorFn<F, P, Y> {
    /// Stores `body` without choosing its call-future proof path.
    #[internal_api]
    pub(crate) fn new(body: F) -> Self
    where
        F: for<'channel> AsyncFnOnce(GeneratorChannel<'channel, P, Y>) -> DeltaResult<()>,
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
        witness: impl FnOnce(Self, C, Arc<Channel<YieldRequest<Y>>>) -> Fut,
    ) -> Generator<'generator, Y>
    where
        C: Borrow<Channel<P>> + Send + 'generator,
        Fut: DeltaFuture<()> + 'generator,
    {
        let witness = move |(this, channel), yields| witness(this, channel, yields);
        Generator(Task::new_with_witness((self, channel), witness))
    }

    /// Runs the body while exposing its concrete future to [`Self::into_generator_with`].
    #[internal_api]
    pub(crate) fn witness(
        self,
        channel: impl Borrow<Channel<P>> + Send,
        yields: Arc<Channel<YieldRequest<Y>>>,
    ) -> impl Future<Output = DeltaResult<()>>
    where
        F: for<'channel> AsyncFnOnce(GeneratorChannel<'channel, P, Y>) -> DeltaResult<()>,
    {
        let future = async move {
            let channel = GeneratorChannel::new(channel.borrow(), yields.as_ref());
            let output = (self.body)(channel).await;
            output.inspect_err(|err| error!(error = %err, "coroutine generator failed"))
        };
        future.instrument(self.span)
    }
}
