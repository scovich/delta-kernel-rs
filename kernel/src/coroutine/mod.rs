//! Runtime-neutral async protocol logic with connector-owned execution.
//!
//! Kernel protocol operations are ordinary Rust futures. Futures let Kernel suspend, but that alone
//! does not avoid control inversion. Merely making connector callbacks async would still require
//! Kernel to invoke connector code through kernel-defined interfaces the connector must implement.
//!
//! A Kernel coroutine instead combines a future with a typed [`Channel`]. When Kernel needs
//! connector work, it posts a [`Request`] through that channel and awaits its [`Reply`]. The
//! request surfaces to the connector as an output of driving the coroutine. Each request carries a
//! single-use reply tied to its originating await point. Sending or dropping the reply makes that
//! point ready and wakes an async driver, but only the connector-owned driver decides when to poll
//! the coroutine again.
//!
//! The futures that back Kernel's coroutines are runtime neutral. A synchronous driver manually
//! polls them with [`Coroutine::try_advance`] and requires no async runtime at all. An async driver
//! uses [`Coroutine::advance`] so its own executor can park and wake the connector as it awaits the
//! coroutine which in turn awaits the connector's reply from a different task.
//!
//! This module provides two coroutine forms. Both may issue zero or more requests. A [`Coroutine`]
//! completes with one final output while a [`generator::GeneratorCoroutine`] interleaves requests
//! with a stream of yielded items before completing.
//!
//! ```text
//!   connector              kernel operation
//!       |                          |
//!       |----- advance/poll ------>|
//!       |<------- Request ---------|
//!       |                     (suspended)
//!  (handle request)                |
//!       |------ Reply::send ------>|
//!       |       (wake async driver)|
//!       |----- advance/poll ------>|
//!       |<------ output -----------|
//! ```
//!
//! Because the channel surfaces every connector operation, the driver may handle requests
//! synchronously, await them one at a time, schedule them concurrently, or even move them across a
//! process or machine boundary for remote execution. A reply refers weakly to the future waiting
//! for it, so outstanding replies do not keep a coroutine alive. Dropping the coroutine or
//! generator abandons the operation, and subsequent replies to any of its outstanding requests are
//! silently discarded.
//!
//! # Driver policies
//!
//! A synchronous driver handles each request, sends its reply, and calls [`Coroutine::try_advance`]
//! again. Kernel's internal async machinery remains hidden and no async runtime is required.
//!
//! An async driver may await each request before awaiting [`Coroutine::advance`] again, or schedule
//! requests concurrently and immediately await another step. In both cases the connector's executor
//! polls Kernel's futures and the connector remains responsible for scheduling and executing every
//! request.
//!
//! Engine-compatible entry points use a synchronous driver that translates each request into calls
//! to [`crate::Engine`] handlers. Connectors that choose these entry points intentionally preserve
//! the legacy Kernel-to-Engine callback model.
//!
//! # Choosing the ownership boundary
//!
//! A [`Coroutine`] already owns its request channel. For a streaming operation without a parent,
//! pass owned inputs through [`generator::StaticGeneratorCoroutine::new`]. Its callback must return
//! `BoundGenerator<'static, _>`, statically preventing borrowed state from escaping.
//!
//! Kernel code that already has a [`kernel::Channel`] calls async implementations directly. Passing
//! that channel to a streaming operation returns a [`BoundGenerator`] whose lifetime also covers
//! any other borrowed inputs. Requests then surface from the parent coroutine. Kernel may compose
//! channel-provided futures with runtime-neutral future and stream combinators; every operation
//! requiring connector work still passes through the channel.
//!
//! # Driving operations
//!
//! A driver waits for [`CoroutineStep::Request`] or [`CoroutineStep::Done`].
//! [`drive_coroutine!`] expands a one-request-at-a-time loop around [`Coroutine::try_advance`].
//! [`drive_coroutine_async!`] does the same with [`Coroutine::advance`], expanding the handler in
//! the caller's async context.
//!
//! # Pagination
//!
//! Operations that may produce large or unbounded results, such as file reads or query output, use
//! a paginated request model. Kernel consumes a logical stream of results while the connector
//! controls in-flight memory and can terminate the operation if resource limits are hit.
//!
//! Paged operations have three phases:
//!
//! - [`PageRequest::Start`] initializes the operation and returns its first [`Page`] of output.
//! - [`PageRequest::Continue`] consumes a cursor and returns the next page, with `Some(cursor)`
//!   when more pages remain.
//! - [`PageRequest::Prepare`] initializes an operation and returns a [`Cursor`] without producing a
//!   first page. This lets the connector begin work in the background while waiting for the first
//!   [`PageRequest::Continue`] from kernel.
//!
//! Every response to a pagination request carries the requested data and an optional [`Cursor`] for
//! continuing. `None` ends pagination; an empty page with a live cursor does not. Connectors choose
//! page boundaries and cursor representation; cursor payloads are opaque to kernel, forwarded
//! blindly back to connector with each continuation request (or dropped if kernel abandoned the
//! operation).
//!
//! # Error handling
//!
//! Replying with `Err` delivers a connector failure to the suspended Kernel await point. Calling an
//! advance method after its terminal step is a logic error. Synchronous `try_advance` returns
//! [`Error::WouldBlock`] if Kernel suspends without producing a step, which can only happen if
//! there are outstanding replies.
use std::any::{type_name, Any};
use std::future::{poll_fn, Future};
use std::mem::ManuallyDrop;
use std::ops::AsyncFnOnce;
use std::sync::Arc;
use std::task::Poll;

use delta_kernel_derive::{internal_api, pub_macro};
use derive_more::Constructor;
use tracing::{error, Instrument as _, Span};

pub use self::core::Reply;
use self::core::{noop_context, Step, Task};
#[doc(inline)]
#[internal_api]
pub(crate) use self::core::{Channel, ChannelExchange, DeltaFuture};
use self::generator::{unfold_mut, ChannelBinding};
pub use self::generator::{
    BoundGenerator, BoxedDeltaResultGenerator, BoxedGenerator, Flatten, Generator, GeneratorExt,
    GeneratorStep, IntoGenerator,
};
#[doc(inline)]
#[internal_api]
pub(crate) use self::kernel::ChannelExt;
pub use self::kernel::{DeltaResultGenerator, PlanRequest, Request};
#[cfg(feature = "declarative-plans")]
pub use crate::plans::Operation as PlanOperation;
use crate::utils::PhantomType;
use crate::{DeltaResult, Error};

/// Uninhabited plan operation used when declarative plans are disabled.
///
/// The placeholder keeps [`Request`]'s variant set independent of Cargo feature unification. A
/// dependency can enable `delta_kernel/declarative-plans` without enabling a connector crate's
/// corresponding feature, so cfg-gating `Request::ExecutePlan` would make exhaustive connector
/// matches depend on features selected elsewhere in the dependency graph.
#[cfg(not(feature = "declarative-plans"))]
pub enum PlanOperation {}

mod async_ext;
mod core;
pub(crate) mod engine;
pub(crate) use self::async_ext::{AsyncBool, AsyncOption};
// cbindgen ignores cfg gates on module declarations. Ignore both arms so coroutine-only generic
// aliases are not registered or monomorphized into the C API.
#[cfg(feature = "internal-api")]
/// cbindgen:ignore
pub mod generator;
#[cfg(not(feature = "internal-api"))]
/// cbindgen:ignore
#[allow(unreachable_pub)]
pub(crate) mod generator;
#[cfg(feature = "internal-api")]
/// cbindgen:ignore
pub mod kernel;
#[cfg(not(feature = "internal-api"))]
/// cbindgen:ignore
#[allow(unreachable_pub)]
pub(crate) mod kernel;

/// Independently driven operation that may request connector work before producing a final output.
///
/// The coroutine owns its request channel. [`Self::advance`] and [`Self::try_advance`] return each
/// request as [`CoroutineStep::Request`] and successful completion as [`CoroutineStep::Done`]. The
/// `'a` lifetime covers any state borrowed by the operation; [`StaticCoroutine`] owns all captured
/// state.
pub struct Coroutine<'a, P: Send + 'static, O>(Task<'a, P, O>);

/// A coroutine that owns all captured state.
pub type StaticCoroutine<P, O> = Coroutine<'static, P, O>;

/// Request or completion produced by advancing a [`Coroutine`].
// The request variant is the common case, and boxing `P` would add an allocation to every request.
#[allow(clippy::large_enum_variant)]
pub enum CoroutineStep<P, O> {
    /// Connector work offered by the coroutine.
    Request(P),
    /// Terminal output.
    Done(O),
}

impl<'a, P: Send + 'static, O> Coroutine<'a, P, O> {
    /// Tries to advance to the next request or completion without waiting.
    ///
    /// Polls the coroutine once and returns with [`Error::WouldBlock`] if no next step is
    /// immediately available. This happens if the coroutine is blocked on one or more outstanding
    /// [`CoroutineStep::Request`]; callers can unblock the coroutine by invoking [`Reply::send`]
    /// (or dropping the `Reply`) for previously-received requests before calling this method again.
    ///
    /// Calling this method after receiving [`CoroutineStep::Done`] is a logic error.
    pub fn try_advance(&mut self) -> DeltaResult<CoroutineStep<P, O>> {
        let Poll::Ready(result) = self.0.poll_step(&mut noop_context()) else {
            return Err(Error::WouldBlock);
        };
        match result? {
            Step::Done(output) => Ok(CoroutineStep::Done(output)),
            Step::Request(request) => Ok(CoroutineStep::Request(request)),
        }
    }

    /// Advances to the next request or completion, waiting asynchronously if the coroutine is
    /// blocked on one or more outstanding [`CoroutineStep::Request`]. Invoke the requests'
    /// [`Reply::send`] (or drop them) to unblock the coroutine.
    ///
    /// Calling this method after receiving [`CoroutineStep::Done`] is a logic error.
    pub async fn advance(&mut self) -> DeltaResult<CoroutineStep<P, O>> {
        poll_fn(|context| match self.0.poll_step(context) {
            Poll::Ready(Ok(Step::Done(output))) => Poll::Ready(Ok(CoroutineStep::Done(output))),
            Poll::Ready(Ok(Step::Request(request))) => {
                Poll::Ready(Ok(CoroutineStep::Request(request)))
            }
            Poll::Ready(Err(err)) => Poll::Ready(Err(err)),
            Poll::Pending => Poll::Pending,
        })
        .await
    }
}

/// Creates a coroutine for a specific request vocabulary.
///
/// The coroutine owns its request channel but may borrow captured state.
///
/// The request type determines the concrete [`Channel`] received by the body. A vocabulary can
/// add typed operations with an extension trait over that channel:
///
/// ```
/// use delta_kernel::coroutine::{coroutine, Channel, ChannelExchange, CoroutineStep, DeltaFuture, Reply};
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
/// let mut coroutine = coroutine!(Request, async move |channel| {
///     let length = channel.length("delta".to_string()).await?;
///     Ok(length * 2)
/// });
/// let CoroutineStep::Request(Request::Length(value, reply)) = coroutine.try_advance()? else {
///     unreachable!("coroutine did not request a length");
/// };
/// reply.send(Ok(value.len()));
/// let CoroutineStep::Done(output) = coroutine.try_advance()? else {
///     unreachable!("coroutine did not complete");
/// };
/// assert_eq!(output, 10);
/// # Ok::<(), delta_kernel::Error>(())
/// ```
#[internal_api]
macro_rules! coroutine {
    ($request:ty, $body:expr) => {
        $crate::coroutine::BodyFn::<_, $request, _>::new($body)
            .into_coroutine_with($crate::coroutine::BodyFn::witness)
    };
}

/// Creates a coroutine that owns all captured state.
///
/// ```
/// use delta_kernel::coroutine::{static_coroutine, StaticCoroutine};
///
/// enum Request {}
///
/// let coroutine: StaticCoroutine<Request, ()> =
///     static_coroutine!(Request, async move |_channel| Ok(()));
/// ```
#[internal_api]
macro_rules! static_coroutine {
    ($request:ty, $body:expr) => {
        $crate::coroutine::BodyFn::<_, $request, _>::new($body)
            .into_coroutine_with::<'static, _>($crate::coroutine::BodyFn::witness)
    };
}

/// Drives a coroutine to completion using a caller-provided request handler.
///
/// The handler expression runs once for each [`CoroutineStep::Request`] and must produce `()`.
/// [`CoroutineStep::Done`] returns the coroutine output.
///
/// The handler is expanded in the caller's context, so it may freely use `?`, `.await`, `return`,
/// and/or `break`.
///
/// ```
/// use delta_kernel::coroutine::kernel::{Request, StaticCoroutine};
/// use delta_kernel::coroutine::{drive_coroutine, CoroutineStep};
/// use delta_kernel::DeltaResult;
///
/// # fn handle(_: Request) -> DeltaResult<()> { Ok(()) }
/// # fn macro_form(coroutine: StaticCoroutine<usize>) -> DeltaResult<usize> {
/// let output = drive_coroutine!(coroutine, |request| handle(request)?);
/// # Ok(output)
/// # }
/// // Equivalent expansion:
/// # fn expanded(mut coroutine: delta_kernel::coroutine::kernel::StaticCoroutine<usize>) -> DeltaResult<usize> {
/// let output = loop {
///     match coroutine.try_advance()? {
///         CoroutineStep::Done(output) => break output,
///         CoroutineStep::Request(request) => handle(request)?,
///     }
/// };
/// # Ok(output)
/// # }
/// ```
#[pub_macro]
macro_rules! drive_coroutine {
    ($coroutine:expr, |$request:ident| $handler:expr) => {{
        let mut coroutine = $coroutine;
        loop {
            match coroutine.try_advance()? {
                $crate::coroutine::CoroutineStep::Done(output) => break output,
                $crate::coroutine::CoroutineStep::Request($request) => $handler,
            }
        }
    }};
}

/// Asynchronously drives a coroutine to completion using a user-provided request handler.
///
/// The handler expression runs once for each [`CoroutineStep::Request`] and must produce `()`.
/// [`CoroutineStep::Done`] returns the coroutine output. The handler is expanded in the caller's
/// context, so it may freely use `?`, `.await`, `return`, and/or `break`.
///
/// ```no_run
/// use delta_kernel::coroutine::kernel::{Request, StaticCoroutine};
/// use delta_kernel::coroutine::{drive_coroutine_async, CoroutineStep};
/// use delta_kernel::DeltaResult;
///
/// # async fn handle(_: Request) -> DeltaResult<()> { Ok(()) }
/// # async fn macro_form(coroutine: StaticCoroutine<usize>) -> DeltaResult<usize> {
/// let output = drive_coroutine_async!(coroutine, |request| handle(request).await?);
/// # Ok(output)
/// # }
/// // Equivalent expansion:
/// # async fn expanded(mut coroutine: delta_kernel::coroutine::kernel::StaticCoroutine<usize>) -> DeltaResult<usize> {
/// let output = loop {
///     match coroutine.advance().await? {
///         CoroutineStep::Done(output) => break output,
///         CoroutineStep::Request(request) => handle(request).await?,
///     }
/// };
/// # Ok(output)
/// # }
/// ```
#[pub_macro]
macro_rules! drive_coroutine_async {
    ($coroutine:expr, |$request:ident| $handler:expr) => {{
        let mut coroutine = $coroutine;
        loop {
            match coroutine.advance().await? {
                $crate::coroutine::CoroutineStep::Done(output) => break output,
                $crate::coroutine::CoroutineStep::Request($request) => $handler,
            }
        }
    }};
}

/// Hidden expansion state for [`coroutine!`] and [`static_coroutine!`].
///
/// Stores an uninvoked async function and the current tracing span until the macro constructs a
/// [`Coroutine`]. This type is public only so exported macros can name it.
#[doc(hidden)]
#[internal_api]
pub(crate) struct BodyFn<F, P, O> {
    body: F,
    span: Span,
    _type_signature: PhantomType<(P, O)>,
}

// `Self::new` and `Self::witness` constrain `F` to be callable with a borrowed `Channel` of any
// `'channel` lifetime. That lifetime is local to the higher-ranked call; because the coroutine owns
// both the channel and resulting future, the future can retain its channel borrow while suspended
// without constraining `'coroutine`.
//
// `Self::into_coroutine_with` passes `self` and `Self::witness` to `Task::new_with_witness`; the
// latter's doc comment explains why we must pass the witness separately. The returned future is
// instrumented with the captured span, and abandoning the coroutine before completion reports a
// tracing error.
impl<F, P: Send + 'static, O> BodyFn<F, P, O> {
    /// Stores `body` without choosing its call-future proof path.
    #[internal_api]
    pub(crate) fn new(body: F) -> Self
    where
        F: for<'channel> AsyncFnOnce(&'channel Channel<P>) -> DeltaResult<O>,
    {
        Self {
            body,
            span: Span::current(),
            _type_signature: PhantomType::default(),
        }
    }

    /// Creates a scoped coroutine using the concrete call-future proof supplied by `witness`.
    #[internal_api]
    pub(crate) fn into_coroutine_with<'coroutine, Fut>(
        self,
        witness: impl FnOnce(Self, Arc<Channel<P>>) -> Fut,
    ) -> Coroutine<'coroutine, P, O>
    where
        Fut: DeltaFuture<O> + 'coroutine,
        P: 'coroutine,
        O: 'coroutine,
    {
        Coroutine(Task::new_with_witness(self, witness))
    }

    /// Runs the body while exposing its concrete future to [`Self::into_coroutine_with`].
    #[internal_api]
    pub(crate) fn witness(self, channel: Arc<Channel<P>>) -> impl Future<Output = DeltaResult<O>>
    where
        F: for<'channel> AsyncFnOnce(&'channel Channel<P>) -> DeltaResult<O>,
    {
        let future = async move {
            let guard = CoroutineCompletionGuard;
            let output = (self.body)(channel.as_ref()).await;
            let _ = ManuallyDrop::new(guard);
            output.inspect_err(|err| error!(error = %err, "coroutine failed"))
        };
        future.instrument(self.span)
    }
}

/// Logs when a coroutine is dropped before producing its terminal result.
///
/// Early termination is ordinary for generators, but a [`Coroutine`] must complete to produce a
/// useful output. The guard is disarmed with `ManuallyDrop` once the body completes. `Instrumented`
/// enters the coroutine span while dropping the body, so the abandonment event retains its context.
struct CoroutineCompletionGuard;

impl Drop for CoroutineCompletionGuard {
    fn drop(&mut self) {
        error!(
            error = "abandoned",
            "coroutine was abandoned while suspended"
        );
    }
}

/// Describes a connector operation that may return multiple pages.
pub trait PagedOperation: Send + Sized + 'static {
    /// Page payload produced by the connector.
    type Page: Send + 'static;
}

/// One page of connector data and the cursor for requesting more.
#[derive(Constructor)]
pub struct Page<Op: PagedOperation> {
    /// Data returned in this page.
    pub data: Op::Page,
    /// Cursor to pass to [`PageRequest::Continue`], or `None` when the operation is exhausted.
    ///
    /// An empty payload with `Some(cursor)` is not exhaustion.
    pub next: Option<Cursor<Op>>,
}

/// Opaque connector-owned continuation state typed to one paged operation.
///
/// Kernel stores and returns cursors without inspecting their contents.
pub struct Cursor<Op> {
    state: Box<dyn Any + Send>,
    _op_type: PhantomType<Op>,
}

impl<Op> Cursor<Op> {
    /// Wrap connector-owned `state` for a later continuation request.
    pub fn new(state: impl Any + Send + 'static) -> Self {
        Self {
            state: Box::new(state),
            _op_type: PhantomType::default(),
        }
    }

    /// Recover the connector-owned state.
    ///
    /// Returns an error if `T` is not the type originally passed to [`Self::new`].
    pub fn into_inner<T: Any>(self) -> DeltaResult<T> {
        self.state.downcast().map(|inner| *inner).map_err(|_| {
            Error::internal_error(format!(
                "invalid cursor: unable to cast as {}",
                type_name::<T>()
            ))
        })
    }
}

/// One phase of a paginated connector operation.
pub enum PageRequest<Op: PagedOperation> {
    /// Initialize the operation and return its first page.
    Start(Op, Reply<Page<Op>>),
    /// Initialize the operation and return a cursor without fetching the first page.
    Prepare(Op, Reply<Cursor<Op>>),
    /// Consume a cursor and return the next page.
    Continue(Cursor<Op>, Reply<Page<Op>>),
}

/// Unbound generator of page payloads.
#[internal_api]
pub(crate) enum PageGenerator<Op: PagedOperation> {
    /// Start a new paged operation.
    Start(Op),
    /// Resume an initialized operation from `cursor`.
    Next(Cursor<Op>),
    /// The operation is exhausted.
    Done,
}

impl<Op: PagedOperation> PageGenerator<Op> {
    /// Create a generator that starts a new paged operation.
    pub(crate) fn start(operation: Op) -> Self {
        Self::Start(operation)
    }

    /// Create a generator that resumes an initialized paged operation.
    pub(crate) fn resume(cursor: Cursor<Op>) -> Self {
        Self::Next(cursor)
    }
}

impl<Op: PagedOperation> GeneratorExt for PageGenerator<Op> {}

impl<P, Op> IntoGenerator<P> for PageGenerator<Op>
where
    P: Send + 'static,
    Op: PagedOperation + Unpin,
    PageRequest<Op>: Into<P>,
{
    type Item = Op::Page;
    type Output = ();

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl Generator<Item = Op::Page, Output = ()> + Unpin + 'a {
        unfold_mut!(BoundPages::new(channel, self), BoundPages::next)
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedGenerator<'a, Op::Page> {
        Box::new((*self).bind_to(channel))
    }
}

struct BoundPages<'a, P: Send + 'static, Op: PagedOperation> {
    channel: ChannelBinding<'a, P>,
    state: PageGenerator<Op>,
}

impl<'a, P: Send + 'static, Op: PagedOperation> BoundPages<'a, P, Op> {
    fn new(channel: ChannelBinding<'a, P>, state: PageGenerator<Op>) -> Self {
        Self { channel, state }
    }

    async fn next(&mut self) -> DeltaResult<Option<Op::Page>>
    where
        PageRequest<Op>: Into<P>,
    {
        let page = match std::mem::replace(&mut self.state, PageGenerator::Done) {
            PageGenerator::Start(operation) => {
                self.channel.exchange(operation, PageRequest::Start).await?
            }
            PageGenerator::Next(cursor) => {
                self.channel.exchange(cursor, PageRequest::Continue).await?
            }
            PageGenerator::Done => return Ok(None),
        };
        if let Some(cursor) = page.next {
            self.state = PageGenerator::Next(cursor);
        }
        Ok(Some(page.data))
    }
}

/// Pulls successive page payloads until the connector reports exhaustion.
///
/// Call sites iterate this helper and then walk [`PagedOperation::Page`] themselves. Flatten
/// collection-valued pages with [`GeneratorExt::flatten`].
#[internal_api]
pub(crate) struct Pages<'a, C: ?Sized, Op: PagedOperation> {
    channel: &'a C,
    state: PageGenerator<Op>,
}

impl<'a, C: ?Sized, Op: PagedOperation> Pages<'a, C, Op> {
    fn new(channel: &'a C, state: PageGenerator<Op>) -> Self {
        Self { channel, state }
    }

    /// Iterate over the pages of `operation`.
    #[internal_api]
    pub(crate) fn start(channel: &'a C, operation: Op) -> Self {
        Self::new(channel, PageGenerator::start(operation))
    }

    /// Iterate over the remaining pages of an in-progress operation, starting from `cursor`.
    #[internal_api]
    pub(crate) fn resume(channel: &'a C, cursor: Cursor<Op>) -> Self {
        Self::new(channel, PageGenerator::resume(cursor))
    }

    /// Drive this page stream as a [`Generator`].
    #[internal_api]
    pub(crate) fn into_generator<P>(
        self,
    ) -> impl Generator<Item = Op::Page, Output = ()> + Unpin + 'a
    where
        PageRequest<Op>: Into<P>,
        C: ChannelExchange<P> + Sync + 'static,
        Op: Unpin,
        P: Send + 'static,
    {
        unfold_mut!(self, Pages::next)
    }

    /// Yield items from collection-valued pages.
    #[internal_api]
    pub(crate) fn flatten<P>(
        self,
    ) -> impl Generator<Item = <Op::Page as IntoIterator>::Item, Output = ()> + Unpin + 'a
    where
        Op: Unpin,
        Op::Page: IntoIterator,
        <Op::Page as IntoIterator>::IntoIter: Send + Unpin,
        <Op::Page as IntoIterator>::Item: Send + 'static,
        PageRequest<Op>: Into<P>,
        C: ChannelExchange<P> + Sync + 'static,
        P: Send + 'static,
    {
        self.into_generator().flatten()
    }

    /// Fetch the next page payload, or `None` after the last page.
    #[internal_api]
    pub(crate) async fn next<P>(&mut self) -> DeltaResult<Option<Op::Page>>
    where
        PageRequest<Op>: Into<P>,
        C: ChannelExchange<P>,
        P: Send + 'static,
    {
        let page = match std::mem::replace(&mut self.state, PageGenerator::Done) {
            PageGenerator::Start(operation) => {
                self.channel.exchange(operation, PageRequest::Start).await?
            }
            PageGenerator::Next(cursor) => {
                self.channel.exchange(cursor, PageRequest::Continue).await?
            }
            PageGenerator::Done => return Ok(None),
        };
        if let Some(cursor) = page.next {
            self.state = PageGenerator::Next(cursor);
        }
        Ok(Some(page.data))
    }
}

#[doc(inline)]
#[internal_api]
pub(crate) use self::engine::drive_with_engine;
#[doc(inline)]
#[internal_api]
pub(crate) use self::engine::drive_with_storage;
#[doc(inline)]
#[internal_api]
pub(crate) use self::engine::{run_coroutine_with_engine, run_coroutine_with_storage};

#[cfg(test)]
mod tests;
