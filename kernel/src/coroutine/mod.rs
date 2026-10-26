//! Runtime-neutral async protocol logic with connector-owned execution.
//!
//! Kernel protocol operations are ordinary Rust futures. Futures let Kernel suspend, but that alone
//! does not avoid control inversion, where kernel decides when and how to invoke connector code,
//! while connector code is already invoking kernel. Merely making connector callbacks async would
//! still require Kernel to invoke connector code through kernel-defined interfaces the connector
//! must implement.
//!
//! A Kernel coroutine instead combines a future with a typed request channel. When Kernel needs
//! connector work, it posts a [`Request`](kernel::Request) through that channel and awaits its
//! [`Reply`]. The request surfaces to the connector as an output of driving the coroutine, instead
//! of as an argument to a connector function kernel invokes. Each request carries a single-use
//! [`Reply`] tied to its originating await point. Sending or dropping the reply makes that point
//! ready and may wake an async driver (sync drivers often use a no-op waker), but only the
//! connector-owned driver decides when to poll the coroutine again.
//!
//! The futures that back Kernel's coroutines are runtime neutral. A synchronous driver manually
//! polls them with [`Workflow::try_advance`] or [`Coroutine::try_advance`] and requires no async
//! runtime at all. An async driver uses [`Workflow::advance`] or [`Coroutine::advance`] so its own
//! executor can park and wake the connector as it awaits the operation, which in turn awaits the
//! connector's reply from a different task.
//!
//! This module provides two types of coroutines, both of which may issue zero or more requests. A
//! [`Coroutine`] interleaves requests with a stream of yielded items before completing with a final
//! output (often but not always `()`). A [`Workflow`] is a specialized kind of coroutine that never
//! yields and completes with one final output (usually not `()`).
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
//! process or machine boundary for remote execution. A reply refers weakly to the kernel future
//! waiting for it, so outstanding replies do not keep a coroutine alive. Dropping the coroutine
//! abandons the operation, and subsequent replies to any of its outstanding requests are silently
//! discarded.
//!
//! # Driver policies
//!
//! A synchronous driver handles each request, sends its reply, and calls [`Workflow::try_advance`]
//! or [`Coroutine::try_advance`] again. Kernel's internal async machinery remains fully hidden from
//! the connector and no async runtime is required.
//!
//! An async driver may await each request before awaiting [`Workflow::advance`] or
//! [`Coroutine::advance`] again, or schedule requests concurrently and immediately await another
//! step. In both cases the connector's executor polls Kernel's futures and the connector remains
//! responsible for scheduling and executing every request.
//!
//! Engine-compatible entry points use a synchronous driver that translates each request into calls
//! to [`crate::Engine`] handlers. Connectors that choose these entry points intentionally opt into
//! the legacy Kernel-to-Engine callback model.
//!
//! # Driving operations
//!
//! A workflow driver waits for [`WorkflowStep::Request`] or [`WorkflowStep::Done`].
//! [`drive_workflow!`] expands a one-request-at-a-time loop around [`Workflow::try_advance`].
//! [`drive_workflow_async!`] does the same with [`Workflow::advance`], expanding the handler in
//! the caller's async context.
//!
//! # Pagination
//!
//! Operations that may produce large or unbounded results, such as file reads or query output, use
//! a paginated request model. Kernel consumes a logical stream of results while the connector
//! controls in-flight memory and can terminate the operation if resource limits are hit.
//!
//! Paged operations have two phases:
//!
//! - [`PageRequest::Prepare`] initializes an operation and returns a [`Cursor`] without producing a
//!   first page. This lets the connector begin work in the background while waiting for the first
//!   [`PageRequest::Next`] from kernel.
//! - [`PageRequest::Next`] consumes a cursor and returns the next [`Page`] of output, with
//!   `Some(cursor)` when more pages remain.
//!
//! [`PageRequest::Next`] returns a [`Page`]: payload plus an optional [`Cursor`]. `None` ends
//! pagination; an empty page with a live cursor does not. Connectors choose page boundaries and
//! cursor representation; cursor payloads are opaque to kernel, forwarded blindly back to the
//! connector with each continuation request (or dropped if kernel abandoned the operation).
//!
//! # Error handling
//!
//! Replying with `Err` delivers a connector failure to the suspended Kernel await point. Attempting
//! to advance a coroutine or workflow after its terminal step is a logic error. Synchronous
//! `try_advance` returns [`KernelError::WouldBlock`] if Kernel suspends without producing a step,
//! which can only happen if there are outstanding replies.

use std::future::poll_fn;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll, Waker};

use delta_kernel_derive::{internal_api, pub_macro};
use tracing::{Instrument as _, Span};

#[doc(inline)]
#[internal_api]
pub(crate) use self::async_ext::AsyncResult;
pub(crate) use self::async_ext::{AsyncBool, AsyncOption};
use crate::{KernelError, KernelResult};

/// Independently driven operation that may request connector work before producing a final output.
///
/// The workflow owns its request channel. [`Self::advance`] and [`Self::try_advance`] return each
/// request as [`WorkflowStep::Request`] and successful completion as [`WorkflowStep::Done`]. The
/// `'a` lifetime covers any state borrowed by the operation; [`StaticWorkflow`] owns all captured
/// state.
pub struct Workflow<'a, P: Send + 'static, O> {
    body: Option<Pin<Box<dyn KernelFuture<O> + 'a>>>,
    requests: Arc<Channel<P>>,
}

/// A workflow that owns all captured state.
pub type StaticWorkflow<P, O> = Workflow<'static, P, O>;

/// Request or completion produced by advancing a [`Workflow`].
// The request variant is the common case, and boxing `P` would add an allocation to every request.
#[allow(clippy::large_enum_variant)]
pub enum WorkflowStep<P, O> {
    /// Connector work offered by the workflow.
    Request(P),
    /// Terminal output.
    Done(O),
}

impl<P: Send + 'static, O> Workflow<'_, P, O> {
    /// Tries to advance to the next request or completion without waiting.
    ///
    /// Polls the workflow once and returns with [`KernelError::WouldBlock`] if no next step is
    /// immediately available. This happens if the workflow is blocked on one or more outstanding
    /// [`WorkflowStep::Request`]; callers can unblock the workflow by invoking [`Reply::send`]
    /// (or dropping the `Reply`) for previously received requests before calling this method again.
    ///
    /// Calling this method after receiving [`WorkflowStep::Done`] is a logic error.
    pub fn try_advance(&mut self) -> KernelResult<WorkflowStep<P, O>> {
        match self.poll_step(&mut noop_context()) {
            Poll::Ready(step) => step,
            Poll::Pending => Err(KernelError::WouldBlock),
        }
    }

    /// Advances to the next request or completion, waiting asynchronously if the workflow is
    /// blocked on one or more outstanding [`WorkflowStep::Request`]. Invoke the requests'
    /// [`Reply::send`] (or drop them) to unblock the workflow.
    ///
    /// Calling this method after receiving [`WorkflowStep::Done`] is a logic error.
    pub async fn advance(&mut self) -> KernelResult<WorkflowStep<P, O>> {
        poll_fn(|context| self.poll_step(context)).await
    }

    fn poll_step(&mut self, context: &mut Context<'_>) -> Poll<KernelResult<WorkflowStep<P, O>>> {
        let Some(body) = self.body.as_mut() else {
            return Poll::Ready(Err(KernelError::internal_error(
                "workflow advanced after completion",
            )));
        };
        // Reconcile ready replies before accepting queued offers. A request becomes visible to the
        // connector only when accepted, so polling first lets completed race branches cancel offers
        // that no longer require connector work.
        if let Poll::Ready(output) = body.as_mut().poll(context) {
            self.body = None;
            return Poll::Ready(output.map(WorkflowStep::Done));
        }
        if let Some(request) = self.requests.pop() {
            return Poll::Ready(Ok(WorkflowStep::Request(request)));
        }
        Poll::Pending
    }
}

/// Yield, request, or completion produced by advancing a [`Coroutine`].
///
/// Yielded items are removed from the pending queue before this step returns. Requests carry their
/// own replies.
pub enum CoroutineStep<P: Send + 'static, Y: Send, O: Send + 'static = ()> {
    /// A yielded item.
    Yield(Y),
    /// Connector work offered by the coroutine.
    Request(P),
    /// The coroutine completed.
    Done(O),
}

/// Independently driven coroutine with owned request state.
///
/// The coroutine owns its request channel and may borrow captured operation state for `'a`.
pub struct Coroutine<'a, P: Send + 'static, Y: Send + 'a, O: Send + 'static = ()> {
    requests: Arc<Channel<P>>,
    generator: BoxedBoundGenerator<'a, Y, O>,
}

/// Independently driven coroutine that owns all captured state.
pub type StaticCoroutine<P, Y, O = ()> = Coroutine<'static, P, Y, O>;

/// Coroutine that enters one tracing span whenever it is advanced.
pub struct InstrumentedCoroutine<'a, P: Send + 'static, Y: Send, O: Send + 'static = ()> {
    inner: Coroutine<'a, P, Y, O>,
    span: Span,
}

/// Instrumented coroutine that owns all captured state.
pub type StaticInstrumentedCoroutine<P, Y, O = ()> = InstrumentedCoroutine<'static, P, Y, O>;

impl<'a, P: Send + 'static, Y: Send + 'a, O: Send + 'static> InstrumentedCoroutine<'a, P, Y, O> {
    /// Wraps `inner` so each advance occurs inside `span`.
    pub fn new(inner: Coroutine<'a, P, Y, O>, span: Span) -> Self {
        Self { inner, span }
    }

    /// Tries to advance to the next yielded item, request, or completion without waiting.
    pub fn try_advance(&mut self) -> KernelResult<CoroutineStep<P, Y, O>> {
        let _guard = self.span.enter();
        self.inner.try_advance()
    }

    /// Advances asynchronously to the next yielded item, request, or completion.
    pub async fn advance(&mut self) -> KernelResult<CoroutineStep<P, Y, O>> {
        self.inner.advance().instrument(self.span.clone()).await
    }

    pub(crate) fn into_parts(self) -> (Coroutine<'a, P, Y, O>, Span) {
        (self.inner, self.span)
    }
}

impl<'a, P: Send + 'static, Y: Send + 'a, O: Send + 'static> Coroutine<'a, P, Y, O> {
    /// Construct a coroutine around an unpolled generator body.
    #[internal_api]
    pub(crate) fn new(
        make_generator: impl FnOnce(Arc<Channel<P>>) -> GeneratorBody<'a, Y, O>,
    ) -> Self {
        let channel = Arc::new(Channel::default());
        Self {
            requests: Arc::clone(&channel),
            generator: Box::new(make_generator(channel)),
        }
    }

    /// Tries to advance to the next yielded item, request, or completion without waiting.
    ///
    /// Polls the coroutine once and returns with [`KernelError::WouldBlock`] if no next step is
    /// immediately available. This happens if the coroutine is blocked on one or more outstanding
    /// [`CoroutineStep::Request`]; callers can unblock it by invoking the requests'
    /// [`Reply::send`] (or dropping them) before calling this method again.
    ///
    /// Calling this method after receiving [`CoroutineStep::Done`] is a logic error.
    pub fn try_advance(&mut self) -> KernelResult<CoroutineStep<P, Y, O>> {
        match self.poll_step(&mut noop_context()) {
            Poll::Ready(step) => step,
            Poll::Pending => Err(KernelError::WouldBlock),
        }
    }

    /// Advances to the next yielded item, request, or completion, waiting asynchronously if the
    /// coroutine is blocked on one or more outstanding [`CoroutineStep::Request`]. Invoke the
    /// requests' [`Reply::send`] (or drop them) to unblock the coroutine.
    ///
    /// Calling this method after receiving [`CoroutineStep::Done`] is a logic error.
    pub async fn advance(&mut self) -> KernelResult<CoroutineStep<P, Y, O>> {
        poll_fn(|context| self.poll_step(context)).await
    }

    fn poll_step(
        &mut self,
        context: &mut Context<'_>,
    ) -> Poll<KernelResult<CoroutineStep<P, Y, O>>> {
        Poll::Ready(match Pin::new(&mut *self.generator).poll_step(context) {
            Poll::Ready(Ok(GeneratorStep::Yield(item))) => Ok(CoroutineStep::Yield(item)),
            Poll::Ready(Ok(GeneratorStep::Done(output))) => Ok(CoroutineStep::Done(output)),
            Poll::Ready(Err(err)) => Err(err),
            Poll::Pending => match self.requests.pop() {
                Some(request) => Ok(CoroutineStep::Request(request)),
                None => return Poll::Pending,
            },
        })
    }

    /// Bind an unbound generator to an owned request channel.
    pub fn from_generator(generator: impl Generator<P, Item = Y, Output = O> + 'a) -> Self {
        let requests = Arc::new(Channel::default());
        let generator = generator.bind_to(generator::ChannelBinding::Owned(Arc::clone(&requests)));
        Self {
            generator: Box::new(generator),
            requests,
        }
    }
}

impl<'a, P: Send + 'static, Y: Send + 'a, O: Send + 'static>
    From<Box<dyn Generator<P, Item = Y, Output = O> + 'a>> for Coroutine<'a, P, Y, O>
{
    fn from(generator: Box<dyn Generator<P, Item = Y, Output = O> + 'a>) -> Self {
        let requests = Arc::new(Channel::default());
        Self {
            generator: generator
                .bind_boxed(generator::ChannelBinding::Owned(Arc::clone(&requests))),
            requests,
        }
    }
}

/// Creates a workflow for a specific request vocabulary.
///
/// The `|channel| future` form forwards one future as the workflow body. Use an explicit async
/// closure for multi-step control flow.
#[internal_api]
macro_rules! workflow {
    ($request:ty, |$channel:ident| $future:expr $(,)?) => {
        $crate::coroutine::workflow!($request, async move |$channel| ($future).await)
    };
    ($request:ty, $body:expr $(,)?) => {
        $crate::coroutine::BodyFn::<_, $request, _>::new($body)
            .into_workflow_with($crate::coroutine::BodyFn::witness)
    };
}

/// Creates a workflow that owns all captured state.
///
/// The `|channel| future` form forwards one future as the workflow body. Use an explicit async
/// closure for multi-step control flow.
#[internal_api]
macro_rules! static_workflow {
    ($request:ty, |$channel:ident| $future:expr $(,)?) => {
        $crate::coroutine::static_workflow!($request, async move |$channel| ($future).await)
    };
    ($request:ty, $body:expr $(,)?) => {
        $crate::coroutine::BodyFn::<_, $request, _>::new($body)
            .into_workflow_with::<'static, _>($crate::coroutine::BodyFn::witness)
    };
}

/// Creates a [`Coroutine`] for a specific request vocabulary.
///
/// The `|channel| future` form forwards one future as the coroutine body. Use an explicit async
/// closure for multi-step control flow.
#[internal_api]
macro_rules! coroutine {
    ($request:ty, |$channel:ident| $future:expr $(,)?) => {
        $crate::coroutine::coroutine!($request, async move |$channel| ($future).await)
    };
    ($request:ty, $body:expr $(,)?) => {
        $crate::coroutine::Coroutine::<$request, _, _>::new(move |channel| {
            $crate::coroutine::generator::generator!(channel, $body)
        })
    };
}

/// Creates a [`StaticCoroutine`] that owns all captured state.
///
/// The `|channel| future` form forwards one future as the coroutine body. Use an explicit async
/// closure for multi-step control flow.
///
/// ```compile_fail
/// use delta_kernel::coroutine::static_coroutine;
///
/// enum Request {}
///
/// fn borrow<'a>(value: &'a str) {
///     let _coroutine = static_coroutine!(Request, |channel| channel.yield_item(value));
/// }
/// ```
#[internal_api]
macro_rules! static_coroutine {
    ($request:ty, |$channel:ident| $future:expr $(,)?) => {
        $crate::coroutine::static_coroutine!($request, async move |$channel| ($future).await)
    };
    ($request:ty, $body:expr $(,)?) => {
        $crate::coroutine::StaticCoroutine::<$request, _, _>::new(move |channel| {
            $crate::coroutine::generator::generator!(channel, $body)
        })
    };
}

/// Drives a workflow to completion using a caller-provided request handler.
///
/// The handler expression runs once for each [`WorkflowStep::Request`] and must produce `()`.
/// [`WorkflowStep::Done`] returns the workflow output. The handler is expanded in the caller's
/// context, so it may use `?`, `.await`, `return`, and `break`.
#[pub_macro]
macro_rules! drive_workflow {
    ($workflow:expr, |$request:ident| $handler:expr) => {{
        let mut workflow = $workflow;
        loop {
            match workflow.try_advance()? {
                $crate::coroutine::WorkflowStep::Done(output) => break output,
                $crate::coroutine::WorkflowStep::Request($request) => $handler,
            }
        }
    }};
}

/// Asynchronously drives a workflow to completion using a caller-provided request handler.
///
/// The handler expression runs once for each [`WorkflowStep::Request`] and must produce `()`.
/// [`WorkflowStep::Done`] returns the workflow output. The handler is expanded in the caller's
/// context, so it may use `?`, `.await`, `return`, and `break`.
#[pub_macro]
macro_rules! drive_workflow_async {
    ($workflow:expr, |$request:ident| $handler:expr) => {{
        let mut workflow = $workflow;
        loop {
            match workflow.advance().await? {
                $crate::coroutine::WorkflowStep::Done(output) => break output,
                $crate::coroutine::WorkflowStep::Request($request) => $handler,
            }
        }
    }};
}

/// Create a task context backed by a no-op waker.
pub(crate) fn noop_context() -> Context<'static> {
    Context::from_waker(Waker::noop())
}

pub use self::channel::Reply;
#[doc(inline)]
#[internal_api]
pub(crate) use self::channel::{Channel, ChannelExchange, KernelFuture};
#[doc(inline)]
#[internal_api]
pub(crate) use self::generator::{
    BoundGenerator, BoxedBoundGenerator, Flatten, Generator, GeneratorBody, GeneratorExt,
    GeneratorStep,
};
#[doc(inline)]
#[internal_api]
pub(crate) use self::pagination::PageGenerator;
pub use self::pagination::{Cursor, Page, PageRequest, PagedOperation};
#[doc(hidden)]
#[internal_api]
pub(crate) use self::workflow::BodyFn;

mod async_ext;
mod channel;
mod pagination;
mod witness;
mod workflow;
// cbindgen ignores cfg gates on module declarations. Ignore both arms so coroutine-only generic
// aliases are not registered or monomorphized into the C API.
#[cfg(feature = "internal-api")]
/// cbindgen:ignore
pub mod generator;
#[cfg(not(feature = "internal-api"))]
/// cbindgen:ignore
#[allow(unreachable_pub)]
pub(crate) mod generator;
/// cbindgen:ignore
pub mod kernel;

#[cfg(test)]
mod tests;
