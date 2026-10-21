//! Channel-backed operations that complete with one value and never yield.

use std::future::{poll_fn, Future};
use std::mem::ManuallyDrop;
use std::ops::AsyncFnOnce;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll};

use delta_kernel_derive::{internal_api, pub_macro};
use tracing::{error, Instrument as _, Span};

use super::channel::noop_context;
use super::witness::invoke_witness;
use super::{Channel, DeltaFuture, Reply};
use crate::utils::PhantomType;
use crate::{DeltaResult, Error};

/// Independently driven operation that may request connector work before producing a final output.
///
/// The workflow owns its request channel. [`Self::advance`] and [`Self::try_advance`] return each
/// request as [`WorkflowStep::Request`] and successful completion as [`WorkflowStep::Done`]. The
/// `'a` lifetime covers any state borrowed by the operation; [`StaticWorkflow`] owns all captured
/// state.
pub struct Workflow<'a, P: Send + 'static, O> {
    body: Option<Pin<Box<dyn DeltaFuture<O> + 'a>>>,
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

impl<'a, P: Send + 'static, O> Workflow<'a, P, O> {
    /// Tries to advance to the next request or completion without waiting.
    ///
    /// Polls the workflow once and returns with [`Error::WouldBlock`] if no next step is
    /// immediately available. This happens if the workflow is blocked on one or more outstanding
    /// [`WorkflowStep::Request`]; callers can unblock the workflow by invoking [`Reply::send`]
    /// (or dropping the `Reply`) for previously-received requests before calling this method again.
    ///
    /// Calling this method after receiving [`WorkflowStep::Done`] is a logic error.
    pub fn try_advance(&mut self) -> DeltaResult<WorkflowStep<P, O>> {
        let Poll::Ready(result) = self.poll_step(&mut noop_context()) else {
            return Err(Error::WouldBlock);
        };
        result
    }

    /// Advances to the next request or completion, waiting asynchronously if the workflow is
    /// blocked on one or more outstanding [`WorkflowStep::Request`]. Invoke the requests'
    /// [`Reply::send`] (or drop them) to unblock the workflow.
    ///
    /// Calling this method after receiving [`WorkflowStep::Done`] is a logic error.
    pub async fn advance(&mut self) -> DeltaResult<WorkflowStep<P, O>> {
        poll_fn(|context| self.poll_step(context)).await
    }

    fn poll_step(&mut self, context: &mut Context<'_>) -> Poll<DeltaResult<WorkflowStep<P, O>>> {
        let Some(body) = self.body.as_mut() else {
            return Poll::Ready(Err(Error::internal_error(
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

/// Creates a workflow for a specific request vocabulary.
///
/// The workflow owns its request channel but may borrow captured state.
///
/// The request type determines the concrete [`Channel`] received by the body. A vocabulary can
/// add typed operations with an extension trait over that channel:
///
/// ```
/// use delta_kernel::coroutine::{workflow, Channel, ChannelExchange, WorkflowStep, DeltaFuture, Reply};
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
/// let mut workflow = workflow!(Request, async move |channel| {
///     let length = channel.length("delta".to_string()).await?;
///     Ok(length * 2)
/// });
/// let WorkflowStep::Request(Request::Length(value, reply)) = workflow.try_advance()? else {
///     unreachable!("workflow did not request a length");
/// };
/// reply.send(Ok(value.len()));
/// let WorkflowStep::Done(output) = workflow.try_advance()? else {
///     unreachable!("workflow did not complete");
/// };
/// assert_eq!(output, 10);
/// # Ok::<(), delta_kernel::Error>(())
/// ```
#[internal_api]
macro_rules! workflow {
    ($request:ty, $body:expr) => {
        $crate::coroutine::BodyFn::<_, $request, _>::new($body)
            .into_workflow_with($crate::coroutine::BodyFn::witness)
    };
}

/// Creates a workflow that owns all captured state.
///
/// ```
/// use delta_kernel::coroutine::{static_workflow, StaticWorkflow};
///
/// enum Request {}
///
/// let workflow: StaticWorkflow<Request, ()> =
///     static_workflow!(Request, async move |_channel| Ok(()));
/// ```
#[internal_api]
macro_rules! static_workflow {
    ($request:ty, $body:expr) => {
        $crate::coroutine::BodyFn::<_, $request, _>::new($body)
            .into_workflow_with::<'static, _>($crate::coroutine::BodyFn::witness)
    };
}

/// Drives a workflow to completion using a caller-provided request handler.
///
/// The handler expression runs once for each [`WorkflowStep::Request`] and must produce `()`.
/// [`WorkflowStep::Done`] returns the workflow output.
///
/// The handler is expanded in the caller's context, so it may freely use `?`, `.await`, `return`,
/// and/or `break`.
///
/// ```
/// use delta_kernel::coroutine::kernel::{Request, StaticWorkflow};
/// use delta_kernel::coroutine::{drive_workflow, WorkflowStep};
/// use delta_kernel::DeltaResult;
///
/// # fn handle(_: Request) -> DeltaResult<()> { Ok(()) }
/// # fn macro_form(workflow: StaticWorkflow<usize>) -> DeltaResult<usize> {
/// let output = drive_workflow!(workflow, |request| handle(request)?);
/// # Ok(output)
/// # }
/// // Equivalent expansion:
/// # fn expanded(mut workflow: delta_kernel::coroutine::kernel::StaticWorkflow<usize>) -> DeltaResult<usize> {
/// let output = loop {
///     match workflow.try_advance()? {
///         WorkflowStep::Done(output) => break output,
///         WorkflowStep::Request(request) => handle(request)?,
///     }
/// };
/// # Ok(output)
/// # }
/// ```
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

/// Asynchronously drives a workflow to completion using a user-provided request handler.
///
/// The handler expression runs once for each [`WorkflowStep::Request`] and must produce `()`.
/// [`WorkflowStep::Done`] returns the workflow output. The handler is expanded in the caller's
/// context, so it may freely use `?`, `.await`, `return`, and/or `break`.
///
/// ```no_run
/// use delta_kernel::coroutine::kernel::{Request, StaticWorkflow};
/// use delta_kernel::coroutine::{drive_workflow_async, WorkflowStep};
/// use delta_kernel::DeltaResult;
///
/// # async fn handle(_: Request) -> DeltaResult<()> { Ok(()) }
/// # async fn macro_form(workflow: StaticWorkflow<usize>) -> DeltaResult<usize> {
/// let output = drive_workflow_async!(workflow, |request| handle(request).await?);
/// # Ok(output)
/// # }
/// // Equivalent expansion:
/// # async fn expanded(mut workflow: delta_kernel::coroutine::kernel::StaticWorkflow<usize>) -> DeltaResult<usize> {
/// let output = loop {
///     match workflow.advance().await? {
///         WorkflowStep::Done(output) => break output,
///         WorkflowStep::Request(request) => handle(request).await?,
///     }
/// };
/// # Ok(output)
/// # }
/// ```
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

/// Hidden expansion state for [`workflow!`] and [`static_workflow!`].
///
/// Stores an uninvoked async function and the current tracing span until the macro constructs a
/// [`Workflow`]. This type is public only so exported macros can name it.
#[doc(hidden)]
#[internal_api]
pub(crate) struct BodyFn<F, P, O> {
    body: F,
    span: Span,
    _type_signature: PhantomType<(P, O)>,
}

// `Self::new` and `Self::witness` constrain `F` to be callable with a borrowed `Channel` of any
// `'channel` lifetime. That lifetime is local to the higher-ranked call; because the workflow owns
// both the channel and resulting future, the future can retain its channel borrow while suspended
// without constraining `'workflow`.
//
// `Self::into_workflow_with` passes `self` and `Self::witness` through [`invoke_witness`], whose
// doc comment explains why the witness must remain separate. The returned future is instrumented
// with the captured span, and abandoning the workflow before completion reports a tracing error.
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

    /// Creates a scoped workflow using the concrete call-future proof supplied by `witness`.
    #[internal_api]
    pub(crate) fn into_workflow_with<'workflow, Fut>(
        self,
        witness: impl FnOnce(Self, Arc<Channel<P>>) -> Fut,
    ) -> Workflow<'workflow, P, O>
    where
        Fut: DeltaFuture<O> + 'workflow,
        P: 'workflow,
        O: 'workflow,
    {
        let requests = Arc::default();
        Workflow {
            body: Some(Box::pin(invoke_witness(witness, self, Arc::clone(&requests))) as _),
            requests,
        }
    }

    /// Runs the body while exposing its concrete future to [`Self::into_workflow_with`].
    #[internal_api]
    pub(crate) fn witness(self, channel: Arc<Channel<P>>) -> impl Future<Output = DeltaResult<O>>
    where
        F: for<'channel> AsyncFnOnce(&'channel Channel<P>) -> DeltaResult<O>,
    {
        let future = async move {
            let guard = WorkflowCompletionGuard;
            let output = (self.body)(channel.as_ref()).await;
            let _ = ManuallyDrop::new(guard);
            output.inspect_err(|err| error!(error = %err, "workflow failed"))
        };
        future.instrument(self.span)
    }
}

/// Logs when a workflow is dropped before producing its terminal result.
///
/// Early termination is ordinary for generators, but a [`Workflow`] must complete to produce a
/// useful output. The guard is disarmed with `ManuallyDrop` once the body completes. `Instrumented`
/// enters the workflow span while dropping the body, so the abandonment event retains its context.
struct WorkflowCompletionGuard;

impl Drop for WorkflowCompletionGuard {
    fn drop(&mut self) {
        error!(
            error = "abandoned",
            "workflow was abandoned while suspended"
        );
    }
}
