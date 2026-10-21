//! Internal workflow construction and lifecycle instrumentation.

use std::future::Future;
use std::mem::ManuallyDrop;
use std::ops::AsyncFnOnce;
use std::sync::Arc;

use delta_kernel_derive::internal_api;
use tracing::{error, Instrument as _, Span};

use super::witness::invoke_witness;
use super::{Channel, KernelFuture, Workflow};
use crate::utils::PhantomType;
use crate::KernelResult;

/// Hidden expansion state for [`workflow!`](super::workflow) and
/// [`static_workflow!`](super::static_workflow).
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
        F: for<'channel> AsyncFnOnce(&'channel Channel<P>) -> KernelResult<O>,
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
        Fut: KernelFuture<O> + 'workflow,
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
    pub(crate) fn witness(self, channel: Arc<Channel<P>>) -> impl Future<Output = KernelResult<O>>
    where
        F: for<'channel> AsyncFnOnce(&'channel Channel<P>) -> KernelResult<O>,
    {
        let guard = WorkflowCompletionGuard;
        let future = async move {
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
