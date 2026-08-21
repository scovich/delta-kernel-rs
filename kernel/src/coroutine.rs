//! Connector-driven coroutines for kernel workflows.
//!
//! By design, kernel maintains a strict separation of concerns, delegating I/O and dataflow
//! operations to the connector. Connector-initiated kernel workflows that delegate work back to the
//! connector are designed as coroutines. Invoking a kernel workflow returns an enum implementing
//! [`Workflow`], with one output variant carrying the completed result and one or more operation
//! variants. Each operation variant carries an [`Operation`] for the connector to perform and a
//! [`Resume`] that connector uses to reply back to kernel. Resuming produces another value of the
//! same workflow type. Connectors drive the workflow in a loop until it produces either an error or
//! an output.
//!
//! Connectors define the [`Workflow`] enums they use, with variants using kernel-provided
//! [`Operation`] descriptors. Kernel workflows accept any workflow that has the correct output type
//! and that provides all operations the workflow requires. Alternatively, kernel provides legacy
//! entry points that accept an [`Engine`](crate::Engine) instead of a [`Workflow`], and which
//! drive the workflow to completion by dispatching operations to the `Engine`.
//!
//! Meanwhile, kernel workflows are implemented as `async` functions that call [`Channel::offload`]
//! or [`Channel::offload_paginated`] and `await` the connector's response. The coroutine
//! infrastructure is a fully internal, trivial, no-concurrency async executor; used only for the
//! stack ripping that coroutines require. Starting or resuming a workflow runs kernel synchronously
//! on the calling thread until the next delegated work item or completion, at which point the
//! method call returns the delegated work to the connector. The connector remains free to use its
//! own async runtime and scheduling strategy on top. In particular, work items are [`Send`] and
//! the workflow can migrate between threads and/or async tasks as it progresses.
//!
//! # Pagination
//!
//! Some operations that kernel delegates, such as file listing, technically has unbounded response
//! sizes even tho the responses are usually small. These operations are paginated, in order to give
//! engines full visibility and control over potentially unbounded work. The workflow enum defines a
//! paginated variant that includes a second field for connector-defined and managed pagination
//! state, and returns that state to kernel along with its initial response when resuming. Connector
//! decides the page size; if kernel needs more data than was supplied, it returns the connector's
//! state in a new operation request, which connector can use to resume the operation where it left
//! off. If the operation is still not complete, connector can resume kernel by passing an updated
//! state to kernel along with the next page.
//!
//! NOTE: Kernel may interleave requests for other operations while consuming the output of a
//! paginated operation (e.g. read a file whose status the listing returned). Connectors must ensure
//! that the state variable they pass to kernel contains all information necessary to resume the
//! operation, even if kernel requests other work between pages.
//!
//! # Error handling
//!
//! All calls are fallible:
//! * The initial workflow call returns `DeltaResult<Workflow>`, where `Err` indicates that kernel
//!   was unable to start the workflow (e.g. invalid inputs). Connector decides whether the failure
//!   is user-facing or not.
//! * The connector passes `DeltaResult<R>` to [`Resume::resume`] when resuming kernel, where `Err`
//!   indicates that the connector encountered an error executing the work item. Kernel decides
//!   whether/how that failure influences the workflow.
//! * The return value of [`Resume::resume`] is again `DeltaResult<Workflow>`, where `Err` indicates
//!   that kernel encountered an error while processing the connector's response or advancing the
//!   workflow. Connector again decides whether such failures are user-facing or not.
//!
//! Additionally, the connector can always choose to abandon a workflow by dropping the `Resume`
//! instead of invoking it. In particular, connector decides whether to resume kernel when a work
//! item fails, or just fail directly without further kernel involvement.
//!
//! # Example
//!
//! ```
//! use std::ops::Range;
//!
//! use delta_kernel::coroutine::{
//!     self, coroutine_workflow, drive_workflow, Operation, PaginatedOperation, Pagination,
//! };
//! use delta_kernel::DeltaResult;
//!
//! struct Double(u32);
//!
//! impl Operation for Double {
//!     type Response = u32;
//! }
//!
//! struct Numbers(Range<u32>);
//!
//! impl Operation for Numbers {
//!     type Response = Vec<u32>;
//! }
//!
//! impl PaginatedOperation for Numbers {}
//!
//! #[coroutine_workflow]
//! enum ExampleWorkflow {
//!     #[output]
//!     Done(Vec<u32>),
//!
//!     Double(Double),
//!
//!     #[paginated]
//!     Numbers(Numbers, Range<u32>),
//! }
//!
//! async fn kernel_workflow(
//!     mut channel: coroutine::Channel<ExampleWorkflow>,
//! ) -> DeltaResult<Vec<u32>> {
//!     let mut output = Vec::new();
//!     let mut pagination = Pagination::Start(Numbers(1..4));
//!     loop {
//!         let (page, cursor) = channel.offload_paginated(pagination).await?;
//!         for number in page {
//!             output.push(channel.offload(Double(number)).await?);
//!         }
//!         pagination = match cursor {
//!             Some(cursor) => Pagination::Continue(cursor),
//!             None => return Ok(output),
//!         };
//!     }
//! }
//!
//! let result = drive_workflow!(
//!     coroutine::start(kernel_workflow),
//!     |workflow| match workflow {
//!         ExampleWorkflow::Done(output) => break output,
//!         ExampleWorkflow::Double(Double(number), resume) => resume.resume(Ok(number * 2)),
//!         ExampleWorkflow::Numbers(pagination, resume) => resume.resume_with(|| {
//!             let mut state = match pagination {
//!                 Pagination::Start(Numbers(work)) => work,
//!                 Pagination::Continue(state) => state,
//!             };
//!             let page = state.by_ref().take(2).collect();
//!             let state = (!state.is_empty()).then_some(state);
//!             Ok((page, state))
//!         }),
//!     },
//! )?;
//!
//! assert_eq!(result, vec![2, 4, 6]);
//! # Ok::<(), delta_kernel::Error>(())
//! ```
pub(crate) mod engine;
pub mod listing;
pub mod read;
pub mod write;
use std::any::Any;
use std::fmt;
use std::future::Future;
use std::marker::PhantomData;

/// Generate the blanket implementation for an empty workflow capability trait.
///
/// The annotated trait's supertraits are the required operation capabilities.
///
/// With the macro:
///
/// ```
/// use delta_kernel::coroutine::{coroutine_capabilities, CanRequest, Operation};
///
/// struct Read;
///
/// impl Operation for Read {
///     type Response = ();
/// }
///
/// impl Operation for Write {
///     type Response = ();
/// }
///
/// #[coroutine_capabilities]
/// trait WorkflowCapabilities: CanRequest<Read> + CanRequest<Write> {}
/// ```
///
/// The equivalent manual definition would be:
///
/// ```
/// # use delta_kernel::coroutine::{CanRequest, Operation};
/// #
/// # struct Read;
/// #
/// # impl Operation for Read {
/// #     type Response = ();
/// # }
/// #
/// # trait WorkflowCapabilities: CanRequest<Read> + CanRequest<Write> {}
/// #
/// impl<W> WorkflowCapabilities for W where W: CanRequest<Read> + CanRequest<Write> {}
/// ```
pub use delta_kernel_derive::coroutine_capabilities;
/// Expands a connector-defined workflow enum from kernel [`Operation`] descriptors, and derive
/// appropriate implementations of [`Workflow`] [`CanRequest`] and [`CanRequestPaginated`] for
/// its variants.
///
/// Exactly one single-field variant must be marked `#[output]`; it supplies
/// [`Workflow::Output`] in the derived [`Workflow`].
///
/// To express a one-shot operation, define a tuple variant containing the operation type. The
/// macro adds the corresponding [`Resume`] field and derives a matching impl [`CanRequest`].
///
/// To express a paginated operation, mark a tuple variant `#[paginated]` and give it the
/// operation type followed by the connector state type. The macro adds the corresponding
/// [`PaginatedResume`] field, wraps the operation in [`Pagination`], and derives a matching
/// impl [`CanRequestPaginated`].
///
/// # Example
///
/// With the macro:
///
/// ```
/// use std::ops::Range;
///
/// use delta_kernel::coroutine::{
///     coroutine_workflow, Operation, PaginatedOperation,
/// };
///
/// struct Double(u32);
///
/// impl Operation for Double {
///     type Response = u32;
/// }
///
/// struct Numbers(Range<u32>);
///
/// impl Operation for Numbers {
///     type Response = Vec<u32>;
/// }
///
/// impl PaginatedOperation for Numbers {}
///
/// #[coroutine_workflow]
/// enum ExampleWorkflow {
///     #[output]
///     Done(Vec<u32>),
///
///     Double(Double),
///
///     #[paginated]
///     Numbers(Numbers, Range<u32>),
/// }
/// ```
///
/// Equivalent manual definitions:
///
/// ```
/// # use std::ops::Range;
/// #
/// # use delta_kernel::coroutine::{Operation, PaginatedOperation};
/// # struct Double(u32);
/// #
/// # impl Operation for Double {
/// #     type Response = u32;
/// # }
/// #
/// # struct Numbers(Range<u32>);
/// #
/// # impl Operation for Numbers {
/// #     type Response = Vec<u32>;
/// # }
/// #
/// # impl PaginatedOperation for Numbers {}
/// #
/// use delta_kernel::coroutine::{
///     CanRequest, CanRequestPaginated, PaginatedResume, Pagination, Resume, Workflow,
/// };
///
/// enum ExampleWorkflow {
///     Done(Vec<u32>),
///     Double(Double, Resume<ExampleWorkflow, Double>),
///     Numbers(
///         Pagination<Numbers, Range<u32>>,
///         PaginatedResume<ExampleWorkflow, Numbers, Range<u32>>,
///     ),
/// }
///
/// impl Workflow for ExampleWorkflow {
///     type Output = Vec<u32>;
///
///     fn finish(output: Self::Output) -> Self {
///         Self::Done(output)
///     }
/// }
///
/// impl CanRequest<Double> for ExampleWorkflow {
///     fn request(operation: Double, resume: Resume<Self, Double>) -> Self {
///         Self::Double(operation, resume)
///     }
/// }
///
/// impl CanRequestPaginated<Numbers> for ExampleWorkflow {
///     type State = Range<u32>;
///
///     fn request(
///         pagination: Pagination<Numbers, Self::State>,
///         resume: PaginatedResume<Self, Numbers, Self::State>,
///     ) -> Self {
///         Self::Numbers(pagination, resume)
///     }
/// }
/// ```
pub use delta_kernel_derive::coroutine_workflow;

/// Drives a kernel workflow to its output state.
///
/// `start` and every non-output arm in `body` must return [`DeltaResult`] containing the next
/// workflow state. An output arm completes the generated loop with `break output`.
///
/// The invocation
///
/// ```text
/// drive_workflow!(start, |workflow| match workflow {
///     Workflow::Done(output) => break output,
///     Workflow::Read(operation, resume) => resume.resume(read(operation)),
/// })
/// ```
///
/// expands to the equivalent of:
///
/// ```text
/// (|| -> DeltaResult<_> {
///     let mut workflow = start?;
///     Ok(loop {
///         workflow = (match workflow {
///             Workflow::Done(output) => break output,
///             Workflow::Read(operation, resume) => resume.resume(read(operation)),
///         })?;
///     })
/// })()
/// ```
#[macro_export]
#[doc(hidden)]
macro_rules! __drive_workflow {
    ($start:expr, |$workflow:ident| $body:expr $(,)?) => {{
        (|| -> $crate::DeltaResult<_> {
            let mut $workflow = $start?;
            Ok(loop {
                $workflow = ($body)?;
            })
        })()
    }};
}

use delta_kernel_derive::internal_api;
use genawaiter2::sync::{Co, Gen, GenBoxed};
use genawaiter2::GeneratorState;
use tracing::Instrument as _;

#[doc(inline)]
pub use crate::__drive_workflow as drive_workflow;
use crate::{DeltaResult, Error};

// === Implementation ===
//
// See module-level documentation for an overview of kernel/connector communication via coroutines.
//
// Rust compiles kernel's async workflow function into a `Future` that stores kernel's locals and
// control flow across each `.await` call; `genawaiter2` wraps and polls that future in a
// `Generator`, an object that can suspend by yielding a value and later resumed with a
// connector-provide value. Its yield and resume types are fixed for the lifetime of the
// generator. When the workflow suspends, the generator owns the future and all state required to
// continue it; in that state, the generator is the continuation. While kernel runs, `advance` owns
// it. While the connector performs work, the request's `Resume` owns it. No hidden kernel-owned
// state connects the two calls.
//
// Each offload selects one physical operation variant and its typed contract. To fit heterogeneous
// contracts through the generator's fixed transport types, each pending operation is boxed behind
// `Box<dyn PendingRequest>` and each inbound response behind `Box<dyn Any>`. The concrete pending
// type and its `CanRequest` implementation preserve those relationships.
//
// From `offload` to the next operation state:
//
// - `offload` packages `Op` in a concrete `TypedPending<Op>`, erased as `dyn PendingRequest` so it
//   can travel through the generator's single yield type. `yield_(...).await` then suspends the
//   workflow and returns that pending request to `advance`.
// - `advance` still owns the now-suspended generator. Calling `PendingRequest::attach` dispatches
//   back to `TypedPending<Op>`, moves the generator into a typed `Resume<W, Op>`, and invokes `W`'s
//   `CanRequest<Op>` implementation to produce the operation variant.
// - `advance` returns `W`, giving the connector both its concrete work and exclusive ownership of
//   the continuation that accepts the matching response.
//
// From `Resume::resume` to the return from `.await`:
//
// - The connector matches the operation variant and passes `DeltaResult<Op::Response>` to its
//   `Resume<W, Op>`.
// - `Resume::resume` consumes the continuation. It erases a successful `R` behind `Any`, the
//   generator's single resume type, and gives both generator and response back to `advance`.
// - Resuming the generator delivers the erased response to the suspended `yield_(...).await`.
//   `Channel::offload` propagates a connector error or downcasts the success value to `R`, then
//   returns it to the ordinary async workflow.
// - Kernel runs until the workflow yields again or completes. On completion, `start` has already
//   mapped the producer's output through `Workflow::finish`.
//
// The downcast is structurally guaranteed: `TypedPending<Op>` constructs only a `Resume<W, Op>`,
// which accepts only `Op::Response` before resuming the exact generator suspended by the matching
// `offload`.
//
// Pagination applies the same pattern to `(Op, Op::Response, S)`;
// `TypedPaginatedPending<Op, S>` preserves that relationship while suspended.

/// Erases a typed request contract only while the generator is suspended.
trait PendingRequest<W>: Send {
    fn attach(self: Box<Self>, generator: Generator<W>) -> W;
}

/// Erases heterogeneous request contracts into the generator's single yield type.
type Pending<W> = Box<dyn PendingRequest<W>>;

/// Erases successful connector responses into the generator's single resume type.
type ErasedResponse = DeltaResult<Box<dyn Any + Send>>;

/// Boxes workflow futures so every continuation can store the same concrete generator type.
type Generator<W> = GenBoxed<Pending<W>, ErasedResponse, DeltaResult<W>>;

/// Kernel-side channel used by a coroutine workflow to yield connector requests.
#[internal_api]
pub(crate) struct Channel<W>(Co<Pending<W>, ErasedResponse>);

impl<W> Channel<W>
where
    W: Send + 'static,
{
    /// Yield one `Op` request carrying `work`.
    ///
    /// Returns the connector's successful response after the workflow is resumed.
    ///
    /// Errors if the connector resumes the workflow with an error. An internal error is returned
    /// if the resumed value does not have `Op::Response`'s type.
    pub async fn offload<Op>(&mut self, operation: Op) -> DeltaResult<Op::Response>
    where
        Op: Operation,
        W: CanRequest<Op>,
    {
        let pending: Pending<W> = Box::new(TypedPending { operation });
        suspend(self, pending).await
    }

    /// Yield one page request for `Op`.
    ///
    /// `request` starts pagination with [`Pagination::Start`] or continues it with connector state
    /// from a preceding response. Returns one page and optional state for another request (None if
    /// exhausted, Some if more pages remain).
    ///
    /// Errors if the connector resumes the workflow with an error. An internal error is returned
    /// if the resumed value does not have the expected response-and-state type.
    pub async fn offload_paginated<Op>(
        &mut self,
        request: Pagination<Op, <W as CanRequestPaginated<Op>>::State>,
    ) -> DeltaResult<(Op::Response, Option<<W as CanRequestPaginated<Op>>::State>)>
    where
        Op: PaginatedOperation,
        W: CanRequestPaginated<Op>,
    {
        let pending: Pending<W> =
            Box::new(
                TypedPaginatedPending::<Op, <W as CanRequestPaginated<Op>>::State> { request },
            );
        suspend(self, pending).await
    }
}

/// Initial operation or connector state for one pagination request.
#[derive(Debug)]
pub enum Pagination<Op, S> {
    /// Starts pagination with the operation request.
    Start(Op),
    /// Continues pagination with connector state returned by the preceding response.
    Continue(S),
}

/// Describes one low-level operation that kernel may delegate to a connector.
pub trait Operation: Send + Sized + 'static {
    /// Successful response returned by the connector.
    type Response: Send + 'static;
}

/// A top-level kernel workflow represented by its current state.
pub trait Workflow: Send + Sized + 'static {
    /// Value produced when the workflow completes.
    type Output: Send + 'static;

    /// Construct the workflow's output state.
    fn finish(output: Self::Output) -> Self;
}

/// Resume handle for a one-shot operation.
pub type Resume<W, Op> = TypedResume<W, <Op as Operation>::Response>;

/// Resume handle for a paginated operation using connector state `S`.
pub type PaginatedResume<W, Op, S> = TypedResume<W, (<Op as Operation>::Response, Option<S>)>;

/// Marks an [`Operation`] as supporting pagination.
pub trait PaginatedOperation: Operation {}

/// Allows a workflow enum to represent a one-shot operation.
///
/// [`Channel::offload`] uses this trait only to inject typed work and its [`Resume`] into the
/// connector's workflow enum. Connectors may implement it directly or use [`coroutine_workflow`].
pub trait CanRequest<Op: Operation>: Send + Sized + 'static {
    /// Construct the workflow variant carrying `operation` and `resume`.
    fn request(operation: Op, resume: Resume<Self, Op>) -> Self;
}

/// Allows a workflow enum to represent a paginated operation.
///
/// [`Channel::offload_paginated`] uses this trait only to inject one typed page request and its
/// [`Resume`] into the connector's workflow enum.
pub trait CanRequestPaginated<Op: PaginatedOperation>: Send + Sized + 'static {
    /// Connector state retained between pages.
    type State: Send + 'static;

    /// Construct the workflow variant carrying `pagination` and `resume`.
    fn request(
        pagination: Pagination<Op, Self::State>,
        resume: PaginatedResume<Self, Op, Self::State>,
    ) -> Self;
}

/// Continuation typed to accept a response of type `R` for workflow `W`.
///
/// Connector-facing workflow variants normally use [`Resume`] or [`PaginatedResume`], which derive
/// `R` from an [`Operation`].
pub struct TypedResume<W, R> {
    generator: Generator<W>,
    response_type: PhantomData<R>,
}

impl<W: Send + 'static, R: Send + 'static> TypedResume<W, R> {
    /// Submit `response` and run kernel until its next workflow state.
    ///
    /// Returns the next operation state or the workflow's output state.
    pub fn resume(self, response: DeltaResult<R>) -> DeltaResult<W> {
        let response = response.map(|response| Box::new(response) as _);
        advance(self.generator, response)
    }

    /// Produce a response and submit its result to the suspended kernel workflow.
    ///
    /// Errors produced by `response` are delivered to kernel rather than returned directly by the
    /// connector driver; this method's return value comes directly from the resumed kernel.
    pub fn resume_with(self, response: impl FnOnce() -> DeltaResult<R>) -> DeltaResult<W> {
        self.resume(response())
    }
}

impl<W, R> fmt::Debug for TypedResume<W, R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Resume")
    }
}

/// Start `workflow` and run it until its first operation or completion.
#[internal_api]
pub(crate) fn start<W, F, Fut>(workflow: F) -> DeltaResult<W>
where
    W: Workflow,
    F: FnOnce(Channel<W>) -> Fut + Send + 'static,
    Fut: Future<Output = DeltaResult<W::Output>> + Send + 'static,
{
    let span = tracing::Span::current();
    let generator = Gen::new_boxed(async move |channel| {
        async move {
            workflow(Channel(channel))
                .await
                .inspect_err(|err| tracing::error!(error = %err, "kernel workflow failed"))
                .map(W::finish)
        }
        .instrument(span)
        .await
    });

    // The Generator API starts a new coroutine by "resuming" it from a fake internal "yield" with
    // a dummy initial "response" that is silently dropped before the coroutine starts running.
    advance(generator, Err(Error::InternalError(String::new())))
}

/// Preserves a typed one-shot operation until it can attach the generator and construct `W`.
struct TypedPending<Op: Operation> {
    operation: Op,
}

impl<Op: Operation, W: CanRequest<Op>> PendingRequest<W> for TypedPending<Op> {
    fn attach(self: Box<Self>, generator: Generator<W>) -> W {
        let resume = TypedResume {
            generator,
            response_type: PhantomData,
        };
        W::request(self.operation, resume)
    }
}

/// Preserves a typed paginated work/response/state relationship while suspended.
struct TypedPaginatedPending<Op: PaginatedOperation, S> {
    request: Pagination<Op, S>,
}

impl<W, Op, S> PendingRequest<W> for TypedPaginatedPending<Op, S>
where
    W: CanRequestPaginated<Op, State = S>,
    Op: PaginatedOperation,
    S: Send + 'static,
{
    fn attach(self: Box<Self>, generator: Generator<W>) -> W {
        let resume = TypedResume {
            generator,
            response_type: PhantomData,
        };
        <W as CanRequestPaginated<Op>>::request(self.request, resume)
    }
}

async fn suspend<W, R>(channel: &mut Channel<W>, pending: Pending<W>) -> DeltaResult<R>
where
    W: Send + 'static,
    R: Send + 'static,
{
    let response = channel.0.yield_(pending).await?;
    response.downcast().map(|response| *response).map_err(|_| {
        Error::internal_error("coroutine resumed with an unexpected connector response type")
    })
}

// Advances the generator by `response` to its next workflow state.
fn advance<W: Send + 'static>(
    mut generator: Generator<W>,
    response: ErasedResponse,
) -> DeltaResult<W> {
    match generator.resume_with(response) {
        GeneratorState::Yielded(pending) => Ok(pending.attach(generator)),
        GeneratorState::Complete(result) => result,
    }
}

#[cfg(test)]
mod capability_tests {
    use std::ops::Range;

    use super::*;

    struct Echo(String);

    impl Operation for Echo {
        type Response = usize;
    }

    struct Numbers(Range<u32>);

    impl Operation for Numbers {
        type Response = Vec<u32>;
    }

    impl PaginatedOperation for Numbers {}

    #[cfg(any())]
    enum Disabled {}

    #[cfg(any())]
    impl Operation for Disabled {
        type Response = ();
    }

    type Output = (usize, Vec<u32>);

    #[coroutine_workflow]
    enum TestWorkflow {
        #[output]
        Done(Output),

        Echo(Echo),

        #[cfg(test)]
        #[paginated]
        Numbers(Numbers, Range<u32>),

        #[cfg(any())]
        Disabled(Disabled),
    }

    #[coroutine_capabilities]
    trait TestCapabilities: CanRequest<Echo> + CanRequestPaginated<Numbers> {}

    #[derive(Default)]
    struct TestConnector {
        echo_calls: usize,
    }

    impl TestConnector {
        fn dispatch(&mut self, workflow: TestWorkflow) -> DeltaResult<TestWorkflow> {
            match workflow {
                TestWorkflow::Done(_) => Err(Error::internal_error(
                    "completed workflow cannot be dispatched",
                )),
                TestWorkflow::Echo(Echo(work), resume) => resume.resume_with(|| {
                    self.echo_calls += 1;
                    Ok(work.len())
                }),
                TestWorkflow::Numbers(pagination, resume) => resume.resume_with(|| {
                    let mut state = match pagination {
                        Pagination::Start(Numbers(work)) => work,
                        Pagination::Continue(state) => state,
                    };
                    let page = state.by_ref().take(2).collect();
                    let state = (!state.is_empty()).then_some(state);
                    Ok((page, state))
                }),
            }
        }
    }

    async fn workflow<W>(mut channel: Channel<W>) -> DeltaResult<Output>
    where
        W: TestCapabilities,
    {
        let length = channel.offload(Echo("kernel".to_string())).await?;

        let mut numbers = Vec::new();
        let mut pagination = Pagination::Start(Numbers(1..4));
        loop {
            let (page, cursor) = channel.offload_paginated(pagination).await?;
            numbers.extend(page);
            pagination = match cursor {
                Some(cursor) => Pagination::Continue(cursor),
                None => break,
            };
        }
        Ok((length, numbers))
    }

    #[test]
    fn generated_request_bundle_drives_stateful_connector() {
        let mut connector = TestConnector::default();
        let result = drive_workflow!(start(workflow), |workflow| match workflow {
            TestWorkflow::Done(output) => break output,
            request => connector.dispatch(request),
        },)
        .unwrap();

        assert_eq!(result, (6, vec![1, 2, 3]));
        assert_eq!(connector.echo_calls, 1);
        assert!(
            connector
                .dispatch(TestWorkflow::Done((0, Vec::new())))
                .is_err(),
            "completed workflows cannot be dispatched"
        );
    }
}
