//! Connector-driven coroutines for kernel workflows.
//!
//! By design, kernel maintains a strict separation of concerns, delegating I/O and dataflow
//! operations to the connector. Connector-initiated kernel workflows that delegate work back to
//! the connector are designed as coroutines. The initial call returns a [`ControlFlow`] that
//! communicates the next step to connector:
//! * `Break` indicates the requested workflow completed without delegation, and yields the
//!   workflow's final result.
//! * `Continue` indicates that kernel delegated a work item for the connector to execute on its
//!   behalf, along with a [`Resume`] that routes the connector's response back to kernel. Resuming
//!   produces another [`ControlFlow`] of the same type as before, allowing the kernel to delegate
//!   additional work items back to the connector as needed.
//!
//! Connectors define request enums from kernel-provided [`Operation`] descriptors. Kernel workflows
//! declare the exact request capabilities they require, while a connector enum may contain a
//! superset. Each request variant carries typed work plus a matching [`Resume`]. The
//! [`drive_to_completion!`] macro repeatedly dispatches `Continue` cases and returns the final
//! `Break` output.
//!
//! Meanwhile, kernel workflows are implemented as `async` functions that [`offload`] work items to
//! the connector and `await` responses on the resulting futures. The coroutine infrastructure is a
//! trivial, no-concurrency async executor used only for the stack ripping that coroutines require.
//! Starting or resuming a workflow runs kernel synchronously on the calling thread until the next
//! delegated work item or completion; delegated work itself runs only after kernel returns. The
//! connector remains free to use its own async runtime and scheduling strategy. Work items are
//! [`Send`] and can be resumed from different threads or async tasks than they were delivered to.
//!
//! # Error handling
//!
//! All calls are fallible:
//! * The initial workflow call returns `DeltaResult<ControlFlow>`, where `Err` indicates that
//!   kernel determined up front that the workflow cannot succeed (e.g. invalid inputs). Connector
//!   decides whether the failure is user-facing or not.
//! * The connector passes `DeltaResult<R>` to [`Resume::resume`] when resuming kernel, where `Err`
//!   indicates that the connector encountered an error executing the work item. Kernel decides
//!   whether/how that failure influences the workflow.
//! * The return value of [`Resume::resume`] is again `DeltaResult<ControlFlow>`, where `Err`
//!   indicates that kernel encountered an error while processing the connector's response or
//!   advancing the workflow. Connector decides whether the failure is user-facing or not.
//!
//! Additionally, the connector can always choose to abandon a request by dropping the `Resume`
//! instead of invoking it. In particular, connector decides whether to forward a work item error
//! back to kernel, or just fail directly without further kernel involvement.
//!
//! # Example
//!
//! ```
//! use std::ops::Range;
//!
//! use delta_kernel::coroutine::{
//!     self, coroutine_request, Operation, PaginatedOperation, Pagination, Supports,
//!     SupportsPaginated,
//! };
//! use delta_kernel::DeltaResult;
//!
//! enum Length {}
//!
//! impl Operation for Length {
//!     type Work = String;
//!     type Response = usize;
//! }
//!
//! enum Numbers {}
//!
//! impl Operation for Numbers {
//!     type Work = Range<u32>;
//!     type Response = Vec<u32>;
//! }
//!
//! impl PaginatedOperation for Numbers {}
//!
//! #[coroutine_request(output = (usize, Vec<u32>))]
//! enum Request {
//!     Length(Length),
//!
//!     #[paginated(state = Range<u32>)]
//!     Numbers(Numbers),
//! }
//!
//! #[derive(Default)]
//! struct Connector {
//!     length_calls: usize,
//! }
//!
//! impl Supports<Length> for Connector {
//!     fn execute(&mut self, work: String) -> DeltaResult<usize> {
//!         self.length_calls += 1;
//!         Ok(work.len())
//!     }
//! }
//!
//! impl SupportsPaginated<Numbers> for Connector {
//!     type State = Range<u32>;
//!
//!     fn start(&mut self, work: Range<u32>) -> DeltaResult<Self::State> {
//!         Ok(work)
//!     }
//!
//!     fn next(
//!         &mut self,
//!         mut state: Self::State,
//!     ) -> DeltaResult<(Vec<u32>, Option<Self::State>)> {
//!         let page = state.by_ref().take(2).collect();
//!         let state = (!state.is_empty()).then_some(state);
//!         Ok((page, state))
//!     }
//! }
//!
//! async fn kernel_workflow(
//!     mut channel: coroutine::Channel<(usize, Vec<u32>), Request>,
//! ) -> DeltaResult<(usize, Vec<u32>)> {
//!     let length = coroutine::offload(
//!         &mut channel,
//!         Request::Length,
//!         "kernel".to_string(),
//!     )
//!     .await?;
//!
//!     let mut numbers = Vec::new();
//!     let mut pagination = Pagination::Start(1..4);
//!     loop {
//!         let (page, cursor) =
//!             coroutine::offload_paginated(&mut channel, Request::Numbers, pagination).await?;
//!         numbers.extend(page);
//!         pagination = match cursor {
//!             Some(cursor) => Pagination::Continue(cursor),
//!             None => return Ok((length, numbers)),
//!         };
//!     }
//! }
//!
//! let mut connector = Connector::default();
//! let result = coroutine::drive_to_completion!(
//!     coroutine::start(kernel_workflow),
//!     |request| request.resume(&mut connector),
//! )?;
//!
//! assert_eq!(result, (6, vec![1, 2, 3]));
//! assert_eq!(connector.length_calls, 1);
//! # Ok::<(), delta_kernel::Error>(())
//! ```
pub(crate) mod engine;
pub mod listing;
pub mod read;
use std::any::Any;
use std::fmt;
use std::future::Future;
use std::marker::PhantomData;
use std::ops::ControlFlow;

/// Add a blanket implementation to a workflow capability trait.
///
/// Apply this attribute to an empty trait whose supertraits are the exact [`CanRequest`] and
/// [`CanRequestPaginated`] capabilities required by a kernel workflow. The macro preserves the
/// trait and implements it for every request enum satisfying those bounds.
///
/// # Example
///
/// ```
/// use delta_kernel::coroutine::{
///     coroutine_capabilities, CanRequest, CanRequestPaginated, Operation, PaginatedOperation,
/// };
///
/// enum Head {}
///
/// impl Operation for Head {
///     type Work = String;
///     type Response = usize;
/// }
///
/// enum Numbers {}
///
/// impl Operation for Numbers {
///     type Work = Vec<u32>;
///     type Response = Vec<u32>;
/// }
///
/// impl PaginatedOperation for Numbers {}
///
/// #[coroutine_capabilities]
/// trait InspectCapabilities<O>:
///     CanRequest<O, Head> + CanRequestPaginated<O, Numbers>
/// where
///     O: Send + 'static,
/// {
/// }
/// ```
pub use delta_kernel_derive::coroutine_capabilities;
/// Expand a connector-defined request enum from kernel [`Operation`] descriptors.
///
/// Each source variant must contain exactly one operation tag. Ordinary variants become
/// one-shot `(Work, Resume)` variants. Mark a paginated variant with
/// `#[paginated(state = ConnectorState)]`; it becomes a
/// `(Pagination<Work, ConnectorState>, Resume<(Response, Option<ConnectorState>)>)` variant.
///
/// The macro also implements [`CanRequest`] or [`CanRequestPaginated`] for each operation and
/// generates a `resume(&mut connector)` method that dispatches through [`Supports`] or
/// [`SupportsPaginated`].
///
/// The required `output` argument is the final output type of the kernel workflow. Direct
/// `#[cfg(...)]` attributes on variants are supported. `#[cfg_attr(...)]` is not yet
/// supported.
///
/// # Example
///
/// ```
/// use std::ops::Range;
///
/// use delta_kernel::coroutine::{
///     coroutine_request, Operation, PaginatedOperation,
/// };
///
/// enum Head {}
///
/// impl Operation for Head {
///     type Work = String;
///     type Response = usize;
/// }
///
/// enum Numbers {}
///
/// impl Operation for Numbers {
///     type Work = Range<u32>;
///     type Response = Vec<u32>;
/// }
///
/// impl PaginatedOperation for Numbers {}
///
/// #[coroutine_request(output = (usize, Vec<u32>))]
/// enum Requests {
///     Head(Head),
///
///     #[paginated(state = Range<u32>)]
///     Numbers(Numbers),
/// }
/// ```
pub use delta_kernel_derive::coroutine_request;
use delta_kernel_derive::internal_api;
use genawaiter2::sync::{Co, Gen, GenBoxed};
use genawaiter2::GeneratorState;

use crate::{DeltaResult, Error};

/// Drives a kernel coroutine through zero or more work items until it returns its final output.
///
/// The first argument produces the initial `DeltaResult<ControlFlow>`, which immediately terminates
/// the loop with a `DeltaResult` on `Err` or `Ok(ControlFlow::Break)`. Otherwise, the provided
/// handler code receives the delegated work item from `Ok(ControlFlow::Continue)`. The handler
/// returns the `DeltaResult<ControlFlow>` produced by that work item's [`Resume::resume`], which
/// becomes the input of the next loop iteration.
///
/// # Example
///
/// ```
/// # use std::ops::ControlFlow;
/// # use delta_kernel::coroutine::{self, Channel};
/// # use delta_kernel::DeltaResult;
/// use delta_kernel::coroutine::Resume;
///
/// /// The kernel-provided workflow-specific `Resume` type for `QuotientAsStringRequest`.
/// pub type QuotientAsStringResume<R> = Resume<String, QuotientAsStringRequest, R>;
///
/// /// The kernel-provided workflow-specific request enum for `kernel_quotient_as_string`.
/// pub enum QuotientAsStringRequest {
///     Divide((u32, u32), QuotientAsStringResume<f64>),
///     ToString(f64, QuotientAsStringResume<String>),
/// }
/// # async fn kernel_quotient_as_string_impl(
/// #     mut channel: Channel<String, QuotientAsStringRequest>,
/// #     dividend: u32,
/// #     divisor: u32,
/// # ) -> DeltaResult<String> {
/// #     let quotient = coroutine::offload(
/// #         &mut channel,
/// #         QuotientAsStringRequest::Divide,
/// #         (dividend, divisor),
/// #     )
/// #     .await?;
/// #     coroutine::offload(
/// #         &mut channel,
/// #         QuotientAsStringRequest::ToString,
/// #         quotient,
/// #     )
/// #     .await
/// # }
/// # fn kernel_quotient_as_string(
/// #     dividend: u32,
/// #     divisor: u32,
/// # ) -> DeltaResult<ControlFlow<String, QuotientAsStringRequest>> {
/// #     coroutine::start(move |channel| {
/// #         kernel_quotient_as_string_impl(channel, dividend, divisor)
/// #     })
/// # }
///
/// // Use the macro to drive the `kernel_quotient_as_string` workflow.
/// let output = coroutine::drive_to_completion!(
///     kernel_quotient_as_string(1, 4),
///     |request| match request {
///     QuotientAsStringRequest::Divide((dividend, divisor), resume) => {
///         resume.resume(Ok(dividend as f64 / divisor as f64))
///     }
///     QuotientAsStringRequest::ToString(value, resume) => {
///         resume.resume(Ok(value.to_string()))
///     }
/// })?;
/// assert_eq!(output, "0.25");
/// # Ok::<(), delta_kernel::Error>(())
/// ```
#[macro_export]
#[doc(hidden)]
macro_rules! __drive_to_completion {
    ($step:expr, |$request:ident| $handler:expr $(,)?) => {{
        let mut next = $step;
        loop {
            match next {
                ::std::result::Result::Ok(::std::ops::ControlFlow::Continue($request)) => {
                    next = $handler;
                }
                ::std::result::Result::Ok(::std::ops::ControlFlow::Break(output)) => {
                    break ::std::result::Result::Ok(output);
                }
                ::std::result::Result::Err(error) => {
                    break ::std::result::Result::Err(error);
                }
            }
        }
    }};
}

#[doc(inline)]
pub use crate::__drive_to_completion as drive_to_completion;

// === Implementation ===
//
// See module-level documentation for an overview of kernel/connector communication via coroutines.
//
// A workflow-specific request enum `Q` (defined by kernel) serves as the shared contract between
// kernel and connector. A one-shot variant pairs concrete work `W` with a `Resume<R>`. A paginated
// variant instead pairs `Pagination<W, S>` with `Resume<(R, Option<S>)>`, preserving the ordinary
// one-shot resume mechanism while distinguishing initial work from continuation state.
//
// Around that shared contract, this module deliberately presents different strongly typed APIs to
// kernel and connector authors. Kernel calls `offload(&mut channel, Q::Variant, work: W).await?`
// and receives a concrete response of type `R`. Because it is a tuple variant, the variant's name
// is also a constructor of type `fn(W, Resume<O, Q, R>) -> Q`, where `O` is the final output type
// of the workflow, `Q` is the workflow request enum, and `R` is the successful response type of the
// selected work item. Kernel supplies both the variant name/constructor and the work `W` that
// initializes its first field, while this module supplies the second field `Resume`. The latter
// encapsulates a continuation that resumes kernel at its `await` point, so the kernel-side workflow
// implementation reads as ordinary sequential async code.
//
// Meanwhile, connector code sees `DeltaResult<ControlFlow<O, Q>>`. Matching a `Q` variant tells the
// connector both what to do and what response type kernel expects. Its `Resume<R>` accepts the
// typed response, consumes itself, resumes kernel, and returns kernel's next step. For pagination,
// connector additionally receives `Option<S>` and returns either `More(R, S)` or `Done(R)`.
//
// Rust compiles the async workflow into a `Future` that stores kernel's locals and control flow
// across each `.await`. `genawaiter2` wraps and polls that future in a `Generator`, an object that
// can suspend by yielding a value and later continue with a resumed value. Its yield and resume
// types are fixed for the lifetime of the generator. When the workflow suspends, the generator owns
// the future and all state required to continue it; in that state, the generator is the
// continuation. While kernel runs, `advance` owns it. While the connector performs work, the
// request's `Resume` owns it. No additional kernel-owned state table connects the two calls.
//
// Each offload selects one variant of `Q` and its corresponding typed contract. Multiple variants
// could have the same signature while still representing logically distinct work items. To fit
// heterogeneous contracts through the generator's fixed transport types, this module erases each
// outbound request behind `dyn PendingRequest` and each inbound response behind `Any`. Typed
// constructors preserve the relationships so concrete types are restored before kernel or
// connector handles them.
//
// From `offload` to `Continue`:
//
// - `offload<W, R>` packages `work: W` with the request variant constructor `fn(W, Resume<O, Q, R>)
//   -> Q`. The constructor's signature preserves the relationship between this work and its
//   expected response.
// - The package is a concrete `TypedPending<W, R>` instance, erased as `dyn PendingRequest` so it
//   can travel through the generator's single yield type. `yield_(...).await` then suspends the
//   workflow and returns that pending request to `advance`.
// - `advance` still owns the now-suspended generator. Calling `PendingRequest::attach` dispatches
//   back to the concrete `TypedPending<W, R>`, moves the generator into a typed `Resume<R>`, and
//   invokes the stored constructor to produce `Q`.
// - `advance` returns `Continue(Q)`, giving the connector both its concrete work and exclusive
//   ownership of the continuation that accepts the matching response.
//
// From `Resume::resume` to the return from `.await`:
//
// - The connector matches the request variant and passes `DeltaResult<R>` to its `Resume<R>`.
// - `Resume::resume` consumes the continuation. It erases a successful `R` behind `Any`, the
//   generator's single resume type, and gives both generator and response back to `advance`.
// - Resuming the generator delivers the erased response to the suspended `yield_(...).await`.
//   `offload` propagates a connector error or downcasts the success value to `R`, then returns it
//   to the ordinary async workflow.
// - Kernel runs until the workflow yields again or completes, which `advance` translates into
//   `Continue(next_request)` or `Break(final_output)`, respectively.
//
// The downcast is structurally guaranteed: `TypedPending<W, R>` can construct only a `Resume<R>`,
// and that `Resume<R>` can accept only an `R` before resuming the exact generator suspended by the
// matching `offload<W, R>`.
//
// Pagination applies the same pattern to `(W, R, S)`. `PaginationCursor<S>` retains typed connector
// state in the async workflow, while `PaginatedPending<W, R, S>` restores it to `Pagination`.

/// Erases a typed request contract only while the generator is suspended.
trait PendingRequest<O, Q>: Send {
    fn attach(self: Box<Self>, generator: Generator<O, Q>) -> Q;
}

/// Erases heterogeneous request contracts into the generator's single yield type.
type Pending<O, Q> = Box<dyn PendingRequest<O, Q>>;

/// Erases successful connector responses into the generator's single resume type.
type ErasedResponse = DeltaResult<Box<dyn Any + Send>>;

/// Boxes workflow futures so every continuation can store the same concrete generator type.
type Generator<O, Q> = GenBoxed<Pending<O, Q>, ErasedResponse, DeltaResult<O>>;

/// The Generator API starts a new coroutine by "resuming" it with a dummy initial "response" that
/// is unobservable to the coroutine itself (because it did not actually resume from a yield).
#[allow(dead_code)]
const UNOBSERVABLE_INITIAL_RESPONSE: ErasedResponse = Err(Error::InternalError(String::new()));

/// Kernel-side channel used by a coroutine workflow to yield connector requests.
#[allow(dead_code)]
#[internal_api]
pub(crate) struct Channel<O, Q>(Co<Pending<O, Q>, ErasedResponse>);

/// Connector state retained by kernel between pagination requests.
///
/// The state field is private, so kernel can only move or drop a cursor. Pagination machinery
/// returns the state to the connector with its original type.
#[must_use = "dropping the cursor abandons the connector's pagination state"]
pub struct PaginationCursor<S>(S);

impl<S> fmt::Debug for PaginationCursor<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("PaginationCursor")
    }
}

/// Work or connector state for one pagination request.
#[derive(Debug)]
pub enum Pagination<W, S> {
    /// Starts pagination with the initial work.
    Start(W),
    /// Continues pagination with connector state returned by the preceding response.
    Continue(S),
}

/// Describes one low-level operation that kernel may delegate to a connector.
pub trait Operation: Send + Sized + 'static {
    /// Work supplied by kernel.
    type Work: Send + 'static;
    /// Successful response returned by the connector.
    type Response: Send + 'static;
}

/// Resume handle for a one-shot operation.
pub type OperationResume<O, Q, Op> = Resume<O, Q, <Op as Operation>::Response>;

/// Kernel-side request construction for one-shot operations.
#[allow(dead_code)]
pub(crate) trait OperationRequestExt: Operation {
    /// Construct a request enum variant for this one-shot operation.
    fn request<O, Q>(work: Self::Work, resume: OperationResume<O, Q, Self>) -> Q
    where
        O: Send + 'static,
        Q: CanRequest<O, Self>,
    {
        <Q as CanRequest<O, Self>>::request(work, resume)
    }
}

impl<T: Operation> OperationRequestExt for T {}

/// Marks an [`Operation`] as supporting pagination.
pub trait PaginatedOperation: Operation {}

/// Pagination request for an operation using connector state `S`.
pub(crate) type PaginatedOperationRequest<Op, S> = Pagination<<Op as Operation>::Work, S>;

/// Resume handle for a paginated operation using connector state `S`.
pub(crate) type PaginatedOperationResume<O, Q, Op, S> =
    Resume<O, Q, (<Op as Operation>::Response, Option<S>)>;

/// Kernel-side request construction for paginated operations.
pub(crate) trait PaginatedOperationRequestExt: PaginatedOperation {
    /// Construct a request enum variant for this paginated operation.
    fn paginated_request<O: Send + 'static, Q: CanRequestPaginated<O, Self>>(
        pagination: PaginatedOperationRequest<Self, <Q as CanRequestPaginated<O, Self>>::State>,
        resume: PaginatedOperationResume<O, Q, Self, <Q as CanRequestPaginated<O, Self>>::State>,
    ) -> Q {
        <Q as CanRequestPaginated<O, Self>>::request(pagination, resume)
    }
}

impl<T: PaginatedOperation> PaginatedOperationRequestExt for T {}

/// Allows a request enum to represent a one-shot operation.
pub trait CanRequest<O: Send + 'static, Op: Operation>: Send + Sized + 'static {
    /// Construct the request enum variant for `Op`.
    fn request(work: Op::Work, resume: OperationResume<O, Self, Op>) -> Self;
}

/// Allows a request enum to represent a paginated operation.
pub trait CanRequestPaginated<O: Send + 'static, Op: PaginatedOperation>:
    Send + Sized + 'static
{
    /// Connector state retained between pages.
    type State: Send + 'static;

    /// Construct the request enum variant for `Op`.
    #[allow(clippy::type_complexity)]
    fn request(
        pagination: Pagination<Op::Work, Self::State>,
        resume: Resume<O, Self, (Op::Response, Option<Self::State>)>,
    ) -> Self;
}

/// Executes a one-shot operation for a connector.
pub trait Supports<Op: Operation>: Send + 'static {
    /// Execute `work`.
    fn execute(&mut self, work: Op::Work) -> DeltaResult<Op::Response>;
}

/// Executes a paginated operation for a connector.
pub trait SupportsPaginated<Op: PaginatedOperation>: Send + 'static {
    /// Connector state retained between pages.
    type State: Send + 'static;

    /// Start pagination from `work`.
    fn start(&mut self, work: Op::Work) -> DeltaResult<Self::State>;

    /// Continue pagination from `state`.
    fn next(&mut self, state: Self::State) -> DeltaResult<(Op::Response, Option<Self::State>)>;
}

type PaginationConstructor<O, Q, W, R, S> = fn(Pagination<W, S>, Resume<O, Q, (R, Option<S>)>) -> Q;

/// One-shot continuation accepting the response type required by its request.
#[must_use = "the kernel workflow remains suspended until this continuation is resumed"]
pub struct Resume<O, Q, R> {
    generator: Generator<O, Q>,
    response_type: PhantomData<R>,
}

impl<O: Send + 'static, Q: Send + 'static, R: Send + 'static> Resume<O, Q, R> {
    /// Submit `response` and run kernel until its next request or completion.
    ///
    /// Returns [`ControlFlow::Continue`] with the next connector request, or
    /// [`ControlFlow::Break`] with the completed workflow's output.
    pub fn resume(self, response: DeltaResult<R>) -> DeltaResult<ControlFlow<O, Q>> {
        let response = response.map(|response| Box::new(response) as _);
        advance(self.generator, response)
    }

    /// Produce a response and submit its result to the suspended kernel workflow.
    ///
    /// Errors produced by `response` are delivered to kernel rather than returned directly by the
    /// connector driver.
    pub fn resume_with(
        self,
        response: impl FnOnce() -> DeltaResult<R>,
    ) -> DeltaResult<ControlFlow<O, Q>> {
        self.resume(response())
    }
}

impl<O, Q, R> fmt::Debug for Resume<O, Q, R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Resume")
    }
}

/// Start `producer` and run it until its first request or completion.
#[allow(dead_code)]
#[internal_api]
pub(crate) fn start<O, Q, F, Fut>(producer: F) -> DeltaResult<ControlFlow<O, Q>>
where
    O: Send + 'static,
    Q: Send + 'static,
    F: FnOnce(Channel<O, Q>) -> Fut + Send + 'static,
    Fut: Future<Output = DeltaResult<O>> + Send + 'static,
{
    let generator = Gen::new_boxed(move |channel| producer(Channel(channel)));
    advance(generator, UNOBSERVABLE_INITIAL_RESPONSE)
}

/// Preserves a typed work/response pair until it can attach the generator and construct `Q`.
#[allow(dead_code)]
struct TypedPending<O, Q, W, R> {
    work: W,
    constructor: fn(W, Resume<O, Q, R>) -> Q,
}

impl<O, Q, W, R> PendingRequest<O, Q> for TypedPending<O, Q, W, R>
where
    O: Send + 'static,
    Q: Send + 'static,
    W: Send + 'static,
    R: Send + 'static,
{
    fn attach(self: Box<Self>, generator: Generator<O, Q>) -> Q {
        let resume = Resume {
            generator,
            response_type: PhantomData,
        };
        (self.constructor)(self.work, resume)
    }
}

/// Preserves a typed paginated work/response/state relationship while suspended.
struct PaginatedPending<O, Q, W, R, S> {
    request: Pagination<W, PaginationCursor<S>>,
    constructor: PaginationConstructor<O, Q, W, R, S>,
}

impl<O, Q, W, R, S> PendingRequest<O, Q> for PaginatedPending<O, Q, W, R, S>
where
    O: Send + 'static,
    Q: Send + 'static,
    W: Send + 'static,
    R: Send + 'static,
    S: Send + 'static,
{
    fn attach(self: Box<Self>, generator: Generator<O, Q>) -> Q {
        let request = match self.request {
            Pagination::Start(work) => Pagination::Start(work),
            Pagination::Continue(cursor) => Pagination::Continue(cursor.0),
        };
        let resume = Resume {
            generator,
            response_type: PhantomData,
        };
        (self.constructor)(request, resume)
    }
}

/// Yield `work` and resume with the response type encoded by `constructor`.
#[allow(dead_code)]
#[internal_api]
pub(crate) async fn offload<O, Q, W, R>(
    channel: &mut Channel<O, Q>,
    constructor: fn(W, Resume<O, Q, R>) -> Q,
    work: W,
) -> DeltaResult<R>
where
    O: Send + 'static,
    Q: Send + 'static,
    W: Send + 'static,
    R: Send + 'static,
{
    // The generator has one yield type, so erase this work/response pair while suspended.
    // `TypedPending` preserves `R` until it can attach the generator to a typed `Resume<R>`.
    let pending: Pending<O, Q> = Box::new(TypedPending { work, constructor });
    suspend(channel, pending).await
}

/// Yield a pagination request and resume with its typed response and optional continuation state.
///
/// Start with [`Pagination::Start`]. A cursor returned by this function may be retained across
/// unrelated offloads and supplied through [`Pagination::Continue`] using the same request
/// constructor and connector state type `S`.
///
/// Returns the connector response and `Some` cursor when more pagination work remains, or `None`
/// when the connector completed the sequence.
///
/// Errors if the connector reports an error.
#[allow(dead_code)]
#[internal_api]
pub(crate) async fn offload_paginated<O, Q, W, R, S>(
    channel: &mut Channel<O, Q>,
    constructor: PaginationConstructor<O, Q, W, R, S>,
    request: Pagination<W, PaginationCursor<S>>,
) -> DeltaResult<(R, Option<PaginationCursor<S>>)>
where
    O: Send + 'static,
    Q: Send + 'static,
    W: Send + 'static,
    R: Send + 'static,
    S: Send + 'static,
{
    let pending: Pending<O, Q> = Box::new(PaginatedPending {
        request,
        constructor,
    });
    let (response, state): (R, Option<S>) = suspend(channel, pending).await?;
    Ok((response, state.map(PaginationCursor)))
}

async fn suspend<O, Q, R>(channel: &mut Channel<O, Q>, pending: Pending<O, Q>) -> DeltaResult<R>
where
    O: Send + 'static,
    Q: Send + 'static,
    R: Send + 'static,
{
    let response = channel.0.yield_(pending).await?;
    response.downcast().map(|response| *response).map_err(|_| {
        Error::internal_error("coroutine resumed with an unexpected connector response type")
    })
}

// Advances the generator by `response` and translates the result to `ControlFlow`.
fn advance<O: Send + 'static, Q: Send + 'static>(
    mut generator: Generator<O, Q>,
    response: ErasedResponse,
) -> DeltaResult<ControlFlow<O, Q>> {
    match generator.resume_with(response) {
        GeneratorState::Yielded(pending) => Ok(ControlFlow::Continue(pending.attach(generator))),
        GeneratorState::Complete(result) => result.map(ControlFlow::Break),
    }
}

#[cfg(test)]
mod capability_tests {
    use std::ops::Range;

    use super::*;

    enum Echo {}

    impl Operation for Echo {
        type Work = String;
        type Response = usize;
    }

    enum Numbers {}

    impl Operation for Numbers {
        type Work = Range<u32>;
        type Response = Vec<u32>;
    }

    impl PaginatedOperation for Numbers {}

    #[cfg(any())]
    enum Disabled {}

    #[cfg(any())]
    impl Operation for Disabled {
        type Work = ();
        type Response = ();
    }

    #[coroutine_request(output = O)]
    enum TestRequest<O: Send + 'static> {
        Echo(Echo),

        #[cfg(test)]
        #[paginated(state = Range<u32>)]
        Numbers(Numbers),

        #[cfg(any())]
        Disabled(Disabled),
    }

    #[coroutine_capabilities]
    trait TestCapabilities<O>: CanRequest<O, Echo> + CanRequestPaginated<O, Numbers>
    where
        O: Send + 'static,
    {
    }

    #[derive(Default)]
    struct TestConnector {
        echo_calls: usize,
    }

    impl Supports<Echo> for TestConnector {
        fn execute(&mut self, work: String) -> DeltaResult<usize> {
            self.echo_calls += 1;
            Ok(work.len())
        }
    }

    impl SupportsPaginated<Numbers> for TestConnector {
        type State = Range<u32>;

        fn start(&mut self, work: Range<u32>) -> DeltaResult<Self::State> {
            Ok(work)
        }

        fn next(&mut self, mut state: Self::State) -> DeltaResult<(Vec<u32>, Option<Self::State>)> {
            let page = state.by_ref().take(2).collect();
            let state = (!state.is_empty()).then_some(state);
            Ok((page, state))
        }
    }

    type Output = (usize, Vec<u32>);

    async fn workflow<Q>(mut channel: Channel<Output, Q>) -> DeltaResult<Output>
    where
        Q: TestCapabilities<Output>,
    {
        let length = offload(&mut channel, Echo::request, "kernel".to_string()).await?;

        let mut numbers = Vec::new();
        let mut pagination = Pagination::Start(1..4);
        loop {
            let (page, cursor) =
                offload_paginated(&mut channel, Numbers::paginated_request, pagination).await?;
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
        let result =
            drive_to_completion!(start(workflow::<TestRequest<Output>>), |request| request
                .resume(&mut connector),)
            .unwrap();

        assert_eq!(result, (6, vec![1, 2, 3]));
        assert_eq!(connector.echo_calls, 1);
    }
}
