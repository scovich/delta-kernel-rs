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
//! The work item's type is specific to each kernel workflow, an enum whose two-field tuple variants
//! document the work items kernel might delegate to the connector. The first field describes the
//! actual work to be done, and the second field is a [`Resume`] instance matched to the responses
//! kernel expects back. The [`drive_to_completion!`] macro simplifies the `ControlFlow` loop by
//! repeatedly dispatching `Continue` cases as needed, and returning the content of the final
//! `Break`.
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
//! Consider a hypothetical kernel workflow that delegates division and formatting, then returns the
//! quotient as a string. We can define a workflow-specific [`Resume`] alias, request enum, and
//! coroutine:
//!
//! ```
//! use std::ops::ControlFlow;
//!
//! use delta_kernel::coroutine::{self, Channel, Resume};
//! use delta_kernel::DeltaResult;
//!
//! /// The kernel-provided workflow-specific `Resume` type for `QuotientAsStringRequest`.
//! pub type QuotientAsStringResume<R> = Resume<String, QuotientAsStringRequest, R>;
//!
//! /// The kernel-provided workflow-specific request enum for `quotient_as_string`.
//! pub enum QuotientAsStringRequest {
//!     Divide((u32, u32), QuotientAsStringResume<f64>),
//!     ToString(f64, QuotientAsStringResume<String>),
//! }
//!
//! // The public connector entry point for a kernel workflow is a normal function that
//! // returns a `ControlFlow` which communicates the next step to the connector.
//! pub fn quotient_as_string(
//!     dividend: u32,
//!     divisor: u32,
//! ) -> DeltaResult<ControlFlow<String, QuotientAsStringRequest>> {
//!     coroutine::start(move |channel| {
//!         quotient_as_string_impl(channel, dividend, divisor)
//!     })
//! }
//!
//! // Kernel workflows are implemented internally as async functions that `offload` work items to
//! // the connector and `await` responses on the resulting coroutine-backed futures.
//! async fn quotient_as_string_impl(
//!     mut channel: Channel<String, QuotientAsStringRequest>,
//!     dividend: u32,
//!     divisor: u32,
//! ) -> DeltaResult<String> {
//!     let quotient = coroutine::offload(
//!         &mut channel,
//!         QuotientAsStringRequest::Divide,
//!         (dividend, divisor),
//!     )
//!     .await?;
//!     coroutine::offload(
//!         &mut channel,
//!         QuotientAsStringRequest::ToString,
//!         quotient,
//!     )
//!     .await
//! }
//!
//! // The connector performs delegated work however it likes.
//! fn connector_divide((dividend, divisor): (u32, u32)) -> DeltaResult<f64> {
//!     Ok(dividend as f64 / divisor as f64)
//! }
//!
//! fn connector_to_string(value: f64) -> DeltaResult<String> {
//!     Ok(value.to_string())
//! }
//!
//! // The connector drives the kernel workflow by looping on `ControlFlow::Continue`: performing
//! // each work item kernel requests and resuming the workflow with the response.
//! //
//! // NOTE: Connectors should prefer the `drive_to_completion!` macro instead of a manual loop.
//! fn connector_quotient_as_string(dividend: u32, divisor: u32) -> DeltaResult<String> {
//!     let mut next = quotient_as_string(dividend, divisor)?;
//!     loop {
//!         next = match next {
//!             ControlFlow::Continue(QuotientAsStringRequest::Divide(work, resume)) => {
//!                 resume.resume(connector_divide(work))?
//!             }
//!             ControlFlow::Continue(QuotientAsStringRequest::ToString(value, resume)) => {
//!                 resume.resume(connector_to_string(value))?
//!             }
//!             ControlFlow::Break(output) => return Ok(output),
//!         };
//!     }
//! }
//!
//! assert_eq!("0.25", connector_quotient_as_string(1, 4)?);
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
// kernel and connector. Each variant is a two-field tuple:
// * The first field carries a concrete work item (of type `W`) that kernel might delegate to
//   connector as part of the workflow.
// * The second field is an opaque `Resume` instance that connector uses to communicate its response
//   (of type `R`) back to kernel so the workflow can continue.
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
// connector both what to do and what response type kernel expects. Its `Resume<R>` provides a
// `resume` method that accepts `DeltaResult<R>`, consumes `self`, resumes kernel, and returns
// kernel's next `DeltaResult<ControlFlow<O, Q>>` back to the connector.
//
// Rust compiles the async workflow into a `Future` that stores kernel's locals and control flow
// across each `.await`. `genawaiter2` wraps and polls that future in a `Generator`, an object that
// can suspend by yielding a value and later continue with a resumed value. Its yield and resume
// types are fixed for the lifetime of the generator. When the workflow suspends, the generator owns
// the future and all state required to continue it; in that state, the generator is the
// continuation. While kernel runs, `advance` owns it. While the connector performs work, the
// request's `Resume` owns it. No additional kernel-owned state table connects the two calls.
//
// Each `offload` call selects one variant of `Q` and its corresponding `(W, R)` pair. Multiple
// variants could have the same signature while still representing logically distinct work items.
// To fit all `(W, R)` pairs through the generator's fixed transport types, this module erases each
// outbound pair behind a `dyn PendingRequest` and each inbound `R` behind `Any`. The typed
// constructor and `Resume<R>` preserve their relationship so the concrete types can be restored
// before kernel or connector code handles them.
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

/// Erases the work/response pair only while the generator is suspended.
trait PendingRequest<O, Q>: Send {
    fn attach(self: Box<Self>, generator: Generator<O, Q>) -> Q;
}

/// Erases heterogeneous work/response pairs into the generator's single yield type.
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
    let pending = Box::new(TypedPending { work, constructor });
    let response = channel.0.yield_(pending).await?;

    // `Resume<R>` can only box an `R` before resuming this exact suspended generator, so this
    // downcast is guaranteed to succeed.
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
