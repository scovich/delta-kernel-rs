//! Stackless, connector-driven kernel coroutines.
//!
//! When a connector invokes a kernel operation such as snapshot creation, kernel delegates all I/O
//! and dataflow operations back to the connector. However, delegation by calling back into
//! connector code would produce nested connector -> kernel -> connector call stacks, with connector
//! blocking on kernel while kernel invokes connector code:
//!
//! ```text
//!   +---------------------------------------------------------------------------+
//!   |                                   CONNECTOR                               |
//!   |                                                                           |
//!   |                        +-----------------------------+                    |
//!   |                        |                             |                    |
//!   |                        |     +-----KERNEL------+     |                    |
//! ----> analyze_query        |     |                 |     |                    |
//!   |     |                  |     |                 |     |                    |
//!   |     +--------------------------> load_snapshot |     |                    |
//!   |     .                  |     |        |        |     |                    |
//!   |     .                  |     |        +----------------> list             |
//!   |     .                  |     |        .        |     |    |               |
//!   |     .                  |     |        .        |     |    |               |
//!   |     .                  |     |        + <-----------------+               |
//!   |     .                  |     |        |        |     |                    |
//!   |     .                  |     |        +----------------> read_json        |
//!   |     .                  |     |        .        |     |    |               |
//!   |     .                  |     |        .        |     |    |               |
//!   |     .                  |     |        + <-----------------+               |
//!   |     .                  |     |        |        |     |                    |
//!   |     .                  |     |        +----------------> read_parquet     |
//!   |     .                  |     |        .        |     |    |               |
//!   |     .                  |     |        .        |     |    |               |
//!   |     .                  |     |        + <-----------------+               |
//!   |     .                  |     |        |        |     |                    |
//!   |     + <-----(snapshot return value)---+        |     |                    |
//!   |     |                  |     |                 |     |                    |
//! <-------+                  |     |                 |     |                    |
//!   |                        |     |                 |     |                    |
//!   +------------------------+     +-----------------+     +--------------------+
//! ```
//!
//! Because kernel is wedged between two "halves" of the connector, it interferes with connector's
//! scheduling model (sync/async/parallel/distributed), error handling, etc. It also depends on the
//! complex [`Engine`](crate::Engine) trait hierarchy.
//!
//! Coroutines flip the control flow around by modeling kernel's work as a resumable continuation
//! function. Connector still invokes a kernel function to start running kernel, but kernel
//! delegates requests back to the connector by suspending itself. When that happens, the
//! connector's function call returns with a (request, continuation) pair as a return value, and
//! connector inovkes the continuation function to pass a response to kernel and allow it to
//! continue. The process repeats until the coroutine finishes and returns the requested value:
//!
//! ```text
//!   +------------------------+     +-----------------+
//!   |       CONNECTOR        |     |     KERNEL      |
//!   |                        |     |                 |
//! ----> analyze_query        |     |                 |
//!   |     |                  |     |                 |
//!   |     +--------------------------> load_snapshot |
//!   |     .                  |     |        |        |
//!   |     + <----------(request listing)----S        |
//!   |     |                  |     |        .        |
//!   |     +---> list         |     |        .        |
//!   |            |           |     |        .        |
//!   |            |           |     |        .        |
//!   |     + <----+           |     |        .        |
//!   |     |                  |     |        .        |
//!   |     +---(listing response)----------> R        |
//!   |     .                  |     |        |        |
//!   |     + <--------(request JSON read)----S        |
//!   |     |                  |     |        .        |
//!   |     +---> read_json    |     |        .        |
//!   |            |           |     |        .        |
//!   |            |           |     |        .        |
//!   |     + <----+           |     |        .        |
//!   |     |                  |     |        .        |
//!   |     +---(JSON data response)--------> R        |
//!   |     .                  |     |        |        |
//!   |     + <------(request parquet read)---S        |
//!   |     |                  |     |        .        |
//!   |     +---> read_parquet |     |        .        |
//!   |            |           |     |        .        |
//!   |            |           |     |        .        |
//!   |     + <----+           |     |        .        |
//!   |     |                  |     |        .        |
//!   |     +---(parquet data response)-----> R        |
//!   |     .                  |     |        |        |
//!   |     + <-----(snapshot return value)---+        |
//!   |     |                  |     |                 |
//! <-------+                  |     |                 |
//!   |                        |     |                 |
//!   +------------------------+     +-----------------+
//! ```
//!
//! This way, the connector always invokes its own code, acting on behalf of kenrel as needd. The
//! coroutines are "stackless" because there is no kernel code in the call stack while the connector
//! processes a kernel request. When the connector starts or resumes a kernel coroutine, kernel runs
//! synchronously in the calling thread until it either completes or suspends again (which happens
//! quickly because kernel delegates all slow or heavy work to the connector). At each step, the
//! connector decides how to serve the request and whether to resume kernel.
//!
//! Kernel coroutines come in two flavors:
//! * A [`Workflow`] suspends zero or more times before producing a final output
//! * A [`Generator`] is like a workflow, but can additionally yield zero or more items back to the
//!   connector before producing its final output.
//!
//! Kernel coroutines are an async-friendly synchronous API: Between suspensions, kernel performs
//! only light, bounded CPU work. It delegates all blocking, heavy, or unbounded work to the
//! connector as one or more [`Request`] instances. Connectors therefore do not need to treat kernel
//! calls as scheduling points, and they can schedule requests like any other work. Resume handles
//! are [`Send`] and may move freely between tasks and threads as the workflow progresses. The
//! connector may execute requests synchronously, asynchronously, in parallel with threads, or
//! across distributed workers; and it can use the runtimes and libraries of its choosing to do so.
//!
//! The [`Request`] enum defines the operations kernel may delegate. Each request carries the
//! operation inputs and a [`Resume`] function, which owns the suspended continuation and which
//! declares the expected response type. A connector drives a [`Workflow`] or [`Generator`] by
//! repeatedly matching each request, performing the operation, and resuming kernel with the result.
//!
//! For example, if kernel defines this:
//!
//! ```no_run
//! use delta_kernel::coroutine::listing::ForwardListing;
//! use delta_kernel::coroutine::{PageRequest, Workflow};
//! use delta_kernel::DeltaResult;
//!
//! # /*
//! /// Operations kernel can request.
//! ///
//! /// `N` is the next [`Workflow`] or [`Generator`] state returned by each resume handle.
//! pub enum Request<N: Send + 'static> {
//!     /// List a bounded path range in ascending order.
//!     ListForward(PageRequest<N, ForwardListing>),
//!     /// List page ranges from high to low, with entries ascending within each page.
//!     ListBackward(PageRequest<N, BackwardListing>),
//!     /// Read a whole file when the range is `None`, or exactly the half-open range otherwise.
//!     ReadSmallFile(FileSlice, Resume<N, Bytes>),
//!       ...
//! }
//! # */
//! # use delta_kernel::coroutine::Request;
//!
//! // A kernel-defined return type.
//! pub struct Foo;
//!
//! // A public kernel workflow entry point. It does _not_ take an `Engine` instance.
//! pub fn public_kernel_entry_point() -> DeltaResult<Workflow<Foo>> {
//!     // Kernel constructs and starts its coroutine here, returning to the caller
//!     // when it needs to make a Request or has a final result ready.
//!     todo!()
//! }
//! ```
//!
//! Then a connector would drive the `public_kernel_entry_point` workflow as follows:
//!
//! ```no_run
//! use delta_kernel::coroutine::listing::ForwardListing;
//! use delta_kernel::coroutine::{Cursor, Page, PageRequest, Request, Workflow};
//! use delta_kernel::DeltaResult;
//! # pub struct Foo;
//! #
//! # pub fn public_kernel_entry_point() -> DeltaResult<Workflow<Foo>> {
//! #    todo!()
//! # }
//!
//! async fn prepare_listing(
//!     request: ForwardListing,
//! ) -> DeltaResult<Cursor<ForwardListing>> {
//!     // Connector creates its cursor state and may begin prefetching here.
//!     todo!()
//! }
//!
//! async fn advance_listing(
//!     cursor: Cursor<ForwardListing>,
//! ) -> DeltaResult<Page<ForwardListing>> {
//!     // Connector resolves its cursor state and fetches the next page here.
//!     todo!()
//! }
//!
//! // Connector decides how to dispatch kernel requests. Here it uses async scheduling.
//! async fn serve_request<N: Send + 'static>(
//!     request: Request<N>,
//! ) -> DeltaResult<N> {
//!     match request {
//!         Request::ListForward(PageRequest::Prepare(args, resume)) => {
//!             resume(prepare_listing(args).await)
//!         }
//!         Request::ListForward(PageRequest::Start(args, resume)) => {
//!             let cursor = prepare_listing(args).await?;
//!             resume(advance_listing(cursor).await)
//!         }
//!         Request::ListForward(PageRequest::Continue(cursor, resume)) => {
//!             resume(advance_listing(cursor).await)
//!         }
//!         _ => Err(delta_kernel::Error::unsupported(
//!             "this connector does not support the requested operation",
//!         )),
//!     }
//! }
//!
//! // Connector also decides how to drive workflows. Here, a generic driver loop. It could also
//! // inject cancellation checks, logging, etc. if desired.
//! async fn connector_workflow_driver<T: Send + 'static>(
//!     mut workflow: DeltaResult<Workflow<T>>,
//! ) -> DeltaResult<T> {
//!     loop {
//!         workflow = match workflow? {
//!             Workflow::Done(output) => return Ok(output),
//!             Workflow::Request(request) => serve_request(request).await,
//!         };
//!     }
//! }
//!
//! // A convenient connector-side entry point for the workflow, that hides all of the above.
//! async fn connector_entry_point() -> DeltaResult<Foo> {
//!     connector_workflow_driver(public_kernel_entry_point()).await
//! }
//! ```
//!
//! # Pagination
//!
//! Operations that may produce large or unbounded results, such as file reads or query output, use
//! a paginated request model. This allows kernel to consume a logical stream of results while still
//! allowing connector to control the amount of in-flight memory, terminate the operation if
//! resource limits are hit, etc.
//!
//! Paged operations have three phases:
//!
//! - [`PageRequest::Start`] initializes the operation and returns its first [`Page`] of output.
//! - [`PageRequest::Continue`] consumes a cursor and returns the next page, along with Some new
//!   cursor if the operation has not yet completed.
//! - [`PageRequest::Prepare`] initializes an operation and returns a [`Cursor`] without producing a
//!   first page. This lets the connector begin work in the background while waiting for the first
//!   [`PageRequest::Continue`] from kernel.
//!
//! Every response to a pagination request carries its data and an optional [`Cursor`] for
//! continuing. `None` ends pagination; an empty page with a live cursor does not. Connectors choose
//! page boundaries and cursor representation; cursor payloads are opaque to kernel, forwarded
//! blindly back to connector with each continuation request (or dropped if kernel abandoned the
//! operation). [`CursorState::Boxed`] owns and drops in-process state while [`CursorState::Id`] is
//! a raw (connector-managed) reference with no drop notification.
//!
//! Kernel coroutines expose only one request at a time, but pagination allows kernel to expose
//! upcoming I/O streams so connectors can begin prefetching.
//!
//! # Yielding
//!
//! A [`Generator`] adds [`Generator::Yield`] to the completion and request states of a
//! [`Workflow`]. Resuming the corresponding [`YieldResume`] continues the generator, and dropping
//! the resume abandons the generator.
//!
//! # Error handling
//!
//! Every transition is fallible:
//!
//! - Starting a coroutine returns `DeltaResult<Workflow<_>>` or `DeltaResult<Generator<_, _>>`. An
//!   error means the workflow failed before producing the next state.
//! - The [`Resume`] function accepts `DeltaResult<R>`. A connector error is delivered to the
//!   suspended kernel await point, where kernel may handle it or propagate it.
//! - [`Resume`] itself returns `DeltaResult`. An error means the workflow failed while kernel was
//!   processing the response or advancing the coroutine.
//!
//! A connector may drop any [`Resume`] handle at any time. This abandons the suspended coroutine
//! and drops all associated state. For connector-side work failures, resume with `Err` when kernel
//! should observe the failure; otherwise drop the handle and return the error directly.
// === Kernel implementation ===
//
// Kernel workflows are ordinary async Rust functions, but this module allows the connector to drive
// them directly, without runtime scheduling: Kernel futures are polled with a no-op waker and
// advance only on explicit start or resume calls:
//
// 1. The connector calls a kernel entry point that creates a `Workflow`.
//
// 2. The entry point calls `Workflow::start` with a closure that, when invoked synchronously,
//    associates the workflow's async logic with a `Channel` and returns the resulting async future.
//
// 3. `Workflow::start` creates an `Arc<Mailbox<PendingRequest>>`. It keeps one reference for
//    itself, and shares a second reference with kernel by wrapping it in a `Channel`, which it uses
//    to invoke the workflow-creation closure. Because that closure returns a future, invoking it
//    creates but does not start the workflow's (compiler-generated) async state machine.
//
// 4. `Workflow::start` wraps and instruments that future, and box-pins it (type alias: `Task<O>`).
//    This `Task` owns the complete compiler-generated state of the kernel workflow.
//
// 5. `Workflow::start` passes the task and mailbox to `advance_workflow`.
//
// 6. `advance_workflow` creates a `Context` with a no-op waker and explicitly polls the task. No
//    async runtime participates in coroutine scheduling.
//
// 7. Polling the task launches it (standard async Rust), executing the kernel workflow until it
//    calls an async channel method such as `channel.read_small_file(...)`. All channel methods are
//    thin wrappers that call and await the async `Channel::exchange` method.
//
// 8. The kernel’s `.await` polls that channel-method future. Its body calls
//    `Channel::exchange(...)`, creating and immediately awaiting another compiler-generated future.
//
// 9. The first poll of the `exchange` future creates an `Arc<Exchange>` in `Outbound(operation)`
//    state and stores a `Weak<Exchange>` in the channel mailbox. The weak pointer will be dropped
//    automatically if kernel abandons the exchange after publishing it to the connector.
//
// 10. The `exchange` future then constructs `Wait(exchange)` and awaits it. The generated
//     `exchange` future invokes `Wait::poll`.
//
// 11. `Wait::poll` sees `Outbound`, leaves that state intact, and returns `Pending`.
//
// 12. `Pending` propagates through the `exchange` future, channel-method future, and workflow
//     future. Their compiler-generated async state machines retain the nested futures and all
//     suspended locals, including the only strong reference to the `Exchange`. The task’s poll
//     finally returns `Pending` to `advance_workflow`.
//
// 13. After that poll returns, `advance_workflow` removes the pending request from the mailbox and
//     calls `PendingRequest::attach`.
//
// 14. `attach` upgrades the mailbox’s `Weak<Exchange>`, claims the operation, and transitions the
//     exchange from `Outbound` to `InFlight`.
//
// 15. `attach` creates a `TypedResume` whose closure owns the upgraded `Arc<Exchange>`, pinned
//     workflow task (which still owns the original `Arc<Exchange>`), mailbox, and the function
//     needed to advance that task again. It combines the claimed operation with this resume handle
//     to produce a typed `Request`.
//
// 16. `advance_workflow` returns `Workflow::Request(request)` to `Workflow::start`, which returns
//     it to the connector. At this point, the `Mailbox`, `Channel`, and `Exchange` are all
//     reachable only through the `TypedResume`. If the connector drops that `request`, the entire
//     workflow and all associated state are dropped as Arc reference counts go to zero.
//
// 17. The connector matches the request, performs the operation, and calls
//     `TypedResume::resume(response)`. The resume closure stores the response in the exchange,
//     transitioning `InFlight` to `Inbound(response)`, then forwards the `Task` and the `Mailbox`
//     to `advance_workflow`.
//
// 18. `advance_workflow` polls the task again, and the compiler-generated async state machines
//     resume the nested poll chain at the suspended `.await`. The `Wait` instance now sees
//     `Inbound(response)`, transitions the exchange to `Complete`, and returns `Ready(response)`.
//     That result propagates through the `exchange` and channel-method futures, and becomes the
//     return value of kernel’s original `.await`.
//
// 19. The kernel workflow continues until it creates and awaits another channel future, repeating
//     the process, or returns its final `DeltaResult<O>`. `advance_workflow` converts successful
//     completion to `Workflow::Done(output)` and propagates any error directly.
//
// `Yielder` adds `yield_item`, which logically separates the entity that consumes yielded items
// from the entity that processes requests. When kernel drives a child generator, it consumes the
// yielded items directly while forwarding its requests back to the connector through its own
// `Channel`. The connector therefore sees those requests as part of the overall workflow's
// coroutine rather than driving a separate child generator. Kernel uses these internal generators
// to manipulate streaming outputs produced by e.g. log replay.
//
// For connectors that don't want to deal with coroutines at all, kernel continues to provide legacy
// sync/imperative entry points. Each entry point launches the corresponding workflow, then drives
// it to completion using an `EngineConnector` that uses the caller-provided `Engine` instance to
// serve requestse.

use std::any::Any;
use std::marker::PhantomData;

use bytes::Bytes;
use delta_kernel_derive::internal_api;

#[internal_api]
pub(crate) use self::kernel::{Channel, DeltaFuture, GeneratorState, Yielder};
use self::listing::{BackwardListing, BackwardListingResult, ForwardListing, ListingBounds};
use self::read::{ReadJsonFiles, ReadParquetFiles};
use self::write::WriteBytes;
#[cfg(feature = "declarative-plans")]
pub(crate) use crate::plans::Operation as PlanOperation;
use crate::{DeltaResult, FileMeta, FileSlice, ParquetFooter};

mod core;
pub(crate) mod engine;
#[cfg(feature = "internal-api")]
pub mod kernel;
#[cfg(not(feature = "internal-api"))]
pub(crate) mod kernel;
pub mod listing;
pub mod read;
pub mod write;

#[cfg(test)]
mod tests;

/// Resume handle for one yielded generator item.
///
/// Pass `Ok(())` to continue or an error for kernel to observe at the suspended yield.
pub type YieldResume<N> = Resume<N, ()>;

/// Describes a connector operation that may return multiple pages.
pub trait PagedOperation: Send + Sized + 'static {
    /// Data returned in one page.
    type Page: Send + 'static;
}

/// One page of connector data and the cursor for requesting more.
pub struct Page<Op: PagedOperation> {
    /// Data returned in this page.
    pub data: Op::Page,
    /// Cursor to pass to `Continue`, or `None` when no pages remain.
    pub next: Option<Cursor<Op>>,
}

/// Opaque pagination handle typed to one operation; kernel stores it until `Continue`.
pub struct Cursor<Op: PagedOperation> {
    state: CursorState,
    operation: PhantomData<fn() -> Op>,
}

/// Type-erased connector state carried by a [`Cursor`].
pub enum CursorState {
    /// Scalar handle, commonly for FFI; kernel is not notified when it is dropped.
    Id(i64),
    /// Owned in-process state, dropped with the cursor.
    Boxed(Box<dyn Any + Send>),
}

impl<Op: PagedOperation> Cursor<Op> {
    /// Construct and return a cursor carrying connector-defined scalar `id`.
    pub fn id(id: i64) -> Self {
        Self::from_state(CursorState::Id(id))
    }

    /// Construct and return a cursor owning connector-defined in-process `state`.
    pub fn boxed(state: impl Any + Send) -> Self {
        Self::from_state(CursorState::Boxed(Box::new(state)))
    }

    /// Consume this cursor and return its connector-defined state.
    pub fn into_state(self) -> CursorState {
        self.state
    }

    fn from_state(state: CursorState) -> Self {
        Self {
            state,
            operation: PhantomData,
        }
    }
}

/// A workflow that has either completed or suspended with a request for the connector.
pub enum Workflow<O: Send + 'static> {
    Done(O),
    Request(Request<Self>),
}

/// A connector-facing generator that can complete, yield an item, or request connector work.
pub enum Generator<O: Send + 'static, Y: Send + 'static> {
    Done(O),
    Yield(Y, YieldResume<Self>),
    Request(Request<Self>),
}

/// Operations kernel can request.
///
/// `N` is the next [`Workflow`] or [`Generator`] state returned by each resume handle.
pub enum Request<N: Send + 'static> {
    /// List a bounded path range in ascending order.
    ListForward(PageRequest<N, ForwardListing>),
    /// List page ranges from high to low, with entries ascending within each page.
    ListBackward(PageRequest<N, BackwardListing>),
    /// Read a whole file when the range is `None`, or exactly the half-open range otherwise.
    ReadSmallFile(FileSlice, Resume<N, Bytes>),
    /// Read one Parquet footer.
    ReadParquetFooter(FileMeta, Resume<N, ParquetFooter>),
    /// Read JSON files as ordered [`crate::EngineData`] batches: Each file may produce multiple
    /// batches, but batches may not span multiple files.
    ReadJson(PageRequest<N, ReadJsonFiles>),
    /// Read Parquet files as ordered [`crate::EngineData`] batches: Each file may produce multiple
    /// batches, but batches may not span multiple files.
    ReadParquet(PageRequest<N, ReadParquetFiles>),
    #[cfg(feature = "declarative-plans")]
    /// Execute a declarative plan in connector-selected pages.
    ExecutePlan(PageRequest<N, PlanOperation>),
    /// Write one complete object.
    WriteBytes(WriteBytes, Resume<N, ()>),
}

/// One phase of a paginated connector operation.
pub enum PageRequest<N: Send + 'static, Op: PagedOperation> {
    /// Initialize the operation and return its first page.
    Start(Op, Resume<N, Page<Op>>),
    /// Initialize the operation and return a cursor without fetching the first page.
    Prepare(Op, Resume<N, Cursor<Op>>),
    /// Consume a cursor and return the next page.
    Continue(Cursor<Op>, Resume<N, Page<Op>>),
}

impl<N: Send + 'static> Request<N> {
    /// Forward this request to the connector through the given channel.
    async fn forward_to(self, parent: &Channel) -> DeltaResult<N> {
        match self {
            Self::ListForward(request) => request.forward_to(parent).await,
            Self::ListBackward(request) => request.forward_to(parent).await,
            Self::ReadSmallFile(file, resume) => {
                resume(parent.read_small_file(file.0, file.1).await)
            }
            Self::ReadParquetFooter(file, resume) => resume(parent.read_parquet_footer(file).await),
            Self::ReadJson(request) => request.forward_to(parent).await,
            Self::ReadParquet(request) => request.forward_to(parent).await,
            #[cfg(feature = "declarative-plans")]
            Self::ExecutePlan(request) => request.forward_to(parent).await,
            Self::WriteBytes(operation, resume) => resume(
                parent
                    .write_bytes(operation.url, operation.data, operation.overwrite)
                    .await,
            ),
        }
    }
}

/// Resumes a suspended [`Workflow`] or [`Generator`] with connector's `response`, advancing it to
/// the next communication point (suspend, yield, or completion).
pub type Resume<N, R> = Box<dyn FnOnce(DeltaResult<R>) -> DeltaResult<N> + Send>;
