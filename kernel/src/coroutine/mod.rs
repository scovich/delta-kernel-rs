//! Runtime-neutral async protocol logic with connector-owned execution.
//!
//! Kernel protocol operations are ordinary Rust futures. Futures let Kernel suspend, but that alone
//! does not solve control inversion. Making connector callbacks async would still require Kernel to
//! invoke connector code through kernel-defined interfaces the connector must implement.
//!
//! A Kernel coroutine combines a future with a typed [`Channel`]. When Kernel needs connector work,
//! it posts a [`Request`] through that channel and awaits its [`Reply`]. The request surfaces to
//! the connector as an output of driving the coroutine. Each request carries a single-use reply
//! tied to its originating await point. Sending or dropping the reply makes that point ready and
//! wakes an async driver, but only the connector-owned driver decides when to poll the coroutine
//! again.
//!
//! Kernel's futures are runtime neutral. A synchronous driver manually polls them with
//! [`Workflow::try_advance`] and requires no async runtime at all. An async driver uses
//! [`Workflow::advance`] so its own executor can park and wake the operation.
//!
//! This module provides two coroutine forms. Both may issue zero or more requests. A [`Workflow`]
//! completes with one final output while a [`Generator`] interleaves requests with a stream of
//! yielded items before completing.
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
//! for it, so outstanding replies do not keep a coroutine alive. Dropping the workflow or generator
//! abandons the operation, and subsequent replies to any of its outstanding requests are silently
//! discarded.
//!
//! # Driver policies
//!
//! A synchronous driver handles each request, sends its reply, and calls [`Workflow::try_advance`]
//! again. Kernel's internal async machinery remains hidden and no async runtime is required.
//!
//! An async driver may await each request before awaiting [`Workflow::advance`] again, or schedule
//! requests concurrently and immediately await another step. In both cases the connector's
//! executor polls Kernel's futures and the connector remains responsible for scheduling and
//! executing every request.
//!
//! Engine-compatible entry points use a synchronous driver that translates each request into calls
//! to [`crate::Engine`] handlers. Connectors that choose these entry points intentionally preserve
//! the legacy Kernel-to-Engine callback model.
//!
//! # Choosing the ownership boundary
//!
//! A [`Workflow`] already owns its request channel. For a streaming operation without a parent,
//! pass owned inputs through [`StaticGenerator::new`]. Its callback must return
//! `Generator<'static, _>`, statically preventing borrowed state from escaping.
//!
//! Kernel code that already has a [`kernel::Channel`] calls async workflow implementations
//! directly. Passing that channel to a streaming operation returns a [`Generator`] whose lifetime
//! also covers any other borrowed inputs. Requests then surface from the parent coroutine.
//! Kernel may compose channel-provided futures with runtime-neutral future and stream combinators;
//! every operation requiring connector work still passes through the channel.
//!
//! # Driving operations
//!
//! An async workflow driver waits for [`WorkflowStep::Request`] or [`WorkflowStep::Done`]:
//!
//! ```ignore
//! let output = drive_async_workflow!(workflow, |request| {
//!     connector.reply(request).await?;
//! });
//! ```
//!
//! [`drive_async_workflow!`] provides this one-request-at-a-time loop while expanding the handler
//! in the caller's async context. Synchronous drivers use [`drive_workflow!`] and
//! [`Workflow::try_advance`] instead.
//!
//! Process static generator requests and complete their replies. Calling either advance method
//! after [`GeneratorStep::Done`] is a logic error.
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
//! - [`PageRequest::Continue`] consumes a cursor and returns the next page, with `Some(cursor)`
//!   when more pages remain.
//! - [`PageRequest::Prepare`] initializes an operation and returns a [`Cursor`] without producing a
//!   first page. This lets the connector begin work in the background while waiting for the first
//!   [`PageRequest::Continue`] from kernel.
//!
//! Every response to a pagination request carries its data and an optional [`Cursor`] for
//! continuing. `None` ends pagination; an empty page with a live cursor does not. Connectors choose
//! page boundaries and cursor representation; cursor payloads are opaque to kernel, forwarded
//! blindly back to connector with each continuation request (or dropped if kernel abandoned the
//! operation).
//!
//! Each advance returns at most one request. Kernel may offer several requests when it composes
//! channel futures; the connector controls how many become outstanding by deciding whether to
//! advance again before replying to earlier requests. The connector also controls how many of those
//! requests to service at a time, and in what order. Pagination additionally lets a connector
//! retain state and prefetch work between pages.
//!
//! # Error handling
//!
//! Replying with `Err` delivers a connector failure to the suspended Kernel await point. Calling
//! an advance method after its terminal step is a logic error. Synchronous `try_advance` returns
//! [`Error::WouldBlock`] if Kernel suspends without producing a step.
use std::any::{type_name, Any};

use delta_kernel_derive::internal_api;
use derive_more::Constructor;

pub use self::core::Reply;
#[doc(inline)]
#[internal_api]
pub(crate) use self::core::{Channel, ChannelExchange, DeltaFuture};
#[doc(inline)]
#[internal_api]
pub(crate) use self::kernel::ChannelExt;
pub use self::kernel::{
    Generator, GeneratorStep, Request, StaticGenerator, StaticWorkflow, UnboundGenerator, Workflow,
    WorkflowStep,
};
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

mod core;
pub(crate) mod engine;
pub mod evaluation;
// cbindgen parses cfg-exclusive module declarations together. Ignore each private declaration so
// generic types from its public counterpart are not registered twice.
#[cfg(feature = "internal-api")]
pub mod generator;
#[cfg(not(feature = "internal-api"))]
/// cbindgen:ignore
#[allow(unreachable_pub)]
pub(crate) mod generator;
#[cfg(feature = "internal-api")]
pub mod kernel;
#[cfg(not(feature = "internal-api"))]
/// cbindgen:ignore
#[allow(unreachable_pub)]
pub(crate) mod kernel;
pub mod listing;
pub mod read;
pub mod workflow;
pub mod write;

#[doc(inline)]
#[internal_api]
pub(crate) use self::engine::drive_storage;
#[doc(inline)]
#[internal_api]
pub(crate) use self::engine::drive_workflow;
#[cfg(feature = "internal-api")]
#[doc(inline)]
pub use self::engine::{run_workflow_with_engine, run_workflow_with_storage};
#[doc(inline)]
pub use self::workflow::{drive_async_workflow, drive_workflow};

#[cfg(test)]
mod tests;

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
    operation: PhantomType<Op>,
}

impl<Op> Cursor<Op> {
    /// Wrap connector-owned `state` for a later continuation request.
    pub fn new(state: impl Any + Send + 'static) -> Self {
        Self {
            state: Box::new(state),
            operation: PhantomType::default(),
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
