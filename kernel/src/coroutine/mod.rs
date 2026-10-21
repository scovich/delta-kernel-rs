//! Runtime-neutral async protocol logic with connector-owned execution.
//!
//! Kernel protocol operations are ordinary Rust futures. Futures let Kernel suspend, but that alone
//! does not avoid control inversion. Merely making connector callbacks async would still require
//! Kernel to invoke connector code through kernel-defined interfaces the connector must implement.
//!
//! A Kernel coroutine instead combines a future with a typed request channel. When Kernel needs
//! connector work, it posts a [`Request`](kernel::Request) through that channel and awaits its
//! [`Reply`]. The request surfaces to the connector as an output of driving the coroutine. Each
//! request carries a single-use [`Reply`] tied to its originating await point. Sending or dropping
//! the reply makes that point ready and may wake an async driver (sync drivers often use a no-op
//! waker), but only the connector-owned driver decides when to poll the coroutine again.
//!
//! The futures that back Kernel's coroutines are runtime neutral. A synchronous driver manually
//! polls them with [`Workflow::try_advance`] or [`Coroutine::try_advance`] and requires no async
//! runtime at all. An async driver uses [`Workflow::advance`] or [`Coroutine::advance`] so its own
//! executor can park and wake the connector as it awaits the operation, which in turn awaits the
//! connector's reply from a different task.
//!
//! This module provides two forms. Both may issue zero or more requests. A [`Coroutine`]
//! interleaves requests with a stream of yielded items before completing. A [`Workflow`] is a
//! coroutine that never yields and completes with one final output.
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
//! for it, so outstanding replies do not keep a coroutine alive. Dropping the coroutine abandons
//! the operation, and subsequent replies to any of its outstanding requests are silently
//! discarded.
//!
//! # Driver policies
//!
//! A synchronous driver handles each request, sends its reply, and calls [`Workflow::try_advance`]
//! or [`Coroutine::try_advance`] again. Kernel's internal async machinery remains hidden and no
//! async runtime is required.
//!
//! An async driver may await each request before awaiting [`Workflow::advance`] or
//! [`Coroutine::advance`] again, or schedule
//! requests concurrently and immediately await another step. In both cases the connector's executor
//! polls Kernel's futures and the connector remains responsible for scheduling and executing every
//! request.
//!
//! Engine-compatible entry points use a synchronous driver that translates each request into calls
//! to [`crate::Engine`] handlers. Connectors that choose these entry points intentionally preserve
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
//! Replying with `Err` delivers a connector failure to the suspended Kernel await point. Calling an
//! advance method after its terminal step is a logic error. Synchronous `try_advance` returns
//! [`Error::WouldBlock`](crate::Error::WouldBlock) if Kernel suspends without producing a step,
//! which can only happen if there are outstanding replies.
use delta_kernel_derive::internal_api;

#[doc(inline)]
#[internal_api]
pub(crate) use self::async_ext::AsyncResult;
pub(crate) use self::async_ext::{AsyncBool, AsyncOption};
pub use self::channel::Reply;
#[doc(inline)]
#[internal_api]
pub(crate) use self::channel::{Channel, ChannelExchange, DeltaFuture};
#[doc(inline)]
#[internal_api]
pub(crate) use self::generator::{
    BoundGenerator, BoxedBoundGenerator, Flatten, Generator, GeneratorBody, GeneratorExt,
    GeneratorStep,
};
pub use self::generator::{Coroutine, CoroutineStep, StaticCoroutine};
#[doc(inline)]
#[internal_api]
pub(crate) use self::pagination::PageGenerator;
pub use self::pagination::{Cursor, Page, PageRequest, PagedOperation};
#[doc(hidden)]
#[internal_api]
pub(crate) use self::workflow::BodyFn;
pub use self::workflow::{
    drive_workflow, drive_workflow_async, StaticWorkflow, Workflow, WorkflowStep,
};
#[internal_api]
pub(crate) use self::workflow::{static_workflow, workflow};

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
