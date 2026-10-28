//! Kernel's connector request vocabulary and the coroutine types specialized to it.
//!
//! Kernel operations surface connector work as [`Request`](crate::coroutine::kernel::Request)s.
//! Each variant carries an operation payload from
//! [`request`](crate::coroutine::kernel::request) and the [`Reply`] that completes it, either
//! directly or nested in a [`PageRequest`]. The aliases in this module fix the generic
//! [`Workflow`] and [`Coroutine`] to this vocabulary; the [parent module] explains how to drive
//! them.
//!
//! Some entry points return a workflow whose output is a
//! [`KernelResultGenerator`](crate::coroutine::kernel::KernelResultGenerator). Pass that output to
//! [`Coroutine::from_generator`] to drive its stream.
//!
//! [`Reply`]: crate::coroutine::Reply
//! [`PageRequest`]: crate::coroutine::PageRequest
//! [`Workflow`]: crate::coroutine::Workflow
//! [`Coroutine`]: crate::coroutine::Coroutine
//! [parent module]: crate::coroutine
//! [`Coroutine::from_generator`]: crate::coroutine::Coroutine::from_generator

use delta_kernel_derive::internal_api;

#[internal_api]
pub(crate) use self::channel::{
    coroutine, static_coroutine, static_workflow, workflow, Channel, ChannelExt, GeneratorBody,
};
#[cfg(feature = "declarative-plans")]
#[internal_api]
pub(crate) use self::engine::drive_with_plan_executor;
#[doc(inline)]
#[internal_api]
pub(crate) use self::engine::{
    drive_with_catalog_committer, drive_with_committer, drive_with_engine, drive_with_storage,
    iterate_prepared_result_coroutine_with_engine, iterate_result_coroutine_with_engine,
    run_workflow_with_engine, run_workflow_with_storage,
};
pub use self::request::{PlanRequest, Request};
use crate::KernelResult;

#[cfg(feature = "internal-api")]
pub mod channel;
#[cfg(not(feature = "internal-api"))]
#[allow(unreachable_pub)]
pub(crate) mod channel;
pub(crate) mod engine;
pub mod request;

/// Kernel workflow that may borrow captured state.
pub type Workflow<'a, O> = super::Workflow<'a, Request, O>;

/// Kernel workflow that owns all captured state.
pub type StaticWorkflow<O> = super::StaticWorkflow<Request, O>;

/// Request or completion from a Kernel [`Workflow`].
pub type WorkflowStep<O> = super::WorkflowStep<Request, O>;

/// Kernel coroutine that may borrow captured state.
pub type Coroutine<'a, Y, O = ()> = super::Coroutine<'a, Request, Y, O>;

/// Kernel coroutine that owns all captured state.
pub type StaticCoroutine<Y, O = ()> = super::StaticCoroutine<Request, Y, O>;

/// Instrumented Kernel coroutine that owns all captured state.
pub type StaticInstrumentedCoroutine<Y, O = ()> = super::StaticInstrumentedCoroutine<Request, Y, O>;

/// Yield, request, or completion from a Kernel [`Coroutine`].
pub type CoroutineStep<Y, O = ()> = super::CoroutineStep<Request, Y, O>;

/// Unstarted Kernel result stream that yields `KernelResult<T>` items and completes with `O`.
///
/// Polling may fail before an item is yielded, each yielded item may independently be an error,
/// and `O` is returned only after successful terminal completion.
pub trait KernelResultGenerator<T, O = ()>:
    super::Generator<Request, Item = KernelResult<T>, Output = O>
where
    T: Send,
    O: Send + 'static,
{
}

impl<G, T, O> KernelResultGenerator<T, O> for G
where
    G: super::Generator<Request, Item = KernelResult<T>, Output = O>,
    T: Send,
    O: Send + 'static,
{
}
