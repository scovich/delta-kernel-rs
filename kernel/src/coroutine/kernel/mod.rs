//! Kernel's connector request vocabulary and the coroutine types specialized to it.
//!
//! Kernel operations surface connector work as [`Request`]s. Each variant carries an operation
//! payload from [`request`] and the [`Reply`] that completes it, either directly or nested in a
//! [`PlanRequest`] or [`PageRequest`]. The aliases in this module fix the generic [`Workflow`] and
//! [`Coroutine`] to this vocabulary; the [parent module] explains how to drive them.
//!
//! Some entry points return a workflow whose output is a [`DeltaResultGenerator`]. Pass that
//! output to [`Coroutine::from_generator`] to drive its stream.
//!
//! [`Request`]: crate::coroutine::kernel::Request
//! [`request`]: crate::coroutine::kernel::request
//! [`PlanRequest`]: crate::coroutine::kernel::PlanRequest
//! [`PageRequest`]: crate::coroutine::PageRequest
//! [`Reply`]: crate::coroutine::Reply
//! [`Workflow`]: crate::coroutine::Workflow
//! [`Coroutine`]: crate::coroutine::Coroutine
//! [parent module]: crate::coroutine
//! [`DeltaResultGenerator`]: crate::coroutine::kernel::DeltaResultGenerator
//! [`Coroutine::from_generator`]: crate::coroutine::Coroutine::from_generator
use delta_kernel_derive::internal_api;

#[internal_api]
pub(crate) use self::channel::{
    coroutine, static_coroutine, static_workflow, workflow, BoundDeltaResultGenerator, Channel,
    ChannelExt, GeneratorBody,
};
pub(crate) use self::channel::{generator_fn, GeneratorChannel};
#[doc(inline)]
#[internal_api]
pub(crate) use self::engine::{
    drive_with_engine, drive_with_engine_and_committer, drive_with_storage,
    iterate_prepared_result_coroutine_with_engine, iterate_result_coroutine_with_engine,
    run_workflow_with_engine, run_workflow_with_storage,
};
pub use self::request::{PlanRequest, Request};
use crate::coroutine::generator;
use crate::DeltaResult;

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

/// Request or completion from a kernel [`Workflow`].
pub type WorkflowStep<O> = super::WorkflowStep<Request, O>;

/// Kernel coroutine that may borrow captured state.
pub type Coroutine<'a, Y, O = ()> = generator::Coroutine<'a, Request, Y, O>;

/// Kernel coroutine that owns all captured state.
pub type StaticCoroutine<Y, O = ()> = generator::StaticCoroutine<Request, Y, O>;

/// Yield, request, or completion from a kernel [`Coroutine`].
pub type CoroutineStep<Y, O = ()> = generator::CoroutineStep<Request, Y, O>;

/// Unstarted Kernel result stream that yields `DeltaResult<T>` items and completes with `O`.
///
/// Polling may fail before an item is yielded, each yielded item may independently be an error,
/// and `O` is returned only after successful terminal completion.
pub trait DeltaResultGenerator<T, O = ()>:
    generator::Generator<Request, Item = DeltaResult<T>, Output = O>
where
    T: Send,
    O: Send + 'static,
{
}

impl<G, T, O> DeltaResultGenerator<T, O> for G
where
    G: generator::Generator<Request, Item = DeltaResult<T>, Output = O>,
    T: Send,
    O: Send + 'static,
{
}
