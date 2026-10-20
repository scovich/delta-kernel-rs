//! Runtime-neutral async protocol logic with connector-owned execution.
//!
//! Kernel protocol operations are ordinary Rust futures. Futures let Kernel suspend, but that alone
//! does not avoid control inversion. Merely making connector callbacks async would still require
//! Kernel to invoke connector code through kernel-defined interfaces the connector must implement.
//!
//! A Kernel coroutine instead combines a future with a typed [`Channel`]. When Kernel needs
//! connector work, it posts a [`Request`] through that channel and awaits its [`Reply`]. The
//! request surfaces to the connector as an output of driving the coroutine. Each request carries a
//! single-use reply tied to its originating await point. Sending or dropping the reply makes that
//! point ready and wakes an async driver, but only the connector-owned driver decides when to poll
//! the coroutine again.
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
//! for it, so outstanding replies do not keep a coroutine alive. Dropping the coroutine or
//! generator abandons the operation, and subsequent replies to any of its outstanding requests are
//! silently discarded.
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
//! # Choosing the ownership boundary
//!
//! A [`Workflow`] already owns its request channel. For a streaming operation without a parent,
//! pass owned inputs through [`StaticCoroutine::new`]. Its callback must return
//! `GeneratorBody<'static, _>`, statically preventing borrowed state from escaping.
//!
//! Kernel code that already has a [`kernel::Channel`] calls async implementations directly. Passing
//! that channel to a streaming operation returns a [`GeneratorBody`] whose lifetime also covers
//! any other borrowed inputs. Requests then surface from the parent channel. Kernel may compose
//! channel-provided futures with runtime-neutral future and stream combinators; every operation
//! requiring connector work still passes through the channel.
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
//! [`Error::WouldBlock`] if Kernel suspends without producing a step, which can only happen if
//! there are outstanding replies.
use std::any::{type_name, Any};
use std::future::Future;

use delta_kernel_derive::internal_api;
use derive_more::Constructor;

pub use self::channel::Reply;
#[doc(inline)]
#[internal_api]
pub(crate) use self::channel::{Channel, ChannelExchange, DeltaFuture};
use self::generator::{unfold_mut, ChannelBinding};
#[doc(inline)]
#[internal_api]
pub(crate) use self::generator::{
    BoundGenerator, BoxedBoundGenerator, Flatten, Generator, GeneratorBody, GeneratorExt,
    GeneratorStep,
};
pub use self::generator::{Coroutine, CoroutineStep, StaticCoroutine};
#[doc(inline)]
#[internal_api]
pub(crate) use self::kernel::ChannelExt;
pub use self::kernel::{BoundDeltaResultGenerator, DeltaResultGenerator, PlanRequest, Request};
#[doc(hidden)]
#[internal_api]
pub(crate) use self::workflow::BodyFn;
pub use self::workflow::{drive_workflow, drive_workflow_async};
#[internal_api]
pub(crate) use self::workflow::{static_workflow, workflow};
pub use self::workflow::{StaticWorkflow, Workflow, WorkflowStep};
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

mod async_ext;
mod channel;
pub(crate) mod engine;
mod workflow;
#[doc(inline)]
#[internal_api]
pub(crate) use self::async_ext::AsyncResult;
pub(crate) use self::async_ext::{AsyncBool, AsyncOption};
// cbindgen ignores cfg gates on module declarations. Ignore both arms so coroutine-only generic
// aliases are not registered or monomorphized into the C API.
#[cfg(feature = "internal-api")]
/// cbindgen:ignore
pub mod generator;
#[cfg(not(feature = "internal-api"))]
/// cbindgen:ignore
#[allow(unreachable_pub)]
pub(crate) mod generator;
#[cfg(feature = "internal-api")]
/// cbindgen:ignore
pub mod kernel;
#[cfg(not(feature = "internal-api"))]
/// cbindgen:ignore
#[allow(unreachable_pub)]
pub(crate) mod kernel;

/// Prove type and lifetime bounds on the future produced by an async callable.
///
/// Every coroutine owns an underlying [`Future`] that executes its logic, invariably of an unnamed
/// type produced by an async callable implementing e.g. [`AsyncFnOnce`]. The lifetime of that
/// future is bounded by any borrowed state it closes over, and it is Send only if every value
/// stored in its suspended state is Send.
///
/// When using an async callable to construct a new coroutine, the compiler must prove that the
/// resulting future is `Send + 'a`, where `'a` is the lifetime of the coroutine. Unfortunately,
/// stable Rust gives no direct way to apply type or lifetime bounds to the future produced by an
/// async callable. Naive code such as the following fails to compile:
///
/// ```compile_fail
/// # use std::future::Future;
/// fn foo<'a, A, O>(
///     mut f: impl AsyncFnMut(A) -> O + 'a,
///     arg: A,
/// ) -> impl Future<Output = O> + Send + 'a
/// {
///     f(arg)
/// }
/// ```
///
/// Fortunately, a carefully crafted "witness" idiom allows us to apply type and lifetime bounds to
/// that future even tho we cannot name it. This function generalizes the idiom for easy reference
/// and reuse.
///
/// # How it works
///
/// ```
/// # use std::future::Future;
/// async fn witness<A, O>(mut f: impl AsyncFnMut(A) -> O, arg: A) -> O {
///     f(arg).await
/// }
///
/// fn invoke_witness<'a, A, O, F, Fut>(
///     witness: impl FnOnce(F, A) -> Fut,
///     f: F,
///     arg: A,
/// ) -> Fut
/// where
///     F: AsyncFnMut(A) -> O,
///     Fut: Future<Output = O> + Send + 'a,
/// {
///     witness(f, arg)
/// }
///
/// let future = invoke_witness(witness, async |i| i + 1, 10);
/// ```
///
/// Passing the uninvoked `witness` lets type inference bind its opaque return type to `Fut` (the
/// witness’s outer future, which owns and awaits the unnamed inner future produced by the async
/// callable). Requiring `Fut: 'a` ensures that the witness future and any external state it
/// captures remain valid for the coroutine’s lifetime; `Fut: Send` applies transitively to the
/// inner future held across the await. The witness itself only needs the expressible argument and
/// output bounds on the async callable.
///
/// Crucially, `invoke_witness` must be called from a context where the async callable's concrete
/// type is still exposed to type inference. A caller generic over `F: AsyncFn*` is universally
/// quantified over every `F` satisfying those bounds, so it can only prove properties of `F`'s
/// unnameable call future that those bounds express (namely, its return type). With a particular
/// callable type, inference instead checks whether this specific `Fut` -- the witness future
/// produced for that callable -- satisfies `Fut: Send + 'a`. Invoking the witness inside an
/// already universally quantified generic layer cannot recover that proof:
///
/// ```compile_fail
/// # use std::future::Future;
/// # fn invoke_witness<'a, A, O, F, Fut>(
/// #     witness: impl FnOnce(F, A) -> Fut,
/// #     f: F,
/// #     arg: A,
/// # ) -> Fut
/// # where
/// #     F: AsyncFnMut(A) -> O,
/// #     Fut: Future<Output = O> + Send + 'a,
/// # {
/// #     witness(f, arg)
/// # }
/// async fn witness<A, O>(mut f: impl AsyncFnMut(A) -> O, arg: A) -> O {
///     f(arg).await
/// }
/// fn call_generically<'a, A: Send + 'a, O: Send + 'a>(
///     f: impl AsyncFnMut(A) -> O + Send + 'a,
///     arg: A,
/// ) -> impl Future<Output = O> + Send + 'a {
///     invoke_witness(witness, f, arg)
/// }
/// ```
///
/// NOTE: This witness incantation could be removed if either of the [`return_type_notation`] or
/// [`async_fn_traits`] language features were to stabilize.
///
/// [`return_type_notation`]: https://doc.rust-lang.org/beta/unstable-book/language-features/return-type-notation.html
/// [`async_fn_traits`]: https://doc.rust-lang.org/beta/unstable-book/library-features/async-fn-traits.html
pub fn invoke_witness<'a, Callable, Arg, Fut>(
    witness: impl FnOnce(Callable, Arg) -> Fut,
    callable: Callable,
    arg: Arg,
) -> Fut
where
    Fut: Future + Send + 'a,
{
    witness(callable, arg)
}

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
    /// Cursor to pass to [`PageRequest::Next`], or `None` when the operation is exhausted.
    ///
    /// An empty payload with `Some(cursor)` is not exhaustion.
    pub next: Option<Cursor<Op>>,
}

/// Opaque connector-owned continuation state typed to one paged operation.
///
/// Kernel stores and returns cursors without inspecting their contents.
pub struct Cursor<Op> {
    state: Box<dyn Any + Send>,
    _op_type: PhantomType<Op>,
}

impl<Op> Cursor<Op> {
    /// Wrap connector-owned `state` for a later continuation request.
    pub fn new(state: impl Any + Send + 'static) -> Self {
        Self {
            state: Box::new(state),
            _op_type: PhantomType::default(),
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
    /// Initialize the operation and return a cursor without fetching the first page.
    Prepare(Op, Reply<Cursor<Op>>),
    /// Consume a cursor and return the next page.
    Next(Cursor<Op>, Reply<Page<Op>>),
}

/// Unbound generator of page payloads from a prepared cursor.
#[internal_api]
pub(crate) enum PageGenerator<Op: PagedOperation> {
    /// Stream pages from `cursor`.
    Next(Cursor<Op>),
    /// The operation is exhausted.
    Done,
}

impl<Op: PagedOperation> GeneratorExt for PageGenerator<Op> {}

impl<P, Op> Generator<P> for PageGenerator<Op>
where
    P: Send + 'static,
    Op: PagedOperation,
    PageRequest<Op>: Into<P>,
{
    type Item = Op::Page;
    type Output = ();

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl BoundGenerator<Item = Op::Page, Output = ()> + Unpin + 'a
    where
        Self: 'a,
    {
        unfold_mut!(BoundPages::new(channel, self), BoundPages::next)
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedBoundGenerator<'a, Op::Page>
    where
        Self: 'a,
    {
        Box::new((*self).bind_to(channel))
    }
}

struct BoundPages<'a, P: Send + 'static, Op: PagedOperation> {
    channel: ChannelBinding<'a, P>,
    state: PageGenerator<Op>,
}

impl<'a, P: Send + 'static, Op: PagedOperation> BoundPages<'a, P, Op> {
    fn new(channel: ChannelBinding<'a, P>, state: PageGenerator<Op>) -> Self {
        Self { channel, state }
    }

    async fn next(&mut self) -> DeltaResult<Option<Op::Page>>
    where
        PageRequest<Op>: Into<P>,
    {
        let cursor = match std::mem::replace(&mut self.state, PageGenerator::Done) {
            PageGenerator::Next(cursor) => cursor,
            PageGenerator::Done => return Ok(None),
        };
        let page = self.channel.exchange(cursor, PageRequest::Next).await?;
        if let Some(cursor) = page.next {
            self.state = PageGenerator::Next(cursor);
        }
        Ok(Some(page.data))
    }
}

#[doc(inline)]
#[internal_api]
pub(crate) use self::engine::drive_with_storage;
#[doc(inline)]
#[internal_api]
pub(crate) use self::engine::{
    drive_with_engine, drive_with_engine_and_committer, iterate_result_generator_with_engine,
};
#[doc(inline)]
#[internal_api]
pub(crate) use self::engine::{
    iterate_prepared_result_generator_with_engine, run_workflow_with_engine,
    run_workflow_with_storage,
};

#[cfg(test)]
mod tests;
