//! Kernel coroutines and generators post [`Request`]s through a [`Channel`] and await connector
//! replies.
//!
//! Each operation owns the receiving end of its request channel. Generator bodies receive the
//! same request channel plus a separate lane for yielded items.
use delta_kernel_derive::internal_api;

use crate::coroutine::generator;
use crate::DeltaResult;

mod channel;
pub mod request;

#[doc(inline)]
#[internal_api]
pub(crate) use channel::ChannelExt;
pub use request::{PlanRequest, Request};

/// Borrowed capability used by kernel operations to post requests through their parent coroutine.
///
/// Coroutine implementations receive this channel as an ordinary async-function argument. Child
/// generators use it for the lifetime of their [`BoundGenerator`].
pub type Channel = super::Channel<Request>;

/// Unbound kernel generator equivalent to
/// `IntoGenerator<Request, Item = DeltaResult<T>, Output = O>`.
///
/// Polling may fail before an item is yielded, each yielded item may independently be an error,
/// and `O` is returned only after successful terminal completion.
pub trait DeltaResultGenerator<T, O = ()>:
    generator::IntoGenerator<Request, Item = DeltaResult<T>, Output = O> + Sized
where
    T: Send,
    O: Send + 'static,
{
}

impl<G, T, O> DeltaResultGenerator<T, O> for G
where
    G: generator::IntoGenerator<Request, Item = DeltaResult<T>, Output = O>,
    T: Send,
    O: Send + 'static,
{
}

/// Active streaming kernel operation using an existing request channel.
///
/// Connector requests surface through the driver that owns the channel. The `'a` lifetime
/// covers that channel and any state captured by the operation.
pub type BoundGenerator<'a, Y, O = ()> = generator::BoundGenerator<'a, Y, O>;

/// Independently driven kernel generator that may borrow captured state.
pub type GeneratorCoroutine<'a, Y, O = ()> = generator::GeneratorCoroutine<'a, Request, Y, O>;

/// Kernel generator that owns all captured state and its request channel.
pub type StaticGeneratorCoroutine<Y, O = ()> = generator::StaticGeneratorCoroutine<Request, Y, O>;

/// Yield, request, or completion from a kernel [`StaticGeneratorCoroutine`].
///
/// Yielded items are removed from the pending queue before this step returns. Requests carry their
/// own replies.
pub type GeneratorCoroutineStep<Y, O = ()> = generator::GeneratorCoroutineStep<Request, Y, O>;

/// Independently driven kernel operation with an owned request channel.
///
/// Process each returned request and complete its reply.
pub type Coroutine<'a, O> = super::Coroutine<'a, Request, O>;

/// Kernel coroutine that owns all captured state.
pub type StaticCoroutine<O> = super::StaticCoroutine<Request, O>;

/// Request or completion from a kernel [`Coroutine`].
pub type CoroutineStep<O> = super::CoroutineStep<Request, O>;

pub(crate) type GeneratorChannel<'a, Y> = generator::GeneratorChannel<'a, Request, Y>;
pub(crate) use generator::generator_fn;

/// Creates an independently driven generator that may borrow captured state.
#[internal_api]
macro_rules! generator_coroutine {
    ($body:expr) => {
        $crate::coroutine::generator::generator_coroutine!(
            $crate::coroutine::kernel::Request,
            $body
        )
    };
}

/// Creates an unpolled [`StaticGeneratorCoroutine`] from a static generator body.
#[internal_api]
macro_rules! static_generator_coroutine {
    ($body:expr) => {
        $crate::coroutine::generator::static_generator_coroutine!(
            $crate::coroutine::kernel::Request,
            $body
        )
    };
}

/// Creates a [`Coroutine`] that may borrow captured state.
#[internal_api]
macro_rules! coroutine {
    ($body:expr) => {
        $crate::coroutine::coroutine!($crate::coroutine::kernel::Request, $body)
    };
}

/// Creates an unpolled [`StaticCoroutine`] from a static coroutine body.
#[internal_api]
macro_rules! static_coroutine {
    ($body:expr) => {
        $crate::coroutine::static_coroutine!($crate::coroutine::kernel::Request, $body)
    };
}
