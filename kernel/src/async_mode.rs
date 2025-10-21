//! Async mode implementation
//!
//! This module provides the async-mode implementation of kernel utilities.
//! When the async feature is enabled, this module is re-exported as the main kernel utilities.

use crate::DeltaResult;
use futures::future;
use futures::stream::{Stream, StreamExt as _, TryStream, TryStreamExt as _};
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};

// Re-export macros
pub use async_trait::async_trait;
pub use delta_kernel_derive::async_fn;

/// Conditionally adds `.await` to an expression (adds .await in async mode)
#[macro_export]
macro_rules! await_ {
    ($e:expr) => { $e.await };
}

/// Cooperative yielding for long-running synchronous operations
///
/// In async mode, yields control back to the executor to prevent task starvation.
/// Use this in loops or computations that might run for a long time without hitting
/// natural await points.
///
/// # Example
///
/// ```ignore
/// for item in large_collection {
///     expensive_computation(item);
///     yield_now!();  // Yield periodically to allow other tasks to run
/// }
/// ```
#[macro_export]
macro_rules! yield_now {
    () => {
        $crate::YieldNow::default().await
    };
}

/// Create a closure that works uniformly in both sync and async modes
///
/// In async mode, creates an async closure.
/// In sync mode, creates a regular closure.
///
/// Use this with `async_then` to perform I/O operations on stream items.
///
/// # Example
///
/// ```ignore
/// items.async_then(async_closure!(|item| {
///     let data = await_!(fetch_data(item))?;
///     Ok(process(data))
/// }))
/// ```
#[macro_export]
macro_rules! async_closure {
    (| $($arg:tt),* | $body:expr) => {
        async |$($arg),*| $body
    };
    (move | $($arg:tt),* | $body:expr) => {
        async move |$($arg),*| $body
    };
}

/// Type alias for boxed streams
pub type BoxedAsyncIterator<T> = Pin<Box<dyn Stream<Item = T> + Send>>;

/// Future that yields control back to the executor once
///
/// Used by the `yield_now!()` macro to implement cooperative yielding in async mode.
#[derive(Default)]
pub struct YieldNow {
    yielded: bool,
}

impl Future for YieldNow {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<()> {
        if self.yielded {
            Poll::Ready(())
        } else {
            self.yielded = true;
            cx.waker().wake_by_ref();
            Poll::Pending
        }
    }
}

/// Unified trait for iterator operations
///
/// In async mode, this is implemented for all Stream types.
pub trait AsyncIterator: Stream + Send + 'static {
    /// Map each item synchronously
    ///
    /// Note: The closure `f` is synchronous - it returns `R`, not `Future<Output = R>`.
    /// This uses `StreamExt::map` directly for efficiency.
    fn async_map<F, R>(self, f: F) -> impl AsyncIterator<Item = R>
    where
        F: FnMut(Self::Item) -> R + Send + 'static,
        R: Send + 'static,
        Self::Item: Send,
        Self: Sized,
    {
        self.map(f)
    }

    /// Filter items synchronously
    ///
    /// Note: The closure `f` is synchronous - it returns `bool`, not `Future<Output = bool>`.
    /// We wrap the predicate result in a ready future to satisfy Stream's signature.
    fn async_filter<F>(self, mut f: F) -> impl AsyncIterator<Item = Self::Item>
    where
        F: FnMut(&Self::Item) -> bool + Send + 'static,
        Self::Item: Send,
        Self: Sized,
    {
        self.filter(move |item| future::ready(f(&item)))
    }

    /// Flatten nested streams
    fn async_flatten(self) -> impl AsyncIterator<Item = <Self::Item as Stream>::Item>
    where
        Self: Sized,
        Self::Item: Stream + Send,
        <Self::Item as Stream>::Item: Send + 'static,
    {
        self.flatten()
    }

    /// Try fold with early exit on error
    ///
    /// This method requires the stream to yield `Result` types.
    /// The closure receives the unwrapped `Ok` value.
    async fn async_try_fold<B, F>(self, init: B, mut f: F) -> Result<B, Self::Error>
    where
        F: FnMut(B, Self::Ok) -> Result<B, Self::Error> + Send + 'static,
        B: Send + 'static,
        Self: Unpin + Sized + TryStream,
        Self::Ok: Send + 'static,
        Self::Error: Send + 'static,
    {
        self.try_fold(init, |acc, item| future::ready(f(acc, item))).await
    }

    /// Map over the successful values in a Result-yielding stream
    ///
    /// This method requires the stream to yield `Result` types.
    /// The closure receives the unwrapped `Ok` value and returns a new value.
    /// Errors are passed through unchanged.
    fn async_map_ok<F, R>(self, f: F) -> impl AsyncIterator<Item = Result<R, Self::Error>>
    where
        F: FnMut(Self::Ok) -> R + Send + 'static,
        R: Send + 'static,
        Self: Sized + TryStream,
        Self::Ok: Send + 'static,
        Self::Error: Send + 'static,
    {
        self.map_ok(f)
    }

    /// Chain two streams
    fn async_chain<U>(self, other: U) -> impl AsyncIterator<Item = Self::Item>
    where
        U: Stream<Item = Self::Item> + Send + 'static,
        Self: Sized,
    {
        self.chain(other)
    }

    /// Convert to boxed stream
    fn into_boxed(self) -> BoxedAsyncIterator<Self::Item>
    where
        Self: Sized,
    {
        Box::pin(self)
    }

    /// Transform items with an async operation
    ///
    /// In async mode, the closure returns a `Future` (async operation).
    /// Use with `async_closure!` macro for code that works in both modes.
    ///
    /// # Example
    ///
    /// ```ignore
    /// items.async_then(async_closure!(|item| {
    ///     let data = await_!(fetch_data(item))?;
    ///     Ok(process(data))
    /// }))
    /// ```
    fn async_then<F, Fut, R>(self, f: F) -> impl AsyncIterator<Item = R>
    where
        F: FnMut(Self::Item) -> Fut + Send + 'static,
        Fut: Future<Output = R> + Send + 'static,
        R: Send + 'static,
        Self: Sized,
    {
        self.then(f)
    }
}

// Blanket implementation for all streams
impl<S: Stream + Send + 'static> AsyncIterator for S {}

/// Helper to convert IntoIterator types to AsyncIterator (Stream)
pub fn into_async_iter<I: IntoIterator>(i: I) -> impl AsyncIterator<Item = I::Item>
where
    I::IntoIter: Send + 'static,
    I::Item: Send + 'static,
{
    futures::stream::iter(i)
}

/// Adapter for converting Stream-producing futures to BoxedAsyncIterator
///
/// In async mode: awaits the future and boxes the stream.
pub async fn into_boxed_async_iterator<Fut, S, T>(
    stream_future: Fut,
) -> DeltaResult<BoxedAsyncIterator<T>>
where
    Fut: Future<Output = DeltaResult<S>>,
    S: Stream<Item = T> + Send + 'static,
    T: Send + 'static,
{
    let stream = stream_future.await?;
    Ok(stream.into_boxed())
}

