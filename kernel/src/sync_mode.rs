//! Sync mode implementation
//!
//! This module provides the sync-mode implementation of kernel utilities.
//! When the async feature is disabled, this module is re-exported as the main kernel utilities.

use crate::DeltaResult;
use std::future::Future;
use futures::stream::{Stream, StreamExt as _};
use itertools::Itertools as _;

// Re-export macros
pub use delta_kernel_derive::{async_fn, async_trait};

/// Conditionally adds `.await` to an expression (no-op in sync mode)
#[macro_export]
macro_rules! await_ {
    ($e:expr) => { $e };
}

/// Cooperative yielding for long-running synchronous operations
///
/// In sync mode, this is a no-op since the OS thread scheduler handles preemption.
/// Included for API compatibility with async mode.
///
/// # Example
///
/// ```ignore
/// for item in large_collection {
///     expensive_computation(item);
///     yield_now!();  // No-op in sync mode
/// }
/// ```
#[macro_export]
macro_rules! yield_now {
    () => {
        // No-op in sync mode
    };
}

/// Create a closure that works uniformly in both sync and async modes
///
/// In sync mode, creates a regular closure.
/// In async mode, creates an async closure.
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
    (| $($arg:tt),* | $( -> $return:ty )? $body:block ) => {
        |$($arg),*| $( -> $return )? { $body }
    };
    (move | $($arg:tt),* | $( -> $return:ty )? $body:block ) => {
        move |$($arg),*| $( -> $return )? { $body }
    };
}

/// Type alias for boxed iterators
pub type BoxedAsyncIterator<T> = Box<dyn Iterator<Item = T> + Send>;

/// Unified trait for iterator operations
///
/// In sync mode, this is implemented for all Iterator types.
pub trait AsyncIterator: Iterator + Send + 'static {
    /// Get the next item from the iterator
    ///
    /// In sync mode, this is just a wrapper around `Iterator::next()`.
    /// In async mode, this becomes `.await` on the stream's poll.
    #[allow(clippy::should_implement_trait)]
    fn async_next(&mut self) -> Option<Self::Item> {
        self.next()
    }

    /// Map each item
    fn async_map<F, R>(self, f: F) -> impl AsyncIterator<Item = R>
    where
        F: FnMut(Self::Item) -> R + Send + 'static,
        R: Send + 'static,
        Self: Sized,
    {
        self.map(f)
    }

    /// Filter items
    fn async_filter<F>(self, f: F) -> impl AsyncIterator<Item = Self::Item>
    where
        F: FnMut(&Self::Item) -> bool + Send + 'static,
        Self: Sized,
    {
        self.filter(f)
    }

    /// Flatten nested iterators
    fn async_flatten(self) -> impl AsyncIterator<Item = <Self::Item as IntoIterator>::Item>
    where
        Self: Sized,
        Self::Item: IntoIterator,
        <Self::Item as IntoIterator>::Item: Send + 'static,
        <Self::Item as IntoIterator>::IntoIter: Send + 'static,
    {
        self.flatten()
    }

    /// Flatten an iterator of Result<Iterator> into an iterator of Result
    ///
    /// This is equivalent to itertools::flatten_ok. It handles Iterator<Item = Result<I, E>>
    /// where I: IntoIterator, and produces Iterator<Item = Result<I::Item, E>>.
    fn async_flatten_ok<T, E, I>(self) -> impl AsyncIterator<Item = Result<T, E>>
    where
        Self: Iterator<Item = Result<I, E>> + Sized,
        I: IntoIterator<Item = T> + 'static,
        I::IntoIter: Send + 'static,
        T: Send + 'static,
        E: Send + 'static,
    {
        self.flatten_ok()
    }

    /// Try fold with early exit on error
    ///
    /// This method requires the iterator to yield `Result` types.
    /// The closure receives the unwrapped `Ok` value and is synchronous
    /// (returns `Result<B, E>`).
    fn async_try_fold<T, E, B, F>(mut self, init: B, mut f: F) -> Result<B, E>
    where
        F: FnMut(B, T) -> Result<B, E> + Send + 'static,
        B: Send + 'static,
        T: Send + 'static,
        E: Send + 'static,
        Self: Iterator<Item = Result<T, E>> + Sized,
    {
        self.try_fold(init, move |acc, item| f(acc, item?))
    }

    /// Map over the successful values in a Result-yielding iterator
    ///
    /// This method requires the iterator to yield `Result` types.
    /// The closure receives the unwrapped `Ok` value and returns a new value.
    /// Errors are passed through unchanged.
    fn async_map_ok<T, E, F, R>(self, f: F) -> impl AsyncIterator<Item = Result<R, E>>
    where
        F: FnMut(T) -> R + Send + 'static,
        T: Send + 'static,
        E: Send + 'static,
        R: Send + 'static,
        Self: Iterator<Item = Result<T, E>> + Sized,
    {
        self.map_ok(f)
    }

    /// Chain two iterators
    fn async_chain<U>(self, other: U) -> impl AsyncIterator<Item = Self::Item>
    where
        U: IntoIterator<Item = Self::Item>,
        U::IntoIter: Send + 'static,
        Self: Sized,
    {
        self.chain(other)
    }

    /// Convert to boxed iterator
    fn into_boxed(self) -> BoxedAsyncIterator<Self::Item>
    where
        Self: Sized,
    {
        Box::new(self)
    }

    /// Transform items with a potentially async operation
    ///
    /// In sync mode, the closure returns `R` directly (synchronous).
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
    fn async_then<F, R>(self, f: F) -> impl AsyncIterator<Item = R>
    where
        F: FnMut(Self::Item) -> R + Send + 'static,
        R: Send + 'static,
        Self: Sized,
    {
        self.map(f)
    }
}

// Blanket implementation for all iterators
impl<I: Iterator + Send + 'static> AsyncIterator for I {}

/// Helper to convert IntoIterator types to AsyncIterator
pub fn into_async_iter<I: IntoIterator>(i: I) -> impl AsyncIterator<Item = I::Item>
where
    I::IntoIter: Send + 'static,
    I::Item: Send + 'static,
{
    i.into_iter()
}

/// Adapter for converting Stream-producing futures to BoxedAsyncIterator
///
/// In sync mode: blocks on the future and wraps the stream to block on each item.
///
/// # Panics
///
/// This function uses `futures::executor::block_on` which will panic if called from
/// within an async context. If you need to use kernel APIs from async code, enable
/// the `async` feature and use the async mode APIs instead.
pub fn into_boxed_async_iterator<Fut, S, T>(
    stream_future: Fut,
) -> DeltaResult<BoxedAsyncIterator<T>>
where
    Fut: Future<Output = DeltaResult<S>>,
    S: Stream<Item = T> + Send + 'static,
    T: Send + 'static,
{
    let mut stream = Box::pin(futures::executor::block_on(stream_future)?);
    let iter = std::iter::from_fn(move || {
        futures::executor::block_on(async { stream.next().await })
    });
    Ok(iter.into_boxed())
}

