//! Sync mode implementation
//!
//! This module provides the sync-mode implementation of kernel utilities.
//! When the async feature is disabled, this module is re-exported as the main kernel utilities.

use crate::DeltaResult;
use futures::stream::{Stream, StreamExt as _};
use itertools::Itertools as _;
use std::future::Future;

// Re-export macros
pub use delta_kernel_derive::{async_fn, async_trait, async_trait_fn};

/// Conditionally adds `.await` to an expression (no-op in sync mode)
#[macro_export]
macro_rules! await_ {
    ($e:expr) => {
        $e
    };
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
/// In sync mode, creates a regular closure with zero-cost reference captures.
/// In async mode, creates an async closure and clones captured variables.
///
/// Use this with `async_then` to perform I/O operations on stream items.
///
/// # Clone Specification
///
/// Use e.g. `clone[owned1, &ref2, &ref3, owned4, ...]` to specify:
/// - `var`: Variables captured by value (cloned in both modes)
/// - `&var`: Variables captured by reference (zero-cost in sync, cloned in async)
///
/// # Examples
///
/// ```ignore
/// // With reference captures (zero-cost in sync mode)
/// items.async_then(async_closure!(move |item| clone[&engine, &schema] {
///     let data = await_!(fetch_data(item, engine))?;
///     Ok(process(data, schema))
/// }))
///
/// // With owned captures (cloned in both modes)
/// items.async_then(async_closure!(move |item| clone[table_root] {
///     await_!(process(item, table_root))
/// }))
///
/// // Mixed (both refs and owned)
/// items.async_then(async_closure!(move |item| clone[table_root, &ctx] -> DeltaResult<T> {
///     await_!(fetch(item, ctx, table_root))
/// }))
///
/// // Without clones (simple case)
/// items.async_then(async_closure!(move |item| {
///     Ok(process(item?))
/// }))
/// ```
#[macro_export]
macro_rules! async_closure {
    (move | $($arg:tt),* | $( clone[ $( $( $owned:ident )? $( &$borrowed:ident )? ),+ $(,)? ] )? $( -> $return:ty )? $body:block ) => {
        move |$($arg),*| $( -> $return )? {
            // Only clone owned values in sync mode (refs are zero-cost)
            $( $( $( let $owned = $owned.clone(); )? )+ )?
            $body
        }
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

    /// Flatten an iterator of Result<Iterator<T>> into an iterator of Result<T>
    ///
    /// This is the traditional itertools `flatten_ok` semantics:
    /// - Iterator<Item = Result<I, E>> where I: IntoIterator<Item = T>
    /// - Produces: Iterator<Item = Result<T, E>>
    ///
    /// The inner iterator should yield plain values (not Results). Use [`Self::async_try_flatten`]
    //  when both outer and inner iterators yield Results.
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

    /// Flatten an iterator of Result<Iterator<Result<T>>> into an iterator of Result<T>
    ///
    /// This emulates TryStreamExt::try_flatten behavior:
    /// - Iterator<Item = Result<I, E>> where I: IntoIterator<Item = Result<T, E>>
    /// - Produces: Iterator<Item = Result<T, E>>
    ///
    /// Both the outer and inner iterators yield Results. Any error at either level
    /// propagates through as Err.
    fn async_try_flatten<T, E, I>(self) -> impl AsyncIterator<Item = Result<T, E>>
    where
        Self: Iterator<Item = Result<I, E>> + Sized,
        I: IntoIterator<Item = Result<T, E>> + 'static,
        I::IntoIter: Send + 'static,
        T: Send + 'static,
        E: Send + 'static,
    {
        // Use flatten_ok to flatten the iterator structure, producing Iterator<Item = Result<Result<T, E>, E>>
        // Then map to flatten the nested Results: Result<Result<T, E>, E> -> Result<T, E>
        self.flatten_ok().map(|result| result?)
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
    let iter =
        std::iter::from_fn(move || futures::executor::block_on(async { stream.next().await }));
    Ok(iter.into_boxed())
}
