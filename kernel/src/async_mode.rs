//! Async mode implementation
//!
//! This module provides the async-mode implementation of kernel utilities.
//! When the async feature is enabled, this module is re-exported as the main kernel utilities.

use futures::future;
use futures::stream::{Stream, StreamExt as _, TryStream, TryStreamExt as _};
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};

// Re-export macros
pub use async_trait::async_trait;
pub use delta_kernel_derive::{async_fn, async_trait_fn};

/// Conditionally adds `.await` to an expression (adds .await in async mode)
#[macro_export]
macro_rules! await_ {
    ($e:expr) => {
        $e.await
    };
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
/// In sync mode, creates a regular closure with zero-cost reference captures.
/// In async mode, creates an async closure and clones captured variables.
///
/// This allows writing closures that work in both sync and async contexts
/// with `async_then`, avoiding the FnMut incompatibility of async closures.
///
/// # Clone Specification
///
/// Use `clone[owned1, &ref2, &ref3, owned4, ...]` to specify:
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
/// // Mixed (both owned and refs)
/// items.async_then(async_closure!(move |item| clone[&ctx, table_root] -> DeltaResult<T> {
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
    // NOTE: Every item on the list is a pair of owned and borrowed items -- both optional. In practice
    // the macro will only succeed if one or the other is present, because we miss a comma between them.
    (move | $($arg:ident),* | $( clone[ $( $( $owned:ident )? $( &$borrowed:ident )? ),+ $(,)? ] )? $( -> $return:ty )? $body:block ) => {
        move |$($arg),*| {
            // Clone everything in async mode
            $( $(
                $( let $owned = $owned.clone(); )?
                $( let $borrowed = $borrowed.clone(); )?
            )+ )?
            async move {
                // Apply the return type here, because the async block returns impl Future and
                // closure return types are not allowed to use impl Trait syntax.
                let result $( : $return )? = $body;
                result
            }
        }
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
pub trait AsyncIterator: Stream + Send + Sized + 'static {
    /// Get the next item from the stream
    ///
    /// In sync mode, this is just a wrapper around `Iterator::next()`.
    /// In async mode, this polls the stream and must be awaited.
    fn async_next(&mut self) -> impl Future<Output = Option<Self::Item>> + Send
    where
        Self: Unpin,
    {
        async move { self.next().await }
    }

    /// Map each item synchronously
    ///
    /// Note: The closure `f` is synchronous - it returns `R`, not `Future<Output = R>`.
    /// This uses `StreamExt::map` directly for efficiency.
    fn async_map<F, R>(self, f: F) -> impl AsyncIterator<Item = R>
    where
        F: FnMut(Self::Item) -> R + Send + 'static,
        R: Send + 'static,
        Self::Item: Send,
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
    {
        self.filter(move |item| future::ready(f(item)))
    }

    /// Flatten nested streams
    fn async_flatten(self) -> impl AsyncIterator<Item = <Self::Item as Stream>::Item>
    where
        Self::Item: Stream + Send,
        <Self::Item as Stream>::Item: Send + 'static,
    {
        self.flatten()
    }

    /// Map and flatten - applies a function that returns an iterator/stream,
    /// then flattens the result
    fn async_flat_map<F, I>(self, mut f: F) -> impl AsyncIterator<Item = I::Item>
    where
        F: FnMut(Self::Item) -> I + Send + 'static,
        I: Stream + Send + 'static,
        I::Item: Send + 'static,
    {
        self.then(move |item| future::ready(f(item))).flatten()
    }

    /// Flatten a stream of Result<Stream<T>> into a stream of Result<T>
    ///
    /// This is the traditional flatten_ok semantics for streams:
    /// - Stream<Item = Result<S, E>> where S: Stream<Item = T>
    /// - Produces: Stream<Item = Result<T, E>>
    ///
    /// The inner stream yields plain values (not Results). Any error in the outer
    /// stream propagates through as Err.
    fn async_flatten_ok<T, E>(self) -> impl AsyncIterator<Item = Result<T, E>>
    where
        Self: TryStream<Error = E>,
        Self::Ok: Stream<Item = T> + Send + 'static,
        T: Send + 'static,
    {
        // Convert T to Result<T, E> so we can use try_flatten
        self.map_ok(|inner| inner.map(Ok)).try_flatten()
    }

    /// Flatten a stream of Result<Stream<Result<T>>> into a stream of Result<T>
    ///
    /// This emulates TryStreamExt::try_flatten behavior:
    /// - Stream<Item = Result<S, E>> where S: Stream<Item = Result<T, E>>
    /// - Produces: Stream<Item = Result<T, E>>
    ///
    /// Both the outer and inner streams yield Results. Any error at either level
    /// propagates through as Err.
    fn async_try_flatten<T>(self) -> impl AsyncIterator<Item = Result<T, Self::Error>>
    where
        Self: TryStream,
        Self::Ok: TryStream<Ok = T, Error = Self::Error> + Send,
        T: Send + 'static,
    {
        self.try_flatten()
    }

    /// Folds an iterator into a single value.
    fn async_fold<B, F>(self, init: B, mut f: F) -> impl Future<Output = B> + Send
    where
        F: FnMut(B, Self::Item) -> B + Send + 'static,
        B: Send + 'static,
        Self: Unpin,
        Self::Item: Send + 'static,
    {
        async move {
            self.fold(init, |acc, item| future::ready(f(acc, item)))
                .await
        }
    }

    /// Try fold with early exit on error
    ///
    /// This method requires the stream to yield `Result` types.
    /// The closure receives the unwrapped `Ok` value and is **synchronous**
    /// (returns `Result<B, E>`, not `Future<Output = Result<B, E>>`).
    ///
    /// Note: Unlike `TryStreamExt::try_fold`, this takes a sync closure for consistency
    /// with sync mode. The closure is wrapped in `future::ready` internally.
    fn async_try_fold<B, F>(
        self,
        init: B,
        mut f: F,
    ) -> impl Future<Output = Result<B, Self::Error>> + Send
    where
        F: FnMut(B, Self::Ok) -> Result<B, Self::Error> + Send + 'static,
        B: Send + 'static,
        Self: Unpin + TryStream,
        Self::Ok: Send + 'static,
        Self::Error: Send + 'static,
    {
        async move {
            self.try_fold(init, |acc, item| future::ready(f(acc, item)))
                .await
        }
    }

    /// Stateful stream adapter that maintains state and can optionally terminate early.
    ///
    /// Similar to Iterator::scan, this maintains a state value and uses it to transform
    /// each item. The closure can return None to terminate the stream early.
    fn async_scan<St, B, F>(self, initial_state: St, mut f: F) -> impl AsyncIterator<Item = B>
    where
        F: FnMut(&mut St, Self::Item) -> Option<B> + Send + 'static,
        St: Send + 'static,
        B: Send + 'static,
        Self::Item: Send + 'static,
    {
        self.scan(initial_state, move |st, item| future::ready(f(st, item)))
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
        Self: TryStream,
        Self::Ok: Send + 'static,
        Self::Error: Send + 'static,
    {
        self.map_ok(f)
    }

    /// Filter and map over the successful values in a Result-yielding stream
    ///
    /// This method requires the stream to yield `Result` types.
    /// The closure receives the unwrapped `Ok` value and returns `Option<R>`:
    /// - `Some(value)` yields `Ok(value)`
    /// - `None` skips the item
    /// - Errors are passed through unchanged
    ///
    /// This matches itertools::Itertools::filter_map_ok semantics.
    /// Uses futures::stream::TryStreamExt::try_filter_map with a non-failing closure.
    fn async_filter_map_ok<F, R>(self, mut f: F) -> impl AsyncIterator<Item = Result<R, Self::Error>>
    where
        F: FnMut(Self::Ok) -> Option<R> + Send + 'static,
        R: Send + 'static,
        Self: TryStream,
        Self::Ok: Send + 'static,
        Self::Error: Send + 'static,
    {
        self.try_filter_map(move |value| future::ready(Ok(f(value))))
    }

    /// Take items while predicate is true
    ///
    /// Yields items from the stream as long as the predicate returns `true`.
    /// Once the predicate returns `false`, the stream stops.
    fn async_take_while<F>(self, mut f: F) -> impl AsyncIterator<Item = Self::Item>
    where
        F: FnMut(&Self::Item) -> bool + Send + 'static,
        Self::Item: Send + 'static,
    {
        self.take_while(move |item| future::ready(f(item)))
    }

    /// Chain two streams
    fn async_chain<U>(self, other: U) -> impl AsyncIterator<Item = Self::Item>
    where
        U: Stream<Item = Self::Item> + Send + 'static,
    {
        self.chain(other)
    }

    /// Zip two streams together
    ///
    /// Combines this stream with another, yielding pairs of items.
    /// The resulting stream ends when either input stream ends.
    fn async_zip<U>(self, other: U) -> impl AsyncIterator<Item = (Self::Item, U::Item)>
    where
        U: Stream + Send + 'static,
        U::Item: Send,
        Self::Item: Send,
    {
        self.zip(other)
    }

    /// Collect a stream of Results into a Result of collection
    ///
    /// This returns a Future in async mode. Use with `await_!(stream.async_try_collect())`.
    fn async_try_collect<C>(self) -> impl Future<Output = Result<C, Self::Error>> + Send
    where
        Self: TryStream + Unpin,
        C: Default + Extend<Self::Ok> + Send,
        Self::Ok: Send,
        Self::Error: Send,
    {
        self.try_collect()
    }

    /// Collect a stream into a collection
    ///
    /// This returns a Future in async mode. Use with `await_!(stream.async_collect())`.
    fn async_collect<C>(self) -> impl Future<Output = C> + Send
    where
        Self: Unpin,
        C: Default + Extend<Self::Item> + Send,
        Self::Item: Send,
    {
        self.collect()
    }

    /// Count the number of items in the iterator
    fn async_count(self) -> impl Future<Output = usize> + Send
    where
        Self::Item: Send + 'static,
    {
        self.count()
    }

    /// Enumerate items with their index
    fn async_enumerate(self) -> impl AsyncIterator<Item = (usize, Self::Item)>
    where
        Self::Item: Send + 'static,
    {
        self.enumerate()
    }

    /// Returns an iterable version of this AsyncIterator (ie suitable for use with [`Self::async_next`]).
    ///
    /// In sync mode, this is a no-op (every iterator is iterable)
    /// In async mode, the returned stream implements `Unpin` (required by `async_next`).
    fn async_pin(self) -> impl AsyncIterator<Item = Self::Item> + Unpin {
        self.into_boxed()
    }

    /// Convert to boxed stream
    fn into_boxed(self) -> BoxedAsyncIterator<Self::Item> {
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
