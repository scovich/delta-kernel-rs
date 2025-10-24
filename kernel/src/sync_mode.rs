//! Sync mode implementation
//!
//! This module provides the sync-mode implementation of kernel utilities.
//! When the async feature is disabled, this module is re-exported as the main kernel utilities.

use itertools::Itertools as _;

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
pub trait AsyncIterator: Iterator + Send + Sized + 'static {
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
    {
        self.map(f)
    }

    /// Filter items
    fn async_filter<F>(self, f: F) -> impl AsyncIterator<Item = Self::Item>
    where
        F: FnMut(&Self::Item) -> bool + Send + 'static,
    {
        self.filter(f)
    }

    /// Flatten nested iterators
    fn async_flatten(self) -> impl AsyncIterator<Item = <Self::Item as IntoIterator>::Item>
    where
        Self::Item: IntoIterator,
        <Self::Item as IntoIterator>::Item: Send + 'static,
        <Self::Item as IntoIterator>::IntoIter: Send + 'static,
    {
        self.flatten()
    }

    /// Map and flatten - applies a function that returns an iterator, then flattens the result
    fn async_flat_map<F, I>(self, f: F) -> impl AsyncIterator<Item = I::Item>
    where
        F: FnMut(Self::Item) -> I + Send + 'static,
        I: Iterator + Send + 'static,
        I::Item: Send + 'static,
    {
        self.flat_map(f)
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
        Self: Iterator<Item = Result<I, E>>,
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
        Self: Iterator<Item = Result<I, E>>,
        I: IntoIterator<Item = Result<T, E>> + 'static,
        I::IntoIter: Send + 'static,
        T: Send + 'static,
        E: Send + 'static,
    {
        // Use flatten_ok to flatten the iterator structure, producing Iterator<Item = Result<Result<T, E>, E>>
        // Then map to flatten the nested Results: Result<Result<T, E>, E> -> Result<T, E>
        self.flatten_ok().map(|result| result?)
    }

    /// Folds an iterator into a single value.
    fn async_fold<B, F>(self, init: B, f: F) -> B
    where
        F: FnMut(B, Self::Item) -> B + Send + 'static,
        B: Send + 'static,
        Self::Item: Send + 'static,
    {
        self.fold(init, f)
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
        Self: Iterator<Item = Result<T, E>>,
    {
        self.try_fold(init, move |acc, item| f(acc, item?))
    }

    /// Stateful iterator adapter that maintains state and can optionally terminate early.
    ///
    /// Similar to Iterator::scan, this maintains a state value and uses it to transform
    /// each item. The closure can return None to terminate the iterator early.
    fn async_scan<St, B, F>(self, initial_state: St, f: F) -> impl AsyncIterator<Item = B>
    where
        F: FnMut(&mut St, Self::Item) -> Option<B> + Send + 'static,
        St: Send + 'static,
        B: Send + 'static,
        Self::Item: Send + 'static,
    {
        self.scan(initial_state, f)
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
        Self: Iterator<Item = Result<T, E>>,
    {
        self.map_ok(f)
    }

    /// Filter and map over the successful values in a Result-yielding iterator
    ///
    /// This method requires the iterator to yield `Result` types.
    /// The closure receives the unwrapped `Ok` value and returns `Option<R>`:
    /// - `Some(value)` yields `Ok(value)` 
    /// - `None` skips the item
    /// - Errors are passed through unchanged
    ///
    /// This matches itertools::Itertools::filter_map_ok semantics.
    fn async_filter_map_ok<T, E, F, R>(self, f: F) -> impl AsyncIterator<Item = Result<R, E>>
    where
        F: FnMut(T) -> Option<R> + Send + 'static,
        T: Send + 'static,
        E: Send + 'static,
        R: Send + 'static,
        Self: Iterator<Item = Result<T, E>>,
    {
        self.filter_map_ok(f)
    }

    /// Take items while predicate is true
    ///
    /// Yields items from the iterator as long as the predicate returns `true`.
    /// Once the predicate returns `false`, iteration stops.
    fn async_take_while<F>(self, f: F) -> impl AsyncIterator<Item = Self::Item>
    where
        F: FnMut(&Self::Item) -> bool + Send + 'static,
        Self::Item: Send + 'static,
    {
        self.take_while(f)
    }

    /// Chain two iterators
    fn async_chain<U>(self, other: U) -> impl AsyncIterator<Item = Self::Item>
    where
        U: Iterator<Item = Self::Item> + Send + 'static,
    {
        self.chain(other)
    }

    /// Zip two iterators together
    ///
    /// Combines this iterator with another, yielding pairs of items.
    /// The resulting iterator ends when either input iterator ends.
    fn async_zip<U>(self, other: U) -> impl AsyncIterator<Item = (Self::Item, U::Item)>
    where
        U: Iterator + Send + 'static,
        U::Item: Send,
        Self::Item: Send,
    {
        self.zip(other)
    }

    /// Collect an iterator of Results into a Result of collection
    ///
    /// In sync mode, this collects immediately (no Future).
    /// Use with `await_!(iter.async_try_collect())` for mode-agnostic code.
    fn async_try_collect<T, E, C>(self) -> Result<C, E>
    where
        Self: Iterator<Item = Result<T, E>>,
        C: Extend<T> + FromIterator<T> + Default,
    {
        self.collect()
    }

    /// Collect an iterator into a collection
    ///
    /// In sync mode, this collects immediately (no Future).
    /// Use with `await_!(iter.async_collect())` for mode-agnostic code.
    fn async_collect<C>(self) -> C
    where
        C: Extend<Self::Item> + FromIterator<Self::Item> + Default,
    {
        self.collect()
    }

    /// Count the number of items in the iterator
    fn async_count(self) -> usize
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
    fn async_pin(self) -> impl AsyncIterator<Item = Self::Item> {
        self
    }

    /// Convert to boxed iterator
    fn into_boxed(self) -> BoxedAsyncIterator<Self::Item> {
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

