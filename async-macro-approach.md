# Async/Sync Unification Using Proc Macros

**Date**: October 18, 2025  
**Goal**: Eliminate sync/async code duplication using simple proc macros and unified trait

---

## Executive Summary

**Can we eliminate sync/async code duplication using proc macros?**

**Answer**: **YES** - Using simple proc macros + unified trait

### The Key Idea

Write all code in "async style" using simple macros that conditionally add/remove `async` keywords and `.await`:

```rust
#[async_fn]
fn read_file(engine: &dyn Engine) -> DeltaResult<Data> {
    let data = await_!(engine.storage_handler().read_file(path))?;
    Ok(data)
}
```

- **Sync mode** (feature off): Standard function, `await_!()` is no-op  
- **Async mode** (feature on): Async function, `await_!()` adds `.await`

### Infrastructure (~150 lines total)

| Component | Purpose |
|-----------|---------|
| `#[async_fn]` macro (~15 LOC) | Conditionally adds `async` keyword to functions |
| `await_!()` macro (~10 LOC) | Conditionally adds `.await` to expressions |
| `AsyncIterator` trait (~100 LOC) | Minimal unified trait (6 methods) for I/O boundaries only |
| Type aliases (~10 LOC) | Boxed types for API boundaries |
| AsyncIterator adapters (~30 LOC) | Convert concrete Iterator/Stream types to internal AsyncIterator abstraction |

### Three-Layer Architecture

The approach minimizes duplication by separating concerns:

1. **I/O Primitives** (bottom layer): Native async implementations
   - One async implementation method per handler (all the real I/O logic)
   - Uses `into_boxed_async_iterator` helper (Component 5) to bridge to appropriate mode
   - **No logic duplication** - each handler has single implementation

2. **Business Logic** (middle layer): Single implementation using `#[async_fn]`
   - Uses `impl AsyncIterator` for zero-cost abstraction
   - Kernel code that transforms/filters/processes data
   - **No duplication!**

3. **Public API** (top layer): Thin wrapper using `#[async_fn]` + `await_!`
   - Calls I/O primitive and uses helper to convert appropriately
   - Single unified trait implementation per handler method
   - **No duplication!**

**Key insight**: Zero logic duplication anywhere. All five infrastructure components work together to enable single-source implementations at every layer. Only ~10-12 call sites need `AsyncIterator` - the rest of the codebase (200+ iterator chains) continues using regular `Iterator`/`Stream` for pure business logic.

### Benefits

✅ **Zero duplication** of business logic (only I/O primitives differ)  
✅ **Zero overhead** - sync mode compiles to standard iterators, no boxing internally  
✅ **Single source** - fix bugs once, add features once, refactor once  
✅ **IDE support** - go-to-definition, autocomplete, refactoring all work  
✅ **Low risk** - can prototype incrementally, fall back if needed

**Note**: The `Engine` trait itself doesn't need modifications - it just returns `Arc<dyn Handler>`. Only the handler traits (like `ParquetHandler`) use `#[async_fn]` on their I/O methods. No separate `AsyncEngine` trait needed.

### Why This Matters

Delta-kernel-rs needs to support both sync and async modes. Without this approach, async orchestration code must be duplicated - even when all I/O-free business logic is factored out into shared functions. Worse, some scenarios are **impossible to unify** without language-level support.

### Recommendation

**✅ PROCEED WITH PROTOTYPE**

1. Simple, straightforward infrastructure
2. Eliminates duplication without compromising performance
3. Low risk - prototype in Week 1-2, evaluate before full rollout

---

## Current State: The Hidden Async Architecture

### DefaultEngine Is Already Async

**Key Discovery**: DefaultEngine is **already doing async I/O internally**, just wrapped in blocking calls.

```rust
// Today's architecture
pub struct DefaultEngine<E: TaskExecutor> {
    executor: Arc<E>,  // TokioBackgroundExecutor
}

impl JsonHandler for DefaultJsonHandler {
    fn read_json_files(&self, ...) -> DeltaResult<Box<dyn EngineData>> {
        // Async I/O happens here, but we block on it!
        self.executor.block_on(async {
            let store_get = object_store.get(path).await?;
            // ... async parquet reading ...
        })
    }
}
```

The kernel **already uses async I/O under the hood** and exposes a sync API by blocking on futures.

**What this means**: This macro approach doesn't fundamentally change the architecture - it just makes the async nature **optional and exposed** instead of **hidden and forced**. We're not adding async to the kernel; we're giving consumers a choice to use the async that's already there.

---

## The Approach

### The Challenge

Even with all I/O-free business logic factored into shared functions, we face three problems:

**Problem 1: `async` and `.await` syntax**

```rust
// Sync version
fn read_metadata(engine: &dyn Engine) -> DeltaResult<Metadata> {
    let data = engine.json_handler().read_json_files(&[path])?;
    parse_metadata(data)
}

// Async version  
async fn read_metadata(engine: &dyn Engine) -> DeltaResult<Metadata> {
    let data = engine.json_handler().read_json_files(&[path]).await?;
    parse_metadata(data)
}
```

Nearly identical, but two syntax differences:
- `async fn` vs `fn` - async functions implicitly return `Future`s
- `.await` is required in async mode and forbidden in sync mode

**Problem 2: Iterator vs Stream methods**

```rust
// Sync version
fn process_files(files: Vec<File>) -> impl Iterator<Item = Result> {
    files.into_iter()
        .filter(|f| f.is_valid())      // Iterator::filter takes a normal closure
        .map(|f| process(f))           // Iterator::map takes a normal closure
}

// Async version
fn process_files(files: Vec<File>) -> impl Stream<Item = Result> {
    stream::iter(files)
        .filter(|f| async move { f.is_valid() })  // Stream::filter takes an async closure
        .then(|f| async move { process(f) })      // Stream calls it .then(), not .map()
}
```

Different types (`Iterator` vs `Stream`) with incompatible methods:
- `.map()` vs `.then()`
- `.filter()` takes sync closures vs async closures
- Can't write generic code that works with both

**Problem 3: Returning iterators/streams** ⚠️

```rust
// This doesn't work - can't abstract over Iterator/Stream in return type
fn get_scan_files(engine: &dyn Engine) -> ??? {
    let files = discover_files(engine);  // I/O returns iterator/stream
    files.map(|f| {
        let data = read_file(engine, f);  // More I/O
        transform(data)  // Business logic
    })
}
```

**Why you can't just write this code once**:
- Return type must be either `impl Iterator` or `impl Stream` - no way to abstract
- Can't use generics: `Iterator` and `Stream` are unrelated traits
- Can't use a trait object: need concrete types for `impl Trait` returns
- Business logic is interleaved with iteration, so can't factor it out

Without our approach, you must duplicate this entire function for sync and async modes.

This is common in delta-kernel-rs: functions that create and transform iterators/streams.

### The Solution

The `AsyncIterator` trait provides a unified interface for the iterator operations that kernel relies on:

```rust
pub trait AsyncIterator: Sized {
    type Item;
    fn async_map<F, R>(self, f: F) -> impl AsyncIterator<Item = R>;
    fn async_filter<F>(self, f: F) -> impl AsyncIterator<Item = Self::Item>;
    fn async_try_fold<B, E, F>(self, init: B, f: F) -> Result<B, E>;
    // ... other methods
}
```

**In sync mode** (when `async` feature is disabled), `Iterator` implements it:
```rust
impl<I: Iterator> AsyncIterator for I {
    fn async_map<F>(self, f: F) -> impl AsyncIterator<Item = R> {
        self.map(f)  // Delegates to standard Iterator::map
    }
}
```

**In async mode** (when `async` feature is enabled), `Stream` implements it:
```rust
impl<S: Stream> AsyncIterator for S {
    fn async_map<F>(self, f: F) -> impl AsyncIterator<Item = R> {
        self.then(|x| async move { f(x) })  // Delegates to Stream::then
    }
}
```

**Key point**: Only one implementation exists in any given build. The `#[cfg]` guards in separate module files ensure no conflicts.

**How this solves the three problems**:

1. **Problem 1 (`async` and `.await` syntax)**: Solved by `#[async_fn]` macro (adds `async` keyword) + `await_!()` macro (adds `.await`)
2. **Problem 2 (Iterator vs Stream methods)**: Solved by `AsyncIterator` trait providing unified methods (`.async_map()`, `.async_filter()`, etc.) that delegate to the appropriate Iterator or Stream methods
3. **Problem 3 (Returning iterators)**: Solved! Functions can return `impl AsyncIterator` and work in both modes

### Putting It Together

**Your code** (single source):
```rust
#[async_fn]
fn process_data(engine: &dyn Engine) -> DeltaResult<Vec<i32>> {
    let items = await_!(engine.get_items())?;
    let result = items
        .async_filter(|x| x > 0)
        .async_map(|x| x * 2);
    Ok(result)
}
```

**What the code becomes in sync mode**:
```rust
// #[async_fn] is a no-op
fn process_data(engine: &dyn Engine) -> DeltaResult<Vec<i32>> {
    let items = engine.get_items()?;  // await_!() is no-op
    let result = items
        .filter(|x| x > 0)   // AsyncIterator::async_filter → Iterator::filter
        .map(|x| x * 2);     // AsyncIterator::async_map → Iterator::map
    Ok(result)
}
```

**What the code becomes in async mode**:
```rust
// #[async_fn] injects the `async` keyword
async fn process_data(engine: &dyn Engine) -> DeltaResult<Vec<i32>> {
    let items = engine.get_items().await?;  // await_!() adds .await
    let result = items
        .filter(|x| async move { x > 0 })   // AsyncIterator::async_filter → Stream::filter
        .then(|x| async move { x * 2 });    // AsyncIterator::async_map → Stream::then
    Ok(result)
}
```

**How it works**:
- `#[async_fn]`: Conditionally adds `async` keyword
- `await_!()`: Conditionally adds `.await`
- `AsyncIterator` trait: Provides unified `.async_map()`, `.async_filter()` methods
- That's it! No type rewriting needed because `AsyncIterator` is a real trait that either `Iterator` or `Stream` implements, depending on the mode.

---

## Infrastructure Components

The async macro approach requires five simple components working together:

1. **`#[async_fn]` macro** - Conditionally adds `async` keyword
2. **`await_!()` macro** - Conditionally adds `.await`
3. **Type aliases** - Boxed types for API boundaries
4. **`AsyncIterator` trait** - Unified abstraction over Iterator/Stream
5. **AsyncIterator adapters** - Convert concrete Iterator/Stream types to internal abstraction

Components 1-4 enable unified business logic in kernel itself. Component 5 enables unified engine trait definitions as well (i.e. in the default engine).

---

### Component 1: The `#[async_fn]` Proc Macro

**Purpose**: Conditionally adds `async` keyword to function signatures

**Implementation** (~15 lines):
```rust
// In derive-macros/src/lib.rs

#[proc_macro_attribute]
pub fn async_fn(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(item as ItemFn);
    
    #[cfg(feature = "async")]
    let asyncness = Some(syn::token::Async::default());
    
    #[cfg(not(feature = "async"))]
    let asyncness = None;
    
    input.sig.asyncness = asyncness;
    
    quote! { #input }.into()
}
```

**Key points**:
- Only adds/removes `async` keyword
- No complex type rewriting
- Works on free functions, methods, and trait methods

---

### Component 2: The `await_!()` Macro

**Purpose**: Conditionally adds `.await` to expressions

**Implementation** (~10 lines):
```rust
// In kernel/src/macros.rs

#[cfg(not(feature = "async"))]
macro_rules! await_ {
    ($e:expr) => { $e };
}

#[cfg(feature = "async")]
macro_rules! await_ {
    ($e:expr) => { $e.await };
}
```

**Usage**:
```rust
let data = await_!(engine.read_file(path))?;
```

**Note**: Named `await_!()` instead of `await!()` to avoid keyword conflict.

---

### Component 3: Type Aliases for API Boundaries

**Purpose**: Provide concrete types for public API returns, exposing real `Iterator`/`Stream` types to consumers

**Implementation** (~10 lines):
```rust
// In kernel/src/lib.rs

// Generic boxed type for API boundaries (AsyncIterator trait is not dyn-compatible)
#[cfg(not(feature = "async"))]
pub type BoxedAsyncIterator<T> = Box<dyn Iterator<Item = T> + Send>;

#[cfg(feature = "async")]
pub type BoxedAsyncIterator<T> = Pin<Box<dyn Stream<Item = T> + Send>>;

// Specialized types build on BoxedAsyncIterator
pub type ScanFilesIterator = BoxedAsyncIterator<DeltaResult<ScanFile>>;
pub type FileDataReadResultIterator = BoxedAsyncIterator<DeltaResult<Box<dyn EngineData>>>;
```

**Key points**:
- **`BoxedAsyncIterator` exposes real types**: `Box<dyn Iterator>` in sync mode, `Pin<Box<dyn Stream>>` in async mode
- **`AsyncIterator` is internal only**: Used inside kernel for unified code, never exposed in public API
- **Consumers get full Iterator/Stream APIs**: No limitations from internal trait design choices
- Internal code uses `impl AsyncIterator` (unboxed, zero-cost)
- `into_boxed()` method converts from internal `impl AsyncIterator` to public boxed types

---

### Component 4: The `AsyncIterator` Trait

**Purpose**: Internal unified trait for iterator operations that works in both modes

**Scope**: Only used at engine I/O boundaries (~10-12 call sites). Business logic continues to use regular `Iterator`/`Stream`.

**Implementation**:

```rust
// In kernel/src/async_iterator/mod.rs

/// Unified trait for async-style iterator operations at engine I/O boundaries
/// 
/// Implemented by Iterator (sync mode) and Stream (async mode).
/// Provides a consistent API for working with sequences in both modes.
/// 
/// **Design Philosophy**: This trait intentionally provides a minimal set of methods
/// needed at engine I/O boundaries. Most iterator operations in the kernel happen
/// on regular Iterator/Stream types in pure business logic and don't need this trait.
/// 
/// See Appendix: Method Selection Rationale for details on which methods were included.
#[internal_api]
pub trait AsyncIterator: Sized {
    type Item;
    
    /// Map each item - the most common operation at I/O boundaries
    fn async_map<F, R>(self, f: F) -> impl AsyncIterator<Item = R>
    where
        F: FnMut(Self::Item) -> R + Send + 'static,
        R: Send + 'static;
    
    /// Filter items - used for data skipping and selection
    fn async_filter<F>(self, f: F) -> impl AsyncIterator<Item = Self::Item>
    where
        F: FnMut(&Self::Item) -> bool + Send + 'static;
    
    /// Flatten nested iterators/streams - needed for nested file operations
    fn async_flatten(self) -> impl AsyncIterator<Item = <Self::Item as AsyncIterator>::Item>
    where
        Self::Item: AsyncIterator + Send + 'static;
    
    /// Try fold with early exit on error - for stateful reducers (P&M, domain metadata)
    /// 
    /// Note: Will become more important with two-phase log replay where protocol,
    /// metadata, app IDs, and domain metadata all become fold-based reducers.
    #[async_fn]
    fn async_try_fold<B, E, F>(self, init: B, f: F) -> Result<B, E>
    where
        F: FnMut(B, Self::Item) -> Result<B, E> + Send + 'static,
        B: Send + 'static,
        E: Send + 'static;
    
    /// Chain two iterators/streams - for composing multiple I/O sources
    fn async_chain<U>(self, other: U) -> impl AsyncIterator<Item = Self::Item>
    where
        U: AsyncIterator<Item = Self::Item> + Send + 'static;
    
    /// Converts this iterator/stream into `BoxedAsyncIterator` for API boundaries
    /// 
    /// This method serves two purposes:
    /// 1. Recovers the concrete type (Iterator or Stream) from the generic `impl AsyncIterator`
    /// 2. Boxes it appropriately (`Box::new` for Iterator, `Box::pin` for Stream)
    fn into_boxed(self) -> BoxedAsyncIterator<Self::Item>;
}

// Note: Helper functions like into_async_iter() are documented in Component 5

// Re-export the appropriate implementation based on feature flag
#[cfg(not(feature = "async"))]
mod sync_impl;
#[cfg(not(feature = "async"))]
pub use sync_impl::*;

#[cfg(feature = "async")]
mod async_impl;
#[cfg(feature = "async")]
pub use async_impl::*;
```

**Separate implementation files**:

```rust
// ============================================================================
// kernel/src/async_iterator/sync_impl.rs
// ============================================================================

use super::*;

impl<I: Iterator + Send + 'static> AsyncIterator for I {
    type Item = I::Item;
    
    fn async_map<F, R>(self, mut f: F) -> impl AsyncIterator<Item = R>
    where
        F: FnMut(Self::Item) -> R + Send + 'static,
        R: Send + 'static,
    {
        self.map(move |item| f(item))
    }
    
    fn async_filter<F>(self, mut f: F) -> impl AsyncIterator<Item = Self::Item>
    where
        F: FnMut(&Self::Item) -> bool + Send + 'static,
    {
        self.filter(move |item| f(item))
    }
    
    fn async_flatten<U>(self) -> impl AsyncIterator<Item = U>
    where
        Self::Item: IntoIterator<Item = U>,
        U: Send + 'static,
    {
        self.flatten()
    }
    
    fn async_try_fold<B, E, F>(self, init: B, mut f: F) -> Result<B, E>
    where
        F: FnMut(B, Self::Item) -> Result<B, E> + Send + 'static,
        B: Send + 'static,
        E: Send + 'static,
    {
        // Sync: direct call to try_fold
        self.try_fold(init, |acc, item| f(acc, item))
    }
    
    fn async_chain<U>(self, other: U) -> impl AsyncIterator<Item = Self::Item>
    where
        U: IntoIterator<Item = Self::Item>,
    {
        self.chain(other)
    }
    
    fn into_boxed(self) -> BoxedAsyncIterator<Self::Item> {
        Box::new(self)
    }
}
```

```rust
// ============================================================================
// kernel/src/async_iterator/async_impl.rs
// ============================================================================

use super::*;
use futures::StreamExt as _;
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};

/// Yields control back to the executor to prevent starvation
async fn yield_now() {
    YieldNow { yielded: false }.await
}

struct YieldNow {
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

impl<S: Stream + Send + 'static> AsyncIterator for S {
    type Item = S::Item;
    
    fn async_map<F, R>(self, mut f: F) -> impl AsyncIterator<Item = R>
    where
        F: FnMut(Self::Item) -> R + Send + 'static,
        R: Send + 'static,
    {
        self.then(move |item| async move { f(item) })
    }
    
    fn async_filter<F>(self, mut f: F) -> impl AsyncIterator<Item = Self::Item>
    where
        F: FnMut(&Self::Item) -> bool + Send + 'static,
    {
        self.filter(move |item| async move { f(item) })
    }
    
    fn async_flatten<U>(self) -> impl AsyncIterator<Item = U>
    where
        Self::Item: Stream<Item = U> + Send,
        U: Send + 'static,
    {
        self.then(|stream| async move {
            yield_now().await;  // Cooperative yielding
            stream
        }).flatten()
    }
    
    async fn async_try_fold<B, E, F>(self, init: B, mut f: F) -> Result<B, E>
    where
        F: FnMut(B, Self::Item) -> Result<B, E> + Send + 'static,
        B: Send + 'static,
        E: Send + 'static,
    {
        // Async: StreamExt::try_fold returns a future, await it
        self.try_fold(init, |acc, item| async move { f(acc, item) }).await
    }
    
    fn async_chain<U>(self, other: U) -> impl AsyncIterator<Item = Self::Item>
    where
        U: Stream<Item = Self::Item>,
    {
        self.chain(other)
    }
    
    fn into_boxed(self) -> BoxedAsyncIterator<Self::Item> {
        Box::pin(self)
    }
}
```

**Module structure**:
- `kernel/src/async_iterator/mod.rs` - trait definition + helper functions (see Component 5) + conditional module imports
- `kernel/src/async_iterator/sync_impl.rs` - sync implementation (delegates to `Iterator`)
- `kernel/src/async_iterator/async_impl.rs` - async implementation (delegates to `Stream`)

**Key points**:
- Marked `#[internal_api]` - never exposed to consumers
- Implemented by either `Iterator` (sync) or `Stream` (async), depending on the selected mode.
- Trait methods return `impl AsyncIterator` (which compiles to `impl Iterator` or `impl Stream`)
- `BoxedAsyncIterator<T>` type alias exposes real boxed `Iterator`/`Stream` at API boundaries
- Most internal code uses `impl AsyncIterator` and never needs boxing
- `into_boxed()` method converts from internal trait to public API types
- Modular structure: main trait in `mod.rs`, implementations in separate `sync_impl` and `async_impl` modules

---

### Component 5: AsyncIterator Adapters

**Purpose**: Convert concrete Iterator/Stream types to internal AsyncIterator abstraction

Two adapter functions bridge from different sources:

#### 5a. `into_boxed_async_iterator` - For Streams (Critical Path)

Converts a **Stream** (from async I/O) to work uniformly in both modes. This is the critical path for engine I/O:

```rust
// Sync mode: blocks on future, converts Stream to Iterator
#[cfg(not(feature = "async"))]
pub(crate) fn into_boxed_async_iterator<E, Fut, S, T>(
    executor: &E,
    stream_future: Fut,
) -> DeltaResult<BoxedAsyncIterator<T>>
where
    E: TaskExecutor,
    Fut: Future<Output = DeltaResult<S>>,
    S: Stream<Item = T> + Send + 'static,
    T: Send + 'static,
{
    let mut stream = Box::pin(executor.block_on(stream_future)?);
    let executor = executor.clone();
    let iter = std::iter::from_fn(move || {
        executor.block_on(async { stream.next().await })
    });
    Ok(iter.into_boxed())
}

// Async mode: awaits future, boxes Stream
#[cfg(feature = "async")]
pub(crate) async fn into_boxed_async_iterator<E, Fut, S, T>(
    _executor: &E,
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
```

**Why is this needed?**

The engine layer produces native async I/O that returns `Stream`s. In sync mode, we need `Iterator`s. This conversion requires:
1. Blocking on the future that produces the Stream
2. Wrapping the Stream to block on each item as it's pulled

This can't be unified at the Rust language level - sync and async iteration are fundamentally different.

**Usage in handler implementations**:

```rust
impl ParquetHandler for DefaultParquetHandler {
    #[async_fn]
    fn read_parquet_files(&self, files: &[FileMeta]) -> DeltaResult<FileDataReadResultIterator> {
        // Calls async implementation, uses helper to convert appropriately
        await_!(into_boxed_async_iterator(&self.executor, self.read_parquet_impl(files)))
    }
}
```

**Impact**: Enables zero duplication in handler wrappers. Each handler method gets a single unified trait implementation using the new sync/async infrastructure.

---

#### 5b. `into_async_iter` - For IntoIterator Types

Converts types that produce **Iterators** (like `Vec`, `Option`) to work uniformly in both modes:

```rust
pub fn into_async_iter<I: IntoIterator>(i: I) -> impl AsyncIterator<Item = I::Item> {
    #[cfg(not(feature = "async"))]
    { i.into_iter() }
    
    #[cfg(feature = "async")]
    { futures::stream::iter(i) }
}
```

**Usage**: Allows collections to work with AsyncIterator methods in kernel code:

```rust
// Works in both modes
option.into_async_iter().async_flatten()
vec.into_async_iter().async_map(|x| x + 1)
```

**Note**: Both adapters serve the same conceptual purpose (converting to AsyncIterator), but `into_boxed_async_iterator` is on the critical path (engine I/O) while this handles less-critical cases (collections).

---

## Complete Examples

### Example 0: Engine Trait Implementation (Three-Layer Approach)

This example shows all three layers working together:

```rust
use crate::async_iterator::AsyncIterator;

// Type alias for the item type to reduce verbosity
type FileDataResult = DeltaResult<Box<dyn EngineData>>;

// ============= LAYER 1: I/O PRIMITIVES =============
// One native async implementation - contains all the real I/O logic

impl DefaultParquetHandler {
    async fn read_parquet_impl(&self, files: &[FileMeta]) -> DeltaResult<impl Stream<Item = FileDataResult>> {
        // ALL the I/O logic here - single source!
        // Uses async I/O (object_store, parquet reader, etc.)
        // Returns a Stream of results
    }
}

// ============= LAYER 2: BUSINESS LOGIC =============
// Single implementation using #[async_fn] - returns impl AsyncIterator (no boxing!)
// This is kernel code that uses the engine, doesn't care about sync/async details

impl Snapshot {
    #[async_fn]
    fn get_scan_files(&self, engine: &dyn Engine) -> DeltaResult<impl AsyncIterator<Item = ScanFile>> {
        // Business logic that uses the handler - single source for both modes!
        let handler = engine.get_parquet_handler();
        let files = self.get_file_metadata();
        
        // Read parquet data using the handler
        let data = await_!(handler.read_parquet_files(files))?;
        
        // Transform and filter - AsyncIterator trait makes this mode-agnostic
        Ok(data
            .async_filter(|batch| batch.num_rows() > 0)
            .async_map(|batch| ScanFile::from_batch(batch)))
    }
}

// ============= LAYER 3: PUBLIC API =============
// Trait definition uses BoxedAsyncIterator for API boundaries (trait object)

pub trait ParquetHandler {
    #[async_fn]
    fn read_parquet_files(&self, files: &[FileMeta]) -> DeltaResult<FileDataReadResultIterator>;
}

// Trait implementation is a thin wrapper - single source, no duplication!
impl ParquetHandler for DefaultParquetHandler {
    #[async_fn]
    fn read_parquet_files(&self, files: &[FileMeta]) -> DeltaResult<FileDataReadResultIterator> {
        // Uses into_boxed_async_iterator helper (Component 5)
        // - Sync mode: blocks on future, converts Stream→Iterator
        // - Async mode: awaits future, boxes Stream
        await_!(into_boxed_async_iterator(&self.executor, self.read_parquet_impl(files)))
    }
}
```

**Key insights**:
- **I/O layer**: One native async implementation containing all logic (Layer 1)
- **Handler wrapper**: Uses `into_boxed_async_iterator` helper (Component 5) to bridge to both modes
- **Business logic**: Single implementation using `#[async_fn]` and `impl AsyncIterator` (Layer 2)
- **Public API**: Thin wrapper using `#[async_fn]` + `await_!` + helper (Layer 3)
- **No boxing overhead**: `impl AsyncIterator` is unboxed internally; only box at API boundaries

**Result**: **Zero logic duplication**. Each handler method gets a single unified wrapper using all five infrastructure components.

---

### Example 1: One-Shot I/O

**Scenario**: Helper function that performs I/O once and returns parsed data.

**Single source code**:
```rust
#[async_fn]
fn read_metadata(engine: &dyn Engine, path: &Path) -> DeltaResult<Metadata> {
    let bytes = await_!(engine.json_handler().read_json_files(&[path]))?;
    parse_metadata(bytes)  // Shared I/O-free logic
}
```

**Duplication**: **ZERO**

---

### Example 2: Stateful Processing

**Scenario**: Stateful processor that accumulates results iteratively.

**Single source code**:
```rust
use crate::async_iterator::AsyncIterator;

#[async_fn]
fn process_log(engine: &dyn Engine) -> DeltaResult<Output> {
    let actions = await_!(self.read_actions(engine))?;
    
    let processor = await_!(
        actions.async_try_fold(Processor::new(), |mut proc, batch| {
            proc.process_batch(batch)  // Shared business logic
        })
    )?;
    
    processor.finalize()
}
```

**Duplication**: **ZERO**

---

### Example 3: Two-Phase Processing

**Scenario**: Discover items, then fetch them.

**Single source code**:
```rust
use crate::async_iterator::AsyncIterator;

#[async_fn]
fn execute(engine: &dyn Engine) -> DeltaResult<impl AsyncIterator<Item = Data>> {
    // Phase 1: Discover what to fetch
    let items = await_!(self.discover_items(engine))?;
    
    // Phase 2: Fetch in parallel (iterator combinator style)
    Ok(items.async_map(|item| {
        let data = await_!(engine.fetch(item))?;
        Ok(data)
    }))
}
```

**Duplication**: **ZERO**

---

### Example 4: Nested Iteration with I/O

**Scenario**: Outer loop performs I/O, inner loop processes results (the "impossible" case from earlier).

**Single source code**:
```rust
use crate::async_iterator::AsyncIterator;

#[async_fn]
fn execute(engine: &dyn Engine) -> DeltaResult<impl AsyncIterator<Item = ScanResult>> {
    let files = await_!(self.scan_metadata(engine))?;
    
    Ok(files
        .async_map(|file| {
            let data = await_!(engine.read_parquet(file))?;
            Ok(data.async_map(|batch| transform(batch)))  // Nested!
        })
        .async_flatten())  // Works in both modes!
}
```

**Duplication**: **ZERO**

---

### Example 5: Two-Pass Processing

**Scenario**: Aggregate metadata about items, then reprocess with that context.

**Single source code**:
```rust
use crate::async_iterator::AsyncIterator;

#[async_fn]
fn execute(engine: &dyn Engine) -> DeltaResult<Vec<Output>> {
    let items = await_!(self.get_items(engine))?;
    
    // Pass 1: Aggregate metadata
    let metadata = await_!(items.async_try_fold(Metadata::new(), |mut meta, item| {
        meta.record(item);
        Ok(meta)
    }))?;
    
    // Pass 2: Reprocess with metadata
    let items = await_!(self.get_items(engine))?;  // Fetch again
    let results = items
        .async_map(|item| process_with_metadata(item, &metadata))
        .collect();
    
    Ok(results)
}
```

**Duplication**: **ZERO**

---

## Implementation Plan

### Phase 1: Infrastructure (Week 1)

**Day 1: Proc macro**
- Create `derive-macros` crate
- Implement `#[async_fn]` macro (~15 lines)
- Test with simple functions

**Day 2-3: AsyncIterator trait**
- Create `kernel/src/async_iterator/` module
- Define `AsyncIterator` trait
- Implement for `Iterator` (sync_impl.rs)
- Implement for `Stream` (async_impl.rs)
- Test all methods in both modes

**Day 4: Type aliases + await macro + I/O helper**
- Add `BoxedAsyncIterator<T>` type alias
- Add `await_!()` macro
- Add `into_boxed_async_iterator` helper (Component 5)
- Test with engine trait examples

**Day 5: Test infrastructure**
- CI configuration for both modes
- Basic integration tests
- Verify zero overhead in sync mode

**Deliverable**: Infrastructure compiles and passes tests in both modes

---

### Phase 2: Prototype (Week 2)

**Goal**: Prove the approach works with real kernel code

**Day 1-2: Convert Snapshot Building**
- Convert `Snapshot::build_snapshot` (Pattern C)
- Keep existing code as backup
- Test in both modes

**Day 3: Validate**
- Performance benchmarks (ensure zero overhead)
- IDE support testing (go-to-definition, autocomplete)
- Code complexity comparison

**Day 4: Review**
- Team review of prototype
- Identify any issues or edge cases
- Gather feedback

**Day 5: Decision**
- **Go/no-go decision on full rollout**
- If issues found, iterate or reconsider approach

**Deliverable**: Working prototype with metrics and team buy-in

---

### Phase 3: Rollout (Weeks 3-4, if Phase 2 succeeds)

**Week 3: Convert remaining patterns**
- Pattern A: Helper functions (LogReplay, etc.)
- Pattern B: Processor + try_fold
- Pattern D: Nested iteration
- Pattern E: Two-pass processing

**Week 4: Polish and integration**
- Update all engine implementations
- Documentation updates
- Final integration tests
- Performance validation

**Deliverable**: All kernel entry points unified

---

### Phase 4: Cleanup (Week 5)

- Remove old duplicated code
- Update CONTRIBUTING.md with macro usage guidelines
- Knowledge transfer to team
- Monitor for issues

**Total timeline**: 4-5 weeks for full implementation

---

## Conclusion

This approach provides a clean, performant solution to eliminate sync/async code duplication in delta-kernel-rs. The infrastructure is simple and straightforward, the performance overhead is zero, and the benefits (single source, easier maintenance) are substantial.

The three-layer architecture clearly separates concerns:
- **Layer 1 (I/O)**: One async implementation per handler, using Component 5 to bridge modes
- **Layer 2 (Business Logic)**: Zero duplication, single source using `#[async_fn]`
- **Layer 3 (Public API)**: Zero duplication, thin unified wrapper per handler method

**Result**: **Zero logic duplication** anywhere in the codebase. All five infrastructure components enable single-source implementations everywhere.

**Recommendation**: Proceed with prototype in Phase 1-2, then evaluate for full rollout.
---

## Appendix

### AsyncIterator Method Selection

**Question**: Why does `AsyncIterator` only have 6 methods when there are 100+ iterator usage sites in the codebase?

**Answer**: Because only ~10-12 call sites actually cross the sync/async boundary. The rest are pure business logic.

#### The Two Iterator Patterns

**Pattern 1: Engine I/O Boundaries** (needs AsyncIterator):
```rust
// Public API that wraps engine calls
pub fn execute(&self, engine: Arc<dyn Engine>) 
    -> DeltaResult<impl Iterator<Item = DeltaResult<ScanResult>>> 
{
    // THIS crosses the sync/async boundary
    let read_iter = engine.parquet_handler().read_parquet_files(...)?;
    
    // Wrap with transform logic (needs AsyncIterator methods)
    Ok(read_iter.async_map(|data| transform(data)))
}
```

**Call sites**: 
- 3 public APIs (`Scan::execute`, `Scan::scan_metadata`, `TableChangesScan::execute`)
- 4 engine trait methods (`read_json_files`, `read_parquet_files`, `list_from`, `read_files`)
- 3-5 internal wrappers

**Total**: ~10-12 sites

---

**Pattern 2: Pure Business Logic** (uses regular Iterator/Stream):
```rust
// Transform already-loaded data - no engine calls
fn scan_metadata_to_scan_file(
    metadata: impl Iterator<Item = DeltaResult<TableChangesScanMetadata>>,
) -> impl Iterator<Item = DeltaResult<CdfScanFile>> {
    metadata.map(|m| transform(m?))  // Regular Iterator, no I/O
}
```

**Call sites**: 200+ throughout the codebase

**Key insight**: These already work with regular `Iterator`/`Stream` - no need for `AsyncIterator`!

---

#### Method Selection Criteria

We analyzed all iterator method usage and categorized by whether it's used at I/O boundaries:

| Method | I/O Boundary Uses | Business Logic Uses | Included? |
|--------|-------------------|---------------------|-----------|
| `map` | 8+ | 100+ | ✅ Yes - core |
| `filter` | 2-3 | 30+ | ✅ Yes - data skipping |
| `flatten` | 3-4 | 10+ | ✅ Yes - nested files |
| `try_fold` | 0 (future) | 5+ | ✅ Yes - reducers* |
| `chain` | 2-3 | 15+ | ✅ Yes - composing I/O |
| `into_boxed` | 10-12 | 0 | ✅ Yes - API conversion |
| **`try_collect`** | **0** | **55** | ❌ No - use itertools |
| **`map_ok`** | **0** | **50+** | ❌ No - use itertools |
| **`flatten_ok`** | **0** | **9** | ❌ No - use itertools |
| **`enumerate`** | **0** | **30+** | ❌ No - pure logic |
| **`zip`** | **0** | **30+** | ❌ No - pure logic |

\* `try_fold` is included because it will be critical for two-phase log replay where protocol, metadata, app IDs, and domain metadata extraction all become fold-based reducers over action streams.

---

#### Why This Matters

**Before analysis**: Thought we needed 15+ methods to handle 200+ iterator usage sites.

**After analysis**: Only need 6 methods for the 10-12 I/O boundary sites.

**Impact**:
- ✅ Smaller API surface
- ✅ Less implementation complexity
- ✅ Most code continues using familiar Iterator/Stream APIs
- ✅ Can add more methods later if needed

---

#### Future Extensions

Methods we may add later:

- `async_filter_map` - if combined filter+map becomes common at I/O boundaries
- `async_inspect` - for debugging I/O operations
- `async_take` / `async_skip` - for pagination/limits
- `async_buffered` - for parallel I/O (Stream-specific)

But for MVP, the 6 core methods handle all current use cases.

**Detailed Analysis**: See [ASYNC-ITERATOR-ANALYSIS.md](ASYNC-ITERATOR-ANALYSIS.md) for complete call chain tracing and categorization by module (core kernel vs. engine vs. tests).


---

### Why futures (Not tokio) Dependency?

The `AsyncIterator` trait uses `futures::stream::Stream`, not tokio-specific types. This is the correct choice because:

**Why it works**:
1. ✅ Kernel already depends on `futures` (in default-engine-base feature)
2. ✅ `tokio` re-exports `futures::stream::Stream` - they're the same trait
3. ✅ Current DefaultEngine already uses `futures::Stream` with tokio runtime successfully
4. ✅ Keeps kernel runtime-agnostic (could work with async-std, smol, etc.)

**The only friction**: StreamExt trait conflict

Both `futures::StreamExt` and `tokio_stream::StreamExt` provide similar combinator methods.

**Solution**: Use `futures::StreamExt` consistently throughout kernel code:

```rust
use futures::StreamExt;  // Always use this

impl<S: Stream> AsyncIterator for S {
    fn async_map<F, R>(self, f: F) -> impl AsyncIterator<Item = R> {
        self.then(|x| async move { f(x) })  // futures::StreamExt::then
    }
}
```

This is already the pattern in DefaultEngine today (see `kernel/src/engine/default/*.rs`).

**Architecture**:
- **futures** provides traits and combinators (Stream, StreamExt, Future, etc.)
- **tokio** provides the runtime (thread pools, scheduling, I/O drivers)
- They work together seamlessly - this is the standard pattern in the Rust async ecosystem

**Bottom line**: Kernel depends on `futures` for traits/combinators, DefaultEngine depends on `tokio` for runtime. This separation keeps the kernel runtime-agnostic while allowing engine implementations to use any async runtime they prefer.

---

### When NOT to Use This Approach

**Don't use `#[async_fn]` for**:
1. Pure computation (no I/O) - just write regular sync code
2. Functions that never await anything - use `#[cfg]` if truly needed
3. External crate APIs - can't modify them

**Don't use `AsyncIterator` trait for**:
1. Borrowed iterators (e.g., `&[T]`) - the `+ 'static` bounds don't apply
2. Pure computation with no I/O - regular iterators are fine
3. One-off iterator operations - overhead of trait isn't worth it

---

### Trade-offs

**Pros**:
- ✅ Zero duplication of business logic
- ✅ Zero performance overhead
- ✅ Single source to maintain
- ✅ Good IDE support

**Cons**:
- ⚠️ Learning curve (~30 min for understanding the five components)
- ⚠️ Need to test both modes in CI (2x test time)
- ⚠️ `+ 'static` bounds may be restrictive in some cases (but this is already true for async code)

---

### Testing Strategy

**Unit tests**: Regular Rust tests work in both modes
```rust
#[test]
fn test_process_data() {
    // Test code works in both sync and async modes
    // In async mode, test runtime automatically handles awaiting
}
```

**CI configuration**:
```yaml
test-sync:
  run: cargo test

test-async:
  run: cargo test --features async
```

**Integration tests**: Same approach - single source, tested in both modes

---

### Migration Strategy

**Incremental approach**:
1. Build infrastructure (Week 1)
2. Prototype one entry point (Week 2)
3. If successful, convert remaining entry points (Weeks 3-4)
4. Keep old code until new code is validated

**Rollback plan**:
- Infrastructure is isolated in its own module
- Can remove `#[async_fn]` and `await_!()` and go back to manual duplication
- Low risk, easy to revert


