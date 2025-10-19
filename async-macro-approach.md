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

### Infrastructure (~225 lines total)

| Component | Purpose |
|-----------|---------|
| `#[async_fn]` macro (~15 LOC) | Conditionally adds `async` keyword to functions |
| `await_!()` macro (~10 LOC) | Conditionally adds `.await` to expressions |
| `AsyncIterator` trait (~200 LOC) | Unified trait implemented by `Iterator` (sync) or `Stream` (async) |
| Type aliases (~10 LOC) | Boxed types for API boundaries |

### Three-Layer Architecture

The approach minimizes duplication by separating concerns:

1. **I/O Primitives** (bottom layer): Two `#[cfg]` methods for actual I/O
   - Sync I/O and async I/O are fundamentally different operations
   - This is the only layer with duplication

2. **Business Logic** (middle layer): Single implementation using `#[async_fn]`
   - Uses `impl AsyncIterator` for zero-cost abstraction
   - Kernel code that transforms/filters/processes data
   - **No duplication!**

3. **Public API** (top layer): Thin wrapper using `.into_boxed()`
   - Converts `impl AsyncIterator` to `BoxedAsyncIterator` for trait objects
   - **No duplication!**

**Key insight**: Duplication only exists at the I/O boundary. All higher layers are unified.

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

1. Simple infrastructure (~225 lines)
2. Eliminates duplication without compromising performance
3. Low risk - prototype in Week 1-2, evaluate before full rollout

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

**Purpose**: Provide concrete types for trait object returns

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
- Only used for API boundaries (trait object methods)
- Internal code uses `impl AsyncIterator` (unboxed, zero-cost)
- `into_boxed()` method converts between them

---

### Component 4: The `AsyncIterator` Trait

**Purpose**: Unified trait for iterator operations that works in both modes

**Implementation** (~200 lines):

```rust
// In kernel/src/async_iterator/mod.rs

/// Unified trait for async-style iterator operations
/// 
/// Implemented by Iterator (sync mode) and Stream (async mode).
/// Provides a consistent API for working with sequences in both modes.
/// 
/// Note: These transformation methods (map, filter, etc.) are NOT async - they just
/// build up lazy transformation chains. Both Iterator and Stream have non-async
/// transformation methods, so our trait methods can be simple non-async delegates.
/// The async only matters when consuming the iterator/stream (e.g., collect, fold).
pub trait AsyncIterator: Sized {
    type Item;
    
    /// Map each item
    fn async_map<F, R>(self, f: F) -> impl AsyncIterator<Item = R>
    where
        F: FnMut(Self::Item) -> R + Send + 'static,
        R: Send + 'static;
    
    /// Filter items
    fn async_filter<F>(self, f: F) -> impl AsyncIterator<Item = Self::Item>
    where
        F: FnMut(&Self::Item) -> bool + Send + 'static;
    
    /// Flatten nested iterators/streams
    /// 
    /// Note: The item must already be an AsyncIterator (e.g., an Iterator or Stream)
    fn async_flatten(self) -> impl AsyncIterator<Item = <Self::Item as AsyncIterator>::Item>
    where
        Self::Item: AsyncIterator + Send + 'static;
    
    /// Try fold with early exit on error
    #[async_fn]
    fn async_try_fold<B, E, F>(self, init: B, f: F) -> Result<B, E>
    where
        F: FnMut(B, Self::Item) -> Result<B, E> + Send + 'static,
        B: Send + 'static,
        E: Send + 'static;
    
    /// Chain two iterators/streams
    /// 
    /// Note: The `other` parameter must already be an AsyncIterator (e.g., an Iterator or Stream)
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

/// Helper function to convert IntoIterator types into AsyncIterator
/// 
/// Use this when you have types like `Option<T>` or `Vec<T>` that implement
/// `IntoIterator` but not `Stream`. This provides a uniform API in both modes.
/// 
/// # Examples
/// ```rust
/// // Works in both sync and async modes
/// option.into_async_iter().async_flatten()
/// vec.into_async_iter().async_map(|x| x + 1)
/// ```
pub fn into_async_iter<I: IntoIterator>(i: I) -> impl AsyncIterator<Item = I::Item>
where
    I::Item: Send,
    I::IntoIter: Send + 'static,
{
    #[cfg(not(feature = "async"))]
    { i.into_iter() }
    
    #[cfg(feature = "async")]
    { futures::stream::iter(i) }
}

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
- `kernel/src/async_iterator/mod.rs` - trait definition + `into_async_iter()` helper + conditional module imports
- `kernel/src/async_iterator/sync_impl.rs` - sync implementation (delegates to `Iterator`)
- `kernel/src/async_iterator/async_impl.rs` - async implementation (delegates to `Stream`)

**Key points**:
- `AsyncIterator` is a trait implemented by both `Iterator` (sync) and `Stream` (async)
- Trait methods return `impl AsyncIterator` (which compiles to `impl Iterator` or `impl Stream`)
- `BoxedAsyncIterator<T>` type alias is only for API boundaries (trait object methods)
- Most internal code uses `impl AsyncIterator` and never needs boxing
- `into_boxed()` method recovers the concrete type and boxes appropriately (`Box::new` in sync, `Box::pin` in async)
- Modular structure: main trait in `mod.rs`, implementations in separate `sync_impl` and `async_impl` modules
- Users only see the unified `AsyncIterator` trait via conditional `use` statements

---

## Complete Examples

### Example 0: Engine Trait Implementation (Three-Layer Approach)

This example shows all three layers working together:

```rust
use crate::async_iterator::AsyncIterator;

// Type alias for the item type to reduce verbosity
type FileDataResult = DeltaResult<Box<dyn EngineData>>;

// ============= LAYER 1: I/O PRIMITIVES =============
// Two separate implementations - sync I/O is fundamentally different from async I/O

impl DefaultParquetHandler {
    #[cfg(not(feature = "async"))]
    fn read_impl(&self, files: &[FileMeta]) -> DeltaResult<impl AsyncIterator<Item = FileDataResult>> {
        // ... create iterator that reads data from file ...
    }
    
    #[cfg(feature = "async")]
    async fn read_impl(&self, files: &[FileMeta]) -> DeltaResult<impl AsyncIterator<Item = FileDataResult>> {
        // ... create stream that reads data from file ...
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
        // Calls the I/O primitive, boxes the result for the trait object
        // - await_!() handles .await in async mode
        // - into_boxed() recovers concrete type (Iterator/Stream) and boxes appropriately
        Ok(await_!(self.read_impl(files))?.into_boxed())
    }
}
```

**Key insights**:
- **I/O primitives need `#[cfg]`**: Two separate methods because sync/async I/O are fundamentally different
- **Business logic uses `#[async_fn]`**: Single implementation returning `impl AsyncIterator` (a real trait, no macro rewriting!)
- **Public API "just works"**: Thin wrapper using `await_!()` and `.into_boxed()`
- **No boxing overhead**: `impl AsyncIterator` is unboxed; only box at API boundaries with `.into_boxed()`

**Duplication**: Only at the I/O boundary (Layer 1). All higher layers are unified.

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

**Day 4: Type aliases + await macro**
- Add `BoxedAsyncIterator<T>` type alias
- Add `await_!()` macro
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

## Appendix

### Where Duplication Remains

**I/O Primitives (Layer 1)**: Two separate implementations are required because sync and async I/O are fundamentally different:

```rust
#[cfg(not(feature = "async"))]
fn read_impl() -> DeltaResult<impl AsyncIterator<...>> {
    let data = self.storage.read_files(files)?;  // Blocking I/O
    // ...
}

#[cfg(feature = "async")]
async fn read_impl() -> DeltaResult<impl AsyncIterator<...>> {
    let data = self.storage.read_files(files).await?;  // Async I/O
    // ...
}
```

This is **unavoidable** - sync and async I/O have different semantics and cannot be unified.

However, this duplication is:
- **Tightly scoped**: Methods do nothing except the I/O that is fundamentally different
- **Isolated**: The differences don't leak into business logic that uses them

All higher layers (business logic, public API) are completely unified.

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
- ⚠️ Learning curve (~30 min for macro usage)
- ⚠️ I/O layer still requires duplication (unavoidable)
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

---

## Conclusion

This approach provides a clean, performant solution to eliminate sync/async code duplication in delta-kernel-rs. The infrastructure is simple (~225 lines), the performance overhead is zero, and the benefits (single source, easier maintenance) are substantial.

The three-layer architecture clearly separates concerns:
- **Layer 1 (I/O)**: Minimal unavoidable duplication
- **Layer 2 (Business Logic)**: Zero duplication, single source
- **Layer 3 (Public API)**: Zero duplication, thin wrapper

**Recommendation**: Proceed with prototype in Phase 1-2, then evaluate for full rollout.

