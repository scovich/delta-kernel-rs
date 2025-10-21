# Async/Sync Unification Using Proc Macros

**Date**: October 18, 2025  
**Goal**: Eliminate sync/async code duplication using simple proc macros and unified trait

---

## Table of Contents

- [Executive Summary](#executive-summary)
- [The Two Challenges](#the-two-challenges)
- [The Approach](#the-approach)
- [Infrastructure Components](#infrastructure-components)
  - [1. `#[async_fn]` Macro](#1-async_fn-macro)
  - [2. `await_!()` Macro](#2-await-macro)
  - [3. `#[async_trait]` Macro](#3-async_trait-macro)
  - [4. `AsyncIterator` Trait](#4-asynciterator-trait)
  - [5. `BoxedAsyncIterator` Type Alias](#5-boxedasynciterator-type-alias)
  - [6. Bridge Adapters](#6-bridge-adapters)
- [Three-Layer Architecture](#three-layer-architecture)
- [Feature Flags and Build Configuration](#feature-flags-and-build-configuration)
- [Real Code Migration Examples](#real-code-migration-examples)
- [Implementation Plan](#implementation-plan)
- [Risk Assessment and Mitigation](#risk-assessment-and-mitigation)
- [Open Questions](#open-questions)
- [Next Steps](#next-steps)

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

### Infrastructure (~165 lines total)

| Component | Purpose |
|-----------|---------|
| `#[async_fn]` macro (~15 LOC) | Conditionally adds `async` keyword to functions |
| `await_!()` macro (~10 LOC) | Conditionally adds `.await` to expressions |
| `#[async_trait]` macro (~15 LOC) | Conditionally boxes futures for trait dyn-compatibility |
| `AsyncIterator` trait (~100 LOC) | Unified abstraction over Iterator/Stream |
| `BoxedAsyncIterator` alias (~5 LOC) | Concrete type for API boundaries |
| AsyncIterator adapters (~30 LOC) | Conversion helpers between concrete types and AsyncIterator |

### Three-Layer Architecture

The approach minimizes duplication by separating concerns:

1. **I/O Primitives** (bottom layer): Native async implementations
   - One async method per handler containing all I/O logic
   - Uses `into_boxed_async_iterator` helper to bridge modes

2. **Business Logic** (middle layer): Single implementation using `#[async_fn]`
   - Uses `impl AsyncIterator` for zero-cost abstraction
   - Transforms/filters/processes data

3. **Public API** (top layer): Thin wrapper using `#[async_fn]` + `await_!`
   - Calls I/O primitive and converts appropriately
   - Single trait implementation per handler method

**Key insight**: All six components work together to enable single-source implementations at every layer. Components 1-3 handle conditional syntax, 4-6 handle type abstraction. Only ~10-12 call sites need `AsyncIterator` - the rest (200+ iterator chains) use regular `Iterator`/`Stream`.

### Benefits

✅ Zero duplication - single source for all business logic  
✅ Zero overhead - sync mode uses standard iterators, no boxing  
✅ Fix bugs once, add features once, refactor once  
✅ Full IDE support - go-to-definition, autocomplete, refactoring  
✅ Low risk - prototype incrementally, easy fallback

**Note**: The `Engine` trait itself doesn't need modifications - it just returns `Arc<dyn Handler>`. Only the handler traits (like `ParquetHandler`) use `#[async_trait]` + `#[async_fn]` on their I/O methods to maintain dyn-compatibility. No separate `AsyncEngine` trait needed.

### Why This Matters

Delta-kernel-rs needs both sync and async modes. Without this approach, orchestration code must be duplicated even when business logic is shared.

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

**What this means**: This approach makes the existing async nature **optional and exposed** instead of **hidden and forced**. We're not adding async - we're giving consumers choice.

---

## The Two Challenges

Delta-kernel-rs faces two fundamental challenges when trying to eliminate sync/async duplication.

---

### Challenge 1: Conditional Compilation Syntax

**The Problem:**

Three syntax elements need to change based on whether async mode is enabled:

**A. The `async` keyword**

```rust
// Sync:  fn read_metadata(...) -> Result<Metadata>
// Async: async fn read_metadata(...) -> Result<Metadata>
```

**B. The `.await` operator**

```rust
// Sync:  let data = engine.read_file(path)?;
// Async: let data = engine.read_file(path).await?;
```

**C. Trait dyn-compatibility with async methods**

Handler traits are used as trait objects:

```rust
pub trait Engine {
    fn parquet_handler(&self) -> Arc<dyn ParquetHandler>;  // Trait object
}
```

But `async fn` in traits is not dyn-compatible:

```rust
pub trait ParquetHandler {
    async fn read_files(...) -> Result<...>;  // Returns impl Future
}

// This won't compile!
let handler: Arc<dyn ParquetHandler> = ...;  // ERROR: trait not dyn-compatible
```

**Why not dyn-compatible?**
- `async fn` desugars to `-> impl Future<Output = ...>`
- `impl Trait` in return position makes a trait not dyn-compatible
- Can't create `Arc<dyn Trait>` when trait isn't dyn-compatible

Without solutions: duplicate every function, call site, and trait definition.

**The Solution: Three Conditional Syntax Macros (Components 1-3)**

**Component 1: `#[async_fn]` macro** - Conditionally adds `async` keyword  
**Component 2: `await_!()` macro** - Conditionally adds `.await`  
**Component 3: `#[async_trait]` macro** - Conditionally boxes futures for dyn-compatibility

**Example using all three together:**

```rust
// Unified trait definition
#[async_trait]  // Component 3: Makes trait dyn-compatible
pub trait ParquetHandler {
    #[async_fn]  // Component 1: Adds async keyword
    fn read_parquet_files(&self, files: &[FileMeta]) 
        -> DeltaResult<FileDataReadResultIterator>;
}

// Unified implementation
#[async_trait]  // Component 3: Also needed on impl
impl ParquetHandler for DefaultParquetHandler {
    #[async_fn]  // Component 1: Adds async keyword
    fn read_parquet_files(&self, files: &[FileMeta]) 
        -> DeltaResult<FileDataReadResultIterator> {
        await_!(  // Component 2: Adds .await
            into_boxed_async_iterator(&self.executor, self.read_parquet_impl(files))
        )
    }
}

// Unified business logic
#[async_fn]  // Component 1
fn process(engine: &dyn Engine) -> DeltaResult<Data> {
    let handler = engine.parquet_handler();
    let data = await_!(handler.read_parquet_files(...))?;  // Component 2
    Ok(data)
}
```

**In sync mode:**
- `#[async_fn]` is no-op → regular `fn`
- `await_!()` is no-op → expression evaluates directly
- `#[async_trait]` is no-op → no boxing

**In async mode:**
- `#[async_fn]` adds `async` → `async fn`
- `await_!()` adds `.await` → expression is awaited
- `#[async_trait]` boxes futures → trait becomes dyn-compatible

**Result:** Single source for all conditional syntax.

---

### Challenge 2: Iterator and Stream Are Incompatible Types

**The Problem:**

Iterator (sync) and Stream (async) are fundamentally different types, creating three related issues:

**A. Can't return "iterator or stream" from functions**

```rust
// This doesn't work - can't abstract over Iterator/Stream in return type
fn get_scan_files(engine: &dyn Engine) -> impl ??? {
    let files = discover_files(engine);  // I/O returns Iterator or Stream
    files.map(|f| transform(f))          // Uses Iterator or Stream methods
}
```

You need a return type that works in both modes. Neither `impl Iterator` nor `impl Stream` works - you need a **unified abstraction**.

**B. Once you have that abstraction, you need unified methods**

Even if you could somehow return a unified type, Iterator and Stream have incompatible APIs:

```rust
// Iterator methods (sync)
items.filter(|x| x.is_valid())     // Sync closure
     .map(|x| process(x))          // Called .map()

// Stream methods (async)  
items.filter(|x| async { x.is_valid() })  // Async closure
     .then(|x| async { process(x) })      // Called .then(), not .map()
```

The unified abstraction needs to provide its own methods that work in both modes.

**C. Trait methods can't return `impl Trait`**

At API boundaries (trait definitions), you can't return `impl AsyncIterator` - traits need concrete types:

```rust
pub trait ParquetHandler {
    fn read_files(...) -> impl AsyncIterator<...>;  // ERROR: not allowed in traits
}
```

You need a concrete boxed type: `Box<dyn Iterator>` (sync) or `Pin<Box<dyn Stream>>` (async).

**Root cause:** Iterator and Stream are unrelated types.

**The Solution: AsyncIterator Abstraction (Components 4-6)**

This problem requires three pieces working together:

**Component 4: `AsyncIterator` trait** - Unified abstraction providing `.async_map()`, `.async_filter()` methods  
**Component 5: `BoxedAsyncIterator` alias** - Concrete boxed type for API boundaries  
**Component 6: AsyncIterator adapters** - Conversion helpers between concrete types and abstractions

**Complete example:**

```rust
// Internal business logic - uses AsyncIterator trait (Component 4)
#[async_fn]
fn get_scan_files(engine: &dyn Engine) -> DeltaResult<impl AsyncIterator<Item = ScanFile>> {
    let files = await_!(discover_files(engine))?;
    Ok(files
        .async_filter(|f| f.is_valid())    // Component 4: unified method
        .async_map(|f| {                    // Component 4: unified method
            let data = await_!(read_file(engine, f))?;
            transform(data)
        })
    )
}

// API boundary - uses BoxedAsyncIterator (Component 5)
#[async_trait]
pub trait ParquetHandler {
    #[async_fn]
    fn read_parquet_files(&self, files: &[FileMeta]) 
        -> DeltaResult<FileDataReadResultIterator>;  // Component 5: type alias
}

// Implementation - uses adapter (Component 6)
#[async_trait]
impl ParquetHandler for DefaultParquetHandler {
    #[async_fn]
    fn read_parquet_files(&self, files: &[FileMeta]) 
        -> DeltaResult<FileDataReadResultIterator> {
        await_!(
            into_boxed_async_iterator(  // Component 6: converts Stream to boxed type
                &self.executor,
                self.read_parquet_impl(files)
            )
        )
    }
}
```

**In sync mode:**
- `AsyncIterator` implemented for `Iterator` - delegates to `.map()`, `.filter()`
- `BoxedAsyncIterator` = `Box<dyn Iterator>`
- Adapter blocks on futures, converts `Stream` → `Iterator`
- Returns `impl Iterator` or `Box<dyn Iterator>`

**In async mode:**
- `AsyncIterator` implemented for `Stream` - delegates to `.then()`, `.filter()`
- `BoxedAsyncIterator` = `Pin<Box<dyn Stream>>`
- Adapter awaits futures, boxes `Stream`
- Returns `impl Stream` or `Pin<Box<dyn Stream>>`

**Key point:** Only one implementation exists in any given build. The `#[cfg]` guards ensure no conflicts.

**Result:** Single source using unified `.async_*()` methods, `impl AsyncIterator` returns, and `BoxedAsyncIterator` for trait boundaries.

---

## The Approach

### Challenge-Solution Mapping

Both challenges are now solved:

| Challenge | Components | What They Do |
|-----------|------------|--------------|
| **Challenge 1**: Conditional Syntax | **1-3** | `async_fn`, `await_!`, `async_trait` |
| **Challenge 2**: Type Incompatibility | **4-6** | `AsyncIterator`, `BoxedAsyncIterator`, adapters |

The following sections detail each component's implementation.

---

## Infrastructure Components

Each component is detailed below:

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

**Key points**: Works on free functions, methods, and trait methods

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

### Component 3: The `#[async_trait]` Macro

**Purpose**: Conditionally boxes futures to make traits with async methods dyn-compatible

**Why needed**: Traits used as trait objects (like `Arc<dyn ParquetHandler>`) must be dyn-compatible. Native `async fn` in traits returns `impl Future`, which makes the trait not dyn-compatible. The `async-trait` crate boxes these futures, making the trait dyn-compatible.

**Implementation** (~15 lines in derive-macros/src/lib.rs):
```rust
/// No-op proc macro that stands in for async-trait in sync mode.
/// In async mode, kernel imports the real async-trait crate using this name.
#[proc_macro_attribute]
pub fn async_trait(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    // Sync mode: no-op, return item unchanged
    item
}
```

**Conditional import** (in kernel/src/lib.rs):
```rust
// Async mode: use real async-trait crate
#[cfg(feature = "async")]
use async_trait::async_trait;

// Sync mode: use our no-op macro
#[cfg(not(feature = "async"))]
use delta_kernel_derive::async_trait;
```

**Usage**: Applied to both trait definitions AND implementations:

```rust
// On trait definition
#[async_trait]
pub trait ParquetHandler: AsAny {
    #[async_fn]
    fn read_parquet_files(...) -> DeltaResult<FileDataReadResultIterator>;
}

// On trait implementation (also needed!)
#[async_trait]
impl ParquetHandler for DefaultParquetHandler {
    #[async_fn]
    fn read_parquet_files(...) -> DeltaResult<FileDataReadResultIterator> {
        // implementation
    }
}
```

**Key points**:
- Applied to both trait definitions AND `impl` blocks (not struct definitions)
- Sync mode: no-op; Async mode: boxes futures for dyn-compatibility

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
- Internal only - never exposed to consumers
- Both `Iterator` and `Stream` implement this trait (conditionally, based on mode)
- Returns `impl AsyncIterator` - use `.into_boxed()` to recover concrete `Iterator`/`Stream`
- Modular: trait in `mod.rs`, implementations in `sync_impl.rs` and `async_impl.rs`

---

### Component 5: `BoxedAsyncIterator` Type Alias

**Purpose**: Provide concrete boxed types for API boundaries (trait method returns)

**Why needed**: Trait methods can't return `impl AsyncIterator` - they need concrete types. The `BoxedAsyncIterator` alias provides the appropriate boxed type for each mode.

**Implementation** (~5 lines):
```rust
// In kernel/src/lib.rs

// Generic boxed type for API boundaries
#[cfg(not(feature = "async"))]
pub type BoxedAsyncIterator<T> = Box<dyn Iterator<Item = T> + Send>;

#[cfg(feature = "async")]
pub type BoxedAsyncIterator<T> = Pin<Box<dyn Stream<Item = T> + Send>>;

// Specialized types build on BoxedAsyncIterator
pub type ScanFilesIterator = BoxedAsyncIterator<DeltaResult<ScanFile>>;
pub type FileDataReadResultIterator = BoxedAsyncIterator<DeltaResult<Box<dyn EngineData>>>;
```

**Usage**:
```rust
// Trait methods use BoxedAsyncIterator as concrete return type
#[async_trait]
pub trait ParquetHandler {
    #[async_fn]
    fn read_parquet_files(&self, files: &[FileMeta]) 
        -> DeltaResult<FileDataReadResultIterator>;  // Concrete boxed type
}
```

**Key points**:
- Exposes real types: `Box<dyn Iterator>` (sync) or `Pin<Box<dyn Stream>>` (async)
- Internal code uses unboxed `impl AsyncIterator`

---

### Component 6: AsyncIterator Adapters

**Purpose**: Convert concrete Iterator/Stream types to internal AsyncIterator abstraction

Two adapter functions bridge from different sources:

#### 6a. `into_boxed_async_iterator` - For Streams (Critical Path)

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

**Impact**: Single unified trait implementation per handler method.

---

#### 6b. `into_async_iter` - For IntoIterator Types

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

**Note**: `into_boxed_async_iterator` is on the critical path (engine I/O), while `into_async_iter` handles collections.

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
// Trait definition uses both async-trait (Component 3) and async_fn (Component 1)

#[async_trait]  // Component 3: Boxes futures, makes trait dyn-compatible
pub trait ParquetHandler {
    #[async_fn]  // Component 1: Adds async keyword
    fn read_parquet_files(&self, files: &[FileMeta]) -> DeltaResult<FileDataReadResultIterator>;
}

// Trait implementation also uses async-trait + async_fn
#[async_trait]  // Component 3: Also needed on impl
impl ParquetHandler for DefaultParquetHandler {
    #[async_fn]  // Component 1: Adds async keyword
    fn read_parquet_files(&self, files: &[FileMeta]) -> DeltaResult<FileDataReadResultIterator> {
        // Uses into_boxed_async_iterator helper (Component 6)
        // - Sync mode: blocks on future, converts Stream→Iterator
        // - Async mode: awaits future, boxes Stream
        await_!(into_boxed_async_iterator(&self.executor, self.read_parquet_impl(files)))
    }
}
```

**Key insights**:
- **I/O layer**: One native async implementation (all logic in one place)
- **Handler trait**: Uses Components 1 + 3 (async_fn + async_trait)
- **Handler impl**: Uses Components 1, 2, 3, 6 (async_fn, await_!, async_trait, adapter)
- **Business logic**: Uses Component 1 + 4 (async_fn + AsyncIterator)
- **Boxing**: Only at trait object boundary (async mode) and API boundary

**Result**: Single implementation using all six components (see Challenge-Solution Mapping table).

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

---

## Implementation Plan

### Phase 1: Infrastructure (Week 1)

**Day 1: Foundation macros**
- Create `derive-macros` crate (already exists)
- Implement `#[async_fn]` macro (~15 lines) - Component 1
- Implement `await_!()` macro (~10 lines) - Component 2
- Implement `#[async_trait]` no-op macro (~15 lines) - Component 3
- Add async-trait as optional dependency in kernel/Cargo.toml
- Add conditional import in kernel/src/lib.rs
- Test all three macros with simple functions

**Day 2-3: AsyncIterator trait**
- Create `kernel/src/async_iterator/` module
- Define `AsyncIterator` trait (Component 4)
- Implement for `Iterator` (sync_impl.rs)
- Implement for `Stream` (async_impl.rs)
- Test all methods in both modes

**Day 4: Type aliases + adapters**
- Add `BoxedAsyncIterator<T>` type alias (Component 5)
- Add `into_boxed_async_iterator` helper (Component 6)
- Add `into_async_iter` helper (Component 6)
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

This approach eliminates sync/async duplication using six simple components (~165 lines total) with zero overhead in sync mode.

**Architecture**:
- **Components 1-3**: Conditional syntax transformations
- **Components 4-6**: Type abstraction system

**Result**: Single-source implementations at all three layers (I/O, business logic, public API).

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

**Note**: This analysis was conducted during the exploration phase to ensure all async callsites are properly handled.


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

**Pros**: Zero duplication, zero overhead, single source, full IDE support

**Cons**: ~30 min learning curve, test both modes in CI, `+ 'static` bounds

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

## Feature Flags and Build Configuration

### Feature Hierarchy

The implementation requires adding new feature flags to `kernel/Cargo.toml`:

```toml
[features]
# Core async support - enables async/await mode
async = ["futures"]

# Default engine features (work in both sync and async modes)
default-engine-base = [
  "arrow-conversion",
  "arrow-expression",
  "futures",
  "need-arrow",
  "tokio",
]
default-engine-native-tls = ["default-engine-base", "reqwest/default"]
default-engine-rustls = [
  "default-engine-base",
  "reqwest/rustls-tls-native-roots",
  "reqwest/http2",
]
```

**Key points**:
- `async` feature is fully orthogonal to engine features
- Engine features work identically in both sync and async modes
- No need for separate `default-engine-async-*` variants

### Choosing Features

**For library consumers**:
```toml
# Sync mode (default, backward compatible)
delta_kernel = { version = "0.17", features = ["default-engine-rustls"] }

# Async mode (opt-in) - just add "async" to existing features
delta_kernel = { version = "0.17", features = ["async", "default-engine-rustls"] }
```

**For FFI consumers**:
```toml
# FFI always uses sync mode (never enable "async")
delta_kernel_ffi = { version = "0.17", features = ["default-engine-rustls"] }
```

### TLS Selection

Choose ONE of:
- `default-engine-native-tls` (uses system TLS)
- `default-engine-rustls` (pure Rust TLS)

Then optionally add `async` for async mode:
- `features = ["default-engine-rustls"]` → sync mode
- `features = ["async", "default-engine-rustls"]` → async mode

**Recommendation**: Use `rustls` variant (pure Rust, no system dependencies, modern TLS 1.3 support).

**Workspace limitation**: If different crates in your workspace request different TLS variants (e.g., one uses `native-tls`, another uses `rustls`), Cargo's feature unification will enable both. This is a known Cargo limitation. Best practice: align your entire workspace on one TLS choice.

### FFI Considerations

The FFI layer always operates in sync mode. Async mode is incompatible with C FFI because:
- C code cannot `.await` Rust futures
- FFI expects synchronous `next()`, not `async fn poll_next()`
- Would require FFI to expose and manage a tokio runtime

The FFI crate enforces this with compile guards:
```rust
#[cfg(feature = "async")]
compile_error!("The FFI crate does not support async mode. FFI must use synchronous APIs.");
```

**Note**: The current `default-engine-rustls` uses async I/O internally (via tokio executor) but exposes a synchronous API. This works perfectly for FFI - the executor's `block_on()` bridges async internals to sync API.

### Valid Feature Combinations

| Mode | Arrow | TLS | Features | Valid? |
|------|-------|-----|----------|--------|
| Minimal | None | - | `[]` | ✅ |
| Sync | 56 | Rustls | `["default-engine-rustls"]` | ✅ |
| Sync | 56 | Native | `["default-engine-native-tls"]` | ✅ |
| Async | 56 | Rustls | `["async", "default-engine-rustls"]` | ✅ |
| Async | 56 | Native | `["async", "default-engine-native-tls"]` | ✅ |
| Async-only | None | - | `["async"]` | ✅ (no engine) |

---

## Further Reading

- **Consumer impact summary**: [ASYNC-CONSUMER-SUMMARY.md](ASYNC-CONSUMER-SUMMARY.md) - Migration examples, decision matrix, gotchas
- **Entry point**: [README-ASYNC.md](README-ASYNC.md) - Quick overview and navigation

