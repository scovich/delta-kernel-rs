# AsyncIterator: Detailed Analysis - What Actually Needs It?

**Date**: October 20, 2025  
**Purpose**: Precise analysis of which iterator patterns truly cross async boundaries vs. pure business logic

---

## Executive Summary

**Key Finding**: The vast majority of iterator usage is **pure business logic** that doesn't need `AsyncIterator` at all. Only a small subset of patterns actually cross the sync/async boundary.

### What Actually Needs AsyncIterator

**Public APIs (3)**:
1. `Scan::scan_metadata()` → `impl Iterator<Item = DeltaResult<ScanMetadata>>`
2. `Scan::execute()` → `impl Iterator<Item = DeltaResult<ScanResult>>`
3. `TableChangesScan::execute()` → `impl Iterator<Item = DeltaResult<ScanResult>>`

**Engine Trait Methods (4)**:
1. `JsonHandler::read_json_files()` → `FileDataReadResultIterator`
2. `ParquetHandler::read_parquet_files()` → `FileDataReadResultIterator`
3. `StorageHandler::list_from()` → `Box<dyn Iterator<Item = DeltaResult<FileMeta>>>`
4. `StorageHandler::read_files()` → `Box<dyn Iterator<Item = DeltaResult<Bytes>>>`

**Internal patterns (2-3)**:
- Iterator chains in `Scan::execute()` that wrap engine calls
- Iterator chains in log replay that call storage handlers

**Total**: ~10-12 specific call sites that truly cross async boundaries.

---

## Categorization by Module

### Core Kernel (kernel/src/)

**Uses AsyncIterator**: 
- `scan/mod.rs`: 2 public APIs (`scan_metadata`, `execute`) + internal chains
- `table_changes/scan.rs`: 1 public API (`execute`) + internal chains
- `log_segment.rs`: Internal iterator that calls storage handler

**Estimated**: 3-5 AsyncIterator usage sites

**Pure business logic** (no AsyncIterator needed):
- `scan/log_replay.rs`: Processes action batches (already in memory)
- `action_reconciliation/log_replay.rs`: Processes action batches
- `table_changes/scan_file.rs`: Transforms ScanMetadata → CdfScanFile
- `schema/mod.rs`: All schema manipulation
- `expressions/*.rs`: All expression evaluation
- `transforms.rs`: Partition value parsing
- Most of `actions/*.rs`: Action visiting/processing

**Estimated**: 50+ iterator chains that are pure business logic

---

### Default Engine (kernel/src/engine/default/)

**Uses AsyncIterator**:
- `json.rs`: `read_json_files()` - returns iterator, uses `.buffered()`
- `parquet.rs`: `read_parquet_files()` - returns iterator
- `filesystem.rs`: `list_from()`, `read_files()` - return iterators

**Estimated**: 3-4 AsyncIterator usage sites (handler implementations)

**Pure business logic** (no AsyncIterator needed):
- `file_stream.rs`: Internal Stream implementation (stays as Stream)
- Most of `json.rs` / `parquet.rs` internals

---

### Sync Engine (kernel/src/engine/sync/)

**Uses AsyncIterator**:
- `json.rs`: `read_json_files()` implementation
- `parquet.rs`: `read_parquet_files()` implementation
- `storage.rs`: `list_from()`, `read_files()` implementations

**Estimated**: 3-4 AsyncIterator usage sites (handler implementations)

**Note**: Sync engine is already sync, so these would use `#[async_fn]` but never actually await.

---

### Tests (kernel/tests/, kernel/src/*/tests.rs)

**Uses AsyncIterator**: Nearly none! Tests consume iterators but don't produce them.

**Pure business logic**: All test code uses regular iterators to consume results:
- `.try_collect()` to collect results
- `.map_ok()` / `.flatten_ok()` for test assertions
- All iteration is over already-fetched data

**Important insight**: Tests show we need **extension methods like `try_collect`, `map_ok`, `flatten_ok`** but these are used on *regular iterators*, not AsyncIterator!

---

### FFI (ffi/src/)

**Uses AsyncIterator**: None directly. FFI exposes C-style iteration APIs.

---

## The Critical Insight: Two Different Iterator Patterns

### Pattern A: Engine Boundary Iterators (AsyncIterator needed)

**These cross the sync/async boundary**:

```rust
// Public API in kernel
impl Scan {
    pub fn execute(&self, engine: Arc<dyn Engine>) 
        -> DeltaResult<impl Iterator<Item = DeltaResult<ScanResult>>> 
    {
        // ... business logic to discover files ...
        
        // THIS crosses the boundary - calls engine which does I/O
        let read_result_iter = engine.parquet_handler().read_parquet_files(...)?;
        
        // Wraps the engine iterator with business logic
        Ok(read_result_iter.map(|result| {
            // Transform data (no I/O)
            transform_to_logical(...)
        }))
    }
}
```

**Characteristics**:
- Returns from engine trait methods
- Wraps actual file I/O operations
- Must work in both sync and async modes
- ~10-12 total sites in codebase

**Needs AsyncIterator**: ✅ YES

---

### Pattern B: Business Logic Iterators (regular Iterator/Stream)

**These don't cross boundaries**:

```rust
// Internal processing - no I/O
fn scan_metadata_to_scan_file(
    scan_metadata: impl Iterator<Item = DeltaResult<TableChangesScanMetadata>>,
) -> impl Iterator<Item = DeltaResult<CdfScanFile>> {
    scan_metadata.map(|scan_metadata| -> DeltaResult<_> {
        let scan_metadata = scan_metadata?;
        // ... pure data transformation ...
        Ok(CdfScanFile { /* ... */ })
    })
}
```

**Characteristics**:
- Operates on already-fetched data
- No engine calls, no I/O
- Pure data transformation
- Hundreds of sites in codebase

**Needs AsyncIterator**: ❌ NO - just use regular iterators!

---

## What This Means for AsyncIterator Trait

### Minimal Required Methods

For the **~10-12 actual AsyncIterator usage sites**, we need:

**Core transformation methods**:
- `async_map` - wrapping engine iterators with transforms
- `async_flatten` - flattening nested engine iterators

**That's it!** The other 90% of iterator methods are used in pure business logic.

### Extension Methods Still Important

Methods like `try_collect`, `map_ok`, `flatten_ok` ARE heavily used, but on **regular iterators** in business logic, not on `AsyncIterator`.

**Solution**: These should work on regular `Iterator` and `Stream` separately, not require `AsyncIterator`.

---

## Detailed Call Chain Analysis

### Scan::execute() - The Primary Use Case

```rust
pub fn execute(&self, engine: Arc<dyn Engine>) 
    -> DeltaResult<impl Iterator<Item = DeltaResult<ScanResult>>> 
{
    // Step 1: Get scan metadata (calls engine for log replay - AsyncIterator!)
    let scan_metadata_iter = self.scan_metadata(engine.as_ref())?;
    
    // Step 2: Transform to scan files (pure logic - regular iterator)
    let scan_files_iter = scan_metadata_iter
        .map(|res| { /* visit scan metadata */ })
        .flatten_ok();  // itertools extension - regular iterator!
    
    // Step 3: For each file, read parquet data (AsyncIterator!)
    let result = scan_files_iter
        .map(move |scan_file| -> DeltaResult<_> {
            // THIS is the AsyncIterator boundary:
            let read_result_iter = engine.parquet_handler()
                .read_parquet_files(&[meta], schema, None)?;
            
            // Wrap with transform (needs async_map)
            Ok(read_result_iter.async_map(move |read_result| -> DeltaResult<_> {
                // Pure transform logic
                transform_to_logical(...)
            }))
        })
        .flatten_ok()  // itertools - still on regular iterator!
        .map(|x| x?);
    
    Ok(result)
}
```

**AsyncIterator needed**: 
- ✅ `read_parquet_files()` return type
- ✅ `.async_map()` on that return value

**Regular iterator extensions needed**:
- `.flatten_ok()` on regular iterator
- `.map()` on regular iterator

---

### Scan::scan_metadata() - Internal Use Case

```rust
pub fn scan_metadata(&self, engine: &dyn Engine) 
    -> DeltaResult<impl Iterator<Item = DeltaResult<ScanMetadata>>> 
{
    // Calls log_segment.read_actions()
    let action_iter = self.snapshot.log_segment().read_actions(
        engine,
        commit_schema,
        checkpoint_schema,
        None,
    )?;
    
    // Process actions (pure logic, but wraps engine call)
    Ok(scan_action_iter(engine, action_iter, self.state_info.clone()))
}
```

**Inside `log_segment.read_actions()`**:
```rust
fn read_actions(&self, engine: &dyn Engine, ...) 
    -> DeltaResult<impl Iterator<Item = DeltaResult<ActionsBatch>> + Send> 
{
    // THIS crosses boundary - calls engine
    let commit_stream = engine.json_handler()
        .read_json_files(&commit_files, schema, None)?;
    
    // Wrap in ActionsBatch (needs async_map)
    Ok(commit_stream.async_map(|data| {
        Ok(ActionsBatch::new(data?, true))
    }))
}
```

**AsyncIterator needed**:
- ✅ `read_json_files()` return type
- ✅ `.async_map()` to wrap in ActionsBatch

---

## Revised Method Requirements

### Tier 1: Critical (For the ~10 real AsyncIterator sites)

**Must have**:
1. `async_map` - Most common operation (8+ uses)
2. `async_flatten` - Nested iterators (3-4 uses)
3. `into_boxed` - API boundary conversion

**That's the core!** Only 3 methods for actual AsyncIterator usage.

---

### Tier 2: Extension Methods (For regular iterators)

These are heavily used but on **regular `Iterator`/`Stream`**, not `AsyncIterator`:

1. `try_collect()` - 55 uses (all on regular iterators in tests/business logic)
2. `map_ok()` - 50+ uses (all on regular iterators)
3. `flatten_ok()` - 9 uses (all on regular iterators)

**Solution**: Don't bloat AsyncIterator - these work fine with itertools/futures extensions.

---

### Tier 3: Potentially Useful (For AsyncIterator, if convenient)

If easy to add to AsyncIterator:
- `async_filter` - Might be used in future data skipping
- `async_try_fold` - Already in proposal, useful for stateful processing
- `async_chain` - Composing multiple engine iterators

But usage is currently 0-1 in the actual AsyncIterator call sites!

---

## The `buffered()` Special Case

**Finding**: Only used in `default/json.rs`:

```rust
let mut stream = stream::iter(file_futures)
    .buffered(buffer_size)  // Execute N futures concurrently
    .try_flatten()
```

**This is Stream-specific** for parallel I/O. In sync mode, there's no parallelism.

**Options**:
1. Keep as Stream-specific code (don't unify this part)
2. Add to AsyncIterator with no-op in sync mode
3. Use `into_boxed_async_iterator` helper before buffering

**Recommendation**: Option 1 - this stays as native Stream code in the async implementation.

---

## Revised Recommendations

### Minimal Viable AsyncIterator

```rust
pub trait AsyncIterator: Sized {
    type Item;
    
    // Core - actually needed
    fn async_map<F, R>(self, f: F) -> impl AsyncIterator<Item = R>
    where
        F: FnMut(Self::Item) -> R + Send + 'static,
        R: Send + 'static;
    
    fn async_flatten(self) -> impl AsyncIterator<Item = <Self::Item as AsyncIterator>::Item>
    where
        Self::Item: AsyncIterator + Send + 'static;
    
    // API boundary
    fn into_boxed(self) -> BoxedAsyncIterator<Self::Item>;
}

// Helper for engine implementations
pub(crate) fn into_boxed_async_iterator<E, Fut, S, T>(
    executor: &E,
    stream_future: Fut,
) -> DeltaResult<BoxedAsyncIterator<T>>;
```

**Total**: 3 methods. That's it!

---

### Potentially Useful Additions

If we want richer API (still useful for the 10 sites):

```rust
// Add these if desired:
fn async_filter<F>(self, f: F) -> impl AsyncIterator<Item = Self::Item>;
fn async_chain<U>(self, other: U) -> impl AsyncIterator<Item = Self::Item>;
fn async_try_fold<B, E, F>(self, init: B, f: F) -> Result<B, E>;  // #[async_fn]
```

---

### Don't Add These to AsyncIterator

Let regular itertools/futures extensions handle:
- `try_collect` - used 55 times, all on regular iterators
- `map_ok` / `flatten_ok` - used 60+ times, all on regular iterators
- `enumerate`, `zip`, `inspect` - pure business logic operations
- `collect`, `for_each` - terminal operations on regular iterators

---

## Migration Impact Analysis

### Files That Actually Need Changes

**Core kernel** (~5 files):
1. `scan/mod.rs` - Update `execute()` and `scan_metadata()` return types
2. `table_changes/scan.rs` - Update `execute()` return type
3. `log_segment.rs` - Update `read_actions()` internals

**Engine implementations** (~6 files):
1. `engine/default/json.rs` - Update `read_json_files()`
2. `engine/default/parquet.rs` - Update `read_parquet_files()`
3. `engine/default/filesystem.rs` - Update `list_from()`, `read_files()`
4. `engine/sync/json.rs` - Same updates
5. `engine/sync/parquet.rs` - Same updates
6. `engine/sync/storage.rs` - Same updates

**Total affected files**: ~11 files

**All other iterator usage** (100+ files): No changes needed - stays as regular iterators!

---

## Conclusion

### Key Findings

1. **Only ~10-12 call sites** actually need `AsyncIterator`
2. **~200+ iterator usage sites** are pure business logic - keep as regular iterators
3. **Extension methods** (`try_collect`, `map_ok`, etc.) are used on regular iterators, not AsyncIterator
4. **Minimal trait** needs only 3 methods: `async_map`, `async_flatten`, `into_boxed`

### Recommendation

✅ **Proceed with minimal AsyncIterator trait** (3 core methods)

❌ **Don't bloat it** with methods that are only used on regular iterators

✅ **Rely on itertools/futures** for extension methods on regular iterators

### The Win

Instead of:
- ❌ Converting 200+ iterator chains to AsyncIterator
- ❌ Implementing 15+ methods on AsyncIterator
- ❌ Testing all combinations

We only need:
- ✅ Convert 10-12 engine boundary iterators to AsyncIterator  
- ✅ Implement 3 core methods
- ✅ Keep 200+ business logic chains as regular iterators

**Effort reduction**: ~95% fewer changes than originally estimated!

