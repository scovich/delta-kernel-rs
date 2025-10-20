# Async Macro Approach: Consumer Impact Summary

**Date**: October 20, 2025

---

## TL;DR

The async macro approach would make kernel APIs conditionally async/sync based on feature flags. Here's what happens to each consumer:

| Consumer Type | Impact | Recommended Action |
|---------------|--------|-------------------|
| **FFI Layer** | ✅ No changes needed | Keep using sync mode |
| **Example Programs** | ⚠️ Require async runtime setup | Can stay sync or migrate to async |
| **Test Suite** | ⚠️ Minor changes | Already using `#[tokio::test]`, add `.await` calls |
| **Library Consumers** | ✅ Opt-in choice | Choose sync (default) or async (opt-in) |

---

## Key Findings

### 1. The DefaultEngine Paradox

**Critical Discovery**: DefaultEngine is **already async internally**!

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

**Implication**: The macro approach helps kernel business logic, but the **engine layer still needs conditional code** at the I/O boundary.

### 2. FFI Layer Can Stay Untouched

The FFI should compile with async feature **disabled**:

```toml
# ffi/Cargo.toml
[dependencies]
delta_kernel = { path = "../kernel" }  # No async feature
```

This keeps the C API stable and synchronous, which is what FFI consumers expect.

### 3. Example Programs Need Migration (If They Go Async)

**Today**:
```rust
fn main() -> ExitCode {
    let engine = get_engine()?;
    let snapshot = Snapshot::builder_for(url).build(&engine)?;
    for item in scan.execute(engine)? {
        // process
    }
}
```

**With async feature enabled**:
```rust
#[tokio::main]  // <-- Required!
async fn main() -> ExitCode {
    let engine = get_engine()?;
    let snapshot = Snapshot::builder_for(url).build(&engine).await?;  // <-- .await
    let mut stream = scan.execute(engine).await?;  // <-- Stream, not Iterator
    while let Some(item) = stream.next().await {  // <-- async iteration
        // process
    }
}
```

**Key changes**:
- Need `#[tokio::main]` or equivalent
- Add `.await` to kernel calls
- Iterator → Stream (different API)

**BUT**: Examples can also just stay in sync mode (no feature flag) and work as-is!

### 4. Library Consumers Get a Choice

**Backward compatible**: By default, nothing changes:

```rust
// No changes to Cargo.toml, kernel is sync
fn process_table(path: &str) -> Result<Data> {
    let snapshot = Snapshot::build(&engine)?;  // Sync
    // ... Iterator-based processing
}
```

**Opt-in async**: Enable feature for async APIs:

```toml
[dependencies]
delta-kernel = { version = "0.x", features = ["async"] }
```

```rust
#[tokio::main]
async fn main() {
    let data = process_table(path).await?;
}

async fn process_table(path: &str) -> Result<Data> {
    let snapshot = Snapshot::build(&engine).await?;  // Async
    // ... Stream-based processing
}
```

---

## The Multi-threaded Example: A Case Study

The `read-table-multi-threaded` example shows the fundamental difference between sync and async:

### Today (Sync + OS Threads)

```rust
thread::scope(|s| {
    (0..thread_count).for_each(|_| {
        s.spawn(|| {
            // Blocking I/O on this thread
            while let Ok(file) = scan_file_rx.recv() {
                let data = engine.read_parquet_files(&[file])?;  // Blocks
                // process data
            }
        });
    });
    // Main thread distributes work
    for metadata in scan.scan_metadata(&engine)? {
        // ...
    }
});
```

### With Async (Tokio Tasks)

```rust
#[tokio::main]
async fn main() {
    let tasks: Vec<_> = (0..thread_count)
        .map(|_| {
            tokio::spawn(async move {
                while let Some(file) = scan_file_rx.recv().await {
                    let data = engine.read_parquet_files(&[file]).await?;  // Async
                    // process data
                }
            })
        })
        .collect();
    
    // Main task distributes work
    let mut metadata_stream = scan.scan_metadata(&engine).await?;
    while let Some(metadata) = metadata_stream.next().await {
        // ...
    }
    
    for task in tasks {
        task.await?;
    }
}
```

**Key difference**: OS threads vs async tasks. Fundamentally different concurrency models.

---

## What the Macro Approach Does NOT Solve

### 1. Engine Implementation Still Needs Duplication

At the I/O boundary, you still need:

```rust
#[cfg(not(feature = "async"))]
fn read_impl(&self, files: &[FileMeta]) -> DeltaResult<impl AsyncIterator<...>> {
    // Block on async I/O
    self.executor.block_on(async {
        // ... async operations ...
    })
}

#[cfg(feature = "async")]
async fn read_impl(&self, files: &[FileMeta]) -> DeltaResult<impl AsyncIterator<...>> {
    // Direct async I/O
    // ... async operations ...
}
```

**Why?** Because sync I/O and async I/O are **fundamentally different operations**:
- Sync: Blocks thread until complete
- Async: Yields control, allows other work to proceed

### 2. Runtime Management

Async mode requires a tokio runtime. Questions:
- Who creates it?
- When is it created?
- How is it shared?
- What about multiple engines in one process?

Today's `TokioBackgroundExecutor` solves this by spawning a background thread with its own runtime. But this means we're **blocking threads to bridge async to sync**.

In true async mode, the runtime management becomes the application's responsibility.

### 3. Testing Complexity

Must test both modes:

```yaml
test-sync:
  run: cargo test

test-async:
  run: cargo test --features async
```

Double the CI time, double the maintenance burden.

---

## Real-World Migration Examples

### Case 1: DuckDB FFI Consumer (No Changes)

```c
// C/C++ code using FFI - works identically
SharedExternEngine engine = get_default_engine(path, allocate_error);
SharedSnapshot snapshot = snapshot(path, engine);
SharedScanMetadataIterator iter = scan_metadata_iter_init(engine, scan);

// Iterate synchronously (as C expects)
while (scan_metadata_next(iter, context, visitor)) {
    // process
}
```

**Impact**: None. FFI stays sync.

### Case 2: Simple CLI Tool (Optional Migration)

**Option A: Stay Sync** (zero changes):
```rust
fn main() {
    // Everything stays the same
}
```

**Option B: Go Async**:
```rust
#[tokio::main]
async fn main() {
    // Add async/await
}
```

Developer's choice based on needs.

### Case 3: Web Server with Actix-web (Natural Fit)

```rust
// Before: awkward sync call in async context
async fn get_table_data(path: String) -> Result<Json<TableData>> {
    // This blocks a tokio thread! Bad!
    let snapshot = Snapshot::build(&engine)?;
    // ...
}

// After: natural async flow
async fn get_table_data(path: String) -> Result<Json<TableData>> {
    // This yields properly! Good!
    let snapshot = Snapshot::build(&engine).await?;
    // ...
}
```

---

## Performance Implications

### Sync Mode
- Same as today
- No overhead
- Well-understood performance characteristics

### Async Mode
- **Better**: More efficient I/O concurrency
- **Better**: Can handle more concurrent operations with fewer threads
- **Worse**: Async state machine overhead (small)
- **Worse**: Runtime scheduler overhead (small)

**Net result**: For I/O-bound workloads (which Delta is), async should be **faster**.

**Caveat**: Requires proper async runtime configuration.

---

## Decision Matrix for Consumers

### Choose Sync Mode If:
- ✅ You have a simple CLI tool
- ✅ You're using FFI from C/C++
- ✅ You want zero dependencies on async runtimes
- ✅ Your application is already blocking/synchronous
- ✅ You prioritize simplicity over performance

### Choose Async Mode If:
- ✅ Your application is already async (web server, etc.)
- ✅ You need maximum I/O concurrency
- ✅ You're willing to manage a tokio runtime
- ✅ You want cutting-edge performance
- ✅ You're okay with more complex code

---

## Migration Timeline Recommendation

### Phase 1: Infrastructure (Week 1-2)
- Add `#[async_fn]` macro
- Add `AsyncIterator` trait
- Keep feature **disabled by default**
- **No consumer impact**

### Phase 2: Internal Conversion (Week 3-4)
- Convert kernel internals to use macros
- Test both modes in CI
- **Still no consumer impact** (default is sync)

### Phase 3: Documentation (Week 5)
- Write migration guides
- Update examples to show both modes
- Create decision tree for consumers

### Phase 4: Opt-in Adoption (Ongoing)
- Consumers migrate at their own pace
- FFI stays sync forever
- Examples can demonstrate both approaches

---

## Open Questions

1. **Should examples show sync or async?**
   - Option A: Keep sync (simpler for newcomers)
   - Option B: Show both (more complete)
   - Option C: Default sync, have `examples-async/` directory

2. **How do we test FFI with async kernel?**
   - Probably: Don't. FFI always uses sync kernel.

3. **What about `no_std`?**
   - Sync mode: Should work
   - Async mode: Requires `std` + `tokio`

4. **Performance benchmarks?**
   - Need to quantify async benefits
   - Help users make informed choice

---

## Conclusion

### ✅ Good News
1. Backward compatible (sync by default)
2. FFI unaffected (stays sync)
3. Consumers can opt-in to async
4. No breaking changes to existing code

### ⚠️ Challenges
1. Dual-mode maintenance burden
2. Documentation complexity
3. Runtime management in async mode
4. Engine layer still needs conditional code

### 📊 Overall Assessment

The async macro approach is **viable and valuable**, but not a magic bullet. It provides:
- **Real benefits** for async-native applications
- **Zero disruption** for sync consumers (default)
- **Incremental adoption** path

The main cost is **maintenance complexity** of supporting both modes long-term.

**Recommendation**: **Proceed with prototype**, with eyes open about:
- Long-term dual-mode maintenance
- Need for comprehensive documentation
- Engine layer limitations
- Testing burden

The async ecosystem would benefit significantly, and the backward compatibility story is solid.

---

## Further Reading

- Full analysis: `async-consumer-impact-analysis.md`
- Macro approach details: `async-macro-approach.md`
- Control flow analysis: `CONTROL-FLOW-SUMMARY.md`

