# Async Macro Approach: Consumer Impact Summary

**Date**: October 20, 2025

> **For the complete technical proposal**, see [async-macro-approach.md](async-macro-approach.md). This document provides an executive summary focused on consumer impact.

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

### 1. DefaultEngine Is Already Async Internally

**Discovery**: DefaultEngine is **already async internally**! The kernel already uses async I/O under the hood and exposes a sync API by blocking on futures.

**The Implementation Pattern**: Engine handlers use a unified pattern:
- **One native async impl method** - contains all the real logic
- **One trait wrapper** - uses standard macros (`#[async_fn]` + `await_!`)
- **One helper function** (`into_boxed_async_iterator`) - handles mode-specific conversion

This achieves **zero logic duplication** and **single trait wrapper** per handler method.

**For complete technical details**, see [async-macro-approach.md § Current State](async-macro-approach.md#current-state-the-hidden-async-architecture) and [§ Component 5](async-macro-approach.md#component-5-the-io-boundary-helper).

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

### Case 3: Async Rust Consumer (e.g., delta-rs)

Projects like `delta-rs` are natively async but currently have to bridge sync `delta-kernel-rs` calls awkwardly:

```rust
// Before: awkward sync call in async context
async fn read_delta_table(path: &str) -> Result<impl Stream<Item = Result<RecordBatch>>> {
    // This blocks a tokio thread! Bad!
    // delta-kernel-rs APIs are sync, forcing blocking in async context
    let snapshot = Snapshot::build(&engine)?;
    let scan = snapshot.scan_builder().build()?;
    
    // Iterator must be manually converted to Stream
    let iter = scan.execute(engine)?;
    Ok(futures::stream::iter(iter))  // Awkward wrapper
}

// After: natural async flow
async fn read_delta_table(path: &str) -> Result<impl Stream<Item = Result<RecordBatch>>> {
    // This yields properly! Good!
    // delta-kernel-rs APIs are now async-compatible
    let snapshot = Snapshot::build(&engine).await?;
    let scan = snapshot.scan_builder().build()?;
    
    // Returns Stream directly - natural async API
    scan.execute(engine).await
}
```

**Impact**: Async ecosystem projects can integrate naturally without blocking threads.

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

