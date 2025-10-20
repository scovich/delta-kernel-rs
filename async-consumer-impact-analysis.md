# Async Macro Approach: Consumer Impact Analysis

**Date**: October 20, 2025  
**Purpose**: Analyze how the async macro approach would affect all consumers of delta-kernel-rs

---

## Executive Summary

The async macro approach proposes making kernel APIs conditionally async using feature flags. This analysis examines the real-world impact on all major consumers:

**Key Finding**: The async approach would **fundamentally change** how consumers interact with the kernel, requiring:
1. **Example programs** to adopt `#[tokio::main]` or equivalent async runtime setup
2. **FFI layer** to remain sync-only (blocking on async operations internally)
3. **Tests** to use `#[tokio::test]` instead of plain `#[test]`
4. **Library consumers** to either provide async runtime or use sync mode

**Verdict**: The async approach is **viable but disruptive**, with significant migration complexity for existing consumers.

---

## Consumer Categories

### 1. Example Programs (Rust CLI Tools)

**Current State**: All examples are synchronous programs with simple `fn main()`.

**Examples analyzed**:
- `read-table-single-threaded` - Simple single-threaded table reader
- `read-table-multi-threaded` - Multi-threaded reader using `std::thread::scope`
- `write-table` - Table writing example
- `inspect-table` - Table inspection tool

#### Current Pattern

```rust
fn main() -> ExitCode {
    env_logger::init();
    match try_main() {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            println!("{e:#?}");
            ExitCode::FAILURE
        }
    }
}

fn try_main() -> DeltaResult<()> {
    let cli = Cli::parse();
    let url = delta_kernel::try_parse_uri(&cli.path)?;
    let engine = common::get_engine(&url)?;
    let snapshot = Snapshot::builder_for(url).build(&engine)?;  // Sync!
    let scan = snapshot.scan_builder().build()?;
    
    for result in scan.execute(Arc::new(engine))? {  // Returns Iterator
        // Process result...
    }
    Ok(())
}
```

#### With Async Feature Enabled

```rust
// Option 1: Use tokio::main
#[tokio::main]
async fn main() -> ExitCode {
    env_logger::init();
    match try_main().await {  // Now async!
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            println!("{e:#?}");
            ExitCode::FAILURE
        }
    }
}

async fn try_main() -> DeltaResult<()> {
    let cli = Cli::parse();
    let url = delta_kernel::try_parse_uri(&cli.path)?;
    let engine = common::get_engine(&url)?;
    let snapshot = Snapshot::builder_for(url).build(&engine).await?;  // Async!
    let scan = snapshot.scan_builder().build()?;
    
    // Returns Stream, needs await
    let mut stream = scan.execute(Arc::new(engine)).await?;
    while let Some(result) = stream.next().await {  // Stream iteration
        // Process result...
    }
    Ok(())
}
```

#### Multi-threaded Example Impact

The `read-table-multi-threaded` example is particularly interesting:

**Current**: Uses `std::thread::scope` to spawn OS threads that do blocking I/O:

```rust
thread::scope(|s| {
    (0..cli.thread_count).for_each(|_| {
        s.spawn(|| {
            do_work(&engine, scan_state, rb_tx, scan_file_rx);
        });
    });
    
    for res in scan_metadata {  // Iterator
        let scan_metadata = res?;
        // ... process
    }
    Ok(())
})
```

**With async**: Would need to choose between:

1. **Keep threading model, block on async internally**:
```rust
thread::scope(|s| {
    (0..cli.thread_count).for_each(|_| {
        s.spawn(|| {
            // Create a runtime per thread? Or share one?
            let rt = tokio::runtime::Runtime::new().unwrap();
            rt.block_on(async {
                do_work_async(&engine, scan_state, rb_tx, scan_file_rx).await;
            });
        });
    });
    // ...
})
```

2. **Switch to tokio tasks** (more idiomatic):
```rust
#[tokio::main]
async fn main() {
    let tasks: Vec<_> = (0..cli.thread_count)
        .map(|_| {
            tokio::spawn(async move {
                do_work_async(&engine, scan_state, rb_tx, scan_file_rx).await
            })
        })
        .collect();
    
    for task in tasks {
        task.await.unwrap();
    }
}
```

**Analysis**: The multi-threaded example shows that async mode fundamentally changes parallelism patterns. OS threads + blocking is replaced with async tasks.

---

### 2. FFI Layer (C/C++ Interop)

**Current State**: FFI provides a synchronous C API that blocks on async operations internally.

**Key insight**: The FFI layer **already handles async-to-sync bridging** today!

#### Current FFI Architecture

```rust
// ffi/src/scan.rs

// C-callable function - always synchronous
#[no_mangle]
pub unsafe extern "C" fn scan_metadata_iter_init(
    engine: Handle<SharedExternEngine>,
    scan: Handle<SharedScan>,
) -> ExternResult<Handle<SharedScanMetadataIterator>> {
    let engine = unsafe { engine.clone_as_arc() };
    let scan = unsafe { scan.as_ref() };
    scan_metadata_iter_init_impl(&engine, scan).into_extern_result(&engine.as_ref())
}

fn scan_metadata_iter_init_impl(
    engine: &Arc<dyn ExternEngine>,
    scan: &Scan,
) -> DeltaResult<Handle<SharedScanMetadataIterator>> {
    // This call is currently sync, but kernel internally uses TaskExecutor
    let scan_metadata = scan.scan_metadata(engine.engine().as_ref())?;
    let data = ScanMetadataIterator {
        data: Mutex<Box<dyn Iterator<Item = DeltaResult<ScanMetadata>> + Send>>,
        engine: engine.clone(),
    };
    Ok(Arc::new(data).into())
}
```

#### How It Works Today

The kernel is **already conceptually async** under the hood:
1. DefaultEngine uses `TokioBackgroundExecutor`
2. All I/O operations spawn async tasks
3. `TaskExecutor::block_on()` bridges async to sync:

```rust
// kernel/src/engine/default/executor.rs

impl TaskExecutor for TokioBackgroundExecutor {
    fn block_on<T>(&self, task: T) -> T::Output
    where
        T: Future + Send + 'static,
        T::Output: Send + 'static,
    {
        // We cannot call `tokio::runtime::Runtime::block_on` here because
        // it panics if called within an async context. So instead we spawn
        // the future on the runtime and send the result back using a channel.
        let (sender, receiver) = channel::<T::Output>();
        
        let fut = Box::pin(async move {
            let task_output = task.await;
            tokio::task::spawn_blocking(move || {
                sender.send(task_output).ok();
            })
            .await
            .unwrap();
        });
        
        self.send_future(fut);
        
        receiver
            .recv()
            .expect("TokioBackgroundExecutor has crashed")
    }
}
```

**Key point**: The kernel already does async I/O internally, and provides a sync interface by blocking on futures.

#### With Async Macro Approach

The FFI layer has **two options**:

**Option 1: Keep FFI Sync, Build with `async` Feature Disabled**

The FFI crate would always compile in sync mode:

```toml
# ffi/Cargo.toml
[dependencies]
delta_kernel = { path = "../kernel" }  # No async feature!
```

**Pros**:
- No FFI changes needed
- C/C++ consumers remain unaffected
- Maintains stable ABI

**Cons**:
- FFI users don't benefit from async (but they don't today either)
- Two kernel builds needed (sync for FFI, async for Rust users)

**Option 2: FFI Wrapper Does Blocking**

FFI crate depends on async kernel but blocks internally:

```rust
// With async feature enabled in kernel
#[no_mangle]
pub unsafe extern "C" fn scan_metadata_iter_init(
    engine: Handle<SharedExternEngine>,
    scan: Handle<SharedScan>,
) -> ExternResult<Handle<SharedScanMetadataIterator>> {
    let engine = unsafe { engine.clone_as_arc() };
    let scan = unsafe { scan.as_ref() };
    
    // Block on the async kernel call
    let runtime = get_or_create_runtime();  // Thread-local or global?
    let result = runtime.block_on(async {
        scan_metadata_iter_init_impl(&engine, scan).await
    });
    
    result.into_extern_result(&engine.as_ref())
}
```

**Pros**:
- Single kernel build
- FFI can use async kernel

**Cons**:
- FFI needs to manage tokio runtime(s)
- Complexity of runtime lifecycle management
- Performance implications of blocking

**Recommendation**: **Option 1** (FFI uses sync mode) is cleaner and maintains existing architecture.

---

### 3. Test Suite

**Current State**: Tests use `#[tokio::test]` but kernel APIs are synchronous.

#### Current Test Pattern

```rust
#[tokio::test]
async fn golden_basic_decimal_table() -> Result<(), Box<dyn std::error::Error>> {
    let (engine, table, expected, _test_dir) = setup_golden_table("basic-decimal-table");
    latest_snapshot_test(engine, table, expected).await?;  // async wrapper
    Ok(())
}

async fn latest_snapshot_test(
    engine: DefaultEngine<TokioBackgroundExecutor>,
    url: Url,
    expected_path: Option<PathBuf>,
) -> Result<(), Box<dyn std::error::Error>> {
    // Kernel calls are sync!
    let snapshot = Snapshot::builder_for(url).build(&engine)?;
    let scan = snapshot.scan_builder().build()?;
    let scan_res = scan.execute(Arc::new(engine))?;  // Returns Iterator
    
    let batches: Vec<RecordBatch> = scan_res
        .map(|scan_result| -> DeltaResult<_> {
            // Process...
        })
        .try_collect()?;
    
    // Reading expected data IS async
    let expected = read_expected(&expected_path.expect("expect an expected dir")).await?;
    // ...
}
```

**Why `#[tokio::test]` today?**: The test setup code (like `read_expected()`) uses async parquet reading, but kernel APIs are sync.

#### With Async Feature Enabled

```rust
#[tokio::test]
async fn golden_basic_decimal_table() -> Result<(), Box<dyn std::error::Error>> {
    let (engine, table, expected, _test_dir) = setup_golden_table("basic-decimal-table");
    latest_snapshot_test(engine, table, expected).await?;
    Ok(())
}

async fn latest_snapshot_test(
    engine: DefaultEngine<TokioBackgroundExecutor>,
    url: Url,
    expected_path: Option<PathBuf>,
) -> Result<(), Box<dyn std::error::Error>> {
    // Kernel calls are NOW async!
    let snapshot = Snapshot::builder_for(url).build(&engine).await?;
    let scan = snapshot.scan_builder().build()?;
    let scan_res = scan.execute(Arc::new(engine)).await?;  // Returns Stream
    
    let batches: Vec<RecordBatch> = scan_res
        .then(|scan_result| async move {  // Stream combinator
            // Process...
        })
        .try_collect()
        .await?;  // Collect is async!
    
    let expected = read_expected(&expected_path.expect("expect an expected dir")).await?;
    // ...
}
```

**Impact**: Tests are already async, so adaptation is minimal. Main changes:
- Add `.await` to kernel calls
- Change `Iterator` operations to `Stream` operations
- Add `.await` to collection operations

**CI Impact**: Tests need to run twice:
```yaml
test-sync:
  run: cargo test

test-async:
  run: cargo test --features async
```

---

### 4. Library Consumers (Other Rust Crates)

**Current State**: Consumers use kernel as a regular synchronous library.

#### Current Consumer Pattern

```rust
// In some application that uses delta-kernel
fn process_delta_table(path: &str) -> Result<Vec<Data>> {
    let url = delta_kernel::try_parse_uri(path)?;
    let engine = delta_kernel::engine::default::DefaultEngine::try_new(
        &url,
        HashMap::new(),
        Arc::new(TokioBackgroundExecutor::new()),
    )?;
    
    let snapshot = Snapshot::builder_for(url).build(&engine)?;
    let scan = snapshot.scan_builder().build()?;
    
    let mut results = Vec::new();
    for item in scan.execute(Arc::new(engine))? {
        results.push(process(item?));
    }
    Ok(results)
}
```

This works from:
- Synchronous CLI applications
- Blocking web servers
- Data processing pipelines
- Anywhere that doesn't have an async runtime

#### With Async Feature

**Option 1**: Consumer enables async feature and goes async:

```rust
// Application is now async
#[tokio::main]
async fn main() {
    let results = process_delta_table(path).await.unwrap();
}

async fn process_delta_table(path: &str) -> Result<Vec<Data>> {
    let url = delta_kernel::try_parse_uri(path)?;
    let engine = delta_kernel::engine::default::DefaultEngine::try_new(
        &url,
        HashMap::new(),
        Arc::new(TokioBackgroundExecutor::new()),
    )?;
    
    let snapshot = Snapshot::builder_for(url).build(&engine).await?;
    let scan = snapshot.scan_builder().build()?;
    
    let mut results = Vec::new();
    let mut stream = scan.execute(Arc::new(engine)).await?;
    while let Some(item) = stream.next().await {
        results.push(process(item?));
    }
    Ok(results)
}
```

**Option 2**: Consumer doesn't enable async feature, uses sync mode:

```rust
// Same as today - no changes!
fn process_delta_table(path: &str) -> Result<Vec<Data>> {
    let url = delta_kernel::try_parse_uri(path)?;
    let engine = delta_kernel::engine::default::DefaultEngine::try_new(
        &url,
        HashMap::new(),
        Arc::new(TokioBackgroundExecutor::new()),
    )?;
    
    let snapshot = Snapshot::builder_for(url).build(&engine)?;
    let scan = snapshot.scan_builder().build()?;
    
    let mut results = Vec::new();
    for item in scan.execute(Arc::new(engine))? {
        results.push(process(item?));
    }
    Ok(results)
}
```

**Key insight**: Library consumers can choose! The feature flag approach means:
- **Backward compatible**: Existing code continues to work (sync mode)
- **Opt-in async**: Consumers that want async can enable the feature
- **No breaking changes**: Default behavior is sync

---

## Special Case: Unity Catalog Client

The `uc-client` crate is already fully async:

```rust
// uc-client/examples/uc-cli.rs

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    let client = UCClient::builder(&cli.workspace_url, &cli.token).build()?;
    
    match cli.command {
        Commands::Table { name } => {
            match client.get_table(&name).await {  // Already async!
                Ok(table) => println!("{}", table),
                Err(e) => eprintln!("Failed: {}", e),
            }
        }
        // ...
    }
    Ok(())
}
```

**Impact**: None! UC client is already async-native and would benefit from async kernel APIs.

---

## Critical Architecture Question: The DefaultEngine Paradox

### The Current Situation

Today's `DefaultEngine` is **already async internally** but exposes a **sync interface**:

```rust
// Today: DefaultEngine internals
pub struct DefaultEngine<E: TaskExecutor> {
    executor: Arc<E>,  // TokioBackgroundExecutor!
    // ...
}

impl JsonHandler for DefaultJsonHandler<E> {
    fn read_json_files(&self, /* ... */) -> DeltaResult<Box<dyn EngineData>> {
        // Async I/O happens here, but we block on it
        self.executor.block_on(async {
            // async parquet reading, object store I/O, etc.
        })
    }
}
```

The `TaskExecutor::block_on()` method bridges async to sync by:
1. Spawning async work on a tokio runtime (in a background thread)
2. Blocking the current thread until complete
3. Returning the result

### The Question

**If we make kernel APIs async, what happens to `DefaultEngine`?**

**Option A: DefaultEngine stays sync internally (feature-dependent)**

```rust
// With async feature DISABLED
impl JsonHandler for DefaultJsonHandler<E> {
    fn read_json_files(&self, /* ... */) -> DeltaResult<Box<dyn EngineData>> {
        self.executor.block_on(async {
            // async I/O
        })
    }
}

// With async feature ENABLED
impl JsonHandler for DefaultJsonHandler<E> {
    async fn read_json_files(&self, /* ... */) -> DeltaResult<Box<dyn EngineData>> {
        // Direct async I/O, no blocking!
        // ...
    }
}
```

**Pros**:
- True async in async mode (no blocking thread overhead)
- Sync mode still works

**Cons**:
- Duplication at the engine implementation layer (the proposed approach was meant to avoid this!)
- Engine implementors need to maintain two versions

**Option B: DefaultEngine ALWAYS blocks, even in async mode**

```rust
// Even with async feature enabled, engine still blocks
impl JsonHandler for DefaultJsonHandler<E> {
    async fn read_json_files(&self, /* ... */) -> DeltaResult<Box<dyn EngineData>> {
        // Block on async work, then return
        let result = self.executor.block_on(async {
            // async I/O
        });
        result
    }
}
```

**Pros**:
- No duplication in engine implementation
- Single code path

**Cons**:
- **Defeats the purpose of async!** We'd be blocking threads in async contexts
- Poor async performance (blocking threads in tokio runtime is bad)
- Async ecosystem would consider this an anti-pattern

**Option C: Two DefaultEngine implementations**

```rust
// Sync version
pub struct SyncDefaultEngine<E: TaskExecutor> { /* ... */ }

// Async version  
pub struct AsyncDefaultEngine { /* uses tokio directly */ }
```

**Pros**:
- Clean separation
- Each optimized for its context

**Cons**:
- Essentially what we have today (async internally, sync wrapper)
- More code to maintain

### The Real Question

**Does the async macro approach actually help DefaultEngine implementation?**

Looking at the code:
- The I/O layer (object_store, parquet readers) is **already async**
- DefaultEngine handlers **already** use async code internally
- The blocking happens at the engine trait boundary

**The macro approach helps kernel business logic**, but the **engine implementation** may still need platform-specific logic:

```rust
// This is at the I/O boundary - fundamentally different
#[cfg(not(feature = "async"))]
fn read_impl(&self, files: &[FileMeta]) -> DeltaResult<impl AsyncIterator<...>> {
    let data = self.executor.block_on(async {
        // async I/O but blocked
    });
    // return iterator
}

#[cfg(feature = "async")]
async fn read_impl(&self, files: &[FileMeta]) -> DeltaResult<impl AsyncIterator<...>> {
    // Direct async I/O
    let data = /* async read */;
    // return stream
}
```

**Conclusion**: The macro approach **does not eliminate** the need for conditional code at the engine I/O layer. This is inherent to the sync/async boundary.

---

## Migration Path Analysis

### Phase 1: Add Async Infrastructure (No Breaking Changes)

1. Add `#[async_fn]` macro and `AsyncIterator` trait
2. Keep `async` feature **disabled by default**
3. Existing code continues to work unchanged

### Phase 2: Convert Kernel Internals

1. Convert kernel methods to use `#[async_fn]`
2. Convert iterator returns to `impl AsyncIterator`
3. In sync mode: compiles to what we have today
4. In async mode: new async API emerges

### Phase 3: Test Both Modes

```yaml
# CI configuration
jobs:
  test-sync:
    name: Test Sync Mode
    runs-on: ubuntu-latest
    steps:
      - run: cargo test
  
  test-async:
    name: Test Async Mode
    runs-on: ubuntu-latest
    steps:
      - run: cargo test --features async
```

### Phase 4: Update Examples (Optional)

Examples can choose:
1. Stay sync (no changes)
2. Opt into async (add `#[tokio::main]`, `.await`)

### Phase 5: Document Migration

For library consumers:
```markdown
## Using delta-kernel-rs

### Synchronous Mode (Default)
```rust
fn main() {
    let engine = DefaultEngine::try_new(/* ... */)?;
    let snapshot = Snapshot::builder_for(url).build(&engine)?;
    // ...
}
```

### Asynchronous Mode
Enable the `async` feature in Cargo.toml:
```toml
[dependencies]
delta-kernel = { version = "0.x", features = ["async"] }
```

Then use async APIs:
```rust
#[tokio::main]
async fn main() {
    let engine = DefaultEngine::try_new(/* ... */)?;
    let snapshot = Snapshot::builder_for(url).build(&engine).await?;
    // ...
}
```

---

## Performance Implications

### Sync Mode

**No overhead**: Compiles to iterators, no boxing unless at API boundaries.

### Async Mode

**Potential benefits**:
- Better concurrency for I/O-bound operations
- Cooperative task scheduling
- Can saturate network/disk better

**Potential costs**:
- Async state machines (small)
- Runtime overhead (tokio scheduler)
- Boxing at API boundaries (if using trait objects)

**Net result**: For I/O-bound workloads (which Delta is), async should be **faster or comparable**.

---

## Risk Assessment

### Low Risk

1. **FFI Layer**: Keep using sync mode, no changes needed
2. **Backward Compatibility**: Default to sync mode, existing code works
3. **Incremental Adoption**: Consumers choose when to migrate

### Medium Risk

1. **Test Suite Maintenance**: Must test both modes, 2x CI time
2. **Documentation**: Need clear guidance for sync vs async choice
3. **Example Complexity**: Examples become less "simple" with async

### High Risk

1. **Engine Implementation Complexity**: Still need conditional code at I/O boundary
2. **Runtime Requirements**: Async mode requires tokio dependency
3. **Ecosystem Confusion**: "Do I need async or not?" is unclear

---

## Recommendations

### For the Kernel Team

1. **Proceed with prototype**, but be clear about limitations:
   - Macro approach **does not** eliminate engine-layer duplication
   - Async mode requires runtime management
   - Two testing modes required

2. **Default to sync mode** for:
   - FFI layer (always)
   - Examples (initially)
   - Conservative users

3. **Offer async mode** for:
   - Performance-critical applications
   - Async ecosystems (web servers, etc.)
   - Users who want cutting edge

### For Consumers

**Choose Sync Mode if**:
- You have a synchronous application
- You want simplicity and stability
- You don't want runtime dependencies
- You're using FFI

**Choose Async Mode if**:
- You're already in an async ecosystem
- You need maximum I/O concurrency
- You're willing to manage tokio runtime
- You want to contribute to async development

---

## Example Consumer Adaptations

### Example 1: Simple CLI (Stays Sync)

```rust
// No changes needed!
fn main() {
    let engine = get_engine();
    let snapshot = Snapshot::build(&engine)?;
    // ...
}
```

### Example 2: Web Server (Goes Async)

```rust
// Was sync:
fn get_table_data(path: &str) -> Result<Vec<Row>> {
    let snapshot = Snapshot::build(&engine)?;
    // ...
}

// Becomes async:
async fn get_table_data(path: &str) -> Result<Vec<Row>> {
    let snapshot = Snapshot::build(&engine).await?;
    // ...
}
```

### Example 3: FFI Consumer (No Change)

```c
// C code stays exactly the same
ExternResult snapshot_result = snapshot(path, engine);
if (snapshot_result.error) {
    // handle error
}
SharedSnapshot snapshot = snapshot_result.result;
```

---

## Unanswered Questions

1. **How do we help users choose?**
   - Need clear decision tree
   - Benchmarks showing when async helps

2. **What about mixed mode?**
   - Can one application use both sync and async?
   - Probably not easily

3. **What about no_std?**
   - Sync mode should work without std
   - Async mode requires std + tokio

4. **Migration tooling?**
   - Can we provide migration scripts?
   - Automated refactoring?

---

## Conclusion

The async macro approach is **technically viable** but has **significant ecosystem implications**:

### ✅ Works Well For
- Rust library consumers (opt-in choice)
- Applications already using tokio
- Performance-critical async workloads

### ⚠️ Challenges For
- Example code (becomes more complex)
- FFI consumers (stick with sync)
- Mixed sync/async scenarios

### ❌ Does Not Solve
- Engine implementation duplication (I/O boundary still needs `#[cfg]`)
- Runtime lifecycle management
- Documentation/education burden

**Overall Assessment**: The approach provides **real value** for async consumers while maintaining backward compatibility. The main cost is **complexity**: maintaining two modes, testing both, and educating users on the choice.

**Recommendation**: Proceed with prototype, but plan for:
1. Comprehensive documentation
2. Clear migration guides
3. Example updates
4. Long-term maintenance of dual modes

The async ecosystem would benefit, but it's not a silver bullet for code duplication.

