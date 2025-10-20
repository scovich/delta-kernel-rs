# Async Macro Approach: Architecture Diagrams

**Date**: October 20, 2025

---

## Current Architecture (Sync API with Async Internals)

```
┌─────────────────────────────────────────────────────────────────────┐
│                         CONSUMERS                                   │
│                                                                     │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐               │
│  │   Examples   │  │  FFI Layer   │  │   Library    │               │
│  │  (Rust CLI)  │  │  (C/C++)     │  │  Consumers   │               │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘               │
│         │                 │                  │                      │
│         │ Sync calls      │ Sync calls       │ Sync calls           │
│         │ (no async/await)│ (C API)          │ (no async/await)     │
└─────────┼─────────────────┼──────────────────┼──────────────────────┘
          │                 │                  │
          ▼                 ▼                  ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    DELTA KERNEL (PUBLIC API)                        │
│                                                                     │
│  ┌────────────────────────────────────────────────────────────┐     │
│  │  Sync Methods                                              │     │
│  │  fn build(&self, engine: &dyn Engine) -> Result<Snapshot>  │     │
│  │  fn execute(&self) -> Result<Iterator<Item = ScanResult>>  │     │
│  └────────────────────────────────────────────────────────────┘     │
│                                                                     │
│                            ▼                                        │
│                                                                     │
│  ┌────────────────────────────────────────────────────────────┐     │
│  │  Engine Trait                                              │     │
│  │  fn parquet_handler(&self) -> Arc<dyn ParquetHandler>      │     │
│  │  fn json_handler(&self) -> Arc<dyn JsonHandler>            │     │
│  └────────────────────────────────────────────────────────────┘     │
└───────────────────────────────┬─────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    DEFAULT ENGINE IMPLEMENTATION                    │
│                                                                     │
│  ┌────────────────────────────────────────────────────────────┐     │
│  │  Sync Wrapper Methods                                      │     │
│  │  fn read_json_files(...) -> Result<Box<dyn EngineData>> {  │     │
│  │      self.executor.block_on(async {                        │     │
│  │          // Async I/O here! ────────────────────┐          │     │
│  │      })                                         │          │     │
│  │  }                                              │          │     │
│  └─────────────────────────────────────────────────┼──────────┘     │
│                                                    │                │
│  ┌─────────────────────────────────────────────────▼──────────┐     │
│  │  TokioBackgroundExecutor                                   │     │
│  │  - Spawns async tasks on background tokio runtime          │     │
│  │  - Blocks calling thread until task completes              │     │
│  │  - Bridges sync API to async I/O                           │     │
│  └────────────────────────────────────────────────────────────┘     │
│                                                                     │
│                            ▼                                        │
│                                                                     │
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │  Async I/O Layer                                            │    │
│  │  - object_store (async)                                     │    │
│  │  - parquet reader (async)                                   │    │
│  │  - tokio::fs (async)                                        │    │
│  └─────────────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────────────┘
```

**Key characteristics**:
- ✅ Consumers see simple sync API
- ✅ Engine does async I/O internally for efficiency
- ⚠️ Thread blocking overhead at sync/async boundary
- ⚠️ Cannot efficiently compose with other async code

---

## Proposed Architecture (Dual Mode)

### Mode 1: Sync Mode (Default, Backward Compatible)

```
┌─────────────────────────────────────────────────────────────────────┐
│                    CONSUMERS (Sync Mode)                            │
│                                                                     │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐               │
│  │   Examples   │  │  FFI Layer   │  │   Library    │               │
│  │  (Rust CLI)  │  │  (C/C++)     │  │  Consumers   │               │
│  │   fn main()  │  │              │  │  fn process()│               │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘               │
│         │                 │                  │                      │
│         │ Sync calls      │ Sync calls       │ Sync calls           │
└─────────┼─────────────────┼──────────────────┼──────────────────────┘
          │                 │                  │
          ▼                 ▼                  ▼
┌─────────────────────────────────────────────────────────────────────┐
│               DELTA KERNEL (Sync Mode - feature OFF)                │
│                                                                     │
│  ┌────────────────────────────────────────────────────────────┐     │
│  │  #[async_fn] compiles to regular fn                        │     │
│  │  fn build(&self, engine: &dyn Engine) -> Result<Snapshot>  │     │
│  │                                                            │     │
│  │  impl AsyncIterator returns Iterator                       │     │
│  │  fn execute() -> Result<impl Iterator<Item = ScanResult>>  │     │
│  └────────────────────────────────────────────────────────────┘     │
└───────────────────────────────┬─────────────────────────────────────┘
                                │
                                ▼
                    [Same as current architecture]
```

**Key characteristics**:
- ✅ Identical to current system
- ✅ No breaking changes
- ✅ Works for all existing consumers

---

### Mode 2: Async Mode (Opt-in with Feature Flag)

```
┌─────────────────────────────────────────────────────────────────────┐
│                    CONSUMERS (Async Mode)                           │
│                                                                     │
│  ┌──────────────────────────┐        ┌─────────────────────┐        │
│  │   Examples (Async)       │        │  Library Consumers  │        │
│  │   #[tokio::main]         │        │  #[tokio::main]     │        │
│  │   async fn main()        │        │  async fn process() │        │
│  └──────┬───────────────────┘        └──────┬──────────────┘        │
│         │                                   │                       │
│         │ async/await calls                 │ async/await calls     │
└─────────┼───────────────────────────────────┼───────────────────────┘
          │                                   │
          ▼                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│              DELTA KERNEL (Async Mode - feature ON)                 │
│                                                                     │
│  ┌────────────────────────────────────────────────────────────┐     │
│  │  #[async_fn] compiles to async fn                          │     │
│  │  async fn build(&self, engine: &dyn Engine)                │     │
│  │               -> Result<Snapshot>                          │     │
│  │                                                            │     │
│  │  impl AsyncIterator returns Stream                         │     │
│  │  async fn execute() -> Result<impl Stream<Item = Result>>  │     │
│  └────────────────────────────────────────────────────────────┘     │
│                                                                     │
│                            ▼                                        │
│                                                                     │
│  ┌────────────────────────────────────────────────────────────┐     │
│  │  Engine Trait (with async methods)                         │     │
│  │  async fn parquet_handler(&self)                           │     │
│  │  async fn json_handler(&self)                              │     │
│  └────────────────────────────────────────────────────────────┘     │
└───────────────────────────────┬─────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    DEFAULT ENGINE (Async Mode)                      │
│                                                                     │
│  ┌────────────────────────────────────────────────────────────┐     │
│  │  Direct Async Methods (No blocking!)                       │     │
│  │  async fn read_json_files(...)                             │     │
│  │          -> Result<Box<dyn EngineData>> {                  │     │
│  │      // Direct async I/O ────────────────┐                 │     │
│  │      let data = object_store.get(path)   │                 │     │
│  │                            .await?;      │                 │     │
│  │      // ...                              │                 │     │
│  │  }                                       │                 │     │
│  └──────────────────────────────────────────┼─────────────────┘     │
│                                             │                       │
│                              ┌──────────────▼───────────────┐       │
│                              │  NO TokioBackgroundExecutor  │       │
│                              │  NO thread blocking!         │       │
│                              │  Pure async runtime          │       │
│                              └──────────────────────────────┘       │
│                                                                     │
│                            ▼                                        │
│                                                                     │
│  ┌────────────────────────────────────────────────────────────┐     │
│  │  Async I/O Layer                                           │     │
│  │  - object_store (async) ←────────────────────┐             │     │
│  │  - parquet reader (async) ←──┐               │             │     │
│  │  - tokio::fs (async) ←───────┼───────────────┘             │     │
│  │                              │                             │     │
│  │  All composed efficiently!   │                             │     │
│  └──────────────────────────────┼─────────────────────────────┘     │
│                                 │                                   │
│                                 └─ Managed by application's         │
│                                    tokio runtime                    │
└─────────────────────────────────────────────────────────────────────┘
```

**Key characteristics**:
- ✅ True async composition (no thread blocking)
- ✅ Better performance for I/O-bound workloads
- ✅ Natural integration with async ecosystem
- ⚠️ Requires tokio runtime management
- ⚠️ More complex API for consumers

---

## FFI Layer Strategy

### Option: FFI Always Uses Sync Kernel

```
┌─────────────────────────────────────────────────────────────────────┐
│                    C/C++ APPLICATION                                │
│                                                                     │
│  void process_table(const char* path) {                             │
│      SharedExternEngine engine = get_default_engine(path);          │
│      SharedSnapshot snapshot = snapshot(path, engine);              │
│      // ... synchronous C API calls ...                             │
│  }                                                                  │
└─────────────────────────┬───────────────────────────────────────────┘
                          │
                          │ C ABI
                          ▼
┌─────────────────────────────────────────────────────────────────────┐
│                     FFI LAYER (ffi crate)                           │
│                                                                     │
│  Cargo.toml:                                                        │
│  [dependencies]                                                     │
│  delta_kernel = { path = "../kernel" }  ← NO async feature          │
│                                                                     │
│  pub extern "C" fn snapshot(                                        │
│      path: KernelStringSlice,                                       │
│      engine: Handle<SharedExternEngine>                             │
│  ) -> ExternResult<Handle<SharedSnapshot>> {                        │
│      let snapshot = Snapshot::builder_for(url)                      │
│                              .build(&engine)?;  ← Sync!             │
│      // ...                                                         │
│  }                                                                  │
└─────────────────────────┬───────────────────────────────────────────┘
                          │
                          │ Sync kernel API
                          ▼
┌─────────────────────────────────────────────────────────────────────┐
│              DELTA KERNEL (Sync Mode Only)                          │
│                                                                     │
│  [Same as sync mode diagram above]                                  │
└─────────────────────────────────────────────────────────────────────┘
```

**Key characteristics**:
- ✅ No changes to FFI implementation
- ✅ Stable C ABI
- ✅ C/C++ consumers unaffected
- ✅ Simple mental model

---

## Example Program Evolution

### Single-Threaded Example

#### Before (Current - Sync)
```
┌─────────────────────────────────────────────────────────────────────┐
│        fn main() -> ExitCode                                        │
│                                                                     │
│  ┌───────────────────────────────────────────┐                      │
│  │ let engine = get_engine()?                │                      │
│  │ let snapshot = Snapshot::build (&engine)? │                      │
│  │                                           │                      │
│  │ for result in scan.execute()?             │                      │
│  │     .filter(|r| r.is_valid())             │                      │
│  │     .map(|r| process(r)) {                │                      │
│  │     println!("{:?}", result);             │                      │
│  │ }                                         │                      │
│  └───────────────────────────────────────────┘                      │
│                                                                     │
│  Single thread, blocking I/O                                        │
└─────────────────────────────────────────────────────────────────────┘
```

#### After Option 1 (Stay Sync - No Changes)
```
┌─────────────────────────────────────────────────────────────────────┐
│        fn main() -> ExitCode                                        │
│                                                                     │
│  [Identical to before]                                              │
│                                                                     │
│  Single thread, blocking I/O                                        │
└─────────────────────────────────────────────────────────────────────┘
```

#### After Option 2 (Go Async)
```
┌─────────────────────────────────────────────────────────────────────┐
│   #[tokio::main]                                                    │
│   async fn main() -> ExitCode                                       │
│                                                                     │
│  ┌─────────────────────────────────────────────────┐                │
│  │ let engine = get_engine()?                      │                │
│  │ let snapshot = Snapshot::build(&engine).await?  │                │
│  │                                                 │                │
│  │ let mut stream = scan                           │                │
│  │     .execute().await?                           │                │
│  │     .filter(|r| async { r.is_valid() })         │                │
│  │     .then(|r| async { process(r) });            │                │
│  │                                                 │                │
│  │ while let Some(result) = stream.next().await {  │                │
│  │     println!("{:?}", result);                   │                │
│  │ }                                               │                │
│  └─────────────────────────────────────────────────┘                │
│                                                                     │
│  Tokio runtime, async I/O                                           │
└─────────────────────────────────────────────────────────────────────┘
```

### Multi-Threaded Example

#### Before (Current - OS Threads)
```
┌─────────────────────────────────────────────────────────────────────┐
│        fn main()                                                    │
│                                                                     │
│  ┌─────────────────────────────────────────────────┐                │
│  │ let (tx, rx) = mpsc::channel();                 │                │
│  │                                                 │                │
│  │ thread::scope(|s| {                             │                │
│  │     for _ in 0..NUM_THREADS {                   │                │
│  │         let rx = rx.clone();                    │                │
│  │         s.spawn(move || {                       │                │
│  │             // Worker thread                    │                │
│  │             while let Ok(file) = rx.recv() {    │                │
│  │                 let data = engine               │                │
│  │                     .read_parquet(&file)?;      │                │
│  │                 // Process on this thread       │                │
│  │             }                                   │                │
│  │         });                                     │                │
│  │     }                                           │                │
│  │                                                 │                │
│  │     // Main thread distributes work             │                │
│  │     for file in scan.files()? {                 │                │
│  │         tx.send(file)?;                         │                │
│  │     }                                           │                │
│  │ });                                             │                │
│  └─────────────────────────────────────────────────┘                │
│                                                                     │
│  OS threads (1:1 with cores)                                        │
│  Blocking I/O on each thread                                        │
└─────────────────────────────────────────────────────────────────────┘
```

#### After (Async - Tokio Tasks)
```
┌─────────────────────────────────────────────────────────────────────┐
│   #[tokio::main]                                                    │
│   async fn main()                                                   │
│                                                                     │
│  ┌────────────────────────────────────────────────┐                 │
│  │ let (tx, mut rx) =                             │                 │
│  │     tokio::sync::mpsc::channel(100);           │                 │
│  │                                                │                 │
│  │ let workers: Vec<_> = (0..NUM_TASKS)           │                 │
│  │     .map(|_| {                                 │                 │
│  │         let mut rx = rx.clone();               │                 │
│  │         tokio::spawn(async move {              │                 │
│  │             // Async task (not OS thread!)     │                 │
│  │             while let Some(file) =             │                 │
│  │                     rx.recv().await {          │                 │
│  │                 let data = engine              │                 │
│  │                     .read_parquet(&file)       │                 │
│  │                     .await?;  // Yields!       │                 │
│  │                 // Process asynchronously      │                 │
│  │             }                                  │                 │
│  │         })                                     │                 │
│  │     })                                         │                 │
│  │     .collect();                                │                 │
│  │                                                │                 │
│  │ // Main task distributes work                  │                 │
│  │ let mut files = scan.files().await?;           │                 │
│  │ while let Some(file) = files.next().await {    │                 │
│  │     tx.send(file).await?;                      │                 │
│  │ }                                              │                 │
│  │                                                │                 │
│  │ for worker in workers {                        │                 │
│  │     worker.await?;                             │                 │
│  │ }                                              │                 │
│  └────────────────────────────────────────────────┘                 │
│                                                                     │
│  Tokio tasks (M:N with cores)                                       │
│  Async I/O, cooperative scheduling                                  │
└─────────────────────────────────────────────────────────────────────┘
```

**Key difference**: 
- Before: `N` OS threads, each blocking on I/O
- After: `N` async tasks, multiplexed onto M cores (typically M << N)

---

## The I/O Boundary Pattern

### Engine Implementation Strategy

```
┌─────────────────────────────────────────────────────────────────────┐
│                    BUSINESS LOGIC                                   │
│                                                                     │
│   #[async_fn]                                                       │
│   fn process_log(engine: &dyn Engine) -> Result<Output> {           │
│       let actions = await_!(read_actions(engine))?;                 │
│       actions.async_fold(/* ... */)                                 │
│   }                                                                 │
│                                                                     │
│   ✅ Single source! No duplication!                                 │
│   ✅ Compiles to sync or async based on feature                     │
└──────────────────────┬──────────────────────────────────────────────┘
                       │
                       │ Calls engine
                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    ENGINE TRAIT                                     │
│                                                                     │
│   #[async_fn]                                                       │
│   fn read_json_files(&self, paths: &[Path])                         │
│                    -> Result<Box<dyn EngineData>>;                  │
│                                                                     │
│   ✅ Single trait definition!                                       │
└──────────────────────┬──────────────────────────────────────────────┘
                       │
                       │ Must be implemented
                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│              ENGINE IMPLEMENTATION (I/O LAYER)                      │
│                                                                     │
│   ✅ ONE NATIVE ASYNC IMPL - ALL THE LOGIC                          │
│                                                                     │
│   impl DefaultJsonHandler {                                         │
│       async fn read_json_impl(&self, paths: &[Path])                │
│                          -> Result<impl Stream<...>> {              │
│           let data = object_store.get(path).await?;                 │
│           // ... parse JSON ...                                     │
│           // ALL logic here, single source!                         │
│       }                                                             │
│   }                                                                 │
│                                                                     │
│   ✅ ONE UNIFIED TRAIT WRAPPER (uses macros)                        │
│                                                                     │
│   impl JsonHandler for DefaultJsonHandler {                         │
│       #[async_fn]                                                   │
│       fn read_json_files(&self, paths: &[Path])                     │
│                        -> Result<...> {                             │
│           await_!(into_boxed_async_iterator(                        │
│               &self.executor,                                       │
│               self.read_json_impl(paths)                            │
│           ))                                                        │
│       }                                                             │
│   }                                                                 │
│                                                                     │
│   ✅ ONE HELPER FUNCTION (conditional compilation)                  │
│                                                                     │
│   // Sync: block + convert Stream→Iterator + box                    │
│   #[cfg(not(feature = "async"))]                                    │
│   fn into_boxed_async_iterator(...) { ... }                         │
│                                                                     │
│   // Async: await + box Stream                                      │
│   #[cfg(feature = "async")]                                         │
│   async fn into_boxed_async_iterator(...) { ... }                   │
│                                                                     │
│   ✅ Zero logic duplication!                                        │
│   ✅ Single wrapper per handler method                              │
│   ✅ Consistent pattern across all handlers                         │
└──────────────────────┬──────────────────────────────────────────────┘
                       │
                       │ All I/O is async underneath
                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│                ASYNC I/O LIBRARIES                                  │
│                                                                     │
│   object_store::ObjectStore (async)                                 │
│   parquet::ParquetRecordBatchStream (async)                         │
│   tokio::fs (async)                                                 │
│   reqwest (async)                                                   │
│                                                                     │
│   Everything here is async!                                         │
└─────────────────────────────────────────────────────────────────────┘
```

**The Pattern**:
1. **One native async impl method** - contains all the real logic
2. **One unified trait wrapper** - uses `#[async_fn]` + `await_!` macros
3. **One helper function** - handles Stream→Iterator conversion in sync mode

**What this achieves**:
- ✅ Eliminates duplication in all **business logic** (100% of code)
- ✅ Zero logic duplication at **I/O boundary**
- ✅ Single trait wrapper per handler method (no `#[cfg]` blocks)
- ✅ Consistent pattern across all handlers

**Is it worth it?** 
- **Absolutely YES** - no logic duplication anywhere
- Single trait wrapper per handler (maximum unification)
- ~25 lines of one-time helper code

---

## Testing Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                      CI PIPELINE                                    │
│                                                                     │
│  ┌─────────────────────────────────────────────────┐                │
│  │  Test Suite (Single Source)                     │                │
│  │                                                 │                │
│  │  #[tokio::test]  ← Already async for test setup │                │
│  │  async fn test_snapshot_building() {            │                │
│  │      let engine = get_test_engine();            │                │
│  │      let snapshot = Snapshot::builder_for(url)  │                │
│  │                         .build(&engine)         │                │
│  │                         .await_()?;  ← Macro!   │                │
│  │      // ... test assertions ...                 │                │
│  │  }                                              │                │
│  └─────────────────────────────────────────────────┘                │
│                                                                     │
│                          │                                          │
│                          │ Compiled twice                           │
│                          ▼                                          │
│                                                                     │
│  ┌────────────────────────┐        ┌────────────────────────┐       │
│  │  Sync Mode             │        │  Async Mode            │       │
│  │                        │        │                        │       │
│  │  cargo test            │        │  cargo test            │       │
│  │                        │        │  --features            │       │
│  │                        │        │      async             │       │
│  │                        │        │                        │       │
│  │  .await_() = nop       │        │  .await_() adds .await │       │
│  │  Returns Iterator      │        │  Returns Stream        │       │
│  └────────────────────────┘        └────────────────────────┘       │
│                                                                     │
│         Both must pass!                                             │
└─────────────────────────────────────────────────────────────────────┘
```

**CI time impact**: 2x (must run both modes)

---

## Decision Tree for Consumers

```
                      START: Choose Your Mode
                              │
                              │
                              ▼
                    ┌──────────────────────┐
                    │ Are you writing      │
                    │ C/C++ FFI consumer?  │
                    └──────────┬───────────┘
                              │
                 ┌────────────┴────────────┐
                 │                         │
                YES                        NO
                 │                         │
                 ▼                         ▼
         ┌──────────────┐      ┌──────────────────┐
         │  USE SYNC    │      │ Is your app      │
         │   MODE       │      │ already async?   │
         │              │      │ (using tokio,    │
         │ (No choice!) │      │  actix, etc.)    │
         └──────────────┘      └─────┬────────────┘
                                     │
                         ┌───────────┴───────────┐
                         │                       │
                        YES                      NO
                         │                       │
                         ▼                       ▼
                ┌─────────────────┐    ┌────────────────────┐
                │ Do you need     │    │ Is your app a      │
                │ max performance │    │ simple CLI tool?   │
                │ or composing    │    └─────┬──────────────┘
                │ with other      │          │
                │ async code?     │          │
                └────┬────────────┘    ┌─────┴─────┐
                     │                 │           │
              ┌──────┴──────┐         YES         NO
              │             │          │           │
             YES            NO         │           ▼
              │             │          │    ┌───────────────┐
              │             │          │    │ Do you need   │
              │             │          │    │ complex       │
              │             │          │    │ concurrency?  │
              │             │          │    └───┬───────────┘
              │             │          │        │
              │             │          │  ┌─────┴─────┐
              │             │          │  │           │
              │             │          │ YES          NO
              │             │          │  │           │
              ▼             ▼          ▼  ▼           ▼
        ┌──────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐
        │   USE    │  │   USE   │  │   USE   │  │   USE   │
        │  ASYNC   │  │  SYNC   │  │  SYNC   │  │  SYNC   │
        │   MODE   │  │  MODE   │  │  MODE   │  │  MODE   │
        │          │  │         │  │         │  │         │
        │ Get max  │  │ Simpler │  │ Simpler │  │ Simpler │
        │ perf!    │  │ mental  │  │ & proven│  │ & less  │
        │          │  │ model   │  │         │  │ deps    │
        └──────────┘  └─────────┘  └─────────┘  └─────────┘
```

**General rule of thumb**:
- **Default to sync** unless you have a specific need for async
- **Choose async** if you're already in the async ecosystem

---

## Summary

### Architecture Wins
- ✅ Kernel business logic: **100% unified**
- ✅ Public API: **100% unified** (via macros)
- ✅ Engine I/O layer: **0% logic duplication** (one async impl + two one-line trait wrappers)
- ✅ Consumers: **Backward compatible**, opt-in async

### Consumer Impact
| Consumer | Current | With Macro Approach (Sync) | With Macro Approach (Async) |
|----------|---------|----------------------------|----------------------------|
| FFI | Sync C API | Sync C API (unchanged) | N/A (FFI stays sync) |
| Simple CLI | `fn main()` | `fn main()` (unchanged) | `#[tokio::main] async fn main()` |
| Multi-threaded | OS threads | OS threads (unchanged) | Tokio tasks |
| Web server | Blocking | Blocking (unchanged) | Native async |
| Library | Sync calls | Sync calls (unchanged) | Async calls |

### The Trade-off
**Gain**: Unified codebase, better async composition, optional performance improvements

**Cost**: Dual-mode maintenance, testing complexity, documentation burden

**Verdict**: **Worth it** if async ecosystem adoption is a goal.

