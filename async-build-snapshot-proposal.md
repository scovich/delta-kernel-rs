# Control Flow 1: Snapshot Creation - Refactoring Guide

## 1. Executive Summary

This document analyzes Control Flow 1 (Snapshot Creation) in `delta-kernel-rs` and provides a concrete refactoring plan to support async operations while maintaining sync compatibility.

### Key Findings

- **40% of code is already I/O-free** (computation on fetched data)
- **30% is fundamental I/O** (file reads, directory listings)
- **30% mixes I/O with computation** (choreography that needs refactoring)
- **🔍 Control flow has 5+ levels of complexity** beneath `replay_for_metadata`

### Pattern Refinement

Analysis reveals **three core patterns** needed (Section 2.2):

- **Pattern A (Helper Functions)**: One-shot operations ✅
- **Pattern B (Processor + try_fold)**: Iterative processing with early exit ✅
- **Pattern C (Two-Phase Processing)**: Sequential coordination + parallelizable work ✅
  - Phase 1 uses Pattern B (ControlFlow-based state machine)
  - Phase 2 uses nested Pattern B (files × batches)

**Status**: All patterns designed and refined. Pattern C incorporates insights from [PR #1160](https://github.com/delta-io/delta-kernel-rs/pull/1160) with ControlFlow-based improvements (Section 10).

### Recommendations

1. **Extract I/O-free processors** from mixed choreography code
2. **Use pattern library** to match refactoring approach to problem structure:
   - **Pattern A (Helper Functions)**: For one-shot operations (e.g., `LastCheckpointHint`) ✅
   - **Pattern B (Processor + try_fold)**: For iterative processing (e.g., `MetadataExtractor`) ✅
   - **Pattern C (Two-Phase + ControlFlow)**: For checkpoint processing with manifest + sidecars ✅
3. **Minimal sync/async duplication**: `try_fold` + `ControlFlow` pattern reduces duplication to ~33 lines (mostly `.await`)
4. **Async virality is contained**: Only 35% of functions need async variants; 65% of code is shared
5. **Phased approach**: Start with Patterns A & B (proven), then implement C (design validated by PR #1160)

### Navigation

- **Section 2**: Current and refactored state analysis
  - **2.1**: Current control flow (what we have now)
  - **2.2**: I/O vs computation breakdown (categorizing the code)
  - **2.3**: Refactored control flow (what it will look like after patterns applied)
- **Section 3**: Foundation (principles + pattern library)
- **Section 4**: Applying patterns (concrete refactoring examples)
- **Section 5**: Implementation plan (phased rollout)
- **Section 6-8**: Decisions, outcomes, open questions
- **Section 9-10**: Appendices (learnings, comparisons)

---

## 2. Current State Analysis

### 2.1 Overall Control Flow Diagram

```
User: Snapshot::builder_for(url).build(engine)
    ↓
SnapshotBuilder::build(engine)
    ├─ LogSegment::for_snapshot(storage, log_root, ...)
    │   ├─ LastCheckpointHint::try_read(storage, log_root)     ← I/O #1: Read _last_checkpoint
    │   │   └─ storage.read_files([_last_checkpoint])
    │   ├─ ListedLogFiles::list(storage, log_root, ...)
    │   │   └─ storage.list_from(log_root)                     ← I/O #2: List directory
    │   └─ LogSegment::try_new(listed_files)                   ← CPU only ✅
    │
    ├─ Snapshot::try_new_from_log_segment(log_segment, engine)
    │   ├─ log_segment.read_metadata(engine)                   ← Thin wrapper (error handling)
    │   │   └─ log_segment.protocol_and_metadata(engine)       ← I/O #3: Mixed (extraction loop) ⚠️
    │   │       ├─ log_segment.replay_for_metadata(engine)     ← Thin wrapper (schema selection)
    │   │       │   └─ log_segment.read_actions(engine, ...)   ← I/O #4: Multi-source choreography ⚠️
    │   │   │   │       ├─ find_commit_cover()                     ← CPU: compute file list ✅
    │   │   │   │       ├─ json_handler.read_json_files(...)       ← I/O #5: Returns Iterator (hides async)
    │   │   │   │       └─ create_checkpoint_stream(...)           ← I/O #6: Complex nested choreography ⚠️
    │   │   │   │           ├─ validation logic                    ← CPU: schema checks ✅
    │   │   │   │           ├─ extract checkpoint files           ← CPU: list building ✅
    │   │   │   │           ├─ parquet/json_handler.read_*_files  ← I/O #7: Returns Iterator (hides async)
    │   │   │   │           └─ For each checkpoint batch:
    │   │   │   │               └─ process_sidecars(...)           ← I/O #8: Conditional nested I/O ⚠️
    │   │   │   │                   ├─ SidecarVisitor.visit_rows  ← CPU: extract sidecar refs ✅
    │   │   │   │                   └─ parquet_handler.read_*     ← I/O #9: Returns Iterator (more async!)
    │   │   │
    │   │   └─ for actions_batch in actions_batches {          ← I/O #10: CONSUMING chained iterators!
    │   │         ⮡ THIS IS WHERE ALL NESTED I/O HAPPENS ⮡
    │   │         Metadata::try_new_from_data(actions)?       ← CPU (on fetched data)
    │   │         Protocol::try_new_from_data(actions)?        ← CPU (on fetched data)
    │   │       }
    │   │
    │   └─ TableConfiguration::try_new(metadata, protocol)      ← CPU only ✅
    │
    └─ wrap in Arc<Snapshot>
```

**Key observations**:
- ⚠️ **Multi-source**: `read_actions` orchestrates commits + checkpoints (different schemas)
- ⚠️ **Nested I/O**: Each checkpoint batch can trigger N sidecar reads (iterator-in-iterator)
- ⚠️ **Conditional I/O**: Sidecars only read when schema needs file actions AND single-part checkpoint
- ⚠️ **Iterator hiding**: Engine handlers return iterators that hide async I/O operations

### 2.2 I/O vs Computation Breakdown

#### Category 1: Pure Computation (Already I/O-free ✅)

These operations already work on pre-fetched data:

1. **`LogSegment::try_new(ListedLogFiles)`**
   - Input: struct with Vecs of already-listed files
   - Output: validated LogSegment
   - Work: Checks for gaps, ensures contiguity
   - **Already I/O-free** ✅

2. **`Metadata::try_new_from_data(&dyn EngineData)`**
   - Input: EngineData (already fetched!)
   - Output: Option<Metadata>
   - Work: Extract metadata fields from batch
   - **Already I/O-free** ✅

3. **`Protocol::try_new_from_data(&dyn EngineData)`**
   - Input: EngineData (already fetched!)
   - Output: Option<Protocol>
   - Work: Extract protocol fields from batch
   - **Already I/O-free** ✅

4. **`TableConfiguration::try_new(Metadata, Protocol, ...)`**
   - Input: Already-parsed metadata and protocol
   - Output: TableConfiguration
   - Work: Validation
   - **Already I/O-free** ✅

#### Category 2: Pure I/O (Fundamental operations)

These are inherently I/O and represent the engine's interface:

1. **`StorageHandler::list_from(log_root)`**
   - Lists _delta_log directory
   - Returns `Iterator<FileMeta>`
   - **Note**: This is fundamentally I/O - cannot be made I/O-free

2. **`JsonHandler::read_json_files(files)`**
   - Reads commit JSON files
   - Returns `Iterator<DeltaResult<Box<dyn EngineData>>>`
   - **Note**: OK for engine interface; kernel should not consume internally
   - **Implementation**: Default engine spawns async tasks, returns sync iterator backed by channel

3. **`ParquetHandler::read_parquet_files(files)`**
   - Reads checkpoint Parquet files
   - Returns `Iterator<DeltaResult<Box<dyn EngineData>>>`
   - **Note**: Same as JSON - async I/O hidden behind sync iterator
   - **Problem**: Iterator pattern hides async complexity, makes analysis harder

#### Category 3: Mixed Choreography (Needs refactoring ⚠️)

These operations mix I/O orchestration with computation logic:

**Problem 1: `LastCheckpointHint::try_read(storage, log_root)`**
- Mixes: file reading (I/O) + JSON parsing (computation) + error handling (computation)
- Cannot test parsing logic without I/O
- Cannot reuse for async without duplicating error handling
- **Complexity**: Simple (one-shot operation)
- **Refactor approach**: Separate read from parse using Pattern A (Helper Function)

**Problem 2: `LogSegment::protocol_and_metadata(engine)`**
- Delegates to `replay_for_metadata` (thin wrapper) → `read_actions` (see Problem 3)
- Mixes: iterator consumption (I/O) + metadata/protocol extraction (computation)
- **Complexity**: Medium (stateful, early exit)
- **Refactor approach**: Extract processor using Pattern B (Processor + try_fold)

**Problem 3: `LogSegment::read_actions(engine, ...)`**
- **Location**: `kernel/src/log_segment.rs:288-310`
- **Complexity**: High (multi-source orchestration)
- Mixes:
  - `find_commit_cover()` - ✅ CPU: compute minimal file set (already I/O-free)
  - `json_handler.read_json_files()` - ❌ I/O: returns iterator hiding async
  - `create_checkpoint_stream()` - ❌ I/O: see Problem 4 below
  - `.chain()` - ❌ I/O: combines two lazy iterators
- **Problem**: Orchestrates two independent sources (commits + checkpoints) with different schemas
- **Cannot easily apply Pattern B**: Multi-source requires different approach
- **Refactor approach**: Multi-source orchestration handled by Pattern C's phase 1 (chains commits + checkpoint manifest) (Section 3.2.2)

**Problem 4: `LogSegment::create_checkpoint_stream(engine, ...)`**
- **Location**: `kernel/src/log_segment.rs:366-459`
- **Complexity**: Very High (nested conditional I/O)
- Mixes:
  - Validation logic (need_file_actions, sidecar requirements) - ✅ CPU
  - Extract checkpoint file list - ✅ CPU
  - Match checkpoint type (json vs parquet) - ✅ CPU
  - Read checkpoint files via handlers - ❌ I/O
  - **For each checkpoint batch**: conditionally read sidecars - ❌ Nested I/O!
- **Problem**: Iterator-in-iterator pattern (checkpoint batches → sidecar batches)
- **Problem**: Conditional nested I/O (only read sidecars if single-part + file actions needed)
- **Refactor approach**: Extract processor that returns I/O requests (Pattern C in Section 3.2)

**Problem 5: `LogSegment::process_sidecars(...)`**
- **Location**: `kernel/src/log_segment.rs:465-493`
- **Complexity**: Medium (I/O-free extraction + I/O)
- Mixes:
  - `SidecarVisitor::visit_rows()` - ✅ CPU: extract sidecar refs from batch
  - Convert to `FileMeta` paths - ✅ CPU
  - Read sidecar files via handler - ❌ I/O
- **Problem**: Already partially separated, but still consumes iterator internally
- **Refactor approach**: Extract visitor logic, return I/O request (partial Pattern C)

**Summary of Category 3 complexity**:
- **Simple** (1 operation): `LastCheckpointHint::try_read` 
- **Medium** (2 operations): `protocol_and_metadata`, `process_sidecars`
- **High** (1 operation): `read_actions` (multi-source)
- **Very High** (1 operation): `create_checkpoint_stream` (nested conditional I/O)

**Example**: Problem 2 (`protocol_and_metadata`) shows ~20 lines of extraction logic intertwined with I/O loop, making it untestable and unreusable for async. See Section 4.2 for detailed analysis and refactoring.

### 2.3 Refactored Control Flow

After applying the patterns from Section 3, the control flow becomes:

```
SnapshotBuilder::build[_async](engine)                                [async: + .await]
    ├─ LogSegment::for_snapshot[_async](storage, log_root, ...)      [async: + .await]
    │   ├─ LastCheckpointHint::try_read[_async](storage, log_root)   [async: + .await]
    │   │   ├─ storage.read_file(path)                                ← I/O
    │   │   └─ LastCheckpointHint::from_file_result(result)           [shared ✅]
    │   │
    │   ├─ ListedLogFiles::list[_async](storage, log_root, ...)      [async: + .await]
    │   │   └─ storage.list_from(log_root)                            ← I/O
    │   │
    │   └─ LogSegment::try_new(listed_files)                          [shared ✅]
    │
    ├─ Snapshot::try_new_from_log_segment[_async](log_segment, engine)  [async: + .await]
    │   ├─ log_segment.read_metadata[_async](engine)                  [async: + .await]
    │   │   └─ log_segment.protocol_and_metadata[_async](engine)      [async: + .await]
    │   │       ├─ log_segment.phase1_[sync|async](engine, processor)   [async: + .await]
    │   │       │   ├─ find_commit_cover()                            [shared ✅]
    │   │       │   ├─ json_handler.read_json_files(...)              ← I/O (returns iterator/stream)
    │   │       │   ├─ parquet_handler.read_parquet_files(...)        ← I/O (returns iterator/stream)
    │   │       │   └─ iterators.chain().try_fold(processor, ...)     ← I/O (consumes iterator)
    │   │       │       └─ Phase1InProgress::process_batch(batch)     [shared ✅]
    │   │       │
    │   │       └─ phase1_result.process_sidecars_[sync|async](engine)  [async: + .await]
    │   │           └─ sidecar_files.try_fold(processor, ...)         ← I/O (reads + consumes)
    │   │               └─ batches.try_fold(processor, ...)           ← I/O (consumes iterator)
    │   │                   └─ processor.process_batch(batch)         [shared ✅]
    │   │
    │   └─ TableConfiguration::try_new(metadata, protocol)            [shared ✅]
    │
    └─ Arc::new(Snapshot)

Legend:
  [shared ✅]       - Same code used by both sync and async
  [async: + .await] - Async version adds .await (and async move in closures), otherwise identical
  func[_async]      - Has both sync and async variants
  func[_sync|async] - Explicitly different method names (not just suffix)
```

**Key changes from Section 2.1**:

1. **Pattern A applied**: `LastCheckpointHint::from_file_result` extracted (I/O-free helper)
2. **Pattern B applied**: `MetadataExtractor` processor extracted (I/O-free state machine)
3. **Pattern C applied**: Two-phase processing with `phase1_*` and `process_sidecars_*`
4. **Shared computation**: All processors and pure functions work for both sync and async

#### Code Metrics After Refactoring

| Category | Sync Lines | Async Lines | Shared Lines | Total | Actual Duplication |
|----------|-----------|-------------|--------------|-------|-------------------|
| **Choreography** | | | | | |
| `LastCheckpointHint::try_read[_async]` | 3 | 3 | - | 6 | 1 line (`.await`) |
| `ListedLogFiles::list[_async]` | ~20 | ~20 | - | 40 | ~15 lines |
| `LogSegment::phase1_[sync\|async]` | ~15 | ~15 | - | 30 | ~5 lines |
| `Phase1Result::process_sidecars_*` | ~10 | ~10 | - | 20 | ~3 lines |
| `LogSegment::read_metadata[_async]` | 3 | 3 | - | 6 | 2 lines |
| `Snapshot::try_new_from_log_segment[_async]` | 5 | 5 | - | 10 | 2 lines |
| `SnapshotBuilder::build[_async]` | ~10 | ~10 | - | 20 | ~5 lines |
| **I/O-free components (shared)** | | | | | |
| `LastCheckpointHint::from_file_result` | - | - | 8 | 8 | **0** |
| `MetadataExtractor` (processor) | - | - | ~30 | 30 | **0** |
| `Phase1InProgress` (processor) | - | - | ~20 | 20 | **0** |
| Pure computation (various) | - | - | ~30 | 30 | **0** |
| **Totals** | ~66 | ~66 | ~88 | 220 | **~33 lines** |

**Analysis**:
- **132 lines of choreography** (~66 sync + ~66 async), but only **~33 lines actually differ**
- **88 lines of shared logic** (processors + pure functions) used by both sync and async
- **Duplication rate**: 33/220 = **15%** (vs 50% if we duplicated everything)
- **Actual difference**: Mostly `.await` keywords and `async move` in closures

#### Async Virality Assessment

**Functions requiring async variants** (7 total):
1. `LastCheckpointHint::try_read_async` - Pattern A choreography
2. `ListedLogFiles::list_async` - Pure I/O wrapper
3. `LogSegment::phase1_async` - Pattern C Phase 1 choreography
4. `Phase1Result::process_sidecars_async` - Pattern C Phase 2 choreography
5. `LogSegment::read_metadata_async` - Thin wrapper
6. `Snapshot::try_new_from_log_segment_async` - Thin wrapper
7. `SnapshotBuilder::build_async` - Entry point

**Functions NOT requiring async variants** (13+ total):
- `LastCheckpointHint::from_file_result` ✅ (I/O-free helper)
- `MetadataExtractor::process_batch` ✅ (I/O-free processor)
- `MetadataExtractor::try_finish` ✅ (I/O-free processor)
- `Phase1InProgress::process_batch` ✅ (I/O-free processor)
- `LogSegment::try_new` ✅ (pure computation)
- `TableConfiguration::try_new` ✅ (pure computation)
- `find_commit_cover` ✅ (pure computation)
- All `Metadata`/`Protocol` extraction functions ✅ (pure computation)
- All validation functions ✅ (pure computation)
- Extension traits (`ResultExt`, `ControlFlowExt`) ✅ (generic utilities)

**Virality ratio**: 7 async wrappers / 20 total functions = **35% need async variants**

The other 65% of code is I/O-free and shared between sync and async paths!

**Why this matters**:
- New features added to processors work for both sync and async automatically
- Test coverage of processors applies to both sync and async paths
- Bug fixes in processors fix both sync and async simultaneously
- Most complexity (business logic) lives in shared code

---

## 3. Foundation: Refactoring Principles & Patterns

### 3.1 Core Principles

These principles guide all refactoring decisions:

#### Principle 1: Iterator Consumption IS I/O

```rust
// This loop is I/O, not just computation!
for item in iterator {  // ← .next() may trigger network/disk reads
    process(item);      // ← This is computation (on fetched data)
}
```

**Key insight**: Calling `.next()` on an iterator may trigger file reads. The loop itself is the I/O operation, not just the code that creates the iterator.

**Why this matters**: We can't make iteration I/O-free by changing what the iterator returns. The act of iterating is the I/O.

#### Principle 2: Materialization Just Shifts the Problem

```rust
// DON'T do this:
fn read_metadata(&self, engine: &dyn Engine) -> DeltaResult<(Metadata, Protocol)> {
    let batches: Vec<_> = self.replay_for_metadata(engine)?.collect()?;  // ← Blocks here
    process_batches(&batches)  // ← Now I/O-free, but we just moved the blocking
}
```

**Problems**:
- Blocks at a different point (during `collect()`)
- Wastes memory (forces all data into RAM)
- Loses opportunity for early exit
- Doesn't actually make anything I/O-free

**Insight**: Forcing a `Vec` just shifts where blocking happens. It doesn't separate I/O from computation.

#### Principle 3: Separate Computation from Choreography

**Computation** = Pure logic on already-fetched data
- Can be tested without I/O
- Reusable across sync/async
- Expressible as pure functions or state machines

**Choreography** = Thin I/O orchestration that feeds computation
- Fetches data (sync or async)
- Feeds data to computation
- Handles results
- Minimal logic (just I/O coordination)

**Benefits**:
- Testable: Can unit test computation with mock data
- Reusable: Same computation works for sync and async
- Flexible: Users can write custom choreography

#### Principle 4: Match Pattern to Problem Structure

Different problems need different patterns:

| Problem Structure | Pattern | Example | Status |
|-------------------|---------|---------|--------|
| One-shot operation (read file, parse, done) | A: Helper Function | `LastCheckpointHint` | ✅ Implemented |
| Iterative processing (multi-batch, stateful, early exit) | B: Processor + try_fold | `MetadataExtractor` | ✅ Implemented |
| Nested I/O with parallelizable phase | C: Two-Phase (PR #1160) | Checkpoint + sidecars | ⏳ Design refined |
| ~~Multi-source orchestration~~ | ~~D: Expose Separately~~ | ~~`read_actions`~~ | ✅ Subsumed by C |

**Don't over-engineer**: Simple problems deserve simple solutions.

### 3.2 Pattern Library

#### Pattern A: Helper Functions (for one-shot operations)

**When to use**: 
- Single file read, parse, done
- No iteration or state accumulation
- Simple error handling

**Structure**:

```rust
// 1. I/O-free helper takes Result<Data>, handles all cases
impl Thing {
    fn from_file_result(result: DeltaResult<Data>) -> DeltaResult<Output> {
        match result {
            // All computation here (parsing, validation, transformation)
            Ok(data) => {
                let processed = process(data)?;  // Fallible operation (can use ?)
                Ok(transform(processed))         // Infallible transformation
            }
            // Handle expected errors (e.g., file not found)
            Err(Error::NotFound) => Ok(Output::default()),
            // Propagate unexpected errors
            Err(e) => Err(e),
        }
    }
}

// 2. Sync choreography (minimal glue)
pub fn read(storage: &dyn Storage, path: &Path) -> DeltaResult<Output> {
    Thing::from_file_result(storage.read_file(path))
}

// 3. Async choreography (only .await differs!)
pub async fn read_async(storage: &dyn AsyncStorage, path: &Path) 
    -> DeltaResult<Output> {
    Thing::from_file_result(storage.read_file(path).await)
    //                                             ^^^^^^ Only difference!
}
```

**Key characteristics**:
- Helper is a pure function on `Result<Data>`
- All error handling in one place (match on Result)
- All computation in one place (inside Ok arm)
- Choreography is just: path construction + I/O + helper call

**Benefits**:
- ✅ Minimal choreography (2-3 lines each)
- ✅ Only `.await` differs between sync and async
- ✅ All logic (error handling + computation) in shared helper
- ✅ Simple - no over-engineering
- ✅ Testable with mock `Result<Data>`

**Example**: `LastCheckpointHint::from_file_result` (see Section 4.1)

#### Pattern B: Processor + try_fold (for iterative processing)

**When to use**:
- Multi-batch processing
- Stateful accumulation
- Early exit possible
- Need both sync and async versions

**The Challenge**: Sync and async loops duplicate significant code

```rust
// Sync version (~10 lines)
let mut state = State::new();
for item in iterator {
    let item = item?;
    state.process(&item)?;
    if state.is_done() {
        return Ok(state.finish());
    }
}
Err(Error::Incomplete)

// Async version - nearly identical! (~10 lines duplicated)
let mut state = State::new();
while let Some(item) = stream.next().await {
    let item = item?;
    state.process(&item)?;
    if state.is_done() {
        return Ok(state.finish());
    }
}
Err(Error::Incomplete)
```

**The Solution**: Use `try_fold` + `ControlFlow`

Both [`Iterator::try_fold`](https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.try_fold) and [`TryStreamExt::try_fold`](https://docs.rs/futures/latest/futures/stream/trait.TryStreamExt.html#method.try_fold) support early exit via `std::ops::ControlFlow`, which implements the `Try` trait.

**Complete Pattern**:

```rust
use std::ops::ControlFlow;
// Uses extension traits (defined below) for .transpose() and .unwrap_break_or_else()

// 1. Processor: I/O-free state machine
#[derive(Default)]
pub struct Processor {
    // Accumulated state
}

impl Processor {
    /// Process one item (pure computation, no I/O).
    /// 
    /// Takes self by value, returns:
    /// - `Ok(Break(output))` when processing is complete
    /// - `Ok(Continue(self))` when more items are needed
    /// - `Err(...)` on processing errors
    pub fn process(mut self, item: &Item) -> DeltaResult<ControlFlow<Output, Self>> {
        // Can use ? for error handling!
        let data = fallible_operation(item)?;
        
        // Update internal state
        self.accumulate(data);
        
        // Check if complete (cheap check, no allocation)
        match self {
            Self { complete: true, result } => Ok(ControlFlow::Break(result)),
            processor => Ok(ControlFlow::Continue(processor)),
        }
    }
    
    /// Extract result from incomplete processor (e.g., when items exhausted).
    /// Only allocates error when actually incomplete (optimized hot path).
    pub fn try_finish(self) -> DeltaResult<Output> {
        match self {
            Self { complete: true, result } => Ok(result),
            _ => Err(Error::Incomplete),
        }
    }
}

// 2. Sync choreography uses try_fold with transpose
pub fn operation(&self, engine: &dyn Engine) -> DeltaResult<Output> {
    self.get_data(engine)?
        .try_fold(Processor::default(), |proc, item| {
            proc.process(item).transpose()
            // Returns ControlFlow<Result<Output>, Processor>
            // - Break(Ok(output)): found result
            // - Break(Err(e)): processing error
            // - Continue(proc): keep going with updated processor
        })
        .unwrap_break_or_else(Processor::try_finish)?
        //                    ^^^^^^^^^^^^^^^^^^^^^ Called on Processor if items exhausted
}

// 3. Async choreography
pub async fn operation_async(&self, engine: &dyn AsyncEngine) -> DeltaResult<Output> {
    self.get_data(engine).await?
        .try_fold(Processor::default(), |proc, item| async move {
            proc.process(item).transpose()
        })
        .await
        .unwrap_break_or_else(Processor::try_finish)?
}
```

**Why This Works**:

1. **`try_fold` handles iteration**: Both sync (`for`) and async (`while let .await`) are replaced by `try_fold`
2. **Processor takes ownership**: `process(self, item)` takes ownership, avoiding borrow issues in async
3. **Processor returns `Result<ControlFlow<Output, Self>>`**: Can use `?` internally, signals completion via:
   - `Ok(Break(output))` when complete
   - `Ok(Continue(self))` when more items needed
   - `Err(e)` on processing errors
4. **`.transpose()` bridges the types**: Converts `Result<ControlFlow<O, S>>` to `ControlFlow<Result<O>, S>`:
   - `Ok(Break(output))` → `Break(Ok(output))` → stop with success
   - `Ok(Continue(proc))` → `Continue(proc)` → keep folding
   - `Err(e)` → `Break(Err(e))` → stop with error
5. **`try_fold` respects `ControlFlow`**: Stops on `Break`, continues on `Continue` (threading processor through)
6. **`try_finish()` handles exhaustion**: When items run out, convert incomplete processor to result

**Key insight**: The processor is both the accumulator AND the state machine. It moves through the fold, updating itself, until it either completes (Break) or we run out of items (Continue).

**Extension Traits for Ergonomics**:

The pattern uses two extension traits to bridge the gap between processor API (`Result<ControlFlow>`) and `try_fold` requirements (`ControlFlow<Result>`):

```rust
use std::ops::ControlFlow;

/// Extension trait for `Result<T, E>` providing additional combinators.
///
/// Processors naturally want to return `Result<ControlFlow<Output>>` (can use `?` internally),
/// but `try_fold` needs `ControlFlow<Result<Output>>` (tells try_fold to stop or continue).
/// The `transpose` method converts between these representations.
pub trait ResultExt<T, E>: Sized {
    /// Transpose `Result<ControlFlow<B, C>, E>` to `ControlFlow<Result<B, E>, C>`.
    ///
    /// Only callable when `Self = Result<ControlFlow<B, C>, E>`.
    fn transpose<B, C>(self) -> ControlFlow<Result<B, E>, C>
    where
        Self: Into<Result<ControlFlow<B, C>, E>>;
}

impl<T, E> ResultExt<T, E> for Result<T, E> {
    fn transpose<B, C>(self) -> ControlFlow<Result<B, E>, C>
    where
        Self: Into<Result<ControlFlow<B, C>, E>>
    {
        match self.into() {
            Ok(ControlFlow::Break(b)) => ControlFlow::Break(Ok(b)),
            Ok(ControlFlow::Continue(c)) => ControlFlow::Continue(c),
            Err(e) => ControlFlow::Break(Err(e)),
        }
    }
}

/// Extension trait for `ControlFlow<B, C>` providing utility methods.
pub trait ControlFlowExt<B, C>: Sized {
    /// Unwrap a Break value or compute a default from Continue.
    /// Useful for handling ControlFlow after try_fold completes.
    fn unwrap_break_or_else<F: FnOnce(C) -> B>(self, f: F) -> B;
    
    /// Unwrap a Break value or return a default.
    fn unwrap_break_or(self, default: B) -> B {
        self.unwrap_break_or_else(|_| default)
    }
}

impl<B, C> ControlFlowExt<B, C> for ControlFlow<B, C> {
    fn unwrap_break_or_else<F: FnOnce(C) -> B>(self, f: F) -> B {
        match self {
            ControlFlow::Break(b) => b,
            ControlFlow::Continue(c) => f(c),
        }
    }
}
```

**Design Notes**:

**Two traits instead of one**: We split into `ResultExt` and `ControlFlowExt` for type safety - each method only appears where it makes sense, preventing misuse.

**Generic traits with constrained methods**: Both traits are generic (`ResultExt<T, E>` and `ControlFlowExt<B, C>`), but individual methods have constraints that limit when they're callable:
- `ResultExt::transpose` uses `Self: Into<Result<ControlFlow<B, C>, E>>` to ensure it only works on `Result<ControlFlow<...>>`
- This allows adding more methods to these traits in the future while keeping each method type-safe

**The `Into` constraint trick**: `Self: Into<Result<ControlFlow<B, C>, E>>` cleverly expresses "Self must be `Result<ControlFlow<B, C>, E>`" since `Result<T, E>` only implements `Into<Result<T, E>>` for itself. This is more flexible than a trait specific to `Result<ControlFlow>`.

**Future: Generic choreography?** If we see a recurring pattern of processors with `default()`, `process()`, and `try_finish()` methods, we could define a trait and write generic sync/async choreography helpers:

```rust
pub trait Processor: Default {
    type Item;
    type Output;
    fn process(self, item: &Self::Item) -> DeltaResult<ControlFlow<Self::Output, Self>>;
    fn try_finish(self) -> DeltaResult<Self::Output>;
}

// Generic async helper
async fn process_with_async<P, S>(stream: S) -> DeltaResult<P::Output>
where
    P: Processor,
    S: futures::Stream<Item = P::Item>,
{
    use futures::stream::TryStreamExt as _;
    stream
        .try_fold(P::default(), |p, item| async move {
            p.process(&item).transpose()
        })
        .await
        .unwrap_break_or_else(P::try_finish)?
}
```

However, this abstraction may not be worth it:
- Processor signatures vary (some take `&Item`, others take `Item`)
- Some processors need configuration in their constructor
- Generic code obscures what's happening
- Only worth if we have many (5+) processors with identical patterns

**Recommendation**: Wait for more examples before adding this abstraction. The current pattern is simple enough to write per-processor.

**Benefits**:

| Aspect | Manual Loop | try_fold + ControlFlowExt |
|--------|-------------|---------------------------|
| Duplication | ~18 lines | ~1 line (just `async move`) ✅ |
| Processor complexity | Must handle errors manually | Can use `?` ✅ |
| Testability | Need I/O mocks | Processor is I/O-free ✅ |
| Power users | Need custom loop | Can use processor directly ✅ |
| Idiomaticity | Basic Rust | Advanced but stdlib ✅ |
| Debuggability | Easy (breakpoints in loop) | Harder (inside closure) ⚠️ |

**Trade-offs**:

**Pro**: Massive reduction in duplication with type-safe, reusable pattern

**Con**: Requires understanding `try_fold` and `ControlFlow` - more advanced than basic loops

**Verdict**: Worth it for any processor used in both sync and async contexts. The pattern is well-documented here and the extension trait makes it approachable.

**When to use this pattern**:
- ✅ You need both sync (Iterator) and async (Stream) versions
- ✅ The operation involves stateful processing with potential early exit
- ✅ The processing logic is complex enough to warrant extraction
- ✅ Processing may fail (need error handling)

**When NOT to use**:
- ❌ Only sync or only async needed (use simple loop)
- ❌ No early exit (use `fold` instead)
- ❌ Stateless transformation (use `map`/`filter`)
- ❌ Infallible processing (use simpler types)

**Example**: `MetadataExtractor::process_batch` (see Section 4.2)

#### Pattern C: Two-Phase Checkpoint Processing (Refined based on PR #1160)

**When to use**:
- Processing checkpoint manifests that reference sidecar files
- Need to separate sequential coordination (phase 1) from parallelizable work (phase 2)
- Want same pattern to work for simple, parallel, sync, and async cases

**Applies to**: `LogSegment::create_checkpoint_stream` (lines 366-459)

**The Challenge**:

Checkpoints can reference additional files (sidecars) that contain the bulk of the data:
```rust
// V2 Checkpoint structure:
// - manifest.parquet (small: metadata + sidecar references)
// - sidecar_1.parquet (large: actual file actions)
// - sidecar_2.parquet (large: actual file actions)
// - ...

for checkpoint_batch in checkpoint_iterator {
    process_checkpoint(checkpoint_batch)?;
    
    // This batch contains sidecar references!
    if batch_has_sidecars {
        for sidecar_batch in read_sidecars(...) {  // Nested I/O!
            process_sidecar(sidecar_batch)?;
        }
    }
}
```

**Two-Phase Solution** (based on [PR #1160](https://github.com/delta-io/delta-kernel-rs/pull/1160), refined with ControlFlow):

Split checkpoint processing into two distinct phases:

```rust
/// Phase 1 state: In progress
pub struct Phase1InProgress<P> {
    processor: P,
    sidecar_files: Vec<FileMeta>,
}

/// Phase 1 result
pub enum Phase1Result<P> {
    /// Phase 1 completed normally, phase 2 needed
    NeedPhase2 {
        processor: P,
        sidecar_files: Vec<FileMeta>,
    },
    /// Processor found what it needed early, phase 2 not needed
    Complete(P::Output),
}

// Ergonomic conversions for state transitions
impl<P> From<P> for Phase1InProgress<P> {
    fn from(processor: P) -> Self {
        Self {
            processor,
            sidecar_files: Vec::new(),
        }
    }
}

impl<P> From<Phase1InProgress<P>> for Phase1Result<P> {
    fn from(state: Phase1InProgress<P>) -> Self {
        Self::NeedPhase2 {
            processor: state.processor,
            sidecar_files: state.sidecar_files,
        }
    }
}

impl<P: LogReplayProcessor> Phase1InProgress<P> {
    /// Process one batch (I/O-free!)
    pub fn process_batch(
        mut self,
        batch: &ActionsBatch,
    ) -> DeltaResult<ControlFlow<Phase1Result<P>, Self>> {
        // Process with internal processor
        match self.processor.process_batch(batch)? {
            ControlFlow::Continue(proc) => {
                self.processor = proc;
                self.sidecar_files.extend(extract_sidecars(batch)?);
                Ok(ControlFlow::Continue(self))  // More batches needed
            }
            ControlFlow::Break(output) => {
                // Processor found what it needed - we're done!
                Ok(ControlFlow::Break(Phase1Result::Complete(output)))
            }
        }
    }
}

impl LogSegment {
    /// Phase 1: Process commits + checkpoint manifest (sync)
    pub fn phase1_sync<P>(
        &self,
        engine: &dyn Engine,
        processor: P,
    ) -> DeltaResult<Phase1Result<P>> 
    where
        P: LogReplayProcessor,
    {
        // Create iterators (lazy, owned by this function)
        let commit_batches = engine.read_json_files(self.find_commit_cover(), ...)?;
        let checkpoint_batches = engine.read_parquet_files(self.checkpoint_parts(), ...)?;
        
        // Pattern B: try_fold over commits + checkpoint
        commit_batches.chain(checkpoint_batches)
            .try_fold(
                processor.into(),  // Phase1InProgress::from(processor)
                |state, batch| {
                    state.process_batch(&batch).transpose()
                }
            )
            .unwrap_break_or_else(|state| Ok(state.into()))  // Phase1Result::from(state)
    }
    
    /// Phase 1: Process commits + checkpoint manifest (async)
    pub async fn phase1_async<P>(
        &self,
        engine: &dyn AsyncEngine,
        processor: P,
    ) -> DeltaResult<Phase1Result<P>> 
    where
        P: LogReplayProcessor,
    {
        use futures::stream::TryStreamExt as _;
        
        // Create streams (lazy, owned by this function)
        let commit_batches = engine.read_json_files(self.find_commit_cover(), ...).await?;
        let checkpoint_batches = engine.read_parquet_files(self.checkpoint_parts(), ...).await?;
        
        // Pattern B: try_fold over commits + checkpoint
        commit_batches.chain(checkpoint_batches)
            .try_fold(
                processor.into(),  // Phase1InProgress::from(processor)
                |state, batch| async move {
                    state.process_batch(&batch).transpose()
                }
            )
            .await
            .unwrap_break_or_else(|state| Ok(state.into()))  // Phase1Result::from(state)
    }
}

// Phase 2: Helper methods on Phase1Result for processing sidecars
impl<P: LogReplayProcessor> Phase1Result<P> {
    /// Phase 2: Process sidecars sequentially (sync)
    pub fn process_sidecars_sync(
        self,
        engine: &dyn Engine,
    ) -> DeltaResult<P::Output> {
        match self {
            Phase1Result::Complete(output) => Ok(output),  // Early exit, no phase 2 needed
            Phase1Result::NeedPhase2 { processor, sidecar_files } => {
                // Pattern B applied twice! Nested try_fold for nested iteration
                sidecar_files
                    .into_iter()
                    .try_fold(processor, |proc, sidecar_file| {
                        // Read sidecar file → iterator of batches
                        let batches = engine.read_parquet_file(&sidecar_file, ...)?;
                        
                        // Inner Pattern B: Process batches with processor
                        batches.try_fold(proc, |p, batch| p.process_batch(batch).transpose())
                        // Returns: ControlFlow<Result<Output, E>, Processor>
                        // This is exactly what outer try_fold wants!
                        // - Break(Ok(output)) → found what we need, stop all processing
                        // - Break(Err(e))     → error, propagate up
                        // - Continue(proc)    → finished this file, continue to next
                    })
                    .unwrap_break_or_else(P::try_finish)?
            }
        }
    }
    
    /// Phase 2: Process sidecars sequentially (async)
    pub async fn process_sidecars_async(
        self,
        engine: &dyn AsyncEngine,
    ) -> DeltaResult<P::Output> {
        use futures::stream::TryStreamExt as _;
        
        match self {
            Phase1Result::Complete(output) => Ok(output),  // Early exit, no phase 2 needed
            Phase1Result::NeedPhase2 { processor, sidecar_files } => {
                // Pattern B with async! Same structure, just add async/await
                futures::stream::iter(sidecar_files)
                    .try_fold(processor, |proc, sidecar_file| async move {
                        let batches = engine.read_parquet_file(&sidecar_file, ...).await?;
                        
                        batches.try_fold(proc, |p, batch| async move {
                            p.process_batch(batch).transpose()
                        }).await
                        // Same type composition as sync version
                    })
                    .await
                    .unwrap_break_or_else(P::try_finish)?
            }
        }
    }
}
```

**Key Insight: Pattern C Uses Pattern B Internally!**

Looking at the code above, Pattern B (`try_fold` with processor) appears **twice**:

1. **Outer loop**: Fold over sidecar **files**
   ```rust
   self.sidecar_files.into_iter().try_fold(self.processor, |proc, file| ...)
   ```

2. **Inner loop**: Fold over **batches** within each file
   ```rust
   batches.try_fold(proc, |p, batch| p.process(batch)...)
   ```

**This reveals the patterns compose!**

- **Pattern C** = Two-phase choreography (sequential phase 1 + parallelizable phase 2)
- **Pattern B** = Processor + `try_fold` (iterate with early exit possible)
- **Pattern C's phase 2 uses Pattern B** to process the sidecar list!

**Why the nested `try_fold`?**

We have **two levels of iteration**:
1. Files (sidecars)
2. Batches (within each file)

Each level benefits from Pattern B:
- Type-safe iteration (no manual loops)
- Error propagation via `?`
- Processor ownership threading (no borrow issues in async)
- Same code structure for sync and async

**Early exit behavior**: If the inner `try_fold` breaks (processor returns `Break(Ok(output))`), that break **propagates to the outer loop**, stopping all processing. This is correct!

- `Break(Ok(output))` → Found what we need (e.g., metadata + protocol), stop processing remaining files
- `Break(Err(e))` → Error occurred, propagate up immediately
- `Continue(processor)` → Finished this file, continue to next file

The types compose perfectly: inner `try_fold` returns `ControlFlow<Result<Output, E>, Processor>`, which is exactly what the outer `try_fold` needs. No conversion required!

**Duplication eliminated**: The sync and async versions are nearly identical (only differ by `async move` and `.await`), proving Pattern B's value for reducing sync/async duplication!

**Usage Examples**:

```rust
// Simple case: sync, sequential
pub fn read_metadata(&self, engine: &dyn Engine) -> DeltaResult<(Metadata, Protocol)> {
    self.phase1_sync(engine, MetadataExtractor::default())?
        .process_sidecars_sync(engine)?  // Handles both Complete and NeedPhase2 cases
}

// Async case: nearly identical!
pub async fn read_metadata_async(&self, engine: &dyn AsyncEngine) 
    -> DeltaResult<(Metadata, Protocol)> {
    self.phase1_async(engine, MetadataExtractor::default()).await?
        .process_sidecars_async(engine).await?
}

// Power user: parallel processing
pub fn read_metadata_parallel(&self, engine: &dyn Engine) 
    -> DeltaResult<(Metadata, Protocol)> {
    let phase1_result = self.phase1_sync(engine, MetadataExtractor::default())?;
    
    match phase1_result {
        Phase1Result::Complete(output) => Ok(output),  // Found early, no phase 2
        Phase1Result::NeedPhase2 { processor, sidecar_files } => {
            // Split sidecars across workers (rayon, thread pool, distributed nodes, etc.)
            let results: Vec<_> = sidecar_files
                .par_iter()
                .map(|file| {
                    let batches = engine.read_parquet_file(file, ...)?;
                    // Process batches with a clone of the processor
                    batches.try_fold(processor.clone(), |p, batch| {
                        p.process_batch(batch).transpose()
                    })
                    .unwrap_break_or_else(MetadataExtractor::try_finish)
                })
                .collect()?;
            
            // Merge results from parallel workers
            merge_processor_results(results)
        }
    }
}
```

**Key Insights**:

1. **Clean separation**: Phase 1 (manifest) is always sequential, Phase 2 (sidecars) is parallelizable
2. **Natural for async**: Clear boundaries make async control flow straightforward
3. **Supports all use cases**: Simple sequential, async, parallel, distributed - same pattern
4. **Manifest is small**: Reading entire manifest first adds < 0.1% latency (see Section 9.6)
5. **Better for parallelization**: Having all sidecar files up front enables load balancing, caching, progress tracking
6. **Patterns compose**: Pattern C's phase 2 uses Pattern B (`try_fold`) internally - nested iteration benefits from same techniques!

**Benefits**:
- ✅ Processor is I/O-free (testable)
- ✅ Simple case is clean (user doesn't see phases)
- ✅ Power users can parallelize phase 2
- ✅ Sync and async differ only in choreography
- ✅ Natural boundary for distributed processing

**Why two-phase over incremental?** (See Section 9.6 for detailed analysis):
- Checkpoint manifests are tiny (< 1MB, just metadata + paths)
- Reading entire manifest adds negligible time (< 50ms vs minutes-hours for sidecars)
- Two-phase is simpler and enables better parallelization
- Incremental approach saves < 0.1% time while adding significant complexity

**Status**: Design refined based on [PR #1160](https://github.com/delta-io/delta-kernel-rs/pull/1160). See Section 9.6 for detailed analysis and comparison.

**Note on Multi-Source Orchestration**: The challenge of orchestrating multiple sources (commits + checkpoints) with different schemas is naturally handled by Pattern C's phase 1. By chaining the commit and checkpoint iterators together, both sources are processed uniformly through the same `try_fold` loop. No separate pattern needed.

---

## 4. Applying Patterns to Category 3

This section shows concrete refactoring examples for the two problems identified in Category 3.

### 4.1 Problem 1: LastCheckpointHint (Simple case - Pattern A)

The simpler case (Pattern A) is presented first, followed by the more complex iterative case (Pattern B).

#### Current Implementation: Mixed Read + Parse

```rust
impl LastCheckpointHint {
    pub fn try_read(storage: &dyn Storage, log_root: &Url) 
        -> DeltaResult<Option<Self>> {
        let path = log_root.join("_last_checkpoint")?;
        
        // I/O + parsing mixed together
        let bytes = match storage.read_file(&path) {
            Ok(b) => b,
            Err(Error::FileNotFound(_)) => return Ok(None),
            Err(e) => return Err(e),
        };
        
        if bytes.is_empty() {
            return Ok(None);
        }
        
        serde_json::from_slice(&bytes).map_err(Error::ParseError)
    }
}
```

**Problems**:
- Read and parse logic are intertwined (~15 lines)
- Can't test parse logic without I/O
- Can't reuse for async without duplicating all error handling

#### Refactored: Pattern A (Helper Function)

**Step 1: Extract I/O-free helper**

```rust
impl LastCheckpointHint {
    /// Parse from file read result (I/O-free).
    /// 
    /// Handles common cases:
    /// - FileNotFound → Ok(None)
    /// - Empty file → Ok(None)
    /// - Valid JSON → Ok(Some(hint))
    /// - Parse error → Err
    fn from_file_result(result: DeltaResult<Bytes>) -> DeltaResult<Option<Self>> {
        match result {
            Ok(bytes) if bytes.is_empty() => Ok(None),
            Ok(bytes) => Ok(Some(serde_json::from_slice(&bytes).map_err(Error::ParseError)?)),
            Err(Error::FileNotFound(_)) => Ok(None),
            Err(e) => Err(e),
        }
    }
}
```

**Benefits**:
- ✅ No I/O - pure function on `Result<Bytes>`
- ✅ Testable with mock file results
- ✅ All error handling in one place
- ✅ Can use `?` operator naturally

**Step 2: Minimal choreography**

```rust
impl LastCheckpointHint {
    /// Read checkpoint hint from storage (sync).
    pub fn try_read(storage: &dyn Storage, log_root: &Url) 
        -> DeltaResult<Option<Self>> {
        let path = log_root.join("_last_checkpoint")?;
        Self::from_file_result(storage.read_file(&path))
        // Helper handles all error cases and parsing
    }
    
    /// Read checkpoint hint from storage (async).
    pub async fn try_read_async(storage: &dyn AsyncStorage, log_root: &Url)
        -> DeltaResult<Option<Self>> {
        let path = log_root.join("_last_checkpoint")?;
        Self::from_file_result(storage.read_file(&path).await)
    }
}
```

**Result**:
- Sync: 3 lines of choreography
- Async: 3 lines of choreography (only `.await` differs)
- Shared: ~8 lines of helper logic

**Duplication eliminated**: Was ~30 lines (15 sync + 15 async), now ~14 lines (8 helper + 3 sync + 3 async) ✅

### 4.2 Problem 2: Protocol & Metadata Extraction (Complex case - Pattern B)

Now we tackle the complex case with stateful processing and early exit.

#### Current Implementation: Mixed I/O + Computation

```rust
impl LogSegment {
    pub fn protocol_and_metadata(&self, engine: &dyn Engine) 
        -> DeltaResult<(Metadata, Protocol)> {
        // Returns Iterator (lazy - I/O happens on .next())
        let actions_batches = self.replay_for_metadata(engine)?;
        
        let mut metadata_opt = None;
        let mut protocol_opt = None;
        
        // THIS LOOP IS I/O - consuming iterator triggers fetches
        for actions_batch in actions_batches {
            let actions = actions_batch?.actions;
            
            // These are pure computation (on already-fetched data)
            if metadata_opt.is_none() {
                metadata_opt = Metadata::try_new_from_data(actions.as_ref())?;
            }
            if protocol_opt.is_none() {
                protocol_opt = Protocol::try_new_from_data(actions.as_ref())?;
            }
            
            // Early exit when done
            if metadata_opt.is_some() && protocol_opt.is_some() {
                break;
            }
        }
        
        match (metadata_opt, protocol_opt) {
            (Some(m), Some(p)) => Ok((m, p)),
            _ => Err(Error::MissingMetadata),
        }
    }
}
```

**Problems**:
- ~20 lines of logic mixed with I/O loop
- Can't test extraction without I/O
- To add async, must duplicate all ~20 lines
- Can't expose for custom choreography (distributed, cached, etc.)

#### Refactored: Pattern B (Processor + try_fold)

**Step 1: Extract I/O-free processor**

```rust
use std::ops::ControlFlow;

/// I/O-free processor for extracting metadata and protocol from action batches.
/// 
/// This is a state machine that processes batches one at a time and signals
/// when extraction is complete using ControlFlow.
#[derive(Default)]
pub struct MetadataExtractor {
    metadata: Option<Metadata>,
    protocol: Option<Protocol>,
}

impl MetadataExtractor {
    /// Process one batch of actions (pure computation, no I/O).
    ///
    /// Takes self by value, returns:
    /// - `Ok(Break(metadata, protocol))` when both are found
    /// - `Ok(Continue(self))` when more batches are needed
    /// - `Err(...)` on processing errors
    pub fn process_batch(mut self, batch: &ActionsBatch) 
        -> DeltaResult<ControlFlow<(Metadata, Protocol), Self>> {
        
        // Extract metadata if we haven't found it yet (can use ? for errors!)
        if self.metadata.is_none() {
            self.metadata = Metadata::try_new_from_data(batch.actions.as_ref())?;
        }
        
        // Extract protocol if we haven't found it yet (can use ? for errors!)
        if self.protocol.is_none() {
            self.protocol = Protocol::try_new_from_data(batch.actions.as_ref())?;
        }
        
        // Check if we're done
        match self {
            Self { metadata: Some(m), protocol: Some(p) } => Ok(ControlFlow::Break((m, p))),
            processor => Ok(ControlFlow::Continue(processor)),
        }
    }
    
    /// Called when items are exhausted without finding metadata/protocol.
    /// Always returns an error since we only reach here if incomplete.
    pub fn try_finish(self) -> DeltaResult<(Metadata, Protocol)> {
        Err(Error::MissingMetadata)
    }
}
```

**Benefits**:
- ✅ No I/O - can be tested with mock `ActionsBatch`
- ✅ State machine is explicit
- ✅ Takes ownership, avoiding async borrow issues
- ✅ Can use `?` for error handling (returns `Result<ControlFlow<O, Self>>`)
- ✅ Reusable for both sync and async choreography via `try_fold`
- ✅ `try_finish()` handles incomplete state when items exhausted
- ✅ No error allocation on Continue path (optimized hot path)
- ✅ `#[derive(Default)]` - idiomatic, zero-cost initialization

**Step 2: Sync choreography (minimal glue)**

```rust
impl LogSegment {
    /// Read metadata and protocol from log segment (sync choreography).
    ///
    /// Uses try_fold with MetadataExtractor to handle I/O orchestration.
    pub fn read_metadata(&self, engine: &dyn Engine) 
        -> DeltaResult<(Metadata, Protocol)> {
        self.replay_for_metadata(engine)?
            .try_fold(MetadataExtractor::default(), |p, batch| p.process_batch(batch).transpose())
            .unwrap_break_or_else(MetadataExtractor::try_finish)?
    }
}
```

**3 lines of choreography** (vs 20 lines before) ✅

**Changes from current**:
- Uses `try_fold` instead of manual loop
- Extraction logic in `MetadataExtractor` (reusable)
- `.transpose()` (from `TransposeResultToControlFlowExt`) adapts processor for try_fold
- Processor takes ownership, avoiding borrow issues
- `try_finish()` handles incomplete state
- Clean, simple choreography

**Step 3: Async choreography (nearly identical)**

```rust
impl LogSegment {
    /// Read metadata and protocol from log segment (async choreography).
    ///
    /// Uses async try_fold with the SAME MetadataExtractor.
    pub async fn read_metadata_async(&self, engine: &dyn AsyncEngine)
        -> DeltaResult<(Metadata, Protocol)> {
        use futures::stream::TryStreamExt as _;
        
        self.replay_for_metadata_async(engine).await?
            .try_fold(MetadataExtractor::default(), |p, batch| async move {
                p.process_batch(batch).transpose()
            })
            .await
            .unwrap_break_or_else(MetadataExtractor::try_finish)?
    }
}
```

**Duplication reduced from 20 lines to 1 line** (`async move`) ✅

**Result**:
- Extraction logic: ~25 lines (written once)
- Sync choreography: 3 lines
- Async choreography: 5 lines (only differs by `async move` and `.await`)

**Total**: ~33 lines (vs ~40 lines before, and would have been ~60 with naive async duplication) ✅

---

## 5. Implementation Plan

### Phase 0: Foundation - Extension Traits (Day 1)

**Deliverables**:
- [ ] Create `kernel/src/control_flow_ext.rs` with two extension traits:
  - `ResultExt<T, E>` with `.transpose()` method (constrained to `Result<ControlFlow>`)
  - `ControlFlowExt<B, C>` with `.unwrap_break_or_else()` and `.unwrap_break_or()` methods
- [ ] Add comprehensive unit tests for both traits
- [ ] Add module-level documentation with examples
- **Effort**: 1 day
- **Priority**: High - needed for all subsequent phases

### Phase 1: Extract Processors (Week 1)

**Deliverables**:
- [ ] Create `MetadataExtractor` struct (I/O-free processor using `Result<ControlFlow>`)
- [ ] Add `LastCheckpointHint::from_file_result` helper (I/O-free parser)
- [ ] Unit tests for both (with mock data, no I/O!)
- **Effort**: 2-3 days
- **Depends on**: Phase 0 (ControlFlowExt)

### Phase 2: Refactor Sync Choreography (Week 1-2)

**Deliverables**:
- [ ] Refactor `LogSegment::read_metadata()` to use `try_fold` + `MetadataExtractor`
- [ ] Refactor `LastCheckpointHint::try_read()` to use `from_file_result` helper
- [ ] Ensure existing tests pass (behavior unchanged)
- [ ] Add tests showing processor reuse
- **Effort**: 2-3 days
- **Depends on**: Phase 1 (Processors)

### Phase 3: Add Async Choreography (Week 2-3)

**Deliverables**:
- [ ] Add `LogSegment::read_metadata_async()` using async `try_fold` + same processor
- [ ] Add `LastCheckpointHint::try_read_async()` using same helper
- [ ] Requires: `AsyncEngine` trait or feature-gated async
- [ ] Tests for async variants
- **Effort**: 3-4 days
- **Depends on**: Phase 2 (Sync choreography as template)

### Phase 4: Documentation & Examples (Week 3-4)

**Deliverables**:
- [ ] Document processor pattern in module docs
- [ ] Document choreography pattern in module docs
- [ ] Example: custom choreography (cached, distributed)
- [ ] Example: testing processors without I/O
- **Effort**: 2-3 days

**Total**: 3 weeks for complete refactoring of Control Flow 1

---

## 6. Key Architectural Decisions

### Decision 1: Processors Are Public API

**Question**: Should `MetadataExtractor` be public or internal?

**Recommendation**: Make it public (or `#[internal_api]`)
- **Benefit**: Power users can write custom choreography
- **Benefit**: Enables testing without I/O
- **Cost**: More API surface
- **Mitigation**: Clear documentation that most users should use choreography methods

### Decision 2: Async Variants Now or Later?

**Question**: Add async methods in Phase 1 or wait for user demand?

**Recommendation**: Add in Phase 1 (while refactoring anyway)
- **Benefit**: Proves the pattern works
- **Benefit**: Immediate value for async users
- **Cost**: Need `AsyncEngine` trait (but this is needed anyway)
- **Alternative**: Use feature flags to gate async (no runtime cost if not used)

### Decision 3: How Much Duplication Is Acceptable?

**Question**: Is ~3-5 lines of choreography per method (sync + async) acceptable?

**Analysis**:
- Duplication: ~3-5 lines per method × 2 (sync/async) = 6-10 lines
- Shared: All complex logic in processor (written once)
- Alternative: Macro to generate both? (adds complexity)

**Recommendation**: Accept duplication
- It's thin boilerplate (just I/O orchestration)
- Easy to understand and maintain
- Macro would obscure what's happening

### Decision 4: What About Sequential Operations?

**Question**: For operations that MUST be sequential (metadata extraction), is exposing processors worth it?

**Answer**: YES, for these reasons:
1. **Testing**: Can test extraction logic without I/O
2. **Reuse**: Sync and async share same processor
3. **Clarity**: Separates "what to do" from "how to fetch data"
4. **Custom choreography**: Users can write distributed/cached versions
5. **Future-proof**: Instruction DSL (Phase 2 of exploration) can use processors

Even though the operation is sequential, the separation has value.

---

## 7. Success Criteria & Expected Outcomes

### For Simple Users (Sync)

**Before**:
```rust
let snapshot = Snapshot::builder_for(url).build(engine)?;
```

**After** (unchanged):
```rust
let snapshot = Snapshot::builder_for(url).build(engine)?;
```

✅ No breaking changes, no complexity increase

### For Async Users

**Before** (had to use blocking):
```rust
let snapshot = tokio::task::spawn_blocking(|| {
    Snapshot::builder_for(url).build(engine)
}).await??;
```

**After** (native async):
```rust
let snapshot = Snapshot::builder_for(url).build_async(async_engine).await?;
```

✅ Native async support, no blocking threads

### For Power Users

**Before** (couldn't customize):
```rust
// Only option: use provided method
let snapshot = Snapshot::builder_for(url).build(engine)?;
```

**After** (can customize):
```rust
// Use processor directly with custom choreography
let batches = my_custom_fetch_strategy(log_segment);
let (metadata, protocol) = batches
    .try_fold(MetadataExtractor::default(), |p, batch| p.process_batch(batch).transpose())
    .unwrap_break_or_else(MetadataExtractor::try_finish)?;
```

✅ Can write custom choreography

### Technical Metrics

After refactoring Control Flow 1:
- ✅ All processors are I/O-free (unit testable with mock data)
- ✅ Sync choreography uses processors
- ✅ Async choreography uses same processors
- ✅ No code duplication in extraction logic

### Usability Metrics

- ✅ Simple case unchanged (no breaking changes)
- ✅ Async users have native async methods
- ✅ Power users can write custom choreography
- ✅ Clear documentation with examples

### Performance Metrics

- ✅ No regression in sync path
- ✅ Async path avoids blocking threads

---

## 8. Open Questions

### Q1: AsyncEngine Trait Design

How should `AsyncEngine` relate to `Engine`?

**Option A**: Separate trait
```rust
pub trait Engine { /* sync methods */ }
pub trait AsyncEngine { /* async methods */ }
impl Engine for DefaultEngine { /* ... */ }
impl AsyncEngine for DefaultEngine { /* ... */ }
```

**Option B**: Single trait with both
```rust
pub trait Engine {
    fn read_file(&self, ...) -> Iterator<...>;
    async fn read_file_async(&self, ...) -> Stream<...>;
}
```

**Option C**: Feature-gated
```rust
#[cfg(feature = "async")]
pub async fn read_metadata_async(&self, engine: &dyn AsyncEngine) { /* ... */ }
```

**Recommendation**: Option A (separate traits) - clearest separation

### Q2: When to Use Processors vs Direct Calls?

Current code has both:
- `Metadata::try_new_from_data(batch)` - direct call
- `MetadataExtractor` - stateful processor

When to use which?

**Guidelines**:
- Use processor when: stateful, multi-batch, early exit possible
- Use direct call when: stateless, single batch, process all data
- Use helper function when: one-shot operation, no iteration

**Examples**:
- Metadata extraction: Processor (stateful, early exit) ✅
- Checkpoint hint: Helper function (one-shot) ✅
- Schema validation: Direct call (stateless, single pass) ✅

### Q3: How to Handle Errors in Processors?

Should processors return `Result` or panic on invalid input?

**Recommendation**: Return `Result`
- Processors are user-facing (might be public)
- Invalid data is possible (corrupted files)
- Caller should decide how to handle errors

---

## 9. Appendix: Key Learnings & Discovery Process

### 6. Analysis of PR #1160: Two-Phase Checkpoint Processing

[PR #1160](https://github.com/delta-io/delta-kernel-rs/pull/1160) explores distributed log replay with a two-phase approach that's highly relevant to Pattern C.

#### The PR's Approach

**Core Concept**: Split checkpoint processing into two phases:

1. **Phase 1 (Coordinator)**:
   - Processes all commit files (sequential, newest→oldest)
   - Processes single-part checkpoints (V1, V2 manifests)
   - **Does NOT** process multi-part checkpoints or V2 sidecars
   - Returns: `Phase1LogReplay<P>` containing:
     - The processor `P` (with partial state)
     - List of sidecar files to process
     - Iterator that chains commits + initial checkpoint

2. **Phase 2 (Distributed Workers)**:
   - Takes the processor from Phase 1
   - Each worker processes a subset of sidecar files
   - Workers can run in parallel (threads or nodes)
   - User controls work distribution

**Key Code Structure**:

```rust
pub struct Phase1LogReplay<P> {
    processor: P,
    sidecar_files: Vec<ParsedLogPath>,
    commit_actions: Box<dyn Iterator<Item = DeltaResult<ActionsBatch>>>,
    checkpoint_actions: Box<dyn Iterator<Item = DeltaResult<ActionsBatch>>>,
}

impl<P: LogReplayProcessor> Iterator for Phase1LogReplay<P> {
    type Item = DeltaResult<ActionsBatch>;
    
    fn next(&mut self) -> Option<Self::Item> {
        let result = self.commit_actions.next().or_else(|| {
            self.checkpoint_actions
                .next()
                .map(|batch| self.extract_sidecars(batch))
        })?;
        Some(result.and_then(|batch| self.processor.process_actions_batch(batch)))
    }
}
```

#### Comparison to Our Pattern C

**Similarities** ✅:

1. **Two-phase separation**: Both recognize that checkpoint processing has two distinct phases
2. **Processor continuity**: Both pass processor state from phase 1 to phase 2
3. **Sidecar extraction**: Both extract sidecar file lists during checkpoint processing
4. **I/O-free processing**: The processor itself doesn't do I/O
5. **Flexibility**: Both allow caller to control orchestration in phase 2

**Key Differences**:

| Aspect | PR #1160 | Our Pattern C (Refined) |
|--------|----------|-------------------------|
| **Phase boundary** | After single-part checkpoint | After all commits + checkpoint manifest |
| **Processor state** | Mutable, passed through | Immutable (takes `self`, returns `Self`) |
| **Sidecar handling** | Accumulate all, process in phase 2 | Same: accumulate in phase 1, process in phase 2 |
| **Phase 1 interface** | `Phase1LogReplay` is an Iterator (hides I/O) | `phase1_sync/async` choreography (explicit I/O) |
| **Phase 1 state** | Owns iterators internally | `Phase1InProgress` is just data (no iterators) |
| **Early exit** | Must exhaust iterator to check | Type-safe via `Phase1Result::Complete` |
| **Async support** | Problematic (iterators not Sized/Send) | Natural (state is just data, choreography is async fn) |

#### PR's Strengths

1. **Clean phase separation**: Clear boundary between coordinator and distributed work
2. **Distributed-first**: Designed with distributed processing as primary use case
3. **Iterator composability**: `Phase1LogReplay` is an iterator, works with existing APIs
4. **Processor reuse**: Same processor works for both phases
5. **Batched sidecars**: Collecting all sidecars enables better work distribution

#### PR's Challenges (from description)

The PR author identifies several open questions:

1. **Processor broadcasting**: "How to broadcast it, if large and read-only?"
   - Phase 1 processor might be large (e.g., deduplication state in checkpoints)
   - Workers need efficient access to shared state
   
2. **Integration complexity**: "Lots of unanswered questions... integrating into P&M query or incremental snapshot API"
   - Not clear how existing APIs should expose two-phase processing
   - Breaking changes vs. opt-in complexity
   
3. **Checkpoint vs. Log Replay**: "maybe this wasn't the ideal starting point"
   - Distributed checkpointing has additional concerns (write coordination)
   - Log replay might be simpler starting point

#### How Our Refined Pattern C Addresses PR's Challenges

**What we adopted from PR #1160**:

1. **Two-phase separation**: Clear boundary between coordinator and distributed work ✅
   - Phase 1: Sequential processing of commits + checkpoint manifest
   - Phase 2: Parallelizable processing of sidecars
   
2. **Batched sidecars**: Accumulating all sidecars enables ✅:
   - Load balancing (split N sidecars across M workers)
   - Caching (processed sidecars can be cached)
   - Monitoring (track progress across workers)
   
3. **Processor continuity**: Same processor flows from phase 1 to phase 2 ✅

**What we improved**:

1. **No hidden I/O**: Phase 1 choreography is explicit (doesn't hide behind Iterator)
   - Makes async support natural (no `Sized`/`Send` issues)
   - Caller controls when I/O happens
   
2. **Type-safe early exit**: `Phase1Result::Complete` vs `NeedPhase2`
   - Compiler enforces checking if phase 2 is needed
   - PR requires exhausting iterator to discover completion
   
3. **ControlFlow composability**: Reuses Pattern B everywhere
   - Phase 1 choreography = Pattern B (`try_fold` + `ControlFlow`)
   - Phase 2 choreography = nested Pattern B (files × batches)
   - Same techniques, less ad-hoc code
   
4. **Async designed-in from start**: Not an afterthought
   - `phase1_sync` and `phase1_async` differ by only `async move` + `.await`
   - Same for `process_sidecars_sync` and `process_sidecars_async`

**Addressing PR's open questions**:

1. **Processor broadcasting** (large read-only state):
   - ✅ Our processor is immutable (`takes self, returns Self`)
   - ✅ Can be `Clone` for distributed workers if needed
   - ✅ Arc-wrapping is straightforward if read-only
   
2. **Integration complexity**:
   - ✅ Simple case is clean: `phase1_sync(...)?.process_sidecars_sync(...)?`
   - ✅ Power users can match on `Phase1Result` for custom orchestration
   - ✅ No breaking changes - new APIs alongside existing ones
   
3. **Checkpoint vs. Log Replay**:
   - ✅ Pattern applies to both! Phase 1 = log replay, Phase 2 = sidecars
   - ✅ Unified approach reduces code duplication

**The synthesis**: Our refined Pattern C adopts PR #1160's fundamental two-phase insight while fixing its limitations (hidden I/O, async incompatibility, less type safety). See Section 10 for complete design.

#### Key Realizations

1. **The PR's open questions are orthogonal to sync/async**:
   - Processor broadcasting, API integration, work distribution
   - These are **choreography** problems, not processor problems
   - Same challenges exist whether sync or async
   - This validates the processor/choreography separation!

2. **Two-phase makes async *easier*, not harder**:
   - Phase 1: Sequential (commits + manifest) - natural for async Stream
   - Phase 2: Parallel (sidecars) - natural for async join/spawn
   - Clear boundaries = easier to reason about async control flow
   - Async doesn't complicate the pattern, it *fits naturally*

3. **Significant pattern overlap**:
   - Both PR #1160 and our Pattern C tackle the same fundamental problem
   - Both need processor state management and flexible choreography
   - Our refined design adopts the two-phase insight while fixing the Iterator issues

4. **ControlFlow is the missing piece**:
   - PR #1160 used Iterator (hides I/O, async problems)
   - Our Pattern C uses `ControlFlow` (explicit I/O, async-friendly)
   - Both patterns share the same two-phase structure and processor concepts
   - We should **incorporate good ideas** even if C & D change significantly
   - The processor/choreography foundation is sound; the details need work

#### Revised Understanding (Incorporated into Document)

**What we initially got wrong**: Treating PR #1160 as "distributed only" and our initial patterns as "local only"

**Better framing**: PR #1160's two-phase approach is **the fundamental choreography pattern** for checkpoint processing. It naturally supports:
- ✅ Simple sequential case (phase 2 runs on same thread)
- ✅ Parallel case (phase 2 distributes to workers)
- ✅ Sync case (using iterators)
- ✅ Async case (using streams)

**The key insight**: Two-phase isn't about distribution, it's about **separating manifest processing from detail processing**.

**Our refinement**: We adopted the two-phase structure but replaced the Iterator interface with ControlFlow-based state machines, fixing async compatibility issues and making I/O explicit.

**Status**: ✅ These insights have been incorporated into:
- **Section 3.2.2**: Pattern C now uses the refined two-phase + ControlFlow approach
- **Section 10**: Complete design with `Phase1InProgress`, `Phase1Result`, and choreography examples

---

### 7. Deep Dive: Streaming vs. Batched Sidecar Processing

**The trade-off question**: Should we process sidecars **incrementally** (as discovered) or **batched** (after phase 1 completes)?

**Original Pattern C approach**:
```rust
for checkpoint_batch in checkpoint_manifest {
    process_checkpoint_batch(batch)?;
    
    // Immediately process sidecars discovered in this batch
    if let Some(request) = extract_sidecars(batch)? {
        for sidecar_batch in read_sidecars(request)? {
            process_sidecar(sidecar_batch)?;
        }
    }
}
```

**Two-phase approach**:
```rust
// Phase 1: Accumulate all sidecars
let mut sidecar_list = Vec::new();
for checkpoint_batch in checkpoint_manifest {
    process_checkpoint_batch(batch)?;
    if let Some(sidecars) = extract_sidecars(batch)? {
        sidecar_list.extend(sidecars);
    }
}

// Phase 2: Process all sidecars
for sidecar in sidecar_list {
    process_sidecar(read_file(sidecar)?)?;
}
```

**Theoretical advantage of incremental** (Pattern C):
- Start fetching sidecar files while still reading manifest
- Potential for I/O parallelism (reading manifest + sidecars concurrently)
- Lower latency to first sidecar result
- More "streaming" feel

**Practical reality check**:

1. **Checkpoint manifest structure** (from Delta spec):
   ```
   V2 Checkpoint:
   - Main file: Contains checkpoint metadata action + sidecar references
   - Sidecar files: Contain the actual file actions (add/remove)
   ```
   
   **Key insight**: The manifest is a **single file** that lists sidecars. You must read the entire file to know all sidecars. There's no "stream of manifest batches" - there's one manifest file.

2. **Multi-part checkpoints**:
   - V1 multi-part: Each part is independent (no manifest)
   - Each part can contain file actions directly
   - These ARE processed incrementally (already!)
   
3. **V2 checkpoint manifest size**:
   - Contains: checkpoint metadata + list of sidecar file paths
   - Typical size: A few KB (just metadata + paths)
   - Does NOT contain actual file actions
   - Reading it is essentially instantaneous

**Analysis of incremental approach**:

**When would it help?**
- Manifest file is very large (many batches)
- Network has high latency (want to start sidecars early)
- Sidecars are also slow to fetch (want parallelism)

**Reality check**:
- **Manifest is not large**: Just metadata + paths, typically < 1MB
- **Reading manifest takes ~milliseconds**: Network round trip dominates
- **Sidecars are the bottleneck**: Many files, large sizes, most time spent here
- **Incremental processing of manifest**: Saves at most a few milliseconds

**When would it hurt?**
- Adds complexity to choreography (nested iteration)
- Harder to reason about (interleaved phases)
- Breaks clean separation needed for distribution
- Makes parallelization awkward (some sidecars already processing)

**Concrete example** (realistic cloud storage parameters):

```
Network characteristics:
- Time to first byte (TTFB): 50-100ms
- Throughput (single fetch): 50-80 MB/s

Checkpoint structure:
- manifest.parquet (150-200 KB, lists 500 sidecars)
- sidecar_1.parquet (10 MB)
- sidecar_2.parquet (10 MB)
- ...
- sidecar_500.parquet (10 MB)

Total: 5 GB of sidecars, 200 KB manifest
```

**Incremental approach timing**:
```
T=0ms:    Start reading manifest batch 1
T=75ms:   Finish manifest batch 1 (100 KB), discover 250 sidecars
           TTFB: 100ms + transfer: 100KB/50MB/s ≈ 2ms ≈ 75ms total
T=75ms:   Start reading sidecar_1 (async prefetch if supported!)
T=150ms:  Finish manifest batch 2 (100 KB), discover 250 more sidecars
T=375ms:  Finish sidecar_1
           TTFB: 100ms + transfer: 10MB/50MB/s = 200ms = 300ms total
T=675ms:  Finish sidecar_2
...
Total time for 500 sidecars: ~150 seconds (500 × 300ms)
```

**Two-phase approach timing**:
```
T=0ms:    Start reading manifest
T=100ms:  Finish reading manifest (200 KB), discovered 500 sidecars
           TTFB: 100ms + transfer: 200KB/50MB/s ≈ 4ms ≈ 100ms total
T=100ms:  Start reading sidecar_1
T=400ms:  Finish sidecar_1
T=700ms:  Finish sidecar_2
...
Total time for 500 sidecars: ~150 seconds (500 × 300ms)
```

**Difference**: ~25-50ms saved by starting first sidecar earlier (incremental)
- This is **0.03%** of total time (50ms / 150,000ms)
- Requires concurrent I/O (fetch sidecar while reading manifest)
- Only saves 1-2 sidecar latencies worth of time

**But wait!** The incremental approach only helps if:
1. You can **actually parallelize** manifest + sidecar reads (concurrent I/O)
2. The engine supports **request pipelining** (async or multi-threaded)
3. The manifest is **large enough** that incremental reading matters (not the case: 200 KB)

**For most checkpoints**:
- Manifest read time: ~100ms (TTFB dominated)
- Single sidecar time: ~300ms (100ms TTFB + 200ms transfer)
- Total sidecar time: 150+ seconds (500 × 300ms)
- Incremental advantage: < 0.05 seconds = **< 0.03%** improvement
- Manifest is 0.004% of total data (200 KB / 5 GB)

#### What About Massive Parallelism?

**Question**: If we process sidecars in parallel, does the manifest read time become significant?

Let's examine two scenarios:

**Scenario 1: 32 cores (realistic beefy server/workstation)**

```
Parallelism: 32 workers
Rounds needed: 500 sidecars / 32 = 16 rounds (round up)

Round 1:  32 sidecars in parallel, TTFB overlapped
          Time: 300ms per round (same as single sidecar)
Round 2:  Next 32 sidecars
          Time: 300ms
...
Round 16: Final 20 sidecars
          Time: 300ms

Total phase 2 time: 16 × 300ms = 4.8 seconds
```

**Timing comparison**:
- **Incremental**: Start first round at T=50ms → finish at T=4.85s
- **Two-phase**: Start first round at T=100ms → finish at T=4.9s
- **Difference**: 50ms out of 4900ms = **1.0%**

**Scenario 2: 256 cores (extreme distributed case)**

```
Parallelism: 256 workers
Rounds needed: 500 sidecars / 256 = 2 rounds

Round 1:  256 sidecars in parallel
          Time: 300ms
Round 2:  Remaining 244 sidecars  
          Time: 300ms

Total phase 2 time: 2 × 300ms = 0.6 seconds
```

**Timing comparison**:
- **Incremental**: Start first round at T=50ms → finish at T=0.65s
- **Two-phase**: Start first round at T=100ms → finish at T=0.7s
- **Difference**: 50ms out of 700ms = **7.1%**

**Analysis**:

1. **Absolute difference is constant**: ~50ms saved (max), regardless of parallelism
2. **Relative percentage grows** with more parallelism (1% → 7%)
3. **But two-phase advantages become MORE important** with parallelism:
   - Need full sidecar list for optimal load balancing
   - Need total count for work distribution (500 / 256 workers)
   - Need all file sizes for smart scheduling
   - Can't pre-check cache without full list
   - Can't track progress (N of M complete) without knowing M

**Incremental can't optimize early batches**:
- First manifest batch discovers 250 sidecars
- Start processing them... but wait, there might be 250 more!
- Can't split work optimally (don't know total count)
- Workers may finish early rounds while manifest still reading
- Suboptimal load balancing

**Conclusion**: Even at 256 cores where manifest time is 7% of total, two-phase is still better because:
- Absolute difference is tiny (50-100ms)
- Can't load balance optimally without full sidecar list
- Incremental approach loses all parallelization advantages
- Simpler is better unless performance difference is significant

**Verdict**: Parallelism doesn't change the conclusion - two-phase wins across the entire spectrum from 1 core to 256+ cores.

#### Verdict: Two-Phase Wins

**Recommendation**: Use two-phase approach (batch sidecars), not incremental (stream sidecars)

**Reasons**:

1. **Manifest is tiny**: Reading entire manifest first adds negligible latency
   - Sequential: < 0.03% of total time (50ms / 150s)
   - 32 cores: 1% of total time (50ms / 4.9s)
   - 256 cores: 7% of total time (50ms / 0.7s)
   - Even in extreme parallel case, absolute difference is only 50-100ms

2. **Simpler reasoning**: Clean phase boundary makes code easier to understand and debug

3. **Better for parallelization**: Having all sidecars up front enables:
   - Load balancing (split 500 sidecars across N workers optimally)
   - Smart scheduling (process large files first, small files last)
   - Caching (skip already-processed sidecars)
   - Progress tracking (N of 500 complete)
   - Error recovery (retry failed sidecars)
   - **Incremental approach can't optimize without full list**

4. **Async doesn't change the math**: 
   - Async makes concurrent I/O easier, but...
   - You still need the full sidecar list to orchestrate optimally
   - Starting a few sidecars early doesn't help if you can't parallelize them

5. **Spec supports it**: Delta checkpoint design assumes manifest is small
   - Manifests with sidecars don't contain file actions
   - If you need streaming, use multi-part V1 checkpoints (already incremental)

6. **Litmus test**: Can we express incremental in terms of two-phase?
   ```rust
   // "Incremental" using two-phase (if someone really wants it)
   let phase1 = start_phase1(engine, processor)?;
   
   // Process sidecars one at a time (no batching)
   for sidecar in phase1.sidecar_files {
       processor = processor.process_sidecar(read_file(sidecar)?)?;
   }
   ```
   ✅ Yes! Two-phase is more general.

7. **Can't express two-phase in terms of incremental**:
   ```rust
   // Try to batch using incremental pattern?
   let mut sidecars = Vec::new();
   for batch in manifest {
       if let Some(req) = process_batch(batch)? {
           sidecars.extend(req.files);  // Have to accumulate anyway!
           // Can't process yet, need to wait for all batches
       }
   }
   // Now process all sidecars... but we just reinvented two-phase!
   ```
   ❌ No! You end up recreating two-phase anyway.

#### Complexity Cost/Benefit

**Incremental approach complexity**:
- Nested iteration (outer: manifest, inner: sidecars per batch)
- Interleaved processing (some sidecars done, more being discovered)
- Harder to parallelize (how do you distribute partial work?)
- More state to track (which sidecars from which batch)
- Breaks clean phase separation

**Benefit**: Save ~10-50ms on a multi-minute operation (< 0.1%)

**Two-phase complexity**:
- Two clear phases (easier to reason about)
- All sidecars known before processing (easier to parallelize)
- Clean checkpoint: "manifest complete, now process details"
- Processor state transfers cleanly between phases

**Benefit**: Simpler code, better parallelization, negligible time cost

**Verdict**: Complexity cost of incremental is **not justified** by the negligible performance gain.

#### When Would Incremental Make Sense?

Hypothetical scenario where incremental would help:

1. **Manifest is huge**: 100+ MB, takes seconds to read
2. **Manifest is actually streamed**: Not a single file, but a sequence
3. **Sidecars are small**: Can fully process each before next manifest batch
4. **No parallelization needed**: Single-threaded processing

**Reality**: This doesn't match Delta checkpoint design:
- Manifests are small by design (just metadata + paths)
- Manifests are single files (not streams)
- Sidecars are large (GBs of file actions)
- Parallelization is a key use case

**Conclusion**: Don't optimize for a theoretical case that doesn't match real-world usage.

#### Final Recommendation

✅ **Use two-phase approach**: Clean separation, better for parallelization, simpler code, < 0.1% time difference

❌ **Avoid incremental approach**: Complex, hard to parallelize, negligible benefit given Delta checkpoint design

**Document should**:
- Present two-phase as the pattern
- Explain why (manifest small, clean separation, parallel-friendly)
- Note that multi-part V1 checkpoints already support incremental (different use case)
- Don't over-engineer for theoretical streaming that checkpoint design doesn't need

---

### Pattern Mechanics Summary

**Key Design Decisions** (full details in Section 3.2.2):

1. **`try_fold` + `ControlFlow`**: Reduced sync/async choreography duplication from ~18 lines to ~1 line
2. **`Result<ControlFlow>` signature**: Allows using `?` naturally inside processors (more idiomatic than `ControlFlow<Result>`)
3. **Extension traits**: `ResultExt::transpose()` and `ControlFlowExt::unwrap_break_or_else()` bridge between processor API and `try_fold` requirements
4. **When to use**: Stateful processing with early exit + need both sync and async versions
5. **When not to use**: One-shot operations (use Pattern A), no early exit (use `fold`), stateless (use `map`)

**Trade-off**: Slightly higher learning curve (`try_fold` + `ControlFlow`) for massive reduction in duplication and improved testability.

---

## 10. Appendix: Phase 1 Design Evolution - From Iterator to ControlFlow

### The Iterator Problem

The initial design for Pattern C's Phase 1 (inspired by PR #1160) used an iterator interface:

```rust
pub struct Phase1LogReplay<P> {
    processor: P,
    sidecar_files: Vec<FileMeta>,
    remaining_actions: Box<dyn Iterator<...>>,  // ❌ Problem!
}

impl<P> Iterator for Phase1LogReplay<P> {
    type Item = DeltaResult<ActionsBatch>;
    fn next(&mut self) -> Option<Self::Item> { ... }
}
```

**Problems identified**:

1. **Hides I/O behind Iterator**: Same problem we're trying to solve elsewhere!
2. **Can't own async iterator**: Not `Sized`, not `Send`, lifetime issues
3. **Dual purpose hidden**: Returns batches to user AND accumulates sidecars internally
4. **Type safety missing**: Can't enforce "must consume phase 1 before phase 2"

### The ControlFlow Solution

**Key insight**: Phase 1 has dual output (batches to user + internal state for phase 2). Solution: Make it explicit with `ControlFlow`-based state machine.

**Complete design and code**: See Section 3.2.2 for full `Phase1InProgress`, `Phase1Result`, `From` impls, `phase1_sync`, `phase1_async`, and usage examples.

### Comparison to Iterator Approach (PR #1160)

| Aspect | Iterator Approach | ControlFlow Approach |
|--------|------------------|---------------------|
| **I/O visibility** | Hidden in Iterator | Explicit in choreography |
| **Async support** | Problematic (not Sized/Send) | Natural (state is just data) |
| **Type safety** | Runtime (must consume iterator) | Compile-time (`ControlFlow` enforces) |
| **Dual output** | Hidden (batches via Iterator, sidecars internal) | Explicit (`process_batch` parameter vs return) |
| **Pattern reuse** | Custom pattern | Reuses Pattern B everywhere |
| **Early exit** | Must check after iterator exhausted | Type-safe via `Phase1Result::Complete` |
| **State construction** | Choreography must know struct/enum details | `From` impls hide data layout (`processor.into()`) |

### Key Takeaways

1. **Don't hide I/O behind iterators** - especially not internal ones
2. **ControlFlow for state machines** - Perfect for "continue" vs "done" semantics
3. **Pattern B is a building block** - Phase 1 choreography uses Pattern B, Phase 2 uses nested Pattern B
4. **Type states enforce protocols** - Can't access phase 2 data without completing phase 1
5. **`From` impls maximize separation** - Choreography uses `.into()` instead of constructing structs/enums
6. **Pattern C = Pattern B composed** - Elegant proof of composability

---

## 11. Appendix: Evolution from Initial Approach

### What Changed

**Initial approach**: "Make APIs take Vec instead of Iterator"
- **Problem**: Just shifts I/O to caller, wastes memory
- **Insight**: Didn't recognize iterator consumption = I/O

**Current approach**: "Extract I/O-free processors, add thin choreography"
- **Solution**: Separate computation from I/O orchestration
- **Benefit**: Testable, reusable, supports sync and async

### What Stayed the Same

- LogSegment validation is I/O-free ✅ (was right)
- Metadata/Protocol parsing is I/O-free ✅ (was right)

### What Was Wrong

- Thinking Vec solves the problem ❌
- Not recognizing `for batch in iter` is I/O ❌
- Suggesting `IntoIterator` helps ❌ (it doesn't for this use case)

### What Was Missing

- The processor pattern (computation as state machine)
- Clear choreography separation  
- Understanding that some duplication is OK (boilerplate)
- Recognizing value even for sequential operations (testing, reuse)
- **The `try_fold` + `ControlFlow` pattern**
- **Generic extension traits with constrained methods** (`ResultExt`, `ControlFlowExt`) for minimizing choreography duplication
- **The `Into` constraint trick** for expressing type equality in trait bounds

---

## Next Steps

This refactoring guide for Control Flow 1 should be used as the template for analyzing the remaining control flows:
- Control Flow 2: Table Scan
- Control Flow 3: Transaction Commit
- Control Flow 4: Domain Metadata
- Control Flow 5: Checkpoint Writing

For each, apply the same pattern:
1. Identify where I/O happens (iterator consumption!)
2. Categorize: Pure computation vs Pure I/O vs Mixed choreography
3. For mixed choreography, choose appropriate pattern:
   - Pattern A (Helper Functions) for one-shot operations
   - Pattern B (Processor + try_fold) for iterative processing
4. Extract I/O-free processors/helpers
5. Implement sync choreography
6. Implement async choreography (minimal duplication)
7. Document power user APIs

