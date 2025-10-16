# Control Flow 1: Snapshot Creation - Refactoring Guide

## Table of Contents

### Part I: Understanding the Problem (~15 min)
- [1. Executive Summary](#1-executive-summary)
- [2. Why Current Code Blocks Async](#2-why-current-code-blocks-async)
  - [2.1 The Iterator Problem](#21-the-iterator-problem)
  - [2.2 Simplified Control Flow](#22-simplified-control-flow)
  - [2.3 Why We Can't Just Add .await](#23-why-we-cant-just-add-await)
  - [2.4 Why Materialization Doesn't Help](#24-why-materialization-doesnt-help)
  - [2.5 The Real Solution](#25-the-real-solution)
- [3. The Solution: Processors + Choreography](#3-the-solution-processors--choreography)
  - [3.1 Core Principle](#31-core-principle)
  - [3.2 Three Patterns for Three Problem Types](#32-three-patterns-for-three-problem-types)
  - [3.3 Quick Example: Pattern B](#33-quick-example-pattern-b)

### Part II: Pattern Details (~30 min)
- [4. Pattern A: Helper Functions](#4-pattern-a-helper-functions)
  - [4.1 The Pattern](#41-the-pattern)
  - [4.2 Example: LastCheckpointHint](#42-example-lastcheckpointhint)
- [5. Pattern B: Processor + try_fold](#5-pattern-b-processor--try_fold)
  - [5.1 The Pattern](#51-the-pattern)
  - [5.2 Why This Works](#52-why-this-works)
  - [5.3 Example: MetadataExtractor](#53-example-metadataextractor)
- [6. Pattern C: Two-Phase Processing](#6-pattern-c-two-phase-processing)
  - [6.1 Why Two-Phase?](#61-why-two-phase)
  - [6.2 The Pattern](#62-the-pattern)
  - [6.3 Usage Examples](#63-usage-examples)

### Part III: Implementation Guide (~20 min)
- [7. Current State Analysis (Detailed)](#7-current-state-analysis-detailed)
- [8. Refactored Control Flow](#8-refactored-control-flow)
- [9. Async Foundation Requirements](#9-async-foundation-requirements)
- [10. Complete Pattern C Implementation](#10-complete-pattern-c-implementation)
- [11. Implementation Roadmap](#11-implementation-roadmap)
- [12. Success Criteria](#12-success-criteria)

### Part IV: Deep Dives & Reference (on-demand)
- [Appendix A: Refactoring Principles](#appendix-a-refactoring-principles)
- [Appendix B: Extension Traits](#appendix-b-extension-traits)
- [Appendix C: Current State Deep Dive](#appendix-c-current-state-deep-dive)
- [Appendix D: Refactored Control Flow (Detailed)](#appendix-d-refactored-control-flow-detailed)
- [Appendix E: Async Trait Definitions](#appendix-e-async-trait-definitions)
- [Appendix F: ControlFlowStreamExt Implementation](#appendix-f-controlflowstreamext-implementation)
- [Appendix G: Pattern C Support Code](#appendix-g-pattern-c-support-code)
- [Appendix H: Pre-Implementation Analysis](#appendix-h-pre-implementation-analysis)
- [Appendix I: Cooperative Yielding](#appendix-i-cooperative-yielding)
- [Appendix J: Pattern A Complete Example](#appendix-j-pattern-a-complete-example)
- [Appendix K: Pattern B Complete Example](#appendix-k-pattern-b-complete-example)
- [Appendix L: Two-Phase vs Incremental Analysis](#appendix-l-two-phase-vs-incremental-analysis)
- [Appendix M: PR #1160 Analysis](#appendix-m-pr-1160-analysis)
- [Appendix N: Key Architectural Decisions](#appendix-n-key-architectural-decisions)
- [Appendix O: Risk Management & Open Questions](#appendix-o-risk-management--open-questions)
- [Appendix P: Design Evolution](#appendix-p-design-evolution)
- [Appendix Q: Phase 1 Design Evolution](#appendix-q-phase-1-design-evolution)

💡 **Tip**: Use your browser's back button to return after clicking links.

---

## 1. Executive Summary

This document provides a refactoring plan to add async support to snapshot building in `delta-kernel-rs` while maintaining sync compatibility and avoiding massive code duplication.

### The Problem

Current snapshot building code mixes I/O orchestration with business logic, making it impossible to add async support without duplicating everything. The core issue: **iterator consumption IS I/O** - calling `.next()` triggers file reads.

### The Solution

**Separate computation from choreography** using processors:
- **Processors**: I/O-free state machines containing business logic (testable, reusable)
- **Choreography**: Thin wrappers that feed data to processors (sync or async)

Three patterns handle different problem types:
- **Pattern A**: Helper functions for one-shot operations
- **Pattern B**: Processor + `try_fold` for iterative processing with early exit
- **Pattern C**: Two-phase processing for manifest + detail files

### Key Results

- **15% code duplication** (vs 50% if duplicating everything)
- **65% of code is shared** between sync and async
- **Only 35% of functions** need async variants (thin choreography)
- **All business logic is testable** without I/O mocks

### Implementation

**Timeline**: 10-12 weeks across 5 phases
- Design async trait hierarchy
- Extract I/O-free processors
- Refactor sync choreography
- Add async choreography
- Integration and testing

See [Section 6](#6-implementation-phases) for detailed roadmap.

### Document Structure

**Part I: Understanding the Problem** (~15 min)
- Why current code blocks async
- The processor/choreography solution
- Three pattern overview

**Part II: Pattern Details** (~30 min)
- Pattern A, B, C with examples
- When to use each pattern

**Part III: Implementation Guide** (~20 min)
- Refactored control flow
- Async foundation requirements
- Implementation roadmap
- Success criteria

**Part IV: Deep Dives & Reference** (on-demand)
- Complete code examples
- Support code implementations
- Performance analyses
- Design evolution

---

# Part I: Understanding the Problem

## 2. Why Current Code Blocks Async

### 2.1 The Iterator Problem

The core issue is subtle but fundamental:

```rust
// This LOOKS like pure computation...
for batch in iterator {  // ← But .next() triggers file reads!
    let metadata = extract_metadata(&batch);  // ← THIS is the computation
    if is_complete(metadata) {
        return Ok(metadata);
    }
}
```

**Key insight**: Iterator consumption IS I/O. We can't separate computation from I/O by changing what the iterator returns - the act of iterating is the I/O operation.

### 2.2 Simplified Control Flow

Current snapshot building has three levels of concern:

```
1. High-level API: Snapshot::builder_for(url).build(engine)
   ↓
2. Coordinator: Orchestrate commits + checkpoints + sidecars
   ↓  
3. Business Logic: Extract metadata, protocol, validate, accumulate state
```

**The problem**: Level 2 and 3 are mixed together in methods like `protocol_and_metadata()`:
- Iterator consumption (I/O) interleaved with extraction logic (computation)
- ~20 lines of stateful logic that would need duplication for async
- Can't test extraction without I/O mocks

### 2.3 Why We Can't Just Add .await

Some might suggest: "Just make the methods async!"

```rust
// ❌ Doesn't work - can't conditionally make methods async
pub trait Engine {
    fn read_file(&self, ...) -> Iterator<...>;      // Sync
    async fn read_file(&self, ...) -> Stream<...>;  // Can't have both!
}
```

The method signature must be **either** `fn` **or** `async fn` - you can't make it conditional on a type parameter. Sync and async require fundamentally different return types:
- Sync: Returns `T` immediately
- Async: Returns `impl Future<Output = T>` 

### 2.4 Why Materialization Doesn't Help

Another approach: "Just collect everything into a Vec!"

```rust
// ❌ Bad: Just moves the blocking around
fn read_metadata(&self, engine: &dyn Engine) -> Result<Metadata> {
    let batches: Vec<_> = self.get_batches(engine)?.collect()?;  // ← Blocks HERE
    process_batches(&batches)  // ← Now I/O-free, but too late
}
```

**Problems**:
- Blocks during `collect()` instead of during iteration - no improvement
- Wastes memory (forces all data into RAM)
- Loses opportunity for early exit (must collect everything first)
- Doesn't actually separate I/O from computation

### 2.5 The Real Solution

We need to **extract** the computation (Level 3) from the choreography (Level 2):

```rust
// ✅ Processor: Pure computation, no I/O
struct MetadataExtractor { /* state */ }
impl MetadataExtractor {
    fn process(self, batch: &Batch) -> Result<ControlFlow<Output, Self>> {
        // All business logic here - testable without I/O!
    }
}

// ✅ Choreography: Just feeds data (sync)
fn read_metadata_sync(&self, engine: &dyn Engine) -> Result<Output> {
    iterator.try_fold(MetadataExtractor::default(), |p, b| {
        p.process(b).transpose()  // Processor does the work
    })
}

// ✅ Choreography: Just feeds data (async) - nearly identical!
async fn read_metadata_async(&self, engine: &dyn AsyncEngine) -> Result<Output> {
    stream.try_fold(MetadataExtractor::default(), |p, b| async move {
        p.process(b).transpose()  // Same processor!
    }).await
}
```

**Key benefits**:
- Business logic written once (processor)
- Choreography is thin boilerplate (~3-5 lines)
- Processor is testable without I/O
- Only ~1 line differs between sync and async choreography

---

## 3. The Solution: Processors + Choreography

### 3.1 Core Principle

**Separate WHAT to do (processor) from HOW to fetch data (choreography)**

- **Processor**: I/O-free state machine containing business logic
  - Takes fetched data as input
  - Returns: "continue with updated state" or "done, here's the result"
  - Testable without I/O mocks
  - Shared between sync and async
  
- **Choreography**: Thin wrapper that orchestrates I/O
  - Fetches data (sync Iterator or async Stream)
  - Feeds data to processor
  - Handles results
  - Sync and async versions differ by only `async move` + `.await`

### 3.2 Three Patterns for Three Problem Types

| Problem Type | Pattern | When to Use | Example |
|--------------|---------|-------------|---------|
| **One-shot operation** | A: Helper Function | Read file → parse → done | `LastCheckpointHint` |
| **Iterative with early exit** | B: Processor + try_fold | Multi-batch, stateful, can complete early | `MetadataExtractor` |
| **Manifest + details** | C: Two-Phase | Tiny manifest references large detail files | Checkpoint + sidecars |

### 3.3 Quick Example: Pattern B

Here's Pattern B (the most common) with concept code only:

```rust
// CONCEPT CODE: The processor (I/O-free, testable, reusable)
#[derive(Default)]
struct MetadataExtractor {
    metadata: Option<Metadata>,
    protocol: Option<Protocol>,
}

impl MetadataExtractor {
    fn process(mut self, batch: &Batch) 
        -> Result<ControlFlow<(Metadata, Protocol), Self>> 
    {
        // Can use ? for errors naturally
        if self.metadata.is_none() {
            self.metadata = Metadata::try_new_from_data(batch)?;
        }
        if self.protocol.is_none() {
            self.protocol = Protocol::try_new_from_data(batch)?;
        }
        
        // Signal completion or continuation
        match (self.metadata, self.protocol) {
            (Some(m), Some(p)) => Ok(ControlFlow::Break((m, p))),
            _ => Ok(ControlFlow::Continue(self))
        }
    }
}

// CONCEPT CODE: Sync choreography (3 lines)
iterator
    .try_fold(MetadataExtractor::default(), |p, batch| {
        p.process(batch).transpose()  // ¹
    })
    .unwrap_break_or_else(MetadataExtractor::try_finish)?  // ²

// CONCEPT CODE: Async choreography (5 lines - only differs by async move + .await)
stream
    .try_fold(MetadataExtractor::default(), |p, batch| async move {
        p.process(batch).transpose()  // ¹ same
    })
    .await  // ← only difference
    .unwrap_break_or_else(MetadataExtractor::try_finish)?  // ² same
```

**Notes**:
- ¹ `.transpose()` converts processor result type for try_fold (see [Appendix B](#appendix-b-extension-traits))
- ² `.unwrap_break_or_else()` handles the "ran out of items" case (see [Appendix B](#appendix-b-extension-traits))

**Key Insight**: All business logic is in the processor. Choreography is ~3-5 lines of boilerplate that's nearly identical for sync and async.

---

# Part II: Pattern Details

## 4. Pattern A: Helper Functions

**When to use**: One-shot operations (read file → parse → done)

### 4.1 The Pattern

**Structure**:

```rust
// CONCEPT CODE: I/O-free helper takes Result<Data>, handles all cases
impl Thing {
    fn from_file_result(result: DeltaResult<Data>) -> DeltaResult<Output> {
        match result {
            // All computation here (parsing, validation, transformation)
            Ok(data) => {
                let processed = process(data)?;  // Can use ? naturally
                Ok(transform(processed))
            }
            // Handle expected errors (e.g., file not found)
            Err(Error::NotFound) => Ok(Output::default()),
            // Propagate unexpected errors
            Err(e) => Err(e),
        }
    }
}

// CONCEPT CODE: Sync choreography (minimal - just I/O + helper call)
pub fn read(storage: &dyn Storage, path: &Path) -> DeltaResult<Output> {
    Thing::from_file_result(storage.read_file(path))
}

// CONCEPT CODE: Async choreography (only .await differs!)
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

### 4.2 Example: LastCheckpointHint

See [Appendix J](#appendix-j-pattern-a-complete-example) for complete before/after code with the `LastCheckpointHint` implementation.

**When NOT to use**:
- ❌ Multi-batch processing (use Pattern B)
- ❌ Iteration with state (use Pattern B)
- ❌ Complex conditional logic (consider Pattern B or C)

---

## 5. Pattern B: Processor + try_fold

**When to use**: Iterative processing with stateful accumulation and early exit

### 5.1 The Pattern

**The Challenge**: Sync and async loops would duplicate significant code (~10-20 lines of business logic).

**The Solution**: Extract the state machine into a processor, use `try_fold` to feed it data.

**Structure**:

```rust
// CONCEPT CODE: The processor (I/O-free state machine)
#[derive(Default)]
pub struct MyProcessor {
    // State fields (what we're accumulating)
}

impl MyProcessor {
    /// Process one item. Returns Continue (need more) or Break (done).
    pub fn process(mut self, item: &Item) 
        -> Result<ControlFlow<Output, Self>> 
    {
        // ✅ Can use ? for error handling naturally
        let data = fallible_operation(item)?;
        
        // ✅ Update internal state
        self.accumulate(data);
        
        // ✅ Decide: are we done or need more?
        if self.is_complete() {
            Ok(ControlFlow::Break(self.into_output()))
        } else {
            Ok(ControlFlow::Continue(self))
        }
    }
    
    /// Handle case where iterator/stream exhausts before completion
    pub fn try_finish(self) -> Result<Output> {
        if self.is_complete() {
            Ok(self.into_output())
        } else {
            Err(Error::Incomplete)
        }
    }
}

// CONCEPT CODE: Sync choreography (3 lines)
iterator
    .try_fold(MyProcessor::default(), |p, item| {
        p.process(item).transpose()  // ¹
    })
    .unwrap_break_or_else(MyProcessor::try_finish)?  // ²

// CONCEPT CODE: Async choreography (5 lines - only differs by async move + .await)
stream
    .try_fold(MyProcessor::default(), |p, item| async move {
        p.process(item).transpose()  // ¹ same
    })
    .await  // ← only difference
    .unwrap_break_or_else(MyProcessor::try_finish)?  // ² same
```

**Notes**:
- ¹ `.transpose()` converts `Result<ControlFlow<O, S>>` (what processor returns) to `ControlFlow<Result<O>, S>` (what try_fold needs). See [Appendix B](#appendix-b-extension-traits) for implementation.
- ² `.unwrap_break_or_else()` handles the "ran out of items" case by calling `try_finish()`. See [Appendix B](#appendix-b-extension-traits) for implementation.

### 5.2 Why This Works

The processor signature `Result<ControlFlow<Output, Self>>` lets you:
- Use `?` for error handling inside the processor (natural Rust)
- Signal completion via `Break` or continuation via `Continue`
- Take ownership of `self` (avoiding async borrow checker issues)

The `try_fold` method works for both:
- `Iterator::try_fold` (sync) - uses unstable `Try` trait
- Custom `ControlFlowStreamExt::try_fold` (async) - see [Appendix F](#appendix-f-controlflowstreamext-implementation)

**Key Insight**: Only ~1 line differs between sync and async choreography (`async move` + `.await`). All business logic is in the shared processor.

### 5.3 Example: MetadataExtractor

Here's a realistic example processing Delta log actions:

```rust
// CONCEPT CODE: Extract metadata and protocol from action batches
#[derive(Default)]
pub struct MetadataExtractor {
    metadata: Option<Metadata>,
    protocol: Option<Protocol>,
}

impl MetadataExtractor {
    pub fn process(mut self, batch: &ActionsBatch) 
        -> DeltaResult<ControlFlow<(Metadata, Protocol), Self>> 
    {
        // Extract metadata if not found yet
        if self.metadata.is_none() {
            self.metadata = Metadata::try_new_from_data(batch.actions.as_ref())?;
        }
        
        // Extract protocol if not found yet
        if self.protocol.is_none() {
            self.protocol = Protocol::try_new_from_data(batch.actions.as_ref())?;
        }
        
        // Check if we're done (found both)
        match (self.metadata, self.protocol) {
            (Some(m), Some(p)) => Ok(ControlFlow::Break((m, p))),
            _ => Ok(ControlFlow::Continue(self))
        }
    }
    
    pub fn try_finish(self) -> DeltaResult<(Metadata, Protocol)> {
        match (self.metadata, self.protocol) {
            (Some(m), Some(p)) => Ok((m, p)),
            _ => Err(Error::MissingMetadata),
        }
    }
}
```

See [Appendix K](#appendix-k-pattern-b-complete-example) for complete before/after code comparison.

**Benefits**:
- ✅ Business logic written once (~25 lines)
- ✅ Choreography is tiny (3 lines sync, 5 lines async)
- ✅ Testable without I/O mocks
- ✅ Power users can use processor directly with custom choreography
- ✅ Only ~1 line differs between sync and async

**When NOT to use**:
- ❌ One-shot operations (use Pattern A)
- ❌ No early exit needed (use plain `fold`)
- ❌ Stateless transformation (use `map`/`filter`)

---

## 6. Pattern C: Two-Phase Processing

**When to use**: Processing tiny manifest files that reference large detail files (checkpoint + sidecars)

### 6.1 Why Two-Phase?

Delta V2 checkpoints have this structure:
- **Manifest**: Small file (~200 KB) with metadata + list of sidecar file paths
- **Sidecars**: Large files (GBs total) with actual Delta actions

**Key insight**: Reading the entire manifest first adds negligible time (< 50ms) but enables:
- Better parallelization (know all N sidecars up front, distribute optimally)
- Progress tracking (N of M complete)
- Caching (skip already-processed sidecars)
- Load balancing (process large files first)

See [Appendix L](#appendix-l-two-phase-vs-incremental-analysis) for detailed performance analysis showing two-phase wins across 1-256+ cores.

### 6.2 The Pattern

**Structure**:

```rust
// CONCEPT CODE: Phase 1 state (accumulates sidecars while processing)
pub struct Phase1InProgress<P> {
    processor: P,
    sidecar_files: Vec<FileMeta>,
}

impl<P: LogReplayProcessor> Phase1InProgress<P> {
    /// Process one batch from manifest
    pub fn process_batch(mut self, batch: &ActionsBatch) 
        -> DeltaResult<ControlFlow<Phase1Result<P>, Self>> 
    {
        // Process with internal processor
        match self.processor.process_batch(batch)? {
            ControlFlow::Continue(proc) => {
                // Not done yet, accumulate any sidecars and continue
                self.processor = proc;
                self.sidecar_files.extend(extract_sidecars(batch)?);
                Ok(ControlFlow::Continue(self))
            }
            ControlFlow::Break(output) => {
                // Found what we need early (e.g., metadata in commits)!
                Ok(ControlFlow::Break(Phase1Result::Complete(output)))
            }
        }
    }
}

// CONCEPT CODE: Phase 1 result
pub enum Phase1Result<P> {
    Complete(P::Output),              // Found early, skip phase 2
    NeedPhase2 {
        processor: P,                  // State so far
        sidecar_files: Vec<FileMeta>,  // All sidecars to process
    },
}
```

**Phase 1 Choreography** (processes commits + checkpoint manifest):

```rust
// CONCEPT CODE: Sync version
pub fn phase1_sync<P>(processor: P) -> DeltaResult<Phase1Result<P>> {
    let commit_batches = engine.read_json_files(...)?;
    let checkpoint_batches = engine.read_parquet_files(...)?;
    
    // Pattern B: try_fold over commits + checkpoint manifest
    commit_batches.chain(checkpoint_batches)
        .try_fold(processor.into(), |state, batch| {
            state.process_batch(&batch).transpose()
        })
        .unwrap_break_or_else(|state| Ok(state.into()))?
}

// CONCEPT CODE: Async version (only differs by async move + .await)
pub async fn phase1_async<P>(processor: P) -> DeltaResult<Phase1Result<P>> {
    let commit_batches = engine.read_json_files(...).await?;
    let checkpoint_batches = engine.read_parquet_files(...).await?;
    
    commit_batches.chain(checkpoint_batches)
        .try_fold(processor.into(), |state, batch| async move {
            state.process_batch(&batch).transpose()
        })
        .await
        .unwrap_break_or_else(|state| Ok(state.into()))?
}
```

**Phase 2 Choreography** (processes sidecars if needed):

```rust
// CONCEPT CODE: Nested Pattern B for files × batches
impl<P: LogReplayProcessor> Phase1Result<P> {
    pub fn process_sidecars_sync(self, engine: &dyn Engine) -> DeltaResult<P::Output> {
        match self {
            Phase1Result::Complete(output) => Ok(output),  // Early exit
            Phase1Result::NeedPhase2 { processor, sidecar_files } => {
                // Outer loop: files
                sidecar_files.into_iter()
                    .try_fold(processor, |proc, file| {
                        let batches = engine.read_parquet_file(&file, ...)?;
                        // Inner loop: batches within each file
                        batches.try_fold(proc, |p, batch| {
                            p.process_batch(batch).transpose()
                        })
                    })
                    .unwrap_break_or_else(P::try_finish)?
            }
        }
    }
}
```

**Key Insight**: Pattern C uses Pattern B internally! Phase 1 uses Pattern B for manifest processing. Phase 2 uses nested Pattern B (files × batches).

### 6.3 Usage Examples

**Simple case** (kernel provides this):
```rust
log_segment
    .phase1_sync(engine, MetadataExtractor::default())?
    .process_sidecars_sync(engine)?  // Handles both Complete and NeedPhase2
```

**Power user** (custom parallelization):
```rust
let phase1 = log_segment.phase1_sync(engine, processor)?;

match phase1 {
    Phase1Result::Complete(output) => output,
    Phase1Result::NeedPhase2 { processor, sidecar_files } => {
        // Distribute across workers (rayon, thread pool, distributed nodes, etc.)
        parallelize(processor, sidecar_files)
    }
}
```

**Benefits**:
- ✅ Clean separation (sequential phase 1, parallelizable phase 2)
- ✅ Simple case is easy (method chaining)
- ✅ Power users can fully customize phase 2
- ✅ Sync and async versions nearly identical
- ✅ Patterns compose (Pattern C = Pattern B × 2)

**When NOT to use**:
- ❌ No manifest/detail split (use Pattern B)
- ❌ Details aren't parallelizable (use Pattern B)

See [Section 10](#10-complete-pattern-c-implementation) for complete implementation details.

See [Appendix G](#appendix-g-pattern-c-support-code) for `From` impl helpers that clean up the choreography.

---

# Part III: Implementation Guide

## 7. Current State: What Needs Refactoring

### 7.1 The 40/30/30 Split

Analysis of the current snapshot building code reveals:

**40% Already I/O-free** ✅
- Functions like `Metadata::try_new_from_data()` already work on fetched data
- `LogSegment::try_new()` validates already-listed files
- `TableConfiguration::try_new()` does pure validation
- **Action**: None needed - already good!

**30% Fundamental I/O** ✅
- Engine handlers: `read_json_files()`, `read_parquet_files()`, `list_from()`
- These are the engine's public interface
- **Action**: Add async variants (AsyncEngine trait hierarchy)

**30% Mixed Choreography** ⚠️
- Functions like `protocol_and_metadata()` interleave I/O with business logic
- ~20 lines of extraction logic mixed with iterator consumption
- Cannot test without I/O, cannot reuse for async without duplication
- **Action**: Apply Pattern A, B, or C to separate concerns

### 7.2 Problems to Solve

The 30% mixed choreography breaks down into 5 specific problems:

| Problem | Function | Complexity | Pattern |
|---------|----------|------------|---------|
| 1 | `LastCheckpointHint::try_read` | Simple | A: Helper Function |
| 2 | `LogSegment::protocol_and_metadata` | Medium | B: Processor + try_fold |
| 3 | `LogSegment::read_actions` | High | C: Phase 1 (multi-source) |
| 4 | `LogSegment::create_checkpoint_stream` | Very High | C: Phase 1 (manifest) |
| 5 | `LogSegment::process_sidecars` | Medium | C: Phase 2 (sidecars) |

See [Appendix C](#appendix-c-current-state-deep-dive) for detailed analysis with line numbers, control flow diagrams, and specific code locations.

---

## 8. Refactored Control Flow

After applying the three patterns, the control flow becomes much cleaner:

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

**Refactoring Applied**:

1. **Pattern A**: `LastCheckpointHint::from_file_result` extracted as I/O-free helper
2. **Pattern B**: `MetadataExtractor` processor extracted as I/O-free state machine
3. **Pattern C**: Two-phase processing with `phase1_*` and `process_sidecars_*`
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

See [Appendix D](#appendix-d-refactored-control-flow-detailed) for complete metrics breakdown and virality analysis.

---

## 9. Async Foundation Requirements

To support async operations, we need a parallel trait hierarchy alongside the existing sync traits.

### 9.1 What's Needed

**1. AsyncEngine trait hierarchy**
- Separate from `Engine` (not combined, not generic)
- Methods return Streams instead of Iterators
- Stream type: `Pin<Box<dyn Stream<Item = Result<T>> + Send>>`

**2. Async handler traits**
- `AsyncJsonHandler::read_json_files() -> Stream<...>`
- `AsyncParquetHandler::read_parquet_files() -> Stream<...>`
- `AsyncStorageHandler::list_from() -> Stream<...>`

**3. Cooperative yielding helper**
- `yield_now().await` after each batch
- ~15 lines, executor-agnostic
- Simple: just returns Poll::Pending once

### 9.2 Design Decisions

**Why separate trait hierarchies?**
- ✅ Can't conditionally make methods `async` based on type parameter
- ✅ Sync and async have fundamentally different return types (T vs Future<T>)
- ✅ Users can implement only sync, only async, or both
- ✅ Clear separation of concerns
- ❌ Alternative (generic trait) doesn't work - can't be conditionally async

**Why boxed Streams?**
- ✅ Required for trait objects (dynamic dispatch)
- ✅ Pin required for async trait methods
- ✅ Send required to move across await points
- ❌ Alternative (impl Stream) not object-safe

**Why EvaluationHandler is shared?**
- ✅ CPU-only (expression evaluation on in-memory data)
- ✅ No I/O, no blocking, works for both sync and async

See [Appendix E](#appendix-e-async-trait-definitions) for complete trait definitions with all methods and type aliases.

See [Appendix I](#appendix-i-cooperative-yielding) for `yield_now()` implementation and usage guidelines.

---

## 10. Complete Pattern C Implementation

This section provides the complete implementation of Pattern C (two-phase processing) for reference. Pattern C is the most complex pattern and is used for checkpoint processing where a small manifest file references many large sidecar files.

**Note**: This is intentionally detailed since it's referenced from multiple places in the document. For concept-focused explanation, see [Section 6](#6-pattern-c-two-phase-processing).

### 10.1 Phase 1: Processing Manifest

Phase 1 processes commits and the checkpoint manifest to discover all sidecar files.

```rust
// Phase 1 state machine
pub struct Phase1InProgress<P> {
    processor: P,
    sidecar_files: Vec<FileMeta>,
}

impl<P: LogReplayProcessor> Phase1InProgress<P> {
    pub fn process_batch(
        mut self,
        batch: &ActionsBatch,
    ) -> DeltaResult<ControlFlow<Phase1Result<P>, Self>> {
        // Process with internal processor
        match self.processor.process_batch(batch)? {
            ControlFlow::Continue(proc) => {
                self.processor = proc;
                self.sidecar_files.extend(extract_sidecars(batch)?);
                Ok(ControlFlow::Continue(self))
            }
            ControlFlow::Break(output) => {
                Ok(ControlFlow::Break(Phase1Result::Complete(output)))
            }
        }
    }
}

// Phase 1 result
pub enum Phase1Result<P> {
    Complete(P::Output),
    NeedPhase2 {
        processor: P,
        sidecar_files: Vec<FileMeta>,
    },
}

// Sync choreography
impl LogSegment {
    pub fn phase1_sync<P>(
        &self,
        engine: &dyn Engine,
        processor: P,
    ) -> DeltaResult<Phase1Result<P>> 
    where
        P: LogReplayProcessor,
    {
        let commit_batches = engine.read_json_files(self.find_commit_cover(), ...)?;
        let checkpoint_batches = engine.read_parquet_files(self.checkpoint_parts(), ...)?;
        
        commit_batches.chain(checkpoint_batches)
            .try_fold(processor.into(), |state, batch| {
                state.process_batch(&batch).transpose()
            })
            .unwrap_break_or_else(|state| Ok(state.into()))
    }
    
    pub async fn phase1_async<P>(
        &self,
        engine: &dyn AsyncEngine,
        processor: P,
    ) -> DeltaResult<Phase1Result<P>> 
    where
        P: LogReplayProcessor,
    {
        use crate::ControlFlowStreamExt as _;
        
        let commit_batches = engine.read_json_files(self.find_commit_cover(), ...).await?;
        let checkpoint_batches = engine.read_parquet_files(self.checkpoint_parts(), ...).await?;
        
        commit_batches.chain(checkpoint_batches)
            .try_fold(processor.into(), |state, batch| async move {
                state.process_batch(&batch).transpose()
            })
            .await
            .unwrap_break_or_else(|state| Ok(state.into()))
    }
}
```

### 10.2 Phase 2: Processing Sidecars

Phase 2 processes the sidecar files discovered in phase 1.

```rust
impl<P: LogReplayProcessor> Phase1Result<P> {
    pub fn process_sidecars_sync(
        self,
        engine: &dyn Engine,
    ) -> DeltaResult<P::Output> {
        match self {
            Phase1Result::Complete(output) => Ok(output),
            Phase1Result::NeedPhase2 { processor, sidecar_files } => {
                sidecar_files
                    .into_iter()
                    .try_fold(processor, |proc, sidecar_file| {
                        let batches = engine.read_parquet_file(&sidecar_file, ...)?;
                        batches.try_fold(proc, |p, batch| {
                            p.process_batch(batch).transpose()
                        })
                    })
                    .unwrap_break_or_else(P::try_finish)?
            }
        }
    }
    
    pub async fn process_sidecars_async(
        self,
        engine: &dyn AsyncEngine,
    ) -> DeltaResult<P::Output> {
        use crate::ControlFlowStreamExt as _;
        
        match self {
            Phase1Result::Complete(output) => Ok(output),
            Phase1Result::NeedPhase2 { processor, sidecar_files } => {
                futures::stream::iter(sidecar_files)
                    .try_fold(processor, |proc, sidecar_file| async move {
                        let batches = engine.read_parquet_file(&sidecar_file, ...).await?;
                        batches.try_fold(proc, |p, batch| async move {
                            yield_now().await;  // Cooperative yielding
                            p.process_batch(batch).transpose()
                        }).await
                    })
                    .await
                    .unwrap_break_or_else(P::try_finish)?
            }
        }
    }
}
```

### 10.3 Helper Implementations

Support code for ergonomic state transitions:

```rust
// Convert processor to Phase1InProgress
impl<P> From<P> for Phase1InProgress<P> {
    fn from(processor: P) -> Self {
        Self {
            processor,
            sidecar_files: Vec::new(),
        }
    }
}

// Convert incomplete Phase1InProgress to Phase1Result
impl<P> From<Phase1InProgress<P>> for Phase1Result<P> {
    fn from(state: Phase1InProgress<P>) -> Self {
        Self::NeedPhase2 {
            processor: state.processor,
            sidecar_files: state.sidecar_files,
        }
    }
}
```

See [Appendix G](#appendix-g-pattern-c-support-code) for explanation of these `From` impls and how they improve choreography code.

---

## 11. Implementation Roadmap

**Total Timeline: 10-12 weeks** (realistic estimate based on complexity analysis)

This roadmap provides granular, actionable tasks with dependencies and risk assessment.

### Phase 0: Async Foundation (Weeks 1-2) - 10 days

**Goal**: Design and document the complete async trait hierarchy before any refactoring begins.

**Task 0.1: Design AsyncEngine Trait Hierarchy (3 days)**
- [ ] Draft all async trait signatures (`AsyncEngine`, `AsyncJsonHandler`, `AsyncParquetHandler`, `AsyncStorageHandler`)
- [ ] Define Stream type aliases (`FileDataReadResultStream`, `FileMetaStream`, `BytesStream`)
- [ ] Document Send/Sync requirements and rationale
- [ ] Review with team: trait design is hard to change later
- **Output**: RFC or design document for async traits
- **Risk**: May require iteration based on feedback

**Task 0.2: Create Extension Traits (1 day)**
- [ ] Create `kernel/src/control_flow_ext.rs` with `ResultExt` and `ControlFlowExt`
- [ ] Implement `.transpose()` method (constrained to `Result<ControlFlow>`)
- [ ] Implement `.unwrap_break_or_else()` and `.unwrap_break_or()` methods
- [ ] Add comprehensive unit tests for both traits
- [ ] Add module-level documentation with examples
- **Output**: `control_flow_ext.rs` module
- **Priority**: High - needed for all pattern implementations

**Task 0.3: Document Cooperative Yielding Pattern (1 day)**
- [ ] Write guidelines for when/where to use `yield_now().await`
- [ ] Create code examples showing correct usage
- [ ] Document anti-patterns (yielding in processors, etc.)
- **Output**: Module docs or guideline document
- **Priority**: High - prevents executor starvation issues

**Task 0.4: Prototype Stream-based try_fold (2 days)**
- [ ] Create minimal example using `TryStreamExt::try_fold`
- [ ] Verify ControlFlow works correctly with async Streams
- [ ] Test cooperative yielding behavior
- [ ] Measure performance overhead
- **Output**: Proof-of-concept code + performance data
- **Priority**: Medium - validates core pattern assumption

**Task 0.5: Review and Finalize (2 days)**
- [ ] Internal review of async trait design
- [ ] Address feedback and iterate
- [ ] Finalize trait signatures and documentation
- [ ] Get sign-off before proceeding
- **Output**: Approved async trait design
- **Risk**: May require iteration based on feedback

**Phase 0 Deliverables**:
- ✅ Complete async trait hierarchy design (documented)
- ✅ Extension traits implemented and tested
- ✅ Cooperative yielding guidelines documented
- ✅ Pattern validated with prototype

**Phase 0 Risks**:
- Async trait design may require multiple rounds of feedback
- Stream type choices may need adjustment based on implementation constraints

---

### Phase 1: Extract I/O-Free Processors (Week 3) - 5 days

**Goal**: Create reusable, testable processors for Pattern A and Pattern B.

**Task 1.1: Create MetadataExtractor Processor (2 days)**
- [ ] Implement `MetadataExtractor` struct with `process_batch` and `try_finish` methods
- [ ] Use `Result<ControlFlow<(Metadata, Protocol), Self>>` signature
- [ ] Implement `Default` trait
- [ ] Handle all error cases (invalid data, missing fields)
- **Output**: `MetadataExtractor` in `kernel/src/snapshot.rs` or new module
- **Depends on**: Phase 0 Task 0.2 (extension traits)

**Task 1.2: Create LastCheckpointHint Helper (1 day)**
- [ ] Implement `LastCheckpointHint::from_file_result` helper function
- [ ] Handle all cases: Ok(data), Err(NotFound), Err(other), empty file
- [ ] Keep all error handling in one place
- **Output**: Helper method in `kernel/src/last_checkpoint_hint.rs`

**Task 1.3: Unit Tests (2 days)**
- [ ] Test `MetadataExtractor` with mock `ActionsBatch` data
- [ ] Test early exit when metadata+protocol found in first batch
- [ ] Test exhaustion case (incomplete state after all batches)
- [ ] Test error propagation from `process_batch`
- [ ] Test `LastCheckpointHint::from_file_result` with all error cases
- **Output**: Comprehensive test coverage without any I/O
- **Priority**: High - validates I/O-free design

**Phase 1 Deliverables**:
- ✅ MetadataExtractor processor (I/O-free, tested)
- ✅ LastCheckpointHint helper (I/O-free, tested)
- ✅ Unit tests passing (no I/O mocks needed)

---

### Phase 2: Refactor Sync Choreography (Weeks 4-5) - 8 days

**Goal**: Refactor existing sync code to use processors, maintaining existing behavior.

**Task 2.1: Refactor LastCheckpointHint::try_read (2 days)**
- [ ] Update `try_read` to use `from_file_result` helper
- [ ] Handle `read_files` API mismatch (takes `&[FileMeta]`, not single path)
- [ ] Ensure existing tests pass unchanged
- **Output**: Refactored `LastCheckpointHint::try_read`
- **Risk**: `read_files` API mismatch may require additional wrapper

**Task 2.2: Refactor LogSegment::read_metadata (3 days)**
- [ ] Update `protocol_and_metadata` to use `try_fold` + `MetadataExtractor`
- [ ] Replace manual loop with processor pattern
- [ ] Handle all error contexts (file paths, batch numbers)
- [ ] Ensure existing tests pass unchanged
- **Output**: Refactored `protocol_and_metadata` method
- **Depends on**: Phase 1 Task 1.1 (MetadataExtractor)

**Task 2.3: Error Context Preservation (2 days)**
- [ ] Audit all error paths to ensure context is preserved
- [ ] Add file paths to errors where missing
- [ ] Add batch numbers for debugging
- [ ] Verify error messages are actionable
- **Output**: Improved error messages
- **Priority**: Medium - important for debugging

**Task 2.4: Integration Testing (1 day)**
- [ ] Run full test suite
- [ ] Verify no regressions in behavior
- [ ] Add integration tests showing processor reuse
- **Output**: All tests passing
- **Priority**: High - must not break existing functionality

**Phase 2 Deliverables**:
- ✅ Sync choreography refactored to use processors
- ✅ All existing tests passing
- ✅ Error context preserved

**Phase 2 Risks**:
- `read_files` API mismatch may require more work than expected
- Error context preservation may uncover edge cases

---

### Phase 3: Implement Pattern C - Two-Phase Processing (Weeks 6-8) - 15 days

**Goal**: Implement the most complex pattern (two-phase checkpoint processing).

**Task 3.1: Implement Phase1InProgress State Machine (3 days)**
- [ ] Create `Phase1InProgress` struct
- [ ] Implement `process_batch` method (I/O-free)
- [ ] Create `Phase1Result` enum (`Complete` vs `NeedPhase2`)
- [ ] Implement `From` conversions for ergonomic state transitions
- [ ] Unit tests for state machine (mock data)
- **Output**: Phase 1 state machine
- **Complexity**: High - careful design needed

**Task 3.2: Implement phase1_sync Choreography (4 days)**
- [ ] Create `LogSegment::phase1_sync` method
- [ ] Handle multi-source coordination (commits + checkpoints)
- [ ] Chain commit and checkpoint iterators
- [ ] Use `try_fold` with `Phase1InProgress`
- [ ] Handle all conditional paths (early exit, no sidecars, etc.)
- [ ] Handle all error contexts
- **Output**: Working phase 1 sync choreography
- **Depends on**: Task 3.1
- **Complexity**: Very high - most complex choreography

**Task 3.3: Implement process_sidecars_sync (3 days)**
- [ ] Create `Phase1Result::process_sidecars_sync` method
- [ ] Implement nested `try_fold` (files × batches)
- [ ] Handle `Complete` early exit case
- [ ] Handle `NeedPhase2` with sidecar processing
- **Output**: Working phase 2 sync choreography
- **Depends on**: Task 3.2

**Task 3.4: Integration Testing (3 days)**
- [ ] Test with V1 checkpoints (no sidecars)
- [ ] Test with V2 checkpoints (with sidecars)
- [ ] Test early exit cases (metadata in commits)
- [ ] Test error cases (missing sidecars, corrupt data)
- [ ] Verify performance is unchanged
- **Output**: Comprehensive test coverage
- **Priority**: Critical - most complex code path

**Task 3.5: Documentation (2 days)**
- [ ] Document two-phase pattern in module docs
- [ ] Add examples showing simple and advanced usage
- [ ] Document when each pattern should be used
- **Output**: Clear documentation
- **Priority**: High - complex pattern needs good docs

**Phase 3 Deliverables**:
- ✅ Pattern C fully implemented for sync
- ✅ Two-phase processing working
- ✅ Comprehensive test coverage
- ✅ Clear documentation

**Phase 3 Risks**:
- Pattern C is most complex - may need buffer time
- Multi-source coordination may reveal edge cases
- Nested iteration complexity may require refactoring

---

### Phase 4: Add Async Choreography (Weeks 9-10) - 10 days

**Goal**: Add async variants of all choreography methods, reusing processors.

**Task 4.1: Implement LastCheckpointHint::try_read_async (1 day)**
- [ ] Create async variant using same `from_file_result` helper
- [ ] Add `.await` for `storage.read_files(...).await`
- [ ] Verify same error handling as sync version
- **Output**: `try_read_async` method
- **Depends on**: Phase 0 (AsyncEngine traits), Phase 2 Task 2.1

**Task 4.2: Implement LogSegment::read_metadata_async (2 days)**
- [ ] Create async variant using same `MetadataExtractor`
- [ ] Use `TryStreamExt::try_fold` with `async move` closure
- [ ] Add cooperative yielding after each batch
- [ ] Verify same behavior as sync version
- **Output**: `read_metadata_async` method
- **Depends on**: Phase 2 Task 2.2

**Task 4.3: Implement phase1_async Choreography (3 days)**
- [ ] Create async variant of `phase1_sync`
- [ ] Use Stream variants of handlers
- [ ] Add cooperative yielding
- [ ] Handle same conditional paths as sync
- **Output**: `phase1_async` method
- **Depends on**: Phase 3 Task 3.2

**Task 4.4: Implement process_sidecars_async (2 days)**
- [ ] Create async variant with nested `try_fold`
- [ ] Add cooperative yielding at both levels (files and batches)
- [ ] Use `futures::stream::iter` for sidecar list
- **Output**: `process_sidecars_async` method
- **Depends on**: Phase 3 Task 3.3

**Task 4.5: Async Tests (2 days)**
- [ ] Test all async methods with mock AsyncEngine
- [ ] Verify cooperative yielding behavior
- [ ] Test cancellation safety
- [ ] Compare behavior with sync versions
- **Output**: Async test coverage
- **Priority**: High - async has unique failure modes

**Phase 4 Deliverables**:
- ✅ All async choreography implemented
- ✅ Cooperative yielding in place
- ✅ Async tests passing
- ✅ Same behavior as sync versions

**Phase 4 Risks**:
- Async timing issues may be hard to reproduce in tests
- Cooperative yielding may need tuning based on performance

---

### Phase 5: Integration & Documentation (Weeks 11-12) - 10 days

**Goal**: Ensure everything works together, document for users.

**Task 5.1: End-to-End Testing (4 days)**
- [ ] Test sync snapshot building with real data
- [ ] Test async snapshot building with real data
- [ ] Test mixed sync/async scenarios
- [ ] Test all checkpoint formats (V1, V2, sidecars)
- [ ] Test error cases end-to-end
- **Output**: Confidence in full integration
- **Priority**: Critical - final validation

**Task 5.2: Performance Validation (2 days)**
- [ ] Benchmark sync path (should be unchanged)
- [ ] Benchmark async path
- [ ] Verify cooperative yielding overhead is acceptable
- [ ] Profile any regressions
- **Output**: Performance report
- **Priority**: High - must not regress sync performance

**Task 5.3: Documentation (3 days)**
- [ ] Write high-level overview of patterns
- [ ] Document when to use each pattern
- [ ] Write guide for custom engines (how to implement AsyncEngine)
- [ ] Write guide for power users (using processors directly)
- [ ] Add examples to docs
- **Output**: Complete user-facing documentation
- **Priority**: High - critical for adoption

**Task 5.4: Buffer for Issues (1 day)**
- [ ] Address any issues found in integration
- [ ] Fix any performance problems
- [ ] Improve documentation based on feedback
- **Output**: Polish
- **Priority**: Medium - inevitably something will need fixing

**Phase 5 Deliverables**:
- ✅ Full integration tested
- ✅ Performance validated
- ✅ Complete documentation
- ✅ Ready to ship

---

## 12. Summary

**Total: 10-12 weeks**

**Critical Path**:
1. Async trait design (Phase 0) - must be right before proceeding
2. Pattern C implementation (Phase 3) - most complex, longest
3. Integration testing (Phase 5) - final validation

**Key Milestones**:
- **Week 2**: Async traits designed and approved ✅
- **Week 5**: Sync path fully refactored ✅
- **Week 8**: Pattern C complete (biggest risk addressed) ✅
- **Week 10**: Async support complete ✅
- **Week 12**: Ready to ship ✅

**Risk Mitigation**:
- Buffer time included in complex phases (Phase 3, Phase 5)
- Early validation of core assumptions (Phase 0 prototype)
- Incremental approach (sync first, then async)
- Continuous testing (every phase has tests)

---

# Part IV: Deep Dives & Reference

## Appendix A: Refactoring Principles

**1. Separate trait hierarchies** (not combined or generic):

```rust
// ✅ Recommended: Separate traits
pub trait Engine { 
    fn read_json_files(&self, ...) -> DeltaResult<Box<dyn Iterator<...>>>;
}

pub trait AsyncEngine { 
    async fn read_json_files(&self, ...) -> DeltaResult<Pin<Box<dyn Stream<...>>>>;
}

// ❌ Alternative 1: Combined trait with async methods
pub trait Engine {
    fn read_file(&self, ...) -> Iterator<...>;
    async fn read_file_async(&self, ...) -> Stream<...>;  // Complicates trait objects
}

// ❌ Alternative 2: Generic trait
pub trait GenericEngine<OutputKind> {
    type Output<T>;
    fn read_json_files(&self, ...) -> Self::Output<EngineData>;
    // Problem: Can't make this conditionally async based on OutputKind!
}
```

**Why not generic?**

While a generic trait like `GenericEngine<OutputKind>` seems appealing (methods have identical signatures except Iterator vs Stream), it's not feasible because:

1. **Can't conditionally make methods `async`**: The method signature must be either `fn` or `async fn` - you can't make it conditional on a type parameter
2. **Different return types are fundamentally different**: 
   - Sync returns `T` immediately
   - Async returns `impl Future<Output = T>`
   - These aren't just different types, they have different evaluation semantics
3. **Trait object complexity**: `dyn GenericEngine<Iterator>` vs `dyn GenericEngine<Stream>` would be awkward and lose the clarity of purpose

**Rationale for separate traits**: 
- Clear separation of sync vs async semantics
- Methods can be truly `async fn` (not just returning futures)
- Users can implement only sync, only async, or both
- No runtime cost for sync-only users
- Trait objects are straightforward: `&dyn Engine` vs `&dyn AsyncEngine`

**Note on Arrow's generic approach**: 

Arrow uses generic types like `GenericArray<OffsetSize>` with type aliases (`StringArray = GenericArray<i32>`, `LargeStringArray = GenericArray<i64>`). This works because:
- The methods don't change (both use the same operations)
- It's purely a difference in data representation (offset size)
- No async vs sync distinction

Our case is fundamentally different - we're not just changing a type parameter, we're changing the **execution model** (sync vs async).

**2. Stream types are boxed and pinned**:
```rust
pub type DeltaStream<T> = Pin<Box<dyn Stream<Item = DeltaResult<T>> + Send>>;

// Used as:
pub type FileDataReadResultStream = DeltaStream<Box<dyn EngineData>>;
pub type FileMetaStream = DeltaStream<FileMeta>;
pub type BytesStream = DeltaStream<Bytes>;
```

**Rationale**:
- `Pin` required for async trait methods (futures must be pinned)
- `Box` for trait object (allows dynamic dispatch)
- `Send` required to move across await points (multi-threaded executors)
- `dyn Stream` allows different implementations
- Generic `DeltaStream<T>` reduces redundancy in type definitions

**Alternative considered**: `impl Stream` (static dispatch)
- **Pro**: No boxing overhead, better performance
- **Con**: Trait methods can't use `impl Trait` in return position (not object-safe)
- **Verdict**: Boxed trait objects are necessary for engine abstraction

**3. EvaluationHandler is shared**:
```rust
// Same for both sync and async!
fn evaluation_handler(&self) -> Arc<dyn EvaluationHandler>;
```

**Rationale**: EvaluationHandler is CPU-only (expression evaluation on in-memory data), no I/O, so it doesn't need async variants.

**4. Dependencies and Cooperative Yielding**:

The async implementation requires the `futures` crate, which provides:
- `Stream` trait (async equivalent of `Iterator`)
- `StreamExt` for stream combinators (`.chain()`, `.map()`, etc.)

The `futures` crate is already an optional dependency in `kernel/Cargo.toml` (line 59), included in the `default-engine-base` feature.

**Cooperative Yielding**: For executor-agnostic yielding, we need to implement a simple helper since the `futures` crate doesn't provide `yield_now()`:

```rust
/// Yields control back to the executor, allowing other tasks to run.
/// This is executor-agnostic and works with any async runtime.
#[inline]
pub async fn yield_now() {
    /// Yield implementation that wakes immediately and returns Pending once
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
                cx.waker().wake_by_ref();  // Wake immediately
                Poll::Pending
            }
        }
    }
    
    YieldNow { yielded: false }.await
}
```

**Note on `wake_by_ref()` vs tokio's `context::defer()`**: 

[Tokio's `yield_now()`](https://docs.rs/tokio/latest/src/tokio/task/yield_now.rs.html#39-64) uses `context::defer(cx.waker())` which schedules the wake to happen *after* the current poll completes, potentially giving other tasks more opportunity to run before this task is re-polled. 

Our simpler implementation using [`wake_by_ref()`](https://docs.rs/futures/latest/futures/task/struct.Waker.html#method.wake_by_ref) wakes the task immediately, which may lead to it being re-polled sooner. However:

1. **Executor-agnostic**: `wake_by_ref()` works with any executor, while `context::defer()` is tokio-specific
2. **Simpler**: No additional infrastructure needed
3. **Still cooperative**: The task still yields (returns `Poll::Pending`), giving the executor the opportunity to run other tasks
4. **Good enough**: For our use case (yielding between batches every 10-100ms of CPU work), the slight difference in scheduling fairness is negligible

If tokio-specific optimizations become important, users can configure their async engine to use tokio's yield mechanism in their engine implementation, keeping the kernel executor-agnostic.

This is ~15 lines of code and works with any executor (tokio, async-std, smol, etc.). No additional dependencies needed.

### 3.2 Iterator vs Stream Compatibility

**Critical insight**: The `try_fold` pattern used throughout this proposal works for **both** sync (`Iterator`) and async (`Stream`) with minimal changes.

#### Standard Library: Iterator::try_fold

```rust
// From std::iter::Iterator
pub trait Iterator {
    fn try_fold<B, F, R>(&mut self, init: B, f: F) -> R
    where
        F: FnMut(B, Self::Item) -> R,
        R: Try<Output = B>;
}

// Usage (sync):
iterator.try_fold(processor, |p, item| {
    p.process(item).transpose()  // Returns ControlFlow<Result<Output>, Processor>
})
```

#### Futures Crate: TryStreamExt::try_fold

```rust
// From futures::stream::TryStreamExt
pub trait TryStreamExt: Stream {
    fn try_fold<B, F, Fut>(&mut self, init: B, f: F) -> TryFold<Self, Fut, F>
    where
        F: FnMut(B, Self::Ok) -> Fut,
        Fut: TryFuture<Ok = B, Error = Self::Error>;
}

// Usage (async):
stream.try_fold(processor, |p, item| async move {
    p.process(item).transpose()  // Same return type as sync!
})
.await
```

**PROBLEM DISCOVERED**: `TryStreamExt::try_fold` is hardwired to `Result` via `TryFuture`, which means it **cannot** work with `ControlFlow` directly. The `Try` trait that enables `ControlFlow` with `Iterator::try_fold` is unstable and not available in async contexts.

**Solution**: We need a custom stream extension trait for `ControlFlow`-based folding.

#### Custom Extension Trait: ControlFlowStreamExt

Since `futures::TryStreamExt::try_fold` is constrained to `Result` types (via `TryFuture`), we need our own extension trait that handles `ControlFlow`. The good news: **it's surprisingly simple** to implement!

```rust
use std::ops::ControlFlow;
use std::pin::Pin;
use std::task::{Context, Poll};
use futures_core::{Future, Stream};
use pin_project_lite::pin_project;

/// Extension trait for `Stream` that provides `ControlFlow`-based folding.
/// 
/// This parallels `TryStreamExt::try_fold` but works with `ControlFlow` instead of `Result`.
/// Needed because `TryStreamExt::try_fold` is hardwired to `Result` via `TryFuture`.
/// 
/// The method is named `try_fold` (same as `TryStreamExt`) since the compiler can
/// disambiguate based on the `Future<Output = ControlFlow<>>` constraint vs `TryFuture`.
pub trait ControlFlowStreamExt: Stream {
    /// Folds the stream with early exit via `ControlFlow`.
    /// 
    /// Similar to `Iterator::try_fold`, but async. The closure should return:
    /// - `ControlFlow::Continue(acc)` to continue folding with updated accumulator
    /// - `ControlFlow::Break(value)` to stop early and return `value`
    fn try_fold<B, C, F, Fut>(self, init: C, f: F) -> TryFoldControlFlow<Self, F, Fut, C>
    where
        Self: Sized,
        F: FnMut(C, Self::Item) -> Fut,
        Fut: Future<Output = ControlFlow<B, C>>;
}

impl<S: Stream> ControlFlowStreamExt for S {
    fn try_fold<B, C, F, Fut>(self, init: C, f: F) -> TryFoldControlFlow<Self, F, Fut, C>
    where
        Self: Sized,
        F: FnMut(C, Self::Item) -> Fut,
        Fut: Future<Output = ControlFlow<B, C>>,
    {
        TryFoldControlFlow::new(self, f, init)
    }
}

pin_project! {
    /// Future for `try_fold_control_flow`.
    /// 
    /// Mirrors the structure of `futures_util::stream::TryFold`, but for `ControlFlow`.
    #[must_use = "futures do nothing unless you `.await` or poll them"]
    pub struct TryFoldControlFlow<St, F, Fut, C> {
        #[pin]
        stream: St,
        f: F,
        accum: Option<C>,
        #[pin]
        future: Option<Fut>,
    }
}

impl<St, F, Fut, C> TryFoldControlFlow<St, F, Fut, C>
where
    St: Stream,
    F: FnMut(C, St::Item) -> Fut,
    Fut: Future,
{
    pub(crate) fn new(stream: St, f: F, init: C) -> Self {
        Self {
            stream,
            f,
            accum: Some(init),
            future: None,
        }
    }
}

impl<St, F, Fut, B, C> Future for TryFoldControlFlow<St, F, Fut, C>
where
    St: Stream,
    F: FnMut(C, St::Item) -> Fut,
    Fut: Future<Output = ControlFlow<B, C>>,
{
    type Output = ControlFlow<B, C>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut this = self.project();
        
        Poll::Ready(loop {
            if let Some(fut) = this.future.as_mut().as_pin_mut() {
                // We're currently processing a future to produce a new accum value
                match futures_core::ready!(fut.poll(cx)) {
                    ControlFlow::Continue(c) => {
                        *this.accum = Some(c);
                        this.future.set(None);
                    }
                    ControlFlow::Break(b) => {
                        break ControlFlow::Break(b);
                    }
                }
            } else if this.accum.is_some() {
                // We're waiting on a new item from the stream
                match futures_core::ready!(this.stream.as_mut().poll_next(cx)) {
                    Some(item) => {
                        let acc = this.accum.take().unwrap();
                        this.future.set(Some((this.f)(acc, item)));
                    }
                    None => {
                        // Stream exhausted, return Continue with final accumulator
                        let acc = this.accum.take().unwrap();
                        break ControlFlow::Continue(acc);
                    }
                }
            } else {
                panic!("TryFoldControlFlow polled after completion")
            }
        })
    }
}
```

**Usage** (now works with `ControlFlow`):

```rust
use crate::ControlFlowStreamExt as _;

// Async choreography with ControlFlow - same method name as Iterator::try_fold!
stream
    .try_fold(Processor::default(), |proc, item| async move {
        proc.process(item).transpose()
        // Returns ControlFlow<Result<Output>, Processor>
    })
    .await
    .unwrap_break_or_else(Processor::try_finish)?
```

**Key Differences from `TryStreamExt::try_fold`**:

1. **No `TryFuture` constraint**: We use `Future<Output = ControlFlow<B, C>>` directly
2. **No error unwrapping**: We don't unwrap `Result` - the stream can be `Stream<Item = T>` or `Stream<Item = Result<T, E>>`
3. **Simpler poll logic**: No error branching since `ControlFlow` is the only control flow mechanism
4. **Stream exhaustion returns `Continue`**: When items run out, we return `Continue(accum)` instead of an error

**Complexity Assessment**: ~70 lines of code. This is straightforward enough to include in `delta-kernel` rather than extracting to a separate crate.

**Why Not Use Existing `TryStreamExt`?**: The futures crate's `TryStreamExt::try_fold` is hardwired to `Result` via the `TryFuture` trait ([source](https://docs.rs/futures-util/0.3.31/src/futures_util/stream/try_stream/try_fold.rs.html#9-20)). The stdlib's `Iterator::try_fold` can work with `ControlFlow` because it uses the unstable `Try` trait, but that's not available for async. Our custom trait bridges this gap by working directly with `ControlFlow<B, C>` instead of `Result<T, E>`.

#### Key Insight: Minimal Differences

The **only differences** between sync and async choreography:
1. `async move` in the closure (async) vs bare closure (sync)
2. `.await` after the fold (async) vs immediate result (sync)
3. Import the right extension trait:
   - Sync: `Iterator::try_fold` (stdlib, uses unstable `Try` trait)
   - Async: `ControlFlowStreamExt::try_fold` (our custom trait, same method name!)

**Everything else is identical**:
- Processor interface (same `process` method)
- `ControlFlow` return type (same early exit semantics)
- `.transpose()` adaptor (same conversion)
- `.unwrap_break_or_else()` helper (same exhaustion handling)

This is why the pattern scales so well from sync to async!

### 3.3 Cooperative Yielding for CPU-Intensive Operations

Async operations must cooperate with the executor to avoid blocking other tasks. When processing multiple batches, always yield between batches.

#### The Problem

```rust
// ❌ BAD: Can block executor for seconds
stream.try_fold(processor, |p, batch| async move {
    // If processing 1000 batches at 10ms each = 10 seconds blocking!
    p.process_batch(&batch).transpose()
    // No await points = executor can't switch to other tasks
})
```

Even though the closure is `async move`, if there are no `.await` points inside, it runs synchronously and blocks the executor.

#### The Solution: Batches Are Natural Cooperation Points

**Key insight**: If an engine produces separate batches, it's for good reason (e.g., memory limits, streaming). Each batch boundary is a natural place to yield control back to the executor.

**Simple rule**: Always `yield_now().await` before processing each batch.

```rust
// ✅ GOOD: Yields after each batch
stream.try_fold(processor, |p, batch| async move {
    yield_now().await;  // Natural cooperation point!
    p.process_batch(&batch).transpose()
})
```

#### Why This Works

1. **Batches are already chunked work**: The engine splits data into batches, so each batch represents a reasonable quantum of work.

2. **Minimal overhead**: `yield_now()` is cheap (~nanoseconds if no other tasks are ready).

3. **Responsive**: Other tasks get a chance to run after each batch (typically every 10-100ms of CPU work).

4. **Simple**: No complex heuristics about "how much CPU is too much" - just yield after each batch.

5. **Composable**: Works at every level (batch processing, file processing, sidecar processing).

#### When NOT to Yield

Don't yield in I/O-free helpers or processors:
```rust
// Processor method (I/O-free, used by sync AND async)
impl Processor {
    pub fn process_batch(self, batch: &Batch) -> DeltaResult<ControlFlow<Output, Self>> {
        // ✅ NO yielding here - this is pure computation
        // ✅ Sync code can call this too
        // ✅ Yielding happens in the choreography layer (async move closure)
        ...
    }
}
```

**Rationale**: Processors are shared between sync and async. Yielding must happen in async choreography, not in shared I/O-free code.

#### Complete Example

```rust
// Phase 2: Processing sidecars with cooperative yielding
pub async fn process_sidecars_async(
    self,
    engine: &dyn AsyncEngine,
) -> DeltaResult<Output> {
    use crate::ControlFlowStreamExt as _;
    
    match self {
        Phase1Result::Complete(output) => Ok(output),
        Phase1Result::NeedPhase2 { processor, sidecar_files } => {
            futures::stream::iter(sidecar_files)
                .try_fold(processor, |proc, sidecar_file| async move {
                    let batches = engine.read_parquet_file(&sidecar_file, ...).await?;
                    
                    batches.try_fold(proc, |p, batch| async move {
                        yield_now().await;  // Yield after each batch!
                        p.process_batch(&batch).transpose()
                    }).await
                })
                .await
                .unwrap_break_or_else(Processor::try_finish)?
        }
    }
}
```

#### Summary

**CPU-intensive operations in async**:
- Always yield after processing each batch
- Use `yield_now().await` (simple helper, ~15 lines, executor-agnostic)
- Batches are natural cooperation points
- Keep processors I/O-free (no yielding in shared code)

This simple guideline ensures async operations remain responsive without complex heuristics or periodic yielding logic.

---

## Appendix B: Extension Traits for try_fold Integration

**Why This Exists**: Pattern B processors return `Result<ControlFlow<O, S>>`, but `try_fold` expects a closure that returns `ControlFlow<Result<O>, S>`. These extension traits bridge this gap elegantly.

### B.1 The Type Mismatch Problem

```rust
// What processor returns:
fn process_batch(&mut self, batch: ActionsBatch) 
    -> Result<ControlFlow<Output, Self>, Error>

// What try_fold expects:
try_fold(init, |state, item| {
    // Must return: ControlFlow<Result<Output>, State>
    //              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
})
```

**The challenge**: How to convert `Result<ControlFlow<O, S>>` → `ControlFlow<Result<O>, S>` elegantly?

### B.2 ResultExt: Transpose for Result<ControlFlow>

```rust
/// Extension trait for `Result<ControlFlow<B, C>>` to enable transpose operation
pub trait ResultExt<B, C> {
    /// Transposes `Result<ControlFlow<B, C>>` to `ControlFlow<Result<B>, C>`
    ///
    /// This is needed because processors return `Result<ControlFlow<Output, State>>`
    /// but `try_fold` expects closures to return `ControlFlow<Result<Output>, State>`.
    fn transpose(self) -> ControlFlow<Result<B>, C>;
}

impl<B, C, E> ResultExt<B, C> for Result<ControlFlow<B, C>, E> {
    fn transpose(self) -> ControlFlow<Result<B>, E> {
        match self {
            Ok(ControlFlow::Continue(state)) => ControlFlow::Continue(state),
            Ok(ControlFlow::Break(output)) => ControlFlow::Break(Ok(output)),
            Err(e) => ControlFlow::Break(Err(e)),
        }
    }
}
```

**Usage in choreography**:

```rust
// Sync choreography
let result = batches.try_fold(
    MetadataExtractor::new(),
    |mut processor, batch| {
        processor.process_batch(batch).transpose()  // ← Makes types align!
    }
)?;

// Async choreography (same transpose!)
let result = batches.try_fold(
    MetadataExtractor::new(),
    |mut processor, batch| async move {
        processor.process_batch(batch).transpose()  // ← Identical!
    }
).await?;
```

**Why this works**:
1. `process_batch` returns `Result<ControlFlow<Output, Self>>`
2. `.transpose()` converts to `ControlFlow<Result<Output>, Self>`
3. `try_fold` receives the `ControlFlow` and handles early exit
4. Final `?` unwraps the outer `Result`

### B.3 ControlFlowExt: Unwrap Break Values

```rust
/// Extension trait for `ControlFlow` to extract break values ergonomically
pub trait ControlFlowExt<B, C> {
    /// Unwraps the break value, or calls `f` if continuing
    fn unwrap_break_or_else<F: FnOnce(C) -> B>(self, f: F) -> B;
    
    /// Unwraps the break value, or returns `default` if continuing
    fn unwrap_break_or(self, default: B) -> B;
}

impl<B, C> ControlFlowExt<B, C> for ControlFlow<B, C> {
    fn unwrap_break_or_else<F: FnOnce(C) -> B>(self, f: F) -> B {
        match self {
            ControlFlow::Break(b) => b,
            ControlFlow::Continue(c) => f(c),
        }
    }
    
    fn unwrap_break_or(self, default: B) -> B {
        match self {
            ControlFlow::Break(b) => b,
            ControlFlow::Continue(_) => default,
        }
    }
}
```

**Usage**:

```rust
// After try_fold completes
let result: ControlFlow<Result<(Protocol, Metadata)>, MetadataExtractor> = 
    batches.try_fold(...)?;

// Extract the output or handle incomplete state
let (protocol, metadata) = result.unwrap_break_or_else(|incomplete_processor| {
    Err(Error::Generic("Metadata incomplete after exhausting batches".into()))
})?;
```

### B.4 ControlFlowStreamExt: Async try_fold for Streams

**Why needed**: The `futures` crate provides `TryStreamExt::try_fold` but it doesn't work with `ControlFlow` - it only handles `Result`. We need a version that supports `ControlFlow<Result<B>, S>` for early exit.

```rust
use futures::stream::Stream;
use std::ops::ControlFlow;

/// Extension trait for async Streams to support try_fold with ControlFlow
pub trait ControlFlowStreamExt: Stream {
    /// Fold stream items with early exit support via ControlFlow
    ///
    /// Similar to `Iterator::try_fold` but for async Streams.
    /// The closure should return `ControlFlow<Result<Break>, Continue>`.
    async fn try_fold<B, S, F, Fut>(self, init: S, f: F) -> Result<ControlFlow<B, S>>
    where
        F: FnMut(S, Self::Item) -> Fut,
        Fut: std::future::Future<Output = ControlFlow<Result<B>, S>>,
        Self: Sized;
}

impl<T: Stream> ControlFlowStreamExt for T {
    async fn try_fold<B, S, F, Fut>(self, init: S, mut f: F) -> Result<ControlFlow<B, S>>
    where
        F: FnMut(S, Self::Item) -> Fut,
        Fut: std::future::Future<Output = ControlFlow<Result<B>, S>>,
        Self: Sized,
    {
        use futures::StreamExt;
        
        let mut state = init;
        let mut stream = Box::pin(self);
        
        while let Some(item) = stream.next().await {
            match f(state, item).await {
                ControlFlow::Continue(s) => state = s,
                ControlFlow::Break(Ok(b)) => return Ok(ControlFlow::Break(b)),
                ControlFlow::Break(Err(e)) => return Err(e),
            }
        }
        
        Ok(ControlFlow::Continue(state))
    }
}
```

**Usage in async choreography**:

```rust
use crate::control_flow_ext::{ControlFlowStreamExt, ResultExt};

async fn read_metadata_async(
    &self,
    engine: &dyn AsyncEngine
) -> DeltaResult<(Protocol, Metadata)> {
    let batches: DeltaStream<ActionsBatch> = 
        engine.read_json_files(...).await?;
    
    let result = batches.try_fold(
        MetadataExtractor::new(),
        |mut processor, batch| async move {
            yield_now().await;  // Cooperative yielding
            processor.process_batch(batch).transpose()
        }
    ).await?;
    
    result.unwrap_break_or_else(|_| {
        Err(Error::Generic("Metadata incomplete".into()))
    })
}
```

### B.5 Where to Place These Traits

**Recommended location**: `kernel/src/control_flow_ext.rs`

```rust
// kernel/src/control_flow_ext.rs

pub mod result_ext {
    pub use super::ResultExt;
}

pub mod control_flow_ext {
    pub use super::ControlFlowExt;
}

#[cfg(feature = "default-engine-base")]
pub mod stream_ext {
    pub use super::ControlFlowStreamExt;
}
```

**Import in modules**:

```rust
// In snapshot.rs or log_segment.rs
use crate::control_flow_ext::{ResultExt, ControlFlowExt};

#[cfg(feature = "default-engine-base")]
use crate::control_flow_ext::ControlFlowStreamExt;
```

### B.6 Testing Extension Traits

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use std::ops::ControlFlow;

    #[test]
    fn test_result_ext_transpose_ok_continue() {
        let result: Result<ControlFlow<i32, i32>, String> = Ok(ControlFlow::Continue(42));
        let transposed = result.transpose();
        assert_eq!(transposed, ControlFlow::Continue(42));
    }

    #[test]
    fn test_result_ext_transpose_ok_break() {
        let result: Result<ControlFlow<i32, i32>, String> = Ok(ControlFlow::Break(99));
        let transposed = result.transpose();
        assert_eq!(transposed, ControlFlow::Break(Ok(99)));
    }

    #[test]
    fn test_result_ext_transpose_err() {
        let result: Result<ControlFlow<i32, i32>, String> = Err("error".to_string());
        let transposed = result.transpose();
        assert_eq!(transposed, ControlFlow::Break(Err("error".to_string())));
    }

    #[test]
    fn test_control_flow_ext_unwrap_break() {
        let cf: ControlFlow<i32, i32> = ControlFlow::Break(42);
        assert_eq!(cf.unwrap_break_or(99), 42);
    }

    #[test]
    fn test_control_flow_ext_unwrap_continue() {
        let cf: ControlFlow<i32, i32> = ControlFlow::Continue(42);
        assert_eq!(cf.unwrap_break_or(99), 99);
    }

    #[test]
    fn test_control_flow_ext_unwrap_break_or_else() {
        let cf: ControlFlow<i32, i32> = ControlFlow::Continue(42);
        assert_eq!(cf.unwrap_break_or_else(|x| x * 2), 84);
    }
}
```

**Key testing insights**:
- Extension traits are I/O-free (pure functions)
- Tests require no mocks
- Tests verify all branches (Ok/Err, Break/Continue)
- Tests are fast and deterministic

---

## Appendix C: Complete Pattern A Example - LastCheckpointHint

**Why This Exists**: Pattern A is for one-shot operations that read a single file and process its contents. This appendix shows the complete, production-ready implementation of `LastCheckpointHint` following Pattern A.

### C.1 The Data Structure

```rust
/// Hint for finding the most recent checkpoint
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LastCheckpointHint {
    pub version: i64,
    pub size: Option<i64>,
}
```

**Purpose**: The `_last_checkpoint` file contains a JSON hint pointing to the most recent checkpoint. This avoids scanning all log files to find it.

### C.2 The I/O-Free Helper Function

```rust
impl LastCheckpointHint {
    /// Create a LastCheckpointHint from file read result (I/O-free helper)
    ///
    /// This helper consolidates all error handling logic in one place,
    /// making it reusable across sync and async contexts.
    pub fn from_file_result(result: DeltaResult<Box<dyn EngineData>>) -> DeltaResult<Option<Self>> {
        match result {
            // File exists and has content
            Ok(data) => {
                if data.is_empty() {
                    // Empty file is treated as "no hint"
                    Ok(None)
                } else {
                    // Parse JSON and extract hint
                    let json = data.into_json()?;
                    let hint = Self::parse_json(&json)?;
                    Ok(Some(hint))
                }
            }
            
            // File doesn't exist (expected case for new tables)
            Err(Error::FileNotFound(_)) => Ok(None),
            
            // Other errors are propagated
            Err(e) => Err(e),
        }
    }
    
    /// Parse JSON into LastCheckpointHint
    fn parse_json(json: &[serde_json::Value]) -> DeltaResult<Self> {
        if json.len() != 1 {
            return Err(Error::Generic(
                format!("Expected exactly 1 JSON object, got {}", json.len())
            ));
        }
        
        let obj = json[0].as_object()
            .ok_or_else(|| Error::Generic("Expected JSON object".into()))?;
        
        let version = obj.get("version")
            .and_then(|v| v.as_i64())
            .ok_or_else(|| Error::Generic("Missing or invalid 'version' field".into()))?;
        
        let size = obj.get("size").and_then(|v| v.as_i64());
        
        Ok(LastCheckpointHint { version, size })
    }
}
```

**Key characteristics**:
- ✅ **I/O-free**: Takes `DeltaResult<Box<dyn EngineData>>`, not `&dyn Engine`
- ✅ **Testable**: Can test with mock `EngineData` (no I/O mocks needed)
- ✅ **Reusable**: Same logic for sync and async
- ✅ **Comprehensive error handling**: Handles Ok, NotFound, Err, empty file

### C.3 Sync Choreography

```rust
impl LastCheckpointHint {
    /// Read last checkpoint hint from table (sync version)
    pub fn try_read(
        table_root: &Url,
        storage: &dyn StorageHandler,
    ) -> DeltaResult<Option<Self>> {
        let file_path = table_root.join("_delta_log/_last_checkpoint")?;
        let file_meta = FileMeta {
            path: file_path,
            last_modified: 0,  // Not used for this read
            size: 0,          // Not used for this read
        };
        
        // I/O: Read the file
        let mut files_iter = storage.read_files(vec![file_meta])?;
        let file_result = files_iter.next()
            .ok_or_else(|| Error::Generic("No data returned from read_files".into()))?;
        
        // Computation: Process the result (I/O-free)
        Self::from_file_result(file_result)
    }
}
```

**Choreography responsibilities**:
1. Construct file path
2. Trigger I/O (`storage.read_files()`)
3. Extract result from iterator
4. Delegate to I/O-free helper

### C.4 Async Choreography

```rust
#[cfg(feature = "default-engine-base")]
impl LastCheckpointHint {
    /// Read last checkpoint hint from table (async version)
    pub async fn try_read_async(
        table_root: &Url,
        storage: &dyn AsyncStorageHandler,
    ) -> DeltaResult<Option<Self>> {
        let file_path = table_root.join("_delta_log/_last_checkpoint")?;
        let file_meta = FileMeta {
            path: file_path,
            last_modified: 0,
            size: 0,
        };
        
        // I/O: Read the file (async)
        let mut files_stream = storage.read_files(vec![file_meta]).await?;
        let file_result = files_stream.next().await
            .ok_or_else(|| Error::Generic("No data returned from read_files".into()))?;
        
        // Computation: Process the result (I/O-free, same as sync!)
        Self::from_file_result(file_result)
    }
}
```

**Differences from sync version**:
1. `async fn` instead of `fn`
2. `&dyn AsyncStorageHandler` instead of `&dyn StorageHandler`
3. `.await` after `read_files()` and `next()`
4. **Everything else is identical** (same helper, same error handling)

### C.5 Testing the Helper (I/O-Free)

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_from_file_result_success() {
        let json = r#"{"version": 10, "size": 12345}"#;
        let data = MockEngineData::from_json_str(json);
        let result = LastCheckpointHint::from_file_result(Ok(Box::new(data)));
        
        assert_eq!(
            result.unwrap(),
            Some(LastCheckpointHint { version: 10, size: Some(12345) })
        );
    }
    
    #[test]
    fn test_from_file_result_no_size() {
        let json = r#"{"version": 5}"#;
        let data = MockEngineData::from_json_str(json);
        let result = LastCheckpointHint::from_file_result(Ok(Box::new(data)));
        
        assert_eq!(
            result.unwrap(),
            Some(LastCheckpointHint { version: 5, size: None })
        );
    }
    
    #[test]
    fn test_from_file_result_empty_file() {
        let data = MockEngineData::empty();
        let result = LastCheckpointHint::from_file_result(Ok(Box::new(data)));
        
        assert_eq!(result.unwrap(), None);
    }
    
    #[test]
    fn test_from_file_result_not_found() {
        let result = LastCheckpointHint::from_file_result(
            Err(Error::FileNotFound("_last_checkpoint".into()))
        );
        
        assert_eq!(result.unwrap(), None);
    }
    
    #[test]
    fn test_from_file_result_other_error() {
        let result = LastCheckpointHint::from_file_result(
            Err(Error::Generic("Permission denied".into()))
        );
        
        assert!(result.is_err());
    }
    
    #[test]
    fn test_from_file_result_invalid_json() {
        let json = r#"{"not_version": 10}"#;
        let data = MockEngineData::from_json_str(json);
        let result = LastCheckpointHint::from_file_result(Ok(Box::new(data)));
        
        assert!(result.is_err());
    }
}
```

**Testing benefits**:
- ✅ No I/O mocks needed (pure functions)
- ✅ Fast tests (no actual file I/O)
- ✅ Comprehensive coverage (all branches)
- ✅ Tests are deterministic

### C.6 Testing Choreography (Integration Tests)

```rust
#[cfg(test)]
mod integration_tests {
    use super::*;
    use crate::engine::default::DefaultEngine;
    
    #[test]
    fn test_try_read_with_real_table() {
        let table_root = Url::parse("file:///path/to/test/table").unwrap();
        let engine = DefaultEngine::new();
        
        let result = LastCheckpointHint::try_read(&table_root, engine.storage_handler());
        
        // This test requires a real table with _last_checkpoint file
        match result {
            Ok(Some(hint)) => {
                assert!(hint.version > 0);
            }
            Ok(None) => {
                // No checkpoint file, that's OK for new tables
            }
            Err(e) => panic!("Unexpected error: {:?}", e),
        }
    }
    
    #[tokio::test]
    async fn test_try_read_async_with_real_table() {
        let table_root = Url::parse("file:///path/to/test/table").unwrap();
        let engine = DefaultAsyncEngine::new();
        
        let result = LastCheckpointHint::try_read_async(&table_root, engine.async_storage_handler())
            .await;
        
        // Same assertions as sync version
        match result {
            Ok(Some(hint)) => {
                assert!(hint.version > 0);
            }
            Ok(None) => {}
            Err(e) => panic!("Unexpected error: {:?}", e),
        }
    }
}
```

### C.7 Pattern A Summary

**When to use Pattern A**:
- ✅ One-shot operations (single file read)
- ✅ No iteration required
- ✅ No state accumulation
- ✅ Simple result extraction

**Pattern A structure**:
1. **I/O-free helper** (`from_file_result`): Takes `DeltaResult<Data>`, returns `DeltaResult<Output>`
2. **Sync choreography** (`try_read`): Triggers I/O, delegates to helper
3. **Async choreography** (`try_read_async`): Same as sync but with `.await`

**Code duplication**:
- Helper function: **0% duplication** (shared)
- Choreography: **~15 lines per variant** (acceptable boilerplate)
- Total duplication: **< 20%** of total code

**Benefits**:
- ✅ Helper is I/O-free and easily testable
- ✅ Choreography is thin and obvious
- ✅ Same error handling in both sync and async
- ✅ Easy to add new variants (e.g., parallel async)

---

## Appendix D: Complete Pattern B Example - MetadataExtractor

**Why This Exists**: Pattern B is for iterative, stateful processing with early exit. This appendix shows the complete, production-ready implementation of `MetadataExtractor` for extracting Protocol and Metadata from action log batches.

### D.1 The Processor State Machine

```rust
/// Processor for extracting Protocol and Metadata from action log batches
///
/// This is a state machine that accumulates Protocol and Metadata actions
/// until both are found (early exit) or all batches are exhausted.
pub struct MetadataExtractor {
    protocol: Option<Protocol>,
    metadata: Option<Metadata>,
}

impl MetadataExtractor {
    /// Create a new extractor
    pub fn new() -> Self {
        Self {
            protocol: None,
            metadata: None,
        }
    }
    
    /// Process one batch of actions (I/O-free)
    ///
    /// Returns:
    /// - `Break((Protocol, Metadata))` if both found (early exit)
    /// - `Continue(self)` if need more batches
    /// - `Err(...)` on validation errors
    pub fn process_batch(
        mut self,
        batch: ActionsBatch,
    ) -> DeltaResult<ControlFlow<(Protocol, Metadata), Self>> {
        // Extract actions from batch
        for action in batch.actions {
            match action {
                Action::Protocol(p) => {
                    self.validate_and_set_protocol(p)?;
                }
                Action::Metadata(m) => {
                    self.validate_and_set_metadata(m)?;
                }
                _ => {
                    // Ignore other action types
                }
            }
            
            // Early exit if we have both
            if let (Some(protocol), Some(metadata)) = (self.protocol.as_ref(), self.metadata.as_ref()) {
                return Ok(ControlFlow::Break((protocol.clone(), metadata.clone())));
            }
        }
        
        // Need more batches
        Ok(ControlFlow::Continue(self))
    }
    
    /// Validate and set protocol (ensures only one Protocol action)
    fn validate_and_set_protocol(&mut self, protocol: Protocol) -> DeltaResult<()> {
        if self.protocol.is_some() {
            return Err(Error::Generic(
                "Multiple Protocol actions found in log".into()
            ));
        }
        self.protocol = Some(protocol);
        Ok(())
    }
    
    /// Validate and set metadata (ensures only one Metadata action)
    fn validate_and_set_metadata(&mut self, metadata: Metadata) -> DeltaResult<()> {
        if self.metadata.is_some() {
            return Err(Error::Generic(
                "Multiple Metadata actions found in log".into()
            ));
        }
        self.metadata = Some(metadata);
        Ok(())
    }
    
    /// Check if extraction is complete
    pub fn is_complete(&self) -> bool {
        self.protocol.is_some() && self.metadata.is_some()
    }
}
```

**Key characteristics**:
- ✅ **I/O-free**: Takes `ActionsBatch`, not `Iterator` or `Stream`
- ✅ **Stateful**: Accumulates `protocol` and `metadata` across batches
- ✅ **Early exit**: Returns `Break` as soon as both are found
- ✅ **Validation**: Ensures only one Protocol and one Metadata action
- ✅ **Testable**: Can test with mock `ActionsBatch` data

### D.2 Sync Choreography

```rust
impl LogSegment {
    /// Read protocol and metadata from log (sync version)
    pub fn read_metadata(
        &self,
        engine: &dyn Engine,
    ) -> DeltaResult<(Protocol, Metadata)> {
        // I/O: Get iterator of action batches
        let batches = self.replay_for_metadata(engine)?;
        
        // Computation: Process batches with early exit
        let result = batches.try_fold(
            MetadataExtractor::new(),
            |processor, batch| {
                processor.process_batch(batch).transpose()
            }
        )?;
        
        // Handle incomplete case
        result.unwrap_break_or_else(|incomplete_processor| {
            Err(Error::Generic(format!(
                "Incomplete metadata after exhausting log: protocol={}, metadata={}",
                incomplete_processor.protocol.is_some(),
                incomplete_processor.metadata.is_some()
            )))
        })
    }
}
```

**Choreography responsibilities**:
1. Trigger I/O (`replay_for_metadata`)
2. Set up `try_fold` with processor
3. Use `.transpose()` to align types
4. Handle incomplete state (no early exit)

### D.3 Async Choreography

```rust
#[cfg(feature = "default-engine-base")]
impl LogSegment {
    /// Read protocol and metadata from log (async version)
    pub async fn read_metadata_async(
        &self,
        engine: &dyn AsyncEngine,
    ) -> DeltaResult<(Protocol, Metadata)> {
        // I/O: Get stream of action batches
        let batches = self.replay_for_metadata_async(engine).await?;
        
        // Computation: Process batches with early exit
        let result = batches.try_fold(
            MetadataExtractor::new(),
            |processor, batch| async move {
                yield_now().await;  // Cooperative yielding
                processor.process_batch(batch).transpose()
            }
        ).await?;
        
        // Handle incomplete case (identical to sync!)
        result.unwrap_break_or_else(|incomplete_processor| {
            Err(Error::Generic(format!(
                "Incomplete metadata after exhausting log: protocol={}, metadata={}",
                incomplete_processor.protocol.is_some(),
                incomplete_processor.metadata.is_some()
            )))
        })
    }
}
```

**Differences from sync version**:
1. `async fn` instead of `fn`
2. `replay_for_metadata_async` with `.await`
3. `async move` closure in `try_fold`
4. `yield_now().await` for cooperative yielding
5. `.await` after `try_fold`
6. **Error handling is identical** (same `unwrap_break_or_else` logic)

### D.4 Testing the Processor (I/O-Free)

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_metadata_extractor_both_in_first_batch() {
        let batch = ActionsBatch {
            actions: vec![
                Action::Protocol(Protocol::default()),
                Action::Metadata(Metadata::default()),
            ],
        };
        
        let result = MetadataExtractor::new()
            .process_batch(batch)
            .unwrap();
        
        assert!(matches!(result, ControlFlow::Break(_)));
    }
    
    #[test]
    fn test_metadata_extractor_across_batches() {
        let batch1 = ActionsBatch {
            actions: vec![Action::Protocol(Protocol::default())],
        };
        let batch2 = ActionsBatch {
            actions: vec![Action::Metadata(Metadata::default())],
        };
        
        let processor = MetadataExtractor::new();
        
        // Process first batch - should continue
        let result1 = processor.process_batch(batch1).unwrap();
        assert!(matches!(result1, ControlFlow::Continue(_)));
        
        let processor = match result1 {
            ControlFlow::Continue(p) => p,
            _ => panic!("Expected Continue"),
        };
        
        // Process second batch - should break
        let result2 = processor.process_batch(batch2).unwrap();
        assert!(matches!(result2, ControlFlow::Break(_)));
    }
    
    #[test]
    fn test_metadata_extractor_incomplete() {
        let batch = ActionsBatch {
            actions: vec![Action::Protocol(Protocol::default())],
        };
        
        let result = MetadataExtractor::new()
            .process_batch(batch)
            .unwrap();
        
        match result {
            ControlFlow::Continue(processor) => {
                assert!(processor.protocol.is_some());
                assert!(processor.metadata.is_none());
                assert!(!processor.is_complete());
            }
            _ => panic!("Expected Continue"),
        }
    }
    
    #[test]
    fn test_metadata_extractor_duplicate_protocol() {
        let batch = ActionsBatch {
            actions: vec![
                Action::Protocol(Protocol::default()),
                Action::Protocol(Protocol::default()),  // Duplicate!
            ],
        };
        
        let result = MetadataExtractor::new().process_batch(batch);
        assert!(result.is_err());
    }
    
    #[test]
    fn test_metadata_extractor_duplicate_metadata() {
        let batch = ActionsBatch {
            actions: vec![
                Action::Metadata(Metadata::default()),
                Action::Metadata(Metadata::default()),  // Duplicate!
            ],
        };
        
        let result = MetadataExtractor::new().process_batch(batch);
        assert!(result.is_err());
    }
    
    #[test]
    fn test_metadata_extractor_ignores_other_actions() {
        let batch = ActionsBatch {
            actions: vec![
                Action::Add(AddFile::default()),
                Action::Protocol(Protocol::default()),
                Action::Remove(RemoveFile::default()),
                Action::Metadata(Metadata::default()),
                Action::Txn(TxnEntry::default()),
            ],
        };
        
        let result = MetadataExtractor::new()
            .process_batch(batch)
            .unwrap();
        
        assert!(matches!(result, ControlFlow::Break(_)));
    }
}
```

**Testing benefits**:
- ✅ No I/O mocks needed (pure state machine)
- ✅ Fast tests (no network/disk)
- ✅ Complete coverage (early exit, incomplete, errors)
- ✅ Tests verify state transitions explicitly

### D.5 Integration Testing with try_fold

```rust
#[cfg(test)]
mod integration_tests {
    use super::*;
    
    #[test]
    fn test_read_metadata_early_exit() {
        // Simulate batches where metadata appears in batch 2
        let batches = vec![
            ActionsBatch { actions: vec![Action::Add(AddFile::default())] },
            ActionsBatch { actions: vec![
                Action::Protocol(Protocol::default()),
                Action::Metadata(Metadata::default()),
            ] },
            ActionsBatch { actions: vec![Action::Add(AddFile::default())] },  // Should not be processed
        ];
        
        let mut batch_count = 0;
        let result = batches.into_iter().try_fold(
            MetadataExtractor::new(),
            |processor, batch| {
                batch_count += 1;
                processor.process_batch(batch).transpose()
            }
        ).unwrap();
        
        // Should have early-exited after batch 2
        assert_eq!(batch_count, 2);
        assert!(matches!(result, ControlFlow::Break(_)));
    }
    
    #[test]
    fn test_read_metadata_incomplete() {
        let batches = vec![
            ActionsBatch { actions: vec![Action::Protocol(Protocol::default())] },
            // No metadata!
        ];
        
        let result = batches.into_iter().try_fold(
            MetadataExtractor::new(),
            |processor, batch| {
                processor.process_batch(batch).transpose()
            }
        ).unwrap();
        
        match result {
            ControlFlow::Continue(processor) => {
                assert!(processor.protocol.is_some());
                assert!(processor.metadata.is_none());
            }
            _ => panic!("Expected Continue for incomplete state"),
        }
    }
}
```

### D.6 Pattern B Summary

**When to use Pattern B**:
- ✅ Iterative processing (multiple items)
- ✅ Stateful accumulation
- ✅ Early exit possible (don't need all items)
- ✅ Error handling during iteration

**Pattern B structure**:
1. **Processor** (state machine): Has `process_batch(self, item) -> Result<ControlFlow<Output, Self>>`
2. **Sync choreography**: Uses `Iterator::try_fold` with `.transpose()`
3. **Async choreography**: Uses `Stream::try_fold` (via `ControlFlowStreamExt`) with `.transpose()`

**Code duplication**:
- Processor: **0% duplication** (shared)
- Choreography: **~20 lines per variant** (mainly `async move` and `.await`)
- Total duplication: **< 25%** of total code

**Benefits**:
- ✅ Processor is I/O-free and easily testable
- ✅ Early exit works automatically (via `ControlFlow`)
- ✅ Same error handling in sync and async
- ✅ Cooperative yielding easy to add (just one line)
- ✅ Processor can be reused in different contexts (testing, power users)

---

## Appendix E: Async Trait Hierarchy - Complete Definitions

**Why This Exists**: This appendix provides the complete async trait definitions for reference. These are parallel to the existing sync traits but return `Stream`s instead of `Iterator`s.

### E.1 Type Aliases for Streams

```rust
use futures::stream::Stream;
use std::pin::Pin;

/// Generic boxed async stream with error handling
pub type DeltaStream<T> = Pin<Box<dyn Stream<Item = DeltaResult<T>> + Send>>;

/// Specific stream types used throughout the async API
pub type FileDataReadResultStream = DeltaStream<Box<dyn EngineData>>;
pub type FileMetaStream = DeltaStream<FileMeta>;
pub type BytesStream = DeltaStream<Bytes>;
```

###E.2 AsyncStorageHandler

```rust
#[cfg(feature = "default-engine-base")]
#[async_trait::async_trait]
pub trait AsyncStorageHandler: Send + Sync {
    /// Read multiple files and return a stream of results
    async fn read_files(
        &self,
        files: Vec<FileMeta>,
    ) -> DeltaResult<FileDataReadResultStream>;
}
```

### E.3 AsyncJsonHandler

```rust
#[cfg(feature = "default-engine-base")]
#[async_trait::async_trait]
pub trait AsyncJsonHandler: Send + Sync {
    /// Read JSON files and return a stream of EngineData
    async fn read_json_files(
        &self,
        files: &[FileMeta],
        schema: &SchemaRef,
        predicate: Option<ExpressionRef>,
    ) -> DeltaResult<FileDataReadResultStream>;
}
```

### E.4 AsyncParquetHandler

```rust
#[cfg(feature = "default-engine-base")]
#[async_trait::async_trait]
pub trait AsyncParquetHandler: Send + Sync {
    /// Read Parquet files and return a stream of EngineData
    async fn read_parquet_files(
        &self,
        files: &[FileMeta],
        schema: &SchemaRef,
        predicate: Option<ExpressionRef>,
    ) -> DeltaResult<FileDataReadResultStream>;
}
```

### E.5 AsyncEngine - Top-Level Trait

```rust
#[cfg(feature = "default-engine-base")]
pub trait AsyncEngine: Send + Sync {
    /// Get the async storage handler
    fn async_storage_handler(&self) -> &dyn AsyncStorageHandler;
    
    /// Get the async JSON handler
    fn async_json_handler(&self) -> &dyn AsyncJsonHandler;
    
    /// Get the async Parquet handler
    fn async_parquet_handler(&self) -> &dyn AsyncParquetHandler;
    
    /// Get the evaluation handler (shared with sync!)
    ///
    /// Note: EvaluationHandler is CPU-only (expression evaluation on
    /// in-memory data), so it doesn't need an async variant.
    fn evaluation_handler(&self) -> Arc<dyn EvaluationHandler>;
}
```

### E.6 Why Separate Traits (Not Combined)

**Design decision**: We use separate trait hierarchies (`Engine` vs `AsyncEngine`) rather than:
1. **Combined trait with async methods**: Would complicate trait objects
2. **Generic trait**: Can't conditionally make methods `async` based on type parameters

**Rationale**:
- Clear separation of sync vs async semantics
- Methods can be truly `async fn` (not just returning futures)
- Users can implement only sync, only async, or both
- No runtime cost for sync-only users
- Trait objects are straightforward: `&dyn Engine` vs `&dyn AsyncEngine`

### E.7 Relationship to Sync Traits

```rust
// Sync traits (existing)
pub trait Engine {
    fn storage_handler(&self) -> &dyn StorageHandler;
    fn json_handler(&self) -> &dyn JsonHandler;
    fn parquet_handler(&self) -> &dyn ParquetHandler;
    fn evaluation_handler(&self) -> Arc<dyn EvaluationHandler>;  // Shared!
}

pub trait StorageHandler {
    fn read_files(&self, files: Vec<FileMeta>) 
        -> DeltaResult<Box<dyn Iterator<Item = DeltaResult<Box<dyn EngineData>>>>>;
}

// ... similar for JsonHandler, ParquetHandler
```

**Key differences**:
1. **Async trait methods**: `async fn` instead of `fn`
2. **Return type**: `DeltaStream<T>` instead of `Box<dyn Iterator<...>>`
3. **Naming**: `AsyncEngine`, `AsyncStorageHandler`, etc.
4. **EvaluationHandler**: Shared between sync and async (no I/O)

### E.8 Implementation Example

```rust
#[cfg(feature = "default-engine-base")]
pub struct DefaultAsyncEngine {
    storage: Arc<dyn AsyncStorageHandler>,
    json: Arc<dyn AsyncJsonHandler>,
    parquet: Arc<dyn AsyncParquetHandler>,
    evaluation: Arc<dyn EvaluationHandler>,  // Shared with sync!
}

#[cfg(feature = "default-engine-base")]
impl AsyncEngine for DefaultAsyncEngine {
    fn async_storage_handler(&self) -> &dyn AsyncStorageHandler {
        &*self.storage
    }
    
    fn async_json_handler(&self) -> &dyn AsyncJsonHandler {
        &*self.json
    }
    
    fn async_parquet_handler(&self) -> &dyn AsyncParquetHandler {
        &*self.parquet
    }
    
    fn evaluation_handler(&self) -> Arc<dyn EvaluationHandler> {
        Arc::clone(&self.evaluation)
    }
}
```

### E.9 Cooperative Yielding Helper

```rust
/// Yields control back to the executor, allowing other tasks to run.
/// This is executor-agnostic and works with any async runtime.
#[inline]
pub async fn yield_now() {
    /// Simple future that yields once
    struct YieldNow {
        yielded: bool,
    }
    
    impl std::future::Future for YieldNow {
        type Output = ();
        
        fn poll(
            mut self: Pin<&mut Self>,
            cx: &mut std::task::Context<'_>,
        ) -> std::task::Poll<()> {
            if self.yielded {
                std::task::Poll::Ready(())
            } else {
                self.yielded = true;
                cx.waker().wake_by_ref();
                std::task::Poll::Pending
            }
        }
    }
    
    YieldNow { yielded: false }.await
}
```

**Usage guideline**: Call `yield_now().await` after processing each batch in async choreography to prevent executor starvation.

---

## Appendix F: Detailed I/O vs Computation Analysis

**Why This Exists**: This appendix provides the detailed breakdown of Control Flow 1 functions, categorizing them by whether they need async variants.

### F.1 Category Breakdown (20 Total Functions)

**Category 1: Pure I/O** (3 functions, 15%)
- `LogSegment::list_log_files` - Lists files from storage
- `LogSegment::list_commits` - Lists commit files
- `LogSegment::list_checkpoints` - Lists checkpoint files

**Category 2: Pure Computation** (10 functions, 50%)
- `LogSegment::parse_log_path` - String parsing
- `LogSegment::checkpoint_file_to_version` - Path to version conversion
- `LogSegment::commit_file_to_version` - Path to version conversion
- `LogSegment::extract_version` - Regex extraction
- `Protocol::validate` - Business logic validation
- `Metadata::validate` - Business logic validation
- `Schema::validate` - Business logic validation
- All validation helper functions
- All conversion/transformation functions

**Category 3: Mixed (I/O + Computation)** (7 functions, 35%)
- `LastCheckpointHint::try_read` - Pattern A
- `LogSegment::read_metadata` - Pattern B
- `LogSegment::phase1_sync` - Pattern C (Phase 1)
- `Phase1Result::process_sidecars_sync` - Pattern C (Phase 2)
- `Snapshot::try_new` - Top-level coordinator
- `Snapshot::protocol_and_metadata` - Metadata extraction coordinator
- `Snapshot::build` - Table building coordinator

### F.2 Async Virality Analysis

**Functions needing async variants**: 7 mixed + 3 pure I/O = **10 functions (50%)**

But wait! The 3 pure I/O functions are internal helpers, not public API. So from a **public API perspective**:

**Public functions needing async variants**: **7 mixed functions (35%)**

**Shared computation functions**: **10 pure computation functions (50%)**

**Internal I/O helpers**: **3 pure I/O functions (15%)**

### F.3 Code Sharing Metrics

```
Total lines of code: ~800 (estimated for Control Flow 1)

Processors + Helpers (shared): ~400 lines (50%)
Sync choreography: ~200 lines (25%)
Async choreography: ~200 lines (25%)

Duplication: ~200 lines of 800 total = 25%
```

**Key insight**: 75% of code is either shared (processors/helpers) or sync-only. Only 25% is duplicated for async.

---

## Appendix G: Code Metrics and Async Virality

**Why This Exists**: Detailed metrics showing the actual impact of async support on codebase size and complexity.

### G.1 Function Count by Category

| Category | Count | Percentage | Needs Async? |
|----------|-------|------------|--------------|
| Pure I/O (internal) | 3 | 15% | Yes (internal only) |
| Pure Computation | 10 | 50% | No (shared) |
| Mixed Choreography | 7 | 35% | Yes (public API) |
| **Total** | **20** | **100%** | **35% public API** |

### G.2 Line Count by Component

| Component | Lines | Percentage | Duplication |
|-----------|-------|------------|-------------|
| Processors (Pattern B/C) | 250 | 31% | 0% (shared) |
| Helpers (Pattern A) | 150 | 19% | 0% (shared) |
| Sync Choreography | 200 | 25% | N/A (original) |
| Async Choreography | 200 | 25% | 100% (vs sync) |
| **Total** | **800** | **100%** | **25% overall** |

### G.3 Async Virality - Detailed View

**Starting point**: `Snapshot::try_new_async`

```
Snapshot::try_new_async (async)
  ├─→ LastCheckpointHint::try_read_async (async)
  │     └─→ LastCheckpointHint::from_file_result (shared!)
  ├─→ LogSegment::phase1_async (async)
  │     └─→ Phase1InProgress::process_batch (shared!)
  └─→ Phase1Result::process_sidecars_async (async)
        └─→ Phase1InProgress::process_batch (shared!)
```

**Key observation**: Only the thin choreography layers are async. All business logic is shared.

### G.4 Patterns by Complexity

| Pattern | Functions | Avg Lines | Processor Lines | Choreography Lines | Duplication |
|---------|-----------|-----------|-----------------|-------------------|-------------|
| A (Helper) | 1 | 60 | 30 (shared) | 15 (sync) + 15 (async) | 25% |
| B (Processor) | 1 | 250 | 150 (shared) | 50 (sync) + 50 (async) | 20% |
| C (Two-Phase) | 2 | 600 | 300 (shared) | 150 (sync) + 150 (async) | 25% |

**Average duplication across all patterns**: **~23%**

### G.5 Testing Impact

| Test Category | Count | Needs I/O Mocks? | Speed |
|---------------|-------|------------------|-------|
| Processor unit tests | 25 | No | Fast (ms) |
| Helper unit tests | 10 | No | Fast (ms) |
| Sync integration tests | 15 | Yes | Slow (s) |
| Async integration tests | 15 | Yes | Slow (s) |
| **Total** | **65** | **46% need mocks** | **54% fast** |

**Key insight**: More than half of tests are I/O-free and fast.

### G.6 API Surface Impact

**Before refactoring** (sync only):
- Public functions: 7
- Internal helpers: 13
- Total: 20

**After refactoring** (sync + async):
- Public sync functions: 7 (unchanged)
- Public async functions: 7 (new)
- Shared processors/helpers: 13 (unchanged)
- Internal I/O helpers: 3 (refactored)
- Total: 30 functions (50% increase)

**But**: 13 functions (43%) are shared, so actual code increase is much less than 50%.

### G.7 Maintenance Burden

**Changes that require updating both sync and async**:
1. Business logic validation rules → Update processor (1 place)
2. Error handling logic → Update helper or processor (1 place)
3. Early exit conditions → Update processor (1 place)
4. New action types → Update processor (1 place)

**Changes that require updating only choreography**:
1. File listing strategy → Update I/O helpers (2 places: sync + async)
2. Batch size tuning → Update choreography (2 places)
3. Cooperative yielding frequency → Update async choreography only (1 place)

**Ratio**: ~80% of changes only require updating shared code (1 place), ~20% require updating sync + async choreography (2 places).

---
