# Async/Sync Pattern Generalizability Analysis

## Overview

This document analyzes the generalizability of the async/sync patterns proposed in `async-build-snapshot-proposal.md` by examining other major kernel entry points. The original proposal identified patterns A, B, and C for snapshot building. This analysis confirms those patterns generalize well, and identifies two additional patterns (D and E) needed for other operations.

## Terminology

**Important distinction**:
- **"Phase"**: Pattern C has two **phases** where Phase 1 reads a manifest to discover what to fetch in Phase 2 (e.g., checkpoint→sidecars). Can't start Phase 2 until Phase 1 completes discovery.
- **"Pass"**: Some operations make multiple **passes** over the same already-fetched data (e.g., CDF reads each commit twice to discover metadata then apply selection).

---

## Methodology

For each entry point, we:
1. Trace the current call graph (similar to Section 8.1 of the proposal)
2. Identify functions involved, distinguishing previously-analyzed ones from new ones
3. Categorize new functions according to Patterns A, B, or C
4. Describe functions that don't fit any existing pattern and why

This process identified two new patterns (D and E) needed for operations not covered by the original proposal.

---

## Pattern Summary

From the proposal and this analysis, we've identified **5 patterns** for async/sync code sharing:

| Pattern | Description | When to Use | Key Tools | Details |
|---------|-------------|-------------|-----------|---------|
| **A: Helper Functions** | One-shot I/O operations | Read file → parse → done | `async fn` + `.await` | Proposal §4 |
| **B: Processor + try_fold** | Stateful iteration with accumulation | Process batches, track state, may exit early | `try_fold`, `ControlFlow`, `.transpose()` | Proposal §5 |
| **C: Two-Phase Processing** | Discovery then fetch | Can't know what to fetch until Phase 1 completes | `Phase1Result`, `process_sidecars` | Proposal §6 |
| **D: Nested Stream Processing** 🆕 | Nested iteration with I/O in outer loop | Multiple levels of iteration, stateless | `Stream::then()`, `flatten()`, `yield_now()` | §3.6 below |
| **E: Two-Pass Per-Item** 🆕 | Aggregate metadata, then reprocess same data | Can't rewind stream, need aggregated state | Pattern B (pass 1) + `then()` (pass 2) | §4.5 below |

**Key**: Patterns A-C from proposal, D-E discovered in this analysis.

**Pattern relationships**:
- Pattern C uses Pattern B for Phase 2 processing
- Pattern E uses Pattern B for Pass 1 aggregation
- Pattern D is stateless (no Pattern B)

---

## Summary of Findings

All 7 kernel entry points have been analyzed. Here are the key findings:

### Pattern Applicability

| Entry Point | Pattern A | Pattern B | Pattern C | Pattern D | Pattern E |
|-------------|-----------|-----------|-----------|-----------|-----------|
| 1. Snapshot Building | ✅ | ✅ | ✅ | ❌ | ❌ |
| 2. Scan Metadata | ✅ | ✅ | ✅ * | ❌ | ❌ |
| 3. Scan Execute | ✅ | ❌ | ❌ | ✅ | ❌ |
| 4. Table Changes (CDF) | ✅ | Pattern E † | ❌ | ❌ | ✅ |
| 5. Transaction Commit | ✅ | ❌ | ❌ | ❌ | ❌ |
| 6. Checkpoint Writing | ✅ | ✅ | ✅ * | ❌ | ❌ |
| 7. Log Compaction | ✅ | ✅ | ✅ * | ❌ | ❌ |

\* Pattern C dependency (via `read_actions`)  
† Pattern E's Pass 1 uses Pattern B

### Critical Dependencies

**Pattern C is a hard dependency** for 5 out of 7 entry points:
1. ✅ Snapshot Building (proposal doc)
2. ✅ Scan Metadata (via `read_actions`)
3. ✅ Scan Execute (via Scan Metadata)
4. ✅ Checkpoint Writing (via `read_actions`)
5. ✅ Log Compaction (via `read_actions`)

**Without Pattern C refactoring**, only Table Changes (#4) and Transaction Commit (#5) can be made async independently.

### Pattern Generalizability

1. **Pattern A (Helper Functions)**: Universal - applies to all 7 entry points
2. **Pattern B (Processor + try_fold)**: Highly generalizable - applies to 4 entry points directly (#1, #2, #6, #7), plus Pattern E's Pass 1 (#4)
3. **Pattern C (Two-Phase)**: Critical shared infrastructure - enables 5 entry points
4. **Pattern D (Nested Streams)**: Specialized - applies to Scan Execute (#3) for stateless nested processing
5. **Pattern E (Two-Pass)**: Specialized - applies to Table Changes (#4) for per-item aggregation + processing

### Code Sharing Success

The analysis confirms excellent code sharing:

| Entry Point | Async LOC | I/O-Free LOC | Duplication |
|-------------|-----------|--------------|-------------|
| 2. Scan Metadata | ~115 | ~670 | ~15% |
| 3. Scan Execute | ~60 | ~220 | ~21% |
| 4. Table Changes | ~140 | ~500 | ~22% |
| 5. Transaction Commit | ~25 | ~85 | ~23% |
| 6. Checkpoint Writing | ~95 | ~470 | ~17% |
| 7. Log Compaction | ~70 | ~505 | ~12% |

**Average duplication**: ~18% (excellent, well below the 50% threshold)

### Functions That Don't Fit Existing Patterns

**None!** All functions across all 7 entry points fit into Patterns A, B, C, D, or E. This validates the comprehensiveness of the pattern set.

### Implementation Sequencing

Based on dependencies, the recommended implementation order is:

1. **Phase 1**: Pattern C refactoring (`read_actions`)
   - Enables: Snapshot Building, Scan Metadata, Checkpoint Writing, Log Compaction
   - Marginal cost per entry point: ~25 lines

2. **Phase 2**: Scan Execute (Pattern D)
   - Depends on: Scan Metadata (Pattern C)
   - Marginal cost: ~60 lines

3. **Phase 3**: Independent entry points (Patterns A, E)
   - Table Changes (Pattern E, which uses Pattern B)
   - Transaction Commit (Pattern A only)
   - Total marginal cost: ~165 lines

**Total estimated async LOC across all entry points**: ~505 lines  
**Total estimated I/O-free shared LOC**: ~2,450 lines  
**Overall duplication**: ~17% (excellent)

---

## Main Kernel Entry Points

Based on the public API, delta-kernel-rs has **7 major entry points**:

1. **[Snapshot Building](#entry-point-1-snapshot-building)** - Create snapshot at version
   - Analyzed in `async-build-snapshot-proposal.md`
   - Patterns: A, B, C

2. **[Scan Metadata](#entry-point-2-scan-metadata)** - Get file list for scan
   - Patterns: A, B, C (via `read_actions`)

3. **[Scan Execute](#entry-point-3-scan-execute)** - Read actual parquet data
   - Patterns: A, D (nested streams)

4. **[Table Changes (CDF)](#entry-point-4-table-changes-cdf)** - Read change data feed
   - Patterns: A, E (two-pass per-commit)

5. **[Transaction Commit](#entry-point-5-transaction-commit)** - Write new commits
   - Patterns: A only

6. **[Checkpoint Writing](#entry-point-6-checkpoint-writing)** - Write checkpoint file
   - Patterns: A, B, C (via `read_actions`)

7. **[Log Compaction](#entry-point-7-log-compaction)** - Compact commit range
   - Patterns: A, B, C (via `read_actions`)

**Analysis Approach**: For each entry point, trace call graphs, identify new functions, categorize by pattern, and highlight functions that don't fit existing patterns.

**Call Graph Legend**:
- 📍 = Entry point
- 🔄 = Previously analyzed (link to section)
- 🆕 = New function (this analysis)
- CPU ✅ = Pure computation, no I/O (shared between sync/async)
- I/O ⚠️ = Contains or orchestrates I/O
- I/O 💥 = Direct I/O operation
- ✓ = Already analyzed in proposal doc

---

## Entry Point #2: Scan Metadata

**API**: `Scan::scan_metadata(engine) -> impl Iterator<DeltaResult<ScanMetadata>>`

**Purpose**: Get list of files to scan with pruning and deduplication.

---

### 2.1 Call Graph

```
User: scan.scan_metadata(engine)
    ↓
Scan::scan_metadata(engine)                                    🆕 async wrapper
    ├─ Scan::replay_for_scan_metadata(engine)                  🆕 async wrapper
    │   └─ log_segment.read_actions(...)                       ✓ Pattern C Sec 10
    │
    └─ Scan::scan_metadata_inner(engine, iter)                 🆕 async wrapper
        └─ scan_action_iter(engine, iter, state_info)          🆕 creates processor
            └─ ScanLogReplayProcessor                          🆕 Pattern B CPU ✅
                ├─ ::new()                                     🆕 CPU ✅
                └─ ::process_actions_batch()                   🆕 CPU ✅
                    ├─ DataSkippingFilter::apply()             🆕 CPU ✅
                    ├─ AddRemoveDedupVisitor                   🆕 CPU ✅
                    └─ ScanMetadata::try_new()                 🆕 CPU ✅

────────────────────────────────────────────────────────────
Previously Analyzed (Snapshot Building):
  ✓ log_segment.read_actions          Pattern C, Sec 10

New Functions:
  🆕 Scan::scan_metadata               3 async wrappers (~19 lines total)
  🆕 Scan::replay_for_scan_metadata
  🆕 Scan::scan_metadata_inner
  🆕 ScanLogReplayProcessor            Pattern B processor + 5 support fns
                                       (~385 lines CPU ✅ all shared)

Duplication: ~7% (19 async lines vs 385 shared lines)
```

---

### 2.2 Key Observations

1. **Control Flow**: Entry → `read_actions()` (Pattern C) → Iterator → `ScanLogReplayProcessor` (Pattern B)

2. **ScanLogReplayProcessor is Pattern B**:
   - I/O-free state machine implementing `LogReplayProcessor` trait
   - Stateful (deduplication via `seen_file_keys`)
   - No early exit (must process all batches)
   - Applies data skipping (CPU-only stats filtering)

3. **vs. Snapshot Building**:
   - No manifest/sidecar split (no Pattern C needed)
   - No early exit (must find all files)
   - Already I/O-free and testable

---

### 2.3 Function Classification

**I/O Functions (need async)**: 3 thin orchestration wrappers (~19 lines total)
- `Scan::scan_metadata`, `replay_for_scan_metadata`, `scan_metadata_inner`

**CPU Functions (shared)**: 7 functions (~385 lines total)
- `ScanLogReplayProcessor` (Pattern B processor)
- `DataSkippingFilter::apply` (stats filtering)
- `AddRemoveDedupVisitor`, `FileActionDeduplicator` (deduplication)
- Result construction helpers

---

### 2.4 Pattern Categorization

#### Pattern B: Processor + try_fold

**Perfect Match**: `ScanLogReplayProcessor` implements the `LogReplayProcessor` trait (I/O-free state machine with stateful deduplication, no early exit).

**Key Discovery**: The `LogReplayProcessor` trait codifies Pattern B as a reusable abstraction. Current implementations:
- `ScanLogReplayProcessor` (this entry point)
- `ActionReconciliationProcessor` (checkpoint/compaction)

**Implication**: Checkpoint writing and log compaction will likely show similar excellent metrics (~7% duplication) since they use the same trait-based Pattern B design.

---

#### Thin Choreography Functions

These follow Pattern B choreography (just orchestrate I/O + processor):

| Function | Lines | Async Difference | Pattern |
|----------|-------|------------------|---------|
| `scan_metadata` | ~5 | `async fn` + `.await` | Pattern B entry |
| `replay_for_scan_metadata` | ~8 | `.await` on `read_actions` | Pattern B (calls C) |
| `scan_metadata_inner` | ~6 | `.await` on `process_actions_iter` | Pattern B wrapper |

All three are thin wrappers with minimal code duplication.

---

#### I/O-Free Support Functions

All CPU-only, shared between sync and async:

| Function | Purpose | Complexity |
|----------|---------|------------|
| `ScanLogReplayProcessor::new` | Constructor | Simple (~30 lines) |
| `ScanLogReplayProcessor::process_actions_batch` | Main processor logic | Medium (~100 lines) |
| `DataSkippingFilter::apply` | Stats-based pruning | Medium (~50 lines) |
| `AddRemoveDedupVisitor::visit` | Reconcile Add/Remove | High (~150 lines) |
| `FileActionDeduplicator::*` | Track seen files | Simple (~50 lines total) |
| `ScanMetadata::try_new` | Construct result | Simple (~5 lines) |

Total: **~385 lines of I/O-free code** (shared!)

---

### 2.5 Functions That Don't Fit Patterns

**Good News: Everything Fits!**

All functions in Scan Metadata fit the existing patterns:

✅ **Pattern B**: `ScanLogReplayProcessor` (already I/O-free, uses `LogReplayProcessor` trait)
✅ **Pattern C**: `LogSegment::read_actions` (shared with snapshot building)
✅ **Thin Choreography**: Entry points (5-10 line wrappers needing `async fn` + `.await`)
✅ **Pure CPU**: All support functions (~385 LOC shared between sync/async)

---

#### Deep Dive: read_actions Hidden Complexity

**Critical Discovery**: Examining `log_segment.rs:288-493` reveals significant hidden complexity in `read_actions`:

#### Current Implementation Complexity

```rust
fn read_actions(...) -> Iterator<ActionsBatch> {
    let commits = json_handler.read_json_files(...)?;  // Simple
    let checkpoints = create_checkpoint_stream(...)?;   // COMPLEX!
    Ok(commits.chain(checkpoints))
}

fn create_checkpoint_stream(...) -> Iterator<ActionsBatch> {
    let need_file_actions = schema.contains(ADD) || schema.contains(REMOVE);
    
    // Schema validation (CPU ✅)
    // File format dispatch: JSON vs Parquet (CPU ✅)
    let checkpoint_batches = handler.read_*_files(...)?;
    
    // ❌ NESTED I/O: For each checkpoint batch, conditionally read sidecars!
    checkpoint_batches.map(|batch| {
        let sidecars = if need_file_actions && single_part_checkpoint {
            Self::process_sidecars(batch)?  // ← Triggers MORE file reads!
        } else {
            None
        };
        chain(batch, sidecars)  // Returns iterator-of-iterators
    })
    .flatten_ok()  // Flatten the nested iterators
}
```

**Problems**:
1. **Schema-dependent behavior**: Whether sidecars are read depends on runtime schema
2. **Conditional nested I/O**: `process_sidecars` is called PER checkpoint batch (in `.map()` closure)
3. **Iterator-in-iterator pattern**: `.map().flatten_ok()` hides nested I/O
4. **Discovery embedded in iteration**: Sidecar fetching happens lazily during iteration

This is **exactly the complexity** the proposal doc encountered (see `async-build-snapshot-critique.md` Issue 4).

---

#### Refactoring Approach

**Pattern C applies to scans**: The critique (Issue 4) already identified nested I/O in `read_actions` as a critical problem requiring two-phase processing. Applying Pattern C to scans adds ~30 lines of scan-specific wrappers, while the Pattern C infrastructure (`Phase1InProgress`, `Phase1Result`, `read_actions_phase1/phase2`) is provided by the snapshot refactor.

---

### 2.6 Metrics Summary

#### Already Provided by Snapshot Refactor (Free for Scans):
- ✓ `LogSegment::read_actions_phase1_async` (~20 lines) [shared] - reads manifest, collects sidecar refs
- ✓ `Phase1InProgress<P>` wrapper [shared] - holds sidecar references
- ✓ `Phase1Result<P>` enum [shared] - result with optional sidecar refs
- ✓ `Phase1Result::process_sidecars_async` (~20 lines) [shared] - fetches sidecars
- ✓ Pattern C infrastructure established

#### Scan-Specific Additions (Marginal Cost):
1. `Scan::scan_metadata_async` - entry point wrapper (~5 lines)
2. `Scan::replay_for_scan_metadata_phase1_async` - calls shared `read_actions_phase1_async` (~10 lines)
3. `Scan::scan_metadata_phase2_async` - calls shared `process_sidecars_async` (~15 lines)

**Metrics**:
- Scan async choreography: ~30 lines (3 thin wrappers)
- Scan processor logic: ~385 lines (I/O-free, shared)
- Marginal duplication: ~30 lines (~7%)

---

## Entry Point #3: Scan Execute

**API**: `Scan::execute(engine) -> Iterator<DeltaResult<ScanResult>>`

**Purpose**: Read actual data from parquet files, apply row-level transformations, handle deletion vectors.

---

### 3.1 Call Graph

```
Scan::execute [📍 Entry Point]
├─ Scan::scan_metadata [🔄 Already Analyzed - Entry Point #2]
│  └─ ... (see Entry Point #2)
├─ ScanMetadata::visit_scan_files [CPU ✅]
│  └─ ScanFileVisitor::visit [CPU ✅]
│     └─ scan_metadata_callback [CPU ✅]
└─ [for each scan file] (in .map iterator)
   ├─ DvInfo::get_selection_vector [I/O ⚠️]
   │  ├─ DvInfo::get_treemap [I/O ⚠️]
   │  │  └─ DeletionVectorDescriptor::read [I/O ⚠️]
   │  │     └─ StorageHandler::read_files [I/O 💥]
   │  └─ deletion_treemap_to_bools [CPU ✅]
├─ ParquetHandler::read_parquet_files [I/O 💥]
└─ [for each row group batch] (in nested .map iterator)
   ├─ state::transform_to_logical [CPU ✅]
   │  └─ ExpressionEvaluator::evaluate [CPU ✅]
   └─ split_vector [CPU ✅]
```

---

### 3.2 Key Observations

1. **Double-nested I/O**: Outer loop reads files+DVs, inner loop processes batches
   → `Iterator<Iterator<ScanResult>>`

2. **I/O**: DV read (per file if present), parquet read (per file, multiple batches)

3. **CPU**: Expression evaluation is I/O-free (in-memory only)

4. **Pattern**: Not B/C (stateless), requires new Pattern D (nested streams)

---

### 3.3 Focus: New Functions Only

**Already analyzed**: Entry Point #2 (`Scan::scan_metadata`)

**New functions**:
1. ✅ `Scan::execute` - Entry point orchestration
2. ✅ `ScanMetadata::visit_scan_files` - Visitor pattern for metadata
3. ✅ `DvInfo::get_selection_vector` - Deletion vector orchestration
4. ✅ `DvInfo::get_treemap` - Deletion vector orchestration
5. ✅ `DeletionVectorDescriptor::read` - Deletion vector I/O
6. ✅ `state::transform_to_logical` - Apply transformations

---

### 3.4 Function Classification

**I/O Functions (need async)**: 4 functions (~193 lines)
- `Scan::execute` (nested I/O, high complexity)
- `DvInfo::get_selection_vector`, `get_treemap` (trivial I/O wrappers)
- `DeletionVectorDescriptor::read` (storage I/O + parsing, medium complexity)

**CPU Functions (shared)**: 5 functions (~88 lines)
- `ScanFileVisitor::visit` (metadata extraction)
- `transform_to_logical` (expression evaluation)
- DV/selection vector helpers

---

### 3.5 Pattern Categorization

#### Pattern A: Helper Functions ✅ (Perfect Fit)

**Applies to**: `DvInfo::get_selection_vector` / `get_treemap` / `DeletionVectorDescriptor::read`

One-shot operations (read DV file → parse → return treemap). Async variant just adds `.await` to storage call; ~50 LOC of parsing logic is I/O-free.

---

#### The Real Pattern: Nested I/O in Iterators

**Core challenge**: `execute` has I/O operations **inside iterator closures**:

```rust
// Current (simplified):
fn execute() -> Iterator<DeltaResult<ScanResult>> {
    let scan_files = self.scan_metadata()?;  // Iterator of metadata
    
    scan_files
        .map(|file| {
            let dv = file.dv_info.get_selection_vector()?;  // ← I/O per file!
            let batches = engine.read_parquet_files(...)?;   // ← I/O per file!
            
            batches.map(|batch| {
                transform_to_logical(batch)  // ← CPU ✅
            })
        })
        .flatten()
}
```

**Problem**: 
- Sync: Each iteration blocks on I/O
- Async: `.map(async move {...})` creates `Iterator<Future<_>>`, not `Stream<_>`

**Async Pattern**: Convert to `Stream` with async closures and cooperative yielding:

```rust
async fn execute_async() -> Stream<DeltaResult<ScanResult>> {
    let scan_files = self.scan_metadata_async().await?;  // Stream of metadata
    
    scan_files
        .then(|file| async move {  // ← `then` for ASYNC outer closure (has .await)
            let dv = file.dv_info.get_selection_vector_async().await?;
            let batches = engine.read_parquet_files_async(...).await?;  // Returns Stream
            
            batches.then(|batch| async move {  // ← `then` for ASYNC to yield!
                let result = transform_to_logical(batch)?;  // ← CPU work
                yield_now().await;  // ← Yield to executor between batches
                Ok(result)
            })
        })
        .flatten()  // ← Flatten Stream<Stream<ScanResult>> to Stream<ScanResult>
}
```

**Key insight**: Even though `transform_to_logical` is CPU-only, we need `.then()` + `async` on the inner loop to call `yield_now().await` between batches.

**Why yielding matters**:
- Each batch can take 10-100ms of CPU time (expression evaluation, DV filtering)
- Without yielding, this blocks the executor and starves other tasks
- `yield_now().await` gives other tasks a chance to run

**The choice**:
- **Outer**: `.then()` because we need `.await` for I/O (DV read, file read)
- **Inner**: `.then()` because we need `.await` for **cooperative yielding**, even though transform is CPU

**This is NOT Pattern A/B/C** — it's **Pattern D: Nested Stream Processing** (see Pattern Summary above).

---

### 3.6 Functions That Don't Fit Existing Patterns

#### Pattern D: Nested Stream Processing (NEW)

Generic example:

```rust
// PATTERN D: Nested Stream Processing
// Outer loop: fetch items (I/O)
// Inner loop: process chunks from each item (CPU)

async fn process_items(items: Vec<ItemId>) -> Stream<Result<Output>> {
    stream::iter(items)
        .then(|item_id| async move {
            // I/O: fetch item
            let chunks = fetch_item(item_id).await?;  // Returns Stream<Chunk>
            
            // CPU: transform each chunk
            Ok(chunks.then(|chunk| async move {
                let result = transform(chunk)?;  // Pure CPU
                yield_now().await;  // Cooperative yielding
                Ok(result)
            }))
        })
        .flatten()  // Stream<Stream<T>> → Stream<T>
}

// Key characteristics:
// - Nested iteration: items × chunks
// - Stateless (each item/chunk independent)
// - Both loops use `then` for async
// - Uses `flatten()` to unwrap nesting
// - Cooperative yielding prevents executor starvation
```

Application to `Scan::execute`:

| Function | Why It Doesn't Fit | Characteristics | Refactoring Approach |
|----------|-------------------|------------------|----------------------|
| `Scan::execute` | Uses **map** (stateless) not **fold** (stateful). Not one-shot (A), no state accumulation (B), no discovery phase (C) | - Double-nested iterators<br>- I/O per file + per batch<br>- Stateless transforms<br>- No early exit<br>- Each file/batch independent | **Pattern D: Nested Stream Processing**<br>- Convert `Iterator` → `Stream`<br>- Use `.then()` for both loops (async + yielding)<br>- Flatten with `.flatten()` |

---

### 3.7 Metrics Summary

#### Async Choreography (New):
1. `Scan::execute_async` - Convert to Stream with nested `.then()` + yielding (~120 lines)
2. `DvInfo::get_selection_vector_async` - Thin wrapper, just adds `.await` (~8 lines)
3. `DvInfo::get_treemap_async` - Thin wrapper, just adds `.await` (~10 lines)
4. `DeletionVectorDescriptor::read_async` - Async storage call + **SHARED parsing** (~20 lines choreography)

**Async LOC**: ~158 lines (choreography only, +10 for yielding logic)

#### Processor Logic (I/O-Free):
- All 5 pure computation functions (~88 LOC)
- DV parsing logic (~50 LOC)

**Total I/O-Free Logic**: ~138 LOC

**Duplication**: ~60 / (~60 + ~220) ≈ **21% duplication**

---

## Entry Point #4: Table Changes (CDF)

**API**: `TableChanges::try_new(url, engine, start_version, end_version)` → `TableChangesScan::execute(engine)`

**Purpose**: Read change data feed between two table versions (Add/Remove/CDC actions with metadata columns).

---

### 4.1 Call Graph

```
TableChanges::try_new [📍 Entry Point #4a]
├─ LogSegment::for_table_changes [I/O ⚠️]
│  └─ StorageHandler::list_from [I/O 💥]
├─ Snapshot::builder_for().at_version().build() [🔄 Entry Point #1 × 2]
│  └─ ... (see proposal doc - builds start + end snapshots)
└─ Validation (CPU ✅)
   ├─ Check CDF enabled at start/end
   └─ Check schema compatibility

TableChangesScan::execute [📍 Entry Point #4b]
├─ TableChangesScan::scan_metadata [I/O ⚠️]
│  └─ table_changes_action_iter [🆕 creates scanner]
│     └─ LogReplayScanner [🆕 CDF-specific, NOT LogReplayProcessor]
│        ├─ Pass 1: LogReplayScanner::try_new [I/O ⚠️] [🆕]
│        │  ├─ JsonHandler::read_json_files [I/O 💥]
│        │  └─ PreparePassVisitor [CPU ✅] [🆕]
│        │     ├─ Detect CDC actions
│        │     ├─ Build remove_dvs map
│        │     ├─ Validate protocol/metadata
│        │     └─ Check schema compatibility
│        │
│        └─ Pass 2: LogReplayScanner::into_scan_batches [I/O ⚠️] [🆕]
│           ├─ JsonHandler::read_json_files [I/O 💥]
│           ├─ DataSkippingFilter::apply [CPU ✅]
│           ├─ FileActionSelectionVisitor [CPU ✅] [🆕]
│           │  └─ Build selection vectors (CDC vs Add/Remove logic)
│           └─ ExpressionEvaluator::evaluate [CPU ✅]
│
└─ [for each scan file] (Pattern D - nested streams)
   ├─ resolve_scan_file_dv [I/O ⚠️] [🆕 CDF-specific variant]
   ├─ ParquetHandler::read_parquet_files [I/O 💥]
   └─ [for each batch]
      ├─ ExpressionEvaluator::evaluate [CPU ✅]
      └─ split_vector [CPU ✅]
```

---

### 4.2 Key Observations

1. **Two Entry Points**: `try_new` (validation), `execute` (data reading)

2. **Novel Pattern E: Two-Pass Per-Commit**:
   - Pass 1: Read actions, aggregate metadata (has_cdc?, remove_dvs) → `LogReplayScanner`
   - Pass 2: Re-read same commit, apply selection based on Pass 1
   - Cost: Reads each commit twice (streams can't rewind)
   - Does NOT use `LogReplayProcessor` (fundamentally different: forward iteration, two-pass, per-commit scope vs. reverse, single-pass, cross-commit)

4. **File-level**: Uses Pattern D (nested streams) like Scan Execute

5. **vs. Regular Scans**: Ascending order, CDF metadata columns, two-pass per-commit

---

### 4.3 Focus: New Functions Only

**Already analyzed**: Entry Points #1 (Snapshot), #2 (Scan Metadata), #3 (Scan Execute)

**New functions**:
1. ✅ `TableChanges::try_new` - Entry point, validation
2. ✅ `TableChangesScan::execute` - Data reading entry point  
3. ✅ `table_changes_action_iter` - Creates scanners
4. ✅ `LogReplayScanner::try_new` - Pass 1 processor
5. ✅ `LogReplayScanner::into_scan_batches` - Pass 2 processor
6. ✅ `PreparePassVisitor` - Pass 1 visitor (CPU)
7. ✅ `FileActionSelectionVisitor` - Pass 2 visitor (CPU)
8. ✅ `resolve_scan_file_dv` - DV pairing logic
9. ✅ `read_scan_file` - Read and transform CDF data

---

### 4.4 Pattern Categorization

#### Unique Pattern: Two-Pass Per-Commit Processing

`LogReplayScanner` doesn't fit Patterns A, B, C, or D:

**NOT Pattern A** ❌: Not a one-shot helper
**NOT Pattern B** ❌: Not iterative accumulation with try_fold
**NOT Pattern C** ❌: Two passes over same data (no manifest→sidecars discovery)
**NOT Pattern D** ❌: Not nested streams with stateless transforms

**It's a NEW pattern: Pattern E (Two-Pass Per-Item)** (see Pattern Summary above)

**CDF-specific application**:
- Pass 1: `LogReplayScanner::try_new` - reads commit, aggregates metadata (has_cdc?, remove_dvs)
- Pass 2: `into_scan_batches` - rereads commit, applies selection based on Pass 1 metadata
- Uses Pattern B choreography for Pass 1 aggregation

---

### 4.5 Functions That Don't Fit Existing Patterns

#### Pattern E: Two-Pass Per-Item (NEW)

Generic example:

```rust
// PATTERN E: Two-Pass Per-Item
// Pass 1: Pattern B processor over item (may exit early)
// Pass 2: re-read same item, process using Pass 1 result

struct ItemScanner {
    item_id: ItemId,
    control_state: ControlState,  // From Pass 1's final output
}

impl ItemScanner {
    // Pass 1: use Pattern B (processor + try_fold)
    async fn prepare(item_id: ItemId) -> Result<Self> {
        let chunks = read_item(item_id).await?;  // Returns Stream<Chunk>
        
        // Pattern B choreography (exactly as in proposal)
        let control_state = chunks
            .try_fold(Processor::new(), |proc, chunk| async move {
                proc.process_chunk(chunk).transpose()  // Result<CF> → CF<Result>
            })
            .await
            .unwrap_break_or_else(|proc| proc.finalize())?;  // Handle Break or Continue
        
        Ok(ItemScanner { item_id, control_state })
    }
    
    // Pass 2: re-read item, process using control state from Pass 1
    async fn process(self) -> Result<Stream<Output>> {
        let chunks = read_item(self.item_id).await?;  // Re-read SAME item
        
        Ok(chunks.then(move |chunk| async move {
            let result = self.control_state.process(chunk?)?;
            yield_now().await;
            Ok(result)
        }))
    }
}

// Usage: per-item two-pass
async fn scan_items(items: Vec<ItemId>) -> Stream<Result<Output>> {
    stream::iter(items)
        .then(|id| async move {
            let scanner = ItemScanner::prepare(id).await?;  // Pass 1 (Pattern B)
            scanner.process().await                          // Pass 2
        })
        .flatten()
}

// Key characteristics:
// - Pass 1 IS Pattern B: processor + try_fold + transpose + unwrap_break_or_else
// - Pass 2 uses Pass 1 result to process same item differently
// - Both passes read same data (can't rewind stream)
// - Unlike Pattern C: processes SAME data twice, not manifest→sidecars
```

Application to CDF `LogReplayScanner`:

| Function | Why It Doesn't Fit | Characteristics | Refactoring Approach |
|----------|--------------------|-----------------|----------------------|
| `LogReplayScanner::try_new` | Two-pass per-commit, not per-table. Pass 1 of a per-item two-pass pattern. | - Reads entire commit<br>- Extracts metadata (has_cdc, remove_dvs)<br>- Validates protocol/schema<br>- Returns scanner for Pass 2 | **Pattern E: Two-Pass Per-Item**<br>- Pass 1 async: just add `.await` to json_handler<br>- Scanner state is I/O-free<br>- Pass 2 async: separate `into_scan_batches_async` |
| `LogReplayScanner::into_scan_batches` | Pass 2 of Pattern E | - Re-reads same commit<br>- Applies selection based on Pass 1<br>- Transforms to scan metadata | Same as Pass 1 (add `.await`) |

**Async refactoring for CDF**:
- Pass 1: `async fn try_new_async(...).await?` - Pattern B choreography with `.await`
- Pass 2: `async fn into_scan_batches_async(...).await?` - returns Stream
- Scanner struct remains I/O-free (just holds control state from Pass 1)

---

### 4.6 Metrics Summary

#### Async Choreography (New):
1. `TableChanges::try_new_async` - Thin wrapper with `.await` for snapshots + list (~15 lines)
2. `table_changes_action_iter_async` - Returns Stream instead of Iterator (~10 lines)
3. `LogReplayScanner::try_new_async` - Add `.await` to json_handler (~10 lines)
4. `LogReplayScanner::into_scan_batches_async` - Add `.await`, return Stream (~15 lines)
5. `TableChangesScan::scan_metadata_async` - Stream wrapper (~10 lines)
6. `TableChangesScan::execute_async` - Pattern D choreography (similar to Scan Execute) (~120 lines)

**Async LOC**: ~180 lines

#### Processor Logic (I/O-Free):
- `LogReplayScanner` state (~30 LOC) - I/O-free struct
- `PreparePhaseVisitor` (~80 LOC) - Pure CPU visitor
- `FileActionSelectionVisitor` (~50 LOC) - Pure CPU visitor  
- `resolve_scan_file_dv` (~60 LOC) - Logic shared with regular scans
- `read_scan_file` (~80 LOC) - Transform logic (CPU)
- All CDF-specific validation/schema logic (~100 LOC)

**Total Shared Logic**: ~400 LOC

**Duplication**: ~140 / (~140 + ~500) ≈ **22% duplication**

**Why reasonable**:
- Pattern E is straightforward (just add `.await` in two places for two-pass)
- Most complexity is in visitors and validation (I/O-free)
- Execute pass reuses Pattern D from Scan Execute


---

## Entry Point #5: Transaction Commit

**API**: `Transaction::commit(engine) -> Result<CommitResult>`

**Purpose**: Write a new commit file to the Delta log with Add actions and metadata.

---

### 5.1 Call Graph

```
Transaction::commit [📍 Entry Point]
├─ Validation (CPU ✅)
│  └─ Check duplicate app_ids
├─ Generate actions (CPU ✅)
│  ├─ into_engine_data (SetTransaction actions)
│  ├─ Snapshot::get_in_commit_timestamp [I/O ⚠️] [🆕]
│  │  ├─ ICT validation (CPU ✅)
│  │  └─ ParsedLogPath::read_in_commit_timestamp [I/O ⚠️] [🆕]
│  │     ├─ JsonHandler::read_json_files [I/O 💥]
│  │     └─ CommitInfo visitor (CPU ✅)
│  ├─ CommitInfo::new + into_engine_data
│  ├─ generate_adds (CPU ✅)
│  │  └─ build_add_actions (CPU ✅)
│  └─ generate_domain_metadata_actions (CPU ✅)
├─ JsonHandler::write_json_file [I/O 💥]
│  └─ Write single JSON commit file (atomic)
└─ Result handling (CPU ✅)
   ├─ Ok → CommittedTransaction
   ├─ FileAlreadyExists → ConflictedTransaction
   └─ IOError → RetryableTransaction
```

---

### 5.2 Key Observations

1. **Mostly CPU**: Action generation is pure computation, no iterators/streams

2. **Minimal I/O**: Read ICT (if enabled), write commit file

3. **Pattern A Only**: One-shot operation (prepare → write), no B/C/D/E patterns needed

---

### 5.3 Focus: New Functions Only

**Already analyzed**: Engine traits (will have async variants from snapshot refactor)

**New functions**:
1. ✅ `Transaction::commit` - Entry point
2. ✅ `Snapshot::get_in_commit_timestamp` - Read ICT from last commit (Pattern A)
3. ✅ `ParsedLogPath::read_in_commit_timestamp` - Read single commit file (Pattern A)
4. ✅ `Transaction::generate_adds` - CPU-only action generation
5. ✅ `Transaction::generate_domain_metadata_actions` - CPU-only
6. ✅ `build_add_actions` - CPU-only transformation
7. ✅ `CommitInfo::new` / `into_engine_data` - CPU-only

---

### 5.4 Pattern Categorization

#### Pattern A: Helper Functions ✅ (Perfect Fit)

All transaction commit functions fit Pattern A (one-shot operations):

1. **`Transaction::commit`**: Generate actions (CPU) → write commit file (I/O)
2. **`get_in_commit_timestamp`** / **`read_in_commit_timestamp`**: Read commit file (I/O) → parse CommitInfo (CPU)

Async variants just add `.await` to I/O calls. All action generation and ICT parsing logic is I/O-free.

---

### 5.5 Functions That Don't Fit Existing Patterns

**None!** All functions fit Pattern A perfectly.

Transaction commit is the simplest entry point analyzed so far:
- No complex iteration
- No state accumulation
- No nested streams
- Just: prepare actions (CPU) → write file (I/O)

---

### 5.6 Metrics Summary

#### Async Choreography (New):
1. `Transaction::commit_async` - Add `.await` to I/O calls (~80 lines, mostly unchanged)
2. `Snapshot::get_in_commit_timestamp_async` - Add `.await` to read (~15 lines)
3. `ParsedLogPath::read_in_commit_timestamp_async` - Add `.await` to json_handler (~20 lines)

**Async LOC**: ~115 lines (3 thin Pattern A wrappers)

#### Processor Logic (I/O-Free):
- `Transaction::generate_adds` (~110 LOC) - Pure CPU
- `Transaction::generate_domain_metadata_actions` (~40 LOC) - Pure CPU
- `build_add_actions` (~50 LOC) - Pure CPU transformation
- `Snapshot::get_in_commit_timestamp` logic (~20 LOC) - Validation (shared)
- `ParsedLogPath::read_in_commit_timestamp` parsing (~15 LOC) - CommitInfo visitor (shared)
- All validation and result handling (~30 LOC) - Pure CPU
- All action construction (`CommitInfo`, `DomainMetadata`, etc.) (~100 LOC) - Pure CPU

**Total Shared Logic**: ~365 LOC

**Duplication**: ~25 / (~25 + ~85) ≈ **23% duplication**

**Why reasonable**:
- Pattern A applies perfectly (one-shot operation)
- Vast majority of code is action generation (I/O-free)
- Only I/O is ICT read + file write

---

## Entry Point #6: Checkpoint Writing

**API**: `Snapshot::checkpoint() -> CheckpointWriter` + `CheckpointWriter::checkpoint_data(engine)` + `CheckpointWriter::finalize(engine, metadata, data)`

**Purpose**: Write checkpoint file containing reconciled table state at a specific version.

---

### 6.1 Call Graph

```
Snapshot::checkpoint [📍 Entry Point #6a]
├─ CheckpointWriter::try_new [🆕]
│  ├─ Version conversion (CPU ✅)
│  └─ LogSegment::validate_no_staged_commits [CPU ✅]

CheckpointWriter::checkpoint_path [📍 Entry Point #6b]
└─ ParsedLogPath::new_classic_parquet_checkpoint [CPU ✅]

CheckpointWriter::checkpoint_data [📍 Entry Point #6c]
├─ TableConfiguration::is_v2_checkpoint_write_supported [CPU ✅]
├─ LogSegment::read_actions [🔄 Pattern C - Entry Point #1]
│  └─ ... (see Snapshot Building proposal)
├─ ActionReconciliationProcessor::new [🆕 CPU ✅]
├─ ActionReconciliationProcessor::process_actions_iter [🆕 Pattern B CPU ✅]
│  └─ ActionReconciliationProcessor::process_actions_batch [🆕 CPU ✅]
│     └─ ActionReconciliationVisitor [🆕 CPU ✅]
│        ├─ check_file_action [CPU ✅]
│        ├─ check_protocol_action [CPU ✅]
│        ├─ check_metadata_action [CPU ✅]
│        └─ check_txn_action [CPU ✅]
└─ CheckpointWriter::create_checkpoint_metadata_batch [🆕 CPU ✅]

CheckpointWriter::finalize [📍 Entry Point #6d]
├─ Iterator exhaustion validation (CPU ✅)
├─ create_last_checkpoint_data [🆕 CPU ✅]
│  └─ Build LastCheckpointHint JSON (CPU ✅)
├─ LastCheckpointHint::path [CPU ✅]
└─ JsonHandler::write_json_file [I/O 💥]
```

**Summary**:
```
🆕 CheckpointWriter::try_new            Pattern A wrapper (~20 lines)
🆕 CheckpointWriter::checkpoint_path    Pattern A wrapper (~5 lines)
🆕 CheckpointWriter::checkpoint_data    Pattern B wrapper (~30 lines)
🆕 CheckpointWriter::finalize           Pattern A wrapper (~40 lines)
🆕 ActionReconciliationProcessor        Pattern B processor (~150 lines CPU ✅)
🆕 ActionReconciliationVisitor          Pattern B visitor (~290 lines CPU ✅)
🆕 create_last_checkpoint_data          Pattern A helper (~30 lines CPU ✅)
                                        (~565 lines: ~95 async + ~470 shared)

Duplication: ~17% (95 async lines vs 470 shared lines)
```

---

### 6.2 Key Observations

1. **Pattern B Confirmed**: Uses `ActionReconciliationProcessor` implementing `LogReplayProcessor` trait (predicted in §2.4)

2. **Critical Dependency on Pattern C**: `checkpoint_data` calls `log_segment.read_actions()` which has nested I/O (§2.5). Checkpoint writing has the SAME dependency on Pattern C refactoring as scans.

3. **Multi-phase API**: 
   - Phase 1: Create writer (`checkpoint()`)
   - Phase 2: Get data iterator (`checkpoint_data()`) - **calls monolithic `read_actions`**
   - Phase 3: Engine writes checkpoint file (user-provided)
   - Phase 4: Finalize (`finalize()`)

4. **Shared with Log Compaction**: `ActionReconciliationProcessor` is used by both checkpoint writing (#6) and log compaction (#7)

5. **Minimal I/O (misleading)**: While checkpoint writer itself only calls `write_json_file`, `checkpoint_data` internally calls `read_actions` which has nested I/O complexity

---

### 6.3 Focus: New Functions Only

**Already analyzed**: Entry Point #1 (`LogSegment::read_actions`), `LogReplayProcessor` trait

**New functions**:
1. ✅ `CheckpointWriter::try_new` - Constructor, validation
2. ✅ `CheckpointWriter::checkpoint_path` - Path generation
3. ✅ `CheckpointWriter::checkpoint_data` - Creates iterator
4. ✅ `CheckpointWriter::finalize` - Writes `_last_checkpoint`
5. ✅ `ActionReconciliationProcessor` - Pattern B processor
6. ✅ `ActionReconciliationVisitor` - Row visitor for filtering
7. ✅ `create_last_checkpoint_data` - Helper for LastCheckpointHint

---

### 6.4 Function Classification

**I/O Functions (need async)**: 4 thin wrappers (~95 lines total)
- `CheckpointWriter::try_new`, `checkpoint_path`, `checkpoint_data`, `finalize`

**CPU Functions (shared)**: 3 functions (~470 lines total)
- `ActionReconciliationProcessor` (Pattern B processor)
- `ActionReconciliationVisitor` (action filtering logic)
- `create_last_checkpoint_data` (helper)

---

### 6.5 Pattern Categorization

#### Pattern B: Processor + try_fold ✅

**Perfect Match**: `ActionReconciliationProcessor` implements `LogReplayProcessor` trait (identical to `ScanLogReplayProcessor` from Entry Point #2).

Processes log in reverse chronological order, deduplicates actions, applies retention policies.

#### Pattern A: Helper Functions ✅

**Applies to**: All checkpoint writer methods

- `try_new`: Validate + construct
- `checkpoint_path`: Generate path
- `checkpoint_data`: Create iterator (calls Pattern B)
- `finalize`: Write `_last_checkpoint` file

All are one-shot operations. Async variants just add `.await` to I/O calls.

---

### 6.6 Pattern C Dependency

**Critical**: Checkpoint writing depends on Pattern C refactoring of `read_actions` (§2.5).

`checkpoint_data` currently calls monolithic `read_actions()`:
```rust
let actions = self.snapshot.log_segment().read_actions(
    engine,
    CHECKPOINT_ACTIONS_SCHEMA.clone(),
    CHECKPOINT_ACTIONS_SCHEMA.clone(),
    None,
)?;
```

This has the nested I/O problem identified in §2.5. After Pattern C refactoring for snapshots/scans, checkpoint writing will need similar updates:

**Pattern C Migration** (after snapshot/scan refactor):
```rust
// Phase 1: Read manifest + collect sidecar refs
let phase1_result = self.snapshot.log_segment()
    .read_actions_phase1(engine, schema)?;

// Phase 2: Process manifest + fetch sidecars
let actions = phase1_result.process_sidecars(engine)?;

// Then: Pattern B processing (unchanged)
let checkpoint_data = ActionReconciliationProcessor::new(...)
    .process_actions_iter(actions);
```

**Cost**: ~25 lines of checkpoint-specific wrappers (same marginal cost as scans, §2.6)

---

### 6.7 Metrics Summary

#### Already Provided by Snapshot/Scan Refactor (Free):
- ✓ `LogSegment::read_actions_phase1_async` (~20 lines) [shared]
- ✓ `Phase1InProgress<P>` wrapper [shared]
- ✓ `Phase1Result<P>` enum [shared]
- ✓ `Phase1Result::process_sidecars_async` (~20 lines) [shared]
- ✓ Pattern C infrastructure established

#### Checkpoint-Specific Additions (Marginal Cost):
1. `CheckpointWriter::try_new_async` - Add `.await` (~20 lines)
2. `CheckpointWriter::checkpoint_path_async` - No changes needed (already sync) (~5 lines)
3. `CheckpointWriter::checkpoint_data_async` - Pattern C choreography (~30 lines)
4. `CheckpointWriter::finalize_async` - Add `.await` to write (~40 lines)

**Async LOC**: ~95 lines (checkpoint-specific wrappers)

#### Processor Logic (I/O-Free):
- `ActionReconciliationProcessor` (~150 LOC) - Pattern B
- `ActionReconciliationVisitor` (~290 LOC) - Row visitor
- `create_last_checkpoint_data` (~30 LOC) - Helper

**Total I/O-Free Logic**: ~470 LOC

**Duplication**: ~95 / (~95 + ~470) ≈ **17% duplication**

**Why reasonable**:
- Pattern B applies perfectly (uses `LogReplayProcessor` trait)
- Pattern C infrastructure shared with snapshots/scans (marginal cost)
- Vast majority of code is action reconciliation logic (I/O-free)
- Very low duplication (17%)

**Note**: Metrics assume Pattern C refactoring is done. Without it, checkpoint writing cannot be made async (same nested I/O issue as §2.5).

---

## Entry Point #7: Log Compaction

**API**: `Snapshot::log_compaction_writer(start, end) -> LogCompactionWriter`, then `LogCompactionWriter::compaction_data(engine) -> LogCompactionDataIterator`

**Purpose**: Aggregate commit files in a version range into a single compacted file (similar to checkpoints but for a subset of versions).

---

### 7.1 Call Graph

```
Snapshot::log_compaction_writer [📍 Entry Point #7a]
├─ LogCompactionWriter::try_new [🆕]
│  ├─ Version validation (CPU ✅)
│  ├─ LogSegment::validate_no_staged_commits [CPU ✅]
│  └─ ParsedLogPath::new_log_compaction [CPU ✅]

LogCompactionWriter::compaction_path [📍 Entry Point #7b]
└─ Returns cached Url [CPU ✅]

LogCompactionWriter::compaction_data [📍 Entry Point #7c]
├─ Version range validation [CPU ✅]
├─ LogSegment::for_table_changes [I/O 💥]
├─ LogSegment::read_actions [🔄 Pattern C - Entry Point #1]
│  └─ ... (see Snapshot Building proposal)
├─ ActionReconciliationProcessor::new [CPU ✅]
└─ ActionReconciliationProcessor::process_actions_iter [Pattern B CPU ✅]
   └─ ... (see Entry Point #6)
```

**Functions Summary**:
```
🆕 LogCompactionWriter::try_new            Pattern A wrapper (~25 lines)
🆕 LogCompactionWriter::compaction_path    Pattern A accessor (~5 lines)
🆕 LogCompactionWriter::compaction_data    Pattern B wrapper (~40 lines)
🆕 LogCompactionDataIterator               Pattern B iterator wrapper (~60 lines)
🆕 should_compact                          Pattern A helper (~5 lines)
                                           (~135 lines: ~70 async + ~65 shared)

Duplication: ~70 / (~70 + ~65 + ~470) ≈ 12% (shares ActionReconciliationProcessor with Entry Point #6)
```

---

### 7.2 Key Observations

1. **Nearly Identical to Checkpoint Writing**: Uses same `ActionReconciliationProcessor` and Pattern B choreography

2. **Critical Dependency on Pattern C**: `compaction_data` calls `log_segment.read_actions()` which has nested I/O (§2.5). Same dependency as checkpoint writing (§6.2).

3. **Multi-phase API**: 
   - Phase 1: Create writer (`log_compaction_writer`)
   - Phase 2: Get data iterator (`compaction_data`) - **calls monolithic `read_actions`**
   - Phase 3: Engine writes compaction file (user-provided)

4. **Shares Processor**: Both checkpoint writing (#6) and log compaction (#7) use `ActionReconciliationProcessor`

5. **Version Range Filtering**: Creates filtered `LogSegment` for specific version range (CPU operation)

---

### 7.3 Focus: New Functions Only

All significant business logic is shared with Entry Point #6 (checkpoint writing):
- ✓ `ActionReconciliationProcessor` [shared with #6]
- ✓ `ActionReconciliationVisitor` [shared with #6]

**New wrapper-specific code** (~135 lines):
1. `LogCompactionWriter::try_new` - Validate version range, compute path (~25 lines)
2. `LogCompactionWriter::compaction_path` - Return cached path (~5 lines)
3. `LogCompactionWriter::compaction_data` - Create filtered log segment, call `read_actions`, wire up processor (~40 lines)
4. `LogCompactionDataIterator` - Iterator wrapper with counters (~60 lines)
5. `should_compact` - Utility to determine if compaction is needed (~5 lines)

---

### 7.4 Pattern Categorization

**Pattern A** (one-shot I/O wrappers):
- `LogCompactionWriter::try_new` - Create writer with validation
- `LogCompactionWriter::compaction_path` - Return cached path
- `should_compact` - Pure CPU helper

**Pattern B** (iterative processing):
- `LogCompactionWriter::compaction_data` - Calls `read_actions` then wires up `ActionReconciliationProcessor` (same as checkpoint writing)

All are thin wrappers over shared Pattern B processor. Async variants add `.await` to I/O calls.

---

### 7.5 Pattern C Dependency

**Critical**: Log compaction depends on Pattern C refactoring of `read_actions` (§2.5), identical to checkpoint writing (§6.6).

`compaction_data` currently calls monolithic `read_actions()`:
```rust
let actions_iter = compaction_log_segment.read_actions(
    engine,
    COMPACTION_ACTIONS_SCHEMA.clone(),
    COMPACTION_ACTIONS_SCHEMA.clone(),
    None,
)?;
```

After Pattern C refactoring for snapshots/scans, log compaction will need similar updates (same as checkpoint writing).

**Cost**: ~25 lines of compaction-specific wrappers (same marginal cost as scans §2.6 and checkpoints §6.6)

---

### 7.6 Metrics Summary

#### Already Provided by Snapshot/Scan/Checkpoint Refactor (Free):
- ✓ `LogSegment::read_actions_phase1_async` [shared]
- ✓ Pattern C infrastructure [shared]
- ✓ `ActionReconciliationProcessor` (~150 LOC) [shared with #6]
- ✓ `ActionReconciliationVisitor` (~290 LOC) [shared with #6]

#### Compaction-Specific Additions (Marginal Cost):
1. `LogCompactionWriter::try_new_async` - Add `.await` (~25 lines)
2. `LogCompactionWriter::compaction_path_async` - Already sync (~5 lines)
3. `LogCompactionWriter::compaction_data_async` - Pattern C choreography (~40 lines)
4. `LogCompactionDataIterator` - Iterator wrapper (no async changes) (~60 lines)
5. `should_compact` - Pure CPU helper (no async changes) (~5 lines)

**Async LOC**: ~70 lines (compaction-specific wrappers)

#### Shared I/O-Free Logic (with Checkpoint Writing):
- `ActionReconciliationProcessor` (~150 LOC) [shared with #6]
- `ActionReconciliationVisitor` (~290 LOC) [shared with #6]
- Compaction-specific helpers (~65 LOC)

**Total I/O-Free Logic**: ~505 LOC

**Duplication**: ~70 / (~70 + ~505) ≈ **12% duplication**

**Why reasonable**:
- Pattern B applies perfectly (reuses `LogReplayProcessor` trait)
- Pattern C infrastructure shared with snapshots/scans/checkpoints (marginal cost)
- Shares all reconciliation logic with checkpoint writing
- Very low duplication (12%)

**Note**: Metrics assume Pattern C refactoring is done. Without it, log compaction cannot be made async (same nested I/O issue as §2.5 and §6.6).

---

