# Async/Sync Pattern Generalizability Analysis

## Overview

This document analyzes the generalizability of the async/sync patterns proposed in `async-build-snapshot-proposal.md` by examining other major kernel entry points. The original proposal focused on snapshot building (Control Flow 1), and we now examine whether the patterns (A, B, and C) apply to other operations.

## Terminology

**Important distinction**:
- **"Phase"**: Pattern C has two **phases** where Phase 1 reads a manifest to discover what to fetch in Phase 2 (e.g., checkpoint→sidecars). Can't start Phase 2 until Phase 1 completes discovery.
- **"Pass"**: Some operations make multiple **passes** over the same already-fetched data (e.g., CDF reads each commit twice to discover metadata then apply selection).

---

## Pattern Summary

From the proposal and this analysis, we've identified **5 patterns** for async/sync code sharing:

| Pattern | Description | When to Use | Key Tools | Details |
|---------|-------------|-------------|-----------|---------|
| **A: Helper Functions** | One-shot I/O operations | Read file → parse → done | `async fn` + `.await` | Proposal §4 |
| **B: Processor + try_fold** | Stateful iteration with accumulation | Process batches, track state, may exit early | `try_fold`, `ControlFlow`, `.transpose()` | Proposal §5 |
| **C: Two-Phase Processing** | Discovery then fetch | Can't know what to fetch until Phase 1 completes | `Phase1Result`, `process_sidecars` | Proposal §6 |
| **D: Nested Stream Processing** 🆕 | Nested iteration with I/O in outer loop | Multiple levels of iteration, stateless | `Stream::then()`, `flatten()`, `yield_now()` | §6.8 below |
| **E: Two-Pass Per-Item** 🆕 | Aggregate metadata, then reprocess same data | Can't rewind stream, need aggregated state | Pattern B (pass 1) + `then()` (pass 2) | §7.5 below |

**Key**: Patterns A-C from proposal, D-E discovered in this analysis.

**Pattern relationships**:
- Pattern C uses Pattern B for Phase 2 processing
- Pattern E uses Pattern B for Pass 1 aggregation
- Pattern D is stateless (no Pattern B)

---

## Methodology

For each entry point, we:
1. Trace the current call graph (similar to Section 8.1 of the proposal)
2. Identify functions involved, distinguishing previously-analyzed ones from new ones
3. Categorize new functions according to Patterns A, B, or C
4. Describe functions that don't fit any pattern and why

---

## Step 1: Identify Main Kernel Entry Points

Based on the public API, delta-kernel-rs has **7 major entry points**:

| # | Entry Point | Purpose | Status |
|---|-------------|---------|--------|
| 1 | **Snapshot Building** | Create snapshot at version | ✅ Analyzed in proposal |
| 2 | **Scan Metadata** | Get file list for scan | ✅ Analyzed (Step 2-5) |
| 3 | **Scan Execute** | Read actual parquet data | ✅ Analyzed (Entry Point #3) |
| 4 | **Transaction Commit** | Write new commits | ❌ Not analyzed |
| 5 | **Table Changes (CDF)** | Read change data feed | ❌ Not analyzed |
| 6 | **Checkpoint Writing** | Write checkpoint file | ❌ Not analyzed |
| 7 | **Log Compaction** | Compact commit range | ❌ Not analyzed |

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

**Duplication**: ~108 / (~108 + ~138) ≈ **44% duplication**

---

## Entry Point #4: Table Changes (CDF)

**API**: `TableChanges::try_new(url, engine, start_version, end_version)` → `TableChangesScan::execute(engine)`

**Purpose**: Read change data feed between two table versions (Add/Remove/CDC actions with metadata columns).

---

### 4.1 Call Graph

```
TableChanges::try_new [📍 Entry Point #5a]
├─ LogSegment::for_table_changes [I/O ⚠️]
│  └─ StorageHandler::list_from [I/O 💥]
├─ Snapshot::builder_for().at_version().build() [🔄 Entry Point #1 × 2]
│  └─ ... (see proposal doc - builds start + end snapshots)
└─ Validation (CPU ✅)
   ├─ Check CDF enabled at start/end
   └─ Check schema compatibility

TableChangesScan::execute [📍 Entry Point #5b]
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

**Duplication**: ~180 / (~180 + ~400) ≈ **31% duplication**

**Why reasonable**:
- Pattern E is novel but straightforward (just add `.await` in two places)
- Most complexity is in visitors and validation (I/O-free)
- Execute pass reuses Pattern D from Scan Execute
- Lower than Scan Execute due to less DV parsing complexity


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

**Duplication**: ~115 / (~115 + ~365) ≈ **24% duplication**

**Why reasonable**:
- Pattern A applies perfectly (one-shot operation)
- Vast majority of code is action generation (I/O-free)
- ICT read functions are simple Pattern A wrappers (~35 LOC async)
- Only I/O is ICT read + file write (trivial async)
- Very low duplication (24%)

---


