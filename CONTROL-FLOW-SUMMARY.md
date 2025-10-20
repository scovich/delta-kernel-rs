# Control Flow Analysis - Executive Summary

**Date**: October 2025  
**Purpose**: Detailed control flow analysis for async API refactoring  
**Status**: ✅ Complete

---

## Key Findings

### 1. There Are 5 Major Control Flows

All public kernel entry points map to these flows:

1. **Snapshot Creation** - `Snapshot::builder_for().build()`
2. **Scan Metadata** - `Scan::scan_metadata()`
3. **Scan Execution** - `Scan::execute()`
4. **Transaction Commit** - `Transaction::commit()`
5. **Checkpoint Writing** - `Snapshot::checkpoint()`

### 2. They All Share One Critical Building Block

**`LogSegment::read_actions`** is used by ALL 5 control flows!

This single method:
- Reads commit JSON files
- Reads checkpoint Parquet files  
- Returns Iterator (hides async parallelism)
- Is the **highest leverage refactoring target**

**Impact**: Refactoring this one method improves all 5 control flows!

### 3. The Code Is Already ~60% I/O-Free

**Already I/O-free** (no changes needed):
- LogSegment validation
- Schema operations
- Expression transforms
- Table configuration
- All parsing/processing logic

**Needs refactoring** (~40%):
- LogSegment::read_actions
- Snapshot listing
- Scan file listing
- Transaction ICT reading (uses LogSegment)

### 4. The Solution Is Clear

**Three-phase separation** for I/O-bound operations:

```
Phase 1: List files (I/O-free, returns Vec<FileMeta>)
Phase 2: Fetch data (user controls: sync/async/parallel)
Phase 3: Parse data (I/O-free, takes IntoIterator)
```

**Key insight**: Phase 3 takes `IntoIterator`, NOT `Iterator`
- Works with Vec (from Stream)
- Works with Iterator (from sync)
- Doesn't force choice on caller

---

## Concrete Example: LogSegment::read_actions

### Current (Iterator Hides Async)

```rust
// Current API
fn read_actions(&self, engine: &dyn Engine) 
    -> DeltaResult<Iterator<ActionsBatch>>

// Usage - simple but inflexible
let actions = log_segment.read_actions(engine)?;
for action in actions {
    // Sequential processing, can't parallelize fetch
}
```

**Problem**: Engine does async fetch internally, converts to sync Iterator. User can't control parallelism.

### Proposed (Separate List/Fetch/Parse)

```rust
// NEW: Phase 1 - List files (I/O-free)
fn action_files(&self) -> ActionFiles {
    ActionFiles {
        commit_files: self.ascending_commit_files.clone(),
        checkpoint_files: self.checkpoint_parts.clone(),
    }
}

// NEW: Phase 3 - Parse (I/O-free, takes IntoIterator!)
fn parse_actions(
    &self,
    commit_data: impl IntoIterator<Item = DeltaResult<Box<dyn EngineData>>>,
    checkpoint_data: impl IntoIterator<Item = DeltaResult<Box<dyn EngineData>>>,
) -> DeltaResult<Iterator<ActionsBatch>>

// KEEP: Convenience wrapper
fn read_actions(&self, engine: &dyn Engine) 
    -> DeltaResult<Iterator<ActionsBatch>> {
    let files = self.action_files();
    let commit_iter = engine.json_handler().read_json_files(&files.commit_files)?;
    let checkpoint_iter = engine.parquet_handler().read_parquet_files(&files.checkpoint_files)?;
    self.parse_actions(commit_iter, checkpoint_iter)
}
```

### Usage Patterns

**Simple (unchanged)**:
```rust
// Old API still works
let actions = log_segment.read_actions(engine)?;
```

**Parallel fetch (new capability)**:
```rust
let files = log_segment.action_files();

// Fetch in parallel
let commit_data: Vec<_> = files.commit_files
    .par_iter()
    .map(|f| engine.read_file(f))
    .collect()?;

let checkpoint_data: Vec<_> = files.checkpoint_files
    .par_iter()
    .map(|f| engine.read_file(f))
    .collect()?;

// Parse sequentially (maintains order)
let actions = log_segment.parse_actions(
    commit_data.into_iter(),
    checkpoint_data.into_iter()
)?;
```

**Async (new capability)**:
```rust
let files = log_segment.action_files();

// Async fetch
let commit_stream = async_engine.read_json_files_stream(&files.commit_files).await?;
let checkpoint_stream = async_engine.read_parquet_files_stream(&files.checkpoint_files).await?;

// Collect (Stream → Vec → IntoIterator)
let commit_data: Vec<_> = commit_stream.try_collect().await?;
let checkpoint_data: Vec<_> = checkpoint_stream.try_collect().await?;

// Parse (same as sync!)
let actions = log_segment.parse_actions(
    commit_data.into_iter(),
    checkpoint_data.into_iter()
)?;
```

**Why IntoIterator instead of Iterator?**  
Because `Stream.try_collect().await?` returns `Vec`, not `Iterator`.  
`IntoIterator` accepts both `Vec` and `Iterator`, so it works for sync AND async!

---

## Recommended Implementation Plan

### Phase 1: LogSegment (2 weeks) - START HERE

**Why first**: Affects all 5 control flows, highest impact

**Deliverables**:
- `LogSegment::action_files()` 
- `LogSegment::parse_actions(IntoIterator)`
- Keep `read_actions()` as wrapper
- Tests + example

**Breaking**: No (additive only)

### Phase 2: Snapshot Builder (1.5 weeks)

**Deliverables**:
- `SnapshotBuilder::list_log_files()`
- `SnapshotBuilder::build_from_files()`
- Keep `build()` as wrapper

### Phase 3: Scan (2-3 weeks)

**Deliverables**:
- `Scan::files_to_scan()`
- `Scan::execute_with_data()`
- Keep `execute()` as wrapper

### Phase 4: Transaction (1-2 weeks)

**Deliverables**:
- `Transaction::prepare()`
- `Transaction::write_commit()`
- Keep `commit()` as wrapper

### Phase 5: Documentation (2 weeks)

**Deliverables**:
- Migration guide
- Performance tuning guide
- Examples (parallel, async)

**Total**: 9-10 weeks for complete I/O-free refactoring  
**Minimum viable**: 2 weeks (Phase 1 only) with immediate impact

---

## Effort vs Impact Analysis

| Phase | Effort | Impact | Breaking | Priority |
|-------|--------|--------|----------|----------|
| LogSegment | 2 weeks | 🔥 ALL 5 flows | No | 1 |
| Snapshot | 1.5 weeks | 1 flow | No | 2 |
| Scan | 2-3 weeks | 2 flows | No | 3 |
| Transaction | 1-2 weeks | 1 flow | No | 4 |
| Docs | 2 weeks | Adoption | No | 5 |

**Recommendation**: Start with Phase 1, evaluate results, then proceed.

---

## Glue Code Comparison

### Current Approach

**Glue Location**: Inside engine (hidden from user)  
**Glue Amount**: ~40 lines × 3 handlers = ~120 lines  
**User Control**: None  
**Duplication**: High (same pattern 3+ times)

```rust
// Inside engine/default/json.rs (user can't see or control this)
fn read_json_files(...) -> Iterator<EngineData> {
    let (sender, receiver) = mpsc::sync_channel(readahead);
    
    task_executor.spawn(async move {
        let stream = futures::stream::iter(files)
            .buffered(concurrency)
            .try_flatten();
        while let Some(batch) = stream.next().await {
            sender.send(batch)?;
        }
    });
    
    Box::new(receiver.into_iter())
}
```

### Proposed Approach

**Glue Location**: User code (when needed)  
**Glue Amount**: ~5-10 lines (optional!)  
**User Control**: Full  
**Duplication**: Zero (write only when needed)

```rust
// Simple case: ZERO glue (use convenience method)
let actions = log_segment.read_actions(engine)?;

// Advanced case: ~5 lines of glue (full control)
let files = log_segment.action_files();
let data: Vec<_> = files.commit_files.par_iter()
    .map(|f| engine.read_file(f))
    .collect()?;
let actions = log_segment.parse_actions(data.into_iter(), ...)?;
```

**Net effect**: Less total glue, and it's optional!

---

## Answers to Original Questions

### "What would it actually mean to make log segment action parsing I/O free?"

**Answer**: Three distinct phases:
1. **List**: Return file paths (I/O-free)
2. **Fetch**: Caller controls (their choice: sync/async/parallel)
3. **Parse**: Process pre-fetched data (I/O-free, takes IntoIterator)

Key: Parse takes `IntoIterator` (not `Iterator`) so it works whether data came from Iterator or Stream.

### "How many major control flows?"

**Answer**: 5 flows, sharing 1 critical building block (`LogSegment::read_actions`)

### "Where does I/O lurk in the call stack?"

**Answer**: 
- Listing: `StorageHandler::list_from()`
- Reading files: `JsonHandler::read_json_files()`, `ParquetHandler::read_parquet_files()`
- Writing: `JsonHandler::write_json_file()`

All return Iterator (hides async). Refactoring exposes file lists, lets user control fetch.

### "How much is I/O-free vs needs refactoring?"

**Answer**: 
- ~60% already I/O-free (computation)
- ~40% needs refactoring (I/O coordination)

### "How to minimize glue code?"

**Answer**: 
- Keep convenience methods (zero glue for simple case)
- Advanced users write ~5-10 lines (only when needed)
- Net result: LESS glue than current (no hidden duplication in engine)

### "Which entry points are priority?"

**Answer**:
1. `#[internal_api]` LogSegment methods (used by all flows)
2. Public Snapshot/Scan methods (most common)
3. Public Transaction methods
4. Public checkpoint methods

But LogSegment refactoring helps ALL of them!

---

## Risk Assessment

### Low Risk ✅

- Additive changes only (no breaking)
- Keep existing methods as wrappers
- Litmus test passes (simple wraps sophisticated)
- Already ~60% I/O-free (just exposing what's there)

### Medium Risk ⚠️

- API surface increases (but organized, not random)
- Users must understand 3-phase pattern (docs will help)
- Materialization may increase memory (but controllable)

### Mitigation

- Start with Phase 1 only (2 weeks, high impact)
- Evaluate with real users before continuing
- Comprehensive docs and examples
- Performance guide (when to materialize vs stream)

---

## Success Criteria

**After Phase 1** (LogSegment refactoring):

✅ Old API still works (zero user changes)  
✅ New APIs available for advanced users  
✅ Parallel fetch demo shows performance gain  
✅ Litmus test passes (old wraps new)  
✅ No regressions in simple case

**After Full Refactoring**:

✅ All 5 control flows have I/O-free variants  
✅ Users can choose sync/async/parallel  
✅ Simple case requires zero glue  
✅ Advanced case requires minimal glue (<10 lines)  
✅ Documentation covers all patterns  
✅ Examples demonstrate capabilities

---

## Next Steps

1. **Review this analysis** with stakeholders
2. **Decide**: Proceed with Phase 1?
3. **Implement**: LogSegment refactoring (2 weeks)
4. **Evaluate**: Performance gains, API ergonomics
5. **Decide**: Continue to Phase 2-5 or adjust?

**Recommendation**: Start Phase 1 immediately. It's low risk, high impact, and provides concrete data for deciding whether to continue.

---

## Document Organization

This analysis is part of a series:

- **`control-flow-analysis.md`** (this file) - Detailed technical analysis (840 lines)
- **`CONTROL-FLOW-SUMMARY.md`** (current) - Executive summary (this document)
- **`async-exploration-findings.md`** - Original Phase 1 findings (720 lines)
- **`async-exploration-phase2-plan.md`** - Original Phase 2 plan (610 lines)

All documents are interconnected and should be read together for full context.


