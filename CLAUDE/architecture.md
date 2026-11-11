# Source navigation

This file helps contributors find implementation boundaries. It does not define public API
contracts or retell the connector workflow. Use the
[user guide](https://docs.delta.io/kernel/rust/) for introductions and connector workflows, and
[rustdoc](https://docs.rs/delta_kernel/latest/delta_kernel/) for API usage and behavior.
Source comments are authoritative for implementation gotchas, pitfalls, invariants, and low-level
design decisions. This map provides entry points and broader design context.

## Find the owning module

| Task | Start here | Related implementation |
|------|------------|------------------------|
| Load or refresh a snapshot | `kernel/src/snapshot/` | `log_segment/`, `log_reader/`, `crc/` |
| Build or execute a read | `kernel/src/scan/` | `log_replay/`, `actions/deletion_vector.rs` |
| Read changes between versions | `kernel/src/incremental_scan/`, `kernel/src/table_changes/` | `log_replay/` |
| Create or modify a table | `kernel/src/transaction/` | `committer/`, `partition/` |
| Write checkpoints or CRCs | `kernel/src/checkpoint/`, `kernel/src/crc/` | `actions/` |
| Implement engine capabilities | `kernel/src/lib.rs`, `kernel/src/engine/` | `default-engine/src/` |
| Add or change connector-served I/O | `kernel/src/coroutine/` | `default-engine/src/coroutine.rs`, `delta-kernel-unity-catalog/src/coroutine.rs` |
| Change schemas or expressions | `kernel/src/schema/`, `kernel/src/expressions/` | `transforms/` |
| Change error types or propagation | `kernel/src/error.rs`, `kernel/src/lib.rs` | API result and iterator aliases |
| Change protocol feature checks | `kernel/src/table_features/` | `table_configuration.rs`, `actions/` |
| Work on catalog-managed tables | `kernel/src/committer/` | `delta-kernel-unity-catalog/` |

Search for the public type or operation before assuming these entry points are exhaustive. Module
layouts change more often than their ownership boundaries.

## Boundaries that matter in implementation work

Kernel handles the Delta protocol; connectors handle execution, distribution, and data flow.

```text
Compute engine (Spark, Flink, DuckDB, Polars, ...)
  -> Delta connector (implements the compute engine's data source API)
    -> Delta Kernel (protocol logic)
      -> Engine trait, or a connector-owned driver (I/O and computation boundary)
        -> DefaultEngine, AsyncEngineConnector, or a custom engine
          -> Storage and computation runtime
```

- Kernel describes I/O and computation through `Engine` handlers or through typed requests that a
  connector-owned driver serves; engine implementations and drivers perform them. The module docs
  of `kernel/src/coroutine/` explain the request protocol.
- New I/O goes through the `Channel`. Write I/O-performing code as
  `async fn foo_with_channel(channel: &Channel, ..)` using `ChannelExt` operations, and keep
  `foo(engine)` as a shim over it (`run_workflow_with_engine!`). Put `#[instrument]` on the
  `_with_channel` function so every path reports the same metrics. A new kind of connector work
  needs a `Request` variant and an arm in both `EngineConnector` and `AsyncEngineConnector`. Do not
  call `engine.*_handler()` from new kernel code.
- `EngineData` is opaque to kernel production code. Inspect rows through the visitor APIs, and
  handle every batch returned for a file.
