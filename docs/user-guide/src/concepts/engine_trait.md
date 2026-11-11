# The Engine trait

The `Engine` trait separates Kernel's protocol logic from connector-provided I/O and computation.
This boundary lets you use Kernel with different storage systems, columnar formats, and runtimes.
The [`Engine` rustdoc] owns the exact trait and handler contracts. A connector that serves Kernel's
I/O itself can skip the trait and drive Kernel's operations as workflows. See
[Driving connector workflows](../connector/driving_workflows.md).

[`Engine` rustdoc]: https://docs.rs/delta_kernel/latest/delta_kernel/trait.Engine.html

## Where the Engine fits

Kernel decides what a connector must do, such as listing a transaction log or evaluating a data
skipping predicate. Your Engine decides how to do it with the systems your connector already uses.

```text
Connector workflow
       |
       v
Kernel protocol and transaction logic
       |
       v
Engine capabilities
       |
       v
Storage, columnar data, and expression runtime
```

The `Engine`-based Kernel APIs stay synchronous and runtime-independent. An Engine can use
asynchronous I/O internally, as the default engine does, without requiring every Kernel caller to
adopt that runtime.

## Capability boundaries

An Engine supplies these handler capabilities:

| Handler | Responsibility |
|---------|----------------|
| [`StorageHandler`] | List objects and read or write raw bytes |
| [`JsonHandler`] | Translate Delta JSON files and strings to and from `EngineData` |
| [`ParquetHandler`] | Read and write data files and checkpoints |
| [`EvaluationHandler`] | Evaluate Kernel expressions and predicates |

[`StorageHandler`]: https://docs.rs/delta_kernel/latest/delta_kernel/trait.StorageHandler.html
[`JsonHandler`]: https://docs.rs/delta_kernel/latest/delta_kernel/trait.JsonHandler.html
[`ParquetHandler`]: https://docs.rs/delta_kernel/latest/delta_kernel/trait.ParquetHandler.html
[`EvaluationHandler`]: https://docs.rs/delta_kernel/latest/delta_kernel/trait.EvaluationHandler.html

With the experimental `declarative-plans` feature, your Engine can also provide a [`PlanExecutor`].
Kernel describes data work as a plan that your executor can run using your compute engine's
optimizer, parallelism, and I/O. This capability is optional; see the
[declarative plans rustdoc] for the execution model and contracts.

[`PlanExecutor`]: https://docs.rs/delta_kernel/latest/delta_kernel/trait.PlanExecutor.html
[declarative plans rustdoc]: https://docs.rs/delta_kernel/latest/delta_kernel/plans/index.html

The split lets you replace one capability without coupling Kernel to the rest of your connector.
For example, a connector can keep the default storage and JSON handlers while providing a Parquet
reader that produces its native columnar format.

Handlers exchange columnar values through `EngineData`. Kernel treats those values as opaque and
uses visitors when it needs typed values. [The EngineData trait](../connector/engine_data.md)
explains how that abstraction works.

## Cancellation

A connector can pass a cancellation token to a single call so long-running reads stop when their
caller goes away. Kernel carries that token to cancellation-aware storage, JSON, and Parquet
operations.

### Passing a token

Pass the token to each call whose storage reads you want to cancel, through its
`*_with_cancellation` sibling. Kernel stores the token nowhere: not on the builder, the snapshot,
the reader, or the scan. A metadata reader does not inherit the token used to build its `Snapshot`.
Workflow entry points take no token. See
[Cancellation](../connector/driving_workflows.md#cancellation).

```rust,no_run
# extern crate delta_kernel;
# use delta_kernel::{CancellationTokenRef, Engine, Snapshot};
# fn read_metadata(
#     engine: &dyn Engine,
#     table_root: &str,
#     token: CancellationTokenRef,
# ) -> delta_kernel::Result<()> {
let snapshot = Snapshot::builder_for(table_root)
    .build_with_cancellation(engine, Some(token.clone()))?;

let reader = snapshot.reader();
let version =
    reader.get_app_id_version_with_cancellation("my-app", engine, Some(token.clone()))?;

let scan = snapshot.scan_builder().build()?;
let metadata = scan.scan_metadata_with_cancellation(engine, Some(token))?;
# let _ = (version, metadata);
# Ok(())
# }
```

Passing `None` leaves your engine's own cancellation behavior unchanged.

`SnapshotReader` getters remain synchronous and may block. The token applies to handler-backed
I/O, so a getter may return metadata already held by the `Snapshot` without checking cancellation.
For more about cancelling scans, see
[Advanced reads with `scan_metadata()`](../reading/scan_metadata.md#cancelling-a-scan).

See the [Engine operation cancellation contract] for exact behavior and
[Implementing the Engine trait](../connector/implementing_engine.md#support-cancellation) for the
implementation workflow.

[Engine operation cancellation contract]: https://docs.rs/delta_kernel/latest/delta_kernel/cancellation/index.html#engine-operation-contract

## The default engine

`DefaultEngine` combines Arrow, `object_store`, and Tokio into a ready-to-use Engine. Use it when
your connector doesn't need its own columnar representation or I/O implementation.

```rust,no_run
# extern crate delta_kernel;
# extern crate delta_kernel_default_engine;
# extern crate url;
# use delta_kernel_default_engine::DefaultEngine;
# use delta_kernel_default_engine::storage::store_from_url;
# use delta_kernel::KernelResult;
# fn example() -> KernelResult<()> {
let url = url::Url::parse("file:///path/to/table")?;
let store = store_from_url(&url)?;
let engine = DefaultEngine::builder(store).build();
# let _ = engine;
# Ok(())
# }
```

The builder lets you replace the task executor and configure observability without changing
Kernel's APIs. See the [`DefaultEngine` rustdoc] for its current options.

[`DefaultEngine` rustdoc]: https://docs.rs/delta_kernel_default_engine/latest/delta_kernel_default_engine/struct.DefaultEngine.html

### Choosing an executor

The default background executor owns a single-threaded Tokio runtime on a dedicated thread. It is a
good fit when your process has no runtime to share or when you want to isolate Kernel I/O.

Use the multi-thread executor when your connector already owns a multi-threaded Tokio runtime or
needs a dedicated pool with explicit sizing. Do not give it a current-thread runtime handle.

| Connector environment | Executor choice |
|-----------------------|-----------------|
| No existing Tokio runtime | Background executor, which is the default |
| Existing multi-threaded Tokio runtime | Multi-thread executor with that runtime's handle |
| Dedicated pool with connector-defined limits | Owned multi-thread executor |

If a client library requires an active Tokio context during construction, enter the default
engine's runtime context while constructing that client. Drop the returned guard before entering
another context.

## When to implement your own Engine

Implement an Engine when your connector needs at least one of these boundaries to behave
differently:

- data must stay in a non-Arrow columnar representation;
- storage access must use an existing distributed or authenticated client;
- expression evaluation should reuse the compute engine's optimizer and kernels; or
- I/O scheduling and resource ownership must integrate with the connector's runtime.

Changing only the object-store backend does not require a custom Engine. Configure the default
engine for local storage, S3, GCS, or Azure instead.

## What's next

- [Implementing the Engine trait](../connector/implementing_engine.md) walks through a custom
  implementation.
- [The EngineData trait](../connector/engine_data.md) explains the opaque data boundary.
- [Configuring storage](../storage/configuring_storage.md) configures the default engine's storage.
