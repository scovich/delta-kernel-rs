# Building a connector: overview

A **connector** is the adapter layer between a **compute engine** (Spark, Flink, DuckDB,
Polars, DataFusion, or your own query engine) and Delta tables. This matters because
compute engines know nothing about the Delta protocol. They expose their own DataSource
APIs, and the connector translates those calls into Delta Kernel operations.

## The big picture

Every compute engine defines its own interfaces for reading and writing data. For example:

- **Apache Spark** has [DataSourceV2](https://spark.apache.org/docs/latest/api/java/org/apache/spark/sql/connector/catalog/Table.html):
  `Table`, `ScanBuilder`, `Scan`, `Batch`, `PartitionReader`, `WriteBuilder`, and others.
- **Apache Flink** has its Source/Sink APIs:
  `Source`, `SplitEnumerator`, `SourceReader`, `Sink`, `SinkWriter`, and others.
- **DuckDB** has its [Extension API](https://duckdb.org/docs/extensions/overview.html):
  `TableFunction`, `TableFunctionBindInput`, and others.

Building a Delta connector means implementing these compute-engine-specific interfaces and
using Delta Kernel to fulfill them:

```text
┌─────────────────────────────────────────────────────────┐
│                    Compute Engine                       │
│          (Spark, Flink, DuckDB, Polars, ...)            │
│                                                         │
│  "I need a Table, a Scan, a Writer..."                  │
└───────────────────────┬─────────────────────────────────┘
                        │ calls your DataSource API impl
                        ▼
┌─────────────────────────────────────────────────────────┐
│                 Your Delta Connector                    │
│                                                         │
│  Implements the compute engine's DataSource interfaces  │
│  (e.g. Spark's Table/ScanBuilder/Scan/Batch/Writer)     │
│                                                         │
│  Uses Kernel to fulfill those interfaces:               │
│    Snapshot  ->  table metadata, schema, version        │
│    Scan      ->  which files to read, data skipping     │
│    Transaction -> describe a table write                │
│    Committer   -> commit atomically                     │
└───────────────────────┬─────────────────────────────────┘
                        │ calls Kernel APIs
                        ▼
┌─────────────────────────────────────────────────────────┐
│                    Delta Kernel                         │
│                                                         │
│  Protocol logic, log replay, data skipping,             │
│  schema enforcement, transaction coordination           │
│                                                         │
│  Returns typed requests or uses Engine compatibility    │
└───────────────────────┬─────────────────────────────────┘
                        │
                        ▼
┌─────────────────────────────────────────────────────────┐
│                  Connector execution                    │
│                                                         │
│  AsyncEngineConnector or your request driver            │
│  DefaultEngine compatibility adapter                    │
└─────────────────────────────────────────────────────────┘
```

## Example: how the Spark connector works

To make this concrete, here's how the
[Delta Spark connector](https://github.com/delta-io/delta/tree/master/spark) maps Spark's
DataSourceV2 interfaces to Kernel APIs:

| Spark DataSourceV2 interface | Connector class | Uses Kernel... |
|------------------------------|----------------|----------------|
| `Table` (entry point, schema, capabilities) | `SparkTable` | `Snapshot` for schema and metadata |
| `ScanBuilder` (filter pushdown, column pruning) | `SparkScanBuilder` | `ScanBuilder` with predicates and column selection |
| `Scan` (plan which files to read) | `SparkScan` | `Scan.getScanFiles()` to get the list of data files |
| `Batch` (partition files for parallel execution) | `SparkBatch` | Partitions scan files across Spark tasks |
| `PartitionReader` (read data from files) | `SparkPartitionReader` | Reads Parquet files assigned to this task |

The pattern is always the same: the compute engine asks for something through its DataSource
API, and the connector translates that into the corresponding Kernel call.

## The Snapshot is the center of everything

All connector operations start from a `Snapshot`, an immutable view of the table at a
specific version:

```rust,ignore
let snapshot = Snapshot::builder_for(table_url).build(&engine)?;
```

From a snapshot, you can:

| Operation | Kernel API | Typical DataSource equivalent |
|-----------|-----------|-------------------------------|
| Get schema and metadata | `snapshot.schema()`, `snapshot.table_properties()` | Table schema discovery |
| Read data | `snapshot.scan_builder()` | Scan / PartitionReader |
| Write data | `snapshot.transaction_with_committer(committer, &engine)` | Writer / Committer |
| Checkpoint | `snapshot.create_checkpoint_writer(&engine)` | Maintenance task |

## What your connector does vs. what Kernel does

The key principle: **Kernel handles the Delta protocol, your connector handles execution.**

**Kernel handles:**
- Log replay (figuring out which files are active at a version)
- Data skipping (pruning files using statistics and predicates)
- Schema enforcement and column mapping
- Transaction conflict detection
- Protocol compliance (table features, reader/writer requirements)

**Your connector handles:**
- Implementing the compute engine's DataSource API
- Controlling parallelism and distribution (how many threads/tasks, which worker reads which file)
- Data format conversion (Kernel's `EngineData` to your engine's native format)
- Query planning integration (pushing filters and projections down to Kernel)
- Resource management (memory budgets, connection pooling)

## Choose an execution surface

Connector-driven entry points expose lazy `Workflow` and `Generator` operations. A workflow owns
its request channel; a generator binds to an existing channel or to an owning `StaticGenerator`
wrapper. Kernel protocol logic is async, but every operation requiring connector work passes
through the channel as a typed `Request`. Your connector owns the coroutine and serves each request,
so Kernel never calls connector operations. A synchronous connector can drive this surface without
an async runtime; an async connector can use its own executor sequentially or concurrently. Use
this surface when your connector should control scheduling, cancellation, or catalog requests. See
[Driving connector workflows](./coroutines.md).

Engine-compatible entry points accept `&dyn Engine`. Kernel's synchronous compatibility adapter
drives the same protocol futures and calls Engine handlers for each request. Use this surface when a
blocking callback API fits your connector or when you need to preserve an existing Engine
implementation.

## The Engine trait compatibility surface

Kernel never does I/O directly. Engine-compatible methods call the `Engine` trait when they need
to list files, read Parquet, parse JSON, or evaluate expressions.

Kernel provides `DefaultEngine` (Arrow + `object_store` + Tokio) as the simplest integration path.
It supports local storage, S3, Azure, and GCS without custom handlers. If you need native data,
custom I/O or evaluation, or finer execution control, drive workflow requests directly instead of
implementing a custom Engine. Custom Engine implementations mainly preserve existing Engine-based
integrations. See [The Engine trait](../concepts/engine_trait.md) for compatibility details.

## Getting started

Building a connector typically involves these steps:

1. **Choose the execution surface.** Use `AsyncEngineConnector` or your own synchronous or async
   request loop for connector-controlled execution. Use `DefaultEngine` when maximum simplicity is
   more important than control. See [Driving connector workflows](./coroutines.md) and
   [The Engine trait](../concepts/engine_trait.md).

2. **Implement your DataSource's read interfaces** using Kernel's Scan API:
   - Create a `Snapshot` to discover the table schema and version
   - Use `ScanBuilder` to push down filters and column projections
   - Use `Scan` to get the list of files to read
   - Distribute those files across your engine's execution model
   - Serve read and transformation requests through your chosen execution surface
   - See [Building a scan](../reading/building_a_scan.md) and
     [Filter pushdown](../reading/filter_pushdown.md)

3. **Implement your DataSource's write interfaces** using Kernel's Transaction API:
   - Create a `Transaction` from a `Snapshot`
   - Write Parquet files and register them with the transaction
   - Commit atomically, handling conflicts and retries
   - See [Creating a table](../writing/create_table.md) and
     [Appending data](../writing/append.md)

4. **Handle distribution** (if your engine is distributed):
   - Serialize scan metadata and transaction state to ship to workers
   - Collect results from workers and commit on the driver
   - See [Advanced reads with scan_metadata()](../reading/scan_metadata.md)

## What's next

- [Driving connector workflows](./coroutines.md): connector-controlled execution
- [Implementing Engine compatibility](./implementing_engine.md): the legacy compatibility
  interface
- [Building a scan](../reading/building_a_scan.md): the Kernel read API
- [Appending data](../writing/append.md): the Kernel write API
