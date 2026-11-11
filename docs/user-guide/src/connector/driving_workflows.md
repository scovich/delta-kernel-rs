# Driving connector workflows

To run Kernel on your own async runtime, or to serve its I/O requests yourself, you drive Kernel
operations as workflows instead of handing Kernel an `Engine`.

Before reading this page, make sure you understand the [connector's role](./overview.md).

> [!NOTE]
> Workflows are experimental. Driving Kernel's built-in requests uses public APIs. Enable the
> `internal-api` feature only to define your own request vocabulary.

## Choose how to run an operation

Operations that take an `&dyn Engine` have a lazy twin that takes none. Calling the twin does no
I/O:

| Call | What runs |
|------|-----------|
| `Snapshot::builder_for(url).build(&engine)` | Kernel drives the operation synchronously through your `Engine` handlers (the **Engine shim**). |
| `Snapshot::builder_for(url).build_workflow()` | Kernel returns a `Workflow`. You drive it and serve its requests. |
| `Snapshot::builder_for(url).build_with_cancellation(&engine, Some(token))` | The Engine shim, with a [per-call token](#cancellation). |

Both forms run the same protocol logic. Use the Engine shim when an `Engine` is the simplest fit.
Use workflows when you want to choose the runtime, the concurrency, or where each request executes.

A workflow is a future paired with a typed request channel. When Kernel needs connector work, such
as listing, reading, writing, evaluating, or committing, it posts a `Request` that carries the
operation and a single-use `Reply`, then suspends until you reply. Kernel has no runtime, threads,
or tasks of its own: nothing advances until you drive the workflow.

## Run a workflow with the default async connector

`DefaultEngine::async_connector()` returns an `AsyncEngineConnector` that serves every request with
native `object_store` I/O. Pass it a workflow with `run`:

```rust,no_run
# extern crate delta_kernel;
# extern crate delta_kernel_default_engine;
# extern crate tokio;
# extern crate url;
# use delta_kernel::{KernelError, Snapshot};
# use delta_kernel_default_engine::storage::store_from_url;
# use delta_kernel_default_engine::AsyncEngineConnector;
# #[tokio::main]
# async fn main() -> delta_kernel::Result<()> {
let url = url::Url::parse("file:///path/to/table/").map_err(KernelError::from)?;
let connector = AsyncEngineConnector::new(store_from_url(&url)?);

let workflow = Snapshot::builder_for(url).build_workflow();
let snapshot = connector.run(workflow).await?;
# let _ = snapshot;
# Ok(())
# }
```

`run` serves one request at a time until the workflow completes.

To commit, run the transaction's committer-free workflow:

```rust,ignore
let transaction = connector.run(update_builder.build_workflow()).await?;
// ... write files and call transaction.add_files(...) ...
let result = connector.run(transaction.commit_workflow()).await?;
```

The connector that serves `Request::Commit` selects the committer. `AsyncEngineConnector` commits to
path-based tables with `FileSystemCommitter` and rejects catalog-managed tables. To commit to a Unity
Catalog table, [serve its requests yourself](#serve-unity-catalog-commits).

## Serve requests yourself

To intercept some requests, such as catalog commits, own the driver loop and pass the rest to
`AsyncEngineConnector::respond`:

```rust,ignore
use delta_kernel::coroutine::drive_workflow_async;
use delta_kernel::coroutine::kernel::Request;

let snapshot = drive_workflow_async!(workflow, |request| match request {
    Request::Commit(commit, reply) => reply.send(my_catalog.commit(*commit).await),
    request => connector.respond(request).await?,
});
```

`drive_workflow_async!` awaits each step and runs its handler for every request until the workflow
completes. The handler can use `?` and `.await`. Synchronous drivers use `drive_workflow!`, which
calls `try_advance`. When no step is ready, `try_advance` returns `KernelError::WouldBlock` instead
of waiting.

Match on the request variant to get the operation and its `Reply`, then call `reply.send(result)`
once. If you drop a reply without sending, the waiting operation fails with
`KernelError::RequestRejected`. The [`Request`] rustdoc lists every variant with the work it asks
for and the reply it expects.

An async driver can serve several requests concurrently: keep awaiting `advance` while earlier
requests run. You then own the fanout, backpressure, and cancellation policy.

## Serve Unity Catalog commits

To commit to a Unity Catalog (UC) table from a native driver, serve the commit requests yourself.
UC ratifies a commit through its own API, so `UCCommitter::commit_workflow()` posts
`delta_kernel_unity_catalog::coroutine::Request`s (`uc::Request`) that only your connector can
serve. `AsyncEngineConnector` can't serve them.

This example depends on the `delta-kernel-unity-catalog` and `unity-catalog-delta-client-api`
crates. `run_kernel` drives a kernel workflow with an `AsyncEngineConnector`, a `UCCommitter`, and
your own `UpdateTableClient`:

```rust,ignore
use delta_kernel::committer::CommitResponse;
use delta_kernel::coroutine::{drive_workflow_async, kernel};
use delta_kernel::{KernelError, KernelResult};
use delta_kernel_default_engine::AsyncEngineConnector;
use delta_kernel_unity_catalog::{UCCommitter, coroutine as uc};
use unity_catalog_delta_client_api::UpdateTableClient;

async fn run_kernel<C: UpdateTableClient + 'static, O: Send + 'static>(
    connector: &AsyncEngineConnector,
    committer: &UCCommitter<C>,
    client: &C,
    workflow: kernel::Workflow<'_, O>,
) -> KernelResult<O> {
    Ok(drive_workflow_async!(workflow, |request| match request {
        kernel::Request::Commit(commit, reply) => {
            let workflow = committer.commit_workflow(*commit);
            reply.send(drive_catalog_workflow(connector, client, workflow).await);
        }
        kernel::Request::RequireCatalogCommitter(reply) => reply.send(Ok(())),
        kernel::Request::Publish(metadata, reply) => {
            reply.send(connector.run(committer.publish_workflow(metadata)).await);
        }
        request => connector.respond(request).await?,
    }))
}

async fn drive_catalog_workflow<C: UpdateTableClient>(
    connector: &AsyncEngineConnector,
    client: &C,
    workflow: uc::CommitWorkflow<'_>,
) -> KernelResult<CommitResponse> {
    Ok(drive_workflow_async!(workflow, |request| {
        serve_catalog(connector, client, request).await?;
    }))
}

async fn serve_catalog<C: UpdateTableClient>(
    connector: &AsyncEngineConnector,
    client: &C,
    request: uc::Request,
) -> KernelResult<()> {
    match request {
        uc::Request::WriteJson(operation, reply) => {
            connector.respond(kernel::Request::WriteJson(operation, reply)).await
        }
        uc::Request::Head(url, reply) => {
            connector.respond(kernel::Request::Head(url, reply)).await
        }
        uc::Request::UpdateTable(target, request, reply) => {
            let result = client
                .update_table(&target, request)
                .await
                .map_err(|e| KernelError::Generic(format!("UC update_table error: {e}")));
            reply.send(result);
            Ok(())
        }
        uc::Request::WriteDeclarative(..) => Err(KernelError::internal_error(
            "imperative commit requested a declarative JSON write",
        )),
    }
}
```

To commit, run the transaction's committer-free workflow through `run_kernel`:

```rust,ignore
let result = run_kernel(&connector, &committer, &client, transaction.commit_workflow()).await?;
```

`run_kernel` handles the three commit-related kernel requests, [`Request::Commit`],
[`Request::RequireCatalogCommitter`], and [`Request::Publish`], and forwards everything else to
`AsyncEngineConnector`. See [`uc::Request`] for what serving each Unity Catalog request requires.

## Stream results

Operations such as `Scan::scan_metadata_workflow()` first return a workflow whose output is a
stream. Pass that output to `Coroutine::from_generator` and advance the coroutine:

```rust,ignore
use std::ops::ControlFlow;
use delta_kernel::coroutine::kernel::Coroutine;

let stream = connector.run(scan.scan_metadata_workflow()).await?;
let mut coroutine = Coroutine::from_generator(stream);
while let ControlFlow::Continue(scan_metadata) = connector.advance(&mut coroutine).await? {
    let scan_metadata = scan_metadata?;
    // ... read the files it lists ...
}
```

Each step of a coroutine is a [`CoroutineStep`]. Dropping the coroutine abandons the operation, and
later replies to its requests are discarded.

## Handle pagination

Large reads and listings arrive as a [`PageRequest`]. You choose the page boundaries and the
contents of each `Cursor`. Kernel stores the cursor opaquely and returns it with the next request.
The [`Page`] rustdoc defines how a page ends the stream.

## Cancellation

Workflow entry points take no token. To cancel, drop the workflow or end your driver loop.
`AsyncEngineConnector::with_cancellation_token` does this for you: it races each drive against a
Tokio `CancellationToken` and returns `KernelError::Cancelled`.

The two drivers differ in what the cancelled operation reports:

| Driver | What happens | Operation metrics |
|--------|--------------|-------------------|
| Engine shim (`build(&engine)`, `build_with_cancellation(&engine, token)`) | A handler returns `KernelError::Cancelled`. Kernel delivers it to the waiting operation, drives the operation to completion, and returns `Cancelled` even if the operation produced a result. | The failure event, such as `SnapshotBuildFailure`. |
| `AsyncEngineConnector` with a token | Kernel drops the drive at its next suspension point, and in-flight I/O is dropped with it. The operation never sees an error. Kernel logs an `error` event, "workflow was abandoned while suspended". | The success event, such as `SnapshotBuildSuccess`, with the duration up to cancellation. |

A cancelled snapshot build driven by `AsyncEngineConnector` therefore reports `SnapshotBuildSuccess`
even though no snapshot was built. If your monitoring counts operation outcomes from these events,
treat `KernelError::Cancelled` from your driver as the authoritative result. To make every
cancelled operation report a failure event, use the Engine shim. To tear down in-flight I/O
promptly, use a native driver.

## Tracing and metrics

- **Requests you serve run outside the operation's span.** Kernel enters an operation's `tracing`
  span, such as `snap.checkpoint` or `txn.commit`, only while it advances the workflow. Storage I/O
  and any spans your driver creates are not children of that span in a trace-tree view, such as an
  OpenTelemetry exporter. To group them with the operation, advance the workflow inside a span you
  own.
- **Kernel metrics are unaffected.** `ReportGeneratorLayer` reports each metric span on its own and
  never reads its parents.
- **The Engine shim reads every handler once per drive.** It calls `storage_handler()`,
  `json_handler()`, `parquet_handler()`, and `evaluation_handler()` when the drive starts. This
  matters only to a test `Engine` that panics or counts calls on those accessors.

A driver that serves storage requests itself should emit the storage metrics the built-in handlers
emit. See [Metrics and monitoring](../observability/observability.md#storage-and-file-io-events).

## What's next

- [The Engine trait](../concepts/engine_trait.md) explains the compatibility surface and the
  per-call cancellation variants.
- [The `EngineData` trait](./engine_data.md) defines the opaque data exchanged with Kernel.
- [Implementing a catalog committer](../catalog_managed/committer.md) explains catalog-specific
  commit and publish handling.

[`Request`]: https://docs.rs/delta_kernel/latest/delta_kernel/coroutine/kernel/enum.Request.html
[`CoroutineStep`]: https://docs.rs/delta_kernel/latest/delta_kernel/coroutine/enum.CoroutineStep.html
[`PageRequest`]: https://docs.rs/delta_kernel/latest/delta_kernel/coroutine/enum.PageRequest.html
[`Page`]: https://docs.rs/delta_kernel/latest/delta_kernel/coroutine/struct.Page.html
[`Request::Commit`]: https://docs.rs/delta_kernel/latest/delta_kernel/coroutine/kernel/enum.Request.html#variant.Commit
[`Request::Publish`]: https://docs.rs/delta_kernel/latest/delta_kernel/coroutine/kernel/enum.Request.html#variant.Publish
[`Request::RequireCatalogCommitter`]: https://docs.rs/delta_kernel/latest/delta_kernel/coroutine/kernel/enum.Request.html#variant.RequireCatalogCommitter
[`uc::Request`]: https://docs.rs/delta-kernel-unity-catalog/latest/delta_kernel_unity_catalog/coroutine/enum.Request.html
