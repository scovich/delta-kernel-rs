# Driving connector workflows

Connector-driven workflows and generators use typed request/reply channels to surface every
operation requiring connector work. Their protocol logic runs in ordinary runtime-neutral Rust
futures, allowing Kernel to await replies from the connector instead of invoking connector code.

Your connector owns the coroutine and decides when to advance it, how to execute each request, and
when to reply. Kernel has no async runtime, scheduler, or tasks of its own.

Before reading this page, make sure you understand the
[connector's role](./overview.md).

> [!NOTE]
> Driving Kernel's built-in request vocabulary uses public APIs. Enable the experimental
> `internal-api` feature only to define a custom request vocabulary.

## Choose the execution surface

Kernel exposes two execution surfaces:

- **Connector-driven execution** exposes lazy `Workflow` and `Generator` operations whose channels
  surface typed requests. Your connector owns each root operation and serves its requests.
- **Engine compatibility execution** accepts `&dyn Engine`. Kernel's compatibility adapter drives
  the operation synchronously through Engine handlers.

Both surfaces run the same protocol logic. Choose based on who should control execution. An
asynchronous connector normally uses connector-driven execution. Making Engine methods async would
not provide the same control: Kernel would still call from the connector back into connector code.

The default-engine crate supports both choices:

- `AsyncEngineConnector` serves one workflow request at a time through native asynchronous
  `object_store` operations.
- `DefaultEngine` implements the synchronous `Engine` compatibility surface through a
  `TaskExecutor`. Kernel's compatibility adapter drives the coroutine and calls Engine handlers for
  each request.

## Choose a driver policy

The channel exposes every Kernel request, so the connector chooses how to drive the coroutine:

- A **synchronous driver** handles one request, sends its reply, then calls `try_advance()` again.
  Kernel's internal futures are manually polled with a no-op waker; no async runtime is required.
- A **one-at-a-time async driver** awaits one request handler, sends its reply, then awaits
  `advance()` again. Kernel's futures run inside the driver future on the connector's async runtime.
- A **concurrent async driver** schedules requests and continues awaiting `advance()`. It owns
  fanout, cancellation, and backpressure policy.
- The **Engine compatibility adapter** is another synchronous driver. It translates requests into
  calls to Engine handler traits, preserving existing Engine-based APIs.

## Run a workflow with the default async connector

When the default async connector should handle every request, pass a lazy workflow to
`AsyncEngineConnector::run`:

```rust,no_run
# extern crate delta_kernel;
# extern crate delta_kernel_default_engine;
# extern crate tokio;
# use std::sync::Arc;
# use delta_kernel::object_store::memory::InMemory;
# use delta_kernel::{DeltaResult, Snapshot};
# use delta_kernel_default_engine::AsyncEngineConnector;
# #[tokio::main]
# async fn main() -> DeltaResult<()> {
let store = Arc::new(InMemory::new());
let connector = AsyncEngineConnector::new(store);

let workflow = Snapshot::builder_for("memory:///table/").workflow();
let snapshot = connector.run(workflow).await?;
# let _ = snapshot;
# Ok(())
# }
```

Creating `workflow` does no I/O. `run` retains the workflow, waits for each Kernel step, and serves
one request at a time until the workflow completes.

## Own the driver loop

Own the workflow loop when you need to intercept requests, such as catalog commit and publish
operations:

```rust,no_run
# extern crate delta_kernel;
# extern crate delta_kernel_default_engine;
# use delta_kernel::coroutine::{drive_async_workflow, StaticWorkflow};
# use delta_kernel::DeltaResult;
# use delta_kernel_default_engine::AsyncEngineConnector;
async fn drive<O: Send + 'static>(
    connector: &AsyncEngineConnector,
    workflow: StaticWorkflow<O>,
) -> DeltaResult<O> {
    Ok(drive_async_workflow!(workflow, |request| {
        connector.reply(request).await?;
    }))
}
```

`drive_async_workflow!` awaits `advance` and runs its handler for each request until Kernel
completes. `reply` performs the request and delivers its result to the suspended Kernel await point.
Replying wakes an awaiting driver, but the connector's executor remains responsible for polling it.
Synchronous drivers use `drive_workflow!` and `try_advance` instead; `try_advance` returns
`Error::WouldBlock` rather than waiting when no step is available.

If you handle a request yourself, match its variant to obtain the operation and single-use `Reply`.
Call `reply.send(result)` after processing the operation.

## Drive generators

A caller without a parent channel constructs a `StaticGenerator` from a channel-taking operation:

```ignore
let mut generator = StaticGenerator::new(move |channel| operation(channel, owned_inputs));
```

The callback must return `Generator<'static, _>`, so borrowed state cannot escape into the static
generator:

- `GeneratorStep::Request` asks the connector to perform work.
- `GeneratorStep::Yield` transfers one item to the caller.
- `GeneratorStep::Done` indicates that the generator completed.

Use `advance` in an async driver or `try_advance` in a synchronous driver. Kernel acknowledges a
yielded item before returning it. Calling either method after `Done` returns an error. Dropping the
generator abandons it and expires its outstanding replies.

## Compose inside Kernel

When code already has a borrowed Kernel `Channel`, call async workflow implementations directly.
Pass the channel to streaming operations and consume the returned `Generator` with its async
`next` method. Its lifetime covers the channel and any other borrowed inputs. The child posts
requests through its parent coroutine, and each successful `next` acknowledges the yielded item
automatically.

Kernel code can combine futures returned by `Channel` with runtime-neutral future and stream
combinators. Operations that perform I/O, data processing, or other connector work still pass
through `Channel`.

## Handle pagination

Large reads and listings use `PageRequest`:

- `Start` initializes an operation and returns its first `Page`.
- `Prepare` initializes an operation and returns a `Cursor` without fetching the first page.
- `Continue` consumes a cursor and returns the next page.

The cursor contains connector-owned state. Kernel stores it opaquely and returns it with the next
continuation request. `Page.next == None` marks exhaustion; an empty payload with a live cursor
doesn't.

## Propagate errors and cancellation

Replying with `Err(error)` returns that error to the exact Kernel await point that created the
request. Replies are single-use and request-specific, so one request's response cannot resume
another. For a live asynchronously polled operation, sending or dropping a reply wakes the
executor task that last polled the corresponding Kernel future.

Configure `AsyncEngineConnector::with_cancellation_token` to race I/O and stream polling against a
Tokio cancellation token. Dropping a workflow or static generator abandons that operation.
Dropping a generator ends its stream early. In either case, Kernel drops its suspended state, and
later replies are discarded.

## What's next

- [The Engine trait](../concepts/engine_trait.md) explains the synchronous compatibility surface.
- [The `EngineData` trait](./engine_data.md) defines the opaque data exchanged with Kernel.
- [Implementing a catalog committer](../catalog_managed/committer.md) explains catalog-specific
  request handling.
