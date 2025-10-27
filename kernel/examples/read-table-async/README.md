# read-table-async

A truly asynchronous example demonstrating pure async usage of the delta kernel with native streams and `.await`.

## Purpose

This example demonstrates:
- Pure async API usage with native `.await` (no `await_!` macro)
- Native async streams from `futures` crate
- Async-only compilation (fails without `--features async`)
- Idiomatic async Rust patterns

## When to Use This Pattern

Use this pattern when:
- Your application is fully async
- You want to use the kernel only in async mode
- You prefer native async/await over mode-agnostic macros
- You're building async-first applications

## Comparison with Other Examples

- **read-table**: Mode-agnostic example using `await_!` macro (works with both sync/async kernel)
- **read-table-async**: This example - pure async, requires async kernel
- **read-table-sync**: Pure sync, requires sync kernel

## Building

```bash
# Async mode (works) - async feature is already enabled in Cargo.toml
cargo build --package read-table-async

# Without async mode (fails with compile_error)
# This would require manually removing the async feature from Cargo.toml
```

## Running

```bash
cargo run --package read-table-async -- <path-to-delta-table>
```

## Key Differences from `read-table`

### `read-table` (mode-agnostic):
```rust
let snapshot = await_!(Snapshot::builder_for(url).build(&engine))?;
let scan_data = await_!(scan.execute(Arc::new(engine)))?;
let batches: Vec<_> = await_!(scan_data.async_pin().async_try_collect())?;
```

### `read-table-async` (async-only):
```rust
let snapshot = Snapshot::builder_for(url).build(&engine).await?;
let mut scan_stream = scan.execute(Arc::new(engine)).await?;
while let Some(scan_result) = scan_stream.next().await {
    // Process natively with .await
}
```

