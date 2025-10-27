# read-table-sync

A truly synchronous example demonstrating pure sync usage of the delta kernel without any async/await machinery.

## Purpose

This example demonstrates:
- Pure synchronous API usage without `#[async_fn]` macros
- No async runtime required (no tokio)
- Direct use of standard Rust iterators
- Sync-only compilation (fails with `--features async`)

## When to Use This Pattern

Use this pattern when:
- Your application has no async runtime
- You want zero async overhead
- You're integrating with purely synchronous systems
- You need to understand the "native" sync API

## Comparison with Other Examples

- **read-table**: Natively async example that works in both sync/async kernel modes using `await_!`
- **read-table-sync**: This example - pure sync, no async machinery at all
- **read-table-multi-threaded**: Advanced sync example showing custom thread pool parallelism

## Building

```bash
# Sync mode (works)
cargo build --package read-table-sync

# Async mode (fails with compile_error)
cargo build --package read-table-sync --features async
```

## Running

```bash
cargo run --package read-table-sync -- <path-to-delta-table>
```

