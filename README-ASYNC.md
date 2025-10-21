# Async Macro Approach

This proposal adds optional async/await support to delta-kernel-rs using proc macros.

## Documentation

**Decision makers** (15 min): Read [ASYNC-CONSUMER-SUMMARY.md](ASYNC-CONSUMER-SUMMARY.md)
- Impact on consumers, migration examples, decision matrix

**Implementers** (35 min): Read [async-macro-approach.md](async-macro-approach.md)  
- Complete technical proposal, infrastructure components, feature flags

## Quick Summary

- **Default**: Sync mode (backward compatible, no changes needed)
- **Opt-in**: Async mode via `features = ["async", "default-engine-rustls"]`
- **FFI**: Always sync, no changes needed
- **Benefits**: Natural async integration, zero code duplication
- **Cost**: Dual-mode maintenance

## Key Points

### For Existing Code
No changes required. Sync mode is default and backward compatible.

### For Async Applications
Add the `async` feature flag and use `.await` with kernel calls:
```rust
#[tokio::main]
async fn main() {
    let snapshot = Snapshot::build(&engine).await?;
    let mut stream = scan.execute(engine).await?;
    while let Some(item) = stream.next().await {
        // process
    }
}
```

### For FFI Consumers
No changes. FFI always uses sync mode.

---

Start with the consumer summary, then dive into the technical proposal if needed.

