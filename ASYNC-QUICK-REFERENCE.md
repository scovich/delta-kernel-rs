# Async Macro Approach: Quick Reference

**Last Updated**: October 20, 2025  
**Full Details**: See [async-macro-approach.md](async-macro-approach.md)

---

## The Question

Can we eliminate sync/async code duplication using proc macros?

**Answer**: **YES** - Zero logic duplication with simple infrastructure.

---

## Consumer Impact

| Consumer | Change Required? |
|----------|------------------|
| **FFI (C/C++)** | ✅ No - always uses sync mode |
| **Examples (default)** | ✅ No - sync mode is default |
| **Examples (opt-in async)** | ⚠️ Minor - add `#[tokio::main]`, `.await` |
| **Library users (sync)** | ✅ No - sync mode is default |
| **Library users (async)** | ⚠️ Medium - enable feature, add `async`/`.await` |

---

## Code Changes

### Stay Sync (No Changes)

```rust
fn main() {
    let snapshot = Snapshot::builder_for(url).build(&engine)?;
    for item in scan.execute(engine)? {  // Iterator
        println!("{:?}", item);
    }
}
```

### Go Async (Opt-in)

```toml
[dependencies]
delta-kernel = { version = "0.x", features = ["async"] }
```

```rust
#[tokio::main]  // ← Add this
async fn main() {
    let snapshot = Snapshot::builder_for(url).build(&engine).await?;  // ← needs await
    let mut stream = scan.execute(engine).await?;  // ← Stream, not Iterator
    while let Some(item) = stream.next().await {   // ← async iteration
        println!("{:?}", item);
    }
}
```

---

## Decision Tree

```
Using C/C++ FFI? ──YES──> Use sync mode
        │
        NO
        │
        ▼
Already have tokio runtime? ──NO──> Use sync mode (simpler)
        │
        YES
        │
        ▼
Need max I/O concurrency? ──NO──> Use sync mode (good enough)
        │
        YES
        │
        ▼
Use async mode!
```

---

## Key Gotchas

### ⚠️ Can't Mix Modes
```rust
// This doesn't work - pick one mode per application
[dependencies]
delta-kernel = { features = ["async"] }  // ← async
some-crate-using-sync-kernel = { ... }  // ← conflict!
```

### ⚠️ Runtime Required for Async
```rust
// Need this:
#[tokio::main]
async fn main() { ... }

// Not just this:
async fn main() { ... }  // Won't work!
```

### ⚠️ Iterator vs Stream APIs Differ
```rust
// Sync mode
scan.execute()?.filter(|x| x.is_valid())

// Async mode
scan.execute().await?.filter(|x| async { x.is_valid() })
```

---

## When to Use Each Mode

### Use Sync Mode ✅
- Simple CLI tools
- FFI consumers (C/C++)
- No existing async runtime
- Prioritize simplicity

### Use Async Mode ✅
- Already using tokio/async-std
- Building web server/API
- Need high I/O concurrency
- Composing with async ecosystem

---

## Quick Links

| Document | Purpose |
|----------|---------|
| [async-macro-approach.md](async-macro-approach.md) | Complete technical proposal |
| [ASYNC-CONSUMER-SUMMARY.md](ASYNC-CONSUMER-SUMMARY.md) | Executive summary |
| [async-architecture-diagrams.md](async-architecture-diagrams.md) | Visual guide |
| [ASYNC-ANALYSIS-INDEX.md](ASYNC-ANALYSIS-INDEX.md) | Navigation hub |

---

## Bottom Line

### For End Users
- ✅ Backward compatible (nothing breaks)
- ✅ Opt-in async (your choice)
- ⚠️ Need to choose one mode per app

### For Kernel Team
- ✅ Zero logic duplication achieved
- ✅ Simple infrastructure enables unification
- ⚠️ Dual-mode testing required

### Recommendation
**👍 PROCEED WITH PROTOTYPE**

Benefits outweigh costs. Strong backward compatibility story makes this low-risk.
