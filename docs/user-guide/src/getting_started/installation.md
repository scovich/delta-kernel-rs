# Installation

The Delta Kernel ships as two crates on crates.io:

- [`delta_kernel`](https://crates.io/crates/delta_kernel): the core library. No I/O, no Arrow.
- [`delta_kernel_default_engine`](https://crates.io/crates/delta_kernel_default_engine): the default
  Arrow + Tokio execution crate, providing `DefaultEngine` and `AsyncEngineConnector`.

Cargo feature flags keep both crates dependency-light.

## Requirements

- **Rust edition**: 2021
- **Minimum Rust version**: 1.88

## Adding the dependency

For the common case (use the default engine), add both crates:

```toml
[dependencies]
delta_kernel = "0.23"
delta_kernel_default_engine = { version = "0.23", features = ["rustls"] }
```

That gives you Kernel plus a default engine that handles I/O and expression evaluation for you,
backed by Arrow with `rustls` for TLS.

If your connector drives workflow requests through its own I/O and evaluation code, depend on just
`delta_kernel`:

```toml
[dependencies]
delta_kernel = "0.23"
```

Enable `arrow-conversion` or `arrow-expression` only if your request handlers use Kernel's Arrow
interop helpers.

## Feature flags

You only pay for what you enable.

### `delta_kernel_default_engine` features

| Feature | Description |
|---------|-------------|
| `rustls` | Default engine with `rustls` for TLS. **Recommended for most users.** |
| `native-tls` | Default engine using your platform's native TLS (OpenSSL on Linux, Schannel on Windows, Secure Transport on macOS). Use this if `rustls` doesn't work in your environment. |
| `arrow` | Use the latest Arrow version Kernel supports. Currently maps to Arrow 59. |

You need exactly one of `rustls` or `native-tls`. See
[Building a connector](../connector/overview.md) for choosing an execution surface.

### Arrow version pinning

If you need a specific Arrow version (e.g. to match your existing Arrow dependency), pin it
explicitly on both crates:

| Feature | Arrow version |
|---------|---------------|
| `arrow-59` | Arrow 59 (current default) |
| `arrow-58` | Arrow 58 |

For more details on managing Arrow version compatibility, see
[Feature Flags](../concepts/feature_flags.md).

### `delta_kernel` features

| Feature | Description |
|---------|-------------|
| `arrow-conversion` | Convert between kernel types and Arrow types |
| `arrow-expression` | Evaluate kernel expressions over Arrow data |
| `internal-api` | Expose additional APIs that aren't yet stabilized. Some examples in this guide need this. |
| `schema-diff` | Experimental schema diffing |

The `arrow-conversion` and `arrow-expression` flags are pulled in automatically by
`delta_kernel_default_engine`, so set them directly only when connector request handlers use
Kernel's Arrow helpers without that crate.

## Example `Cargo.toml`

A typical project using Kernel:

```toml
[package]
name = "my-delta-reader"
version = "0.1.0"
edition = "2021"

[dependencies]
delta_kernel = "0.23"
delta_kernel_default_engine = { version = "0.23", features = ["rustls"] }

# Kernel re-exports arrow, but you can also depend on it directly:
# arrow = "59"
```

## What's next

With the dependencies added, head to [Quick Start: Reading a Table](./quick_start_read.md) to
read your first Delta table.
