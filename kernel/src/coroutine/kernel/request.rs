//! Kernel's connector [`Request`]s and their operation payloads.
//!
//! Variants are added in lockstep with connector support and the Kernel operations that issue them.

use bytes::Bytes;

use crate::coroutine::Reply;
use crate::FileSlice;

/// Operations Kernel can request from a connector.
pub enum Request {
    /// Read a whole file when the range is `None`, or exactly the half-open range otherwise.
    ///
    /// Missing-object failures remain connector errors. `Ok(None)` means the connector returned no
    /// result. Extra objects from a single-file read are an error.
    ReadSmallFile(FileSlice, Reply<Option<Bytes>>),
}
