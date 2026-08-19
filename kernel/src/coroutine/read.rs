//! Handler-shaped work items for storage reads.
//!
//! Each request resumes with `Option<Vec<bytes::Bytes>>`: `Some` carries a connector-sized page
//! and `None` marks the active read exhausted.

use bytes::Bytes;

use super::Resume;
use crate::FileSlice;

/// One page request within a raw file read.
#[derive(Debug, Clone)]
pub enum ReadFiles {
    /// Starts reading the supplied file slices in order.
    Start(Vec<FileSlice>),
    /// Continues the active read.
    Continue,
}

/// Continuation accepting one connector-sized raw file-read page, or `None` at exhaustion.
pub type ReadFilesResume<O, Q> = Resume<O, Q, Option<Vec<Bytes>>>;

/// Constructor for a workflow request variant that delegates a raw file read.
pub(crate) type ReadFilesConstructor<O, Q> = fn(ReadFiles, ReadFilesResume<O, Q>) -> Q;
