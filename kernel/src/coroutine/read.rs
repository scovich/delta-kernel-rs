//! Handler-shaped work items for storage and file-format reads.
//!
//! Raw file reads resume with `Option<Vec<bytes::Bytes>>`. JSON and Parquet reads resume with
//! `Option<Box<dyn EngineData>>`. In every case `Some` is one connector-sized page and `None`
//! marks the active read exhausted.

use bytes::Bytes;

use super::{Pagination, PaginationResponse, Resume};
use crate::engine_data::EngineData;
use crate::schema::SchemaRef;
use crate::{FileMeta, FileSlice, PredicateRef};

/// One page request within a raw file read.
#[derive(Debug, Clone)]
pub enum ReadFiles {
    /// Starts reading the supplied file slices in order.
    Start(Vec<FileSlice>),
    /// Continues the active read.
    Continue,
}

/// Continuation accepting one connector-sized raw file-read page, or `None` at exhaustion.
pub type ReadFilesResume<O, Q, S> =
    Pagination<S, Resume<O, Q, PaginationResponse<Option<Vec<Bytes>>, S>>>;

/// Constructor for a workflow request variant that delegates a raw file read.
pub(crate) type ReadFilesConstructor<O, Q, S> = fn(ReadFiles, ReadFilesResume<O, Q, S>) -> Q;

/// Arguments that start a JSON or Parquet file read.
#[derive(Debug, Clone)]
pub struct ReadFileFormatStart {
    /// Files to read, in order.
    pub files: Vec<FileMeta>,
    /// Columns to read from each file.
    pub physical_schema: SchemaRef,
    /// Optional conservative push-down predicate. Connectors may ignore it.
    pub predicate: Option<PredicateRef>,
}

/// One page request within a JSON file read.
#[derive(Debug, Clone)]
pub enum ReadJsonFiles {
    /// Starts reading the supplied JSON files in order.
    Start(ReadFileFormatStart),
    /// Continues the active read.
    Continue,
}

/// Continuation accepting one JSON batch, or `None` at exhaustion.
pub type ReadJsonFilesResume<O, Q, S> =
    Pagination<S, Resume<O, Q, PaginationResponse<Option<Box<dyn EngineData>>, S>>>;

/// Constructor for a workflow request variant that delegates a JSON file read.
pub(crate) type ReadJsonFilesConstructor<O, Q, S> =
    fn(ReadJsonFiles, ReadJsonFilesResume<O, Q, S>) -> Q;

/// One page request within a Parquet file read.
#[derive(Debug, Clone)]
pub enum ReadParquetFiles {
    /// Starts reading the supplied Parquet files in order.
    Start(ReadFileFormatStart),
    /// Continues the active read.
    Continue,
}

/// Continuation accepting one Parquet batch, or `None` at exhaustion.
pub type ReadParquetFilesResume<O, Q, S> =
    Pagination<S, Resume<O, Q, PaginationResponse<Option<Box<dyn EngineData>>, S>>>;

/// Constructor for a workflow request variant that delegates a Parquet file read.
pub(crate) type ReadParquetFilesConstructor<O, Q, S> =
    fn(ReadParquetFiles, ReadParquetFilesResume<O, Q, S>) -> Q;

/// One page request within a declarative-plan execution.
///
/// `Start` asks the connector to execute [`Operation`]. Subsequent `Continue`s page data
/// batches from the resulting [`PlanResult::Data`](crate::plans::PlanResult::Data) stream.
#[cfg(feature = "declarative-plans")]
#[derive(Debug)]
pub enum ExecutePlan {
    /// Starts executing the supplied operation.
    Start(crate::plans::Operation),
    /// Continues paging the active plan's data stream.
    Continue,
}

/// Continuation accepting one plan-output batch, or `None` at exhaustion.
#[cfg(feature = "declarative-plans")]
pub type ExecutePlanResume<O, Q, S> =
    Pagination<S, Resume<O, Q, PaginationResponse<Option<Box<dyn EngineData>>, S>>>;

/// Constructor for a workflow request variant that delegates plan execution.
#[cfg(feature = "declarative-plans")]
pub(crate) type ExecutePlanConstructor<O, Q, S> = fn(ExecutePlan, ExecutePlanResume<O, Q, S>) -> Q;
