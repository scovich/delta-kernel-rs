//! Handler-shaped work items for storage and file-format reads.
//!
//! Raw file reads materialize one [`FileSlice`] as [`bytes::Bytes`]. JSON, Parquet, and plan
//! execution return `Vec<Box<dyn EngineData>>` pages. Paginated exhaustion is signalled by absence
//! of a continuation state.

use bytes::Bytes;

use super::{Operation as CoroutineOperation, PaginatedOperation};
use crate::engine_data::EngineData;
#[cfg(feature = "declarative-plans")]
use crate::plans::Operation as PlanOperation;
use crate::schema::SchemaRef;
use crate::{FileMeta, FileSlice, PredicateRef};

/// Read one complete file slice into memory.
///
/// Intended for reading small metadata files smaller than perhaps 10MB. Connectors may reject reads
/// exceeding their configured resource limits.
pub struct SmallFileRead(
    /// File URL and optional byte range to read.
    pub FileSlice,
);

impl CoroutineOperation for SmallFileRead {
    type Response = Bytes;
}

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

/// Paginated JSON file-read operation.
pub struct ReadJsonFiles(
    /// JSON read parameters.
    pub ReadFileFormatStart,
);

impl CoroutineOperation for ReadJsonFiles {
    type Response = Vec<Box<dyn EngineData>>;
}

impl PaginatedOperation for ReadJsonFiles {}

/// Paginated Parquet file-read operation.
pub struct ReadParquetFiles(
    /// Parquet read parameters.
    pub ReadFileFormatStart,
);

impl CoroutineOperation for ReadParquetFiles {
    type Response = Vec<Box<dyn EngineData>>;
}

impl PaginatedOperation for ReadParquetFiles {}

/// Operation supplied when starting paginated declarative-plan execution.
#[cfg(feature = "declarative-plans")]
pub struct ExecutePlan(
    /// Declarative plan operation to execute.
    pub PlanOperation,
);

#[cfg(feature = "declarative-plans")]
impl CoroutineOperation for ExecutePlan {
    type Response = Vec<Box<dyn EngineData>>;
}

#[cfg(feature = "declarative-plans")]
impl PaginatedOperation for ExecutePlan {}
