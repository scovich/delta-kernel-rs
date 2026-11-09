//! Kernel's connector [`Request`]s and their operation payloads.
//!
//! Variants are added in lockstep with connector support and the Kernel operations that issue them.

use std::any::Any;
use std::sync::Arc;

use bytes::Bytes;
use derive_more::{Constructor, From};
use url::Url;

use super::StaticCoroutine;
use crate::coroutine::{PageRequest, PagedOperation, Reply};
use crate::engine_data::FilteredEngineData;
use crate::expressions::{ExpressionRef, PredicateRef};
#[cfg(feature = "declarative-plans")]
pub use crate::plans::Operation as PlanOperation;
use crate::schema::{DataType, SchemaRef};
use crate::{
    EngineData, FileMeta, FileSize, FileSlice, KernelResult, ParquetFooter, ParquetMultiFileWrite,
    Scalar,
};

/// Operations Kernel can request from a connector.
#[derive(From)]
pub enum Request {
    /// List a bounded path range in ascending order.
    ListForward(PageRequest<ForwardListing>),
    /// Read a whole file when the range is `None`, or exactly the half-open range otherwise.
    ///
    /// Missing-object failures remain connector errors. `Ok(None)` means the connector returned no
    /// result. Extra objects from a single-file read are an error.
    ReadSmallFile(FileSlice, Reply<Option<Bytes>>),
    /// Fetch metadata for one object.
    Head(Url, Reply<FileMeta>),
    /// Read one Parquet footer.
    ReadParquetFooter(FileMeta, Reply<ParquetFooter>),
    /// Read JSON files as ordered [`EngineData`] batches. Each file may produce multiple batches,
    /// but batches may not span files.
    ReadJson(PageRequest<ReadJsonFiles>),
    /// Read Parquet files as ordered [`EngineData`] batches. Each file may produce multiple
    /// batches, but batches may not span files.
    ReadParquet(PageRequest<ReadParquetFiles>),
    /// Execute a declarative plan operation.
    ExecutePlan(PlanRequest),
    /// Materialize scalar rows as one [`EngineData`] batch.
    CreateEngineData(CreateEngineData, Reply<Box<dyn EngineData>>),
    /// Prepare an expression evaluator for repeated use.
    CreateExpressionEvaluator(CreateExpressionEvaluator, Reply<ExpressionEvaluatorHandle>),
    /// Prepare a predicate evaluator for repeated use.
    CreatePredicateEvaluator(CreatePredicateEvaluator, Reply<PredicateEvaluatorHandle>),
    /// Evaluate a prepared expression against one [`EngineData`] batch.
    EvaluateExpression(EvaluateExpression, Reply<Box<dyn EngineData>>),
    /// Evaluate a prepared predicate against one [`EngineData`] batch.
    EvaluatePredicate(EvaluatePredicate, Reply<Box<dyn EngineData>>),
    /// Write one newline-delimited JSON file from streamed row batches.
    WriteJson(Box<WriteJsonFile>, Reply<FileSize>),
    /// Write one Parquet file from streamed row batches.
    WriteParquet(Box<WriteParquetFile>, Reply<FileSize>),
    /// Write streamed row batches to connector-selected Parquet files.
    WriteParquetFiles(Box<WriteParquetFiles>, Reply<WriteParquetFilesResult>),
    /// Write one complete object.
    WriteBytes(WriteBytes, Reply<()>),
}

/// Declarative plan operations with result-specific reply types.
pub enum PlanRequest {
    /// Reports whether the connector executes declarative plans. Every connector must reply.
    Available(Reply<bool>),
    /// Executes a query plan in connector-selected pages.
    Query(PageRequest<PlanOperation>),
    /// Reads a Parquet footer through the plan-execution path.
    ParquetFooter(FileMeta, Reply<ParquetFooter>),
}

impl From<PageRequest<PlanOperation>> for Request {
    fn from(request: PageRequest<PlanOperation>) -> Self {
        Self::ExecutePlan(PlanRequest::Query(request))
    }
}

impl PagedOperation for PlanOperation {
    type Page = Vec<KernelResult<Box<dyn EngineData>>>;
}

/// Uninhabited plan operation used when declarative plans are disabled.
#[cfg(not(feature = "declarative-plans"))]
pub enum PlanOperation {}

/// URL bounds for a forward listing.
pub struct ListingBounds {
    /// Exclusive lower URL bound.
    pub low: Url,
    /// Exclusive upper URL bound.
    pub high: Url,
}

/// A bounded listing whose pages and entries are in ascending lexicographic order.
pub struct ForwardListing(pub Box<ListingBounds>);

impl ForwardListing {
    /// Creates a forward listing over `bounds`.
    pub fn new(bounds: ListingBounds) -> Self {
        Self(Box::new(bounds))
    }

    /// Default number of entries in one listing page.
    pub const DEFAULT_PAGE_SIZE: usize = 1000;
}

impl PagedOperation for ForwardListing {
    type Page = Vec<KernelResult<FileMeta>>;
}

/// Paginated JSON file read.
pub struct ReadJsonFiles {
    /// Files to read in order.
    pub files: Vec<FileMeta>,
    /// Exact physical schema required for every returned batch.
    pub physical_schema: SchemaRef,
    /// Optional conservative push-down. Connectors may ignore it; if applied, omit data only when
    /// it cannot be true. Returned data need not satisfy it.
    pub predicate: Option<PredicateRef>,
}

impl ReadJsonFiles {
    /// Creates a JSON read operation.
    pub fn new(
        files: impl Into<Vec<FileMeta>>,
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
    ) -> Self {
        Self {
            files: files.into(),
            physical_schema,
            predicate,
        }
    }
}

impl PagedOperation for ReadJsonFiles {
    type Page = Vec<KernelResult<Box<dyn EngineData>>>;
}

/// Paginated Parquet file read.
pub struct ReadParquetFiles {
    /// Files to read in order.
    pub files: Vec<FileMeta>,
    /// Exact physical schema required for every returned batch.
    pub physical_schema: SchemaRef,
    /// Optional conservative push-down. Connectors may ignore it; if applied, omit data only when
    /// it cannot be true. Returned data need not satisfy it.
    pub predicate: Option<PredicateRef>,
}

impl ReadParquetFiles {
    /// Creates a Parquet read operation.
    pub fn new(
        files: impl Into<Vec<FileMeta>>,
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
    ) -> Self {
        Self {
            files: files.into(),
            physical_schema,
            predicate,
        }
    }
}

impl PagedOperation for ReadParquetFiles {
    type Page = Vec<KernelResult<Box<dyn EngineData>>>;
}

// === Evaluation ===

/// Values to materialize as one [`EngineData`] batch.
#[derive(Constructor)]
pub struct CreateEngineData {
    /// Schema describing each row.
    pub schema: SchemaRef,
    /// Owned rows with one scalar per top-level schema field.
    pub rows: Vec<Vec<Scalar>>,
}

/// Type-erased, shared connector state for a prepared expression evaluator.
pub type ExpressionEvaluatorHandle = Arc<dyn Any + Send + Sync>;

/// Type-erased, shared connector state for a prepared predicate evaluator.
pub type PredicateEvaluatorHandle = Arc<dyn Any + Send + Sync>;

/// Parameters for preparing an expression evaluator.
#[derive(Constructor)]
pub struct CreateExpressionEvaluator {
    pub input_schema: SchemaRef,
    pub expression: ExpressionRef,
    pub output_type: DataType,
}

/// Parameters for preparing a predicate evaluator.
#[derive(Constructor)]
pub struct CreatePredicateEvaluator {
    pub input_schema: SchemaRef,
    pub predicate: PredicateRef,
}

/// A prepared expression evaluator and input batch to evaluate.
#[derive(Constructor)]
pub struct EvaluateExpression {
    pub evaluator: ExpressionEvaluatorHandle,
    pub input: Box<dyn EngineData>,
}

/// A prepared predicate evaluator and input batch to evaluate.
#[derive(Constructor)]
pub struct EvaluatePredicate {
    pub evaluator: PredicateEvaluatorHandle,
    pub input: Box<dyn EngineData>,
}

// === Writes ===

/// Files written from one input stream.
pub type WriteParquetFilesResult = Vec<(Url, FileSize)>;

/// Writes newline-delimited JSON rows to one file.
#[derive(Constructor)]
pub struct WriteJsonFile {
    /// Destination URL.
    pub url: Url,
    /// Row batches to serialize in yield order.
    pub input: StaticCoroutine<KernelResult<FilteredEngineData>>,
    /// Whether an existing destination may be overwritten.
    pub overwrite: bool,
}

/// Writes row batches to one Parquet file.
#[derive(Constructor)]
pub struct WriteParquetFile {
    /// Destination URL.
    pub url: Url,
    /// Row batches to serialize in yield order.
    pub input: StaticCoroutine<KernelResult<Box<dyn EngineData>>>,
}

/// Writes one row stream to connector-selected Parquet files.
#[derive(Constructor)]
pub struct WriteParquetFiles {
    /// Connector-owned file placement and sizing policy.
    pub write: ParquetMultiFileWrite,
    /// Row batches to serialize in yield order.
    pub input: StaticCoroutine<KernelResult<Box<dyn EngineData>>>,
}

/// Writes raw bytes as the entire contents of one object.
#[derive(Constructor)]
pub struct WriteBytes {
    /// Destination URL.
    pub url: Url,
    /// Complete object contents.
    pub data: Bytes,
    /// Whether an existing destination may be overwritten. If not, an existing destination fails
    /// with [`crate::KernelError::FileAlreadyExists`].
    pub overwrite: bool,
}
