//! Kernel's connector [`Request`]s and their operation payloads.
//!
//! Variants are added in lockstep with connector support and the Kernel operations that issue them.

use std::any::Any;
use std::sync::Arc;

use bytes::Bytes;
use derive_more::{Constructor, From};
use url::Url;

use super::StaticCoroutine;
use crate::committer::{Commit, CommitResponse, PublishMetadata};
use crate::coroutine::{PageRequest, PagedOperation, Reply};
use crate::engine_data::FilteredEngineData;
use crate::expressions::{ExpressionRef, PredicateRef};
use crate::path::may_begin_listable_log_path;
#[cfg(feature = "declarative-plans")]
pub use crate::plans::Operation as PlanOperation;
use crate::schema::{DataType, SchemaRef};
use crate::{
    EngineData, FileMeta, FileSize, FileSlice, KernelError, KernelResult, ParquetFooter,
    ParquetMultiFileWrite, Scalar, Version,
};

/// Operations Kernel can request from a connector.
#[derive(From)]
pub enum Request {
    /// List every object beneath one directory URL in ascending order.
    ListDirectory(PageRequest<DirectoryListing>),
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
    // Boxed to keep the request enum small without boxing every variant.
    WriteJson(Box<WriteJsonFile>, Reply<FileSize>),
    /// Write one Parquet file from streamed row batches.
    // Boxed to keep the request enum small without boxing every variant.
    WriteParquet(Box<WriteParquetFile>, Reply<FileSize>),
    /// Write streamed row batches to connector-selected Parquet files.
    // Boxed to keep the request enum small without boxing every variant.
    WriteParquetFiles(Box<WriteParquetFiles>, Reply<WriteParquetFilesResult>),
    /// Write one complete object.
    WriteBytes(WriteBytes, Reply<()>),
    /// Copy one immutable object to a new destination. The connector must fail with
    /// [`KernelError::FileAlreadyExists`] if the destination already exists.
    CopyAtomic(CopyAtomic, Reply<()>),
    /// Commit one prepared transaction through a connector-selected committer.
    Commit(Box<Commit>, Reply<CommitResponse>),
    /// Verifies that the connector selected a catalog committer.
    RequireCatalogCommitter(Reply<()>),
    /// Publish catalog commits through a connector-selected committer.
    Publish(PublishMetadata, Reply<()>),
}

/// Declarative plan operations with result-specific reply types.
pub enum PlanRequest {
    /// Verifies that the connector provides a declarative plan executor.
    Require(Reply<()>),
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

/// An ascending listing of every object beneath one directory URL.
pub struct DirectoryListing {
    prefix: Url,
}

impl DirectoryListing {
    /// Creates a listing rooted at `prefix`.
    ///
    /// Returns an error unless `prefix` ends in `/`, preventing sibling paths with the same textual
    /// prefix from entering the listing.
    pub fn try_new(prefix: Url) -> KernelResult<Self> {
        if !prefix.path().ends_with('/') {
            return Err(KernelError::generic(format!(
                "directory listing prefix must end in '/': {prefix}"
            )));
        }
        Ok(Self { prefix })
    }

    /// Returns the listed directory URL.
    pub fn prefix(&self) -> &Url {
        &self.prefix
    }

    /// Returns whether `entry` remains within this directory.
    pub fn contains(&self, entry: &KernelResult<FileMeta>) -> bool {
        let Ok(entry) = entry else {
            return true;
        };
        entry.location.as_str().starts_with(self.prefix.as_str())
    }
}

impl PagedOperation for DirectoryListing {
    type Page = Vec<KernelResult<FileMeta>>;
}

/// URL bounds for a forward listing.
pub struct ListingBounds {
    /// Common directory prefix for listed entries.
    pub prefix: Url,
    /// Exclusive lower URL bound.
    pub low: Url,
    /// Exclusive upper URL bound.
    pub high: Url,
}

impl ListingBounds {
    /// Returns whether `entry` remains within these bounds.
    pub fn contains(&self, entry: &KernelResult<FileMeta>) -> bool {
        let Ok(entry) = entry else {
            return true;
        };
        let path = entry.location.as_str();
        path < self.high.as_str()
            && path
                .strip_prefix(self.prefix.as_str())
                .is_none_or(may_begin_listable_log_path)
    }
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

/// Maps an inclusive Delta version range to exclusive URL bounds.
pub(crate) fn log_listing_bounds(
    log_root: &Url,
    start_version: Version,
    end_version: Version,
) -> KernelResult<ListingBounds> {
    let high = if end_version == Version::MAX {
        log_root.join("_")?
    } else {
        bare_version_path(log_root, end_version + 1)?
    };
    Ok(ListingBounds {
        prefix: log_root.clone(),
        low: bare_version_path(log_root, start_version)?,
        high,
    })
}

fn bare_version_path(log_root: &Url, version: Version) -> KernelResult<Url> {
    Ok(log_root.join(&format!("{version:020}"))?)
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
    pub input: StaticCoroutine<FilteredEngineData>,
    /// Whether an existing destination may be overwritten.
    pub overwrite: bool,
}

/// Writes row batches to one Parquet file.
#[derive(Constructor)]
pub struct WriteParquetFile {
    /// Destination URL.
    pub url: Url,
    /// Row batches to serialize in yield order.
    pub input: StaticCoroutine<Box<dyn EngineData>>,
}

/// Writes one row stream to connector-selected Parquet files.
#[derive(Constructor)]
pub struct WriteParquetFiles {
    /// Connector-owned file placement and sizing policy.
    pub write: ParquetMultiFileWrite,
    /// Row batches to serialize in yield order.
    pub input: StaticCoroutine<Box<dyn EngineData>>,
}

/// Writes one complete object.
#[derive(Constructor)]
pub struct WriteBytes {
    /// Destination URL.
    pub url: Url,
    /// Complete object contents.
    pub data: Bytes,
    /// Whether an existing destination may be overwritten.
    pub overwrite: bool,
}

/// Atomically copies one immutable object to a destination that must not already exist.
#[derive(Constructor)]
pub struct CopyAtomic {
    /// Existing source object.
    pub source: Url,
    /// New destination object.
    pub destination: Url,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::KernelError;

    #[test]
    fn unbounded_log_listing_includes_max_version_and_excludes_special_directories() {
        let log_root = Url::parse("memory:///_delta_log/").unwrap();
        let bounds = log_listing_bounds(&log_root, 0, Version::MAX).unwrap();
        let entry = |path| Ok(FileMeta::new(log_root.join(path).unwrap(), 0, 0));

        assert!(bounds.contains(&entry("18446744073709551615.json")));
        assert!(!bounds.contains(&entry("_sidecars/part.parquet")));
        assert!(bounds.contains(&Err(KernelError::generic("listing failed"))));
    }
}
