//! Kernel request vocabulary posted through a [`super::Channel`].
//!
//! Each variant pairs a connector operation with the reply that completes it. Paginated reads and
//! listings use [`PageRequest`]; JSON and Parquet writes carry a generator so the connector can
//! pull row batches while it owns the write.

use std::any::Any;
use std::sync::Arc;

use bytes::Bytes;
use derive_more::{Constructor, From};
use url::Url;

use crate::committer::PublishMetadata;
use crate::coroutine::generator::StaticGeneratorCoroutine;
use crate::coroutine::{PageRequest, PagedOperation, PlanOperation, Reply};
use crate::engine_data::EngineData;
use crate::expressions::{ExpressionRef, PredicateRef, Scalar};
use crate::path::may_begin_listable_log_path;
use crate::schema::{DataType, SchemaRef};
use crate::{
    DeltaResult, Error, FileMeta, FileSize, FileSlice, FilteredEngineData, ParquetFooter,
    ParquetMultiFileWrite, Version,
};

/// Operations kernel can request from a connector.
#[derive(From)]
pub enum Request {
    /// List every object beneath one directory URL in ascending order.
    ListDirectory(PageRequest<DirectoryListing>),
    /// List a bounded path range in ascending order.
    ListForward(PageRequest<ForwardListing>),
    /// List page ranges from high to low, with entries ascending within each page.
    ListBackward(PageRequest<BackwardListing>),
    /// Read a whole file when the range is `None`, or exactly the half-open range otherwise.
    ReadSmallFile(FileSlice, Reply<Bytes>),
    /// Read one Parquet footer.
    ReadParquetFooter(FileMeta, Reply<ParquetFooter>),
    /// Read JSON files as ordered [`EngineData`] batches: Each file may produce multiple
    /// batches, but batches may not span multiple files.
    ReadJson(PageRequest<ReadJsonFiles>),
    /// Read Parquet files as ordered [`EngineData`] batches: Each file may produce multiple
    /// batches, but batches may not span multiple files.
    ReadParquet(PageRequest<ReadParquetFiles>),
    /// Execute a declarative plan operation.
    ExecutePlan(PlanRequest),
    /// Materialize scalar rows as one [`EngineData`] batch.
    CreateEngineData(CreateEngineData, Reply<Box<dyn EngineData>>),
    /// Prepare an expression evaluator for repeated use.
    CreateExpressionEvaluator(CreateExpressionEvaluator, Reply<EvaluatorHandle>),
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
    WriteParquet(Box<WriteParquetFile>, Reply<()>),
    /// Write streamed row batches to connector-selected Parquet files.
    // Boxed to keep the request enum small without boxing every variant.
    WriteParquetFiles(Box<WriteParquetFiles>, Reply<WriteParquetFilesResult>),
    /// Write one complete object.
    WriteBytes(WriteBytes, Reply<()>),
    /// Atomically copy one immutable object to a new destination.
    CopyAtomic(CopyAtomic, Reply<()>),
    /// Publish catalog commits through the connector's catalog committer.
    Publish(PublishMetadata, Reply<()>),
}

/// Declarative plan operations with result-specific reply types.
pub enum PlanRequest {
    /// Execute a query plan in connector-selected pages.
    Query(PageRequest<PlanOperation>),
    /// Read a Parquet footer through the plan-execution path.
    ParquetFooter(FileMeta, Reply<ParquetFooter>),
}

impl From<PageRequest<PlanOperation>> for Request {
    fn from(request: PageRequest<PlanOperation>) -> Self {
        Self::ExecutePlan(PlanRequest::Query(request))
    }
}

// === Listing ===

/// An ascending listing of every object beneath one directory URL.
pub struct DirectoryListing {
    prefix: Url,
}

impl DirectoryListing {
    /// Create a listing rooted at `prefix`.
    ///
    /// # Errors
    ///
    /// Returns an error unless `prefix` ends in `/`, which prevents sibling paths with the same
    /// textual prefix from entering the listing.
    pub fn try_new(prefix: Url) -> DeltaResult<Self> {
        if !prefix.path().ends_with('/') {
            return Err(Error::generic(format!(
                "directory listing prefix must end in '/': {prefix}"
            )));
        }
        Ok(Self { prefix })
    }

    /// Return the listed directory URL.
    pub fn prefix(&self) -> &Url {
        &self.prefix
    }

    /// Return whether `entry` remains within this directory.
    ///
    /// Errors remain within bounds so the listing consumer can observe them.
    pub fn contains(&self, entry: &DeltaResult<FileMeta>) -> bool {
        let Ok(entry) = entry else {
            return true;
        };
        entry.location.as_str().starts_with(self.prefix.as_str())
    }
}

impl PagedOperation for DirectoryListing {
    type Page = Vec<DeltaResult<FileMeta>>;
}

/// Selects descendants of `prefix` whose full URLs are in the exclusive range `(low, high)`.
///
/// Comparisons use UTF-8 byte order. A bare-version bound sorts before files carrying that version
/// prefix.
pub struct ListingBounds {
    /// Common URL prefix for entries in this listing.
    pub prefix: Url,
    /// Exclusive lower URL bound.
    pub low: Url,
    /// Exclusive upper URL bound.
    pub high: Url,
}

impl ListingBounds {
    /// Return whether `entry` remains within these bounds.
    ///
    /// Errors remain within bounds so the listing consumer can observe them.
    pub fn contains(&self, entry: &DeltaResult<FileMeta>) -> bool {
        is_within_listing_bounds(entry, &self.prefix, &self.high)
    }

    /// Parse the upper bound's final path segment as a Delta version.
    ///
    /// Returns an error if the URL has no final segment or that segment is not a version.
    pub fn high_version(&self) -> DeltaResult<Version> {
        version_from_listing_bound(&self.high)
    }

    /// Select the next descending version window within these bounds.
    ///
    /// `high` is the current exclusive upper version. `window_size` controls how many versions the
    /// returned window spans.
    ///
    /// Returns an error if a bound does not end in a version or `window_size` is zero.
    pub fn backward_window(
        &self,
        high: Version,
        window_size: Version,
    ) -> DeltaResult<BackwardListingWindow> {
        if window_size == 0 {
            return Err(Error::generic(
                "backward listing window size must be greater than zero",
            ));
        }
        let start = version_from_listing_bound(&self.low)?;
        let lower = high.saturating_sub(window_size).max(start);
        Ok(BackwardListingWindow {
            prefix: self.prefix.clone(),
            low: bare_version_path(&self.prefix, lower)?,
            high: bare_version_path(&self.prefix, high)?,
            next_high: (lower > start).then_some(lower),
        })
    }
}

/// A bounded listing whose pages and entries are in ascending lexicographic order.
pub struct ForwardListing(pub Box<ListingBounds>);

impl ForwardListing {
    /// Create a forward listing over `bounds`.
    pub fn new(bounds: ListingBounds) -> Self {
        Self(Box::new(bounds))
    }

    /// Default number of entries in one listing page.
    pub const DEFAULT_PAGE_SIZE: usize = 1024;
}

impl PagedOperation for ForwardListing {
    type Page = Vec<DeltaResult<FileMeta>>;
}

/// A bounded listing whose page ranges move high to low, with entries ascending within each page.
pub struct BackwardListing(pub Box<ListingBounds>);

impl BackwardListing {
    /// Create a backward listing over `bounds`.
    pub fn new(bounds: ListingBounds) -> Self {
        Self(Box::new(bounds))
    }

    /// Default number of Delta versions covered by one listing request.
    pub const DEFAULT_WINDOW_SIZE: Version = 1000;
}

impl PagedOperation for BackwardListing {
    type Page = BackwardListingResult;
}

/// One backward-listing page with entries in ascending lexicographic order.
pub struct BackwardListingResult {
    /// Entries in this page.
    pub entries: Vec<DeltaResult<FileMeta>>,
    /// True if all file version numbers in all future pages will be strictly lower than the lowest
    /// file version seen so far.
    ///
    /// False is conservative and always valid.
    pub known_version_boundary: bool,
}

/// URL bounds and continuation state for one backward-listing window.
pub struct BackwardListingWindow {
    /// Common URL prefix for entries in this window.
    pub prefix: Url,
    /// Inclusive lower bound for this window.
    pub low: Url,
    /// Exclusive upper bound for this window.
    pub high: Url,
    /// Upper version for the next lower window, or `None` when this window reaches the range
    /// start.
    pub next_high: Option<Version>,
}

impl BackwardListingWindow {
    /// Return whether `entry` remains within this window.
    ///
    /// Errors remain within bounds so the listing consumer can observe them.
    pub fn contains(&self, entry: &DeltaResult<FileMeta>) -> bool {
        is_within_listing_bounds(entry, &self.prefix, &self.high)
    }
}

fn is_within_listing_bounds(entry: &DeltaResult<FileMeta>, prefix: &Url, high: &Url) -> bool {
    let Ok(entry) = entry else {
        return true;
    };
    let path = entry.location.as_str();
    path < high.as_str()
        && path
            .strip_prefix(prefix.as_str())
            .is_none_or(may_begin_listable_log_path)
}

fn version_from_listing_bound(bound: &Url) -> DeltaResult<Version> {
    bound
        .path_segments()
        .and_then(|mut segments| segments.next_back())
        .ok_or_else(|| Error::internal_error("listing bound has no path segment"))?
        .parse()
        .map_err(|_| Error::internal_error("listing bound is not a version"))
}

/// Map an inclusive version range to exclusive bare-version URL bounds.
pub(crate) fn log_listing_bounds(
    log_root: &Url,
    start_version: Version,
    end_version: Version,
) -> DeltaResult<ListingBounds> {
    Ok(ListingBounds {
        prefix: log_root.clone(),
        low: bare_version_path(log_root, start_version)?,
        high: bare_version_path(log_root, end_version.saturating_add(1))?,
    })
}

fn bare_version_path(log_root: &Url, version: Version) -> DeltaResult<Url> {
    Ok(log_root.join(&format!("{version:020}"))?)
}

// === Reads ===

/// Paginated JSON read operation.
pub struct ReadJsonFiles {
    /// Files to read in order; batches preserve file and row order and never span files.
    pub files: Vec<FileMeta>,
    /// Exact schema required for every returned batch, including field order and physical names.
    pub physical_schema: SchemaRef,
    /// Optional conservative push-down. Connectors may ignore it; if applied, omit data only when
    /// it cannot be true. Returned data need not satisfy it.
    pub predicate: Option<PredicateRef>,
}

impl ReadJsonFiles {
    pub fn new(
        files: Vec<FileMeta>,
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
    ) -> Self {
        Self {
            files,
            physical_schema,
            predicate,
        }
    }
}

impl PagedOperation for ReadJsonFiles {
    type Page = Vec<Box<dyn EngineData>>;
}

/// Paginated Parquet read operation.
pub struct ReadParquetFiles {
    /// Files to read in order; batches preserve file and row order and never span files.
    pub files: Vec<FileMeta>,
    /// Exact schema required for every returned batch, including field order and physical names.
    pub physical_schema: SchemaRef,
    /// Optional conservative push-down. Connectors may ignore it; if applied, omit data only when
    /// it cannot be true. Returned data need not satisfy it.
    pub predicate: Option<PredicateRef>,
}

impl ReadParquetFiles {
    pub fn new(
        files: Vec<FileMeta>,
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
    ) -> Self {
        Self {
            files,
            physical_schema,
            predicate,
        }
    }
}

impl PagedOperation for ReadParquetFiles {
    type Page = Vec<Box<dyn EngineData>>;
}

/// Paginated declarative-plan execution.
impl PagedOperation for PlanOperation {
    type Page = Vec<Box<dyn EngineData>>;
}

// === Evaluation ===

/// Request-owned expression or predicate evaluation input.
#[derive(From)]
pub enum EvaluationInput {
    /// Uniquely owned input.
    Boxed(Box<dyn EngineData>),
    /// Shared input.
    Shared(Arc<dyn EngineData>),
}

impl AsRef<dyn EngineData> for EvaluationInput {
    fn as_ref(&self) -> &(dyn EngineData + 'static) {
        match self {
            Self::Boxed(input) => input.as_ref(),
            Self::Shared(input) => input.as_ref(),
        }
    }
}

/// Values to materialize as one [`EngineData`] batch.
#[derive(Constructor)]
pub struct CreateEngineData {
    /// Schema describing each row.
    pub schema: SchemaRef,
    /// Owned rows with one scalar per top-level schema field.
    pub rows: Vec<Vec<Scalar>>,
}

/// Type-erased, shared connector state for a prepared expression evaluator.
pub type EvaluatorHandle = Arc<dyn Any + Send + Sync>;

/// Type-erased, shared connector state for a prepared predicate evaluator.
pub type PredicateEvaluatorHandle = Arc<dyn Any + Send + Sync>;

/// Parameters for preparing an expression evaluator.
#[derive(Constructor)]
pub struct CreateExpressionEvaluator {
    /// Schema of each input batch.
    pub input_schema: SchemaRef,
    /// Expression to evaluate.
    pub expression: ExpressionRef,
    /// Type of each evaluation result.
    pub output_type: DataType,
}

/// Parameters for preparing a predicate evaluator.
#[derive(Constructor)]
pub struct CreatePredicateEvaluator {
    /// Schema of each input batch.
    pub input_schema: SchemaRef,
    /// Predicate to evaluate.
    pub predicate: PredicateRef,
}

/// A prepared evaluator and input batch to evaluate.
#[derive(Constructor)]
pub struct EvaluateExpression {
    /// Connector-provided prepared evaluator.
    pub evaluator: EvaluatorHandle,
    /// Input batch.
    pub input: EvaluationInput,
}

/// A prepared predicate evaluator and input batch to evaluate.
#[derive(Constructor)]
pub struct EvaluatePredicate {
    /// Connector-provided prepared evaluator.
    pub evaluator: PredicateEvaluatorHandle,
    /// Input batch.
    pub input: EvaluationInput,
}

// === Writes ===

/// Opaque kernel state returned after a multi-file Parquet input is exhausted.
pub type WriteParquetFilesState = Box<dyn Any + Send>;

/// Files written from one input stream and its terminal state.
///
/// Connectors must return the state received from
/// [`super::GeneratorCoroutineStep::Done`](super::GeneratorCoroutineStep::Done) unchanged.
pub type WriteParquetFilesResult = (Vec<Url>, WriteParquetFilesState);

/// Controls how a file sink handles an existing destination.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileWriteMode {
    /// Fail if the destination already exists.
    CreateNew,
    /// Replace an existing destination.
    Overwrite,
}

impl FileWriteMode {
    pub(crate) fn overwrite(self) -> bool {
        self == Self::Overwrite
    }
}

/// Writes newline-delimited JSON rows to one file.
#[derive(Constructor)]
pub struct WriteJsonFile {
    /// Destination URL.
    pub url: Url,
    /// Existing-destination behavior.
    pub mode: FileWriteMode,
    /// Row batches to serialize in generator order.
    pub input: StaticGeneratorCoroutine<Request, FilteredEngineData>,
}

/// Writes row batches to one Parquet file.
#[derive(Constructor)]
pub struct WriteParquetFile {
    /// Destination URL.
    pub url: Url,
    /// Row batches to serialize in generator order.
    pub input: StaticGeneratorCoroutine<Request, Box<dyn EngineData>>,
}

/// Writes one row stream to connector-selected Parquet files.
#[derive(Constructor)]
pub struct WriteParquetFiles {
    /// Connector-owned file placement and sizing policy.
    pub write: ParquetMultiFileWrite,
    /// Row batches to serialize in generator order and their opaque terminal state.
    pub input: StaticGeneratorCoroutine<Request, Box<dyn EngineData>, WriteParquetFilesState>,
}

/// Write the bytes of one complete storage object to the specified destination URL.
#[derive(Constructor)]
pub struct WriteBytes {
    /// Destination URL.
    pub url: Url,
    /// Complete object contents.
    pub data: Bytes,
    /// Whether to replace an existing destination.
    ///
    /// If false, an existing destination must produce [`crate::Error::FileAlreadyExists`].
    pub overwrite: bool,
}

/// Atomically copy one immutable object to a destination that must not already exist.
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
    use crate::Error;

    fn bounds(start: Version, end: Version) -> ListingBounds {
        log_listing_bounds(&Url::parse("memory:///_delta_log/").unwrap(), start, end).unwrap()
    }

    #[test]
    fn backward_windows_descend_without_overlapping_versions() {
        let bounds = bounds(10, 3010);
        let first = bounds
            .backward_window(
                bounds.high_version().unwrap(),
                BackwardListing::DEFAULT_WINDOW_SIZE,
            )
            .unwrap();
        assert_eq!(first.low.path(), "/_delta_log/00000000000000002011");
        assert_eq!(first.high.path(), "/_delta_log/00000000000000003011");
        assert_eq!(first.next_high, Some(2011));

        let last = bounds
            .backward_window(
                first.next_high.unwrap(),
                BackwardListing::DEFAULT_WINDOW_SIZE * 3,
            )
            .unwrap();
        assert_eq!(last.low.path(), "/_delta_log/00000000000000000010");
        assert_eq!(last.next_high, None);
    }

    #[test]
    fn listing_bounds_keep_errors_and_stop_after_version_named_paths() {
        let bounds = bounds(0, 10);
        let error = Err(Error::generic("listing failed"));
        assert!(bounds.contains(&error));

        let sidecar = Ok(FileMeta::new(
            Url::parse("memory:///_delta_log/_sidecars/part.parquet").unwrap(),
            0,
            0,
        ));
        assert!(!bounds.contains(&sidecar));
    }

    #[test]
    fn directory_listing_requires_directory_url() {
        let prefix = Url::parse("memory:///_delta_log").unwrap();
        assert!(DirectoryListing::try_new(prefix).is_err());
    }

    #[test]
    fn directory_listing_excludes_sibling_prefixes() {
        let listing =
            DirectoryListing::try_new(Url::parse("memory:///_delta_log/").unwrap()).unwrap();
        let child = Ok(FileMeta::new(
            Url::parse("memory:///_delta_log/00000000000000000000.json").unwrap(),
            0,
            0,
        ));
        let sibling = Ok(FileMeta::new(
            Url::parse("memory:///_delta_log2/00000000000000000000.json").unwrap(),
            0,
            0,
        ));
        assert!(listing.contains(&child));
        assert!(!listing.contains(&sibling));
    }
}
