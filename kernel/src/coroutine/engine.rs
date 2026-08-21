//! Compatibility drivers for serving coroutine requests through an [`Engine`](crate::Engine).

use std::future::Future;
use std::sync::Arc;

use url::Url;

use super::listing::{
    bare_version_path, BackwardListing, BackwardListingResult, ForwardListing,
    ForwardListingResult, ListingBounds,
};
#[cfg(feature = "declarative-plans")]
use super::read::ExecutePlan;
use super::read::{ReadJsonFiles, ReadParquetFiles, SmallFileRead};
use super::write::WriteBytes;
use super::{coroutine_workflow, Channel, PaginatedResume, Pagination, Resume, TypedResume};
use crate::engine_data::EngineData;
use crate::path::may_begin_listable_log_path;
use crate::{
    DeltaResult, DeltaResultIteratorStatic, Engine, Error, FileDataReadResultIterator, FileMeta,
    JsonHandler, ParquetHandler, StorageHandler, Version,
};

#[cfg(test)]
const FORWARD_LISTING_PAGE_SIZE: usize = 2;
#[cfg(not(test))]
const FORWARD_LISTING_PAGE_SIZE: usize = 1024;
const BACKWARD_LISTING_WINDOW_SIZE: Version = 1000;

/// Legacy Engine iterator retained across forward listing pages.
pub(crate) type ListingIterator = DeltaResultIteratorStatic<FileMeta>;
/// Legacy Engine iterator retained across file-format and plan result pages.
pub(crate) type EngineDataIterator = FileDataReadResultIterator;

type EngineDataResume<W> = TypedResume<W, (Vec<Box<dyn EngineData>>, Option<EngineDataIterator>)>;

/// Legacy Engine state retained between backward listing requests.
pub(crate) struct BackwardListingState {
    bounds: Box<ListingBounds>,
    high: Version,
}

/// Owned compatibility adapter for serving coroutine requests through an [`Engine`].
pub(crate) struct EngineConnector {
    storage: Arc<dyn StorageHandler>,
}

#[coroutine_workflow]
pub(crate) enum ListingWorkflow<O: Send + 'static> {
    #[output]
    Done(O),
    #[paginated]
    ListForward(ForwardListing, ListingIterator),
    #[paginated]
    ListBackward(BackwardListing, BackwardListingState),
}

/// Drive an Engine-backed listing workflow to completion.
pub(crate) fn drive_listing<O, F, Fut>(storage: &dyn StorageHandler, workflow: F) -> DeltaResult<O>
where
    O: Send + 'static,
    F: FnOnce(Channel<ListingWorkflow<O>>) -> Fut + Send + 'static,
    Fut: Future<Output = DeltaResult<O>> + Send + 'static,
{
    super::drive_workflow!(super::start(workflow), |workflow| match workflow {
        ListingWorkflow::Done(output) => break output,
        ListingWorkflow::ListForward(pagination, resume) => {
            resume_forward_listing(storage, pagination, resume)
        }
        ListingWorkflow::ListBackward(pagination, resume) => {
            resume_backward_listing(storage, pagination, resume)
        }
    },)
}

impl EngineConnector {
    /// Create an adapter by cloning the engine's handlers.
    pub(crate) fn new(engine: &dyn Engine) -> Self {
        Self {
            storage: engine.storage_handler(),
        }
    }

    /// Execute one Engine-backed request and resume the kernel workflow.
    pub(crate) fn dispatch<O: Send + 'static>(
        &mut self,
        workflow: ListingWorkflow<O>,
    ) -> DeltaResult<ListingWorkflow<O>> {
        match workflow {
            ListingWorkflow::Done(_) => Err(Error::internal_error(
                "completed listing workflow cannot be dispatched",
            )),
            ListingWorkflow::ListForward(pagination, resume) => {
                resume_forward_listing(self.storage.as_ref(), pagination, resume)
            }
            ListingWorkflow::ListBackward(pagination, resume) => {
                resume_backward_listing(self.storage.as_ref(), pagination, resume)
            }
        }
    }
}

/// Serves one paginated forward listing request through a legacy [`StorageHandler`].
pub(crate) fn resume_forward_listing<W: Send + 'static>(
    storage: &dyn StorageHandler,
    pagination: Pagination<ForwardListing, ListingIterator>,
    resume: PaginatedResume<W, ForwardListing, ListingIterator>,
) -> DeltaResult<W> {
    resume.resume_with(|| {
        let listing = match pagination {
            Pagination::Start(ForwardListing(bounds)) => start_forward_listing(storage, bounds)?,
            Pagination::Continue(listing) => listing,
        };
        Ok(next_forward_listing(listing))
    })
}

/// Serves one paginated backward listing request through a legacy [`StorageHandler`].
pub(crate) fn resume_backward_listing<W: Send + 'static>(
    storage: &dyn StorageHandler,
    pagination: Pagination<BackwardListing, BackwardListingState>,
    resume: PaginatedResume<W, BackwardListing, BackwardListingState>,
) -> DeltaResult<W> {
    resume.resume_with(|| {
        let (bounds, high) = match pagination {
            Pagination::Start(BackwardListing(bounds)) => {
                let high = version_bound(&bounds.high)?;
                (Box::new(bounds), high)
            }
            Pagination::Continue(BackwardListingState { bounds, high }) => (bounds, high),
        };
        let start = version_bound(&bounds.low)?;
        let lower = high.saturating_sub(BACKWARD_LISTING_WINDOW_SIZE).max(start);
        let lower_url = bare_version_path(&bounds.prefix, lower)?;
        let upper_url = bare_version_path(&bounds.prefix, high)?;
        let prefix = bounds.prefix.as_str();
        let entries = storage
            .list_from(&lower_url)?
            .take_while(|entry| within_log_listing_bounds(entry, prefix, upper_url.as_str()))
            .collect();
        let result = BackwardListingResult {
            entries,
            known_version_boundary: true,
        };
        let state = (lower > start).then_some(BackwardListingState {
            bounds,
            high: lower,
        });
        Ok((result, state))
    })
}

/// Serve one complete file-slice read through a legacy [`StorageHandler`].
pub(crate) fn resume_read_file<W: Send + 'static>(
    storage: &dyn StorageHandler,
    operation: SmallFileRead,
    resume: Resume<W, SmallFileRead>,
) -> DeltaResult<W> {
    resume.resume_with(|| {
        let mut reads = storage.read_files(vec![operation.0])?;
        let data = reads
            .next()
            .transpose()?
            .ok_or_else(|| Error::internal_error("single-file read returned no result"))?;
        if reads.next().is_some() {
            return Err(Error::internal_error(
                "single-file read returned more than one result",
            ));
        }
        Ok(data)
    })
}

/// Serve one byte-write request through a legacy [`StorageHandler`].
pub(crate) fn resume_write_bytes<W: Send + 'static>(
    storage: &dyn StorageHandler,
    operation: WriteBytes,
    resume: Resume<W, WriteBytes>,
) -> DeltaResult<W> {
    resume.resume_with(|| storage.put(&operation.url, operation.data, operation.overwrite))
}

/// Serves one paginated JSON read through a legacy [`JsonHandler`].
pub(crate) fn resume_read_json_files<W: Send + 'static>(
    json: &dyn JsonHandler,
    pagination: Pagination<ReadJsonFiles, EngineDataIterator>,
    resume: EngineDataResume<W>,
) -> DeltaResult<W> {
    let reads = match pagination {
        Pagination::Start(ReadJsonFiles(start)) => {
            json.read_json_files(&start.files, start.physical_schema, start.predicate)
        }
        Pagination::Continue(reads) => Ok(reads),
    };
    resume_engine_data(reads, resume)
}

/// Serves one paginated Parquet read through a legacy [`ParquetHandler`].
pub(crate) fn resume_read_parquet_files<W: Send + 'static>(
    parquet: &dyn ParquetHandler,
    pagination: Pagination<ReadParquetFiles, EngineDataIterator>,
    resume: EngineDataResume<W>,
) -> DeltaResult<W> {
    let reads = match pagination {
        Pagination::Start(ReadParquetFiles(start)) => {
            parquet.read_parquet_files(&start.files, start.physical_schema, start.predicate)
        }
        Pagination::Continue(reads) => Ok(reads),
    };
    resume_engine_data(reads, resume)
}

/// Serves one paginated declarative-plan execution through a legacy [`Engine`].
#[cfg(feature = "declarative-plans")]
pub(crate) fn resume_plan<W: Send + 'static>(
    engine: &dyn Engine,
    pagination: Pagination<ExecutePlan, EngineDataIterator>,
    resume: EngineDataResume<W>,
) -> DeltaResult<W> {
    let reads = match pagination {
        Pagination::Start(ExecutePlan(operation)) => engine
            .plan_executor()
            .ok_or_else(|| Error::unsupported("this engine does not provide a PlanExecutor"))
            .and_then(|executor| executor.execute_op(operation)?.into_data()),
        Pagination::Continue(reads) => Ok(reads),
    };
    resume_engine_data(reads, resume)
}

fn within_log_listing_bounds(entry: &DeltaResult<FileMeta>, prefix: &str, high: &str) -> bool {
    let Ok(entry) = entry else {
        return true;
    };
    let path = entry.location.as_str();
    path < high
        && path
            .strip_prefix(prefix)
            .is_none_or(may_begin_listable_log_path)
}

fn start_forward_listing(
    storage: &dyn StorageHandler,
    bounds: ListingBounds,
) -> DeltaResult<ListingIterator> {
    let prefix = bounds.prefix.to_string();
    let high = bounds.high.to_string();
    Ok(Box::new(storage.list_from(&bounds.low)?.take_while(
        move |entry| within_log_listing_bounds(entry, &prefix, &high),
    )))
}

fn next_forward_listing(
    mut listing: ListingIterator,
) -> (ForwardListingResult, Option<ListingIterator>) {
    let entries = Vec::from_iter(listing.by_ref().take(FORWARD_LISTING_PAGE_SIZE));
    let may_have_more = entries.len() == FORWARD_LISTING_PAGE_SIZE;
    let result = ForwardListingResult(entries);
    (result, may_have_more.then_some(listing))
}

fn resume_engine_data<W: Send + 'static>(
    reads: DeltaResult<EngineDataIterator>,
    resume: EngineDataResume<W>,
) -> DeltaResult<W> {
    resume.resume_with(|| {
        let mut reads = reads?;
        Ok(match reads.next().transpose()? {
            Some(batch) => (vec![batch], Some(reads)),
            None => (Vec::new(), None),
        })
    })
}

fn version_bound(bound: &Url) -> DeltaResult<Version> {
    let segment = bound
        .path_segments()
        .and_then(|mut segments| segments.next_back())
        .ok_or_else(|| Error::internal_error("listing bound has no path segment"))?;
    segment
        .parse()
        .map_err(|_| Error::internal_error("listing bound is not a version"))
}
