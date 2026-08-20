//! Compatibility drivers for serving coroutine requests through an [`Engine`](crate::Engine).

use std::ops::ControlFlow;
use std::sync::Arc;

use bytes::Bytes;
use url::Url;

use super::listing::{
    bare_version_path, ForwardListing, ForwardListingResult, ListFiles, ListFilesResult,
    ListingBounds,
};
#[cfg(feature = "declarative-plans")]
use super::read::ExecutePlan;
use super::read::{ReadFiles, ReadJsonFiles, ReadParquetFiles};
use super::{coroutine_request, Pagination, Resume, SupportsPaginated};
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
const READ_PAGE_SIZE: usize = 1;

/// Legacy Engine iterator retained across forward listing pages.
pub(crate) type ListingIterator = DeltaResultIteratorStatic<FileMeta>;
/// Legacy Engine iterator retained across raw read pages.
pub(crate) type ReadIterator = DeltaResultIteratorStatic<Bytes>;
/// Legacy Engine iterator retained across file-format and plan result pages.
pub(crate) type EngineDataIterator = FileDataReadResultIterator;

/// Legacy Engine state retained by a listing pagination cursor.
pub(crate) enum ListingState {
    /// Active forward listing iterator.
    Forward(ListingIterator),
    /// Bounds and exclusive upper version for the next backward listing window.
    Backward {
        bounds: Box<ListingBounds>,
        high: Version,
    },
}

pub(crate) type ListingPagination = Pagination<ListFiles, ListingState>;
pub(crate) type ListingResume<O, Q> = Resume<O, Q, (ListFilesResult, Option<ListingState>)>;
pub(crate) type ReadPagination = Pagination<ReadFiles, ReadIterator>;
pub(crate) type ReadResume<O, Q> = Resume<O, Q, (Option<Vec<Bytes>>, Option<ReadIterator>)>;
pub(crate) type EngineDataResume<O, Q> =
    Resume<O, Q, (Option<Box<dyn EngineData>>, Option<EngineDataIterator>)>;
pub(crate) type EngineDataPagination<W> = Pagination<W, EngineDataIterator>;

/// Owned compatibility adapter for serving coroutine requests through an [`Engine`].
pub(crate) struct EngineConnector {
    storage: Arc<dyn StorageHandler>,
}

#[coroutine_request(output = O)]
pub(crate) enum EngineRequest<O: Send + 'static> {
    #[paginated(state = ListingIterator)]
    ListForward(ForwardListing),
}

impl EngineConnector {
    /// Create an adapter by cloning the engine's handlers.
    pub(crate) fn new(engine: &dyn Engine) -> Self {
        Self {
            storage: engine.storage_handler(),
        }
    }
}

impl SupportsPaginated<ForwardListing> for EngineConnector {
    type State = ListingIterator;

    fn start(&mut self, bounds: ListingBounds) -> DeltaResult<Self::State> {
        start_forward_listing(self.storage.as_ref(), bounds)
    }

    fn next(
        &mut self,
        listing: Self::State,
    ) -> DeltaResult<(ForwardListingResult, Option<Self::State>)> {
        Ok(next_forward_listing(listing))
    }
}

/// Serves one paginated listing request through a legacy [`StorageHandler`].
pub(crate) fn resume_list_files<O: Send + 'static, Q: Send + 'static>(
    storage: &dyn StorageHandler,
    pagination: ListingPagination,
    resume: ListingResume<O, Q>,
) -> DeltaResult<ControlFlow<O, Q>> {
    resume.resume_with(|| {
        let forward_page = |listing: ListingIterator| {
            let (result, listing) = next_forward_listing(listing);
            let list_files_result = ListFilesResult {
                entries: result.entries,
                known_version_boundary: listing.is_none(),
            };
            (list_files_result, listing.map(ListingState::Forward))
        };
        let backward_page = |bounds: Box<ListingBounds>, high: Version| -> DeltaResult<_> {
            let start = version_bound(&bounds.low)?;
            let lower = high.saturating_sub(BACKWARD_LISTING_WINDOW_SIZE).max(start);
            let lower_url = bare_version_path(&bounds.prefix, lower)?;
            let upper_url = bare_version_path(&bounds.prefix, high)?;
            let prefix = bounds.prefix.as_str();
            let entries = storage
                .list_from(&lower_url)?
                .take_while(|entry| within_log_listing_bounds(entry, prefix, upper_url.as_str()))
                .collect();
            let result = ListFilesResult {
                entries,
                known_version_boundary: true,
            };
            let state = (lower > start).then_some(ListingState::Backward {
                bounds,
                high: lower,
            });
            Ok((result, state))
        };

        match pagination {
            Pagination::Start(ListFiles::Forward(bounds)) => {
                let listing = start_forward_listing(storage, bounds)?;
                Ok(forward_page(listing))
            }
            Pagination::Continue(ListingState::Forward(listing)) => Ok(forward_page(listing)),
            Pagination::Start(ListFiles::Backward(bounds)) => {
                let high = version_bound(&bounds.high)?;
                backward_page(Box::new(bounds), high)
            }
            Pagination::Continue(ListingState::Backward { bounds, high }) => {
                backward_page(bounds, high)
            }
        }
    })
}

/// Serves one paginated raw file-read request through a legacy [`StorageHandler`].
pub(crate) fn resume_read_files<O: Send + 'static, Q: Send + 'static>(
    storage: &dyn StorageHandler,
    pagination: ReadPagination,
    resume: ReadResume<O, Q>,
) -> DeltaResult<ControlFlow<O, Q>> {
    resume.resume_with(|| {
        let mut reads = match pagination {
            Pagination::Start(files) => storage.read_files(files)?,
            Pagination::Continue(reads) => reads,
        };
        let data = reads
            .by_ref()
            .take(READ_PAGE_SIZE)
            .collect::<DeltaResult<Vec<_>>>()?;
        Ok(if data.is_empty() {
            (None, None)
        } else {
            (Some(data), Some(reads))
        })
    })
}

/// Serves one paginated JSON read through a legacy [`JsonHandler`].
pub(crate) fn resume_read_json_files<O: Send + 'static, Q: Send + 'static>(
    json: &dyn JsonHandler,
    pagination: EngineDataPagination<ReadJsonFiles>,
    resume: EngineDataResume<O, Q>,
) -> DeltaResult<ControlFlow<O, Q>> {
    let reads = match pagination {
        Pagination::Start(start) => {
            json.read_json_files(&start.files, start.physical_schema, start.predicate)
        }
        Pagination::Continue(reads) => Ok(reads),
    };
    resume_engine_data(reads, resume)
}

/// Serves one paginated Parquet read through a legacy [`ParquetHandler`].
pub(crate) fn resume_read_parquet_files<O: Send + 'static, Q: Send + 'static>(
    parquet: &dyn ParquetHandler,
    pagination: EngineDataPagination<ReadParquetFiles>,
    resume: EngineDataResume<O, Q>,
) -> DeltaResult<ControlFlow<O, Q>> {
    let reads = match pagination {
        Pagination::Start(start) => {
            parquet.read_parquet_files(&start.files, start.physical_schema, start.predicate)
        }
        Pagination::Continue(reads) => Ok(reads),
    };
    resume_engine_data(reads, resume)
}

/// Serves one paginated declarative-plan execution through a legacy [`Engine`].
#[cfg(feature = "declarative-plans")]
pub(crate) fn resume_plan<O: Send + 'static, Q: Send + 'static>(
    engine: &dyn Engine,
    pagination: EngineDataPagination<ExecutePlan>,
    resume: EngineDataResume<O, Q>,
) -> DeltaResult<ControlFlow<O, Q>> {
    let reads = match pagination {
        Pagination::Start(operation) => engine
            .plan_executor()
            .ok_or_else(|| Error::internal_error("plan execution requested without a PlanExecutor"))
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
    let result = ForwardListingResult { entries };
    (result, may_have_more.then_some(listing))
}

fn resume_engine_data<O: Send + 'static, Q: Send + 'static>(
    reads: DeltaResult<EngineDataIterator>,
    resume: EngineDataResume<O, Q>,
) -> DeltaResult<ControlFlow<O, Q>> {
    resume.resume_with(|| {
        let mut reads = reads?;
        Ok(match reads.next().transpose()? {
            Some(batch) => (Some(batch), Some(reads)),
            None => (None, None),
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
