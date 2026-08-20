//! Compatibility drivers for serving coroutine requests through an [`Engine`](crate::Engine).

use std::ops::ControlFlow;

use bytes::Bytes;
use url::Url;

use super::listing::{bare_version_path, ListFiles, ListFilesResult};
#[cfg(feature = "declarative-plans")]
use super::read::ExecutePlan;
use super::read::{ReadFiles, ReadJsonFiles, ReadParquetFiles};
use super::{Pagination, PaginationResponse, Resume};
use crate::engine_data::EngineData;
use crate::path::may_begin_listable_log_path;
use crate::{
    DeltaResult, DeltaResultIteratorStatic, Error, FileDataReadResultIterator, FileMeta,
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
    /// Exclusive upper version for the next backward listing window.
    Backward(Version),
}

pub(crate) type ListingPagination<O, Q> =
    Pagination<ListingState, Resume<O, Q, PaginationResponse<ListFilesResult, ListingState>>>;
pub(crate) type ReadPagination<O, Q> =
    Pagination<ReadIterator, Resume<O, Q, PaginationResponse<Option<Vec<Bytes>>, ReadIterator>>>;
type EngineDataResume<O, Q> =
    Resume<O, Q, PaginationResponse<Option<Box<dyn EngineData>>, EngineDataIterator>>;
pub(crate) type EngineDataPagination<O, Q> = Pagination<EngineDataIterator, EngineDataResume<O, Q>>;

/// Serves one paginated listing request through a legacy [`StorageHandler`].
pub(crate) fn resume_list_files<O: Send + 'static, Q: Send + 'static>(
    storage: &dyn StorageHandler,
    request: ListFiles,
    pagination: ListingPagination<O, Q>,
) -> DeltaResult<ControlFlow<O, Q>> {
    let Pagination(state, resume) = pagination;
    resume.resume_with(|| {
        if request.is_forward_listing {
            let new_listing = || -> DeltaResult<_> {
                let prefix = request.prefix.to_string();
                let high = request.high.to_string();
                Ok(Box::new(storage.list_from(&request.low)?.take_while(
                    move |entry| within_log_listing_bounds(entry, &prefix, &high),
                )))
            };
            let mut listing = match state {
                None => new_listing()?,
                Some(ListingState::Forward(listing)) => listing,
                Some(ListingState::Backward(_)) => {
                    return Err(Error::internal_error(
                        "forward listing received backward pagination state",
                    ));
                }
            };
            let entries = Vec::from_iter(listing.by_ref().take(FORWARD_LISTING_PAGE_SIZE));
            let may_have_more = entries.len() == FORWARD_LISTING_PAGE_SIZE;
            let result = ListFilesResult {
                entries,
                known_version_boundary: !may_have_more,
            };
            return Ok(if may_have_more {
                PaginationResponse::More(result, ListingState::Forward(listing))
            } else {
                PaginationResponse::Done(result)
            });
        }

        let start = version_bound(&request.low)?;
        let high = match state {
            None => version_bound(&request.high)?,
            Some(ListingState::Backward(high)) => high,
            Some(ListingState::Forward(_)) => {
                return Err(Error::internal_error(
                    "backward listing received forward pagination state",
                ));
            }
        };
        let lower = high.saturating_sub(BACKWARD_LISTING_WINDOW_SIZE).max(start);
        let lower_url = bare_version_path(&request.prefix, lower)?;
        let upper_url = bare_version_path(&request.prefix, high)?;
        let prefix = request.prefix.as_str();
        let entries = storage
            .list_from(&lower_url)?
            .take_while(|entry| within_log_listing_bounds(entry, prefix, upper_url.as_str()))
            .collect();
        let result = ListFilesResult {
            entries,
            known_version_boundary: true,
        };
        Ok(if lower > start {
            PaginationResponse::More(result, ListingState::Backward(lower))
        } else {
            PaginationResponse::Done(result)
        })
    })
}

/// Serves one paginated raw file-read request through a legacy [`StorageHandler`].
pub(crate) fn resume_read_files<O: Send + 'static, Q: Send + 'static>(
    storage: &dyn StorageHandler,
    request: ReadFiles,
    pagination: ReadPagination<O, Q>,
) -> DeltaResult<ControlFlow<O, Q>> {
    let Pagination(state, resume) = pagination;
    resume.resume_with(|| {
        let mut reads = match (state, request) {
            (None, ReadFiles::Start(files)) => storage.read_files(files)?,
            (Some(reads), ReadFiles::Continue) => reads,
            (None, ReadFiles::Continue) => {
                return Err(Error::internal_error(
                    "raw file read continued without pagination state",
                ));
            }
            (Some(_), ReadFiles::Start(_)) => {
                return Err(Error::internal_error(
                    "raw file read started with existing pagination state",
                ));
            }
        };
        let data = reads
            .by_ref()
            .take(READ_PAGE_SIZE)
            .try_collect()?;
        Ok(if data.is_empty() {
            PaginationResponse::Done(None)
        } else {
            PaginationResponse::More(Some(data), reads)
        })
    })
}

/// Serves one paginated JSON read through a legacy [`JsonHandler`].
pub(crate) fn resume_read_json_files<O, Q>(
    json: &dyn JsonHandler,
    request: ReadJsonFiles,
    pagination: EngineDataPagination<O, Q>,
) -> DeltaResult<ControlFlow<O, Q>>
where
    O: Send + 'static,
    Q: Send + 'static,
{
    let Pagination(state, resume) = pagination;
    let reads = match (state, request) {
        (None, ReadJsonFiles::Start(start)) => {
            json.read_json_files(&start.files, start.physical_schema, start.predicate)
        }
        (Some(reads), ReadJsonFiles::Continue) => Ok(reads),
        (None, ReadJsonFiles::Continue) => Err(Error::internal_error(
            "JSON read continued without pagination state",
        )),
        (Some(_), ReadJsonFiles::Start(_)) => Err(Error::internal_error(
            "JSON read started with existing pagination state",
        )),
    };
    resume_engine_data(reads, resume)
}

/// Serves one paginated Parquet read through a legacy [`ParquetHandler`].
pub(crate) fn resume_read_parquet_files<O, Q>(
    parquet: &dyn ParquetHandler,
    request: ReadParquetFiles,
    pagination: EngineDataPagination<O, Q>,
) -> DeltaResult<ControlFlow<O, Q>>
where
    O: Send + 'static,
    Q: Send + 'static,
{
    let Pagination(state, resume) = pagination;
    let reads = match (state, request) {
        (None, ReadParquetFiles::Start(start)) => {
            parquet.read_parquet_files(&start.files, start.physical_schema, start.predicate)
        }
        (Some(reads), ReadParquetFiles::Continue) => Ok(reads),
        (None, ReadParquetFiles::Continue) => Err(Error::internal_error(
            "Parquet read continued without pagination state",
        )),
        (Some(_), ReadParquetFiles::Start(_)) => Err(Error::internal_error(
            "Parquet read started with existing pagination state",
        )),
    };
    resume_engine_data(reads, resume)
}

/// Serves one paginated declarative-plan execution through a legacy [`Engine`](crate::Engine).
#[cfg(feature = "declarative-plans")]
pub(crate) fn resume_plan<O, Q>(
    engine: &dyn crate::Engine,
    request: ExecutePlan,
    pagination: EngineDataPagination<O, Q>,
) -> DeltaResult<ControlFlow<O, Q>>
where
    O: Send + 'static,
    Q: Send + 'static,
{
    let Pagination(state, resume) = pagination;
    let reads = match (state, request) {
        (None, ExecutePlan::Start(operation)) => engine
            .plan_executor()
            .ok_or_else(|| Error::internal_error("plan execution requested without a PlanExecutor"))
            .and_then(|executor| executor.execute_op(operation)?.into_data()),
        (Some(reads), ExecutePlan::Continue) => Ok(reads),
        (None, ExecutePlan::Continue) => Err(Error::internal_error(
            "plan execution continued without pagination state",
        )),
        (Some(_), ExecutePlan::Start(_)) => Err(Error::internal_error(
            "plan execution started with existing pagination state",
        )),
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

fn resume_engine_data<O, Q>(
    reads: DeltaResult<EngineDataIterator>,
    resume: EngineDataResume<O, Q>,
) -> DeltaResult<ControlFlow<O, Q>>
where
    O: Send + 'static,
    Q: Send + 'static,
{
    resume.resume_with(|| {
        let mut reads = reads?;
        Ok(match reads.next().transpose()? {
            Some(batch) => PaginationResponse::More(Some(batch), reads),
            None => PaginationResponse::Done(None),
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
