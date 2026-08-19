//! Compatibility drivers for serving coroutine requests through an [`Engine`](crate::Engine).

use std::iter::Peekable;

use bytes::Bytes;
use url::Url;

use super::listing::{bare_version_path, ListFiles, ListFilesResult};
use super::read::ReadFiles;
use crate::path::may_begin_listable_log_path;
use crate::{DeltaResult, Error, FileMeta, StorageHandler, Version};

const FORWARD_LISTING_PAGE_SIZE: usize = 1024;
const BACKWARD_LISTING_WINDOW_SIZE: Version = 1000;
const READ_PAGE_SIZE: usize = 1;

type ListingIterator = Peekable<Box<dyn Iterator<Item = DeltaResult<FileMeta>>>>;
type ReadIterator = Box<dyn Iterator<Item = DeltaResult<Bytes>>>;

/// State retained while an Engine compatibility driver serves paginated coroutine requests.
#[derive(Default)]
pub(crate) struct EngineRequestState {
    listing: Option<ListingIterator>,
    reads: Option<ReadIterator>,
}

impl EngineRequestState {
    /// Serves one paginated raw file-read request through a legacy [`StorageHandler`].
    pub(crate) fn execute_read_files(
        &mut self,
        storage: &dyn StorageHandler,
        request: ReadFiles,
    ) -> DeltaResult<Option<Vec<Bytes>>> {
        match request {
            ReadFiles::Start(files) => {
                self.reads = Some(storage.read_files(files)?);
            }
            ReadFiles::Continue if self.reads.is_none() => {
                return Err(Error::internal_error(
                    "raw file read continued without an active Engine iterator",
                ));
            }
            ReadFiles::Continue => {}
        }

        let reads = self.reads.as_mut().ok_or_else(|| {
            Error::internal_error("raw file read did not initialize an Engine iterator")
        })?;
        let data = reads
            .by_ref()
            .take(READ_PAGE_SIZE)
            .collect::<DeltaResult<Vec<_>>>()?;
        if data.is_empty() {
            self.reads = None;
            return Ok(None);
        }
        Ok(Some(data))
    }

    /// Serves one paginated listing request through a legacy [`StorageHandler`].
    pub(crate) fn execute_list_files(
        &mut self,
        storage: &dyn StorageHandler,
        request: ListFiles,
    ) -> DeltaResult<ListFilesResult> {
        self.reads = None;
        if request.request.is_forward_listing {
            return self.execute_forward_listing(storage, request);
        }
        self.execute_backward_listing(storage, request)
    }

    fn execute_forward_listing(
        &mut self,
        storage: &dyn StorageHandler,
        request: ListFiles,
    ) -> DeltaResult<ListFilesResult> {
        if request.cursor.is_none() {
            let prefix = request.request.prefix.to_string();
            let high = request.request.high.to_string();
            let listing = storage
                .list_from(&request.request.low)?
                .take_while(move |entry| match entry {
                    Ok(entry) => within_log_listing_bounds(entry.location.as_str(), &prefix, &high),
                    Err(_) => true,
                });
            let listing: Box<dyn Iterator<Item = DeltaResult<FileMeta>>> = Box::new(listing);
            self.listing = Some(listing.peekable());
        } else if self.listing.is_none() {
            return Err(Error::internal_error(
                "listing continued without an active Engine iterator",
            ));
        }

        let listing = self.listing.as_mut().ok_or_else(|| {
            Error::internal_error("listing did not initialize an Engine iterator")
        })?;
        let entries = listing.by_ref().take(FORWARD_LISTING_PAGE_SIZE).collect();
        let has_more = listing.peek().is_some();
        if !has_more {
            self.listing = None;
        }
        Ok(ListFilesResult {
            entries,
            known_version_boundary: !has_more,
            next_cursor: has_more.then(String::new),
        })
    }

    fn execute_backward_listing(
        &mut self,
        storage: &dyn StorageHandler,
        request: ListFiles,
    ) -> DeltaResult<ListFilesResult> {
        self.listing = None;
        let start = version_bound(&request.request.low)?;
        let upper = match request.cursor {
            Some(cursor) => cursor
                .parse()
                .map_err(|_| Error::internal_error("invalid backward listing cursor"))?,
            None => {
                let high = version_bound(&request.request.high)?;
                if high == Version::MAX {
                    Version::MAX
                } else {
                    high.checked_sub(1)
                        .ok_or_else(|| Error::internal_error("invalid listing upper bound"))?
                }
            }
        };
        let lower = upper
            .saturating_sub(BACKWARD_LISTING_WINDOW_SIZE - 1)
            .max(start);
        let lower_url = bare_version_path(&request.request.prefix, lower)?;
        let upper_url = bare_version_path(&request.request.prefix, upper.saturating_add(1))?;
        let prefix = request.request.prefix.as_str();
        let entries = storage
            .list_from(&lower_url)?
            .take_while(|entry| match entry {
                Ok(entry) => {
                    within_log_listing_bounds(entry.location.as_str(), prefix, upper_url.as_str())
                }
                Err(_) => true,
            })
            .collect();

        Ok(ListFilesResult {
            entries,
            known_version_boundary: true,
            next_cursor: (lower > start).then(|| (lower - 1).to_string()),
        })
    }
}

fn within_log_listing_bounds(path: &str, prefix: &str, high: &str) -> bool {
    path < high
        && path
            .strip_prefix(prefix)
            .is_none_or(may_begin_listable_log_path)
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
