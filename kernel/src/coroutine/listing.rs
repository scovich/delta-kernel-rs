//! Paginated file listing for connector-driven log discovery.
//!
//! Kernel fixes an exclusive lexicographic interval `(low, high)` and search direction. The
//! connector chooses page size and how to emulate reverse search. Pagination state is carried by
//! the coroutine infrastructure.

use url::Url;

use super::{Pagination, PaginationResponse, Resume};
use crate::{DeltaResult, FileMeta, Version};

/// Exclusive lexicographic bounds shared by every page of one listing.
#[derive(Debug, PartialEq, Eq)]
pub struct ListingBounds {
    /// Directory or prefix that bounds the listing.
    pub prefix: Url,
    /// Exclusive lexicographic lower bound of the interval.
    pub low: Url,
    /// Exclusive lexicographic upper bound of the interval.
    pub high: Url,
}

/// Initial work for a paginated file listing.
#[derive(Debug, PartialEq, Eq)]
pub enum ListFiles {
    /// List from the lower bound toward the upper bound.
    Forward(ListingBounds),
    /// List from the upper bound toward the lower bound.
    ///
    /// Entries remain ascending within every page.
    Backward(ListingBounds),
}

/// Result of one [`ListFiles`] page.
#[derive(Debug)]
pub struct ListFilesResult {
    /// Files and per-entry errors in this page, in ascending lexicographic order.
    pub entries: Vec<DeltaResult<FileMeta>>,
    /// Whether the page ends at a known version boundary. The boundary is after the highest
    /// version in a forward page and before the lowest version in a backward page.
    pub known_version_boundary: bool,
}

/// Constructor for a workflow request variant that delegates paginated file listing.
pub(crate) type ListFilesConstructor<O, Q, S> =
    fn(Pagination<ListFiles, S>, Resume<O, Q, PaginationResponse<ListFilesResult, S>>) -> Q;

/// Builds the bare version path used as an exclusive lexicographic listing bound.
///
/// Files for `version` sort strictly after this path because their names append a file suffix.
pub(super) fn bare_version_path(log_root: &Url, version: Version) -> DeltaResult<Url> {
    Ok(log_root.join(&format!("{version:020}"))?)
}

/// Builds a forward listing over the inclusive version range.
pub(crate) fn forward_log_listing_request(
    log_root: &Url,
    start_version: Version,
    end_version: Version,
) -> DeltaResult<ListFiles> {
    Ok(ListFiles::Forward(log_listing_bounds(
        log_root,
        start_version,
        end_version,
    )?))
}

/// Builds a backward listing over the inclusive version range.
pub(crate) fn backward_log_listing_request(
    log_root: &Url,
    start_version: Version,
    end_version: Version,
) -> DeltaResult<ListFiles> {
    Ok(ListFiles::Backward(log_listing_bounds(
        log_root,
        start_version,
        end_version,
    )?))
}

fn log_listing_bounds(
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn listing_bounds_are_exclusive_and_max_saturates() {
        let log_root = Url::parse("memory:///_delta_log/").unwrap();

        let ListFiles::Forward(bounded) = forward_log_listing_request(&log_root, 3, 7).unwrap()
        else {
            unreachable!()
        };
        assert_eq!(bounded.low, bare_version_path(&log_root, 3).unwrap());
        assert_eq!(bounded.high, bare_version_path(&log_root, 8).unwrap());

        let ListFiles::Forward(unbounded) =
            forward_log_listing_request(&log_root, 3, Version::MAX).unwrap()
        else {
            unreachable!()
        };
        assert_eq!(
            unbounded.high,
            bare_version_path(&log_root, Version::MAX).unwrap()
        );
    }
}
