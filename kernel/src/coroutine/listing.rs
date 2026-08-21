//! Paginated file listing for connector-driven log discovery.
//!
//! Kernel fixes an exclusive lexicographic interval `(low, high)` and search direction. The
//! connector chooses page size and how to emulate reverse search. Pagination state is carried by
//! the coroutine infrastructure.

use url::Url;

use super::{Operation, PaginatedOperation};
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

/// Forward file-listing operation.
pub struct ForwardListing(
    /// Exclusive listing bounds.
    pub ListingBounds,
);

impl Operation for ForwardListing {
    type Response = ForwardListingResult;
}

impl PaginatedOperation for ForwardListing {}

/// One page returned by [`ForwardListing`].
pub struct ForwardListingResult(
    /// Files and per-entry errors in this page, in ascending lexicographic order.
    pub Vec<DeltaResult<FileMeta>>,
);

/// Backward file-listing operation.
pub struct BackwardListing(
    /// Exclusive listing bounds.
    pub ListingBounds,
);

impl Operation for BackwardListing {
    type Response = BackwardListingResult;
}

impl PaginatedOperation for BackwardListing {}

/// One page returned by [`BackwardListing`].
pub struct BackwardListingResult {
    /// Files and per-entry errors in this page, in ascending lexicographic order.
    pub entries: Vec<DeltaResult<FileMeta>>,
    /// True if the page is known to end at the boundary between two versions
    pub known_version_boundary: bool,
}

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
) -> DeltaResult<ForwardListing> {
    Ok(ForwardListing(log_listing_bounds(
        log_root,
        start_version,
        end_version,
    )?))
}

/// Builds bounds for a forward listing over the inclusive version range.
pub(crate) fn forward_listing_bounds(
    log_root: &Url,
    start_version: Version,
    end_version: Version,
) -> DeltaResult<ListingBounds> {
    log_listing_bounds(log_root, start_version, end_version)
}

/// Builds a backward listing over the inclusive version range.
pub(crate) fn backward_log_listing_request(
    log_root: &Url,
    start_version: Version,
    end_version: Version,
) -> DeltaResult<BackwardListing> {
    Ok(BackwardListing(log_listing_bounds(
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

        let ForwardListing(bounded) = forward_log_listing_request(&log_root, 3, 7).unwrap();
        assert_eq!(bounded.low, bare_version_path(&log_root, 3).unwrap());
        assert_eq!(bounded.high, bare_version_path(&log_root, 8).unwrap());

        let ForwardListing(unbounded) =
            forward_log_listing_request(&log_root, 3, Version::MAX).unwrap();
        assert_eq!(
            unbounded.high,
            bare_version_path(&log_root, Version::MAX).unwrap()
        );
    }
}
