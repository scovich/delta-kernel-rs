//! Paginated file listing for connector-driven log discovery.
//!
//! Kernel fixes an exclusive lexicographic interval `(low, high)` and search direction. The
//! connector chooses page size and how to emulate reverse search. Pagination uses an opaque
//! continuation token on [`ListFiles::cursor`].

use std::sync::Arc;

use url::Url;

use crate::{DeltaResult, FileMeta, Version};

/// Bounds and direction shared by every page of one listing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ListFilesRequest {
    /// Directory or prefix that bounds the listing.
    pub prefix: Url,
    /// Exclusive lexicographic lower bound of the interval.
    pub low: Url,
    /// Exclusive lexicographic upper bound of the interval.
    pub high: Url,
    /// Whether pages proceed from the lower bound toward the upper bound. When false, pages
    /// proceed from the upper bound toward the lower bound. Entries remain ascending within
    /// every page.
    pub is_forward_listing: bool,
}

/// One page request within a bounded listing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ListFiles {
    /// Bounds and direction shared by all pages of this listing.
    pub request: Arc<ListFilesRequest>,
    /// `None` starts a new listing at the direction-appropriate end of `(low, high)`. `Some`
    /// resumes with a connector-opaque token from a prior [`ListFilesResult::next_cursor`].
    pub cursor: Option<String>,
}

/// Result of one [`ListFiles`] page.
#[derive(Debug)]
pub struct ListFilesResult {
    /// Files and per-entry errors in this page, in ascending lexicographic order.
    pub entries: Vec<DeltaResult<FileMeta>>,
    /// Whether the page ends at a known version boundary. The boundary is after the highest
    /// version in a forward page and before the lowest version in a backward page.
    pub known_version_boundary: bool,
    /// Opaque continuation for the next [`ListFiles::cursor`], or `None` when the listing is
    /// exhausted under the request's bounds and direction.
    pub next_cursor: Option<String>,
}

/// Builds the bare version path used as an exclusive lexicographic listing bound.
///
/// Files for `version` sort strictly after this path because their names append a file suffix.
pub(super) fn bare_version_path(log_root: &Url, version: Version) -> DeltaResult<Url> {
    Ok(log_root.join(&format!("{version:020}"))?)
}

/// Builds the bounds shared by every page of one Delta log listing.
///
/// The inclusive `end_version` advances to an exclusive bound, with [`Version::MAX`] saturating as
/// the canonical no-upper-bound value.
pub(crate) fn log_listing_request(
    log_root: &Url,
    start_version: Version,
    end_version: Version,
    is_forward_listing: bool,
) -> DeltaResult<Arc<ListFilesRequest>> {
    Ok(Arc::new(ListFilesRequest {
        prefix: log_root.clone(),
        low: bare_version_path(log_root, start_version)?,
        high: bare_version_path(log_root, end_version.saturating_add(1))?,
        is_forward_listing,
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn listing_bounds_are_exclusive_and_max_saturates() {
        let log_root = Url::parse("memory:///_delta_log/").unwrap();

        let bounded = log_listing_request(&log_root, 3, 7, true).unwrap();
        assert_eq!(bounded.low, bare_version_path(&log_root, 3).unwrap());
        assert_eq!(bounded.high, bare_version_path(&log_root, 8).unwrap());

        let unbounded = log_listing_request(&log_root, 3, Version::MAX, true).unwrap();
        assert_eq!(
            unbounded.high,
            bare_version_path(&log_root, Version::MAX).unwrap()
        );
    }
}
