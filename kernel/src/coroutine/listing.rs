//! Paginated file-listing requests.

use url::Url;

use super::core::{PendingPageRequest, PendingRequest};
use super::{Channel, Cursor, Page, PageRequest, PagedOperation};
use crate::{DeltaResult, FileMeta, Version};

pub struct ListingBounds {
    pub prefix: Url,
    pub low: Url,
    pub high: Url,
}

/// A bounded listing whose pages and entries are in ascending lexicographic order.
pub struct ForwardListing(pub Box<ListingBounds>);

impl PagedOperation for ForwardListing {
    type Page = Vec<DeltaResult<FileMeta>>;
}

/// A bounded listing whose page ranges move high to low, with page entries are in ascending order.
pub struct BackwardListing(pub Box<ListingBounds>);

impl PagedOperation for BackwardListing {
    type Page = BackwardListingResult;
}

/// One backward-listing page with entries in ascending lexicographic order.
pub struct BackwardListingResult {
    pub entries: Vec<DeltaResult<FileMeta>>,
    /// True only if no later, lower page can contain another file for any Delta log version here.
    ///
    /// False is conservative and always valid.
    pub known_version_boundary: bool,
}

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

/// Build the bare version path used as an exclusive listing bound.
pub(crate) fn bare_version_path(log_root: &Url, version: Version) -> DeltaResult<Url> {
    Ok(log_root.join(&format!("{version:020}"))?)
}

impl Channel {
    pub(crate) async fn start_forward_listing(
        &self,
        bounds: ListingBounds,
    ) -> DeltaResult<Page<ForwardListing>> {
        self.exchange(ForwardListing(Box::new(bounds)), |request| {
            PendingRequest::ListForward(PendingPageRequest::Start(request))
        })
        .await
    }

    pub(crate) async fn prepare_forward_listing(
        &self,
        bounds: ListingBounds,
    ) -> DeltaResult<Cursor<ForwardListing>> {
        self.exchange(ForwardListing(Box::new(bounds)), |request| {
            PendingRequest::ListForward(PendingPageRequest::Prepare(request))
        })
        .await
    }

    pub(crate) async fn continue_forward_listing(
        &self,
        cursor: Cursor<ForwardListing>,
    ) -> DeltaResult<Page<ForwardListing>> {
        self.exchange(cursor, |request| {
            PendingRequest::ListForward(PendingPageRequest::Continue(request))
        })
        .await
    }

    pub(crate) async fn start_backward_listing(
        &self,
        bounds: ListingBounds,
    ) -> DeltaResult<Page<BackwardListing>> {
        self.exchange(BackwardListing(Box::new(bounds)), |request| {
            PendingRequest::ListBackward(PendingPageRequest::Start(request))
        })
        .await
    }

    pub(crate) async fn prepare_backward_listing(
        &self,
        bounds: ListingBounds,
    ) -> DeltaResult<Cursor<BackwardListing>> {
        self.exchange(BackwardListing(Box::new(bounds)), |request| {
            PendingRequest::ListBackward(PendingPageRequest::Prepare(request))
        })
        .await
    }

    pub(crate) async fn continue_backward_listing(
        &self,
        cursor: Cursor<BackwardListing>,
    ) -> DeltaResult<Page<BackwardListing>> {
        self.exchange(cursor, |request| {
            PendingRequest::ListBackward(PendingPageRequest::Continue(request))
        })
        .await
    }
}

impl<N: Send + 'static> PageRequest<N, ForwardListing> {
    pub(super) async fn forward_to(self, parent: &Channel) -> DeltaResult<N> {
        match self {
            Self::Start(ForwardListing(bounds), resume) => {
                resume.resume(parent.start_forward_listing(*bounds).await)
            }
            Self::Prepare(ForwardListing(bounds), resume) => {
                resume.resume(parent.prepare_forward_listing(*bounds).await)
            }
            Self::Continue(cursor, resume) => {
                resume.resume(parent.continue_forward_listing(cursor).await)
            }
        }
    }
}

impl<N: Send + 'static> PageRequest<N, BackwardListing> {
    pub(super) async fn forward_to(self, parent: &Channel) -> DeltaResult<N> {
        match self {
            Self::Start(BackwardListing(bounds), resume) => {
                resume.resume(parent.start_backward_listing(*bounds).await)
            }
            Self::Prepare(BackwardListing(bounds), resume) => {
                resume.resume(parent.prepare_backward_listing(*bounds).await)
            }
            Self::Continue(cursor, resume) => {
                resume.resume(parent.continue_backward_listing(cursor).await)
            }
        }
    }
}
