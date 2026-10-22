//! Kernel's connector [`Request`]s and their operation payloads.
//!
//! Variants are added in lockstep with connector support and the Kernel operations that issue them.

use bytes::Bytes;
use derive_more::From;
use url::Url;

use crate::coroutine::{PageRequest, PagedOperation, Reply};
use crate::expressions::PredicateRef;
use crate::path::may_begin_listable_log_path;
use crate::schema::SchemaRef;
use crate::{EngineData, FileMeta, FileSlice, KernelResult, Version};

/// Operations Kernel can request from a connector.
#[derive(From)]
pub enum Request {
    /// List a bounded path range in ascending order.
    ListForward(PageRequest<ForwardListing>),
    /// Read a whole file when the range is `None`, or exactly the half-open range otherwise.
    ///
    /// Missing-object failures remain connector errors. `Ok(None)` means the connector returned no
    /// result. Extra objects from a single-file read are an error.
    ReadSmallFile(FileSlice, Reply<Option<Bytes>>),
    /// Read JSON files as ordered [`EngineData`] batches. Each file may produce multiple batches,
    /// but batches may not span files.
    ReadJson(PageRequest<ReadJsonFiles>),
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
