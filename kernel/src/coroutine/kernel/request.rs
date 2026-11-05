//! Kernel's connector [`Request`]s and their operation payloads.
//!
//! Variants are added in lockstep with connector support and the Kernel operations that issue them.

use bytes::Bytes;
use derive_more::From;
use url::Url;

use crate::coroutine::{PageRequest, PagedOperation, Reply};
use crate::expressions::PredicateRef;
use crate::schema::SchemaRef;
use crate::{EngineData, FileMeta, FileSlice, KernelResult};

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
    /// Exclusive lower URL bound.
    pub low: Url,
    /// Exclusive upper URL bound.
    pub high: Url,
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
