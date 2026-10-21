//! Kernel-side construction of coroutines and typed connector operations.
//!
//! Kernel operations are async functions that receive a [`Channel`]. [`ChannelExt`] methods post
//! the corresponding [`Request`] and suspend until the connector completes its reply. The
//! [`workflow!`] and [`coroutine!`] macros, and their `static_` forms, wrap such a function in a
//! connector-driven [`Workflow`](super::Workflow) or [`Coroutine`](super::Coroutine) that owns
//! its channel.

use std::ops::Range;

use bytes::Bytes;
use delta_kernel_derive::internal_api;
use url::Url;

use super::request::ReadJsonFiles;
use super::Request;
use crate::coroutine::generator::{self, BoundGenerator, Flatten, Generator, GeneratorExt as _};
use crate::coroutine::{
    ChannelExchange, Cursor, KernelFuture, Page, PageGenerator, PageRequest, PagedOperation,
};
use crate::expressions::PredicateRef;
use crate::schema::SchemaRef;
use crate::{EngineData, FileIndex, FileMeta, KernelResult};

/// Kernel's [`crate::coroutine::Channel`] specialized to [`Request`].
///
/// Workflow and coroutine bodies take this as an async-function argument. Child generators borrow
/// it for the lifetime of their [`GeneratorBody`].
#[internal_api]
pub(crate) type Channel = crate::coroutine::Channel<Request>;

type FlattenedPages<Op> =
    Flatten<PageGenerator<Op>, std::vec::IntoIter<KernelResult<Box<dyn EngineData>>>>;

/// Typed Kernel operations posted through a request channel.
///
/// Paged reads initialize the connector operation before returning an unbound generator, so
/// initialization errors surface at the call and callers can compose the stream before
/// [`Self::bind`].
#[internal_api]
#[allow(async_fn_in_trait)]
pub(crate) trait ChannelExt: ChannelExchange<Request> {
    /// Read one small file, or one half-open byte range, completely into memory via
    /// [`Request::ReadSmallFile`].
    fn read_small_file(
        &self,
        url: Url,
        range: Option<Range<FileIndex>>,
    ) -> impl KernelFuture<Option<Bytes>> {
        self.exchange((url, range), Request::ReadSmallFile)
    }

    /// Reads JSON files via [`Request::ReadJson`] as a stream of result batches.
    async fn read_json_files<'a>(
        &'a self,
        files: impl Into<Vec<FileMeta>> + 'a,
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
    ) -> KernelResult<
        impl BoundGenerator<Item = KernelResult<Box<dyn EngineData>>, Output = ()> + Unpin + 'a,
    > {
        let prepare_json_files = self.prepare_json_files(files, physical_schema, predicate);
        Ok(self.bind(prepare_json_files.await?))
    }

    /// Prepares a JSON read as an unbound stream of result batches.
    async fn prepare_json_files(
        &self,
        files: impl Into<Vec<FileMeta>>,
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
    ) -> KernelResult<FlattenedPages<ReadJsonFiles>> {
        let operation = ReadJsonFiles::new(files, physical_schema, predicate);
        let cursor = self.prepare_paged(operation).await?;
        Ok(PageGenerator::Next(cursor).flatten())
    }

    /// Initialize `operation` via [`PageRequest::Prepare`] without fetching its first page.
    fn prepare_paged<Op>(&self, operation: Op) -> impl KernelFuture<Cursor<Op>>
    where
        Op: PagedOperation,
        PageRequest<Op>: Into<Request>,
    {
        self.exchange(operation, PageRequest::Prepare)
    }

    /// Fetch the next page for `cursor` via [`PageRequest::Next`].
    fn next_page<Op>(&self, cursor: Cursor<Op>) -> impl KernelFuture<Page<Op>>
    where
        Op: PagedOperation,
        PageRequest<Op>: Into<Request>,
    {
        self.exchange(cursor, PageRequest::Next)
    }

    /// Bind `generator` so its connector requests post through this channel.
    fn bind<'a, G>(
        &'a self,
        generator: G,
    ) -> impl BoundGenerator<Item = G::Item, Output = G::Output> + Unpin + 'a
    where
        G: Generator<Request> + 'a,
        G::Item: 'a;
}

impl ChannelExt for Channel {
    fn bind<'a, G>(
        &'a self,
        generator: G,
    ) -> impl BoundGenerator<Item = G::Item, Output = G::Output> + Unpin + 'a
    where
        G: Generator<Request> + 'a,
        G::Item: 'a,
    {
        generator.bind(self)
    }
}

/// Active streaming Kernel operation using an existing request channel.
///
/// Connector requests surface through the driver that owns the channel. The `'a` lifetime covers
/// that channel and any state captured by the operation.
#[internal_api]
pub(crate) type GeneratorBody<'a, Y, O = ()> = generator::GeneratorBody<'a, Y, O>;

/// Creates a [`Coroutine`](super::Coroutine) that may borrow captured state.
#[internal_api]
macro_rules! coroutine {
    ($($body:tt)+) => {
        $crate::coroutine::coroutine!($crate::coroutine::kernel::Request, $($body)+)
    };
}

/// Creates an unpolled [`StaticCoroutine`](super::StaticCoroutine) from a static generator body.
#[internal_api]
macro_rules! static_coroutine {
    ($($body:tt)+) => {
        $crate::coroutine::static_coroutine!($crate::coroutine::kernel::Request,$($body)+)
    };
}

/// Creates a [`Workflow`](super::Workflow) that may borrow captured state.
#[internal_api]
macro_rules! workflow {
    ($($body:tt)+) => {
        $crate::coroutine::workflow!($crate::coroutine::kernel::Request, $($body)+)
    };
}

/// Creates an unpolled [`StaticWorkflow`](super::StaticWorkflow) from a static workflow body.
#[internal_api]
macro_rules! static_workflow {
    ($($body:tt)+) => {
        $crate::coroutine::static_workflow!($crate::coroutine::kernel::Request, $($body)+)
    };
}
