//! Native async driver for Delta Kernel coroutines.
//!
//! [`AsyncEngineConnector`] advances kernel coroutines and serves their requests through the
//! default engine's object store, Arrow evaluator, and readers. It awaits each request before
//! advancing Kernel again; connectors that desire request concurrency can provide their own driver.
//!
//! Use [`AsyncEngineConnector::run`] when this connector should serve every kernel request. To
//! intercept requests such as catalog commit and publish operations, drive the coroutine yourself,
//! handle selected variants, and pass the rest to [`AsyncEngineConnector::reply`].
//!
//! This driver awaits native asynchronous I/O. [`crate::DefaultEngine`] instead implements
//! Kernel's synchronous [`delta_kernel::Engine`] compatibility interface through a
//! [`crate::executor::TaskExecutor`].

use std::any::Any;
use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::future::Future;
use std::num::{NonZero, NonZeroU64, NonZeroUsize};
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll};
use std::time::{Duration, Instant};

use bytes::Bytes;
use delta_kernel::arrow::record_batch::RecordBatch;
use delta_kernel::coroutine::kernel::request::{
    BackwardListing, BackwardListingResult, DirectoryListing, EvaluatorHandle, FileWriteMode,
    ForwardListing, ListingBounds, PredicateEvaluatorHandle, ReadJsonFiles, ReadParquetFiles,
    WriteJsonFile, WriteParquetFile, WriteParquetFiles, WriteParquetFilesResult,
};
use delta_kernel::coroutine::kernel::{
    GeneratorCoroutineStep, StaticCoroutine, StaticGeneratorCoroutine,
};
use delta_kernel::coroutine::{
    drive_coroutine_async, Cursor, Page, PageRequest, PagedOperation, PlanRequest, Request,
};
use delta_kernel::engine::arrow_data::ArrowEngineData;
use delta_kernel::engine::arrow_expression::ArrowEvaluationHandler;
use delta_kernel::engine::{to_json_bytes, writer_options};
use delta_kernel::metrics::{
    emit_json_read_completed, emit_parquet_read_completed, emit_storage_list_completed,
    emit_storage_read_completed,
};
use delta_kernel::object_store::path::Path;
use delta_kernel::object_store::DynObjectStore;
use delta_kernel::parquet::arrow::async_writer::{AsyncArrowWriter, ParquetObjectWriter};
#[cfg(test)]
use delta_kernel::FilteredEngineData;
use delta_kernel::{
    DeltaResult, EngineData, Error, EvaluationHandler, ExpressionEvaluator, FileMeta, FileSize,
    FileSlice, ParquetFooter, ParquetMultiFileWrite, PredicateEvaluator, Version,
};
use futures::stream::BoxStream;
use futures::{Stream, StreamExt as _};
use tokio_util::sync::CancellationToken;
use url::Url;

#[cfg(test)]
use crate::filesystem::head_impl;
use crate::filesystem::{copy_atomic_impl, list_from_impl, put_impl, read_files_impl};
use crate::json::read_json_files_impl;
use crate::parquet::{read_parquet_files_impl, read_parquet_footer_impl};
use crate::{DEFAULT_READ_BATCH_SIZE, DEFAULT_READ_BUFFER_SIZE};

/// Drives kernel coroutines one request at a time through the default engine's native async I/O.
///
/// This connector does not use [`delta_kernel::Engine`] or a
/// [`crate::executor::TaskExecutor`]. Callers may handle selected [`Request`] variants themselves
/// and delegate any remaining requests to [`Self::reply`].
#[derive(Clone)]
pub struct AsyncEngineConnector {
    object_store: Arc<DynObjectStore>,
    cancellation_token: Option<CancellationToken>,
    storage_readahead: usize,
    buffer_size: NonZero<usize>,
    batch_size: NonZero<usize>,
    forward_listing_page_size: NonZero<usize>,
    backward_listing_window_size: NonZero<Version>,
}

impl Debug for AsyncEngineConnector {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_struct("AsyncEngineConnector")
            .field("cancellation_token", &self.cancellation_token)
            .field("storage_readahead", &self.storage_readahead)
            .field("buffer_size", &self.buffer_size)
            .field("batch_size", &self.batch_size)
            .field("forward_listing_page_size", &self.forward_listing_page_size)
            .field(
                "backward_listing_window_size",
                &self.backward_listing_window_size,
            )
            .finish_non_exhaustive()
    }
}

impl AsyncEngineConnector {
    /// Create a connector backed by `object_store` with default read and pagination settings.
    pub fn new(object_store: Arc<DynObjectStore>) -> Self {
        Self {
            object_store,
            cancellation_token: None,
            storage_readahead: STORAGE_READAHEAD,
            buffer_size: DEFAULT_READ_BUFFER_SIZE,
            batch_size: DEFAULT_READ_BATCH_SIZE,
            forward_listing_page_size: DEFAULT_FORWARD_PAGE,
            backward_listing_window_size: DEFAULT_BACKWARD_WINDOW,
        }
    }

    /// Race each asynchronous I/O operation and stream poll against `cancellation_token`.
    ///
    /// Cancellation produces [`Error::Cancelled`] and abandons in-flight work.
    pub fn with_cancellation_token(mut self, cancellation_token: CancellationToken) -> Self {
        self.cancellation_token = Some(cancellation_token);
        self
    }

    /// Set the maximum number of storage reads performed concurrently.
    pub fn with_storage_readahead(mut self, storage_readahead: NonZero<usize>) -> Self {
        self.storage_readahead = storage_readahead.get();
        self
    }

    /// Set the maximum number of JSON or Parquet files read concurrently.
    pub fn with_buffer_size(mut self, buffer_size: NonZero<usize>) -> Self {
        self.buffer_size = buffer_size;
        self
    }

    /// Set the maximum number of rows in each JSON or Parquet batch.
    pub fn with_batch_size(mut self, batch_size: NonZero<usize>) -> Self {
        self.batch_size = batch_size;
        self
    }

    /// Set the maximum number of listing entries in one forward-listing page.
    ///
    /// Defaults to [`ForwardListing::DEFAULT_PAGE_SIZE`].
    pub fn with_forward_listing_page_size(mut self, page_size: NonZero<usize>) -> Self {
        self.forward_listing_page_size = page_size;
        self
    }

    /// Set how many Delta versions one backward-listing request covers.
    ///
    /// Defaults to [`BackwardListing::DEFAULT_WINDOW_SIZE`].
    pub fn with_backward_listing_window_size(mut self, window_size: NonZero<Version>) -> Self {
        self.backward_listing_window_size = window_size;
        self
    }

    /// Serve every request from `coroutine` until it completes.
    pub async fn run<O: Send + 'static>(&self, coroutine: StaticCoroutine<O>) -> DeltaResult<O> {
        Ok(drive_coroutine_async!(coroutine, |request| {
            self.reply(request).await?
        }))
    }

    /// Perform `request` and deliver its response to the suspended coroutine.
    ///
    /// Publish requests are rejected because they require a catalog-specific driver.
    pub async fn reply(&self, request: Request) -> DeltaResult<()> {
        match request {
            Request::ListDirectory(request) => respond_paged(self, request).await,
            Request::ListForward(request) => respond_paged(self, request).await,
            Request::ListBackward(request) => respond_paged(self, request).await,
            Request::ReadSmallFile(file, reply) => reply.send(self.read_small_file(file).await),
            Request::ReadParquetFooter(file, reply) => {
                reply.send(self.read_parquet_footer(file).await)
            }
            Request::ReadJson(request) => respond_paged(self, request).await,
            Request::ReadParquet(request) => respond_paged(self, request).await,
            Request::ExecutePlan(PlanRequest::Query(request)) => {
                let err = Error::unsupported("Default engine does not execute plans");
                match request {
                    PageRequest::Start(_, reply) => reply.send(Err(err)),
                    PageRequest::Prepare(_, reply) => reply.send(Err(err)),
                    PageRequest::Continue(_, reply) => reply.send(Err(err)),
                }
            }
            Request::ExecutePlan(PlanRequest::ParquetFooter(_, reply)) => {
                reply.send(Err(Error::unsupported(
                    "Default engine does not execute plans",
                )));
            }
            Request::WriteBytes(operation, reply) => reply.send(
                self.write_bytes(operation.url, operation.data, operation.overwrite)
                    .await,
            ),
            Request::CopyAtomic(operation, reply) => reply.send(
                self.copy_atomic(operation.source, operation.destination)
                    .await,
            ),
            Request::WriteJson(operation, reply) => {
                reply.send(Box::pin(self.write_json_file(*operation)).await)
            }
            Request::WriteParquet(operation, reply) => {
                reply.send(Box::pin(self.write_parquet_file(*operation)).await)
            }
            Request::WriteParquetFiles(operation, reply) => {
                reply.send(Box::pin(self.write_parquet_files(*operation)).await)
            }
            Request::CreateEngineData(operation, reply) => {
                reply.send(ArrowEvaluationHandler.create_many(operation.schema, operation.rows))
            }
            Request::CreateExpressionEvaluator(operation, reply) => reply.send(
                ArrowEvaluationHandler
                    .new_expression_evaluator(
                        operation.input_schema,
                        operation.expression,
                        operation.output_type,
                    )
                    .map(|evaluator| Arc::new(evaluator) as _),
            ),
            Request::CreatePredicateEvaluator(operation, reply) => reply.send(
                ArrowEvaluationHandler
                    .new_predicate_evaluator(operation.input_schema, operation.predicate)
                    .map(|evaluator| Arc::new(evaluator) as _),
            ),
            Request::EvaluateExpression(operation, reply) => reply.send(
                expression_evaluator(&operation.evaluator)
                    .and_then(|evaluator| evaluator.evaluate(operation.input.as_ref())),
            ),
            Request::EvaluatePredicate(operation, reply) => reply.send(
                predicate_evaluator(&operation.evaluator)
                    .and_then(|evaluator| evaluator.evaluate(operation.input.as_ref())),
            ),
            Request::Publish(..) => {
                return Err(Error::unsupported(
                    "the async default engine does not publish catalog commits",
                ))
            }
        }
        Ok(())
    }

    async fn read_small_file(&self, file: FileSlice) -> DeltaResult<Bytes> {
        let start = Instant::now();
        let stream = self
            .cancel(read_files_impl(
                self.object_store.clone(),
                vec![file],
                self.storage_readahead,
            ))
            .await?;
        let observe = |bytes: &Bytes| (1, bytes.len() as u64);
        let mut stream = metered_stream(stream, start, observe, emit_storage_read_completed);
        let Some(data) = self.next(&mut stream).await?.transpose()? else {
            return Err(Error::internal_error("single-file read returned no result"));
        };
        if self.next(&mut stream).await?.transpose()?.is_some() {
            return Err(Error::internal_error(
                "single-file read returned more than one result",
            ));
        }
        Ok(data)
    }

    async fn read_parquet_footer(&self, file: FileMeta) -> DeltaResult<ParquetFooter> {
        self.cancel(read_parquet_footer_impl(self.object_store.clone(), file))
            .await
    }

    async fn next_data<Op: PagedOperation<Page = Vec<Box<dyn EngineData>>>>(
        &self,
        mut stream: ResultStream<Box<dyn EngineData>>,
    ) -> DeltaResult<Page<Op>> {
        let (data, next) = match self.next(&mut stream).await?.transpose()? {
            Some(data) => (vec![data], Some(Cursor::new(stream))),
            None => (Vec::new(), None),
        };
        Ok(Page { data, next })
    }

    async fn next_listing_page<Op>(
        &self,
        mut stream: ResultStream<FileMeta>,
    ) -> DeltaResult<Page<Op>>
    where
        Op: PagedOperation<Page = Vec<DeltaResult<FileMeta>>>,
    {
        let page_size = self.forward_listing_page_size.get();
        let mut data = Vec::with_capacity(page_size);
        while data.len() < page_size {
            let Some(entry) = self.next(&mut stream).await? else {
                break;
            };
            data.push(entry);
        }
        let next = (data.len() == page_size).then(|| Cursor::new(stream));
        Ok(Page { data, next })
    }

    async fn write_bytes(&self, url: Url, data: Bytes, overwrite: bool) -> DeltaResult<()> {
        let path = Path::from_url_path(url.path())?;
        self.cancel(put_impl(self.object_store.clone(), path, data, overwrite))
            .await
    }

    /// Atomically copy `source` to `destination`, which must not already exist.
    pub async fn copy_atomic(&self, source: Url, destination: Url) -> DeltaResult<()> {
        let source = Path::from_url_path(source.path())?;
        let destination = Path::from_url_path(destination.path())?;
        let store = self.object_store.clone();
        self.cancel(copy_atomic_impl(store, source, destination))
            .await
    }

    async fn write_json_file(&self, operation: WriteJsonFile) -> DeltaResult<FileSize> {
        let WriteJsonFile { url, mode, input } = operation;
        let mut input = input;
        let mut buffer = Vec::new();
        loop {
            self.check_cancelled()?;
            match input.advance().await? {
                GeneratorCoroutineStep::Done(()) => break,
                GeneratorCoroutineStep::Yield(data) => {
                    buffer.extend(to_json_bytes(std::iter::once(Ok(data)))?);
                }
                GeneratorCoroutineStep::Request(request) => {
                    self.reply(request).await?;
                }
            }
        }
        let overwrite = mode == FileWriteMode::Overwrite;
        let written_size = buffer.len() as FileSize;
        self.check_cancelled()?;
        let path = Path::from_url_path(url.path())?;
        put_impl(
            self.object_store.clone(),
            path,
            Bytes::from(buffer),
            overwrite,
        )
        .await?;
        Ok(written_size)
    }

    async fn write_parquet_file(&self, operation: WriteParquetFile) -> DeltaResult<()> {
        let WriteParquetFile { url, mut input } = operation;
        let first = loop {
            self.check_cancelled()?;
            match input.advance().await? {
                GeneratorCoroutineStep::Done(()) => {
                    return Err(Error::generic(
                        "Cannot write parquet file with empty data generator",
                    ))
                }
                GeneratorCoroutineStep::Yield(data) => break data,
                GeneratorCoroutineStep::Request(request) => self.reply(request).await?,
            }
        };

        let first = ArrowEngineData::try_from_engine_data(first)?;
        let first: RecordBatch = (*first).into();
        let path = Path::from_url_path(url.path())?;
        let object_writer = ParquetObjectWriter::new(self.object_store.clone(), path);
        let mut writer = AsyncArrowWriter::try_new_with_options(
            object_writer,
            first.schema(),
            writer_options(),
        )?;
        self.cancel(async {
            writer.write(&first).await?;
            Ok(())
        })
        .await?;

        loop {
            self.check_cancelled()?;
            match input.advance().await? {
                GeneratorCoroutineStep::Done(()) => break,
                GeneratorCoroutineStep::Yield(data) => {
                    let data = ArrowEngineData::try_from_engine_data(data)?;
                    let data: RecordBatch = (*data).into();
                    self.cancel(async {
                        writer.write(&data).await?;
                        Ok(())
                    })
                    .await?;
                }
                GeneratorCoroutineStep::Request(request) => self.reply(request).await?,
            }
        }
        self.check_cancelled()?;
        writer.finish().await?;
        Ok(())
    }

    async fn write_parquet_files(
        &self,
        operation: WriteParquetFiles,
    ) -> DeltaResult<WriteParquetFilesResult> {
        let WriteParquetFiles { write, mut input } = operation;
        let target_rows = write
            .preferred_rows_per_file
            .map(NonZero::get)
            .unwrap_or(DEFAULT_ROWS_PER_FILE);
        let mut rows = 0usize;
        let mut batches = Vec::new();
        let mut files = Vec::new();
        loop {
            self.check_cancelled()?;
            match input.advance().await? {
                GeneratorCoroutineStep::Done(state) => {
                    if !batches.is_empty() {
                        files.push(self.write_parquet_chunk(&write, batches).await?);
                    }
                    return Ok((files, state));
                }
                GeneratorCoroutineStep::Yield(data) => {
                    rows = rows.saturating_add(data.len());
                    batches.push(data);
                    if rows >= target_rows {
                        let chunk = std::mem::take(&mut batches);
                        files.push(self.write_parquet_chunk(&write, chunk).await?);
                        rows = 0;
                    }
                }
                GeneratorCoroutineStep::Request(request) => self.reply(request).await?,
            }
        }
    }

    async fn write_parquet_chunk(
        &self,
        write: &ParquetMultiFileWrite,
        batches: Vec<Box<dyn EngineData>>,
    ) -> DeltaResult<Url> {
        let url = write.new_file_url()?;
        let input = StaticGeneratorCoroutine::from_iterator(Box::new(batches.into_iter().map(Ok)));
        self.write_parquet_file(WriteParquetFile::new(url.clone(), input))
            .await?;
        Ok(url)
    }

    async fn cancel<T>(&self, future: impl Future<Output = DeltaResult<T>>) -> DeltaResult<T> {
        let Some(token) = &self.cancellation_token else {
            return future.await;
        };
        tokio::select! {
            biased;
            _ = token.cancelled() => Err(Error::Cancelled),
            result = future => result,
        }
    }

    async fn next<T>(&self, stream: &mut ResultStream<T>) -> DeltaResult<Option<DeltaResult<T>>> {
        let Some(token) = &self.cancellation_token else {
            return Ok(stream.next().await);
        };
        tokio::select! {
            biased;
            _ = token.cancelled() => Err(Error::Cancelled),
            item = stream.next() => Ok(item),
        }
    }

    fn check_cancelled(&self) -> DeltaResult<()> {
        match &self.cancellation_token {
            Some(token) if token.is_cancelled() => Err(Error::Cancelled),
            _ => Ok(()),
        }
    }
}

const STORAGE_READAHEAD: usize = 10;
const DEFAULT_ROWS_PER_FILE: usize = 50_000;
const DEFAULT_FORWARD_PAGE: NonZero<usize> = match NonZero::new(ForwardListing::DEFAULT_PAGE_SIZE) {
    Some(size) => size,
    None => NonZeroUsize::MIN,
};
const DEFAULT_BACKWARD_WINDOW: NonZero<Version> =
    match NonZero::new(BackwardListing::DEFAULT_WINDOW_SIZE) {
        Some(size) => size,
        None => NonZeroU64::MIN,
    };

type ResultStream<T> = BoxStream<'static, DeltaResult<T>>;

/// Async pagination stored in opaque connector cursors.
///
/// The synchronous Engine driver and native async driver cannot share pagination without making
/// one block or the other async.
trait AsyncPagination<Op: PagedOperation> {
    type State: Any + Send;

    async fn initialize(&self, operation: Op) -> DeltaResult<Self::State>;

    async fn next_page(&self, state: Self::State) -> DeltaResult<Page<Op>>;

    async fn start(&self, operation: Op) -> DeltaResult<Page<Op>> {
        self.continue_from(self.prepare(operation).await?).await
    }

    async fn prepare(&self, operation: Op) -> DeltaResult<Cursor<Op>> {
        Ok(Cursor::new(self.initialize(operation).await?))
    }

    async fn continue_from(&self, cursor: Cursor<Op>) -> DeltaResult<Page<Op>> {
        self.next_page(cursor.into_inner()?).await
    }
}

async fn respond_paged<C, Op>(connector: &C, request: PageRequest<Op>)
where
    C: AsyncPagination<Op>,
    Op: PagedOperation,
{
    match request {
        PageRequest::Start(operation, reply) => {
            reply.send(AsyncPagination::start(connector, operation).await);
        }

        PageRequest::Prepare(operation, reply) => {
            reply.send(AsyncPagination::prepare(connector, operation).await);
        }

        PageRequest::Continue(cursor, reply) => {
            reply.send(AsyncPagination::continue_from(connector, cursor).await);
        }
    }
}

impl AsyncPagination<ForwardListing> for AsyncEngineConnector {
    type State = ResultStream<FileMeta>;

    async fn initialize(&self, ForwardListing(bounds): ForwardListing) -> DeltaResult<Self::State> {
        let start = Instant::now();
        let stream = self
            .cancel(list_from_impl(
                self.object_store.clone(),
                bounds.low.clone(),
            ))
            .await?;
        let stream = metered_stream(stream, start, observe_file, emit_storage_list);
        let stream = stream.take_while(move |entry| std::future::ready(bounds.contains(entry)));
        Ok(Box::pin(stream))
    }

    async fn next_page(&self, stream: Self::State) -> DeltaResult<Page<ForwardListing>> {
        self.next_listing_page(stream).await
    }
}

impl AsyncPagination<DirectoryListing> for AsyncEngineConnector {
    type State = ResultStream<FileMeta>;

    async fn initialize(&self, listing: DirectoryListing) -> DeltaResult<Self::State> {
        let start = Instant::now();
        let stream = self
            .cancel(list_from_impl(
                self.object_store.clone(),
                listing.prefix().clone(),
            ))
            .await?;
        let stream = metered_stream(stream, start, observe_file, emit_storage_list);
        let stream = stream.take_while(move |entry| std::future::ready(listing.contains(entry)));
        Ok(Box::pin(stream))
    }

    async fn next_page(&self, stream: Self::State) -> DeltaResult<Page<DirectoryListing>> {
        self.next_listing_page(stream).await
    }
}

impl AsyncPagination<BackwardListing> for AsyncEngineConnector {
    type State = BackwardListingState;

    async fn initialize(&self, operation: BackwardListing) -> DeltaResult<Self::State> {
        initialize_backward_listing(operation)
    }

    async fn next_page(&self, state: Self::State) -> DeltaResult<Page<BackwardListing>> {
        let BackwardListingState { bounds, high } = state;
        let window = bounds.backward_window(high, self.backward_listing_window_size.get())?;
        let next_high = window.next_high;
        let start_time = Instant::now();
        let store = self.object_store.clone();
        let low = window.low.clone();
        let stream = self.cancel(list_from_impl(store, low)).await?;
        let stream = metered_stream(stream, start_time, observe_file, emit_storage_list);
        let stream = stream.take_while(move |entry| std::future::ready(window.contains(entry)));
        let mut stream: ResultStream<FileMeta> = Box::pin(stream);
        let mut entries = Vec::new();
        while let Some(entry) = self.next(&mut stream).await? {
            entries.push(entry);
        }
        let next = next_high.map(|high| Cursor::new(BackwardListingState { bounds, high }));
        let data = BackwardListingResult {
            entries,
            known_version_boundary: true,
        };
        Ok(Page { data, next })
    }
}

impl AsyncPagination<ReadJsonFiles> for AsyncEngineConnector {
    type State = ResultStream<Box<dyn EngineData>>;

    async fn initialize(&self, read: ReadJsonFiles) -> DeltaResult<Self::State> {
        let num_files = read.files.len() as u64;
        let bytes_read = read.files.iter().map(|file| file.size).sum();
        let stream = self
            .cancel(read_json_files_impl(
                self.object_store.clone(),
                read.files,
                read.physical_schema,
                read.predicate,
                self.batch_size.get(),
                self.buffer_size.get(),
            ))
            .await?;
        Ok(precounted_stream(
            stream,
            num_files,
            bytes_read,
            |_, num_files, bytes_read| emit_json_read_completed(num_files, bytes_read),
        ))
    }

    async fn next_page(&self, stream: Self::State) -> DeltaResult<Page<ReadJsonFiles>> {
        self.next_data(stream).await
    }
}

impl AsyncPagination<ReadParquetFiles> for AsyncEngineConnector {
    type State = ResultStream<Box<dyn EngineData>>;

    async fn initialize(&self, read: ReadParquetFiles) -> DeltaResult<Self::State> {
        let num_files = read.files.len() as u64;
        let bytes_read = read.files.iter().map(|file| file.size).sum();
        let stream = self
            .cancel(read_parquet_files_impl(
                self.object_store.clone(),
                read.files,
                read.physical_schema,
                read.predicate,
                self.buffer_size.get(),
                self.batch_size.get(),
            ))
            .await?;
        Ok(precounted_stream(
            stream,
            num_files,
            bytes_read,
            |_, num_files, bytes_read| emit_parquet_read_completed(num_files, bytes_read),
        ))
    }

    async fn next_page(&self, stream: Self::State) -> DeltaResult<Page<ReadParquetFiles>> {
        self.next_data(stream).await
    }
}

fn expression_evaluator(handle: &EvaluatorHandle) -> DeltaResult<&Arc<dyn ExpressionEvaluator>> {
    handle
        .downcast_ref()
        .ok_or_else(|| Error::internal_error("invalid async engine expression evaluator handle"))
}

fn predicate_evaluator(
    handle: &PredicateEvaluatorHandle,
) -> DeltaResult<&Arc<dyn PredicateEvaluator>> {
    handle
        .downcast_ref()
        .ok_or_else(|| Error::internal_error("invalid async engine predicate evaluator handle"))
}

struct BackwardListingState {
    bounds: Box<ListingBounds>,
    high: Version,
}

fn initialize_backward_listing(
    BackwardListing(bounds): BackwardListing,
) -> DeltaResult<BackwardListingState> {
    let high = bounds.high_version()?;
    Ok(BackwardListingState { bounds, high })
}

type Observe<T> = fn(&T) -> (u64, u64);
type EmitMetric = fn(Duration, u64, u64);

struct MeteredStream<T> {
    inner: ResultStream<T>,
    start: Instant,
    num_files: u64,
    bytes_read: u64,
    observe: Observe<T>,
    emit: EmitMetric,
}

impl<T> Stream for MeteredStream<T> {
    type Item = DeltaResult<T>;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let item = self.inner.as_mut().poll_next(cx);
        if let Poll::Ready(Some(Ok(ref value))) = item {
            let (files, bytes) = (self.observe)(value);
            self.num_files += files;
            self.bytes_read += bytes;
        }
        item
    }
}

impl<T> Drop for MeteredStream<T> {
    fn drop(&mut self) {
        (self.emit)(self.start.elapsed(), self.num_files, self.bytes_read);
    }
}

fn metered_stream<T: Send + 'static>(
    inner: ResultStream<T>,
    start: Instant,
    observe: Observe<T>,
    emit: EmitMetric,
) -> ResultStream<T> {
    Box::pin(MeteredStream {
        inner,
        start,
        num_files: 0,
        bytes_read: 0,
        observe,
        emit,
    })
}

fn precounted_stream<T: Send + 'static>(
    inner: ResultStream<T>,
    num_files: u64,
    bytes_read: u64,
    emit: EmitMetric,
) -> ResultStream<T> {
    Box::pin(MeteredStream {
        inner,
        start: Instant::now(),
        num_files,
        bytes_read,
        observe: |_| (0, 0),
        emit,
    })
}

fn observe_file(_: &FileMeta) -> (u64, u64) {
    (1, 0)
}

fn emit_storage_list(elapsed: Duration, num_files: u64, _bytes_read: u64) {
    emit_storage_list_completed(elapsed, num_files);
}

#[cfg(test)]
mod tests {
    use delta_kernel::coroutine::kernel::StaticGeneratorCoroutine;
    use delta_kernel::object_store::memory::InMemory;

    use super::*;

    #[tokio::test]
    async fn json_write_returns_stored_size() {
        let store: Arc<DynObjectStore> = Arc::new(InMemory::new());
        let connector = AsyncEngineConnector::new(Arc::clone(&store));
        let url = Url::parse("memory:///empty.json").unwrap();
        let input: StaticGeneratorCoroutine<FilteredEngineData> =
            StaticGeneratorCoroutine::from_iterator(Box::new(std::iter::empty()));

        let size = connector
            .write_json_file(WriteJsonFile::new(
                url.clone(),
                FileWriteMode::CreateNew,
                input,
            ))
            .await
            .unwrap();

        let expected = head_impl(store, url).await.unwrap();
        assert_eq!(size, expected.size);
    }

    #[tokio::test]
    async fn cancellation_interrupts_in_flight_future() {
        let token = CancellationToken::new();
        let cancel = token.clone();
        let connector =
            AsyncEngineConnector::new(Arc::new(InMemory::new())).with_cancellation_token(token);
        tokio::spawn(async move {
            tokio::task::yield_now().await;
            cancel.cancel();
        });

        let result = connector
            .cancel(std::future::pending::<DeltaResult<()>>())
            .await;

        assert!(matches!(result, Err(Error::Cancelled)));
    }
}
