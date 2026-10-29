//! Native async driver for Delta Kernel coroutines.
//!
//! [`AsyncEngineConnector`] advances Kernel coroutines and serves their requests through the
//! default engine's object store and readers. It awaits each request before advancing Kernel
//! again; connectors that desire request concurrency can provide their own driver.
//!
//! Use [`AsyncEngineConnector::run`] when this connector should serve every Kernel request. To
//! intercept requests the default engine does not handle, drive the workflow yourself, handle
//! selected variants, and pass the rest to [`AsyncEngineConnector::respond`].
//!
//! This driver awaits native asynchronous I/O. [`crate::DefaultEngine`] instead implements
//! Kernel's synchronous [`delta_kernel::Engine`] compatibility interface through a
//! [`crate::executor::TaskExecutor`].

use std::any::Any;
use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::future::Future;
use std::num::NonZero;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll};
use std::time::{Duration, Instant};

use bytes::Bytes;
use delta_kernel::coroutine::kernel::request::{
    ExpressionEvaluatorHandle, ForwardListing, PredicateEvaluatorHandle, ReadJsonFiles,
    ReadParquetFiles,
};
use delta_kernel::coroutine::kernel::{Request, Workflow};
use delta_kernel::coroutine::{
    drive_workflow_async, AsyncResult as _, Cursor, Page, PageRequest, PagedOperation,
};
use delta_kernel::engine::arrow_expression::ArrowEvaluationHandler;
use delta_kernel::metrics::{
    emit_json_read_completed, emit_parquet_read_completed, emit_storage_list_completed,
    emit_storage_read_completed,
};
use delta_kernel::object_store::DynObjectStore;
use delta_kernel::{
    EngineData, EvaluationHandler, ExpressionEvaluator, FileMeta, FileSlice, KernelError,
    KernelResult, PredicateEvaluator,
};
use futures::stream::BoxStream;
use futures::{Stream, StreamExt as _};
use tokio_util::sync::CancellationToken;

use crate::filesystem::{list_from_impl, read_files_impl};
use crate::json::read_json_files_impl;
use crate::parquet::{read_parquet_files_impl, read_parquet_footer_impl};
use crate::{DEFAULT_READ_BATCH_SIZE, DEFAULT_READ_BUFFER_SIZE};

/// Drives Kernel workflows one request at a time through the default engine's native async I/O.
///
/// This connector does not use [`delta_kernel::Engine`] or a
/// [`crate::executor::TaskExecutor`].
#[derive(Clone)]
pub struct AsyncEngineConnector {
    object_store: Arc<DynObjectStore>,
    cancellation_token: Option<CancellationToken>,
    storage_readahead: usize,
    buffer_size: NonZero<usize>,
    batch_size: NonZero<usize>,
}

impl Debug for AsyncEngineConnector {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_struct("AsyncEngineConnector")
            .field("cancellation_token", &self.cancellation_token)
            .field("storage_readahead", &self.storage_readahead)
            .field("buffer_size", &self.buffer_size)
            .field("batch_size", &self.batch_size)
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
        }
    }

    /// Race connector-driven operations against `cancellation_token`.
    ///
    /// Cancellation produces [`KernelError::Cancelled`] and abandons the coroutine or workflow
    /// and in-flight connector work.
    pub fn with_cancellation_token(mut self, cancellation_token: CancellationToken) -> Self {
        self.cancellation_token = Some(cancellation_token);
        self
    }

    /// Set the maximum number of storage reads performed concurrently.
    pub fn with_storage_readahead(mut self, storage_readahead: NonZero<usize>) -> Self {
        self.storage_readahead = storage_readahead.get();
        self
    }

    /// Sets the maximum number of files read concurrently by JSON and Parquet operations.
    pub fn with_buffer_size(mut self, buffer_size: NonZero<usize>) -> Self {
        self.buffer_size = buffer_size;
        self
    }

    /// Sets the maximum number of rows in each JSON and Parquet batch.
    pub fn with_batch_size(mut self, batch_size: NonZero<usize>) -> Self {
        self.batch_size = batch_size;
        self
    }

    /// Serve supported requests from `workflow` until it completes.
    ///
    /// Cancellation is owned entirely by this driver and is never delivered through a
    /// [`delta_kernel::coroutine::Reply`]. Because this API otherwise returns [`KernelResult`],
    /// driver cancellation is represented as [`KernelError::Cancelled`] at this outer boundary.
    pub async fn run<O: Send + 'static>(&self, workflow: Workflow<'_, O>) -> KernelResult<O> {
        self.race_cancellation(self.run_inner(workflow)).await
    }

    async fn run_inner<O: Send + 'static>(&self, workflow: Workflow<'_, O>) -> KernelResult<O> {
        Ok(drive_workflow_async!(workflow, |request| {
            self.respond_inner(request).await?
        }))
    }

    async fn race_cancellation<O>(
        &self,
        drive: impl Future<Output = KernelResult<O>>,
    ) -> KernelResult<O> {
        let Some(token) = &self.cancellation_token else {
            return drive.await;
        };
        tokio::select! {
            biased;
            _ = token.cancelled() => Err(KernelError::Cancelled),
            result = drive => result,
        }
    }

    /// Perform `request` and deliver its response to the suspended caller.
    pub async fn respond(&self, request: Request) -> KernelResult<()> {
        self.race_cancellation(self.respond_inner(request)).await
    }

    async fn respond_inner(&self, request: Request) -> KernelResult<()> {
        match request {
            Request::ListForward(request) => respond_paged(self, request).await?,
            Request::ReadSmallFile(file, reply) => reply.send(self.read_small_file(file).await),
            Request::ReadParquetFooter(file, reply) => {
                reply.send(read_parquet_footer_impl(Arc::clone(&self.object_store), file).await)
            }
            Request::ReadJson(request) => respond_paged(self, request).await?,
            Request::ReadParquet(request) => respond_paged(self, request).await?,
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
            Request::Commit(_, reply) => reply.send(Err(KernelError::internal_error(
                "commit request requires a committer-aware driver",
            ))),
        }
        Ok(())
    }

    async fn read_small_file(&self, file: FileSlice) -> KernelResult<Option<Bytes>> {
        let start = Instant::now();
        let stream = read_files_impl(
            self.object_store.clone(),
            vec![file],
            self.storage_readahead,
        )
        .await?;
        let observe = |bytes: &Bytes| (1, bytes.len() as u64);
        let mut stream = metered_stream(stream, start, observe, emit_storage_read_completed);
        let data = stream.next().await.transpose()?;
        if stream.next().await.transpose()?.is_some() {
            return Err(KernelError::internal_error(
                "single-file read returned more than one result",
            ));
        }
        Ok(data)
    }

    async fn next_data<Op>(
        &self,
        mut stream: ResultStream<Box<dyn EngineData>>,
    ) -> KernelResult<Page<Op>>
    where
        Op: PagedOperation<Page = Vec<KernelResult<Box<dyn EngineData>>>>,
    {
        let (data, next) = match stream.next().await {
            Some(data) => (vec![data], Some(Cursor::new(stream))),
            None => (Vec::new(), None),
        };
        Ok(Page { data, next })
    }

    async fn next_listing_page<Op>(
        &self,
        mut stream: ResultStream<FileMeta>,
    ) -> KernelResult<Page<Op>>
    where
        Op: PagedOperation<Page = Vec<KernelResult<FileMeta>>>,
    {
        let mut data = Vec::with_capacity(ForwardListing::DEFAULT_PAGE_SIZE);
        while data.len() < ForwardListing::DEFAULT_PAGE_SIZE {
            let Some(entry) = stream.next().await else {
                break;
            };
            data.push(entry);
        }
        let next = (data.len() == ForwardListing::DEFAULT_PAGE_SIZE).then(|| Cursor::new(stream));
        Ok(Page { data, next })
    }
}

const STORAGE_READAHEAD: usize = 10;

type ResultStream<T> = BoxStream<'static, KernelResult<T>>;

type Observe<T> = fn(&T) -> (u64, u64);
type EmitMetric = fn(Duration, u64, u64);

/// Async pagination stored in opaque connector cursors.
trait AsyncPagination<Op: PagedOperation> {
    type State: Any + Send;

    async fn prepare(&self, operation: Op) -> KernelResult<Self::State>;

    async fn next(&self, state: Self::State) -> KernelResult<Page<Op>>;
}

async fn respond_paged<C, Op>(connector: &C, request: PageRequest<Op>) -> KernelResult<()>
where
    C: AsyncPagination<Op>,
    Op: PagedOperation,
{
    match request {
        PageRequest::Prepare(operation, reply) => {
            reply.send(connector.prepare(operation).await.map(Cursor::new));
        }
        PageRequest::Next(cursor, reply) => {
            let result = cursor
                .into_inner()
                .and_then_async(|state| connector.next(state))
                .await;
            reply.send(result);
        }
    }
    Ok(())
}

impl AsyncPagination<ForwardListing> for AsyncEngineConnector {
    type State = ResultStream<FileMeta>;

    async fn prepare(&self, ForwardListing(bounds): ForwardListing) -> KernelResult<Self::State> {
        let start = Instant::now();
        let stream = list_from_impl(Arc::clone(&self.object_store), bounds.low.clone()).await?;
        let observe = |_: &FileMeta| (1, 0);
        let emit = |elapsed, num_files, _| emit_storage_list_completed(elapsed, num_files);
        let stream = metered_stream(stream, start, observe, emit);
        let in_bounds =
            move |entry: &KernelResult<FileMeta>| std::future::ready(bounds.contains(entry));
        Ok(Box::pin(stream.take_while(in_bounds)))
    }

    async fn next(&self, stream: Self::State) -> KernelResult<Page<ForwardListing>> {
        self.next_listing_page(stream).await
    }
}

impl AsyncPagination<ReadJsonFiles> for AsyncEngineConnector {
    type State = ResultStream<Box<dyn EngineData>>;

    async fn prepare(&self, read: ReadJsonFiles) -> KernelResult<Self::State> {
        let num_files = read.files.len() as u64;
        let bytes_read = read.files.iter().map(|file| file.size).sum();
        let stream = read_json_files_impl(
            self.object_store.clone(),
            read.files,
            read.physical_schema,
            read.predicate,
            self.batch_size.get(),
            self.buffer_size.get(),
        )
        .await?;
        let emit = |_, num_files, bytes_read| emit_json_read_completed(num_files, bytes_read);
        Ok(precounted_stream(stream, num_files, bytes_read, emit))
    }

    async fn next(&self, stream: Self::State) -> KernelResult<Page<ReadJsonFiles>> {
        self.next_data(stream).await
    }
}

impl AsyncPagination<ReadParquetFiles> for AsyncEngineConnector {
    type State = ResultStream<Box<dyn EngineData>>;

    async fn prepare(&self, read: ReadParquetFiles) -> KernelResult<Self::State> {
        let num_files = read.files.len() as u64;
        let bytes_read = read.files.iter().map(|file| file.size).sum();
        let stream = read_parquet_files_impl(
            Arc::clone(&self.object_store),
            read.files,
            read.physical_schema,
            read.predicate,
            self.buffer_size.get(),
            self.batch_size.get(),
        )
        .await?;
        let emit = |_, num_files, bytes_read| emit_parquet_read_completed(num_files, bytes_read);
        Ok(precounted_stream(stream, num_files, bytes_read, emit))
    }

    async fn next(&self, stream: Self::State) -> KernelResult<Page<ReadParquetFiles>> {
        self.next_data(stream).await
    }
}

fn expression_evaluator(
    handle: &ExpressionEvaluatorHandle,
) -> KernelResult<&Arc<dyn ExpressionEvaluator>> {
    handle.downcast_ref().ok_or_else(|| {
        KernelError::internal_error("invalid async engine expression evaluator handle")
    })
}

fn predicate_evaluator(
    handle: &PredicateEvaluatorHandle,
) -> KernelResult<&Arc<dyn PredicateEvaluator>> {
    handle.downcast_ref().ok_or_else(|| {
        KernelError::internal_error("invalid async engine predicate evaluator handle")
    })
}

struct MeteredStream<T> {
    inner: ResultStream<T>,
    start: Instant,
    num_files: u64,
    bytes_read: u64,
    observe: Observe<T>,
    emit: EmitMetric,
}

impl<T> Stream for MeteredStream<T> {
    type Item = KernelResult<T>;

    fn poll_next(mut self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let item = self.inner.as_mut().poll_next(context);
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

#[cfg(test)]
mod tests {
    use delta_kernel::coroutine::generator::{BoundGenerator as _, GeneratorExt as _};
    use delta_kernel::coroutine::kernel::request::{ForwardListing, ListingBounds};
    use delta_kernel::coroutine::kernel::{static_workflow, ChannelExt as _, WorkflowStep};
    use delta_kernel::coroutine::PageGenerator;
    use delta_kernel::expressions::{col, lit};
    use delta_kernel::object_store::local::LocalFileSystem;
    use delta_kernel::object_store::memory::InMemory;
    use delta_kernel::object_store::path::Path;
    use delta_kernel::object_store::ObjectStoreExt as _;
    use delta_kernel::schema::{schema_ref, DataType};
    use delta_kernel::FileMeta;
    use url::Url;

    use super::*;

    #[tokio::test]
    async fn reads_small_file_through_native_async_storage() {
        let store: Arc<DynObjectStore> = Arc::new(InMemory::new());
        store
            .put(&Path::from("data"), Bytes::from_static(b"delta").into())
            .await
            .unwrap();
        let connector = AsyncEngineConnector::new(Arc::clone(&store));
        let workflow = static_workflow!(|channel| {
            channel.read_small_file(Url::parse("memory:///data")?, None)
        });

        let bytes = connector.run(workflow).await.unwrap().unwrap();

        assert_eq!(bytes, Bytes::from_static(b"delta"));
    }

    #[tokio::test]
    async fn missing_small_file_returns_file_not_found() {
        let connector = AsyncEngineConnector::new(Arc::new(InMemory::new()));
        let workflow = static_workflow!(|channel| {
            channel.read_small_file(Url::parse("memory:///missing")?, None)
        });

        let error = connector.run(workflow).await.unwrap_err();

        assert!(matches!(error, KernelError::FileNotFound(_)));
    }

    #[tokio::test]
    async fn respond_honors_configured_cancellation() {
        let token = CancellationToken::new();
        token.cancel();
        let connector =
            AsyncEngineConnector::new(Arc::new(InMemory::new())).with_cancellation_token(token);
        let mut workflow = static_workflow!(|channel| {
            channel.read_small_file(Url::parse("memory:///data")?, None)
        });
        let WorkflowStep::Request(request) = workflow.try_advance().unwrap() else {
            panic!("read workflow completed without a request");
        };

        let result = connector.respond(request).await;

        assert!(matches!(result, Err(KernelError::Cancelled)));
        assert!(workflow.advance().await.is_err());
    }

    #[tokio::test]
    async fn reads_json_through_native_async_storage() {
        let store: Arc<DynObjectStore> = Arc::new(InMemory::new());
        let data = Bytes::from_static(b"{\"value\":1}\n");
        store
            .put(&Path::from("data.json"), data.clone().into())
            .await
            .unwrap();
        let location = Url::parse("memory:///data.json").unwrap();
        let file = FileMeta::new(location, 0, data.len() as u64);
        let connector = AsyncEngineConnector::new(Arc::clone(&store));
        let workflow = static_workflow!(async move |channel| {
            let mut batches = channel
                .read_json_files([file], schema_ref! { nullable "value": LONG }, None)
                .await?;
            let batch = batches
                .next()
                .await?
                .ok_or_else(|| KernelError::generic("JSON read returned no data"))??;
            Ok((batch.len(), batches.next().await?.is_none()))
        });

        let result = connector.run(workflow).await.unwrap();

        assert_eq!(result, (1, true));
    }

    #[tokio::test]
    async fn creates_engine_data_through_native_async_connector() {
        let connector = AsyncEngineConnector::new(Arc::new(InMemory::new()));
        let workflow = static_workflow!(|channel| {
            channel.create_row(schema_ref! { nullable "value": LONG }, 1_i64)
        });

        let data = connector.run(workflow).await.unwrap();

        assert_eq!(data.len(), 1);
    }

    #[tokio::test]
    async fn evaluates_through_native_async_connector() {
        let connector = AsyncEngineConnector::new(Arc::new(InMemory::new()));
        let workflow = static_workflow!(async move |channel| {
            let input_schema = schema_ref! { nullable "value": LONG };
            let input = channel.create_row(input_schema.clone(), 1_i64).await?;
            let expression = channel
                .create_expression_evaluator(
                    input_schema,
                    Arc::new(col!("value") + lit(1_i64)),
                    DataType::LONG,
                )
                .await?;
            let output = channel.evaluate_expression(&expression, input).await?;
            let predicate = channel
                .create_predicate_evaluator(
                    schema_ref! { nullable "output": LONG },
                    Arc::new(col!("output").gt(lit(1_i64))),
                )
                .await?;
            channel.evaluate_predicate(&predicate, output).await
        });

        let data = connector.run(workflow).await.unwrap();

        assert_eq!(data.len(), 1);
    }

    #[tokio::test]
    async fn reads_parquet_through_native_async_storage() {
        let path = std::fs::canonicalize(
            "../kernel/tests/data/table-with-dv-small/\
             part-00000-fae5310a-a37d-4e51-827b-c3d5516560ca-c000.snappy.parquet",
        )
        .unwrap();
        let location = Url::from_file_path(&path).unwrap();
        let file = FileMeta::new(location, 0, std::fs::metadata(path).unwrap().len());
        let connector = AsyncEngineConnector::new(Arc::new(LocalFileSystem::new()));
        let workflow = static_workflow!(async move |channel| {
            let footer = channel.read_parquet_footer(&file).await?;
            let mut batches = channel
                .read_parquet_files([file], footer.schema, None)
                .await?;
            let mut rows = 0;
            while let Some(batch) = batches.next().await? {
                rows += batch?.len();
            }
            Ok(rows)
        });

        let rows = connector.run(workflow).await.unwrap();

        assert_eq!(rows, 10);
    }

    #[tokio::test]
    async fn lists_bounded_paths_through_native_async_storage() {
        let store: Arc<DynObjectStore> = Arc::new(InMemory::new());
        for path in [
            "00000000000000000000.json",
            "00000000000000000001.json",
            "_sidecars/part.parquet",
        ] {
            store
                .put(&Path::from(path), Bytes::new().into())
                .await
                .unwrap();
        }
        let connector = AsyncEngineConnector::new(Arc::clone(&store));
        let prefix = Url::parse("memory:///").unwrap();
        let bounds = ListingBounds {
            prefix: prefix.clone(),
            low: prefix.join("00000000000000000000").unwrap(),
            high: prefix.join("00000000000000000001").unwrap(),
        };
        let workflow = static_workflow!(async move |channel| {
            let cursor = channel.prepare_paged(ForwardListing::new(bounds)).await?;
            let mut files = channel.bind(PageGenerator::Next(cursor).flatten());
            let mut locations = Vec::new();
            while let Some(file) = files.next().await? {
                locations.push(file?.location);
            }
            Ok(locations)
        });

        let locations = connector.run(workflow).await.unwrap();

        assert_eq!(
            locations,
            [prefix.join("00000000000000000000.json").unwrap()]
        );
    }

    #[tokio::test]
    async fn cancellation_interrupts_the_whole_drive() {
        let token = CancellationToken::new();
        let cancel = token.clone();
        let connector =
            AsyncEngineConnector::new(Arc::new(InMemory::new())).with_cancellation_token(token);
        let workflow = static_workflow!(async move |_channel| {
            std::future::pending::<()>().await;
            Ok(())
        });
        tokio::spawn(async move {
            tokio::task::yield_now().await;
            cancel.cancel();
        });

        let result = connector.run(workflow).await;

        assert!(matches!(result, Err(KernelError::Cancelled)));
    }
}
