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
use delta_kernel::coroutine::kernel::request::ReadJsonFiles;
use delta_kernel::coroutine::kernel::{Request, Workflow};
use delta_kernel::coroutine::{
    drive_workflow_async, AsyncResult as _, Cursor, Page, PageRequest, PagedOperation,
};
use delta_kernel::metrics::{emit_json_read_completed, emit_storage_read_completed};
use delta_kernel::object_store::DynObjectStore;
use delta_kernel::{EngineData, FileSlice, KernelError, KernelResult};
use futures::stream::BoxStream;
use futures::{Stream, StreamExt as _};
use tokio_util::sync::CancellationToken;

use crate::filesystem::read_files_impl;
use crate::json::read_json_files_impl;
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

    /// Sets the maximum number of files read concurrently by JSON operations.
    pub fn with_buffer_size(mut self, buffer_size: NonZero<usize>) -> Self {
        self.buffer_size = buffer_size;
        self
    }

    /// Sets the maximum number of rows in each JSON batch.
    pub fn with_batch_size(mut self, batch_size: NonZero<usize>) -> Self {
        self.batch_size = batch_size;
        self
    }

    /// Serve every request from `workflow` until it completes.
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
            Request::ReadSmallFile(file, reply) => reply.send(self.read_small_file(file).await),
            Request::ReadJson(request) => respond_paged(self, request).await?,
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
    use delta_kernel::coroutine::generator::BoundGenerator as _;
    use delta_kernel::coroutine::kernel::{static_workflow, ChannelExt as _, WorkflowStep};
    use delta_kernel::object_store::memory::InMemory;
    use delta_kernel::object_store::path::Path;
    use delta_kernel::object_store::ObjectStoreExt as _;
    use delta_kernel::schema::schema_ref;
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
