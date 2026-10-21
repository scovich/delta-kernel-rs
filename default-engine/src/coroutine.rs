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

use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::future::Future;
use std::num::NonZero;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll};
use std::time::{Duration, Instant};

use bytes::Bytes;
use delta_kernel::coroutine::drive_workflow_async;
use delta_kernel::coroutine::kernel::{Request, Workflow};
use delta_kernel::metrics::emit_storage_read_completed;
use delta_kernel::object_store::DynObjectStore;
use delta_kernel::{FileSlice, KernelError, KernelResult};
use futures::stream::BoxStream;
use futures::{Stream, StreamExt as _};
use tokio_util::sync::CancellationToken;

use crate::filesystem::read_files_impl;

/// Drives Kernel workflows one request at a time through the default engine's native async I/O.
///
/// This connector does not use [`delta_kernel::Engine`] or a
/// [`crate::executor::TaskExecutor`].
#[derive(Clone)]
pub struct AsyncEngineConnector {
    object_store: Arc<DynObjectStore>,
    cancellation_token: Option<CancellationToken>,
    storage_readahead: usize,
}

impl Debug for AsyncEngineConnector {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_struct("AsyncEngineConnector")
            .field("cancellation_token", &self.cancellation_token)
            .field("storage_readahead", &self.storage_readahead)
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
}

const STORAGE_READAHEAD: usize = 10;

type ResultStream<T> = BoxStream<'static, KernelResult<T>>;

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

#[cfg(test)]
mod tests {
    use delta_kernel::coroutine::kernel::{static_workflow, ChannelExt as _, WorkflowStep};
    use delta_kernel::object_store::memory::InMemory;
    use delta_kernel::object_store::path::Path;
    use delta_kernel::object_store::ObjectStoreExt as _;
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
