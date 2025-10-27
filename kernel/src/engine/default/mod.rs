//! # The Default Engine
//!
//! The default implementation of [`Engine`] is [`DefaultEngine`].
//!
//! The underlying implementations use asynchronous IO. Async tasks are run on
//! a separate thread pool, provided by the [`TaskExecutor`] trait. Read more in
//! the [executor] module.

use std::collections::HashMap;
use std::sync::Arc;

use object_store::DynObjectStore;
use url::Url;

use self::executor::TaskExecutor;
use self::filesystem::ObjectStoreStorageHandler;
use self::json::DefaultJsonHandler;
use self::parquet::DefaultParquetHandler;
use super::arrow_conversion::TryFromArrow as _;
use super::arrow_data::ArrowEngineData;
use super::arrow_expression::ArrowEvaluationHandler;
use crate::schema::Schema;
use crate::transaction::WriteContext;
use crate::{
    async_fn, await_, DeltaResult, Engine, EngineData, EvaluationHandler, JsonHandler,
    ParquetHandler, StorageHandler,
};

#[cfg(not(feature = "async"))]
use crate::{AsyncIterator, BoxedAsyncIterator};

pub mod executor;
pub mod file_stream;
pub mod filesystem;
pub mod json;
pub mod parquet;
pub mod storage;

/// Type alias for DefaultEngine in async mode (no executor needed).
#[cfg(feature = "async")]
pub type DefaultEngineAsync = DefaultEngine<()>;

/// Adapter for converting Stream-producing futures to BoxedAsyncIterator
///
/// In sync mode: blocks on the future and wraps the stream to block on each item.
/// In async mode: this should not be used (just await the future directly).
///
/// This is a utility for the default engine implementations (filesystem, json, parquet)
/// to bridge between the async object_store API and the sync StorageHandler trait.
///
/// Uses the provided TaskExecutor to spawn the stream in the background and bridge via channel.
/// This avoids nested block_on calls that can cause deadlocks.
#[cfg(not(feature = "async"))]
pub(crate) fn into_boxed_async_iterator<Fut, S, T, E: executor::TaskExecutor>(
    task_executor: Arc<E>,
    stream_future: Fut,
) -> DeltaResult<BoxedAsyncIterator<T>>
where
    Fut: std::future::Future<Output = DeltaResult<S>> + Send + 'static,
    S: futures::stream::Stream<Item = T> + Send + 'static,
    T: Send + 'static,
{
    use futures::stream::StreamExt as _;
    
    // Create the stream by blocking on the future
    let mut stream = Box::pin(task_executor.block_on(stream_future)?);
    
    // Create a channel to bridge async stream to sync iterator
    let (sender, receiver) = std::sync::mpsc::sync_channel(50);
    
    // Spawn the stream processing in the background
    let executor_for_block = task_executor.clone();
    task_executor.spawn(async move {
        while let Some(item) = stream.next().await {
            let sender_clone = sender.clone();
            let join_res = executor_for_block
                .spawn_blocking(move || sender_clone.send(item))
                .await;
            match join_res {
                Ok(Ok(())) => continue,
                Ok(Err(_)) => break, // Receiver dropped
                Err(_) => break,     // spawn_blocking failed
            }
        }
    });
    
    // Return the receiver as an iterator
    Ok(receiver.into_iter().into_boxed())
}

#[derive(Debug)]
pub struct DefaultEngine<E: TaskExecutor = executor::DefaultTaskExecutor> {
    object_store: Arc<DynObjectStore>,
    storage: Arc<ObjectStoreStorageHandler<E>>,
    json: Arc<DefaultJsonHandler<E>>,
    parquet: Arc<DefaultParquetHandler<E>>,
    evaluation: Arc<ArrowEvaluationHandler>,
}

impl DefaultEngine<executor::DefaultTaskExecutor> {
    /// Create a new [`DefaultEngine`] instance with the default executor.
    ///
    /// Uses the default task executor for the current mode:
    /// - In sync mode: `TokioBackgroundExecutor`
    /// - In async mode: no-op executor
    ///
    /// For custom executors, use [`DefaultEngine::new_with_executor`].
    ///
    /// # Parameters
    ///
    /// - `object_store`: The object store to use.
    pub fn new(object_store: Arc<DynObjectStore>) -> Self {
        Self::new_with_executor(object_store, Arc::default())
    }
}

impl<E: TaskExecutor> DefaultEngine<E> {
    /// Create a new [`DefaultEngine`] instance with a custom executor.
    ///
    /// Most users should use [`DefaultEngine::new`] instead. This method is only
    /// needed for specialized testing scenarios (e.g., multi-threaded executors).
    ///
    /// # Parameters
    ///
    /// - `object_store`: The object store to use.
    /// - `task_executor`: Used to spawn async IO tasks. See [executor::TaskExecutor].
    pub fn new_with_executor(object_store: Arc<DynObjectStore>, task_executor: Arc<E>) -> Self {
        Self {
            storage: Arc::new(ObjectStoreStorageHandler::new(
                object_store.clone(),
                task_executor.clone(),
            )),
            json: Arc::new(DefaultJsonHandler::new(
                object_store.clone(),
                task_executor.clone(),
            )),
            parquet: Arc::new(DefaultParquetHandler::new(
                object_store.clone(),
                task_executor,
            )),
            object_store,
            evaluation: Arc::new(ArrowEvaluationHandler {}),
        }
    }

    pub fn get_object_store_for_url(&self, _url: &Url) -> Option<Arc<DynObjectStore>> {
        Some(self.object_store.clone())
    }

    /// Write data to a parquet file in the table.
    ///
    /// This method transforms logical data to physical layout and writes it as a parquet file.
    /// 
    /// # Sync/Async Modes
    /// 
    /// - In sync mode: Blocks on the write operation using the task executor
    /// - In async mode: Awaits the write operation asynchronously
    #[async_fn]
    pub fn write_parquet(
        &self,
        data: &ArrowEngineData,
        write_context: &WriteContext,
        partition_values: HashMap<String, String>,
        data_change: bool,
    ) -> DeltaResult<Box<dyn EngineData>> {
        let transform = write_context.logical_to_physical();
        let input_schema = Schema::try_from_arrow(data.record_batch().schema())?;
        let output_schema = write_context.schema();
        let logical_to_physical_expr = self.evaluation_handler().new_expression_evaluator(
            input_schema.into(),
            transform.clone(),
            output_schema.clone().into(),
        );
        let physical_data = logical_to_physical_expr.evaluate(data)?;
        await_!(self.parquet.write_parquet_file(
            write_context.target_dir(),
            physical_data,
            partition_values,
            data_change,
        ))
    }
}

impl<E: TaskExecutor> Engine for DefaultEngine<E> {
    fn evaluation_handler(&self) -> Arc<dyn EvaluationHandler> {
        self.evaluation.clone()
    }

    fn storage_handler(&self) -> Arc<dyn StorageHandler> {
        self.storage.clone()
    }

    fn json_handler(&self) -> Arc<dyn JsonHandler> {
        self.json.clone()
    }

    fn parquet_handler(&self) -> Arc<dyn ParquetHandler> {
        self.parquet.clone()
    }
}

trait UrlExt {
    // Check if a given url is a presigned url and can be used
    // to access the object store via simple http requests
    fn is_presigned(&self) -> bool;
}

impl UrlExt for Url {
    fn is_presigned(&self) -> bool {
        matches!(self.scheme(), "http" | "https")
            && (
                // https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html
                // https://developers.cloudflare.com/r2/api/s3/presigned-urls/
                self
                .query_pairs()
                .any(|(k, _)| k.eq_ignore_ascii_case("X-Amz-Signature")) ||
                // https://learn.microsoft.com/en-us/rest/api/storageservices/create-user-delegation-sas#version-2020-12-06-and-later
                // note signed permission (sp) must always be present
                self
                .query_pairs().any(|(k, _)| k.eq_ignore_ascii_case("sp")) ||
                // https://cloud.google.com/storage/docs/authentication/signatures
                self
                .query_pairs().any(|(k, _)| k.eq_ignore_ascii_case("X-Goog-Credential")) ||
                // https://www.alibabacloud.com/help/en/oss/user-guide/upload-files-using-presigned-urls
                self
                .query_pairs().any(|(k, _)| k.eq_ignore_ascii_case("X-OSS-Credential"))
            )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::tests::test_arrow_engine;
    use crate::{async_test, await_};
    use object_store::local::LocalFileSystem;

    #[async_test]
    fn test_default_engine() {
        let tmp = tempfile::tempdir().unwrap();
        let url = Url::from_directory_path(tmp.path()).unwrap();
        let object_store = Arc::new(LocalFileSystem::new());
        let engine = DefaultEngine::new(object_store);
        await_!(test_arrow_engine(&engine, &url));
    }

    #[test]
    fn test_pre_signed_url() {
        let url = Url::parse("https://example.com?X-Amz-Signature=foo").unwrap();
        assert!(url.is_presigned());

        let url = Url::parse("https://example.com?sp=foo").unwrap();
        assert!(url.is_presigned());

        let url = Url::parse("https://example.com?X-Goog-Credential=foo").unwrap();
        assert!(url.is_presigned());

        let url = Url::parse("https://example.com?X-OSS-Credential=foo").unwrap();
        assert!(url.is_presigned());

        // assert that query keys are case insensitive
        let url = Url::parse("https://example.com?x-gooG-credenTIAL=foo").unwrap();
        assert!(url.is_presigned());

        let url = Url::parse("https://example.com?x-oss-CREDENTIAL=foo").unwrap();
        assert!(url.is_presigned());

        let url = Url::parse("https://example.com").unwrap();
        assert!(!url.is_presigned());
    }
}
