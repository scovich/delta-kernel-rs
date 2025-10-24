use std::pin::Pin;
use std::sync::Arc;

use bytes::Bytes;
use delta_kernel_derive::internal_api;
use futures::stream::{self, Stream, StreamExt as _};
use itertools::Itertools;
use object_store::path::Path;
use object_store::{DynObjectStore, ObjectStore};
use url::Url;

use super::UrlExt;
use crate::engine::default::executor::TaskExecutor;
use crate::{async_trait, BoxedAsyncIterator, DeltaResult, Error, FileMeta, FileSlice, StorageHandler};

#[derive(Debug)]
pub struct ObjectStoreStorageHandler<E: TaskExecutor> {
    inner: Arc<DynObjectStore>,
    task_executor: Arc<E>,
    readahead: usize,
}

impl<E: TaskExecutor> ObjectStoreStorageHandler<E> {
    #[internal_api]
    pub(crate) fn new(store: Arc<DynObjectStore>, task_executor: Arc<E>) -> Self {
        Self {
            inner: store,
            task_executor,
            readahead: 10,
        }
    }

    /// Set the maximum number of files to read in parallel.
    pub fn with_readahead(mut self, readahead: usize) -> Self {
        self.readahead = readahead;
        self
    }

    /// Native async implementation for list_from
    async fn list_from_impl(
        &self,
        path: &Url,
    ) -> DeltaResult<Pin<Box<dyn Stream<Item = DeltaResult<FileMeta>> + Send>>> {
        // The offset is used for list-after; the prefix is used to restrict the listing to a specific directory.
        // Unfortunately, `Path` provides no easy way to check whether a name is directory-like,
        // because it strips trailing /, so we're reduced to manually checking the original URL.
        let offset = Path::from_url_path(path.path())?;
        let prefix = if path.path().ends_with('/') {
            offset.clone()
        } else {
            let mut parts = offset.parts().collect_vec();
            if parts.pop().is_none() {
                return Err(Error::Generic(format!(
                    "Offset path must not be a root directory. Got: '{}'",
                    path.as_str()
                )));
            }
            Path::from_iter(parts)
        };

        let store = self.inner.clone();

        // HACK to check if we're using a LocalFileSystem from ObjectStore. We need this because
        // local filesystem doesn't return a sorted list by default. Although the `object_store`
        // crate explicitly says it _does not_ return a sorted listing, in practice all the cloud
        // implementations actually do:
        // - AWS:
        //   [`ListObjectsV2`](https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListObjectsV2.html)
        //   states: "For general purpose buckets, ListObjectsV2 returns objects in lexicographical
        //   order based on their key names." (Directory buckets are out of scope for now)
        // - Azure: Docs state
        //   [here](https://learn.microsoft.com/en-us/rest/api/storageservices/enumerating-blob-resources):
        //   "A listing operation returns an XML response that contains all or part of the requested
        //   list. The operation returns entities in alphabetical order."
        // - GCP: The [main](https://cloud.google.com/storage/docs/xml-api/get-bucket-list) doc
        //   doesn't indicate order, but [this
        //   page](https://cloud.google.com/storage/docs/xml-api/get-bucket-list) does say: "This page
        //   shows you how to list the [objects](https://cloud.google.com/storage/docs/objects) stored
        //   in your Cloud Storage buckets, which are ordered in the list lexicographically by name."
        // So we just need to know if we're local and then if so, we sort the returned file list
        let has_ordered_listing = path.scheme() != "file";
        let url = path.clone();

        let stream = store.list_with_offset(Some(&prefix), &offset)
            .map(move |meta| {
                match meta {
                    Ok(meta) => {
                        let mut location = url.clone();
                        location.set_path(&format!("/{}", meta.location.as_ref()));
                        Ok(FileMeta {
                            location,
                            last_modified: meta.last_modified.timestamp_millis(),
                            size: meta.size,
                        })
                    }
                    Err(e) => Err(e.into()),
                }
            });

        if !has_ordered_listing {
            // Local filesystem doesn't return sorted list - need to collect and sort
            let items: Vec<_> = stream.collect().await;
            let mut sorted: Vec<FileMeta> = items.into_iter().try_collect()?;
            sorted.sort_unstable();
            Ok(Box::pin(stream::iter(sorted.into_iter().map(Ok))))
        } else {
            Ok(Box::pin(stream))
        }
    }

    /// Native async implementation for read_files
    async fn read_files_impl(
        &self,
        files: Vec<FileSlice>,
    ) -> DeltaResult<Pin<Box<dyn Stream<Item = DeltaResult<Bytes>> + Send>>> {
        let store = self.inner.clone();
        
        Ok(Box::pin(
            stream::iter(files)
                .map(move |(url, range)| {
                    let store = store.clone();
                    async move {
                        // Wasn't checking the scheme before calling to_file_path causing the url path to
                        // be eaten in a strange way. Now, if not a file scheme, just blindly convert to a path.
                        // https://docs.rs/url/latest/url/struct.Url.html#method.to_file_path has more
                        // details about why this check is necessary
                        let path = if url.scheme() == "file" {
                            let file_path = url.to_file_path().map_err(|_| {
                                Error::InvalidTableLocation(format!("Invalid file URL: {url}"))
                            })?;
                            Path::from_absolute_path(file_path).map_err(|e| {
                                Error::InvalidTableLocation(format!("Invalid file path: {e}"))
                            })?
                        } else {
                            Path::from(url.path())
                        };
                        if url.is_presigned() {
                            // have to annotate type here or rustc can't figure it out
                            Ok::<bytes::Bytes, Error>(reqwest::get(url).await?.bytes().await?)
                        } else if let Some(rng) = range {
                            Ok(store.get_range(&path, rng).await?)
                        } else {
                            let result = store.get(&path).await?;
                            Ok(result.bytes().await?)
                        }
                    }
                })
                // We allow executing up to `readahead` futures concurrently and
                // buffer the results. This allows us to achieve async concurrency.
                .buffered(self.readahead),
        ))
    }
}

#[async_trait]
impl<E: TaskExecutor> StorageHandler for ObjectStoreStorageHandler<E> {
    // Sync mode: pass future to helper which blocks internally
    #[cfg(not(feature = "async"))]
    fn list_from(
        &self,
        path: &Url,
    ) -> DeltaResult<BoxedAsyncIterator<DeltaResult<FileMeta>>> {
        super::into_boxed_async_iterator(self.list_from_impl(path))
    }

    // Async mode: await the future and return the boxed stream
    #[cfg(feature = "async")]
    async fn list_from(
        &self,
        path: &Url,
    ) -> DeltaResult<BoxedAsyncIterator<DeltaResult<FileMeta>>> {
        self.list_from_impl(path).await
    }

    // Sync mode: pass future to helper which blocks internally
    #[cfg(not(feature = "async"))]
    fn read_files(
        &self,
        files: Vec<FileSlice>,
    ) -> DeltaResult<BoxedAsyncIterator<DeltaResult<Bytes>>> {
        super::into_boxed_async_iterator(self.read_files_impl(files))
    }

    // Async mode: await the future and return the boxed stream
    #[cfg(feature = "async")]
    async fn read_files(
        &self,
        files: Vec<FileSlice>,
    ) -> DeltaResult<BoxedAsyncIterator<DeltaResult<Bytes>>> {
        self.read_files_impl(files).await
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Range;
    use std::time::Duration;

    use object_store::memory::InMemory;
    use object_store::{local::LocalFileSystem, ObjectStore};

    use test_utils::delta_path_for_version;

    use crate::engine::default::executor::tokio::TokioBackgroundExecutor;
    use crate::engine::default::DefaultEngine;
    use crate::utils::current_time_duration;
    use crate::{await_, into_async_iter, AsyncIterator as _, Engine as _};

    use super::*;

    #[tokio::test]
    async fn test_read_files() {
        let tmp = tempfile::tempdir().unwrap();
        let tmp_store = LocalFileSystem::new_with_prefix(tmp.path()).unwrap();

        let data = Bytes::from("kernel-data");
        tmp_store
            .put(&Path::from("a"), data.clone().into())
            .await
            .unwrap();
        tmp_store
            .put(&Path::from("b"), data.clone().into())
            .await
            .unwrap();
        tmp_store
            .put(&Path::from("c"), data.clone().into())
            .await
            .unwrap();

        let mut url = Url::from_directory_path(tmp.path()).unwrap();

        let store = Arc::new(LocalFileSystem::new());
        let executor = Arc::new(TokioBackgroundExecutor::new());
        let storage = ObjectStoreStorageHandler::new(store, executor);

        let mut slices: Vec<FileSlice> = Vec::new();

        let mut url1 = url.clone();
        url1.set_path(&format!("{}/b", url.path()));
        slices.push((url1.clone(), Some(Range { start: 0, end: 6 })));
        slices.push((url1, Some(Range { start: 7, end: 11 })));

        url.set_path(&format!("{}/c", url.path()));
        slices.push((url, Some(Range { start: 4, end: 9 })));
        dbg!("Slices are: {}", &slices);
        let iter = await_!(storage.read_files(slices)).unwrap();
        let data: Vec<Bytes> = await_!(iter.async_pin().async_try_collect()).unwrap();

        assert_eq!(data.len(), 3);
        assert_eq!(data[0], Bytes::from("kernel"));
        assert_eq!(data[1], Bytes::from("data"));
        assert_eq!(data[2], Bytes::from("el-da"));
    }

    #[tokio::test]
    async fn test_file_meta_is_correct() {
        let store = Arc::new(InMemory::new());

        let begin_time = current_time_duration().unwrap();

        let data = Bytes::from("kernel-data");
        let name = delta_path_for_version(1, "json");
        store.put(&name, data.clone().into()).await.unwrap();

        let table_root = Url::parse("memory:///").expect("valid url");
        let engine = DefaultEngine::new(store, Arc::new(TokioBackgroundExecutor::new()));
        let iter = await_!(engine
            .storage_handler()
            .list_from(&table_root.join("_delta_log").unwrap().join("0").unwrap()))
            .unwrap();
        let files: Vec<_> = await_!(iter.async_pin().async_try_collect()).unwrap();

        assert!(!files.is_empty());
        for meta in files.into_iter() {
            let meta_time = Duration::from_millis(meta.last_modified.try_into().unwrap());
            assert!(meta_time.abs_diff(begin_time) < Duration::from_secs(10));
        }
    }
    #[tokio::test]
    async fn test_default_engine_listing() {
        let tmp = tempfile::tempdir().unwrap();
        let tmp_store = LocalFileSystem::new_with_prefix(tmp.path()).unwrap();
        let data = Bytes::from("kernel-data");

        let expected_names: Vec<Path> =
            (0..10).map(|i| delta_path_for_version(i, "json")).collect();

        // put them in in reverse order
        for name in expected_names.iter().rev() {
            tmp_store.put(name, data.clone().into()).await.unwrap();
        }

        let url = Url::from_directory_path(tmp.path()).unwrap();
        let store = Arc::new(LocalFileSystem::new());
        let engine = DefaultEngine::new(store, Arc::new(TokioBackgroundExecutor::new()));
        let files = await_!(engine
            .storage_handler()
            .list_from(&url.join("_delta_log").unwrap().join("0").unwrap()))
            .unwrap();
        let mut len = 0;
        let mut zipped = files.async_zip(into_async_iter(expected_names)).async_pin();
        while let Some((file, expected)) = await_!(zipped.async_next()) {
            assert!(
                file.as_ref()
                    .unwrap()
                    .location
                    .path()
                    .ends_with(expected.as_ref()),
                "{} does not end with {}",
                file.unwrap().location.path(),
                expected
            );
            len += 1;
        }
        assert_eq!(len, 10, "list_from should have returned 10 files");
    }
}
