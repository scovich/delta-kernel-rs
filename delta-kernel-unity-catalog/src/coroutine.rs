//! Connector-driven Unity Catalog commit operations.
//!
//! [`Request::WriteJson`] carries a kernel commit-action generator. A connector handling that
//! request must drive both the write and the nested kernel requests produced by its input.

use delta_kernel::committer::{Commit, CommitActions, CommitResponse};
use delta_kernel::coroutine::kernel::request::{FileWriteMode, WriteJsonFile};
use delta_kernel::coroutine::{workflow, ChannelExchange, DeltaFuture, Reply, Workflow};
use delta_kernel::{FileMeta, FileSize};
use unity_catalog_delta_client_api::{TableIdentifier, UpdateTableClient, UpdateTableRequest};
use url::Url;

use crate::UCCommitter;

/// Operations a Unity Catalog commit may request from its connector.
pub enum Request {
    /// Write one newline-delimited JSON file from streamed commit actions.
    WriteJson(Box<WriteJsonFile>, Reply<FileSize>),
    /// Fetch metadata for one staged commit.
    Head(Url, Reply<FileMeta>),
    /// Apply one atomic Unity Catalog table update.
    UpdateTable(TableIdentifier, UpdateTableRequest, Reply<()>),
}

/// Unity Catalog commit request channel.
pub(crate) type Channel = delta_kernel::coroutine::Channel<Request>;

/// Typed Unity Catalog operations available on [`Channel`].
pub(crate) trait ChannelExt: ChannelExchange<Request> {
    /// Write generated commit actions to a new newline-delimited JSON file.
    fn write_commit_file(&self, url: &Url, actions: CommitActions) -> impl DeltaFuture<FileSize> {
        let operation = WriteJsonFile::new(url.clone(), FileWriteMode::CreateNew, actions);
        self.exchange(Box::new(operation), Request::WriteJson)
    }

    /// Fetch metadata for `location`.
    fn head(&self, location: &Url) -> impl DeltaFuture<FileMeta> {
        self.exchange(location.clone(), Request::Head)
    }

    /// Apply one atomic table update through Unity Catalog.
    fn update_table(
        &self,
        target: TableIdentifier,
        request: UpdateTableRequest,
    ) -> impl DeltaFuture<()> {
        self.exchange((target, request), |(target, request), reply| {
            Request::UpdateTable(target, request, reply)
        })
    }
}

impl ChannelExt for Channel {}

/// Connector-driven Unity Catalog commit workflow.
pub type CommitWorkflow<'a> = Workflow<'a, Request, CommitResponse>;

impl<C: UpdateTableClient + 'static> UCCommitter<C> {
    /// Return a lazy workflow that commits `commit` through Unity Catalog.
    pub fn commit_workflow(&self, commit: Commit) -> CommitWorkflow<'_> {
        workflow!(Request, async move |channel| {
            let (commit_metadata, actions) = commit.try_into_parts(true)?;
            self.commit_impl(channel, actions, commit_metadata).await
        })
    }
}
