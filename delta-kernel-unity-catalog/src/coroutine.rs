//! Connector-driven Unity Catalog commit operations.
//!
//! [`Request::WriteJson`] carries a kernel commit-action coroutine. A connector handling that
//! request must drive both the write and any nested kernel requests produced by its input.

#[cfg(feature = "declarative-plans")]
use delta_kernel::committer::write_commit_json_declarative_workflow;
#[cfg(feature = "declarative-plans")]
use delta_kernel::committer::CommitMetadata;
use delta_kernel::committer::{Commit, CommitActions, CommitResponse, PublishMetadata};
use delta_kernel::coroutine::generator::GeneratorExt as _;
use delta_kernel::coroutine::kernel::request::WriteJsonFile;
use delta_kernel::coroutine::kernel::Request as KernelRequest;
#[cfg(feature = "declarative-plans")]
use delta_kernel::coroutine::kernel::StaticWorkflow as KernelWorkflow;
use delta_kernel::coroutine::{
    workflow, ChannelExchange, KernelFuture, Reply, StaticCoroutine, Workflow,
};
#[cfg(feature = "declarative-plans")]
use delta_kernel::plans::RelationRef;
use delta_kernel::{FileMeta, FileSize};
use unity_catalog_delta_client_api::{TableIdentifier, UpdateTableClient, UpdateTableRequest};
use url::Url;

use crate::UCCommitter;

/// Operations a Unity Catalog commit may request from its connector.
pub enum Request {
    /// Write one newline-delimited JSON file from streamed commit actions.
    WriteJson(Box<WriteJsonFile>, Reply<FileSize>),
    /// Execute a declarative JSON-write workflow.
    #[cfg(feature = "declarative-plans")]
    WriteDeclarative(KernelWorkflow<FileMeta>, Reply<FileMeta>),
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
    fn write_imperative_commit_file(
        &self,
        url: &Url,
        actions: CommitActions,
    ) -> impl KernelFuture<FileSize> {
        let actions = StaticCoroutine::from_generator(actions.promote_errors());
        let operation = WriteJsonFile::new(url.clone(), actions, false);
        self.exchange(Box::new(operation), Request::WriteJson)
    }

    #[cfg(feature = "declarative-plans")]
    fn write_declarative_commit_file(
        &self,
        url: &Url,
        actions: Vec<RelationRef>,
    ) -> impl KernelFuture<FileMeta> {
        let workflow = write_commit_json_declarative_workflow(actions, url.clone());
        self.exchange(workflow, Request::WriteDeclarative)
    }

    /// Fetch metadata for `location`.
    fn head(&self, location: &Url) -> impl KernelFuture<FileMeta> {
        self.exchange(location.clone(), Request::Head)
    }

    /// Apply one atomic table update through Unity Catalog.
    fn update_table(
        &self,
        target: TableIdentifier,
        request: UpdateTableRequest,
    ) -> impl KernelFuture<()> {
        self.exchange((target, request), |(target, request), reply| {
            Request::UpdateTable(target, request, reply)
        })
    }
}

impl ChannelExt for Channel {}

/// Connector-driven Unity Catalog commit workflow.
pub type CommitWorkflow<'a> = Workflow<'a, Request, CommitResponse>;

/// Connector-driven Unity Catalog publish workflow, which issues kernel requests.
pub type PublishWorkflow<'a> = Workflow<'a, KernelRequest, ()>;

impl<C: UpdateTableClient + 'static> UCCommitter<C> {
    /// Returns a lazy workflow that commits `commit` through Unity Catalog.
    pub fn commit_workflow(&self, commit: Commit) -> CommitWorkflow<'_> {
        workflow!(Request, async move |channel| {
            let (commit_metadata, actions) = commit.try_into_parts(true)?;
            self.commit_with_channel(channel, actions, commit_metadata)
                .await
        })
    }

    /// Returns a lazy workflow that commits declarative actions through Unity Catalog.
    #[cfg(feature = "declarative-plans")]
    pub fn commit_declarative_workflow(
        &self,
        actions: Vec<RelationRef>,
        commit_metadata: CommitMetadata,
    ) -> CommitWorkflow<'_> {
        workflow!(Request, async move |channel| {
            commit_metadata.validate_committer(true)?;
            self.commit_declarative_with_channel(channel, actions, commit_metadata)
                .await
        })
    }

    /// Returns a lazy workflow that publishes the staged commits in `publish_metadata`.
    /// Connectors that select this committer can serve [`KernelRequest::Publish`] by running it.
    pub fn publish_workflow(&self, publish_metadata: PublishMetadata) -> PublishWorkflow<'_> {
        workflow!(KernelRequest, |channel| {
            self.publish_with_channel(channel, publish_metadata)
        })
    }
}
