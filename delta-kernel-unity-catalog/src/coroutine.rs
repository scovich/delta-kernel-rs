//! Connector-driven Unity Catalog commit operations.
//!
//! [`UCCommitter::commit_workflow`] posts these requests. A connector that serves kernel's
//! [`Request::Commit`] with a [`UCCommitter`] drives that workflow and serves each request as its
//! variant describes.
//!
//! [`Request::Commit`]: delta_kernel::coroutine::kernel::Request::Commit
//! [`UCCommitter`]: crate::UCCommitter
//! [`UCCommitter::commit_workflow`]: crate::UCCommitter::commit_workflow

#[cfg(feature = "declarative-plans")]
use delta_kernel::committer::write_commit_json_declarative_workflow;
use delta_kernel::committer::{CommitActionsGenerator, CommitResponse};
#[cfg(feature = "declarative-plans")]
use delta_kernel::coroutine::kernel::StaticWorkflow as KernelWorkflow;
use delta_kernel::coroutine::kernel::request::WriteJsonFile;
use delta_kernel::coroutine::{ChannelExchange, KernelFuture, Reply, StaticCoroutine, Workflow};
#[cfg(feature = "declarative-plans")]
use delta_kernel::plans::RelationRef;
use delta_kernel::{FileMeta, FileSize};
use unity_catalog_delta_client_api::{TableIdentifier, UpdateTableRequest};
use url::Url;

/// Declarative JSON-write payload for Unity Catalog commit workflows.
#[cfg(feature = "declarative-plans")]
pub type DeclarativeWrite = KernelWorkflow<FileMeta>;

/// Uninhabited declarative JSON-write payload used when declarative plans are disabled.
#[cfg(not(feature = "declarative-plans"))]
pub struct DeclarativeWrite(Uninhabited);

#[cfg(not(feature = "declarative-plans"))]
enum Uninhabited {}

/// Operations a Unity Catalog commit may request from its connector.
pub enum Request {
    /// Write one newline-delimited JSON file from streamed commit actions.
    ///
    /// The payload and reply match kernel's
    /// [`Request::WriteJson`](delta_kernel::coroutine::kernel::Request::WriteJson), so a
    /// connector can forward it unchanged as that kernel request. The input is a kernel coroutine,
    /// so serving it also drives the nested kernel requests that the input posts.
    WriteJson(Box<WriteJsonFile>, Reply<FileSize>),
    /// Execute a declarative JSON-write workflow.
    ///
    /// Only declarative commits post it. Without the `declarative-plans` feature, its payload is
    /// uninhabited, so a connector's match arm compiles with or without the feature.
    WriteDeclarative(DeclarativeWrite, Reply<FileMeta>),
    /// Fetch metadata for one staged commit.
    ///
    /// The payload and reply match kernel's
    /// [`Request::Head`](delta_kernel::coroutine::kernel::Request::Head), so a connector can
    /// forward it unchanged as that kernel request.
    Head(Url, Reply<FileMeta>),
    /// Apply one atomic Unity Catalog table update.
    ///
    /// The connector serves it with its own Unity Catalog client and maps client errors itself.
    /// The Engine shim does this through `execute_update_table_blocking`, and native connectors
    /// may produce different error text. This request never reaches the default engine's
    /// `AsyncEngineConnector`, so that connector's cancellation token doesn't cover it. To cancel
    /// a commit while this request is in flight, race your whole driver loop against your own
    /// cancellation token.
    UpdateTable(TableIdentifier, UpdateTableRequest, Reply<()>),
}

/// Unity Catalog commit request channel.
pub(crate) type Channel = delta_kernel::coroutine::Channel<Request>;

/// Typed Unity Catalog operations available on [`Channel`].
pub(crate) trait ChannelExt: ChannelExchange<Request> {
    /// Write generated commit actions to one newline-delimited JSON file.
    fn write_json_file(
        &self,
        url: &Url,
        actions: CommitActionsGenerator,
        overwrite: bool,
    ) -> impl KernelFuture<FileSize> {
        let actions = StaticCoroutine::from_generator(actions);
        let operation = WriteJsonFile::new(url.clone(), actions, overwrite);
        self.exchange(Box::new(operation), Request::WriteJson)
    }

    /// Write declarative commit-action relations to a new JSON file at `path`.
    #[cfg(feature = "declarative-plans")]
    fn write_commit_json_declarative(
        &self,
        actions: Vec<RelationRef>,
        path: &Url,
    ) -> impl KernelFuture<FileMeta> {
        let workflow = write_commit_json_declarative_workflow(actions, path.clone());
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
