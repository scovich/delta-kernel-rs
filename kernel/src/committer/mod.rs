//! The `committer` module provides a [`Committer`] trait which allows different implementations to
//! define how to commit transactions to a catalog or filesystem. For catalog-managed tables, a
//! [`Committer`] specific to the managing catalog should be provided. For non-catalog-managed
//! tables, the [`FileSystemCommitter`] should be used to commit directly to the object store (via
//! put-if-absent call to storage to atomically write new commit files).
//!
//! By implementing the [`Committer`] trait, different catalogs can define what happens when the
//! kernel needs to commit a transaction to a table. The goal terminal state of every
//! [`Transaction`] is to be committed to the table. This means writing the changes (we call these
//! actions) in the transaction as a new version of the table. The [`Committer`] trait exposes
//! [`commit`] for a prepared [`Commit`] and, with declarative plans, `commit_declarative` for
//! ordered relation references. Both allow different catalogs to define what it means to commit
//! actions to a table.
//! For some, this may mean writing staged commits to object storage and retaining an in-memory list
//! (server side) of commits. For others, this may mean writing new (version, actions) tuples to a
//! database.
//!
//! Implementations must ensure that actions are committed atomically to the
//! table at the given version and either (1) persisted directly to object storage as published
//! deltas as in non-catalog-managed tables or (2) persisted within the catalog and made available
//! to readers during snapshot construction via the [`log_tail`] API.
//!
//! [`Transaction`]: crate::transaction::Transaction
//! [`commit`]: crate::committer::Committer::commit
//! [`log_tail`]: crate::snapshot::SnapshotBuilder::with_log_tail

mod commit_types;
#[cfg(feature = "declarative-plans")]
mod declarative;
mod filesystem;
mod publish_types;

pub use commit_types::{CommitMetadata, CommitProtocolMetadata, CommitResponse, CommitType};
#[cfg(feature = "declarative-plans")]
pub use declarative::write_commit_json_declarative;
pub use filesystem::FileSystemCommitter;
pub use publish_types::{CatalogCommit, PublishMetadata};

use crate::coroutine::generator::{AsyncIntoIterator as _, Generator};
use crate::coroutine::kernel::engine::EngineConnector;
use crate::coroutine::kernel::{Coroutine, Request};
#[cfg(feature = "declarative-plans")]
use crate::plans::{RelationRef, ScopedPlanExecutor};
#[cfg(feature = "declarative-plans")]
use crate::KernelError;
use crate::{Engine, FilteredEngineData, KernelResult, KernelResultIterator};

/// Unpolled actions for one prepared Delta log commit.
pub type CommitActions =
    Box<dyn Generator<Request, Item = KernelResult<FilteredEngineData>, Output = ()>>;

/// A Delta log commit ready for a connector-selected committer.
pub struct Commit {
    metadata: CommitMetadata,
    actions: CommitActions,
}

impl Commit {
    /// Creates a prepared commit from connector-driven actions and their metadata.
    pub(crate) fn new(metadata: CommitMetadata, actions: CommitActions) -> Self {
        Self { metadata, actions }
    }

    /// Creates a prepared commit from an Engine compatibility iterator.
    pub fn from_iterator(actions: CommitActionsIterator, metadata: CommitMetadata) -> Self {
        Self::new(metadata, Box::new(actions.into_generator()))
    }

    /// Validates the selected committer and returns this commit's metadata and actions.
    pub fn try_into_parts(
        self,
        is_catalog_committer: bool,
    ) -> KernelResult<(CommitMetadata, CommitActions)> {
        self.metadata.validate_committer(is_catalog_committer)?;
        Ok((self.metadata, self.actions))
    }

    /// Validates the selected committer and returns Engine-compatible metadata and actions.
    pub fn try_into_engine_parts(
        self,
        engine: &dyn Engine,
        is_catalog_committer: bool,
    ) -> KernelResult<(CommitMetadata, CommitActionsIterator)> {
        let (metadata, actions) = self.try_into_parts(is_catalog_committer)?;
        let actions = EngineConnector::new(engine)
            .iterate_result_coroutine(Coroutine::from_generator(actions));
        Ok((metadata, Box::new(actions)))
    }

    pub(crate) fn version(&self) -> crate::Version {
        self.metadata.version()
    }
}

/// Engine compatibility actions ready to be committed in Delta log order.
pub type CommitActionsIterator = Box<dyn KernelResultIterator<FilteredEngineData>>;

/// A Committer is the system by which transactions are committed to a table. Transactions are
/// effectively a collection of actions performed on the table at a specific version. The kernel
/// exposes this trait so different catalogs can build their own commit implementations. For
/// example, different catalogs may: commit directly to a database, commit to an object store, or
/// use another system entirely.
///
/// Critically, a Committer must implement [`commit`] which takes an engine and a prepared
/// [`Commit`] to commit to the table.
///
/// [`commit`]: Committer::commit
//
// Note: While we could omit the Send bound, we keep it here for simplicity - so usage can be
// Arc<dyn Committer> (instead of Arc<dyn Committer + Send>). If there is a strong case for a !Send
// Committer then we can remove this bound and possibly just do an alias like CommitterRef =
// Arc<dyn Committer + Send>.
pub trait Committer: Send {
    /// Commits a prepared transaction.
    ///
    /// Implementations must ensure that actions are committed atomically and either:
    /// 1. Persisted directly to object storage as published deltas (for filesystem-based tables),
    ///    or
    /// 2. Persisted as per the managing catalog's semantics (for catalog-managed tables)
    fn commit(&self, engine: &dyn Engine, commit: Commit) -> KernelResult<CommitResponse>;

    /// Commits `actions`, at the version in `commit_metadata`, using a declarative plan `executor`.
    ///
    /// `actions` provides an *ordered* collection of RelationRefs that should be committed.
    /// Each relation must have the same schema and remain valid within the provided `executor`
    /// until this method returns.
    /// Implementations must also provide the same atomicity and conflict behavior as `commit`.
    #[cfg(feature = "declarative-plans")]
    fn commit_declarative(
        &self,
        _executor: &dyn ScopedPlanExecutor,
        _actions: Box<dyn Iterator<Item = RelationRef> + '_>,
        _commit_metadata: CommitMetadata,
    ) -> KernelResult<CommitResponse> {
        Err(KernelError::unsupported(
            "This committer does not support declarative commits",
        ))
    }

    /// Returns `true` if this committer is for a catalog-managed table, else `false`.
    fn is_catalog_committer(&self) -> bool;

    /// Publishes catalog commits to the Delta log. Applicable only to catalog-managed tables.
    ///
    /// Publishing is the act of copying ratified catalog commits to the Delta log as published
    /// Delta files (e.g., `_delta_log/00000000000000000001.json`).
    ///
    /// # When to call
    ///
    /// This method should only be called on catalog committers (i.e., when [`is_catalog_committer`]
    /// returns `true`). Filesystem committers will error if called with catalog commits to publish.
    ///
    /// # Benefits
    ///
    /// - Reduces the number of commits the catalog needs to store internally and serve to readers
    /// - Enables table maintenance operations that must operate on published versions only, such as
    ///   checkpointing and log compaction
    ///
    /// # Requirements
    ///
    /// - This method must ensure that all catalog commits are published to the Delta log up to and
    ///   including the snapshot version specified in [`PublishMetadata`]
    /// - Commits must be published in order: version V-1 must be published before version V
    ///
    /// # Catalog-specific semantics
    ///
    /// Each catalog implementation may specify its own rules and semantics for publishing,
    /// including whether it expects to be notified immediately upon publishing success, whether
    /// published commits must appear with PUT-if-absent semantics in the Delta log, and whether
    /// publishing happens in the client-side or server-side catalog component.
    ///
    /// # Errors
    ///
    /// Returns an error if the publish operation fails.
    ///
    /// [`is_catalog_committer`]: Committer::is_catalog_committer
    fn publish(&self, engine: &dyn Engine, publish_metadata: PublishMetadata) -> KernelResult<()>;
}
