//! The `committer` module provides a [`Committer`] trait which allows different implementations to
//! define how to commit transactions to a catalog or filesystem. For catalog-managed tables, a
//! [`Committer`] specific to the managing catalog should be provided. For non-catalog-managed
//! tables, the [`FileSystemCommitter`] should be used to commit directly to the object store (via
//! put-if-absent call to storage to atomically write new commit files).
//!
//! By implementing the [`Committer`] trait, different catalogs can define what happens when the
//! kernel needs to commit a transaction to a table. The goal terminal state of every
//! [`Transaction`] is to be committed to the table. This means writing the changes (we call these
//! actions) in the transaction as a new version of the table. The [`Committer`] trait exposes a
//! single method, [`commit`], which takes an engine and a prepared [`Commit`] to allow different
//! catalogs to define what it means to commit the actions to a table.
//! For some, this may mean writing staged commits to object storage and retaining an in-memory list
//! (server side) of commits. For others, this may mean writing new (version, actions) tuples to a
//! database.
//!
//! The implementation of [`commit`] must ensure that the actions are committed atomically to the
//! table at the given version and either (1) persisted directly to object storage as published
//! deltas as in non-catalog-managed tables or (2) persisted within the catalog and made available
//! to readers during snapshot contstruction via the [`log_tail`] API.
//!
//! [`Transaction`]: crate::transaction::Transaction
//! [`commit`]: crate::committer::Committer::commit
//! [`log_tail`]: crate::snapshot::SnapshotBuilder::with_log_tail

mod commit_types;
mod filesystem;
mod publish_types;

pub use commit_types::{CommitMetadata, CommitProtocolMetadata, CommitResponse, CommitType};
use derive_more::Constructor;
pub use filesystem::FileSystemCommitter;
pub use publish_types::{CatalogCommit, PublishMetadata};

use crate::coroutine::engine::EngineConnector;
use crate::coroutine::generator::BoundGenerator as _;
use crate::coroutine::kernel::{static_coroutine, StaticCoroutine};
use crate::coroutine::DeltaResultGenerator;
use crate::{DeltaResult, DeltaResultIteratorStatic, Engine, FilteredEngineData};

/// An unpolled generator of commit actions.
pub type CommitActions = StaticCoroutine<FilteredEngineData>;

/// A Delta log commit ready for a [`Committer`] to persist.
#[derive(Constructor)]
pub struct Commit {
    metadata: CommitMetadata,
    actions: CommitActions,
}

impl Commit {
    /// Construct a prepared commit from an Engine compatibility iterator.
    pub fn from_iterator(
        actions: DeltaResultIteratorStatic<FilteredEngineData>,
        metadata: CommitMetadata,
    ) -> Self {
        let actions = StaticCoroutine::from_iterator(actions);
        Self { metadata, actions }
    }

    /// Validate the selected committer and return this commit's metadata and actions.
    pub fn try_into_parts(
        self,
        is_catalog_committer: bool,
    ) -> DeltaResult<(CommitMetadata, CommitActions)> {
        self.metadata.validate_committer(is_catalog_committer)?;
        Ok((self.metadata, self.actions))
    }

    /// Validate the selected committer and return Engine-compatible metadata and actions.
    pub fn try_into_engine_parts(
        self,
        engine: &dyn Engine,
        is_catalog_committer: bool,
    ) -> DeltaResult<(
        CommitMetadata,
        DeltaResultIteratorStatic<FilteredEngineData>,
    )> {
        let (metadata, actions) = self.try_into_parts(is_catalog_committer)?;
        let actions = EngineConnector::new(engine).iterate_generator(actions);
        Ok((metadata, Box::new(actions)))
    }

    /// Construct a prepared commit by promoting yielded action errors to generator errors.
    pub(crate) fn from_result_generator(
        metadata: CommitMetadata,
        actions: impl DeltaResultGenerator<FilteredEngineData> + 'static,
    ) -> Self {
        let actions = static_coroutine!(async move |channel| {
            let mut actions = actions.bind(&channel);
            while let Some(action) = actions.next().await? {
                channel.yield_item(action?).await?;
            }
            Ok(())
        });
        Self { metadata, actions }
    }

    pub(crate) fn version(&self) -> crate::Version {
        self.metadata.version()
    }
}

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
    fn commit(&self, engine: &dyn Engine, commit: Commit) -> DeltaResult<CommitResponse>;

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
    fn publish(&self, engine: &dyn Engine, publish_metadata: PublishMetadata) -> DeltaResult<()>;
}
