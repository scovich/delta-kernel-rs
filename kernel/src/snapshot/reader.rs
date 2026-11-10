//! Access to snapshot metadata that may require storage reads.

use std::collections::HashSet;

use delta_kernel_derive::internal_api;

use super::Snapshot;
use crate::actions::DomainMetadata;
use crate::clustering::ClusteringColumnInfo;
use crate::coroutine::kernel::{Channel, Workflow, run_workflow_with_engine, workflow};
use crate::engine::CancellableEngine;
use crate::expressions::ColumnName;
use crate::log_segment::DomainMetadataMap;
use crate::{CancellationTokenRef, Engine, KernelResult};

/// Reads snapshot metadata that may require storage access.
///
/// Use [`Snapshot::reader`] for domain metadata, clustering columns, application transactions,
/// row-tracking metadata, and commit timestamps. These getters may read storage when the required
/// metadata is not cached in the snapshot. In-memory getters such as [`Snapshot::schema`] and
/// [`Snapshot::version`] remain on the snapshot.
///
/// Getters run synchronously and may block on I/O. By default, they use the supplied engine's
/// normal cancellation and timeout behavior. The `*_with_cancellation` siblings add cooperative
/// cancellation to handler-backed reads; they do not make the getters asynchronous. Cached results
/// can be returned without checking the token, and interrupting in-flight I/O depends on the
/// engine. Creating a reader does not issue I/O.
///
/// The reader borrows its snapshot and retains no engine.
///
/// ```no_run
/// # use delta_kernel::{CancellationTokenRef, Engine, KernelResult, Snapshot};
/// # fn read_metadata(
/// #     snapshot: &Snapshot,
/// #     engine: &dyn Engine,
/// #     token: CancellationTokenRef,
/// # ) -> KernelResult<()> {
/// // Read synchronously with the engine's normal behavior.
/// let domain = snapshot.reader().get_domain_metadata("my-domain", engine)?;
/// // Share an operation's cancellation scope across related metadata reads.
/// let reader = snapshot.reader();
/// let version = reader.get_app_id_version_with_cancellation("my-app", engine, Some(token.clone()))?;
/// let timestamp = reader.get_timestamp_with_cancellation(engine, Some(token))?;
/// # Ok(())
/// # }
/// ```
pub struct SnapshotReader<'a> {
    /// Immutable table state shared independently of this read scope.
    snapshot: &'a Snapshot,
}

#[cfg(test)]
mod tests;

impl<'a> SnapshotReader<'a> {
    /// Returns the latest non-expired transaction version for `application_id`.
    ///
    /// Returns `None` if no transaction exists for the application or its latest transaction has
    /// expired under `delta.setTransactionRetentionDuration`.
    ///
    /// Uses an at-version CRC when it can answer the query. Otherwise, uses `engine` to scan only
    /// commits after a stale CRC with complete transaction state, skipping the checkpoint, or to
    /// replay the full log when no such CRC is available.
    ///
    /// Reports `SetTransactionLoadSuccess` or `SetTransactionLoadFailure`. Returns read,
    /// retention-property, or cancellation errors.
    ///
    /// Use [`Self::get_app_id_version_with_cancellation`] to add a per-call cancellation token.
    pub fn get_app_id_version(
        &self,
        application_id: &str,
        engine: &dyn Engine,
    ) -> KernelResult<Option<i64>> {
        self.snapshot.get_app_id_version(application_id, engine)
    }

    /// Returns the latest non-expired transaction version with a cancellation token.
    pub fn get_app_id_version_with_cancellation(
        &self,
        application_id: &str,
        engine: &dyn Engine,
        cancellation_token: Option<CancellationTokenRef>,
    ) -> KernelResult<Option<i64>> {
        if let Some(token) = cancellation_token {
            self.get_app_id_version(application_id, &CancellableEngine::new(engine, token))
        } else {
            self.get_app_id_version(application_id, engine)
        }
    }

    /// Returns a lazy workflow that gets the latest non-expired transaction version for
    /// `application_id`.
    ///
    /// Cancellation is the connector's.
    pub fn get_app_id_version_workflow(
        &self,
        application_id: impl Into<String>,
    ) -> Workflow<'a, Option<i64>> {
        let snapshot = self.snapshot;
        let application_id = application_id.into();
        workflow!(|channel| snapshot.get_app_id_version_with_channel(&application_id, channel))
    }

    /// Returns the latest configuration for the user-controlled `domain`, or `None` if absent.
    ///
    /// Uses `engine` to replay the log when needed. Returns an error for system-controlled
    /// `delta.*` domains, read failures, or cancellation.
    ///
    /// Use [`Self::get_domain_metadata_with_cancellation`] to add a per-call cancellation token.
    pub fn get_domain_metadata(
        &self,
        domain: &str,
        engine: &dyn Engine,
    ) -> KernelResult<Option<String>> {
        self.snapshot.get_domain_metadata(domain, engine)
    }

    /// Returns the latest domain configuration with a cancellation token.
    pub fn get_domain_metadata_with_cancellation(
        &self,
        domain: &str,
        engine: &dyn Engine,
        cancellation_token: Option<CancellationTokenRef>,
    ) -> KernelResult<Option<String>> {
        if let Some(token) = cancellation_token {
            self.get_domain_metadata(domain, &CancellableEngine::new(engine, token))
        } else {
            self.get_domain_metadata(domain, engine)
        }
    }

    /// Returns a lazy workflow that gets the latest configuration for the user-controlled
    /// `domain`, or `None` if absent.
    ///
    /// Cancellation is the connector's.
    pub fn get_domain_metadata_workflow(
        &self,
        domain: impl Into<String>,
    ) -> Workflow<'a, Option<String>> {
        let snapshot = self.snapshot;
        let domain = domain.into();
        workflow!(|channel| snapshot.get_domain_metadata_with_channel(&domain, channel))
    }

    /// Returns the row-tracking high-water mark, or `None` if no active row-tracking domain exists.
    ///
    /// Uses `engine` to load domain metadata when needed. Returns read, malformed-configuration,
    /// or cancellation errors. [`crate::ROW_TRACKING_INITIAL_HIGH_WATER_MARK`] indicates that no
    /// row IDs have been assigned yet.
    ///
    /// Use [`Self::get_row_tracking_high_water_mark_with_cancellation`] to add a per-call
    /// cancellation token.
    pub fn get_row_tracking_high_water_mark(
        &self,
        engine: &dyn Engine,
    ) -> KernelResult<Option<i64>> {
        self.snapshot.get_row_tracking_high_water_mark(engine)
    }

    /// Returns the row-tracking high-water mark with a cancellation token.
    pub fn get_row_tracking_high_water_mark_with_cancellation(
        &self,
        engine: &dyn Engine,
        cancellation_token: Option<CancellationTokenRef>,
    ) -> KernelResult<Option<i64>> {
        if let Some(token) = cancellation_token {
            self.get_row_tracking_high_water_mark(&CancellableEngine::new(engine, token))
        } else {
            self.get_row_tracking_high_water_mark(engine)
        }
    }

    /// Returns a lazy workflow that gets the row-tracking high-water mark, or `None` if no active
    /// row-tracking domain exists.
    ///
    /// Cancellation is the connector's.
    pub fn get_row_tracking_high_water_mark_workflow(&self) -> Workflow<'a, Option<i64>> {
        let snapshot = self.snapshot;
        workflow!(|channel| snapshot.get_row_tracking_high_water_mark_with_channel(channel))
    }

    /// Gets the row-tracking high-water mark through `channel`, or `None` if no active
    /// row-tracking domain exists.
    ///
    /// Cancellation is the connector's.
    #[internal_api]
    pub(crate) async fn get_row_tracking_high_water_mark_with_channel(
        &self,
        channel: &Channel,
    ) -> KernelResult<Option<i64>> {
        self.snapshot
            .get_row_tracking_high_water_mark_with_channel(channel)
            .await
    }

    /// Returns this version's timestamp in milliseconds since the Unix epoch.
    ///
    /// Uses the In-Commit Timestamp (ICT) when enabled, reading commit JSON through `engine` if
    /// needed; otherwise returns the retained commit-file modification time. Returns missing
    /// commit, invalid ICT configuration, read, or cancellation errors.
    ///
    /// Use [`Self::get_timestamp_with_cancellation`] to add a per-call cancellation token.
    pub fn get_timestamp(&self, engine: &dyn Engine) -> KernelResult<i64> {
        run_workflow_with_engine!(engine, |channel| self.get_timestamp_with_channel(channel))
    }

    /// Returns this version's timestamp with a cancellation token.
    pub fn get_timestamp_with_cancellation(
        &self,
        engine: &dyn Engine,
        cancellation_token: Option<CancellationTokenRef>,
    ) -> KernelResult<i64> {
        if let Some(token) = cancellation_token {
            self.get_timestamp(&CancellableEngine::new(engine, token))
        } else {
            self.get_timestamp(engine)
        }
    }

    /// Returns a lazy workflow that gets this snapshot's timestamp.
    ///
    /// Cancellation is the connector's.
    pub fn get_timestamp_workflow(&self) -> Workflow<'a, i64> {
        let snapshot = self.snapshot;
        workflow!(|channel| snapshot.get_timestamp_with_channel(channel))
    }

    /// Gets this version's timestamp through `channel`, in milliseconds since the Unix epoch.
    ///
    /// Cancellation is the connector's.
    #[internal_api]
    pub(crate) async fn get_timestamp_with_channel(&self, channel: &Channel) -> KernelResult<i64> {
        self.snapshot.get_timestamp_with_channel(channel).await
    }

    /// Returns clustering descriptors in domain order.
    ///
    /// Returns `None` when the clustering feature is absent or `delta.clustering` has no active
    /// entry. Each descriptor contains the physical name, logical name, and type. Uses `engine` to
    /// load domain metadata when needed. Returns malformed-metadata, column-resolution, read, or
    /// cancellation errors. An empty vector means the clustering domain names no columns.
    #[cfg_attr(not(feature = "internal-api"), allow(dead_code))]
    #[internal_api]
    pub(crate) fn get_clustering_column_infos(
        &self,
        engine: &dyn Engine,
    ) -> KernelResult<Option<Vec<ClusteringColumnInfo>>> {
        self.snapshot.get_clustering_column_infos(engine)
    }

    /// Returns a lazy workflow that gets clustering descriptors in domain order.
    ///
    /// Cancellation is the connector's.
    #[cfg_attr(not(feature = "internal-api"), expect(dead_code))]
    #[internal_api]
    pub(crate) fn get_clustering_column_infos_workflow(
        &self,
    ) -> Workflow<'a, Option<Vec<ClusteringColumnInfo>>> {
        let snapshot = self.snapshot;
        workflow!(|channel| snapshot.get_clustering_column_infos_with_channel(channel))
    }

    /// Returns physical clustering column names in domain order.
    ///
    /// Returns `None` when the clustering feature is absent or `delta.clustering` has no active
    /// entry. Uses `engine` to load domain metadata when needed and preserves column mapping.
    /// Returns malformed-metadata, read, or cancellation errors. An empty vector means the
    /// clustering domain names no columns.
    #[cfg_attr(not(feature = "internal-api"), allow(dead_code))]
    #[internal_api]
    pub(crate) fn get_physical_clustering_columns(
        &self,
        engine: &dyn Engine,
    ) -> KernelResult<Option<Vec<ColumnName>>> {
        self.snapshot.get_physical_clustering_columns(engine)
    }

    /// Gets physical clustering column names in domain order through `channel`.
    ///
    /// Cancellation is the connector's.
    #[internal_api]
    pub(crate) async fn get_physical_clustering_columns_with_channel(
        &self,
        channel: &Channel,
    ) -> KernelResult<Option<Vec<ColumnName>>> {
        self.snapshot
            .get_physical_clustering_columns_with_channel(channel)
            .await
    }

    /// Returns current metadata for `domains`, or all domains when the filter is `None`.
    ///
    /// Includes system-controlled domains and excludes removed entries. Uses an at-version CRC
    /// with complete domain state, or a partial CRC containing every domain in an explicit filter.
    /// Otherwise, uses `engine` to scan commits after a stale CRC with complete domain state and
    /// reconcile them against its map, skipping the checkpoint, or replays the full log when no
    /// such CRC is available.
    ///
    /// Reports `DomainMetadataLoadSuccess` or `DomainMetadataLoadFailure`. Returns read or
    /// cancellation errors.
    #[cfg_attr(not(feature = "internal-api"), allow(dead_code))]
    #[internal_api]
    pub(crate) fn get_domain_metadatas_internal(
        &self,
        engine: &dyn Engine,
        domains: Option<&HashSet<&str>>,
    ) -> KernelResult<DomainMetadataMap> {
        self.snapshot.get_domain_metadatas_internal(engine, domains)
    }

    /// Gets current metadata for `domains` through `channel`, or all domains when the filter is
    /// `None`.
    ///
    /// Cancellation is the connector's.
    #[internal_api]
    pub(crate) async fn get_domain_metadatas_internal_with_channel(
        &self,
        channel: &Channel,
        domains: Option<&HashSet<&str>>,
    ) -> KernelResult<DomainMetadataMap> {
        self.snapshot
            .get_domain_metadatas_internal_with_channel(channel, domains)
            .await
    }

    /// Returns the latest configuration for a user or system `domain`, or `None` if absent.
    ///
    /// Unlike [`Self::get_domain_metadata`], accepts `delta.*` domains. Uses `engine` to replay
    /// the log when needed and returns read or cancellation errors.
    #[cfg_attr(not(feature = "internal-api"), allow(dead_code))]
    #[internal_api]
    pub(crate) fn get_domain_metadata_internal(
        &self,
        domain: &str,
        engine: &dyn Engine,
    ) -> KernelResult<Option<String>> {
        self.snapshot.get_domain_metadata_internal(domain, engine)
    }

    /// Gets the latest configuration for a user or system `domain` through `channel`, or `None` if
    /// absent.
    ///
    /// Cancellation is the connector's.
    #[cfg_attr(not(feature = "internal-api"), expect(dead_code))]
    #[internal_api]
    pub(crate) async fn get_domain_metadata_internal_with_channel(
        &self,
        domain: &str,
        channel: &Channel,
    ) -> KernelResult<Option<String>> {
        self.snapshot
            .get_domain_metadata_internal_with_channel(domain, channel)
            .await
    }

    /// Returns all active user-controlled domain metadata, excluding `delta.*` domains.
    ///
    /// Uses `engine` to replay the log when needed and returns read or cancellation errors.
    #[cfg_attr(not(feature = "internal-api"), allow(dead_code))]
    #[internal_api]
    pub(crate) fn get_all_domain_metadata(
        &self,
        engine: &dyn Engine,
    ) -> KernelResult<Vec<DomainMetadata>> {
        self.snapshot.get_all_domain_metadata(engine)
    }

    /// Returns a lazy workflow that gets all active user-controlled domain metadata, excluding
    /// `delta.*` domains.
    ///
    /// Cancellation is the connector's.
    #[cfg_attr(not(feature = "internal-api"), expect(dead_code))]
    #[internal_api]
    pub(crate) fn get_all_domain_metadata_workflow(&self) -> Workflow<'a, Vec<DomainMetadata>> {
        let snapshot = self.snapshot;
        workflow!(|channel| snapshot.get_all_domain_metadata_with_channel(channel))
    }

    /// Gets all active user-controlled domain metadata through `channel`, excluding `delta.*`
    /// domains.
    ///
    /// Cancellation is the connector's.
    #[cfg_attr(not(feature = "internal-api"), expect(dead_code))]
    #[internal_api]
    pub(crate) async fn get_all_domain_metadata_with_channel(
        &self,
        channel: &Channel,
    ) -> KernelResult<Vec<DomainMetadata>> {
        self.snapshot
            .get_all_domain_metadata_with_channel(channel)
            .await
    }

    /// Returns this version's In-Commit Timestamp (ICT), or `None` when ICT is disabled.
    ///
    /// Uses `engine` to read the commit when no at-version CRC is available. Returns missing
    /// commit, invalid or missing ICT, read, or cancellation errors.
    #[cfg_attr(not(feature = "internal-api"), allow(dead_code))]
    #[internal_api]
    pub(crate) fn get_in_commit_timestamp(&self, engine: &dyn Engine) -> KernelResult<Option<i64>> {
        run_workflow_with_engine!(engine, |channel| {
            self.get_in_commit_timestamp_with_channel(channel)
        })
    }

    /// Returns a lazy workflow that gets this version's In-Commit Timestamp, or `None` when ICT is
    /// disabled.
    ///
    /// Cancellation is the connector's.
    #[cfg_attr(not(feature = "internal-api"), allow(dead_code))]
    #[internal_api]
    pub(crate) fn get_in_commit_timestamp_workflow(&self) -> Workflow<'a, Option<i64>> {
        let snapshot = self.snapshot;
        workflow!(|channel| snapshot.get_in_commit_timestamp_with_channel(channel))
    }

    /// Gets this version's In-Commit Timestamp through `channel`, or `None` when ICT is disabled.
    ///
    /// Cancellation is the connector's.
    #[internal_api]
    pub(crate) async fn get_in_commit_timestamp_with_channel(
        &self,
        channel: &Channel,
    ) -> KernelResult<Option<i64>> {
        self.snapshot
            .get_in_commit_timestamp_with_channel(channel)
            .await
    }

    /// Creates an unscoped metadata reader without issuing I/O.
    pub(super) fn new(snapshot: &'a Snapshot) -> Self {
        Self { snapshot }
    }
}
