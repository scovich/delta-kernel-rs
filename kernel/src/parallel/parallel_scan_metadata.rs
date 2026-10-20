use std::sync::Arc;
use std::time::Instant;

use delta_kernel_derive::internal_api;
use tracing::{info_span, Instrument as _, Span};

use crate::coroutine::engine::{EngineConnector, EngineResultGeneratorIterator};
use crate::coroutine::generator::AsyncIntoIterator as _;
use crate::coroutine::kernel::{
    static_workflow, Channel, CoroutineStep, DeltaResultGenerator, StaticCoroutine, StaticWorkflow,
};
use crate::coroutine::{run_workflow_with_engine, DeltaFuture};
use crate::log_replay::{ActionsBatch, ParallelLogReplayProcessor};
use crate::metrics::events::emit_scan_metadata_completed;
use crate::metrics::{MetricId, ScanType};
use crate::parallel::parallel_phase::ParallelPhase;
use crate::parallel::sequential_phase::AfterSequential;
use crate::scan::log_replay::{ScanLogReplayProcessor, SerializableScanState};
use crate::scan::ScanMetadata;
use crate::schema::SchemaRef;
use crate::{DeltaResult, Engine, EngineData, Error, FileMeta};

/// Result of sequential scan metadata processing.
///
/// This enum indicates whether distributed processing is needed:
/// - `Done`: All processing completed sequentially - no distributed phase needed.
/// - `Parallel`: Contains state and files for parallel processing.
pub enum AfterSequentialScanMetadata {
    Done,
    Parallel {
        state: Box<ParallelState>,
        files: Vec<FileMeta>,
    },
}

type SequentialScanGenerator =
    StaticCoroutine<DeltaResult<ScanMetadata>, DeltaResult<AfterSequential>>;
type SequentialScanEngineIterator =
    EngineResultGeneratorIterator<'static, ScanMetadata, DeltaResult<AfterSequential>>;

/// Connector-driven sequential scan metadata processing.
pub struct SequentialScanMetadata {
    inner: SequentialScanGenerator,
    operation_id: MetricId,
    /// Opaque, caller-supplied correlation id propagated to both phases' metric events.
    correlation_id: Option<Arc<str>>,
    start: Instant,
    span: Span,
}

impl SequentialScanMetadata {
    pub(crate) fn new(
        sequential: impl DeltaResultGenerator<ScanMetadata, DeltaResult<AfterSequential>> + 'static,
        correlation_id: Option<Arc<str>>,
    ) -> Self {
        Self {
            inner: StaticCoroutine::from_generator(sequential),
            operation_id: MetricId::new(),
            correlation_id,
            start: Instant::now(),
            // TODO: Associate a unique scan ID with this span to correlate sequential and parallel
            // phases
            span: info_span!("sequential_scan_metadata"),
        }
    }

    /// Advance until metadata is yielded, connector work is requested, or processing ends.
    ///
    /// # Errors
    ///
    /// Returns processing errors, or [`Error::WouldBlock`] if processing cannot make progress.
    pub fn try_advance(
        &mut self,
    ) -> DeltaResult<
        CoroutineStep<DeltaResult<ScanMetadata>, DeltaResult<AfterSequentialScanMetadata>>,
    > {
        let _guard = self.span.enter();
        let step = self.inner.try_advance()?;
        Ok(finish_sequential_step(
            step,
            self.operation_id,
            &mut self.correlation_id,
            self.start,
        ))
    }

    /// Asynchronously advance until metadata, connector work, or completion is available.
    ///
    /// # Errors
    ///
    /// Returns connector or processing errors.
    pub async fn advance(
        &mut self,
    ) -> DeltaResult<
        CoroutineStep<DeltaResult<ScanMetadata>, DeltaResult<AfterSequentialScanMetadata>>,
    > {
        let step = self.inner.advance().instrument(self.span.clone()).await?;
        let _guard = self.span.enter();
        Ok(finish_sequential_step(
            step,
            self.operation_id,
            &mut self.correlation_id,
            self.start,
        ))
    }

    pub(crate) fn into_engine_iterator(
        self,
        connector: EngineConnector,
    ) -> SequentialScanMetadataIterator {
        SequentialScanMetadataIterator {
            inner: connector.iterate_result_generator(self.inner),
            operation_id: self.operation_id,
            correlation_id: self.correlation_id,
            start: self.start,
            span: self.span,
        }
    }
}

/// Sequential scan metadata processing.
///
/// This phase processes commits and single-part checkpoint manifests sequentially. After
/// exhaustion, call [`Self::finish`] to determine whether a distributed phase is needed.
pub struct SequentialScanMetadataIterator {
    inner: SequentialScanEngineIterator,
    operation_id: MetricId,
    /// Opaque, caller-supplied correlation id propagated to both phases' metric events.
    correlation_id: Option<Arc<str>>,
    start: Instant,
    span: Span,
}

impl SequentialScanMetadataIterator {
    pub fn finish(self) -> DeltaResult<AfterSequentialScanMetadata> {
        let _guard = self.span.enter();
        if !self.inner.is_complete() {
            return Err(Error::generic(
                "Must exhaust iterator before calling finish()",
            ));
        }
        let after = self.inner.into_output()??;
        Ok(finish_sequential_scan_metadata(
            after,
            self.operation_id,
            self.correlation_id,
            self.start,
        ))
    }
}

impl Iterator for SequentialScanMetadataIterator {
    type Item = DeltaResult<ScanMetadata>;

    fn next(&mut self) -> Option<Self::Item> {
        let _guard = self.span.enter();
        self.inner.next()
    }
}

fn finish_sequential_step(
    step: CoroutineStep<DeltaResult<ScanMetadata>, DeltaResult<AfterSequential>>,
    operation_id: MetricId,
    correlation_id: &mut Option<Arc<str>>,
    start: Instant,
) -> CoroutineStep<DeltaResult<ScanMetadata>, DeltaResult<AfterSequentialScanMetadata>> {
    match step {
        CoroutineStep::Yield(item) => CoroutineStep::Yield(item),
        CoroutineStep::Request(request) => CoroutineStep::Request(request),
        CoroutineStep::Done(after) => {
            let correlation_id = correlation_id.take();
            CoroutineStep::Done(after.map(|after| {
                finish_sequential_scan_metadata(after, operation_id, correlation_id, start)
            }))
        }
    }
}

fn finish_sequential_scan_metadata(
    after: AfterSequential,
    operation_id: MetricId,
    correlation_id: Option<Arc<str>>,
    start: Instant,
) -> AfterSequentialScanMetadata {
    match after {
        AfterSequential::Done(processor) => {
            let event = processor.get_metrics().to_event(
                operation_id,
                processor.is_catalog_managed(),
                correlation_id,
                ScanType::SequentialPhase,
                start.elapsed(),
            );
            processor
                .get_metrics()
                .log("Sequential scan metadata completed");
            emit_scan_metadata_completed(&event);
            AfterSequentialScanMetadata::Done
        }
        AfterSequential::Parallel { processor, files } => {
            let event = processor.get_metrics().to_event(
                operation_id,
                processor.is_catalog_managed(),
                correlation_id.clone(),
                ScanType::SequentialPhase,
                start.elapsed(),
            );
            processor
                .get_metrics()
                .log("Sequential scan metadata completed");
            emit_scan_metadata_completed(&event);
            processor.get_metrics().reset_counters();

            AfterSequentialScanMetadata::Parallel {
                state: Box::new(ParallelState {
                    inner: processor,
                    operation_id,
                    correlation_id,
                    parallel_start: Instant::now(),
                }),
                files,
            }
        }
    }
}

/// State for parallel scan metadata processing.
///
/// This state can be serialized and distributed to remote workers, or wrapped
/// in Arc and shared across threads for local parallel processing.
pub struct ParallelState {
    inner: ScanLogReplayProcessor,
    /// Operation ID inherited from the sequential phase for event correlation.
    operation_id: MetricId,
    /// Opaque, caller-supplied correlation id inherited from the sequential phase. Does not
    /// survive the serialization boundary; a reconstructed state carries `None` (tracked in
    /// #2736).
    correlation_id: Option<Arc<str>>,
    /// Start time for the parallel phase, set when this state is created.
    parallel_start: Instant,
}

impl ParallelLogReplayProcessor for Arc<ParallelState> {
    type Output = ScanMetadata;

    fn process_actions_batch(
        &self,
        channel: &Channel,
        actions_batch: ActionsBatch,
    ) -> impl DeltaFuture<Self::Output> {
        self.inner.process_actions_batch(channel, actions_batch)
    }
}

impl ParallelState {
    /// Log the accumulated metrics from parallel processing.
    ///
    /// Call this after all parallel workers complete. The metrics will be logged
    /// in the current tracing span context.
    ///
    /// # Example
    ///
    /// ```no_run
    /// # use std::sync::Arc;
    /// # use delta_kernel::scan::ParallelState;
    /// # use tracing::instrument;
    /// #[instrument(skip_all, name = "parallel_scan")]
    /// async fn process(state: Arc<ParallelState>) {
    ///     // ... spawn workers that share Arc<ParallelState> ...
    ///     // ... wait for workers to complete ...
    ///
    ///     // Log accumulated metrics
    ///     state.log_metrics();
    /// }
    /// ```
    pub fn log_metrics(&self) {
        let event = self.inner.get_metrics().to_event(
            self.operation_id,
            self.inner.is_catalog_managed(),
            self.correlation_id.clone(),
            ScanType::ParallelPhase,
            self.parallel_start.elapsed(),
        );
        self.inner
            .get_metrics()
            .log("Parallel scan metadata completed");
        emit_scan_metadata_completed(&event);
    }

    /// Get the schema to use for reading checkpoint files.
    ///
    /// Returns the checkpoint read schema which may have stats excluded
    /// if skip_stats was enabled when the scan was created.
    pub fn file_read_schema(&self) -> SchemaRef {
        self.inner.checkpoint_info().checkpoint_read_schema.clone()
    }

    /// Serialize the processor state for distributed processing.
    ///
    /// Returns a `SerializableScanState` containing all information needed to
    /// reconstruct this state on remote compute nodes.
    ///
    /// # Errors
    /// Returns an error if the state cannot be serialized (e.g., contains opaque predicates).
    #[internal_api]
    #[allow(unused)]
    pub(crate) fn into_serializable_state(self) -> DeltaResult<SerializableScanState> {
        self.inner.into_serializable_state()
    }

    /// Reconstruct a ParallelState from serialized state.
    ///
    /// # Parameters
    /// - `engine`: Engine for creating evaluators and filters
    /// - `state`: The serialized state from a previous `into_serializable_state()` call
    #[internal_api]
    #[allow(unused)]
    pub(crate) fn from_serializable_state(
        engine: &dyn Engine,
        state: SerializableScanState,
    ) -> DeltaResult<Self> {
        run_workflow_with_engine!(engine, async move |channel| {
            Self::from_serializable_state_with_channel(channel, state).await
        })
    }

    /// Reconstruct parallel state from serialized state through `channel`.
    pub(crate) async fn from_serializable_state_with_channel(
        channel: &Channel,
        state: SerializableScanState,
    ) -> DeltaResult<Self> {
        let inner =
            ScanLogReplayProcessor::from_serializable_state_with_channel(channel, state).await?;
        Ok(Self {
            inner,
            // Neither operation_id nor correlation_id survive the serialization boundary today; a
            // reconstructed state starts a fresh operation_id and has no correlation_id (#2736).
            operation_id: MetricId::new(),
            correlation_id: None,
            parallel_start: Instant::now(),
        })
    }

    /// Serialize the processor state directly to bytes.
    ///
    /// This is a convenience method that combines `into_serializable_state()` with
    /// JSON serialization. For more control over serialization format, use
    /// `into_serializable_state()` directly.
    ///
    /// # Errors
    /// Returns an error if the state cannot be serialized.
    #[allow(unused)]
    pub fn into_bytes(self) -> DeltaResult<Vec<u8>> {
        let state = self.into_serializable_state()?;
        serde_json::to_vec(&state)
            .map_err(|e| Error::generic(format!("Failed to serialize ParallelState to bytes: {e}")))
    }

    /// Reconstruct a ParallelState from bytes.
    ///
    /// This is a convenience method that combines JSON deserialization with
    /// `from_serializable_state()`. The bytes must have been produced by `into_bytes()`.
    ///
    /// # Parameters
    /// - `engine`: Engine for creating evaluators and filters
    /// - `bytes`: The serialized bytes from a previous `into_bytes()` call
    #[allow(unused)]
    pub fn from_bytes(engine: &dyn Engine, bytes: &[u8]) -> DeltaResult<Self> {
        let state: SerializableScanState =
            serde_json::from_slice(bytes).map_err(Error::MalformedJson)?;
        Self::from_serializable_state(engine, state)
    }
}

type ParallelScanGenerator = StaticCoroutine<DeltaResult<ScanMetadata>>;
type ParallelScanEngineIterator = EngineResultGeneratorIterator<'static, ScanMetadata>;

/// Connector-driven parallel scan metadata processing.
pub struct ParallelScanMetadata {
    inner: ParallelScanGenerator,
    span: Span,
}

impl ParallelScanMetadata {
    /// Return a workflow that reads and processes `leaf_files` using shared `state`.
    ///
    /// # Errors
    ///
    /// When driven, returns checkpoint-read or action-processing errors.
    pub fn try_new_workflow(
        state: Arc<ParallelState>,
        leaf_files: Vec<FileMeta>,
    ) -> StaticWorkflow<Self> {
        static_workflow!(async move |channel| {
            Self::try_new_impl(channel, state, leaf_files).await
        })
    }

    async fn try_new_impl(
        channel: &Channel,
        state: Arc<ParallelState>,
        leaf_files: Vec<FileMeta>,
    ) -> DeltaResult<Self> {
        let read_schema = state.file_read_schema();
        let generator =
            ParallelPhase::try_new_with_channel(channel, state, leaf_files, read_schema).await?;
        Ok(Self {
            inner: StaticCoroutine::from_generator(generator),
            // TODO: Associate the same scan ID from sequential phase to correlate phases
            span: info_span!("parallel_scan_metadata"),
        })
    }

    /// Create a parallel metadata stream from pre-read action batches.
    pub fn new_from_iter(
        state: Arc<ParallelState>,
        iter: impl IntoIterator<Item = DeltaResult<Box<dyn EngineData>>, IntoIter: Send + 'static>,
    ) -> Self {
        let generator = ParallelPhase::process_batches(Arc::clone(&state), iter.into_generator());
        Self {
            inner: StaticCoroutine::from_generator(generator),
            // TODO: Associate the same scan ID from sequential phase to correlate phases
            span: info_span!("parallel_scan_metadata"),
        }
    }

    /// Advance until metadata is yielded, connector work is requested, or processing ends.
    ///
    /// # Errors
    ///
    /// Returns processing errors, or [`Error::WouldBlock`] if processing cannot make progress.
    pub fn try_advance(&mut self) -> DeltaResult<CoroutineStep<DeltaResult<ScanMetadata>>> {
        let _guard = self.span.enter();
        self.inner.try_advance()
    }

    /// Asynchronously advance until metadata, connector work, or completion is available.
    ///
    /// # Errors
    ///
    /// Returns connector or processing errors.
    pub async fn advance(&mut self) -> DeltaResult<CoroutineStep<DeltaResult<ScanMetadata>>> {
        self.inner.advance().instrument(self.span.clone()).await
    }

    fn into_engine_iterator(self, connector: EngineConnector) -> ParallelScanMetadataIterator {
        ParallelScanMetadataIterator {
            inner: connector.iterate_result_generator(self.inner),
            span: self.span,
        }
    }
}

/// Engine iterator compatibility adapter for parallel scan metadata.
pub struct ParallelScanMetadataIterator {
    inner: ParallelScanEngineIterator,
    span: Span,
}

impl ParallelScanMetadataIterator {
    /// Create an Engine-driven iterator over `leaf_files` using shared `state`.
    ///
    /// # Errors
    ///
    /// Returns checkpoint preparation errors.
    pub fn try_new(
        engine: Arc<dyn Engine>,
        state: Arc<ParallelState>,
        leaf_files: Vec<FileMeta>,
    ) -> DeltaResult<Self> {
        let connector = EngineConnector::new(engine.as_ref());
        let stream = connector.drive(ParallelScanMetadata::try_new_workflow(state, leaf_files))?;
        Ok(stream.into_engine_iterator(connector))
    }
}

impl Iterator for ParallelScanMetadataIterator {
    type Item = DeltaResult<ScanMetadata>;

    fn next(&mut self) -> Option<Self::Item> {
        let _guard = self.span.enter();
        self.inner.next()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use std::sync::Arc;

    use super::ParallelState;
    use crate::engine::sync::SyncEngine;
    use crate::log_segment::CheckpointReadInfo;
    use crate::metrics::{MetricEvent, ScanType, TableType};
    use crate::scan::log_replay::{
        ScanLogReplayProcessor, ScanPartitionValuesOptions, ScanStatsOptions,
    };
    use crate::scan::state_info::StateInfo;
    use crate::scan::PhysicalPredicate;
    use crate::schema::{schema_ref, SchemaRef};
    use crate::table_features::ColumnMappingMode;
    use crate::unit_test_utils::{install_thread_local_metrics_reporter, CapturingReporter};

    #[test]
    fn test_parallel_state_log_metrics_carries_round_tripped_table_type() {
        let engine = SyncEngine::new();
        let schema: SchemaRef = schema_ref! {
            nullable "id": INTEGER,
        };
        let state_info = Arc::new(StateInfo {
            logical_schema: schema.clone(),
            physical_schema: schema,
            physical_predicate: PhysicalPredicate::None,
            transform_spec: None,
            column_mapping_mode: ColumnMappingMode::None,
            physical_stats_schema: None,
            physical_partition_schema: None,
            eligible_physical_stats_columns: HashSet::new(),
            requested_physical_stats_columns: Vec::new(),
            is_catalog_managed: true,
            skip_row_transforms: false,
        });
        let processor = ScanLogReplayProcessor::new(
            &engine,
            state_info,
            CheckpointReadInfo::without_stats_parsed(),
            ScanStatsOptions::default(),
            ScanPartitionValuesOptions::default(),
        )
        .unwrap();
        let serialized = processor.into_serializable_state().unwrap();
        let state = ParallelState::from_serializable_state(&engine, serialized).unwrap();

        let reporter = Arc::new(CapturingReporter::default());
        let _guard = install_thread_local_metrics_reporter(reporter.clone());
        state.log_metrics();

        let event = reporter
            .events()
            .into_iter()
            .find_map(|e| match e {
                MetricEvent::ScanMetadataCompleted(c) => Some(c),
                _ => None,
            })
            .expect("ScanMetadataCompleted emitted");
        assert_eq!(event.table_type, TableType::CatalogManaged);
        assert_eq!(event.scan_type, ScanType::ParallelPhase);
    }
}
