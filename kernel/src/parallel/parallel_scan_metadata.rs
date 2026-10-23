use std::sync::Arc;
use std::time::Instant;

use delta_kernel_derive::internal_api;
use tracing::{info_span, Instrument as _, Span};

use crate::coroutine::generator::{generator_fn, AsyncIntoIterator as _};
use crate::coroutine::kernel::engine::{EngineConnector, EngineResultCoroutineIterator};
use crate::coroutine::kernel::{
    run_workflow_with_engine, static_workflow, workflow, Channel, KernelResultGenerator,
    StaticCoroutine, StaticInstrumentedCoroutine, StaticWorkflow, Workflow,
};
use crate::coroutine::KernelFuture;
use crate::log_replay::{ActionsBatch, ParallelLogReplayProcessor};
use crate::metrics::events::emit_scan_metadata_completed;
use crate::metrics::{MetricId, ScanType};
use crate::parallel::parallel_phase::ParallelPhase;
use crate::parallel::sequential_phase::AfterSequential;
use crate::scan::log_replay::{ScanLogReplayProcessor, SerializableScanState};
use crate::scan::ScanMetadata;
use crate::schema::SchemaRef;
use crate::{Engine, EngineData, FileMeta, KernelError, KernelResult};

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

type RawSequentialScanCoroutine =
    StaticCoroutine<KernelResult<ScanMetadata>, KernelResult<AfterSequential>>;
type SequentialScanEngineIterator =
    EngineResultCoroutineIterator<'static, ScanMetadata, KernelResult<AfterSequential>>;

/// Connector-driven first phase of distributed scan metadata replay.
///
/// Its terminal output either completes replay or returns the state and checkpoint files needed for
/// parallel processing.
/// cbindgen:ignore
pub type SequentialScanMetadata = StaticInstrumentedCoroutine<
    KernelResult<ScanMetadata>,
    KernelResult<AfterSequentialScanMetadata>,
>;

/// Creates the span shared by sequential connector and Engine drivers.
pub(crate) fn sequential_scan_span() -> Span {
    // TODO: Associate a unique scan ID with this span to correlate sequential and parallel phases.
    info_span!("sequential_scan_metadata")
}

/// Wraps sequential replay for connector-driven execution and terminal metrics reporting.
pub(crate) fn sequential_scan_metadata(
    sequential: impl KernelResultGenerator<ScanMetadata, KernelResult<AfterSequential>> + 'static,
    correlation_id: Option<Arc<str>>,
    span: Span,
) -> SequentialScanMetadata {
    let completion = SequentialScanCompletion::new(correlation_id);
    let generator = generator_fn!(async move |channel| {
        let after = channel.yield_from(sequential.bind(&channel)).await?;
        Ok(after.map(|after| completion.finish(after)))
    });
    let coroutine = StaticCoroutine::from_generator(generator);
    SequentialScanMetadata::new(coroutine, span)
}

/// Engine-driven compatibility iterator for sequential scan metadata.
///
/// Exhaust the iterator before calling [`Self::finish`] to recover any parallel work.
pub struct SequentialScanMetadataIterator {
    inner: SequentialScanEngineIterator,
    completion: SequentialScanCompletion,
    span: Span,
}

impl SequentialScanMetadataIterator {
    pub(crate) fn new(
        connector: EngineConnector,
        sequential: impl KernelResultGenerator<ScanMetadata, KernelResult<AfterSequential>> + 'static,
        correlation_id: Option<Arc<str>>,
        span: Span,
    ) -> Self {
        let raw: RawSequentialScanCoroutine = StaticCoroutine::from_generator(sequential);
        Self {
            inner: connector.iterate_result_coroutine(raw),
            completion: SequentialScanCompletion::new(correlation_id),
            span,
        }
    }

    /// Finishes the sequential phase, failing if the iterator has not been exhausted.
    pub fn finish(self) -> KernelResult<AfterSequentialScanMetadata> {
        let _guard = self.span.enter();
        if !self.inner.is_complete() {
            return Err(KernelError::generic(
                "Must exhaust iterator before calling finish()",
            ));
        }
        let after = self.inner.into_output()??;
        Ok(self.completion.finish(after))
    }
}

impl Iterator for SequentialScanMetadataIterator {
    type Item = KernelResult<ScanMetadata>;

    fn next(&mut self) -> Option<Self::Item> {
        let _guard = self.span.enter();
        self.inner.next()
    }
}

struct SequentialScanCompletion {
    operation_id: MetricId,
    /// Opaque, caller-supplied correlation id propagated to both phases' metric events.
    correlation_id: Option<Arc<str>>,
    start: Instant,
}

impl SequentialScanCompletion {
    fn new(correlation_id: Option<Arc<str>>) -> Self {
        Self {
            operation_id: MetricId::new(),
            correlation_id,
            start: Instant::now(),
        }
    }

    fn finish(self, after: AfterSequential) -> AfterSequentialScanMetadata {
        match after {
            AfterSequential::Done(processor) => {
                let event = processor.get_metrics().to_event(
                    self.operation_id,
                    processor.is_catalog_managed(),
                    self.correlation_id,
                    ScanType::SequentialPhase,
                    self.start.elapsed(),
                );
                processor
                    .get_metrics()
                    .log("Sequential scan metadata completed");
                emit_scan_metadata_completed(&event);
                AfterSequentialScanMetadata::Done
            }
            AfterSequential::Parallel { processor, files } => {
                let event = processor.get_metrics().to_event(
                    self.operation_id,
                    processor.is_catalog_managed(),
                    self.correlation_id.clone(),
                    ScanType::SequentialPhase,
                    self.start.elapsed(),
                );
                processor
                    .get_metrics()
                    .log("Sequential scan metadata completed");
                emit_scan_metadata_completed(&event);
                processor.get_metrics().reset_counters();

                AfterSequentialScanMetadata::Parallel {
                    state: Box::new(ParallelState {
                        inner: processor,
                        operation_id: self.operation_id,
                        correlation_id: self.correlation_id,
                        parallel_start: Instant::now(),
                    }),
                    files,
                }
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

    fn process_actions_batch_parallel(
        &self,
        channel: &Channel,
        actions_batch: ActionsBatch,
    ) -> impl KernelFuture<Self::Output> {
        self.inner
            .process_actions_batch_parallel(channel, actions_batch)
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

    /// Returns a workflow that processes `leaf_files` using this shared replay state.
    pub fn scan_metadata_workflow(
        self: Arc<Self>,
        leaf_files: Vec<FileMeta>,
    ) -> StaticWorkflow<ParallelScanMetadata> {
        static_workflow!(|channel| self.scan_metadata_with_channel(channel, leaf_files))
    }

    /// Processes pre-read action batches using this shared replay state.
    pub fn scan_metadata_from_iter(
        self: Arc<Self>,
        iter: impl IntoIterator<Item = KernelResult<Box<dyn EngineData>>, IntoIter: Send + 'static>,
    ) -> ParallelScanMetadata {
        let span = parallel_scan_span();
        span.in_scope(|| {
            let generator =
                ParallelPhase::process_batches(Arc::clone(&self), iter.into_generator());
            parallel_scan_metadata(generator, span.clone())
        })
    }

    async fn scan_metadata_with_channel(
        self: Arc<Self>,
        channel: &Channel,
        leaf_files: Vec<FileMeta>,
    ) -> KernelResult<ParallelScanMetadata> {
        let span = parallel_scan_span();
        let read_schema = self.file_read_schema();
        let generator = ParallelPhase::try_new_with_channel(channel, self, leaf_files, read_schema)
            .instrument(span.clone())
            .await?;
        Ok(span.in_scope(|| parallel_scan_metadata(generator, span.clone())))
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
    pub(crate) fn into_serializable_state(self) -> KernelResult<SerializableScanState> {
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
    ) -> KernelResult<Self> {
        run_workflow_with_engine!(engine, |channel| {
            Self::from_serializable_state_with_channel(channel, state)
        })
    }

    pub(crate) async fn from_serializable_state_with_channel(
        channel: &Channel,
        state: SerializableScanState,
    ) -> KernelResult<Self> {
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
    pub fn into_bytes(self) -> KernelResult<Vec<u8>> {
        let state = self.into_serializable_state()?;
        serde_json::to_vec(&state).map_err(|e| {
            KernelError::generic(format!("Failed to serialize ParallelState to bytes: {e}"))
        })
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
    pub fn from_bytes(engine: &dyn Engine, bytes: &[u8]) -> KernelResult<Self> {
        run_workflow_with_engine!(engine, |channel| {
            Self::from_bytes_with_channel(channel, bytes)
        })
    }

    /// Returns a workflow that reconstructs a ParallelState from bytes produced by
    /// [`Self::into_bytes`].
    pub fn from_bytes_workflow(bytes: &[u8]) -> Workflow<'_, Self> {
        workflow!(|channel| Self::from_bytes_with_channel(channel, bytes))
    }

    async fn from_bytes_with_channel(channel: &Channel, bytes: &[u8]) -> KernelResult<Self> {
        let state: SerializableScanState =
            serde_json::from_slice(bytes).map_err(KernelError::MalformedJson)?;
        Self::from_serializable_state_with_channel(channel, state).await
    }
}

type ParallelScanEngineIterator = EngineResultCoroutineIterator<'static, ScanMetadata>;

/// Connector-driven processing of one partition of checkpoint files or pre-read batches.
/// cbindgen:ignore
pub type ParallelScanMetadata = StaticInstrumentedCoroutine<KernelResult<ScanMetadata>>;

fn parallel_scan_span() -> Span {
    // TODO: Associate the same scan ID from sequential phase to correlate phases.
    info_span!("parallel_scan_metadata")
}

fn parallel_scan_metadata(
    generator: impl KernelResultGenerator<ScanMetadata> + 'static,
    span: Span,
) -> ParallelScanMetadata {
    let coroutine = StaticCoroutine::from_generator(generator);
    ParallelScanMetadata::new(coroutine, span)
}

/// Engine-driven compatibility iterator for one partition of parallel scan metadata.
pub struct ParallelScanMetadataIterator {
    inner: ParallelScanEngineIterator,
    span: Span,
}

impl ParallelScanMetadataIterator {
    /// Creates an Engine-driven iterator over `leaf_files` using shared `state`.
    pub fn try_new(
        engine: Arc<dyn Engine>,
        state: Arc<ParallelState>,
        leaf_files: Vec<FileMeta>,
    ) -> KernelResult<Self> {
        let connector = EngineConnector::new(engine.as_ref());
        let stream = connector.drive(state.scan_metadata_workflow(leaf_files))?;
        let (coroutine, span) = stream.into_parts();
        Ok(Self {
            inner: connector.iterate_result_coroutine(coroutine),
            span,
        })
    }
}

impl Iterator for ParallelScanMetadataIterator {
    type Item = KernelResult<ScanMetadata>;

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
            physical_stats_schemas: None,
            physical_partition_schema: None,
            eligible_physical_stats_columns: HashSet::new(),
            requested_physical_stats_columns: Vec::new(),
            is_catalog_managed: true,
            skip_row_transforms: false,
            dv_normalization_root: None,
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
