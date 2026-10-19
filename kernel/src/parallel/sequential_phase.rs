//! Sequential log replay processor that happens before the parallel phase.
//!
//! This module provides sequential phase log replay that processes commits and
//! single-part checkpoint manifests, then returns the processor and any files (sidecars or
//! multi-part checkpoint parts) for parallel processing by the parallel phase. This phase
//! must be completed before the parallel phase can start.
//!
//! For multi-part checkpoints, the sequential phase skips manifest processing and returns
//! the checkpoint parts for parallel processing.
#![allow(unused)]

use std::future::poll_fn;
use std::pin::Pin;

use delta_kernel_derive::internal_api;
use itertools::Itertools;

use crate::coroutine::generator::{
    generator_fn, BoundGenerator, Generator as _, GeneratorExt as _, GeneratorStep,
};
use crate::coroutine::kernel::{BoundDeltaResultGenerator, Channel, DeltaResultGenerator};
use crate::log_reader::checkpoint_manifest::checkpoint_manifest_reader_impl;
use crate::log_replay::ActionsBatch;
use crate::log_segment::LogSegment;
use crate::scan::log_replay::ScanLogReplayProcessor;
use crate::scan::{ScanMetadata, COMMIT_READ_SCHEMA};
use crate::schema::SchemaRef;
use crate::utils::require;
use crate::{DeltaResult, Error, FileMeta};

/// Sequential log replay state for parallel execution.
///
/// This state processes log replay sequentially:
/// 1. Commit files (JSON)
/// 2. Manifest (single-part checkpoint, if present)
///
/// Its terminal output contains:
/// - The processor (for serialization and distribution)
/// - Files (sidecars or multi-part checkpoint parts) for parallel processing
/// cbindgen:ignore
#[internal_api]
pub(crate) struct SequentialPhase<C, M> {
    // The processor that will be used to process the action batches
    processor: ScanLogReplayProcessor,
    // Commit action batches, exhausted before the checkpoint manifest
    commit_phase: Option<C>,
    // The checkpoint manifest generator for a single-part checkpoint
    checkpoint_manifest_phase: Option<M>,
    // Terminal sidecar output, populated when checkpoint_manifest_phase completes
    checkpoint_manifest_output: Option<DeltaResult<Vec<FileMeta>>>,
    // Whether the sequential state has been fully exhausted
    is_finished: bool,
    // Checkpoint parts for potential parallel phase processing
    checkpoint_parts: Vec<FileMeta>,
}

/// Result of sequential log replay processing.
/// cbindgen:ignore
#[internal_api]
pub(crate) enum AfterSequential {
    /// All processing complete sequentially - no parallel phase needed.
    Done(ScanLogReplayProcessor),
    /// Parallel phase needed - distribute files for parallel processing.
    Parallel {
        processor: ScanLogReplayProcessor,
        files: Vec<FileMeta>,
    },
}

impl SequentialPhase<(), ()> {
    /// Create a new sequential phase log replay generator.
    ///
    /// # Parameters
    /// - `channel`: Connector channel for reading files
    /// - `processor`: The log replay processor
    /// - `log_segment`: The log segment to process
    /// - `checkpoint_read_schema`: Schema for checkpoint manifests and leaf files
    pub(crate) async fn try_new_with_channel(
        channel: &Channel,
        processor: ScanLogReplayProcessor,
        log_segment: &LogSegment,
        checkpoint_read_schema: SchemaRef,
    ) -> DeltaResult<impl DeltaResultGenerator<ScanMetadata, DeltaResult<AfterSequential>> + 'static>
    {
        let commit_phase = log_segment
            .read_commit_actions_generator(channel, COMMIT_READ_SCHEMA.clone())
            .await?;
        // Concurrently start reading the checkpoint manifest. Only create a checkpoint manifest
        // reader if the checkpoint is single-part.
        let checkpoint_manifest_phase = match log_segment.listed.checkpoint_parts.as_slice() {
            [single_part] => Some(
                checkpoint_manifest_reader_impl(
                    channel,
                    single_part,
                    log_segment.log_root.clone(),
                    checkpoint_read_schema,
                )
                .await?,
            ),
            _ => None,
        };

        let checkpoint_parts = log_segment
            .listed
            .checkpoint_parts
            .iter()
            .map(|path| path.location.clone())
            .collect_vec();

        let state = SequentialPhase {
            processor,
            commit_phase: Some(commit_phase),
            checkpoint_manifest_phase,
            checkpoint_manifest_output: None,
            is_finished: false,
            checkpoint_parts,
        };
        Ok(generator_fn!(async move |channel| {
            let mut state = state.bind(&channel);
            while let Some(item) = state.advance(&channel).await? {
                channel.yield_item(item).await?;
            }
            Ok(state.finish())
        }))
    }
}

impl<C, M> SequentialPhase<C, M> {
    /// Complete sequential phase and extract processor + files for distribution.
    ///
    /// Must be called after the state is exhausted.
    ///
    /// # Returns
    /// - `Done`: All processing done sequentially - no parallel phase needed
    /// - `Parallel`: Parallel phase needed. The resulting files may be processed in parallel.
    ///
    /// # Errors
    /// Returns an error if called before exhaustion.
    fn finish(self) -> DeltaResult<AfterSequential> {
        if !self.is_finished {
            return Err(Error::generic(
                "Must exhaust iterator before calling finish()",
            ));
        }

        let parallel_files = match self.checkpoint_manifest_output {
            Some(sidecars) => sidecars?,
            None => {
                let parts = self.checkpoint_parts;
                require!(
                    parts.len() != 1,
                    Error::generic(
                        "Invariant violation: If there is exactly one checkpoint part,
                        there must be a manifest reader"
                    )
                );
                // If this is a multi-part checkpoint, use the checkpoint parts for parallel phase
                parts
            }
        };

        if parallel_files.is_empty() {
            Ok(AfterSequential::Done(self.processor))
        } else {
            Ok(AfterSequential::Parallel {
                processor: self.processor,
                files: parallel_files,
            })
        }
    }

    async fn next_action(&mut self) -> DeltaResult<Option<DeltaResult<ActionsBatch>>>
    where
        C: BoundDeltaResultGenerator<ActionsBatch>,
        M: BoundDeltaResultGenerator<ActionsBatch, DeltaResult<Vec<FileMeta>>>,
    {
        if let Some(commit_phase) = self.commit_phase.as_mut() {
            match poll_fn(|context| Pin::new(&mut *commit_phase).poll_step(context)).await? {
                GeneratorStep::Yield(result) => return Ok(Some(result)),
                GeneratorStep::Done(()) => self.commit_phase = None,
            }
        }

        if let Some(checkpoint_manifest_phase) = self.checkpoint_manifest_phase.as_mut() {
            match poll_fn(|context| Pin::new(&mut *checkpoint_manifest_phase).poll_step(context))
                .await?
            {
                GeneratorStep::Yield(result) => return Ok(Some(result)),
                GeneratorStep::Done(output) => {
                    self.checkpoint_manifest_phase = None;
                    self.checkpoint_manifest_output = Some(output);
                }
            }
        }

        self.is_finished = true;
        Ok(None)
    }

    fn bind<'a>(
        self,
        channel: &'a Channel,
    ) -> SequentialPhase<
        impl BoundDeltaResultGenerator<ActionsBatch> + 'a,
        impl BoundDeltaResultGenerator<ActionsBatch, DeltaResult<Vec<FileMeta>>> + 'a,
    >
    where
        C: DeltaResultGenerator<ActionsBatch> + 'a,
        M: DeltaResultGenerator<ActionsBatch, DeltaResult<Vec<FileMeta>>> + 'a,
    {
        SequentialPhase {
            processor: self.processor,
            commit_phase: self.commit_phase.map(|phase| phase.bind(channel)),
            checkpoint_manifest_phase: self
                .checkpoint_manifest_phase
                .map(|phase| phase.bind(channel)),
            checkpoint_manifest_output: self.checkpoint_manifest_output,
            is_finished: self.is_finished,
            checkpoint_parts: self.checkpoint_parts,
        }
    }
}

async fn process_action_batch_with_channel(
    processor: &mut ScanLogReplayProcessor,
    channel: &Channel,
    batch: DeltaResult<ActionsBatch>,
) -> DeltaResult<ScanMetadata> {
    processor
        .process_actions_batch_with_channel(channel, batch?)
        .await
}

impl<C, M> SequentialPhase<C, M>
where
    C: BoundDeltaResultGenerator<ActionsBatch>,
    M: BoundDeltaResultGenerator<ActionsBatch, DeltaResult<Vec<FileMeta>>>,
{
    async fn advance(
        &mut self,
        channel: &Channel,
    ) -> DeltaResult<Option<DeltaResult<ScanMetadata>>> {
        let Some(result) = self.next_action().await? else {
            return Ok(None);
        };
        let result = process_action_batch_with_channel(&mut self.processor, channel, result).await;
        Ok(Some(result))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scan::{AfterSequentialScanMetadata, StatsOptions};
    use crate::unit_test_utils::{assert_result_error_with_message, load_test_table};

    /// Core helper function to verify sequential processing with expected adds and sidecars.
    fn verify_sequential_processing(
        table_name: &str,
        stats: StatsOptions,
        expected_adds: &[&str],
        expected_sidecars: &[&str],
    ) -> DeltaResult<()> {
        let (engine, snapshot, _tempdir) = load_test_table(table_name)?;

        let scan = snapshot.scan_builder().with_stats(stats).build()?;
        let mut sequential = scan.parallel_scan_metadata(engine)?;

        // Process all batches and collect Add file paths
        let mut file_paths = Vec::new();
        for result in sequential.by_ref() {
            let metadata = result?;
            file_paths =
                metadata.visit_scan_files(file_paths, |ps: &mut Vec<String>, file_stat| {
                    ps.push(file_stat.path);
                })?;
        }

        // Assert collected adds match expected
        file_paths.sort();
        assert_eq!(
            file_paths, expected_adds,
            "Sequential phase should collect expected Add file paths"
        );

        // Call finish() and verify result based on expected sidecars
        let result = sequential.finish()?;
        match (expected_sidecars, result) {
            (sidecars, AfterSequentialScanMetadata::Done) => {
                assert!(
                    sidecars.is_empty(),
                    "Expected Done but got sidecars {sidecars:?}"
                );
            }
            (expected_sidecars, AfterSequentialScanMetadata::Parallel { files, .. }) => {
                assert_eq!(
                    files.len(),
                    expected_sidecars.len(),
                    "Should collect exactly {} sidecar files",
                    expected_sidecars.len()
                );

                // Extract and verify sidecar file paths
                let mut collected_paths = files
                    .iter()
                    .map(|fm| {
                        fm.location
                            .path_segments()
                            .and_then(|mut segments| segments.next_back())
                            .unwrap_or("")
                            .to_string()
                    })
                    .collect_vec();

                collected_paths.sort();
                assert_eq!(collected_paths, expected_sidecars);
            }
        }

        Ok(())
    }

    #[test]
    fn test_sequential_v2_with_commits_only() -> DeltaResult<()> {
        verify_sequential_processing(
            "table-without-dv-small",
            StatsOptions::default(),
            &["part-00000-517f5d32-9c95-48e8-82b4-0229cc194867-c000.snappy.parquet"],
            &[], // No sidecars
        )
    }

    #[test]
    fn test_sequential_v2_with_sidecars() -> DeltaResult<()> {
        verify_sequential_processing(
            "v2-checkpoints-json-with-sidecars",
            StatsOptions::default(),
            &[], // No adds in sequential phase (all in checkpoint sidecars)
            &[
                "00000000000000000006.checkpoint.0000000001.0000000002.19af1366-a425-47f4-8fa6-8d6865625573.parquet",
                "00000000000000000006.checkpoint.0000000002.0000000002.5008b69f-aa8a-4a66-9299-0733a56a7e63.parquet",
            ],
        )
    }

    #[test]
    fn test_sequential_finish_before_exhaustion_error() -> DeltaResult<()> {
        let (engine, snapshot, _tempdir) = load_test_table("table-without-dv-small")?;

        let scan = snapshot.scan_builder().build()?;
        let sequential = scan.parallel_scan_metadata(engine)?;

        // Try to call finish() before exhausting the iterator
        let result = sequential.finish();
        assert_result_error_with_message(result, "Must exhaust iterator before calling finish()");

        Ok(())
    }

    #[test]
    fn test_sequential_checkpoint_without_sidecars() -> DeltaResult<()> {
        verify_sequential_processing(
            "v2-checkpoints-json-without-sidecars",
            StatsOptions::default(),
            &[
                // Adds from checkpoint manifest processed in sequential phase
                "test%25file%25prefix-part-00000-0e32f92c-e232-4daa-b734-369d1a800502-c000.snappy.parquet",
                "test%25file%25prefix-part-00000-91daf7c5-9ba0-4f76-aefd-0c3b21d33c6c-c000.snappy.parquet",
                "test%25file%25prefix-part-00001-a5c41be1-ded0-4b18-a638-a927d233876e-c000.snappy.parquet",
            ],
            &[], // No sidecars
        )
    }

    #[rstest::rstest]
    #[case::default(StatsOptions::default())]
    #[case::without_stats(StatsOptions::none())]
    fn test_sequential_parquet_checkpoint_with_sidecars(
        #[case] stats: StatsOptions,
    ) -> DeltaResult<()> {
        verify_sequential_processing(
            "v2-checkpoints-parquet-with-sidecars",
            stats,
            &[], // No adds in sequential phase
            &[
                // Expected sidecars
                "00000000000000000006.checkpoint.0000000001.0000000002.76931b15-ead3-480d-b86c-afe55a577fc3.parquet",
                "00000000000000000006.checkpoint.0000000002.0000000002.4367b29c-0e87-447f-8e81-9814cc01ad1f.parquet",
            ],
        )
    }

    #[test]
    fn test_sequential_checkpoint_no_commits() -> DeltaResult<()> {
        verify_sequential_processing(
            "with_checkpoint_no_last_checkpoint",
            StatsOptions::default(),
            &["part-00000-70b1dcdf-0236-4f63-a072-124cdbafd8a0-c000.snappy.parquet"], /* Add from commit 3 */
            &[], // No sidecars
        )
    }
}
