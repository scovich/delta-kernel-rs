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

use crate::coroutine::generator::{generator_fn, BoundGenerator, GeneratorStep};
use crate::coroutine::kernel::{Channel, ChannelExt as _, KernelResultGenerator};
use crate::log_reader::checkpoint_manifest::CheckpointManifestReader;
use crate::log_replay::ActionsBatch;
use crate::log_segment::LogSegment;
use crate::scan::log_replay::ScanLogReplayProcessor;
use crate::scan::{ScanMetadata, COMMIT_READ_SCHEMA};
use crate::schema::SchemaRef;
use crate::utils::require;
use crate::{FileMeta, KernelError, KernelResult};

/// State carried while commits and an optional checkpoint manifest replay sequentially.
/// cbindgen:ignore
#[internal_api]
pub(crate) struct SequentialPhase<C = (), M = ()> {
    // The processor that will be used to process the action batches
    processor: ScanLogReplayProcessor,
    // Commit action batches, exhausted before the checkpoint manifest
    commit_phase: Option<C>,
    // The checkpoint manifest generator for a single-part checkpoint
    checkpoint_manifest_phase: Option<M>,
    // Terminal sidecar output, populated when checkpoint_manifest_phase completes
    checkpoint_manifest_output: Option<KernelResult<Vec<FileMeta>>>,
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

impl SequentialPhase {
    /// Prepares sequential replay whose terminal output carries any parallel work.
    pub(crate) async fn try_new_with_channel(
        channel: &Channel,
        processor: ScanLogReplayProcessor,
        log_segment: &LogSegment,
        checkpoint_read_schema: SchemaRef,
    ) -> KernelResult<
        impl KernelResultGenerator<ScanMetadata, KernelResult<AfterSequential>> + 'static,
    > {
        let commit_phase = log_segment
            .read_commit_actions_generator(channel, COMMIT_READ_SCHEMA.clone())
            .await?;

        // Concurrently start reading the checkpoint manifest. Only create a checkpoint manifest
        // reader if the checkpoint is single-part.
        let checkpoint_manifest_phase = match log_segment.listed.checkpoint_parts.as_slice() {
            [single_part] => Some(
                CheckpointManifestReader::try_new_generator(
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
    fn finish(self) -> KernelResult<AfterSequential> {
        if !self.is_finished {
            return Err(KernelError::generic(
                "Must exhaust iterator before calling finish()",
            ));
        }

        let parallel_files = match self.checkpoint_manifest_output {
            Some(sidecars) => sidecars?,
            None => {
                let parts = self.checkpoint_parts;
                require!(
                    parts.len() != 1,
                    KernelError::generic(
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
    async fn next_action(&mut self) -> KernelResult<Option<KernelResult<ActionsBatch>>>
    where
        C: BoundGenerator<Item = KernelResult<ActionsBatch>, Output = ()> + Unpin,
        M: BoundGenerator<Item = KernelResult<ActionsBatch>, Output = KernelResult<Vec<FileMeta>>>
            + Unpin,
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
        impl BoundGenerator<Item = KernelResult<ActionsBatch>, Output = ()> + Unpin + 'a,
        impl BoundGenerator<Item = KernelResult<ActionsBatch>, Output = KernelResult<Vec<FileMeta>>>
            + Unpin
            + 'a,
    >
    where
        C: KernelResultGenerator<ActionsBatch> + 'a,
        M: KernelResultGenerator<ActionsBatch, KernelResult<Vec<FileMeta>>> + 'a,
    {
        SequentialPhase {
            processor: self.processor,
            commit_phase: self.commit_phase.map(|phase| channel.bind(phase)),
            checkpoint_manifest_phase: self
                .checkpoint_manifest_phase
                .map(|phase| channel.bind(phase)),
            checkpoint_manifest_output: self.checkpoint_manifest_output,
            is_finished: self.is_finished,
            checkpoint_parts: self.checkpoint_parts,
        }
    }
}

impl<C, M> SequentialPhase<C, M>
where
    C: BoundGenerator<Item = KernelResult<ActionsBatch>, Output = ()> + Unpin,
    M: BoundGenerator<Item = KernelResult<ActionsBatch>, Output = KernelResult<Vec<FileMeta>>>
        + Unpin,
{
    async fn advance(
        &mut self,
        channel: &Channel,
    ) -> KernelResult<Option<KernelResult<ScanMetadata>>> {
        let Some(batch) = self.next_action().await? else {
            return Ok(None);
        };
        let metadata = match batch {
            Ok(batch) => {
                self.processor
                    .process_actions_batch_sequential_with_channel(channel, batch)
                    .await
            }
            Err(err) => Err(err),
        };
        Ok(Some(metadata))
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
    ) -> KernelResult<()> {
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
    fn test_sequential_v2_with_commits_only() -> KernelResult<()> {
        verify_sequential_processing(
            "table-without-dv-small",
            StatsOptions::default(),
            &["part-00000-517f5d32-9c95-48e8-82b4-0229cc194867-c000.snappy.parquet"],
            &[], // No sidecars
        )
    }

    #[test]
    fn test_sequential_v2_with_sidecars() -> KernelResult<()> {
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
    fn test_sequential_finish_before_exhaustion_error() -> KernelResult<()> {
        let (engine, snapshot, _tempdir) = load_test_table("table-without-dv-small")?;

        let scan = snapshot.scan_builder().build()?;
        let sequential = scan.parallel_scan_metadata(engine)?;

        // Try to call finish() before exhausting the iterator
        let result = sequential.finish();
        assert_result_error_with_message(result, "Must exhaust iterator before calling finish()");

        Ok(())
    }

    #[test]
    fn test_sequential_checkpoint_without_sidecars() -> KernelResult<()> {
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
    ) -> KernelResult<()> {
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
    fn test_sequential_checkpoint_no_commits() -> KernelResult<()> {
        verify_sequential_processing(
            "with_checkpoint_no_last_checkpoint",
            StatsOptions::default(),
            &["part-00000-70b1dcdf-0236-4f63-a072-124cdbafd8a0-c000.snappy.parquet"], /* Add from commit 3 */
            &[], // No sidecars
        )
    }
}
