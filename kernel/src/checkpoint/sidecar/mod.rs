use std::future::poll_fn;
use std::ops::ControlFlow;
use std::pin::Pin;
use std::sync::Arc;

use itertools::Itertools;

use crate::actions::{ADD_NAME, REMOVE_NAME, SIDECAR_NAME};
use crate::coroutine::generator::GeneratorStep;
use crate::coroutine::kernel::request::{
    ExpressionEvaluatorHandle, PredicateEvaluatorHandle, WriteParquetFilesState,
};
use crate::coroutine::kernel::{
    static_generator_coroutine, BoundDeltaResultGenerator, Channel, DeltaResultGenerator,
    StaticGeneratorCoroutine,
};
#[cfg(test)]
use crate::coroutine::run_coroutine_with_engine;
use crate::coroutine::ChannelExt as _;
use crate::engine_data::filter_by_predicate;
use crate::expressions::{
    col, null_lit, Expression, ExpressionStructPatchBuilder, Predicate, Scalar, StructData,
};
use crate::schema::{try_schema, DataType, SchemaRef, StructField};
#[cfg(test)]
use crate::Engine;
use crate::{DeltaResult, EngineData, Error, FileMeta, FilteredEngineData};

/// Field names of the `sidecar` action struct.
const SIDECAR_SCHEMA_PATH: &str = "path";
const SIDECAR_SCHEMA_SIZE_IN_BYTES: &str = "sizeInBytes";
const SIDECAR_SCHEMA_MODIFICATION_TIME: &str = "modificationTime";
const SIDECAR_SCHEMA_TAGS: &str = "tags";

/// Creates a single [`EngineData`] batch containing one row per sidecar file.
///
/// Each row has the `sidecar` field populated and all other fields set to null.
/// All rows are created in one batch through the connector channel.
///
/// Returns `None` when there are no sidecars.
///
/// # Parameters
/// - `channel`: Connector channel for creating the batch.
/// - `checkpoint_data_schema`: The checkpoint data schema.
/// - `sidecar_metas`: Pairs of (sidecar filename, FileMeta) for each sidecar file.
pub(super) async fn create_sidecar_action_batch_with_channel(
    channel: &Channel,
    checkpoint_data_schema: &SchemaRef,
    sidecar_metas: &[(String, FileMeta)],
) -> DeltaResult<Option<Box<dyn EngineData>>> {
    if sidecar_metas.is_empty() {
        return Ok(None);
    }

    // Derive the sidecar struct schema and sidecar column index from the checkpoint data schema.
    let checkpoint_data_fields: Vec<&StructField> = checkpoint_data_schema.fields().collect();
    let (sidecar_col_idx, sidecar_field) = checkpoint_data_fields
        .iter()
        .enumerate()
        .find(|(_, f)| f.name() == SIDECAR_NAME)
        .ok_or_else(|| Error::internal_error("checkpoint schema missing sidecar field"))?;
    let DataType::Struct(sidecar_struct) = sidecar_field.data_type() else {
        return Err(Error::internal_error(format!(
            "expected sidecar field to be struct, got {:?}",
            sidecar_field.data_type()
        )));
    };
    let sidecar_fields: Vec<StructField> = sidecar_struct.fields().cloned().collect();
    // Per-row data template (follows checkpoint data schema, all fields null). Each sidecar row is
    // built by cloning this and populating only the `sidecar` field.
    let null_template: Vec<Scalar> = checkpoint_data_fields
        .iter()
        .map(|f| Scalar::Null(f.data_type().clone()))
        .collect();

    // Build one row per sidecar.
    let rows: Vec<Vec<Scalar>> = sidecar_metas
        .iter()
        .map(|(filename, meta)| -> DeltaResult<Vec<Scalar>> {
            let size_in_bytes = meta.size_as_i64()?;

            // Sidecar struct values, ordered to match `sidecar_fields`.
            let values: Vec<Scalar> = sidecar_fields
                .iter()
                .map(|field| match field.name().as_str() {
                    // Per the protocol, implementations are encouraged to store only
                    // the file's name for the `path` field.
                    SIDECAR_SCHEMA_PATH => Ok(Scalar::from(filename.clone())),
                    SIDECAR_SCHEMA_SIZE_IN_BYTES => Ok(Scalar::from(size_in_bytes)),
                    SIDECAR_SCHEMA_MODIFICATION_TIME => Ok(Scalar::from(meta.last_modified)),
                    // Sidecar tags are protocol details; we can let connectors specify them
                    // in the future if there is a need. For now we leave it as null.
                    // We explicitly fill it with `Scalar::Null` here, since
                    // `StructData::try_new` requires one value per schema field.
                    SIDECAR_SCHEMA_TAGS => Ok(Scalar::Null(field.data_type().clone())),
                    other => Err(Error::CheckpointWrite(format!(
                        "Unexpected sidecar field: {other}"
                    ))),
                })
                .try_collect()?;

            let mut row = null_template.clone();
            row[sidecar_col_idx] =
                Scalar::Struct(StructData::try_new(sidecar_fields.clone(), values)?);
            Ok(row)
        })
        .try_collect()?;

    let batch = channel
        .create_many(checkpoint_data_schema.clone(), rows)
        .await?;
    Ok(Some(batch))
}

/// Transforms [`EngineData`] and filters out all-null rows.
struct NonNullRowsPicker {
    transform: ExpressionEvaluatorHandle,
    null_row_filter: PredicateEvaluatorHandle,
}

impl NonNullRowsPicker {
    async fn pick(
        &self,
        channel: &Channel,
        batch: &dyn EngineData,
    ) -> DeltaResult<Box<dyn EngineData>> {
        let transformed = channel
            .evaluate_expression(&self.transform, batch.clone_data())
            .await?;
        let predicate_result = channel
            .evaluate_predicate(&self.null_row_filter, transformed.clone_data())
            .await?;
        filter_by_predicate(transformed, predicate_result.as_ref())
    }
}

/// Splits checkpoint data into file-action batches (for sidecars) and non-file-action batches
/// (for the main checkpoint file).
///
/// # Example
///
/// Given a checkpoint data batch with columns `[add, remove, protocol, metadata, txn]`:
///
/// ```text
/// file_actions_picker   -> projects [add, remove], drops rows where both are null
/// non_file_actions_picker -> nulls out add/remove, drops rows where all remaining are null
/// ```
///
/// So a batch like:
///
/// ```text
/// | add    | remove | protocol | metadata | txn  |
/// |--------|--------|----------|----------|------|
/// | <add1> | null   | null     | null     | null |   // file action row
/// | null   | null   | <proto>  | null     | null |   // non-file action row
/// | null   | null   | null     | <meta>   | null |   // non-file action row
/// ```
///
/// is split into:
/// - file batch: `[add: <add1>, remove: null]`
/// - non-file batches:
///   - `[add: null, remove: null, protocol: <prot>, metadata: null, txn: null]`
///   - `[add: null, remove: null, protocol: null, metadata: <meta>, txn: null]`
pub(super) struct SidecarSplitter {
    /// Projects to add/remove columns and filters out rows where both are null.
    file_actions_picker: NonNullRowsPicker,
    /// Nulls out add/remove columns and filters out rows where all remaining columns are null.
    non_file_actions_picker: NonNullRowsPicker,
    /// Accumulated non-file-action batches (protocol, metadata, txn, etc.).
    non_file_action_batches: Vec<Box<dyn EngineData>>,
}

impl SidecarSplitter {
    #[cfg(test)]
    pub(super) fn new(engine: &dyn Engine, checkpoint_data_schema: SchemaRef) -> DeltaResult<Self> {
        run_coroutine_with_engine!(engine, async move |channel| {
            Self::new_with_channel(channel, checkpoint_data_schema).await
        })
    }

    pub(super) async fn new_with_channel(
        channel: &Channel,
        checkpoint_data_schema: SchemaRef,
    ) -> DeltaResult<Self> {
        // Derive sidecar output schema from the checkpoint data schema (add + remove only).
        let add_field = checkpoint_data_schema.field(ADD_NAME).ok_or_else(|| {
            Error::checkpoint_write(format!("Checkpoint data schema missing '{ADD_NAME}' field"))
        })?;
        let remove_field = checkpoint_data_schema.field(REMOVE_NAME).ok_or_else(|| {
            Error::checkpoint_write(format!(
                "Checkpoint data schema missing '{REMOVE_NAME}' field"
            ))
        })?;
        if !add_field.is_nullable() {
            return Err(Error::checkpoint_write(format!(
                "Checkpoint data schema '{ADD_NAME}' field must be nullable"
            )));
        }
        if !remove_field.is_nullable() {
            return Err(Error::checkpoint_write(format!(
                "Checkpoint data schema '{REMOVE_NAME}' field must be nullable"
            )));
        }
        let sidecar_output_schema = Arc::new(try_schema! {
            (add_field),
            (remove_field),
        }?);

        // Sidecar projector: select only add/remove columns.
        let file_action_projector = channel.create_expression_evaluator(
            checkpoint_data_schema.clone(),
            Arc::new(Expression::struct_from([col!(ADD_NAME), col!(REMOVE_NAME)])),
            sidecar_output_schema.clone().into(),
        );
        let file_action_projector = file_action_projector.await?;

        // Filters out rows where both add and remove are null.
        let file_actions_null_row_filter = channel.create_predicate_evaluator(
            sidecar_output_schema,
            Arc::new(Predicate::or(
                Predicate::is_not_null(col!(ADD_NAME)),
                Predicate::is_not_null(col!(REMOVE_NAME)),
            )),
        );
        let file_actions_null_row_filter = file_actions_null_row_filter.await?;

        // Nulls out add/remove instead of dropping them so the data schema stays the
        // same as checkpoint_data_schema (add/remove columns are already nullable).
        let non_file_action_nullifier = channel.create_expression_evaluator(
            checkpoint_data_schema.clone(),
            Arc::new(Expression::struct_patch(
                ExpressionStructPatchBuilder::new()
                    .replace(ADD_NAME, null_lit(add_field.data_type.clone()))
                    .replace(REMOVE_NAME, null_lit(remove_field.data_type.clone())),
            )?),
            checkpoint_data_schema.clone().into(),
        );
        let non_file_action_nullifier = non_file_action_nullifier.await?;

        // Filters out rows where all non-file-action columns are null.
        let non_file_actions_null_row_filter = {
            let predicate = Predicate::or_from(
                checkpoint_data_schema
                    .fields()
                    .filter(|f| f.name != ADD_NAME && f.name != REMOVE_NAME)
                    .map(|f| Expression::column([&f.name]).is_not_null()),
            );
            channel
                .create_predicate_evaluator(checkpoint_data_schema, Arc::new(predicate))
                .await?
        };

        Ok(Self {
            file_actions_picker: NonNullRowsPicker {
                transform: file_action_projector,
                null_row_filter: file_actions_null_row_filter,
            },
            non_file_actions_picker: NonNullRowsPicker {
                transform: non_file_action_nullifier,
                null_row_filter: non_file_actions_null_row_filter,
            },
            non_file_action_batches: Vec::new(),
        })
    }

    /// Consume the splitter and return the buffered non-file-action batches.
    pub(super) fn into_non_file_batches(self) -> Vec<Box<dyn EngineData>> {
        self.non_file_action_batches
    }

    /// Yield every file-action batch from `splitter`, buffering non-file actions as it drains.
    pub(super) fn file_action_batches<G>(
        mut splitter: Self,
        checkpoint_data_iter: G,
    ) -> StaticGeneratorCoroutine<Box<dyn EngineData>, WriteParquetFilesState>
    where
        G: DeltaResultGenerator<FilteredEngineData> + 'static,
    {
        static_generator_coroutine!(async move |channel| {
            let mut checkpoint_data_iter = checkpoint_data_iter.bind(&channel);
            loop {
                match splitter
                    .next_file_actions_batch(&channel, &mut checkpoint_data_iter)
                    .await?
                {
                    ControlFlow::Continue(batch) => channel.yield_item(batch).await?,
                    ControlFlow::Break(()) => return Ok(Box::new(splitter) as _),
                }
            }
        })
    }

    /// Pull the next batch from the inner iterator, split it into file-action and non-file-action
    /// parts. Buffers the non-file-action part; returns the next non-empty file-action batch.
    /// Returns `ControlFlow::Break` with the terminal state when the inner generator is exhausted.
    async fn next_file_actions_batch<G>(
        &mut self,
        channel: &Channel,
        checkpoint_data_iter: &mut G,
    ) -> DeltaResult<ControlFlow<(), Box<dyn EngineData>>>
    where
        G: BoundDeltaResultGenerator<FilteredEngineData>,
    {
        loop {
            let result =
                match poll_fn(|context| Pin::new(&mut *checkpoint_data_iter).poll_step(context))
                    .await?
                {
                    GeneratorStep::Yield(result) => result,
                    GeneratorStep::Done(()) => return Ok(ControlFlow::Break(())),
                };
            let result = result.and_then(|f| f.apply_selection_vector());
            let batch = match result {
                Ok(b) => b,
                Err(e) => return Err(e),
            };
            let non_file_actions_batch = self.non_file_actions_picker.pick(channel, batch.as_ref());
            let non_file_actions_batch = non_file_actions_batch.await;
            let non_file_actions_batch = match non_file_actions_batch {
                Ok(b) => b,
                Err(e) => return Err(e),
            };
            if !non_file_actions_batch.is_empty() {
                self.non_file_action_batches.push(non_file_actions_batch);
            }
            let file_actions_batch = self.file_actions_picker.pick(channel, batch.as_ref()).await;
            match file_actions_batch {
                Ok(file_actions_batch) if file_actions_batch.is_empty() => continue,
                Ok(file_actions_batch) => return Ok(ControlFlow::Continue(file_actions_batch)),
                Err(e) => return Err(e),
            }
        }
    }
}

#[cfg(test)]
mod tests;
