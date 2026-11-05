#[cfg(test)]
use std::num::NonZero;
use std::sync::Arc;

use bytes::Bytes;
#[cfg(test)]
use derive_more::Constructor;
#[cfg(test)]
use url::Url;

use super::get_bytes;
#[cfg(test)]
use super::{put_bytes, read_files_arrow};
use crate::engine::arrow_conversion::TryFromArrow as _;
use crate::engine::arrow_data::ArrowEngineData;
use crate::engine::arrow_utils::{
    RowIndexBuilder, fixup_parquet_read, ordering_needs_row_indexes, parquet_read_plan,
};
use crate::engine::parquet_row_group_skipping::ParquetRowGroupSkipping;
use crate::engine::reader_options;
#[cfg(test)]
use crate::engine::writer_options;
use crate::object_store::DynObjectStore;
use crate::parquet::arrow::arrow_reader::{ArrowReaderMetadata, ParquetRecordBatchReaderBuilder};
#[cfg(test)]
use crate::parquet::arrow::arrow_writer::ArrowWriter;
use crate::schema::{SchemaRef, StructType};
use crate::utils::FoldWithOption as _;
#[cfg(test)]
use crate::{
    EngineData, FileDataReadResultIterator, FileSize, ParquetHandler, ParquetMultiFileWrite,
};
use crate::{FileMeta, KernelResult, KernelResultIterator, ParquetFooter, PredicateRef};

#[cfg(test)]
#[derive(Constructor)]
pub(crate) struct SyncParquetHandler {
    store: Option<Arc<DynObjectStore>>,
}

pub(super) fn try_create_from_parquet(
    data: Bytes,
    schema: SchemaRef,
    predicate: Option<PredicateRef>,
    file_location: String,
) -> KernelResult<impl KernelResultIterator<ArrowEngineData>> {
    let metadata = ArrowReaderMetadata::load(&data, reader_options())?;
    let (requested_ordering, mask) = parquet_read_plan(&schema, &metadata)?;

    let mut row_indexes = ordering_needs_row_indexes(&requested_ordering)
        .then(|| RowIndexBuilder::new(metadata.metadata().row_groups()));

    let builder = ParquetRecordBatchReaderBuilder::new_with_metadata(data, metadata)
        .fold_with(mask, ParquetRecordBatchReaderBuilder::with_projection)
        .fold_with(predicate, |builder, predicate| {
            builder.with_row_group_filter(predicate.as_ref(), row_indexes.as_mut())
        });

    let mut row_indexes = row_indexes.map(|rb| rb.build()).transpose()?;
    let stream = builder.build()?;
    Ok(stream.map(move |rbr| {
        fixup_parquet_read(
            rbr?,
            &requested_ordering,
            row_indexes.as_mut(),
            Some(&file_location),
            Some(&schema),
        )
    }))
}

#[cfg(test)]
impl ParquetHandler for SyncParquetHandler {
    fn read_parquet_files(
        &self,
        files: &[FileMeta],
        schema: SchemaRef,
        predicate: Option<PredicateRef>,
    ) -> KernelResult<FileDataReadResultIterator> {
        let iter = read_files_arrow(
            self.store.as_ref(),
            files,
            schema,
            predicate,
            try_create_from_parquet,
        );
        Ok(Box::new(iter.map(|data| Ok(Box::new(data?) as _))))
    }

    /// Writes engine data to a Parquet file at the specified location.
    ///
    /// Buffers the entire file in memory and `put`s it to the underlying [`ObjectStore`].
    /// If a file already exists at the given location, it will be overwritten.
    ///
    /// # Parameters
    ///
    /// - `location` - The full URL path where the Parquet file should be written (e.g. `file:///path/to/file.parquet`).
    /// - `data` - An iterator of engine data to be written to the Parquet file.
    fn write_parquet_file(
        &self,
        location: Url,
        mut data: Box<dyn KernelResultIterator<Box<dyn EngineData>>>,
    ) -> KernelResult<FileSize> {
        let first = data.next().ok_or_else(|| {
            crate::KernelError::generic("Cannot write parquet file with empty data iterator")
        })??;
        let buf = write_one_parquet_file(first, &mut data, usize::MAX)?;
        let size_in_bytes = buf.len() as u64;
        put_bytes(self.store.as_ref(), &location, buf.into(), true)?;
        Ok(size_in_bytes)
    }

    fn write_parquet_files(
        &self,
        write: ParquetMultiFileWrite,
        mut data: Box<dyn KernelResultIterator<Box<dyn EngineData>>>,
    ) -> KernelResult<Vec<(Url, FileSize)>> {
        // Cut files between batches after the advisory row target is reached. Batches are never
        // split.
        let preferred_rows_per_file = write
            .preferred_rows_per_file
            .map(NonZero::get)
            .unwrap_or(usize::MAX);
        let mut files = Vec::new();
        while let Some(first) = data.next() {
            let buf = write_one_parquet_file(first?, &mut data, preferred_rows_per_file - 1)?;
            let size_in_bytes = buf.len() as u64;
            let location = write.new_file_url()?;
            put_bytes(self.store.as_ref(), &location, buf.into(), true)?;
            files.push((location, size_in_bytes));
        }
        Ok(files)
    }

    fn read_parquet_footer(&self, file: &FileMeta) -> KernelResult<ParquetFooter> {
        parquet_footer(self.store.as_ref(), file)
    }
}

#[cfg(test)]
fn engine_data_to_record_batch(
    data: Box<dyn EngineData>,
) -> KernelResult<crate::arrow::array::RecordBatch> {
    let arrow = ArrowEngineData::try_from_engine_data(data)?;
    Ok((*arrow).into())
}

/// Writes `first` plus further batches while the running row count is `<= rowcount_limit`.
/// `usize::MAX` means no limit.
#[cfg(test)]
fn write_one_parquet_file(
    first: Box<dyn EngineData>,
    data: &mut dyn KernelResultIterator<Box<dyn EngineData>>,
    rowcount_limit: usize,
) -> KernelResult<Vec<u8>> {
    let first = engine_data_to_record_batch(first)?;
    let mut buf = Vec::new();
    let mut writer = ArrowWriter::try_new_with_options(&mut buf, first.schema(), writer_options())?;
    writer.write(&first)?;
    let mut rows = first.num_rows();
    while rows <= rowcount_limit {
        let Some(batch) = data.next() else {
            break;
        };
        let batch = engine_data_to_record_batch(batch?)?;
        writer.write(&batch)?;
        rows = rows.saturating_add(batch.num_rows());
    }
    writer.close()?; // writer must be closed to write the footer
    Ok(buf)
}

/// Read the [`ParquetFooter`] (schema) of `file`.
pub(super) fn parquet_footer(
    store: Option<&Arc<DynObjectStore>>,
    file: &FileMeta,
) -> KernelResult<ParquetFooter> {
    let data = get_bytes(store, &file.location)?;
    let metadata = ArrowReaderMetadata::load(&data, reader_options())?;
    let schema = Arc::new(StructType::try_from_arrow(metadata.schema().as_ref())?);
    Ok(ParquetFooter { schema })
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::num::NonZero;
    use std::sync::Arc;

    use tempfile::tempdir;
    use url::Url;

    use super::*;
    use crate::EngineData;
    use crate::arrow::array::{Array, Int64Array, RecordBatch, StringArray};
    use crate::engine::arrow_conversion::TryIntoKernel as _;

    fn test_data_iter() -> Box<dyn KernelResultIterator<Box<dyn EngineData>>> {
        let engine_data: Box<dyn EngineData> = Box::new(ArrowEngineData::new(
            RecordBatch::try_from_iter(vec![
                (
                    "id",
                    Arc::new(Int64Array::from(vec![1, 2, 3])) as Arc<dyn Array>,
                ),
                (
                    "name",
                    Arc::new(StringArray::from(vec!["a", "b", "c"])) as Arc<dyn Array>,
                ),
            ])
            .unwrap(),
        ));
        Box::new(std::iter::once(Ok(engine_data)))
    }

    #[test]
    fn test_sync_write_parquet_file() {
        let handler = SyncParquetHandler::new(None);
        let temp_dir = tempdir().unwrap();
        let file_path = temp_dir.path().join("test.parquet");
        let url = Url::from_file_path(&file_path).unwrap();

        let write_size = handler
            .write_parquet_file(url.clone(), test_data_iter())
            .unwrap();
        assert!(file_path.exists());

        // Read it back to verify
        let file = File::open(&file_path).unwrap();
        let reader =
            crate::parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder::try_new(file)
                .unwrap();
        let schema = reader.schema().clone();
        let file_size = std::fs::metadata(&file_path).unwrap().len();
        // The reported size must be non-zero and match the on-disk file length.
        assert_ne!(write_size, 0);
        assert_eq!(write_size, file_size);
        let file_meta = FileMeta {
            location: url,
            last_modified: 0,
            size: file_size,
        };

        let mut result = handler
            .read_parquet_files(
                &[file_meta],
                Arc::new(schema.try_into_kernel().unwrap()),
                None,
            )
            .unwrap();

        let engine_data = result.next().unwrap().unwrap();
        let batch = ArrowEngineData::try_from_engine_data(engine_data).unwrap();
        let record_batch = batch.record_batch();

        assert_eq!(record_batch.num_rows(), 3);
        assert_eq!(record_batch.num_columns(), 2);

        let id_col = record_batch
            .column(0)
            .as_any()
            .downcast_ref::<Int64Array>()
            .unwrap();
        assert_eq!(id_col.values(), &[1, 2, 3]);

        let name_col = record_batch
            .column(1)
            .as_any()
            .downcast_ref::<StringArray>()
            .unwrap();
        assert_eq!(name_col.value(0), "a");
        assert_eq!(name_col.value(1), "b");
        assert_eq!(name_col.value(2), "c");

        assert!(result.next().is_none());
    }

    /// The Parquet `FILE` type lets a group define only the fields it uses. Reading such a file as
    /// the canonical six-field `file` fills the fields the group omits with null.
    /// Writes `group` as the Parquet column `f` and reads it back as a `file` column.
    fn read_file_group(
        group: crate::arrow::array::StructArray,
    ) -> crate::KernelResult<RecordBatch> {
        use crate::schema::{DataType, StructField};

        let batch =
            RecordBatch::try_from_iter(vec![("f", Arc::new(group) as Arc<dyn Array>)]).unwrap();
        let temp_dir = tempdir().unwrap();
        let file_path = temp_dir.path().join("file_group.parquet");
        let mut writer =
            ArrowWriter::try_new(File::create(&file_path).unwrap(), batch.schema(), None).unwrap();
        writer.write(&batch).unwrap();
        writer.close().unwrap();

        let file_meta = FileMeta {
            location: Url::from_file_path(&file_path).unwrap(),
            last_modified: 0,
            size: std::fs::metadata(&file_path).unwrap().len(),
        };
        let schema = Arc::new(StructType::new_unchecked([StructField::nullable(
            "f",
            DataType::file_type(),
        )]));
        let engine_data = SyncParquetHandler::new(None)
            .read_parquet_files(&[file_meta], schema, None)?
            .next()
            .unwrap()?;
        Ok(ArrowEngineData::try_from_engine_data(engine_data)?
            .record_batch()
            .clone())
    }

    #[test]
    fn test_read_file_column_with_partial_group() {
        use crate::arrow::array::StructArray;
        use crate::arrow::buffer::NullBuffer;
        use crate::arrow::datatypes::{DataType as ArrowDataType, Field};

        // A group that defines only `uri` and `size`: an external whole-or-ranged reference,
        // followed by a null `file` value.
        let group = StructArray::try_new(
            vec![
                Field::new("uri", ArrowDataType::Utf8, true),
                Field::new("size", ArrowDataType::Int64, true),
            ]
            .into(),
            vec![
                Arc::new(StringArray::from(vec![Some("s3://b/a.png"), None])) as Arc<dyn Array>,
                Arc::new(Int64Array::from(vec![Some(10), None])),
            ],
            Some(NullBuffer::from(vec![true, false])),
        )
        .unwrap();
        let record_batch = read_file_group(group).unwrap();

        let f = record_batch
            .column(0)
            .as_any()
            .downcast_ref::<StructArray>()
            .unwrap();
        let names: Vec<_> = f
            .fields()
            .iter()
            .map(|field| field.name().as_str())
            .collect();
        assert_eq!(
            names,
            [
                "uri",
                "offset",
                "size",
                "content_type",
                "checksum",
                "inline"
            ]
        );
        assert!(f.is_null(1));
        let uri = f
            .column_by_name("uri")
            .unwrap()
            .as_any()
            .downcast_ref::<StringArray>()
            .unwrap();
        assert_eq!(uri.value(0), "s3://b/a.png");
        let size = f
            .column_by_name("size")
            .unwrap()
            .as_any()
            .downcast_ref::<Int64Array>()
            .unwrap();
        assert_eq!(size.value(0), 10);
        for omitted in ["offset", "content_type", "checksum", "inline"] {
            assert_eq!(
                f.column_by_name(omitted).unwrap().null_count(),
                2,
                "omitted field {omitted} must read as null"
            );
        }
    }

    /// A non-null `file` value that resolves to no referent fails the read.
    #[test]
    fn test_read_file_column_without_referent_fails() {
        use crate::arrow::array::StructArray;
        use crate::arrow::datatypes::{DataType as ArrowDataType, Field};

        let group = StructArray::from(vec![(
            Arc::new(Field::new("size", ArrowDataType::Int64, true)),
            Arc::new(Int64Array::from(vec![Some(10)])) as Arc<dyn Array>,
        )]);

        let error = read_file_group(group).unwrap_err().to_string();
        assert!(
            error.contains("Invalid value in `file` column 'f': neither `uri` nor `inline` is set"),
            "got: {error}"
        );
    }

    #[test]
    fn test_sync_write_parquet_files_honors_row_hint() {
        let handler = SyncParquetHandler::new(None);
        let temp_dir = tempdir().unwrap();
        let directory = Url::from_directory_path(temp_dir.path()).unwrap();
        let write = ParquetMultiFileWrite::new(directory, "part-", ".parquet", NonZero::new(2));
        let batch = || {
            Box::new(ArrowEngineData::new(
                RecordBatch::try_from_iter(vec![(
                    "id",
                    Arc::new(Int64Array::from(vec![1])) as Arc<dyn Array>,
                )])
                .unwrap(),
            )) as Box<dyn EngineData>
        };
        let data = Box::new([Ok(batch()), Ok(batch()), Ok(batch())].into_iter());

        let files = handler.write_parquet_files(write.clone(), data).unwrap();

        assert_eq!(files.len(), 2);
        for (file, size) in files {
            let name = file.path_segments().unwrap().next_back().unwrap();
            assert!(name.starts_with("part-") && name.ends_with(".parquet"));
            assert_eq!(
                size,
                std::fs::metadata(file.to_file_path().unwrap())
                    .unwrap()
                    .len()
            );
        }

        let empty = handler
            .write_parquet_files(write, Box::new(std::iter::empty()))
            .unwrap();
        assert!(empty.is_empty());
    }

    #[test]
    fn test_sync_write_parquet_file_multiple_batches() {
        let handler = SyncParquetHandler::new(None);
        let temp_dir = tempdir().unwrap();
        let file_path = temp_dir.path().join("test_multi_batch.parquet");
        let url = Url::from_file_path(&file_path).unwrap();

        let batch1: Box<dyn crate::EngineData> = Box::new(ArrowEngineData::new(
            RecordBatch::try_from_iter(vec![(
                "value",
                Arc::new(Int64Array::from(vec![1, 2, 3])) as Arc<dyn Array>,
            )])
            .unwrap(),
        ));
        let batch2: Box<dyn crate::EngineData> = Box::new(ArrowEngineData::new(
            RecordBatch::try_from_iter(vec![(
                "value",
                Arc::new(Int64Array::from(vec![4, 5, 6])) as Arc<dyn Array>,
            )])
            .unwrap(),
        ));
        let batch3: Box<dyn crate::EngineData> = Box::new(ArrowEngineData::new(
            RecordBatch::try_from_iter(vec![(
                "value",
                Arc::new(Int64Array::from(vec![7, 8, 9])) as Arc<dyn Array>,
            )])
            .unwrap(),
        ));

        let batches = vec![Ok(batch1), Ok(batch2), Ok(batch3)];
        let data_iter: Box<dyn KernelResultIterator<Box<dyn EngineData>>> =
            Box::new(batches.into_iter());

        handler.write_parquet_file(url.clone(), data_iter).unwrap();
        assert!(file_path.exists());

        let file = File::open(&file_path).unwrap();
        let reader =
            crate::parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder::try_new(file)
                .unwrap();
        let schema = reader.schema().clone();
        let file_size = std::fs::metadata(&file_path).unwrap().len();
        let file_meta = FileMeta {
            location: url,
            last_modified: 0,
            size: file_size,
        };

        let mut result = handler
            .read_parquet_files(
                &[file_meta],
                Arc::new(schema.try_into_kernel().unwrap()),
                None,
            )
            .unwrap();

        let engine_data = result.next().unwrap().unwrap();
        let batch = ArrowEngineData::try_from_engine_data(engine_data).unwrap();
        let record_batch = batch.record_batch();

        assert_eq!(record_batch.num_rows(), 9);
        let value_col = record_batch
            .column(0)
            .as_any()
            .downcast_ref::<Int64Array>()
            .unwrap();
        assert_eq!(value_col.values(), &[1, 2, 3, 4, 5, 6, 7, 8, 9]);

        assert!(result.next().is_none());
    }

    #[test]
    fn write_parquet_creates_parent_directories() {
        let handler = SyncParquetHandler::new(None);
        let temp_dir = tempdir().unwrap();
        let file_path = temp_dir.path().join("a/b/c/test.parquet");
        let url = Url::from_file_path(&file_path).unwrap();

        handler.write_parquet_file(url, test_data_iter()).unwrap();
        assert!(file_path.exists());
    }

    /// Ensures `write_parquet_file` and `read_parquet_footer` work end-to-end with an
    /// `ObjectStore` backend. The local path is exercised by the other tests in this module.
    #[test]
    fn parquet_store_write_and_footer_roundtrip() {
        let store = Arc::new(crate::object_store::memory::InMemory::new());
        let handler = SyncParquetHandler::new(Some(store));
        let url = Url::parse("memory:///t/data.parquet").unwrap();

        handler
            .write_parquet_file(url.clone(), test_data_iter())
            .unwrap();

        let footer = handler
            .read_parquet_footer(&FileMeta {
                location: url,
                last_modified: 0,
                size: 0,
            })
            .unwrap();
        let field_names: Vec<_> = footer
            .schema
            .fields()
            .map(|f| f.name().to_string())
            .collect();
        assert_eq!(field_names, vec!["id".to_string(), "name".to_string()]);
    }

    // TODO(#2618): Restore once the engine contract helpers move to test_utils and SyncEngine can
    // call them without the kernel-cfg-test cycle issue.
    //
    // #[test]
    // fn parquet_handler_reads_footer() {
    //     test_parquet_handler_reads_footer(&SyncParquetHandler::new(None));
    // }
    //
    // #[test]
    // fn parquet_handler_footer_errors_on_missing_file() {
    //     test_parquet_handler_footer_errors_on_missing_file(&SyncParquetHandler::new(None));
    // }
    //
    // #[test]
    // fn parquet_handler_footer_preserves_field_ids() {
    //     test_parquet_handler_footer_preserves_field_ids(&SyncParquetHandler::new(None));
    // }
    //
    // #[test]
    // fn parquet_handler_write_always_overwrites() {
    //     test_parquet_handler_write_always_overwrites(&SyncParquetHandler::new(None));
    // }
    //
    // #[test]
    // fn parquet_handler_write_omits_arrow_schema() {
    //     test_parquet_handler_write_omits_arrow_schema(&SyncParquetHandler::new(None));
    // }
    //
    // #[test]
    // fn parquet_handler_reads_file_with_arrow_schema() {
    //     test_parquet_handler_reads_file_with_arrow_schema(&SyncParquetHandler::new(None));
    // }
}
