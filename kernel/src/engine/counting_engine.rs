//! Engine decorator that counts handler operations while delegating their work.

use std::fmt;
use std::ops::Deref;
use std::sync::Arc;

use bytes::Bytes;
use url::Url;

use crate::cancellation::CancellationTokenRef;
use crate::engine::RelaxedCounter;
use crate::expressions::{ExpressionRef, PredicateRef, Scalar};
#[cfg(feature = "declarative-plans")]
use crate::plans::{Operation, PlanExecutor, PlanResult};
use crate::schema::{DataType, SchemaRef};
use crate::{
    Engine, EngineData, EvaluationHandler, ExpressionEvaluator, FileDataReadResultIterator,
    FileMeta, FileSize, FileSlice, FilteredEngineData, JsonHandler, KernelResult,
    KernelResultIterator, ParquetFooter, ParquetHandler, ParquetMultiFileWrite, PredicateEvaluator,
    StorageHandler,
};

/// A handler decorator that counts each dereference to its inner handler.
pub struct CountingHandler<T: ?Sized> {
    inner: Arc<T>,
    operations: RelaxedCounter,
}

impl<T: ?Sized> CountingHandler<T> {
    fn new(inner: Arc<T>) -> Self {
        let operations = RelaxedCounter::default();
        Self { inner, operations }
    }

    /// Returns the number of delegated operations.
    pub fn op_count(&self) -> u64 {
        self.operations.get()
    }
}

// Prints only the operation count: the inner handler is not required to implement `Debug`.
impl<T: ?Sized> fmt::Debug for CountingHandler<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.op_count(), f)
    }
}

impl<T: ?Sized> Deref for CountingHandler<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.operations.inc();
        self.inner.as_ref()
    }
}

/// Handler decorators shared by a [`CountingEngine`].
#[derive(Debug)]
pub struct CountingHandlers {
    /// Evaluation-handler operations.
    pub evaluation: CountingHandler<dyn EvaluationHandler>,
    /// Storage-handler operations.
    pub storage: CountingHandler<dyn StorageHandler>,
    /// JSON-handler operations.
    pub json: CountingHandler<dyn JsonHandler>,
    /// Parquet-handler operations.
    pub parquet: CountingHandler<dyn ParquetHandler>,
    /// Plan-executor operations, or `None` if the inner engine provides no executor.
    #[cfg(feature = "declarative-plans")]
    pub plans: Option<CountingHandler<dyn PlanExecutor>>,
}

/// An [`Engine`] decorator that counts handler operations and delegates them to an inner engine.
///
/// Fetching a handler is not an operation; only calls on the handler's methods are counted.
#[derive(Clone, Debug)]
pub struct CountingEngine(Arc<CountingHandlers>);

impl CountingEngine {
    /// Wraps `inner` with zeroed operation counters.
    pub fn new(inner: Arc<dyn Engine>) -> Self {
        Self(Arc::new(CountingHandlers {
            evaluation: CountingHandler::new(inner.evaluation_handler()),
            storage: CountingHandler::new(inner.storage_handler()),
            json: CountingHandler::new(inner.json_handler()),
            parquet: CountingHandler::new(inner.parquet_handler()),
            #[cfg(feature = "declarative-plans")]
            plans: inner.plan_executor().map(CountingHandler::new),
        }))
    }

    /// Asserts that no delegated handler operations have run.
    pub fn assert_no_io(&self) {
        assert_eq!(self.total_op_count(), 0, "expected no engine I/O: {self:?}");
    }
}

impl CountingHandlers {
    /// Returns the number of delegated operations across all handlers.
    pub fn total_op_count(&self) -> u64 {
        self.evaluation.op_count()
            + self.storage.op_count()
            + self.json.op_count()
            + self.parquet.op_count()
            + {
                #[cfg(feature = "declarative-plans")]
                {
                    self.plans.as_ref().map_or(0, CountingHandler::op_count)
                }
                #[cfg(not(feature = "declarative-plans"))]
                {
                    0
                }
            }
    }
}

impl Deref for CountingEngine {
    type Target = CountingHandlers;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Engine for CountingEngine {
    fn evaluation_handler(&self) -> Arc<dyn EvaluationHandler> {
        self.0.clone()
    }

    fn storage_handler(&self) -> Arc<dyn StorageHandler> {
        self.0.clone()
    }

    fn json_handler(&self) -> Arc<dyn JsonHandler> {
        self.0.clone()
    }

    fn parquet_handler(&self) -> Arc<dyn ParquetHandler> {
        self.0.clone()
    }

    #[cfg(feature = "declarative-plans")]
    fn plan_executor(&self) -> Option<Arc<dyn PlanExecutor>> {
        let _ = self.0.plans.as_ref()?;
        Some(self.0.clone())
    }
}

impl EvaluationHandler for CountingHandlers {
    fn new_expression_evaluator(
        &self,
        input_schema: SchemaRef,
        expression: ExpressionRef,
        output_type: DataType,
    ) -> KernelResult<Arc<dyn ExpressionEvaluator>> {
        self.evaluation
            .new_expression_evaluator(input_schema, expression, output_type)
    }

    fn new_predicate_evaluator(
        &self,
        input_schema: SchemaRef,
        predicate: PredicateRef,
    ) -> KernelResult<Arc<dyn PredicateEvaluator>> {
        self.evaluation
            .new_predicate_evaluator(input_schema, predicate)
    }

    fn create_many(
        &self,
        schema: SchemaRef,
        rows: Vec<Vec<Scalar>>,
    ) -> KernelResult<Box<dyn EngineData>> {
        self.evaluation.create_many(schema, rows)
    }
}

impl StorageHandler for CountingHandlers {
    fn list_from(
        &self,
        path: &Url,
        end: Option<&Url>,
    ) -> KernelResult<Box<dyn KernelResultIterator<FileMeta>>> {
        self.storage.list_from(path, end)
    }

    fn list_from_with_cancellation(
        &self,
        path: &Url,
        end: Option<&Url>,
        cancellation_token: Option<CancellationTokenRef>,
    ) -> KernelResult<Box<dyn KernelResultIterator<FileMeta>>> {
        self.storage
            .list_from_with_cancellation(path, end, cancellation_token)
    }

    fn read_files(
        &self,
        files: Vec<FileSlice>,
    ) -> KernelResult<Box<dyn KernelResultIterator<Bytes>>> {
        self.storage.read_files(files)
    }

    fn read_files_with_cancellation(
        &self,
        files: Vec<FileSlice>,
        cancellation_token: Option<CancellationTokenRef>,
    ) -> KernelResult<Box<dyn KernelResultIterator<Bytes>>> {
        self.storage
            .read_files_with_cancellation(files, cancellation_token)
    }

    fn copy_atomic(&self, src: &Url, dest: &Url) -> KernelResult<()> {
        self.storage.copy_atomic(src, dest)
    }

    fn put(&self, path: &Url, data: Bytes, overwrite: bool) -> KernelResult<()> {
        self.storage.put(path, data, overwrite)
    }

    fn head(&self, path: &Url) -> KernelResult<FileMeta> {
        self.storage.head(path)
    }

    fn delete(&self, path: &Url) -> KernelResult<()> {
        self.storage.delete(path)
    }
}

impl JsonHandler for CountingHandlers {
    fn parse_json(
        &self,
        json_strings: Box<dyn EngineData>,
        output_schema: SchemaRef,
    ) -> KernelResult<Box<dyn EngineData>> {
        self.json.parse_json(json_strings, output_schema)
    }

    fn read_json_files(
        &self,
        files: &[FileMeta],
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
    ) -> KernelResult<FileDataReadResultIterator> {
        self.json.read_json_files(files, physical_schema, predicate)
    }

    fn read_json_files_with_cancellation(
        &self,
        files: &[FileMeta],
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
        cancellation_token: Option<CancellationTokenRef>,
    ) -> KernelResult<FileDataReadResultIterator> {
        self.json.read_json_files_with_cancellation(
            files,
            physical_schema,
            predicate,
            cancellation_token,
        )
    }

    fn write_json_file(
        &self,
        path: &Url,
        data: Box<dyn KernelResultIterator<FilteredEngineData> + '_>,
        overwrite: bool,
    ) -> KernelResult<FileSize> {
        self.json.write_json_file(path, data, overwrite)
    }
}

impl ParquetHandler for CountingHandlers {
    fn read_parquet_files(
        &self,
        files: &[FileMeta],
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
    ) -> KernelResult<FileDataReadResultIterator> {
        self.parquet
            .read_parquet_files(files, physical_schema, predicate)
    }

    fn read_parquet_files_with_cancellation(
        &self,
        files: &[FileMeta],
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
        cancellation_token: Option<CancellationTokenRef>,
    ) -> KernelResult<FileDataReadResultIterator> {
        self.parquet.read_parquet_files_with_cancellation(
            files,
            physical_schema,
            predicate,
            cancellation_token,
        )
    }

    fn write_parquet_file(
        &self,
        location: Url,
        data: Box<dyn KernelResultIterator<Box<dyn EngineData>>>,
    ) -> KernelResult<FileSize> {
        self.parquet.write_parquet_file(location, data)
    }

    fn write_parquet_files(
        &self,
        write: ParquetMultiFileWrite,
        data: Box<dyn KernelResultIterator<Box<dyn EngineData>>>,
    ) -> KernelResult<Vec<(Url, FileSize)>> {
        self.parquet.write_parquet_files(write, data)
    }

    fn read_parquet_footer(&self, file: &FileMeta) -> KernelResult<ParquetFooter> {
        self.parquet.read_parquet_footer(file)
    }

    fn read_parquet_footer_with_cancellation(
        &self,
        file: &FileMeta,
        cancellation_token: Option<CancellationTokenRef>,
    ) -> KernelResult<ParquetFooter> {
        self.parquet
            .read_parquet_footer_with_cancellation(file, cancellation_token)
    }
}

#[cfg(feature = "declarative-plans")]
#[allow(clippy::expect_used)]
impl PlanExecutor for CountingHandlers {
    fn execute_op(&self, op: Operation) -> KernelResult<PlanResult> {
        self.plans
            .as_ref()
            .expect("CountingHandlers is exposed as a PlanExecutor only when one is configured")
            .execute_op(op)
    }

    fn read_parquet_footer(&self, file: FileMeta) -> KernelResult<ParquetFooter> {
        self.plans
            .as_ref()
            .expect("CountingHandlers is exposed as a PlanExecutor only when one is configured")
            .read_parquet_footer(file)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::sync::SyncEngine;
    use crate::object_store::memory::InMemory;

    #[test]
    fn handler_access_does_not_count_as_an_operation() {
        let inner = Arc::new(SyncEngine::new());
        let engine = CountingEngine::new(inner);

        let _ = engine.evaluation_handler();
        let _ = engine.storage_handler();
        let _ = engine.json_handler();
        let _ = engine.parquet_handler();

        assert_eq!(engine.evaluation.op_count(), 0);
        assert_eq!(engine.storage.op_count(), 0);
        assert_eq!(engine.json.op_count(), 0);
        assert_eq!(engine.parquet.op_count(), 0);
    }

    #[test]
    fn handler_operation_increments_its_counter() {
        let inner = Arc::new(SyncEngine::new_with_store(Arc::new(InMemory::new())));
        let engine = CountingEngine::new(inner);

        let result = engine
            .storage_handler()
            .head(&Url::parse("memory:///missing").unwrap());

        assert!(result.is_err());
        assert_eq!(engine.storage.op_count(), 1);
        assert_eq!(engine.json.op_count(), 0);
    }
}
