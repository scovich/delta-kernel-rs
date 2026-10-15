//! Kernel-side typed connector operations.
//!
//! These methods construct the corresponding [`Request`] variant and suspend until its reply is
//! completed by the task driver.

use std::ops::Range;

use bytes::Bytes;
use delta_kernel_derive::internal_api;
use url::Url;

use super::request::{
    CopyAtomic, CreateEngineData, CreateExpressionEvaluator, CreatePredicateEvaluator,
    EvaluateExpression, EvaluatePredicate, EvaluationInput, EvaluatorHandle, FileWriteMode,
    PredicateEvaluatorHandle, ReadJsonFiles, ReadParquetFiles, WriteBytes, WriteJsonFile,
    WriteParquetFile, WriteParquetFiles, WriteParquetFilesResult,
};
#[cfg(feature = "declarative-plans")]
use super::PlanRequest;
use super::{Channel, Request, StaticGeneratorCoroutine};
use crate::committer::PublishMetadata;
#[cfg(feature = "declarative-plans")]
use crate::coroutine::PlanOperation;
use crate::coroutine::{
    ChannelExchange, Cursor, DeltaFuture, Generator, Page, PageGenerator, PageRequest,
    PagedOperation, Pages,
};
use crate::engine_data::{EngineData, FilteredEngineData};
use crate::expressions::{ExpressionRef, PredicateRef, Scalar};
#[cfg(feature = "declarative-plans")]
use crate::plans::ir::plan::Plan;
#[cfg(feature = "declarative-plans")]
use crate::plans::Operation;
use crate::schema::{DataType, SchemaRef};
use crate::{DeltaResult, FileIndex, FileMeta, FileSize, ParquetFooter};

/// Typed kernel operations available on its request channel.
#[internal_api]
pub(crate) trait ChannelExt: ChannelExchange<Request> {
    /// Read one small file, or one half-open byte range, completely into memory.
    fn read_small_file(
        &self,
        url: Url,
        range: Option<Range<FileIndex>>,
    ) -> impl DeltaFuture<Option<Bytes>> {
        self.exchange((url, range), Request::ReadSmallFile)
    }

    /// Read a Parquet file footer.
    fn read_parquet_footer(&self, file: &FileMeta) -> impl DeltaFuture<ParquetFooter> {
        self.exchange(file.clone(), Request::ReadParquetFooter)
    }

    /// Initialize `operation` and return its first page.
    fn start_paged<Op>(&self, operation: Op) -> impl DeltaFuture<Page<Op>>
    where
        Op: PagedOperation,
        PageRequest<Op>: Into<Request>,
    {
        self.exchange(operation, PageRequest::Start)
    }

    /// Verify that the connector provides a declarative plan executor.
    #[cfg(feature = "declarative-plans")]
    fn require_plan_executor(&self) -> impl DeltaFuture<()> {
        self.exchange((), |(), reply| {
            Request::ExecutePlan(PlanRequest::Require(reply))
        })
    }

    /// Initialize execution of `plan` and return its first output page.
    #[cfg(feature = "declarative-plans")]
    fn start_query_plan(&self, plan: Plan) -> impl DeltaFuture<Page<PlanOperation>> {
        self.start_paged(Operation::QueryPlan(plan))
    }

    /// Read one Parquet footer through the declarative plan executor.
    #[cfg(feature = "declarative-plans")]
    fn read_plan_parquet_footer(&self, file: &FileMeta) -> impl DeltaFuture<ParquetFooter> {
        self.exchange(file.clone(), |file, reply| {
            Request::ExecutePlan(PlanRequest::ParquetFooter(file, reply))
        })
    }

    /// Initialize `operation` without fetching its first page.
    fn prepare_paged<Op>(&self, operation: Op) -> impl DeltaFuture<Cursor<Op>>
    where
        Op: PagedOperation,
        PageRequest<Op>: Into<Request>,
    {
        self.exchange(operation, PageRequest::Prepare)
    }

    /// Continue the paged operation represented by `cursor`.
    fn continue_paged<Op>(&self, cursor: Cursor<Op>) -> impl DeltaFuture<Page<Op>>
    where
        Op: PagedOperation,
        PageRequest<Op>: Into<Request>,
    {
        self.exchange(cursor, PageRequest::Continue)
    }

    /// Iterate pages of `operation`, starting it on the first [`Pages::next`] call.
    fn paged<Op>(&self, operation: Op) -> Pages<'_, Self, Op>
    where
        Op: PagedOperation,
        PageRequest<Op>: Into<Request>,
    {
        Pages::start(self, operation)
    }

    /// Iterate remaining pages of `operation`, starting from `cursor`.
    fn resume_paged<Op>(&self, cursor: Cursor<Op>) -> Pages<'_, Self, Op>
    where
        Op: PagedOperation,
        PageRequest<Op>: Into<Request>,
    {
        Pages::resume(self, cursor)
    }

    /// Iterate items of `operation`, flattening collection-valued pages.
    fn flattened<Op>(
        &self,
        operation: Op,
    ) -> impl Generator<Item = <Op::Page as IntoIterator>::Item, Output = ()> + Unpin + '_
    where
        Op: PagedOperation + Unpin,
        Op::Page: IntoIterator,
        <Op::Page as IntoIterator>::IntoIter: Send + Unpin,
        <Op::Page as IntoIterator>::Item: Send + 'static,
        PageRequest<Op>: Into<Request>,
        Self: Sync + 'static,
    {
        self.paged(operation).flatten()
    }

    /// Read JSON files as a stream of result batches.
    fn read_json_files(
        &self,
        files: impl Into<Vec<FileMeta>>,
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
    ) -> impl Generator<Item = DeltaResult<Box<dyn EngineData>>, Output = ()> + Unpin + '_
    where
        Self: Sync + 'static,
    {
        self.flattened(ReadJsonFiles::new(files, physical_schema, predicate))
    }

    /// Prepare a JSON read without fetching its first page.
    fn prepare_json_files(
        &self,
        files: impl Into<Vec<FileMeta>>,
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
    ) -> impl DeltaFuture<PageGenerator<ReadJsonFiles>> + '_
    where
        Self: Sync,
    {
        let operation = ReadJsonFiles::new(files, physical_schema, predicate);
        async move {
            let cursor = self.prepare_paged(operation).await?;
            Ok(PageGenerator::resume(cursor))
        }
    }

    /// Prepare a Parquet read without fetching its first page.
    fn prepare_parquet_files(
        &self,
        files: impl Into<Vec<FileMeta>>,
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
    ) -> impl DeltaFuture<PageGenerator<ReadParquetFiles>> + '_
    where
        Self: Sync,
    {
        let operation = ReadParquetFiles::new(files, physical_schema, predicate);
        async move {
            let cursor = self.prepare_paged(operation).await?;
            Ok(PageGenerator::resume(cursor))
        }
    }

    /// Fetch metadata for one object.
    fn head(&self, url: &Url) -> impl DeltaFuture<FileMeta> {
        self.exchange(url.clone(), Request::Head)
    }

    /// Write generated row batches to one newline-delimited JSON file.
    fn write_json_file(
        &self,
        url: &Url,
        mode: FileWriteMode,
        input: StaticGeneratorCoroutine<FilteredEngineData>,
    ) -> impl DeltaFuture<FileSize> {
        let operation = WriteJsonFile::new(url.clone(), mode, input);
        self.exchange(Box::new(operation), Request::WriteJson)
    }

    /// Write generated row batches to one Parquet file.
    fn write_parquet_file(&self, operation: WriteParquetFile) -> impl DeltaFuture<()> {
        self.exchange(Box::new(operation), Request::WriteParquet)
    }

    /// Write generated row batches to connector-selected Parquet files.
    fn write_parquet_files(
        &self,
        operation: WriteParquetFiles,
    ) -> impl DeltaFuture<WriteParquetFilesResult> {
        self.exchange(Box::new(operation), Request::WriteParquetFiles)
    }

    /// Write `data` to `url`, replacing an existing object only when `overwrite` is true.
    fn write_bytes(&self, url: &Url, data: Bytes, overwrite: bool) -> impl DeltaFuture<()> {
        let outbound = WriteBytes::new(url.clone(), data, overwrite);
        self.exchange(outbound, Request::WriteBytes)
    }

    /// Atomically copy `source` to a new `destination`.
    #[allow(unused)]
    fn copy_atomic(&self, source: Url, destination: Url) -> impl DeltaFuture<()> {
        self.exchange(CopyAtomic::new(source, destination), Request::CopyAtomic)
    }

    /// Publish catalog commits through the connector's catalog committer.
    fn publish(&self, metadata: PublishMetadata) -> impl DeltaFuture<()> {
        self.exchange(metadata, Request::Publish)
    }

    /// Materialize one row containing a single scalar value.
    fn create_row(
        &self,
        schema: SchemaRef,
        value: impl Into<Scalar>,
    ) -> impl DeltaFuture<Box<dyn EngineData>> {
        self.create_many(schema, vec![vec![value.into()]])
    }

    /// Materialize owned scalar rows as one engine-data batch.
    fn create_many(
        &self,
        schema: SchemaRef,
        rows: Vec<Vec<Scalar>>,
    ) -> impl DeltaFuture<Box<dyn EngineData>> {
        let outbound = CreateEngineData::new(schema, rows);
        self.exchange(outbound, Request::CreateEngineData)
    }

    /// Prepare `expression` for repeated evaluation.
    fn create_expression_evaluator(
        &self,
        input_schema: SchemaRef,
        expression: ExpressionRef,
        output_type: DataType,
    ) -> impl DeltaFuture<EvaluatorHandle> {
        let outbound = CreateExpressionEvaluator::new(input_schema, expression, output_type);
        self.exchange(outbound, Request::CreateExpressionEvaluator)
    }

    /// Prepare `predicate` for repeated evaluation.
    fn create_predicate_evaluator(
        &self,
        input_schema: SchemaRef,
        predicate: PredicateRef,
    ) -> impl DeltaFuture<PredicateEvaluatorHandle> {
        let outbound = CreatePredicateEvaluator::new(input_schema, predicate);
        self.exchange(outbound, Request::CreatePredicateEvaluator)
    }

    /// Evaluate a prepared expression against `input`.
    fn evaluate_expression(
        &self,
        evaluator: &EvaluatorHandle,
        input: impl Into<EvaluationInput>,
    ) -> impl DeltaFuture<Box<dyn EngineData>> {
        let outbound = EvaluateExpression::new(evaluator.clone(), input.into());
        self.exchange(outbound, Request::EvaluateExpression)
    }

    /// Evaluate a prepared predicate against `input`.
    fn evaluate_predicate(
        &self,
        evaluator: &PredicateEvaluatorHandle,
        input: impl Into<EvaluationInput>,
    ) -> impl DeltaFuture<Box<dyn EngineData>> {
        let outbound = EvaluatePredicate::new(evaluator.clone(), input.into());
        self.exchange(outbound, Request::EvaluatePredicate)
    }
}

impl ChannelExt for Channel {}
