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
    EvaluateExpression, EvaluatePredicate, EvaluationInput, ExpressionEvaluatorHandle,
    FileWriteMode, PredicateEvaluatorHandle, ReadJsonFiles, ReadParquetFiles, WriteBytes,
    WriteJsonFile, WriteParquetFile, WriteParquetFiles, WriteParquetFilesResult,
};
#[cfg(feature = "declarative-plans")]
use super::PlanRequest;
use super::{
    BoundDeltaResultGenerator, Channel, DeltaResultGenerator, Request, StaticGeneratorCoroutine,
};
use crate::committer::{Commit, CommitResponse, PublishMetadata};
use crate::coroutine::generator::{BoundGenerator, Flatten, Generator, GeneratorExt as _};
use crate::coroutine::{
    ChannelExchange, Cursor, DeltaFuture, Page, PageGenerator, PageRequest, PagedOperation,
};
use crate::engine_data::{EngineData, FilteredEngineData};
use crate::expressions::{ExpressionRef, PredicateRef, Scalar};
#[cfg(feature = "declarative-plans")]
use crate::plans::ir::plan::Plan;
#[cfg(feature = "declarative-plans")]
use crate::plans::Operation;
use crate::schema::{DataType, SchemaRef};
use crate::{DeltaResult, FileIndex, FileMeta, FileSize, ParquetFooter};

type FlattenedPages<Op> =
    Flatten<PageGenerator<Op>, std::vec::IntoIter<DeltaResult<Box<dyn EngineData>>>>;

/// Typed kernel operations posted through a request channel.
///
/// Paged reads initialize the connector operation before returning a generator so initialization
/// errors surface at the call. `read_*` returns a bound generator; `prepare_*` returns an unbound
/// generator for composition before [`Self::bind`].
#[internal_api]
#[allow(async_fn_in_trait)]
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

    /// Verify that the connector provides a declarative plan executor.
    #[cfg(feature = "declarative-plans")]
    fn require_plan_executor(&self) -> impl DeltaFuture<()> {
        self.exchange((), |(), reply| {
            Request::ExecutePlan(PlanRequest::Require(reply))
        })
    }

    /// Execute `plan`, streaming its output batches.
    #[cfg(feature = "declarative-plans")]
    async fn execute_query_plan(
        &self,
        plan: Plan,
    ) -> DeltaResult<impl BoundDeltaResultGenerator<Box<dyn EngineData>> + '_> {
        let cursor = self.prepare_paged(Operation::QueryPlan(plan)).await?;
        Ok(self.bind(PageGenerator::Next(cursor).flatten()))
    }

    /// Prepare `plan` without fetching output. The returned generator streams output batches.
    #[cfg(feature = "declarative-plans")]
    async fn prepare_query_plan(
        &self,
        plan: Plan,
    ) -> DeltaResult<impl DeltaResultGenerator<Box<dyn EngineData>> + use<Self>> {
        let cursor = self.prepare_paged(Operation::QueryPlan(plan)).await?;
        Ok(PageGenerator::Next(cursor).flatten())
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

    /// Fetch the next page for `cursor`.
    fn next_page<Op>(&self, cursor: Cursor<Op>) -> impl DeltaFuture<Page<Op>>
    where
        Op: PagedOperation,
        PageRequest<Op>: Into<Request>,
    {
        self.exchange(cursor, PageRequest::Next)
    }

    /// Bind `generator` so its connector requests post through this channel.
    fn bind<'a, G>(
        &'a self,
        generator: G,
    ) -> impl BoundGenerator<Item = G::Item, Output = G::Output> + Unpin + 'a
    where
        G: Generator<Request> + 'a,
        G::Item: 'a;

    /// Read JSON files as a stream of result batches.
    async fn read_json_files(
        &self,
        files: impl Into<Vec<FileMeta>>,
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
    ) -> DeltaResult<impl BoundDeltaResultGenerator<Box<dyn EngineData>> + '_> {
        let operation = ReadJsonFiles::new(files, physical_schema, predicate);
        let cursor = self.prepare_paged(operation).await?;
        Ok(self.bind(PageGenerator::Next(cursor).flatten()))
    }

    /// Read Parquet files as a stream of result batches.
    async fn read_parquet_files(
        &self,
        files: impl Into<Vec<FileMeta>>,
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
    ) -> DeltaResult<impl BoundDeltaResultGenerator<Box<dyn EngineData>> + '_> {
        let operation = ReadParquetFiles::new(files, physical_schema, predicate);
        let cursor = self.prepare_paged(operation).await?;
        Ok(self.bind(PageGenerator::Next(cursor).flatten()))
    }

    /// Prepare a JSON read without fetching its first page.
    async fn prepare_json_files(
        &self,
        files: impl Into<Vec<FileMeta>>,
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
    ) -> DeltaResult<FlattenedPages<ReadJsonFiles>> {
        let operation = ReadJsonFiles::new(files, physical_schema, predicate);
        let cursor = self.prepare_paged(operation).await?;
        Ok(PageGenerator::Next(cursor).flatten())
    }

    /// Prepare a Parquet read without fetching its first page.
    async fn prepare_parquet_files(
        &self,
        files: impl Into<Vec<FileMeta>>,
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
    ) -> DeltaResult<FlattenedPages<ReadParquetFiles>> {
        let operation = ReadParquetFiles::new(files, physical_schema, predicate);
        let cursor = self.prepare_paged(operation).await?;
        Ok(PageGenerator::Next(cursor).flatten())
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
    fn copy_atomic(&self, source: &Url, destination: &Url) -> impl DeltaFuture<()> {
        let operation = CopyAtomic::new(source.clone(), destination.clone());
        self.exchange(operation, Request::CopyAtomic)
    }

    /// Delegate a prepared transaction to the connector's committer.
    fn commit(&self, commit: Commit) -> impl DeltaFuture<CommitResponse> {
        self.exchange(Box::new(commit), Request::Commit)
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
    ) -> impl DeltaFuture<ExpressionEvaluatorHandle> {
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
        evaluator: &ExpressionEvaluatorHandle,
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

impl ChannelExt for Channel {
    fn bind<'a, G>(
        &'a self,
        generator: G,
    ) -> impl BoundGenerator<Item = G::Item, Output = G::Output> + Unpin + 'a
    where
        G: Generator<Request> + 'a,
        G::Item: 'a,
    {
        generator.bind(self)
    }
}
