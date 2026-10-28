//! Kernel-side construction of coroutines and typed connector operations.
//!
//! Kernel operations are async functions that receive a [`Channel`]. [`ChannelExt`] methods post
//! the corresponding [`Request`] and suspend until the connector completes its reply. The
//! [`workflow!`] and [`coroutine!`] macros, and their `static_` forms, wrap such a function in a
//! connector-driven [`Workflow`](super::Workflow) or [`Coroutine`](super::Coroutine) that owns
//! its channel.

use std::ops::Range;
use std::sync::Arc;

use bytes::Bytes;
use delta_kernel_derive::internal_api;
use url::Url;

use super::request::{
    CreateEngineData, CreateExpressionEvaluator, CreatePredicateEvaluator, DirectoryListing,
    EvaluateExpression, EvaluatePredicate, ExpressionEvaluatorHandle, PredicateEvaluatorHandle,
    ReadJsonFiles, ReadParquetFiles, WriteBytes, WriteJsonFile, WriteParquetFile,
    WriteParquetFiles, WriteParquetFilesResult,
};
#[cfg(feature = "declarative-plans")]
use super::PlanRequest;
use super::{Request, StaticCoroutine};
use crate::committer::{Commit, CommitResponse, PublishMetadata};
use crate::coroutine::generator::{self, BoundGenerator, Flatten, Generator, GeneratorExt as _};
use crate::coroutine::{
    ChannelExchange, Cursor, KernelFuture, Page, PageGenerator, PageRequest, PagedOperation,
};
use crate::engine_data::FilteredEngineData;
use crate::expressions::{ExpressionRef, PredicateRef};
#[cfg(feature = "declarative-plans")]
use crate::plans::{Operation, Plan};
use crate::schema::{DataType, SchemaRef};
use crate::{
    EngineData, FileIndex, FileMeta, FileSize, KernelResult, ParquetFooter, ParquetMultiFileWrite,
    Scalar,
};

/// Kernel's [`crate::coroutine::Channel`] specialized to [`Request`].
///
/// Workflow and coroutine bodies take this as an async-function argument. Child generators borrow
/// it for the lifetime of their [`GeneratorBody`].
#[internal_api]
pub(crate) type Channel = crate::coroutine::Channel<Request>;

type FlattenedPages<Op> =
    Flatten<PageGenerator<Op>, std::vec::IntoIter<KernelResult<Box<dyn EngineData>>>>;

/// Typed Kernel operations posted through a request channel.
///
/// Paged reads initialize the connector operation before returning an unbound generator, so
/// initialization errors surface at the call and callers can compose the stream before
/// [`Self::bind`].
#[internal_api]
#[allow(async_fn_in_trait)]
pub(crate) trait ChannelExt: ChannelExchange<Request> {
    /// Lists every object beneath one directory URL in ascending order.
    async fn list_directory(
        &self,
        prefix: Url,
    ) -> KernelResult<impl BoundGenerator<Item = KernelResult<FileMeta>, Output = ()> + Unpin + '_>
    {
        let cursor = self
            .prepare_paged(DirectoryListing::try_new(prefix)?)
            .await?;
        Ok(self.bind(PageGenerator::Next(cursor).flatten()))
    }

    /// Read one small file, or one half-open byte range, completely into memory via
    /// [`Request::ReadSmallFile`].
    fn read_small_file(
        &self,
        url: Url,
        range: Option<Range<FileIndex>>,
    ) -> impl KernelFuture<Option<Bytes>> {
        self.exchange((url, range), Request::ReadSmallFile)
    }

    /// Reads one Parquet file footer via [`Request::ReadParquetFooter`].
    fn read_parquet_footer(&self, file: &FileMeta) -> impl KernelFuture<ParquetFooter> {
        self.exchange(file.clone(), Request::ReadParquetFooter)
    }

    /// Verifies that the connector provides a declarative plan executor.
    #[cfg(feature = "declarative-plans")]
    fn require_plan_executor(&self) -> impl KernelFuture<()> {
        self.exchange((), |(), reply| {
            Request::ExecutePlan(PlanRequest::Require(reply))
        })
    }

    /// Executes `plan`, streaming its output batches.
    #[cfg(feature = "declarative-plans")]
    async fn execute_query_plan(
        &self,
        plan: Plan,
    ) -> KernelResult<
        impl BoundGenerator<Item = KernelResult<Box<dyn EngineData>>, Output = ()> + Unpin + '_,
    > {
        let query = self.prepare_query_plan(plan).await?;
        Ok(self.bind(query))
    }

    /// Prepares a declarative query as an unbound stream of result batches.
    #[cfg(feature = "declarative-plans")]
    async fn prepare_query_plan(&self, plan: Plan) -> KernelResult<FlattenedPages<Operation>> {
        let cursor = self.prepare_paged(Operation::QueryPlan(plan)).await?;
        Ok(PageGenerator::Next(cursor).flatten())
    }

    /// Reads one Parquet footer through the declarative plan executor.
    #[cfg(feature = "declarative-plans")]
    fn read_plan_parquet_footer(&self, file: &FileMeta) -> impl KernelFuture<ParquetFooter> {
        self.exchange(file.clone(), |file, reply| {
            Request::ExecutePlan(PlanRequest::ParquetFooter(file, reply))
        })
    }

    /// Fetches metadata for one object via [`Request::Head`].
    fn head(&self, url: &Url) -> impl KernelFuture<FileMeta> {
        self.exchange(url.clone(), Request::Head)
    }

    /// Reads JSON files via [`Request::ReadJson`] as a stream of result batches.
    async fn read_json_files<'a>(
        &'a self,
        files: impl Into<Vec<FileMeta>> + 'a,
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
    ) -> KernelResult<
        impl BoundGenerator<Item = KernelResult<Box<dyn EngineData>>, Output = ()> + Unpin + 'a,
    > {
        let prepare_json_files = self.prepare_json_files(files, physical_schema, predicate);
        Ok(self.bind(prepare_json_files.await?))
    }

    /// Prepares a JSON read as an unbound stream of result batches.
    async fn prepare_json_files(
        &self,
        files: impl Into<Vec<FileMeta>>,
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
    ) -> KernelResult<FlattenedPages<ReadJsonFiles>> {
        let operation = ReadJsonFiles::new(files, physical_schema, predicate);
        let cursor = self.prepare_paged(operation).await?;
        Ok(PageGenerator::Next(cursor).flatten())
    }

    /// Reads Parquet files via [`Request::ReadParquet`] as a stream of result batches.
    async fn read_parquet_files<'a>(
        &'a self,
        files: impl Into<Vec<FileMeta>> + 'a,
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
    ) -> KernelResult<
        impl BoundGenerator<Item = KernelResult<Box<dyn EngineData>>, Output = ()> + Unpin + 'a,
    > {
        let prepare_parquet_files = self.prepare_parquet_files(files, physical_schema, predicate);
        Ok(self.bind(prepare_parquet_files.await?))
    }

    /// Prepares a Parquet read as an unbound stream of result batches.
    async fn prepare_parquet_files(
        &self,
        files: impl Into<Vec<FileMeta>>,
        physical_schema: SchemaRef,
        predicate: Option<PredicateRef>,
    ) -> KernelResult<FlattenedPages<ReadParquetFiles>> {
        let operation = ReadParquetFiles::new(files, physical_schema, predicate);
        let cursor = self.prepare_paged(operation).await?;
        Ok(PageGenerator::Next(cursor).flatten())
    }

    /// Materializes one row containing a single scalar value via [`Request::CreateEngineData`].
    fn create_row(
        &self,
        schema: SchemaRef,
        value: impl Into<Scalar>,
    ) -> impl KernelFuture<Box<dyn EngineData>> {
        self.create_many(schema, vec![vec![value.into()]])
    }

    /// Materializes owned scalar rows as one engine-data batch via [`Request::CreateEngineData`].
    fn create_many(
        &self,
        schema: SchemaRef,
        rows: Vec<Vec<Scalar>>,
    ) -> impl KernelFuture<Box<dyn EngineData>> {
        self.exchange(
            CreateEngineData::new(schema, rows),
            Request::CreateEngineData,
        )
    }

    /// Prepares `expression` for repeated evaluation.
    fn create_expression_evaluator(
        &self,
        input_schema: SchemaRef,
        expression: ExpressionRef,
        output_type: DataType,
    ) -> impl KernelFuture<ExpressionEvaluatorHandle> {
        let evaluator = CreateExpressionEvaluator::new(input_schema, expression, output_type);
        self.exchange(evaluator, Request::CreateExpressionEvaluator)
    }

    /// Prepares `predicate` for repeated evaluation.
    fn create_predicate_evaluator(
        &self,
        input_schema: SchemaRef,
        predicate: PredicateRef,
    ) -> impl KernelFuture<PredicateEvaluatorHandle> {
        let evaluator = CreatePredicateEvaluator::new(input_schema, predicate);
        self.exchange(evaluator, Request::CreatePredicateEvaluator)
    }

    /// Evaluates a prepared expression against `input`.
    fn evaluate_expression(
        &self,
        evaluator: &ExpressionEvaluatorHandle,
        input: Box<dyn EngineData>,
    ) -> impl KernelFuture<Box<dyn EngineData>> {
        let evaluation = EvaluateExpression::new(Arc::clone(evaluator), input);
        self.exchange(evaluation, Request::EvaluateExpression)
    }

    /// Evaluates a prepared predicate against `input`.
    fn evaluate_predicate(
        &self,
        evaluator: &PredicateEvaluatorHandle,
        input: Box<dyn EngineData>,
    ) -> impl KernelFuture<Box<dyn EngineData>> {
        let evaluation = EvaluatePredicate::new(Arc::clone(evaluator), input);
        self.exchange(evaluation, Request::EvaluatePredicate)
    }

    /// Writes generated row batches to one newline-delimited JSON file.
    fn write_json_file(
        &self,
        url: &Url,
        input: StaticCoroutine<FilteredEngineData>,
        overwrite: bool,
    ) -> impl KernelFuture<FileSize> {
        let operation = WriteJsonFile::new(url.clone(), input, overwrite);
        self.exchange(Box::new(operation), Request::WriteJson)
    }

    /// Writes generated row batches to one Parquet file.
    fn write_parquet_file(&self, operation: WriteParquetFile) -> impl KernelFuture<FileSize> {
        self.exchange(Box::new(operation), Request::WriteParquet)
    }

    /// Writes generated row batches to connector-selected Parquet files.
    fn write_parquet_files(
        &self,
        write: ParquetMultiFileWrite,
        input: StaticCoroutine<Box<dyn EngineData>>,
    ) -> impl KernelFuture<WriteParquetFilesResult> {
        let operation = WriteParquetFiles::new(write, input);
        self.exchange(Box::new(operation), Request::WriteParquetFiles)
    }

    /// Writes one complete object.
    fn write_bytes(&self, url: &Url, data: Bytes, overwrite: bool) -> impl KernelFuture<()> {
        let operation = WriteBytes::new(url.clone(), data, overwrite);
        self.exchange(operation, Request::WriteBytes)
    }

    /// Delegates a prepared transaction to the connector's committer.
    fn commit(&self, commit: Commit) -> impl KernelFuture<CommitResponse> {
        self.exchange(Box::new(commit), Request::Commit)
    }

    /// Verifies that the connector selected a catalog committer.
    fn require_catalog_committer(&self) -> impl KernelFuture<()> {
        self.exchange((), |(), reply| Request::RequireCatalogCommitter(reply))
    }

    /// Publishes catalog commits through the connector's committer.
    fn publish(&self, metadata: PublishMetadata) -> impl KernelFuture<()> {
        self.exchange(metadata, Request::Publish)
    }

    /// Initialize `operation` via [`PageRequest::Prepare`] without fetching its first page.
    fn prepare_paged<Op>(&self, operation: Op) -> impl KernelFuture<Cursor<Op>>
    where
        Op: PagedOperation,
        PageRequest<Op>: Into<Request>,
    {
        self.exchange(operation, PageRequest::Prepare)
    }

    /// Fetch the next page for `cursor` via [`PageRequest::Next`].
    fn next_page<Op>(&self, cursor: Cursor<Op>) -> impl KernelFuture<Page<Op>>
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

/// Active streaming Kernel operation using an existing request channel.
///
/// Connector requests surface through the driver that owns the channel. The `'a` lifetime covers
/// that channel and any state captured by the operation.
#[internal_api]
pub(crate) type GeneratorBody<'a, Y, O = ()> = generator::GeneratorBody<'a, Y, O>;

/// Creates a [`Coroutine`](super::Coroutine) that may borrow captured state.
#[internal_api]
macro_rules! coroutine {
    ($($body:tt)+) => {
        $crate::coroutine::coroutine!($crate::coroutine::kernel::Request, $($body)+)
    };
}

/// Creates an unpolled [`StaticCoroutine`](super::StaticCoroutine) from a static generator body.
#[internal_api]
macro_rules! static_coroutine {
    ($($body:tt)+) => {
        $crate::coroutine::static_coroutine!($crate::coroutine::kernel::Request,$($body)+)
    };
}

/// Creates a [`Workflow`](super::Workflow) that may borrow captured state.
#[internal_api]
macro_rules! workflow {
    ($($body:tt)+) => {
        $crate::coroutine::workflow!($crate::coroutine::kernel::Request, $($body)+)
    };
}

/// Creates an unpolled [`StaticWorkflow`](super::StaticWorkflow) from a static workflow body.
#[internal_api]
macro_rules! static_workflow {
    ($($body:tt)+) => {
        $crate::coroutine::static_workflow!($crate::coroutine::kernel::Request, $($body)+)
    };
}
