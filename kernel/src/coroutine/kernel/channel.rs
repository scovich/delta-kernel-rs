//! Kernel-side construction of coroutines and typed connector operations.
//!
//! Kernel operations are async functions that receive a [`Channel`]. [`ChannelExt`] methods post
//! the corresponding [`Request`] and suspend until the connector completes its reply. The
//! [`workflow!`] and [`coroutine!`] macros, and their `static_` forms, wrap such a function in a
//! connector-driven [`Workflow`](super::Workflow) or [`Coroutine`](super::Coroutine) that owns
//! its channel.
//!
//! # Choosing the ownership boundary
//!
//! A [`Workflow`](super::Workflow) already owns its request channel. For a streaming operation
//! without a parent, pass owned inputs through [`StaticCoroutine::new`]. Its callback must return
//! `GeneratorBody<'static, _>`, statically preventing borrowed state from escaping.
//!
//! Kernel code that already has a [`Channel`] calls async implementations directly. Passing that
//! channel to a streaming operation returns a [`GeneratorBody`] whose lifetime also covers any
//! other borrowed inputs. Requests then surface through the parent channel. Kernel may compose
//! channel-provided futures with runtime-neutral future and stream combinators; every operation
//! requiring connector work still passes through the channel.
//!
//! # Result streams
//!
//! Unbound result streams are named `impl DeltaResultGenerator<T>` and can be erased as
//! `Box<dyn DeltaResultGenerator<T>>`. Helpers such as [`Generator::into_boxed`] erase at the
//! [`Generator`] trait instead, so call sites usually name
//! `Box<dyn Generator<Request, Item = DeltaResult<T>, Output = ()>>`. Binding one produces a
//! [`BoundDeltaResultGenerator`].

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
use super::{DeltaResultGenerator, Request, StaticCoroutine};
use crate::committer::{Commit, CommitResponse, PublishMetadata};
use crate::coroutine::generator::{self, BoundGenerator, Flatten, Generator, GeneratorExt as _};
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

/// Kernel's [`crate::coroutine::Channel`] specialized to [`Request`].
///
/// Workflow and coroutine bodies take this as an async-function argument. Child generators borrow
/// it for the lifetime of their [`GeneratorBody`].
#[internal_api]
pub(crate) type Channel = crate::coroutine::Channel<Request>;

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
    /// Read one small file, or one half-open byte range, completely into memory via
    /// [`Request::ReadSmallFile`].
    fn read_small_file(
        &self,
        url: Url,
        range: Option<Range<FileIndex>>,
    ) -> impl DeltaFuture<Option<Bytes>> {
        self.exchange((url, range), Request::ReadSmallFile)
    }

    /// Read a Parquet file footer via [`Request::ReadParquetFooter`].
    fn read_parquet_footer(&self, file: &FileMeta) -> impl DeltaFuture<ParquetFooter> {
        self.exchange(file.clone(), Request::ReadParquetFooter)
    }

    /// Verify that the connector provides a declarative plan executor via [`PlanRequest::Require`].
    #[cfg(feature = "declarative-plans")]
    fn require_plan_executor(&self) -> impl DeltaFuture<()> {
        self.exchange((), |(), reply| {
            Request::ExecutePlan(PlanRequest::Require(reply))
        })
    }

    /// Execute `plan` via [`PlanRequest::Query`], streaming its output batches.
    #[cfg(feature = "declarative-plans")]
    async fn execute_query_plan(
        &self,
        plan: Plan,
    ) -> DeltaResult<impl BoundDeltaResultGenerator<Box<dyn EngineData>> + '_> {
        let cursor = self.prepare_paged(Operation::QueryPlan(plan)).await?;
        Ok(self.bind(PageGenerator::Next(cursor).flatten()))
    }

    /// Prepare `plan` via [`PlanRequest::Query`] without fetching output. The returned generator
    /// streams output batches.
    #[cfg(feature = "declarative-plans")]
    async fn prepare_query_plan(
        &self,
        plan: Plan,
    ) -> DeltaResult<impl DeltaResultGenerator<Box<dyn EngineData>> + use<Self>> {
        let cursor = self.prepare_paged(Operation::QueryPlan(plan)).await?;
        Ok(PageGenerator::Next(cursor).flatten())
    }

    /// Read one Parquet footer through the declarative plan executor via
    /// [`PlanRequest::ParquetFooter`].
    #[cfg(feature = "declarative-plans")]
    fn read_plan_parquet_footer(&self, file: &FileMeta) -> impl DeltaFuture<ParquetFooter> {
        self.exchange(file.clone(), |file, reply| {
            Request::ExecutePlan(PlanRequest::ParquetFooter(file, reply))
        })
    }

    /// Initialize `operation` via [`PageRequest::Prepare`] without fetching its first page.
    fn prepare_paged<Op>(&self, operation: Op) -> impl DeltaFuture<Cursor<Op>>
    where
        Op: PagedOperation,
        PageRequest<Op>: Into<Request>,
    {
        self.exchange(operation, PageRequest::Prepare)
    }

    /// Fetch the next page for `cursor` via [`PageRequest::Next`].
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

    /// Read JSON files via [`Request::ReadJson`] as a stream of result batches.
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

    /// Read Parquet files via [`Request::ReadParquet`] as a stream of result batches.
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

    /// Prepare a JSON read via [`Request::ReadJson`] without fetching its first page.
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

    /// Prepare a Parquet read via [`Request::ReadParquet`] without fetching its first page.
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

    /// Fetch metadata for one object via [`Request::Head`].
    fn head(&self, url: &Url) -> impl DeltaFuture<FileMeta> {
        self.exchange(url.clone(), Request::Head)
    }

    /// Write generated row batches to one newline-delimited JSON file via [`Request::WriteJson`].
    fn write_json_file(
        &self,
        url: &Url,
        mode: FileWriteMode,
        input: StaticCoroutine<FilteredEngineData>,
    ) -> impl DeltaFuture<FileSize> {
        let operation = WriteJsonFile::new(url.clone(), mode, input);
        self.exchange(Box::new(operation), Request::WriteJson)
    }

    /// Write generated row batches to one Parquet file via [`Request::WriteParquet`].
    fn write_parquet_file(&self, operation: WriteParquetFile) -> impl DeltaFuture<()> {
        self.exchange(Box::new(operation), Request::WriteParquet)
    }

    /// Write generated row batches to connector-selected Parquet files via
    /// [`Request::WriteParquetFiles`].
    fn write_parquet_files(
        &self,
        operation: WriteParquetFiles,
    ) -> impl DeltaFuture<WriteParquetFilesResult> {
        self.exchange(Box::new(operation), Request::WriteParquetFiles)
    }

    /// Write `data` to `url` via [`Request::WriteBytes`], replacing an existing object only when
    /// `overwrite` is true.
    fn write_bytes(&self, url: &Url, data: Bytes, overwrite: bool) -> impl DeltaFuture<()> {
        let outbound = WriteBytes::new(url.clone(), data, overwrite);
        self.exchange(outbound, Request::WriteBytes)
    }

    /// Atomically copy `source` to a new `destination` via [`Request::CopyAtomic`].
    #[allow(unused)]
    fn copy_atomic(&self, source: &Url, destination: &Url) -> impl DeltaFuture<()> {
        let operation = CopyAtomic::new(source.clone(), destination.clone());
        self.exchange(operation, Request::CopyAtomic)
    }

    /// Delegate a prepared transaction to the connector's committer via [`Request::Commit`].
    fn commit(&self, commit: Commit) -> impl DeltaFuture<CommitResponse> {
        self.exchange(Box::new(commit), Request::Commit)
    }

    /// Publish catalog commits through the connector's catalog committer via [`Request::Publish`].
    fn publish(&self, metadata: PublishMetadata) -> impl DeltaFuture<()> {
        self.exchange(metadata, Request::Publish)
    }

    /// Materialize one row containing a single scalar value via [`Request::CreateEngineData`].
    fn create_row(
        &self,
        schema: SchemaRef,
        value: impl Into<Scalar>,
    ) -> impl DeltaFuture<Box<dyn EngineData>> {
        self.create_many(schema, vec![vec![value.into()]])
    }

    /// Materialize owned scalar rows as one engine-data batch via [`Request::CreateEngineData`].
    fn create_many(
        &self,
        schema: SchemaRef,
        rows: Vec<Vec<Scalar>>,
    ) -> impl DeltaFuture<Box<dyn EngineData>> {
        let outbound = CreateEngineData::new(schema, rows);
        self.exchange(outbound, Request::CreateEngineData)
    }

    /// Prepare `expression` for repeated evaluation via [`Request::CreateExpressionEvaluator`].
    fn create_expression_evaluator(
        &self,
        input_schema: SchemaRef,
        expression: ExpressionRef,
        output_type: DataType,
    ) -> impl DeltaFuture<ExpressionEvaluatorHandle> {
        let outbound = CreateExpressionEvaluator::new(input_schema, expression, output_type);
        self.exchange(outbound, Request::CreateExpressionEvaluator)
    }

    /// Prepare `predicate` for repeated evaluation via [`Request::CreatePredicateEvaluator`].
    fn create_predicate_evaluator(
        &self,
        input_schema: SchemaRef,
        predicate: PredicateRef,
    ) -> impl DeltaFuture<PredicateEvaluatorHandle> {
        let outbound = CreatePredicateEvaluator::new(input_schema, predicate);
        self.exchange(outbound, Request::CreatePredicateEvaluator)
    }

    /// Evaluate a prepared expression against `input` via [`Request::EvaluateExpression`].
    fn evaluate_expression(
        &self,
        evaluator: &ExpressionEvaluatorHandle,
        input: impl Into<EvaluationInput>,
    ) -> impl DeltaFuture<Box<dyn EngineData>> {
        let outbound = EvaluateExpression::new(evaluator.clone(), input.into());
        self.exchange(outbound, Request::EvaluateExpression)
    }

    /// Evaluate a prepared predicate against `input` via [`Request::EvaluatePredicate`].
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

/// Shorthand for a bound kernel result stream after [`Generator::bind`].
///
/// When json/parquet (or other) arms must share a type, erase with [`BoundGenerator::into_boxed`].
#[internal_api]
pub(crate) trait BoundDeltaResultGenerator<T, O = ()>:
    BoundGenerator<Item = DeltaResult<T>, Output = O> + Unpin
where
    T: Send,
    O: Send + 'static,
{
}

impl<G, T, O> BoundDeltaResultGenerator<T, O> for G
where
    G: BoundGenerator<Item = DeltaResult<T>, Output = O> + Unpin,
    T: Send,
    O: Send + 'static,
{
}

/// Active streaming kernel operation using an existing request channel.
///
/// Connector requests surface through the driver that owns the channel. The `'a` lifetime
/// covers that channel and any state captured by the operation.
#[internal_api]
pub(crate) type GeneratorBody<'a, Y, O = ()> = generator::GeneratorBody<'a, Y, O>;

pub(crate) type GeneratorChannel<'a, Y> = generator::GeneratorChannel<'a, Request, Y>;
pub(crate) use generator::generator_fn;

/// Creates a [`Coroutine`](super::Coroutine) that may borrow captured state.
#[internal_api]
macro_rules! coroutine {
    ($body:expr) => {
        $crate::coroutine::generator::coroutine!($crate::coroutine::kernel::Request, $body)
    };
}

/// Creates an unpolled [`StaticCoroutine`] from a static generator body.
#[internal_api]
macro_rules! static_coroutine {
    ($body:expr) => {
        $crate::coroutine::generator::static_coroutine!($crate::coroutine::kernel::Request, $body)
    };
}

/// Creates a [`Workflow`](super::Workflow) that may borrow captured state.
#[internal_api]
macro_rules! workflow {
    ($body:expr) => {
        $crate::coroutine::workflow!($crate::coroutine::kernel::Request, $body)
    };
}

/// Creates an unpolled [`StaticWorkflow`](super::StaticWorkflow) from a static workflow body.
#[internal_api]
macro_rules! static_workflow {
    ($body:expr) => {
        $crate::coroutine::static_workflow!($crate::coroutine::kernel::Request, $body)
    };
}
