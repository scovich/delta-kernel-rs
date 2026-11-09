//! Drives Kernel coroutines by serving requests through an [`Engine`].
//!
//! This adapter backs Engine-compatible entry points and coroutine iterators. It handles requests
//! synchronously through Engine handlers; connector-native async execution uses its own driver.

use std::any::Any;
use std::sync::Arc;

use delta_kernel_derive::internal_api;
use derive_more::Constructor;

use super::request::{
    ExpressionEvaluatorHandle, ForwardListing, PredicateEvaluatorHandle, ReadJsonFiles,
    ReadParquetFiles, WriteJsonFile, WriteParquetFile, WriteParquetFiles, WriteParquetFilesResult,
};
#[cfg(feature = "declarative-plans")]
use super::request::{PlanOperation, PlanRequest};
use super::{Coroutine, CoroutineStep, KernelResultGenerator, Request, Workflow};
use crate::coroutine::{Cursor, Page, PageRequest, PagedOperation, Reply, drive_workflow};
#[cfg(feature = "declarative-plans")]
use crate::plans::PlanExecutor;
use crate::{
    Engine, EngineData, EvaluationHandler, ExpressionEvaluator, FileDataReadResultIterator,
    FileMeta, FileSize, JsonHandler, KernelError, KernelResult, KernelResultIterator,
    ParquetHandler, PredicateEvaluator, StorageHandler,
};

/// Maps a paged request onto synchronous Engine state stored in an opaque cursor.
trait EnginePagination<Op: PagedOperation> {
    type State: Any + Send;

    fn prepare(&self, operation: Op) -> KernelResult<Self::State>;

    fn next(&self, state: Self::State) -> KernelResult<Page<Op>>;
}

fn respond_paged<C, Op>(connector: &C, request: PageRequest<Op>) -> KernelResult<()>
where
    C: EnginePagination<Op>,
    Op: PagedOperation,
{
    match request {
        PageRequest::Prepare(operation, reply) => {
            complete(reply, connector.prepare(operation).map(Cursor::new))
        }
        PageRequest::Next(cursor, reply) => complete(
            reply,
            cursor.into_inner().and_then(|state| connector.next(state)),
        ),
    }
}

/// Serves coroutine requests through handlers captured from an [`Engine`].
///
/// Engine only exposes handlers through borrowed access, so this adapter captures them eagerly.
/// Cancellation is inherited from those handlers (for example a [`CancellableEngine`] wrapper),
/// not installed on this adapter.
///
/// [`CancellableEngine`]: crate::engine::CancellableEngine
#[derive(Clone)]
pub(crate) struct EngineConnector {
    storage: Arc<dyn StorageHandler>,
    json: Arc<dyn JsonHandler>,
    parquet: Arc<dyn ParquetHandler>,
    evaluation: Arc<dyn EvaluationHandler>,
    #[cfg(feature = "declarative-plans")]
    plan_executor: Option<Arc<dyn PlanExecutor>>,
}

impl EngineConnector {
    /// Capture the handlers exposed by `engine`.
    pub(crate) fn new(engine: &dyn Engine) -> Self {
        Self {
            storage: engine.storage_handler(),
            json: engine.json_handler(),
            parquet: engine.parquet_handler(),
            evaluation: engine.evaluation_handler(),
            #[cfg(feature = "declarative-plans")]
            plan_executor: engine.plan_executor(),
        }
    }

    /// Drive an existing workflow through this connector.
    pub(crate) fn drive<O, E: From<KernelError>>(
        &self,
        workflow: Workflow<'_, O, E>,
    ) -> KernelResult<O, E> {
        drive_connector(self, workflow)
    }

    /// Prepare a fallible result coroutine and return an iterator that drives it and its requests.
    pub(crate) fn iterate_prepared_result_coroutine<'a, G, Item, O>(
        self,
        workflow: Workflow<'_, G>,
    ) -> KernelResult<EngineResultCoroutineIterator<'a, Item, O>>
    where
        G: KernelResultGenerator<Item, O> + 'a,
        Item: Send + 'static,
        O: Send + 'static,
    {
        let coroutine = Coroutine::from_generator(self.drive(workflow)?);
        Ok(self.iterate_result_coroutine(coroutine))
    }

    /// Prepare a fallible result coroutine and split its result with `into_parts` into a generator
    /// and a companion value; return an iterator that drives the generator, plus the companion.
    pub(crate) fn iterate_prepared_result_coroutine_with_companion<'a, R, G, Item, Companion>(
        self,
        workflow: Workflow<'_, R>,
        into_parts: impl FnOnce(R) -> (G, Companion),
    ) -> KernelResult<(EngineResultCoroutineIterator<'a, Item>, Companion)>
    where
        G: KernelResultGenerator<Item> + 'a,
        Item: Send + 'static,
    {
        let (generator, companion) = into_parts(self.drive(workflow)?);
        let coroutine = Coroutine::from_generator(generator);
        Ok((self.iterate_result_coroutine(coroutine), companion))
    }

    /// Return an iterator that drives `coroutine` and its connector requests.
    pub(crate) fn iterate_coroutine<'a, Item: Send + 'static, O: Send + 'static>(
        self,
        coroutine: impl Into<Coroutine<'a, Item, O>>,
    ) -> EngineCoroutineIterator<'a, Item, O> {
        EngineCoroutineIterator {
            connector: self,
            state: CoroutineState::Active(coroutine.into()),
        }
    }

    /// Like [`iterate_coroutine`](Self::iterate_coroutine), but for coroutines that yield
    /// [`KernelResult`] items.
    pub(crate) fn iterate_result_coroutine<'a, Item: Send + 'static, O: Send + 'static>(
        self,
        coroutine: impl Into<Coroutine<'a, KernelResult<Item>, O>>,
    ) -> EngineResultCoroutineIterator<'a, Item, O> {
        EngineResultCoroutineIterator(self.iterate_coroutine(coroutine))
    }

    fn storage_connector(&self) -> StorageConnector<'_> {
        StorageConnector::new(self.storage.as_ref())
    }

    #[cfg(feature = "declarative-plans")]
    fn plan_executor(&self) -> KernelResult<&dyn PlanExecutor> {
        self.plan_executor
            .as_deref()
            .ok_or_else(|| KernelError::unsupported("this engine does not provide a PlanExecutor"))
    }

    fn write_json_file(&self, operation: WriteJsonFile) -> KernelResult<FileSize> {
        let iterator = self.clone().iterate_result_coroutine(operation.input);
        self.json
            .write_json_file(&operation.url, Box::new(iterator), operation.overwrite)
    }

    fn write_parquet_file(&self, operation: WriteParquetFile) -> KernelResult<FileSize> {
        let WriteParquetFile { url, input } = operation;
        let iterator = self.clone().iterate_result_coroutine(input);
        self.parquet.write_parquet_file(url, Box::new(iterator))
    }

    fn write_parquet_files(
        &self,
        operation: WriteParquetFiles,
    ) -> KernelResult<WriteParquetFilesResult> {
        let WriteParquetFiles { write, input } = operation;
        let iterator = self.clone().iterate_result_coroutine(input);
        self.parquet.write_parquet_files(write, Box::new(iterator))
    }
}

impl ConnectorRespond for EngineConnector {
    fn respond(&self, request: Request) -> KernelResult<()> {
        match request {
            Request::ListForward(request) => respond_paged(&self.storage_connector(), request)?,
            Request::ReadSmallFile(..) => self.storage_connector().respond(request)?,
            Request::Head(..) => self.storage_connector().respond(request)?,
            Request::ReadParquetFooter(file, reply) => {
                complete(reply, self.parquet.read_parquet_footer(&file))?
            }
            Request::ReadJson(request) => respond_paged(self, request)?,
            Request::ReadParquet(request) => respond_paged(self, request)?,
            #[cfg(feature = "declarative-plans")]
            Request::ExecutePlan(PlanRequest::Available(reply)) => {
                complete(reply, Ok(self.plan_executor.is_some()))?
            }
            #[cfg(feature = "declarative-plans")]
            Request::ExecutePlan(PlanRequest::Query(request)) => {
                self.plan_executor()?;
                respond_paged(self, request)?
            }
            #[cfg(feature = "declarative-plans")]
            Request::ExecutePlan(PlanRequest::ParquetFooter(file, reply)) => {
                let executor = self.plan_executor()?;
                complete(reply, executor.read_parquet_footer(file))?
            }
            #[cfg(not(feature = "declarative-plans"))]
            Request::ExecutePlan(..) => {
                return Err(KernelError::internal_error(
                    "Engine connector does not execute declarative plans",
                ));
            }
            Request::CreateEngineData(operation, reply) => complete(
                reply,
                self.evaluation
                    .create_many(operation.schema, operation.rows),
            )?,
            Request::CreateExpressionEvaluator(operation, reply) => complete(
                reply,
                self.evaluation
                    .new_expression_evaluator(
                        operation.input_schema,
                        operation.expression,
                        operation.output_type,
                    )
                    .map(|evaluator| Arc::new(evaluator) as _),
            )?,
            Request::CreatePredicateEvaluator(operation, reply) => complete(
                reply,
                self.evaluation
                    .new_predicate_evaluator(operation.input_schema, operation.predicate)
                    .map(|evaluator| Arc::new(evaluator) as _),
            )?,
            Request::EvaluateExpression(operation, reply) => complete(
                reply,
                expression_evaluator(&operation.evaluator)
                    .and_then(|evaluator| evaluator.evaluate(operation.input.as_ref())),
            )?,
            Request::EvaluatePredicate(operation, reply) => complete(
                reply,
                predicate_evaluator(&operation.evaluator)
                    .and_then(|evaluator| evaluator.evaluate(operation.input.as_ref())),
            )?,
            Request::WriteJson(operation, reply) => {
                complete(reply, self.write_json_file(*operation))?
            }
            Request::WriteParquet(operation, reply) => {
                // The write request's input must drop before size verification takes the state.
                complete(reply, self.write_parquet_file(*operation))?
            }
            Request::WriteParquetFiles(operation, reply) => {
                // The write request's input must drop before size verification takes the state.
                complete(reply, self.write_parquet_files(*operation))?
            }
        }
        Ok(())
    }
}

/// Drive a workflow using only storage operations.
#[internal_api]
pub(crate) fn drive_with_storage<O>(
    storage: &dyn StorageHandler,
    workflow: Workflow<'_, O>,
) -> KernelResult<O> {
    drive_connector(&StorageConnector::new(storage), workflow)
}

/// Drives a workflow using only declarative plan operations.
#[cfg(feature = "declarative-plans")]
#[internal_api]
pub(crate) fn drive_with_plan_executor<O>(
    executor: &dyn PlanExecutor,
    workflow: Workflow<'_, O>,
) -> KernelResult<O> {
    drive_connector(&PlanExecutorConnector(executor), workflow)
}

/// Prepare and iterate a fallible result coroutine through a storage handler.
#[internal_api]
pub(crate) fn iterate_prepared_result_coroutine_with_storage<'a, G, Item>(
    storage: &'a dyn StorageHandler,
    workflow: Workflow<'_, G>,
) -> KernelResult<impl KernelResultIterator<Item> + 'a>
where
    G: KernelResultGenerator<Item> + 'a,
    Item: Send + 'static,
{
    let coroutine = Coroutine::from_generator(drive_with_storage(storage, workflow)?);
    Ok(EngineResultCoroutineIterator(EngineCoroutineIterator {
        connector: StorageConnector::new(storage),
        state: CoroutineState::Active(coroutine),
    }))
}

/// Drive `workflow` through `engine`.
#[internal_api]
pub(crate) fn drive_with_engine<O, E: From<KernelError>>(
    engine: &dyn Engine,
    workflow: Workflow<'_, O, E>,
) -> KernelResult<O, E> {
    EngineConnector::new(engine).drive(workflow)
}

/// Return an iterator that drives a fallible result coroutine through `engine`.
#[internal_api]
#[cfg_attr(not(feature = "internal-api"), expect(dead_code))]
pub(crate) fn iterate_result_coroutine_with_engine<'a, Item: Send + 'static, O: Send + 'static>(
    engine: &dyn Engine,
    coroutine: impl Into<Coroutine<'a, KernelResult<Item>, O>>,
) -> impl Iterator<Item = KernelResult<Item>> + Send + 'a {
    EngineConnector::new(engine).iterate_result_coroutine(coroutine)
}

/// Prepare and iterate a fallible result coroutine through one connector captured from `engine`.
#[doc(hidden)]
#[internal_api]
pub(crate) fn iterate_prepared_result_coroutine_with_engine<'a, G, Item>(
    engine: &dyn Engine,
    workflow: Workflow<'_, G>,
) -> KernelResult<impl Iterator<Item = KernelResult<Item>> + Send + 'a + use<'a, G, Item>>
where
    G: KernelResultGenerator<Item> + 'a,
    Item: Send + 'static,
{
    EngineConnector::new(engine).iterate_prepared_result_coroutine(workflow)
}

/// Construct, prepare, and iterate a fallible result coroutine through `engine`.
#[internal_api]
macro_rules! iterate_prepared_result_coroutine_with_engine {
    ($engine:expr, $($body:tt)+) => {{
        let workflow = $crate::coroutine::kernel::workflow!($($body)+);
        $crate::coroutine::kernel::iterate_prepared_result_coroutine_with_engine(
            $engine,
            workflow,
        )
    }};
}

/// Construct and drive a workflow through an [`Engine`].
#[internal_api]
macro_rules! run_workflow_with_engine {
    ($engine:expr, $($body:tt)+) => {
        $crate::coroutine::kernel::drive_with_engine(
            $engine,
            $crate::coroutine::kernel::workflow!($($body)+),
        )
    };
}

/// Construct and drive a workflow through a storage handler.
#[internal_api]
macro_rules! run_workflow_with_storage {
    ($storage:expr, $($body:tt)+) => {
        $crate::coroutine::kernel::drive_with_storage(
            $storage,
            $crate::coroutine::kernel::workflow!($($body)+),
        )
    };
}

/// Iterator over yielded items that serves the coroutine's connector requests.
pub(crate) struct EngineCoroutineIterator<
    'a,
    Item: Send + 'static,
    O: Send + 'static = (),
    C = EngineConnector,
> {
    connector: C,
    state: CoroutineState<'a, Item, O>,
}

/// Iterator over fallible yielded items that also serves coroutine requests.
pub(crate) struct EngineResultCoroutineIterator<
    'a,
    Item: Send + 'static,
    O: Send + 'static = (),
    C = EngineConnector,
>(EngineCoroutineIterator<'a, KernelResult<Item>, O, C>);

impl<C: ConnectorRespond, Item: Send + 'static, O: Send + 'static> Iterator
    for EngineCoroutineIterator<'_, Item, O, C>
{
    type Item = KernelResult<Item>;

    fn next(&mut self) -> Option<Self::Item> {
        next_coroutine(&self.connector, &mut self.state)
    }
}

impl<C: ConnectorRespond, Item: Send + 'static, O: Send + 'static> Iterator
    for EngineResultCoroutineIterator<'_, Item, O, C>
{
    type Item = KernelResult<Item>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|result| result?)
    }
}

impl<Item: Send + 'static, O: Send + 'static, C> EngineResultCoroutineIterator<'_, Item, O, C> {
    /// Returns whether the coroutine completed and stored its terminal output.
    pub(crate) fn is_complete(&self) -> bool {
        matches!(self.0.state, CoroutineState::Complete(_))
    }

    /// Returns the terminal output after this iterator has been exhausted.
    pub(crate) fn into_output(self) -> KernelResult<O> {
        match self.0.state {
            CoroutineState::Complete(output) => Ok(output),
            CoroutineState::Active(_) => Err(KernelError::internal_error(
                "coroutine iterator must be exhausted before taking its output",
            )),
            CoroutineState::Failed => Err(KernelError::internal_error(
                "coroutine iterator failed before producing its output",
            )),
        }
    }
}

enum CoroutineState<'a, Item: Send + 'static, O: Send + 'static> {
    Active(Coroutine<'a, Item, O>),
    Complete(O),
    Failed,
}

/// Serves workflows that require only declarative plan execution.
#[cfg(feature = "declarative-plans")]
struct PlanExecutorConnector<'a>(&'a dyn PlanExecutor);

#[cfg(feature = "declarative-plans")]
impl ConnectorRespond for PlanExecutorConnector<'_> {
    fn respond(&self, request: Request) -> KernelResult<()> {
        match request {
            Request::ExecutePlan(PlanRequest::Available(reply)) => complete(reply, Ok(true)),
            Request::ExecutePlan(PlanRequest::Query(request)) => respond_paged(self, request),
            Request::ExecutePlan(PlanRequest::ParquetFooter(file, reply)) => {
                complete(reply, self.0.read_parquet_footer(file))
            }
            Request::ListForward(_)
            | Request::ReadSmallFile(..)
            | Request::Head(..)
            | Request::ReadParquetFooter(..)
            | Request::ReadJson(_)
            | Request::ReadParquet(_)
            | Request::CreateEngineData(..)
            | Request::CreateExpressionEvaluator(..)
            | Request::CreatePredicateEvaluator(..)
            | Request::EvaluateExpression(..)
            | Request::EvaluatePredicate(..)
            | Request::WriteJson(..)
            | Request::WriteParquet(..)
            | Request::WriteParquetFiles(..) => Err(KernelError::internal_error(
                "plan-executor workflow requested a non-plan operation",
            )),
        }
    }
}

/// Serves entry points that intentionally require only storage access.
#[derive(Constructor)]
struct StorageConnector<'a> {
    storage: &'a dyn StorageHandler,
}

impl ConnectorRespond for StorageConnector<'_> {
    fn respond(&self, request: Request) -> KernelResult<()> {
        match request {
            Request::ListForward(request) => respond_paged(self, request),
            Request::ReadSmallFile(file, reply) => {
                complete(reply, self.storage.read_small_file(file))
            }
            Request::Head(url, reply) => complete(reply, self.storage.head(&url)),
            Request::ReadJson(_) => Err(KernelError::internal_error(
                "JSON reads require an Engine connector",
            )),
            Request::ReadParquetFooter(..) | Request::ReadParquet(_) => Err(
                KernelError::internal_error("Parquet reads require an Engine connector"),
            ),
            Request::ExecutePlan(..) => Err(KernelError::internal_error(
                "declarative plans require a connector with a plan executor",
            )),
            Request::CreateEngineData(..)
            | Request::CreateExpressionEvaluator(..)
            | Request::CreatePredicateEvaluator(..)
            | Request::EvaluateExpression(..)
            | Request::EvaluatePredicate(..) => Err(KernelError::internal_error(
                "evaluation requires an Engine connector",
            )),
            Request::WriteJson(..) | Request::WriteParquet(..) | Request::WriteParquetFiles(..) => {
                Err(KernelError::internal_error(
                    "file writes require an Engine connector",
                ))
            }
        }
    }
}

/// Serves one coroutine request, replying through the request's reply handle.
trait ConnectorRespond {
    fn respond(&self, request: Request) -> KernelResult<()>;
}

fn drive_connector<O, E: From<KernelError>>(
    connector: &impl ConnectorRespond,
    workflow: Workflow<'_, O, E>,
) -> KernelResult<O, E> {
    let mut cancellation = CancellationLatch::default();
    let output = drive_workflow!(workflow, |request| {
        cancellation.record(connector.respond(request))?
    });
    Ok(cancellation.finish(output)?)
}

impl EnginePagination<ForwardListing> for StorageConnector<'_> {
    type State = Box<dyn KernelResultIterator<FileMeta>>;

    fn prepare(&self, ForwardListing(bounds): ForwardListing) -> KernelResult<Self::State> {
        let listing = self.storage.list_from(&bounds.low, Some(&bounds.high))?;
        Ok(Box::new(listing))
    }

    fn next(&self, listing: Self::State) -> KernelResult<Page<ForwardListing>> {
        next_page(listing, ForwardListing::DEFAULT_PAGE_SIZE)
    }
}

#[cfg(feature = "declarative-plans")]
impl EnginePagination<PlanOperation> for EngineConnector {
    type State = FileDataReadResultIterator;

    fn prepare(&self, operation: PlanOperation) -> KernelResult<Self::State> {
        self.plan_executor()?.execute_op(operation)?.into_data()
    }

    fn next(&self, state: Self::State) -> KernelResult<Page<PlanOperation>> {
        next_engine_data(state)
    }
}

#[cfg(feature = "declarative-plans")]
impl EnginePagination<PlanOperation> for PlanExecutorConnector<'_> {
    type State = FileDataReadResultIterator;

    fn prepare(&self, operation: PlanOperation) -> KernelResult<Self::State> {
        self.0.execute_op(operation)?.into_data()
    }

    fn next(&self, state: Self::State) -> KernelResult<Page<PlanOperation>> {
        next_engine_data(state)
    }
}

impl EnginePagination<ReadJsonFiles> for EngineConnector {
    type State = FileDataReadResultIterator;

    fn prepare(&self, read: ReadJsonFiles) -> KernelResult<Self::State> {
        self.json
            .read_json_files(&read.files, read.physical_schema, read.predicate)
    }

    fn next(&self, state: Self::State) -> KernelResult<Page<ReadJsonFiles>> {
        next_engine_data(state)
    }
}

impl EnginePagination<ReadParquetFiles> for EngineConnector {
    type State = FileDataReadResultIterator;

    fn prepare(&self, read: ReadParquetFiles) -> KernelResult<Self::State> {
        self.parquet
            .read_parquet_files(&read.files, read.physical_schema, read.predicate)
    }

    fn next(&self, state: Self::State) -> KernelResult<Page<ReadParquetFiles>> {
        next_engine_data(state)
    }
}

#[derive(Default)]
struct CancellationLatch(bool);

impl CancellationLatch {
    fn record(&mut self, result: KernelResult<()>) -> KernelResult<()> {
        match result {
            Err(KernelError::Cancelled) => {
                self.0 = true;
                Ok(())
            }
            result => result,
        }
    }

    fn finish<O>(self, output: O) -> KernelResult<O> {
        if self.0 {
            Err(KernelError::Cancelled)
        } else {
            Ok(output)
        }
    }
}

fn expression_evaluator(
    handle: &ExpressionEvaluatorHandle,
) -> KernelResult<&Arc<dyn ExpressionEvaluator>> {
    handle
        .downcast_ref()
        .ok_or_else(|| KernelError::internal_error("invalid Engine expression evaluator handle"))
}

fn predicate_evaluator(
    handle: &PredicateEvaluatorHandle,
) -> KernelResult<&Arc<dyn PredicateEvaluator>> {
    handle
        .downcast_ref()
        .ok_or_else(|| KernelError::internal_error("invalid Engine predicate evaluator handle"))
}

fn complete<In: Send + 'static>(reply: Reply<In>, result: KernelResult<In>) -> KernelResult<()> {
    let cancelled = matches!(result, Err(KernelError::Cancelled));
    reply.send(result);
    if cancelled {
        Err(KernelError::Cancelled)
    } else {
        Ok(())
    }
}

/// Data reads return one batch per page, which keeps pulls lazy.
fn next_engine_data<Op>(reads: FileDataReadResultIterator) -> KernelResult<Page<Op>>
where
    Op: PagedOperation<Page = Vec<KernelResult<Box<dyn EngineData>>>>,
{
    next_page(reads, 1)
}

/// Pulls up to `size` (which must be positive) items from `items` into one page.
///
/// A full page carries a cursor for the remaining items; a short page ends the stream. An
/// item-level [`KernelError::Cancelled`] becomes a page error so the drive aborts like it does for
/// every other cancelled request.
fn next_page<Op, T>(
    mut items: impl KernelResultIterator<T> + 'static,
    size: usize,
) -> KernelResult<Page<Op>>
where
    Op: PagedOperation<Page = Vec<KernelResult<T>>>,
    T: Send + 'static,
{
    let mut data = Vec::new();
    for item in items.by_ref().take(size) {
        if matches!(item, Err(KernelError::Cancelled)) {
            return Err(KernelError::Cancelled);
        }
        data.push(item);
    }
    let next = (data.len() == size).then(|| Cursor::new(items));
    Ok(Page { data, next })
}

fn next_coroutine<C: ConnectorRespond, Item: Send + 'static, O: Send + 'static>(
    connector: &C,
    state: &mut CoroutineState<'_, Item, O>,
) -> Option<KernelResult<Item>> {
    let CoroutineState::Active(coroutine) = state else {
        return None;
    };
    loop {
        match coroutine.try_advance() {
            Ok(CoroutineStep::Done(output)) => {
                *state = CoroutineState::Complete(output);
                return None;
            }
            Ok(CoroutineStep::Yield(item)) => return Some(Ok(item)),
            Ok(CoroutineStep::Request(request)) => {
                if let Err(err) = connector.respond(request) {
                    *state = CoroutineState::Failed;
                    return Some(Err(err));
                }
            }
            Err(err) => {
                *state = CoroutineState::Failed;
                return Some(Err(err));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use bytes::Bytes;
    use rstest::rstest;
    use url::Url;

    use super::*;
    use crate::coroutine::kernel::{ChannelExt as _, workflow};
    use crate::engine::sync::SyncEngine;
    use crate::engine::{CancellableEngine, DelegatingEngine};
    use crate::unit_test_utils::TestCancellationToken;
    use crate::{CancellationTokenRef, FileMeta, FileSlice, KernelResultIterator};

    struct CancelAfterFirstRead {
        token: Arc<TestCancellationToken>,
    }

    impl StorageHandler for CancelAfterFirstRead {
        fn list_from(
            &self,
            _path: &Url,
            _end: Option<&Url>,
        ) -> KernelResult<Box<dyn KernelResultIterator<FileMeta>>> {
            Err(KernelError::generic("unused listing"))
        }

        fn read_files(
            &self,
            _files: Vec<FileSlice>,
        ) -> KernelResult<Box<dyn KernelResultIterator<Bytes>>> {
            let token = Arc::clone(&self.token);
            Ok(Box::new(std::iter::once_with(move || {
                token.cancel();
                Ok(Bytes::from_static(b"data"))
            })))
        }

        fn copy_atomic(&self, _src: &Url, _dest: &Url) -> KernelResult<()> {
            Err(KernelError::generic("unused copy"))
        }

        fn put(&self, _path: &Url, _data: Bytes, _overwrite: bool) -> KernelResult<()> {
            Err(KernelError::generic("unused write"))
        }

        fn head(&self, _path: &Url) -> KernelResult<FileMeta> {
            Err(KernelError::generic("unused head"))
        }

        fn delete(&self, _path: &Url) -> KernelResult<()> {
            Err(KernelError::generic("unused delete"))
        }
    }

    #[test]
    fn storage_driver_does_not_let_workflow_swallow_cancellation() {
        let token = Arc::new(TestCancellationToken::default());
        let cancellation_token: CancellationTokenRef = token.clone();
        let storage = Arc::new(CancelAfterFirstRead { token });
        let engine = CancellableEngine::new(
            &DelegatingEngine::new(Arc::new(SyncEngine::new())).with_storage_handler(storage),
            cancellation_token,
        );
        let workflow = workflow!(async move |channel| {
            let _ = channel
                .read_small_file(Url::parse("memory:///data")?, None)
                .await;
            Ok(())
        });

        let result = drive_with_engine(&engine, workflow);

        assert!(matches!(result, Err(KernelError::Cancelled)));
    }

    struct Numbers;

    impl PagedOperation for Numbers {
        type Page = Vec<KernelResult<usize>>;
    }

    #[rstest]
    #[case::many_single_item_pages(5, 1, &[1, 1, 1, 1, 1, 0])]
    #[case::one_many_item_page(5, 100, &[5])]
    #[case::many_many_item_pages(10, 3, &[3, 3, 3, 1])]
    #[case::exact_multiple_of_page_size(9, 3, &[3, 3, 3, 0])]
    #[case::exactly_one_full_page(3, 3, &[3, 0])]
    #[case::empty(0, 3, &[0])]
    fn next_page_splits_items_into_pages(
        #[case] count: usize,
        #[case] size: usize,
        #[case] page_lens: &[usize],
    ) {
        let mut items: Box<dyn KernelResultIterator<usize>> = Box::new((0..count).map(Ok));
        let mut lens = Vec::new();
        let mut seen = Vec::new();
        loop {
            let page: Page<Numbers> = next_page(items, size).unwrap();
            lens.push(page.data.len());
            seen.extend(page.data.into_iter().map(Result::unwrap));
            let Some(cursor) = page.next else {
                break;
            };
            items = cursor.into_inner().unwrap();
        }

        assert_eq!(lens, page_lens);
        assert_eq!(seen, Vec::from_iter(0..count));
    }
}
