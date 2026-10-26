//! Drives Kernel coroutines by serving requests through an [`Engine`].
//!
//! This adapter backs Engine-compatible entry points and coroutine iterators. It handles requests
//! synchronously through Engine handlers; connector-native async execution uses its own driver.

use std::any::Any;
use std::borrow::Borrow;
use std::sync::Arc;

use delta_kernel_derive::internal_api;
use derive_more::Constructor;

use super::request::{
    ExpressionEvaluatorHandle, ForwardListing, PredicateEvaluatorHandle, ReadJsonFiles,
    ReadParquetFiles,
};
use super::{Coroutine, CoroutineStep, KernelResultGenerator, Request, Workflow};
use crate::cancellation::{check_cancelled, CancellationTokenRef};
use crate::committer::Committer;
use crate::coroutine::{drive_workflow, Cursor, Page, PageRequest, PagedOperation, Reply};
use crate::{
    Engine, EngineData, EvaluationHandler, ExpressionEvaluator, FileDataReadResultIterator,
    FileMeta, JsonHandler, KernelError, KernelResult, KernelResultIterator, ParquetHandler,
    PredicateEvaluator, StorageHandler,
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
#[derive(Clone)]
pub(crate) struct EngineConnector {
    storage: Arc<dyn StorageHandler>,
    json: Arc<dyn JsonHandler>,
    parquet: Arc<dyn ParquetHandler>,
    evaluation: Arc<dyn EvaluationHandler>,
    cancellation_token: Option<CancellationTokenRef>,
}

impl EngineConnector {
    /// Capture the handlers exposed by `engine`.
    pub(crate) fn new(engine: &dyn Engine) -> Self {
        Self {
            storage: engine.storage_handler(),
            json: engine.json_handler(),
            parquet: engine.parquet_handler(),
            evaluation: engine.evaluation_handler(),
            cancellation_token: None,
        }
    }

    /// Configure the cancellation token propagated to Engine handlers.
    pub(crate) fn with_cancellation_token(
        mut self,
        cancellation_token: impl Into<Option<CancellationTokenRef>>,
    ) -> Self {
        self.cancellation_token = cancellation_token.into();
        self
    }

    /// Drive an existing workflow through this connector.
    pub(crate) fn drive<O>(&self, workflow: Workflow<'_, O>) -> KernelResult<O> {
        let mut cancellation = CancellationLatch::default();
        let output = drive_workflow!(workflow, |request| {
            cancellation.record(self.respond(request))?
        });
        cancellation.finish(output)
    }

    /// Drives a workflow using `committer` for commit requests.
    pub(crate) fn drive_with_committer<O>(
        &self,
        workflow: Workflow<'_, O>,
        engine: &dyn Engine,
        committer: &dyn Committer,
    ) -> KernelResult<O> {
        let mut cancellation = CancellationLatch::default();
        let output = drive_workflow!(workflow, |request| {
            let result = match request {
                Request::Commit(commit, reply) => {
                    complete(reply, committer.commit(engine, *commit))
                }
                request => self.respond(request),
            };
            cancellation.record(result)?
        });
        cancellation.finish(output)
    }

    /// Drives a workflow using `committer` for catalog publish requests.
    pub(crate) fn drive_with_catalog_committer<O>(
        &self,
        workflow: Workflow<'_, O>,
        engine: &dyn Engine,
        committer: &dyn Committer,
    ) -> KernelResult<O> {
        let mut cancellation = CancellationLatch::default();
        let output = drive_workflow!(workflow, |request| {
            let result = match request {
                Request::RequireCatalogCommitter(reply) => {
                    let result = if committer.is_catalog_committer() {
                        Ok(())
                    } else {
                        Err(KernelError::generic(
                            "There are catalog commits that need publishing, but the committer is \
                             not a catalog committer.",
                        ))
                    };
                    complete(reply, result)
                }
                Request::Publish(metadata, reply) => {
                    complete(reply, committer.publish(engine, metadata))
                }
                request => self.respond(request),
            };
            cancellation.record(result)?
        });
        cancellation.finish(output)
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

    /// Prepare a fallible result coroutine and return its iterator and eager companion value.
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
        StorageConnector::new(self.storage.as_ref(), self.cancellation_token.clone())
    }

    fn check_cancelled(&self) -> KernelResult<()> {
        check_cancelled(self.cancellation_token.as_ref())
    }

    fn respond(&self, request: Request) -> KernelResult<()> {
        match request {
            Request::ListForward(request) => respond_paged(&self.storage_connector(), request)?,
            Request::ReadSmallFile(..) => self.storage_connector().respond(request)?,
            Request::ReadParquetFooter(file, reply) => complete(
                reply,
                self.parquet
                    .read_parquet_footer_with_cancellation(&file, self.cancellation_token.clone()),
            )?,
            Request::ReadJson(request) => respond_paged(self, request)?,
            Request::ReadParquet(request) => respond_paged(self, request)?,
            Request::CreateEngineData(operation, reply) => complete(
                reply,
                self.check_cancelled().and_then(|_| {
                    self.evaluation
                        .create_many(operation.schema, operation.rows)
                }),
            )?,
            Request::CreateExpressionEvaluator(operation, reply) => complete(
                reply,
                self.check_cancelled()
                    .and_then(|_| {
                        self.evaluation.new_expression_evaluator(
                            operation.input_schema,
                            operation.expression,
                            operation.output_type,
                        )
                    })
                    .map(|evaluator| Arc::new(evaluator) as _),
            )?,
            Request::CreatePredicateEvaluator(operation, reply) => complete(
                reply,
                self.check_cancelled()
                    .and_then(|_| {
                        self.evaluation
                            .new_predicate_evaluator(operation.input_schema, operation.predicate)
                    })
                    .map(|evaluator| Arc::new(evaluator) as _),
            )?,
            Request::EvaluateExpression(operation, reply) => complete(
                reply,
                self.check_cancelled().and_then(|_| {
                    expression_evaluator(&operation.evaluator)
                        .and_then(|evaluator| evaluator.evaluate(operation.input.as_ref()))
                }),
            )?,
            Request::EvaluatePredicate(operation, reply) => complete(
                reply,
                self.check_cancelled().and_then(|_| {
                    predicate_evaluator(&operation.evaluator)
                        .and_then(|evaluator| evaluator.evaluate(operation.input.as_ref()))
                }),
            )?,
            Request::Commit(..) | Request::RequireCatalogCommitter(..) | Request::Publish(..) => {
                return Err(KernelError::internal_error(
                    "committer request requires a committer-aware driver",
                ));
            }
        }
        Ok(())
    }
}

/// Drive a workflow using only storage operations.
#[internal_api]
pub(crate) fn drive_with_storage<O>(
    storage: &dyn StorageHandler,
    cancellation_token: Option<CancellationTokenRef>,
    workflow: Workflow<'_, O>,
) -> KernelResult<O> {
    let connector = StorageConnector::new(storage, cancellation_token.clone());
    let mut cancellation = CancellationLatch::default();
    let output = drive_workflow!(workflow, |request| {
        cancellation.record(connector.respond(request))?
    });
    cancellation.finish(output)
}

/// Drive `workflow` through `engine`.
///
/// Pass a cancellation token to install it on the Engine connector. `None` matches the
/// two-argument [`run_workflow_with_engine`] form.
#[internal_api]
pub(crate) fn drive_with_engine<O>(
    engine: &dyn Engine,
    cancellation_token: impl Into<Option<CancellationTokenRef>>,
    workflow: Workflow<'_, O>,
) -> KernelResult<O> {
    let connector = EngineConnector::new(engine).with_cancellation_token(cancellation_token);
    connector.drive(workflow)
}

/// Return an iterator that drives a fallible result coroutine through `engine`.
#[internal_api]
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
    cancellation_token: impl Into<Option<CancellationTokenRef>>,
    workflow: Workflow<'_, G>,
) -> KernelResult<impl Iterator<Item = KernelResult<Item>> + Send + 'a>
where
    G: KernelResultGenerator<Item> + 'a,
    Item: Send + 'static,
{
    let connector = EngineConnector::new(engine).with_cancellation_token(cancellation_token);
    connector.iterate_prepared_result_coroutine(workflow)
}

/// Construct, prepare, and iterate a fallible result coroutine through `engine`.
#[internal_api]
macro_rules! iterate_prepared_result_coroutine_with_engine {
    ($engine:expr, $cancellation_token:expr, $($body:tt)+) => {{
        let workflow = $crate::coroutine::kernel::workflow!($($body)+);
        $crate::coroutine::kernel::iterate_prepared_result_coroutine_with_engine(
            $engine,
            $cancellation_token,
            workflow,
        )
    }};
}

/// Construct and drive a workflow through an [`Engine`].
///
/// The two-argument form does not propagate cancellation. Pass a token as the second argument to
/// install it when the Engine connector is first captured.
#[internal_api]
macro_rules! run_workflow_with_engine {
    ($engine:expr, $cancellation_token:expr, $($body:tt)+) => {
        $crate::coroutine::kernel::drive_with_engine(
            $engine,
            $cancellation_token,
            $crate::coroutine::kernel::workflow!($($body)+),
        )
    };
    ($engine:expr, $($body:tt)+) => {
        $crate::coroutine::kernel::drive_with_engine(
            $engine,
            None,
            $crate::coroutine::kernel::workflow!($($body)+),
        )
    };
}

/// Construct and drive a workflow through a storage handler.
#[internal_api]
macro_rules! run_workflow_with_storage {
    ($storage:expr, $cancellation_token:expr, $($body:tt)+) => {
        $crate::coroutine::kernel::drive_with_storage(
            $storage,
            $cancellation_token,
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

impl<C: Borrow<EngineConnector>, Item: Send + 'static, O: Send + 'static> Iterator
    for EngineCoroutineIterator<'_, Item, O, C>
{
    type Item = KernelResult<Item>;

    fn next(&mut self) -> Option<Self::Item> {
        next_coroutine(self.connector.borrow(), &mut self.state)
    }
}

impl<C: Borrow<EngineConnector>, Item: Send + 'static, O: Send + 'static> Iterator
    for EngineResultCoroutineIterator<'_, Item, O, C>
{
    type Item = KernelResult<Item>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|result| result?)
    }
}

enum CoroutineState<'a, Item: Send + 'static, O: Send + 'static> {
    Active(Coroutine<'a, Item, O>),
    Complete(O),
    Failed,
}

/// Serves entry points that intentionally require only storage access.
#[derive(Constructor)]
struct StorageConnector<'a> {
    storage: &'a dyn StorageHandler,
    cancellation_token: Option<CancellationTokenRef>,
}

impl StorageConnector<'_> {
    fn respond(&self, request: Request) -> KernelResult<()> {
        match request {
            Request::ListForward(request) => respond_paged(self, request),
            Request::ReadSmallFile(file, reply) => complete(
                reply,
                self.storage
                    .read_small_file_with_cancellation(file, self.cancellation_token.clone()),
            ),
            Request::ReadJson(_) => Err(KernelError::internal_error(
                "JSON reads require an Engine connector",
            )),
            Request::ReadParquetFooter(..) | Request::ReadParquet(_) => Err(
                KernelError::internal_error("Parquet reads require an Engine connector"),
            ),
            Request::CreateEngineData(..)
            | Request::CreateExpressionEvaluator(..)
            | Request::CreatePredicateEvaluator(..)
            | Request::EvaluateExpression(..)
            | Request::EvaluatePredicate(..) => Err(KernelError::internal_error(
                "evaluation requires an Engine connector",
            )),
            Request::Commit(..) => Err(KernelError::internal_error(
                "commit request requires an Engine connector with a committer",
            )),
            Request::RequireCatalogCommitter(..) => Err(KernelError::internal_error(
                "catalog committer validation requires an Engine connector with a committer",
            )),
            Request::Publish(..) => Err(KernelError::internal_error(
                "publish request requires an Engine connector with a catalog committer",
            )),
        }
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

impl EnginePagination<ForwardListing> for StorageConnector<'_> {
    type State = Box<dyn KernelResultIterator<FileMeta>>;

    fn prepare(&self, ForwardListing(bounds): ForwardListing) -> KernelResult<Self::State> {
        let listing = self
            .storage
            .list_from_with_cancellation(&bounds.low, self.cancellation_token.clone())?
            .take_while(move |entry| bounds.contains(entry));
        Ok(Box::new(listing))
    }

    fn next(&self, mut listing: Self::State) -> KernelResult<Page<ForwardListing>> {
        let data = Vec::from_iter(listing.by_ref().take(ForwardListing::DEFAULT_PAGE_SIZE));
        if data
            .iter()
            .any(|entry| matches!(entry, Err(KernelError::Cancelled)))
        {
            return Err(KernelError::Cancelled);
        }
        let next = (data.len() == ForwardListing::DEFAULT_PAGE_SIZE).then(|| Cursor::new(listing));
        Ok(Page { data, next })
    }
}

impl EnginePagination<ReadJsonFiles> for EngineConnector {
    type State = FileDataReadResultIterator;

    fn prepare(&self, read: ReadJsonFiles) -> KernelResult<Self::State> {
        self.json.read_json_files_with_cancellation(
            &read.files,
            read.physical_schema,
            read.predicate,
            self.cancellation_token.clone(),
        )
    }

    fn next(&self, state: Self::State) -> KernelResult<Page<ReadJsonFiles>> {
        next_engine_data(state)
    }
}

impl EnginePagination<ReadParquetFiles> for EngineConnector {
    type State = FileDataReadResultIterator;

    fn prepare(&self, read: ReadParquetFiles) -> KernelResult<Self::State> {
        self.parquet.read_parquet_files_with_cancellation(
            &read.files,
            read.physical_schema,
            read.predicate,
            self.cancellation_token.clone(),
        )
    }

    fn next(&self, state: Self::State) -> KernelResult<Page<ReadParquetFiles>> {
        next_engine_data(state)
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

fn next_engine_data<Op>(mut reads: FileDataReadResultIterator) -> KernelResult<Page<Op>>
where
    Op: PagedOperation<Page = Vec<KernelResult<Box<dyn EngineData>>>>,
{
    let (data, next) = match reads.next() {
        Some(Err(KernelError::Cancelled)) => return Err(KernelError::Cancelled),
        Some(data) => (vec![data], Some(Cursor::new(reads))),
        None => (Vec::new(), None),
    };
    Ok(Page { data, next })
}

fn next_coroutine<Item: Send + 'static, O: Send + 'static>(
    connector: &EngineConnector,
    state: &mut CoroutineState<'_, Item, O>,
) -> Option<KernelResult<Item>> {
    let CoroutineState::Active(coroutine) = state else {
        return None;
    };
    if let Err(err) = connector.check_cancelled() {
        *state = CoroutineState::Failed;
        return Some(Err(err));
    }
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
    use url::Url;

    use super::*;
    use crate::coroutine::kernel::{workflow, ChannelExt as _};
    use crate::unit_test_utils::TestCancellationToken;
    use crate::{FileMeta, FileSlice, KernelResultIterator};

    struct CancelAfterFirstRead {
        token: Arc<TestCancellationToken>,
    }

    impl StorageHandler for CancelAfterFirstRead {
        fn list_from(&self, _path: &Url) -> KernelResult<Box<dyn KernelResultIterator<FileMeta>>> {
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
        let storage = CancelAfterFirstRead { token };
        let workflow = workflow!(async move |channel| {
            let _ = channel
                .read_small_file(Url::parse("memory:///data")?, None)
                .await;
            Ok(())
        });

        let result = drive_with_storage(&storage, Some(cancellation_token), workflow);

        assert!(matches!(result, Err(KernelError::Cancelled)));
    }
}
