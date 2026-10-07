//! Drives kernel coroutines by serving requests through an [`Engine`].
//!
//! This adapter backs Engine-compatible entry points and generator iterators. It handles requests
//! synchronously through Engine handlers; connector-native async execution uses its own driver.

use std::any::Any;
use std::borrow::Borrow;
use std::sync::{Arc, Mutex};

use bytes::Bytes;
use delta_kernel_derive::internal_api;
use derive_more::Constructor;

use super::kernel::request::{
    BackwardListing, BackwardListingResult, DirectoryListing, ExpressionEvaluatorHandle,
    ForwardListing, ListingBounds, PredicateEvaluatorHandle, ReadJsonFiles, ReadParquetFiles,
    WriteJsonFile, WriteParquetFile, WriteParquetFiles, WriteParquetFilesResult,
};
use super::kernel::{
    coroutine, ChannelExt as _, Coroutine, GeneratorCoroutine, GeneratorCoroutineStep,
    StaticCoroutine, StaticGeneratorCoroutine,
};
#[cfg(feature = "declarative-plans")]
use super::PlanOperation;
use super::{
    drive_coroutine, Cursor, GeneratorExt as _, IntoGenerator, Page, PageGenerator, PageRequest,
    PagedOperation, PlanRequest, Request,
};
use crate::cancellation::{check_cancelled, CancellationTokenRef};
use crate::committer::Committer;
use crate::engine_data::{EngineData, FilteredEngineData};
#[cfg(feature = "declarative-plans")]
use crate::plans::PlanExecutor;
use crate::{
    DeltaResult, DeltaResultIteratorStatic, Engine, Error, EvaluationHandler, ExpressionEvaluator,
    FileDataReadResultIterator, FileMeta, FileSize, FileSlice, JsonHandler, ParquetHandler,
    PredicateEvaluator, StorageHandler, Version,
};

const FORWARD_LISTING_PAGE_SIZE: usize = if cfg!(test) {
    2
} else {
    ForwardListing::DEFAULT_PAGE_SIZE
};

type ListingIterator = DeltaResultIteratorStatic<FileMeta>;
type EngineDataIterator = FileDataReadResultIterator;

struct BackwardListingState {
    bounds: Box<ListingBounds>,
    high: Version,
}

/// Maps paged requests onto a synchronous Engine iterator stored in an opaque cursor.
trait EnginePagination<Op: PagedOperation> {
    type State: Any + Send;

    fn initialize(&self, operation: Op) -> DeltaResult<Self::State>;

    fn next_page(&self, state: Self::State) -> DeltaResult<Page<Op>>;

    fn start(&self, operation: Op) -> DeltaResult<Page<Op>> {
        self.continue_from(self.prepare(operation)?)
    }

    fn prepare(&self, operation: Op) -> DeltaResult<Cursor<Op>> {
        Ok(Cursor::new(self.initialize(operation)?))
    }

    fn continue_from(&self, cursor: Cursor<Op>) -> DeltaResult<Page<Op>> {
        self.next_page(cursor.into_inner()?)
    }
}

fn respond_paged<C, Op>(connector: &C, request: PageRequest<Op>)
where
    C: EnginePagination<Op>,
    Op: PagedOperation,
{
    match request {
        PageRequest::Start(operation, reply) => {
            reply.send(C::start(connector, operation));
        }
        PageRequest::Prepare(operation, reply) => {
            reply.send(C::prepare(connector, operation));
        }
        PageRequest::Continue(cursor, reply) => {
            reply.send(C::continue_from(connector, cursor));
        }
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
    #[cfg(feature = "declarative-plans")]
    plan_executor: Option<Arc<dyn PlanExecutor>>,
    cancellation_token: Option<CancellationTokenRef>,
}

/// Serves entry points that intentionally require only storage access.
#[derive(Constructor)]
struct StorageConnector<'a> {
    storage: &'a dyn StorageHandler,
    cancellation_token: Option<CancellationTokenRef>,
}

#[cfg(feature = "declarative-plans")]
struct PlanExecutorConnector<'a> {
    executor: &'a dyn PlanExecutor,
}

/// Iterator over yielded items that serves the generator's connector requests.
pub(crate) struct EngineGeneratorIterator<
    'a,
    Item: Send + 'static,
    O: Send + 'static = (),
    C = EngineConnector,
> {
    connector: C,
    state: GeneratorState<'a, Item, O>,
}

/// Iterator over fallible yielded items that also serves generator requests.
pub(crate) struct EngineResultGeneratorIterator<
    'a,
    Item: Send + 'static,
    O: Send + 'static = (),
    C = EngineConnector,
> {
    inner: EngineGeneratorIterator<'a, DeltaResult<Item>, O, C>,
}

enum GeneratorState<'a, Item: Send + 'static, O: Send + 'static> {
    Active(GeneratorCoroutine<'a, Item, O>),
    Complete(O),
    Failed,
}

struct OutputTrackingIterator<'a, Item: Send + 'static, O: Send + 'static> {
    inner: EngineGeneratorIterator<'a, Item, O>,
    output: Arc<Mutex<Option<O>>>,
}

impl<Item: Send + 'static, O: Send + 'static> Iterator for OutputTrackingIterator<'_, Item, O> {
    type Item = DeltaResult<Item>;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.inner.next();
        if item.is_none() {
            let state = std::mem::replace(&mut self.inner.state, GeneratorState::Failed);
            if let GeneratorState::Complete(output) = state {
                if let Ok(mut terminal) = self.output.lock() {
                    *terminal = Some(output);
                }
            }
        }
        item
    }
}

fn take_generator_output<O>(
    output: Arc<Mutex<Option<O>>>,
    incomplete_message: &'static str,
) -> DeltaResult<O> {
    Arc::into_inner(output)
        .ok_or_else(|| Error::internal_error("generator output is still shared"))?
        .into_inner()
        .map_err(|_| Error::internal_error("generator output lock poisoned"))?
        .ok_or_else(|| Error::internal_error(incomplete_message))
}

/// Drive a coroutine using only storage operations.
#[internal_api]
pub(crate) fn drive_with_storage<O>(
    storage: &dyn StorageHandler,
    cancellation_token: Option<CancellationTokenRef>,
    coroutine: Coroutine<'_, O>,
) -> DeltaResult<O> {
    let connector = StorageConnector::new(storage, cancellation_token.clone());
    Ok(drive_coroutine!(coroutine, |request| {
        check_cancelled(cancellation_token.as_ref())?;
        connector.respond(request)?
    }))
}

/// Drive `coroutine` through `engine`, capturing all handlers only when an operation needs the
/// stateful connector.
///
/// Pass a cancellation token to install it when the engine connector is first captured. `None`
/// matches the two-argument [`run_coroutine_with_engine`] form.
#[internal_api]
pub(crate) fn drive_with_engine<O>(
    engine: &dyn Engine,
    cancellation_token: impl Into<Option<CancellationTokenRef>>,
    coroutine: Coroutine<'_, O>,
) -> DeltaResult<O> {
    let cancellation_token = cancellation_token.into();
    let mut connector = None;
    Ok(drive_coroutine!(coroutine, |request| {
        check_cancelled(cancellation_token.as_ref())?;
        match request {
            Request::CreateEngineData(operation, reply) => {
                reply.send(
                    engine
                        .evaluation_handler()
                        .create_many(operation.schema, operation.rows),
                );
            }
            request @ Request::WriteBytes(..) => {
                let storage = engine.storage_handler();
                StorageConnector::new(storage.as_ref(), cancellation_token.clone())
                    .respond(request)?
            }
            request => connector
                .get_or_insert_with(|| {
                    EngineConnector::new(engine).with_cancellation_token(cancellation_token.clone())
                })
                .respond(request)?,
        }
    }))
}

/// Drive a coroutine through `engine`, delegating commit and publish requests to `committer`.
#[internal_api]
pub(crate) fn drive_with_engine_and_committer<O>(
    engine: &dyn Engine,
    committer: &dyn Committer,
    coroutine: Coroutine<'_, O>,
) -> DeltaResult<O> {
    EngineConnector::new(engine).drive_with_committer(coroutine, engine, committer)
}

#[cfg(feature = "declarative-plans")]
pub(crate) fn drive_with_plan_executor<O>(
    executor: &dyn PlanExecutor,
    coroutine: Coroutine<'_, O>,
) -> DeltaResult<O> {
    let connector = PlanExecutorConnector { executor };
    Ok(drive_coroutine!(coroutine, |request| {
        connector.respond(request)?
    }))
}

impl StorageConnector<'_> {
    fn respond(&self, request: Request) -> DeltaResult<()> {
        match request {
            Request::ListDirectory(request) => respond_paged(self, request),
            Request::ListForward(request) => respond_paged(self, request),
            Request::ListBackward(request) => respond_paged(self, request),
            Request::ReadSmallFile(file, reply) => reply.send(self.read_small_file(file)),
            Request::Head(url, reply) => reply.send(self.storage.head(&url)),
            Request::CopyAtomic(operation, reply) => reply.send(
                self.storage
                    .copy_atomic(&operation.source, &operation.destination),
            ),
            Request::WriteBytes(operation, reply) => reply.send(self.storage.put(
                &operation.url,
                operation.data,
                operation.overwrite,
            )),
            _ => {
                return Err(Error::internal_error(
                    "storage-only coroutine requested a non-storage operation",
                ));
            }
        }
        Ok(())
    }

    fn read_small_file(&self, file: FileSlice) -> DeltaResult<Option<Bytes>> {
        let mut reads = self
            .storage
            .read_files_with_cancellation(vec![file], self.cancellation_token.clone())?;
        let data = reads.next().transpose()?;
        if reads.next().transpose()?.is_some() {
            return Err(Error::internal_error(
                "single-file read returned more than one result",
            ));
        }
        Ok(data)
    }
}

#[cfg(feature = "declarative-plans")]
impl PlanExecutorConnector<'_> {
    fn respond(&self, request: Request) -> DeltaResult<()> {
        match request {
            Request::ExecutePlan(PlanRequest::Require(reply)) => reply.send(Ok(())),
            Request::ExecutePlan(PlanRequest::ParquetFooter(file, reply)) => {
                reply.send(self.executor.read_parquet_footer(file));
            }
            Request::ExecutePlan(PlanRequest::Query(request)) => respond_paged(self, request),
            _ => {
                return Err(Error::internal_error(
                    "plan-executor coroutine requested a non-plan operation",
                ))
            }
        }
        Ok(())
    }
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
            cancellation_token: None,
        }
    }

    /// Configure the cancellation token propagated to engine handlers.
    pub(crate) fn with_cancellation_token(
        mut self,
        cancellation_token: impl Into<Option<CancellationTokenRef>>,
    ) -> Self {
        self.cancellation_token = cancellation_token.into();
        self
    }

    /// Drive `coroutine` through this connector.
    pub(crate) fn run<O: Send + 'static>(&self, coroutine: StaticCoroutine<O>) -> DeltaResult<O> {
        self.drive(coroutine)
    }

    /// Drive an existing coroutine through this connector.
    pub(crate) fn drive<O>(&self, coroutine: Coroutine<'_, O>) -> DeltaResult<O> {
        Ok(drive_coroutine!(coroutine, |request| {
            self.check_cancelled()?;
            self.respond(request)?
        }))
    }

    /// Prepare a fallible generator and return an iterator that drives it and its requests.
    pub(crate) fn iterate_prepared_result_generator<'a, G, Item>(
        self,
        coroutine: Coroutine<'_, G>,
    ) -> DeltaResult<EngineResultGeneratorIterator<'a, Item>>
    where
        G: IntoGenerator<Request, Item = DeltaResult<Item>, Output = ()> + 'a,
        Item: Send + 'static,
    {
        let generator = GeneratorCoroutine::from_generator(self.drive(coroutine)?);
        Ok(self.iterate_result_generator(generator))
    }

    /// Prepare a fallible generator and return its iterator and eager companion value.
    pub(crate) fn iterate_prepared_result_generator_with_companion<'a, R, G, Item, Companion>(
        self,
        coroutine: Coroutine<'_, R>,
        into_parts: impl FnOnce(R) -> (G, Companion),
    ) -> DeltaResult<(EngineResultGeneratorIterator<'a, Item>, Companion)>
    where
        G: IntoGenerator<Request, Item = DeltaResult<Item>, Output = ()> + 'a,
        Item: Send + 'static,
    {
        let (generator, companion) = into_parts(self.drive(coroutine)?);
        let generator = GeneratorCoroutine::from_generator(generator);
        Ok((self.iterate_result_generator(generator), companion))
    }

    /// Drive a coroutine whose catalog requests are handled by `committer`.
    pub(crate) fn drive_with_committer<O>(
        &self,
        coroutine: Coroutine<'_, O>,
        engine: &dyn Engine,
        committer: &dyn Committer,
    ) -> DeltaResult<O> {
        Ok(drive_coroutine!(coroutine, |request| {
            self.check_cancelled()?;
            match request {
                Request::Publish(metadata, reply) => {
                    reply.send(committer.publish(engine, metadata));
                }
                request => self.respond(request)?,
            }
        }))
    }

    /// Return an iterator that drives `generator` and its connector requests.
    pub(crate) fn iterate_generator<'a, Item: Send + 'static, O: Send + 'static>(
        self,
        generator: impl Into<GeneratorCoroutine<'a, Item, O>>,
    ) -> EngineGeneratorIterator<'a, Item, O> {
        EngineGeneratorIterator {
            connector: self,
            state: GeneratorState::Active(generator.into()),
        }
    }

    /// Like [`iterate_generator`], but for generators that yield [`DeltaResult`] items.
    pub(crate) fn iterate_result_generator<'a, Item: Send + 'static, O: Send + 'static>(
        self,
        generator: impl Into<GeneratorCoroutine<'a, DeltaResult<Item>, O>>,
    ) -> EngineResultGeneratorIterator<'a, Item, O> {
        EngineResultGeneratorIterator {
            inner: self.iterate_generator(generator),
        }
    }

    /// Initialize `operation` and return its continuation cursor without fetching a page.
    pub(crate) fn prepare_paged<Op>(&self, operation: Op) -> DeltaResult<Cursor<Op>>
    where
        Op: PagedOperation,
        PageRequest<Op>: Into<Request>,
    {
        self.drive(coroutine!(async move |channel| {
            channel.prepare_paged(operation).await
        }))
    }

    /// Prepare `operation` and flatten its remaining pages into an independently driven generator.
    pub(crate) fn prepare_flattened_generator<Op>(
        &self,
        operation: Op,
    ) -> DeltaResult<StaticGeneratorCoroutine<<Op::Page as IntoIterator>::Item>>
    where
        Op: PagedOperation,
        Op::Page: IntoIterator,
        <Op::Page as IntoIterator>::IntoIter: Send + 'static,
        <Op::Page as IntoIterator>::Item: Send + 'static,
        PageRequest<Op>: Into<Request>,
    {
        let cursor = self.prepare_paged(operation)?;
        Ok(StaticGeneratorCoroutine::from_generator(
            PageGenerator::resume(cursor).flatten(),
        ))
    }

    fn storage_connector(&self) -> StorageConnector<'_> {
        StorageConnector::new(self.storage.as_ref(), self.cancellation_token.clone())
    }

    fn check_cancelled(&self) -> DeltaResult<()> {
        check_cancelled(self.cancellation_token.as_ref())
    }

    fn respond(&self, request: Request) -> DeltaResult<()> {
        match request {
            request @ (Request::ListDirectory(_)
            | Request::ListForward(_)
            | Request::ListBackward(_)
            | Request::ReadSmallFile(..)
            | Request::Head(..)
            | Request::WriteBytes(..)
            | Request::CopyAtomic(..)) => self.storage_connector().respond(request)?,
            Request::ReadParquetFooter(file, reply) => reply.send(
                self.parquet
                    .read_parquet_footer_with_cancellation(&file, self.cancellation_token.clone()),
            ),
            Request::ReadJson(request) => respond_paged(self, request),
            Request::ReadParquet(request) => respond_paged(self, request),
            #[cfg(feature = "declarative-plans")]
            Request::ExecutePlan(PlanRequest::Require(reply)) => {
                reply.send(self.plan_executor.as_ref().map(|_| ()).ok_or_else(|| {
                    Error::unsupported("this engine does not provide a PlanExecutor")
                }))
            }
            #[cfg(feature = "declarative-plans")]
            Request::ExecutePlan(PlanRequest::Query(request)) => respond_paged(self, request),
            #[cfg(feature = "declarative-plans")]
            Request::ExecutePlan(PlanRequest::ParquetFooter(file, reply)) => reply.send(
                self.plan_executor
                    .as_deref()
                    .ok_or_else(|| {
                        Error::unsupported("this engine does not provide a PlanExecutor")
                    })
                    .and_then(|executor| executor.read_parquet_footer(file)),
            ),
            #[cfg(not(feature = "declarative-plans"))]
            Request::ExecutePlan(PlanRequest::Require(reply)) => {
                reply.send(Err(Error::unsupported("declarative plans are disabled")));
            }
            #[cfg(not(feature = "declarative-plans"))]
            Request::ExecutePlan(PlanRequest::Query(request)) => {
                let err = Error::unsupported("declarative plans are disabled");
                match request {
                    PageRequest::Start(_, reply) => reply.send(Err(err)),
                    PageRequest::Prepare(_, reply) => reply.send(Err(err)),
                    PageRequest::Continue(_, reply) => reply.send(Err(err)),
                }
            }
            #[cfg(not(feature = "declarative-plans"))]
            Request::ExecutePlan(PlanRequest::ParquetFooter(_, reply)) => {
                reply.send(Err(Error::unsupported("declarative plans are disabled")))
            }
            Request::CreateEngineData(operation, reply) => reply.send(
                self.evaluation
                    .create_many(operation.schema, operation.rows),
            ),
            Request::CreateExpressionEvaluator(operation, reply) => reply.send(
                self.evaluation
                    .new_expression_evaluator(
                        operation.input_schema,
                        operation.expression,
                        operation.output_type,
                    )
                    .map(|evaluator| Arc::new(evaluator) as _),
            ),
            Request::CreatePredicateEvaluator(operation, reply) => reply.send(
                self.evaluation
                    .new_predicate_evaluator(operation.input_schema, operation.predicate)
                    .map(|evaluator| Arc::new(evaluator) as _),
            ),
            Request::EvaluateExpression(operation, reply) => reply.send(
                expression_evaluator(&operation.evaluator)
                    .and_then(|evaluator| evaluator.evaluate(operation.input.as_ref())),
            ),
            Request::EvaluatePredicate(operation, reply) => reply.send(
                predicate_evaluator(&operation.evaluator)
                    .and_then(|evaluator| evaluator.evaluate(operation.input.as_ref())),
            ),
            Request::WriteJson(operation, reply) => reply.send(self.write_json_file(*operation)),
            Request::WriteParquet(operation, reply) => {
                reply.send(self.write_parquet_file(*operation))
            }
            Request::WriteParquetFiles(operation, reply) => {
                reply.send(self.write_parquet_files(*operation))
            }
            Request::Publish(..) => {
                return Err(Error::internal_error(
                    "publish request requires a catalog committer",
                ));
            }
        }
        Ok(())
    }

    fn write_json_file(&self, operation: WriteJsonFile) -> DeltaResult<FileSize> {
        let WriteJsonFile { url, mode, input } = operation;
        let mut iterator = EngineGeneratorIterator {
            connector: self,
            state: GeneratorState::Active(input),
        };
        let written_size =
            self.json
                .write_json_file(&url, Box::new(&mut iterator), mode.overwrite())?;
        if !matches!(iterator.state, GeneratorState::Complete(())) {
            return Err(Error::internal_error(
                "JSON handler completed before exhausting its input",
            ));
        }
        Ok(written_size)
    }

    fn write_parquet_file(&self, operation: WriteParquetFile) -> DeltaResult<()> {
        let WriteParquetFile { url, input } = operation;
        let output = Arc::new(Mutex::new(None));
        let iterator = OutputTrackingIterator {
            inner: self.clone().iterate_generator(input),
            output: Arc::clone(&output),
        };
        let mut iterator = iterator.peekable();
        if iterator.peek().is_none() {
            return Err(Error::generic(
                "Cannot write parquet file with empty data generator",
            ));
        }
        self.parquet
            .write_parquet_file(url.clone(), Box::new(iterator))?;
        take_generator_output(
            output,
            "Parquet handler completed before exhausting its input",
        )?;
        Ok(())
    }

    fn write_parquet_files(
        &self,
        operation: WriteParquetFiles,
    ) -> DeltaResult<WriteParquetFilesResult> {
        let WriteParquetFiles { write, input } = operation;
        let output = Arc::new(Mutex::new(None));
        let iterator = OutputTrackingIterator {
            inner: self.clone().iterate_generator(input),
            output: Arc::clone(&output),
        };
        let locations = self
            .parquet
            .write_parquet_files(write, Box::new(iterator))?;
        let output = take_generator_output(
            output,
            "Parquet handler completed before exhausting its input",
        )?;
        Ok((locations, output))
    }
}

/// Return an iterator that drives a fallible generator through `engine`.
#[internal_api]
pub(crate) fn iterate_result_generator_with_engine<'a, Item: Send + 'static, O: Send + 'static>(
    engine: &dyn Engine,
    generator: impl Into<GeneratorCoroutine<'a, DeltaResult<Item>, O>>,
) -> impl Iterator<Item = DeltaResult<Item>> + Send + 'a {
    EngineConnector::new(engine).iterate_result_generator(generator)
}

/// Prepare and iterate a fallible generator through one connector captured from `engine`.
#[doc(hidden)]
#[internal_api]
pub(crate) fn iterate_prepared_result_generator_with_engine<'a, G, Item>(
    engine: &dyn Engine,
    cancellation_token: impl Into<Option<CancellationTokenRef>>,
    coroutine: Coroutine<'_, G>,
) -> DeltaResult<impl Iterator<Item = DeltaResult<Item>> + Send + 'a>
where
    G: IntoGenerator<Request, Item = DeltaResult<Item>, Output = ()> + 'a,
    Item: Send + 'static,
{
    let connector = EngineConnector::new(engine).with_cancellation_token(cancellation_token);
    connector.iterate_prepared_result_generator(coroutine)
}

/// Construct, prepare, and iterate a fallible generator through `engine`.
#[internal_api]
macro_rules! iterate_prepared_result_generator_with_engine {
    ($engine:expr, $cancellation_token:expr, $body:expr) => {{
        let coroutine = $crate::coroutine::kernel::coroutine!($body);
        $crate::coroutine::iterate_prepared_result_generator_with_engine(
            $engine,
            $cancellation_token,
            coroutine,
        )
    }};
}

/// Construct and drive a coroutine through an [`Engine`].
///
/// The two-argument form does not propagate cancellation. Pass a token as the second argument
/// to install it when the engine connector is first captured.
#[internal_api]
macro_rules! run_coroutine_with_engine {
    ($engine:expr, $body:expr) => {
        $crate::coroutine::drive_with_engine(
            $engine,
            None,
            $crate::coroutine::kernel::coroutine!($body),
        )
    };
    ($engine:expr, $cancellation_token:expr, $body:expr) => {
        $crate::coroutine::drive_with_engine(
            $engine,
            $cancellation_token,
            $crate::coroutine::kernel::coroutine!($body),
        )
    };
}

/// Construct and drive a coroutine through a storage handler.
#[internal_api]
macro_rules! run_coroutine_with_storage {
    ($storage:expr, $cancellation_token:expr, $body:expr) => {
        $crate::coroutine::drive_with_storage(
            $storage,
            $cancellation_token,
            $crate::coroutine::kernel::coroutine!($body),
        )
    };
}

/// Construct and drive a coroutine through an existing [`EngineConnector`].
macro_rules! run_coroutine_with_connector {
    ($connector:expr, $body:expr) => {
        $connector.drive($crate::coroutine::kernel::coroutine!($body))
    };
}
pub(crate) use run_coroutine_with_connector;

fn expression_evaluator(
    handle: &ExpressionEvaluatorHandle,
) -> DeltaResult<&Arc<dyn ExpressionEvaluator>> {
    handle
        .downcast_ref()
        .ok_or_else(|| Error::internal_error("invalid Engine expression evaluator handle"))
}

fn predicate_evaluator(
    handle: &PredicateEvaluatorHandle,
) -> DeltaResult<&Arc<dyn PredicateEvaluator>> {
    handle
        .downcast_ref()
        .ok_or_else(|| Error::internal_error("invalid Engine predicate evaluator handle"))
}

impl EnginePagination<DirectoryListing> for StorageConnector<'_> {
    type State = ListingIterator;

    fn initialize(&self, listing: DirectoryListing) -> DeltaResult<Self::State> {
        let prefix = listing.prefix().clone();
        let entries = self
            .storage
            .list_from_with_cancellation(&prefix, self.cancellation_token.clone())?
            .take_while(move |entry| listing.contains(entry));
        Ok(Box::new(entries))
    }

    fn next_page(&self, mut listing: Self::State) -> DeltaResult<Page<DirectoryListing>> {
        let data = Vec::from_iter(listing.by_ref().take(FORWARD_LISTING_PAGE_SIZE));
        let next = (data.len() == FORWARD_LISTING_PAGE_SIZE).then(|| Cursor::new(listing));
        Ok(Page { data, next })
    }
}

impl EnginePagination<ForwardListing> for StorageConnector<'_> {
    type State = ListingIterator;

    fn initialize(&self, ForwardListing(bounds): ForwardListing) -> DeltaResult<Self::State> {
        let listing = self
            .storage
            .list_from_with_cancellation(&bounds.low, self.cancellation_token.clone())?
            .take_while(move |entry| bounds.contains(entry));
        Ok(Box::new(listing))
    }

    fn next_page(&self, mut listing: ListingIterator) -> DeltaResult<Page<ForwardListing>> {
        let data = Vec::from_iter(listing.by_ref().take(FORWARD_LISTING_PAGE_SIZE));
        let next = (data.len() == FORWARD_LISTING_PAGE_SIZE).then(|| Cursor::new(listing));
        Ok(Page { data, next })
    }
}

impl EnginePagination<BackwardListing> for StorageConnector<'_> {
    type State = BackwardListingState;

    fn initialize(&self, BackwardListing(bounds): BackwardListing) -> DeltaResult<Self::State> {
        Ok(BackwardListingState {
            high: bounds.high_version()?,
            bounds: Box::new(*bounds),
        })
    }

    fn next_page(&self, state: Self::State) -> DeltaResult<Page<BackwardListing>> {
        let BackwardListingState { bounds, high } = state;
        let window = bounds.backward_window(high, BackwardListing::DEFAULT_WINDOW_SIZE)?;
        let next_high = window.next_high;
        let entries = self
            .storage
            .list_from_with_cancellation(&window.low, self.cancellation_token.clone())?
            .take_while(move |entry| window.contains(entry))
            .collect();
        let next = next_high.map(|high| Cursor::new(BackwardListingState { bounds, high }));
        let data = BackwardListingResult {
            entries,
            known_version_boundary: true,
        };
        Ok(Page { data, next })
    }
}

impl EnginePagination<ReadJsonFiles> for EngineConnector {
    type State = EngineDataIterator;

    fn initialize(&self, read: ReadJsonFiles) -> DeltaResult<Self::State> {
        self.json.read_json_files_with_cancellation(
            &read.files,
            read.physical_schema,
            read.predicate,
            self.cancellation_token.clone(),
        )
    }

    fn next_page(&self, state: Self::State) -> DeltaResult<Page<ReadJsonFiles>> {
        next_engine_data(state)
    }
}

impl EnginePagination<ReadParquetFiles> for EngineConnector {
    type State = EngineDataIterator;

    fn initialize(&self, read: ReadParquetFiles) -> DeltaResult<Self::State> {
        self.parquet.read_parquet_files_with_cancellation(
            &read.files,
            read.physical_schema,
            read.predicate,
            self.cancellation_token.clone(),
        )
    }

    fn next_page(&self, state: Self::State) -> DeltaResult<Page<ReadParquetFiles>> {
        next_engine_data(state)
    }
}

#[cfg(feature = "declarative-plans")]
impl EnginePagination<PlanOperation> for EngineConnector {
    type State = EngineDataIterator;

    fn initialize(&self, operation: PlanOperation) -> DeltaResult<Self::State> {
        self.plan_executor
            .as_deref()
            .ok_or_else(|| Error::unsupported("this engine does not provide a PlanExecutor"))?
            .execute_op(operation)?
            .into_data()
    }

    fn next_page(&self, state: Self::State) -> DeltaResult<Page<PlanOperation>> {
        next_engine_data(state)
    }
}

#[cfg(feature = "declarative-plans")]
impl EnginePagination<PlanOperation> for PlanExecutorConnector<'_> {
    type State = EngineDataIterator;

    fn initialize(&self, operation: PlanOperation) -> DeltaResult<Self::State> {
        self.executor.execute_op(operation)?.into_data()
    }

    fn next_page(&self, state: Self::State) -> DeltaResult<Page<PlanOperation>> {
        next_engine_data(state)
    }
}

impl<Item: Send + 'static, O: Send + 'static, C> EngineGeneratorIterator<'_, Item, O, C> {
    /// Return whether the generator completed and stored its terminal output.
    pub(crate) fn is_complete(&self) -> bool {
        matches!(self.state, GeneratorState::Complete(_))
    }

    /// Return the terminal output after this iterator has been exhausted.
    pub(crate) fn into_output(self) -> DeltaResult<O> {
        match self.state {
            GeneratorState::Complete(output) => Ok(output),
            GeneratorState::Active(_) => Err(Error::internal_error(
                "generator iterator must be exhausted before taking its output",
            )),
            GeneratorState::Failed => Err(Error::internal_error(
                "generator iterator failed before producing its output",
            )),
        }
    }
}

impl<Item: Send + 'static, O: Send + 'static, C> EngineResultGeneratorIterator<'_, Item, O, C> {
    /// Return whether the generator completed and stored its terminal output.
    pub(crate) fn is_complete(&self) -> bool {
        self.inner.is_complete()
    }

    /// Return the terminal output after this iterator has been exhausted.
    pub(crate) fn into_output(self) -> DeltaResult<O> {
        self.inner.into_output()
    }
}

impl<C: Borrow<EngineConnector>, Item: Send + 'static, O: Send + 'static> Iterator
    for EngineGeneratorIterator<'_, Item, O, C>
{
    type Item = DeltaResult<Item>;

    fn next(&mut self) -> Option<Self::Item> {
        next_generator(self.connector.borrow(), &mut self.state)
    }
}

impl<C: Borrow<EngineConnector>, Item: Send + 'static, O: Send + 'static> Iterator
    for EngineResultGeneratorIterator<'_, Item, O, C>
{
    type Item = DeltaResult<Item>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|result| result?)
    }
}

fn next_generator<Item: Send + 'static, O: Send + 'static>(
    connector: &EngineConnector,
    state: &mut GeneratorState<'_, Item, O>,
) -> Option<DeltaResult<Item>> {
    let GeneratorState::Active(generator) = state else {
        return None;
    };
    if let Err(err) = connector.check_cancelled() {
        *state = GeneratorState::Failed;
        return Some(Err(err));
    }
    loop {
        match generator.try_advance() {
            Ok(GeneratorCoroutineStep::Done(output)) => {
                *state = GeneratorState::Complete(output);
                return None;
            }
            Ok(GeneratorCoroutineStep::Yield(item)) => return Some(Ok(item)),
            Ok(GeneratorCoroutineStep::Request(request)) => {
                if let Err(err) = connector.respond(request) {
                    *state = GeneratorState::Failed;
                    return Some(Err(err));
                }
            }
            Err(err) => {
                *state = GeneratorState::Failed;
                return Some(Err(err));
            }
        }
    }
}

fn next_engine_data<Op>(mut reads: EngineDataIterator) -> DeltaResult<Page<Op>>
where
    Op: PagedOperation<Page = Vec<DeltaResult<Box<dyn EngineData>>>>,
{
    let (data, next) = match reads.next() {
        Some(data) => (vec![data], Some(Cursor::new(reads))),
        None => (Vec::new(), None),
    };
    Ok(Page { data, next })
}

#[cfg(test)]
mod tests {
    use url::Url;

    use super::*;
    use crate::unit_test_utils::TestCancellationToken;

    struct CancelAfterFirstRead {
        token: Arc<TestCancellationToken>,
    }

    impl StorageHandler for CancelAfterFirstRead {
        fn list_from(&self, _path: &Url) -> DeltaResult<DeltaResultIteratorStatic<FileMeta>> {
            Err(Error::generic("unused listing"))
        }

        fn read_files(
            &self,
            _files: Vec<FileSlice>,
        ) -> DeltaResult<DeltaResultIteratorStatic<Bytes>> {
            let token = Arc::clone(&self.token);
            Ok(Box::new(std::iter::once_with(move || {
                token.cancel();
                Ok(Bytes::from_static(b"data"))
            })))
        }

        fn copy_atomic(&self, _src: &Url, _dest: &Url) -> DeltaResult<()> {
            Err(Error::generic("unused copy"))
        }

        fn put(&self, _path: &Url, _data: Bytes, _overwrite: bool) -> DeltaResult<()> {
            Err(Error::generic("unused write"))
        }

        fn head(&self, _path: &Url) -> DeltaResult<FileMeta> {
            Err(Error::generic("unused head"))
        }

        fn delete(&self, _path: &Url) -> DeltaResult<()> {
            Err(Error::generic("unused delete"))
        }
    }

    #[test]
    fn small_file_read_propagates_cancellation_from_exhaustion_probe() {
        let token = Arc::new(TestCancellationToken::default());
        let cancellation_token: CancellationTokenRef = token.clone();
        let storage = CancelAfterFirstRead { token };
        let file = (Url::parse("memory:///data").unwrap(), None);
        let connector = StorageConnector::new(&storage, Some(cancellation_token));

        let result = connector.read_small_file(file);

        assert!(matches!(result, Err(Error::Cancelled)));
    }

    #[cfg(feature = "default-engine-base")]
    mod json_write {
        use super::*;
        use crate::coroutine::kernel::request::FileWriteMode;
        use crate::engine::sync::SyncEngine;
        use crate::engine::test_delegating::DelegatingEngine;
        use crate::schema::SchemaRef;
        use crate::{DeltaResultIterator, FileSize, PredicateRef};

        struct EarlyReturnJsonHandler;

        impl JsonHandler for EarlyReturnJsonHandler {
            fn parse_json(
                &self,
                _json_strings: Box<dyn EngineData>,
                _output_schema: SchemaRef,
            ) -> DeltaResult<Box<dyn EngineData>> {
                Err(Error::generic("unused JSON parsing"))
            }

            fn read_json_files(
                &self,
                _files: &[FileMeta],
                _physical_schema: SchemaRef,
                _predicate: Option<PredicateRef>,
            ) -> DeltaResult<FileDataReadResultIterator> {
                Err(Error::generic("unused JSON read"))
            }

            fn write_json_file(
                &self,
                _path: &Url,
                _data: DeltaResultIterator<'_, FilteredEngineData>,
                _overwrite: bool,
            ) -> DeltaResult<FileSize> {
                Ok(0)
            }
        }

        #[test]
        fn json_handler_cannot_succeed_without_exhausting_generator() {
            let inner: Arc<dyn Engine> = Arc::new(SyncEngine::new());
            let engine =
                DelegatingEngine::new(inner).with_json_handler(Arc::new(EarlyReturnJsonHandler));
            let actions: DeltaResultIteratorStatic<FilteredEngineData> =
                Box::new(std::iter::empty());
            let input = StaticGeneratorCoroutine::from_iterator(actions);
            let operation = WriteJsonFile::new(
                Url::parse("memory:///_delta_log/0.json").unwrap(),
                FileWriteMode::CreateNew,
                input,
            );

            let result = EngineConnector::new(&engine).write_json_file(operation);

            assert!(result
                .unwrap_err()
                .to_string()
                .contains("before exhausting its input"));
        }
    }
}
