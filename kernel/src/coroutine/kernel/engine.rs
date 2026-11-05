//! Drives Kernel coroutines by serving requests through an [`Engine`].
//!
//! This adapter backs Engine-compatible entry points and coroutine iterators. It handles requests
//! synchronously through Engine handlers; connector-native async execution uses its own driver.

use std::any::Any;
use std::sync::Arc;

use delta_kernel_derive::internal_api;
use derive_more::Constructor;

use super::request::ReadJsonFiles;
use super::{Coroutine, CoroutineStep, KernelResultGenerator, Request, Workflow};
use crate::coroutine::{Cursor, Page, PageRequest, PagedOperation, Reply, drive_workflow};
use crate::{
    Engine, EngineData, FileDataReadResultIterator, JsonHandler, KernelError, KernelResult,
    KernelResultIterator, StorageHandler,
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
}

impl EngineConnector {
    /// Capture the handlers exposed by `engine`.
    pub(crate) fn new(engine: &dyn Engine) -> Self {
        Self {
            storage: engine.storage_handler(),
            json: engine.json_handler(),
        }
    }

    /// Drive an existing workflow through this connector.
    pub(crate) fn drive<O>(&self, workflow: Workflow<'_, O>) -> KernelResult<O> {
        drive_connector(self, workflow)
    }

    /// Prepare a fallible result coroutine and return an iterator that drives it and its requests.
    pub(crate) fn iterate_prepared_result_coroutine<'a, G, Item>(
        self,
        workflow: Workflow<'_, G>,
    ) -> KernelResult<EngineResultCoroutineIterator<'a, Item>>
    where
        G: KernelResultGenerator<Item> + 'a,
        Item: Send + 'static,
    {
        let coroutine = Coroutine::from_generator(self.drive(workflow)?);
        Ok(self.iterate_result_coroutine(coroutine))
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
}

impl ConnectorRespond for EngineConnector {
    fn respond(&self, request: Request) -> KernelResult<()> {
        match request {
            Request::ReadSmallFile(..) => self.storage_connector().respond(request)?,
            Request::ReadJson(request) => respond_paged(self, request)?,
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

/// Drive `workflow` through `engine`.
#[internal_api]
pub(crate) fn drive_with_engine<O>(
    engine: &dyn Engine,
    workflow: Workflow<'_, O>,
) -> KernelResult<O> {
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
#[cfg_attr(not(feature = "internal-api"), expect(dead_code))]
pub(crate) fn iterate_prepared_result_coroutine_with_engine<'a, G, Item>(
    engine: &dyn Engine,
    workflow: Workflow<'_, G>,
) -> KernelResult<impl Iterator<Item = KernelResult<Item>> + Send + 'a>
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

enum CoroutineState<'a, Item: Send + 'static, O: Send + 'static> {
    Active(Coroutine<'a, Item, O>),
    Complete(O),
    Failed,
}

/// Serves entry points that intentionally require only storage access.
#[derive(Constructor)]
struct StorageConnector<'a> {
    storage: &'a dyn StorageHandler,
}

impl ConnectorRespond for StorageConnector<'_> {
    fn respond(&self, request: Request) -> KernelResult<()> {
        match request {
            Request::ReadSmallFile(file, reply) => {
                complete(reply, self.storage.read_small_file(file))
            }
            Request::ReadJson(_) => Err(KernelError::internal_error(
                "JSON reads require an Engine connector",
            )),
        }
    }
}

/// Serves one coroutine request, replying through the request's reply handle.
trait ConnectorRespond {
    fn respond(&self, request: Request) -> KernelResult<()>;
}

fn drive_connector<O>(
    connector: &impl ConnectorRespond,
    workflow: Workflow<'_, O>,
) -> KernelResult<O> {
    let mut cancellation = CancellationLatch::default();
    let output = drive_workflow!(workflow, |request| {
        cancellation.record(connector.respond(request))?
    });
    cancellation.finish(output)
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
    use crate::engine::CancellableEngine;
    use crate::engine::sync::SyncEngine;
    use crate::engine::test_delegating::DelegatingEngine;
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
