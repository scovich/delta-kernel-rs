//! Kernel-side coroutine support.
//!
//! On the kernel side, [`Workflow`] and [`Generator`] coroutines are just async functions. They
//! communicate with the connector via a [`Channel`] whose async methods turn requests into futures
//! that kernel code `await`s to receive the connector's response. Normal rust async machinery takes
//! over from there (polling, compiler-generated stack ripping and state machines, etc).
//!
//! [`Workflow::start`], forms the boundary between kernel's public (sync) entry points and the
//! future that encapsulates a workflow's logic. Starting or resuming a coroutine
//! [polls](Future::poll) the workflow's future, allowing it to run. If that poll returns
//! [`Poll::Ready`], the coroutine completed and leads to [`Workflow::Done`]; otherwise, the
//! workflow `await`ed a [`Channel`] method future and the coroutine machinery extracts the
//! corresponding request as [`Workflow::Request`]. When the connector invokes the [`Resume`], the
//! coroutine machinery makes the response available to the workflow and then resumes it by polling
//! again. That process repeats until the workflow completes or the connector abandons it.
//!
//! It is important to note that there is no async runtime; everything happens on the calling
//! thread, polling with a no-op [`Waker`](std::task::Waker::noop). Workflows only advance if a the
//! connector directly polls their future by calling [`Workflow::start`] or [`Resume`].
//!
//! Generators are special in that they communicate over two channels: Delegated requests go over a
//! normal [`Channel`] for the connector to handle, while yielded output items go through a
//! [`Yielder`] for the generator's owner to consume. This allows kernel to create and consume
//! generators internally while still forwarding all requests to the connector. The
//! [`GeneratorState`] wrapper provides an iterator-like surface for kernel-side generator
//! consumption, used for operations such as incremental read CRC and log replay that must
//! manipulate and consume multiple streams of connector-provided data.
use std::future::Future;
use std::ops::Deref;
use std::pin::Pin;
use std::sync::{Arc, Weak};
use std::task::{Context, Poll, Waker};

use bytes::Bytes;
use delta_kernel_derive::internal_api;
use tracing::{error, Instrument as _, Span};

use super::core::{Exchange, Outbox, OutboxEntry, Wait};
#[cfg(feature = "declarative-plans")]
use super::PlanOperation;
use super::{
    BackwardListing, Cursor, ForwardListing, Generator, Page, PageRequest, PagedOperation,
    ReadJsonFiles, ReadParquetFiles, Request, Resume, Workflow, WriteBytes, YieldResume,
};
use crate::{DeltaResult, Error, FileMeta, FileSlice, ParquetFooter};

/// A sendable future that resolves to a kernel result.
#[internal_api]
pub(crate) trait DeltaFuture<O>: Future<Output = DeltaResult<O>> + Send {}

impl<O, F> DeltaFuture<O> for F where F: Future<Output = DeltaResult<O>> + Send {}

/// Kernel-side handle for typed connector operations.
///
/// It shares a request outbox with the coroutine driver and admits one live request at a time.
#[internal_api]
pub(crate) struct Channel(Arc<Outbox<PendingRequest>>);

impl Channel {
    /// Initiate a request/response exchange with the connector
    pub(super) async fn exchange<Out: Send + 'static, In: Send + 'static>(
        &self,
        outbound: Out,
        pending: impl FnOnce(Weak<Exchange<Out, In>>) -> PendingRequest + Send,
    ) -> DeltaResult<In> {
        let exchange = Arc::new(Exchange::new(outbound));
        self.0.put(pending(Arc::downgrade(&exchange)))?;
        Wait(exchange).await
    }
}

/// Kernel-side handle passed to a generator body.
///
/// Forward all requests to the underlying [`Channel`] so connector can handle them, while
/// delivering the input of [`Self::yield_item`] calls to the generator's immediate consumer.
#[internal_api]
pub(crate) struct Yielder<Y> {
    channel: Channel,
    yields: Arc<Outbox<Weak<YieldExchange<Y>>>>,
}

impl<Y> Deref for Yielder<Y> {
    type Target = Channel;

    fn deref(&self) -> &Self::Target {
        &self.channel
    }
}

impl<Y: Send + 'static> Yielder<Y> {
    /// Yield one item and suspend until the consumer resumes the generator.
    ///
    /// An error supplied by the consumer is returned at this await point.
    #[internal_api]
    pub(crate) async fn yield_item(&self, item: Y) -> DeltaResult<()> {
        let emission = Arc::new(Exchange::new(item));
        self.yields.put(Arc::downgrade(&emission))?;
        Wait(emission).await
    }
}

/// Kernel-side state for consuming a child generator.
#[internal_api]
pub(crate) enum GeneratorState<W> {
    Start(W),
    Continue(YieldResume<W>),
    Exhausted,
}

impl<Y: Send + 'static> GeneratorState<Generator<(), Y>> {
    /// Return the next yielded item, forwarding connector requests through `parent`.
    ///
    /// Returns `None` after the child generator completes.
    #[internal_api]
    pub(crate) async fn next(&mut self, parent: &Channel) -> DeltaResult<Option<Y>> {
        let state = std::mem::replace(self, Self::Exhausted);
        let mut generator = match state {
            Self::Start(generator) => Ok(generator),
            Self::Continue(resume) => resume(Ok(())),
            Self::Exhausted => return Ok(None),
        };

        loop {
            generator = match generator? {
                Generator::Request(request) => request.forward_to(parent).await,
                Generator::Done(()) => return Ok(None),
                Generator::Yield(item, resume) => {
                    *self = Self::Continue(resume);
                    return Ok(Some(item));
                }
            };
        }
    }
}

impl<O: Send + 'static> Workflow<O> {
    /// Start a workflow and run it until completion or its first connector request.
    #[internal_api]
    pub(crate) fn start<Fut>(workflow: impl FnOnce(Channel) -> Fut) -> DeltaResult<Self>
    where
        Fut: DeltaFuture<O> + 'static,
    {
        let outbox = Arc::new(Outbox::default());
        let future = workflow(Channel(Arc::clone(&outbox)));
        let future = async move {
            future
                .await
                .inspect_err(|err| error!(error = %err, "kernel workflow failed"))
        };
        let task = Box::pin(future.instrument(Span::current()));
        Self::advance(task, outbox)
    }

    /// Connector-side helper that starts or resumes a kernel workflow.
    fn advance(mut task: Task<O>, outbox: Arc<Outbox<PendingRequest>>) -> DeltaResult<Workflow<O>> {
        let mut context = Context::from_waker(Waker::noop());
        match task.as_mut().poll(&mut context) {
            Poll::Ready(output) => output.map(Workflow::Done),
            Poll::Pending => {
                let pending = outbox.take_request()?;
                let request = pending.into_request(task, outbox, Self::advance)?;
                Ok(Workflow::Request(request))
            }
        }
    }
}

impl<O: Send + 'static, Y: Send + 'static> Generator<O, Y> {
    /// Start a generator and run it until completion, its first yield, or a connector request.
    #[internal_api]
    pub(crate) fn start<Fut>(generator: impl FnOnce(Yielder<Y>) -> Fut) -> DeltaResult<Self>
    where
        Fut: DeltaFuture<O> + 'static,
    {
        let requests = Arc::new(Outbox::default());
        let yields = Arc::new(Outbox::default());
        let yielder = Yielder {
            channel: Channel(Arc::clone(&requests)),
            yields: Arc::clone(&yields),
        };
        let future = generator(yielder);
        let future = async move {
            future
                .await
                .inspect_err(|err| error!(error = %err, "kernel generator failed"))
        };
        let task = Box::pin(future.instrument(Span::current()));
        Self::advance(task, requests, yields)
    }

    /// Connector-side helper that starts or resumes a kernel generator.
    fn advance(
        mut task: Task<O>,
        requests: Arc<Outbox<PendingRequest>>,
        yields: Arc<Outbox<Weak<YieldExchange<Y>>>>,
    ) -> DeltaResult<Generator<O, Y>> {
        let mut context = Context::from_waker(Waker::noop());
        if let Poll::Ready(output) = task.as_mut().poll(&mut context) {
            return output.map(Generator::Done);
        }

        if let Some(pending) = yields.take_yield()? {
            let item = pending.claim()?;
            let resume = Box::new(move |response| {
                pending.respond(response)?;
                Self::advance(task, requests, yields)
            });
            return Ok(Generator::Yield(item, resume));
        }

        let pending = requests.take_request()?;
        let next_yields = Arc::clone(&yields);
        let request = pending.into_request(task, requests, move |task, requests| {
            Self::advance(task, requests, next_yields)
        })?;
        Ok(Generator::Request(request))
    }
}

/// Type-erased coroutine task.
type Task<O> = Pin<Box<dyn DeltaFuture<O> + 'static>>;

/// Exchange used to suspend a generator at a yielded item.
type YieldExchange<Y> = Exchange<Y, ()>;

/// Type-erased requests stored while a coroutine is suspended.
pub(super) enum PendingRequest {
    ListForward(PendingPageRequest<ForwardListing>),
    ListBackward(PendingPageRequest<BackwardListing>),
    ReadSmallFile(Weak<Exchange<FileSlice, Bytes>>),
    ReadParquetFooter(Weak<Exchange<FileMeta, ParquetFooter>>),
    ReadJson(PendingPageRequest<ReadJsonFiles>),
    ReadParquet(PendingPageRequest<ReadParquetFiles>),
    #[cfg(feature = "declarative-plans")]
    ExecutePlan(PendingPageRequest<PlanOperation>),
    WriteBytes(Weak<Exchange<WriteBytes, ()>>),
}

/// A suspended phase of a paginated operation.
pub(super) enum PendingPageRequest<Op: PagedOperation> {
    Start(Weak<Exchange<Op, Page<Op>>>),
    Prepare(Weak<Exchange<Op, Cursor<Op>>>),
    Continue(Weak<Exchange<Cursor<Op>, Page<Op>>>),
}

impl Default for PendingRequest {
    fn default() -> Self {
        // Any variant with an empty weak reference marks an empty outbox.
        Self::ReadSmallFile(Weak::new())
    }
}

impl OutboxEntry for PendingRequest {
    fn is_live(&self) -> bool {
        match self {
            Self::ListForward(pending) => pending.is_live(),
            Self::ListBackward(pending) => pending.is_live(),
            Self::ReadSmallFile(exchange) => exchange.strong_count() > 0,
            Self::ReadParquetFooter(exchange) => exchange.strong_count() > 0,
            Self::ReadJson(pending) => pending.is_live(),
            Self::ReadParquet(pending) => pending.is_live(),
            #[cfg(feature = "declarative-plans")]
            Self::ExecutePlan(pending) => pending.is_live(),
            Self::WriteBytes(exchange) => exchange.strong_count() > 0,
        }
    }
}

impl PendingRequest {
    /// Converts a kernel-provided pending request into a connector-facing request.
    fn into_request<N: Send + 'static, O: Send + 'static>(
        self,
        task: Task<O>,
        requests: Arc<Outbox<PendingRequest>>,
        advance: impl FnOnce(Task<O>, Arc<Outbox<PendingRequest>>) -> DeltaResult<N> + Send + 'static,
    ) -> DeltaResult<Request<N>> {
        match self {
            Self::ListForward(pending) => Ok(Request::ListForward(
                pending.into_request(task, requests, advance)?,
            )),
            Self::ListBackward(pending) => Ok(Request::ListBackward(
                pending.into_request(task, requests, advance)?,
            )),
            Self::ReadSmallFile(exchange) => {
                request_from_exchange(exchange, task, requests, advance, Request::ReadSmallFile)
            }
            Self::ReadParquetFooter(exchange) => request_from_exchange(
                exchange,
                task,
                requests,
                advance,
                Request::ReadParquetFooter,
            ),
            Self::ReadJson(pending) => Ok(Request::ReadJson(
                pending.into_request(task, requests, advance)?,
            )),
            Self::ReadParquet(pending) => Ok(Request::ReadParquet(
                pending.into_request(task, requests, advance)?,
            )),
            #[cfg(feature = "declarative-plans")]
            Self::ExecutePlan(pending) => Ok(Request::ExecutePlan(
                pending.into_request(task, requests, advance)?,
            )),
            Self::WriteBytes(exchange) => {
                request_from_exchange(exchange, task, requests, advance, Request::WriteBytes)
            }
        }
    }
}

impl<Op: PagedOperation> PendingPageRequest<Op> {
    /// False if the underlying weak reference is empty.
    fn is_live(&self) -> bool {
        match self {
            Self::Start(exchange) => exchange.strong_count() > 0,
            Self::Prepare(exchange) => exchange.strong_count() > 0,
            Self::Continue(exchange) => exchange.strong_count() > 0,
        }
    }

    fn into_request<N: Send + 'static, O: Send + 'static>(
        self,
        task: Task<O>,
        requests: Arc<Outbox<PendingRequest>>,
        advance: impl FnOnce(Task<O>, Arc<Outbox<PendingRequest>>) -> DeltaResult<N> + Send + 'static,
    ) -> DeltaResult<PageRequest<N, Op>> {
        match self {
            Self::Start(exchange) => {
                request_from_exchange(exchange, task, requests, advance, PageRequest::Start)
            }
            Self::Prepare(exchange) => {
                request_from_exchange(exchange, task, requests, advance, PageRequest::Prepare)
            }
            Self::Continue(exchange) => {
                request_from_exchange(exchange, task, requests, advance, PageRequest::Continue)
            }
        }
    }
}

/// Extracts a pending kernel-side request from the exchange and converts it to a [`Resume`] the
/// connector can consume.
fn request_from_exchange<N, O, Out, In, T>(
    exchange: Weak<Exchange<Out, In>>,
    task: Task<O>,
    requests: Arc<Outbox<PendingRequest>>,
    advance: impl FnOnce(Task<O>, Arc<Outbox<PendingRequest>>) -> DeltaResult<N> + Send + 'static,
    make_request: impl FnOnce(Out, Resume<N, In>) -> T,
) -> DeltaResult<T>
where
    N: Send + 'static,
    O: Send + 'static,
    Out: Send + 'static,
    In: Send + 'static,
{
    let Some(exchange) = exchange.upgrade() else {
        return Err(Error::internal_error(
            "coroutine request expired before it was claimed",
        ));
    };
    let outbound = exchange.claim()?;
    let resume = Box::new(move |response| {
        exchange.respond(response)?;
        advance(task, requests)
    });
    Ok(make_request(outbound, resume))
}

impl Outbox<PendingRequest> {
    /// Connector-side: Retrieve a pending kernel request after the coroutine suspends.
    pub(super) fn take_request(&self) -> DeltaResult<PendingRequest> {
        self.take()?.ok_or_else(|| {
            Error::internal_error("coroutine returned Pending without a live connector request")
        })
    }
}

/// A weak yield exchange leaves the outbox empty if its waiter is abandoned.
impl<Y> OutboxEntry for Weak<YieldExchange<Y>> {
    fn is_live(&self) -> bool {
        self.strong_count() > 0
    }
}

impl<Y> Outbox<Weak<YieldExchange<Y>>> {
    /// Connector-side: Take a pending yield item (if any) after the coroutine suspends.
    fn take_yield(&self) -> DeltaResult<Option<Arc<YieldExchange<Y>>>> {
        Ok(self.take()?.and_then(|pending| pending.upgrade()))
    }
}
