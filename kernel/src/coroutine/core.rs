use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, Mutex, MutexGuard, Weak};
use std::task::{Context, Poll, Waker};

use bytes::Bytes;

#[cfg(feature = "declarative-plans")]
use super::ExecutePlan;
use super::{
    BackwardListing, Cursor, ForwardListing, Generator, Page, PageRequest, PagedOperation,
    ReadJsonFiles, ReadParquetFiles, Request, TypedResume, Workflow, WriteBytes,
};
use crate::{DeltaResult, Error, FileMeta, FileSlice, ParquetFooter};

pub(crate) trait DeltaFuture<O>: Future<Output = DeltaResult<O>> + Send {}

impl<O, F> DeltaFuture<O> for F where F: Future<Output = DeltaResult<O>> + Send {}

pub(super) type Task<O> = Pin<Box<dyn DeltaFuture<O> + 'static>>;
pub(super) type WeakExchange<Out, In> = Weak<Exchange<Out, In>>;
pub(super) type YieldExchange<Y> = Exchange<Y, ()>;

/// Shared mailbox implementation used by Workflows and generators (for the connector's `Channel`
/// and the generator's `Yielder`). The `is_live` method returns true if the slot contains a value.
pub(super) trait MailboxEntry: Default {
    fn is_live(&self) -> bool;
}

/// Thread-safe single-slot storage for an entry that can be live or empty.
///
/// Starting a coroutine gives one handle to the coroutine `Task` and one to the driver. Both are
/// encapsulated behind kernel APIs and owned by the `TypedResume` that kernel surfaces to
/// connectors, so the handles are always accessed sequentially (kernel side) and move between
/// threads together or not at all (connector side).
///
/// Unfortunately, the compiler cannot infer these invariants, so we must wrap the mailbox with
/// `Arc` (not `Rc`) and its entry with `Mutex` (not `RefCell`) for the handles to be `Send`.
#[derive(Default)]
pub(super) struct Mailbox<T: MailboxEntry>(Mutex<T>);

impl<T: MailboxEntry> Mailbox<T> {
    pub(super) fn publish(&self, pending: T) -> DeltaResult<()> {
        // WARNING: Never acquire another lock or invoke external code while holding this guard.
        let Ok(mut mailbox) = self.0.lock() else {
            return Err(Error::internal_error(
                "coroutine mailbox mutex was poisoned",
            ));
        };
        if mailbox.is_live() {
            return Err(Error::internal_error(
                "coroutine mailbox already contains a live entry",
            ));
        }
        *mailbox = pending;
        Ok(())
    }

    fn take_pending_opt(&self) -> DeltaResult<Option<T>> {
        // WARNING: Never acquire another lock or invoke external code while holding this guard.
        let Ok(mut mailbox) = self.0.lock() else {
            return Err(Error::internal_error(
                "coroutine mailbox mutex was poisoned",
            ));
        };
        let pending = std::mem::take(&mut *mailbox);
        Ok(pending.is_live().then_some(pending))
    }
}

/// Request slot shared by a coroutine task and its driver.
pub(super) type RequestMailbox = Mailbox<PendingRequest>;

impl Mailbox<PendingRequest> {
    /// Connector-side: Retrieve a pending kernel request after the coroutine suspends.
    pub(super) fn take_pending(&self) -> DeltaResult<PendingRequest> {
        self.take_pending_opt()?.ok_or_else(|| {
            Error::internal_error("coroutine returned Pending without a live connector request")
        })
    }
}

pub(super) enum PendingRequest {
    ListForward(PendingPageRequest<ForwardListing>),
    ListBackward(PendingPageRequest<BackwardListing>),
    ReadSmallFile(WeakExchange<FileSlice, Bytes>),
    ReadParquetFooter(WeakExchange<FileMeta, ParquetFooter>),
    ReadJson(PendingPageRequest<ReadJsonFiles>),
    ReadParquet(PendingPageRequest<ReadParquetFiles>),
    #[cfg(feature = "declarative-plans")]
    ExecutePlan(PendingPageRequest<ExecutePlan>),
    WriteBytes(WeakExchange<WriteBytes, ()>),
}

pub(super) enum PendingPageRequest<Op: PagedOperation> {
    Start(WeakExchange<Op, Page<Op>>),
    Prepare(WeakExchange<Op, Cursor<Op>>),
    Continue(WeakExchange<Cursor<Op>, Page<Op>>),
}

impl Default for PendingRequest {
    fn default() -> Self {
        // Any variant with an empty weak reference marks an empty mailbox.
        Self::ReadSmallFile(Weak::new())
    }
}

impl MailboxEntry for PendingRequest {
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
        mailbox: Arc<RequestMailbox>,
        advance: impl FnOnce(Task<O>, Arc<RequestMailbox>) -> DeltaResult<N> + Send + 'static,
    ) -> DeltaResult<Request<N>> {
        match self {
            Self::ListForward(pending) => Ok(Request::ListForward(
                pending.into_request(task, mailbox, advance)?,
            )),
            Self::ListBackward(pending) => Ok(Request::ListBackward(
                pending.into_request(task, mailbox, advance)?,
            )),
            Self::ReadSmallFile(exchange) => {
                request_from_exchange(exchange, task, mailbox, advance, Request::ReadSmallFile)
            }
            Self::ReadParquetFooter(exchange) => {
                request_from_exchange(exchange, task, mailbox, advance, Request::ReadParquetFooter)
            }
            Self::ReadJson(pending) => Ok(Request::ReadJson(
                pending.into_request(task, mailbox, advance)?,
            )),
            Self::ReadParquet(pending) => Ok(Request::ReadParquet(
                pending.into_request(task, mailbox, advance)?,
            )),
            #[cfg(feature = "declarative-plans")]
            Self::ExecutePlan(pending) => Ok(Request::ExecutePlan(
                pending.into_request(task, mailbox, advance)?,
            )),
            Self::WriteBytes(exchange) => {
                request_from_exchange(exchange, task, mailbox, advance, Request::WriteBytes)
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
        mailbox: Arc<RequestMailbox>,
        advance: impl FnOnce(Task<O>, Arc<RequestMailbox>) -> DeltaResult<N> + Send + 'static,
    ) -> DeltaResult<PageRequest<N, Op>> {
        match self {
            Self::Start(exchange) => {
                request_from_exchange(exchange, task, mailbox, advance, PageRequest::Start)
            }
            Self::Prepare(exchange) => {
                request_from_exchange(exchange, task, mailbox, advance, PageRequest::Prepare)
            }
            Self::Continue(exchange) => {
                request_from_exchange(exchange, task, mailbox, advance, PageRequest::Continue)
            }
        }
    }
}

/// Extracts a pending kernel-side request from the exchange and converts it to a `TypedResume` the
/// connector can consume.
fn request_from_exchange<N, O, Out, In, T>(
    exchange: WeakExchange<Out, In>,
    task: Task<O>,
    mailbox: Arc<RequestMailbox>,
    advance: impl FnOnce(Task<O>, Arc<RequestMailbox>) -> DeltaResult<N> + Send + 'static,
    make_request: impl FnOnce(Out, TypedResume<N, In>) -> T,
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
    let resume = TypedResume(Box::new(move |response| {
        exchange.respond(response)?;
        advance(task, mailbox)
    }));
    Ok(make_request(outbound, resume))
}

/// Single-use operation or yield handoff shared by its waiter and resume handle.
///
/// Its references move through these phases:
///
/// - After publication, [`Wait`] inside the task owns the only strong reference; the corresponding
///   mailbox stores a weak reference.
/// - Claiming the handoff upgrades the weak reference. The resulting [`TypedResume`] owns that
///   strong reference directly and owns the other indirectly through the captured task's `Wait`.
/// - Resuming writes the response through the direct reference, then polls the task, whose `Wait`
///   reads the response through the other reference.
/// - Dropping the resume drops both strong references.
///
/// These accesses are sequential on the thread invoking `resume`. The compiler cannot infer this
/// ownership invariant, so we must wrap the exchange in a `Mutex` so the `Arc` can be `Send`.
pub(super) struct Exchange<Out, In>(Mutex<ExchangeState<Out, In>>);

/// The lifecycle steps of an exchange
enum ExchangeState<Out, In> {
    /// Kernel offers a request to connector just before suspending the workflow (very short-lived)
    Outbound(Out),
    /// Connector received the request but has not responded back yet
    InFlight,
    /// Connector offers a response back to kernel just before resuming the workflow
    Inbound(DeltaResult<In>),
    /// Kernel has claimed the response and `Wait::poll` is about to return it (very short-lived)
    Complete,
}

impl<Out, In> Exchange<Out, In> {
    pub(super) fn new(outbound: Out) -> Self {
        Self(Mutex::new(ExchangeState::Outbound(outbound)))
    }

    /// Connector-side: Claim the request kernel offered when suspending a workflow
    pub(super) fn claim(&self) -> DeltaResult<Out> {
        // WARNING: Never acquire another lock or invoke external code while holding this guard.
        let mut state = self.lock()?;
        match std::mem::replace(&mut *state, ExchangeState::InFlight) {
            ExchangeState::Outbound(outbound) => Ok(outbound),
            previous => {
                *state = previous;
                Err(Error::internal_error(
                    "coroutine exchange outbound value was not available",
                ))
            }
        }
    }

    /// Connector-side: Offer a response to kernel just before resuming the workflow
    pub(super) fn respond(&self, response: DeltaResult<In>) -> DeltaResult<()> {
        // WARNING: Never acquire another lock or invoke external code while holding this guard.
        let mut state = self.lock()?;
        if !matches!(*state, ExchangeState::InFlight) {
            return Err(Error::internal_error(
                "coroutine exchange was not awaiting a response",
            ));
        }
        *state = ExchangeState::Inbound(response);
        Ok(())
    }

    // WARNING: Never acquire another lock or invoke external code while holding this guard.
    fn lock(&self) -> DeltaResult<MutexGuard<'_, ExchangeState<Out, In>>> {
        self.0
            .lock()
            .map_err(|_| Error::internal_error("coroutine exchange mutex was poisoned"))
    }
}

/// The `Future` suspension boundary between kernel coroutines and the connector driving them.
///
/// When the connector starts or resumes a kernel coroutine, the compiler-generated `poll` runs
/// kernel code until it reaches a suspension point that creates and polls this `Wait`. The
/// connector has not yet seen the request, so that first call returns `Pending`, which suspends the
/// entire nested chain of futures and returns control to the connector. The futures remain
/// suspended indefinitely, unless/until the connector calls `TypedResume::resume` to trigger a
/// second poll. That second `poll` propagates through the suspended chain of futures until it
/// reaches `Wait::poll`, which now returns `Ready` and physically resumes the kernel coroutine.
///
/// Generator yields use the same mechanism, with the yield consumer taking the connector’s role.
///
/// NOTE: All polling is strictly sequential in the connector's calling thread, using a no-op
/// waker. No async runtime is involved in suspending and resuming coroutines. Further, the
/// connector's own async futures are _not_ disturbed when a kernel coroutine suspends, because the
/// kernel APIs to start and resume coroutines are normal sync methods that break the poll chain.
pub(super) struct Wait<Out, In>(pub(super) Arc<Exchange<Out, In>>);

impl<Out: Send, In: Send> Future for Wait<Out, In> {
    type Output = DeltaResult<In>;

    fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
        // WARNING: Never acquire another lock or invoke external code while holding this guard.
        let Ok(mut state) = self.0.lock() else {
            return Poll::Ready(Err(Error::internal_error(
                "coroutine exchange mutex was poisoned",
            )));
        };
        match std::mem::replace(&mut *state, ExchangeState::Complete) {
            pending @ (ExchangeState::Outbound(_) | ExchangeState::InFlight) => {
                *state = pending;
                Poll::Pending
            }
            ExchangeState::Inbound(response) => Poll::Ready(response),
            ExchangeState::Complete => Poll::Ready(Err(Error::internal_error(
                "coroutine exchange future was polled after completion",
            ))),
        }
    }
}

/// An instrumented `Future` enters its owning span both on `poll` and on `drop`. The former ensures
/// that kernel's work is correctly attributed to the span that created the workflow; the latter
/// (here) lets us mark an abandoned workflow as failed (otherwise, it defaults to success). This
/// works because `poll` always stores `Complete` before returning `Ready`; any other state means
/// the `Task` (which owns this `Wait`) was dropped while still suspended.
impl<Out, In> Drop for Wait<Out, In> {
    fn drop(&mut self) {
        // WARNING: We must drop the lock before invoking other code.
        let completed = self
            .0
            .lock()
            .is_ok_and(|state| matches!(*state, ExchangeState::Complete));
        if !completed {
            tracing::error!(
                error = "abandoned",
                "kernel coroutine was abandoned while suspended"
            );
        }
    }
}

/// Connector-side helper that starts or resumes a kernel workflow
pub(super) fn advance_workflow<O: Send + 'static>(
    mut task: Task<O>,
    mailbox: Arc<RequestMailbox>,
) -> DeltaResult<Workflow<O>> {
    let mut context = Context::from_waker(Waker::noop());
    match task.as_mut().poll(&mut context) {
        Poll::Ready(output) => output.map(Workflow::Done),
        Poll::Pending => {
            let pending = mailbox.take_pending()?;
            Ok(Workflow::Request(pending.into_request(
                task,
                mailbox,
                advance_workflow::<O>,
            )?))
        }
    }
}

/// Connector-side helper that starts or resumes a kernel generator
pub(super) fn advance_generator<O: Send + 'static, Y: Send + 'static>(
    mut task: Task<O>,
    mailbox: Arc<RequestMailbox>,
    yields: Arc<YieldMailbox<Y>>,
) -> DeltaResult<Generator<O, Y>> {
    let mut context = Context::from_waker(Waker::noop());
    if let Poll::Ready(output) = task.as_mut().poll(&mut context) {
        return output.map(Generator::Done);
    }

    if let Some(pending) = yields.take_pending()? {
        let item = pending.claim()?;
        let resume = TypedResume(Box::new(move |response| {
            pending.respond(response)?;
            advance_generator(task, mailbox, yields)
        }));
        return Ok(Generator::Yield(item, resume));
    }

    let pending = mailbox.take_pending()?;
    let next_yields = Arc::clone(&yields);
    let request = pending.into_request(task, mailbox, move |task, mailbox| {
        advance_generator(task, mailbox, next_yields)
    })?;
    Ok(Generator::Request(request))
}

/// Yield slot shared by a generator task and its driver.
///
/// The mailbox stores only a weak reference to the pending [`YieldExchange`], to avoid retaining an
/// exchange whose owning future was abandoned before the driver claimed it.
pub(super) type YieldMailbox<Y> = Mailbox<Weak<YieldExchange<Y>>>;

impl<Y> MailboxEntry for Weak<YieldExchange<Y>> {
    fn is_live(&self) -> bool {
        self.strong_count() > 0
    }
}

impl<Y> Mailbox<Weak<YieldExchange<Y>>> {
    /// Connector-side: Take a pending yield item (if any) after the coroutine suspends.
    pub(super) fn take_pending(&self) -> DeltaResult<Option<Arc<YieldExchange<Y>>>> {
        Ok(self
            .take_pending_opt()?
            .and_then(|pending| pending.upgrade()))
    }
}
