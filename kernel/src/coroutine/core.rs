//! Transport primitives for connector-driven coroutines.
//!
//! A [`Task`] is the backing primitive both [`Workflow`](super::Workflow) and
//! [`Generator`](super::Generator) build on. It is comprised of a [`Channel`] and a [`Future`] that
//! holds an [`Arc`] reference to that same channel. When connectors start a generator or workflow,
//! it triggers a call to [`Task::poll_step`], which polls the future so it can start running.
//!
//! The task body can offload a request to the connector by invoking [`ChannelExchange::exchange`],
//! which creates a new [`Exchange`] in the [`Outbound`](ExchangeState::Outbound) state and returns
//! a [`Wait`] future that wraps it. Polling that future for the first time [pushes](Channel::push)
//! a [`Weak`] reference to the `Exchange` onto the `Channel` FIFO (upcast as dyn [`Accept`]) and
//! returns [`Poll::Pending`]. Subsequent polls see the non-zero weak reference count and do not
//! push it to the channel a second time.
//!
//! Once the async poll stack unwinds back to `Task::poll_step`, the latter [pops](Channel::pop) the
//! `Exchange` out of the `Channel` and invokes [`Accept::accept`] to produce a vocabulary-specific
//! request payload as well as a [`Reply`] that holds a `Weak<Exchange>` reference (now upcast as
//! dyn [`Complete`]). It uses a caller-provided constructor to convert that `(request, reply)` pair
//! to a [`Step::Request`]. At this point the `Exchange` is in the
//! [`Waiting`](ExchangeState::Waiting) state.
//!
//! The connector chooses how to process the request and communicates the result back to the
//! suspended task by invoking [`Reply::send`], which puts the exchange in the
//! [`Inbound`](ExchangeState::Inbound) state. Dropping an unused `Reply` rejects the request, and
//! the task receives [`Error::RequestRejected`].
//!
//! If an async connector provided a [`Context`], sending a reply notifies the appropriate [`Waker`]
//! so the connector's async runtime knows to poll the task again. Otherwise, a [no-op
//! waker](Waker::noop) is used and the task's driver must manually invoke `poll_step` again. Either
//! way, polling the `Wait` again allows it to retrieve the inbound response from the `Exchange` and
//! return it as [`Poll::Ready`]. The task then resumes execution from its `await` point.
//!
//! Tasks are allowed to create and wait on multiple requests concurrently; such requests can be
//! fetched by invoking `Task::poll_step` repeatedly as long as it continues to return
//! [`Poll::Ready`], without waiting for replies to previously-received requests. Tasks are also
//! allowed to abandon requests (failed speculative execution, unneeded prefetching, etc). If the
//! task abandons a request while it is still in the `Channel`, the weak reference becomes empty and
//! `Task::poll_step` ignores it. If the task abandons a request after it has been delivered to the
//! connector, [`Reply::send`] will silently drop the connector's response.
//!
//! Eventually, the task completes and the poll stack unwinds one last time with a [`Poll::Ready`]
//! containing the task's output [`DeltaResult`], which `Task::poll_step` returns as [`Step::Done`].

use std::collections::VecDeque;
use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, Mutex, MutexGuard, Weak};
use std::task::{Context, Poll, Waker};

use delta_kernel_derive::internal_api;

use crate::utils::PhantomType;
use crate::{DeltaResult, Error};

/// Sendable future whose output uses Kernel's error type.
#[internal_api]
pub(crate) trait DeltaFuture<T>: Future<Output = DeltaResult<T>> + Send {}

impl<T, F> DeltaFuture<T> for F where F: Future<Output = DeltaResult<T>> + Send {}

/// Routes typed requests from workflow and generator bodies to their connector driver.
///
/// Kernel futures use a `Channel<P>` to post requests from vocabulary `P` and await replies. Those
/// requests surface to the connector as steps from the enclosing workflow or generator, and the
/// connector chooses whether/how to execute each request.
///
/// Passing an existing channel to nested operations routes all their requests through the same
/// connector-owned driver. Most connectors use channels only indirectly through Kernel entry
/// points; custom coroutine implementations use them directly in concert with [`ChannelExchange`]..
#[internal_api]
pub(crate) struct Channel<P: 'static>(Mutex<VecDeque<Weak<dyn Accept<P>>>>);

impl<P: 'static> Default for Channel<P> {
    fn default() -> Self {
        Self(Mutex::default())
    }
}

impl<P: 'static> Channel<P> {
    /// Push a new outbound [`Exchange`] just before suspending.
    fn push(&self, request: Weak<dyn Accept<P>>) {
        if let Some(mut requests) = self.try_lock() {
            requests.push_back(request);
        }
    }
    /// Pop the oldest outbound [`Exchange`] after the poll stack unwinds.
    pub(crate) fn pop(&self) -> Option<P> {
        loop {
            let request = self.try_lock()?.pop_front()?;
            if let Some(request) = request.upgrade().and_then(Accept::accept) {
                return Some(request);
            }
        }
    }

    /// Lock the mutex and return the resulting guard.
    ///
    /// Channels are never accessed concurrently, so the mutex should always be free; a held or
    /// poisoned mutex produces `None` and effectively disables the channel.
    fn try_lock(&self) -> Option<MutexGuard<'_, VecDeque<Weak<dyn Accept<P>>>>> {
        self.0.try_lock().ok().or_else(|| {
            debug_assert!(false, "channel mutex already held or poisoned");
            None
        })
    }
}

trait Accept<P>: Send + Sync {
    fn accept(self: Arc<Self>) -> Option<P>;
}

trait Complete<In>: Send + Sync {
    fn complete(&self, result: DeltaResult<In>);
}

/// Single-use capability to complete a connector request.
///
/// Sending or dropping this capability wakes the executor task that last polled the request
/// future. Dropping it without replying rejects the request.
#[must_use = "dropping a reply rejects its connector request"]
pub struct Reply<In: Send + 'static>(Weak<dyn Complete<In>>);

impl<In: Send + 'static> Reply<In> {
    /// Deliver `result` to the suspended Kernel operation.
    ///
    /// The result is silently discarded if that operation has been abandoned. Otherwise its
    /// executor task is notified that the operation can make progress.
    pub fn send(mut self, result: DeltaResult<In>) {
        let disarm_drop = Weak::<Mutex<Exchange<(), In, (), ()>>>::new();
        if let Some(exchange) = std::mem::replace(&mut self.0, disarm_drop).upgrade() {
            exchange.complete(result);
        }
    }
}

impl<In: Send + 'static> Drop for Reply<In> {
    fn drop(&mut self) {
        if let Some(exchange) = self.0.upgrade() {
            exchange.complete(Err(Error::RequestRejected));
        }
    }
}

enum ExchangeState<Out, In, Make> {
    Outbound(Out, Make),
    Waiting,
    Inbound(DeltaResult<In>),
}

struct Exchange<Out, In, Make, P> {
    state: ExchangeState<Out, In, Make>,
    waker: Option<Waker>,
    _request_type: PhantomType<P>,
}

impl<Out, In, Make, P> Exchange<Out, In, Make, P> {
    /// Creates a new outbound request
    fn new(outbound: Out, make_request: Make) -> Self {
        Self {
            state: ExchangeState::Outbound(outbound, make_request),
            waker: None,
            _request_type: PhantomType::default(),
        }
    }
}

impl<P, Out: Send, In: Send, Make: Send> Complete<In> for Mutex<Exchange<Out, In, Make, P>> {
    fn complete(&self, result: DeltaResult<In>) {
        if let Some(mut exchange) = lock_exchange(self) {
            if matches!(exchange.state, ExchangeState::Waiting) {
                exchange.state = ExchangeState::Inbound(result);
                let waker = exchange.waker.take();
                drop(exchange);
                if let Some(waker) = waker {
                    waker.wake();
                }
            }
        }
    }
}

impl<Out, In, Make, P> Accept<P> for Mutex<Exchange<Out, In, Make, P>>
where
    Make: FnOnce(Out, Reply<In>) -> P + Send + 'static,
    Out: Send + 'static,
    In: Send + 'static,
    P: 'static,
{
    fn accept(self: Arc<Self>) -> Option<P> {
        let mut exchange = lock_exchange(&self)?;
        match std::mem::replace(&mut exchange.state, ExchangeState::Waiting) {
            ExchangeState::Outbound(outbound, make_request) => {
                drop(exchange);
                let completion = Arc::downgrade(&self);
                Some(make_request(outbound, Reply(completion)))
            }
            invalid => {
                exchange.state = invalid;
                drop(exchange);
                debug_assert!(false, "channel contained a non-outbound exchange");
                None
            }
        }
    }
}

/// Request/reply primitive for vocabulary-specific [`Channel`] extension traits.
///
/// Downstream crates cannot add inherent methods to [`Channel`]. Instead, a request vocabulary
/// author can define an extension trait with `ChannelExchange<P>` as a supertrait, implement
/// request methods in the trait body using [`Self::exchange`], and provide an empty blanket
/// implementation for `Channel<P>`.
///
/// # Example
///
/// ```
/// use delta_kernel::coroutine::{Channel, ChannelExchange, DeltaFuture, Reply};
///
/// enum Request {
///     Length(String, Reply<usize>),
/// }
///
/// trait ChannelExt: ChannelExchange<Request> {
///     fn length(&self, value: String) -> impl DeltaFuture<usize> {
///         self.exchange(value, Request::Length)
///     }
/// }
///
/// impl ChannelExt for Channel<Request> {}
/// ```
#[internal_api]
pub(crate) trait ChannelExchange<P: Send + 'static> {
    /// Create a lazy request/reply exchange.
    ///
    /// Polling the returned future offers the request produced by passing `outbound` and a
    /// single-use [`Reply<In>`](Reply) to `make_request`. The future resolves to the result sent
    /// through that reply. Dropping the reply resolves the future with [`Error::RequestRejected`];
    /// dropping the future abandons the exchange, and any later reply is silently discarded.
    fn exchange<Out: Send + 'static, In: Send + 'static, R: Into<P>>(
        &self,
        outbound: Out,
        make_request: impl FnOnce(Out, Reply<In>) -> R + Send + 'static,
    ) -> impl DeltaFuture<In>;
}

impl<P: Send + 'static> ChannelExchange<P> for Channel<P> {
    fn exchange<Out: Send + 'static, In: Send + 'static, R: Into<P>>(
        &self,
        outbound: Out,
        make_request: impl FnOnce(Out, Reply<In>) -> R + Send + 'static,
    ) -> impl DeltaFuture<In> {
        let make_request = move |outbound, reply| make_request(outbound, reply).into();
        Wait {
            channel: self,
            exchange: Arc::new(Mutex::new(Exchange::new(outbound, make_request))),
        }
    }
}

/// Result of advancing a [`Task`].
pub(crate) enum Step<R, O> {
    /// The task completed.
    Done(O),
    /// The request currently stored in the task's channel.
    Request(R),
}

/// Retains one pinned coroutine and the request lanes observed by its driver.
pub(crate) struct Task<'a, P: Send + 'static, O> {
    body: Option<Pin<Box<dyn DeltaFuture<O> + 'a>>>,
    channel: Arc<Channel<P>>,
}

impl<'a, P: Send + 'static, O> Task<'a, P, O> {
    /// Create a task using the concrete future type exposed by `witness`.
    ///
    /// A task must satisfy `Send + 'a`, where `'a` is the lifetime bound the task's owner imposes.
    /// The task owns the underlying [`Future`] produced by calling the [`AsyncFnOnce`] `F` stored
    /// in `State`, and that future has an unnamed lifetime for whatever state it closes over
    /// (call it `'f` for explanatory purposes). In order for the task to satisfy `Send + 'a`,
    /// we must constrain the underlying future to also satisfy `Send + 'a`, which in turn
    /// requires `'f: 'a`. This is problematic, because stable Rust cannot name or constrain the
    /// type or lifetime of the future an `AsyncFnOnce` produces. `state`, `witness`, and this
    /// method work together to overcome that limitation as follows:
    ///
    /// * `State` preserves `F` as a type parameter; when instantiated with a concrete type, the
    ///   compiler can determine the concrete type of the future `F` produces when invoked, along
    ///   with the (unnamed) lifetime `'f` of any borrowed references that future captures.
    /// * [`Self::new_with_witness`] consumes `state` to produce a `Task<'a, _, _>`. It also
    ///   receives a generic [`FnOnce`] that consumes `State` and the task's channel to produce a
    ///   `Fut: DeltaFuture<O> + 'a`.
    /// * The form-specific witness satisfies the `FnOnce` type bound. It is important that the
    ///   witness' own signature imposes neither lifetime nor type bounds on that future, other than
    ///   its return type, because return type is the only bound the compiler can unconditionally
    ///   enforce against `F: AsyncFnOnce`.
    /// * When this method is invoked with the form-specific witness, type inference unifies the
    ///   latter's opaque return type with `Fut`. The `Fut: DeltaFuture<O> + 'a` bound now
    ///   constrains the concrete future `F` produces, including `'f: 'a`. Any unsatisfied bound
    ///   fails compilation.
    ///
    /// NOTE: This witness incantation could be removed if either of the [`return_type_notation`] or
    /// [`async_fn_traits`] language features were to stabilize.
    ///
    /// [`AsyncFnOnce`]: std::ops::AsyncFnOnce
    /// [`return_type_notation`]: https://doc.rust-lang.org/beta/unstable-book/language-features/return-type-notation.html
    /// [`async_fn_traits`]: https://doc.rust-lang.org/beta/unstable-book/library-features/async-fn-traits.html
    pub(crate) fn new_with_witness<State, F, Fut>(state: State, witness: F) -> Self
    where
        F: FnOnce(State, Arc<Channel<P>>) -> Fut,
        Fut: DeltaFuture<O> + 'a,
    {
        let channel = Arc::default();
        let body = Some(Box::pin(witness(state, Arc::clone(&channel))) as _);
        Self { body, channel }
    }

    /// Poll once using `context`, returning pending when the task offers no request.
    pub(crate) fn poll_step(&mut self, context: &mut Context<'_>) -> Poll<DeltaResult<Step<P, O>>> {
        let Some(body) = self.body.as_mut() else {
            return Poll::Ready(Err(Error::internal_error(
                "coroutine task was advanced after completion",
            )));
        };
        // Reconcile ready replies before accepting queued offers. A request becomes visible to the
        // connector only when accepted, so polling first lets completed race branches cancel offers
        // that no longer require connector work.
        if let Poll::Ready(output) = body.as_mut().poll(context) {
            self.body = None;
            return Poll::Ready(output.map(Step::Done));
        }
        if let Some(request) = self.channel.pop() {
            return Poll::Ready(Ok(Step::Request(request)));
        }
        Poll::Pending
    }
}

/// Create a task context backed by a no-op waker.
pub(super) fn noop_context() -> Context<'static> {
    Context::from_waker(Waker::noop())
}

struct Wait<'a, P: 'static, Out, In, Make> {
    channel: &'a Channel<P>,
    exchange: Arc<Mutex<Exchange<Out, In, Make, P>>>,
}

impl<P, Out, In, Make> Future for Wait<'_, P, Out, In, Make>
where
    Make: FnOnce(Out, Reply<In>) -> P + Send + 'static,
    P: Send + 'static,
    Out: Send + 'static,
    In: Send + 'static,
{
    type Output = DeltaResult<In>;

    fn poll(self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
        // Cloning and dropping a Waker may invoke arbitrary executor code, so we clone it before
        // acquiring the lock; `let _waker = ...` keeps it alive until after we release the lock.
        let waker = context.waker().clone();
        let Some(mut exchange) = lock_exchange(&self.exchange) else {
            return Poll::Pending;
        };
        let _waker = exchange.waker.replace(waker);
        match std::mem::replace(&mut exchange.state, ExchangeState::Waiting) {
            ExchangeState::Outbound(outbound, make_request) => {
                // Read the weak count while holding the lock to prevent a race where the weak count
                // of an already-offered exchange drops to zero just after we release the lock.
                let weak_count = Arc::weak_count(&self.exchange);
                exchange.state = ExchangeState::Outbound(outbound, make_request);
                drop(exchange);
                if weak_count == 0 {
                    let acceptor = Arc::downgrade(&self.exchange);
                    self.channel.push(acceptor);
                }
                Poll::Pending
            }
            ExchangeState::Waiting => {
                drop(exchange);
                Poll::Pending
            }
            ExchangeState::Inbound(inbound) => {
                let _waker = exchange.waker.take();
                drop(exchange);
                Poll::Ready(inbound)
            }
        }
    }
}

fn lock_exchange<T>(mutex: &Mutex<T>) -> Option<MutexGuard<'_, T>> {
    mutex.lock().ok().or_else(|| {
        debug_assert!(false, "coroutine exchange mutex was poisoned");
        None
    })
}

#[cfg(test)]
mod tests {
    use std::pin::pin;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::task::Wake;

    use super::*;

    enum Request {
        Ping(&'static str, Reply<()>),
    }

    #[derive(Default)]
    struct WakeCounter(AtomicUsize);

    impl Wake for WakeCounter {
        fn wake(self: Arc<Self>) {
            self.wake_by_ref();
        }

        fn wake_by_ref(self: &Arc<Self>) {
            self.0.fetch_add(1, Ordering::Relaxed);
        }
    }

    impl WakeCounter {
        fn count(&self) -> usize {
            self.0.load(Ordering::Relaxed)
        }
    }

    #[test]
    fn accepted_request_completes_with_its_reply() {
        let channel = Channel::default();
        let mut future = pin!(channel.exchange("request", Request::Ping));
        assert!(future.as_mut().poll(&mut noop_context()).is_pending());
        let Some(Request::Ping(outbound, reply)) = channel.pop() else {
            panic!("live exchange was not accepted");
        };
        assert_eq!(outbound, "request");
        reply.send(Ok(()));
        assert!(matches!(
            future.as_mut().poll(&mut noop_context()),
            Poll::Ready(Ok(()))
        ));
    }

    #[test]
    fn reply_wakes_the_latest_registered_waker() {
        let channel = Channel::default();
        let mut future = pin!(channel.exchange("request", Request::Ping));
        let first = Arc::new(WakeCounter::default());
        let first_waker = Waker::from(Arc::clone(&first));
        let mut first_context = Context::from_waker(&first_waker);
        assert!(future.as_mut().poll(&mut first_context).is_pending());
        let Some(Request::Ping(_outbound, reply)) = channel.pop() else {
            panic!("live exchange was not accepted");
        };

        let second = Arc::new(WakeCounter::default());
        let second_waker = Waker::from(Arc::clone(&second));
        let mut second_context = Context::from_waker(&second_waker);
        assert!(future.as_mut().poll(&mut second_context).is_pending());
        reply.send(Ok(()));

        assert_eq!(first.count(), 0);
        assert_eq!(second.count(), 1);
        assert!(future.as_mut().poll(&mut second_context).is_ready());
    }

    #[test]
    fn dropping_reply_wakes_the_waiter_with_an_error() {
        let channel = Channel::default();
        let mut future = pin!(channel.exchange("request", Request::Ping));
        let counter = Arc::new(WakeCounter::default());
        let waker = Waker::from(Arc::clone(&counter));
        let mut context = Context::from_waker(&waker);
        assert!(future.as_mut().poll(&mut context).is_pending());
        let Some(Request::Ping(_outbound, reply)) = channel.pop() else {
            panic!("live exchange was not accepted");
        };

        drop(reply);

        assert_eq!(counter.count(), 1);
        let Poll::Ready(Err(err)) = future.as_mut().poll(&mut context) else {
            panic!("dropped reply did not reject its request");
        };
        assert!(matches!(err, Error::RequestRejected));
    }

    #[test]
    fn overlapping_requests_remain_queued_first_in_first_out() {
        let channel = Channel::default();
        let mut first = pin!(channel.exchange("first", Request::Ping));
        let mut second = pin!(channel.exchange("second", Request::Ping));
        assert!(first.as_mut().poll(&mut noop_context()).is_pending());
        assert!(second.as_mut().poll(&mut noop_context()).is_pending());

        let Some(Request::Ping(outbound, first_reply)) = channel.pop() else {
            panic!("first exchange was not accepted");
        };
        assert_eq!(outbound, "first");

        let Some(Request::Ping(outbound, second_reply)) = channel.pop() else {
            panic!("second exchange was not retained");
        };
        assert_eq!(outbound, "second");

        first_reply.send(Ok(()));
        second_reply.send(Ok(()));
        assert!(first.as_mut().poll(&mut noop_context()).is_ready());
        assert!(second.as_mut().poll(&mut noop_context()).is_ready());
    }

    #[test]
    fn expired_offer_does_not_hide_a_later_live_request() {
        let channel = Channel::default();
        let mut expired = Box::pin(channel.exchange("expired", Request::Ping));
        let mut live = pin!(channel.exchange("live", Request::Ping));
        assert!(expired.as_mut().poll(&mut noop_context()).is_pending());
        assert!(live.as_mut().poll(&mut noop_context()).is_pending());
        drop(expired);

        let Some(Request::Ping(outbound, reply)) = channel.pop() else {
            panic!("live exchange after expired offer was not accepted");
        };
        assert_eq!(outbound, "live");
        reply.send(Ok(()));
        assert!(live.as_mut().poll(&mut noop_context()).is_ready());
    }

    #[test]
    fn dropping_accepted_reply_rejects_request() {
        let channel = Channel::default();
        let mut future = pin!(channel.exchange("request", Request::Ping));
        assert!(future.as_mut().poll(&mut noop_context()).is_pending());
        let Some(Request::Ping(_outbound, reply)) = channel.pop() else {
            panic!("live exchange was not accepted");
        };
        drop(reply);

        let Poll::Ready(Err(err)) = future.as_mut().poll(&mut noop_context()) else {
            panic!("dropped reply did not reject its request");
        };
        assert!(matches!(err, Error::RequestRejected));
    }

    #[test]
    fn abandoned_waiter_expires_offer_and_reply() {
        let channel = Channel::default();
        let mut future = Box::pin(channel.exchange("request", Request::Ping));
        assert!(future.as_mut().poll(&mut noop_context()).is_pending());
        drop(future);
        assert!(channel.pop().is_none());

        let mut future = Box::pin(channel.exchange("request", Request::Ping));
        assert!(future.as_mut().poll(&mut noop_context()).is_pending());
        let Some(Request::Ping(_outbound, reply)) = channel.pop() else {
            panic!("live exchange was not accepted");
        };
        drop(future);
        reply.send(Ok(()));
    }
}
