//! Core infrastructure for kernel coroutines.
//!
//! Kernel coroutines are normal async tasks that call normal async functions. The coroutine's
//! synchronous driver code launches the coroutine by polling the compiler-generated future that
//! represents it. As with all async code, the task runs inside [`Future::poll`] and can invoke
//! other functions as it goes. Async functions return futures that it polls in turn. When the
//! coroutine needs to communicate with the connector, it creates and polls a special [`Wait`]
//! future whose [`poll`](Wait::poll) method immediately returns [`Poll::Pending`]. That triggers a
//! cascading unwind of all the parent `poll` invocations until control returns to the synchronous
//! coroutine driver. The driver then returns the coroutine's request to the connector, along with a
//! [`Resume`] closure. When the connector invokes the `Resume` with its response, the closure again
//! polls the coroutine's future, which rebuilds the chain of `poll` calls back to
//! [`Wait::poll`]. This time, that call returns [`Poll::Ready`] with the connector's response, and
//! execution continues until the coroutine either completes or suspends again.
//!
//! Because [`Poll::Pending`] does not carry a payload, the coroutine driver creates an [`Outbox`]
//! which it shares with the coroutine via a [`Channel`](crate::coroutine::Channel). Whenever the
//! coroutine needs to suspend, it creates an [`Exchange`] in [`Outbound`](ExchangeState::Outbound)
//! state. It stores one reference to the exchange in the outbox so the sync coroutine driver can
//! access the request, and initializes a [`Wait`] instance with a second reference to the
//! exchange. [`Wait::poll`] returns [`Poll::Pending`] because the exchange is still
//! [`Outbound`](ExchangeState::Outbound), the async poll stack unwinds, and the sync driver
//! extracts the exchange from the outbox, leaving it empty again. When invoked, the [`Resume`]
//! closure stores the connector's response in the exchange as [`Inbound`](ExchangeState::Inbound),
//! the async poll stack builds back up, and [`Wait::poll`] extracts the response from the exchange.
//!
//! [`Resume`]: crate::coroutine::Resume
use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, Mutex, MutexGuard};
use std::task::{Context, Poll};

use tracing::error;

use crate::{DeltaResult, Error};

/// Entry stored in a coroutine outbox.
///
/// Entries are always weak references, in case the future that posted an entry gets dropped while
/// the coroutine's poll stack is unwinding (after [`Wait::poll`] returned `Pending` but before
/// control returns to the coroutine driver that would claim the entry). If that ever happened, the
/// outbox (and the coroutine as a whole) would be unable to process any more requests, effectively
/// killing the coroutine. This would require unusual circumstances, such as a kernel generator
/// racing requests against yields and dropping the loser (even tho neither request nor yield is a
/// terminal state for a generator).
pub(super) trait OutboxEntry: Default {
    /// True if the entry's weak reference is still valid.
    fn is_live(&self) -> bool;
}

/// Single-slot outbox that delivers [`Exchange`] instances to the coroutine's driver when the
/// coroutine suspends. It is always empty, except while the async poll stack unwinds.
///
/// Each outbox has exactly two shared references. The coroutine driver holds one reference, and the
/// coroutine's [`Channel`](crate::coroutine::Channel) holds the other. While the coroutine is
/// executing, both handles are owned by their respective stack frames, ensuring sequential
/// access. The [`Resume`](crate::coroutine::Resume) closure holds both references while the
/// coroutine is suspended, so the references move between threads together or not at all.
// NOTE: Rc+RefCell would be safe, but Arc+Mutex allows the compiler to derive Send
#[derive(Default)]
pub(super) struct Outbox<T: OutboxEntry>(Mutex<T>);

impl<T: OutboxEntry> Outbox<T> {
    /// Put an entry, returning an error if the outbox is poisoned or already occupied.
    pub(super) fn put(&self, entry: T) -> DeltaResult<()> {
        // WARNING: Never acquire another lock or invoke external code while holding this guard.
        let Ok(mut existing_entry) = self.0.lock() else {
            return Err(Error::internal_error("coroutine outbox mutex was poisoned"));
        };
        if existing_entry.is_live() {
            return Err(Error::internal_error(
                "coroutine outbox is already occupied",
            ));
        }
        *existing_entry = entry;
        Ok(())
    }

    /// Take the entry, returning `None` if the outbox is empty.
    pub(super) fn take(&self) -> DeltaResult<Option<T>> {
        // WARNING: Never acquire another lock or invoke external code while holding this guard.
        let Ok(mut entry) = self.0.lock() else {
            return Err(Error::internal_error("coroutine outbox mutex was poisoned"));
        };
        let entry = std::mem::take(&mut *entry);
        Ok(entry.is_live().then_some(entry))
    }
}

/// Single-use operation or yield handoff shared by its waiter and resume handle.
///
/// Each exchange has exactly two shared references. The task holds one, and places the other in an
/// [`Outbox`] before suspending, for use by the synchronous coroutine driver. All accesses are
/// sequential. The [`Resume`](crate::coroutine::Resume) closure holds both references while the
/// coroutine is suspended, so the references move between threads together or not at all.
// NOTE: Rc+RefCell would be safe, but Arc+Mutex allows the compiler to derive Send
pub(super) struct Exchange<Out, In>(Mutex<ExchangeState<Out, In>>);

/// Lifecycle state of an exchange.
enum ExchangeState<Out, In> {
    /// Kernel offered a request and has suspended the workflow.
    Outbound(Out),
    /// Connector claimed the request but has not responded yet.
    InFlight,
    /// Connector supplied a response but kernel did not claim it yet.
    Inbound(DeltaResult<In>),
    /// Kernel has consumed the response.
    Complete,
}

impl<Out, In> Exchange<Out, In> {
    /// Create an exchange containing the outbound operation.
    pub(super) fn new(outbound: Out) -> Self {
        Self(Mutex::new(ExchangeState::Outbound(outbound)))
    }

    /// Claim the request kernel offered when suspending a workflow.
    pub(super) fn claim(&self) -> DeltaResult<Out> {
        // WARNING: Never acquire another lock or invoke external code while holding this guard.
        let mut state = self.lock()?;
        match std::mem::replace(&mut *state, ExchangeState::InFlight) {
            ExchangeState::Outbound(outbound) => Ok(outbound),
            previous => {
                *state = previous;
                Err(Error::internal_error(
                    "coroutine suspended without providing any outbound exchange",
                ))
            }
        }
    }

    /// Supply a response before resuming the workflow.
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

    fn lock(&self) -> DeltaResult<MutexGuard<'_, ExchangeState<Out, In>>> {
        self.0
            .lock()
            .map_err(|_| Error::internal_error("coroutine exchange mutex was poisoned"))
    }
}

/// The `Future` suspension boundary between kernel coroutines and the connector driving them.
///
/// When the connector starts or resumes a kernel coroutine, its compiler-generated `poll` runs
/// kernel code until it reaches a suspension point that creates and polls this `Wait`. The
/// connector has not yet seen the request, so that first call returns `Pending`, which suspends the
/// entire nested chain of futures and returns control to the connector. The futures remain
/// suspended indefinitely, unless/until the connector invokes its [`crate::coroutine::Resume`] to
/// trigger a second poll. That second `poll` propagates through the suspended chain of futures
/// until it reaches `Wait::poll`, which now returns `Ready` and allows the kernel coroutine to
/// continue executing until it completes or suspends again.
///
/// Generator yields use the same mechanism, with the yield consumer taking the connector's role.
///
/// Polling is strictly sequential in the connector's calling thread, using a no-op waker. The side
/// channels afforded by [`Exchange`] and [`Outbox`] elminate the need for an async runtime.
pub(super) struct Wait<Out, In>(pub(super) Arc<Exchange<Out, In>>);

impl<Out: Send, In: Send> Future for Wait<Out, In> {
    type Output = DeltaResult<In>;

    fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
        // WARNING: Never acquire another lock or invoke external code while holding this guard.
        let mut state = match self.0.lock() {
            Err(err) => return Poll::Ready(Err(err)),
            Ok(state) => state,
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

/// An instrumented `Future` enters its owning span on both `poll` and on `drop`. The former ensures
/// that kernel's work is correctly attributed to the span that created the workflow; the latter
/// (here) lets us mark an abandoned workflow as failed (otherwise, it defaults to success). This
/// works because `poll` always stores `Complete` in the exchange before returning `Ready`; any
/// other state means the task (which owns this `Wait`) was dropped while still suspended.
impl<Out, In> Drop for Wait<Out, In> {
    fn drop(&mut self) {
        // WARNING: We must drop the lock before invoking other code.
        let completed = self
            .0
            .lock()
            .is_ok_and(|state| matches!(*state, ExchangeState::Complete));
        if !completed {
            error!(
                error = "abandoned",
                "kernel coroutine was abandoned while suspended"
            );
        }
    }
}
