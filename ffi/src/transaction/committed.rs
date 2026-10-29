use std::sync::Arc;

use delta_kernel::committer::Committer;
use delta_kernel::transaction::CommitResult;

use super::{ExclusiveCommittedTransaction, FfiCommittedTransaction};
use crate::error::{ExternResult, IntoExternResult};
use crate::handle::Handle;
use crate::{KernelResult, OptionalValue, SharedExternEngine, SharedSnapshot};

/// Converts a [`CommitResult`] to an owned FFI handle, or returns an error when the commit was not
/// successful.
///
/// The returned handle owns the committed transaction and must be released with
/// [`free_committed_transaction`].
// TODO(#3504): Preserve retryable and conflicted transaction state across the FFI boundary.
pub(super) fn commit_result_to_committed_handle<STATE>(
    result: KernelResult<(CommitResult<STATE>, Box<dyn Committer>)>,
) -> KernelResult<Handle<ExclusiveCommittedTransaction>> {
    let (result, committer) = result?;
    match result {
        CommitResult::Committed(transaction) => Ok(Box::new(FfiCommittedTransaction {
            transaction,
            committer,
        })
        .into()),
        CommitResult::Retryable(_) => Err(delta_kernel::KernelError::unsupported(
            "commit failed: retryable transaction not supported in FFI (yet)",
        )),
        CommitResult::Conflicted(conflicted) => Err(delta_kernel::KernelError::Generic(format!(
            "commit conflict at version {}",
            conflicted.conflict_version()
        ))),
    }
}

/// Free a committed-transaction handle.
///
/// # Safety
/// `txn` must be a valid handle and is consumed. Do not use or free it again.
#[no_mangle]
pub unsafe extern "C" fn free_committed_transaction(txn: Handle<ExclusiveCommittedTransaction>) {
    txn.drop_handle();
}

/// Read the committed version without consuming the handle.
///
/// # Safety
/// `txn` must be a valid handle and is borrowed for this call.
#[no_mangle]
pub unsafe extern "C" fn committed_transaction_version(
    txn: &Handle<ExclusiveCommittedTransaction>,
) -> u64 {
    unsafe { txn.as_ref() }.transaction.commit_version()
}

/// Return a fresh handle for the post-commit snapshot when one is available.
///
/// The returned snapshot handle is independently owned. This does not consume `txn`.
///
/// # Safety
/// `txn` must be a valid handle and is borrowed for this call.
#[no_mangle]
pub unsafe extern "C" fn committed_transaction_post_commit_snapshot(
    txn: &Handle<ExclusiveCommittedTransaction>,
) -> OptionalValue<Handle<SharedSnapshot>> {
    unsafe { txn.as_ref() }
        .transaction
        .post_commit_snapshot()
        .map(|snapshot| Arc::clone(snapshot).into())
        .into()
}

/// Publishes the post-commit snapshot using the committer retained by this transaction.
///
/// # Safety
///
/// `txn` and `engine` must be valid handles and are borrowed for this call.
#[no_mangle]
pub unsafe extern "C" fn committed_transaction_publish(
    txn: &Handle<ExclusiveCommittedTransaction>,
    engine: Handle<SharedExternEngine>,
) -> ExternResult<Handle<SharedSnapshot>> {
    let engine = unsafe { engine.as_ref() };
    let txn = unsafe { txn.as_ref() };
    txn.transaction
        .post_commit_snapshot()
        .ok_or_else(|| {
            delta_kernel::KernelError::generic("Cannot publish without a post-commit snapshot")
        })
        .and_then(|snapshot| snapshot.publish(engine.engine().as_ref(), txn.committer.as_ref()))
        .map(Into::into)
        .into_extern_result(&engine)
}
