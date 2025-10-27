// TODO: (#1112) remove panics in this file
#![allow(clippy::unwrap_used)]
#![allow(clippy::expect_used)]
#![allow(clippy::panic)]

//! Task executor for running async IO in synchronous contexts.
//!
//! # Sync Mode (default)
//!
//! In sync mode, the default engine uses async IO internally (via `object_store`),
//! but kernel APIs are synchronous. The [TaskExecutor] trait provides methods to
//! run async tasks on a background thread.
//!
//! Behind the `tokio` feature flag, we provide both single-threaded and multi-threaded
//! executors based on Tokio:
//! - [tokio::TokioBackgroundExecutor] - Single-threaded runtime on a background thread
//! - [tokio::TokioMultiThreadExecutor] - Multi-threaded runtime
//!
//! # Async Mode (with `async` feature)
//!
//! In async mode, kernel APIs are natively async. [TaskExecutor] becomes an empty
//! marker trait with no methods, and the unit type `()` serves as the executor.
//! No actual task executor is needed since everything is already async.
#[cfg(not(feature = "async"))]
use futures::{future::BoxFuture, Future};

#[cfg(not(feature = "async"))]
use crate::DeltaResult;

/// An executor that can be used to run async tasks. This is used by IO functions
/// within the `DefaultEngine`.
///
/// In sync mode: This trait provides methods to run async tasks on a background thread.
/// In async mode: This is an empty marker trait (executor methods are not needed).
///
/// This must be capable of running within an async context and running futures
/// on another thread. This could be a multi-threaded runtime, like Tokio's or
/// could be a single-threaded runtime on a background thread.
#[cfg(not(feature = "async"))]
pub trait TaskExecutor: Send + Sync + 'static {
    /// Block on the given future, returning its output.
    ///
    /// This should NOT panic if called within an async context. Thus it can't
    /// be implemented by `tokio::runtime::Runtime::block_on`.
    fn block_on<T>(&self, task: T) -> T::Output
    where
        T: Future + Send + 'static,
        T::Output: Send + 'static;

    /// Run the future in the background.
    fn spawn<F>(&self, task: F)
    where
        F: Future<Output = ()> + Send + 'static;

    fn spawn_blocking<T, R>(&self, task: T) -> BoxFuture<'_, DeltaResult<R>>
    where
        T: FnOnce() -> R + Send + 'static,
        R: Send + 'static;
}

/// Empty marker trait for async mode - no executor methods needed.
#[cfg(feature = "async")]
pub trait TaskExecutor: Send + Sync + 'static {}

/// In async mode, the unit type `()` serves as the executor.
/// Since TaskExecutor is an empty marker trait in async mode, `()` is perfect:
/// - Zero-sized (no runtime cost)
/// - Already implements Send + Sync + Default
/// - Clearly represents "no executor needed"
#[cfg(feature = "async")]
impl TaskExecutor for () {}

/// The default executor type for the current build configuration.
///
/// In sync mode (default): `TokioBackgroundExecutor`
/// In async mode (with `async` feature): `()` (unit type - no executor needed)
#[cfg(not(feature = "async"))]
pub type DefaultTaskExecutor = tokio::TokioBackgroundExecutor;

#[cfg(feature = "async")]
pub type DefaultTaskExecutor = ();

/// Create a default task executor.
///
/// Returns an `Arc<DefaultTaskExecutor>` which is:
/// - In sync mode: `Arc<TokioBackgroundExecutor>`
/// - In async mode: `Arc<()>`
///
/// This helper is useful for tests to avoid clippy warnings in async mode about
/// "passing a unit value to a function" when calling `Arc::new(DefaultTaskExecutor::default())`.
///
/// # Example
///
/// ```rust
/// # use delta_kernel::engine::default::executor::default_task_executor;
/// let executor = default_task_executor();
/// // Use with handlers, engines, etc.
/// ```
pub fn default_task_executor() -> std::sync::Arc<DefaultTaskExecutor> {
    Default::default()
}

#[cfg(all(any(feature = "tokio", test), not(feature = "async")))]
pub mod tokio {
    use super::TaskExecutor;
    use futures::TryFutureExt;
    use futures::{future::BoxFuture, Future};
    use std::sync::mpsc::channel;
    use tokio::runtime::RuntimeFlavor;

    use crate::DeltaResult;

    /// A [`TaskExecutor`] that uses the tokio single-threaded runtime in a
    /// background thread to service tasks.
    #[derive(Debug)]
    pub struct TokioBackgroundExecutor {
        sender: tokio::sync::mpsc::Sender<BoxFuture<'static, ()>>,
        _thread: std::thread::JoinHandle<()>,
    }

    impl Default for TokioBackgroundExecutor {
        fn default() -> Self {
            Self::new()
        }
    }

    impl TokioBackgroundExecutor {
        pub fn new() -> Self {
            let (sender, mut receiver) = tokio::sync::mpsc::channel::<BoxFuture<'_, ()>>(50);
            let thread = std::thread::spawn(move || {
                let rt = tokio::runtime::Builder::new_current_thread()
                    .enable_all()
                    .build()
                    .unwrap();
                rt.block_on(async move {
                    while let Some(task) = receiver.recv().await {
                        tokio::task::spawn(task);
                    }
                });
            });
            Self {
                sender,
                _thread: thread,
            }
        }
    }

    impl TokioBackgroundExecutor {
        fn send_future(&self, fut: BoxFuture<'static, ()>) {
            // We cannot call `blocking_send()` because that calls `block_on`
            // internally and panics if called within an async context. 🤦
            let mut fut = Some(fut);
            loop {
                match self.sender.try_send(fut.take().unwrap()) {
                    Ok(()) => break,
                    Err(tokio::sync::mpsc::error::TrySendError::Full(original)) => {
                        std::thread::yield_now();
                        fut.replace(original);
                    }
                    Err(tokio::sync::mpsc::error::TrySendError::Closed(_)) => {
                        panic!("TokioBackgroundExecutor channel closed")
                    }
                };
            }
        }
    }

    impl TaskExecutor for TokioBackgroundExecutor {
        fn block_on<T>(&self, task: T) -> T::Output
        where
            T: Future + Send + 'static,
            T::Output: Send + 'static,
        {
            // We cannot call `tokio::runtime::Runtime::block_on` here because
            // it panics if called within an async context. So instead we spawn
            // the future on the runtime and send the result back using a channel.
            let (sender, receiver) = channel::<T::Output>();

            let fut = Box::pin(async move {
                let task_output = task.await;
                tokio::task::spawn_blocking(move || {
                    sender.send(task_output).ok();
                })
                .await
                .unwrap();
            });

            self.send_future(fut);

            receiver
                .recv()
                .expect("TokioBackgroundExecutor has crashed")
        }

        fn spawn<F>(&self, task: F)
        where
            F: Future<Output = ()> + Send + 'static,
        {
            self.send_future(Box::pin(task));
        }

        fn spawn_blocking<T, R>(&self, task: T) -> BoxFuture<'_, DeltaResult<R>>
        where
            T: FnOnce() -> R + Send + 'static,
            R: Send + 'static,
        {
            Box::pin(tokio::task::spawn_blocking(task).map_err(crate::Error::join_failure))
        }
    }

    /// A [`TaskExecutor`] that uses the tokio multi-threaded runtime. You can
    /// create one based on a handle to an existing runtime, so it can share
    /// the runtime with other parts of your application.
    #[derive(Debug)]
    pub struct TokioMultiThreadExecutor {
        handle: tokio::runtime::Handle,
    }

    impl TokioMultiThreadExecutor {
        pub fn new(handle: tokio::runtime::Handle) -> Self {
            assert_eq!(
                handle.runtime_flavor(),
                RuntimeFlavor::MultiThread,
                "TokioExecutor must be created with a multi-threaded runtime"
            );
            Self { handle }
        }
    }

    impl TaskExecutor for TokioMultiThreadExecutor {
        fn block_on<T>(&self, task: T) -> T::Output
        where
            T: Future + Send + 'static,
            T::Output: Send + 'static,
        {
            // We cannot call `tokio::runtime::Runtime::block_on` here because
            // it panics if called within an async context. So instead we spawn
            // the future on the runtime and send the result back using a channel.
            let (sender, receiver) = channel::<T::Output>();

            let fut = Box::pin(async move {
                let task_output = task.await;
                tokio::task::spawn_blocking(move || {
                    sender.send(task_output).ok();
                })
                .await
                .unwrap();
            });

            // We throw away the handle, but it should continue on.
            self.handle.spawn(fut);

            receiver
                .recv()
                .expect("TokioMultiThreadExecutor has crashed")
        }

        fn spawn<F>(&self, task: F)
        where
            F: Future<Output = ()> + Send + 'static,
        {
            self.handle.spawn(task);
        }

        fn spawn_blocking<T, R>(&self, task: T) -> BoxFuture<'_, DeltaResult<R>>
        where
            T: FnOnce() -> R + Send + 'static,
            R: Send + 'static,
        {
            Box::pin(tokio::task::spawn_blocking(task).map_err(crate::Error::join_failure))
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;
        use crate::engine::default::executor::DefaultTaskExecutor;

        async fn test_executor(executor: impl TaskExecutor) {
            // Can run a task
            let task = async {
                tokio::time::sleep(std::time::Duration::from_millis(10)).await;
                2 + 2
            };
            let result = executor.block_on(task);
            assert_eq!(result, 4);

            // Can spawn a task
            let (sender, receiver) = channel::<i32>();
            executor.spawn(async move {
                tokio::time::sleep(std::time::Duration::from_millis(10)).await;
                sender.send(2 + 2).unwrap();
            });
            let result = receiver.recv().unwrap();
            assert_eq!(result, 4);
        }

        #[tokio::test]
        async fn test_tokio_background_executor() {
            let executor = DefaultTaskExecutor::default();
            test_executor(executor).await;
        }

        #[tokio::test(flavor = "multi_thread", worker_threads = 1)]
        async fn test_tokio_multi_thread_executor() {
            let executor = TokioMultiThreadExecutor::new(tokio::runtime::Handle::current());
            test_executor(executor).await;
        }
    }
}
