//! Async counterparts of standard combinators.
//!
//! Synchronous combinators cannot await inside their closures. These methods mirror their
//! synchronous namesakes one for one, except that the closure returns a future whose output is
//! awaited before being used.

use std::future::Future;

use delta_kernel_derive::internal_api;

/// Async counterparts of [`Result`] combinators.
#[internal_api]
#[allow(async_fn_in_trait)]
pub(crate) trait AsyncResult<T, E> {
    /// Async counterpart of [`Result::and_then`].
    async fn and_then_async<U, Fut>(self, f: impl FnOnce(T) -> Fut) -> Result<U, E>
    where
        Fut: Future<Output = Result<U, E>>;
}

impl<T, E> AsyncResult<T, E> for Result<T, E> {
    async fn and_then_async<U, Fut>(self, f: impl FnOnce(T) -> Fut) -> Result<U, E>
    where
        Fut: Future<Output = Result<U, E>>,
    {
        f(self?).await
    }
}

#[cfg(test)]
mod tests {
    use super::AsyncResult as _;

    #[tokio::test]
    async fn and_then_async_matches_result_and_then() {
        let mut result: Result<_, &str> = Ok(7);
        assert_eq!(
            result
                .and_then_async(|value| async move { Ok(value + 1) })
                .await,
            Ok(8)
        );
        result = Err("failed");
        assert_eq!(
            result
                .and_then_async(|_value| async move { panic!("error branch was evaluated") })
                .await,
            result
        );
    }
}
