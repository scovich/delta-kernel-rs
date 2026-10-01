//! Async counterparts of standard combinators.
//!
//! Synchronous combinators cannot await inside their closures. These methods mirror their
//! synchronous namesakes one for one, except that the closure returns a future whose output is
//! awaited before being used.

use std::future::Future;

/// Async counterpart of [`bool`] combinators.
pub(crate) trait AsyncBool {
    /// Async counterpart of [`bool::then`].
    async fn then_async<T, Fut>(self, f: impl FnOnce() -> Fut) -> Option<T>
    where
        Fut: Future<Output = T>;
}

impl AsyncBool for bool {
    async fn then_async<T, Fut>(self, f: impl FnOnce() -> Fut) -> Option<T>
    where
        Fut: Future<Output = T>,
    {
        if self {
            Some(f().await)
        } else {
            None
        }
    }
}

/// Async counterparts of [`Option`] combinators.
pub(crate) trait AsyncOption<T> {
    /// Async counterpart of [`Option::map`].
    async fn map_async<U, Fut>(self, f: impl FnOnce(T) -> Fut) -> Option<U>
    where
        Fut: Future<Output = U>;

    /// Async counterpart of [`Option::and_then`].
    async fn and_then_async<U, Fut>(self, f: impl FnOnce(T) -> Fut) -> Option<U>
    where
        Fut: Future<Output = Option<U>>;
}

impl<T> AsyncOption<T> for Option<T> {
    async fn map_async<U, Fut>(self, f: impl FnOnce(T) -> Fut) -> Option<U>
    where
        Fut: Future<Output = U>,
    {
        match self {
            Some(value) => Some(f(value).await),
            None => None,
        }
    }

    async fn and_then_async<U, Fut>(self, f: impl FnOnce(T) -> Fut) -> Option<U>
    where
        Fut: Future<Output = Option<U>>,
    {
        match self {
            Some(value) => f(value).await,
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::AsyncBool as _;

    #[tokio::test]
    async fn then_async_matches_bool_then() {
        assert_eq!(true.then_async(|| async { 7 }).await, Some(7));
        let value: Option<i32> = false
            .then_async(|| async { panic!("false branch was evaluated") })
            .await;
        assert_eq!(value, None);
    }
}
