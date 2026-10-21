use std::future::Future;

/// Prove type and lifetime bounds on the future produced by an async callable.
///
/// Every coroutine owns an underlying [`Future`] that executes its logic, usually of an unnamed
/// type produced by an async callable implementing e.g. [`AsyncFnOnce`]. The lifetime of that
/// future is bounded by any borrowed state it closes over, and it is Send only if every value
/// stored in its suspended state is Send.
///
/// When using an async callable to construct a new coroutine, the compiler must prove that the
/// resulting future is `Send + 'a`, where `'a` is the lifetime of the coroutine. Unfortunately,
/// stable Rust gives no direct way to apply type or lifetime bounds to the future produced by an
/// async callable. Naive code such as the following fails to compile:
///
/// ```compile_fail
/// # use std::future::Future;
/// fn foo<'a, A, O>(
///     mut f: impl AsyncFnMut(A) -> O + 'a,
///     arg: A,
/// ) -> impl Future<Output = O> + Send + 'a
/// {
///     f(arg)
/// }
/// ```
///
/// Fortunately, a carefully crafted "witness" idiom allows us to apply type and lifetime bounds to
/// that future even tho we cannot name it. This function generalizes the idiom for easy reference
/// and reuse.
///
/// # How it works
///
/// ```
/// # use std::future::Future;
/// async fn witness<A, O>(mut f: impl AsyncFnMut(A) -> O, arg: A) -> O {
///     f(arg).await
/// }
///
/// fn invoke_witness<'a, A, O, F, Fut>(
///     witness: impl FnOnce(F, A) -> Fut,
///     f: F,
///     arg: A,
/// ) -> Fut
/// where
///     F: AsyncFnMut(A) -> O,
///     Fut: Future<Output = O> + Send + 'a,
/// {
///     witness(f, arg)
/// }
///
/// let future = invoke_witness(witness, async |i| i + 1, 10);
/// ```
///
/// Passing the uninvoked `witness` lets type inference bind its opaque return type to `Fut` (the
/// witness’s outer future, which owns and awaits the unnamed inner future produced by the async
/// callable). Requiring `Fut: 'a` ensures that the witness future and any external state it
/// captures remain valid for the coroutine’s lifetime; `Fut: Send` applies transitively to the
/// inner future held across the await. The witness itself only needs the expressible argument and
/// output bounds on the async callable.
///
/// Crucially, `invoke_witness` must be called from a context where the async callable's concrete
/// type is still exposed to type inference. A caller generic over `F: AsyncFn*` is universally
/// quantified over every `F` satisfying those bounds, so it can only prove properties of `F`'s
/// unnameable call future that those bounds express (namely, its return type). With a particular
/// callable type, inference instead checks whether this specific `Fut` -- the witness future
/// produced for that callable -- satisfies `Fut: Send + 'a`. Invoking the witness inside an
/// already universally quantified generic layer cannot recover that proof:
///
/// ```compile_fail
/// # use std::future::Future;
/// # fn invoke_witness<'a, A, O, F, Fut>(
/// #     witness: impl FnOnce(F, A) -> Fut,
/// #     f: F,
/// #     arg: A,
/// # ) -> Fut
/// # where
/// #     F: AsyncFnMut(A) -> O,
/// #     Fut: Future<Output = O> + Send + 'a,
/// # {
/// #     witness(f, arg)
/// # }
/// async fn witness<A, O>(mut f: impl AsyncFnMut(A) -> O, arg: A) -> O {
///     f(arg).await
/// }
/// fn call_generically<'a, A: Send + 'a, O: Send + 'a>(
///     f: impl AsyncFnMut(A) -> O + Send + 'a,
///     arg: A,
/// ) -> impl Future<Output = O> + Send + 'a {
///     invoke_witness(witness, f, arg)
/// }
/// ```
///
/// NOTE: This witness incantation could be removed if either of the [`return_type_notation`] or
/// [`async_fn_traits`] language features were to stabilize.
///
/// [`return_type_notation`]: https://doc.rust-lang.org/beta/unstable-book/language-features/return-type-notation.html
/// [`async_fn_traits`]: https://doc.rust-lang.org/beta/unstable-book/library-features/async-fn-traits.html
pub(crate) fn invoke_witness<'a, Callable, Arg, Fut>(
    witness: impl FnOnce(Callable, Arg) -> Fut,
    callable: Callable,
    arg: Arg,
) -> Fut
where
    Fut: Future + Send + 'a,
{
    witness(callable, arg)
}
