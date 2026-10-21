use std::any::{type_name, Any};

use delta_kernel_derive::internal_api;
use derive_more::Constructor;

use super::channel::{ChannelExchange as _, Reply};
use super::generator::{
    unfold_mut, BoundGenerator, BoxedBoundGenerator, ChannelBinding, Generator, GeneratorExt,
};
use crate::utils::PhantomType;
use crate::{DeltaResult, Error};

/// Describes a connector operation that may return multiple pages.
pub trait PagedOperation: Send + Sized + 'static {
    /// Page payload produced by the connector.
    type Page: Send + 'static;
}

/// One page of connector data and the cursor for requesting more.
#[derive(Constructor)]
pub struct Page<Op: PagedOperation> {
    /// Data returned in this page.
    pub data: Op::Page,
    /// Cursor to pass to [`PageRequest::Next`], or `None` when the operation is exhausted.
    ///
    /// An empty payload with `Some(cursor)` is not exhaustion.
    pub next: Option<Cursor<Op>>,
}

/// Opaque connector-owned continuation state typed to one paged operation.
///
/// Kernel stores and returns cursors without inspecting their contents.
pub struct Cursor<Op> {
    state: Box<dyn Any + Send>,
    _op_type: PhantomType<Op>,
}

impl<Op> Cursor<Op> {
    /// Wrap connector-owned `state` for a later continuation request.
    pub fn new(state: impl Any + Send + 'static) -> Self {
        Self {
            state: Box::new(state),
            _op_type: PhantomType::default(),
        }
    }

    /// Recover the connector-owned state.
    ///
    /// Returns an error if `T` is not the type originally passed to [`Self::new`].
    pub fn into_inner<T: Any>(self) -> DeltaResult<T> {
        self.state.downcast().map(|inner| *inner).map_err(|_| {
            Error::internal_error(format!(
                "invalid cursor: unable to cast as {}",
                type_name::<T>()
            ))
        })
    }
}

/// One phase of a paginated connector operation.
pub enum PageRequest<Op: PagedOperation> {
    /// Initialize the operation and return a cursor without fetching the first page.
    Prepare(Op, Reply<Cursor<Op>>),
    /// Consume a cursor and return the next page.
    Next(Cursor<Op>, Reply<Page<Op>>),
}

/// Unbound generator of page payloads from a prepared cursor.
#[internal_api]
pub(crate) enum PageGenerator<Op: PagedOperation> {
    /// Stream pages from `cursor`.
    Next(Cursor<Op>),
    /// The operation is exhausted.
    Done,
}

impl<Op: PagedOperation> GeneratorExt for PageGenerator<Op> {}

impl<P, Op> Generator<P> for PageGenerator<Op>
where
    P: Send + 'static,
    Op: PagedOperation,
    PageRequest<Op>: Into<P>,
{
    type Item = Op::Page;
    type Output = ();

    fn bind_to<'a>(
        self,
        channel: ChannelBinding<'a, P>,
    ) -> impl BoundGenerator<Item = Op::Page, Output = ()> + Unpin + 'a
    where
        Self: 'a,
    {
        unfold_mut!(BoundPages::new(channel, self), BoundPages::next)
    }

    fn bind_boxed<'a>(
        self: Box<Self>,
        channel: ChannelBinding<'a, P>,
    ) -> BoxedBoundGenerator<'a, Op::Page>
    where
        Self: 'a,
    {
        Box::new((*self).bind_to(channel))
    }
}

struct BoundPages<'a, P: Send + 'static, Op: PagedOperation> {
    channel: ChannelBinding<'a, P>,
    state: PageGenerator<Op>,
}

impl<'a, P: Send + 'static, Op: PagedOperation> BoundPages<'a, P, Op> {
    fn new(channel: ChannelBinding<'a, P>, state: PageGenerator<Op>) -> Self {
        Self { channel, state }
    }

    async fn next(&mut self) -> DeltaResult<Option<Op::Page>>
    where
        PageRequest<Op>: Into<P>,
    {
        let cursor = match std::mem::replace(&mut self.state, PageGenerator::Done) {
            PageGenerator::Next(cursor) => cursor,
            PageGenerator::Done => return Ok(None),
        };
        let page = self.channel.exchange(cursor, PageRequest::Next).await?;
        if let Some(cursor) = page.next {
            self.state = PageGenerator::Next(cursor);
        }
        Ok(Some(page.data))
    }
}
