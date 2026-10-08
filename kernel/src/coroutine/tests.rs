use std::borrow::Borrow;
use std::future::{pending, poll_fn, Future as _};
use std::ops::Deref;
use std::pin::{pin, Pin};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
use std::task::Poll;

use bytes::Bytes;
use futures::future::join;
use futures::stream::{FuturesUnordered, StreamExt as _};
use rstest::rstest;
use tempfile::tempdir;
use tracing::field::Empty;
use tracing::info_span;
use url::Url;

use super::engine::{drive_with_engine, run_coroutine_with_connector, EngineConnector};
use super::generator::{
    empty, from_iter, generator, map_async, static_generator_coroutine, GeneratorExt as _,
    GeneratorStep,
};
use super::kernel::request::{BackwardListing, ForwardListing, ListingBounds};
use super::kernel::{
    coroutine, generator_fn, Channel, GeneratorCoroutineStep, StaticGeneratorCoroutine,
};
use super::*;
use crate::engine::sync::SyncEngine;
use crate::metrics::MetricEvent;
use crate::unit_test_utils::{
    assert_result_error_with_message, install_thread_local_metrics_reporter, CapturingReporter,
};
use crate::{DeltaResult, DeltaResultIteratorStatic, Error};

/// Connector action after the toy coroutine's single `ReadSmallFile` request.
#[derive(Clone, Copy)]
enum ReplyOutcome {
    Ok,
    Err,
    Drop,
}

#[test]
fn lazy_coroutine_runs_only_after_the_caller_requests_a_step() {
    let ran = Arc::new(AtomicBool::new(false));
    let coroutine_ran = Arc::clone(&ran);
    let mut coroutine = coroutine!(async move |_channel| {
        coroutine_ran.store(true, Ordering::Relaxed);
        Ok(())
    });

    assert!(!ran.load(Ordering::Relaxed));
    assert!(matches!(
        coroutine.try_advance().unwrap(),
        CoroutineStep::Done(())
    ));
    assert!(ran.load(Ordering::Relaxed));
}

#[test]
fn lazy_generator_runs_only_after_the_caller_starts_it() {
    let ran = Arc::new(AtomicBool::new(false));
    let generator_ran = Arc::clone(&ran);
    let mut generator = static_generator_coroutine!(Request, async move |channel| {
        generator_ran.store(true, Ordering::Relaxed);
        channel.yield_item(()).await
    });

    assert!(!ran.load(Ordering::Relaxed));
    let GeneratorCoroutineStep::Yield(()) = generator.try_advance().unwrap() else {
        panic!("generator did not yield");
    };
    assert!(ran.load(Ordering::Relaxed));
}

#[tokio::test(flavor = "multi_thread")]
async fn async_coroutine_resumes_after_cross_task_reply() {
    let location = Url::parse("memory:///async-task").unwrap();
    let mut coroutine =
        coroutine!(async move |channel| channel.read_small_file(location, None).await);
    let CoroutineStep::Request(Request::ReadSmallFile(_file, reply)) =
        coroutine.advance().await.unwrap()
    else {
        panic!("coroutine did not produce its request");
    };
    let reply_task = tokio::spawn(async move {
        tokio::task::yield_now().await;
        reply.send(Ok(Some(Bytes::from_static(b"response"))));
    });

    let CoroutineStep::Done(output) = coroutine.advance().await.unwrap() else {
        panic!("coroutine did not complete");
    };
    reply_task.await.unwrap();
    assert_eq!(output, Some(Bytes::from_static(b"response")));
}

#[tokio::test]
async fn selectively_polled_channel_futures_keep_every_request_live() {
    let first = Url::parse("memory:///first").unwrap();
    let second = Url::parse("memory:///second").unwrap();
    let mut coroutine = coroutine!(async move |channel| {
        let mut reads = FuturesUnordered::new();
        reads.push(channel.read_small_file(first, None));
        reads.push(channel.read_small_file(second, None));
        let mut lengths = Vec::new();
        while let Some(bytes) = reads.next().await {
            lengths.push(bytes?.ok_or_else(|| Error::generic("empty read"))?.len());
        }
        Ok(lengths)
    });

    let mut requests = 0;
    let mut lengths = loop {
        match coroutine.advance().await.unwrap() {
            CoroutineStep::Done(lengths) => break lengths,
            CoroutineStep::Request(Request::ReadSmallFile(_file, reply)) => {
                requests += 1;
                reply.send(Ok(Some(Bytes::from(vec![0; requests]))));
            }
            CoroutineStep::Request(_) => panic!("coroutine requested an unexpected operation"),
        }
    };

    lengths.sort_unstable();
    assert_eq!(requests, 2);
    assert_eq!(lengths, [1, 2]);
}

#[tokio::test(flavor = "multi_thread")]
async fn async_generator_resumes_after_cross_task_reply() {
    let location = Url::parse("memory:///async-generator").unwrap();
    let mut generator = static_generator_coroutine!(Request, async move |channel| {
        let bytes = channel
            .read_small_file(location, None)
            .await?
            .ok_or_else(|| Error::generic("empty read"))?;
        channel.yield_item(bytes.len()).await
    });
    let GeneratorCoroutineStep::Request(Request::ReadSmallFile(_file, reply)) =
        generator.advance().await.unwrap()
    else {
        panic!("generator did not produce its request");
    };
    let reply_task = tokio::spawn(async move {
        tokio::task::yield_now().await;
        reply.send(Ok(Some(Bytes::from_static(b"response"))));
    });

    let GeneratorCoroutineStep::Yield(output) = generator.advance().await.unwrap() else {
        panic!("generator did not yield");
    };
    reply_task.await.unwrap();
    assert_eq!(output, 8);
}

#[test]
fn iterator_generator_is_lazy_and_stops_at_first_error() {
    let polls = Arc::new(AtomicUsize::new(0));
    let iterator_polls = Arc::clone(&polls);
    let items: DeltaResultIteratorStatic<usize> = Box::new(std::iter::from_fn(move || {
        match iterator_polls.fetch_add(1, Ordering::Relaxed) {
            0 => Some(Ok(1)),
            1 => Some(Err(Error::generic("iterator failed"))),
            _ => panic!("generator polled past the first error"),
        }
    }));
    let mut generator = StaticGeneratorCoroutine::from_iterator(items);

    assert_eq!(polls.load(Ordering::Relaxed), 0);
    let GeneratorCoroutineStep::Yield(item) = generator.try_advance().unwrap() else {
        panic!("generator did not yield the first item");
    };
    assert_eq!(item, 1);
    assert_eq!(polls.load(Ordering::Relaxed), 1);

    assert_result_error_with_message(generator.try_advance(), "iterator failed");
    assert_eq!(polls.load(Ordering::Relaxed), 2);
}

#[test]
fn nested_async_operation_uses_the_parent_request_channel() {
    async fn child(channel: &Channel, location: Url) -> DeltaResult<Option<Bytes>> {
        channel.read_small_file(location, None).await
    }

    let location = Url::parse("memory:///nested").unwrap();
    let mut coroutine = coroutine!(async move |channel| child(channel, location).await);

    let CoroutineStep::Request(Request::ReadSmallFile(_file, reply)) =
        coroutine.try_advance().unwrap()
    else {
        panic!("nested coroutine did not expose its read request");
    };
    reply.send(Ok(Some(Bytes::from_static(b"response"))));
    let CoroutineStep::Done(bytes) = coroutine.try_advance().unwrap() else {
        panic!("nested coroutine did not complete");
    };
    assert_eq!(bytes, Some(Bytes::from_static(b"response")));
}

#[test]
fn coroutine_retains_handle_between_advances() {
    let location = Url::parse("memory:///async-task").unwrap();
    let mut coroutine =
        coroutine!(async move |channel| { channel.read_small_file(location, None).await });

    let CoroutineStep::Request(Request::ReadSmallFile(_file, reply)) =
        coroutine.try_advance().unwrap()
    else {
        panic!("coroutine did not produce its request");
    };
    let bytes = Ok(Some(Bytes::from_static(b"response")));
    reply.send(bytes);
    let CoroutineStep::Done(output) = coroutine.try_advance().unwrap() else {
        panic!("coroutine did not complete");
    };
    assert_eq!(output, Some(Bytes::from_static(b"response")));
    assert_result_error_with_message(coroutine.try_advance(), "advanced after completion");
}

#[test]
fn coroutine_try_advance_before_reply_would_block() {
    let location = Url::parse("memory:///async-task").unwrap();
    let mut coroutine =
        coroutine!(async move |channel| { channel.read_small_file(location, None).await });
    let CoroutineStep::Request(Request::ReadSmallFile(_file, reply)) =
        coroutine.try_advance().unwrap()
    else {
        panic!("coroutine did not produce its request");
    };
    assert!(matches!(coroutine.try_advance(), Err(Error::WouldBlock)));
    reply.send(Ok(Some(Bytes::new())));
    assert!(matches!(
        coroutine.try_advance().unwrap(),
        CoroutineStep::Done(_)
    ));
}

#[rstest]
#[case::ok(ReplyOutcome::Ok)]
#[case::err(ReplyOutcome::Err)]
#[case::drop(ReplyOutcome::Drop)]
fn reporting_span_tracks_single_reply_outcome(#[case] outcome: ReplyOutcome) {
    let reporter = Arc::new(CapturingReporter::default());
    let _guard = install_thread_local_metrics_reporter(reporter.clone());
    {
        // Any lifecycle span with `report` works; this is not a CRC-read test.
        let span = info_span!("crc_read_completed", report = Empty);
        let _enter = span.enter();
        let location = Url::parse("memory:///toy").unwrap();
        let mut coroutine =
            coroutine!(async move |channel| channel.read_small_file(location, None).await);
        let CoroutineStep::Request(Request::ReadSmallFile(_file, reply)) =
            coroutine.try_advance().unwrap()
        else {
            panic!("toy coroutine should suspend once on ReadSmallFile");
        };
        match outcome {
            ReplyOutcome::Ok => {
                reply.send(Ok(Some(Bytes::from_static(b"ok"))));
                let _ = coroutine.try_advance().unwrap();
            }
            ReplyOutcome::Err => {
                reply.send(Err(Error::generic("connector failed the read")));
                let _ = coroutine.try_advance();
            }
            ReplyOutcome::Drop => {
                drop(reply);
                drop(coroutine);
            }
        }
    }

    let events = reporter.events();
    let success = events
        .iter()
        .any(|e| matches!(e, MetricEvent::CrcReadSuccess(_)));
    let failure = events
        .iter()
        .any(|e| matches!(e, MetricEvent::CrcReadFailure));
    match outcome {
        ReplyOutcome::Ok => {
            assert!(success, "expected CrcReadSuccess; got: {events:?}");
            assert!(!failure, "did not expect CrcReadFailure; got: {events:?}");
        }
        ReplyOutcome::Err | ReplyOutcome::Drop => {
            assert!(failure, "expected CrcReadFailure; got: {events:?}");
            assert!(!success, "did not expect CrcReadSuccess; got: {events:?}");
        }
    }
}

#[test]
fn coroutine_output_is_independent_of_request_response_type() {
    let location = Url::parse("memory:///answer").unwrap();
    let expected_location = location.clone();
    let mut coroutine = coroutine!(async move |channel| {
        let bytes = channel
            .read_small_file(location, None)
            .await?
            .ok_or_else(|| Error::generic("empty read"))?;
        Ok(format!("read {} bytes", bytes.len()))
    });
    let output = loop {
        match coroutine.try_advance().unwrap() {
            CoroutineStep::Done(output) => break output,
            CoroutineStep::Request(Request::ReadSmallFile((location, range), reply)) => {
                assert_eq!(location, expected_location);
                assert_eq!(range, None);
                reply.send(Ok(Some(Bytes::from_static(b"answer"))));
            }
            CoroutineStep::Request(_) => {
                panic!("coroutine requested an unexpected operation")
            }
        }
    };

    assert_eq!(output, "read 6 bytes");
}

#[test]
fn connector_facing_generator_interleaves_requests_and_yields() {
    let location = Url::parse("memory:///item").unwrap();
    let mut generator = static_generator_coroutine!(Request, async move |channel| {
        let bytes = channel
            .read_small_file(location, None)
            .await?
            .ok_or_else(|| Error::generic("empty read"))?;
        channel.yield_item(bytes.len()).await?;
        Ok(())
    });

    let mut yielded = Vec::new();
    loop {
        match generator.try_advance().unwrap() {
            GeneratorCoroutineStep::Done(()) => break,
            GeneratorCoroutineStep::Yield(item) => yielded.push(item),
            GeneratorCoroutineStep::Request(Request::ReadSmallFile(_file, reply)) => {
                reply.send(Ok(Some(Bytes::from_static(b"generated item"))));
            }
            GeneratorCoroutineStep::Request(_) => {
                panic!("generator requested an unexpected operation")
            }
        }
    }

    assert_eq!(yielded, vec![14]);
}

#[test]
fn concurrent_yields_are_drained_before_generator_resumes() {
    let resumed = Arc::new(AtomicBool::new(false));
    let generator_resumed = Arc::clone(&resumed);
    let mut generator = static_generator_coroutine!(Request, async move |channel| {
        let (first, second) = join(channel.yield_item(1), channel.yield_item(2)).await;
        first?;
        second?;
        generator_resumed.store(true, Ordering::Relaxed);
        Ok(3)
    });

    assert!(matches!(
        generator.try_advance().unwrap(),
        GeneratorCoroutineStep::Yield(1)
    ));
    assert!(!resumed.load(Ordering::Relaxed));
    assert!(matches!(
        generator.try_advance().unwrap(),
        GeneratorCoroutineStep::Yield(2)
    ));
    assert!(!resumed.load(Ordering::Relaxed));
    assert!(matches!(
        generator.try_advance().unwrap(),
        GeneratorCoroutineStep::Done(3)
    ));
    assert!(resumed.load(Ordering::Relaxed));
}

#[test]
fn dropped_yield_future_remains_committed_before_terminal_output() {
    let mut generator = static_generator_coroutine!(Request, async move |channel| {
        let mut offered = pin!(channel.yield_item("item"));
        poll_fn(|context| {
            assert!(offered.as_mut().poll(context).is_pending());
            Poll::Ready(())
        })
        .await;
        Ok(42)
    });

    assert!(matches!(
        generator.try_advance().unwrap(),
        GeneratorCoroutineStep::Yield("item")
    ));
    assert!(matches!(
        generator.try_advance().unwrap(),
        GeneratorCoroutineStep::Done(42)
    ));
}

#[test]
fn abandoning_generator_drops_queued_yields_and_terminal_output() {
    struct DropProbe(Arc<AtomicUsize>);

    impl Drop for DropProbe {
        fn drop(&mut self) {
            self.0.fetch_add(1, Ordering::Relaxed);
        }
    }

    let drops = Arc::new(AtomicUsize::new(0));
    let generator_drops = Arc::clone(&drops);
    let mut generator = static_generator_coroutine!(Request, async move |channel| {
        let mut first = pin!(channel.yield_item(DropProbe(Arc::clone(&generator_drops))));
        let mut second = pin!(channel.yield_item(DropProbe(Arc::clone(&generator_drops))));
        poll_fn(|context| {
            assert!(first.as_mut().poll(context).is_pending());
            assert!(second.as_mut().poll(context).is_pending());
            Poll::Ready(())
        })
        .await;
        Ok(DropProbe(generator_drops))
    });

    let GeneratorCoroutineStep::Yield(first) = generator.try_advance().unwrap() else {
        panic!("generator did not yield the first item");
    };
    assert_eq!(drops.load(Ordering::Relaxed), 0);

    drop(generator);
    assert_eq!(drops.load(Ordering::Relaxed), 2);

    drop(first);
    assert_eq!(drops.load(Ordering::Relaxed), 3);
}

#[test]
fn prepare_threads_opaque_state_to_continue() {
    let bounds = ListingBounds {
        prefix: Url::parse("memory:///").unwrap(),
        low: Url::parse("memory:///00000000000000000000").unwrap(),
        high: Url::parse("memory:///00000000000000000002").unwrap(),
    };
    let mut coroutine = coroutine!(async move |channel| {
        let cursor = channel.prepare_paged(ForwardListing::new(bounds)).await?;
        let page = channel.continue_paged(cursor).await?;
        assert!(page.next.is_none());
        Ok(page.data.len())
    });

    let output = loop {
        match coroutine.try_advance().unwrap() {
            CoroutineStep::Done(output) => break output,
            CoroutineStep::Request(Request::ListForward(PageRequest::Prepare(
                _operation,
                reply,
            ))) => {
                reply.send(Ok(Cursor::new(7_i64)));
            }
            CoroutineStep::Request(Request::ListForward(PageRequest::Continue(cursor, reply))) => {
                assert_eq!(cursor.into_inner::<i64>().unwrap(), 7);
                reply.send(Ok(Page::new(Vec::new(), None)));
            }
            CoroutineStep::Request(Request::ListForward(PageRequest::Start(..))) => {
                panic!("coroutine unexpectedly started listing eagerly")
            }
            CoroutineStep::Request(_) => {
                panic!("coroutine requested an unexpected operation")
            }
        }
    };

    assert_eq!(output, 0);
}

#[test]
fn parent_intercepts_child_items_while_child_io_reaches_connector() {
    #[derive(Debug, PartialEq, Eq)]
    struct Report {
        items: Vec<Bytes>,
    }

    async fn parent(channel: &Channel) -> DeltaResult<Report> {
        let first = Url::parse("memory:///first")?;
        let second = Url::parse("memory:///second")?;
        let mut child = generator!(channel, async move |channel| {
            for location in [first, second] {
                let item = channel
                    .read_small_file(location, None)
                    .await?
                    .ok_or_else(|| Error::generic("empty read"))?;
                channel.yield_item(item).await?;
            }
            Ok(())
        });
        let mut items = Vec::new();
        while let Some(item) = child.next().await? {
            items.push(item);
        }
        Ok(Report { items })
    }

    let mut connector_inputs = Vec::new();
    let mut coroutine = coroutine!(parent);
    let output = loop {
        match coroutine.try_advance().unwrap() {
            CoroutineStep::Done(output) => break output,
            CoroutineStep::Request(Request::ReadSmallFile((location, range), reply)) => {
                assert_eq!(range, None);
                connector_inputs.push(location.path().to_string());
                reply.send(Ok(Some(Bytes::copy_from_slice(location.path().as_bytes()))));
            }
            CoroutineStep::Request(_) => {
                panic!("coroutine requested an unexpected operation")
            }
        }
    };

    assert_eq!(connector_inputs, vec!["/first", "/second"]);
    assert_eq!(
        output,
        Report {
            items: vec![
                Bytes::from_static(b"/first"),
                Bytes::from_static(b"/second"),
            ],
        }
    );
}

#[test]
fn from_iter_yields_each_item_then_exhausts() {
    let items = from_iter([1, 2, 3]);
    let mut coroutine = coroutine!(async move |channel| {
        let mut items = items.bind(channel);
        let mut output = Vec::new();
        while let Some(item) = items.next().await? {
            output.push(item);
        }
        Ok(output)
    });
    let CoroutineStep::Done(output) = coroutine.try_advance().unwrap() else {
        panic!("expected completion");
    };
    assert_eq!(output, [1, 2, 3]);
}

#[test]
fn flatten_yields_collection_elements_then_exhausts() {
    let items = from_iter([vec![1, 2], vec![], vec![3]]);
    let mut coroutine = coroutine!(async move |channel| {
        let mut items = items.flatten().bind(channel);
        let mut output = Vec::new();
        while let Some(item) = items.next().await? {
            output.push(item);
        }
        Ok(output)
    });
    let CoroutineStep::Done(output) = coroutine.try_advance().unwrap() else {
        panic!("expected completion");
    };
    assert_eq!(output, [1, 2, 3]);
}

#[test]
fn map_and_chain_transform_items_and_forward_child_requests() {
    let first = generator_fn!(async move |channel| {
        let location = Url::parse("memory:///first")?;
        let bytes = channel
            .read_small_file(location, None)
            .await?
            .ok_or_else(|| Error::generic("empty read"))?;
        channel.yield_item(bytes.len()).await
    });
    let second = generator_fn!(async move |channel| channel.yield_item(100).await);
    let items = empty().chain(first).chain(second).map(|len| len + 1);

    let mut coroutine = coroutine!(async move |channel| {
        let mut items = items.bind(channel);
        let mut output = Vec::new();
        while let Some(item) = items.next().await? {
            output.push(item);
        }
        Ok(output)
    });
    let output = loop {
        match coroutine.try_advance().unwrap() {
            CoroutineStep::Done(output) => break output,
            CoroutineStep::Request(Request::ReadSmallFile(_file, reply)) => {
                reply.send(Ok(Some(Bytes::from_static(b"12345"))));
            }
            CoroutineStep::Request(_) => {
                panic!("coroutine requested an unexpected operation")
            }
        }
    };

    assert_eq!(output, [6, 101]);
}

#[test]
fn map_passes_through_errors_after_mapping_earlier_items() {
    let items = generator_fn!(async move |channel| {
        channel.yield_item(1).await?;
        Err(Error::generic("generator failed"))
    });
    let items = items.map(|item| item * 10);

    let mut coroutine = coroutine!(async move |channel| {
        let mut items = items.bind(channel);
        let first = items.next().await?;
        assert_eq!(first, Some(10));
        items.next().await
    });

    assert_result_error_with_message(coroutine.try_advance(), "generator failed");
}

#[test]
fn filter_ok_retains_matches_and_passes_through_item_errors() {
    let items = vec![Ok(1), Ok(2), Err(Error::generic("item failed")), Ok(4)];
    let items = from_iter(items).filter_ok(|item: &i32| item % 2 == 0);
    let mut coroutine = coroutine!(async move |channel| {
        let mut items = items.bind(channel);
        assert_eq!(items.next().await?.transpose()?, Some(2));
        let error = items.next().await?.unwrap().unwrap_err();
        assert!(error.to_string().contains("item failed"));
        assert_eq!(items.next().await?.transpose()?, Some(4));
        assert!(items.next().await?.is_none());
        Ok(())
    });

    assert!(matches!(
        coroutine.try_advance().unwrap(),
        CoroutineStep::Done(())
    ));
}

#[test]
fn compatible_adapters_preserve_terminal_output() {
    let tail = generator_fn!(async move |channel| -> DeltaResult<i32> {
        channel.yield_item(Ok(1)).await?;
        channel.yield_item(Ok(2)).await?;
        Ok(42)
    });
    let first: [DeltaResult<i32>; 1] = [Ok(0)];
    let items = from_iter(first)
        .chain(tail)
        .map(|item: DeltaResult<i32>| item.map(|value| value + 1))
        .map_ok(|value| value * 2);
    let items = map_async!(items, async move |_channel, item| {
        item.map(|value| value + 1)
    });
    let items = items.filter_ok(|value: &i32| *value >= 5).peekable();
    let mut generator: StaticGeneratorCoroutine<DeltaResult<i32>, i32> =
        static_generator_coroutine!(Request, async move |channel| {
            channel.yield_from(items.into_boxed()).await
        });

    let GeneratorCoroutineStep::Yield(first) = generator.try_advance().unwrap() else {
        panic!("expected first item");
    };
    assert_eq!(first.unwrap(), 5);
    let GeneratorCoroutineStep::Yield(second) = generator.try_advance().unwrap() else {
        panic!("expected second item");
    };
    assert_eq!(second.unwrap(), 7);
    let GeneratorCoroutineStep::Done(output) = generator.try_advance().unwrap() else {
        panic!("expected completion");
    };
    assert_eq!(output, 42);
}

#[test]
fn flatten_ok_flattens_inner_generators_and_preserves_output() {
    let inner = generator_fn!(async move |channel| {
        channel.yield_item(1).await?;
        channel.yield_item(2).await
    });
    let outer = generator_fn!(async move |channel| -> DeltaResult<i32> {
        channel.yield_item(Ok::<_, Error>(inner)).await?;
        channel
            .yield_item(Err(Error::generic("outer item failed")))
            .await?;
        Ok(42)
    });
    let items = outer.flatten_ok();
    let mut generator: StaticGeneratorCoroutine<DeltaResult<i32>, i32> =
        static_generator_coroutine!(Request, async move |channel| {
            channel.yield_from(items.into_boxed()).await
        });

    let GeneratorCoroutineStep::Yield(first) = generator.try_advance().unwrap() else {
        panic!("expected first item");
    };
    assert_eq!(first.unwrap(), 1);
    let GeneratorCoroutineStep::Yield(second) = generator.try_advance().unwrap() else {
        panic!("expected second item");
    };
    assert_eq!(second.unwrap(), 2);
    let GeneratorCoroutineStep::Yield(error) = generator.try_advance().unwrap() else {
        panic!("expected outer item error");
    };
    assert!(error.unwrap_err().to_string().contains("outer item failed"));
    let GeneratorCoroutineStep::Done(output) = generator.try_advance().unwrap() else {
        panic!("expected completion");
    };
    assert_eq!(output, 42);
}

#[test]
fn peekable_preserves_output_after_peeking_at_completion() {
    let items = generator_fn!(async move |channel| {
        channel.yield_item(1).await?;
        Ok(42)
    });
    let mut coroutine = coroutine!(async move |channel| {
        let mut items = items.bind(channel).peekable();
        assert_eq!(items.peek().await?, Some(&1));
        assert!(matches!(
            poll_fn(|context| Pin::new(&mut items).poll_step(context)).await?,
            GeneratorStep::Yield(1)
        ));
        assert_eq!(items.peek().await?, None);
        match poll_fn(|context| Pin::new(&mut items).poll_step(context)).await? {
            GeneratorStep::Done(output) => Ok(output),
            GeneratorStep::Yield(_) => Err(Error::internal_error("expected completion")),
        }
    });

    let CoroutineStep::Done(output) = coroutine.try_advance().unwrap() else {
        panic!("expected completion");
    };
    assert_eq!(output, 42);
}

#[test]
fn try_collect_collects_successes_and_short_circuits_item_errors() {
    let items: Vec<DeltaResult<_>> = vec![Ok(1), Ok(2)];
    let mut coroutine = coroutine!(async move |channel| {
        let collected: Vec<_> = from_iter(items).bind(channel).try_collect().await?;
        Ok(collected)
    });
    let CoroutineStep::Done(collected) = coroutine.try_advance().unwrap() else {
        panic!("generator did not complete");
    };
    assert_eq!(collected, [1, 2]);

    let items = vec![Ok(1), Err(Error::generic("item failed")), Ok(3)];
    let mut coroutine = coroutine!(async move |channel| {
        let collected: Vec<_> = from_iter(items).bind(channel).try_collect().await?;
        Ok(collected)
    });
    assert_result_error_with_message(coroutine.try_advance(), "item failed");
}

#[test]
fn channel_taking_generator_supports_static_and_borrowed_state() {
    fn operation<'a>(
        channel: impl Deref<Target = Channel> + Send + 'a,
        prefix: impl Borrow<str> + Send + 'a,
    ) -> BoundGenerator<'a, String> {
        generator!(channel, async move |channel| {
            channel
                .yield_item(format!("{} item", prefix.borrow()))
                .await?;
            assert!(!prefix.borrow().is_empty());
            Ok(())
        })
    }

    let mut generator =
        StaticGeneratorCoroutine::new(move |channel| operation(channel, String::from("root")));
    let GeneratorCoroutineStep::Yield(item) = generator.try_advance().unwrap() else {
        panic!("static generator did not yield");
    };
    assert_eq!(item, "root item");
    let GeneratorCoroutineStep::Done(()) = generator.try_advance().unwrap() else {
        panic!("static generator did not complete");
    };
    assert_result_error_with_message(generator.try_advance(), "advanced after completion");

    async fn parent(channel: &Channel) -> DeltaResult<Vec<String>> {
        let prefix = "scoped".to_string();
        let prefix = prefix.as_str();
        let mut child = operation(channel, prefix);
        let mut items = Vec::new();
        while let Some(item) = child.next().await? {
            items.push(item);
        }
        Ok(items)
    }

    let mut coroutine = coroutine!(parent);
    let CoroutineStep::Done(items) = coroutine.try_advance().unwrap() else {
        panic!("parent coroutine did not complete");
    };
    assert_eq!(items, ["scoped item"]);
}

#[test]
fn static_generator_coroutine_returns_terminal_output() {
    let mut generator: StaticGeneratorCoroutine<&'static str, usize> =
        static_generator_coroutine!(Request, async move |channel| {
            channel.yield_item("item").await?;
            Ok(42)
        });
    assert!(matches!(
        generator.try_advance().unwrap(),
        GeneratorCoroutineStep::Yield("item")
    ));
    assert!(matches!(
        generator.try_advance().unwrap(),
        GeneratorCoroutineStep::Done(42)
    ));
}

#[test]
fn static_generator_coroutine_try_advance_before_request_reply_would_block() {
    let location = Url::parse("memory:///item").unwrap();
    let mut generator: StaticGeneratorCoroutine<()> =
        static_generator_coroutine!(Request, async move |channel| {
            channel.read_small_file(location, None).await?;
            Ok(())
        });
    let GeneratorCoroutineStep::Request(Request::ReadSmallFile(_file, reply)) =
        generator.try_advance().unwrap()
    else {
        panic!("generator did not request a read");
    };
    assert!(matches!(generator.try_advance(), Err(Error::WouldBlock)));
    reply.send(Ok(Some(Bytes::new())));
    assert!(matches!(
        generator.try_advance().unwrap(),
        GeneratorCoroutineStep::Done(())
    ));
}

#[test]
fn engine_coroutine_can_borrow_caller_state() {
    let engine = SyncEngine::new();
    let value = "scoped".to_string();
    let value = value.as_str();
    let coroutine = coroutine!(async move |_channel| Ok::<_, Error>(value.len()));
    let length = drive_with_engine(&engine, None, coroutine).unwrap();

    assert_eq!(length, 6);
}

#[test]
fn try_advance_without_connector_work_would_block() {
    let mut coroutine = coroutine!(async |_channel| {
        pending::<()>().await;
        Ok(())
    });
    assert!(matches!(coroutine.try_advance(), Err(Error::WouldBlock)));
}

#[test]
fn engine_connector_drives_real_storage_operations() {
    let temp_dir = tempdir().unwrap();
    let location = Url::from_file_path(temp_dir.path().join("data.bin")).unwrap();
    let expected = Bytes::from_static(b"kernel coroutine");
    let write_data = expected.clone();

    let coroutine = coroutine!(async move |channel| {
        channel.write_bytes(&location, write_data, false).await?;
        channel.read_small_file(location, None).await
    });
    let connector = EngineConnector::new(&SyncEngine::new());
    let actual = connector.drive(coroutine).unwrap();
    assert_eq!(actual, Some(expected));
}

#[test]
fn engine_connector_pages_forward_listing() {
    let temp_dir = tempdir().unwrap();
    let root = Url::from_directory_path(temp_dir.path()).unwrap();
    let expected: Vec<_> = (1..=3)
        .map(|version| root.join(&format!("{version:020}.json")).unwrap())
        .collect();
    let sync_engine = SyncEngine::new();
    let connector = EngineConnector::new(&sync_engine).with_cancellation_token(None);

    let generator = static_generator_coroutine!(Request, async move |channel| {
        for version in 1..=3 {
            channel
                .write_bytes(
                    &root.join(&format!("{version:020}.json"))?,
                    Bytes::new(),
                    false,
                )
                .await?;
        }
        let mut pages = channel.flattened(ForwardListing::new(ListingBounds {
            prefix: root.clone(),
            low: root.join("00000000000000000000")?,
            high: root.join("00000000000000000004")?,
        }));
        while let Some(entry) = pages.next().await? {
            channel.yield_item(entry?.location).await?;
        }
        Ok(())
    });
    let actual = connector
        .iterate_generator(generator)
        .collect::<DeltaResult<Vec<_>>>()
        .unwrap();

    assert_eq!(actual, expected);
}

#[test]
fn engine_connector_pages_backward_listing() {
    let temp_dir = tempdir().unwrap();
    let root = Url::from_directory_path(temp_dir.path()).unwrap();
    let expected: Vec<_> = (1..=3)
        .map(|version| root.join(&format!("{version:020}.json")).unwrap())
        .collect();
    let sync_engine = SyncEngine::new();
    let connector = EngineConnector::new(&sync_engine);

    let actual = run_coroutine_with_connector!(connector, async move |channel| {
        for version in 1..=3 {
            channel
                .write_bytes(
                    &root.join(&format!("{version:020}.json"))?,
                    Bytes::new(),
                    false,
                )
                .await?;
        }
        let mut pages = channel.paged(BackwardListing::new(ListingBounds {
            prefix: root.clone(),
            low: root.join("00000000000000000000")?,
            high: root.join("00000000000000000004")?,
        }));
        let mut files = Vec::new();
        while let Some(page) = pages.next().await? {
            assert!(page.known_version_boundary);
            for entry in page.entries {
                files.push(entry?.location);
            }
        }
        Ok(files)
    })
    .unwrap();

    assert_eq!(actual, expected);
}
