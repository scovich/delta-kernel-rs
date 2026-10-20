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

use super::engine::{drive_with_engine, run_workflow_with_connector, EngineConnector};
use super::generator::{
    empty, generator, map_async_witness, static_coroutine, AsyncIntoIterator as _,
    BoundGenerator as _, Generator as _, GeneratorExt as _, GeneratorStep,
};
use super::kernel::request::{BackwardListing, ForwardListing, ListingBounds};
use super::kernel::{generator_fn, workflow, Channel, CoroutineStep, StaticCoroutine};
use super::*;
use crate::engine::sync::SyncEngine;
use crate::metrics::MetricEvent;
use crate::unit_test_utils::{
    assert_result_error_with_message, install_thread_local_metrics_reporter, CapturingReporter,
};
use crate::{DeltaResult, Error};

/// Connector action after the toy workflow's single `ReadSmallFile` request.
#[derive(Clone, Copy)]
enum ReplyOutcome {
    Ok,
    Err,
    Drop,
}

#[test]
fn lazy_workflow_runs_only_after_the_caller_requests_a_step() {
    let ran = Arc::new(AtomicBool::new(false));
    let workflow_ran = Arc::clone(&ran);
    let mut workflow = workflow!(async move |_channel| {
        workflow_ran.store(true, Ordering::Relaxed);
        Ok(())
    });

    assert!(!ran.load(Ordering::Relaxed));
    assert!(matches!(
        workflow.try_advance().unwrap(),
        WorkflowStep::Done(())
    ));
    assert!(ran.load(Ordering::Relaxed));
}

#[test]
fn lazy_generator_runs_only_after_the_caller_starts_it() {
    let ran = Arc::new(AtomicBool::new(false));
    let generator_ran = Arc::clone(&ran);
    let mut generator = static_coroutine!(Request, async move |channel| {
        generator_ran.store(true, Ordering::Relaxed);
        channel.yield_item(()).await
    });

    assert!(!ran.load(Ordering::Relaxed));
    let CoroutineStep::Yield(()) = generator.try_advance().unwrap() else {
        panic!("generator did not yield");
    };
    assert!(ran.load(Ordering::Relaxed));
}

#[tokio::test(flavor = "multi_thread")]
async fn async_workflow_resumes_after_cross_task_reply() {
    let location = Url::parse("memory:///async-task").unwrap();
    let mut workflow =
        workflow!(async move |channel| channel.read_small_file(location, None).await);
    let WorkflowStep::Request(Request::ReadSmallFile(_file, reply)) =
        workflow.advance().await.unwrap()
    else {
        panic!("workflow did not produce its request");
    };
    let reply_task = tokio::spawn(async move {
        tokio::task::yield_now().await;
        reply.send(Ok(Some(Bytes::from_static(b"response"))));
    });

    let WorkflowStep::Done(output) = workflow.advance().await.unwrap() else {
        panic!("workflow did not complete");
    };
    reply_task.await.unwrap();
    assert_eq!(output, Some(Bytes::from_static(b"response")));
}

#[tokio::test]
async fn selectively_polled_channel_futures_keep_every_request_live() {
    let first = Url::parse("memory:///first").unwrap();
    let second = Url::parse("memory:///second").unwrap();
    let mut workflow = workflow!(async move |channel| {
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
        match workflow.advance().await.unwrap() {
            WorkflowStep::Done(lengths) => break lengths,
            WorkflowStep::Request(Request::ReadSmallFile(_file, reply)) => {
                requests += 1;
                reply.send(Ok(Some(Bytes::from(vec![0; requests]))));
            }
            WorkflowStep::Request(_) => panic!("workflow requested an unexpected operation"),
        }
    };

    lengths.sort_unstable();
    assert_eq!(requests, 2);
    assert_eq!(lengths, [1, 2]);
}

#[tokio::test(flavor = "multi_thread")]
async fn async_generator_resumes_after_cross_task_reply() {
    let location = Url::parse("memory:///async-generator").unwrap();
    let mut generator = static_coroutine!(Request, async move |channel| {
        let bytes = channel
            .read_small_file(location, None)
            .await?
            .ok_or_else(|| Error::generic("empty read"))?;
        channel.yield_item(bytes.len()).await
    });
    let CoroutineStep::Request(Request::ReadSmallFile(_file, reply)) =
        generator.advance().await.unwrap()
    else {
        panic!("generator did not produce its request");
    };
    let reply_task = tokio::spawn(async move {
        tokio::task::yield_now().await;
        reply.send(Ok(Some(Bytes::from_static(b"response"))));
    });

    let CoroutineStep::Yield(output) = generator.advance().await.unwrap() else {
        panic!("generator did not yield");
    };
    reply_task.await.unwrap();
    assert_eq!(output, 8);
}

#[test]
fn nested_async_operation_uses_the_parent_request_channel() {
    async fn child(channel: &Channel, location: Url) -> DeltaResult<Option<Bytes>> {
        channel.read_small_file(location, None).await
    }

    let location = Url::parse("memory:///nested").unwrap();
    let mut workflow = workflow!(async move |channel| child(channel, location).await);

    let WorkflowStep::Request(Request::ReadSmallFile(_file, reply)) =
        workflow.try_advance().unwrap()
    else {
        panic!("nested workflow did not expose its read request");
    };
    reply.send(Ok(Some(Bytes::from_static(b"response"))));
    let WorkflowStep::Done(bytes) = workflow.try_advance().unwrap() else {
        panic!("nested workflow did not complete");
    };
    assert_eq!(bytes, Some(Bytes::from_static(b"response")));
}

#[test]
fn workflow_retains_handle_between_advances() {
    let location = Url::parse("memory:///async-task").unwrap();
    let mut workflow =
        workflow!(async move |channel| { channel.read_small_file(location, None).await });

    let WorkflowStep::Request(Request::ReadSmallFile(_file, reply)) =
        workflow.try_advance().unwrap()
    else {
        panic!("workflow did not produce its request");
    };
    let bytes = Ok(Some(Bytes::from_static(b"response")));
    reply.send(bytes);
    let WorkflowStep::Done(output) = workflow.try_advance().unwrap() else {
        panic!("workflow did not complete");
    };
    assert_eq!(output, Some(Bytes::from_static(b"response")));
    assert_result_error_with_message(workflow.try_advance(), "advanced after completion");
}

#[test]
fn workflow_try_advance_before_reply_would_block() {
    let location = Url::parse("memory:///async-task").unwrap();
    let mut workflow =
        workflow!(async move |channel| { channel.read_small_file(location, None).await });
    let WorkflowStep::Request(Request::ReadSmallFile(_file, reply)) =
        workflow.try_advance().unwrap()
    else {
        panic!("workflow did not produce its request");
    };
    assert!(matches!(workflow.try_advance(), Err(Error::WouldBlock)));
    reply.send(Ok(Some(Bytes::new())));
    assert!(matches!(
        workflow.try_advance().unwrap(),
        WorkflowStep::Done(_)
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
        let mut workflow =
            workflow!(async move |channel| channel.read_small_file(location, None).await);
        let WorkflowStep::Request(Request::ReadSmallFile(_file, reply)) =
            workflow.try_advance().unwrap()
        else {
            panic!("toy workflow should suspend once on ReadSmallFile");
        };
        match outcome {
            ReplyOutcome::Ok => {
                reply.send(Ok(Some(Bytes::from_static(b"ok"))));
                let _ = workflow.try_advance().unwrap();
            }
            ReplyOutcome::Err => {
                reply.send(Err(Error::generic("connector failed the read")));
                let _ = workflow.try_advance();
            }
            ReplyOutcome::Drop => {
                drop(reply);
                drop(workflow);
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
fn workflow_output_is_independent_of_request_response_type() {
    let location = Url::parse("memory:///answer").unwrap();
    let expected_location = location.clone();
    let mut workflow = workflow!(async move |channel| {
        let bytes = channel
            .read_small_file(location, None)
            .await?
            .ok_or_else(|| Error::generic("empty read"))?;
        Ok(format!("read {} bytes", bytes.len()))
    });
    let output = loop {
        match workflow.try_advance().unwrap() {
            WorkflowStep::Done(output) => break output,
            WorkflowStep::Request(Request::ReadSmallFile((location, range), reply)) => {
                assert_eq!(location, expected_location);
                assert_eq!(range, None);
                reply.send(Ok(Some(Bytes::from_static(b"answer"))));
            }
            WorkflowStep::Request(_) => {
                panic!("workflow requested an unexpected operation")
            }
        }
    };

    assert_eq!(output, "read 6 bytes");
}

#[test]
fn connector_facing_generator_interleaves_requests_and_yields() {
    let location = Url::parse("memory:///item").unwrap();
    let mut generator = static_coroutine!(Request, async move |channel| {
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
            CoroutineStep::Done(()) => break,
            CoroutineStep::Yield(item) => yielded.push(item),
            CoroutineStep::Request(Request::ReadSmallFile(_file, reply)) => {
                reply.send(Ok(Some(Bytes::from_static(b"generated item"))));
            }
            CoroutineStep::Request(_) => {
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
    let mut generator = static_coroutine!(Request, async move |channel| {
        let (first, second) = join(channel.yield_item(1), channel.yield_item(2)).await;
        first?;
        second?;
        generator_resumed.store(true, Ordering::Relaxed);
        Ok(3)
    });

    assert!(matches!(
        generator.try_advance().unwrap(),
        CoroutineStep::Yield(1)
    ));
    assert!(!resumed.load(Ordering::Relaxed));
    assert!(matches!(
        generator.try_advance().unwrap(),
        CoroutineStep::Yield(2)
    ));
    assert!(!resumed.load(Ordering::Relaxed));
    assert!(matches!(
        generator.try_advance().unwrap(),
        CoroutineStep::Done(3)
    ));
    assert!(resumed.load(Ordering::Relaxed));
}

#[test]
fn dropped_yield_future_remains_committed_before_terminal_output() {
    let mut generator = static_coroutine!(Request, async move |channel| {
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
        CoroutineStep::Yield("item")
    ));
    assert!(matches!(
        generator.try_advance().unwrap(),
        CoroutineStep::Done(42)
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
    let mut generator = static_coroutine!(Request, async move |channel| {
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

    let CoroutineStep::Yield(first) = generator.try_advance().unwrap() else {
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
    let mut workflow = workflow!(async move |channel| {
        let cursor = channel.prepare_paged(ForwardListing::new(bounds)).await?;
        let page = channel.next_page(cursor).await?;
        assert!(page.next.is_none());
        Ok(page.data.len())
    });

    let output = loop {
        match workflow.try_advance().unwrap() {
            WorkflowStep::Done(output) => break output,
            WorkflowStep::Request(Request::ListForward(PageRequest::Prepare(
                _operation,
                reply,
            ))) => {
                reply.send(Ok(Cursor::new(7_i64)));
            }
            WorkflowStep::Request(Request::ListForward(PageRequest::Next(cursor, reply))) => {
                assert_eq!(cursor.into_inner::<i64>().unwrap(), 7);
                reply.send(Ok(Page::new(Vec::new(), None)));
            }
            WorkflowStep::Request(_) => {
                panic!("workflow requested an unexpected operation")
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
    let mut workflow = workflow!(parent);
    let output = loop {
        match workflow.try_advance().unwrap() {
            WorkflowStep::Done(output) => break output,
            WorkflowStep::Request(Request::ReadSmallFile((location, range), reply)) => {
                assert_eq!(range, None);
                connector_inputs.push(location.path().to_string());
                reply.send(Ok(Some(Bytes::copy_from_slice(location.path().as_bytes()))));
            }
            WorkflowStep::Request(_) => {
                panic!("workflow requested an unexpected operation")
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
fn into_generator_yields_each_item_then_exhausts() {
    let items = [1, 2, 3].into_generator();
    let mut workflow = workflow!(async move |channel| {
        let mut items = items.bind(channel);
        let mut output = Vec::new();
        while let Some(item) = items.next().await? {
            output.push(item);
        }
        Ok(output)
    });
    let WorkflowStep::Done(output) = workflow.try_advance().unwrap() else {
        panic!("expected completion");
    };
    assert_eq!(output, [1, 2, 3]);
}

#[test]
fn flatten_yields_collection_elements_then_exhausts() {
    let items = [vec![1, 2], vec![], vec![3]].into_generator();
    let mut workflow = workflow!(async move |channel| {
        let mut items = items.flatten().bind(channel);
        let mut output = Vec::new();
        while let Some(item) = items.next().await? {
            output.push(item);
        }
        Ok(output)
    });
    let WorkflowStep::Done(output) = workflow.try_advance().unwrap() else {
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

    let mut workflow = workflow!(async move |channel| {
        let mut items = items.bind(channel);
        let mut output = Vec::new();
        while let Some(item) = items.next().await? {
            output.push(item);
        }
        Ok(output)
    });
    let output = loop {
        match workflow.try_advance().unwrap() {
            WorkflowStep::Done(output) => break output,
            WorkflowStep::Request(Request::ReadSmallFile(_file, reply)) => {
                reply.send(Ok(Some(Bytes::from_static(b"12345"))));
            }
            WorkflowStep::Request(_) => {
                panic!("workflow requested an unexpected operation")
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

    let mut workflow = workflow!(async move |channel| {
        let mut items = items.bind(channel);
        let first = items.next().await?;
        assert_eq!(first, Some(10));
        items.next().await
    });

    assert_result_error_with_message(workflow.try_advance(), "generator failed");
}

#[test]
fn on_complete_runs_only_after_generator_completion() {
    let completed = Arc::new(AtomicBool::new(false));
    let callback_completed = Arc::clone(&completed);
    let items = generator_fn!(async move |channel| {
        channel.yield_item(1).await?;
        Ok(42)
    })
    .on_complete(move || callback_completed.store(true, Ordering::Relaxed));
    let mut generator: StaticCoroutine<i32, i32> =
        static_coroutine!(Request, async move |channel| {
            channel.yield_from(items.bind(&channel)).await
        });

    assert!(matches!(
        generator.try_advance().unwrap(),
        CoroutineStep::Yield(1)
    ));
    assert!(!completed.load(Ordering::Relaxed));
    assert!(matches!(
        generator.try_advance().unwrap(),
        CoroutineStep::Done(42)
    ));
    assert!(completed.load(Ordering::Relaxed));
}

#[rstest]
#[case(Some(vec![1, 2, 3]), &[1, 2, 3])]
#[case(None, &[])]
fn flatten_async_yields_inner_generator_items(
    #[case] input: Option<Vec<i32>>,
    #[case] expected: &[i32],
) {
    let inner = input.map(|items| items.into_generator());
    let items = inner.into_generator().flatten_async();
    let mut workflow =
        workflow!(async move |channel| { items.bind(channel).collect::<Vec<_>>().await });
    let WorkflowStep::Done(items) = workflow.try_advance().unwrap() else {
        panic!("workflow did not complete");
    };
    assert_eq!(items, expected);
}

#[test]
fn flatten_async_preserves_outer_output() {
    let outer = generator_fn!(async move |channel| -> DeltaResult<i32> {
        channel.yield_item(vec![1, 2].into_generator()).await?;
        channel.yield_item(vec![3].into_generator()).await?;
        Ok(42)
    });
    let items = outer.flatten_async();
    let mut generator: StaticCoroutine<i32, i32> =
        static_coroutine!(Request, async move |channel| {
            channel.yield_from(items.bind(&channel)).await
        });

    for expected in [1, 2, 3] {
        let CoroutineStep::Yield(item) = generator.try_advance().unwrap() else {
            panic!("expected item");
        };
        assert_eq!(item, expected);
    }
    let CoroutineStep::Done(output) = generator.try_advance().unwrap() else {
        panic!("expected completion");
    };
    assert_eq!(output, 42);
}

#[test]
fn filter_ok_retains_matches_and_passes_through_item_errors() {
    let items = vec![Ok(1), Ok(2), Err(Error::generic("item failed")), Ok(4)];
    let items = items.into_generator().filter_ok(|item: &i32| item % 2 == 0);
    let mut workflow = workflow!(async move |channel| {
        let mut items = items.bind(channel);
        assert_eq!(items.next().await?.transpose()?, Some(2));
        let error = items.next().await?.unwrap().unwrap_err();
        assert!(error.to_string().contains("item failed"));
        assert_eq!(items.next().await?.transpose()?, Some(4));
        assert!(items.next().await?.is_none());
        Ok(())
    });

    assert!(matches!(
        workflow.try_advance().unwrap(),
        WorkflowStep::Done(())
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
    let items = first
        .into_generator()
        .chain(tail)
        .map(|item: DeltaResult<i32>| item.map(|value| value + 1))
        .map_ok(|value| value * 2);
    let items = items.map_async(map_async_witness, async move |_channel, item| {
        item.map(|value| value + 1)
    });
    let items = items.filter_ok(|value: &i32| *value >= 5);
    let mut generator: StaticCoroutine<DeltaResult<i32>, i32> =
        static_coroutine!(Request, async move |channel| {
            channel.yield_from(items.bind(&channel).peekable()).await
        });

    let CoroutineStep::Yield(first) = generator.try_advance().unwrap() else {
        panic!("expected first item");
    };
    assert_eq!(first.unwrap(), 5);
    let CoroutineStep::Yield(second) = generator.try_advance().unwrap() else {
        panic!("expected second item");
    };
    assert_eq!(second.unwrap(), 7);
    let CoroutineStep::Done(output) = generator.try_advance().unwrap() else {
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
    let mut generator: StaticCoroutine<DeltaResult<i32>, i32> =
        static_coroutine!(Request, async move |channel| {
            channel.yield_from(items.bind(&channel)).await
        });

    let CoroutineStep::Yield(first) = generator.try_advance().unwrap() else {
        panic!("expected first item");
    };
    assert_eq!(first.unwrap(), 1);
    let CoroutineStep::Yield(second) = generator.try_advance().unwrap() else {
        panic!("expected second item");
    };
    assert_eq!(second.unwrap(), 2);
    let CoroutineStep::Yield(error) = generator.try_advance().unwrap() else {
        panic!("expected outer item error");
    };
    assert!(error.unwrap_err().to_string().contains("outer item failed"));
    let CoroutineStep::Done(output) = generator.try_advance().unwrap() else {
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
    let mut workflow = workflow!(async move |channel| {
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

    let WorkflowStep::Done(output) = workflow.try_advance().unwrap() else {
        panic!("expected completion");
    };
    assert_eq!(output, 42);
}

#[test]
fn try_collect_collects_successes_and_short_circuits_item_errors() {
    let items: Vec<DeltaResult<_>> = vec![Ok(1), Ok(2)];
    let mut workflow = workflow!(async move |channel| {
        let collected: Vec<_> = items.into_generator().bind(channel).try_collect().await?;
        Ok(collected)
    });
    let WorkflowStep::Done(collected) = workflow.try_advance().unwrap() else {
        panic!("workflow did not complete");
    };
    assert_eq!(collected, [1, 2]);

    let items = vec![Ok(1), Err(Error::generic("item failed")), Ok(3)];
    let mut workflow = workflow!(async move |channel| {
        let collected: Vec<_> = items.into_generator().bind(channel).try_collect().await?;
        Ok(collected)
    });
    assert_result_error_with_message(workflow.try_advance(), "item failed");
}

#[test]
fn channel_taking_generator_supports_static_and_borrowed_state() {
    fn operation<'a>(
        channel: impl Deref<Target = Channel> + Send + 'a,
        prefix: impl Borrow<str> + Send + 'a,
    ) -> GeneratorBody<'a, String> {
        generator!(channel, async move |channel| {
            channel
                .yield_item(format!("{} item", prefix.borrow()))
                .await?;
            assert!(!prefix.borrow().is_empty());
            Ok(())
        })
    }

    let mut generator =
        StaticCoroutine::new(move |channel| operation(channel, String::from("root")));
    let CoroutineStep::Yield(item) = generator.try_advance().unwrap() else {
        panic!("static generator did not yield");
    };
    assert_eq!(item, "root item");
    let CoroutineStep::Done(()) = generator.try_advance().unwrap() else {
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

    let mut workflow = workflow!(parent);
    let WorkflowStep::Done(items) = workflow.try_advance().unwrap() else {
        panic!("parent workflow did not complete");
    };
    assert_eq!(items, ["scoped item"]);
}

#[test]
fn channel_taking_generator_can_yield_borrowed_items() {
    fn borrowed<'a>(channel: &'a Channel, value: &'a str) -> GeneratorBody<'a, &'a str> {
        generator!(channel, async move |channel| {
            channel.yield_item(value).await
        })
    }

    async fn parent(channel: &Channel) -> DeltaResult<()> {
        let value = String::from("borrowed");
        let mut child = borrowed(channel, &value);
        assert_eq!(child.next().await?, Some("borrowed"));
        assert_eq!(child.next().await?, None);
        Ok(())
    }

    let mut workflow = workflow!(parent);
    assert!(matches!(
        workflow.try_advance().unwrap(),
        WorkflowStep::Done(())
    ));
}

#[test]
fn static_coroutine_returns_terminal_output() {
    let mut generator: StaticCoroutine<&'static str, usize> =
        static_coroutine!(Request, async move |channel| {
            channel.yield_item("item").await?;
            Ok(42)
        });
    assert!(matches!(
        generator.try_advance().unwrap(),
        CoroutineStep::Yield("item")
    ));
    assert!(matches!(
        generator.try_advance().unwrap(),
        CoroutineStep::Done(42)
    ));
}

#[test]
fn static_coroutine_try_advance_before_request_reply_would_block() {
    let location = Url::parse("memory:///item").unwrap();
    let mut generator: StaticCoroutine<()> = static_coroutine!(Request, async move |channel| {
        channel.read_small_file(location, None).await?;
        Ok(())
    });
    let CoroutineStep::Request(Request::ReadSmallFile(_file, reply)) =
        generator.try_advance().unwrap()
    else {
        panic!("generator did not request a read");
    };
    assert!(matches!(generator.try_advance(), Err(Error::WouldBlock)));
    reply.send(Ok(Some(Bytes::new())));
    assert!(matches!(
        generator.try_advance().unwrap(),
        CoroutineStep::Done(())
    ));
}

#[test]
fn engine_workflow_can_borrow_caller_state() {
    let engine = SyncEngine::new();
    let value = "scoped".to_string();
    let value = value.as_str();
    let workflow = workflow!(async move |_channel| Ok::<_, Error>(value.len()));
    let length = drive_with_engine(&engine, None, workflow).unwrap();

    assert_eq!(length, 6);
}

#[test]
fn try_advance_without_connector_work_would_block() {
    let mut workflow = workflow!(async |_channel| {
        pending::<()>().await;
        Ok(())
    });
    assert!(matches!(workflow.try_advance(), Err(Error::WouldBlock)));
}

#[test]
fn engine_connector_drives_real_storage_operations() {
    let temp_dir = tempdir().unwrap();
    let location = Url::from_file_path(temp_dir.path().join("data.bin")).unwrap();
    let expected = Bytes::from_static(b"kernel coroutine");
    let write_data = expected.clone();

    let workflow = workflow!(async move |channel| {
        channel.write_bytes(&location, write_data, false).await?;
        channel.read_small_file(location, None).await
    });
    let connector = EngineConnector::new(&SyncEngine::new());
    let actual = connector.drive(workflow).unwrap();
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

    let generator = static_coroutine!(Request, async move |channel| {
        for version in 1..=3 {
            channel
                .write_bytes(
                    &root.join(&format!("{version:020}.json"))?,
                    Bytes::new(),
                    false,
                )
                .await?;
        }
        let cursor = channel
            .prepare_paged(ForwardListing::new(ListingBounds {
                prefix: root.clone(),
                low: root.join("00000000000000000000")?,
                high: root.join("00000000000000000004")?,
            }))
            .await?;
        let mut pages = PageGenerator::Next(cursor).flatten().bind(&channel);
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

    let actual = run_workflow_with_connector!(connector, async move |channel| {
        for version in 1..=3 {
            channel
                .write_bytes(
                    &root.join(&format!("{version:020}.json"))?,
                    Bytes::new(),
                    false,
                )
                .await?;
        }
        let cursor = channel
            .prepare_paged(BackwardListing::new(ListingBounds {
                prefix: root.clone(),
                low: root.join("00000000000000000000")?,
                high: root.join("00000000000000000004")?,
            }))
            .await?;
        let mut pages = PageGenerator::Next(cursor).bind(channel);
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
