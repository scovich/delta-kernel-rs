use std::borrow::Borrow;
use std::future::{poll_fn, Future as _};
use std::ops::Deref;
use std::pin::pin;
use std::sync::atomic::{AtomicBool, Ordering};
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

use super::generator::{
    generator, generator_fn, AsyncIntoIterator as _, BoundGenerator as _, Generator as _,
    GeneratorExt as _,
};
use super::kernel::engine::{drive_with_engine, EngineConnector};
use super::kernel::{
    workflow, Channel, ChannelExt as _, CoroutineStep, GeneratorBody, Request, StaticCoroutine,
};
use super::{static_coroutine, *};
use crate::engine::sync::SyncEngine;
use crate::metrics::MetricEvent;
use crate::unit_test_utils::{
    assert_result_error_with_message, install_thread_local_metrics_reporter, CapturingReporter,
};
use crate::{KernelError, KernelResult};

#[derive(Clone, Copy)]
enum ReplyOutcome {
    Ok,
    Err,
    Drop,
}

#[test]
fn static_workflow_and_coroutine_are_send() {
    fn assert_send<T: Send>(_: T) {}

    let workflow =
        crate::coroutine::static_workflow!(Request, |_channel| std::future::ready(Ok(())));
    let coroutine: crate::coroutine::StaticCoroutine<Request, ()> =
        static_coroutine!(Request, |_channel| std::future::ready(Ok(())));

    assert_send(workflow);
    assert_send(coroutine);
}

#[tokio::test(flavor = "multi_thread")]
async fn async_workflow_resumes_after_cross_task_reply() {
    let location = Url::parse("memory:///async-task").unwrap();
    let mut workflow = workflow!(|channel| channel.read_small_file(location, None));
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
            lengths.push(
                bytes?
                    .ok_or_else(|| KernelError::generic("empty read"))?
                    .len(),
            );
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
            .ok_or_else(|| KernelError::generic("empty read"))?;
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
fn workflow_try_advance_before_reply_would_block() {
    let ran = Arc::new(AtomicBool::new(false));
    let workflow_ran = Arc::clone(&ran);
    let location = Url::parse("memory:///async-task").unwrap();
    let mut workflow = workflow!(async move |channel| {
        workflow_ran.store(true, Ordering::Relaxed);
        channel.read_small_file(location, None).await
    });
    assert!(!ran.load(Ordering::Relaxed));
    let WorkflowStep::Request(Request::ReadSmallFile(_file, reply)) =
        workflow.try_advance().unwrap()
    else {
        panic!("workflow did not produce its request");
    };
    assert!(ran.load(Ordering::Relaxed));
    assert!(matches!(
        workflow.try_advance(),
        Err(KernelError::WouldBlock)
    ));
    reply.send(Ok(Some(Bytes::new())));
    assert!(matches!(
        workflow.try_advance().unwrap(),
        WorkflowStep::Done(_)
    ));
    assert_result_error_with_message(workflow.try_advance(), "advanced after completion");
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
        let mut workflow = workflow!(|channel| channel.read_small_file(location, None));
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
                reply.send(Err(KernelError::generic("connector failed the read")));
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
        .any(|event| matches!(event, MetricEvent::CrcReadSuccess(_)));
    let failure = events
        .iter()
        .any(|event| matches!(event, MetricEvent::CrcReadFailure));
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
fn reporting_span_marks_never_polled_workflow_abandoned() {
    let reporter = Arc::new(CapturingReporter::default());
    let _guard = install_thread_local_metrics_reporter(reporter.clone());
    {
        let span = info_span!("crc_read_completed", report = Empty);
        let _enter = span.enter();
        let location = Url::parse("memory:///toy").unwrap();
        let workflow = workflow!(|channel| channel.read_small_file(location, None));
        drop(workflow);
    }

    let events = reporter.events();
    assert!(
        events
            .iter()
            .any(|event| matches!(event, MetricEvent::CrcReadFailure)),
        "expected CrcReadFailure; got: {events:?}"
    );
    assert!(
        events
            .iter()
            .all(|event| !matches!(event, MetricEvent::CrcReadSuccess(_))),
        "did not expect CrcReadSuccess; got: {events:?}"
    );
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
fn parent_intercepts_child_items_while_child_io_reaches_connector() {
    async fn parent(channel: &Channel) -> KernelResult<Vec<Bytes>> {
        let first = Url::parse("memory:///first")?;
        let second = Url::parse("memory:///second")?;
        let mut child = generator!(channel, async move |channel| {
            for location in [first, second] {
                let item = channel
                    .read_small_file(location, None)
                    .await?
                    .ok_or_else(|| KernelError::generic("empty read"))?;
                channel.yield_item(item).await?;
            }
            Ok(())
        });
        let mut items = Vec::new();
        while let Some(item) = child.next().await? {
            items.push(item);
        }
        Ok(items)
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
        }
    };

    assert_eq!(connector_inputs, vec!["/first", "/second"]);
    assert_eq!(
        output,
        vec![
            Bytes::from_static(b"/first"),
            Bytes::from_static(b"/second")
        ]
    );
}

#[test]
fn generator_fn_erasure_preserves_requests_and_items() {
    let items = generator_fn!(async move |channel| {
        let bytes = channel
            .read_small_file(Url::parse("memory:///boxed")?, None)
            .await?
            .ok_or_else(|| KernelError::generic("empty read"))?;
        channel.yield_item(bytes.len()).await
    });
    let mut generator = Coroutine::from_generator(items);

    let CoroutineStep::Request(Request::ReadSmallFile(_, reply)) = generator.try_advance().unwrap()
    else {
        panic!("generator did not request a read");
    };
    reply.send(Ok(Some(Bytes::from_static(b"boxed"))));
    assert!(matches!(
        generator.try_advance().unwrap(),
        CoroutineStep::Yield(5)
    ));
    assert!(matches!(
        generator.try_advance().unwrap(),
        CoroutineStep::Done(())
    ));
    assert_result_error_with_message(generator.try_advance(), "advanced after completion");
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
    assert!(matches!(
        generator.try_advance().unwrap(),
        CoroutineStep::Yield(ref item) if item == "root item"
    ));
    assert!(matches!(
        generator.try_advance().unwrap(),
        CoroutineStep::Done(())
    ));

    async fn parent(channel: &Channel) -> KernelResult<Vec<String>> {
        let prefix = "scoped".to_string();
        let mut child = operation(channel, prefix.as_str());
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
fn static_coroutine_try_advance_before_request_reply_would_block() {
    let ran = Arc::new(AtomicBool::new(false));
    let generator_ran = Arc::clone(&ran);
    let location = Url::parse("memory:///item").unwrap();
    let mut generator: StaticCoroutine<()> = static_coroutine!(Request, async move |channel| {
        generator_ran.store(true, Ordering::Relaxed);
        channel.read_small_file(location, None).await?;
        Ok(())
    });
    assert!(!ran.load(Ordering::Relaxed));
    let CoroutineStep::Request(Request::ReadSmallFile(_file, reply)) =
        generator.try_advance().unwrap()
    else {
        panic!("generator did not request a read");
    };
    assert!(ran.load(Ordering::Relaxed));
    assert!(matches!(
        generator.try_advance(),
        Err(KernelError::WouldBlock)
    ));
    reply.send(Ok(Some(Bytes::new())));
    assert!(matches!(
        generator.try_advance().unwrap(),
        CoroutineStep::Done(())
    ));
}

#[test]
fn engine_connector_drives_real_storage_reads() {
    let temp_dir = tempdir().unwrap();
    let location = Url::from_file_path(temp_dir.path().join("data.bin")).unwrap();
    let expected = Bytes::from_static(b"kernel coroutine");
    std::fs::write(location.to_file_path().unwrap(), &expected).unwrap();

    let location = &location;
    let workflow = workflow!(|channel| channel.read_small_file(location.clone(), None));
    let actual = drive_with_engine(&SyncEngine::new(), None, workflow).unwrap();

    assert_eq!(actual, Some(expected));
}

#[test]
fn engine_result_iterator_drives_generator_requests() {
    let temp_dir = tempdir().unwrap();
    let location = Url::from_file_path(temp_dir.path().join("iter.bin")).unwrap();
    std::fs::write(location.to_file_path().unwrap(), b"item").unwrap();
    let items = generator_fn!(async move |channel| {
        let bytes = channel
            .read_small_file(location, None)
            .await?
            .ok_or_else(|| KernelError::generic("empty read"))?;
        channel.yield_item(Ok(bytes.len())).await?;
        channel
            .yield_item(Err(KernelError::generic("item failed")))
            .await
    });
    let mut iter = EngineConnector::new(&SyncEngine::new())
        .iterate_result_coroutine(Coroutine::from_generator(items));

    assert_eq!(iter.next().unwrap().unwrap(), 4);
    assert!(iter.next().unwrap().is_err());
    assert!(iter.next().is_none());
}

struct NumberPages;

impl PagedOperation for NumberPages {
    type Page = Vec<i32>;
}

#[derive(derive_more::From)]
struct PagingRequest(PageRequest<NumberPages>);

#[test]
fn pagination_keeps_empty_live_pages() {
    let wrong_type = Cursor::<NumberPages>::new(0_usize);
    assert!(wrong_type.into_inner::<String>().is_err());

    let mut workflow = crate::coroutine::workflow!(PagingRequest, async move |channel| {
        let cursor = channel.exchange(NumberPages, PageRequest::Prepare).await?;
        let mut pages = PageGenerator::Next(cursor).bind(channel);
        let first = pages.next().await?;
        let second = pages.next().await?;
        let done = pages.next().await?;
        Ok((first, second, done))
    });

    let output = loop {
        match workflow.try_advance().unwrap() {
            WorkflowStep::Done(output) => break output,
            WorkflowStep::Request(PagingRequest(PageRequest::Prepare(_, reply))) => {
                reply.send(Ok(Cursor::new(0_usize)));
            }
            WorkflowStep::Request(PagingRequest(PageRequest::Next(cursor, reply))) => {
                let page = match cursor.into_inner::<usize>().unwrap() {
                    0 => Page::new(Vec::new(), Some(Cursor::new(1_usize))),
                    1 => Page::new(vec![1, 2], None),
                    _ => panic!("unexpected page request"),
                };
                reply.send(Ok(page));
            }
        }
    };

    assert_eq!(output, (Some(Vec::new()), Some(vec![1, 2]), None));
}
