//! Read a small table with/without deletion vectors.
//! Must run at the root of the crate
use std::path::PathBuf;

use delta_kernel::engine::default::DefaultEngine;
use delta_kernel::scan::ScanResult;
use delta_kernel::{async_fn, await_, AsyncIterator, DeltaResult, Snapshot};
use test_utils::DefaultEngineExtension;

#[async_fn]
fn count_total_scan_rows(
    scan_result_iter: impl AsyncIterator<Item = DeltaResult<ScanResult>>,
) -> DeltaResult<usize> {
    await_!(scan_result_iter
        .async_map(|scan_result| {
            let scan_result = scan_result?;
            // NOTE: The mask only suppresses rows for which it is both present and false.
            let mask = scan_result.raw_mask();
            let deleted_rows = mask.into_iter().flatten().filter(|&&m| !m).count();
            let data = scan_result.raw_data?;
            Ok(data.len() - deleted_rows)
        })
        .async_pin()
        .async_try_fold(0, |acc, count| Ok(acc + count)))
}

#[async_fn]
#[cfg_attr(not(feature = "async"), test_log::test)]
#[cfg_attr(feature = "async", test_log::test(tokio::test))]
fn dv_table() -> Result<(), Box<dyn std::error::Error>> {
    let path = std::fs::canonicalize(PathBuf::from("./tests/data/table-with-dv-small/"))?;
    let url = url::Url::from_directory_path(path).unwrap();
    let engine = DefaultEngine::new_local();

    let snapshot = await_!(Snapshot::builder_for(url).build(engine.as_ref()))?;
    let scan = snapshot.scan_builder().build()?;

    let stream = await_!(scan.execute(engine))?;
    let total_rows = await_!(count_total_scan_rows(stream))?;
    assert_eq!(total_rows, 8);
    Ok(())
}

#[async_fn]
#[cfg_attr(not(feature = "async"), test_log::test)]
#[cfg_attr(feature = "async", test_log::test(tokio::test))]
fn non_dv_table() -> Result<(), Box<dyn std::error::Error>> {
    let path = std::fs::canonicalize(PathBuf::from("./tests/data/table-without-dv-small/"))?;
    let url = url::Url::from_directory_path(path).unwrap();
    let engine = DefaultEngine::new_local();

    let snapshot = await_!(Snapshot::builder_for(url).build(engine.as_ref()))?;
    let scan = snapshot.scan_builder().build()?;

    let stream = await_!(scan.execute(engine))?;
    let total_rows = await_!(count_total_scan_rows(stream))?;
    assert_eq!(total_rows, 10);
    Ok(())
}
