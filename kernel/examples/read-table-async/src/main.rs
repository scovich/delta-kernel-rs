// This example demonstrates truly asynchronous usage of the delta kernel.
// It requires async mode and uses native async/await with streams.
// For examples that work in both modes, see read-table.
#[cfg(not(feature = "async"))]
compile_error!(
    "This example only works in async mode. Use read-table for mode-agnostic examples. \
     Build with: cargo build --package read-table-async"
);

use std::sync::Arc;

use arrow::compute::filter_record_batch;
use arrow::record_batch::RecordBatch;
use arrow::util::pretty::print_batches;
use common::{LocationArgs, ParseWithExamples, ScanArgs};
use delta_kernel::engine::arrow_data::ArrowEngineData;
use delta_kernel::{DeltaResult, Snapshot};
use futures::StreamExt;

use clap::Parser;

/// An asynchronous example program that dumps out the data of a delta table.
/// This demonstrates pure async usage with native streams and .await.
#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(flatten)]
    location_args: LocationArgs,

    #[command(flatten)]
    scan_args: ScanArgs,
}

#[tokio::main]
async fn main() -> DeltaResult<()> {
    env_logger::init();
    let cli = Cli::parse_with_examples(env!("CARGO_PKG_NAME"), "Read", "read", "");
    let url = delta_kernel::try_parse_uri(&cli.location_args.path)?;
    println!("Reading {url}");
    let engine = common::get_engine(&url, &cli.location_args)?;
    
    // Note: In async mode, we use .await directly - no await_! macro needed
    let snapshot = Snapshot::builder_for(url).build(&engine).await?;
    let Some(scan) = common::get_scan(snapshot, &cli.scan_args)? else {
        return Ok(());
    };

    // Execute the scan and collect batches asynchronously using native streams
    let scan_stream = scan.execute(Arc::new(engine)).await?;
    
    let mut batches = Vec::new();
    let mut rows_so_far = 0usize;
    
    // Pin the stream for async iteration
    tokio::pin!(scan_stream);
    
    // Process stream using native async iteration
    while let Some(scan_result) = scan_stream.next().await {
        let scan_result = scan_result?;
        let mask = scan_result.full_mask();
        let data = scan_result.raw_data?;
        let record_batch: RecordBatch = data
            .into_any()
            .downcast::<ArrowEngineData>()
            .map_err(|_| delta_kernel::Error::EngineDataType("ArrowEngineData".to_string()))?
            .into();
        
        // Apply deletion vector mask if present
        let batch = if let Some(mask) = mask {
            filter_record_batch(&record_batch, &mask.into())?
        } else {
            record_batch
        };
        
        // Handle limit truncation
        let batch_rows = batch.num_rows();
        match cli.scan_args.limit {
            Some(limit) if rows_so_far >= limit => break, // over the limit, stop iteration
            Some(limit) => {
                let batch = if rows_so_far + batch_rows > limit {
                    common::truncate_batch(batch, limit - rows_so_far)
                } else {
                    batch
                };
                rows_so_far += batch_rows;
                batches.push(batch);
            }
            None => {
                rows_so_far += batch_rows;
                batches.push(batch);
            }
        }
    }
    
    if let Some(limit) = cli.scan_args.limit {
        if limit >= rows_so_far {
            println!("Printing all {rows_so_far} rows.");
        } else {
            println!("Printing first {limit} rows of at least {rows_so_far} total rows.");
        }
    }
    print_batches(&batches)?;
    Ok(())
}

