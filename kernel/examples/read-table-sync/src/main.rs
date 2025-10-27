// This example demonstrates truly synchronous usage of the delta kernel.
// It works only in sync mode and requires no async runtime (no tokio).
// For async examples that work in both modes, see read-table.
#[cfg(feature = "async")]
compile_error!(
    "This example only works in sync mode. Use read-table for async-compatible examples. \
     Build without --features async or use: cargo build --example read-table-sync"
);

use std::sync::Arc;

use arrow::compute::filter_record_batch;
use arrow::record_batch::RecordBatch;
use arrow::util::pretty::print_batches;
use common::{LocationArgs, ParseWithExamples, ScanArgs};
use delta_kernel::engine::arrow_data::ArrowEngineData;
use delta_kernel::{DeltaResult, Snapshot};

use clap::Parser;

/// A synchronous example program that dumps out the data of a delta table.
/// This demonstrates pure sync usage without any async/await machinery.
#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(flatten)]
    location_args: LocationArgs,

    #[command(flatten)]
    scan_args: ScanArgs,
}

fn main() -> DeltaResult<()> {
    env_logger::init();
    let cli = Cli::parse_with_examples(env!("CARGO_PKG_NAME"), "Read", "read", "");
    let url = delta_kernel::try_parse_uri(&cli.location_args.path)?;
    println!("Reading {url}");
    let engine = common::get_engine(&url, &cli.location_args)?;
    
    // Note: In sync mode, we don't use await_! - just call the methods directly
    let snapshot = Snapshot::builder_for(url).build(&engine)?;
    let Some(scan) = common::get_scan(snapshot, &cli.scan_args)? else {
        return Ok(());
    };

    // Execute the scan and collect batches synchronously
    // Note: In sync mode, we just call standard iterator methods
    let scan_data = scan.execute(Arc::new(engine))?;
    let batches: Vec<RecordBatch> = scan_data
        .map(|scan_result| -> DeltaResult<_> {
            // extract the batches and filter them if they have deletion vectors
            let scan_result = scan_result?;
            let mask = scan_result.full_mask();
            let data = scan_result.raw_data?;
            let record_batch: RecordBatch = data
                .into_any()
                .downcast::<ArrowEngineData>()
                .map_err(|_| delta_kernel::Error::EngineDataType("ArrowEngineData".to_string()))?
                .into();
            if let Some(mask) = mask {
                Ok(filter_record_batch(&record_batch, &mask.into())?)
            } else {
                Ok(record_batch)
            }
        })
        .scan(0usize, move |rows_so_far, record_batch| {
            // handle truncation if we've specified a limit
            let Ok(batch) = record_batch else {
                return Some(record_batch); // just forward the error
            };
            let batch_rows = batch.num_rows();
            let result = match cli.scan_args.limit {
                Some(limit) if *rows_so_far >= limit => return None, // over the limit, stop iteration
                Some(limit) => {
                    let batch = if *rows_so_far + batch_rows > limit {
                        common::truncate_batch(batch, limit - *rows_so_far)
                    } else {
                        batch
                    };
                    Ok(batch)
                }
                None => Ok(batch),
            };
            *rows_so_far += batch_rows;
            Some(result)
        })
        .collect::<Result<Vec<_>, _>>()?;
    
    let rows_so_far = batches.iter().map(|b| b.num_rows()).sum::<usize>();
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

