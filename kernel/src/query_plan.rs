use url::Url;

use crate::{FileMeta, Scalar, SchemaRef};

/// A declarative query plan to be executed by an [`Engine`].
///
/// This API is experimental and intentionally minimal. Each variant defines its expected output
/// schema contract in the variant docs.
///
/// [`Engine`]: crate::Engine
#[derive(Debug, Clone, PartialEq)]
pub enum QueryPlan {
    /// List files starting from `start_from` in the same directory, lexicographically ordered by
    /// path.
    ///
    /// The returned relation must contain three columns with these exact names and types:
    /// - `location`: STRING (non-null) - Fully-qualified URL string for the file.
    /// - `last_modified`: LONG (non-null) - Milliseconds since Unix epoch.
    /// - `size`: LONG (non-null) - File size in bytes.
    ListFiles { start_from: Url },

    /// Scan JSON files and return rows projected to `physical_schema`.
    ///
    /// `metadata_columns` defines additional top-level output columns. For each file, the matching
    /// `metadata_values` are constant across all rows produced from that file.
    ScanJson {
        files: Vec<(FileMeta, Vec<Scalar>)>,
        metadata_columns: Vec<String>,
        physical_schema: SchemaRef,
    },

    /// Scan Parquet files and return rows projected to `physical_schema`.
    ///
    /// `metadata_columns` defines additional top-level output columns. For each file, the matching
    /// `metadata_values` are constant across all rows produced from that file.
    ScanParquet {
        files: Vec<(FileMeta, Vec<Scalar>)>,
        metadata_columns: Vec<String>,
        physical_schema: SchemaRef,
    },

    /// Concatenate inputs without deduplication.
    ///
    /// All inputs must have identical output schemas. This node does not impose any row-ordering
    /// guarantees.
    UnionAll { inputs: Vec<QueryPlan> },

    /// Global aggregate over all input rows.
    ///
    /// For each `c` in `value_columns`, output:
    /// `max_by(c, version_column) FILTER (WHERE c IS NOT NULL)`.
    ///
    /// The output always has exactly one row with columns in `value_columns` order. If no row
    /// satisfies `c IS NOT NULL` (including empty input), that output value is `NULL`.
    LatestNonNullByVersion {
        input: Box<QueryPlan>,
        version_column: String,
        value_columns: Vec<String>,
    },
}

impl QueryPlan {
    /// Builds Some [`QueryPlan::ScanJson`] from non-empty `files`, or None.
    pub fn scan_json(
        files: Vec<(FileMeta, Vec<Scalar>)>,
        metadata_columns: Vec<String>,
        physical_schema: SchemaRef,
    ) -> Option<QueryPlan> {
        (!files.is_empty()).then_some(QueryPlan::ScanJson {
            files,
            metadata_columns,
            physical_schema,
        })
    }

    /// Builds Some [`QueryPlan::ScanParquet`] from non-empty `files`, or None.
    pub fn scan_parquet(
        files: Vec<(FileMeta, Vec<Scalar>)>,
        metadata_columns: Vec<String>,
        physical_schema: SchemaRef,
    ) -> Option<QueryPlan> {
        (!files.is_empty()).then_some(QueryPlan::ScanParquet {
            files,
            metadata_columns,
            physical_schema,
        })
    }

    /// Build a union-all node from `inputs`.
    ///
    /// Returns:
    /// - Some [`QueryPlan::UnionAll`] when there are two or more inputs
    /// - Some unmodified input, when there is exactly one input
    /// - `None` otherwise
    pub fn union_all(inputs: impl IntoIterator<Item = QueryPlan>) -> Option<QueryPlan> {
        let mut inputs: Vec<QueryPlan> = inputs.into_iter().collect();
        if inputs.len() > 1 {
            Some(QueryPlan::UnionAll { inputs })
        } else {
            // Either Some(single_input) or None
            inputs.pop()
        }
    }
}
