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
    ListLogFiles { start_from: Url },

    /// Scan JSON files and return rows projected to `physical_schema`.
    ///
    /// The engine must preserve file order (as provided in `files`) and row order within each
    /// file.
    ///
    /// `metadata_columns` defines additional top-level output columns. For each file, the matching
    /// `metadata_values` are constant across all rows produced from that file.
    ScanJson {
        files: Vec<(FileMeta, Vec<Scalar>)>,
        metadata_columns: Vec<String>,
        physical_schema: SchemaRef,
    },

    /// Aggregate input rows to produce the latest non-null values for the requested columns by
    /// `version`.
    ///
    /// Semantics are equivalent to, for each `c` in `value_columns`:
    /// `max_by(c, version_column) FILTER (WHERE c IS NOT NULL)`.
    ///
    /// Output is a single row containing the columns in `value_columns` order, with the same types
    /// as input.
    LatestNonNullByVersion {
        input: Box<QueryPlan>,
        version_column: String,
        value_columns: Vec<String>,
    },
}
