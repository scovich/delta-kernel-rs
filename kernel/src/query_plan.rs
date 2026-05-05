use url::Url;

/// A declarative query plan to be executed by an [`Engine`].
///
/// This API is experimental and intentionally minimal. Each variant defines its expected output
/// schema contract in the variant docs.
///
/// [`Engine`]: crate::Engine
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum QueryPlan {
    /// List files starting from `start_from` in the same directory, lexicographically ordered by
    /// path.
    ///
    /// The returned relation must contain three columns with these exact names and types:
    /// - `location`: STRING (non-null) - Fully-qualified URL string for the file.
    /// - `last_modified`: LONG (non-null) - Milliseconds since Unix epoch.
    /// - `size`: LONG (non-null) - File size in bytes.
    ListLogFiles { start_from: Url },
}
