//! Connector operations for writing storage objects.

use bytes::Bytes;
use url::Url;

use super::Operation;

/// Write bytes to a storage URL.
pub struct WriteBytes {
    /// Destination URL.
    pub url: Url,
    /// Bytes to write.
    pub data: Bytes,
    /// Whether an existing object may be overwritten.
    pub overwrite: bool,
}

impl Operation for WriteBytes {
    type Response = ();
}
