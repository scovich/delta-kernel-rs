use std::collections::HashSet;

use itertools::Itertools;
use serde::{Deserialize, Serialize};
use strum::{
    AsRefStr, Display as StrumDisplay, EnumCount, EnumIter, EnumString, IntoEnumIterator as _,
};

use crate::actions::Protocol;
use crate::expressions::Scalar;
use crate::schema::derive_macro_utils::ToDataType;
use crate::schema::DataType;
use crate::table_properties::TableProperties;
use crate::utils::require;
use crate::{DeltaResult, Error};
use delta_kernel_derive::internal_api;

pub(crate) use column_mapping::column_mapping_presence;
pub use column_mapping::ColumnMappingMode;
pub(crate) use timestamp_ntz::schema_uses_timestamp_ntz;
mod column_mapping;
#[cfg(test)]
mod feature_tests;
mod timestamp_ntz;

use crate::schema::Schema;

/// Maximum reader protocol version that the kernel can handle.
pub const MAX_VALID_READER_VERSION: i32 = 3;

/// Maximum writer protocol version that the kernel can handle.
pub const MAX_VALID_WRITER_VERSION: i32 = 7;

/// Minimum reader version for tables that use table features.
/// When set to 3, the protocol requires an explicit `readerFeatures` array.
pub const TABLE_FEATURES_MIN_READER_VERSION: i32 = 3;

/// Minimum writer version for tables that use table features.
/// When set to 7, the protocol requires an explicit `writerFeatures` array.
pub const TABLE_FEATURES_MIN_WRITER_VERSION: i32 = 7;

/// Prefix for table feature override properties.
/// Properties with this prefix (e.g., `delta.feature.deletionVectors`) are used to
/// explicitly turn on support for the feature in the protocol.
pub const SET_TABLE_FEATURE_SUPPORTED_PREFIX: &str = "delta.feature.";

/// Value to add support for a table feature when used with [`SET_TABLE_FEATURE_SUPPORTED_PREFIX`].
/// Example: `"delta.feature.deletionVectors" -> "supported"`
pub const SET_TABLE_FEATURE_SUPPORTED_VALUE: &str = "supported";

/// Table features represent protocol capabilities required to correctly read or write a given table.
/// - Readers must implement all features required for correct table reads.
/// - Writers must implement all features required for correct table writes.
///
/// Each variant corresponds to one such feature. A feature is either:
/// - **ReaderWriter** (must be supported by both readers and writers), or
/// - **Writer only** (applies only to writers).
/// There are no Reader only features. See `TableFeature::feature_type` for the category of each.
///
/// The kernel currently supports all reader features except `V2Checkpoint`.
#[derive(
    Serialize,
    Deserialize,
    Debug,
    Clone,
    Eq,
    PartialEq,
    EnumString,
    StrumDisplay,
    AsRefStr,
    EnumCount,
    Hash,
)]
#[strum(
    serialize_all = "camelCase",
    parse_err_fn = xxx__not_needed__default_variant_means_parsing_is_infallible__xxx,
    parse_err_ty = Infallible // ignored, sadly: https://github.com/Peternator7/strum/issues/430
)]
#[serde(rename_all = "camelCase")]
#[internal_api]
#[derive(EnumIter)]
// ^^ We must derive EnumIter only after internal_api adjusts visibility. Otherwise, internal-api
// builds will fail because the now-public `TableFeature::iter()` returns a pub(crate) type.
pub(crate) enum TableFeature {
    //////////////////////////
    // Writer-only features //
    //////////////////////////
    /// Append Only Tables
    AppendOnly,
    /// Table invariants
    Invariants,
    /// Check constraints on columns
    CheckConstraints,
    /// CDF on a table
    ChangeDataFeed,
    /// Columns with generated values
    GeneratedColumns,
    /// ID Columns
    IdentityColumns,
    /// Monotonically increasing timestamps in the CommitInfo
    InCommitTimestamp,
    /// Row tracking on tables
    RowTracking,
    /// domain specific metadata
    DomainMetadata,
    /// Iceberg compatibility support
    IcebergCompatV1,
    /// Iceberg compatibility support
    IcebergCompatV2,
    /// The Clustered Table feature facilitates the physical clustering of rows
    /// that share similar values on a predefined set of clustering columns.
    #[strum(serialize = "clustering")]
    #[serde(rename = "clustering")]
    ClusteredTable,
    /// Materialize partition columns in parquet data files.
    MaterializePartitionColumns,

    ///////////////////////////
    // ReaderWriter features //
    ///////////////////////////
    /// CatalogManaged tables:
    /// <https://github.com/delta-io/delta/blob/master/protocol_rfcs/catalog-managed.md>
    CatalogManaged,
    #[strum(serialize = "catalogOwned-preview")]
    #[serde(rename = "catalogOwned-preview")]
    CatalogOwnedPreview,
    /// Mapping of one column to another
    ColumnMapping,
    /// Deletion vectors for merge, update, delete
    DeletionVectors,
    /// timestamps without timezone support
    #[strum(serialize = "timestampNtz")]
    #[serde(rename = "timestampNtz")]
    TimestampWithoutTimezone,
    // Allow columns to change type
    TypeWidening,
    #[strum(serialize = "typeWidening-preview")]
    #[serde(rename = "typeWidening-preview")]
    TypeWideningPreview,
    /// version 2 of checkpointing
    V2Checkpoint,
    /// vacuumProtocolCheck ReaderWriter feature ensures consistent application of reader and writer
    /// protocol checks during VACUUM operations
    VacuumProtocolCheck,
    /// This feature enables support for the variant data type, which stores semi-structured data.
    VariantType,
    #[strum(serialize = "variantType-preview")]
    #[serde(rename = "variantType-preview")]
    VariantTypePreview,
    #[strum(serialize = "variantShredding-preview")]
    #[serde(rename = "variantShredding-preview")]
    VariantShreddingPreview,

    #[serde(untagged)]
    #[strum(default)]
    Unknown(String),
}

/// Classifies table features by their type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum FeatureType {
    /// Feature only affects write operations
    WriterOnly,
    /// Feature affects both read and write operations (must appear in both feature lists)
    ReaderWriter,
    /// Unknown feature type (for forward compatibility)
    Unknown,
}

/// Build a single effective feature list from protocol + schema + properties.
///
/// This is the **narrow waist** for feature validation: it builds the canonical list of
/// supported features AND validates consistency between protocol and metadata in one pass.
///
/// For protocols with feature lists (writer v7+), the writer list is authoritative: A feature's
/// presence in metadata (if a presence checker is defined) must match its presence in the list. An
/// AlwaysIfSupported feature has no obvious metadata footprint and requires no validation here.
///
/// For legacy protocols (no feature lists), features with presence checkers are inferred from
/// metadata and cross-validated against protocol versions. Legacy features without presence
/// checkers are assumed to be enabled in any version that supports them.
pub(crate) fn build_effective_features(
    protocol: &Protocol,
    schema: &Schema,
    properties: &TableProperties,
) -> DeltaResult<HashSet<TableFeature>> {
    // Step 1: Seed from protocol writer feature list (if any).
    let mut features = match protocol.writer_features() {
        Some(writer_list) => HashSet::from_iter(writer_list.iter().cloned()),
        None => HashSet::new(),
    };

    // `Protocol::try_new` rejects reader-writer features that appear only on the reader list, so we
    // don't need to add them here. But we do need to reject unknown reader-writer features, because
    // the effective feature list cannot distinguish them from writer-only unknown features.
    if let Some(reader_list) = protocol.reader_features() {
        for feature in reader_list {
            if feature.feature_type() == FeatureType::Unknown {
                return Err(Error::unsupported(format!(
                    "Unknown reader feature '{feature}'"
                )));
            }
        }
    }

    let min_writer_version = protocol.min_writer_version();
    let min_reader_version = protocol.min_reader_version();

    // Step 2: Detect features from metadata presence and/or legacy version inference.
    //
    // Features already in the set (from the writer list) are skipped. For the rest:
    // - If a presence checker is defined and reports true, validate and add the feature.
    // - If no presence checker is defined, fall back to legacy version inference.
    //
    // NOTE: TableFeature::Unknown has no presence checker and no legacy version, so is skipped.
    let has_writer_list = protocol.writer_features().is_some();
    for feature in TableFeature::iter() {
        // Listing a feature in the protocol does not require it to be present, but we still have
        // to invoke the presence checker in case it finds invalid metadata.
        let known_present = feature.check_presence(schema, properties)?.unwrap_or(false);
        if features.contains(&feature) {
            continue;
        }

        // Feature-list protocol: not allowed to infer legacy feature presence.
        let desc = feature.metadata_description();
        if has_writer_list {
            if known_present {
                return Err(Error::invalid_protocol(format!(
                    "Table has {desc} but '{feature}' is not in the protocol",
                )));
            }
            continue;
        }

        // Legacy protocol: validate version compatibility.
        // known_present features MUST match (error if not); inferred features skip if not.
        if !feature.is_valid_for_legacy_writer(min_writer_version) {
            if known_present {
                return Err(Error::invalid_protocol(format!(
                    "Table has {desc} but writer version \
                     {min_writer_version} does not support '{feature}'"
                )));
            }
            continue;
        }
        if feature.feature_type() == FeatureType::ReaderWriter
            && !feature.is_valid_for_legacy_reader(min_reader_version)
        {
            if known_present {
                return Err(Error::invalid_protocol(format!(
                    "Table has {desc} but reader version \
                     {min_reader_version} does not support '{feature}'"
                )));
            }
            continue;
        }

        features.insert(feature);
    }

    // Step 3: Validate groups of related features with overlapping metadata presence (such as
    // preview vs. final versions of a feature). We cannot validate them independently because
    // unused group member(s) would appear to have orphaned metadata.
    for group in FEATURE_GROUPS {
        group.validate(&features, schema, properties)?;
    }

    Ok(features)
}

/// Defines how a feature's enablement is determined
#[derive(Debug, Clone, Copy)]
enum EnablementCheck {
    /// Feature is enabled if it's supported (appears in protocol feature lists)
    AlwaysIfSupported,
    /// Feature is enabled if supported AND the provided function returns true when checking table properties
    EnabledIf(fn(&TableProperties) -> bool),
}

/// Represents the type of operation being performed on a table
#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumIter)]
pub(crate) enum Operation {
    /// Read operations on regular table data
    Scan,
    /// Read operations on change data feed data
    Cdf,
    /// Write operations on regular table data
    Write,
}

/// Defines whether the Rust kernel has implementation support for a feature's operation
enum KernelSupport {
    /// Kernel has full support for any operation on this feature
    Supported,
    /// Kernel does not support this operation on this feature
    NotSupported,
    /// Kernel can handle the feature only if its metadata is not actively present.
    ///
    /// At capability-check time, consults the feature's `presence_check`:
    /// - `Some(check)` returning `false` → Ok (feature is dormant)
    /// - `Some(check)` returning `true` → Err (feature is active, kernel can't handle it)
    /// - `None` → Err (can't prove inactive, must reject)
    ///
    /// Example: Invariants — kernel can handle tables that *declare* invariant support,
    /// but cannot enforce invariant expressions if they actually exist in the schema.
    NotSupportedIfPresent,
    /// Custom logic to determine support based on operation type and table properties.
    /// For example: Column Mapping may support Scan but not CDF, or CDF writes may only
    /// be supported when AppendOnly is true.
    Custom(fn(&Protocol, &TableProperties, Operation) -> DeltaResult<()>),
}

/// Types of requirements for feature dependencies
#[derive(Debug)]
pub(crate) enum FeatureRequirement {
    /// Feature must be supported (in protocol)
    Supported(TableFeature),
    /// Feature must be enabled (supported + property set)
    Enabled(TableFeature),
    /// Feature must NOT be supported
    NotSupported(TableFeature),
    /// Feature must NOT be enabled (may be supported but property must not activate it)
    NotEnabled(TableFeature),
}

/// Signature for presence checkers registered in [`FeatureInfo`].
///
/// Takes schema and table properties (some features only need one or the other).
/// Returns:
/// - `Ok(true)` if the feature's metadata traces are present and valid
/// - `Ok(false)` if the feature's metadata traces are absent
/// - `Err` if the feature's metadata traces are present but malformed
type PresenceCheckFn = fn(&Schema, &TableProperties) -> DeltaResult<bool>;

/// A group of features that share metadata traces and cannot be distinguished from each
/// other by metadata alone. If the shared metadata is present, at least one group member
/// must be in the effective feature set. Individual members should have
/// `presence_check: None` in their [`FeatureInfo`].
///
/// Validation direction: metadata present → at least one member supported. NOT the
/// reverse — a feature can be listed without its metadata being present ("supported" ≠
/// "active").
struct FeatureGroup {
    /// The features in this group.
    members: &'static [TableFeature],
    /// Detects whether the group's shared metadata is present.
    presence_check: PresenceCheckFn,
    /// Human-readable description of the shared metadata (e.g. "VARIANT columns").
    metadata_description: &'static str,
}

impl FeatureGroup {
    fn validate(
        &self,
        features: &HashSet<TableFeature>,
        schema: &Schema,
        props: &TableProperties,
    ) -> DeltaResult<()> {
        if (self.presence_check)(schema, props)? {
            require!(
                self.members.iter().any(|f| features.contains(f)),
                Error::invalid_protocol(format!(
                    "Table has {} but none of [{}] are in the protocol",
                    self.metadata_description,
                    self.members.iter().map(|f| f.as_ref()).format(", ")
                ))
            );
        }
        Ok(())
    }
}

/// Feature groups with shared metadata traces. Validated after building the effective
/// feature set, when we have full context to check cross-feature relationships.
static FEATURE_GROUPS: &[FeatureGroup] = &[
    // VariantType and VariantTypePreview share the same schema metadata (variant columns).
    // Neither can be distinguished from the other by metadata alone.
    FeatureGroup {
        members: &[TableFeature::VariantType, TableFeature::VariantTypePreview],
        presence_check: |s, _p| Ok(crate::schema::variant_utils::schema_uses_variant(s)),
        metadata_description: "VARIANT columns",
    },
];

/// Minimum protocol versions for legacy (pre-feature-list) inference.
/// Fields are (min_reader_version, min_writer_version).
struct MinReaderWriterVersion(i32, i32);

/// Rich metadata about a table feature including version requirements, dependencies, and support status
struct FeatureInfo {
    /// The type of feature (WriterOnly, ReaderWriter, or Unknown)
    pub feature_type: FeatureType,
    /// Minimum legacy protocol versions for version-based feature inference.
    /// `Some` for features that predate feature lists and can be inferred from protocol version.
    /// `None` for features that require explicit feature lists (reader v3+ / writer v7+).
    pub min_legacy_version: Option<MinReaderWriterVersion>,
    /// Requirements this feature has (features + custom validations)
    pub feature_requirements: &'static [FeatureRequirement],
    /// Rust kernel's support for this feature (may vary by Operation type)
    ///
    /// Note: `kernel_support` is only checked for features that are relevant to the
    /// current operation. Writer-only features are skipped for read operations.
    /// See `check_kernel_capabilities` for the filtering logic.
    pub kernel_support: KernelSupport,
    /// Optional presence checker: detects whether the feature's metadata traces exist.
    ///
    /// When `Some`, `build_effective_features` uses this for two-way validation:
    /// - Legacy inference: metadata present → validate protocol version allows it
    /// - Feature-list protocols: metadata present but feature not listed → error
    ///   (orphaned metadata that could cause corruption if feature is re-enabled)
    ///
    /// When `None`, the feature is not checkable — either because the feature has no
    /// metadata traces (protocol behavior flags like DomainMetadata, VacuumProtocolCheck),
    /// or because the kernel doesn't parse its metadata yet (CheckConstraints,
    /// GeneratedColumns, IdentityColumns). In the latter case, the presence checker
    /// should be added alongside the capability implementation.
    ///
    /// Note: this field is orthogonal to `kernel_support` and `enablement_check`. A feature
    /// can be `NotSupported` yet still have a presence checker (e.g. IcebergCompat — kernel
    /// can detect the toggle property even though it can't handle the feature's semantics).
    /// Likewise, `AlwaysIfSupported` features can have presence checkers for schema-intrinsic
    /// metadata (e.g. TimestampNtz, Variant — columns of that type must be detectable).
    ///
    /// Three common patterns:
    /// - **Bool-toggle property**: `presence_check` uses `property.is_some()`,
    ///   `enablement_check` uses `property == Some(true)` (e.g. AppendOnly, DeletionVectors)
    /// - **Schema-intrinsic**: `presence_check` traverses the schema for specific types or
    ///   annotations, `enablement_check` is `AlwaysIfSupported` (e.g. TimestampNtz, Variant)
    /// - **Complex/named**: a named function validates metadata consistency and may return
    ///   `Err` for malformed state (e.g. ColumnMapping, RowTracking)
    pub presence_check: Option<PresenceCheckFn>,
    /// Human-readable description of what metadata this feature leaves in a table.
    /// Used in orphaned-metadata error messages, e.g. "TIMESTAMP_NTZ columns",
    /// "column mapping annotations", "row tracking properties".
    pub metadata_description: &'static str,
    /// How to check if this feature is enabled in a table
    pub enablement_check: EnablementCheck,
}

// Static FeatureInfo instances for each table feature
static APPEND_ONLY_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::WriterOnly,
    min_legacy_version: Some(MinReaderWriterVersion(1, 2)),
    feature_requirements: &[],
    kernel_support: KernelSupport::Supported,
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::EnabledIf(|props| props.append_only == Some(true)),
};

static INVARIANTS_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::WriterOnly,
    min_legacy_version: Some(MinReaderWriterVersion(1, 2)),
    feature_requirements: &[],
    kernel_support: KernelSupport::NotSupportedIfPresent,
    presence_check: Some(|s, _p| Ok(crate::schema::InvariantChecker::has_invariants(s))),
    metadata_description: "column invariants",
    enablement_check: EnablementCheck::AlwaysIfSupported,
};

static CHECK_CONSTRAINTS_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::WriterOnly,
    min_legacy_version: Some(MinReaderWriterVersion(1, 3)),
    feature_requirements: &[],
    kernel_support: KernelSupport::NotSupported,
    // TODO: Add presence checker for delta.constraints.* properties and upgrade to NotSupportedIfPresent.
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::AlwaysIfSupported,
};

static CHANGE_DATA_FEED_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::WriterOnly,
    min_legacy_version: Some(MinReaderWriterVersion(1, 4)),
    feature_requirements: &[],
    kernel_support: KernelSupport::Supported,
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::EnabledIf(|props| {
        props.enable_change_data_feed == Some(true)
    }),
};

static GENERATED_COLUMNS_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::WriterOnly,
    min_legacy_version: Some(MinReaderWriterVersion(1, 4)),
    feature_requirements: &[],
    kernel_support: KernelSupport::NotSupported,
    // TODO: Add presence checker for delta.generationExpression metadata and upgrade to NotSupportedIfPresent.
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::AlwaysIfSupported,
};

static IDENTITY_COLUMNS_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::WriterOnly,
    min_legacy_version: Some(MinReaderWriterVersion(1, 6)),
    feature_requirements: &[],
    kernel_support: KernelSupport::NotSupported,
    // TODO: Add presence checker for delta.identity.* column metadata and upgrade to NotSupportedIfPresent.
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::AlwaysIfSupported,
};

static IN_COMMIT_TIMESTAMP_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::WriterOnly,
    min_legacy_version: None,
    feature_requirements: &[],
    kernel_support: KernelSupport::Custom(|_protocol, _properties, operation| match operation {
        Operation::Scan | Operation::Write | Operation::Cdf => Ok(()),
    }),
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::EnabledIf(|props| {
        props.enable_in_commit_timestamps == Some(true)
    }),
};

static ROW_TRACKING_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::WriterOnly,
    min_legacy_version: None,
    feature_requirements: &[FeatureRequirement::Supported(TableFeature::DomainMetadata)],
    kernel_support: KernelSupport::Supported,
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::EnabledIf(|props| {
        props.enable_row_tracking == Some(true) && props.row_tracking_suspended != Some(true)
    }),
};

static DOMAIN_METADATA_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::WriterOnly,
    min_legacy_version: None,
    feature_requirements: &[],
    kernel_support: KernelSupport::Supported,
    // No metadata traces to check — DomainMetadata is a protocol behavior, not a schema/property.
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::AlwaysIfSupported,
};

// TODO(#1125): IcebergCompatV1 requires schema type validation to block Map, Array, and Void types.
// This validation is not yet implemented. The feature is marked as NotSupported for writes until proper validation is added.
// See Delta Spark: IcebergCompat.scala CheckNoListMapNullType (lines 422-433)
// See Java Kernel: IcebergWriterCompatMetadataValidatorAndUpdater.java UNSUPPORTED_TYPES_CHECK
// See https://github.com/delta-io/delta/blob/master/PROTOCOL.md#writer-requirements-for-icebergcompatv1 for more requirements to support
static ICEBERG_COMPAT_V1_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::WriterOnly,
    min_legacy_version: None,
    feature_requirements: &[
        FeatureRequirement::Enabled(TableFeature::ColumnMapping),
        FeatureRequirement::NotSupported(TableFeature::DeletionVectors),
    ],
    kernel_support: KernelSupport::NotSupported,
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::EnabledIf(|props| {
        props.enable_iceberg_compat_v1 == Some(true)
    }),
};

// TODO(#1125): IcebergCompatV2 requires schema type validation. Unlike V1, V2 allows Map and Array
// types but needs validation against an allowlist of supported types.
// This validation is not yet implemented. The feature is marked as NotSupported for writes until proper validation is added.
// See Delta Spark: IcebergCompat.scala CheckTypeInV2AllowList (lines 450-459)
// See Java Kernel: IcebergCompatMetadataValidatorAndUpdater.java V2_SUPPORTED_TYPES
// See https://github.com/delta-io/delta/blob/master/PROTOCOL.md#writer-requirements-for-icebergcompatv2 for more requirements to support.
static ICEBERG_COMPAT_V2_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::WriterOnly,
    min_legacy_version: None,
    feature_requirements: &[
        FeatureRequirement::Enabled(TableFeature::ColumnMapping),
        FeatureRequirement::NotEnabled(TableFeature::IcebergCompatV1),
        FeatureRequirement::NotEnabled(TableFeature::DeletionVectors),
    ],
    kernel_support: KernelSupport::NotSupported,
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::EnabledIf(|props| {
        props.enable_iceberg_compat_v2 == Some(true)
    }),
};

static CLUSTERED_TABLE_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::WriterOnly,
    min_legacy_version: None,
    feature_requirements: &[FeatureRequirement::Supported(TableFeature::DomainMetadata)],
    kernel_support: KernelSupport::NotSupported,
    // TODO: Add presence checker when DomainMetadata access allows checking for delta.clustering domain.
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::AlwaysIfSupported,
};

static MATERIALIZE_PARTITION_COLUMNS_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::WriterOnly,
    min_legacy_version: None,
    feature_requirements: &[],
    kernel_support: KernelSupport::Supported,
    // No metadata traces to check — this is a write behavior flag.
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::AlwaysIfSupported,
};

static CATALOG_MANAGED_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::ReaderWriter,
    min_legacy_version: None,
    feature_requirements: &[],
    #[cfg(feature = "catalog-managed")]
    kernel_support: KernelSupport::Custom(|_, _, op| match op {
        Operation::Scan | Operation::Write => Ok(()),
        Operation::Cdf => Err(Error::unsupported(
            "Feature 'catalogManaged' is not supported for CDF",
        )),
    }),
    #[cfg(not(feature = "catalog-managed"))]
    kernel_support: KernelSupport::NotSupported,
    // No metadata traces — catalog management is external.
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::AlwaysIfSupported,
};

static CATALOG_OWNED_PREVIEW_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::ReaderWriter,
    min_legacy_version: None,
    feature_requirements: &[],
    #[cfg(feature = "catalog-managed")]
    kernel_support: KernelSupport::Custom(|_, _, op| match op {
        Operation::Scan | Operation::Write => Ok(()),
        Operation::Cdf => Err(Error::unsupported(
            "Feature 'catalogOwned-preview' is not supported for CDF",
        )),
    }),
    #[cfg(not(feature = "catalog-managed"))]
    kernel_support: KernelSupport::NotSupported,
    // No metadata traces — catalog management is external.
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::AlwaysIfSupported,
};

static COLUMN_MAPPING_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::ReaderWriter,
    min_legacy_version: Some(MinReaderWriterVersion(2, 5)),
    feature_requirements: &[],
    kernel_support: KernelSupport::Custom(|_, _, op| match op {
        Operation::Scan | Operation::Cdf => Ok(()),
        Operation::Write => Err(Error::unsupported(
            "Feature 'columnMapping' is not supported for writes",
        )),
    }),
    presence_check: Some(column_mapping_presence),
    metadata_description: "column mapping annotations",
    enablement_check: EnablementCheck::EnabledIf(|props| {
        props.column_mapping_mode.is_some()
            && props.column_mapping_mode != Some(ColumnMappingMode::None)
    }),
};

static DELETION_VECTORS_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::ReaderWriter,
    min_legacy_version: None,
    feature_requirements: &[],
    // We support writing to tables with DeletionVectors enabled, but we never write DV files
    // ourselves (no DML). The kernel only performs append operations.
    kernel_support: KernelSupport::Supported,
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::EnabledIf(|props| {
        props.enable_deletion_vectors == Some(true)
    }),
};

static TIMESTAMP_WITHOUT_TIMEZONE_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::ReaderWriter,
    min_legacy_version: None,
    feature_requirements: &[],
    kernel_support: KernelSupport::Supported,
    presence_check: Some(|s, _p| Ok(schema_uses_timestamp_ntz(s))),
    metadata_description: "TIMESTAMP_NTZ columns",
    enablement_check: EnablementCheck::AlwaysIfSupported,
};

static TYPE_WIDENING_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::ReaderWriter,
    min_legacy_version: None,
    feature_requirements: &[FeatureRequirement::NotSupported(
        TableFeature::TypeWideningPreview,
    )],
    kernel_support: KernelSupport::Custom(|_, _, op| match op {
        Operation::Scan | Operation::Cdf => Ok(()),
        Operation::Write => Err(Error::unsupported(
            "Feature 'typeWidening' is not supported for writes",
        )),
    }),
    // Metadata presence validated by FeatureGroup global validation (shared with TypeWideningPreview).
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::EnabledIf(|props| props.enable_type_widening == Some(true)),
};

static TYPE_WIDENING_PREVIEW_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::ReaderWriter,
    min_legacy_version: None,
    feature_requirements: &[],
    kernel_support: KernelSupport::Custom(|_, _, op| match op {
        Operation::Scan | Operation::Cdf => Ok(()),
        Operation::Write => Err(Error::unsupported(
            "Feature 'typeWidening-preview' is not supported for writes",
        )),
    }),
    // Metadata presence validated by FeatureGroup global validation (shared with TypeWidening).
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::EnabledIf(|props| props.enable_type_widening == Some(true)),
};

static V2_CHECKPOINT_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::ReaderWriter,
    min_legacy_version: None,
    feature_requirements: &[],
    kernel_support: KernelSupport::Supported,
    // No metadata traces — checkpoint format is not a schema/property concern.
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::AlwaysIfSupported,
};

static VACUUM_PROTOCOL_CHECK_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::ReaderWriter,
    min_legacy_version: None,
    feature_requirements: &[],
    kernel_support: KernelSupport::Supported,
    // No metadata traces — purely a protocol behavior flag.
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::AlwaysIfSupported,
};

static VARIANT_TYPE_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::ReaderWriter,
    min_legacy_version: None,
    feature_requirements: &[FeatureRequirement::NotSupported(
        TableFeature::VariantTypePreview,
    )],
    kernel_support: KernelSupport::Supported,
    // Metadata presence validated by FeatureGroup global validation (shared with VariantTypePreview).
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::AlwaysIfSupported,
};

static VARIANT_TYPE_PREVIEW_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::ReaderWriter,
    min_legacy_version: None,
    feature_requirements: &[],
    kernel_support: KernelSupport::Supported,
    // Metadata presence validated by FeatureGroup global validation (shared with VariantType).
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::AlwaysIfSupported,
};

static VARIANT_SHREDDING_PREVIEW_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::ReaderWriter,
    min_legacy_version: None,
    feature_requirements: &[],
    kernel_support: KernelSupport::Supported,
    // Shredding is a physical encoding detail, no schema/property metadata to check.
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::AlwaysIfSupported,
};

/// Unknown features: kernel doesn't support them, can't check their metadata, and
/// can't infer them from legacy versions. See [`TableFeature::category`].
static UNKNOWN_FEATURE_INFO: FeatureInfo = FeatureInfo {
    feature_type: FeatureType::Unknown,
    min_legacy_version: None,
    feature_requirements: &[],
    kernel_support: KernelSupport::NotSupported,
    presence_check: None,
    metadata_description: "",
    enablement_check: EnablementCheck::AlwaysIfSupported,
};

impl TableFeature {
    /// Classifies this feature as writer-only, reader-writer, or unknown.
    ///
    /// Unknown features are likely newer legitimate features, not invalid input. They display as
    /// the raw feature name (via strum `#[strum(default)]`), so error messages read naturally
    /// (e.g. "Feature 'futureFeature' is not supported").
    ///
    /// ## How unknown features are handled
    ///
    /// Unknown features have `KernelSupport::NotSupported`, no presence checker, no feature
    /// requirements, and no legacy version — so they cannot be inferred from metadata or protocol
    /// versions. They enter the effective feature set only from explicit protocol feature lists.
    ///
    /// Because the feature type is unknown, they require special handling at several points:
    ///
    /// 1. **Protocol validation** (`Protocol::validate_impl`): Unknown features that appear only on
    ///    the writer list are validated as writer-only. Unknown features on the reader list are
    ///    validated as reader-writer and therefore must appear on the writer list as well.
    ///
    /// 2. **Effective feature construction** (`build_effective_features`): Unknown *reader*
    ///    features are rejected before the reader/writer lists merge. The merge destroys list
    ///    provenance, and `FeatureType::Unknown` can't reconstruct it. Early rejection ensures
    ///    that the effective set does not contain any unknown feature from the reader list.
    ///
    /// 3. **Capability checking** (`check_kernel_capabilities`): Read operations check only
    ///    `ReaderWriter` features. Unknown features are skipped — they're guaranteed writer-only
    ///    after point 2. Write operations check all features, rejecting unknown features because
    ///    they are `KernelSupport::NotSupported`.
    ///
    pub(crate) fn feature_type(&self) -> FeatureType {
        self.info().feature_type
    }

    /// Returns true if this feature is enabled given the table properties.
    /// A feature is "enabled" if its enablement check passes (e.g. the toggle property is true).
    /// This does NOT check whether the feature is supported — the caller must check that separately.
    pub(crate) fn is_enabled(&self, props: &TableProperties) -> bool {
        match self.info().enablement_check {
            EnablementCheck::AlwaysIfSupported => true,
            EnablementCheck::EnabledIf(f) => f(props),
        }
    }

    /// Check if this feature's metadata traces are present in the table.
    ///
    /// Returns `Ok(None)` if the feature has no presence checker (not checkable).
    /// Returns `Ok(Some(true))` if present, `Ok(Some(false))` if absent,
    /// `Err(...)` if the metadata is malformed.
    fn check_presence(
        &self,
        schema: &Schema,
        props: &TableProperties,
    ) -> DeltaResult<Option<bool>> {
        self.info()
            .presence_check
            .map(|check| check(schema, props))
            .transpose()
    }

    /// Human-readable description of this feature's metadata traces.
    /// Used in error messages for orphaned metadata detection.
    fn metadata_description(&self) -> &'static str {
        self.info().metadata_description
    }

    /// Returns true if this feature can be inferred from a legacy reader protocol version.
    /// Always returns false for non-legacy (feature-list-only) features.
    pub(crate) fn is_valid_for_legacy_reader(&self, reader_version: i32) -> bool {
        matches!(
            self.info().min_legacy_version,
            Some(MinReaderWriterVersion(min_reader, _)) if reader_version >= min_reader
        )
    }

    /// Returns true if this feature can be inferred from a legacy writer protocol version.
    /// Always returns false for non-legacy (feature-list-only) features.
    fn is_valid_for_legacy_writer(&self, writer_version: i32) -> bool {
        matches!(
            self.info().min_legacy_version,
            Some(MinReaderWriterVersion(_, min_writer)) if writer_version >= min_writer
        )
    }

    pub(crate) fn feature_requirements(&self) -> &[FeatureRequirement] {
        self.info().feature_requirements
    }

    /// Check if the kernel supports this feature for the given operation.
    ///
    /// Custom checks always receive the operation and decide for themselves. Standard checks
    /// (Supported/NotSupported) skip known writer-only features for read operations, because
    /// writer-only features don't affect readers. Unknown features are conservatively checked.
    pub(crate) fn check_kernel_support(
        &self,
        protocol: &Protocol,
        schema: &Schema,
        props: &TableProperties,
        operation: Operation,
    ) -> DeltaResult<()> {
        // Determine whether this feature is problematic for the requested operation.
        // The "no problem" cases return early; unsupported cases fall through.
        match &self.info().kernel_support {
            KernelSupport::Supported => return Ok(()),
            KernelSupport::Custom(check) => return check(protocol, props, operation),
            KernelSupport::NotSupportedIfPresent => {
                if let Some(false) = self.check_presence(schema, props)? {
                    return Ok(());
                }
            }
            KernelSupport::NotSupported => {}
        }

        // Feature is not supported (or present when it shouldn't be).
        // Writer-only features are irrelevant for non-write operations. Unknown features
        // are safe to skip because build_effective_features rejects unknown reader features
        // at construction time, so any unknown in the effective set is guaranteed writer-only.
        match (operation, self.feature_type()) {
            (Operation::Write, _) | (_, FeatureType::ReaderWriter) => Err(Error::unsupported(
                format!("Feature '{self}' is not supported when enabled"),
            )),
            (Operation::Scan | Operation::Cdf, _) => Ok(()),
        }
    }

    /// Returns rich metadata about this table feature including version requirements,
    /// dependencies, and support status.
    fn info(&self) -> &FeatureInfo {
        match self {
            // Writer-only features
            TableFeature::AppendOnly => &APPEND_ONLY_INFO,
            TableFeature::Invariants => &INVARIANTS_INFO,
            TableFeature::CheckConstraints => &CHECK_CONSTRAINTS_INFO,
            TableFeature::ChangeDataFeed => &CHANGE_DATA_FEED_INFO,
            TableFeature::GeneratedColumns => &GENERATED_COLUMNS_INFO,
            TableFeature::IdentityColumns => &IDENTITY_COLUMNS_INFO,
            TableFeature::InCommitTimestamp => &IN_COMMIT_TIMESTAMP_INFO,
            TableFeature::RowTracking => &ROW_TRACKING_INFO,
            TableFeature::DomainMetadata => &DOMAIN_METADATA_INFO,
            TableFeature::IcebergCompatV1 => &ICEBERG_COMPAT_V1_INFO,
            TableFeature::IcebergCompatV2 => &ICEBERG_COMPAT_V2_INFO,
            TableFeature::ClusteredTable => &CLUSTERED_TABLE_INFO,
            TableFeature::MaterializePartitionColumns => &MATERIALIZE_PARTITION_COLUMNS_INFO,

            // ReaderWriter features
            TableFeature::CatalogManaged => &CATALOG_MANAGED_INFO,
            TableFeature::CatalogOwnedPreview => &CATALOG_OWNED_PREVIEW_INFO,
            TableFeature::ColumnMapping => &COLUMN_MAPPING_INFO,
            TableFeature::DeletionVectors => &DELETION_VECTORS_INFO,
            TableFeature::TimestampWithoutTimezone => &TIMESTAMP_WITHOUT_TIMEZONE_INFO,
            TableFeature::TypeWidening => &TYPE_WIDENING_INFO,
            TableFeature::TypeWideningPreview => &TYPE_WIDENING_PREVIEW_INFO,
            TableFeature::V2Checkpoint => &V2_CHECKPOINT_INFO,
            TableFeature::VacuumProtocolCheck => &VACUUM_PROTOCOL_CHECK_INFO,
            TableFeature::VariantType => &VARIANT_TYPE_INFO,
            TableFeature::VariantTypePreview => &VARIANT_TYPE_PREVIEW_INFO,
            TableFeature::VariantShreddingPreview => &VARIANT_SHREDDING_PREVIEW_INFO,

            // Unknown features: not supported by kernel, no legacy version inference.
            TableFeature::Unknown(_) => &UNKNOWN_FEATURE_INFO,
        }
    }
}

impl ToDataType for TableFeature {
    fn to_data_type() -> DataType {
        DataType::STRING
    }
}

impl From<TableFeature> for Scalar {
    fn from(feature: TableFeature) -> Self {
        Scalar::String(feature.to_string())
    }
}

#[cfg(test)] // currently only used in tests
impl TableFeature {
    pub(crate) fn unknown(s: impl ToString) -> Self {
        TableFeature::Unknown(s.to_string())
    }
}

/// Like `Into<TableFeature>`, but avoids collisions between strum's derived `EnumString` and the
/// blanket impl `TryFrom<&str>` that `From<&str> for TableFeature` would trigger.
///
/// Parsing is infallible: the `Unknown` default variant catches any unrecognized feature name. If
/// https://github.com/Peternator7/strum/pull/432 merges, use impl From for TableFeature instead.
pub(crate) trait IntoTableFeature {
    fn into_table_feature(self) -> TableFeature;
}

impl IntoTableFeature for TableFeature {
    fn into_table_feature(self) -> TableFeature {
        self
    }
}

impl IntoTableFeature for &TableFeature {
    fn into_table_feature(self) -> TableFeature {
        self.clone()
    }
}

/// Parsing is infallible thanks to `TableFeature::Unknown` default variant
impl IntoTableFeature for &str {
    fn into_table_feature(self) -> TableFeature {
        #[allow(clippy::unwrap_used)] // infallible, see strum parse_err_fn
        self.parse().unwrap()
    }
}

impl IntoTableFeature for String {
    fn into_table_feature(self) -> TableFeature {
        self.as_str().into_table_feature()
    }
}

/// Formats a slice of table features using Delta's standard serialization (camelCase).
pub(crate) fn format_features(features: &[TableFeature]) -> String {
    let feature_strings: Vec<&str> = features.iter().map(|f| f.as_ref()).collect_vec();
    format!("[{}]", feature_strings.join(", "))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unknown_features() {
        let mixed_reader = &[
            TableFeature::DeletionVectors,
            TableFeature::unknown("cool_feature"),
            TableFeature::ColumnMapping,
        ];
        let mixed_writer = &[
            TableFeature::DeletionVectors,
            TableFeature::unknown("cool_feature"),
            TableFeature::AppendOnly,
        ];

        let reader_string = serde_json::to_string(mixed_reader).unwrap();
        let writer_string = serde_json::to_string(mixed_writer).unwrap();

        assert_eq!(
            &reader_string,
            "[\"deletionVectors\",\"cool_feature\",\"columnMapping\"]"
        );
        assert_eq!(
            &writer_string,
            "[\"deletionVectors\",\"cool_feature\",\"appendOnly\"]"
        );

        let typed_reader: Vec<TableFeature> = serde_json::from_str(&reader_string).unwrap();
        let typed_writer: Vec<TableFeature> = serde_json::from_str(&writer_string).unwrap();

        assert_eq!(typed_reader.len(), 3);
        assert_eq!(&typed_reader, mixed_reader);
        assert_eq!(typed_writer.len(), 3);
        assert_eq!(&typed_writer, mixed_writer);
    }

    #[test]
    fn test_roundtrip_table_features() {
        use strum::IntoEnumIterator as _;

        for feature in TableFeature::iter() {
            let expected = match feature {
                TableFeature::AppendOnly => "appendOnly",
                TableFeature::Invariants => "invariants",
                TableFeature::CheckConstraints => "checkConstraints",
                TableFeature::ChangeDataFeed => "changeDataFeed",
                TableFeature::GeneratedColumns => "generatedColumns",
                TableFeature::IdentityColumns => "identityColumns",
                TableFeature::InCommitTimestamp => "inCommitTimestamp",
                TableFeature::RowTracking => "rowTracking",
                TableFeature::DomainMetadata => "domainMetadata",
                TableFeature::IcebergCompatV1 => "icebergCompatV1",
                TableFeature::IcebergCompatV2 => "icebergCompatV2",
                TableFeature::ClusteredTable => "clustering",
                TableFeature::MaterializePartitionColumns => "materializePartitionColumns",
                TableFeature::CatalogManaged => "catalogManaged",
                TableFeature::CatalogOwnedPreview => "catalogOwned-preview",
                TableFeature::ColumnMapping => "columnMapping",
                TableFeature::DeletionVectors => "deletionVectors",
                TableFeature::TimestampWithoutTimezone => "timestampNtz",
                TableFeature::TypeWidening => "typeWidening",
                TableFeature::TypeWideningPreview => "typeWidening-preview",
                TableFeature::V2Checkpoint => "v2Checkpoint",
                TableFeature::VacuumProtocolCheck => "vacuumProtocolCheck",
                TableFeature::VariantType => "variantType",
                TableFeature::VariantTypePreview => "variantType-preview",
                TableFeature::VariantShreddingPreview => "variantShredding-preview",
                TableFeature::Unknown(_) => continue, // tested in test_unknown_features
            };

            // strum
            assert_eq!(feature.to_string(), expected);
            assert_eq!(feature, expected.into_table_feature());

            // json
            let serialized = serde_json::to_string(&feature).unwrap();
            assert_eq!(serialized, format!("\"{expected}\""));

            let deserialized: TableFeature = serde_json::from_str(&serialized).unwrap();
            assert_eq!(deserialized, feature);
        }
    }
}
