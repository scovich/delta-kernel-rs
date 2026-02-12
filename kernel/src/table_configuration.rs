//! This module defines [`TableConfiguration`], a high level api to check feature support and
//! feature enablement for a table at a given version. This encapsulates [`Protocol`], [`Metadata`],
//! [`Schema`], [`TableProperties`], and [`ColumnMappingMode`]. These structs in isolation should
//! be considered raw and unvalidated if they are not a part of [`TableConfiguration`]. We unify
//! these fields because they are deeply intertwined when dealing with table features. For example:
//! To check that deletion vector writes are enabled, you must check both both the protocol's
//! reader/writer features, and ensure that the deletion vector table property is enabled in the
//! [`TableProperties`].
//!
//! [`Schema`]: crate::schema::Schema
use std::collections::HashSet;
use std::sync::Arc;

use url::Url;

use crate::actions::{Metadata, Protocol};
use crate::expressions::ColumnName;
use crate::scan::data_skipping::stats_schema::{
    expected_stats_schema, stats_column_names, PhysicalStatsSchemaTransform,
};
use crate::schema::{SchemaRef, SchemaTransform, StructType};
use crate::table_features::{
    build_effective_features, ColumnMappingMode, FeatureRequirement, Operation, TableFeature,
};
use crate::table_properties::TableProperties;
use crate::utils::require;
use crate::{DeltaResult, Error, Version};
use delta_kernel_derive::internal_api;

/// Expected schemas for file statistics.
///
/// Contains both logical and physical versions of the stats schema:
/// - **Logical schema**: Uses original column names (matching table schema)
/// - **Physical schema**: Uses physical column names (for column mapping)
///
/// When column mapping is disabled (`ColumnMappingMode::None`), both schemas are identical.
#[allow(unused)]
#[derive(Debug, Clone)]
pub(crate) struct ExpectedStatsSchemas {
    /// Stats schema using logical (user-facing) column names.
    pub logical: SchemaRef,
    /// Stats schema using physical column names (for storage).
    pub physical: SchemaRef,
}

/// Information about in-commit timestamp enablement state.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum InCommitTimestampEnablement {
    /// In-commit timestamps is not enabled
    NotEnabled,
    /// In-commit timestamps is enabled
    Enabled {
        /// Enablement information, if available. `None` indicates the table was created
        /// with ICT enabled from the beginning (no enablement properties needed).
        enablement: Option<(Version, i64)>,
    },
}

/// Holds all the configuration for a table at a specific version. This includes the supported
/// reader and writer features, table properties, schema, version, and table root. This can be used
/// to check whether a table supports a feature or has it enabled. For example, deletion vector
/// support can be checked with [`TableConfiguration::is_feature_supported`] and deletion
/// vector write enablement can be checked with [`TableConfiguration::is_feature_enabled`].
///
/// [`TableConfiguration`] performs checks upon construction with `TableConfiguration::try_new`
/// to validate that Metadata and Protocol are correctly formatted and mutually compatible.
/// After construction, call `check_kernel_capabilities` to verify that the kernel supports the
/// required operations for the table's protocol features.
#[internal_api]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TableConfiguration {
    metadata: Metadata,
    protocol: Protocol,
    schema: SchemaRef,
    table_properties: TableProperties,
    column_mapping_mode: ColumnMappingMode,
    /// Effective feature list derived from protocol lists (if present) and/or legacy
    /// version inference with presence checks. Downstream code should use this list
    /// via `is_feature_supported` rather than inspecting protocol lists directly.
    ///
    /// NOTE: This is the deduplicated union of reader and writer lists, so reader vs. writer
    /// provenance is lost. We rely on `feature_type()` to reconstruct it: `WriterOnly` and
    /// `ReaderWriter` are static, and unknown reader features are rejected at build
    /// time (see [`TableFeature::feature_type`]).
    effective_features: HashSet<TableFeature>,
    table_root: Url,
    version: Version,
}

impl TableConfiguration {
    /// Constructs a [`TableConfiguration`] for a table located in `table_root` at `version`.
    /// This validates that the [`Metadata`] and [`Protocol`] are compatible with one another
    /// and that the kernel supports reading from this table.
    ///
    /// Note: This only returns successfully if kernel supports reading the table. It's important
    /// to do this validation in `try_new` because all table accesses must first construct
    /// the [`TableConfiguration`]. This ensures that developers never forget to check that kernel
    /// supports reading the table, and that all table accesses are legal.
    ///
    /// Note: In the future, we will perform stricter checks on the set of reader and writer
    /// features. In particular, we will check that:
    ///     - Non-legacy features must appear in both reader features and writer features lists.
    ///       If such a feature is present, the reader version and writer version must be 3, and 5
    ///       respectively.
    ///     - Legacy reader features occur when the reader version is 3, but the writer version is
    ///       either 5 or 6. In this case, the writer feature list must be empty.
    ///     - Column mapping is the only legacy feature present in kernel. No future delta versions
    ///       will introduce new legacy features.
    /// See: <https://github.com/delta-io/delta-kernel-rs/issues/650>
    #[internal_api]
    pub(crate) fn try_new(
        metadata: Metadata,
        protocol: Protocol,
        table_root: Url,
        version: Version,
    ) -> DeltaResult<Self> {
        let schema = Arc::new(metadata.parse_schema()?);
        let table_properties = metadata.parse_table_properties();
        let effective_features = build_effective_features(&protocol, &schema, &table_properties)?;

        // Safe to read the property directly: if column mapping mode is set to id/name but the
        // ColumnMapping feature is not in the effective set, build_effective_features would have
        // already rejected the table as an orphaned metadata violation.
        let column_mapping_mode = table_properties
            .column_mapping_mode
            .unwrap_or(ColumnMappingMode::None);

        let table_config = Self {
            schema,
            metadata,
            protocol,
            table_properties,
            column_mapping_mode,
            effective_features,
            table_root,
            version,
        };

        // Now that we have the effective feature set, validate requirements.
        for feature in &table_config.effective_features {
            table_config.validate_feature_requirements(feature)?;
        }

        Ok(table_config)
    }

    pub(crate) fn try_new_from(
        table_configuration: &Self,
        new_metadata: Option<Metadata>,
        new_protocol: Option<Protocol>,
        new_version: Version,
    ) -> DeltaResult<Self> {
        // simplest case: no new P/M, just return the existing table configuration with new version
        if new_metadata.is_none() && new_protocol.is_none() {
            return Ok(Self {
                version: new_version,
                ..table_configuration.clone()
            });
        }

        // note that while we could pick apart the protocol/metadata updates and validate them
        // individually, instead we just re-parse so that we can recycle the try_new validation
        // (instead of duplicating it here).
        Self::try_new(
            new_metadata.unwrap_or_else(|| table_configuration.metadata.clone()),
            new_protocol.unwrap_or_else(|| table_configuration.protocol.clone()),
            table_configuration.table_root.clone(),
            new_version,
        )
    }

    /// Creates a new [`TableConfiguration`] representing the table configuration immediately
    /// after a commit.
    ///
    /// This method takes a pre-commit table configuration and produces a post-commit
    /// configuration at the committed version. This allows immediate use of the new table
    /// configuration without re-reading metadata from storage.
    ///
    /// TODO: Take in Protocol (when Kernel-RS supports protocol changes)
    /// TODO: Take in Metadata (when Kernel-RS supports metadata changes)
    pub(crate) fn new_post_commit(table_configuration: &Self, new_version: Version) -> Self {
        Self {
            version: new_version,
            ..table_configuration.clone()
        }
    }

    /// Generates the expected schemas for file statistics (both logical and physical).
    ///
    /// Engines can provide statistics for files written to the delta table, enabling
    /// data skipping and other optimizations. This method generates the expected schemas
    /// for structured statistics based on the table configuration.
    ///
    /// Returns a tuple of `(logical_stats_schema, physical_stats_schema)`:
    /// - **Logical schema**: Uses original column names (matching table schema)
    /// - **Physical schema**: Uses physical column names (respecting column mapping mode)
    ///
    /// Both schemas are structured as:
    /// ```text
    /// {
    ///   numRecords: long,
    ///   nullCount: { <columns with LONG type> },
    ///   minValues: { <columns with original types> },
    ///   maxValues: { <columns with original types> },
    /// }
    /// ```
    ///
    /// The schemas are affected by:
    /// - **Column mapping mode**: Physical schema field names use physical names from column
    ///   mapping metadata.
    /// - **`delta.dataSkippingStatsColumns`**: If set, only specified columns are included.
    /// - **`delta.dataSkippingNumIndexedCols`**: Otherwise, includes the first N leaf columns
    ///   (default 32).
    /// - **Clustering columns**: Per the Delta protocol, clustering columns are always included
    ///   in statistics, regardless of the above settings.
    ///
    /// See the Delta protocol for more details on per-file statistics:
    /// <https://github.com/delta-io/delta/blob/master/PROTOCOL.md#per-file-statistics>
    #[allow(unused)]
    #[internal_api]
    pub(crate) fn build_expected_stats_schemas(
        &self,
        clustering_columns: Option<&[ColumnName]>,
    ) -> DeltaResult<ExpectedStatsSchemas> {
        let logical_data_schema = self.logical_data_schema();
        let logical_stats_schema = Arc::new(expected_stats_schema(
            &logical_data_schema,
            self.table_properties(),
            clustering_columns,
        )?);
        let physical_stats_schema = match self.column_mapping_mode() {
            ColumnMappingMode::None => logical_stats_schema.clone(),
            _ => PhysicalStatsSchemaTransform {
                column_mapping_mode: self.column_mapping_mode(),
            }
            .transform_struct(&logical_stats_schema)
            .map(|s| Arc::new(s.into_owned()))
            .unwrap_or_else(|| logical_stats_schema.clone()),
        };
        Ok(ExpectedStatsSchemas {
            logical: logical_stats_schema,
            physical: physical_stats_schema,
        })
    }

    /// Returns the list of logical column names that should have statistics collected.
    ///
    /// Returns leaf column paths as [`ColumnName`] objects, which store path components
    /// separately and handle escaping of special characters (dots, spaces) via backticks.
    ///
    /// Per the Delta protocol, clustering columns are always included in statistics,
    /// regardless of the `delta.dataSkippingStatsColumns` or `delta.dataSkippingNumIndexedCols`
    /// settings.
    #[allow(unused)]
    #[internal_api]
    pub(crate) fn stats_column_names(
        &self,
        clustering_columns: Option<&[ColumnName]>,
    ) -> Vec<ColumnName> {
        stats_column_names(
            &self.logical_data_schema(),
            self.table_properties(),
            clustering_columns,
        )
    }

    /// Returns the logical schema for data columns (excludes partition columns).
    ///
    /// Partition columns are excluded because statistics are only collected for data columns
    /// that are physically stored in the parquet files. Partition values are stored in the
    /// file path, not in the file content, so they don't have file-level statistics.
    fn logical_data_schema(&self) -> StructType {
        let partition_columns = self.metadata().partition_columns();
        StructType::new_unchecked(
            self.schema()
                .fields()
                .filter(|field| !partition_columns.contains(field.name()))
                .cloned(),
        )
    }

    /// The [`Metadata`] for this table at this version.
    #[internal_api]
    pub(crate) fn metadata(&self) -> &Metadata {
        &self.metadata
    }

    /// The [`Protocol`] of this table at this version.
    #[allow(unused)]
    #[internal_api]
    pub(crate) fn protocol(&self) -> &Protocol {
        &self.protocol
    }

    /// The logical schema ([`SchemaRef`]) of this table at this version.
    #[internal_api]
    pub(crate) fn schema(&self) -> SchemaRef {
        self.schema.clone()
    }

    /// The [`TableProperties`] of this table at this version.
    #[internal_api]
    pub(crate) fn table_properties(&self) -> &TableProperties {
        &self.table_properties
    }

    /// True if this table is catalog-managed (either stable or preview feature).
    pub(crate) fn is_catalog_managed(&self) -> bool {
        self.is_feature_supported(&TableFeature::CatalogManaged)
            || self.is_feature_supported(&TableFeature::CatalogOwnedPreview)
    }

    /// The [`ColumnMappingMode`] for this table at this version.
    #[internal_api]
    pub(crate) fn column_mapping_mode(&self) -> ColumnMappingMode {
        self.column_mapping_mode
    }

    /// The [`Url`] of the table this [`TableConfiguration`] belongs to
    #[internal_api]
    pub(crate) fn table_root(&self) -> &Url {
        &self.table_root
    }

    /// The [`Version`] which this [`TableConfiguration`] belongs to.
    #[internal_api]
    pub(crate) fn version(&self) -> Version {
        self.version
    }

    /// Validates that all feature requirements for a given feature are satisfied.
    fn validate_feature_requirements(&self, feature: &TableFeature) -> DeltaResult<()> {
        for req in feature.feature_requirements() {
            match req {
                FeatureRequirement::Enabled(dep) => {
                    require!(
                        self.is_feature_enabled(dep),
                        Error::invalid_protocol(format!(
                            "Feature '{feature}' requires '{dep}' to be enabled"
                        ))
                    );
                }
                FeatureRequirement::NotSupported(dep) => {
                    require!(
                        !self.is_feature_supported(dep),
                        Error::invalid_protocol(format!(
                            "Feature '{feature}' requires '{dep}' to not be supported"
                        ))
                    );
                }
            }
        }
        Ok(())
    }

    /// Returns `Ok` if the kernel supports the given operation on this table. This checks that
    /// the protocol's features are all supported for the requested operation type.
    ///
    /// Feature requirements (dependencies between features) are validated at construction
    /// time by [`build_effective_features`], not here. This only checks kernel capability.
    ///
    /// Write operations must check all features, and all operations must check reader-writer
    /// features, but readers can skip unknown features — see [`TableFeature::feature_type`].
    #[internal_api]
    pub(crate) fn check_kernel_capabilities(&self, operation: Operation) -> DeltaResult<()> {
        let protocol = &self.protocol;
        let schema = self.schema();
        let props = &self.table_properties;
        for feature in &self.effective_features {
            feature.check_kernel_support(protocol, schema.as_ref(), props, operation)?;
        }
        Ok(())
    }

    /// Returns information about in-commit timestamp enablement state.
    ///
    /// Returns an error if only one of the enablement properties is present, as this indicates
    /// an inconsistent state.
    #[allow(unused)]
    pub(crate) fn in_commit_timestamp_enablement(
        &self,
    ) -> DeltaResult<InCommitTimestampEnablement> {
        if !self.is_feature_enabled(&TableFeature::InCommitTimestamp) {
            return Ok(InCommitTimestampEnablement::NotEnabled);
        }

        let enablement_version = self
            .table_properties()
            .in_commit_timestamp_enablement_version;
        let enablement_timestamp = self
            .table_properties()
            .in_commit_timestamp_enablement_timestamp;

        match (enablement_version, enablement_timestamp) {
            (Some(version), Some(timestamp)) => Ok(InCommitTimestampEnablement::Enabled {
                enablement: Some((version, timestamp)),
            }),
            (Some(_), None) => Err(Error::generic(
                "In-commit timestamp enabled, but enablement timestamp is missing",
            )),
            (None, Some(_)) => Err(Error::generic(
                "In-commit timestamp enabled, but enablement version is missing",
            )),
            // If InCommitTimestamps was enabled at the beginning of the table's history,
            // it may have an empty enablement version and timestamp
            (None, None) => Ok(InCommitTimestampEnablement::Enabled { enablement: None }),
        }
    }

    /// Returns `true` if row tracking is suspended for this table.
    ///
    /// Row tracking is suspended when the `delta.rowTrackingSuspended` table property is set to `true`.
    /// Note that:
    /// - Row tracking can be _supported_ and _suspended_ at the same time.
    /// - Row tracking cannot be _enabled_ while _suspended_.
    pub(crate) fn is_row_tracking_suspended(&self) -> bool {
        self.table_properties()
            .row_tracking_suspended
            .unwrap_or(false)
    }

    /// Returns `true` if row tracking information should be written for this table.
    ///
    /// Row tracking information should be written when:
    /// - Row tracking is supported
    /// - Row tracking is not suspended
    ///
    /// Note: We ignore [`is_row_tracking_enabled`] at this point because Kernel does not
    /// preserve row IDs and row commit versions yet.
    pub(crate) fn should_write_row_tracking(&self) -> bool {
        self.is_feature_supported(&TableFeature::RowTracking) && !self.is_row_tracking_suspended()
    }

    /// Returns true if the protocol uses legacy reader version (< 3)
    #[allow(dead_code)]
    fn is_legacy_reader_version(&self) -> bool {
        self.protocol.min_reader_version() < 3
    }

    /// Returns true if the protocol uses legacy writer version (< 7)
    #[allow(dead_code)]
    fn is_legacy_writer_version(&self) -> bool {
        self.protocol.min_writer_version() < 7
    }

    /// Check if a feature is in the effective feature list. The effective list is built at
    /// construction time from protocol lists and/or legacy version inference with presence
    /// checks, so this is a simple lookup.
    #[internal_api]
    pub(crate) fn is_feature_supported(&self, feature: &TableFeature) -> bool {
        self.effective_features.contains(feature)
    }

    /// Check if a feature is enabled.
    ///
    /// A feature is enabled if:
    /// 1. It is in the effective feature list (i.e. supported)
    /// 2. The enablement check passes (e.g. table property is set)
    #[internal_api]
    pub(crate) fn is_feature_enabled(&self, feature: &TableFeature) -> bool {
        self.is_feature_supported(feature) && feature.is_enabled(&self.table_properties)
    }
}

#[cfg(test)]
mod test {

    use std::collections::HashMap;
    use std::sync::Arc;

    use url::Url;

    use crate::actions::{Metadata, Protocol};
    use crate::schema::ColumnName;
    use crate::schema::{DataType, StructField, StructType};
    use crate::table_features::ColumnMappingMode;
    use crate::table_features::{
        FeatureType, Operation, TableFeature, TABLE_FEATURES_MIN_READER_VERSION,
        TABLE_FEATURES_MIN_WRITER_VERSION,
    };
    use crate::table_properties::TableProperties;
    use crate::utils::test_utils::assert_result_error_with_message;
    use crate::Error;

    use super::{InCommitTimestampEnablement, TableConfiguration};

    fn create_mock_table_config(
        props_to_enable: &[&str],
        features: &[TableFeature],
    ) -> TableConfiguration {
        create_mock_table_config_with_version(props_to_enable, Some(features), 3, 7)
    }

    fn create_mock_table_config_with_version(
        props_to_enable: &[&str],
        features_opt: Option<&[TableFeature]>,
        min_reader_version: i32,
        min_writer_version: i32,
    ) -> TableConfiguration {
        let schema = StructType::new_unchecked([StructField::nullable("value", DataType::INTEGER)]);
        let metadata = Metadata::try_new(
            None,
            None,
            schema,
            vec![],
            0,
            HashMap::from_iter(
                props_to_enable
                    .iter()
                    .map(|key| (key.to_string(), "true".to_string())),
            ),
        )
        .unwrap();

        let (reader_features_opt, writer_features_opt) = if let Some(features) = features_opt {
            // This helper only handles known features. Unknown features would need
            // explicit placement on reader vs writer lists.
            assert!(
                features
                    .iter()
                    .all(|f| f.feature_type() != FeatureType::Unknown),
                "Test helper does not support unknown features"
            );
            let reader_features = features
                .iter()
                .filter(|f| f.feature_type() == FeatureType::ReaderWriter);
            (
                // Only add reader_features if reader >= 3 (non-legacy reader mode)
                (min_reader_version >= TABLE_FEATURES_MIN_READER_VERSION)
                    .then_some(reader_features),
                // Only add writer_features if writer >= 7 (non-legacy writer mode)
                (min_writer_version >= TABLE_FEATURES_MIN_WRITER_VERSION).then_some(features),
            )
        } else {
            (None, None)
        };

        let protocol = Protocol::try_new(
            min_reader_version,
            min_writer_version,
            reader_features_opt,
            writer_features_opt,
        )
        .unwrap();
        let table_root = Url::try_from("file:///").unwrap();
        TableConfiguration::try_new(metadata, protocol, table_root, 0).unwrap()
    }

    #[test]
    fn dv_supported_not_enabled() {
        let schema = StructType::new_unchecked([StructField::nullable("value", DataType::INTEGER)]);
        let metadata = Metadata::try_new(
            None,
            None,
            schema,
            vec![],
            0,
            HashMap::from_iter([("delta.enableChangeDataFeed".to_string(), "true".to_string())]),
        )
        .unwrap();
        let protocol = Protocol::try_new(
            3,
            7,
            Some([TableFeature::DeletionVectors]),
            Some([TableFeature::DeletionVectors, TableFeature::ChangeDataFeed]),
        )
        .unwrap();
        let table_root = Url::try_from("file:///").unwrap();
        let table_config = TableConfiguration::try_new(metadata, protocol, table_root, 0).unwrap();
        assert!(table_config.is_feature_supported(&TableFeature::DeletionVectors));
        assert!(!table_config.is_feature_enabled(&TableFeature::DeletionVectors));
    }

    #[test]
    fn dv_enabled() {
        let schema = StructType::new_unchecked([StructField::nullable("value", DataType::INTEGER)]);
        let metadata = Metadata::try_new(
            None,
            None,
            schema,
            vec![],
            0,
            HashMap::from_iter([
                ("delta.enableChangeDataFeed".to_string(), "true".to_string()),
                (
                    "delta.enableDeletionVectors".to_string(),
                    "true".to_string(),
                ),
            ]),
        )
        .unwrap();
        let protocol = Protocol::try_new(
            3,
            7,
            Some([TableFeature::DeletionVectors]),
            Some([TableFeature::DeletionVectors, TableFeature::ChangeDataFeed]),
        )
        .unwrap();
        let table_root = Url::try_from("file:///").unwrap();
        let table_config = TableConfiguration::try_new(metadata, protocol, table_root, 0).unwrap();
        assert!(table_config.is_feature_supported(&TableFeature::DeletionVectors));
        assert!(table_config.is_feature_enabled(&TableFeature::DeletionVectors));
    }

    #[test]
    fn write_with_cdf() {
        use TableFeature::*;
        let cases = [
            (
                // Writing to CDF-enabled table is supported for writes
                create_mock_table_config(&["delta.enableChangeDataFeed"], &[ChangeDataFeed]),
                Ok(()),
            ),
            (
                // Should succeed even if AppendOnly is supported but not enabled
                create_mock_table_config(
                    &["delta.enableChangeDataFeed"],
                    &[ChangeDataFeed, AppendOnly],
                ),
                Ok(()),
            ),
            (
                // Should succeed since AppendOnly is enabled
                create_mock_table_config(
                    &["delta.enableChangeDataFeed", "delta.appendOnly"],
                    &[ChangeDataFeed, AppendOnly],
                ),
                Ok(()),
            ),
            // NOTE: The following cases should be updated if column mapping for writes is
            // supported before cdc is.
            (
                // Should fail since change data feed and column mapping features cannot both be
                // present.
                create_mock_table_config(
                    &["delta.enableChangeDataFeed", "delta.appendOnly"],
                    &[ChangeDataFeed, ColumnMapping, AppendOnly],
                ),
                Err(Error::unsupported(
                    "Feature 'columnMapping' is not supported for writes",
                )),
            ),
            (
                // The table does not require writing CDC files, so it is safe to write to it.
                create_mock_table_config(
                    &["delta.appendOnly"],
                    &[ChangeDataFeed, ColumnMapping, AppendOnly],
                ),
                Err(Error::unsupported(
                    "Feature 'columnMapping' is not supported for writes",
                )),
            ),
            (
                // Should succeed since change data feed is not enabled
                create_mock_table_config(&["delta.appendOnly"], &[AppendOnly]),
                Ok(()),
            ),
        ];

        for (table_configuration, result) in cases {
            match (
                table_configuration.check_kernel_capabilities(Operation::Write),
                result,
            ) {
                (Ok(()), Ok(())) => { /* Correct result */ }
                (actual_result, Err(expected)) => {
                    assert_result_error_with_message(actual_result, &expected.to_string());
                }
                (Err(actual_result), Ok(())) => {
                    panic!("Expected Ok but got error: {actual_result}");
                }
            }
        }

        // Writer version > 7 is rejected at protocol construction time
        let result = Protocol::try_new(1, 8, None::<Vec<String>>, None::<Vec<String>>);
        assert_result_error_with_message(result, "Unsupported minimum writer version 8");
    }
    #[test]
    fn ict_enabled_from_table_creation() {
        let schema = StructType::new_unchecked([StructField::nullable("value", DataType::INTEGER)]);
        let metadata = Metadata::try_new(
            None,
            None,
            schema,
            vec![],
            0, // Table creation version
            HashMap::from_iter([(
                "delta.enableInCommitTimestamps".to_string(),
                "true".to_string(),
            )]),
        )
        .unwrap();
        let protocol = Protocol::try_new(
            3,
            7,
            Some::<Vec<String>>(vec![]),
            Some([TableFeature::InCommitTimestamp]),
        )
        .unwrap();
        let table_root = Url::try_from("file:///").unwrap();
        let table_config = TableConfiguration::try_new(metadata, protocol, table_root, 0).unwrap();
        assert!(table_config.is_feature_supported(&TableFeature::InCommitTimestamp));
        assert!(table_config.is_feature_enabled(&TableFeature::InCommitTimestamp));
        // When ICT is enabled from table creation (version 0), it's perfectly normal
        // for enablement properties to be missing
        let info = table_config.in_commit_timestamp_enablement().unwrap();
        assert_eq!(
            info,
            InCommitTimestampEnablement::Enabled { enablement: None }
        );
    }
    #[test]
    fn ict_supported_and_enabled() {
        let schema = StructType::new_unchecked([StructField::nullable("value", DataType::INTEGER)]);
        let metadata = Metadata::try_new(
            None,
            None,
            schema,
            vec![],
            0,
            HashMap::from_iter([
                (
                    "delta.enableInCommitTimestamps".to_string(),
                    "true".to_string(),
                ),
                (
                    "delta.inCommitTimestampEnablementVersion".to_string(),
                    "5".to_string(),
                ),
                (
                    "delta.inCommitTimestampEnablementTimestamp".to_string(),
                    "100".to_string(),
                ),
            ]),
        )
        .unwrap();
        let protocol = Protocol::try_new(
            3,
            7,
            Some::<Vec<String>>(vec![]),
            Some([TableFeature::InCommitTimestamp]),
        )
        .unwrap();
        let table_root = Url::try_from("file:///").unwrap();
        let table_config = TableConfiguration::try_new(metadata, protocol, table_root, 0).unwrap();
        assert!(table_config.is_feature_supported(&TableFeature::InCommitTimestamp));
        assert!(table_config.is_feature_enabled(&TableFeature::InCommitTimestamp));
        let info = table_config.in_commit_timestamp_enablement().unwrap();
        assert_eq!(
            info,
            InCommitTimestampEnablement::Enabled {
                enablement: Some((5, 100))
            }
        )
    }
    #[test]
    fn ict_enabled_with_partial_enablement_info() {
        let schema = StructType::new_unchecked([StructField::nullable("value", DataType::INTEGER)]);
        let metadata = Metadata::try_new(
            None,
            None,
            schema,
            vec![],
            0,
            HashMap::from_iter([
                (
                    "delta.enableInCommitTimestamps".to_string(),
                    "true".to_string(),
                ),
                (
                    "delta.inCommitTimestampEnablementVersion".to_string(),
                    "5".to_string(),
                ),
                // Missing enablement timestamp
            ]),
        )
        .unwrap();
        let protocol = Protocol::try_new(
            3,
            7,
            Some::<Vec<String>>(vec![]),
            Some([TableFeature::InCommitTimestamp]),
        )
        .unwrap();
        let table_root = Url::try_from("file:///").unwrap();
        let table_config = TableConfiguration::try_new(metadata, protocol, table_root, 0).unwrap();
        assert!(table_config.is_feature_supported(&TableFeature::InCommitTimestamp));
        assert!(table_config.is_feature_enabled(&TableFeature::InCommitTimestamp));
        assert!(matches!(
            table_config.in_commit_timestamp_enablement(),
            Err(Error::Generic(msg)) if msg.contains("In-commit timestamp enabled, but enablement timestamp is missing")
        ));
    }
    #[test]
    fn ict_supported_and_not_enabled() {
        let schema = StructType::new_unchecked([StructField::nullable("value", DataType::INTEGER)]);
        let metadata = Metadata::try_new(None, None, schema, vec![], 0, HashMap::new()).unwrap();
        let protocol = Protocol::try_new(
            3,
            7,
            Some::<Vec<String>>(vec![]),
            Some([TableFeature::InCommitTimestamp]),
        )
        .unwrap();
        let table_root = Url::try_from("file:///").unwrap();
        let table_config = TableConfiguration::try_new(metadata, protocol, table_root, 0).unwrap();
        assert!(table_config.is_feature_supported(&TableFeature::InCommitTimestamp));
        assert!(!table_config.is_feature_enabled(&TableFeature::InCommitTimestamp));
        let info = table_config.in_commit_timestamp_enablement().unwrap();
        assert_eq!(info, InCommitTimestampEnablement::NotEnabled);
    }
    #[test]
    fn unknown_reader_feature_rejected_at_construction() {
        // Unknown reader features are rejected by build_effective_features, before the
        // reader/writer list merge destroys provenance (see NOTE on build_effective_features).
        let schema = StructType::new_unchecked([StructField::nullable("value", DataType::INTEGER)]);
        let metadata = Metadata::try_new(None, None, schema, vec![], 0, HashMap::new()).unwrap();
        let protocol = Protocol::try_new(3, 7, Some(["unknown"]), Some(["unknown"])).unwrap();
        let table_root = Url::try_from("file:///").unwrap();
        TableConfiguration::try_new(metadata, protocol, table_root, 0)
            .expect_err("Unknown reader feature should be rejected");
    }

    #[test]
    fn unknown_writer_feature_allows_reads() {
        // Unknown features only on the writer list survive into the effective set.
        // Reads succeed (unknown writer features don't affect reads); writes fail
        // (KernelSupport::NotSupported).
        let schema = StructType::new_unchecked([StructField::nullable("value", DataType::INTEGER)]);
        let metadata = Metadata::try_new(None, None, schema, vec![], 0, HashMap::new()).unwrap();
        let protocol =
            Protocol::try_new(3, 7, Some(Vec::<String>::new()), Some(["unknown"])).unwrap();
        let table_root = Url::try_from("file:///").unwrap();
        let table_config = TableConfiguration::try_new(metadata, protocol, table_root, 0).unwrap();
        table_config
            .check_kernel_capabilities(Operation::Scan)
            .expect("Unknown writer feature should not block reads");
        table_config
            .check_kernel_capabilities(Operation::Write)
            .expect_err("Unknown writer feature should block writes");
    }
    #[test]
    fn dv_not_supported() {
        let schema = StructType::new_unchecked([StructField::nullable("value", DataType::INTEGER)]);
        let metadata = Metadata::try_new(
            None,
            None,
            schema,
            vec![],
            0,
            HashMap::from_iter([("delta.enableChangeDataFeed".to_string(), "true".to_string())]),
        )
        .unwrap();
        let protocol = Protocol::try_new(
            3,
            7,
            Some([TableFeature::TimestampWithoutTimezone]),
            Some([
                TableFeature::TimestampWithoutTimezone,
                TableFeature::ChangeDataFeed,
            ]),
        )
        .unwrap();
        let table_root = Url::try_from("file:///").unwrap();
        let table_config = TableConfiguration::try_new(metadata, protocol, table_root, 0).unwrap();
        assert!(!table_config.is_feature_supported(&TableFeature::DeletionVectors));
        assert!(!table_config.is_feature_enabled(&TableFeature::DeletionVectors));
    }

    #[test]
    fn test_try_new_from() {
        let schema = StructType::new_unchecked([StructField::nullable("value", DataType::INTEGER)]);
        let metadata = Metadata::try_new(
            None,
            None,
            schema,
            vec![],
            0,
            HashMap::from_iter([("delta.enableChangeDataFeed".to_string(), "true".to_string())]),
        )
        .unwrap();
        let protocol = Protocol::try_new(
            3,
            7,
            Some([TableFeature::DeletionVectors]),
            Some([TableFeature::DeletionVectors, TableFeature::ChangeDataFeed]),
        )
        .unwrap();
        let table_root = Url::try_from("file:///").unwrap();
        let table_config = TableConfiguration::try_new(metadata, protocol, table_root, 0).unwrap();

        let new_schema =
            StructType::new_unchecked([StructField::nullable("value", DataType::INTEGER)]);
        let new_metadata = Metadata::try_new(
            None,
            None,
            new_schema,
            vec![],
            0,
            HashMap::from_iter([
                (
                    "delta.enableChangeDataFeed".to_string(),
                    "false".to_string(),
                ),
                (
                    "delta.enableDeletionVectors".to_string(),
                    "true".to_string(),
                ),
            ]),
        )
        .unwrap();
        let new_protocol = Protocol::try_new(
            3,
            7,
            Some([TableFeature::DeletionVectors, TableFeature::V2Checkpoint]),
            Some([
                TableFeature::DeletionVectors,
                TableFeature::V2Checkpoint,
                TableFeature::AppendOnly,
                TableFeature::ChangeDataFeed,
            ]),
        )
        .unwrap();
        let new_version = 1;
        let new_table_config = TableConfiguration::try_new_from(
            &table_config,
            Some(new_metadata.clone()),
            Some(new_protocol.clone()),
            new_version,
        )
        .unwrap();

        assert_eq!(new_table_config.version(), new_version);
        assert_eq!(new_table_config.metadata(), &new_metadata);
        assert_eq!(new_table_config.protocol(), &new_protocol);
        assert_eq!(new_table_config.schema(), table_config.schema());
        assert_eq!(
            new_table_config.table_properties(),
            &TableProperties {
                enable_change_data_feed: Some(false),
                enable_deletion_vectors: Some(true),
                ..Default::default()
            }
        );
        assert_eq!(
            new_table_config.column_mapping_mode(),
            table_config.column_mapping_mode()
        );
        assert_eq!(new_table_config.table_root(), table_config.table_root());
    }

    #[test]
    fn test_timestamp_ntz_validation_integration() {
        // Schema with TIMESTAMP_NTZ column
        let schema =
            StructType::new_unchecked([StructField::nullable("ts", DataType::TIMESTAMP_NTZ)]);
        let metadata = Metadata::try_new(None, None, schema, vec![], 0, HashMap::new()).unwrap();

        let protocol_without_timestamp_ntz_features = Protocol::try_new(
            3,
            7,
            Some::<Vec<String>>(vec![]),
            Some::<Vec<String>>(vec![]),
        )
        .unwrap();

        let protocol_with_timestamp_ntz_features = Protocol::try_new(
            3,
            7,
            Some([TableFeature::TimestampWithoutTimezone]),
            Some([TableFeature::TimestampWithoutTimezone]),
        )
        .unwrap();

        let table_root = Url::try_from("file:///").unwrap();

        let result = TableConfiguration::try_new(
            metadata.clone(),
            protocol_without_timestamp_ntz_features,
            table_root.clone(),
            0,
        );
        assert_result_error_with_message(
            result,
            "Table has TIMESTAMP_NTZ columns but 'timestampNtz' is not in the protocol",
        );

        let result = TableConfiguration::try_new(
            metadata,
            protocol_with_timestamp_ntz_features,
            table_root,
            0,
        );
        assert!(
            result.is_ok(),
            "Should succeed when TIMESTAMP_NTZ is used with required features"
        );
    }

    #[test]
    fn test_variant_validation_integration() {
        // Schema with VARIANT column
        let schema =
            StructType::new_unchecked([StructField::nullable("v", DataType::unshredded_variant())]);
        let metadata = Metadata::try_new(None, None, schema, vec![], 0, HashMap::new()).unwrap();

        let protocol_without_variant_features = Protocol::try_new(
            3,
            7,
            Some::<Vec<String>>(vec![]),
            Some::<Vec<String>>(vec![]),
        )
        .unwrap();

        let protocol_with_variant_features = Protocol::try_new(
            3,
            7,
            Some([TableFeature::VariantType]),
            Some([TableFeature::VariantType]),
        )
        .unwrap();

        let table_root = Url::try_from("file:///").unwrap();

        let result: Result<TableConfiguration, Error> = TableConfiguration::try_new(
            metadata.clone(),
            protocol_without_variant_features,
            table_root.clone(),
            0,
        );
        assert_result_error_with_message(result, "Table has VARIANT columns but none of [variantType, variantType-preview] are in the protocol");

        let result =
            TableConfiguration::try_new(metadata, protocol_with_variant_features, table_root, 0);
        assert!(
            result.is_ok(),
            "Should succeed when VARIANT is used with required features"
        );
    }

    #[test]
    fn test_is_feature_supported_returns_false_without_info() {
        // is_feature_supported should return false for features without FeatureInfo
        let config = create_mock_table_config(&[], &[TableFeature::DeletionVectors]);
        assert!(!config.is_feature_supported(&TableFeature::unknown("futureFeature")));
    }

    #[test]
    fn test_is_feature_enabled_returns_false_without_info() {
        // is_feature_enabled should return false for features without FeatureInfo
        let config = create_mock_table_config(&[], &[TableFeature::DeletionVectors]);
        assert!(!config.is_feature_enabled(&TableFeature::unknown("futureFeature")));
    }

    #[test]
    fn test_is_feature_supported_writer_only() {
        // AppendOnly: writer-only, min_writer=2, presence_check on delta.appendOnly property
        let feature = TableFeature::AppendOnly;

        // Non-legacy (3,7): AppendOnly in feature list → supported
        let config = create_mock_table_config(&[], &[TableFeature::AppendOnly]);
        assert!(config.is_feature_supported(&feature));

        // Non-legacy (3,7): AppendOnly NOT in list → not supported
        let config = create_mock_table_config(&[], &[TableFeature::DeletionVectors]);
        assert!(!config.is_feature_supported(&feature));

        // Legacy (1,2) with presence (property set) → inferred from version, supported
        let config = create_mock_table_config_with_version(&["delta.appendOnly"], None, 1, 2);
        assert!(config.is_feature_supported(&feature));

        // Legacy (1,2) without presence → still supported (version is sufficient)
        let config = create_mock_table_config_with_version(&[], None, 1, 2);
        assert!(config.is_feature_supported(&feature));

        // Asymmetric (2,7): AppendOnly in writer list → supported
        let config =
            create_mock_table_config_with_version(&[], Some(&[TableFeature::AppendOnly]), 2, 7);
        assert!(config.is_feature_supported(&feature));

        // Asymmetric (2,7): AppendOnly NOT in writer list → not supported
        let config =
            create_mock_table_config_with_version(&[], Some(&[TableFeature::ChangeDataFeed]), 2, 7);
        assert!(!config.is_feature_supported(&feature));
    }

    #[test]
    fn test_is_feature_supported_reader_writer() {
        // ColumnMapping: reader-writer (not writer_only), min_reader=2, min_writer=5
        let feature = TableFeature::ColumnMapping;

        // Non-legacy (3,7): ColumnMapping in feature list → supported
        let config = create_mock_table_config(&[], &[TableFeature::ColumnMapping]);
        assert!(config.is_feature_supported(&feature));

        // Non-legacy (3,7): ColumnMapping NOT in list → not supported
        let config = create_mock_table_config(&[], &[TableFeature::DeletionVectors]);
        assert!(!config.is_feature_supported(&feature));

        // Asymmetric: reader=2 (legacy), writer=7 with ColumnMapping in writer list
        // ColumnMapping seeded from writer list → in effective set
        let config =
            create_mock_table_config_with_version(&[], Some(&[TableFeature::ColumnMapping]), 2, 7);
        assert!(config.is_feature_supported(&feature));

        // Asymmetric: reader=2, writer=7 with only AppendOnly in writer list
        // ColumnMapping not seeded, no CM metadata present → not inferred → not supported
        let config =
            create_mock_table_config_with_version(&[], Some(&[TableFeature::AppendOnly]), 2, 7);
        assert!(!config.is_feature_supported(&feature));
    }

    #[test]
    fn test_is_feature_enabled_with_enablement_check() {
        // AppendOnly: writer-only, EnabledIf(append_only == Some(true))
        let feature = TableFeature::AppendOnly;

        // Supported (in list) but property NOT set → supported but NOT enabled
        let config = create_mock_table_config(&[], &[TableFeature::AppendOnly]);
        assert!(config.is_feature_supported(&feature));
        assert!(!config.is_feature_enabled(&feature));

        // Supported (in list) AND property set → supported AND enabled
        let config = create_mock_table_config(&["delta.appendOnly"], &[TableFeature::AppendOnly]);
        assert!(config.is_feature_supported(&feature));
        assert!(config.is_feature_enabled(&feature));

        // Not supported (not in list) → neither supported nor enabled
        let config = create_mock_table_config(&[], &[TableFeature::DeletionVectors]);
        assert!(!config.is_feature_supported(&feature));
        assert!(!config.is_feature_enabled(&feature));
    }

    #[test]
    fn test_is_feature_enabled_always_if_supported() {
        // V2Checkpoint: reader-writer, AlwaysIfSupported
        let feature = TableFeature::V2Checkpoint;

        // Supported (in list) → automatically enabled
        let config = create_mock_table_config(&[], &[TableFeature::V2Checkpoint]);
        assert!(config.is_feature_supported(&feature));
        assert!(config.is_feature_enabled(&feature));

        // Not supported (not in list) → neither supported nor enabled
        let config = create_mock_table_config(&[], &[TableFeature::DeletionVectors]);
        assert!(!config.is_feature_supported(&feature));
        assert!(!config.is_feature_enabled(&feature));
    }

    #[test]
    fn test_check_kernel_capabilities_reads() {
        let config = create_mock_table_config(&[], &[]);
        assert!(config.check_kernel_capabilities(Operation::Scan).is_ok());

        let config = create_mock_table_config(&[], &[TableFeature::V2Checkpoint]);
        assert!(config.check_kernel_capabilities(Operation::Scan).is_ok());

        let config = create_mock_table_config_with_version(&[], None, 1, 2);
        assert!(config.check_kernel_capabilities(Operation::Scan).is_ok());

        let config = create_mock_table_config_with_version(
            &[],
            Some(&[TableFeature::InCommitTimestamp]),
            2,
            7,
        );
        assert!(config.check_kernel_capabilities(Operation::Scan).is_ok());
    }

    #[test]
    fn test_check_kernel_capabilities_writes() {
        let config = create_mock_table_config(
            &[],
            &[
                TableFeature::AppendOnly,
                TableFeature::DeletionVectors,
                TableFeature::DomainMetadata,
                TableFeature::Invariants,
                TableFeature::RowTracking,
            ],
        );
        assert!(config.check_kernel_capabilities(Operation::Write).is_ok());

        // Type Widening is not supported for writes
        let config = create_mock_table_config(&[], &[TableFeature::TypeWidening]);
        assert_result_error_with_message(
            config.check_kernel_capabilities(Operation::Write),
            r#"Feature 'typeWidening' is not supported for writes"#,
        );
    }

    #[test]
    fn test_illegal_writer_feature_combination() {
        let schema = StructType::new_unchecked([StructField::nullable("value", DataType::INTEGER)]);
        let metadata = Metadata::try_new(None, None, schema, vec![], 0, HashMap::new()).unwrap();
        let protocol = Protocol::try_new(
            3,
            7,
            Some::<Vec<String>>(vec![]),
            Some(vec![TableFeature::RowTracking]),
        )
        .unwrap();
        let table_root = Url::try_from("file:///").unwrap();
        assert_result_error_with_message(
            TableConfiguration::try_new(metadata, protocol, table_root, 0),
            "Feature 'rowTracking' requires 'domainMetadata' to be enabled",
        );
    }

    #[test]
    fn test_row_tracking_with_domain_metadata_requirement() {
        let schema = StructType::new_unchecked([StructField::nullable("value", DataType::INTEGER)]);
        let metadata = Metadata::try_new(None, None, schema, vec![], 0, HashMap::new()).unwrap();
        let protocol = Protocol::try_new(
            3,
            7,
            Some::<Vec<String>>(vec![]),
            Some(vec![
                TableFeature::RowTracking,
                TableFeature::DomainMetadata,
            ]),
        )
        .unwrap();
        let table_root = Url::try_from("file:///").unwrap();
        let config = TableConfiguration::try_new(metadata, protocol, table_root, 0).unwrap();
        assert!(
            config.check_kernel_capabilities(Operation::Write).is_ok(),
            "RowTracking with DomainMetadata should be supported for writes"
        );
    }

    #[cfg(feature = "catalog-managed")]
    #[test]
    fn test_catalog_managed_writes() {
        let config = create_mock_table_config(&[], &[TableFeature::CatalogManaged]);
        assert!(config.check_kernel_capabilities(Operation::Write).is_ok());

        let config = create_mock_table_config(&[], &[TableFeature::CatalogOwnedPreview]);
        assert!(config.check_kernel_capabilities(Operation::Write).is_ok());
    }

    /// Helper to create a schema with column mapping metadata using JSON deserialization
    fn schema_with_column_mapping() -> StructType {
        let field_a: StructField = serde_json::from_str(
            r#"{
                "name": "col_a",
                "type": "long",
                "nullable": true,
                "metadata": {
                    "delta.columnMapping.id": 1,
                    "delta.columnMapping.physicalName": "phys_col_a"
                }
            }"#,
        )
        .unwrap();

        let field_b: StructField = serde_json::from_str(
            r#"{
                "name": "col_b",
                "type": "string",
                "nullable": true,
                "metadata": {
                    "delta.columnMapping.id": 2,
                    "delta.columnMapping.physicalName": "phys_col_b"
                }
            }"#,
        )
        .unwrap();

        StructType::new_unchecked([field_a, field_b])
    }

    fn create_table_config_with_column_mapping(
        schema: StructType,
        column_mapping_mode: &str,
    ) -> TableConfiguration {
        let mut props = HashMap::new();
        props.insert(
            "delta.columnMapping.mode".to_string(),
            column_mapping_mode.to_string(),
        );

        let metadata = Metadata::try_new(None, None, schema, vec![], 0, props).unwrap();

        // Use reader version 2 which supports column mapping
        let protocol = Protocol::try_new(2, 5, None::<Vec<String>>, None::<Vec<String>>).unwrap();
        let table_root = Url::try_from("file:///").unwrap();
        TableConfiguration::try_new(metadata, protocol, table_root, 0).unwrap()
    }

    #[test]
    fn test_build_expected_stats_schemas_no_column_mapping() {
        // Without column mapping, logical and physical schemas should be identical
        let schema = StructType::new_unchecked([
            StructField::nullable("col_a", DataType::LONG),
            StructField::nullable("col_b", DataType::STRING),
        ]);
        let metadata = Metadata::try_new(None, None, schema, vec![], 0, HashMap::new()).unwrap();
        let protocol = Protocol::try_new(1, 2, None::<Vec<String>>, None::<Vec<String>>).unwrap();
        let table_root = Url::try_from("file:///").unwrap();
        let config = TableConfiguration::try_new(metadata, protocol, table_root, 0).unwrap();

        assert_eq!(config.column_mapping_mode(), ColumnMappingMode::None);

        let stats_schemas = config.build_expected_stats_schemas(None).unwrap();

        // Both schemas should be identical (same Arc)
        assert!(Arc::ptr_eq(&stats_schemas.logical, &stats_schemas.physical));

        // Verify field names are logical names
        let min_values = stats_schemas
            .logical
            .field("minValues")
            .unwrap()
            .data_type();
        if let DataType::Struct(inner) = min_values {
            assert!(inner.field("col_a").is_some());
            assert!(inner.field("col_b").is_some());
        } else {
            panic!("Expected minValues to be a struct");
        }
    }

    #[test]
    fn test_build_expected_stats_schemas_with_column_mapping() {
        // With column mapping, logical schema should have logical names,
        // physical schema should have physical names
        let schema = schema_with_column_mapping();
        let config = create_table_config_with_column_mapping(schema, "name");

        assert_eq!(config.column_mapping_mode(), ColumnMappingMode::Name);

        let stats_schemas = config.build_expected_stats_schemas(None).unwrap();

        // Schemas should be different (not the same Arc)
        assert!(!Arc::ptr_eq(
            &stats_schemas.logical,
            &stats_schemas.physical
        ));

        // Verify logical schema has logical names
        let logical_min_values = stats_schemas
            .logical
            .field("minValues")
            .unwrap()
            .data_type();
        if let DataType::Struct(inner) = logical_min_values {
            assert!(
                inner.field("col_a").is_some(),
                "Logical schema should have col_a"
            );
            assert!(
                inner.field("col_b").is_some(),
                "Logical schema should have col_b"
            );
            assert!(inner.field("phys_col_a").is_none());
        } else {
            panic!("Expected minValues to be a struct");
        }

        // Verify physical schema has physical names
        let physical_min_values = stats_schemas
            .physical
            .field("minValues")
            .unwrap()
            .data_type();
        if let DataType::Struct(inner) = physical_min_values {
            assert!(
                inner.field("phys_col_a").is_some(),
                "Physical schema should have phys_col_a"
            );
            assert!(
                inner.field("phys_col_b").is_some(),
                "Physical schema should have phys_col_b"
            );
            assert!(inner.field("col_a").is_none());
        } else {
            panic!("Expected minValues to be a struct");
        }
    }

    #[test]
    fn test_stats_column_names_returns_logical_names() {
        // stats_column_names should return logical column names
        let schema = schema_with_column_mapping();
        let config = create_table_config_with_column_mapping(schema, "name");

        let column_names = config.stats_column_names(None);

        // Should return logical names, not physical names
        assert!(column_names.contains(&ColumnName::new(["col_a"])));
        assert!(column_names.contains(&ColumnName::new(["col_b"])));
        assert!(!column_names.contains(&ColumnName::new(["phys_col_a"])));
        assert!(!column_names.contains(&ColumnName::new(["phys_col_b"])));
    }
}
