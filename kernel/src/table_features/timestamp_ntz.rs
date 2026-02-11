//! Validation for TIMESTAMP_NTZ feature support

use super::TableFeature;
use crate::schema::{PrimitiveType, Schema, SchemaTransform};
use crate::table_configuration::TableConfiguration;
use crate::utils::require;
use crate::{DeltaResult, Error};

use std::borrow::Cow;

/// Returns true if the schema (or any nested schema) contains a TIMESTAMP_NTZ column.
pub(crate) fn schema_uses_timestamp_ntz(schema: &Schema) -> bool {
    let mut checker = UsesTimestampNtz(false);
    let _ = checker.transform_struct(schema);
    checker.0
}

/// Validates that if a table schema contains TIMESTAMP_NTZ columns, the table must have the
/// TimestampWithoutTimezone feature in both reader and writer features.
pub(crate) fn validate_timestamp_ntz_feature_support(tc: &TableConfiguration) -> DeltaResult<()> {
    if !tc.is_feature_supported(&TableFeature::TimestampWithoutTimezone) {
        require!(
            !schema_uses_timestamp_ntz(&tc.schema()),
            Error::unsupported(
                "Table contains TIMESTAMP_NTZ columns but does not have the required 'timestampNtz' feature in reader and writer features"
            )
        );
    }
    Ok(())
}

/// Schema visitor that checks if any column in the schema uses TIMESTAMP_NTZ type
struct UsesTimestampNtz(bool);

impl<'a> SchemaTransform<'a> for UsesTimestampNtz {
    fn transform_primitive(&mut self, ptype: &'a PrimitiveType) -> Option<Cow<'a, PrimitiveType>> {
        if *ptype == PrimitiveType::TimestampNtz {
            self.0 = true;
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::actions::Protocol;
    use crate::schema::{DataType, PrimitiveType, StructField, StructType};
    use crate::utils::test_utils::{assert_result_error_with_message, make_test_tc};

    #[test]
    fn test_timestamp_ntz_feature_validation() {
        let schema_with_timestamp_ntz = StructType::new_unchecked([
            StructField::new("id", DataType::INTEGER, false),
            StructField::new("ts", DataType::Primitive(PrimitiveType::TimestampNtz), true),
        ]);

        let schema_without_timestamp_ntz = StructType::new_unchecked([
            StructField::new("id", DataType::INTEGER, false),
            StructField::new("name", DataType::STRING, true),
        ]);

        // Protocol with TimestampWithoutTimezone features
        let protocol_with_features = Protocol::try_new(
            3,
            7,
            Some([TableFeature::TimestampWithoutTimezone]),
            Some([TableFeature::TimestampWithoutTimezone]),
        )
        .unwrap();

        // Protocol without TimestampWithoutTimezone features
        let protocol_without_features = Protocol::try_new(
            3,
            7,
            Some::<Vec<String>>(vec![]),
            Some::<Vec<String>>(vec![]),
        )
        .unwrap();

        // Schema with TIMESTAMP_NTZ + Protocol with features = OK
        make_test_tc(
            schema_with_timestamp_ntz.clone(),
            protocol_with_features.clone(),
        )
        .expect("Should succeed when features are present");

        // Schema without TIMESTAMP_NTZ + Protocol without features = OK
        make_test_tc(
            schema_without_timestamp_ntz.clone(),
            protocol_without_features.clone(),
        )
        .expect("Should succeed when no TIMESTAMP_NTZ columns are present");

        // Schema without TIMESTAMP_NTZ + Protocol with features = OK
        make_test_tc(
            schema_without_timestamp_ntz.clone(),
            protocol_with_features.clone(),
        )
        .expect("Should succeed when no TIMESTAMP_NTZ columns are present, even with features");

        // Schema with TIMESTAMP_NTZ + Protocol without features = ERROR
        let result = make_test_tc(
            schema_with_timestamp_ntz.clone(),
            protocol_without_features.clone(),
        );
        assert_result_error_with_message(result, "Unsupported: Table contains TIMESTAMP_NTZ columns but does not have the required 'timestampNtz' feature in reader and writer features");

        // Nested schema with TIMESTAMP_NTZ
        let nested_schema_with_timestamp_ntz = StructType::new_unchecked([
            StructField::new("id", DataType::INTEGER, false),
            StructField::new(
                "nested",
                DataType::Struct(Box::new(StructType::new_unchecked([StructField::new(
                    "inner_ts",
                    DataType::Primitive(PrimitiveType::TimestampNtz),
                    true,
                )]))),
                true,
            ),
        ]);

        let result = make_test_tc(nested_schema_with_timestamp_ntz, protocol_without_features);
        assert_result_error_with_message(result, "Unsupported: Table contains TIMESTAMP_NTZ columns but does not have the required 'timestampNtz' feature in reader and writer features");
    }
}
