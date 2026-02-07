//! TIMESTAMP_NTZ schema detection

use crate::schema::{PrimitiveType, Schema, SchemaTransform};

use std::borrow::Cow;

/// Returns true if the schema contains any TIMESTAMP_NTZ columns.
pub(crate) fn schema_uses_timestamp_ntz(schema: &Schema) -> bool {
    let mut checker = UsesTimestampNtz(false);
    let _ = checker.transform_struct(schema);
    checker.0
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
    use crate::actions::Protocol;
    use crate::schema::{DataType, PrimitiveType, StructField, StructType};
    use crate::table_features::TableFeature;
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

        let protocol_with_features = Protocol::try_new(
            3,
            7,
            Some([TableFeature::TimestampWithoutTimezone]),
            Some([TableFeature::TimestampWithoutTimezone]),
        )
        .unwrap();

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
        make_test_tc(schema_without_timestamp_ntz, protocol_with_features)
            .expect("Should succeed when no TIMESTAMP_NTZ columns are present, even with features");

        // Schema with TIMESTAMP_NTZ + Protocol without features = ERROR
        let result = make_test_tc(schema_with_timestamp_ntz, protocol_without_features.clone());
        assert_result_error_with_message(
            result,
            "Table has TIMESTAMP_NTZ columns but 'timestampNtz' is not in the protocol",
        );

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
        assert_result_error_with_message(
            result,
            "Table has TIMESTAMP_NTZ columns but 'timestampNtz' is not in the protocol",
        );
    }
}
