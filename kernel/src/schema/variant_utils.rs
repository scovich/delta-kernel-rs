//! Utility functions for the variant type and variant-related table features.

use crate::schema::{Schema, SchemaTransform, StructType};
use crate::table_configuration::TableConfiguration;
use crate::table_features::TableFeature;
use crate::utils::require;
use crate::{DeltaResult, Error};
use std::borrow::Cow;

/// Schema visitor that checks if any column in the schema uses VARIANT type
#[derive(Debug, Default)]
pub(crate) struct UsesVariant(pub(crate) bool);

impl<'a> SchemaTransform<'a> for UsesVariant {
    fn transform_variant(&mut self, _: &'a StructType) -> Option<Cow<'a, StructType>> {
        self.0 = true;
        None
    }
}

/// Returns true if the schema (or any nested schema) contains a VARIANT column.
pub(crate) fn schema_uses_variant(schema: &Schema) -> bool {
    let mut checker = UsesVariant::default();
    let _ = checker.transform_struct(schema);
    checker.0
}

pub(crate) fn validate_variant_type_feature_support(tc: &TableConfiguration) -> DeltaResult<()> {
    // Both the reader and writer need to have either the VariantType or the VariantTypePreview
    // features.
    if !tc.is_feature_supported(&TableFeature::VariantType)
        && !tc.is_feature_supported(&TableFeature::VariantTypePreview)
    {
        require!(
            !schema_uses_variant(&tc.schema()),
            Error::unsupported(
                "Table contains VARIANT columns but does not have the required 'variantType' feature in reader and writer features"
            )
        );
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::actions::Protocol;
    use crate::schema::{DataType, StructField};
    use crate::utils::test_utils::{assert_result_error_with_message, make_test_tc};

    #[test]
    fn test_is_unshredded_variant() {
        fn is_unshredded_variant(s: &DataType) -> bool {
            s == &DataType::unshredded_variant()
        }
        assert!(!is_unshredded_variant(
            &DataType::variant_type([
                StructField::not_null("metadata", DataType::BINARY),
                StructField::nullable("value", DataType::BINARY),
                StructField::nullable("another_field", DataType::BINARY),
            ])
            .unwrap()
        ));
        assert!(is_unshredded_variant(
            &DataType::variant_type([
                StructField::not_null("metadata", DataType::BINARY),
                StructField::not_null("value", DataType::BINARY),
            ])
            .unwrap()
        ));
    }

    #[test]
    fn test_variant_feature_validation() {
        let features = [
            (TableFeature::VariantType, TableFeature::VariantType),
            (
                TableFeature::VariantTypePreview,
                TableFeature::VariantTypePreview,
            ),
        ];
        let schema_with_variant = StructType::new_unchecked([
            StructField::new("id", DataType::INTEGER, false),
            StructField::new("v", DataType::unshredded_variant(), true),
        ]);

        let schema_without_variant = StructType::new_unchecked([
            StructField::new("id", DataType::INTEGER, false),
            StructField::new("name", DataType::STRING, true),
        ]);

        // Nested schema with VARIANT
        let nested_schema_with_variant = StructType::new_unchecked([
            StructField::new("id", DataType::INTEGER, false),
            StructField::new(
                "nested",
                DataType::Struct(Box::new(StructType::new_unchecked([StructField::new(
                    "inner_v",
                    DataType::unshredded_variant(),
                    true,
                )]))),
                true,
            ),
        ]);
        features
            .iter()
            .for_each(|(variant_reader, variant_writer)| {
                // Protocol with variantType features
                let protocol_with_features =
                    Protocol::try_new(3, 7, Some([variant_reader]), Some([variant_writer]))
                        .unwrap();

                // Protocol without variantType features
                let protocol_without_features = Protocol::try_new(
                    3,
                    7,
                    Some::<Vec<String>>(vec![]),
                    Some::<Vec<String>>(vec![]),
                )
                .unwrap();

                // Since variant features are ReaderWriter feature, protocol that
                // lists a variant feature in only one of reader/writer feature = ERR
                let protocol_without_writer_feature =
                    Protocol::try_new(3, 7, Some([variant_reader]), Some::<Vec<String>>(vec![]));
                assert_result_error_with_message(protocol_without_writer_feature,
                    "Reader features must contain only ReaderWriter features that are also listed in writer features");

                let protocol_without_reader_feature =
                    Protocol::try_new(3, 7, Some::<Vec<String>>(vec![]), Some([variant_writer]));
                assert_result_error_with_message(protocol_without_reader_feature,
                    "Writer features must be Writer-only or also listed in reader features");

                // Schema with VARIANT + Protocol with features = OK
                make_test_tc(schema_with_variant.clone(), protocol_with_features.clone())
                    .expect("Should succeed when features are present");

                // Schema without VARIANT + Protocol without features = OK
                make_test_tc(schema_without_variant.clone(), protocol_without_features.clone())
                    .expect("Should succeed when no VARIANT columns are present");

                // Schema without VARIANT + Protocol with features = OK
                make_test_tc(schema_without_variant.clone(), protocol_with_features)
                    .expect("Should succeed when no VARIANT columns are present, even with features");

                // Schema with VARIANT + Protocol without features = ERROR
                let result = make_test_tc(schema_with_variant.clone(), protocol_without_features.clone());
                assert_result_error_with_message(result, "Unsupported: Table contains VARIANT columns but does not have the required 'variantType' feature in reader and writer features");

                let result = make_test_tc(nested_schema_with_variant.clone(), protocol_without_features);
                assert_result_error_with_message(result, "Unsupported: Table contains VARIANT columns but does not have the required 'variantType' feature in reader and writer features");
            });
    }
}
