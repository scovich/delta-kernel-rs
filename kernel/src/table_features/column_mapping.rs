//! Code to handle column mapping, including modes and schema transforms
use super::TableFeature;
use crate::schema::{
    ColumnName, DataType, MetadataValue, Schema, SchemaTransform, StructField, StructType,
};
use crate::table_configuration::TableConfiguration;
use crate::utils::require;
use crate::{DeltaResult, Error};

use std::borrow::Cow;

use serde::{Deserialize, Serialize};
use strum::EnumString;

/// Modes of column mapping a table can be in
#[derive(Debug, EnumString, Serialize, Deserialize, Copy, Clone, PartialEq, Eq)]
#[strum(serialize_all = "camelCase")]
#[serde(rename_all = "camelCase")]
pub enum ColumnMappingMode {
    /// No column mapping is applied
    None,
    /// Columns are mapped by their field_id in parquet
    Id,
    /// Columns are mapped to a physical name
    Name,
}

/// Validates that the column mapping mode declared by table properties is supported by the
/// protocol, and that the schema annotations are consistent with that mode.
pub(crate) fn validate_column_mapping(tc: &TableConfiguration) -> DeltaResult<()> {
    let mode = tc.column_mapping_mode();
    if mode != ColumnMappingMode::None {
        let supported = match tc.protocol().min_reader_version() {
            2 => true,
            3 => tc.is_feature_supported(&TableFeature::ColumnMapping),
            _ => false,
        };
        require!(
            supported,
            Error::invalid_column_mapping_mode(format!(
                "Column mapping mode '{mode:?}' is set but the protocol does not support column mapping"
            ))
        );
    }
    validate_schema_column_mapping(&tc.schema(), mode)
}

/// When column mapping mode is enabled, verify that each field in the schema is annotated with a
/// physical name and field_id; when not enabled, verify that no fields are annotated.
fn validate_schema_column_mapping(schema: &Schema, mode: ColumnMappingMode) -> DeltaResult<()> {
    let mut validator = ValidateColumnMappings {
        mode,
        path: vec![],
        err: None,
    };
    let _ = validator.transform_struct(schema);
    match validator.err {
        Some(err) => Err(err),
        None => Ok(()),
    }
}

struct ValidateColumnMappings<'a> {
    mode: ColumnMappingMode,
    path: Vec<&'a str>,
    err: Option<Error>,
}

impl<'a> ValidateColumnMappings<'a> {
    fn transform_inner_type(
        &mut self,
        data_type: &'a DataType,
        name: &'a str,
    ) -> Option<Cow<'a, DataType>> {
        if self.err.is_none() {
            self.path.push(name);
            let _ = self.transform(data_type);
            self.path.pop();
        }
        None
    }
    fn check_annotations(&mut self, field: &StructField) {
        // The iterator yields `&&str` but `ColumnName::new` needs `&str`
        let column_name = || ColumnName::new(self.path.iter().copied());
        let annotation = "delta.columnMapping.physicalName";
        match (self.mode, field.metadata.get(annotation)) {
            // Both Id and Name modes require a physical name annotation; None mode forbids it.
            (ColumnMappingMode::None, None) => {}
            (ColumnMappingMode::Name | ColumnMappingMode::Id, Some(MetadataValue::String(_))) => {}
            (ColumnMappingMode::Name | ColumnMappingMode::Id, Some(_)) => {
                self.err = Some(Error::invalid_column_mapping_mode(format!(
                    "The {annotation} annotation on field '{}' must be a string",
                    column_name()
                )));
            }
            (ColumnMappingMode::Name | ColumnMappingMode::Id, None) => {
                self.err = Some(Error::invalid_column_mapping_mode(format!(
                    "Column mapping is enabled but field '{}' lacks the {annotation} annotation",
                    column_name()
                )));
            }
            (ColumnMappingMode::None, Some(_)) => {
                self.err = Some(Error::invalid_column_mapping_mode(format!(
                    "Column mapping is not enabled but field '{annotation}' is annotated with {}",
                    column_name()
                )));
            }
        }

        let annotation = "delta.columnMapping.id";
        match (self.mode, field.metadata.get(annotation)) {
            // Both Id and Name modes require a field ID annotation; None mode forbids it.
            (ColumnMappingMode::None, None) => {}
            (ColumnMappingMode::Name | ColumnMappingMode::Id, Some(MetadataValue::Number(_))) => {}
            (ColumnMappingMode::Name | ColumnMappingMode::Id, Some(_)) => {
                self.err = Some(Error::invalid_column_mapping_mode(format!(
                    "The {annotation} annotation on field '{}' must be a number",
                    column_name()
                )));
            }
            (ColumnMappingMode::Name | ColumnMappingMode::Id, None) => {
                self.err = Some(Error::invalid_column_mapping_mode(format!(
                    "Column mapping is enabled but field '{}' lacks the {annotation} annotation",
                    column_name()
                )));
            }
            (ColumnMappingMode::None, Some(_)) => {
                self.err = Some(Error::invalid_column_mapping_mode(format!(
                    "Column mapping is not enabled but field '{}' is annotated with {annotation}",
                    column_name()
                )));
            }
        }
    }
}

impl<'a> SchemaTransform<'a> for ValidateColumnMappings<'a> {
    // Override array element and map key/value for better error messages
    fn transform_array_element(&mut self, etype: &'a DataType) -> Option<Cow<'a, DataType>> {
        self.transform_inner_type(etype, "<array element>")
    }
    fn transform_map_key(&mut self, ktype: &'a DataType) -> Option<Cow<'a, DataType>> {
        self.transform_inner_type(ktype, "<map key>")
    }
    fn transform_map_value(&mut self, vtype: &'a DataType) -> Option<Cow<'a, DataType>> {
        self.transform_inner_type(vtype, "<map value>")
    }
    fn transform_struct_field(&mut self, field: &'a StructField) -> Option<Cow<'a, StructField>> {
        if self.err.is_none() {
            self.path.push(&field.name);
            self.check_annotations(field);
            let _ = self.recurse_into_struct_field(field);
            self.path.pop();
        }
        None
    }
    fn transform_variant(&mut self, _: &'a StructType) -> Option<Cow<'a, StructType>> {
        // don't recurse into variant's fields, as they are not expected to have column mapping
        // annotations
        // TODO: this changes with icebergcompat right? see issue#1125 for icebergcompat.
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::actions::{Metadata, Protocol};
    use crate::schema::StructType;
    use crate::utils::test_utils::assert_result_error_with_message;
    use std::collections::HashMap;

    #[test]
    fn test_column_mapping_mode_requires_protocol_support() {
        let annotated_schema = create_schema("5", "\"col-a7f4159c\"", "4", "\"col-5f422f40\"");
        let cmm_props = HashMap::from([("delta.columnMapping.mode".to_string(), "id".to_string())]);
        let table_root = url::Url::parse("file:///").unwrap();

        // v3 + ColumnMapping feature + property=id → succeeds
        let metadata = Metadata::try_new(
            None,
            None,
            annotated_schema.clone(),
            vec![],
            0,
            cmm_props.clone(),
        )
        .unwrap();
        let protocol = Protocol::try_new(
            3,
            7,
            Some([TableFeature::ColumnMapping]),
            Some([TableFeature::ColumnMapping]),
        )
        .unwrap();
        let tc = TableConfiguration::try_new(metadata, protocol, table_root.clone(), 0).unwrap();
        assert_eq!(tc.column_mapping_mode(), ColumnMappingMode::Id);

        // v3 + no ColumnMapping feature + property=id → error
        let metadata = Metadata::try_new(
            None,
            None,
            annotated_schema.clone(),
            vec![],
            0,
            cmm_props.clone(),
        )
        .unwrap();
        let protocol = Protocol::try_new(
            3,
            7,
            Some::<Vec<String>>(vec![]),
            Some::<Vec<String>>(vec![]),
        )
        .unwrap();
        assert_result_error_with_message(
            TableConfiguration::try_new(metadata, protocol, table_root.clone(), 0),
            "Column mapping mode",
        );

        // v3 + wrong feature only + property=id → error
        let metadata =
            Metadata::try_new(None, None, annotated_schema, vec![], 0, cmm_props).unwrap();
        let protocol = Protocol::try_new(
            3,
            7,
            Some([TableFeature::DeletionVectors]),
            Some([TableFeature::DeletionVectors]),
        )
        .unwrap();
        assert_result_error_with_message(
            TableConfiguration::try_new(metadata, protocol, table_root, 0),
            "Column mapping mode",
        );
    }

    // Creates optional schema field annotations for column mapping id and physical name, as a string.
    fn create_annotations<'a>(
        id: impl Into<Option<&'a str>>,
        name: impl Into<Option<&'a str>>,
    ) -> String {
        let mut annotations = vec![];
        if let Some(id) = id.into() {
            annotations.push(format!("\"delta.columnMapping.id\": {id}"));
        }
        if let Some(name) = name.into() {
            annotations.push(format!("\"delta.columnMapping.physicalName\": {name}"));
        }
        annotations.join(", ")
    }

    // Creates a generic schema with optional field annotations for column mapping id and physical name.
    fn create_schema<'a>(
        inner_id: impl Into<Option<&'a str>>,
        inner_name: impl Into<Option<&'a str>>,
        outer_id: impl Into<Option<&'a str>>,
        outer_name: impl Into<Option<&'a str>>,
    ) -> StructType {
        let schema = format!(
            r#"
        {{
            "name": "e",
            "type": {{
                "type": "array",
                "elementType": {{
                    "type": "struct",
                    "fields": [
                        {{
                            "name": "d",
                            "type": "integer",
                            "nullable": false,
                            "metadata": {{ {} }}
                        }}
                    ]
                }},
                "containsNull": true
            }},
            "nullable": true,
            "metadata": {{ {} }}
        }}
        "#,
            create_annotations(inner_id, inner_name),
            create_annotations(outer_id, outer_name)
        );
        println!("{schema}");
        StructType::new_unchecked([serde_json::from_str(&schema).unwrap()])
    }

    #[test]
    fn test_column_mapping_enabled() {
        [ColumnMappingMode::Name, ColumnMappingMode::Id]
            .into_iter()
            .for_each(|mode| {
                let schema = create_schema("5", "\"col-a7f4159c\"", "4", "\"col-5f422f40\"");
                validate_schema_column_mapping(&schema, mode).unwrap();

                // missing annotation
                let schema = create_schema(None, "\"col-a7f4159c\"", "4", "\"col-5f422f40\"");
                validate_schema_column_mapping(&schema, mode).expect_err("missing field id");
                let schema = create_schema("5", None, "4", "\"col-5f422f40\"");
                validate_schema_column_mapping(&schema, mode).expect_err("missing field name");
                let schema = create_schema("5", "\"col-a7f4159c\"", None, "\"col-5f422f40\"");
                validate_schema_column_mapping(&schema, mode).expect_err("missing field id");
                let schema = create_schema("5", "\"col-a7f4159c\"", "4", None);
                validate_schema_column_mapping(&schema, mode).expect_err("missing field name");

                // wrong-type field id annotation (string instead of int)
                let schema = create_schema("\"5\"", "\"col-a7f4159c\"", "4", "\"col-5f422f40\"");
                validate_schema_column_mapping(&schema, mode).expect_err("invalid field id");
                let schema = create_schema("5", "\"col-a7f4159c\"", "\"4\"", "\"col-5f422f40\"");
                validate_schema_column_mapping(&schema, mode).expect_err("invalid field id");

                // wrong-type field name annotation (int instead of string)
                let schema = create_schema("5", "555", "4", "\"col-5f422f40\"");
                validate_schema_column_mapping(&schema, mode).expect_err("invalid field name");
                let schema = create_schema("5", "\"col-a7f4159c\"", "4", "444");
                validate_schema_column_mapping(&schema, mode).expect_err("invalid field name");
            });
    }

    #[test]
    fn test_column_mapping_disabled() {
        let schema = create_schema(None, None, None, None);
        validate_schema_column_mapping(&schema, ColumnMappingMode::None).unwrap();

        let schema = create_schema("5", None, None, None);
        validate_schema_column_mapping(&schema, ColumnMappingMode::None).expect_err("field id");
        let schema = create_schema(None, "\"col-a7f4159c\"", None, None);
        validate_schema_column_mapping(&schema, ColumnMappingMode::None).expect_err("field name");
        let schema = create_schema(None, None, "4", None);
        validate_schema_column_mapping(&schema, ColumnMappingMode::None).expect_err("field id");
        let schema = create_schema(None, None, None, "\"col-5f422f40\"");
        validate_schema_column_mapping(&schema, ColumnMappingMode::None).expect_err("field name");
    }

    /// Column mapping mode=none: feature is supported (in protocol) but not enabled.
    /// Requires a plain schema without CM annotations.
    #[test]
    fn test_column_mapping_mode_none_not_enabled() {
        use std::collections::HashMap;

        use url::Url;

        use crate::actions::{Metadata, Protocol};
        use crate::schema::{DataType, StructField, StructType};
        use crate::table_configuration::TableConfiguration;
        use crate::table_features::TableFeature;

        let schema = StructType::new_unchecked([StructField::nullable("value", DataType::INTEGER)]);
        let metadata = Metadata::try_new(
            None,
            None,
            schema,
            vec![],
            0,
            HashMap::from_iter([("delta.columnMapping.mode".to_string(), "none".to_string())]),
        )
        .unwrap();
        let protocol =
            Protocol::try_new(3, 7, Some(["columnMapping"]), Some(["columnMapping"])).unwrap();
        let table_root = Url::try_from("file:///").unwrap();
        let tc = TableConfiguration::try_new(metadata, protocol, table_root, 0).unwrap();

        let feature = TableFeature::ColumnMapping;
        assert!(tc.is_feature_supported(&feature));
        assert!(!tc.is_feature_enabled(&feature));
    }
}
