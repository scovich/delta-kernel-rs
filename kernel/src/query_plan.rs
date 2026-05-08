use url::Url;

use crate::expressions::ColumnName;
use crate::{DeltaResult, Error, ExpressionRef, FileMeta, PredicateRef, Scalar, SchemaRef};

/// A relation reference within a [`QueryPlan`].
///
/// [`RefId`] values are produced by [`QueryPlanBuilder`] and identify the output relation of a
/// specific plan operation, which subsequent operations can use as input. Applying a [`RefId`]
/// to a plan builder that did not produce it is a logic error.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RefId(u32);

/// A declarative plan operation node to be executed by an [`Engine`].
///
/// This API is experimental and intentionally minimal. Each variant defines its expected output
/// schema contract in the variant docs.
///
/// [`Engine`]: crate::Engine
#[derive(Debug, Clone, PartialEq)]
pub enum PlanOp {
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

    /// Project input rows into a new relation.
    ///
    /// `expression` is evaluated against each input row and must produce one struct value.
    /// `output_schema` defines the names and types of that struct's top-level and nested fields.
    Project {
        expression: ExpressionRef,
        output_schema: SchemaRef,
    },

    /// Filter input rows by `predicate`, dropping all rows where it does not evaluate to TRUE.
    Filter { predicate: PredicateRef },

    /// Concatenate inputs without deduplication.
    ///
    /// All inputs must have identical output schemas. This node does not impose any row-ordering
    /// guarantees.
    UnionAll,

    /// Aggregate over input rows, optionally grouped by `group_by_columns`.
    ///
    /// Semantics are equivalent to the following SQL query:
    /// ```sql
    /// SELECT
    ///   group_column_1,
    ///     ...
    ///   group_column_k,
    ///   max_by(value_column_1, version_column) FILTER (WHERE value_column_1 IS NOT NULL),
    ///     ...
    ///   max_by(value_column_N, version_column) FILTER (WHERE value_column_N IS NOT NULL)
    /// FROM input
    /// GROUP BY
    ///   group_column_1,
    ///     ...
    ///   group_column_k
    /// ```
    ///
    /// If `group_by_columns` is empty, the output has exactly one row. If no row satisfies
    /// a value column's `IS NOT NULL` filter (including empty input), that output value is `NULL`.
    ///
    /// If `group_by_columns` is non-empty, output rows are grouped by those columns. If the input
    /// is empty, the output is also empty.
    ///
    /// The output consists of `group_by_columns` followed by `value_columns`, in listed order.
    ///
    /// Output column names are defined by `output_schema` and must follow that same order.
    MaxByVersion {
        group_by_columns: Vec<String>,
        version_column: String,
        value_columns: Vec<String>,
        output_schema: SchemaRef,
    },

    /// Left equi-anti-join between two input relations, comparing `key_pairs` for equality.
    ///
    /// Rows from the left/probe input side are filtered out if they match any row from the
    /// right/build side, while the remaining rows pass through unchanged with the same schema.
    EquiAntiJoin {
        key_pairs: Vec<(ColumnName, ColumnName)>,
    },
}

impl PlanOp {
    /// Returns a stable operation name for diagnostics.
    pub fn name(&self) -> &'static str {
        match self {
            PlanOp::ListFiles { .. } => "ListFiles",
            PlanOp::ScanJson { .. } => "ScanJson",
            PlanOp::ScanParquet { .. } => "ScanParquet",
            PlanOp::Project { .. } => "Project",
            PlanOp::Filter { .. } => "Filter",
            PlanOp::UnionAll => "UnionAll",
            PlanOp::MaxByVersion { .. } => "MaxByVersion",
            PlanOp::EquiAntiJoin { .. } => "EquiAntiJoin",
        }
    }
}

/// A single SSA-style operation instance inside a [`QueryPlan`].
#[derive(Debug, Clone, PartialEq)]
pub struct PlanNode {
    op: PlanOp,
    inputs: Vec<RefId>,
    output: RefId,
}

impl PlanNode {
    /// Returns the operation kind.
    pub fn op(&self) -> &PlanOp {
        &self.op
    }

    /// Returns operation input references.
    pub fn inputs(&self) -> &[RefId] {
        &self.inputs
    }

    /// Returns the output reference produced by this operation.
    pub fn output(&self) -> RefId {
        self.output
    }
}

/// A declarative SSA query plan to be executed by an [`Engine`].
///
/// A plan is a non-empty sequence of [`PlanNode`] entries, whose output is the output
/// of its last operation.
#[derive(Debug, Clone, PartialEq)]
pub struct QueryPlan {
    nodes: Vec<PlanNode>,
}

impl QueryPlan {
    /// Returns the nodes in execution order.
    pub fn nodes(&self) -> &[PlanNode] {
        &self.nodes
    }
}

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)] // `Two` is reserved for upcoming binary operations.
enum InputArity {
    Zero,
    One,
    Two,
    Variadic,
}

impl PlanOp {
    fn input_arity(&self) -> InputArity {
        match self {
            PlanOp::ListFiles { .. } => InputArity::Zero,
            PlanOp::ScanJson { .. } => InputArity::Zero,
            PlanOp::ScanParquet { .. } => InputArity::Zero,
            PlanOp::Project { .. } => InputArity::One,
            PlanOp::Filter { .. } => InputArity::One,
            PlanOp::UnionAll => InputArity::Variadic,
            PlanOp::MaxByVersion { .. } => InputArity::One,
            PlanOp::EquiAntiJoin { .. } => InputArity::Two,
        }
    }
}

/// Builder for constructing validated SSA [`QueryPlan`]s.
///
/// Every appended operation returns a new [`RefId`] that can be used as input to later operations.
/// Applying a [`RefId`] not produced by this builder is a logic error.
#[derive(Debug, Default)]
pub struct QueryPlanBuilder {
    nodes: Vec<PlanNode>,
    next_ref: u32,
}

impl QueryPlanBuilder {
    /// Creates an empty builder.
    pub fn new() -> Self {
        Self::default()
    }

    /// Appends a [`PlanOp::ListFiles`] source.
    pub fn list_files(&mut self, start_from: Url) -> DeltaResult<RefId> {
        self.append(PlanOp::ListFiles { start_from }, [])
    }

    /// Appends a [`PlanOp::ScanJson`] from non-empty `files`, or returns `None`.
    pub fn scan_json(
        &mut self,
        files: Vec<(FileMeta, Vec<Scalar>)>,
        metadata_columns: Vec<String>,
        physical_schema: SchemaRef,
    ) -> DeltaResult<Option<RefId>> {
        if files.is_empty() {
            return Ok(None);
        }
        Ok(Some(self.append(
            PlanOp::ScanJson {
                files,
                metadata_columns,
                physical_schema,
            },
            [],
        )?))
    }

    /// Appends a [`PlanOp::ScanParquet`] from non-empty `files`, or returns `None`.
    pub fn scan_parquet(
        &mut self,
        files: Vec<(FileMeta, Vec<Scalar>)>,
        metadata_columns: Vec<String>,
        physical_schema: SchemaRef,
    ) -> DeltaResult<Option<RefId>> {
        if files.is_empty() {
            return Ok(None);
        }
        Ok(Some(self.append(
            PlanOp::ScanParquet {
                files,
                metadata_columns,
                physical_schema,
            },
            [],
        )?))
    }

    /// Appends a [`PlanOp::Project`] over `input`.
    pub fn project(
        &mut self,
        input: RefId,
        expression: ExpressionRef,
        output_schema: SchemaRef,
    ) -> DeltaResult<RefId> {
        self.append(
            PlanOp::Project {
                expression,
                output_schema,
            },
            [input],
        )
    }

    /// Appends a [`PlanOp::Filter`] over `input`.
    pub fn filter(&mut self, input: RefId, predicate: PredicateRef) -> DeltaResult<RefId> {
        self.append(PlanOp::Filter { predicate }, [input])
    }

    /// Appends a [`PlanOp::UnionAll`] over `inputs`, with normalization.
    ///
    /// Returns:
    /// - `Some(new_output_ref)` representing the union of two or more relations
    /// - `Some(input_ref)` when `inputs` has exactly one relation
    /// - `None` when `inputs` is empty
    pub fn union_all(
        &mut self,
        inputs: impl IntoIterator<Item = RefId>,
    ) -> DeltaResult<Option<RefId>> {
        let mut inputs: Vec<RefId> = inputs.into_iter().collect();
        match inputs.len() {
            0 | 1 => Ok(inputs.pop()), // None if empty, as desired
            _ => Ok(Some(self.append(PlanOp::UnionAll, inputs)?)),
        }
    }

    /// Appends a [`PlanOp::MaxByVersion`] over `input`.
    pub fn max_by_version(
        &mut self,
        input: RefId,
        version_column: impl Into<String>,
        value_columns: impl IntoIterator<Item = impl Into<String>>,
        output_schema: SchemaRef,
    ) -> DeltaResult<RefId> {
        self.max_by_version_grouped(
            input,
            std::iter::empty::<String>(),
            version_column,
            value_columns,
            output_schema,
        )
    }

    /// Appends a grouped [`PlanOp::MaxByVersion`] over `input` and `group_by_columns`.
    pub fn max_by_version_grouped(
        &mut self,
        input: RefId,
        group_by_columns: impl IntoIterator<Item = impl Into<String>>,
        version_column: impl Into<String>,
        value_columns: impl IntoIterator<Item = impl Into<String>>,
        output_schema: SchemaRef,
    ) -> DeltaResult<RefId> {
        self.append(
            PlanOp::MaxByVersion {
                group_by_columns: group_by_columns.into_iter().map(Into::into).collect(),
                version_column: version_column.into(),
                value_columns: value_columns.into_iter().map(Into::into).collect(),
                output_schema,
            },
            [input],
        )
    }

    /// Appends a [`PlanOp::EquiAntiJoin`] with left/probe `left` and right/build `right`.
    pub fn equi_anti_join(
        &mut self,
        left: RefId,
        right: RefId,
        key_pairs: impl IntoIterator<Item = (impl Into<ColumnName>, impl Into<ColumnName>)>,
    ) -> DeltaResult<RefId> {
        self.append(
            PlanOp::EquiAntiJoin {
                key_pairs: key_pairs
                    .into_iter()
                    .map(|(left_key, right_key)| (left_key.into(), right_key.into()))
                    .collect(),
            },
            [left, right],
        )
    }

    /// Appends one operation to this plan and returns its output [`RefId`].
    pub fn append(
        &mut self,
        op: PlanOp,
        inputs: impl IntoIterator<Item = RefId>,
    ) -> DeltaResult<RefId> {
        let inputs: Vec<RefId> = inputs.into_iter().collect();
        let exact_arity = match op.input_arity() {
            InputArity::Zero => Some(0),
            InputArity::One => Some(1),
            InputArity::Two => Some(2),
            InputArity::Variadic => None,
        };
        if let Some(exact_arity) = exact_arity {
            if inputs.len() != exact_arity {
                return Err(Error::generic(format!(
                    "Plan op {} requires exactly {exact_arity} input(s), got {}",
                    op.name(),
                    inputs.len()
                )));
            }
        }

        for input in &inputs {
            if input.0 >= self.next_ref {
                return Err(Error::generic(format!(
                    "Invalid input reference {}: this builder has only produced {} relation(s)",
                    input.0, self.next_ref
                )));
            }
        }

        let output = RefId(self.next_ref);
        self.next_ref = self
            .next_ref
            .checked_add(1)
            .ok_or_else(|| Error::internal_error("QueryPlan ref id overflowed u32"))?;
        self.nodes.push(PlanNode { op, inputs, output });
        Ok(output)
    }

    /// Finalizes and returns a validated non-empty [`QueryPlan`].
    pub fn finish(self) -> DeltaResult<QueryPlan> {
        if self.nodes.is_empty() {
            return Err(Error::generic(
                "QueryPlan must contain at least one operation",
            ));
        }
        Ok(QueryPlan { nodes: self.nodes })
    }
}
