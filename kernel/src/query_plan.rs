use url::Url;

use crate::{DeltaResult, Error, FileMeta, Scalar, SchemaRef};

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

    /// Concatenate inputs without deduplication.
    ///
    /// All inputs must have identical output schemas. This node does not impose any row-ordering
    /// guarantees.
    UnionAll,

    /// Global aggregate over all input rows.
    ///
    /// For each `c` in `value_columns`, its output is equivalent to the SQL expression:
    /// ```sql
    /// max_by(c, version_column) FILTER (WHERE c IS NOT NULL)
    /// ```
    ///
    /// The output always has exactly one row with columns in `value_columns` order. If no row
    /// satisfies `c IS NOT NULL` (including empty input), that output value is `NULL`.
    MaxByVersion {
        version_column: String,
        value_columns: Vec<String>,
    },
}

impl PlanOp {
    /// Returns a stable operation name for diagnostics.
    pub fn name(&self) -> &'static str {
        match self {
            PlanOp::ListFiles { .. } => "ListFiles",
            PlanOp::ScanJson { .. } => "ScanJson",
            PlanOp::ScanParquet { .. } => "ScanParquet",
            PlanOp::UnionAll => "UnionAll",
            PlanOp::MaxByVersion { .. } => "MaxByVersion",
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
            PlanOp::UnionAll => InputArity::Variadic,
            PlanOp::MaxByVersion { .. } => InputArity::One,
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
    ) -> DeltaResult<RefId> {
        self.append(
            PlanOp::MaxByVersion {
                version_column: version_column.into(),
                value_columns: value_columns.into_iter().map(Into::into).collect(),
            },
            [input],
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
