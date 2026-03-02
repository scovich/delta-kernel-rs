//! An implementation of data skipping that leverages parquet stats from the file footer.
use crate::expressions::{
    column_expr, joined_column_expr, joined_column_name, BinaryPredicateOp, ColumnName,
    Expression as Expr, JunctionPredicateOp, OpaquePredicateOpRef, Predicate as Pred, Scalar,
};
use crate::kernel_predicates::{
    DataSkippingPredicateEvaluator, DefaultKernelPredicateEvaluator, KernelPredicateEvaluator as _,
    KernelPredicateEvaluatorDefaults, MetaDataSkippingPredicateEvaluator, ResolveColumnAsScalar,
};
use crate::schema::DataType;

use std::cmp::Ordering;

#[cfg(test)]
mod tests;

/// Maps an ordering and inversion flag to the corresponding comparison predicate.
fn comparison_predicate(ord: Ordering, col: Expr, val: &Scalar, inverted: bool) -> Pred {
    let pred_fn = match (ord, inverted) {
        (Ordering::Less, false) => Pred::lt,
        (Ordering::Less, true) => Pred::ge,
        (Ordering::Equal, false) => Pred::eq,
        (Ordering::Equal, true) => Pred::ne,
        (Ordering::Greater, false) => Pred::gt,
        (Ordering::Greater, true) => Pred::le,
    };
    pred_fn(col, val.clone())
}

/// Collects sub-predicates into a junction (AND/OR), replacing unsupported sub-predicates (None)
/// with a single NULL literal to preserve correct three-valued logic.
fn collect_junction_preds(
    mut op: JunctionPredicateOp,
    preds: &mut dyn Iterator<Item = Option<Pred>>,
    inverted: bool,
) -> Pred {
    if inverted {
        op = op.invert();
    }
    let mut keep_null = true;
    let preds: Vec<_> = preds
        .flat_map(|p| match p {
            Some(pred) => Some(pred),
            None => keep_null.then(|| {
                keep_null = false;
                Pred::null_literal()
            }),
        })
        .collect();
    Pred::junction(op, preds)
}

fn footer_min_expr(col: &ColumnName) -> Expr {
    joined_column_expr!("parquetFooterStats.min", col)
}

fn footer_max_expr(col: &ColumnName) -> Expr {
    joined_column_expr!("parquetFooterStats.max", col)
}

fn footer_nullcount_expr(col: &ColumnName) -> Expr {
    joined_column_expr!("parquetFooterStats.nullCount", col)
}

fn footer_rowcount_expr() -> Expr {
    column_expr!("parquetFooterStats.rowCount")
}

/// A helper trait (mostly exposed for testing). It provides the four stats getters needed by
/// [`DataSkippingStatsProvider`]. From there, we can automatically derive a
/// [`DataSkippingPredicateEvaluator`].
pub(crate) trait ParquetStatsProvider {
    /// The min-value stat for this column, if the column exists in this file, has the expected
    /// type, and the parquet footer provides stats for it.
    fn get_parquet_min_stat(&self, col: &ColumnName, data_type: &DataType) -> Option<Scalar>;

    /// The max-value stat for this column, if the column exists in this file, has the expected
    /// type, and the parquet footer provides stats for it.
    fn get_parquet_max_stat(&self, col: &ColumnName, data_type: &DataType) -> Option<Scalar>;

    /// The nullcount stat for this column, if the column exists in this file, has the expected
    /// type, and the parquet footer provides stats for it.
    fn get_parquet_nullcount_stat(&self, col: &ColumnName) -> Option<i64>;

    /// The rowcount stat for this row group. It is always available in the parquet footer.
    fn get_parquet_rowcount_stat(&self) -> i64;
}

/// Resolver-backed direct evaluation over parquet footer stats using synthetic names:
///
/// - `parquetFooterStats.min.<col>`
/// - `parquetFooterStats.max.<col>`
/// - `parquetFooterStats.nullCount.<col>`
/// - `parquetFooterStats.rowCount`
///
/// NOTE: min/max lookups require logical type hints because parquet footer physical stats alone
/// are ambiguous for some logical types (for example `i32` could map to int/date/byte/short).
#[allow(dead_code)] // Sketch utility; integration comes later.
pub(crate) struct ParquetFooterStatsResolver<P: ParquetStatsProvider> {
    provider: P,
}

#[allow(dead_code)] // Sketch utility; integration comes later.
impl<P: ParquetStatsProvider> ParquetFooterStatsResolver<P> {
    pub(crate) fn new(provider: P) -> Self {
        Self { provider }
    }

    fn parse_footer_column(col: &ColumnName) -> Option<(&str, Option<ColumnName>)> {
        let path = col.path();
        match path {
            [root, field] if root == "parquetFooterStats" && field == "rowCount" => {
                Some(("rowCount", None))
            }
            [root, kind, rest @ ..] if root == "parquetFooterStats" && !rest.is_empty() => {
                Some((kind.as_str(), Some(ColumnName::new(rest.iter().cloned()))))
            }
            _ => None,
        }
    }
}

impl<P: ParquetStatsProvider> ResolveColumnAsScalar for ParquetFooterStatsResolver<P> {
    fn resolve_column_typed(&self, col: &ColumnName, data_type: Option<&DataType>) -> Option<Scalar> {
        let (kind, logical_col) = Self::parse_footer_column(col)?;
        match (kind, logical_col) {
            ("rowCount", None) => Some(Scalar::from(self.provider.get_parquet_rowcount_stat())),
            ("nullCount", Some(logical_col)) => self
                .provider
                .get_parquet_nullcount_stat(&logical_col)
                .map(Scalar::from),
            ("min", Some(logical_col)) => {
                let data_type = data_type?;
                self.provider.get_parquet_min_stat(&logical_col, data_type)
            }
            ("max", Some(logical_col)) => {
                let data_type = data_type?;
                self.provider.get_parquet_max_stat(&logical_col, data_type)
            }
            _ => None,
        }
    }

    fn resolve_column(&self, col: &ColumnName) -> Option<Scalar> {
        self.resolve_column_typed(col, None)
    }
}

/// Evaluates a rewritten predicate (that uses `parquetFooterStats.*` columns) via the default
/// predicate evaluator and a footer-stats-backed column resolver.
#[allow(dead_code)] // Sketch utility; integration comes later.
pub(crate) fn eval_rewritten_with_parquet_footer_stats<P: ParquetStatsProvider + 'static>(
    pred: &Pred,
    provider: P,
) -> Option<bool> {
    let resolver = ParquetFooterStatsResolver::new(provider);
    let evaluator = DefaultKernelPredicateEvaluator::from(resolver);
    evaluator.eval_sql_where(pred)
}

/// Concrete indirect rewriter for parquet footer data skipping.
///
/// This is the "normal" footer-skipping rewrite where an input column `x` maps to synthetic
/// footer-stat columns under:
/// - `parquetFooterStats.min.x`
/// - `parquetFooterStats.max.x`
/// - `parquetFooterStats.nullCount.x`
/// - `parquetFooterStats.rowCount`
///
/// The resulting predicate is intended for evaluation by a normal predicate evaluator whose
/// column resolver is backed by a [`ParquetStatsProvider`].
#[allow(dead_code)] // Sketch utility; integration comes later.
pub(crate) struct ParquetFooterDataSkippingPredicateCreator;

impl DataSkippingPredicateEvaluator for ParquetFooterDataSkippingPredicateCreator {
    type Output = Pred;
    type ColumnStat = Expr;

    fn get_min_stat(&self, col: &ColumnName, _data_type: &DataType) -> Option<Expr> {
        Some(footer_min_expr(col))
    }

    fn get_max_stat(&self, col: &ColumnName, _data_type: &DataType) -> Option<Expr> {
        Some(footer_max_expr(col))
    }

    fn get_nullcount_stat(&self, col: &ColumnName) -> Option<Expr> {
        Some(footer_nullcount_expr(col))
    }

    fn get_rowcount_stat(&self) -> Option<Expr> {
        Some(footer_rowcount_expr())
    }

    fn eval_partial_cmp(
        &self,
        ord: Ordering,
        col: Expr,
        val: &Scalar,
        inverted: bool,
    ) -> Option<Pred> {
        Some(comparison_predicate(ord, col, val, inverted))
    }

    fn eval_pred_scalar(&self, val: &Scalar, inverted: bool) -> Option<Pred> {
        KernelPredicateEvaluatorDefaults::eval_pred_scalar(val, inverted).map(Pred::literal)
    }

    fn eval_pred_scalar_is_null(&self, val: &Scalar, inverted: bool) -> Option<Pred> {
        KernelPredicateEvaluatorDefaults::eval_pred_scalar_is_null(val, inverted).map(Pred::literal)
    }

    fn eval_pred_is_null(&self, col: &ColumnName, inverted: bool) -> Option<Pred> {
        let safe_to_skip = match inverted {
            true => self.get_rowcount_stat()?, // all-null
            false => Expr::literal(0i64),      // no-null
        };
        Some(Pred::ne(self.get_nullcount_stat(col)?, safe_to_skip))
    }

    fn eval_pred_binary_scalars(
        &self,
        op: BinaryPredicateOp,
        left: &Scalar,
        right: &Scalar,
        inverted: bool,
    ) -> Option<Pred> {
        KernelPredicateEvaluatorDefaults::eval_pred_binary_scalars(op, left, right, inverted)
            .map(Pred::literal)
    }

    fn eval_pred_opaque(
        &self,
        op: &OpaquePredicateOpRef,
        exprs: &[Expr],
        inverted: bool,
    ) -> Option<Pred> {
        op.as_data_skipping_predicate(self, exprs, inverted)
    }

    fn finish_eval_pred_junction(
        &self,
        op: JunctionPredicateOp,
        preds: &mut dyn Iterator<Item = Option<Pred>>,
        inverted: bool,
    ) -> Option<Pred> {
        Some(collect_junction_preds(op, preds, inverted))
    }
}

/// Concrete indirect metadata-skipping rewriter.
///
/// - Input columns like `x` are mapped to metadata stats columns
///   `add.stats_parsed.minValues.x` / `add.stats_parsed.maxValues.x`
/// - Each min/max-derived comparison is guarded with per-file safety checks over nested nullcount:
///   `OR(<comparison>, nullCount.<nested_col> != 0)`
///
/// This keeps row groups conservatively whenever nested stats could be missing.
pub(crate) struct CheckpointMetaDataSkippingPredicateCreator;

impl CheckpointMetaDataSkippingPredicateCreator {
    fn metadata_min_col(col: &ColumnName) -> ColumnName {
        joined_column_name!("add.stats_parsed.minValues", col)
    }

    fn metadata_max_col(col: &ColumnName) -> ColumnName {
        joined_column_name!("add.stats_parsed.maxValues", col)
    }

    fn apply_nested_stat_safety(col: &ColumnName, pred: Pred) -> Option<Pred> {
        // Safety rule: only skip when comparison is definitively false *and* nested nullcount is
        // known to be zero. If nested nullcount is missing, `!= 0` evaluates to NULL and OR keeps
        // the result at TRUE/NULL, which conservatively disables skipping.
        Some(Pred::or(pred, Pred::ne(footer_nullcount_expr(col), Expr::literal(0i64))))
    }
}

impl MetaDataSkippingPredicateEvaluator for CheckpointMetaDataSkippingPredicateCreator {
    type Output = Pred;
    type ColumnStat = Expr;

    fn get_metadata_min_stat(&self, col: &ColumnName, _data_type: &DataType) -> Option<Expr> {
        Some(footer_min_expr(&Self::metadata_min_col(col)))
    }

    fn get_metadata_max_stat(&self, col: &ColumnName, _data_type: &DataType) -> Option<Expr> {
        Some(footer_max_expr(&Self::metadata_max_col(col)))
    }

    fn get_metadata_nullcount_stat(&self, _col: &ColumnName) -> Option<Expr> {
        None
    }

    fn get_metadata_rowcount_stat(&self) -> Option<Expr> {
        None
    }

    fn eval_pred_scalar(&self, val: &Scalar, inverted: bool) -> Option<Pred> {
        KernelPredicateEvaluatorDefaults::eval_pred_scalar(val, inverted).map(Pred::literal)
    }

    fn eval_pred_scalar_is_null(&self, val: &Scalar, inverted: bool) -> Option<Pred> {
        KernelPredicateEvaluatorDefaults::eval_pred_scalar_is_null(val, inverted).map(Pred::literal)
    }

    // Delta IS [NOT] NULL semantics rely on file-level nullcount/rowcount stats, which are not
    // safely derivable from checkpoint footer metadata stats.
    fn eval_pred_is_null(&self, _col: &ColumnName, _inverted: bool) -> Option<Pred> {
        None
    }

    fn eval_pred_binary_scalars(
        &self,
        op: BinaryPredicateOp,
        left: &Scalar,
        right: &Scalar,
        inverted: bool,
    ) -> Option<Pred> {
        KernelPredicateEvaluatorDefaults::eval_pred_binary_scalars(op, left, right, inverted)
            .map(Pred::literal)
    }

    // Conservative by default: avoid opaque rewrites until semantics are explicitly mapped.
    fn eval_pred_opaque(
        &self,
        _op: &OpaquePredicateOpRef,
        _exprs: &[Expr],
        _inverted: bool,
    ) -> Option<Pred> {
        None
    }

    fn finish_eval_pred_junction(
        &self,
        op: JunctionPredicateOp,
        preds: &mut dyn Iterator<Item = Option<Pred>>,
        inverted: bool,
    ) -> Option<Pred> {
        Some(collect_junction_preds(op, preds, inverted))
    }

    fn eval_partial_cmp(
        &self,
        ord: Ordering,
        col: Expr,
        val: &Scalar,
        inverted: bool,
    ) -> Option<Pred> {
        Some(comparison_predicate(ord, col, val, inverted))
    }

    fn apply_metadata_min_safety(&self, col: &ColumnName, pred: Pred) -> Option<Pred> {
        Self::apply_nested_stat_safety(&Self::metadata_min_col(col), pred)
    }

    fn apply_metadata_max_safety(&self, col: &ColumnName, pred: Pred) -> Option<Pred> {
        Self::apply_nested_stat_safety(&Self::metadata_max_col(col), pred)
    }
}

// Blanket implementation for all types that impl ParquetStatsProvider.
impl<T: ParquetStatsProvider> DataSkippingPredicateEvaluator for T {
    type Output = bool;
    type ColumnStat = Scalar;

    fn get_min_stat(&self, col: &ColumnName, data_type: &DataType) -> Option<Scalar> {
        self.get_parquet_min_stat(col, data_type)
    }

    fn get_max_stat(&self, col: &ColumnName, data_type: &DataType) -> Option<Scalar> {
        self.get_parquet_max_stat(col, data_type)
    }

    fn get_nullcount_stat(&self, col: &ColumnName) -> Option<Scalar> {
        self.get_parquet_nullcount_stat(col).map(Scalar::from)
    }

    fn get_rowcount_stat(&self) -> Option<Scalar> {
        Some(Scalar::from(self.get_parquet_rowcount_stat()))
    }

    fn eval_partial_cmp(
        &self,
        ord: Ordering,
        col: Scalar,
        val: &Scalar,
        inverted: bool,
    ) -> Option<bool> {
        KernelPredicateEvaluatorDefaults::partial_cmp_scalars(ord, &col, val, inverted)
    }

    fn eval_pred_scalar(&self, val: &Scalar, inverted: bool) -> Option<bool> {
        KernelPredicateEvaluatorDefaults::eval_pred_scalar(val, inverted)
    }

    fn eval_pred_scalar_is_null(&self, val: &Scalar, inverted: bool) -> Option<bool> {
        KernelPredicateEvaluatorDefaults::eval_pred_scalar_is_null(val, inverted)
    }

    // NOTE: This is nearly identical to the impl for DataSkippingPredicateEvaluator in
    // data_skipping.rs, except it uses `Scalar` instead of `Expression` and `Predicate`.
    fn eval_pred_is_null(&self, col: &ColumnName, inverted: bool) -> Option<bool> {
        let safe_to_skip = match inverted {
            true => self.get_rowcount_stat()?, // all-null
            false => Scalar::from(0i64),       // no-null
        };
        Some(self.get_nullcount_stat(col)? != safe_to_skip)
    }

    fn eval_pred_binary_scalars(
        &self,
        op: BinaryPredicateOp,
        left: &Scalar,
        right: &Scalar,
        inverted: bool,
    ) -> Option<bool> {
        KernelPredicateEvaluatorDefaults::eval_pred_binary_scalars(op, left, right, inverted)
    }

    fn eval_pred_opaque(
        &self,
        op: &OpaquePredicateOpRef,
        exprs: &[Expr],
        inverted: bool,
    ) -> Option<bool> {
        op.eval_as_data_skipping_predicate(self, exprs, inverted)
    }

    fn finish_eval_pred_junction(
        &self,
        op: JunctionPredicateOp,
        preds: &mut dyn Iterator<Item = Option<bool>>,
        inverted: bool,
    ) -> Option<bool> {
        KernelPredicateEvaluatorDefaults::finish_eval_pred_junction(op, preds, inverted)
    }
}
