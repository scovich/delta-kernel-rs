//! Support for kernel-driven predicate evaluation via the [`KernelPredicateEvaluator`]
//! trait. Various trait implementations are used for partition pruning, stats-based data skipping,
//! and parquet row group filtering. The evaluation is normally performed over [`Scalar`] values,
//! but data skipping "evaluation" actually produces a transformed predicate that replaces column
//! references with stats column references, which log replay will instruct the engine to evaluate.
use crate::expressions::{
    BinaryExpression, BinaryExpressionOp, BinaryPredicate, BinaryPredicateOp, ColumnName,
    Expression as Expr, JunctionPredicate, JunctionPredicateOp, LetPredicate, OpaqueExpression,
    OpaqueExpressionOpRef, OpaquePredicate, OpaquePredicateOpRef, Predicate as Pred, Scalar,
    UnaryPredicate, UnaryPredicateOp,
};
use crate::schema::DataType;

use std::cmp::Ordering;
use std::collections::HashMap;
use tracing::{debug, warn};

pub(crate) mod parquet_stats_skipping;

#[cfg(test)]
mod tests;

// ==================== Let Bindings Infrastructure ====================

/// Manages let-bindings for predicate evaluation to avoid redundant computation.
///
/// During predicate rewrites (e.g., IS_NULL junction pushdowns), child predicates may need to be
/// referenced multiple times. Without let-bindings, this causes exponential blow-up for nested
/// structures. Two strategies are provided:
///
/// - **Direct evaluation**: Cache results locally in a HashMap and retrieve copies
/// - **Indirect evaluation**: Store results as Let nodes in the output AST and retrieve column references
///
/// Let-bindings capture `Option<Output>` rather than just `Output` to properly handle missing
/// results (e.g., from unsupported operations or missing stats).
pub trait LetBindings: Default {
    type Output;

    /// Store an evaluation result (which may be None) and return its handle/name
    fn store(&mut self, output: Option<Self::Output>) -> String;

    /// Retrieve a reference to a previously stored result (which may be None)
    fn retrieve(&self, name: &str) -> Option<Self::Output>;

    /// Transform the final result (identity for direct eval, wrap in Let for indirect)
    fn finalize(self, result: Option<Self::Output>) -> Option<Self::Output>;
}

/// Result of fused predicate evaluation, containing names to reference the computed values.
///
/// For direct evaluation, these are handles into the local cache. For indirect evaluation,
/// these are binding names that can be used as Column references.
pub struct FusedEvalResult {
    /// Name/handle for the computed value result
    pub value_name: String,
    /// Name/handle for the IS_NULL check result
    pub is_null_name: String,
}

/// Direct let-bindings: caches results locally and returns copies.
///
/// Used by direct predicate evaluators (scalar and parquet stats) where `Output` is a simple
/// value type (`bool` or `Scalar`) that can be cloned. Bindings are stored in a local HashMap
/// and discarded after evaluation completes.
#[derive(Default)]
pub struct DirectLetBindings<T: Clone + Default> {
    cache: HashMap<String, T>,
    counter: usize,
}

impl<T: Clone + Default> LetBindings for DirectLetBindings<T> {
    type Output = T;

    fn store(&mut self, output: Option<T>) -> String {
        let name = format!("${}", self.counter);
        self.counter += 1;
        // Only store successful results; None is represented by absence from the map
        if let Some(value) = output {
            self.cache.insert(name.clone(), value);
        }
        name
    }

    fn retrieve(&self, name: &str) -> Option<T> {
        self.cache.get(name).cloned()
    }

    fn finalize(self, result: Option<T>) -> Option<T> {
        result // Identity - no wrapping needed
    }
}

/// Indirect let-bindings: creates Let nodes in the output AST and returns column references.
///
/// Used by indirect data skipping evaluators where `Output = Pred`. Stored results become
/// bindings in a Let node that wraps the final result. Retrievals return column references
/// that will resolve to those bindings during evaluation.
#[derive(Default)]
pub struct IndirectLetBindings {
    bindings: indexmap::IndexMap<String, Pred>,
    counter: usize,
}

impl LetBindings for IndirectLetBindings {
    type Output = Pred;

    fn store(&mut self, output: Option<Pred>) -> String {
        let name = format!("$__kernel_pred_{}", self.counter);
        self.counter += 1;
        // Only store successful results; None is represented by absence from the map
        if let Some(pred) = output {
            self.bindings.insert(name.clone(), pred);
        }
        name
    }

    fn retrieve(&self, name: &str) -> Option<Pred> {
        // Return a column reference that will resolve to the binding during evaluation.
        // If the name doesn't exist in the map, return None (the evaluation was unsuccessful).
        self.bindings
            .get(name)
            .map(|_| Pred::BooleanExpression(Expr::Column(ColumnName::new([name]))))
    }

    fn finalize(self, result: Option<Pred>) -> Option<Pred> {
        // Emit a Let with the accumulated bindings, if any exist.
        let mut result = result?;
        if !self.bindings.is_empty() {
            result = Pred::Let(LetPredicate::new(self.bindings, result));
        }
        Some(result)
    }
}

// NOTE: When creating `&dyn Foo` for some `impl<'a> Bar<'a>`, the compiler infers `&'r dyn Foo +
// 'a` (and then elides the lifetimes because `'a: 'r`). Creating a type alias for `dyn Foo` causes
// the compiler to infer `dyn Foo + 'static` (the lifetime of the alias). Which in turn requires
// `impl Bar<'static>`, which is almost always an impossible constraint. Defining the type aliases
// below with generic lifetimes allows `&'r DynFoo<'a>` (again with `'a: 'r`). Unfortunately,
// generic lifetimes cannot be hidden, so we end up with `&DynFoo<'_>` at every use site.

/// A predicate evaluator that directly evaluates predicates, resolving column references to scalar values.
pub type DirectPredicateEvaluator<'a> =
    dyn KernelPredicateEvaluator<Output = bool, Bindings = DirectLetBindings<bool>> + 'a;

/// A data skipping predicate evaluator that directly applies data skipping, resolving column
/// references to scalar stats values such as those provided by parquet footer stats.
pub type DirectDataSkippingPredicateEvaluator<'a> = dyn DataSkippingPredicateEvaluator<
        Output = bool,
        ColumnStat = Scalar,
        Bindings = DirectLetBindings<bool>,
    > + 'a;

/// A data skipping predicate evaluator that rewrites the input to a predicate that performs data
/// skipping over column stats for all referenced columns. The resulting predicate can be evaluated
/// against batches of column stats at some future point.
#[rustfmt::skip] // for some reason this type alias ends up as a single very long line??
pub type IndirectDataSkippingPredicateEvaluator<'a> = dyn DataSkippingPredicateEvaluator<
        Output = Pred,
    	ColumnStat = Expr,
    	Bindings = IndirectLetBindings,
    > + 'a;

/// Uses kernel (not engine) logic to evaluate a predicate tree against column names that resolve as
/// scalars. Useful for testing/debugging but also serves as a reference implementation that
/// documents the expression semantics that kernel relies on for data skipping.
///
/// # Inverted expression semantics
///
/// Because inversion (`NOT` operator) has special semantics and can often be optimized away by
/// pushing it down, most methods take an `inverted` flag. That allows operations like [`Pred::Not`]
/// to simply evaluate their operand with a flipped `inverted` flag, and greatly simplifies the
/// implementations of most operators (other than those which have to directly implement NOT
/// semantics, which are unavoidably complex in that regard).
///
/// # Parameterized output type
///
/// The types involved in predicate evaluation are parameterized and implementation-specific. For
/// example, a [`DirectDataSkippingPredicateEvaluator`] directly evaluates the predicate (e.g. using
/// parquet footer stats) and returns boolean results, while
/// [`IndirectDataSkippingPredicateEvaluator`] instead transforms the input predicate to a data
/// skipping predicate that the engine can evaluated directly against Delta data skipping stats
/// during log replay. Although this approach is harder to read and reason about at first, the
/// majority of predicates can be implemented generically, which greatly reduces redundancy and
/// ensures that all flavors of predicate evaluation have the same semantics.
///
/// # NULL and error semantics
///
/// Literal NULL values almost always produce cascading changes in the predicate's structure, so we
/// represent them by `Option::None` rather than `Scalar::Null`. This allows e.g. `A < NULL` to be
/// rewritten as `NULL`, or `AND(NULL, FALSE)` to be rewritten as `FALSE`.
///
/// Almost all operations produce NULL output if any input is `NULL`. Any resolution failures also
/// produce NULL (such as missing columns or type mismatch between a column and the scalar it is
/// compared against). NULL-checking operations like `IS [NOT] NULL` and `DISTINCT` are special, and
/// rely on nullcount stats for their work (NULL/missing nullcount stats makes them output NULL).
///
/// For safety reasons, NULL-checking operations only accept literal and column inputs where
/// stats-based skipping is well-defined. If an arbitrary data skipping predicate evaluates to
/// NULL, there is no way to tell whether the original predicate really evaluated to NULL (safe to
/// use), or the data skipping version evaluated to NULL due to missing stats (very unsafe to use).
///
/// NOTE: The error-handling semantics of this trait's scalar-based predicate evaluation may differ
/// from those of the engine's predicate evaluation, because kernel predicates don't include the
/// necessary type information to reliably detect all type errors.
pub trait KernelPredicateEvaluator {
    type Output;

    /// Let-bindings strategy for managing intermediate results during evaluation.
    ///
    /// Direct evaluators use [`DirectLetBindings`] to cache results locally.
    /// Indirect evaluators use [`IndirectLetBindings`] to create Let nodes in the output AST.
    type Bindings: LetBindings<Output = Self::Output>;

    /// A (possibly inverted) boolean scalar value, e.g. `[NOT] <value>`.
    fn eval_pred_scalar(&self, val: &Scalar, inverted: bool) -> Option<Self::Output>;

    /// A (possibly inverted) scalar NULL test, e.g. `<value> IS [NOT] NULL`.
    fn eval_pred_scalar_is_null(&self, val: &Scalar, inverted: bool) -> Option<Self::Output>;

    /// A (possibly inverted) NULL check, e.g. `<expr> IS [NOT] NULL`.
    fn eval_pred_column_is_null(&self, col: &ColumnName, inverted: bool) -> Option<Self::Output>;

    /// A (possibly inverted) less-than comparison, e.g. `<col> < <value>`.
    fn eval_pred_lt(&self, col: &ColumnName, val: &Scalar, inverted: bool) -> Option<Self::Output>;

    /// A (possibly inverted) greater-than comparison, e.g. `<col> > <value>`
    fn eval_pred_gt(&self, col: &ColumnName, val: &Scalar, inverted: bool) -> Option<Self::Output>;

    /// A (possibly inverted) equality comparison, e.g. `<col> = <value>` or `<col> != <value>`.
    ///
    /// NOTE: Caller is responsible to commute the operation if needed, e.g. `<value> != <col>`
    /// becomes `<col> != <value>`.
    fn eval_pred_eq(&self, col: &ColumnName, val: &Scalar, inverted: bool) -> Option<Self::Output>;

    /// A (possibly inverted) comparison between two scalars, e.g. `<valueA> != <valueB>`.
    fn eval_pred_binary_scalars(
        &self,
        op: BinaryPredicateOp,
        left: &Scalar,
        right: &Scalar,
        inverted: bool,
    ) -> Option<Self::Output>;

    /// A (possibly inverted) comparison between two columns, e.g. `<colA> != <colB>`.
    fn eval_pred_binary_columns(
        &self,
        op: BinaryPredicateOp,
        a: &ColumnName,
        b: &ColumnName,
        inverted: bool,
    ) -> Option<Self::Output>;

    /// Dispatches an opaque predicate.
    fn eval_pred_opaque(
        &self,
        op: &OpaquePredicateOpRef,
        exprs: &[Expr],
        inverted: bool,
    ) -> Option<Self::Output>;

    /// Dispatches an opaque expression used as a predicate
    fn eval_pred_expr_opaque(
        &self,
        op: &OpaqueExpressionOpRef,
        exprs: &[Expr],
        inverted: bool,
    ) -> Option<Self::Output>;

    /// Completes evaluation of a (possibly inverted) junction predicate.
    ///
    /// AND and OR are implemented by first evaluating its (possibly inverted) inputs. This part is
    /// always the same, provided by [`Self::eval_pred_junction`]). The results are then combined to
    /// become the predicate's output in some implementation-defined way (this method).
    fn finish_eval_pred_junction(
        &self,
        op: JunctionPredicateOp,
        preds: &mut dyn Iterator<Item = Option<Self::Output>>,
        inverted: bool,
    ) -> Option<Self::Output>;

    /// Evaluates a (possibly inverted) Let predicate by binding the given predicates and
    /// evaluating the body with those bindings available.
    ///
    /// Each binding can reference previous bindings in the same Let node. The bindings are
    /// stored in implementation-specific ways (e.g., RefCell for direct evaluators).
    fn eval_pred_let(
        &self,
        bindings: &[(String, Pred)],
        body: &Pred,
        inverted: bool,
    ) -> Option<Self::Output>;

    // ==================== PROVIDED METHODS ====================

    /// A (possibly inverted) boolean column access, e.g. `[NOT] <col>`.
    fn eval_pred_column(&self, col: &ColumnName, inverted: bool) -> Option<Self::Output> {
        // The expression <col> is equivalent to <col> != FALSE, and the expression NOT <col> is
        // equivalent to <col> != TRUE.
        self.eval_pred_eq(col, &Scalar::from(inverted), true)
    }

    /// Dispatches a (possibly inverted) NOT predicate
    fn eval_pred_not(&self, pred: &Pred, inverted: bool) -> Option<Self::Output> {
        self.eval_pred(pred, !inverted)
    }

    /// Dispatches a (possibly inverted) boolean expression used as a predicate
    fn eval_pred_expr(&self, expr: &Expr, inverted: bool) -> Option<Self::Output> {
        // Directly evaluate literals and and predicates used as expressions. Evaluate columns as
        // `<col> == TRUE`. All other expressions unsupported.
        match expr {
            Expr::Literal(val) => self.eval_pred_scalar(val, inverted),
            Expr::Column(col) => self.eval_pred_column(col, inverted),
            Expr::Predicate(pred) => self.eval_pred(pred, inverted),
            Expr::Opaque(OpaqueExpression { op, exprs }) => {
                self.eval_pred_expr_opaque(op, exprs, inverted)
            }
            Expr::Struct(_)
            | Expr::Transform(_)
            | Expr::Unary(_)
            | Expr::Binary(_)
            | Expr::Variadic(_)
            | Expr::Let(_)
            | Expr::Unknown(_) => None,
        }
    }

    /// Dispatches a (possibly inverted) unary expression to each operator's specific implementation.
    fn eval_pred_unary(
        &self,
        op: UnaryPredicateOp,
        expr: &Expr,
        inverted: bool,
    ) -> Option<Self::Output> {
        match op {
            UnaryPredicateOp::IsNull => match expr {
                // WARNING: Only literals and columns can be safely null-checked. Attempting to
                // null-check an expressions such as `a < 10` could wrongly produce FALSE in case
                // `a` is just plain missing (rather than known to be NULL. A missing-value can
                // arise e.g. if data skipping encounters a column with missing stats, or if
                // partition pruning encounters a non-partition column.
                Expr::Literal(val) => self.eval_pred_scalar_is_null(val, inverted),
                Expr::Column(col) => self.eval_pred_column_is_null(col, inverted),
                Expr::Predicate(pred) => self.eval_pred_is_null(pred, inverted),
                Expr::Struct(_)
                | Expr::Transform(_)
                | Expr::Unary(_)
                | Expr::Binary(_)
                | Expr::Variadic(_)
                | Expr::Let(_)
                | Expr::Opaque(_)
                | Expr::Unknown(_) => {
                    debug!("Unsupported operand: IS [NOT] NULL: {expr:?}");
                    None
                }
            },
        }
    }

    /// Evaluates IS [NOT] NULL for a predicate, recursively handling junctions.
    ///
    /// This method enables pushing IS_NULL checks through AND/OR operations using the rewrites:
    /// - `IS_NULL(AND(a, b, ...))` → rewritten to check if all non-null inputs are TRUE
    /// - `IS_NULL(OR(a, b, ...))` → rewritten to check if all non-null inputs are FALSE
    /// - `IS_NOT_NULL(AND(...))` → rewritten to check for at least one non-null FALSE or all non-null
    /// - `IS_NOT_NULL(OR(...))` → rewritten to check for at least one non-null TRUE or all non-null
    ///
    /// The nullness of `NOT(x)` is identical to that of `x`, so NOT is transparent.
    fn eval_pred_is_null(&self, pred: &Pred, inverted: bool) -> Option<Self::Output> {
        match pred {
            Pred::Not(inner) => {
                // IS_NULL(NOT(x)) requires handling both NOT and IS_NULL simultaneously.
                // We can only do this if x is a junction (which we know how to push into).
                match inner.as_ref() {
                    Pred::Junction(JunctionPredicate { op, preds }) => {
                        self.eval_is_null_junction(*op, preds, true, inverted)
                    }
                    _ => {
                        // Can't rewrite IS_NULL(NOT(non-junction))
                        debug!("Unsupported operand for IS [NOT] NULL: NOT({inner:?})");
                        None
                    }
                }
            }
            Pred::BooleanExpression(expr) => {
                self.eval_pred_unary(UnaryPredicateOp::IsNull, expr, inverted)
            }
            Pred::Junction(JunctionPredicate { op, preds }) => {
                self.eval_is_null_junction(*op, preds, false, inverted)
            }
            _ => {
                // Other predicate types are not supported for IS_NULL push-down
                debug!("Unsupported operand for IS [NOT] NULL: {pred:?}");
                None
            }
        }
    }

    /// Evaluates both a predicate and its nullness in a single call.
    ///
    /// Returns a [`FusedEvalResult`] containing names to reference the computed values.
    /// The bindings are accumulated in the provided `bindings` parameter.
    ///
    /// The two `inverted` parameters allow independent inversions for the value and null check,
    /// which is needed for the junction null-check pushdown optimization.
    fn eval_pred_with_null_check(
        &self,
        pred: &Pred,
        bindings: &mut Self::Bindings,
        value_inverted: bool,
        null_inverted: bool,
    ) -> FusedEvalResult {
        let (value, is_null) = match pred {
            Pred::Not(inner) => {
                // NOT affects value but is transparent to nullness
                return self.eval_pred_with_null_check(
                    inner,
                    bindings,
                    !value_inverted,
                    null_inverted,
                );
            }
            // Use fused junction evaluation to avoid redundant child evaluations
            Pred::Junction(JunctionPredicate { op, preds }) => self.eval_is_null_junction_fused(
                *op,
                preds,
                bindings,
                value_inverted,
                null_inverted,
            ),
            _ => {
                // For other predicates, evaluate separately (no optimization yet)
                let value = self.eval_pred(pred, value_inverted);
                let is_null = self.eval_pred_is_null(pred, null_inverted);
                (value, is_null)
            }
        };

        FusedEvalResult {
            value_name: bindings.store(value),
            is_null_name: bindings.store(is_null),
        }
    }

    /// Core implementation of IS [NOT] NULL junction evaluation.
    ///
    /// This implements the boolean logic rewrites:
    /// ```text
    /// IS_NULL(AND(a, b, ..., z))
    /// = AND(
    ///     AND(
    ///         OR(IS_NULL(a), a),
    ///         OR(IS_NULL(b), b),
    ///         ...,
    ///         OR(IS_NULL(z), z)
    ///     ),
    ///     OR(IS_NULL(a), IS_NULL(b), ..., IS_NULL(z))
    ///   )
    ///
    /// IS_NULL(OR(a, b, ..., z))
    /// = AND(
    ///     AND(
    ///         OR(IS_NULL(a), NOT(a)),
    ///         OR(IS_NULL(b), NOT(b)),
    ///         ...,
    ///         OR(IS_NULL(z), NOT(z))
    ///     ),
    ///     OR(IS_NULL(a), IS_NULL(b), ..., IS_NULL(z))
    ///   )
    ///
    /// NOT(IS_NULL(AND(a, b, ..., z)))
    /// = OR(
    ///     OR(
    ///         AND(NOT(IS_NULL(a)), NOT(a)),
    ///         AND(NOT(IS_NULL(b)), NOT(b)),
    ///         ...,
    ///         AND(NOT(IS_NULL(z)), NOT(z))
    ///     ),
    ///     AND(NOT(IS_NULL(a)), NOT(IS_NULL(b)), ..., NOT(IS_NULL(z)))
    ///   )
    ///
    /// NOT(IS_NULL(OR(a, b, ..., z)))
    /// = OR(
    ///     OR(
    ///         AND(NOT(IS_NULL(a)), a),
    ///         AND(NOT(IS_NULL(b)), b),
    ///         ...,
    ///         AND(NOT(IS_NULL(z)), z)
    ///     ),
    ///     AND(NOT(IS_NULL(a)), NOT(IS_NULL(b)), ..., NOT(IS_NULL(z)))
    ///   )
    /// ```
    ///
    /// The implementation uses intentional nesting to control evaluation order: the per-variable
    /// clauses are evaluated first and can short-circuit, avoiding evaluation of the final "all
    /// null checks" clause.
    ///
    /// To avoid exponential blow-up from evaluating each child predicate multiple times, we use
    /// [`LetBindings`] to manage intermediate results. For direct evaluation, results are cached
    /// locally. For indirect evaluation, Let nodes are created in the output predicate tree.
    ///
    /// If `value_handles` is provided, the value handles from each child evaluation are captured
    /// for later reuse (needed by the fused version to compute the junction's original value).
    fn eval_is_null_junction_impl(
        &self,
        op: JunctionPredicateOp,
        preds: &[Pred],
        bindings: &mut Self::Bindings,
        value_inverted: bool,
        null_inverted: bool,
        mut value_handles: Option<&mut Vec<String>>,
    ) -> Option<Self::Output> {
        // Determine the dominating value for the junction type and null inversion.
        // The per-variable clauses need to check against the dominator (the value that would
        // prevent the junction from being NULL even if some of its children are NULL).
        let dominator = KernelPredicateEvaluatorDefaults::junction_dominator(op, null_inverted);

        // Determine junction types for the rewrite (depends on null_inverted, not input op):
        //
        // IS_NULL (null_inverted=false):
        //   - Per-var clauses: OR(IS_NULL(pred), pred != dominator)
        //   - Inner junction: AND of all per-var clauses
        //   - Final clause: OR(IS_NULL(a), IS_NULL(b), ...)
        //   - Outer junction: AND(inner, final)
        //
        // IS_NOT_NULL (null_inverted=true):
        //   - Per-var clauses: AND(NOT(IS_NULL(pred)), pred != dominator)
        //   - Inner junction: OR of all per-var clauses
        //   - Final clause: AND(NOT(IS_NULL(a)), NOT(IS_NULL(b)), ...)
        //   - Outer junction: OR(inner, final)
        //
        // Note: Inner and outer use the same junction type (both AND or both OR), ensuring
        // short-circuit alignment. Per-var and final clauses use the opposite type.
        let (outer_op, per_var_op) = match null_inverted {
            true => (JunctionPredicateOp::Or, JunctionPredicateOp::And),
            false => (JunctionPredicateOp::And, JunctionPredicateOp::Or),
        };

        // Lazily evaluate children and build per-variable clauses
        let mut null_handles = Vec::with_capacity(preds.len());
        let mut per_var_clauses = preds.iter().map(|pred| {
            // Evaluate both null check and var clause for this child in one fused call
            let FusedEvalResult {
                value_name,
                is_null_name,
            } = self.eval_pred_with_null_check(pred, bindings, value_inverted, null_inverted);

            // Build the per-variable clause. We need the value with `dominator` inversion.
            // Check if we stored the right inversion or need to evaluate with opposite flag.
            let var_clause = if value_inverted == dominator {
                bindings.retrieve(&value_name)
            } else {
                self.eval_pred(pred, dominator)
            };

            let is_null_clause = bindings.retrieve(&is_null_name);
            if let Some(handles) = value_handles.as_mut() {
                handles.push(value_name);
            }
            null_handles.push(is_null_name);

            let mut pair = [is_null_clause, var_clause].into_iter();
            self.finish_eval_pred_junction(per_var_op, &mut pair, false)
        });

        // Evaluate inner junction (may short-circuit before evaluating all children)
        let inner_result = self.finish_eval_pred_junction(outer_op, &mut per_var_clauses, false);

        // Evaluate outer junction combining inner result with final null-check clause.
        //
        // NOTE: `all_null_checks` is incomplete if `inner_result` short circuited. But in
        // that case, the final junction (same op) will _also_ short circuit on `inner_result` and
        // the final guard clause will never be evaluated.
        //
        // NOTE: We must evaluate the outer junction even if inner_result is None, because
        // the final clause might be a dominating value (e.g., AND(None, FALSE) = FALSE).
        let mut both = std::iter::once(inner_result).chain(std::iter::once_with(|| {
            let mut all_null_checks = null_handles
                .iter()
                .map(|null_handle| bindings.retrieve(null_handle));
            self.finish_eval_pred_junction(per_var_op, &mut all_null_checks, false)
        }));
        self.finish_eval_pred_junction(outer_op, &mut both, false)
    }

    /// Evaluates IS [NOT] NULL of a junction (AND/OR) by rewriting it into evaluable form.
    ///
    /// This is a thin wrapper around [`eval_is_null_junction_impl`] that creates local bindings,
    /// evaluates the IS_NULL rewrite, and finalizes the result.
    fn eval_is_null_junction(
        &self,
        op: JunctionPredicateOp,
        preds: &[Pred],
        value_inverted: bool,
        null_inverted: bool,
    ) -> Option<Self::Output> {
        let mut bindings = Self::Bindings::default();
        let result = self.eval_is_null_junction_impl(
            op,
            preds,
            &mut bindings,
            value_inverted,
            null_inverted,
            None,
        );
        bindings.finalize(result)
    }

    /// Fused version of [`eval_is_null_junction`] that returns both the junction value and its nullness.
    ///
    /// This computes `(junction_value, IS_NULL(junction))` in a single pass by evaluating each child
    /// only once and reusing the results for both the value and null-check computations via let-bindings.
    ///
    /// This is a thin wrapper around [`eval_is_null_junction_impl`] that uses the caller's bindings
    /// (for accumulation), captures value handles, and then computes the junction's original value
    /// from those handles.
    ///
    /// Returns a tuple of (value, is_null) results. All bindings are accumulated in the
    /// provided `bindings` parameter.
    fn eval_is_null_junction_fused(
        &self,
        op: JunctionPredicateOp,
        preds: &[Pred],
        bindings: &mut Self::Bindings,
        value_inverted: bool,
        null_inverted: bool,
    ) -> (Option<Self::Output>, Option<Self::Output>) {
        let mut value_handles = Vec::with_capacity(preds.len());

        // Evaluate IS_NULL rewrite, capturing value handles
        let is_null_result = self.eval_is_null_junction_impl(
            op,
            preds,
            bindings,
            value_inverted,
            null_inverted,
            Some(&mut value_handles),
        );

        // Compute the junction's original value from captured child value handles
        let mut value_clauses = value_handles.iter().map(|handle| bindings.retrieve(handle));
        let value_result = self.finish_eval_pred_junction(op, &mut value_clauses, value_inverted);

        (value_result, is_null_result)
    }

    /// A (possibly inverted) DISTINCT test, e.g. `[NOT] DISTINCT(<col>, false)`. DISTINCT can be
    /// seen as one of two operations, depending on the input:
    ///
    /// 1. `DISTINCT(<col>, NULL)` is equivalent to `<col> IS NOT NULL`
    /// 2. `DISTINCT(<col>, <value>)` is equivalent to `OR(<col> IS NULL, <col> != <value>)`
    fn eval_pred_distinct(
        &self,
        col: &ColumnName,
        val: &Scalar,
        inverted: bool,
    ) -> Option<Self::Output> {
        if let Scalar::Null(_) = val {
            self.eval_pred_column_is_null(col, !inverted)
        } else {
            let mut args = [
                self.eval_pred_column_is_null(col, inverted),
                self.eval_pred_eq(col, val, !inverted),
            ]
            .into_iter();
            self.finish_eval_pred_junction(JunctionPredicateOp::Or, &mut args, inverted)
        }
    }

    /// A (possibly inverted) IN-list check, e.g. `<col> [NOT] IN <array-value>`.
    ///
    /// Unsupported by default, but implementations can override it if they wish.
    fn eval_pred_in(
        &self,
        _col: &ColumnName,
        _val: &Scalar,
        _inverted: bool,
    ) -> Option<Self::Output> {
        None // TODO?
    }

    /// Dispatches a (possibly inverted) binary expression to each operator's specific implementation.
    ///
    /// NOTE: Only binary operators that produce boolean outputs are supported.
    fn eval_pred_binary(
        &self,
        op: BinaryPredicateOp,
        left: &Expr,
        right: &Expr,
        inverted: bool,
    ) -> Option<Self::Output> {
        use BinaryPredicateOp::*;
        use Expr::{Column, Literal};

        match (left, right) {
            (Column(a), Column(b)) => self.eval_pred_binary_columns(op, a, b, inverted),
            (Literal(a), Literal(b)) => self.eval_pred_binary_scalars(op, a, b, inverted),
            (Column(col), Literal(val)) => match op {
                LessThan => self.eval_pred_lt(col, val, inverted),
                GreaterThan => self.eval_pred_gt(col, val, inverted),
                Equal => self.eval_pred_eq(col, val, inverted),
                Distinct => self.eval_pred_distinct(col, val, inverted),
                In => self.eval_pred_in(col, val, inverted),
            },
            (Literal(val), Column(col)) => match op {
                // NOTE: The column has to be on the left, so e.g. `10 < x` becomes `x > 10`
                LessThan => self.eval_pred_gt(col, val, inverted),
                GreaterThan => self.eval_pred_lt(col, val, inverted),
                Equal => self.eval_pred_eq(col, val, inverted),
                Distinct => self.eval_pred_distinct(col, val, inverted),
                In => None, // arg order is semantically important
            },
            _ => {
                debug!("Unsupported binary operand(s): {left:?} {op:?} {right:?}");
                None
            }
        }
    }

    /// Dispatches a predicate junction operation (AND or OR), leveraging each implementation's
    /// [`Self::finish_eval_pred_junction`].
    fn eval_pred_junction(
        &self,
        op: JunctionPredicateOp,
        preds: &[Pred],
        inverted: bool,
    ) -> Option<Self::Output> {
        let mut preds = preds.iter().map(|pred| self.eval_pred(pred, inverted));
        self.finish_eval_pred_junction(op, &mut preds, inverted)
    }

    /// Dispatches a predicate to the specific implementation for each predicate variant.
    fn eval_pred(&self, pred: &Pred, inverted: bool) -> Option<Self::Output> {
        use Pred::*;
        match pred {
            BooleanExpression(expr) => self.eval_pred_expr(expr, inverted),
            Not(pred) => self.eval_pred_not(pred, inverted),
            Unary(UnaryPredicate { op, expr }) => self.eval_pred_unary(*op, expr, inverted),
            Binary(BinaryPredicate { op, left, right }) => {
                self.eval_pred_binary(*op, left, right, inverted)
            }
            Junction(JunctionPredicate { op, preds }) => {
                self.eval_pred_junction(*op, preds, inverted)
            }
            Let(let_pred) => self.eval_pred_let(&let_pred.bindings, &let_pred.body, inverted),
            Opaque(OpaquePredicate { op, exprs }) => self.eval_pred_opaque(op, exprs, inverted),
            Unknown(_) => None, // not supported by definition
        }
    }

    /// Evaluates a (possibly inverted) predicate with SQL WHERE semantics.
    ///
    /// By default, [`Self::eval_pred`] behaves badly for comparisons involving NULL columns
    /// (e.g. `a < 10` when `a` is NULL), because the comparison correctly evaluates to NULL, but
    /// NULL values are interpreted as "stats missing" (= cannot skip). This ambiguity can "poison"
    /// the entire predicate, causing it to return NULL instead of FALSE that would allow skipping:
    ///
    /// ```text
    /// WHERE a < 10 -- NULL (can't skip file)
    /// WHERE a < 10 AND TRUE -- NULL (can't skip file)
    /// WHERE a < 10 OR FALSE -- NULL (can't skip file)
    /// ```
    ///
    /// Meanwhile, SQL WHERE semantics only keeps rows for which the filter evaluates to
    /// TRUE (discarding rows that evaluate to FALSE or NULL):
    ///
    /// ```text
    /// WHERE a < 10 -- NULL (discard row)
    /// WHERE a < 10 AND TRUE -- NULL (discard row)
    /// WHERE a < 10 OR FALSE -- NULL (discard row)
    /// ```
    ///
    /// Conceptually, the behavior difference between data skipping and SQL WHERE semantics can be
    /// addressed by evaluating with null-safe semantics, as if by `<expr> IS NOT NULL AND <expr>`:
    ///
    /// ```text
    /// WHERE (a < 10) IS NOT NULL AND (a < 10) -- FALSE (skip file)
    /// WHERE (a < 10 AND TRUE) IS NOT NULL AND (a < 10 AND TRUE) -- FALSE (skip file)
    /// WHERE (a < 10 OR FALSE) IS NOT NULL AND (a < 10 OR FALSE) -- FALSE (skip file)
    /// ```
    ///
    /// HOWEVER, we cannot safely NULL-check the result of an arbitrary data skipping predicate
    /// because a predicate will also produce NULL if the value is just plain missing (e.g. data
    /// skipping over a column that lacks stats), and if that NULL should propagate all the way to
    /// top-level, it would be wrongly interpreted as FALSE (= skippable).
    ///
    /// To prevent wrong data skipping, the predicate evaluator always returns NULL for a NULL check
    /// over anything except for literals and columns with known values. So we must push the NULL
    /// check down through supported operations (AND as well as null-intolerant comparisons like
    /// `<`, `!=`, etc) until it reaches columns and literals where it can do some good, e.g.:
    ///
    /// ```text
    /// WHERE a < 10 AND (b < 20 OR c < 30)
    /// ```
    ///
    /// would conceptually be interpreted as
    ///
    /// ```text
    /// WHERE
    ///   (a < 10 AND (b < 20 OR c < 30)) IS NOT NULL AND
    ///   (a < 10 AND (b < 20 OR c < 30))
    /// ```
    ///
    /// We then push the NULL check down through the top-level AND:
    ///
    /// ```text
    /// WHERE
    ///   (a < 10 IS NOT NULL AND a < 10) AND
    ///   ((b < 20 OR c < 30) IS NOT NULL AND (b < 20 OR c < 30))
    /// ```
    ///
    /// and attempt to push it further into the `a < 10` and `OR` clauses:
    ///
    /// ```text
    /// WHERE
    ///   (a IS NOT NULL AND 10 IS NOT NULL AND a < 10) AND
    ///   (b < 20 OR c < 30)
    /// ```
    ///
    /// Any time the push-down reaches an operator that does not support push-down (such as OR), we
    /// simply drop the NULL check. This way, the top-level NULL check only applies to
    /// sub-predicates that can safely implement it, while ignoring other sub-predicates. The
    /// unsupported sub-predicates could produce nulls at runtime that prevent skipping, but false
    /// positives are OK -- the query will still correctly filter out the unwanted rows that result.
    ///
    /// At predicate evaluation time, a NULL value of `a` (from our example) would evaluate as:
    ///
    /// ```text
    /// AND(..., AND(a IS NOT NULL, 10 IS NOT NULL, a < 10), ...)
    /// AND(..., AND(FALSE, TRUE, NULL), ...)
    /// AND(..., FALSE, ...)
    /// FALSE
    /// ```
    ///
    /// While a non-NULL value of `a` would instead evaluate as:
    ///
    /// ```text
    /// AND(..., AND(a IS NOT NULL, 10 IS NOT NULL, a < 10), ...)
    /// AND(..., AND(TRUE, TRUE, <result>), ...)
    /// AND(..., <result>, ...)
    /// ```
    ///
    /// And a missing value for `a` would safely disable the clause:
    ///
    /// ```text
    /// AND(..., AND(a IS NOT NULL, 10 IS NOT NULL, a < 10), ...)
    /// AND(..., AND(NULL, TRUE, NULL), ...)
    /// AND(..., NULL, ...)
    /// ```
    ///
    /// WARNING: Not an idempotent transform. If data skipping eval produces a sql predicate,
    /// evaluating the result with sql semantics has undefined behavior.
    fn eval_pred_sql_where(&self, pred: &Pred, inverted: bool) -> Option<Self::Output> {
        use Pred::*;
        match pred {
            Junction(JunctionPredicate { op, preds }) => {
                // Recursively invoke `eval_pred_sql_where` instead of the usual `eval_pred` for AND/OR.
                let mut preds = preds
                    .iter()
                    .map(|pred| self.eval_pred_sql_where(pred, inverted));
                self.finish_eval_pred_junction(*op, &mut preds, inverted)
            }
            Binary(BinaryPredicate { op, left, right }) if op.is_null_intolerant() => {
                // Perform a nullsafe comparison instead of the usual `eval_pred_binary`
                let mut preds = [
                    self.eval_pred_unary(UnaryPredicateOp::IsNull, left, true),
                    self.eval_pred_unary(UnaryPredicateOp::IsNull, right, true),
                    self.eval_pred_binary(*op, left, right, inverted),
                ]
                .into_iter();
                self.finish_eval_pred_junction(JunctionPredicateOp::And, &mut preds, false)
            }
            Not(pred) => self.eval_pred_sql_where(pred, !inverted),
            BooleanExpression(Expr::Column(col)) => {
                // Perform a nullsafe comparison instead of the usual `eval_pred_column`
                let mut preds = [
                    self.eval_pred_column_is_null(col, true),
                    self.eval_pred_column(col, inverted),
                ]
                .into_iter();
                self.finish_eval_pred_junction(JunctionPredicateOp::And, &mut preds, false)
            }
            BooleanExpression(Expr::Literal(val)) if val.is_null() => {
                // AND(NULL IS NOT NULL, NULL) = AND(FALSE, NULL) = FALSE
                self.eval_pred_scalar(&Scalar::from(false), false)
            }
            BooleanExpression(Expr::Predicate(pred)) => self.eval_pred_sql_where(pred, inverted),
            // Process all remaining predicates normally, because they are not proven safe. Indeed,
            // predicates like DISTINCT and IS [NOT] NULL are known-unsafe under SQL semantics:
            //
            // ```
            // x IS NULL    # when x really is NULL
            // = AND(x IS NOT NULL, x IS NULL)
            // = AND(FALSE, TRUE)
            // = FALSE
            //
            // DISTINCT(x, 10)  # when x is NULL
            // = AND(x IS NOT NULL, 10 IS NOT NULL, DISTINCT(x, 10))
            // = AND(FALSE, TRUE, TRUE)
            // = FALSE
            //
            // DISTINCT(x, NULL) # when x is not NULL
            // = AND(x IS NOT NULL, NULL IS NOT NULL, DISTINCT(x, NULL))
            // = AND(TRUE, FALSE, TRUE)
            // = FALSE
            // ```
            //
            _ => self.eval_pred(pred, inverted),
        }
    }

    /// A convenient non-inverted wrapper for [`Self::eval_pred`]
    #[allow(unused)]
    fn eval(&self, pred: &Pred) -> Option<Self::Output> {
        self.eval_pred(pred, false)
    }

    /// A convenient non-inverted wrapper for [`Self::eval_pred_sql_where`].
    fn eval_sql_where(&self, pred: &Pred) -> Option<Self::Output> {
        self.eval_pred_sql_where(pred, false)
    }
}

/// A collection of provided methods from the [`KernelPredicateEvaluator`] trait, factored out to allow
/// reuse by multiple bool-output predicate evaluator implementations.
pub struct KernelPredicateEvaluatorDefaults;
impl KernelPredicateEvaluatorDefaults {
    /// Directly evaluates a boolean scalar. See [`KernelPredicateEvaluator::eval_pred_scalar`].
    pub fn eval_pred_scalar(val: &Scalar, inverted: bool) -> Option<bool> {
        match val {
            Scalar::Boolean(val) => Some(*val != inverted),
            _ => None,
        }
    }

    /// Directly null-tests a scalar. See [`KernelPredicateEvaluator::eval_pred_scalar_is_null`].
    pub fn eval_pred_scalar_is_null(val: &Scalar, inverted: bool) -> Option<bool> {
        Some(val.is_null() != inverted)
    }

    /// A (possibly inverted) partial comparison of two scalars, leveraging the [`PartialOrd`]
    /// trait.
    pub fn partial_cmp_scalars(
        ord: Ordering,
        a: &Scalar,
        b: &Scalar,
        inverted: bool,
    ) -> Option<bool> {
        let cmp = a.partial_cmp(b)?;
        let matched = cmp == ord;
        Some(matched != inverted)
    }

    /// Directly evaluates a boolean comparison. See [`KernelPredicateEvaluator::eval_pred_binary_scalars`].
    pub fn eval_pred_binary_scalars(
        op: BinaryPredicateOp,
        left: &Scalar,
        right: &Scalar,
        inverted: bool,
    ) -> Option<bool> {
        use BinaryPredicateOp::*;
        match op {
            Equal => Self::partial_cmp_scalars(Ordering::Equal, left, right, inverted),
            LessThan => Self::partial_cmp_scalars(Ordering::Less, left, right, inverted),
            GreaterThan => Self::partial_cmp_scalars(Ordering::Greater, left, right, inverted),
            Distinct | In => {
                debug!("Unsupported binary operator: {left:?} {op:?} {right:?}");
                None
            }
        }
    }

    /// Finishes evaluating a (possibly inverted) junction operation. See
    /// [`KernelPredicateEvaluator::finish_eval_pred_junction`].
    ///
    /// The inputs were already inverted by the caller, if needed.
    ///
    /// With AND (OR), any FALSE (TRUE) input dominates, forcing a FALSE (TRUE) output.  If there
    /// was no dominating input, then any NULL input forces NULL output.  Otherwise, return the
    /// non-dominant value. Inverting the operation also inverts the dominant value.
    pub fn finish_eval_pred_junction(
        op: JunctionPredicateOp,
        preds: &mut dyn Iterator<Item = Option<bool>>,
        inverted: bool,
    ) -> Option<bool> {
        let dominator = Self::junction_dominator(op, inverted);
        let mut found_null = false;
        for val in preds {
            match val {
                Some(val) if val == dominator => return Some(dominator), // short circuit!
                None => found_null = true,
                Some(_) => (), // ignore non-dominant values
            }
        }
        (!found_null).then_some(!dominator)
    }

    /// Returns the dominating value for a junction operation: FALSE dominates AND, while TRUE
    /// dominates OR. Inversion flips the dominator, following de Morgan's laws.
    ///
    /// NOTE: The "default" value of an empty junction is just the inverted dominator: AND() = TRUE,
    /// while OR() = FALSE.
    fn junction_dominator(op: JunctionPredicateOp, inverted: bool) -> bool {
        match op {
            JunctionPredicateOp::And => inverted,
            JunctionPredicateOp::Or => !inverted,
        }
    }
}

/// Resolves columns as scalars, as a building block for [`DefaultKernelPredicateEvaluator`].
pub(crate) trait ResolveColumnAsScalar {
    fn resolve_column(&self, col: &ColumnName) -> Option<Scalar>;
}

// Some tests do not actually require column resolution
#[cfg(test)]
pub(crate) struct UnimplementedColumnResolver;
#[cfg(test)]
impl ResolveColumnAsScalar for UnimplementedColumnResolver {
    fn resolve_column(&self, _col: &ColumnName) -> Option<Scalar> {
        unimplemented!()
    }
}

// Used internally and by some tests
pub(crate) struct EmptyColumnResolver;
impl ResolveColumnAsScalar for EmptyColumnResolver {
    fn resolve_column(&self, _col: &ColumnName) -> Option<Scalar> {
        None
    }
}

impl ResolveColumnAsScalar for std::collections::HashMap<ColumnName, Scalar> {
    fn resolve_column(&self, col: &ColumnName) -> Option<Scalar> {
        self.get(col).cloned()
    }
}

/// A predicate evaluator that directly evaluates the predicate to produce an `Option<bool>`
/// result. Column resolution is handled by an embedded [`ResolveColumnAsScalar`] instance.
pub(crate) struct DefaultKernelPredicateEvaluator<R: ResolveColumnAsScalar> {
    resolver: R,
    // TODO: Using String as key precludes drilling into struct bindings (e.g., binding.field).
    // Need to design a better solution for nested access in the future.
    bindings: std::cell::RefCell<indexmap::IndexMap<String, Scalar>>,
}

/// RAII guard for managing let-binding scope in DefaultKernelPredicateEvaluator
struct BindingScope<'a> {
    bindings: &'a std::cell::RefCell<indexmap::IndexMap<String, Scalar>>,
    saved_len: usize,
}

impl<'a> BindingScope<'a> {
    fn insert(&mut self, name: String, value: Scalar) {
        // Short-lived borrow for insertion
        self.bindings.borrow_mut().insert(name, value);
    }
}

impl Drop for BindingScope<'_> {
    fn drop(&mut self) {
        // Truncate bindings back to saved length when scope ends
        // Panic if we can't borrow - indicates a bug that would be caught by
        // the compiler when we switch to &mut self
        self.bindings.borrow_mut().truncate(self.saved_len);
    }
}

impl<R: ResolveColumnAsScalar> DefaultKernelPredicateEvaluator<R> {
    /// Get a binding by name, checking the let-binding stack first
    /// TODO: This only checks the first path segment - need better design for nested access
    fn get_binding(&self, col: &ColumnName) -> Option<Scalar> {
        let binding_name = col.as_ref().first()?;
        self.bindings
            .try_borrow()
            .ok()?
            .get(binding_name.as_str())
            .cloned()
    }

    /// Create a new binding scope for evaluating a Let node
    fn binding_scope(&self) -> BindingScope<'_> {
        let saved_len = self.bindings.borrow().len();
        BindingScope {
            bindings: &self.bindings,
            saved_len,
        }
    }

    /// Helper to evaluate and bind all predicates in a Let node
    fn bind_all_preds(&self, bindings: &[(String, Pred)]) -> Option<BindingScope<'_>> {
        let mut scope = self.binding_scope();
        for (name, pred) in bindings {
            // Bindings can evaluate to None (e.g., when min/max stats are NULL for all-null columns).
            // We store None values so they can be retrieved later, but actual use of None bindings
            // should be guarded by short-circuiting in the parent expression.
            if let Some(value) = self.eval_pred(pred, false) {
                scope.insert(name.clone(), Scalar::from(value));
            }
            // If None, we simply don't insert the binding (represented by absence in the map)
        }
        Some(scope)
    }

    // Convenient thin wrapper - checks bindings first, then falls back to resolver
    fn resolve_column(&self, col: &ColumnName) -> Option<Scalar> {
        self.get_binding(col)
            .or_else(|| self.resolver.resolve_column(col))
    }

    pub(crate) fn eval_expr(&self, expr: &Expr) -> Option<Scalar> {
        match expr {
            Expr::Literal(value) => Some(value.clone()),
            Expr::Column(name) => self.resolve_column(name),
            Expr::Predicate(pred) => self.eval_pred(pred, false).map(Scalar::from),
            Expr::Struct(_) | Expr::Transform(_) | Expr::Unary(_) => None, // TODO?
            Expr::Let(_) => {
                // TODO: Evaluate Let bindings for scalar expression evaluation.
                // A Let node evaluates its bindings in order (each can reference previous bindings),
                // then evaluates the body with all bindings available. For scalar evaluation, we
                // need a local HashMap<String, Scalar> to store binding results.
                None
            }
            Expr::Binary(BinaryExpression { op, left, right }) => {
                let op_fn = match op {
                    BinaryExpressionOp::Plus => Scalar::try_add,
                    BinaryExpressionOp::Minus => Scalar::try_sub,
                    BinaryExpressionOp::Multiply => Scalar::try_mul,
                    BinaryExpressionOp::Divide => Scalar::try_div,
                };
                op_fn(&self.eval_expr(left)?, &self.eval_expr(right)?)
            }
            Expr::Variadic(_) => None, // TODO
            Expr::Opaque(OpaqueExpression { op, exprs }) => op
                .eval_expr_scalar(&|expr| self.eval_expr(expr), exprs)
                .inspect_err(|err| {
                    warn!("Failed to evaluate {:?}: {err:?}", op.as_ref());
                })
                .ok(),
            Expr::Unknown(_) => None,
        }
    }
}

impl<R: ResolveColumnAsScalar + 'static> From<R> for DefaultKernelPredicateEvaluator<R> {
    fn from(resolver: R) -> Self {
        Self {
            resolver,
            bindings: std::cell::RefCell::new(indexmap::IndexMap::new()),
        }
    }
}

/// A "normal" predicate evaluator. It takes expressions as input, uses a [`ResolveColumnAsScalar`]
/// to convert column references to scalars, and evaluates the resulting constant expression to
/// produce a boolean output.
impl<R: ResolveColumnAsScalar> KernelPredicateEvaluator for DefaultKernelPredicateEvaluator<R> {
    type Output = bool;
    type Bindings = DirectLetBindings<bool>;

    fn eval_pred_let(
        &self,
        bindings: &[(String, Pred)],
        body: &Pred,
        inverted: bool,
    ) -> Option<Self::Output> {
        // Evaluate and bind all predicates, creating a scope guard
        let _scope = self.bind_all_preds(bindings)?;

        // Evaluate the body with all bindings available
        // The scope guard ensures bindings are cleaned up when it drops
        self.eval_pred(body, inverted)
    }

    fn eval_pred_scalar(&self, val: &Scalar, inverted: bool) -> Option<bool> {
        KernelPredicateEvaluatorDefaults::eval_pred_scalar(val, inverted)
    }

    fn eval_pred_scalar_is_null(&self, val: &Scalar, inverted: bool) -> Option<bool> {
        KernelPredicateEvaluatorDefaults::eval_pred_scalar_is_null(val, inverted)
    }

    fn eval_pred_column_is_null(&self, col: &ColumnName, inverted: bool) -> Option<bool> {
        let col = self.resolve_column(col)?;
        self.eval_pred_scalar_is_null(&col, inverted)
    }

    fn eval_pred_lt(&self, col: &ColumnName, val: &Scalar, inverted: bool) -> Option<bool> {
        let col = self.resolve_column(col)?;
        self.eval_pred_binary_scalars(BinaryPredicateOp::LessThan, &col, val, inverted)
    }

    fn eval_pred_gt(&self, col: &ColumnName, val: &Scalar, inverted: bool) -> Option<bool> {
        let col = self.resolve_column(col)?;
        self.eval_pred_binary_scalars(BinaryPredicateOp::GreaterThan, &col, val, inverted)
    }

    fn eval_pred_eq(&self, col: &ColumnName, val: &Scalar, inverted: bool) -> Option<bool> {
        let col = self.resolve_column(col)?;
        self.eval_pred_binary_scalars(BinaryPredicateOp::Equal, &col, val, inverted)
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

    fn eval_pred_binary_columns(
        &self,
        op: BinaryPredicateOp,
        left: &ColumnName,
        right: &ColumnName,
        inverted: bool,
    ) -> Option<bool> {
        let left = self.resolve_column(left)?;
        let right = self.resolve_column(right)?;
        self.eval_pred_binary_scalars(op, &left, &right, inverted)
    }

    fn eval_pred_opaque(
        &self,
        op: &OpaquePredicateOpRef,
        exprs: &[Expr],
        inverted: bool,
    ) -> Option<bool> {
        op.eval_pred_scalar(&|expr| self.eval_expr(expr), self, exprs, inverted)
            .unwrap_or_else(|err| {
                warn!("Unable to evaluate {:?}: {err:?}", op.as_ref());
                None
            })
    }

    fn eval_pred_expr_opaque(
        &self,
        op: &OpaqueExpressionOpRef,
        exprs: &[Expr],
        inverted: bool,
    ) -> Option<bool> {
        match op.eval_expr_scalar(&|expr| self.eval_expr(expr), exprs) {
            Ok(Scalar::Boolean(val)) => Some(val != inverted),
            Ok(Scalar::Null(DataType::BOOLEAN)) => None,
            Ok(other) => {
                warn!(
                    "Expected {:?} to produce a boolean value, but got {:?}",
                    op.as_ref(),
                    other.data_type()
                );
                None
            }
            Err(err) => {
                warn!("Unable to evaluate {:?}: {err:?}", op.as_ref());
                None
            }
        }
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

/// A predicate evaluator that implements data skipping semantics over various column stats. For
/// example, comparisons involving a column are converted into comparisons over that column's
/// min/max stats, and NULL checks are converted into comparisons involving the column's nullcount
/// and rowcount stats.
pub trait DataSkippingPredicateEvaluator {
    /// The output type produced by this predicate evaluator
    type Output;
    /// The type for column stats consumed by this predicate evaluator
    type ColumnStat;
    /// Let-bindings strategy for managing intermediate results during evaluation
    type Bindings: LetBindings<Output = Self::Output>;

    /// Retrieves the minimum value of a column, if it exists and has the requested type.
    fn get_min_stat(&self, col: &ColumnName, data_type: &DataType) -> Option<Self::ColumnStat>;

    /// Retrieves the maximum value of a column, if it exists and has the requested type.
    fn get_max_stat(&self, col: &ColumnName, data_type: &DataType) -> Option<Self::ColumnStat>;

    /// Retrieves the null count of a column, if it exists.
    fn get_nullcount_stat(&self, col: &ColumnName) -> Option<Self::ColumnStat>;

    /// Retrieves the row count of a column (parquet footers always include this stat).
    fn get_rowcount_stat(&self) -> Option<Self::ColumnStat>;

    /// See [`KernelPredicateEvaluator::eval_pred_scalar`]
    fn eval_pred_scalar(&self, val: &Scalar, inverted: bool) -> Option<Self::Output>;

    /// See [`KernelPredicateEvaluator::eval_pred_scalar_is_null`]
    fn eval_pred_scalar_is_null(&self, val: &Scalar, inverted: bool) -> Option<Self::Output>;

    /// For IS NULL (IS NOT NULL), we can only skip the file if all-null (no-null). Any other
    /// nullcount always forces us to keep the file.
    ///
    /// NOTE: When deletion vectors are enabled, they could produce a file that is logically
    /// all-null or logically no-null, even tho the physical stats indicate a mix of null and
    /// non-null values. They cannot invalidate a file's physical all-null or non-null status,
    /// however, so the worst that can happen is we fail to skip an unnecessary file.
    fn eval_pred_is_null(&self, col: &ColumnName, inverted: bool) -> Option<Self::Output>;

    /// See [`KernelPredicateEvaluator::eval_pred_binary_scalars`]
    fn eval_pred_binary_scalars(
        &self,
        op: BinaryPredicateOp,
        left: &Scalar,
        right: &Scalar,
        inverted: bool,
    ) -> Option<Self::Output>;

    /// See [`KernelPredicateEvaluator::eval_pred_opaque`].
    fn eval_pred_opaque(
        &self,
        op: &OpaquePredicateOpRef,
        exprs: &[Expr],
        inverted: bool,
    ) -> Option<Self::Output>;

    /// See [`KernelPredicateEvaluator::finish_eval_pred_junction`]
    fn finish_eval_pred_junction(
        &self,
        op: JunctionPredicateOp,
        preds: &mut dyn Iterator<Item = Option<Self::Output>>,
        inverted: bool,
    ) -> Option<Self::Output>;

    /// Helper method that performs a (possibly inverted) partial comparison between a typed column
    /// stat and a scalar.
    fn eval_partial_cmp(
        &self,
        ord: Ordering,
        col: Self::ColumnStat,
        val: &Scalar,
        inverted: bool,
    ) -> Option<Self::Output>;

    /// Performs a partial comparison against a column min-stat. See
    /// [`KernelPredicateEvaluatorDefaults::partial_cmp_scalars`] for details of the comparison semantics.
    fn partial_cmp_min_stat(
        &self,
        col: &ColumnName,
        val: &Scalar,
        ord: Ordering,
        inverted: bool,
    ) -> Option<Self::Output> {
        let min = self.get_min_stat(col, &val.data_type())?;
        self.eval_partial_cmp(ord, min, val, inverted)
    }

    /// Performs a partial comparison against a column max-stat. See
    /// [`KernelPredicateEvaluatorDefaults::partial_cmp_scalars`] for details of the comparison semantics.
    fn partial_cmp_max_stat(
        &self,
        col: &ColumnName,
        val: &Scalar,
        ord: Ordering,
        inverted: bool,
    ) -> Option<Self::Output> {
        let max = self.get_max_stat(col, &val.data_type())?;
        self.eval_partial_cmp(ord, max, val, inverted)
    }

    /// See [`KernelPredicateEvaluator::eval_pred_lt`]
    fn eval_pred_lt(&self, col: &ColumnName, val: &Scalar, inverted: bool) -> Option<Self::Output> {
        if inverted {
            // Given `col >= val`:
            // Skip if `val is greater than _every_ value in [min, max], implies
            // Skip if `val > min AND val > max` implies
            // Skip if `val > max` implies
            // Keep if `NOT(val > max)` implies
            // Keep if `NOT(max < val)`
            self.partial_cmp_max_stat(col, val, Ordering::Less, true)
        } else {
            // Given `col < val`:
            // Skip if `val` is not greater than _all_ values in [min, max], implies
            // Skip if `val <= min AND val <= max` implies
            // Skip if `val <= min` implies
            // Keep if `NOT(val <= min)` implies
            // Keep if `val > min` implies
            // Keep if `min < val`
            self.partial_cmp_min_stat(col, val, Ordering::Less, false)
        }
    }

    /// See [`KernelPredicateEvaluator::eval_pred_gt`]
    fn eval_pred_gt(&self, col: &ColumnName, val: &Scalar, inverted: bool) -> Option<Self::Output> {
        if inverted {
            // Given `col <= val`:
            // Skip if `val` is less than _all_ values in [min, max], implies
            // Skip if `val < min AND val < max` implies
            // Skip if `val < min` implies
            // Keep if `NOT(val < min)` implies
            // Keep if `NOT(min > val)`
            self.partial_cmp_min_stat(col, val, Ordering::Greater, true)
        } else {
            // Given `col > val`:
            // Skip if `val` is not less than _all_ values in [min, max], implies
            // Skip if `val >= min AND val >= max` implies
            // Skip if `val >= max` implies
            // Keep if `NOT(val >= max)` implies
            // Keep if `NOT(max <= val)` implies
            // Keep if `max > val`
            self.partial_cmp_max_stat(col, val, Ordering::Greater, false)
        }
    }

    /// See [`KernelPredicateEvaluator::eval_pred_eq`]
    fn eval_pred_eq(&self, col: &ColumnName, val: &Scalar, inverted: bool) -> Option<Self::Output> {
        let (op, preds) = if inverted {
            // Column could compare not-equal if min or max value differs from the literal.
            let preds = [
                self.partial_cmp_min_stat(col, val, Ordering::Equal, true),
                self.partial_cmp_max_stat(col, val, Ordering::Equal, true),
            ];
            (JunctionPredicateOp::Or, preds)
        } else {
            // Column could compare equal if its min/max values bracket the literal.
            let preds = [
                self.partial_cmp_min_stat(col, val, Ordering::Greater, true),
                self.partial_cmp_max_stat(col, val, Ordering::Less, true),
            ];
            (JunctionPredicateOp::And, preds)
        };
        self.finish_eval_pred_junction(op, &mut preds.into_iter(), false)
    }
}

impl<T: DataSkippingPredicateEvaluator + ?Sized> KernelPredicateEvaluator for T {
    type Output = T::Output;
    type Bindings = T::Bindings;

    fn eval_pred_scalar(&self, val: &Scalar, inverted: bool) -> Option<Self::Output> {
        self.eval_pred_scalar(val, inverted)
    }

    fn eval_pred_scalar_is_null(&self, val: &Scalar, inverted: bool) -> Option<Self::Output> {
        self.eval_pred_scalar_is_null(val, inverted)
    }

    fn eval_pred_column_is_null(&self, col: &ColumnName, inverted: bool) -> Option<Self::Output> {
        self.eval_pred_is_null(col, inverted)
    }

    fn eval_pred_lt(&self, col: &ColumnName, val: &Scalar, inverted: bool) -> Option<Self::Output> {
        self.eval_pred_lt(col, val, inverted)
    }

    fn eval_pred_gt(&self, col: &ColumnName, val: &Scalar, inverted: bool) -> Option<Self::Output> {
        self.eval_pred_gt(col, val, inverted)
    }

    fn eval_pred_eq(&self, col: &ColumnName, val: &Scalar, inverted: bool) -> Option<Self::Output> {
        self.eval_pred_eq(col, val, inverted)
    }

    fn eval_pred_binary_scalars(
        &self,
        op: BinaryPredicateOp,
        left: &Scalar,
        right: &Scalar,
        inverted: bool,
    ) -> Option<Self::Output> {
        self.eval_pred_binary_scalars(op, left, right, inverted)
    }

    // NOTE: We rely on the literal values to provide logical type hints. That means we cannot
    // perform column-column comparisons, because we cannot infer the logical type to use.
    fn eval_pred_binary_columns(
        &self,
        _op: BinaryPredicateOp,
        _a: &ColumnName,
        _b: &ColumnName,
        _inverted: bool,
    ) -> Option<Self::Output> {
        None
    }

    fn eval_pred_opaque(
        &self,
        op: &OpaquePredicateOpRef,
        exprs: &[Expr],
        inverted: bool,
    ) -> Option<Self::Output> {
        self.eval_pred_opaque(op, exprs, inverted)
    }

    fn eval_pred_expr_opaque(
        &self,
        _op: &OpaqueExpressionOpRef,
        _exprs: &[Expr],
        _inverted: bool,
    ) -> Option<Self::Output> {
        None // Unsupported
    }

    fn finish_eval_pred_junction(
        &self,
        op: JunctionPredicateOp,
        preds: &mut dyn Iterator<Item = Option<Self::Output>>,
        inverted: bool,
    ) -> Option<Self::Output> {
        self.finish_eval_pred_junction(op, preds, inverted)
    }

    fn eval_pred_let(
        &self,
        _bindings: &[(String, Pred)],
        _body: &Pred,
        _inverted: bool,
    ) -> Option<Self::Output> {
        // TODO: Indirect data skipping evaluation should recursively rewrite Let bindings and body.
        // For now, return None since Let nodes aren't expected in non-rewritten predicates.
        // Direct data skipping never produces Let nodes, and rewritten predicates (which contain
        // Let nodes) should be evaluated with DefaultKernelPredicateEvaluator, not data skipping.
        None
    }
}
