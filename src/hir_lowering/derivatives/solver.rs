use crate::analysis::DominatorTree;
use crate::hir_lowering::derivatives::{DerivativeMap, PartialDerivativeMap, Unknown};
use crate::hir_lowering::error::Error;
use crate::hir_lowering::error::Type::OnlyNumericExpressionsCanBeDerived;
use crate::hir_lowering::HirToMirFold;
use crate::ir::mir::ControlFlowGraph;
use crate::ir::{IntegerExpressionId, Push, RealExpressionId, VariableId};
use crate::mir::{ExpressionId, RealExpression, Statement};
use log::warn;

impl<'tag, 'lt> HirToMirFold<'tag, 'lt> {
    fn partial_derivative_assignment(
        &mut self,
        new_derivatives: &mut DerivativeMap<'tag>,
        expr: RealExpressionId<'tag>,
        derive_by: Unknown<'tag>,
    ) -> RealExpressionId<'tag> {
        self.partial_derivative(expr, derive_by, &mut |fold, var| {
            let mir = &mut fold.mir;
            *fold
                .variable_to_differentiate
                .entry(var)
                .or_insert_with(|| {
                    PartialDerivativeMap::with_capacity_and_hasher(2, Default::default())
                })
                .entry(derive_by)
                .or_insert_with(|| {
                    let derivative = mir.declare_partial_derivative_variable(var, derive_by);
                    new_derivatives
                        .entry(var)
                        .or_insert_with(|| {
                            PartialDerivativeMap::with_capacity_and_hasher(2, Default::default())
                        })
                        .insert(derive_by, derivative);
                    derivative
                })
        })
        .unwrap_or_else(|| {
            self.mir
                .push(self.mir[expr].clone_as(RealExpression::Literal(0.0)))
        })
    }

    fn partial_derivative_integer_assignment(
        &mut self,
        new_derivatives: &mut DerivativeMap<'tag>,
        expr: IntegerExpressionId<'tag>,
        derive_by: Unknown<'tag>,
    ) -> RealExpressionId<'tag> {
        self.partial_derivative_of_integer_expression(expr, derive_by, &mut |fold, var| {
            let mir = &mut fold.mir;
            *fold
                .variable_to_differentiate
                .entry(var)
                .or_insert_with(|| {
                    PartialDerivativeMap::with_capacity_and_hasher(2, Default::default())
                })
                .entry(derive_by)
                .or_insert_with(|| {
                    let derivative = mir.declare_partial_derivative_variable(var, derive_by);
                    new_derivatives
                        .entry(var)
                        .or_insert_with(|| {
                            PartialDerivativeMap::with_capacity_and_hasher(2, Default::default())
                        })
                        .insert(derive_by, derivative);
                    derivative
                })
        })
        .unwrap_or_else(|| {
            self.mir
                .push(self.mir[expr].clone_as(RealExpression::Literal(0.0)))
        })
    }

    pub(super) fn solve(
        &mut self,
        cfg: &mut ControlFlowGraph<'tag, 'tag>,
        dtree: &DominatorTree<'tag>,
    ) {
        let mut new_variable_to_differentiate = DerivativeMap::default();
        let mut working_set = self.variable_to_differentiate.clone();
        let mut iteration_range = 0..u16::MAX;
        while !working_set.is_empty() && iteration_range.next().is_some() {
            dtree.postorder_cfg_visit(|block| {
                let mut idx = cfg[block].statements.len();
                while idx > 0 {
                    idx -= 1;
                    let statements = &mut cfg[block].statements;
                    // statements only ever grows so this is save
                    let stmt = unsafe { *statements.get_unchecked(idx) };
                    if let Statement::Assignment(attr, dst, val) = self.mir[stmt] {
                        if let Some(partials) = working_set.get(&dst) {
                            match val {
                                ExpressionId::Real(val) => {
                                    let added = partials.len();
                                    statements.splice(
                                        idx..idx,
                                        partials.into_iter().map(|(&derive_by, &variable)| {
                                            let expr = self.partial_derivative_assignment(
                                                &mut new_variable_to_differentiate,
                                                val,
                                                derive_by,
                                            );
                                            self.mir.push(Statement::Assignment(
                                                attr,
                                                variable,
                                                ExpressionId::Real(expr),
                                            ))
                                        }),
                                    );
                                    idx += added;
                                }
                                ExpressionId::Integer(val) => {
                                    let added = partials.len();
                                    statements.splice(
                                        idx..idx,
                                        partials.into_iter().map(|(&derive_by, &variable)| {
                                            let expr = self.partial_derivative_integer_assignment(
                                                &mut new_variable_to_differentiate,
                                                val,
                                                derive_by,
                                            );
                                            self.mir.push(Statement::Assignment(
                                                attr,
                                                variable,
                                                ExpressionId::Real(expr),
                                            ))
                                        }),
                                    );
                                    idx += added;
                                }
                                ExpressionId::String(val) => {
                                    self.errors.push(Error {
                                        error_type: OnlyNumericExpressionsCanBeDerived,
                                        source: self.mir[val].source,
                                    });
                                }
                            }
                        }
                    }
                }
            });
            std::mem::swap(&mut working_set, &mut new_variable_to_differentiate);
            new_variable_to_differentiate.clear();
        }
        if !working_set.is_empty() {
            let names: Vec<_> = working_set
                .keys()
                .map(|&var| self.mir[var].contents.name)
                .collect();
            warn!(
                "The derivative solver reached no solution.\\ This is a bug please report this\\ Still processing {:#?}",
                names
            )
        }
    }
}
