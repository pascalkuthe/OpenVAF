use crate::hir_lowering::derivatives::{DerivativeMap, PartialDerivativeMap, Unknown};
use crate::hir_lowering::error::Error;
use crate::hir_lowering::error::Type::OnlyNumericExpressionsCanBeDerived;
use crate::hir_lowering::HirToMirFold;
use crate::ir::{IntegerExpressionId, RealExpressionId};
use crate::mir::{ExpressionId, RealExpression, Statement};
use crate::ControlFlowGraph;
use log::warn;

impl<'lt> HirToMirFold<'lt> {
    fn partial_derivative_assignment(
        &mut self,
        new_derivatives: &mut DerivativeMap,
        expr: RealExpressionId,
        derive_by: Unknown,
    ) -> RealExpressionId {
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
                .real_expressions
                .push(self.mir[expr].clone_as(RealExpression::Literal(0.0)))
        })
    }

    fn partial_derivative_integer_assignment(
        &mut self,
        new_derivatives: &mut DerivativeMap,
        expr: IntegerExpressionId,
        derive_by: Unknown,
    ) -> RealExpressionId {
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
                .real_expressions
                .push(self.mir[expr].clone_as(RealExpression::Literal(0.0)))
        })
    }

    pub(super) fn solve(&mut self, cfg: &mut ControlFlowGraph) {
        let mut new_variable_to_differentiate = DerivativeMap::default();
        let mut working_set = self.variable_to_differentiate.clone();
        let mut iteration_range = 0..u16::MAX;
        while !working_set.is_empty() && iteration_range.next().is_some() {
            for (_, block) in cfg.postorder_itermut() {
                let mut idx = block.statements.len();
                while idx > 0 {
                    idx -= 1;
                    let stmt = block.statements[idx];
                    if let Statement::Assignment(attr, dst, val) = self.mir[stmt] {
                        if let Some(partials) = working_set.get(&dst) {
                            match val {
                                ExpressionId::Real(val) => {
                                    let added = partials.len();
                                    block.statements.splice(
                                        idx..idx,
                                        partials.iter().map(|(&derive_by, &variable)| {
                                            let expr = self.partial_derivative_assignment(
                                                &mut new_variable_to_differentiate,
                                                val,
                                                derive_by,
                                            );
                                            self.mir.statements.push(Statement::Assignment(
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
                                    block.statements.splice(
                                        idx..idx,
                                        partials.iter().map(|(&derive_by, &variable)| {
                                            let expr = self.partial_derivative_integer_assignment(
                                                &mut new_variable_to_differentiate,
                                                val,
                                                derive_by,
                                            );
                                            self.mir.statements.push(Statement::Assignment(
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
            }

            std::mem::swap(&mut working_set, &mut new_variable_to_differentiate);
            new_variable_to_differentiate.clear();
        }
        if !working_set.is_empty() {
            let names: Vec<_> = working_set
                .keys()
                .map(|&var| self.mir[var].contents.name)
                .collect();
            warn!(
                "The derivative solver reached no solution.\n This is a bug please report this\n Still processing {:#?}",
                names
            )
        }
    }
}
