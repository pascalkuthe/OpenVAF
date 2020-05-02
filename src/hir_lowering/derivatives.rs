/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
*/

use crate::ast::UnaryOperator;
use crate::hir::Branch;
use crate::hir::DisciplineAccess;
use crate::hir_lowering::error::Error;
use crate::hir_lowering::error::Type::OnlyNumericExpressionsCanBeDerived;
use crate::hir_lowering::HirToMirFold;
use crate::ir::mir::RealExpression::Literal;
use crate::ir::mir::{
    ComparisonOperator, ExpressionId, IntegerBinaryOperator, RealExpression, Variable, VariableType,
};
use crate::ir::{
    AttributeNode, Attributes, BranchId, BuiltInFunctionCall1p, BuiltInFunctionCall2p,
    IntegerExpressionId, NetId, Node, ParameterId, Push, RealExpressionId, SafeRangeCreation,
    StatementId, VariableId,
};
use crate::mir::{IntegerExpression, Mir, RealBinaryOperator, Statement};
use crate::symbol::{Ident, Symbol};
use crate::Span;
use rustc_hash::{FxHashMap, FxHashSet};

pub(super) type PartialDerivativeMap<'tag> = FxHashMap<Unknown<'tag>, VariableId<'tag>>;

pub(super) type DerivativeMap<'tag> = FxHashMap<VariableId<'tag>, PartialDerivativeMap<'tag>>;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Unknown<'tag> {
    Parameter(ParameterId<'tag>),
    NodePotential(NetId<'tag>),
    PortFlow(NetId<'tag>),
    Flow(BranchId<'tag>),
    Temperature,
}

impl<'tag, 'lt> HirToMirFold<'tag, 'lt> {
    pub(super) fn generate_partial_derivative_assignment(
        &mut self,
        partial_derivatives: PartialDerivativeMap<'tag>,
        attr: Attributes<'tag>,
        value: ExpressionId<'tag>,
        block: &mut Vec<StatementId<'tag>>,
    ) {
        for (derive_by, derivative_variable) in partial_derivatives {
            let partial_derivatives = self.variable_to_differentiate.remove(&derivative_variable);

            let derivative_expr = match value {
                ExpressionId::Real(value) => self.partial_derivative(value, derive_by),

                ExpressionId::Integer(value) => {
                    self.partial_derivative_of_integer_expression(value, derive_by)
                }
                ExpressionId::String(value) => {
                    self.errors.push(Error {
                        error_type: OnlyNumericExpressionsCanBeDerived,
                        source: self.mir[value].source,
                    });
                    return;
                }
            };

            let derivative_expr = ExpressionId::Real(derivative_expr.unwrap_or_else(|| {
                self.mir
                    .push(Node::new(Literal(0.0), Span::new_short_empty_span(0)))
            }));

            let stmt = self.mir.push(Statement::Assignment(
                attr,
                derivative_variable,
                derivative_expr,
            ));
            block.push(stmt);
            if let Some(partial_derivatives) = partial_derivatives {
                self.generate_partial_derivative_assignment(
                    partial_derivatives,
                    attr,
                    derivative_expr,
                    block,
                );
            }
        }
    }

    /// # Args
    /// * `derivatives_before_branch` - The derivative map of the fold cloned before the branch was folded
    /// * `merge_variables` - A Closure `(expected_after_branch: VariableId<'tag>, expected_inside_branch: VariableId<'tag>) that is called when two conflicting (same variable derived over the same unknown) derivatives are detected.
    pub(super) fn merge_branched_derivatives(
        variable_to_differentiate: &mut DerivativeMap<'tag>,
        derivatives_before_branch: DerivativeMap<'tag>,
        mut merge_variables: impl FnMut(VariableId<'tag>, VariableId<'tag>),
        mut handel_derivative_inside_branch: impl FnMut(VariableId<'tag>) -> (),
    ) {
        for (derived_variable, mut partial_derivatives_before_branch) in derivatives_before_branch {
            variable_to_differentiate
                .entry(derived_variable)
                .and_modify(|partial_derivatives| {
                    for (derived_by, &mut derivative) in partial_derivatives {
                        if let Some(&old_derivative) =
                            partial_derivatives_before_branch.get(derived_by)
                        {
                            if old_derivative != derivative {
                                merge_variables(old_derivative, derivative);
                                handel_derivative_inside_branch(derived_variable);
                            }
                        }
                    }
                })
                .or_insert_with(|| {
                    handel_derivative_inside_branch(derived_variable);
                    partial_derivatives_before_branch
                });
        }
    }

    pub fn partial_derivative(
        &mut self,
        expr: RealExpressionId<'tag>,
        derive_by: Unknown<'tag>,
    ) -> Option<RealExpressionId<'tag>> {
        let res = match self.mir[expr].contents {
            RealExpression::VariableReference(variable) => {
                let mir = &mut self.mir; //necessary because the borrow checker cant split the borrows inside closures apparently
                let derivative = *self
                    .variable_to_differentiate
                    .entry(variable)
                    .or_insert_with(|| FxHashMap::with_capacity_and_hasher(2, Default::default()))
                    .entry(derive_by)
                    .or_insert_with(|| {
                        mir.declare_partial_derivative_variable(variable, derive_by)
                    });
                RealExpression::VariableReference(derivative)
            }

            RealExpression::BinaryOperator(lhs, op, rhs) => {
                let lhs_derived = self.partial_derivative(lhs, derive_by);
                let rhs_derived = self.partial_derivative(rhs, derive_by);
                match (lhs_derived, rhs_derived) {
                    (None, None) => return None,
                    (Some(lhs_derived), Some(rhs_derived)) => {
                        match op.contents {
                            RealBinaryOperator::Sum | RealBinaryOperator::Subtract => {
                                RealExpression::BinaryOperator(lhs_derived, op, rhs_derived)
                            }
                            RealBinaryOperator::Multiply => {
                                let sum1 = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(lhs_derived, op, rhs),
                                ));
                                let sum2 = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(lhs, op, rhs_derived),
                                ));
                                RealExpression::BinaryOperator(
                                    sum1,
                                    op.copy_as(RealBinaryOperator::Sum),
                                    sum2,
                                ) //lhs'*rhs + lhs*rhs'
                            }

                            RealBinaryOperator::Divide => {
                                // (lhs'*rhs-lhs * rhs') / (rhs*rhs)
                                let sum1 = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        lhs_derived,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        rhs,
                                    ),
                                ));
                                let sum2 = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        lhs,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        rhs_derived,
                                    ),
                                ));
                                let top = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        sum1,
                                        op.copy_as(RealBinaryOperator::Subtract),
                                        sum2,
                                    ),
                                ));
                                let bottom = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        rhs,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        rhs,
                                    ),
                                ));
                                RealExpression::BinaryOperator(top, op, bottom)
                            }

                            RealBinaryOperator::Exponent => {
                                // (rhs/lhs * lhs' + ln (lhs) * rhs')*expr
                                let ratio = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        rhs,
                                        op.copy_as(RealBinaryOperator::Divide),
                                        lhs,
                                    ),
                                ));
                                let sum1 = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        ratio,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        lhs_derived,
                                    ),
                                ));

                                let ln_lhs = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BuiltInFunctionCall1p(
                                        BuiltInFunctionCall1p::Ln,
                                        lhs,
                                    ),
                                ));
                                let sum2 = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        ln_lhs,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        rhs_derived,
                                    ),
                                ));

                                let inner_derivative = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        sum1,
                                        op.copy_as(RealBinaryOperator::Sum),
                                        sum2,
                                    ),
                                ));

                                RealExpression::BinaryOperator(
                                    inner_derivative,
                                    op.copy_as(RealBinaryOperator::Multiply),
                                    expr,
                                )
                            }

                            RealBinaryOperator::Modulus => todo!("Error"),
                        }
                    }

                    (None, Some(rhs_derived)) => {
                        match op.contents {
                            RealBinaryOperator::Sum => return Some(rhs_derived),
                            RealBinaryOperator::Subtract => {
                                RealExpression::Negate(op.source, rhs_derived)
                            }
                            RealBinaryOperator::Multiply => {
                                RealExpression::BinaryOperator(lhs, op, rhs_derived)
                            }
                            RealBinaryOperator::Exponent => {
                                // (ln (lhs) * rhs') * expr
                                let ln_lhs = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BuiltInFunctionCall1p(
                                        BuiltInFunctionCall1p::Ln,
                                        lhs,
                                    ),
                                ));
                                let inner_derivative = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        ln_lhs,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        rhs_derived,
                                    ),
                                ));

                                RealExpression::BinaryOperator(
                                    inner_derivative,
                                    op.copy_as(RealBinaryOperator::Multiply),
                                    expr,
                                )
                            }
                            RealBinaryOperator::Divide => {
                                // lhs * rhs' / (rhs*rhs)

                                let top = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        lhs,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        rhs_derived,
                                    ),
                                ));

                                let bottom = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        rhs,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        rhs,
                                    ),
                                ));

                                RealExpression::BinaryOperator(top, op, bottom)
                            }
                            RealBinaryOperator::Modulus => todo!("Error"),
                        }
                    }
                    (Some(lhs_derived), None) => {
                        match op.contents {
                            RealBinaryOperator::Sum | RealBinaryOperator::Subtract => {
                                return Some(lhs_derived)
                            }

                            RealBinaryOperator::Multiply | RealBinaryOperator::Divide => {
                                RealExpression::BinaryOperator(lhs_derived, op, rhs)
                            } // rhs is just a scalar (x/c->x'/c)

                            RealBinaryOperator::Exponent => {
                                // (rhs/lhs*lhs') * expr
                                let ratio = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        rhs,
                                        op.copy_as(RealBinaryOperator::Divide),
                                        lhs,
                                    ),
                                ));
                                let inner_derivative = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        ratio,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        lhs_derived,
                                    ),
                                ));
                                RealExpression::BinaryOperator(
                                    inner_derivative,
                                    op.copy_as(RealBinaryOperator::Multiply),
                                    expr,
                                )
                            }

                            RealBinaryOperator::Modulus => todo!("Error"),
                        }
                    }
                }
            }

            RealExpression::Negate(span, expr) => {
                RealExpression::Negate(span, self.partial_derivative(expr, derive_by)?)
            }

            RealExpression::Condition(cond, question_span, true_val, colon_span, else_val) => {
                let true_val = self.partial_derivative(true_val, derive_by);
                let else_val = self.partial_derivative(else_val, derive_by);
                if true_val.is_none() && else_val.is_none() {
                    return None;
                }
                let else_val = else_val.unwrap_or(
                    self.mir
                        .push(self.mir[expr].clone_as(RealExpression::Literal(0.0))),
                );

                let true_val = true_val.unwrap_or(
                    self.mir
                        .push(self.mir[expr].clone_as(RealExpression::Literal(0.0))),
                );

                RealExpression::Condition(cond, question_span, true_val, colon_span, else_val)
            }

            RealExpression::FunctionCall(_, _) => todo!("Function calls"),

            RealExpression::BuiltInFunctionCall1p(call, arg) => {
                let inner_derivative = self.partial_derivative(arg, derive_by)?;

                match call {
                    BuiltInFunctionCall1p::Ln => RealExpression::BinaryOperator(
                        inner_derivative,
                        self.mir[expr].clone_as(RealBinaryOperator::Divide),
                        arg,
                    ),

                    BuiltInFunctionCall1p::Exp => RealExpression::BinaryOperator(
                        inner_derivative,
                        self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                        expr,
                    ),

                    BuiltInFunctionCall1p::Sqrt => {
                        // f'/(2*sqrt(f))
                        let two = self
                            .mir
                            .push(self.mir[expr].clone_as(RealExpression::Literal(2.0)));
                        let bottom =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    two,
                                    self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                                    expr,
                                )));
                        RealExpression::BinaryOperator(
                            inner_derivative,
                            self.mir[expr].clone_as(RealBinaryOperator::Divide),
                            bottom,
                        )
                    }

                    BuiltInFunctionCall1p::Log => {
                        // (f'/f)*log10_e
                        let log10_e =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::Literal(
                                    std::f64::consts::E.log10(),
                                )));

                        let outer =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    log10_e,
                                    self.mir[expr].clone_as(RealBinaryOperator::Divide),
                                    expr,
                                )));

                        RealExpression::BinaryOperator(
                            inner_derivative,
                            self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                            outer,
                        )
                    }

                    BuiltInFunctionCall1p::Abs => {
                        let literal_0 = self
                            .mir
                            .push(self.mir[expr].clone_as(RealExpression::Literal(0.0)));
                        RealExpression::Condition(
                            self.mir.push(self.mir[expr].clone_as(
                                IntegerExpression::RealComparison(
                                    arg,
                                    self.mir[expr].clone_as(ComparisonOperator::GreaterThen),
                                    literal_0,
                                ),
                            )),
                            self.mir[expr].source,
                            inner_derivative,
                            self.mir[expr].source,
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::Negate(
                                    self.mir[expr].source,
                                    inner_derivative,
                                ))),
                        )
                    }

                    // Undefined for integers. 0 rest of the time. Your problem if you are stupid enough to derive that we are just going to treat it like 0
                    // TODO warn ( or error?)
                    BuiltInFunctionCall1p::Floor | BuiltInFunctionCall1p::Ceil => return None,

                    BuiltInFunctionCall1p::Sin => {
                        let outer_derivative = self.mir.push(self.mir[expr].clone_as(
                            RealExpression::BuiltInFunctionCall1p(BuiltInFunctionCall1p::Cos, arg),
                        ));
                        RealExpression::BinaryOperator(
                            inner_derivative,
                            self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                            outer_derivative,
                        )
                    }

                    BuiltInFunctionCall1p::Cos => {
                        let sin = self.mir.push(self.mir[expr].clone_as(
                            RealExpression::BuiltInFunctionCall1p(BuiltInFunctionCall1p::Sin, arg),
                        ));
                        let neg_sin = self.mir.push(
                            self.mir[expr]
                                .clone_as(RealExpression::Negate(self.mir[expr].source, sin)),
                        );

                        RealExpression::BinaryOperator(
                            inner_derivative,
                            self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                            neg_sin,
                        )
                    }

                    BuiltInFunctionCall1p::Tan => {
                        // f'*(1+tan^2(f))
                        let squared =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    expr,
                                    self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                                    expr,
                                )));
                        let one = self
                            .mir
                            .push(self.mir[expr].clone_as(RealExpression::Literal(1.0)));

                        let outer =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    one,
                                    self.mir[expr].clone_as(RealBinaryOperator::Sum),
                                    squared,
                                )));

                        RealExpression::BinaryOperator(
                            inner_derivative,
                            self.mir[expr].clone_as(RealBinaryOperator::Divide),
                            outer,
                        )
                    }

                    BuiltInFunctionCall1p::ArcSin => {
                        // f' / sqrt(1-f^2)
                        let one = self
                            .mir
                            .push(self.mir[expr].clone_as(RealExpression::Literal(1.0)));

                        let squared =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    arg,
                                    self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                                    arg,
                                )));

                        let diff =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    one,
                                    self.mir[expr].clone_as(RealBinaryOperator::Subtract),
                                    squared,
                                )));

                        let root = self.mir.push(self.mir[expr].clone_as(
                            RealExpression::BuiltInFunctionCall1p(
                                BuiltInFunctionCall1p::Sqrt,
                                diff,
                            ),
                        ));

                        RealExpression::BinaryOperator(
                            inner_derivative,
                            self.mir[expr].clone_as(RealBinaryOperator::Divide),
                            root,
                        )
                    }

                    BuiltInFunctionCall1p::ArcCos => {
                        // - f' / sqrt(1-f^2)
                        let one = self
                            .mir
                            .push(self.mir[expr].clone_as(RealExpression::Literal(1.0)));

                        let squared =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    arg,
                                    self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                                    arg,
                                )));

                        let diff =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    one,
                                    self.mir[expr].clone_as(RealBinaryOperator::Subtract),
                                    squared,
                                )));

                        let root = self.mir.push(self.mir[expr].clone_as(
                            RealExpression::BuiltInFunctionCall1p(
                                BuiltInFunctionCall1p::Sqrt,
                                diff,
                            ),
                        ));

                        let top = self
                            .mir
                            .push(self.mir[expr].clone_as(RealExpression::Negate(
                                self.mir[expr].source,
                                inner_derivative,
                            )));

                        RealExpression::BinaryOperator(
                            top,
                            self.mir[expr].clone_as(RealBinaryOperator::Divide),
                            root,
                        )
                    }

                    BuiltInFunctionCall1p::ArcTan => {
                        //  f' / ( 1 + f*f )
                        let one = self
                            .mir
                            .push(self.mir[expr].clone_as(RealExpression::Literal(1.0)));

                        let squared =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    arg,
                                    self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                                    arg,
                                )));

                        let bottom =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    one,
                                    self.mir[expr].clone_as(RealBinaryOperator::Sum),
                                    squared,
                                )));

                        RealExpression::BinaryOperator(
                            inner_derivative,
                            self.mir[expr].clone_as(RealBinaryOperator::Divide),
                            bottom,
                        )
                    }

                    BuiltInFunctionCall1p::ArcSinH => {
                        //  f' / ( 1 + f*f )
                        let one = self
                            .mir
                            .push(self.mir[expr].clone_as(RealExpression::Literal(1.0)));

                        let squared =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    arg,
                                    self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                                    arg,
                                )));

                        let bottom =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    one,
                                    self.mir[expr].clone_as(RealBinaryOperator::Sum),
                                    squared,
                                )));

                        RealExpression::BinaryOperator(
                            inner_derivative,
                            self.mir[expr].clone_as(RealBinaryOperator::Divide),
                            bottom,
                        )
                    }

                    BuiltInFunctionCall1p::ArcCosH => {
                        //  f' / sqrt( f*f - 1 )
                        let one = self
                            .mir
                            .push(self.mir[expr].clone_as(RealExpression::Literal(1.0)));

                        let squared =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    arg,
                                    self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                                    arg,
                                )));

                        let bottom =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    squared,
                                    self.mir[expr].clone_as(RealBinaryOperator::Subtract),
                                    one,
                                )));

                        let root = self.mir.push(self.mir[expr].clone_as(
                            RealExpression::BuiltInFunctionCall1p(
                                BuiltInFunctionCall1p::Sqrt,
                                bottom,
                            ),
                        ));

                        RealExpression::BinaryOperator(
                            inner_derivative,
                            self.mir[expr].clone_as(RealBinaryOperator::Divide),
                            root,
                        )
                    }

                    BuiltInFunctionCall1p::ArcTanH => {
                        //  f' / ( f*f - 1 )
                        let one = self
                            .mir
                            .push(self.mir[expr].clone_as(RealExpression::Literal(1.0)));

                        let squared =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    arg,
                                    self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                                    arg,
                                )));

                        let bottom =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    one,
                                    self.mir[expr].clone_as(RealBinaryOperator::Subtract),
                                    squared,
                                )));

                        RealExpression::BinaryOperator(
                            inner_derivative,
                            self.mir[expr].clone_as(RealBinaryOperator::Divide),
                            bottom,
                        )
                    }

                    BuiltInFunctionCall1p::SinH => {
                        let outer = self.mir.push(self.mir[expr].clone_as(
                            RealExpression::BuiltInFunctionCall1p(BuiltInFunctionCall1p::CosH, arg),
                        ));

                        RealExpression::BinaryOperator(
                            outer,
                            self.mir[arg].clone_as(RealBinaryOperator::Multiply),
                            inner_derivative,
                        )
                    }

                    BuiltInFunctionCall1p::CosH => {
                        let outer = self.mir.push(self.mir[arg].clone_as(
                            RealExpression::BuiltInFunctionCall1p(BuiltInFunctionCall1p::SinH, arg),
                        ));

                        RealExpression::BinaryOperator(
                            outer,
                            self.mir[arg].clone_as(RealBinaryOperator::Multiply),
                            inner_derivative,
                        )
                    }

                    BuiltInFunctionCall1p::TanH => {
                        let one = self
                            .mir
                            .push(self.mir[arg].clone_as(RealExpression::Literal(1.0)));

                        let squared =
                            self.mir
                                .push(self.mir[arg].clone_as(RealExpression::BinaryOperator(
                                    arg,
                                    self.mir[arg].clone_as(RealBinaryOperator::Multiply),
                                    arg,
                                )));

                        let outer =
                            self.mir
                                .push(self.mir[arg].clone_as(RealExpression::BinaryOperator(
                                    one,
                                    self.mir[arg].clone_as(RealBinaryOperator::Subtract),
                                    squared,
                                )));

                        RealExpression::BinaryOperator(
                            inner_derivative,
                            self.mir[arg].clone_as(RealBinaryOperator::Multiply),
                            outer,
                        )
                    }
                }
            }

            RealExpression::BuiltInFunctionCall2p(call, arg1, arg2) => {
                let arg1_derivative = self.partial_derivative(arg1, derive_by);
                let arg2_derivative = self.partial_derivative(arg2, derive_by);

                if arg1_derivative.is_none() && arg2_derivative.is_none() {
                    return None;
                }
                let arg1_derivative = arg1_derivative.unwrap_or_else(|| {
                    self.mir
                        .push(self.mir[expr].clone_as(RealExpression::Literal(0.0)))
                });
                let arg2_derivative = arg2_derivative.unwrap_or_else(|| {
                    self.mir
                        .push(self.mir[expr].clone_as(RealExpression::Literal(0.0)))
                });

                match call {
                    BuiltInFunctionCall2p::Pow => {
                        // (rhs/lhs * lhs' + ln (lhs) * rhs')*expr
                        let ratio =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    arg2,
                                    self.mir[expr].clone_as(RealBinaryOperator::Divide),
                                    arg1,
                                )));

                        let sum1 =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    ratio,
                                    self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                                    arg1_derivative,
                                )));

                        let ln_lhs = self.mir.push(self.mir[expr].clone_as(
                            RealExpression::BuiltInFunctionCall1p(BuiltInFunctionCall1p::Ln, arg1),
                        ));

                        let sum2 =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    ln_lhs,
                                    self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                                    arg2_derivative,
                                )));

                        let inner_derivative =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    sum1,
                                    self.mir[expr].clone_as(RealBinaryOperator::Sum),
                                    sum2,
                                )));

                        RealExpression::BinaryOperator(
                            inner_derivative,
                            self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                            expr,
                        )
                    }

                    BuiltInFunctionCall2p::Min => RealExpression::Condition(
                        self.mir
                            .push(self.mir[expr].clone_as(IntegerExpression::RealComparison(
                                arg1,
                                self.mir[expr].clone_as(ComparisonOperator::LessThen),
                                arg2,
                            ))),
                        self.mir[expr].source,
                        arg1_derivative,
                        self.mir[expr].source,
                        arg2_derivative,
                    ),

                    BuiltInFunctionCall2p::Max => RealExpression::Condition(
                        self.mir
                            .push(self.mir[expr].clone_as(IntegerExpression::RealComparison(
                                arg1,
                                self.mir[expr].clone_as(ComparisonOperator::GreaterThen),
                                arg2,
                            ))),
                        self.mir[expr].source,
                        arg1_derivative,
                        self.mir[expr].source,
                        arg2_derivative,
                    ),

                    BuiltInFunctionCall2p::ArcTan2 => {
                        // (arg1'arg2 - arg2'*arg1)/(arg1*arg1+arg2*arg2)

                        let sum1 =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    arg1_derivative,
                                    self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                                    arg2,
                                )));

                        let sum2 =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    arg2_derivative,
                                    self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                                    arg1,
                                )));

                        let top =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    sum1,
                                    self.mir[expr].clone_as(RealBinaryOperator::Subtract),
                                    sum2,
                                )));

                        let sum1 =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    arg1_derivative,
                                    self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                                    arg2,
                                )));

                        let sum2 =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    arg2_derivative,
                                    self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                                    arg1,
                                )));

                        let bottom =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    sum1,
                                    self.mir[expr].clone_as(RealBinaryOperator::Sum),
                                    sum2,
                                )));

                        RealExpression::BinaryOperator(
                            top,
                            self.mir[expr].clone_as(RealBinaryOperator::Divide),
                            bottom,
                        )
                    }

                    BuiltInFunctionCall2p::Hypot => {
                        // (  arg1 * arg1' +  arg2 * arg2' ) /  expr
                        let sum1 =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    arg1_derivative,
                                    self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                                    arg1,
                                )));

                        let sum2 =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    arg2_derivative,
                                    self.mir[expr].clone_as(RealBinaryOperator::Multiply),
                                    arg2,
                                )));

                        let top =
                            self.mir
                                .push(self.mir[expr].clone_as(RealExpression::BinaryOperator(
                                    sum1,
                                    self.mir[expr].clone_as(RealBinaryOperator::Sum),
                                    sum2,
                                )));

                        RealExpression::BinaryOperator(
                            top,
                            self.mir[expr].clone_as(RealBinaryOperator::Divide),
                            expr,
                        )
                    }
                }
            }

            RealExpression::IntegerConversion(expr) => {
                return self.partial_derivative_of_integer_expression(expr, derive_by)
            }

            RealExpression::ParameterReference(param) if Unknown::Parameter(param) == derive_by => {
                RealExpression::Literal(1.0)
            }

            RealExpression::SystemFunctionCall(_) if Unknown::Temperature == derive_by => {
                RealExpression::Literal(1.0)
            } //TODO other system function calls

            //TODO branch temperature dependence
            RealExpression::BranchAccess(DisciplineAccess::Potential, branch) => {
                if let Unknown::NodePotential(net) = derive_by {
                    match self.mir[branch].contents.branch {
                        Branch::Nets(high, _) if high == net => RealExpression::Literal(1.0),
                        Branch::Nets(_, low) if low == net => RealExpression::Literal(-1.0),
                        _ => return None,
                    }
                } else {
                    return None;
                }
            }

            RealExpression::BranchAccess(DisciplineAccess::Flow, branch)
                if Unknown::Flow(branch) == derive_by =>
            {
                RealExpression::Literal(1.0)
            }

            RealExpression::BranchAccess(DisciplineAccess::Flow, branch) => {
                if let Unknown::PortFlow(net) = derive_by {
                    match self.mir[branch].contents.branch {
                        // the other way around because the node being low potential means current flows in
                        Branch::Nets(high, _) if high == net => RealExpression::Literal(-1.0),
                        Branch::Nets(_, low) if low == net => RealExpression::Literal(1.0),
                        _ => return None,
                    }
                } else {
                    return None;
                }
            }

            RealExpression::Literal(_)
            | RealExpression::ParameterReference(_)
            | RealExpression::SystemFunctionCall(_)
            | RealExpression::BranchAccess(_, _) => return None,
        };
        Some(self.mir.push(self.mir[expr].clone_as(res)))
    }

    pub fn partial_derivative_of_integer_expression(
        &mut self,
        expr: IntegerExpressionId<'tag>,
        derive_by: Unknown<'tag>,
    ) -> Option<RealExpressionId<'tag>> {
        let res = match self.mir[expr].contents {
            IntegerExpression::VariableReference(variable) => {
                let mir = &mut self.mir; //necessary because the borrow checker cant split the borrows inside closures apparently
                let derivative = *self
                    .variable_to_differentiate
                    .entry(variable)
                    .or_insert_with(|| FxHashMap::with_capacity_and_hasher(2, Default::default()))
                    .entry(derive_by)
                    .or_insert_with(|| {
                        mir.declare_partial_derivative_variable(variable, derive_by)
                    });
                RealExpression::VariableReference(derivative)
            }

            IntegerExpression::ParameterReference(param)
                if Unknown::Parameter(param) == derive_by =>
            {
                RealExpression::Literal(1.0)
            }

            IntegerExpression::BinaryOperator(lhs, op, rhs) => {
                let lhs_derived = self.partial_derivative_of_integer_expression(lhs, derive_by);
                let rhs_derived = self.partial_derivative_of_integer_expression(rhs, derive_by);
                match (lhs_derived, rhs_derived) {
                    (None, None) => return None,
                    (Some(lhs_derived), Some(rhs_derived)) => {
                        let lhs = self
                            .mir
                            .push(self.mir[lhs].clone_as(RealExpression::IntegerConversion(lhs)));
                        let rhs = self
                            .mir
                            .push(self.mir[lhs].clone_as(RealExpression::IntegerConversion(rhs)));

                        match op.contents {
                            IntegerBinaryOperator::Sum => RealExpression::BinaryOperator(
                                lhs_derived,
                                op.copy_as(RealBinaryOperator::Sum),
                                rhs_derived,
                            ),

                            IntegerBinaryOperator::Subtract => RealExpression::BinaryOperator(
                                lhs_derived,
                                op.copy_as(RealBinaryOperator::Subtract),
                                rhs_derived,
                            ),

                            IntegerBinaryOperator::Multiply => {
                                let sum1 = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        lhs_derived,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        rhs,
                                    ),
                                ));
                                let sum2 = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        lhs,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        rhs_derived,
                                    ),
                                ));
                                RealExpression::BinaryOperator(
                                    sum1,
                                    op.copy_as(RealBinaryOperator::Sum),
                                    sum2,
                                ) //lhs'*rhs + lhs*rhs'
                            }

                            IntegerBinaryOperator::Divide => {
                                // (lhs'*rhs-lhs * rhs') / (rhs*rhs)
                                let sum1 = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        lhs_derived,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        rhs,
                                    ),
                                ));
                                let sum2 = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        lhs,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        rhs_derived,
                                    ),
                                ));
                                let top = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        sum1,
                                        op.copy_as(RealBinaryOperator::Subtract),
                                        sum2,
                                    ),
                                ));
                                let bottom = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        rhs,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        rhs,
                                    ),
                                ));
                                RealExpression::BinaryOperator(
                                    top,
                                    op.copy_as(RealBinaryOperator::Divide),
                                    bottom,
                                )
                            }

                            IntegerBinaryOperator::Exponent => {
                                // (rhs/lhs * lhs' + ln (lhs) * rhs')*expr
                                let ratio = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        rhs,
                                        op.copy_as(RealBinaryOperator::Divide),
                                        lhs,
                                    ),
                                ));
                                let sum1 = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        ratio,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        lhs_derived,
                                    ),
                                ));

                                let ln_lhs = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BuiltInFunctionCall1p(
                                        BuiltInFunctionCall1p::Ln,
                                        lhs,
                                    ),
                                ));
                                let sum2 = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        ln_lhs,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        rhs_derived,
                                    ),
                                ));

                                let inner_derivative = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        sum1,
                                        op.copy_as(RealBinaryOperator::Sum),
                                        sum2,
                                    ),
                                ));

                                RealExpression::BinaryOperator(
                                    inner_derivative,
                                    op.copy_as(RealBinaryOperator::Multiply),
                                    self.mir.push(
                                        self.mir[expr]
                                            .clone_as(RealExpression::IntegerConversion(expr)),
                                    ),
                                )
                            }

                            IntegerBinaryOperator::Modulus
                            | IntegerBinaryOperator::Xor
                            | IntegerBinaryOperator::NXor
                            | IntegerBinaryOperator::And
                            | IntegerBinaryOperator::Or
                            | IntegerBinaryOperator::LogicOr
                            | IntegerBinaryOperator::LogicAnd => todo!("Error"),

                            IntegerBinaryOperator::ShiftLeft => {
                                // ( lhs'+ln(2)*lhs*rhs' )* 2 ** rhs
                                let ln2 = self.mir.push(
                                    self.mir[expr].clone_as(RealExpression::Literal(2f64.ln())),
                                );
                                let ln2_lhs = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        ln2,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        lhs,
                                    ),
                                ));
                                let product = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        ln2_lhs,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        rhs_derived,
                                    ),
                                ));

                                let sum = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        product,
                                        op.copy_as(RealBinaryOperator::Sum),
                                        lhs_derived,
                                    ),
                                ));

                                let literal2 = self
                                    .mir
                                    .push(self.mir[expr].clone_as(RealExpression::Literal(2f64)));
                                let pow = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BuiltInFunctionCall2p(
                                        BuiltInFunctionCall2p::Pow,
                                        literal2,
                                        rhs,
                                    ),
                                ));

                                RealExpression::BinaryOperator(
                                    sum,
                                    op.copy_as(RealBinaryOperator::Multiply),
                                    pow,
                                )
                            }
                            IntegerBinaryOperator::ShiftRight => {
                                // ( lhs' - ln(2)*lhs*rhs' ) * 2 ** (- rhs)
                                let ln2 = self.mir.push(
                                    self.mir[expr].clone_as(RealExpression::Literal(2f64.ln())),
                                );
                                let ln2_lhs = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        ln2,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        lhs,
                                    ),
                                ));
                                let product = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        ln2_lhs,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        rhs_derived,
                                    ),
                                ));

                                let sum = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        lhs_derived,
                                        op.copy_as(RealBinaryOperator::Subtract),
                                        product,
                                    ),
                                ));

                                let literal2 = self
                                    .mir
                                    .push(self.mir[expr].clone_as(RealExpression::Literal(2f64)));
                                let neg_rhs = self.mir.push(
                                    self.mir[expr].clone_as(RealExpression::Negate(op.source, rhs)),
                                );
                                let pow = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BuiltInFunctionCall2p(
                                        BuiltInFunctionCall2p::Pow,
                                        literal2,
                                        neg_rhs,
                                    ),
                                ));

                                RealExpression::BinaryOperator(
                                    sum,
                                    op.copy_as(RealBinaryOperator::Multiply),
                                    pow,
                                )
                            }
                        }
                    }

                    (None, Some(rhs_derived)) => {
                        let lhs = self
                            .mir
                            .push(self.mir[lhs].clone_as(RealExpression::IntegerConversion(lhs)));
                        let rhs = self
                            .mir
                            .push(self.mir[lhs].clone_as(RealExpression::IntegerConversion(rhs)));
                        let expr_as_real = self
                            .mir
                            .push(self.mir[lhs].clone_as(RealExpression::IntegerConversion(expr)));

                        match op.contents {
                            IntegerBinaryOperator::Sum => return Some(rhs_derived),
                            IntegerBinaryOperator::Subtract => {
                                RealExpression::Negate(op.source, rhs_derived)
                            }
                            IntegerBinaryOperator::Multiply => RealExpression::BinaryOperator(
                                lhs,
                                op.copy_as(RealBinaryOperator::Multiply),
                                rhs_derived,
                            ),
                            IntegerBinaryOperator::Exponent => {
                                // (ln (lhs) * rhs') * expr
                                let ln_lhs = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BuiltInFunctionCall1p(
                                        BuiltInFunctionCall1p::Ln,
                                        lhs,
                                    ),
                                ));
                                let inner_derivative = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        ln_lhs,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        rhs_derived,
                                    ),
                                ));

                                RealExpression::BinaryOperator(
                                    inner_derivative,
                                    op.copy_as(RealBinaryOperator::Multiply),
                                    expr_as_real,
                                )
                            }
                            IntegerBinaryOperator::Divide => {
                                // lhs * rhs' / (rhs*rhs)

                                let top = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        lhs,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        rhs_derived,
                                    ),
                                ));

                                let bottom = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        rhs,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        rhs,
                                    ),
                                ));

                                RealExpression::BinaryOperator(
                                    top,
                                    op.copy_as(RealBinaryOperator::Divide),
                                    bottom,
                                )
                            }
                            IntegerBinaryOperator::Modulus
                            | IntegerBinaryOperator::Xor
                            | IntegerBinaryOperator::NXor
                            | IntegerBinaryOperator::And
                            | IntegerBinaryOperator::Or
                            | IntegerBinaryOperator::LogicOr
                            | IntegerBinaryOperator::LogicAnd => todo!("Error"),

                            IntegerBinaryOperator::ShiftLeft => {
                                // ln(2)*rhs' * expr
                                let ln2 = self.mir.push(
                                    self.mir[expr].clone_as(RealExpression::Literal(2f64.ln())),
                                );
                                let ln2_drhs = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        ln2,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        rhs_derived,
                                    ),
                                ));

                                RealExpression::BinaryOperator(
                                    ln2_drhs,
                                    op.copy_as(RealBinaryOperator::Multiply),
                                    expr_as_real,
                                )
                            }
                            IntegerBinaryOperator::ShiftRight => {
                                // ( - ln(2)*rhs' ) * expr
                                let neg_ln2 = self.mir.push(
                                    self.mir[expr].clone_as(RealExpression::Literal(-(2f64.ln()))),
                                );
                                let ln2_drhs = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        neg_ln2,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        rhs_derived,
                                    ),
                                ));

                                RealExpression::BinaryOperator(
                                    ln2_drhs,
                                    op.copy_as(RealBinaryOperator::Multiply),
                                    expr_as_real,
                                )
                            }
                        }
                    }
                    (Some(lhs_derived), None) => {
                        let lhs = self
                            .mir
                            .push(self.mir[lhs].clone_as(RealExpression::IntegerConversion(lhs)));
                        let rhs = self
                            .mir
                            .push(self.mir[lhs].clone_as(RealExpression::IntegerConversion(rhs)));
                        let expr_as_real = self
                            .mir
                            .push(self.mir[lhs].clone_as(RealExpression::IntegerConversion(expr)));

                        match op.contents {
                            IntegerBinaryOperator::Sum | IntegerBinaryOperator::Subtract => {
                                return Some(lhs_derived)
                            }

                            IntegerBinaryOperator::Multiply => RealExpression::BinaryOperator(
                                lhs_derived,
                                op.copy_as(RealBinaryOperator::Multiply),
                                rhs,
                            ),
                            IntegerBinaryOperator::Divide => RealExpression::BinaryOperator(
                                lhs_derived,
                                op.copy_as(RealBinaryOperator::Divide),
                                rhs,
                            ), // rhs is just a scalar (x/c->x'/c)

                            IntegerBinaryOperator::Exponent => {
                                // (rhs/lhs*lhs') * expr
                                let ratio = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        rhs,
                                        op.copy_as(RealBinaryOperator::Divide),
                                        lhs,
                                    ),
                                ));
                                let inner_derivative = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BinaryOperator(
                                        ratio,
                                        op.copy_as(RealBinaryOperator::Multiply),
                                        lhs_derived,
                                    ),
                                ));
                                RealExpression::BinaryOperator(
                                    inner_derivative,
                                    op.copy_as(RealBinaryOperator::Multiply),
                                    expr_as_real,
                                )
                            }

                            IntegerBinaryOperator::Modulus
                            | IntegerBinaryOperator::Xor
                            | IntegerBinaryOperator::NXor
                            | IntegerBinaryOperator::And
                            | IntegerBinaryOperator::Or
                            | IntegerBinaryOperator::LogicOr
                            | IntegerBinaryOperator::LogicAnd => todo!("Error"),

                            IntegerBinaryOperator::ShiftLeft => {
                                //  lhs'* 2 ** rhs

                                let literal2 = self
                                    .mir
                                    .push(self.mir[expr].clone_as(RealExpression::Literal(2f64)));
                                let pow = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BuiltInFunctionCall2p(
                                        BuiltInFunctionCall2p::Pow,
                                        literal2,
                                        rhs,
                                    ),
                                ));

                                RealExpression::BinaryOperator(
                                    lhs_derived,
                                    op.copy_as(RealBinaryOperator::Multiply),
                                    pow,
                                )
                            }
                            IntegerBinaryOperator::ShiftRight => {
                                // ( lhs'  * 2 ** (- rhs)
                                let literal2 = self
                                    .mir
                                    .push(self.mir[expr].clone_as(RealExpression::Literal(2f64)));
                                let neg_rhs = self.mir.push(
                                    self.mir[expr].clone_as(RealExpression::Negate(op.source, rhs)),
                                );
                                let pow = self.mir.push(self.mir[expr].clone_as(
                                    RealExpression::BuiltInFunctionCall2p(
                                        BuiltInFunctionCall2p::Pow,
                                        literal2,
                                        neg_rhs,
                                    ),
                                ));

                                RealExpression::BinaryOperator(
                                    lhs_derived,
                                    op.copy_as(RealBinaryOperator::Multiply),
                                    pow,
                                )
                            }
                        }
                    }
                }
            }

            IntegerExpression::UnaryOperator(
                Node {
                    source,
                    contents: UnaryOperator::ArithmeticNegate,
                },
                expr,
            ) => RealExpression::Negate(
                source,
                self.partial_derivative_of_integer_expression(expr, derive_by)?,
            ),

            IntegerExpression::Condition(cond, question_span, true_val, colon_span, else_val) => {
                let true_val = self.partial_derivative_of_integer_expression(true_val, derive_by);
                let else_val = self.partial_derivative_of_integer_expression(else_val, derive_by);
                if true_val.is_none() && else_val.is_none() {
                    return None;
                }
                let else_val = else_val.unwrap_or(
                    self.mir
                        .push(self.mir[expr].clone_as(RealExpression::Literal(0.0))),
                );

                let true_val = true_val.unwrap_or(
                    self.mir
                        .push(self.mir[expr].clone_as(RealExpression::Literal(0.0))),
                );

                RealExpression::Condition(cond, question_span, true_val, colon_span, else_val)
            }

            IntegerExpression::Min(arg1, arg2) => {
                let arg1_derivative =
                    self.partial_derivative_of_integer_expression(arg1, derive_by);
                let arg2_derivative =
                    self.partial_derivative_of_integer_expression(arg2, derive_by);

                if arg1_derivative.is_none() && arg2_derivative.is_none() {
                    return None;
                }
                let arg1_derivative = arg1_derivative.unwrap_or_else(|| {
                    self.mir
                        .push(self.mir[expr].clone_as(RealExpression::Literal(0.0)))
                });
                let arg2_derivative = arg2_derivative.unwrap_or_else(|| {
                    self.mir
                        .push(self.mir[expr].clone_as(RealExpression::Literal(0.0)))
                });

                RealExpression::Condition(
                    self.mir.push(
                        self.mir[expr].clone_as(IntegerExpression::IntegerComparison(
                            arg1,
                            self.mir[expr].clone_as(ComparisonOperator::LessThen),
                            arg2,
                        )),
                    ),
                    self.mir[expr].source,
                    arg1_derivative,
                    self.mir[expr].source,
                    arg2_derivative,
                )
            }

            IntegerExpression::Max(arg1, arg2) => {
                let arg1_derivative =
                    self.partial_derivative_of_integer_expression(arg1, derive_by);
                let arg2_derivative =
                    self.partial_derivative_of_integer_expression(arg2, derive_by);

                if arg1_derivative.is_none() && arg2_derivative.is_none() {
                    return None;
                }
                let arg1_derivative = arg1_derivative.unwrap_or_else(|| {
                    self.mir
                        .push(self.mir[expr].clone_as(RealExpression::Literal(0.0)))
                });
                let arg2_derivative = arg2_derivative.unwrap_or_else(|| {
                    self.mir
                        .push(self.mir[expr].clone_as(RealExpression::Literal(0.0)))
                });

                RealExpression::Condition(
                    self.mir.push(
                        self.mir[expr].clone_as(IntegerExpression::IntegerComparison(
                            arg1,
                            self.mir[expr].clone_as(ComparisonOperator::GreaterThen),
                            arg2,
                        )),
                    ),
                    self.mir[expr].source,
                    arg1_derivative,
                    self.mir[expr].source,
                    arg2_derivative,
                )
            }

            IntegerExpression::Abs(arg) => {
                let arg_derived = self.partial_derivative_of_integer_expression(arg, derive_by)?;
                let literal_0 = self
                    .mir
                    .push(self.mir[expr].clone_as(IntegerExpression::Literal(0)));

                RealExpression::Condition(
                    self.mir.push(
                        self.mir[expr].clone_as(IntegerExpression::IntegerComparison(
                            arg,
                            self.mir[expr].clone_as(ComparisonOperator::GreaterThen),
                            literal_0,
                        )),
                    ),
                    self.mir[expr].source,
                    arg_derived,
                    self.mir[expr].source,
                    self.mir.push(
                        self.mir[expr]
                            .clone_as(RealExpression::Negate(self.mir[expr].source, arg_derived)),
                    ),
                )
            }

            IntegerExpression::Literal(_) | IntegerExpression::ParameterReference(_) => {
                return None
            }

            // rounding undefined at 0.5 + n for any integer n rest is zero TODO warn (or error?)
            IntegerExpression::RealCast(expr) => return None,

            IntegerExpression::FunctionCall(_, _) => todo!("Function calls"),
            IntegerExpression::NetReference(_) | IntegerExpression::PortReference(_) => {
                todo!("Digital")
            }

            IntegerExpression::IntegerComparison(_, _, _)
            | IntegerExpression::RealComparison(_, _, _)
            | IntegerExpression::StringEq(_, _)
            | IntegerExpression::UnaryOperator(_, _)
            | IntegerExpression::StringNEq(_, _) => todo!("error"),
        };
        Some(self.mir.push(self.mir[expr].clone_as(res)))
    }

    pub fn or_for_derivative(
        &mut self,
        expr: IntegerExpressionId<'tag>,
        delta_name: Symbol,
        derivatives_inside_loop: &FxHashSet<VariableId<'tag>>,
    ) -> IntegerExpressionId<'tag> {
        for derived_variable in derivatives_inside_loop.iter().copied() {
            if self.mir[derived_variable].contents.name.name == delta_name {
                if let Some(partial_derivatives) =
                    self.variable_to_differentiate.get(&derived_variable)
                {
                    let mut res = expr;

                    for (derive_by, _) in partial_derivatives {
                        let mut replacements = FxHashMap::with_capacity_and_hasher(
                            derivatives_inside_loop.len(),
                            Default::default(),
                        );

                        for (variable, partial_derivatives) in &self.variable_to_differentiate {
                            if derivatives_inside_loop.contains(variable) {
                                if let Some(&derivative) = partial_derivatives.get(derive_by) {
                                    replacements.insert(*variable, derivative);
                                }
                            }
                        }

                        if let Some(derived_condition) = self.mir.map_int_expr(expr, &replacements)
                        {
                            let span = self.mir[expr].source;
                            res = self.mir.push(Node::new(
                                IntegerExpression::BinaryOperator(
                                    res,
                                    Node::new(IntegerBinaryOperator::LogicOr, span),
                                    derived_condition,
                                ),
                                span,
                            ))
                        }
                    }

                    return res;
                }
            }
        }
        //TODO warning
        expr
    }
}
impl<'tag> Mir<'tag> {
    pub fn declare_partial_derivative_variable(
        &mut self,
        variable: VariableId<'tag>,
        derive_by: Unknown<'tag>,
    ) -> VariableId<'tag> {
        let derive_by = match derive_by {
            Unknown::Parameter(parameter) => self[parameter].contents.name.to_string(),
            Unknown::NodePotential(net) => format!("pot({})", self[net].contents.name),
            Unknown::Flow(branch) => format!("flow({})", self[branch].contents.name),
            Unknown::PortFlow(net) => format!("flow(<{}>)", self[net].contents.name),
            Unknown::Temperature => "T".to_string(),
        };
        let name =
            Ident::from_str(format!("∂{}/∂{}", self[variable].contents.name, derive_by).as_str());
        let res = self.push(AttributeNode {
            attributes: self.empty_range_from_end(),
            source: self[variable].source,
            contents: Variable {
                name,
                variable_type: VariableType::Real(None),
            },
        });
        debug_assert!(&self[self[res].attributes].is_empty());
        res
    }

    pub(super) fn generate_derivative_alias(
        &mut self,
        dst: VariableId<'tag>,
        src: VariableId<'tag>,
    ) -> StatementId<'tag> {
        let value = self.push(Node::new(
            RealExpression::VariableReference(src),
            Span::new_short_empty_span(0),
        ));

        self.push(Statement::Assignment(
            self.empty_range_from_end(),
            dst,
            ExpressionId::Real(value), //derivatives are always real
        ))
    }
}
