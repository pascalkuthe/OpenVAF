/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::{BranchAccess, HierarchicalId};
use crate::ast_lowering::ast_to_hir_fold::{Fold, VerilogContext};
use crate::ast_lowering::branch_resolution::BranchResolver;
use crate::ast_lowering::error::Type::{
    DerivativeNotAllowed, EmptyBranchAccess, UnexpectedTokenInBranchAccess,
};
use crate::ast_lowering::error::*;
use crate::hir::{Expression, Primary};
use crate::hir_lowering::derivatives::Unknown;
use crate::ir::ast::Branch;
use crate::ir::hir::DisciplineAccess;
use crate::ir::NoiseSource;
use crate::ir::{BranchId, DisciplineId, ExpressionId, Node, SystemFunctionCall};
use crate::{ast, hir, Span};

pub trait ExpressionFolder<'lt> {
    fn fold(&mut self, expression_id: ExpressionId, base: &mut Fold<'lt>) -> Option<ExpressionId>;
}
/// Folds real constants. These may not even contain references to parameters
pub struct ConstantExpressionFolder();
impl<'lt> ExpressionFolder<'lt> for ConstantExpressionFolder {
    #[inline]
    fn fold(&mut self, expression_id: ExpressionId, base: &mut Fold<'lt>) -> Option<ExpressionId> {
        base.fold_expression(expression_id, self)
    }
}

impl<'lt> Fold<'lt> {
    #[inline]
    fn fold_expression(
        &mut self,
        expression_id: ExpressionId,
        expr_folder: &mut impl ExpressionFolder<'lt>,
    ) -> Option<ExpressionId> {
        let expression = &self.ast[expression_id];
        let res = match expression.contents {
            ast::Expression::BinaryOperator(lhs, op, rhs) => {
                let lhs = expr_folder.fold(lhs, &mut *self);
                let rhs = expr_folder.fold(rhs, &mut *self);
                self.hir.expressions.push(Node {
                    source: expression.source,
                    contents: Expression::BinaryOperator(lhs?, op, rhs?),
                })
            }

            ast::Expression::UnaryOperator(unary_op, expr) => {
                let expr = expr_folder.fold(expr, &mut *self)?;
                self.hir.expressions.push(Node {
                    source: expression.source,
                    contents: Expression::UnaryOperator(unary_op, expr),
                })
            }

            ast::Expression::Primary(ast::Primary::Integer(val)) => {
                self.hir.expressions.push(Node {
                    source: expression.source,
                    contents: Expression::Primary(Primary::Integer(val)),
                })
            }

            ast::Expression::Primary(ast::Primary::UnsignedInteger(val)) => {
                self.hir.expressions.push(Node {
                    source: expression.source,
                    contents: Expression::Primary(Primary::UnsignedInteger(val)),
                })
            }

            ast::Expression::Primary(ast::Primary::Real(val)) => self.hir.expressions.push(Node {
                source: expression.source,
                contents: Expression::Primary(Primary::Real(val)),
            }),

            ast::Expression::Primary(ast::Primary::String(val)) => {
                self.hir.expressions.push(Node {
                    source: expression.source,
                    contents: Expression::Primary(Primary::String(val)),
                })
            }

            ast::Expression::Primary(ast::Primary::BuiltInFunctionCall2p(call, arg1, arg2)) => {
                let arg1 = expr_folder.fold(arg1, &mut *self);
                let arg2 = expr_folder.fold(arg2, &mut *self);
                self.hir.expressions.push(Node {
                    contents: Expression::Primary(Primary::BuiltInFunctionCall2p(
                        call, arg1?, arg2?,
                    )),
                    source: expression.source,
                })
            }

            ast::Expression::Primary(ast::Primary::BuiltInFunctionCall1p(call, arg)) => {
                let arg = expr_folder.fold(arg, &mut *self)?;
                self.hir.expressions.push(Node {
                    contents: Expression::Primary(Primary::BuiltInFunctionCall1p(call, arg)),
                    source: expression.source,
                })
            }

            ast::Expression::Condtion(condition, question_span, if_val, colon_span, else_val) => {
                let condition = expr_folder.fold(condition, &mut *self);
                let if_val = expr_folder.fold(if_val, &mut *self);
                let else_val = expr_folder.fold(else_val, &mut *self);
                self.hir.expressions.push(Node {
                    source: expression.source,
                    contents: Expression::Condtion(
                        condition?,
                        question_span,
                        if_val?,
                        colon_span,
                        else_val?,
                    ),
                })
            }

            ast::Expression::Primary(ast::Primary::VariableOrNetReference(_)) => {
                self.error(Error {
                    source: expression.source,
                    error_type: Type::NotAllowedInConstantContext(
                        NonConstantExpression::VariableReference,
                    ),
                });
                return None;
            }

            // These are "real" constant. Standard allows this but we just cant do this here
            ast::Expression::Primary(ast::Primary::FunctionCall(_, _)) => {
                self.error(Error {
                    source: expression.source,
                    error_type: Type::NotAllowedInConstantContext(
                        NonConstantExpression::FunctionCall,
                    ),
                });
                return None;
            }

            // These are "real" constant. Standard allows this but we just cant do this here
            ast::Expression::Primary(ast::Primary::SystemFunctionCall(_)) => {
                self.error(Error {
                    source: expression.source,
                    error_type: Type::NotAllowedInConstantContext(
                        NonConstantExpression::FunctionCall,
                    ),
                });
                return None;
            }

            ast::Expression::Primary(ast::Primary::BranchAccess(_, _))
            | ast::Expression::Primary(ast::Primary::DerivativeByBranch(_, _, _))
            | ast::Expression::Primary(ast::Primary::DerivativeByTime(_))
            | ast::Expression::Primary(ast::Primary::DerivativeByTemperature(_))
            | ast::Expression::Primary(ast::Primary::Noise(_, _)) => {
                self.error(Error {
                    source: expression.source,
                    error_type: Type::NotAllowedInConstantContext(
                        NonConstantExpression::BranchAccess,
                    ),
                });
                return None;
            }
        };
        Some(res)
    }
}

pub struct StatementExpressionFolder<'lt> {
    pub(super) state: VerilogContext,
    pub(super) branch_resolver: &'lt mut BranchResolver,
}

impl<'lt> StatementExpressionFolder<'lt> {
    /// Due to an ambiguous grammar the parser cannot determine whether V(x,y) is a functioncall or a Branch access when it appears inside an expression.
    /// When the function call is resolves to a nature this function is called to reinterpret and resolve the function parameters as a branch access
    fn reinterpret_function_parameters_as_branch_access<'fold>(
        &mut self,
        nature_indent_span: Span,
        parameters: &[ExpressionId],
        base: &mut Fold<'fold>,
    ) -> Option<(BranchId, DisciplineId)> {
        match parameters {
            [branch] => {
                let expr_node = &base.ast[*branch];
                let branch_access = ast::BranchAccess::BranchOrNodePotential(
                    self.reinterpret_expression_as_identifier(expr_node, base)?
                        .clone(),
                );
                self.branch_resolver
                    .resolve_branch_access(base, &Node::new(branch_access, expr_node.source))
            }
            [net1, net2] => {
                let expr_node = &base.ast[*net1];
                let span = expr_node.source;
                let net1 = self.reinterpret_expression_as_identifier(expr_node, base);
                let expr_node = &base.ast[*net2];
                let span = span.extend(expr_node.source);
                let net2 = self.reinterpret_expression_as_identifier(expr_node, base);
                let branch_access =
                    ast::BranchAccess::Implicit(ast::Branch::Nets(net1?.clone(), net2?.clone()));
                self.branch_resolver
                    .resolve_branch_access(&mut *base, &Node::new(branch_access, span))
            }
            [] => {
                base.error(Error {
                    source: nature_indent_span,
                    error_type: EmptyBranchAccess,
                });
                None
            }
            [_, _, unexpected, ..] => {
                base.error(Error {
                    source: base.ast[*unexpected]
                        .source
                        .extend(base.ast[*parameters.last().unwrap()].source),
                    error_type: UnexpectedTokenInBranchAccess,
                });
                None
            }
        }
    }

    fn reinterpret_expression_as_identifier<'fold>(
        &mut self,
        expression: &'fold Node<ast::Expression>,
        base: &mut Fold<'fold>,
    ) -> Option<&'fold HierarchicalId> {
        if let ast::Expression::Primary(ast::Primary::VariableOrNetReference(ref name)) =
            expression.contents
        {
            Some(name)
        } else {
            base.error(Error {
                source: expression.source,
                error_type: UnexpectedTokenInBranchAccess,
            });
            None
        }
    }
}

impl<'lt, 'fold> ExpressionFolder<'fold> for StatementExpressionFolder<'lt> {
    fn fold(
        &mut self,
        expression_id: ExpressionId,
        base: &mut Fold<'fold>,
    ) -> Option<ExpressionId> {
        let expression = &base.ast[expression_id];
        let res = match expression.contents {
            ast::Expression::Primary(ast::Primary::BranchAccess(ref nature, ref branch_access)) => {
                if self.state.contains(VerilogContext::FUNCTION) {
                    base.error(Error {
                        source: expression.source,
                        error_type: Type::NotAllowedInFunction(NotAllowedInFunction::BranchAccess),
                    });
                    return None;
                }
                let (branch_access, discipline) = self
                    .branch_resolver
                    .resolve_branch_access(&mut *base, branch_access)?;
                let nature = self
                    .branch_resolver
                    .resolve_discipline_access(&mut *base, nature, discipline)?;
                base.hir.expressions.push(Node {
                    source: expression.source,
                    contents: Expression::Primary(Primary::BranchAccess(nature, branch_access)),
                })
            }
            ast::Expression::Primary(ast::Primary::DerivativeByTime(expr)) => {
                if self.state.contains(VerilogContext::FUNCTION) {
                    base.error(Error {
                        source: expression.source,
                        error_type: Type::NotAllowedInFunction(NotAllowedInFunction::AnalogFilters),
                    });
                    return None;
                }
                let expr = self.fold(expr, base)?;
                base.hir.expressions.push(Node {
                    source: expression.source,
                    contents: Expression::Primary(Primary::Derivative(expr, Unknown::Time)),
                })
            }

            ast::Expression::Primary(ast::Primary::DerivativeByTemperature(expr)) => {
                if self.state.contains(VerilogContext::FUNCTION) {
                    base.error(Error {
                        source: expression.source,
                        error_type: Type::NotAllowedInFunction(NotAllowedInFunction::AnalogFilters),
                    });
                    return None;
                }
                let expr = self.fold(expr, base)?;
                base.hir.expressions.push(Node {
                    source: expression.source,
                    contents: Expression::Primary(Primary::Derivative(expr, Unknown::Temperature)),
                })
            }

            ast::Expression::Primary(ast::Primary::Noise(source, src)) => {
                if self.state.contains(VerilogContext::FUNCTION) {
                    base.error(Error {
                        source: expression.source,
                        error_type: Type::NotAllowedInFunction(NotAllowedInFunction::AnalogFilters),
                    });
                    return None;
                }
                let source = match source {
                    NoiseSource::White(expr) => NoiseSource::White(self.fold(expr, base)?),
                    NoiseSource::Flicker(expr1, expr2) => {
                        NoiseSource::Flicker(self.fold(expr1, base)?, self.fold(expr2, base)?)
                    }
                    NoiseSource::Table(_) | NoiseSource::TableLog(_) => todo!(),
                };
                base.hir.expressions.push(Node {
                    source: expression.source,
                    contents: Expression::Primary(Primary::Noise(source, src)),
                })
            }

            ast::Expression::Primary(ast::Primary::DerivativeByBranch(
                expr_to_derive,
                ref nature,
                ref branch_access,
            )) => {
                if self.state.contains(VerilogContext::FUNCTION) {
                    base.error(Error {
                        source: expression.source,
                        error_type: Type::NotAllowedInFunction(NotAllowedInFunction::AnalogFilters),
                    });
                    return None;
                }
                let expr_to_derive = self.fold(expr_to_derive, base);
                let derive_by = match (&branch_access.contents, nature) {
                    (BranchAccess::BranchOrNodePotential(name), nature) => {
                        let mut res = None;
                        resolve_hierarchical!(base; name as
                            Branch(id) => {
                                let discipline = match base.hir[id].contents.branch {
                                    hir::Branch::Port(portid) => {
                                        base.hir[base.hir[portid].net].contents.discipline
                                    }
                                    hir::Branch::Nets(net1, _) => base.hir[net1].contents.discipline
                                };
                                let discipline_access = self.branch_resolver
                                        .resolve_discipline_access(base, nature, discipline)?;
                                if discipline_access == DisciplineAccess::Flow {
                                    res = Some(Unknown::Flow(id))
                                }else {
                                    base.errors.push(Error {
                                        error_type: DerivativeNotAllowed,
                                        source: expression.source,
                                    });
                                    return None;
                                }
                            },

                            // Needed to resolve ambiguities
                            Port(id) => {
                                let discipline = base.hir[base.hir[id].net].contents.discipline;
                                let discipline_access = self.branch_resolver
                                        .resolve_discipline_access(base, nature, discipline)?;
                                if discipline_access == DisciplineAccess::Potential {
                                    res = Some(Unknown::NodePotential(base.hir[id].net))
                                }else {
                                    base.errors.push(Error {
                                        error_type: DerivativeNotAllowed,
                                        source: expression.source,
                                    });
                                    return None;
                                }
                            },

                            Net(id) => {
                                let discipline = base.hir[id].contents.discipline;
                                let discipline_access = self.branch_resolver
                                        .resolve_discipline_access(base, nature, discipline)?;
                                if discipline_access == DisciplineAccess::Potential {
                                    res = Some(Unknown::NodePotential(id))
                                }else {
                                    base.errors.push(Error {
                                        error_type: DerivativeNotAllowed,
                                        source: expression.source,
                                    });
                                    return None;
                                }
                            }
                        );

                        res?
                    }
                    (BranchAccess::Implicit(branch), nature) => {
                        let (resolved_branch, discipline) =
                            self.branch_resolver.resolve_branch(base, branch)?;

                        let disciplines_access = self
                            .branch_resolver
                            .resolve_discipline_access(base, nature, discipline)?;

                        match resolved_branch {
                            hir::Branch::Port(port)
                                if disciplines_access == DisciplineAccess::Flow =>
                            {
                                Unknown::PortFlow(base.hir[port].net)
                            }

                            hir::Branch::Nets(_, _)
                                if disciplines_access == DisciplineAccess::Flow =>
                            {
                                let (branch, _) = self
                                    .branch_resolver
                                    .resolve_branch_access(base, branch_access)?;
                                Unknown::Flow(branch)
                            }

                            hir::Branch::Nets(node, _)
                                if matches!(branch, Branch::NetToGround(_)) =>
                            {
                                Unknown::NodePotential(node)
                            }

                            _ => {
                                base.errors.push(Error {
                                    error_type: DerivativeNotAllowed,
                                    source: expression.source,
                                });
                                return None;
                            }
                        }
                    }
                };

                base.hir.expressions.push(Node {
                    source: expression.source,
                    contents: Expression::Primary(Primary::Derivative(expr_to_derive?, derive_by)),
                })
            }

            ast::Expression::Primary(ast::Primary::VariableOrNetReference(ref ident)) => {
                resolve_hierarchical! {base; ident as
                    Variable(vid) => {
                        if self.state.contains(VerilogContext::CONSTANT){
                            base.error(Error{
                                source:expression.source,
                                error_type:Type::NotAllowedInConstantContext(NonConstantExpression::VariableReference)
                                });
                        }else {
                            return Some(base.hir.expressions.push(Node{
                                source:expression.source,
                                contents:Expression::Primary(Primary::VariableReference(vid))
                            }))
                        }
                    },
                    Parameter(pid) => {
                        return Some(base.hir.expressions.push(Node{
                            source:expression.source,
                            contents:Expression::Primary(Primary::ParameterReference(pid))
                        }))
                    }
                }
                return None;
            }

            ast::Expression::Primary(ast::Primary::FunctionCall(ref ident, ref parameters)) => {
                resolve_hierarchical! { base; ident as
                    Function(fid) => {
                        let parameters = parameters
                            .iter()
                            .copied()
                            .filter_map(|expr| self.fold(expr,&mut *base))
                            .collect();
                        return Some(base.hir.expressions.push(Node{
                            source:expression.source,
                            contents:Expression::Primary(Primary::FunctionCall(fid,parameters))
                        }))

                    },
                    Nature(nature) => {
                        let (branch_access,discipline) = self.reinterpret_function_parameters_as_branch_access(ident.span(),parameters,&mut *base)?;
                        let discipline_access = BranchResolver::resolve_nature_access(&mut *base,nature,discipline)?;
                        return Some(base.hir.expressions.push(Node{
                            source:expression.source,
                            contents:Expression::Primary(Primary::BranchAccess(discipline_access,branch_access))
                        }))

                    }
                }
                return None;
            }

            ast::Expression::Primary(ast::Primary::SystemFunctionCall(ref call)) => {
                let new_call = match call {
                    SystemFunctionCall::Temperature => SystemFunctionCall::Temperature,
                    SystemFunctionCall::Vt(temp) => {
                        SystemFunctionCall::Vt(temp.map(|temp| self.fold(temp, base)).flatten())
                    }
                    SystemFunctionCall::Simparam(name, default) => {
                        let default = default.map(|default| self.fold(default, base)).flatten();
                        SystemFunctionCall::Simparam(self.fold(*name, base)?, default)
                    }
                    SystemFunctionCall::SimparamStr(name) => {
                        SystemFunctionCall::SimparamStr(self.fold(*name, base)?)
                    }
                    SystemFunctionCall::PortConnected(ref port) => {
                        let mut res = None;
                        resolve_hierarchical!(base; port as Port(port) => {res = Some(SystemFunctionCall::PortConnected(port))});
                        res?
                    }
                    SystemFunctionCall::ParameterGiven(ref parameter) => {
                        let mut res = None;
                        resolve_hierarchical!(base; parameter as Parameter(param) => {res = Some(SystemFunctionCall::ParameterGiven(param))});
                        res?
                    }
                };

                base.hir.expressions.push(Node {
                    source: expression.source,
                    contents: Expression::Primary(Primary::SystemFunctionCall(new_call)),
                })
            }

            _ => base.fold_expression(expression_id, self)?,
        };
        Some(res)
    }
}
