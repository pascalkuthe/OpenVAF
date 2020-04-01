/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use std::result::Result;

use crate::ast::BuiltInFunctionCall::*;
use crate::ast::{HierarchicalId, Node};
use crate::ast_lowering::ast_to_hir_fold::Fold;
use crate::ast_lowering::branch_resolution::BranchResolver;
use crate::ast_lowering::error::Type::{EmptyBranchAccess, UnexpectedTokenInBranchAccess};
use crate::ast_lowering::error::*;
use crate::ast_lowering::name_resolution::Resolver;
use crate::ast_lowering::VerilogContext;
use crate::hir::{Expression, Primary};
use crate::ir::{BranchId, DisciplineId, ExpressionId};
use crate::symbol::keywords;
use crate::util::Push;
use crate::{ast, Ast, Hir, Span};

pub struct ExpressionFolder<'tag, 'fold, 'lt> {
    pub(super) state: VerilogContext,
    pub(super) branch_resolver: &'lt mut BranchResolver<'tag, 'fold>,
    pub(super) base: &'lt mut Fold<'tag, 'fold>,
}

impl<'tag, 'fold, 'lt> ExpressionFolder<'tag, 'fold, 'lt> {
    //TODO refactor fold
    pub fn fold_expression(
        &mut self,
        expression_id: ExpressionId<'tag>,
    ) -> Result<ExpressionId<'tag>, ()> {
        let expression = &self.base.ast[expression_id];
        let res = match expression.contents {
            ast::Expression::BinaryOperator(lhs, op, rhs) => {
                let lhs = self.fold_expression(lhs);
                let rhs = self.fold_expression(rhs);
                self.base.hir.push(Node {
                    source: expression.source,
                    contents: Expression::BinaryOperator(lhs?, op, rhs?),
                })
            }

            ast::Expression::UnaryOperator(unary_op, expr) => {
                let expr = self.fold_expression(expr)?;
                self.base.hir.push(Node {
                    source: expression.source,
                    contents: Expression::UnaryOperator(unary_op, expr),
                })
            }

            ast::Expression::Primary(ast::Primary::BranchAccess(ref nature, ref branch_access)) => {
                let (branch_access, discipline) = self
                    .branch_resolver
                    .resolve_branch_access(&mut self.base, branch_access)?;
                let nature = self.branch_resolver.resolve_discipline_access(
                    &mut self.base,
                    nature,
                    discipline,
                )?;
                self.base.hir.push(Node {
                    source: expression.source,
                    contents: Expression::Primary(Primary::BranchAccess(nature, branch_access)),
                })
            }

            ast::Expression::Primary(ast::Primary::VariableOrNetReference(ref ident)) => {
                resolve_hierarchical! {self.base; ident as
                    Variable(vid) => {
                        if self.state.contains(VerilogContext::constant){
                            self.base.error(Error{
                                source:expression.source,
                                error_type:Type::NotAllowedInConstantContext(NonConstantExpression::VariableReference)
                                });
                        }else {
                            return Ok(self.base.hir.push(Node{
                                source:expression.source,
                                contents:Expression::Primary(Primary::VariableReference(vid))
                            }))
                        }
                    },
                    Parameter(pid) => {
                        return Ok(self.base.hir.push(Node{
                            source:expression.source,
                            contents:Expression::Primary(Primary::ParameterReference(pid))
                        }))
                    }
                /*Port(pid) => {
                            self.base.hir.push(Node{
                                source:expression.source,
                                contents:Expression::Primary(Primary::PortReference(pid))
                            })
                    },
                    Net(nid) => {
                        self.base.hir.push(Node{
                            source:expression.source,
                            contents:Expression::Primary(Primary::NetReference(nid))
                        })
                    } TODO discrete net/por access */
                }
                return Err(());
            }

            ast::Expression::Primary(ast::Primary::Integer(val)) => self.base.hir.push(Node {
                source: expression.source,
                contents: Expression::Primary(Primary::Integer(val)),
            }),
            ast::Expression::Primary(ast::Primary::UnsignedInteger(val)) => {
                self.base.hir.push(Node {
                    source: expression.source,
                    contents: Expression::Primary(Primary::UnsignedInteger(val)),
                })
            }

            ast::Expression::Primary(ast::Primary::Real(val)) => self.base.hir.push(Node {
                source: expression.source,
                contents: Expression::Primary(Primary::Real(val)),
            }),

            ast::Expression::Primary(ast::Primary::FunctionCall(ref ident, ref parameters)) => {
                if self.state.contains(VerilogContext::constant) {
                    self.base.error(Error {
                        source: expression.source,
                        error_type: Type::NotAllowedInConstantContext(
                            NonConstantExpression::VariableReference,
                        ),
                    });
                    return Err(());
                }

                resolve_hierarchical! { self.base; ident as
                    Function(fid) => {
                        let parameters = parameters
                            .iter()
                            .copied()
                            .filter_map(|expr| self.fold_expression(expr).ok())
                            .collect();
                        return Ok(self.base.hir.push(Node{
                            source:expression.source,
                            contents:Expression::Primary(Primary::FunctionCall(fid,parameters))
                        }))

                    },
                    Nature(nature) => {
                        let (branch_access,discipline) = self.reinterpret_function_parameters_as_branch_access(ident.span(),parameters)?;
                        let discipline_access = BranchResolver::resolve_nature_access(&mut self.base,nature,discipline)?;
                        return Ok(self.base.hir.push(Node{
                            source:expression.source,
                            contents:Expression::Primary(Primary::BranchAccess(discipline_access,branch_access))
                        }))

                    }
                }
                return Err(());
            }
            ast::Expression::Primary(ast::Primary::BuiltInFunctionCall(ref function_call)) => {
                let function_call = match function_call {
                    Pow(expr0, expr1) => {
                        let expr0 = self.fold_expression(*expr0);
                        let expr1 = self.fold_expression(*expr1);
                        Pow(expr0?, expr1?)
                    }

                    Hypot(expr0, expr1) => {
                        let expr0 = self.fold_expression(*expr0);
                        let expr1 = self.fold_expression(*expr1);
                        Hypot(expr0?, expr1?)
                    }

                    Min(expr0, expr1) => {
                        let expr0 = self.fold_expression(*expr0);
                        let expr1 = self.fold_expression(*expr1);
                        Min(expr0?, expr1?)
                    }

                    Max(expr0, expr1) => {
                        let expr0 = self.fold_expression(*expr0);
                        let expr1 = self.fold_expression(*expr1);
                        Max(expr0?, expr1?)
                    }

                    ArcTan2(expr0, expr1) => {
                        let expr0 = self.fold_expression(*expr0);
                        let expr1 = self.fold_expression(*expr1);
                        ArcTan2(expr0?, expr1?)
                    }

                    Sqrt(expr) => Sqrt(self.fold_expression(*expr)?),
                    Exp(expr) => Exp(self.fold_expression(*expr)?),
                    Ln(expr) => Ln(self.fold_expression(*expr)?),
                    Log(expr) => Log(self.fold_expression(*expr)?),
                    Abs(expr) => Abs(self.fold_expression(*expr)?),
                    Floor(expr) => Floor(self.fold_expression(*expr)?),
                    Ceil(expr) => Ceil(self.fold_expression(*expr)?),
                    Sin(expr) => Sin(self.fold_expression(*expr)?),
                    Cos(expr) => Cos(self.fold_expression(*expr)?),
                    Tan(expr) => Tan(self.fold_expression(*expr)?),
                    ArcSin(expr) => ArcSin(self.fold_expression(*expr)?),
                    ArcCos(expr) => ArcCos(self.fold_expression(*expr)?),
                    ArcTan(expr) => ArcTan(self.fold_expression(*expr)?),
                    SinH(expr) => SinH(self.fold_expression(*expr)?),
                    CosH(expr) => CosH(self.fold_expression(*expr)?),
                    TanH(expr) => TanH(self.fold_expression(*expr)?),
                    ArcSinH(expr) => ArcSinH(self.fold_expression(*expr)?),
                    ArcCosH(expr) => ArcCosH(self.fold_expression(*expr)?),
                    ArcTanH(expr) => ArcTanH(self.fold_expression(*expr)?),
                };
                self.base.hir.push(Node {
                    contents: Expression::Primary(Primary::BuiltInFunctionCall(function_call)),
                    source: expression.source,
                })
            }

            ast::Expression::Condtion(condition, question_span, if_val, colon_span, else_val) => {
                let condition = self.fold_expression(condition);
                let if_val = self.fold_expression(if_val);
                let else_val = self.fold_expression(else_val);
                self.base.hir.push(Node {
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

            ast::Expression::Primary(ast::Primary::SystemFunctionCall(call)) => {
                if self.state.contains(VerilogContext::constant) {
                    self.base.error(Error {
                        source: expression.source,
                        error_type: Type::NotAllowedInConstantContext(
                            NonConstantExpression::VariableReference,
                        ),
                    });
                    return Err(());
                }
                if call.name == keywords::TEMPERATURE {
                    //todo more calls
                    self.base.hir.push(Node::new(
                        Expression::Primary(Primary::SystemFunctionCall(call)),
                        expression.source,
                    ))
                } else {
                    self.base.error(Error {
                        error_type: Type::NotFound(call.name),
                        source: call.span,
                    });
                    return Err(());
                }
                //TODO args
            }
        };
        Ok(res)
    }

    /// Due to an ambiguous grammar the parser can not determine whether V(x,y) is a function call or a Branch access when it appears inside an expression.
    /// When the function call is resolved to be to a nature this function is called to reinterpret and resolve the function parameters as a branch access
    fn reinterpret_function_parameters_as_branch_access(
        &mut self,
        nature_indent_span: Span,
        parameters: &[ExpressionId<'tag>],
    ) -> Result<(BranchId<'tag>, DisciplineId<'tag>), ()> {
        match parameters {
            [branch] => {
                let expr_node = &self.base.ast[*branch];
                let branch_access = ast::BranchAccess::Explicit(
                    self.reinterpret_expression_as_identifier(expr_node)?
                        .clone(),
                );
                self.branch_resolver
                    .resolve_branch_access(self.base, &Node::new(branch_access, expr_node.source))
            }
            [net1, net2] => {
                let expr_node = &self.base.ast[*net1];
                let span = expr_node.source;
                let net1 = self.reinterpret_expression_as_identifier(expr_node);
                let expr_node = &self.base.ast[*net2];
                let span = span.extend(expr_node.source);
                let net2 = self.reinterpret_expression_as_identifier(expr_node);
                let branch_access =
                    ast::BranchAccess::Implicit(ast::Branch::Nets(net1?.clone(), net2?.clone()));
                self.branch_resolver.resolve_branch_access(
                    &mut self.base,
                    &Node::new(branch_access, expr_node.source),
                )
            }
            [] => {
                self.base.error(Error {
                    source: nature_indent_span,
                    error_type: EmptyBranchAccess,
                });
                Err(())
            }
            [_, _, unexpected, ..] => {
                self.base.error(Error {
                    source: self.base.ast[*unexpected]
                        .source
                        .extend(self.base.ast[*parameters.last().unwrap()].source),
                    error_type: UnexpectedTokenInBranchAccess,
                });
                Err(())
            }
        }
    }

    fn reinterpret_expression_as_identifier(
        &mut self,
        expression: &'lt Node<ast::Expression<'tag>>,
    ) -> Result<&'lt HierarchicalId, ()> {
        if let ast::Expression::Primary(ast::Primary::VariableOrNetReference(ref name)) =
            expression.contents
        {
            Ok(name)
        } else {
            self.base.error(Error {
                source: expression.source,
                error_type: UnexpectedTokenInBranchAccess,
            });
            Err(())
        }
    }
}
