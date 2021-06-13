/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::ast;
use crate::branches::resolver::{BranchProbeKind, BranchResolver, NatureAccess};
use crate::error::Error::{
    DerivativeNotAllowed, NotAllowedInConstantContext, PortFlowCanNotBeANonLinearity,
};
use crate::error::{Error, NonConstantExpression};
use crate::{Fold, VerilogContext};
use openvaf_hir::{DisciplineAccess, LimFunction};
use openvaf_hir::{Expression, Primary};
use openvaf_ir::ids::ExpressionId;
use openvaf_ir::Unknown;
use openvaf_ir::{NoiseSource, SimpleConstVal};
use openvaf_ir::{Spanned, SystemFunctionCall};
use openvaf_session::symbols::{keywords, Symbol};
use tracing::trace_span;

pub trait ExpressionFolder<F: Fn(Symbol) -> AllowedReferences> {
    fn fold(&mut self, expression_id: ExpressionId, base: &mut Fold<F>) -> Option<ExpressionId>;

    fn allowed_references(&self) -> AllowedReferences;
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum AllowedReferences {
    None,
    Parameters,
    All,
}

/// Folds real constants. These may not even contain references to parameters
pub struct ConstantExpressionFolder(pub AllowedReferences);
impl<F: Fn(Symbol) -> AllowedReferences> ExpressionFolder<F> for ConstantExpressionFolder {
    #[inline]
    fn fold(&mut self, expression_id: ExpressionId, base: &mut Fold<F>) -> Option<ExpressionId> {
        let span = trace_span!("expression", id = expression_id.index());
        let _enter = span.enter();
        base.fold_expression(expression_id, self)
    }

    fn allowed_references(&self) -> AllowedReferences {
        self.0
    }
}

impl<'lt, F: Fn(Symbol) -> AllowedReferences> Fold<'lt, F> {
    #[inline]
    fn fold_expression(
        &mut self,
        expression_id: ExpressionId,
        expr_folder: &mut impl ExpressionFolder<F>,
    ) -> Option<ExpressionId> {
        let tspan = trace_span!("expression", id = expression_id.index());
        let _enter = tspan.enter();

        let expression = &self.ast[expression_id];
        let res = match expression.contents {
            ast::Expression::BinaryOperator(lhs, op, rhs) => {
                let lhs = expr_folder.fold(lhs, &mut *self);
                let rhs = expr_folder.fold(rhs, &mut *self);
                self.hir.expressions.push(Spanned {
                    span: expression.span,
                    contents: Expression::BinaryOperator(lhs?, op, rhs?),
                })
            }

            ast::Expression::UnaryOperator(unary_op, expr) => {
                let expr = expr_folder.fold(expr, &mut *self)?;
                self.hir.expressions.push(Spanned {
                    span: expression.span,
                    contents: Expression::UnaryOperator(unary_op, expr),
                })
            }

            ast::Expression::Primary(ast::Primary::Constant(ref val)) => {
                self.hir.expressions.push(Spanned {
                    contents: Expression::Primary(Primary::Constant(val.clone())),
                    span: expression.span,
                })
            }

            ast::Expression::Primary(ast::Primary::DoubleArgMath(call, arg1, arg2)) => {
                let arg1 = expr_folder.fold(arg1, &mut *self);
                let arg2 = expr_folder.fold(arg2, &mut *self);
                self.hir.expressions.push(Spanned {
                    contents: Expression::BuiltInFunctionCall2p(call, arg1?, arg2?),
                    span: expression.span,
                })
            }

            ast::Expression::Primary(ast::Primary::SingleArgMath(call, arg)) => {
                let arg = expr_folder.fold(arg, &mut *self)?;
                self.hir.expressions.push(Spanned {
                    contents: Expression::BuiltInFunctionCall1p(call, arg),
                    span: expression.span,
                })
            }

            ast::Expression::Condition(condition, if_val, else_val) => {
                let condition = expr_folder.fold(condition, &mut *self);
                let if_val = expr_folder.fold(if_val, &mut *self);
                let else_val = expr_folder.fold(else_val, &mut *self);
                self.hir.expressions.push(Spanned {
                    span: expression.span,
                    contents: Expression::Condition(condition?, if_val?, else_val?),
                })
            }

            ast::Expression::Primary(ast::Primary::Reference(ref ident)) => {
                resolve_hierarchical!(self; ident as
                    Variable(vid) => {
                        if expr_folder.allowed_references() == AllowedReferences::All{
                            self.hir.expressions.push(Spanned{
                                span:expression.span,
                                contents:Expression::Primary(Primary::VariableReference(vid))
                            })
                        }else {
                            self.error(NotAllowedInConstantContext(NonConstantExpression::VariableReference,expression.span));
                            return None
                        }
                    },

                    Net(nid) => {
                        if expr_folder.allowed_references() == AllowedReferences::All{
                            self.hir.expressions.push(Spanned{
                                span:expression.span,
                                contents:Expression::Primary(Primary::NetReference(nid))
                            })
                        }else {
                            self.error(NotAllowedInConstantContext(NonConstantExpression::PortReferences,expression.span));
                            return None
                        }
                    },

                    Port(pid) => {
                        if expr_folder.allowed_references() == AllowedReferences::All{
                            self.hir.expressions.push(Spanned{
                                span:expression.span,
                                contents:Expression::Primary(Primary::PortReference(pid))
                            })
                        }else {
                            self.error(NotAllowedInConstantContext(NonConstantExpression::NetReferences,expression.span));
                            return None
                        }
                    },

                    Parameter(pid) => {
                        if expr_folder.allowed_references()  == AllowedReferences::None{
                            self.error(NotAllowedInConstantContext(NonConstantExpression::ParameterReference,expression.span));
                            return None
                        }else {
                            self.hir.expressions.push(Spanned{
                                span:expression.span,
                                contents:Expression::Primary(Primary::ParameterReference(pid))
                            })
                        }
                    }
                )?
            }

            ast::Expression::Array(ref arr) => {
                let arr = arr
                    .iter()
                    .filter_map(|expr| expr_folder.fold(*expr, self))
                    .collect();
                self.hir
                    .expressions
                    .push(Spanned::new(Expression::Array(arr), expression.span))
            }

            // These are "real" constant. Standard allows this but we just cant do this here
            ast::Expression::Primary(ast::Primary::FunctionCall(_, _))
            | ast::Expression::Primary(ast::Primary::SystemFunctionCall(_)) => {
                self.error(NotAllowedInConstantContext(
                    NonConstantExpression::FunctionCall,
                    expression.span,
                ));
                return None;
            }

            ast::Expression::Primary(ast::Primary::PortFlowProbe(_, _))
            | ast::Expression::Primary(ast::Primary::DerivativeByBranch(_, _))
            | ast::Expression::Primary(ast::Primary::DerivativeByTime(_))
            | ast::Expression::Primary(ast::Primary::Noise(_, _)) => {
                self.error(NotAllowedInConstantContext(
                    NonConstantExpression::BranchAccess,
                    expression.span,
                ));

                return None;
            }

            ast::Expression::Error => unreachable!("encountered error node in hir lowerinfg"),
        };
        Some(res)
    }
}

pub struct StatementExpressionFolder<'lt> {
    pub(super) state: VerilogContext,
    pub(super) branch_resolver: &'lt mut BranchResolver,
}

impl<'a, F: Fn(Symbol) -> AllowedReferences> ExpressionFolder<F> for StatementExpressionFolder<'a> {
    fn fold(&mut self, expression_id: ExpressionId, base: &mut Fold<F>) -> Option<ExpressionId> {
        let expression = &base.ast[expression_id];
        let res = match expression.contents {
            ast::Expression::Primary(ast::Primary::PortFlowProbe(access, ref port)) => {
                if self.state.contains(VerilogContext::FUNCTION) {
                    base.error(NotAllowedInConstantContext(
                        NonConstantExpression::FunctionCall,
                        expression.span,
                    ));

                    return None;
                }

                let port = resolve_hierarchical!(base; port as Port(id) => id);
                let access = NatureAccess::resolve_from_ident(access, base)?;
                let port = port?;
                let discipline = base.hir[base.hir[port].net].discipline;
                BranchResolver::check_port_discipline_access(base, access, discipline);
                base.hir.expressions.push(Spanned {
                    span: expression.span,
                    contents: Expression::Primary(Primary::PortFlowAccess(port)),
                })
            }

            ast::Expression::Primary(ast::Primary::DerivativeByTime(expr)) => {
                if self.state.contains(VerilogContext::FUNCTION) {
                    base.error(NotAllowedInConstantContext(
                        NonConstantExpression::AnalogFilter,
                        expression.span,
                    ));
                    return None;
                }
                let expr = self.fold(expr, base)?;
                base.hir.expressions.push(Spanned {
                    span: expression.span,
                    contents: Expression::TimeDerivative(expr),
                })
            }

            ast::Expression::Primary(ast::Primary::Noise(source, src)) => {
                if self.state.contains(VerilogContext::FUNCTION) {
                    base.error(NotAllowedInConstantContext(
                        NonConstantExpression::AnalogFilter,
                        expression.span,
                    ));

                    return None;
                }
                let source = match source {
                    NoiseSource::White(expr) => NoiseSource::White(self.fold(expr, base)?),
                    NoiseSource::Flicker(expr1, expr2) => {
                        NoiseSource::Flicker(self.fold(expr1, base)?, self.fold(expr2, base)?)
                    }
                    NoiseSource::Table(_) | NoiseSource::TableLog(_) => todo!(),
                };

                let src = src.and_then(|expr| self.fold(expr, base));

                base.hir.expressions.push(Spanned {
                    span: expression.span,
                    contents: Expression::Noise(source, src),
                })
            }

            ast::Expression::Primary(ast::Primary::DerivativeByBranch(
                expr_to_derive,
                branch_access,
            )) => {
                if self.state.contains(VerilogContext::FUNCTION) {
                    base.error(NotAllowedInConstantContext(
                        NonConstantExpression::AnalogFilter,
                        expression.span,
                    ));
                    return None;
                }
                let expr_to_derive = self.fold(expr_to_derive, base);

                let unknown = if let ast::Expression::Primary(ast::Primary::FunctionCall(
                    access_ident,
                    args,
                )) = &base.ast[branch_access].contents
                {
                    let access = NatureAccess::resolve_from_ident(*access_ident, base);
                    self.branch_resolver
                        .handle_branch_probe_args(
                            *access_ident,
                            &args,
                            |_, base, net, _span| {
                                let discipline = base.hir[net].discipline;
                                let access = BranchResolver::resolve_discipline_access(
                                    base, access?, discipline,
                                )?;
                                match access {
                                    DisciplineAccess::Flow => {
                                        base.error(DerivativeNotAllowed(expression.span));
                                        None
                                    }
                                    DisciplineAccess::Potential => {
                                        Some(Unknown::NodePotential(net))
                                    }
                                }
                            },
                            |resolver, base, hi, lo, span| {
                                BranchResolver::check_branch(hi, lo, span, base);
                                let discipline = base.hir[hi].discipline;
                                let access = BranchResolver::resolve_discipline_access(
                                    base, access?, discipline,
                                )?;
                                match access {
                                    DisciplineAccess::Flow => Some(Unknown::Flow(
                                        resolver.unnamed_branch(span, hi, lo, base),
                                    )),
                                    DisciplineAccess::Potential => {
                                        base.error(DerivativeNotAllowed(expression.span));
                                        None
                                    }
                                }
                            },
                            |_, base, branch, span| {
                                let discipline = base.hir[base.hir[branch].hi].discipline;
                                let access = BranchResolver::resolve_discipline_access(
                                    base, access?, discipline,
                                )?;
                                match access {
                                    DisciplineAccess::Flow => Some(Unknown::Flow(branch)),
                                    DisciplineAccess::Potential => {
                                        base.error(DerivativeNotAllowed(span));
                                        None
                                    }
                                }
                            },
                            |_, base, _port, span| {
                                base.error(DerivativeNotAllowed(span));
                                None
                            },
                            base,
                        )
                        .flatten()?
                } else {
                    base.error(DerivativeNotAllowed(expression.span));
                    return None;
                };

                base.hir.expressions.push(Spanned {
                    span: expression.span,
                    contents: Expression::PartialDerivative(expr_to_derive?, unknown),
                })
            }

            ast::Expression::Primary(ast::Primary::FunctionCall(ref ident, ref parameters)) => {
                let access = match ident.name {
                    keywords::potential => NatureAccess::Pot(ident.span),
                    keywords::flow => NatureAccess::Flow(ident.span),
                    _ => {
                        let nature = resolve! ( base; ident as
                            Function(fid) => {
                                let parameters = parameters
                                    .iter()
                                    .copied()
                                    .filter_map(|expr| self.fold(expr,&mut *base))
                                    .collect();

                                return Some(base.hir.expressions.push(Spanned{
                                    span:expression.span,
                                    contents:Expression::FunctionCall(fid,parameters)
                                }))
                            },

                            NatureAccess(id) => id
                        )?;
                        NatureAccess::Named(nature)
                    }
                };

                match self
                    .branch_resolver
                    .resolve_branch_probe_call(access, parameters, base)?
                {
                    BranchProbeKind::Port(id) => base.hir.expressions.push(Spanned {
                        span: expression.span,
                        contents: Expression::Primary(Primary::PortFlowAccess(id)),
                    }),
                    BranchProbeKind::Branch(access, branch) => base.hir.expressions.push(Spanned {
                        span: expression.span,
                        contents: Expression::Primary(Primary::BranchAccess(access, branch)),
                    }),
                }
            }

            ast::Expression::Primary(ast::Primary::SystemFunctionCall(ref call)) => {
                let new_call = match call {
                    SystemFunctionCall::Temperature => SystemFunctionCall::Temperature,

                    SystemFunctionCall::Vt(temp) => {
                        SystemFunctionCall::Vt(temp.map(|temp| self.fold(temp, base)).flatten())
                    }

                    SystemFunctionCall::Simparam(name, default) => {
                        let default = default
                            .map(|default| {
                                ConstantExpressionFolder(AllowedReferences::None)
                                    .fold(default, base)
                            })
                            .flatten();
                        SystemFunctionCall::Simparam(self.fold(*name, base)?, default)
                    }

                    SystemFunctionCall::SimparamStr(name) => SystemFunctionCall::SimparamStr(
                        ConstantExpressionFolder(AllowedReferences::None).fold(*name, base)?,
                    ),

                    SystemFunctionCall::PortConnected(ref port) => {
                        let port = base.reinterpret_expression_as_hierarchical_identifier(
                            "$port_connected",
                            &base.ast[*port],
                        )?;
                        resolve_hierarchical!(base; port as Port(port) => SystemFunctionCall::PortConnected(port) )?
                    }

                    SystemFunctionCall::ParameterGiven(parameter) => {
                        let parameter = base.reinterpret_expression_as_hierarchical_identifier(
                            "$param_given",
                            &base.ast[*parameter],
                        )?;
                        resolve_hierarchical!(base; parameter as Parameter(param) => SystemFunctionCall::ParameterGiven(param) )?
                    }

                    SystemFunctionCall::Lim { args, .. } => {
                        let (access, fun, args) = match args.as_slice() {
                            [] => {
                                base.error(Error::EmptyLimit(expression.span));
                                return None;
                            }
                            [_] => {
                                base.error(Error::LimRequiresFunction(expression.span));
                                return None;
                            }

                            [access, fun, args @ ..] => (access, fun, args),
                        };

                        let fun = &base.ast.expressions[*fun];

                        let fun = match fun.contents {
                            ast::Expression::Primary(ast::Primary::Reference(ref name)) => {
                                resolve_hierarchical!(base; name as Function(fun) => fun)
                                    .map(LimFunction::VerilogA)
                            }

                            ast::Expression::Primary(ast::Primary::Constant(
                                ast::ConstVal::Scalar(SimpleConstVal::String(name)),
                            )) => Some(LimFunction::Native(name)),

                            _ => {
                                base.error(Error::IllegalLimitFn(fun.span));
                                None
                            }
                        };

                        let access_expr = &base.ast.expressions[*access];
                        let access = if let ast::Expression::Primary(ast::Primary::FunctionCall(
                            ref ident,
                            ref parameters,
                        )) = access_expr.contents
                        {
                            let access = match ident.name {
                                keywords::potential => Some(NatureAccess::Pot(ident.span)),
                                keywords::flow => Some(NatureAccess::Flow(ident.span)),
                                _ => {
                                    resolve! ( base; ident as NatureAccess(id) => NatureAccess::Named(id))
                                }
                            };

                            access.and_then(|access| {
                                if let BranchProbeKind::Branch(access, branch) = self
                                    .branch_resolver
                                    .resolve_branch_probe_call(access, parameters, base)?
                                {
                                    Some((access, branch))
                                } else {
                                    base.error(PortFlowCanNotBeANonLinearity(access_expr.span));
                                    None
                                }
                            })
                        } else {
                            None
                        };

                        let args = args
                            .iter()
                            .copied()
                            .filter_map(|expr| self.fold(expr, &mut *base))
                            .collect();

                        SystemFunctionCall::Lim {
                            access: access?,
                            fun: fun?,
                            args,
                        }
                    }
                };

                base.hir.expressions.push(Spanned {
                    span: expression.span,
                    contents: Expression::SystemFunctionCall(new_call),
                })
            }

            _ => base.fold_expression(expression_id, self)?,
        };
        Some(res)
    }

    fn allowed_references(&self) -> AllowedReferences {
        if self.state.contains(VerilogContext::CONSTANT) {
            AllowedReferences::Parameters
        } else {
            AllowedReferences::All
        }
    }
}
