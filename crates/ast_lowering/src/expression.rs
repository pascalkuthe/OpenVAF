/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::allowed_operations::VerilogAState;
use crate::ast;
use crate::branches::resolver::{BranchResolver, NatureAccess};
use crate::Fold;
use data_structures::index_vec::IndexVec;
use hir::functions::{
    AcStimulus, Derivative, Discontinuity, Function, LimFunction, Noise, ParameterizationRead,
};
use hir::{AllowedOperation, AllowedOperations, BranchId, Expression};
use ir::ids::{CallArg, ExpressionId};
use ir::{DisciplineAccess, SimpleConstVal};
use ir::{Math1, Math2, Print, Spanned, StopTask};
use session::sourcemap::{Span, StringLiteral};
use session::symbols::{kw, sysfun, Ident, Symbol};
use osdi_types::ConstVal::Scalar;
use tracing::trace_span;

pub(crate) enum FunctionFoldResult {
    BranchAccess((DisciplineAccess, BranchId)),
    Function(Function, IndexVec<CallArg, ExpressionId>, Span),
}
/// Folds real constants. These may not even contain references to parameters
pub struct ExpressionFolder<'a, 'ast, F: Fn(Symbol) -> AllowedOperations> {
    pub state: VerilogAState,
    pub branch_resolver: Option<&'a mut BranchResolver>,
    pub base: &'a mut Fold<'ast, F>,
}
impl<'a, 'ast, F: Fn(Symbol) -> AllowedOperations> ExpressionFolder<'a, 'ast, F> {
    fn fold_branch_probe(
        &mut self,
        access: NatureAccess,
        span: Span,
        parameters: &IndexVec<CallArg, ExpressionId>,
    ) -> Option<(DisciplineAccess, BranchId)> {
        self.test_allowed(AllowedOperation::BranchAccess, span)?;
        let (access, branch) = self
            .branch_resolver
            .as_mut()
            .unwrap()
            .resolve_branch_probe_call(access, parameters, self.base)?;
        match access {
            DisciplineAccess::Potential => self.base.hir[branch].voltage_access.push(span),
            DisciplineAccess::Flow => self.base.hir[branch].current_acccess.push(span),
        }
        Some((access, branch))
    }

    fn test_allowed(&mut self, op: AllowedOperation, span: Span) -> Option<()> {
        if self.state.test_allowed(self.base, op, span) {
            Some(())
        } else {
            None
        }
    }

    pub(crate) fn fold_function(
        &mut self,
        span: Span,
        ident: Ident,
        parameters: &IndexVec<CallArg, ExpressionId>,
    ) -> Option<FunctionFoldResult> {
        let fun = match ident.name {
            kw::potential => {
                return FunctionFoldResult::BranchAccess(self.fold_branch_probe(
                    NatureAccess::Flow(ident.span),
                    span,
                    parameters,
                )?)
                .into()
            }
            kw::flow => {
                return FunctionFoldResult::BranchAccess(self.fold_branch_probe(
                    NatureAccess::Flow(ident.span),
                    span,
                    parameters,
                )?)
                .into()
            }

            sysfun::clog2 => Math1::Clog2.into(),
            kw::abs => Math1::Abs.into(),
            kw::acos | sysfun::acos => Math1::ArcCos.into(),
            kw::acosh | sysfun::acosh => Math1::ArcCosH.into(),
            kw::asin | sysfun::asin => Math1::ArcSin.into(),
            kw::asinh | sysfun::asinh => Math1::ArcSinH.into(),
            kw::atan | sysfun::atan => Math1::ArcTan.into(),
            kw::atanh | sysfun::atanh => Math1::ArcTan.into(),
            kw::ceil | sysfun::ceil => Math1::Ceil.into(),
            kw::cos | sysfun::cos => Math1::Cos.into(),
            kw::cosh | sysfun::cosh => Math1::CosH.into(),
            kw::exp | sysfun::exp => Math1::Exp(false).into(),
            kw::ln | sysfun::ln => Math1::Ln.into(),
            kw::sin | sysfun::sin => Math1::Sin.into(),
            kw::sinh | sysfun::sinh => Math1::SinH.into(),
            kw::sqrt | sysfun::sqrt => Math1::Sqrt.into(),
            kw::tan | sysfun::tan => Math1::Tan.into(),
            kw::tanh | sysfun::tanh => Math1::TanH.into(),
            kw::log | sysfun::log10 => Math1::Log.into(),
            kw::floor | sysfun::floor => Math1::Floor.into(),

            kw::atan2 | sysfun::atan2 => Math2::ArcTan2.into(),
            kw::hypot | sysfun::hypot => Math2::Hypot.into(),
            kw::pow | sysfun::pow => Math2::Pow.into(),
            kw::max => Math2::Max.into(),
            kw::min => Math2::Min.into(),

            kw::limexp => {
                self.test_allowed(AllowedOperation::AnalogFilters, span);
                Math1::Exp(true).into()
            }
            kw::ddt => {
                self.test_allowed(AllowedOperation::AnalogFilters, span);
                Derivative::Time.into()
            }
            kw::ddx => {
                self.test_allowed(AllowedOperation::AnalogFilters, span);
                Derivative::Partial.into()
            }

            kw::idt
            | kw::idtmod
            | kw::absdelay
            | kw::laplace_nd
            | kw::laplace_np
            | kw::laplace_zd
            | kw::laplace_zp
            | kw::zi_nd
            | kw::zi_np
            | kw::zi_zd
            | kw::zi_zp
            | kw::slew
            | kw::transition
            | kw::last_crossing => {
                todo!("Error")
            }

            kw::flicker_noise => Noise::Flicker.into(),
            kw::noise_table => Noise::Table.into(),
            kw::noise_table_log => Noise::TableLog.into(),
            kw::white_noise => Noise::White.into(),
            kw::ac_stim => AcStimulus.into(),

            sysfun::display => Print::Display.into(),
            sysfun::strobe => Print::Strobe.into(),
            sysfun::write => Print::Write.into(),
            sysfun::debug => Print::Debug.into(),

            sysfun::finish => StopTask::Finish.into(),
            sysfun::stop => StopTask::Stop.into(),
            sysfun::fatal => Print::Fatal.into(),

            sysfun::warning => Print::Warn.into(),
            sysfun::error => Print::Err.into(),
            sysfun::info => Print::Info.into(),

            sysfun::temperature => ParameterizationRead::Temperature.into(),
            sysfun::vt => ParameterizationRead::Vt.into(),
            sysfun::simparam => ParameterizationRead::Simparam.into(),
            sysfun::simparam_str => ParameterizationRead::SimparamStr.into(),

            sysfun::param_given => ParameterizationRead::ParameterGiven.into(),
            sysfun::port_connected => ParameterizationRead::PortConnected.into(),

            sysfun::discontinuity => Discontinuity.into(),
            sysfun::limit => parameters
                .get(1)
                .and_then(|fun| match self.base.ast[*fun].contents {
                    ast::Expression::Primary(ast::Primary::Constant(Scalar(
                        SimpleConstVal::String(fun),
                    ))) => LimFunction::Native(fun).into(),
                    ast::Expression::Primary(ast::Primary::Reference(ref fun)) => {
                        let fun = resolve_hierarchical!(self.base; fun as Function(id) => id)?;
                        LimFunction::VerilogA(fun).into()
                    }
                    _ => todo!("error"),
                })
                .unwrap_or(LimFunction::Native(StringLiteral::DUMMY))
                .into(),

            sysfun if sysfun.is_valid_system_function_call() => {
                todo!("error")
            }
            sysfun if sysfun.is_system_function_call() => {
                todo!("error")
            }

            _ => resolve! ( self.base; ident as
                Function(fid) => fid.into(),
                NatureAccess(id) => return FunctionFoldResult::BranchAccess(self.fold_branch_probe(NatureAccess::Named(id), span, parameters)?).into()
            )?,
        };

        let parameters = parameters
            .iter_enumerated()
            .filter_map(|(arg, expr)| {
                let old_state = self.state.enter_function_arg(arg, &fun, &self.base.hir);
                let res = self.fold(*expr);
                self.state = old_state;
                res
            })
            .collect();

        FunctionFoldResult::Function(fun, parameters, span).into()
    }
}

impl<'lt, 'ast, F: Fn(Symbol) -> AllowedOperations> ExpressionFolder<'lt, 'ast, F> {
    pub fn fold(&mut self, expression_id: ExpressionId) -> Option<ExpressionId> {
        let tspan = trace_span!("expression", id = expression_id.index());
        let _enter = tspan.enter();

        let expression = &self.base.ast[expression_id];
        let res = match expression.contents {
            ast::Expression::BinaryOperator(lhs, op, rhs) => {
                let lhs = self.fold(lhs);
                let rhs = self.fold(rhs);
                Expression::BinaryOperator(lhs?, op, rhs?)
            }

            ast::Expression::UnaryOperator(unary_op, expr) => {
                let expr = self.fold(expr)?;
                Expression::UnaryOperator(unary_op, expr)
            }

            ast::Expression::Primary(ast::Primary::Constant(ref val)) => {
                Expression::Constant(val.clone())
            }

            ast::Expression::Condition(condition, if_val, else_val) => {
                let condition = self.fold(condition);
                let old_state = self.state.enter_conditional_behaviour();
                let if_val = self.fold(if_val);
                let else_val = self.fold(else_val);
                self.state = old_state;
                Expression::Condition(condition?, if_val?, else_val?)
            }

            ast::Expression::Primary(ast::Primary::Reference(ref ident)) => {
                resolve_hierarchical!(self.base; ident as
                    Variable(vid) => {
                        self.test_allowed(AllowedOperation::VariableReferences, expression.span)?;
                        Expression::VariableReference(vid)
                    },

                    Node(_,nid) => {
                        self.test_allowed(AllowedOperation::NetReferences, expression.span)?;
                        Expression::NodeReference(nid)
                    },

                    Port(pid) => {
                        self.test_allowed(AllowedOperation::PortReferences, expression.span)?;
                        Expression::PortReference(pid)
                    },

                    Parameter(pid) => {
                        self.test_allowed(AllowedOperation::ParameterReferences, expression.span)?;
                        Expression::ParameterReference(pid)
                    },

                    // Required for lim function args. just an implementation detail
                    Function(fid) => {
                        self.test_allowed(AllowedOperation::UserFunctionReference, expression.span)?;
                        Expression::FunctionCall(fid.into(),IndexVec::new(),expression.span)
                    }
                )?
            }

            ast::Expression::Array(ref arr) => {
                let arr = arr.iter().filter_map(|expr| self.fold(*expr)).collect();
                Expression::Array(arr)
            }

            ast::Expression::Primary(ast::Primary::PortFlowProbe(access, ref port)) => {
                self.test_allowed(AllowedOperation::BranchAccess, expression.span)?;
                let port = resolve_hierarchical!(self.base; port as Port(id) => id);
                let access = NatureAccess::resolve_from_ident(access, self.base)?;
                let port = port?;
                let discipline = self.base.hir[self.base.hir[port].node].discipline?;
                let branch = self
                    .branch_resolver
                    .as_mut()
                    .unwrap()
                    .resolve_port_access(self.base, access, discipline, port);
                Expression::BranchAccess(DisciplineAccess::Flow, branch)
            }

            ast::Expression::Primary(ast::Primary::FunctionCall(ident, ref parameters)) => {
                match self.fold_function(expression.span, ident, parameters)? {
                    FunctionFoldResult::BranchAccess((access, branch)) => {
                        Expression::BranchAccess(access, branch)
                    }
                    FunctionFoldResult::Function(fun, args, span) => {
                        Expression::FunctionCall(fun, args, span)
                    }
                }
            }

            ast::Expression::Error => unreachable!("encountered error node in hir lowerinfg"),
        };

        let res = self.base.hir.expressions.push(Spanned { contents: res, span: expression.span });
        Some(res)
    }
}
