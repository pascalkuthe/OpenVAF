/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::{
    BinaryOperator, CaseItem, Cases, ConstVal, DisciplineAccess, Expression, ForLoop, Statement,
    UnaryOperator,
};

use super::errors::Error::{
    ExpectedNumeric, ExpectedVariableForFunctionOutput, MultiTypeMissmatch, Recursion,
    ReferenceTypeMissmatch, TypeMissmatch,
};
use super::errors::{ReferenceKind, TyPrinter};
use super::{HirFold, HirLowering};

use ir::ids::{BranchId, CallArg, ExpressionId, FunctionId, VariableId};
use ir::{Math2, Spanned};
use middle::cfg::{
    builder::CfgBuilder, BasicBlock, ControlFlowGraph, PhiData, TerminatorKind,
};
use middle::{
    BinOp, ComparisonOp, Local, Operand, OperandData, RValue, SimpleConstVal, StatementId,
    StmntKind, TyRValue, Type,
};

use data_structures::HashMap;
use diagnostics::ListFormatter;

use session::sourcemap::{Span, StringLiteral};

use std::mem::replace;

use crate::functions::{FunctionLoweringReturn, HirFunction};
use crate::lowering::errors::Error::ExpectedValue;
use crate::lowering::lints::{UnusedReturnValue, UselessFunctionCall};
use data_structures::index_vec::{IndexSlice, IndexVec};
use data_structures::iter::zip;
use diagnostics::lints::Linter;
use middle::functions::NodeCollapse;
use middle::inputs::{CurrentProbe, ParameterInput, SimParam, SimParamKind, Voltage};
use tracing::trace_span;

pub struct LocalCtx<'a, 'h, L: HirLowering> {
    pub fold: &'a mut HirFold<'h, L>,
    call_stack: Vec<(FunctionId, Span)>,
    pub cfg_builder: CfgBuilder<ControlFlowGraph>,
}

impl<'a, 'h, L: HirLowering> LocalCtx<'a, 'h, L> {
    pub fn handle_stmnt_attributes(&mut self, stmt: StatementId) {
        let sctx = self.fold.hir[stmt].1;
        let span = trace_span!("stmnt attributes", src = debug(stmt), sctx = sctx.index());
        let _enter = span.enter();

        if !self.fold.lower_sctx.remove(sctx) {
            // DON'T process the same stuff twice
            return;
        }

        for attr in self.fold.hir[sctx].attributes.into_iter() {
            let attr = &self.fold.hir[attr];
            if !self.fold.handle_builtin_attribute(attr, sctx) {
                L::handle_statement_attribute(self, attr, stmt, sctx)
            }
        }
    }

    pub fn new_small(fold: &'a mut HirFold<'h, L>) -> Self {
        Self { cfg_builder: CfgBuilder::new_small(), fold, call_stack: Vec::new() }
    }

    pub fn new_fn(fold: &'a mut HirFold<'h, L>) -> Self {
        Self {
            cfg_builder: CfgBuilder::new_fn(fold.mir.variables.len(), fold.sctx),
            fold,
            call_stack: Vec::with_capacity(32),
        }
    }

    pub fn new_main(fold: &'a mut HirFold<'h, L>) -> Self {
        Self {
            cfg_builder: CfgBuilder::new_main(
                fold.mir.variables.len(),
                fold.mir.branches.len(),
                fold.sctx,
            ),
            fold,
            call_stack: Vec::with_capacity(32),
        }
    }

    pub fn lower_optional_simparam(
        &mut self,
        name: StringLiteral,
        default: ExpressionId,
        span: Span,
    ) -> Option<RValue> {
        let start = self.cfg_builder.current; // save the start block for later

        // lower the default expression
        self.enter_new_block();
        let default_block_head = self.cfg_builder.current;
        let default = self.fold_real_rhs(default)?;
        let default = self.assign_temporary(default);
        let default_block_tail = self.cfg_builder.current;

        // In another bb read the simpara
        self.enter_new_block();
        let read_block = self.cfg_builder.current;
        let read_simpara = Operand::new(
            OperandData::Read(SimParam { name, kind: SimParamKind::RealOptional }.into()),
            span,
        );
        let val =
            self.assign_temporary(TyRValue { val: RValue::Use(read_simpara), ty: Type::REAL });

        self.enter_new_block();

        self.terminate_bb(
            start,
            TerminatorKind::Split {
                condition: RValue::Use(Operand::new(
                    OperandData::Read(
                        SimParam { name, kind: SimParamKind::RealOptionalGiven }.into(),
                    ),
                    span,
                )),
                true_block: read_block,
                false_block: default_block_head,
                loop_head: false,
            },
        );

        self.terminate_bb(read_block, TerminatorKind::Goto(self.cfg_builder.current));
        self.terminate_bb(default_block_tail, TerminatorKind::Goto(self.cfg_builder.current));

        let dst = self.new_temporary(Type::REAL);

        let mut sources = HashMap::with_capacity(2);
        sources.insert(default_block_tail, default);
        sources.insert(read_block, val);

        self.cfg_builder.cfg[self.cfg_builder.current].phi_statements.push(PhiData {
            dst,
            sources,
            sctx: self.fold.sctx,
        });

        RValue::Use(Operand::new(OperandData::Copy(dst), span)).into()
    }

    pub fn lower_block(&mut self, block: &[StatementId]) {
        for stmt in block {
            self.lower_stmt(*stmt)
        }
    }

    pub fn lower_stmt(&mut self, stmnt: StatementId) {
        let tspan = trace_span!("statement", stmnt = stmnt.index(),);
        let _enter = tspan.enter();
        self.handle_stmnt_attributes(stmnt);

        let old = replace(&mut self.fold.sctx, self.fold.hir[stmnt].1);
        let span = self.fold.hir[self.fold.sctx].span;

        match self.fold.hir[stmnt].0 {
            Statement::Assignment(var, val) => {
                let (lhs, ty) = self.variable_local(var);

                if let Some(rhs) = self.lower_assignment_expr(val, ty) {
                    self.assign(lhs, rhs.val);
                }
            }

            Statement::Condition(cond, ref true_block, ref false_block) => {
                let cond = self.fold_bool_rhs(cond);
                if let Some(cond) = cond {
                    let start = self.cfg_builder.current;

                    self.enter_new_block();
                    let true_block_head = self.cfg_builder.current;
                    self.lower_block(true_block);
                    let true_block_tail = self.cfg_builder.current;

                    self.enter_new_block();
                    let false_block_head = self.cfg_builder.current;
                    self.lower_block(false_block);
                    let false_block_tail = self.cfg_builder.current;

                    self.enter_new_block();

                    self.terminate_bb(
                        start,
                        TerminatorKind::Split {
                            condition: cond.val,
                            true_block: true_block_head,
                            false_block: false_block_head,
                            loop_head: false,
                        },
                    );
                    self.terminate_bb(
                        true_block_tail,
                        TerminatorKind::Goto(self.cfg_builder.current),
                    );
                    self.terminate_bb(
                        false_block_tail,
                        TerminatorKind::Goto(self.cfg_builder.current),
                    );
                }
            }
            Statement::While(cond, ref body) => {
                let start = self.cfg_builder.current;

                self.enter_new_block();
                let loop_cond_head = self.cfg_builder.current;
                let cond = self.fold_bool_rhs(cond);
                let loop_cond_tail = self.cfg_builder.current;

                self.enter_new_block();
                let loop_body_head = self.cfg_builder.current;
                self.lower_block(body);
                let loop_body_tail = self.cfg_builder.current;

                self.enter_new_block();

                if let Some(cond) = cond {
                    self.terminate_bb(
                        loop_cond_tail,
                        TerminatorKind::Split {
                            condition: cond.val,
                            true_block: loop_body_head,
                            false_block: self.cfg_builder.current,
                            loop_head: true,
                        },
                    );
                }
                self.terminate_bb(start, TerminatorKind::Goto(loop_cond_head));
                self.terminate_bb(loop_body_tail, TerminatorKind::Goto(loop_cond_head));
            }
            Statement::For(ForLoop { cond, init, incr, ref body }) => {
                self.lower_stmt(init);
                let start = self.cfg_builder.current;

                self.enter_new_block();
                let loop_cond_head = self.cfg_builder.current;
                let cond = self.fold_bool_rhs(cond);
                let loop_cond_tail = self.cfg_builder.current;

                self.enter_new_block();
                let loop_body_head = self.cfg_builder.current;
                self.lower_block(body);
                self.lower_stmt(incr);
                let loop_body_tail = self.cfg_builder.current;

                self.enter_new_block();

                if let Some(cond) = cond {
                    self.terminate_bb(
                        loop_cond_tail,
                        TerminatorKind::Split {
                            condition: cond.val,
                            true_block: loop_body_head,
                            false_block: self.cfg_builder.current,
                            loop_head: true,
                        },
                    );
                }
                self.terminate_bb(start, TerminatorKind::Goto(loop_cond_head));
                self.terminate_bb(loop_body_tail, TerminatorKind::Goto(loop_cond_head));
            }

            // Statement::StopTask(kind, arg) => {
            //     let print_on_finish = arg
            //         .and_then(|e| Some((self.fold.eval_int_constant(e)?, self.fold.hir[e].span)))
            //         .and_then(|(x, span)| {
            //             if let Ok(res) = PrintOnFinish::try_from(x) {
            //                 Some(res)
            //             } else {
            //                 self.fold.errors.add(IllegalFinishNumber(x, span));
            //                 None
            //             }
            //         })
            //         .unwrap_or(PrintOnFinish::Location);
            //
            //     if let Some(stmnt) = C::stop_task(self, kind, print_on_finish, span) {
            //         self.cfg_builder.cfg.blocks[self.cfg_builder.current]
            //             .statements
            //             .push((stmnt, self.fold.sctx));
            //     }
            // }
            Statement::Contribute(access, branch, expr) => {
                if let Some(rhs) = self.fold_real(expr) {
                    if rhs.contents
                        == OperandData::Constant(ConstVal::Scalar(SimpleConstVal::Real(0.0)))
                        && access == DisciplineAccess::Potential
                    {
                        // Node Collapse hint

                        let branch = &self.fold.mir.branches[branch];
                        let hi = branch.hi;
                        let lo = branch.lo;
                        self.cfg_builder.cfg.blocks[self.cfg_builder.current].statements.push((
                            StmntKind::Call(NodeCollapse(hi, lo).into(), IndexVec::new(), span),
                            self.fold.sctx,
                        ));
                    } else {
                        let lhs = self.branch_local(branch, access);
                        let old = Operand::new(OperandData::Copy(lhs), span);
                        let rhs =
                            RValue::BinaryOperation(Spanned::new(BinOp::Plus, span), old, rhs);
                        self.assign(lhs, rhs);
                    }
                }
            }

            Statement::Case(Cases { expr, ref cases, ref default }) => {
                if let Some(cond) = self.lower_expr(expr) {
                    let mut start = self.cfg_builder.current;

                    let end = self.create_block();

                    for CaseItem { values, body } in cases {
                        self.enter_new_block();
                        let body_head = self.cfg_builder.current;
                        self.lower_block(body);
                        self.terminate(TerminatorKind::Goto(end));
                        self.cfg_builder.current = start;

                        for value in values {
                            if let Some(cond) =
                                self.fold_eq_rhs(*value, cond.clone(), self.expr_span(expr), span)
                            {
                                self.enter_new_block();
                                self.terminate_bb(
                                    start,
                                    TerminatorKind::Split {
                                        condition: cond,
                                        true_block: body_head,
                                        false_block: self.cfg_builder.current,
                                        loop_head: false,
                                    },
                                );

                                start = self.cfg_builder.current;
                            }
                        }
                    }

                    self.lower_block(default);
                    self.terminate(TerminatorKind::Goto(end));
                    self.cfg_builder.current = end
                }
            }
            Statement::FunctionCall(function, ref args, span) => {
                function.check_arg_length(args, &mut self.fold, span);

                if function.returns_value(self.fold.hir) {
                    Linter::dispatch_late(
                        Box::new(UnusedReturnValue {
                            span,
                            name: function.symbol(self.fold.hir),
                            decl: function.decl(self.fold.hir),
                        }),
                        self.fold.sctx,
                    )
                }

                if !function.has_side_effects(self.fold.hir) {
                    Linter::dispatch_late(
                        Box::new(UselessFunctionCall {
                            span,
                            name: function.symbol(self.fold.hir),
                            decl: function.decl(self.fold.hir),
                        }),
                        self.fold.sctx,
                    )
                }

                if let Some(lowering_result) = function.lower(self, args, span) {
                    if let FunctionLoweringReturn::Val(TyRValue {
                        val: RValue::Call(fun, args, span),
                        ..
                    })
                    | FunctionLoweringReturn::Void(fun, args, span) = lowering_result
                    {
                        self.cfg_builder.cfg.blocks[self.cfg_builder.current]
                            .statements
                            .push((StmntKind::Call(fun, args, span), self.fold.sctx));
                    }
                }
            }
        }
        self.fold.sctx = old
    }

    pub fn lower_assignment_expr(&mut self, expr: ExpressionId, ty: Type) -> Option<TyRValue> {
        let span = self.expr_span(expr);
        let mut expr = self.lower_expr(expr)?;
        match (expr.ty, ty) {
            (x, y) if x == y => (),
            (Type::INT, Type::REAL) | (Type::REAL, Type::INT) => {
                expr = self.implicit_cast(expr, ty, span)
            }

            (found, expected_type) => self.fold.errors.add(TypeMissmatch {
                span,
                expected_type: expected_type.into(),
                found: found.into(),
            }),
        }
        Some(expr)
    }

    pub fn lower_user_function_call(
        &mut self,
        id: FunctionId,
        args: &IndexSlice<CallArg, [ExpressionId]>,
        span: Span,
    ) -> Option<TyRValue> {
        let function = &self.fold.hir.functions[id];

        if self.call_stack.iter().any(|(x, _)| x == &id) {
            let hir = self.fold.hir;
            self.fold.errors.add(Recursion {
                function_name: function.ident.name,
                recursion_span: span,
                recursion_traceback: self
                    .call_stack
                    .iter()
                    .map(|(id, span)| (hir[*id].ident.name, *span))
                    .collect(),
            });
            return None;
        }

        let dst_block = self.create_block();

        for (val, arg_info) in zip(args, &function.args) {
            let span = self.expr_span(*val);
            let (local, ty) = self.variable_local(arg_info.local_var);

            if arg_info.output {
                let var = if let Expression::VariableReference(var) = self.fold.hir[*val].contents {
                    var
                } else {
                    self.fold.errors.add(ExpectedVariableForFunctionOutput {
                        span,
                        decl: self.fold.hir[arg_info.local_var].ident,
                        fun: self.fold.hir[id].ident,
                    });
                    continue;
                };

                if self.fold.hir[var].ty != ty {
                    self.fold.errors.add(ReferenceTypeMissmatch {
                        expected_type: ty.into(),
                        found_type: self.fold.hir[var].ty.into(),
                        name: self.fold.hir[var].ident,
                        ref_span: span,
                        ref_kind: ReferenceKind::Variable,
                        decl_span: self.fold.hir[self.fold.hir[var].sctx].span,
                    });
                    continue;
                }

                let (lhs, _) = self.variable_local(var);
                let start = replace(&mut self.cfg_builder.current, dst_block);
                self.assign(lhs, RValue::Use(Operand::new(OperandData::Copy(local), span)));
                self.cfg_builder.current = start;
            }

            if arg_info.input {
                if let Some(rhs) = self.lower_assignment_expr(*val, ty) {
                    self.assign(local, rhs.val);
                }
            }

            debug_assert!(arg_info.input | arg_info.output)
        }

        let old = replace(&mut self.fold.sctx, self.fold.hir[id].sctx);

        self.call_stack.push((id, span));

        self.lower_block(&function.body);

        self.call_stack.pop();

        let tail = replace(&mut self.cfg_builder.current, dst_block);
        self.terminate_bb(tail, TerminatorKind::Goto(dst_block));

        let (rvar, rty) = self.variable_local(function.return_variable);

        self.fold.sctx = old;

        let res = self.assign_temporary(TyRValue {
            ty: rty,
            val: RValue::Use(Operand::new(OperandData::Copy(rvar), span)),
        });

        Some(TyRValue { val: RValue::Use(Operand::new(OperandData::Copy(res), span)), ty: rty })
    }

    pub fn lower_expr(&mut self, expr: ExpressionId) -> Option<TyRValue> {
        let Spanned { span, ref contents } = self.fold.hir.expressions[expr];

        let val = match *contents {
            Expression::BinaryOperator(lhs_expr, op, rhs_expr) => match op.contents {
                BinaryOperator::Plus => {
                    let (lhs, rhs, ty) = self.fold_numeric_binop(lhs_expr, rhs_expr)?;

                    TyRValue { val: RValue::BinaryOperation(op.copy_as(BinOp::Plus), lhs, rhs), ty }
                }

                BinaryOperator::Minus => {
                    let (lhs, rhs, ty) = self.fold_numeric_binop(lhs_expr, rhs_expr)?;

                    TyRValue {
                        val: RValue::BinaryOperation(op.copy_as(BinOp::Minus), lhs, rhs),
                        ty,
                    }
                }

                BinaryOperator::Multiply => {
                    let (lhs, rhs, ty) = self.fold_numeric_binop(lhs_expr, rhs_expr)?;
                    TyRValue {
                        val: RValue::BinaryOperation(op.copy_as(BinOp::Multiply), lhs, rhs),
                        ty,
                    }
                }

                BinaryOperator::Divide => {
                    let (lhs, rhs, ty) = self.fold_numeric_binop(lhs_expr, rhs_expr)?;
                    TyRValue {
                        val: RValue::BinaryOperation(op.copy_as(BinOp::Divide), lhs, rhs),
                        ty,
                    }
                }

                BinaryOperator::Modulus => {
                    let (lhs, rhs, ty) = self.fold_numeric_binop(lhs_expr, rhs_expr)?;
                    TyRValue {
                        val: RValue::BinaryOperation(op.copy_as(BinOp::Modulus), lhs, rhs),
                        ty,
                    }
                }

                BinaryOperator::Exponent => {
                    let (lhs, rhs) = self.fold_real_binop(lhs_expr, rhs_expr)?;
                    TyRValue {
                        val: RValue::Math2(op.copy_as(Math2::Pow), lhs, rhs),
                        ty: Type::REAL,
                    }
                }

                BinaryOperator::ShiftLeft => {
                    let (lhs, rhs) = self.fold_int_binop(lhs_expr, rhs_expr)?;
                    TyRValue {
                        val: RValue::BinaryOperation(op.copy_as(BinOp::ShiftLeft), lhs, rhs),
                        ty: Type::INT,
                    }
                }

                BinaryOperator::ShiftRight => {
                    let (lhs, rhs) = self.fold_int_binop(lhs_expr, rhs_expr)?;
                    TyRValue {
                        val: RValue::BinaryOperation(op.copy_as(BinOp::ShiftRight), lhs, rhs),
                        ty: Type::INT,
                    }
                }

                BinaryOperator::LessThen => {
                    let (lhs, rhs, ty) = self.fold_numeric_binop(lhs_expr, rhs_expr)?;
                    TyRValue {
                        val: RValue::Comparison(op.copy_as(ComparisonOp::LessThen), lhs, rhs, ty),
                        ty: Type::BOOL,
                    }
                }

                BinaryOperator::LessEqual => {
                    let (lhs, rhs, ty) = self.fold_numeric_binop(lhs_expr, rhs_expr)?;
                    TyRValue {
                        val: RValue::Comparison(op.copy_as(ComparisonOp::LessEqual), lhs, rhs, ty),
                        ty: Type::BOOL,
                    }
                }

                BinaryOperator::GreaterThen => {
                    let (lhs, rhs, ty) = self.fold_numeric_binop(lhs_expr, rhs_expr)?;
                    TyRValue {
                        val: RValue::Comparison(
                            op.copy_as(ComparisonOp::GreaterThen),
                            lhs,
                            rhs,
                            ty,
                        ),
                        ty: Type::BOOL,
                    }
                }

                BinaryOperator::GreaterEqual => {
                    let (lhs, rhs, ty) = self.fold_numeric_binop(lhs_expr, rhs_expr)?;
                    TyRValue {
                        val: RValue::Comparison(
                            op.copy_as(ComparisonOp::GreaterEqual),
                            lhs,
                            rhs,
                            ty,
                        ),
                        ty: Type::BOOL,
                    }
                }

                BinaryOperator::LogicEqual => {
                    let (lhs, rhs, ty) = self.fold_compatible_bin_op(lhs_expr, rhs_expr, span)?;
                    TyRValue {
                        val: RValue::Comparison(op.copy_as(ComparisonOp::Equal), lhs, rhs, ty),
                        ty: Type::BOOL,
                    }
                }

                BinaryOperator::LogicalNotEqual => {
                    let (lhs, rhs, ty) = self.fold_compatible_bin_op(lhs_expr, rhs_expr, span)?;
                    TyRValue {
                        val: RValue::Comparison(op.copy_as(ComparisonOp::NotEqual), lhs, rhs, ty),
                        ty: Type::BOOL,
                    }
                }

                BinaryOperator::LogicOr => {
                    let lhs = self.fold_bool_rhs(lhs_expr);
                    let start = self.cfg_builder.current;
                    self.enter_new_block();
                    let false_block_head = self.cfg_builder.current;

                    let rhs = self.fold_bool_rhs(rhs_expr);
                    let false_block_tail = self.cfg_builder.current;
                    self.enter_new_block();
                    let true_val = self.assign_temporary(TyRValue {
                        val: RValue::Use(Operand::new(
                            OperandData::Constant(true.into()),
                            self.expr_span(lhs_expr),
                        )),
                        ty: Type::BOOL,
                    });
                    let true_block = self.cfg_builder.current;
                    self.enter_new_block();

                    self.terminate_bb(true_block, TerminatorKind::Goto(self.cfg_builder.current));
                    self.terminate_bb(
                        false_block_tail,
                        TerminatorKind::Goto(self.cfg_builder.current),
                    );

                    self.terminate_bb(
                        start,
                        TerminatorKind::Split {
                            condition: lhs?.val,
                            true_block,
                            false_block: false_block_head,
                            loop_head: false,
                        },
                    );

                    let false_val = self.assign_temporary_in_bb(false_block_tail, rhs?);
                    let dst = self.new_temporary(Type::BOOL);
                    let mut sources = HashMap::with_capacity(2);
                    sources.insert(true_block, true_val);
                    sources.insert(false_block_tail, false_val);
                    self.cfg_builder.cfg[self.cfg_builder.current].phi_statements.push(PhiData {
                        dst,
                        sources,
                        sctx: self.fold.sctx,
                    });

                    TyRValue {
                        val: RValue::Use(Operand::new(OperandData::Copy(dst), span)),
                        ty: Type::BOOL,
                    }
                }

                BinaryOperator::LogicAnd => {
                    let lhs = self.fold_bool_rhs(lhs_expr);
                    let start = self.cfg_builder.current;

                    self.enter_new_block();
                    let true_block_head = self.cfg_builder.current;
                    let rhs = self.fold_bool_rhs(rhs_expr);
                    let true_block_tail = self.cfg_builder.current;

                    self.enter_new_block();
                    let false_val = self.assign_temporary(TyRValue {
                        val: RValue::Use(Operand::new(
                            OperandData::Constant(false.into()),
                            self.expr_span(lhs_expr),
                        )),
                        ty: Type::BOOL,
                    });
                    let false_block = self.cfg_builder.current;

                    self.enter_new_block();

                    self.terminate_bb(
                        true_block_tail,
                        TerminatorKind::Goto(self.cfg_builder.current),
                    );
                    self.terminate_bb(false_block, TerminatorKind::Goto(self.cfg_builder.current));
                    self.terminate_bb(
                        start,
                        TerminatorKind::Split {
                            condition: lhs?.val,
                            true_block: true_block_head,
                            false_block,
                            loop_head: false,
                        },
                    );

                    let true_val = self.assign_temporary_in_bb(true_block_tail, rhs?);

                    let mut sources = HashMap::with_capacity(2);
                    sources.insert(true_block_tail, true_val);
                    sources.insert(false_block, false_val);

                    let dst = self.new_temporary(Type::BOOL);
                    self.cfg_builder.cfg[self.cfg_builder.current].phi_statements.push(PhiData {
                        dst,
                        sources,
                        sctx: self.fold.sctx,
                    });

                    TyRValue {
                        val: RValue::Use(Operand::new(OperandData::Copy(dst), span)),
                        ty: Type::BOOL,
                    }
                }

                BinaryOperator::Xor => {
                    let (lhs, rhs, ty) = self.fold_bitwise_binop(lhs_expr, rhs_expr)?;
                    TyRValue { val: RValue::BinaryOperation(op.copy_as(BinOp::NXor), lhs, rhs), ty }
                }

                BinaryOperator::NXor => {
                    let (lhs, rhs, ty) = self.fold_bitwise_binop(lhs_expr, rhs_expr)?;
                    TyRValue { val: RValue::BinaryOperation(op.copy_as(BinOp::NXor), lhs, rhs), ty }
                }

                BinaryOperator::And => {
                    let (lhs, rhs, ty) = self.fold_bitwise_binop(lhs_expr, rhs_expr)?;
                    TyRValue { val: RValue::BinaryOperation(op.copy_as(BinOp::And), lhs, rhs), ty }
                }

                BinaryOperator::Or => {
                    let (lhs, rhs, ty) = self.fold_bitwise_binop(lhs_expr, rhs_expr)?;
                    TyRValue { val: RValue::BinaryOperation(op.copy_as(BinOp::Or), lhs, rhs), ty }
                }
            },

            Expression::UnaryOperator(op, expr) => match op.contents {
                UnaryOperator::BitNegate => {
                    let (arg, ty) = self.fold_bitwise(expr)?;
                    if ty == Type::BOOL {
                        // saves me from introducing an extract operation that is completely equivalent
                        TyRValue {
                            val: RValue::UnaryOperation(
                                op.copy_as(UnaryOperator::LogicNegate),
                                arg,
                            ),
                            ty,
                        }
                    } else {
                        TyRValue { val: RValue::UnaryOperation(op, arg), ty }
                    }
                }

                UnaryOperator::LogicNegate => {
                    let arg = self.fold_bool(expr)?;
                    TyRValue { val: RValue::UnaryOperation(op, arg), ty: Type::BOOL }
                }

                UnaryOperator::ArithmeticNegate => {
                    let (arg, ty) = self.fold_numeric(expr)?;
                    TyRValue { val: RValue::UnaryOperation(op, arg), ty }
                }
            },

            Expression::Condition(cond, true_expr, false_expr) => {
                let cond = self.fold_bool(cond);

                let (true_val, false_val, ty) =
                    self.fold_compatible_bin_op(true_expr, false_expr, span)?;

                TyRValue { val: RValue::Select(cond?, true_val, false_val), ty }
            }

            Expression::PartialDerivative(expr, unkown) => {
                let arg = self.fold_numeric(expr)?.0;
                let derivative = self
                    .cfg_builder
                    .cfg
                    .demand_operand_derivative_unchecked(&self.fold.mir, &arg, unkown)
                    .into_operand();

                TyRValue { val: RValue::Use(Spanned::new(derivative, arg.span)), ty: Type::REAL }
            }

            Expression::Constant(ref val) => TyRValue {
                val: RValue::Use(Operand::new(OperandData::Constant(val.clone()), span)),
                ty: val.ty(),
            },

            Expression::VariableReference(var) => {
                let (local, ty) = self.variable_local(var);

                TyRValue { val: Operand::new(OperandData::Copy(local), span).into(), ty }
            }

            Expression::NodeReference(_)
            | Expression::PortReference(_)
            | Expression::NatureReference(_) => {
                todo!("Error")
            }
            Expression::ParameterReference(param) => {
                let input = ParameterInput::Value(param);
                TyRValue {
                    val: Operand::new(OperandData::Read(input.into()), span).into(),
                    ty: self.fold.hir[param].ty,
                }
            }

            Expression::BranchAccess(acccess, branch) => {
                let input = match acccess {
                    DisciplineAccess::Potential => {
                        Voltage { hi: self.fold.hir[branch].hi, lo: self.fold.hir[branch].lo }
                            .into()
                    }
                    DisciplineAccess::Flow => CurrentProbe(branch).into(),
                };

                TyRValue {
                    val: Operand::new(OperandData::Read(input), span).into(),
                    ty: Type::REAL,
                }
            }

            Expression::Array(ref arr) => {
                let rvalues: Vec<_> = arr
                    .iter()
                    .filter_map(|&e| Some((self.lower_expr(e)?, self.expr_span(e))))
                    .collect();

                let ty = if let Some((val, _)) = rvalues.first() {
                    val.ty
                } else if rvalues.len() != arr.len() {
                    return None;
                } else {
                    unreachable!("Empty arrays are unspupported");
                };

                if rvalues.iter().all(|x| x.0.ty == ty) {
                    let elements: Vec<_> = rvalues
                        .into_iter()
                        .map(|(x, span)| self.rvalue_to_operand(x, span))
                        .collect();

                    TyRValue { val: RValue::Array(elements, span), ty }
                } else if rvalues.iter().all(|(x, _)| x.ty == Type::REAL || x.ty == Type::INT) {
                    let elements: Vec<_> = rvalues
                        .into_iter()
                        .map(|(mut x, span)| {
                            if x.ty == Type::INT {
                                x = self.implicit_cast(x, Type::REAL, span)
                            }
                            self.rvalue_to_operand(x, span)
                        })
                        .collect();

                    TyRValue { val: RValue::Array(elements, span), ty: Type::REAL }
                } else {
                    self.fold.errors.add(MultiTypeMissmatch {
                        span,
                        types: ListFormatter::with_final_seperator(
                            rvalues.iter().map(|(x, _)| TyPrinter(x.ty)).collect(),
                            " and ",
                        ),
                        type_spans: rvalues.iter().map(|(_, span)| *span).collect(),
                    });
                    return None;
                }
            }
            Expression::FunctionCall(function, ref args, span) => {
                function.check_arg_length(args, &mut self.fold, span);

                match function.lower(self, args, span) {
                    Some(FunctionLoweringReturn::Val(res)) => res,
                    Some(FunctionLoweringReturn::Void(_, _, _)) => {
                        self.fold
                            .errors
                            .add(ExpectedValue { span, function: function.symbol(self.fold.hir) });
                        return None;
                    }
                    None => {
                        if !function.returns_value(self.fold.hir) {
                            self.fold.errors.add(ExpectedValue {
                                span,
                                function: function.symbol(self.fold.hir),
                            });
                        }
                        return None;
                    }
                }
            }
        };

        Some(val)
    }

    pub fn expr_span(&self, expr: ExpressionId) -> Span {
        self.fold.hir[expr].span
    }

    pub fn int_to_bool(&mut self, int_val: TyRValue, span: Span) -> TyRValue {
        let int_val = self.assign_temporary(int_val);
        TyRValue {
            val: RValue::Comparison(
                Spanned::new(ComparisonOp::NotEqual, span),
                Operand::new(OperandData::Copy(int_val), span),
                Operand::new(OperandData::Constant(0.into()), span),
                Type::INT,
            ),
            ty: Type::BOOL,
        }
    }

    pub fn implicit_cast(&mut self, src: TyRValue, dst: Type, span: Span) -> TyRValue {
        // Most common case that can be performed without introducing additional local (important for node collapse)
        if let RValue::Use(Spanned {
            contents: OperandData::Constant(ConstVal::Scalar(SimpleConstVal::Integer(val))),
            ..
        }) = src.val
        {
            if dst == Type::REAL {
                let val = OperandData::Constant(ConstVal::Scalar(SimpleConstVal::Real(val as f64)));
                return TyRValue { ty: dst, val: RValue::Use(Operand::new(val, span)) };
            }
        }

        let src = self.assign_temporary(src);
        TyRValue { ty: dst, val: RValue::Cast(Operand::new(OperandData::Copy(src), span)) }
    }

    pub fn fold_numeric(&mut self, expr: ExpressionId) -> Option<(Operand, Type)> {
        let res = self.lower_expr(expr)?;
        let res = match res.ty {
            Type::INT | Type::REAL => res,
            Type::BOOL => self.implicit_cast(res, Type::INT, self.expr_span(expr)),
            _ => {
                self.fold
                    .errors
                    .add(ExpectedNumeric { span: self.fold.hir[expr].span, found: res.ty.into() });
                return None;
            }
        };

        let ty = res.ty;
        Some((self.rvalue_to_operand(res, self.fold.hir[expr].span), ty))
    }

    pub fn fold_numeric_binop(
        &mut self,
        lhs_expr: ExpressionId,
        rhs_expr: ExpressionId,
    ) -> Option<(Operand, Operand, Type)> {
        let mut lhs = self.lower_expr(lhs_expr)?;
        let mut rhs = self.lower_expr(rhs_expr)?;

        let ty = match (lhs.ty, rhs.ty) {
            (Type::REAL, Type::REAL) => Type::REAL,
            (Type::INT, Type::INT) => Type::INT,

            (Type::BOOL, Type::BOOL) => {
                rhs = self.implicit_cast(rhs, Type::INT, self.expr_span(rhs_expr));
                lhs = self.implicit_cast(lhs, Type::INT, self.expr_span(lhs_expr));
                Type::INT
            }

            (Type::INT, Type::BOOL) => {
                rhs = self.implicit_cast(rhs, Type::INT, self.expr_span(rhs_expr));
                Type::INT
            }

            (Type::BOOL, Type::INT) => {
                lhs = self.implicit_cast(lhs, Type::INT, self.expr_span(lhs_expr));
                Type::INT
            }

            (Type::REAL, Type::INT) | (Type::REAL, Type::BOOL) => {
                rhs = self.implicit_cast(rhs, Type::REAL, self.expr_span(rhs_expr));
                Type::REAL
            }

            (Type::INT, Type::REAL) | (Type::BOOL, Type::REAL) => {
                lhs = self.implicit_cast(lhs, Type::REAL, self.expr_span(lhs_expr));
                Type::REAL
            }

            (Type::INT, found) | (Type::REAL, found) | (Type::BOOL, found) => {
                self.fold
                    .errors
                    .add(ExpectedNumeric { span: self.expr_span(rhs_expr), found: found.into() });
                return None;
            }

            (found, Type::INT) | (found, Type::REAL) | (found, Type::BOOL) => {
                self.fold
                    .errors
                    .add(ExpectedNumeric { span: self.expr_span(lhs_expr), found: found.into() });
                return None;
            }

            (lhs_ty, rhs_ty) => {
                self.fold
                    .errors
                    .add(ExpectedNumeric { span: self.expr_span(lhs_expr), found: lhs_ty.into() });
                self.fold
                    .errors
                    .add(ExpectedNumeric { span: self.expr_span(rhs_expr), found: rhs_ty.into() });
                return None;
            }
        };

        let lhs = self.rvalue_to_operand(lhs, self.expr_span(lhs_expr));
        let rhs = self.rvalue_to_operand(rhs, self.expr_span(rhs_expr));

        Some((lhs, rhs, ty))
    }

    pub fn fold_real(&mut self, expr: ExpressionId) -> Option<Operand> {
        let res = self.fold_real_rhs(expr)?;

        Some(self.rvalue_to_operand(res, self.expr_span(expr)))
    }

    pub fn fold_real_rhs(&mut self, expr: ExpressionId) -> Option<TyRValue> {
        let res = self.lower_expr(expr)?;

        match res.ty {
            Type::INT | Type::BOOL => {
                Some(self.implicit_cast(res, Type::REAL, self.expr_span(expr)))
            }

            Type::REAL => Some(res),

            ty => {
                self.fold
                    .errors
                    .add(ExpectedNumeric { span: self.expr_span(expr), found: ty.into() });
                None
            }
        }
    }

    pub fn fold_int(&mut self, expr: ExpressionId) -> Option<Operand> {
        let res = self.fold_int_rhs(expr)?;

        Some(self.rvalue_to_operand(res, self.expr_span(expr)))
    }

    pub fn fold_int_rhs(&mut self, expr: ExpressionId) -> Option<TyRValue> {
        let res = self.lower_expr(expr)?;

        let res = match res.ty {
            Type::INT => res,
            Type::BOOL => self.implicit_cast(res, Type::INT, self.expr_span(expr)),
            _ => {
                self.fold.errors.add(TypeMissmatch {
                    span: self.fold.hir[expr].span,
                    expected_type: TyPrinter(Type::INT),
                    found: res.ty.into(),
                });
                return None;
            }
        };

        Some(res)
    }

    pub fn fold_bitwise_binop(
        &mut self,
        lhs_expr: ExpressionId,
        rhs_expr: ExpressionId,
    ) -> Option<(Operand, Operand, Type)> {
        let lhs = self.lower_expr(lhs_expr);
        let mut rhs = self.lower_expr(rhs_expr)?;
        let mut lhs = lhs?;

        let ty = match (lhs.ty, rhs.ty) {
            (Type::INT, Type::INT) => Type::INT,
            (Type::BOOL, Type::BOOL) => Type::BOOL,
            (Type::BOOL, Type::INT) => {
                lhs = self.implicit_cast(lhs, Type::INT, self.expr_span(lhs_expr));
                Type::INT
            }
            (Type::INT, Type::BOOL) => {
                rhs = self.implicit_cast(rhs, Type::INT, self.expr_span(rhs_expr));
                Type::INT
            }

            (lhs_type, rhs_type) => {
                if !matches!(lhs_type, Type::INT | Type::BOOL) {
                    self.fold.errors.add(TypeMissmatch {
                        span: self.fold.hir[lhs_expr].span,
                        expected_type: TyPrinter(Type::INT),
                        found: lhs_type.into(),
                    });
                }

                if !matches!(rhs_type, Type::INT | Type::BOOL) {
                    self.fold.errors.add(TypeMissmatch {
                        span: self.fold.hir[rhs_expr].span,
                        expected_type: TyPrinter(Type::INT),
                        found: rhs_type.into(),
                    });
                }

                return None;
            }
        };

        let lhs = self.rvalue_to_operand(lhs, self.expr_span(lhs_expr));
        let rhs = self.rvalue_to_operand(rhs, self.expr_span(rhs_expr));

        Some((lhs, rhs, ty))
    }

    pub fn fold_bitwise(&mut self, expr: ExpressionId) -> Option<(Operand, Type)> {
        let res = self.lower_expr(expr)?;

        if !matches!(res.ty, Type::INT | Type::BOOL) {
            self.fold.errors.add(TypeMissmatch {
                span: self.fold.hir[expr].span,
                expected_type: TyPrinter(Type::INT),
                found: res.ty.into(),
            });
            return None;
        }

        let ty = res.ty;
        Some((self.rvalue_to_operand(res, self.expr_span(expr)), ty))
    }

    pub fn fold_bool(&mut self, expr: ExpressionId) -> Option<Operand> {
        let res = self.fold_bool_rhs(expr)?;

        Some(self.rvalue_to_operand(res, self.expr_span(expr)))
    }

    pub fn fold_bool_rhs(&mut self, expr: ExpressionId) -> Option<TyRValue> {
        let res = self.lower_expr(expr)?;

        let res = match res.ty {
            Type::BOOL => res,
            Type::INT => self.int_to_bool(res, self.expr_span(expr)),
            ty => {
                self.fold.errors.add(TypeMissmatch {
                    span: self.fold.hir[expr].span,
                    expected_type: TyPrinter(Type::INT),
                    found: ty.into(),
                });
                return None;
            }
        };

        Some(res)
    }

    pub fn fold_real_binop(
        &mut self,
        lhs_expr: ExpressionId,
        rhs_expr: ExpressionId,
    ) -> Option<(Operand, Operand)> {
        let lhs = self.fold_real(lhs_expr);
        let rhs = self.fold_real(rhs_expr);
        Some((lhs?, rhs?))
    }

    pub fn fold_int_binop(
        &mut self,
        lhs_expr: ExpressionId,
        rhs_expr: ExpressionId,
    ) -> Option<(Operand, Operand)> {
        let lhs = self.fold_int(lhs_expr);
        let rhs = self.fold_int(rhs_expr);

        Some((lhs?, rhs?))
    }

    pub fn fold_compatible_rhs(
        &mut self,
        lhs_expr: ExpressionId,
        rhs_expr: ExpressionId,
        span: Span,
    ) -> Option<(Operand, Operand, Type)> {
        let lhs = self.lower_expr(lhs_expr);
        let rhs = self.lower_expr(rhs_expr);
        let mut lhs = lhs?;
        let mut rhs = rhs?;

        let ty = match (lhs.ty, rhs.ty) {
            (x, y) if x == y => x,

            (Type::REAL, Type::INT) => {
                lhs = self.implicit_cast(lhs, Type::REAL, span);
                Type::REAL
            }

            (Type::INT, Type::REAL) => {
                rhs = self.implicit_cast(rhs, Type::REAL, span);
                Type::REAL
            }

            (x, y) => {
                self.fold.errors.add(MultiTypeMissmatch {
                    span,
                    types: ListFormatter::with_final_seperator(
                        vec![TyPrinter(x), TyPrinter(y)],
                        " and ",
                    ),
                    type_spans: vec![self.expr_span(lhs_expr), self.expr_span(rhs_expr)],
                });
                return None;
            }
        };

        let lhs = self.rvalue_to_operand(lhs, self.expr_span(lhs_expr));
        let rhs = self.rvalue_to_operand(rhs, self.expr_span(rhs_expr));
        Some((lhs, rhs, ty))
    }

    pub fn fold_compatible_bin_op(
        &mut self,
        lhs_expr: ExpressionId,
        rhs_expr: ExpressionId,
        span: Span,
    ) -> Option<(Operand, Operand, Type)> {
        let lhs = self.lower_expr(lhs_expr);
        let rhs = self.lower_expr(rhs_expr);
        let mut lhs = lhs?;
        let mut rhs = rhs?;

        let ty = match (lhs.ty, rhs.ty) {
            (x, y) if x == y => x,

            (Type::REAL, Type::INT) | (Type::REAL, Type::BOOL) => {
                rhs = self.implicit_cast(rhs, Type::REAL, span);
                Type::REAL
            }

            (Type::INT, Type::REAL) | (Type::BOOL, Type::REAL) => {
                lhs = self.implicit_cast(lhs, Type::REAL, span);
                Type::REAL
            }

            (Type::INT, Type::BOOL) => {
                rhs = self.implicit_cast(rhs, Type::INT, span);
                Type::INT
            }

            (Type::BOOL, Type::INT) => {
                lhs = self.implicit_cast(lhs, Type::INT, span);
                Type::INT
            }

            (x, y) => {
                self.fold.errors.add(MultiTypeMissmatch {
                    span,
                    types: ListFormatter::with_final_seperator(
                        vec![TyPrinter(x), TyPrinter(y)],
                        " and ",
                    ),
                    type_spans: vec![self.expr_span(lhs_expr), self.expr_span(rhs_expr)],
                });
                return None;
            }
        };

        let lhs = self.rvalue_to_operand(lhs, self.expr_span(lhs_expr));
        let rhs = self.rvalue_to_operand(rhs, self.expr_span(rhs_expr));
        Some((lhs, rhs, ty))
    }

    fn fold_eq_rhs(
        &mut self,
        lhs_expr: ExpressionId,
        mut rhs: TyRValue,
        rhs_span: Span,
        span: Span,
    ) -> Option<RValue> {
        let lhs = self.lower_expr(lhs_expr);
        let mut lhs = lhs?;

        let ty = match (lhs.ty, rhs.ty) {
            (x, y) if x == y => x,

            (Type::REAL, Type::INT) => {
                rhs = self.implicit_cast(rhs, Type::REAL, span);
                Type::REAL
            }

            (Type::INT, Type::REAL) => {
                lhs = self.implicit_cast(lhs, Type::REAL, span);
                Type::REAL
            }

            (x, y) => {
                self.fold.errors.add(MultiTypeMissmatch {
                    span,
                    types: ListFormatter::with_final_seperator(
                        vec![TyPrinter(x), TyPrinter(y)],
                        " and ",
                    ),
                    type_spans: vec![self.expr_span(lhs_expr), rhs_span],
                });
                return None;
            }
        };

        let lhs = self.rvalue_to_operand(lhs, self.expr_span(lhs_expr));
        let rhs = self.rvalue_to_operand(rhs, rhs_span);
        Some(RValue::Comparison(Spanned::new(ComparisonOp::Equal, span), lhs, rhs, ty))
    }

    fn new_temporary(&mut self, ty: Type) -> Local {
        self.cfg_builder.new_temporary(ty)
    }

    fn variable_local(&mut self, var: VariableId) -> (Local, Type) {
        self.cfg_builder.variable_local(var, &self.fold.mir)
    }

    fn branch_local(&mut self, branch: BranchId, access: DisciplineAccess) -> Local {
        self.cfg_builder.branch_local(branch, access)
    }

    pub fn rvalue_to_operand(&mut self, data: TyRValue, span: Span) -> Operand {
        self.cfg_builder.rvalue_to_operand(data, span, self.fold.sctx)
    }

    pub fn rvalue_to_operand_in_bb(
        &mut self,
        bb: BasicBlock,
        data: TyRValue,
        span: Span,
    ) -> Operand {
        self.cfg_builder.rvalue_to_operand_in_bb(bb, data, span, self.fold.sctx)
    }

    pub fn assign_temporary(&mut self, data: TyRValue) -> Local {
        self.cfg_builder.assign_temporary(data, self.fold.sctx)
    }

    pub fn assign_temporary_in_bb(&mut self, bb: BasicBlock, data: TyRValue) -> Local {
        self.cfg_builder.assign_temporary_in_bb(bb, data, self.fold.sctx)
    }

    pub fn assign(&mut self, lhs: Local, rhs: RValue) -> StatementId {
        self.cfg_builder.assign(lhs, rhs, self.fold.sctx)
    }

    pub fn assign_in_bb(&mut self, bb: BasicBlock, lhs: Local, rhs: RValue) -> StatementId {
        self.cfg_builder.assign_in_bb(bb, lhs, rhs, self.fold.sctx)
    }

    pub fn terminate_bb(&mut self, block: BasicBlock, kind: TerminatorKind) {
        self.cfg_builder.terminate_bb(block, kind, self.fold.sctx)
    }

    pub fn terminate(&mut self, kind: TerminatorKind) {
        self.cfg_builder.terminate(kind, self.fold.sctx)
    }

    pub fn create_block(&mut self) -> BasicBlock {
        self.cfg_builder.create_block()
    }

    pub fn enter_new_block(&mut self) {
        self.cfg_builder.enter_new_block()
    }
}
