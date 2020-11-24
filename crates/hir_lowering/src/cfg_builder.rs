/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use openvaf_hir::{CaseItem, Cases, DisciplineAccess, Expression, ForLoop};
use openvaf_ir::ids::{BranchId, ExpressionId, FunctionId, VariableId};
use openvaf_ir::{DoubleArgMath, PrintOnFinish, SingleArgMath, Spanned};
use openvaf_middle::cfg::{
    BasicBlock, BasicBlockData, ControlFlowGraph, PhiData, Terminator, TerminatorKind,
};
use openvaf_middle::{
    BinOp, COperand, ComparisonOp, Local, LocalDeclaration, LocalKind, Operand, OperandData,
    RValue, StatementId, StmntKind, TyRValue, Type, VariableLocalKind,
};

use crate::error::Error::{
    ExpectedNumeric, ExpectedVariableForFunctionOutput, IllegalFinishNumber, MultiTypeMissmatch,
    Recursion, ReferenceTypeMissmatch, TypeMissmatch, WrongFunctionArgCount,
};
use crate::error::{ReferenceKind, TyPrinter};
use crate::{ExpressionLowering, HirFold, HirLowering};
use openvaf_data_structures::index_vec::{index_vec, IndexVec};
use tracing::{trace, trace_span};

use openvaf_data_structures::HashMap;
use openvaf_diagnostics::ListFormatter;
use openvaf_hir::{BinaryOperator, Primary, Statement, UnaryOperator};
use openvaf_session::sourcemap::Span;
use std::convert::TryFrom;
use std::mem::replace;

pub struct LocalCtx<'a, 'h, C: ExpressionLowering, L: HirLowering> {
    pub cfg: ControlFlowGraph<C>,
    pub fold: &'a mut HirFold<'h, L>,
    vars: IndexVec<VariableId, Option<Local>>,
    flows: IndexVec<BranchId, Option<Local>>,
    potentials: IndexVec<BranchId, Option<Local>>,
    pub current: BasicBlock,
    call_stack: Vec<(FunctionId, Span)>,
}

impl<'a, 'h, C: ExpressionLowering, L: HirLowering> LocalCtx<'a, 'h, C, L> {
    pub fn handle_stmnt_attributes(&mut self, stmt: StatementId) {
        let sctx = self.fold.hir[stmt].1;
        let span = trace_span!("attributes", src = debug(stmt), sctx = sctx.index());
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
        let mut res = Self {
            cfg: ControlFlowGraph::new(),
            fold,
            vars: IndexVec::new(),
            flows: IndexVec::new(),
            potentials: IndexVec::new(),
            current: BasicBlock::from_raw_unchecked(0),
            call_stack: Vec::new(),
        };
        res.enter_new_block();
        res
    }

    pub fn new_main(fold: &'a mut HirFold<'h, L>) -> Self {
        let mut res = Self {
            cfg: ControlFlowGraph::new(),
            vars: index_vec![None; fold.hir.variables.len()],
            flows: index_vec![None; fold.mir.branches.len()],
            potentials: index_vec![None; fold.mir.branches.len()],
            fold,
            current: BasicBlock::from_raw_unchecked(0),
            call_stack: Vec::with_capacity(32),
        };
        // CFGS should have an empty entry node to easily prepending basic blocks
        let entry = res.create_block();
        res.enter_new_block();
        res.terminate_bb(entry, TerminatorKind::Goto(res.current));
        res
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
                    let start = self.current;

                    self.enter_new_block();
                    let true_block_head = self.current;
                    self.lower_block(true_block);
                    let true_block_tail = self.current;

                    self.enter_new_block();
                    let false_block_head = self.current;
                    self.lower_block(false_block);
                    let false_block_tail = self.current;

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
                    self.terminate_bb(true_block_tail, TerminatorKind::Goto(self.current));
                    self.terminate_bb(false_block_tail, TerminatorKind::Goto(self.current));
                }
            }
            Statement::While(cond, ref body) => {
                let start = self.current;

                self.enter_new_block();
                let loop_cond_head = self.current;
                let cond = self.fold_bool_rhs(cond);
                let loop_cond_tail = self.current;

                self.enter_new_block();
                let loop_body_head = self.current;
                self.lower_block(body);
                let loop_body_tail = self.current;

                self.enter_new_block();

                if let Some(cond) = cond {
                    self.terminate_bb(
                        loop_cond_tail,
                        TerminatorKind::Split {
                            condition: cond.val,
                            true_block: loop_body_head,
                            false_block: self.current,
                            loop_head: true,
                        },
                    );
                }
                self.terminate_bb(start, TerminatorKind::Goto(loop_cond_head));
                self.terminate_bb(loop_body_tail, TerminatorKind::Goto(loop_cond_head));
            }
            Statement::For(ForLoop {
                cond,
                init,
                incr,
                ref body,
            }) => {
                self.lower_stmt(init);
                let start = self.current;

                self.enter_new_block();
                let loop_cond_head = self.current;
                let cond = self.fold_bool_rhs(cond);
                let loop_cond_tail = self.current;

                self.enter_new_block();
                let loop_body_head = self.current;
                self.lower_block(body);
                self.lower_stmt(incr);
                let loop_body_tail = self.current;

                self.enter_new_block();

                if let Some(cond) = cond {
                    self.terminate_bb(
                        loop_cond_tail,
                        TerminatorKind::Split {
                            condition: cond.val,
                            true_block: loop_body_head,
                            false_block: self.current,
                            loop_head: true,
                        },
                    );
                }
                self.terminate_bb(start, TerminatorKind::Goto(loop_cond_head));
                self.terminate_bb(loop_body_tail, TerminatorKind::Goto(loop_cond_head));
            }

            Statement::StopTask(kind, arg) => {
                let print_on_finish = arg
                    .and_then(|e| Some((self.fold.eval_int_constant(e)?, self.fold.hir[e].span)))
                    .and_then(|(x, span)| {
                        if let Ok(res) = PrintOnFinish::try_from(x) {
                            Some(res)
                        } else {
                            self.fold.errors.add(IllegalFinishNumber(x, span));
                            None
                        }
                    })
                    .unwrap_or(PrintOnFinish::Location);

                if let Some(stmnt) = C::stop_task(self, kind, print_on_finish, span) {
                    self.cfg.blocks[self.current]
                        .statements
                        .push((stmnt, self.fold.sctx));
                }
            }

            Statement::Contribute(access, branch, expr) => {
                if let Some(rhs) = self.fold_real(expr) {
                    let lhs = self.branch_local(branch, access);
                    let old = Operand::new(OperandData::Copy(lhs), span);
                    let rhs = RValue::BinaryOperation(Spanned::new(BinOp::Plus, span), old, rhs);
                    self.assign(lhs, rhs);
                }
            }

            Statement::Case(Cases {
                expr,
                ref cases,
                ref default,
            }) => {
                if let Some(cond) = self.lower_expr(expr) {
                    let mut start = self.current;

                    let end = self.create_block();

                    for CaseItem { values, body } in cases {
                        let body_head = self.current;

                        self.enter_new_block();
                        self.lower_block(body);
                        self.terminate(TerminatorKind::Goto(end));
                        self.current = start;

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
                                        false_block: self.current,
                                        loop_head: false,
                                    },
                                );

                                start = self.current;
                            }
                        }
                    }

                    self.lower_block(default);
                    self.terminate(TerminatorKind::Goto(end));
                    self.current = end
                }
            }
        }
        self.fold.sctx = old
    }

    pub fn lower_assignment_expr(&mut self, expr: ExpressionId, ty: Type) -> Option<TyRValue<C>> {
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

    pub fn lower_expr(&mut self, expr: ExpressionId) -> Option<TyRValue<C>> {
        let Spanned { span, ref contents } = self.fold.hir.expressions[expr];

        let val = match *contents {
            Expression::BinaryOperator(lhs_expr, op, rhs_expr) => match op.contents {
                BinaryOperator::Plus => {
                    let (lhs, rhs, ty) = self.fold_numeric_binop(lhs_expr, rhs_expr)?;

                    TyRValue {
                        val: RValue::BinaryOperation(op.copy_as(BinOp::Plus), lhs, rhs),
                        ty,
                    }
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
                        val: RValue::DoubleArgMath(op.copy_as(DoubleArgMath::Pow), lhs, rhs),
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
                    let start = self.current;
                    self.enter_new_block();
                    let false_block_head = self.current;

                    let rhs = self.fold_bool_rhs(rhs_expr);
                    let false_block_tail = self.current;
                    self.enter_new_block();
                    let true_val = self.assign_temporary(TyRValue {
                        val: RValue::Use(Operand::new(
                            OperandData::Constant(true.into()),
                            self.expr_span(lhs_expr),
                        )),
                        ty: Type::BOOL,
                    });
                    let true_block = self.current;
                    self.enter_new_block();

                    self.terminate_bb(true_block, TerminatorKind::Goto(self.current));
                    self.terminate_bb(false_block_tail, TerminatorKind::Goto(self.current));

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
                    self.cfg[self.current].phi_statements.push(PhiData {
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
                    let start = self.current;

                    self.enter_new_block();
                    let true_block_head = self.current;
                    let rhs = self.fold_bool_rhs(rhs_expr);
                    let true_block_tail = self.current;

                    self.enter_new_block();
                    let false_val = self.assign_temporary(TyRValue {
                        val: RValue::Use(Operand::new(
                            OperandData::Constant(false.into()),
                            self.expr_span(lhs_expr),
                        )),
                        ty: Type::BOOL,
                    });
                    let false_block = self.current;

                    self.enter_new_block();

                    self.terminate_bb(true_block_tail, TerminatorKind::Goto(self.current));
                    self.terminate_bb(false_block, TerminatorKind::Goto(self.current));
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
                    self.cfg[self.current].phi_statements.push(PhiData {
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
                    TyRValue {
                        val: RValue::BinaryOperation(op.copy_as(BinOp::NXor), lhs, rhs),
                        ty,
                    }
                }

                BinaryOperator::NXor => {
                    let (lhs, rhs, ty) = self.fold_bitwise_binop(lhs_expr, rhs_expr)?;
                    TyRValue {
                        val: RValue::BinaryOperation(op.copy_as(BinOp::NXor), lhs, rhs),
                        ty,
                    }
                }

                BinaryOperator::And => {
                    let (lhs, rhs, ty) = self.fold_bitwise_binop(lhs_expr, rhs_expr)?;
                    TyRValue {
                        val: RValue::BinaryOperation(op.copy_as(BinOp::And), lhs, rhs),
                        ty,
                    }
                }

                BinaryOperator::Or => {
                    let (lhs, rhs, ty) = self.fold_bitwise_binop(lhs_expr, rhs_expr)?;
                    TyRValue {
                        val: RValue::BinaryOperation(op.copy_as(BinOp::Or), lhs, rhs),
                        ty,
                    }
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
                        TyRValue {
                            val: RValue::UnaryOperation(op, arg),
                            ty,
                        }
                    }
                }

                UnaryOperator::LogicNegate => {
                    let arg = self.fold_bool(expr)?;
                    TyRValue {
                        val: RValue::UnaryOperation(op, arg),
                        ty: Type::BOOL,
                    }
                }

                UnaryOperator::ArithmeticNegate => {
                    let (arg, ty) = self.fold_numeric(expr)?;
                    TyRValue {
                        val: RValue::UnaryOperation(op, arg),
                        ty,
                    }
                }
            },

            Expression::Condition(cond, true_expr, false_expr) => {
                let cond = self.fold_bool(cond);

                let (true_val, false_val, ty) =
                    self.fold_compatible_bin_op(true_expr, false_expr, span)?;

                TyRValue {
                    val: RValue::Select(cond?, true_val, false_val),
                    ty,
                }
            }

            Expression::BuiltInFunctionCall2p(call, arg1, arg2) => {
                let (arg1, arg2, ty) = match call {
                    DoubleArgMath::Pow | DoubleArgMath::ArcTan2 | DoubleArgMath::Hypot => {
                        let (arg1, arg2) = self.fold_real_binop(arg1, arg2)?;
                        (arg1, arg2, Type::REAL)
                    }

                    DoubleArgMath::Min | DoubleArgMath::Max => {
                        self.fold_numeric_binop(arg1, arg2)?
                    }
                };

                TyRValue {
                    val: RValue::DoubleArgMath(Spanned::new(call, span), arg1, arg2),
                    ty,
                }
            }

            Expression::BuiltInFunctionCall1p(call, arg) => {
                let (arg, ty) = if call == SingleArgMath::Abs {
                    self.fold_numeric(arg)?
                } else {
                    (self.fold_real(arg)?, Type::REAL)
                };

                TyRValue {
                    val: RValue::SingleArgMath(Spanned::new(call, span), arg),
                    ty,
                }
            }

            Expression::PartialDerivative(expr, unkown) => {
                let arg = self.fold_numeric(expr)?.0;
                let derivative = self
                    .cfg
                    .demand_operand_derivative_unchecked(&self.fold.mir, &arg, unkown)
                    .into_operand();

                TyRValue {
                    val: RValue::Use(Spanned::new(derivative, arg.span)),
                    ty: Type::REAL,
                }
            }

            Expression::TimeDerivative(arg) => TyRValue {
                val: C::time_derivative(self, arg, span)?,
                ty: Type::REAL,
            },

            Expression::FunctionCall(id, ref args) => {
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

                if function.args.len() != args.len() {
                    self.fold.errors.add(WrongFunctionArgCount {
                        expected: self.fold.hir.functions[id].args.len(),
                        found: args.len(),
                        span,
                    });
                    return None;
                }

                let dst_block = self.create_block();

                for (val, arg_info) in args.iter().zip(&function.args) {
                    let span = self.expr_span(*val);
                    let (local, ty) = self.variable_local(arg_info.local_var);

                    if arg_info.output {
                        let var = if let Expression::Primary(Primary::VariableReference(var)) =
                            self.fold.hir[*val].contents
                        {
                            var
                        } else {
                            self.fold
                                .errors
                                .add(ExpectedVariableForFunctionOutput(span));
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
                        let start = replace(&mut self.current, dst_block);
                        self.assign(
                            lhs,
                            RValue::Use(Operand::new(OperandData::Copy(local), span)),
                        );
                        self.current = start;
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

                let tail = replace(&mut self.current, dst_block);
                self.terminate_bb(tail, TerminatorKind::Goto(dst_block));

                let (rvar, rty) = self.variable_local(function.return_variable);

                self.fold.sctx = old;

                let res = self.assign_temporary(TyRValue {
                    ty: rty,
                    val: RValue::Use(Operand::new(OperandData::Copy(rvar), span)),
                });

                TyRValue {
                    val: RValue::Use(Operand::new(OperandData::Copy(res), span)),
                    ty: rty,
                }
            }

            Expression::SystemFunctionCall(call) => TyRValue {
                val: C::system_function_call(self, call, span)?,
                ty: call.ty(),
            },

            Expression::Noise(source, name) => TyRValue {
                val: C::noise(self, source, name, span)?,
                ty: Type::REAL,
            },

            Expression::Primary(ref val) => match *val {
                Primary::Constant(ref val) => TyRValue {
                    val: RValue::Use(Operand::new(OperandData::Constant(val.clone()), span)),
                    ty: val.ty(),
                },

                Primary::VariableReference(var) => {
                    let (local, ty) = self.variable_local(var);

                    TyRValue {
                        val: RValue::Use(Operand::new(OperandData::Copy(local), span)),
                        ty,
                    }
                }

                Primary::NetReference(_net) => todo!("Digital"),
                Primary::PortReference(_port) => todo!("Digital"),

                Primary::ParameterReference(param) => TyRValue {
                    val: C::parameter_ref(self, param, span)?,
                    ty: self.fold.hir[param].ty,
                },
                Primary::PortFlowAccess(port) => TyRValue {
                    val: C::port_flow(self, port, span)?,
                    ty: Type::REAL,
                },

                Primary::BranchAccess(discipline, branch) => TyRValue {
                    val: C::branch_access(self, discipline, branch, span)?,
                    ty: Type::REAL,
                },
            },
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

                    TyRValue {
                        val: RValue::Array(elements, span),
                        ty,
                    }
                } else if rvalues
                    .iter()
                    .all(|(x, _)| x.ty == Type::REAL || x.ty == Type::INT)
                {
                    let elements: Vec<_> = rvalues
                        .into_iter()
                        .map(|(mut x, span)| {
                            if x.ty == Type::INT {
                                x = self.implicit_cast(x, Type::REAL, span)
                            }
                            self.rvalue_to_operand(x, span)
                        })
                        .collect();

                    TyRValue {
                        val: RValue::Array(elements, span),
                        ty: Type::REAL,
                    }
                } else {
                    self.fold.errors.add(MultiTypeMissmatch {
                        span,
                        types: ListFormatter(
                            rvalues.iter().map(|(x, _)| TyPrinter(x.ty)).collect(),
                            "'",
                            " and ",
                        ),
                        type_spans: rvalues.iter().map(|(_, span)| *span).collect(),
                    });
                    return None;
                }
            }
        };

        Some(val)
    }

    pub fn expr_span(&self, expr: ExpressionId) -> Span {
        self.fold.hir[expr].span
    }

    pub fn int_to_bool(&mut self, int_val: TyRValue<C>, span: Span) -> TyRValue<C> {
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

    pub fn implicit_cast(&mut self, src: TyRValue<C>, dst: Type, span: Span) -> TyRValue<C> {
        let src = self.assign_temporary(src);
        TyRValue {
            ty: dst,
            val: RValue::Cast(Operand::new(OperandData::Copy(src), span)),
        }
    }

    pub fn fold_numeric(&mut self, expr: ExpressionId) -> Option<(COperand<C>, Type)> {
        let res = self.lower_expr(expr)?;
        let res = match res.ty {
            Type::INT | Type::REAL => res,
            Type::BOOL => self.implicit_cast(res, Type::INT, self.expr_span(expr)),
            _ => {
                self.fold.errors.add(ExpectedNumeric {
                    span: self.fold.hir[expr].span,
                    found: res.ty.into(),
                });
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
    ) -> Option<(COperand<C>, COperand<C>, Type)> {
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
                self.fold.errors.add(ExpectedNumeric {
                    span: self.expr_span(rhs_expr),
                    found: found.into(),
                });
                return None;
            }

            (found, Type::INT) | (found, Type::REAL) | (found, Type::BOOL) => {
                self.fold.errors.add(ExpectedNumeric {
                    span: self.expr_span(lhs_expr),
                    found: found.into(),
                });
                return None;
            }

            (lhs_ty, rhs_ty) => {
                self.fold.errors.add(ExpectedNumeric {
                    span: self.expr_span(lhs_expr),
                    found: lhs_ty.into(),
                });
                self.fold.errors.add(ExpectedNumeric {
                    span: self.expr_span(rhs_expr),
                    found: rhs_ty.into(),
                });
                return None;
            }
        };

        let lhs = self.rvalue_to_operand(lhs, self.expr_span(lhs_expr));
        let rhs = self.rvalue_to_operand(rhs, self.expr_span(rhs_expr));

        Some((lhs, rhs, ty))
    }

    pub fn fold_real(&mut self, expr: ExpressionId) -> Option<COperand<C>> {
        let res = self.fold_real_rhs(expr)?;

        Some(self.rvalue_to_operand(res, self.expr_span(expr)))
    }

    pub fn fold_real_rhs(&mut self, expr: ExpressionId) -> Option<TyRValue<C>> {
        let res = self.lower_expr(expr)?;

        match res.ty {
            Type::INT | Type::BOOL => {
                Some(self.implicit_cast(res, Type::REAL, self.expr_span(expr)))
            }

            Type::REAL => Some(res),

            ty => {
                self.fold.errors.add(ExpectedNumeric {
                    span: self.expr_span(expr),
                    found: ty.into(),
                });
                None
            }
        }
    }

    pub fn fold_int(&mut self, expr: ExpressionId) -> Option<COperand<C>> {
        let res = self.fold_int_rhs(expr)?;

        Some(self.rvalue_to_operand(res, self.expr_span(expr)))
    }

    pub fn fold_int_rhs(&mut self, expr: ExpressionId) -> Option<TyRValue<C>> {
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
    ) -> Option<(COperand<C>, COperand<C>, Type)> {
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

    pub fn fold_bitwise(&mut self, expr: ExpressionId) -> Option<(COperand<C>, Type)> {
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

    pub fn fold_bool(&mut self, expr: ExpressionId) -> Option<COperand<C>> {
        let res = self.fold_bool_rhs(expr)?;

        Some(self.rvalue_to_operand(res, self.expr_span(expr)))
    }

    pub fn fold_bool_rhs(&mut self, expr: ExpressionId) -> Option<TyRValue<C>> {
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
    ) -> Option<(COperand<C>, COperand<C>)> {
        let lhs = self.fold_real(lhs_expr);
        let rhs = self.fold_real(rhs_expr);
        Some((lhs?, rhs?))
    }

    pub fn fold_int_binop(
        &mut self,

        lhs_expr: ExpressionId,
        rhs_expr: ExpressionId,
    ) -> Option<(COperand<C>, COperand<C>)> {
        let lhs = self.fold_int(lhs_expr);
        let rhs = self.fold_int(rhs_expr);

        Some((lhs?, rhs?))
    }

    pub fn fold_compatible_rhs(
        &mut self,

        lhs_expr: ExpressionId,
        rhs_expr: ExpressionId,
        span: Span,
    ) -> Option<(COperand<C>, COperand<C>, Type)> {
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
                    types: ListFormatter(vec![TyPrinter(x), TyPrinter(y)], "'", " and "),
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
    ) -> Option<(COperand<C>, COperand<C>, Type)> {
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
                    types: ListFormatter(vec![TyPrinter(x), TyPrinter(y)], "'", " and "),
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
        mut rhs: TyRValue<C>,
        rhs_span: Span,
        span: Span,
    ) -> Option<RValue<C>> {
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
                    types: ListFormatter(vec![TyPrinter(x), TyPrinter(y)], "'", " and "),
                    type_spans: vec![self.expr_span(lhs_expr), rhs_span],
                });
                return None;
            }
        };

        let lhs = self.rvalue_to_operand(lhs, self.expr_span(lhs_expr));
        let rhs = self.rvalue_to_operand(rhs, rhs_span);
        Some(RValue::Comparison(
            Spanned::new(ComparisonOp::Equal, span),
            lhs,
            rhs,
            ty,
        ))
    }

    fn new_temporary(&mut self, ty: Type) -> Local {
        self.cfg.new_temporary(ty)
    }

    fn variable_local(&mut self, var: VariableId) -> (Local, Type) {
        if let Some(res) = self.vars[var] {
            (res, self.cfg.locals[res].ty)
        } else {
            let ty = self.fold.hir[var].ty;
            let local = self.cfg.locals.push(LocalDeclaration {
                kind: LocalKind::Variable(var, VariableLocalKind::User),
                ty,
            });
            self.vars[var] = Some(local);
            (local, ty)
        }
    }

    fn branch_local(&mut self, branch: BranchId, access: DisciplineAccess) -> Local {
        let local_store = match access {
            DisciplineAccess::Flow => &mut self.flows,
            DisciplineAccess::Potential => &mut self.potentials,
        };

        if let Some(local) = local_store[branch] {
            local
        } else {
            let local = self.cfg.locals.push(LocalDeclaration {
                kind: LocalKind::Branch(access, branch),
                ty: Type::REAL,
            });

            local_store[branch] = Some(local);
            local
        }
    }

    pub fn rvalue_to_operand(&mut self, data: TyRValue<C>, span: Span) -> COperand<C> {
        self.rvalue_to_operand_in_bb(self.current, data, span)
    }

    pub fn rvalue_to_operand_in_bb(
        &mut self,
        bb: BasicBlock,
        data: TyRValue<C>,
        span: Span,
    ) -> COperand<C> {
        if let RValue::Use(op) = data.val {
            op
        } else {
            Spanned::new(
                OperandData::Copy(self.assign_temporary_in_bb(bb, data)),
                span,
            )
        }
    }

    pub fn assign_temporary(&mut self, data: TyRValue<C>) -> Local {
        self.assign_temporary_in_bb(self.current, data)
    }

    pub fn assign_temporary_in_bb(&mut self, bb: BasicBlock, data: TyRValue<C>) -> Local {
        let lhs = self.new_temporary(data.ty);
        self.assign_in_bb(bb, lhs, data.val);
        lhs
    }

    pub fn assign(&mut self, lhs: Local, rhs: RValue<C>) -> StatementId {
        self.cfg.blocks[self.current]
            .statements
            .push((StmntKind::Assignment(lhs, rhs), self.fold.sctx))
    }

    pub fn assign_in_bb(&mut self, bb: BasicBlock, lhs: Local, rhs: RValue<C>) -> StatementId {
        self.cfg.blocks[bb]
            .statements
            .push((StmntKind::Assignment(lhs, rhs), self.fold.sctx))
    }

    pub fn terminate_bb(&mut self, block: BasicBlock, kind: TerminatorKind<C>) {
        trace!(block = block.index(), kind = debug(&kind), "terminating");
        debug_assert!(
            self.cfg[block].terminator.is_none(),
            "terminate: block {:?}={:?} already has a terminator set",
            block,
            self.cfg[block]
        );

        self.cfg[block].terminator = Some(Terminator {
            sctx: self.fold.sctx,
            kind,
        });
    }

    pub fn terminate(&mut self, kind: TerminatorKind<C>) {
        self.terminate_bb(self.current, kind)
    }

    pub fn create_block(&mut self) -> BasicBlock {
        self.cfg.blocks.push(BasicBlockData {
            phi_statements: IndexVec::with_capacity(16),
            statements: IndexVec::with_capacity(128),
            terminator: None,
        })
    }

    pub fn enter_new_block(&mut self) {
        self.current = self.cfg.blocks.push(BasicBlockData {
            phi_statements: IndexVec::with_capacity(16),
            statements: IndexVec::with_capacity(128),
            terminator: None,
        })
    }
}
