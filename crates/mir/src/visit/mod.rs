/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::StringLiteral;
use crate::{
    ComparisonOperator, DisciplineAccess, IntegerBinaryOperator, RealBinaryOperator,
    RealExpression, Statement, StringExpression, UnaryOperator,
};
use crate::{ExpressionId, IntegerExpression, Mir};
use openvaf_ir::ids::{
    BranchId, IntegerExpressionId, NetId, ParameterId, PortId, RealExpressionId, StatementId,
    StringExpressionId, VariableId,
};
use openvaf_ir::{DoubleArgMath, NoiseSource, SingleArgMath, Spanned};
use openvaf_session::sourcemap::Span;

pub mod integer_expressions;
pub mod real_expressions;

pub fn walk_expr<V: ExpressionVisit>(visit: &mut V, expr: ExpressionId) {
    match expr {
        ExpressionId::Real(expr) => visit.visit_real_expr(expr),
        ExpressionId::Integer(expr) => visit.visit_integer_expr(expr),
        ExpressionId::String(expr) => visit.visit_string_expr(expr),
    }
}

pub fn walk_string_expression<V: ExpressionVisit>(visit: &mut V, expr: StringExpressionId) {
    let mir = visit.mir();
    match mir[expr].contents {
        StringExpression::Literal(val) => visit.visit_str_literal(val),
        StringExpression::VariableReference(var) => visit.visit_variable_reference(var),
        StringExpression::ParameterReference(param) => visit.visit_parameter_reference(param),
        StringExpression::Condition(cond, true_expr, false_expr) => {
            visit.visit_str_condition(cond, true_expr, false_expr)
        }
        StringExpression::SimParam(name) => visit.visit_string_expr(name),
    }
}

pub fn walk_integer_expression<V: ExpressionVisit>(visit: &mut V, expr: IntegerExpressionId) {
    match visit.mir()[expr].contents {
        IntegerExpression::Literal(val) => visit.visit_int_literal(val),
        IntegerExpression::BinaryOperator(lhs, op, rhs) => {
            visit.visit_int_binary_operator(lhs, op, rhs)
        }
        IntegerExpression::UnaryOperator(op, expr) => visit.visit_int_unary_op(op, expr),
        IntegerExpression::Condition(cond, true_expr, false_expr) => {
            visit.visit_int_condition(cond, true_expr, false_expr)
        }
        IntegerExpression::VariableReference(var) => visit.visit_variable_reference(var),
        IntegerExpression::ParameterReference(param) => visit.visit_parameter_reference(param),
        IntegerExpression::RealCast(expr) => visit.visit_real_cast(expr),
        IntegerExpression::IntegerComparison(lhs, op, rhs) => {
            visit.visit_integer_comparison(lhs, op, rhs)
        }
        IntegerExpression::RealComparison(lhs, op, rhs) => {
            visit.visit_real_comparison(lhs, op, rhs)
        }
        IntegerExpression::StringEq(lhs, rhs) => visit.visit_string_eq(lhs, rhs),
        IntegerExpression::StringNEq(lhs, rhs) => visit.visit_string_neq(lhs, rhs),
        IntegerExpression::NetReference(net) => visit.visit_net_reference(net),
        IntegerExpression::PortReference(port) => visit.visit_port_reference(port),
        IntegerExpression::Min(arg1, arg2) => visit.visit_int_min(arg1, arg2),
        IntegerExpression::Max(arg1, arg2) => visit.visit_int_max(arg1, arg2),
        IntegerExpression::Abs(arg) => visit.visit_int_abs(arg),
        IntegerExpression::ParamGiven(param) => visit.visit_param_given(param),
        IntegerExpression::PortConnected(port) => visit.visit_port_connected(port),
    }
}

pub fn walk_real_expression<V: ExpressionVisit>(visit: &mut V, expr: RealExpressionId) {
    let mir = visit.mir();
    match mir[expr].contents {
        RealExpression::Literal(val) => visit.visit_real_literal(val),
        RealExpression::BinaryOperator(lhs, op, rhs) => {
            visit.visit_real_binary_operator(lhs, op, rhs)
        }
        RealExpression::Negate(op, expr) => visit.visit_real_negate(op, expr),
        RealExpression::Condition(cond, true_expr, false_expr) => {
            visit.visit_real_condition(cond, true_expr, false_expr)
        }
        RealExpression::VariableReference(var) => visit.visit_variable_reference(var),
        RealExpression::ParameterReference(param) => visit.visit_parameter_reference(param),
        RealExpression::BranchAccess(discipline_access, branch, derivative_order) => {
            visit.visit_branch_access(discipline_access, branch, derivative_order)
        }
        RealExpression::PortFlowAccess(port, order) => visit.visit_port_flow_access(port, order),
        RealExpression::Noise(noise_src, name) => visit.visit_noise(noise_src, name),
        RealExpression::BuiltInFunctionCall1p(call, arg) => {
            visit.visit_builtin_function_call_1p(call, arg)
        }
        RealExpression::BuiltInFunctionCall2p(call, arg1, arg2) => {
            visit.visit_builtin_function_call_2p(call, arg1, arg2)
        }
        RealExpression::IntegerConversion(integer_expr) => {
            visit.visit_integer_conversion(integer_expr)
        }
        RealExpression::Temperature => visit.visit_temperature(),
        RealExpression::SimParam(name, default) => visit.visit_sim_param(name, default),
    }
}

pub trait ExpressionVisit: Sized {
    fn mir(&self) -> &Mir;

    // Entry methods

    fn visit_expr(&mut self, expr: ExpressionId) {
        walk_expr(self, expr)
    }

    fn visit_stmt(&mut self, stmt: StatementId) {
        match self.mir()[stmt].contents {
            Statement::Contribute(_, _, expr) => self.visit_real_expr(expr),
            Statement::Assignment(_, expr) => self.visit_expr(expr),
            Statement::StopTask(_, _) => (),
        }
    }

    #[inline]
    fn visit_string_expr(&mut self, expr: StringExpressionId) {
        walk_string_expression(self, expr)
    }

    #[inline]
    fn visit_integer_expr(&mut self, expr: IntegerExpressionId) {
        walk_integer_expression(self, expr)
    }

    #[inline]
    fn visit_real_expr(&mut self, expr: RealExpressionId) {
        walk_real_expression(self, expr)
    }

    // branches

    fn visit_int_binary_operator(
        &mut self,
        lhs: IntegerExpressionId,
        _op: Spanned<IntegerBinaryOperator>,
        rhs: IntegerExpressionId,
    ) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs)
    }

    fn visit_real_binary_operator(
        &mut self,
        lhs: RealExpressionId,
        _op: Spanned<RealBinaryOperator>,
        rhs: RealExpressionId,
    ) {
        self.visit_real_expr(lhs);
        self.visit_real_expr(rhs)
    }

    fn visit_real_negate(&mut self, _op: Span, arg: RealExpressionId) {
        self.visit_real_expr(arg)
    }

    fn visit_int_unary_op(&mut self, _op: Spanned<UnaryOperator>, arg: IntegerExpressionId) {
        self.visit_integer_expr(arg)
    }

    fn visit_integer_comparison(
        &mut self,
        lhs: IntegerExpressionId,
        _op: Spanned<ComparisonOperator>,
        rhs: IntegerExpressionId,
    ) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs)
    }

    fn visit_real_comparison(
        &mut self,
        lhs: RealExpressionId,
        _op: Spanned<ComparisonOperator>,
        rhs: RealExpressionId,
    ) {
        self.visit_real_expr(lhs);
        self.visit_real_expr(rhs)
    }

    fn visit_string_eq(&mut self, lhs: StringExpressionId, rhs: StringExpressionId) {
        self.visit_string_expr(lhs);
        self.visit_string_expr(rhs);
    }

    fn visit_string_neq(&mut self, lhs: StringExpressionId, rhs: StringExpressionId) {
        self.visit_string_expr(lhs);
        self.visit_string_expr(rhs);
    }

    fn visit_str_condition(
        &mut self,
        cond: IntegerExpressionId,
        true_expr: StringExpressionId,
        false_expr: StringExpressionId,
    ) {
        self.visit_integer_expr(cond);
        self.visit_string_expr(false_expr);
        self.visit_string_expr(true_expr)
    }

    fn visit_int_condition(
        &mut self,
        cond: IntegerExpressionId,
        true_expr: IntegerExpressionId,
        false_expr: IntegerExpressionId,
    ) {
        self.visit_integer_expr(cond);
        self.visit_integer_expr(true_expr);
        self.visit_integer_expr(false_expr);
    }

    fn visit_real_condition(
        &mut self,
        cond: IntegerExpressionId,
        true_expr: RealExpressionId,
        false_expr: RealExpressionId,
    ) {
        self.visit_integer_expr(cond);
        self.visit_real_expr(true_expr);
        self.visit_real_expr(false_expr);
    }

    fn visit_int_min(&mut self, arg1: IntegerExpressionId, arg2: IntegerExpressionId) {
        self.visit_integer_expr(arg1);
        self.visit_integer_expr(arg2);
    }

    fn visit_int_max(&mut self, arg1: IntegerExpressionId, arg2: IntegerExpressionId) {
        self.visit_integer_expr(arg1);
        self.visit_integer_expr(arg2);
    }

    fn visit_int_abs(&mut self, arg: IntegerExpressionId) {
        self.visit_integer_expr(arg);
    }

    fn visit_real_cast(&mut self, expr: RealExpressionId) {
        self.visit_real_expr(expr);
    }

    fn visit_str_sim_param(&mut self, name: StringExpressionId) {
        self.visit_string_expr(name)
    }

    fn visit_sim_param(&mut self, name: StringExpressionId, default: Option<RealExpressionId>) {
        self.visit_string_expr(name);
        if let Some(default) = default {
            self.visit_real_expr(default)
        }
    }

    fn visit_noise(
        &mut self,
        noise_src: NoiseSource<RealExpressionId, ()>,
        _name: Option<StringLiteral>,
    ) {
        match noise_src {
            NoiseSource::White(expr) => self.visit_real_expr(expr),
            NoiseSource::Flicker(expr1, expr2) => {
                self.visit_real_expr(expr1);
                self.visit_real_expr(expr2);
            }
            NoiseSource::Table(_) => {}
            NoiseSource::TableLog(_) => {}
        }
    }

    fn visit_builtin_function_call_1p(&mut self, _call: SingleArgMath, arg: RealExpressionId) {
        self.visit_real_expr(arg)
    }

    fn visit_builtin_function_call_2p(
        &mut self,
        _call: DoubleArgMath,
        arg1: RealExpressionId,
        arg2: RealExpressionId,
    ) {
        self.visit_real_expr(arg1);
        self.visit_real_expr(arg2);
    }

    fn visit_integer_conversion(&mut self, expr: IntegerExpressionId) {
        self.visit_integer_expr(expr)
    }

    // Leaves

    #[inline(always)]
    fn visit_real_literal(&mut self, _val: f64) {}

    #[inline(always)]
    fn visit_int_literal(&mut self, _val: i64) {}

    #[inline(always)]
    fn visit_str_literal(&mut self, _val: StringLiteral) {}

    #[inline(always)]
    fn visit_variable_reference(&mut self, _var: VariableId) {}

    #[inline(always)]
    fn visit_parameter_reference(&mut self, _param: ParameterId) {}

    #[inline(always)]
    fn visit_port_reference(&mut self, _port: PortId) {}

    #[inline(always)]
    fn visit_net_reference(&mut self, _net: NetId) {}

    #[inline(always)]
    fn visit_branch_access(
        &mut self,
        _discipline_accesss: DisciplineAccess,
        _branch: BranchId,
        _time_derivative_order: u8,
    ) {
    }

    #[inline(always)]
    fn visit_port_flow_access(&mut self, _port: PortId, _time_derivative_order: u8) {}

    #[inline(always)]
    fn visit_port_connected(&mut self, _port: PortId) {}

    #[inline(always)]
    fn visit_param_given(&mut self, _param: ParameterId) {}

    #[inline(always)]
    fn visit_temperature(&mut self) {}
}
