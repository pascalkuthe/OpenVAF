//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use openvaf_ir::ids::{
    BranchId, IntegerExpressionId, NetId, ParameterId, PortId, RealExpressionId,
    StringExpressionId, VariableId,
};
use openvaf_ir::SingleArgMath::{Cos, CosH, Ln, Sin, SinH, Sqrt};
use openvaf_ir::{DoubleArgMath, NoiseSource, SingleArgMath, Spanned};
use openvaf_mir::{
    ComparisonOperator, DisciplineAccess, ExpressionId, IntegerBinaryOperator, IntegerExpression,
    Mir, RealBinaryOperator, RealExpression, UnaryOperator,
};

use openvaf_mir::fold::integer_expressions::{
    walk_integer_expression, IntegerBinaryOperatorFold, IntegerExprFold,
};
use openvaf_mir::fold::real_expressions::{
    walk_real_expression, RealBinaryOperatorFold, RealBuiltInFunctionCall1pFold,
    RealBuiltInFunctionCall2pFold, RealExprFold,
};
use openvaf_mir::RealBinaryOperator::{Divide, Multiply, Subtract, Sum};
use openvaf_mir::RealExpression::{BranchAccess, IntegerConversion};

use crate::error::Error::{DerivativeNotDefined, OnlyNumericExpressionsCanBeDerived};
use crate::error::UndefinedDerivative;
use crate::lints::RoundingDerivativeNotFullyDefined;
use crate::{AutoDiff, Unknown};
use openvaf_diagnostics::lints::Linter;
use openvaf_session::sourcemap::span::DUMMY_SP;
use openvaf_session::sourcemap::{Span, StringLiteral};

pub struct ExpressionAutoDiff<'lt, 'mir: 'lt, E> {
    current_expr: E,
    ad: &'lt mut AutoDiff<'mir>,
    unknown: Unknown,
}

/// Inside this module Nonde reprents a derivative that evaluates to 0
type Derivative = Option<RealExpressionId>;

impl<'lt, 'mir: 'lt, E: Into<ExpressionId> + Copy> ExpressionAutoDiff<'lt, 'mir, E> {
    fn add_to_mir(&mut self, expr: RealExpression) -> RealExpressionId {
        let node = Spanned::new(expr, self.current_expr.into().span(self.ad.mir));
        self.ad.mir.real_expressions.push(node)
    }

    fn gen_constant(&mut self, val: f64) -> RealExpressionId {
        let expr = RealExpression::Literal(val);
        self.add_to_mir(expr)
    }

    fn gen_int_constant(&mut self, val: i64) -> IntegerExpressionId {
        let expr = IntegerExpression::Literal(val);
        let node = Spanned::new(expr, self.current_expr.into().span(self.ad.mir));
        self.ad.mir.integer_expressions.push(node)
    }

    fn gen_neg(&mut self, expr: RealExpressionId) -> RealExpressionId {
        let span = self.current_expr.into().span(self.ad.mir);
        let expr = RealExpression::Negate(span, expr);
        self.add_to_mir(expr)
    }

    fn gen_binary_op(
        &mut self,
        lhs: RealExpressionId,
        op: RealBinaryOperator,
        rhs: RealExpressionId,
    ) -> RealExpressionId {
        let span = self.current_expr.into().span(self.ad.mir);
        let expr = RealExpression::BinaryOperator(lhs, Spanned::new(op, span), rhs);
        self.add_to_mir(expr)
    }

    fn gen_math_function(
        &mut self,
        call: SingleArgMath,
        arg: RealExpressionId,
    ) -> RealExpressionId {
        let expr = RealExpression::BuiltInFunctionCall1p(call, arg);
        self.add_to_mir(expr)
    }

    fn gen_one_plus_minus_squared(
        &mut self,
        minus: bool,
        arg: RealExpressionId,
    ) -> RealExpressionId {
        let one = self.gen_constant(1.0);
        let sqare = self.gen_binary_op(arg, Multiply, arg);
        let op = if minus { Subtract } else { Sum };
        self.gen_binary_op(one, op, sqare)
    }

    /// # Returns
    ///
    ///  * `None` — if `arg1` and `arg2` are `None`
    /// * One argument and 0 — if one is `None` but the other isn't
    /// * the expressions of arg1 and arg2 — if both are something
    ///
    /// # Note
    ///
    /// The order of arg1 and arg2 is preserved in the return argument
    ///
    fn convert_to_paired(
        &mut self,
        arg1: Derivative,
        arg2: Derivative,
    ) -> Option<(RealExpressionId, RealExpressionId)> {
        let (arg1, arg2) = match (arg1, arg2) {
            (Some(arg1), Some(arg2)) => (arg1, arg2),
            (Some(arg1), None) => (arg1, self.gen_constant(0.0)),
            (None, Some(arg2)) => (self.gen_constant(0.0), arg2),
            (None, None) => return None,
        };
        Some((arg1, arg2))
    }

    fn derivative_sum(&mut self, dlhs: Derivative, drhs: Derivative) -> Derivative {
        match (dlhs, drhs) {
            (Some(dlhs), Some(drhs)) => Some(self.gen_binary_op(dlhs, Sum, drhs)),
            (Some(res), None) | (None, Some(res)) => Some(res),
            (None, None) => None,
        }
    }

    fn mul_derivative(
        &mut self,
        lhs: RealExpressionId,
        dlhs: Derivative,
        rhs: RealExpressionId,
        drhs: Derivative,
    ) -> Derivative {
        // u = a'*b
        let factor1 = if let Some(dlhs) = dlhs {
            Some(self.gen_binary_op(dlhs, Multiply, rhs))
        } else {
            None
        };

        // v = a*b'
        let factor2 = if let Some(drhs) = drhs {
            Some(self.gen_binary_op(lhs, Multiply, drhs))
        } else {
            None
        };

        // u+v
        self.derivative_sum(factor1, factor2)
    }

    fn quotient_derivative(
        &mut self,
        lhs: RealExpressionId,
        dlhs: Derivative,
        rhs: RealExpressionId,
        drhs: Derivative,
    ) -> Derivative {
        let drhs = drhs.map(|drhs| self.gen_neg(drhs));

        // num = u'*v+v'*u (u=lhs, v=rhs, derivatives are calculated above)
        let num = self.mul_derivative(lhs, dlhs, rhs, drhs)?;

        // den = g*g
        let den = self.gen_binary_op(rhs, Multiply, rhs);

        // (f/g)' = num/den = (f'*g+(-g')*f)/g*g
        Some(self.gen_binary_op(num, Divide, den))
    }

    fn pow_derivative(
        &mut self,
        lhs: RealExpressionId,
        dlhs: Derivative,
        rhs: RealExpressionId,
        drhs: Derivative,
        original: RealExpressionId,
    ) -> Derivative {
        // rhs/lhs * lhs'
        let sum1 = if let Some(dlhs) = dlhs {
            let quotient = self.gen_binary_op(rhs, Divide, lhs);
            Some(self.gen_binary_op(quotient, Multiply, dlhs))
        } else {
            None
        };

        //ln (lhs) * rhs'
        let sum2 = if let Some(drhs) = drhs {
            let ln = self.gen_math_function(Ln, lhs);
            Some(self.gen_binary_op(ln, Multiply, drhs))
        } else {
            None
        };

        // f'/f*g + ln(f)*g'
        let sum = self.derivative_sum(sum1, sum2)?;

        // (f**g)' = sum* f**g = (f'/f*g + ln(f)*g')* f**g
        Some(self.gen_binary_op(sum, Multiply, original))
    }

    fn undefined_derivative(&mut self, undefined: UndefinedDerivative) {
        let span = self.current_expr.into().span(self.ad.mir);
        self.ad.errors.add(DerivativeNotDefined(undefined, span))
    }

    fn param_derivative(&mut self, param: ParameterId) -> Derivative {
        if Unknown::Parameter(param) == self.unknown {
            Some(self.gen_constant(1.0))
        } else {
            None
        }
    }
}

impl<'lt, 'mir: 'lt> ExpressionAutoDiff<'lt, 'mir, RealExpressionId> {
    /// Generates arg1 < arg2
    fn gen_lt_condition(
        &mut self,
        arg1: RealExpressionId,
        arg2: RealExpressionId,
    ) -> IntegerExpressionId {
        let span = self.ad.mir[self.current_expr].span;
        let condition = IntegerExpression::RealComparison(
            arg1,
            Spanned::new(ComparisonOperator::LessThen, span),
            arg2,
        );
        let condition = Spanned::new(condition, span);
        self.ad.mir.integer_expressions.push(condition)
    }
    pub fn run(&mut self) -> RealExpressionId {
        self.fold_real_expr(self.current_expr)
            .unwrap_or_else(|| self.gen_constant(0.0))
    }
}

impl<'lt, 'mir: 'lt> ExpressionAutoDiff<'lt, 'mir, IntegerExpressionId> {
    /// Generates arg1 < arg2
    fn gen_lt_condition(
        &mut self,
        arg1: IntegerExpressionId,
        arg2: IntegerExpressionId,
    ) -> IntegerExpressionId {
        let span = self.ad.mir[self.current_expr].span;
        let condition = IntegerExpression::IntegerComparison(
            arg1,
            Spanned::new(ComparisonOperator::LessThen, span),
            arg2,
        );
        let condition = Spanned::new(condition, span);
        self.ad.mir.integer_expressions.push(condition)
    }

    pub fn run(&mut self) -> RealExpressionId {
        self.fold_integer_expr(self.current_expr)
            .unwrap_or_else(|| self.gen_constant(0.0))
    }
}

impl<'lt, 'mir: 'lt> RealExprFold for ExpressionAutoDiff<'lt, 'mir, RealExpressionId> {
    type T = Derivative;

    #[inline]
    fn fold_real_expr(&mut self, expr: RealExpressionId) -> Derivative {
        let old = self.current_expr;
        self.current_expr = expr;
        let res = walk_real_expression(self, expr);
        self.current_expr = old;
        res
    }

    fn mir(&self) -> &Mir {
        self.ad.mir
    }

    fn fold_literal(&mut self, _val: f64) -> Derivative {
        None
    }

    fn fold_binary_operator(
        &mut self,
        lhs: RealExpressionId,
        op: Spanned<RealBinaryOperator>,
        rhs: RealExpressionId,
    ) -> Derivative {
        self.fold_real_binary_op(lhs, op.contents, rhs)
    }

    fn fold_builtin_function_call_1p(
        &mut self,
        call: SingleArgMath,
        arg: RealExpressionId,
    ) -> Derivative {
        RealBuiltInFunctionCall1pFold::fold_real_builtin_function_call_1p(self, call, arg)
    }

    fn fold_builtin_function_call_2p(
        &mut self,
        call: DoubleArgMath,
        arg1: RealExpressionId,
        arg2: RealExpressionId,
    ) -> Derivative {
        RealBuiltInFunctionCall2pFold::fold_real_builtin_function_call_2p(self, call, arg1, arg2)
    }

    fn fold_negate(&mut self, op: Span, arg: RealExpressionId) -> Derivative {
        let arg = self.fold_real_expr(arg)?;
        Some(self.add_to_mir(RealExpression::Negate(op, arg)))
    }

    fn fold_condition(
        &mut self,
        cond: IntegerExpressionId,
        true_expr: RealExpressionId,
        false_expr: RealExpressionId,
    ) -> Derivative {
        let true_expr = self.fold_real_expr(true_expr);
        let false_expr = self.fold_real_expr(false_expr);
        let (true_expr, false_expr) = self.convert_to_paired(true_expr, false_expr)?;
        let expr = RealExpression::Condition(cond, true_expr, false_expr);
        Some(self.add_to_mir(expr))
    }

    fn fold_variable_reference(&mut self, var: VariableId) -> Derivative {
        let var = self.ad.mir.derivative_var(var, self.unknown);
        Some(self.add_to_mir(RealExpression::VariableReference(var)))
    }

    fn fold_parameter_reference(&mut self, param: ParameterId) -> Derivative {
        self.param_derivative(param)
    }

    fn fold_branch_access(
        &mut self,
        discipline_accesss: DisciplineAccess,
        branch: BranchId,
        time_derivative_order: u8,
    ) -> Derivative {
        match self.unknown {
            Unknown::Time => Some(self.add_to_mir(BranchAccess(
                discipline_accesss,
                branch,
                time_derivative_order + 1,
            ))),
            Unknown::NodePotential(net) if self.mir()[branch].contents.hi == net => {
                Some(self.gen_constant(1.0))
            }
            Unknown::NodePotential(net) if self.mir()[branch].contents.lo == net => {
                Some(self.gen_constant(-1.0))
            }
            Unknown::Flow(unknown) if unknown == branch => Some(self.gen_constant(1.0)),
            _ => None,
        }
    }

    fn fold_port_flow_access(&mut self, _port: PortId, _time_derivative_order: u8) -> Derivative {
        None
    }

    fn fold_noise(
        &mut self,
        _noise_src: NoiseSource<RealExpressionId, ()>,
        _name: Option<StringLiteral>,
    ) -> Derivative {
        // TODO Warn
        None
    }

    fn fold_temperature(&mut self) -> Derivative {
        if self.unknown == Unknown::Temperature {
            Some(self.gen_constant(1.0))
        } else {
            None
        }
    }

    fn fold_sim_param(
        &mut self,
        _name: StringExpressionId,
        _default: Option<RealExpressionId>,
    ) -> Derivative {
        None
    }

    fn fold_integer_conversion(&mut self, expr: IntegerExpressionId) -> Derivative {
        ExpressionAutoDiff {
            current_expr: expr,
            unknown: self.unknown,
            ad: self.ad,
        }
        .fold_integer_expr(expr)
    }
}

impl<'lt, 'mir: 'lt> RealBinaryOperatorFold for ExpressionAutoDiff<'lt, 'mir, RealExpressionId> {
    type T = Derivative;

    fn fold_sum(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Derivative {
        let dlhs = self.fold_real_expr(lhs);
        let drhs = self.fold_real_expr(rhs);
        self.derivative_sum(dlhs, drhs)
    }

    fn fold_diff(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Derivative {
        let dlhs = self.fold_real_expr(lhs);
        let drhs = self.fold_real_expr(rhs);
        let drhs = drhs.map(|drhs| self.gen_neg(drhs));
        self.derivative_sum(dlhs, drhs)
    }

    fn fold_mul(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Derivative {
        let dlhs = self.fold_real_expr(lhs);
        let drhs = self.fold_real_expr(rhs);
        self.mul_derivative(lhs, dlhs, rhs, drhs)
    }

    fn fold_quotient(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Derivative {
        let dlhs = self.fold_real_expr(lhs);
        let drhs = self.fold_real_expr(rhs);

        self.quotient_derivative(lhs, dlhs, rhs, drhs)
    }

    fn fold_pow(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Derivative {
        let dlhs = self.fold_real_expr(lhs);
        let drhs = self.fold_real_expr(rhs);

        self.pow_derivative(lhs, dlhs, rhs, drhs, self.current_expr)
    }

    fn fold_mod(&mut self, _lhs: RealExpressionId, _rhs: RealExpressionId) -> Derivative {
        self.undefined_derivative(UndefinedDerivative::Modulus);
        None
    }
}

impl<'lt, 'mir: 'lt> RealBuiltInFunctionCall2pFold
    for ExpressionAutoDiff<'lt, 'mir, RealExpressionId>
{
    type T = Derivative;

    fn fold_pow(&mut self, arg1: RealExpressionId, arg2: RealExpressionId) -> Derivative {
        // a**b is the same as pow(a,b)
        RealBinaryOperatorFold::fold_pow(self, arg1, arg2)
    }

    fn fold_hypot(&mut self, arg1: RealExpressionId, arg2: RealExpressionId) -> Derivative {
        let darg1 = self.fold_real_expr(arg1);
        let darg2 = self.fold_real_expr(arg2);

        // arguments swapped to get arg2*darg2+ar1*darg1 instead of arg1*darg2+arg2*darg1
        let num = self.mul_derivative(arg2, darg1, arg1, darg2)?;

        // ( hypport(f,g) )' = ( f * f' + g * g' ) /  hyppot(f,g)
        Some(self.gen_binary_op(num, Divide, self.current_expr))
    }

    fn fold_arctan2(&mut self, arg1: RealExpressionId, arg2: RealExpressionId) -> Derivative {
        // u' = f'
        let darg1 = self.fold_real_expr(arg1);

        // v' = -g'
        let darg2 = self.fold_real_expr(arg2);
        let darg2 = darg2.map(|darg2| self.gen_neg(darg2));

        // num = u'*v+v'*u (u=lhs, v=rhs, derivatives are calculated above)
        let num = self.mul_derivative(arg1, darg1, arg2, darg2)?;

        // den = g*g + f*f
        let sum1 = self.gen_binary_op(arg1, Multiply, arg1);
        let sum2 = self.gen_binary_op(arg2, Multiply, arg2);
        let den = self.gen_binary_op(sum1, Sum, sum2);

        // ( arctan2(f,g) )' = num/den = (f'g - g'*f)/(f^2+g^2)
        Some(self.gen_binary_op(num, Divide, den))
    }

    fn fold_max(&mut self, arg1: RealExpressionId, arg2: RealExpressionId) -> Derivative {
        // arg2 < arg1
        let condition = self.gen_lt_condition(arg2, arg1);
        // max = if (arg2 < arg1) arg1 else arg2
        // this generates the derivative of that
        self.fold_condition(condition, arg1, arg2)
    }

    fn fold_min(&mut self, arg1: RealExpressionId, arg2: RealExpressionId) -> Derivative {
        // arg1 < arg2
        let condition = self.gen_lt_condition(arg1, arg2);
        // max = if (arg1 < arg2) arg1 else arg2
        // this generates the derivative of that
        self.fold_condition(condition, arg1, arg2)
    }
}

impl<'lt, 'mir: 'lt> RealBuiltInFunctionCall1pFold
    for ExpressionAutoDiff<'lt, 'mir, RealExpressionId>
{
    type T = Derivative;

    fn fold_sqrt(&mut self, arg: RealExpressionId) -> Derivative {
        let inner = self.fold_real_expr(arg)?;
        // f'/ ( 2*sqrt(f) )
        let two = self.gen_constant(2.0);
        let num = self.gen_binary_op(two, Multiply, self.current_expr);
        Some(self.gen_binary_op(inner, Divide, num))
    }

    fn fold_exp(&mut self, arg: RealExpressionId) -> Derivative {
        let inner = self.fold_real_expr(arg)?;
        // f'*exp(f)
        Some(self.gen_binary_op(inner, Multiply, self.current_expr))
    }

    fn fold_ln(&mut self, arg: RealExpressionId) -> Derivative {
        let inner = self.fold_real_expr(arg)?;
        // f'/f
        Some(self.gen_binary_op(inner, Divide, arg))
    }

    fn fold_log(&mut self, arg: RealExpressionId) -> Derivative {
        // (ln(f))' * log10_e
        let res = self.fold_ln(arg)?;
        let factor = self.gen_constant(std::f64::consts::LOG10_E);
        Some(self.gen_binary_op(factor, Multiply, res))
    }

    fn fold_abs(&mut self, arg: RealExpressionId) -> Derivative {
        // f < 0
        let zero = self.gen_constant(0.0);
        let condition = self.gen_lt_condition(arg, zero);

        let derivative = self.fold_real_expr(arg)?;
        // -f
        let negated = self.gen_neg(derivative);

        // abs(f) = if (f < 0) -f' else f'
        let expr = RealExpression::Condition(condition, negated, derivative);
        Some(self.add_to_mir(expr))
    }

    fn fold_floor(&mut self, _arg: RealExpressionId) -> Derivative {
        Linter::dispatch_late(
            Box::new(RoundingDerivativeNotFullyDefined {
                span: self.ad.mir[self.current_expr].span,
            }),
            self.current_expr.into(),
        );
        None
    }

    fn fold_ceil(&mut self, _arg: RealExpressionId) -> Derivative {
        Linter::dispatch_late(
            Box::new(RoundingDerivativeNotFullyDefined {
                span: self.ad.mir[self.current_expr].span,
            }),
            self.current_expr.into(),
        );
        None
    }

    fn fold_sin(&mut self, arg: RealExpressionId) -> Derivative {
        let inner = self.fold_real_expr(arg)?;
        let outer = self.gen_math_function(Cos, arg);
        Some(self.gen_binary_op(inner, Multiply, outer))
    }

    fn fold_cos(&mut self, arg: RealExpressionId) -> Derivative {
        let inner = self.fold_real_expr(arg)?;
        let sin = self.gen_math_function(Sin, arg);
        let outer = self.gen_neg(sin);
        Some(self.gen_binary_op(inner, Multiply, outer))
    }

    fn fold_tan(&mut self, arg: RealExpressionId) -> Derivative {
        // f'*(1+tan^2(f))
        let inner = self.fold_real_expr(arg)?;
        let squred = self.gen_binary_op(self.current_expr, Multiply, self.current_expr);
        let one = self.gen_constant(1.0);
        let sum = self.gen_binary_op(one, Sum, squred);
        Some(self.gen_binary_op(inner, Multiply, sum))
    }

    fn fold_arcsin(&mut self, arg: RealExpressionId) -> Derivative {
        let inner = self.fold_real_expr(arg)?;

        // 1 - f²
        let sqrt_arg = self.gen_one_plus_minus_squared(true, arg);

        // sqrt(1-f²)
        let den = self.gen_math_function(Sqrt, sqrt_arg);

        // f'/sqrt(1-f²)
        Some(self.gen_binary_op(inner, Divide, den))
    }

    fn fold_arccos(&mut self, arg: RealExpressionId) -> Derivative {
        // - (arcsin(f)')
        let darcsin = self.fold_arcsin(arg)?;
        Some(self.gen_neg(darcsin))
    }

    fn fold_arctan(&mut self, arg: RealExpressionId) -> Derivative {
        let inner = self.fold_real_expr(arg)?;

        // 1-f²
        let den = self.gen_one_plus_minus_squared(false, arg);

        // f'/(1-f²)
        Some(self.gen_binary_op(inner, Divide, den))
    }

    fn fold_sinh(&mut self, arg: RealExpressionId) -> Derivative {
        let inner = self.fold_real_expr(arg)?;
        let outer = self.gen_math_function(CosH, arg);
        Some(self.gen_binary_op(inner, Multiply, outer))
    }

    fn fold_cosh(&mut self, arg: RealExpressionId) -> Derivative {
        let inner = self.fold_real_expr(arg)?;
        let outer = self.gen_math_function(SinH, arg);
        Some(self.gen_binary_op(inner, Multiply, outer))
    }

    fn fold_tanh(&mut self, arg: RealExpressionId) -> Derivative {
        // f'*(1-(tanh(f))²)
        let inner = self.fold_real_expr(arg)?;
        // 1-(tanh(f))²
        let outer = self.gen_one_plus_minus_squared(true, self.current_expr);
        Some(self.gen_binary_op(inner, Multiply, outer))
    }

    fn fold_arcsinh(&mut self, arg: RealExpressionId) -> Derivative {
        let inner = self.fold_real_expr(arg)?;

        // 1 + f²
        let sqrt_arg = self.gen_one_plus_minus_squared(false, arg);

        // sqrt(1+f²)
        let den = self.gen_math_function(Sqrt, sqrt_arg);

        // f'/sqrt(1+f²)
        Some(self.gen_binary_op(inner, Divide, den))
    }

    fn fold_arccosh(&mut self, arg: RealExpressionId) -> Derivative {
        let inner = self.fold_real_expr(arg)?;

        // 1-f²
        let minus_sqrt_arg = self.gen_one_plus_minus_squared(true, arg);

        // f²-1
        let sqrt_arg = self.gen_neg(minus_sqrt_arg);

        // sqrt(f²-1)
        let den = self.gen_math_function(Sqrt, sqrt_arg);

        // f'/sqrt(f²-1)
        Some(self.gen_binary_op(inner, Divide, den))
    }

    fn fold_arctanh(&mut self, arg: RealExpressionId) -> Derivative {
        let inner = self.fold_real_expr(arg)?;

        // 1-f²
        let den = self.gen_one_plus_minus_squared(true, arg);

        // f'/(1-f²)
        Some(self.gen_binary_op(inner, Divide, den))
    }
}

impl<'lt, 'mir: 'lt> IntegerExprFold for ExpressionAutoDiff<'lt, 'mir, IntegerExpressionId> {
    type T = Derivative;

    #[inline]
    fn fold_integer_expr(&mut self, expr: IntegerExpressionId) -> Derivative {
        let old = self.current_expr;
        self.current_expr = expr;
        let res = walk_integer_expression(self, expr);
        self.current_expr = old;
        res
    }
    fn mir(&self) -> &Mir {
        self.ad.mir
    }

    fn fold_literal(&mut self, _val: i64) -> Derivative {
        None
    }

    fn fold_binary_operator(
        &mut self,
        lhs: IntegerExpressionId,
        op: Spanned<IntegerBinaryOperator>,
        rhs: IntegerExpressionId,
    ) -> Derivative {
        self.fold_integer_binary_op(lhs, op.contents, rhs)
    }

    fn fold_integer_comparison(
        &mut self,
        _lhs: IntegerExpressionId,
        _op: Spanned<ComparisonOperator>,
        _rhs: IntegerExpressionId,
    ) -> Derivative {
        self.undefined_derivative(UndefinedDerivative::Comparison);
        None
    }

    fn fold_real_comparison(
        &mut self,
        _lhs: RealExpressionId,
        _op: Spanned<ComparisonOperator>,
        _rhs: RealExpressionId,
    ) -> Derivative {
        self.undefined_derivative(UndefinedDerivative::Comparison);
        None
    }

    fn fold_unary_op(
        &mut self,
        op: Spanned<UnaryOperator>,
        arg: IntegerExpressionId,
    ) -> Derivative {
        match op.contents {
            UnaryOperator::ArithmeticNegate => {
                let arg = self.fold_integer_expr(arg)?;
                Some(self.gen_neg(arg))
            }
            UnaryOperator::ExplicitPositive => self.fold_integer_expr(arg),
            UnaryOperator::BitNegate => {
                self.undefined_derivative(UndefinedDerivative::BitWiseOp);
                None
            }
            UnaryOperator::LogicNegate => {
                self.undefined_derivative(UndefinedDerivative::LogicOp);
                None
            }
        }
    }

    fn fold_condition(
        &mut self,
        cond: IntegerExpressionId,
        true_expr: IntegerExpressionId,
        false_expr: IntegerExpressionId,
    ) -> Derivative {
        let true_expr = self.fold_integer_expr(true_expr);
        let false_expr = self.fold_integer_expr(false_expr);
        let (true_expr, false_expr) = self.convert_to_paired(true_expr, false_expr)?;
        let expr = RealExpression::Condition(cond, true_expr, false_expr);
        Some(self.add_to_mir(expr))
    }

    fn fold_min(&mut self, arg1: IntegerExpressionId, arg2: IntegerExpressionId) -> Derivative {
        // arg1 < arg2
        let condition = self.gen_lt_condition(arg1, arg2);
        // max = if (arg1 < arg2) arg1 else arg2
        // this generates the derivative of that
        self.fold_condition(condition, arg1, arg2)
    }

    fn fold_max(&mut self, arg1: IntegerExpressionId, arg2: IntegerExpressionId) -> Derivative {
        // arg2 < arg1
        let condition = self.gen_lt_condition(arg2, arg1);
        // max = if (arg2 < arg1) arg1 else arg2
        // this generates the derivative of that
        self.fold_condition(condition, arg1, arg2)
    }

    fn fold_abs(&mut self, arg: IntegerExpressionId) -> Derivative {
        // f < 0
        let zero = self.gen_int_constant(0);
        let condition = self.gen_lt_condition(arg, zero);

        let derivative = self.fold_integer_expr(arg)?;
        // -f
        let negated = self.gen_neg(derivative);

        // abs(f) = if (f < 0) -f' else f'
        let expr = RealExpression::Condition(condition, negated, derivative);
        Some(self.add_to_mir(expr))
    }

    fn fold_variable_reference(&mut self, var: VariableId) -> Derivative {
        let var = self.ad.mir.derivative_var(var, self.unknown);
        Some(self.add_to_mir(RealExpression::VariableReference(var)))
    }

    fn fold_parameter_reference(&mut self, param: ParameterId) -> Derivative {
        self.param_derivative(param)
    }

    fn fold_real_cast(&mut self, expr: RealExpressionId) -> Derivative {
        ExpressionAutoDiff {
            current_expr: expr,
            ad: self.ad,
            unknown: self.unknown,
        }
        .fold_real_expr(expr)
    }

    fn fold_port_connected(&mut self, _port: PortId) -> Derivative {
        None
    }

    fn fold_param_given(&mut self, _param: ParameterId) -> Derivative {
        None
    }

    fn fold_port_reference(&mut self, _port: PortId) -> Derivative {
        unimplemented!("Ditigal")
    }

    fn fold_net_reference(&mut self, _net: NetId) -> Derivative {
        unimplemented!("Ditigal")
    }

    fn fold_string_eq(&mut self, _lhs: StringExpressionId, _rhs: StringExpressionId) -> Derivative {
        self.undefined_derivative(UndefinedDerivative::Comparison);
        None
    }

    fn fold_string_neq(
        &mut self,
        _lhs: StringExpressionId,
        _rhs: StringExpressionId,
    ) -> Derivative {
        self.undefined_derivative(UndefinedDerivative::Comparison);
        None
    }
}

impl<'lt, 'mir: 'lt> IntegerBinaryOperatorFold
    for ExpressionAutoDiff<'lt, 'mir, IntegerExpressionId>
{
    type T = Derivative;

    fn fold_sum(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Derivative {
        let dlhs = self.fold_integer_expr(lhs);
        let drhs = self.fold_integer_expr(rhs);
        self.derivative_sum(dlhs, drhs)
    }

    fn fold_diff(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Derivative {
        let dlhs = self.fold_integer_expr(lhs);
        let drhs = self.fold_integer_expr(rhs);
        let drhs = drhs.map(|drhs| self.gen_neg(drhs));
        self.derivative_sum(dlhs, drhs)
    }

    fn fold_mul(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Derivative {
        let dlhs = self.fold_integer_expr(lhs);
        let drhs = self.fold_integer_expr(rhs);
        let lhs = self.add_to_mir(IntegerConversion(lhs));
        let rhs = self.add_to_mir(IntegerConversion(rhs));
        self.mul_derivative(lhs, dlhs, rhs, drhs)
    }

    fn fold_quotient(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Derivative {
        let dlhs = self.fold_integer_expr(lhs);
        let drhs = self.fold_integer_expr(rhs);
        let lhs = self.add_to_mir(IntegerConversion(lhs));
        let rhs = self.add_to_mir(IntegerConversion(rhs));
        self.quotient_derivative(lhs, dlhs, rhs, drhs)
    }

    fn fold_pow(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Derivative {
        let dlhs = self.fold_integer_expr(lhs);
        let drhs = self.fold_integer_expr(rhs);
        let lhs = self.add_to_mir(IntegerConversion(lhs));
        let rhs = self.add_to_mir(IntegerConversion(rhs));
        let original = self.add_to_mir(RealExpression::IntegerConversion(self.current_expr));
        self.pow_derivative(lhs, dlhs, rhs, drhs, original)
    }

    fn fold_mod(&mut self, _lhs: IntegerExpressionId, _rhs: IntegerExpressionId) -> Derivative {
        self.undefined_derivative(UndefinedDerivative::Modulus);
        None
    }

    fn fold_shiftl(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Derivative {
        // ln(2)*lhs*rhs'
        let product = if let Some(drhs) = self.fold_integer_expr(rhs) {
            let lhs = self.add_to_mir(IntegerConversion(lhs));
            let ln2 = self.gen_constant(std::f64::consts::LN_2);
            let product = self.gen_binary_op(ln2, Multiply, lhs);
            Some(self.gen_binary_op(product, Multiply, drhs))
        } else {
            None
        };

        //lhs'
        let dlhs = self.fold_integer_expr(lhs);

        // lhs' + ln(2)*lhs*rhs'
        let sum = self.derivative_sum(dlhs, product)?;
        let expr = self.add_to_mir(IntegerConversion(self.current_expr));

        // (lhs' + ln(2)*lhs*rhs')* 2**rhs
        Some(self.gen_binary_op(sum, Multiply, expr))
    }

    fn fold_shiftr(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Derivative {
        // -ln(2)*lhs*rhs'
        let product = if let Some(drhs) = self.fold_integer_expr(rhs) {
            let lhs = self.add_to_mir(IntegerConversion(lhs));
            let ln2 = self.gen_constant(-1.0 * std::f64::consts::LN_2);
            let product = self.gen_binary_op(ln2, Multiply, lhs);
            Some(self.gen_binary_op(product, Multiply, drhs))
        } else {
            None
        };

        //lhs'
        let dlhs = self.fold_integer_expr(lhs);

        // lhs' + (- ln(2)*lhs*rhs')
        let sum = self.derivative_sum(dlhs, product)?;
        let expr = self.add_to_mir(IntegerConversion(self.current_expr));

        // (lhs' - ln(2)*lhs*rhs')* 2**-rhs
        Some(self.gen_binary_op(sum, Multiply, expr))
    }

    fn fold_xor(&mut self, _lhs: IntegerExpressionId, _rhs: IntegerExpressionId) -> Derivative {
        self.undefined_derivative(UndefinedDerivative::BitWiseOp);
        None
    }

    fn fold_nxor(&mut self, _lhs: IntegerExpressionId, _rhs: IntegerExpressionId) -> Derivative {
        self.undefined_derivative(UndefinedDerivative::BitWiseOp);
        None
    }

    fn fold_and(&mut self, _lhs: IntegerExpressionId, _rhs: IntegerExpressionId) -> Derivative {
        self.undefined_derivative(UndefinedDerivative::BitWiseOp);
        None
    }

    fn fold_or(&mut self, _lhs: IntegerExpressionId, _rhs: IntegerExpressionId) -> Derivative {
        self.undefined_derivative(UndefinedDerivative::BitWiseOp);
        None
    }

    fn fold_logic_and(
        &mut self,
        _lhs: IntegerExpressionId,
        _rhs: IntegerExpressionId,
    ) -> Derivative {
        self.undefined_derivative(UndefinedDerivative::LogicOp);
        None
    }

    fn fold_logic_or(
        &mut self,
        _lhs: IntegerExpressionId,
        _rhs: IntegerExpressionId,
    ) -> Derivative {
        self.undefined_derivative(UndefinedDerivative::LogicOp);
        None
    }
}

impl<'lt> AutoDiff<'lt> {
    pub fn partial_derivative(
        &mut self,
        expr: ExpressionId,
        derive_by: Unknown,
    ) -> RealExpressionId {
        match expr {
            ExpressionId::Real(expr) => ExpressionAutoDiff {
                current_expr: expr,
                unknown: derive_by,
                ad: self,
            }
            .run(),
            ExpressionId::Integer(expr) => ExpressionAutoDiff {
                current_expr: expr,
                unknown: derive_by,
                ad: self,
            }
            .run(),
            ExpressionId::String(expr) => {
                self.errors
                    .add(OnlyNumericExpressionsCanBeDerived(self.mir[expr].span));

                // Just a placeholder
                self.mir
                    .real_expressions
                    .push(Spanned::new(RealExpression::Literal(0.0), DUMMY_SP))
            }
        }
    }
}
