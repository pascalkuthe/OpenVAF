//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

#![allow(clippy::float_cmp)]
#![allow(clippy::similar_names)]



use crate::analysis::data_flow::reaching_variables::UseDefGraph;
use crate::ast::UnaryOperator;

use crate::cfg::{BasicBlock, Terminator};
use crate::data_structures::BitSet;
use crate::ir::cfg::{BasicBlockId, ControlFlowGraph};
use crate::ir::mir::Mir;
use crate::ir::{
    BuiltInFunctionCall1p, BuiltInFunctionCall2p, IntegerExpressionId, ParameterId,
    RealExpressionId, StatementId, StringExpressionId, VariableId,
};
use crate::literals::StringLiteral;
use crate::mir::{
    ComparisonOperator, ExpressionId, IntegerBinaryOperator, IntegerExpression, RealBinaryOperator,
    RealExpression, Statement, StringExpression,
};
use bitflags::_core::option::Option::Some;
use index_vec::IndexVec;
use log::*;
use rustc_hash::FxHashMap;

#[derive(Clone, Debug, Default)]
pub struct ConstantFoldState {
    pub real_definitions: FxHashMap<StatementId, f64>,
    pub integer_definitions: FxHashMap<StatementId, i64>,
    pub string_definitions: FxHashMap<StatementId, StringLiteral>,
    pub real_parameters: FxHashMap<ParameterId, f64>,
    pub int_parameters: FxHashMap<ParameterId, i64>,
    pub string_parameters: FxHashMap<ParameterId, StringLiteral>,
}

impl ConstantResolver for () {
    #[inline(always)]
    fn get_real_variable_value(&mut self, _var: VariableId) -> Option<f64> {
        None
    }

    #[inline(always)]
    fn get_int_variable_value(&mut self, _var: VariableId) -> Option<i64> {
        None
    }

    #[inline(always)]
    fn get_str_variable_value(&mut self, _var: VariableId) -> Option<StringLiteral> {
        None
    }

    #[inline(always)]
    fn get_real_parameter_value(&mut self, _param: ParameterId) -> Option<f64> {
        None
    }

    #[inline(always)]
    fn get_int_parameter_value(&mut self, _param: ParameterId) -> Option<i64> {
        None
    }

    #[inline(always)]
    fn get_str_parameter_value(&mut self, _param: ParameterId) -> Option<StringLiteral> {
        None
    }
}

pub struct ConstantPropagator<'lt> {
    known_values: &'lt ConstantFoldState,
    dependencys_before: &'lt BitSet<StatementId>,
    dependencys_after: &'lt mut BitSet<StatementId>,
    variables_assignments: &'lt IndexVec<VariableId, BitSet<StatementId>>,
}

impl<'lt> ConstantResolver for ConstantPropagator<'lt> {
    #[inline]
    fn get_real_variable_value(&mut self, var: VariableId) -> Option<f64> {
        let mut definitions = self
            .dependencys_before
            .intersection(&self.variables_assignments[var])
            .map(|id| self.known_values.real_definitions.get(&id));

        // TODO constant fold default values
        let value = *definitions.next().flatten()?;
        if definitions.any(|x| Some(&value) != x) {
            return None;
        }
        self.dependencys_after
            .difference_with(&self.variables_assignments[var]);

        Some(value)
    }

    #[inline]
    fn get_int_variable_value(&mut self, var: VariableId) -> Option<i64> {
        let mut definitions = self
            .dependencys_before
            .intersection(&self.variables_assignments[var])
            .map(|id| self.known_values.integer_definitions.get(&id));

        // TODO constant fold default values
        let value = *definitions.next().flatten()?;
        if definitions.any(|x| Some(&value) != x) {
            return None;
        }
        self.dependencys_after
            .difference_with(&self.variables_assignments[var]);

        Some(value)
    }

    #[inline]
    fn get_str_variable_value(&mut self, var: VariableId) -> Option<StringLiteral> {
        let mut definitions = self
            .dependencys_before
            .intersection(&self.variables_assignments[var])
            .map(|id| self.known_values.string_definitions.get(&id));

        let value = *definitions.next().flatten()?;
        if definitions.any(|x| Some(&value) != x) {
            return None;
        }
        self.dependencys_after
            .difference_with(&self.variables_assignments[var]);
        Some(value)
    }

    #[inline]
    fn get_real_parameter_value(&mut self, param: ParameterId) -> Option<f64> {
        self.known_values.real_parameters.get(&param).copied()
    }

    #[inline]
    fn get_int_parameter_value(&mut self, param: ParameterId) -> Option<i64> {
        self.known_values.int_parameters.get(&param).copied()
    }

    #[inline]
    fn get_str_parameter_value(&mut self, param: ParameterId) -> Option<StringLiteral> {
        self.known_values.string_parameters.get(&param).copied()
    }
}

pub trait ConstantResolver {
    fn get_real_variable_value(&mut self, var: VariableId) -> Option<f64>;
    fn get_int_variable_value(&mut self, var: VariableId) -> Option<i64>;
    fn get_str_variable_value(&mut self, var: VariableId) -> Option<StringLiteral>;
    fn get_real_parameter_value(&mut self, param: ParameterId) -> Option<f64>;
    fn get_int_parameter_value(&mut self, param: ParameterId) -> Option<i64>;
    fn get_str_parameter_value(&mut self, param: ParameterId) -> Option<StringLiteral>;
}

pub fn real_constant_fold(
    fold: &mut impl ConstantFolder,
    resolver: &mut impl ConstantResolver,
    expr: RealExpressionId,
) -> Option<f64> {
    let res = match fold.mir()[expr].contents {
        RealExpression::Literal(val) => return Some(val),
        RealExpression::VariableReference(var) => resolver.get_real_variable_value(var)?,
        RealExpression::ParameterReference(param) => resolver.get_real_parameter_value(param)?,

        RealExpression::BinaryOperator(lhs_id, op, rhs_id) => {
            let lhs = fold.real_constant_fold(resolver, lhs_id);
            let rhs = fold.real_constant_fold(resolver, rhs_id);

            /*
                This fold also does some algebraic simplification here for expressions where only one operator can be folded.
                Algebraic simiplifications are technically not IEEE-754 compliant because you have to special case NAN (NAN*0.0 = NAN)
                In practice if you have a term z=x*y where x is known at compile time to be == 0.0 then you probably do intend z to be always 0.0
                We do this because this non standard behavior is required to correctly calculate some derivatives
                and it also dramatically speeds up compile times for code with lots of derivatives.
                TODO properly document this
                Note that simplifying 0.0 + x or 1.0*x to x is perfectly fine
                These simplifications are however also not very interesting but are done because its faster to do here than at runtime / let llvm do it.
                All simplifications that are not standard compliant will be marked with a comment
            */

            match op.contents {
                RealBinaryOperator::Sum => match (lhs, rhs) {
                    (Some(lhs), Some(rhs)) => lhs + rhs,
                    (Some(lhs), None) if lhs == 0.0 => {
                        fold.resolve_to_real_subexpressions(expr, rhs_id);
                        return None;
                    }
                    (None, Some(rhs)) if rhs == 0.0 => {
                        fold.resolve_to_real_subexpressions(expr, lhs_id);
                        return None;
                    }
                    (_, _) => return None,
                },
                RealBinaryOperator::Subtract => match (lhs, rhs) {
                    (Some(lhs), Some(rhs)) => lhs - rhs,
                    (Some(lhs), None) if lhs == 0.0 => {
                        fold.resolve_to_real_subexpressions(expr, rhs_id);
                        return None;
                    }
                    (None, Some(rhs)) if rhs == 0.0 => {
                        fold.resolve_to_real_subexpressions(expr, lhs_id);
                        return None;
                    }
                    (_, _) => return None,
                },
                RealBinaryOperator::Multiply => match (lhs, rhs) {
                    //not IEEE-754 compliant
                    (Some(x), _) | (_, Some(x)) if x == 0.0 => 0.0,

                    (Some(lhs), Some(rhs)) => lhs * rhs,

                    (Some(lhs), None) if lhs == 1.0 => {
                        fold.resolve_to_real_subexpressions(expr, rhs_id);
                        return None;
                    }

                    (None, Some(rhs)) if rhs == 1.0 => {
                        fold.resolve_to_real_subexpressions(expr, rhs_id);
                        return None;
                    }

                    (_, _) => return None,
                },
                RealBinaryOperator::Divide => match (lhs, rhs) {
                    //not IEEE-754 compliant
                    (Some(lhs), _) if lhs == 0.0 => 0.0,
                    (Some(lhs), Some(rhs)) => lhs / rhs,

                    (None, Some(rhs)) if rhs == 1.0 => {
                        fold.resolve_to_real_subexpressions(expr, lhs_id);
                        return None;
                    }
                    (_, _) => return None,
                },
                RealBinaryOperator::Exponent => match (lhs, rhs) {
                    //not IEEE-754 compliant
                    (Some(lhs), _) if lhs == 0.0 => 0.0,

                    //not IEEE-754 compliant
                    (Some(lhs), _) if lhs == 1.0 => 1.0,

                    //not IEEE-754 compliant
                    (None, Some(rhs)) if rhs == 0.0 => 1.0,

                    (None, Some(rhs)) if rhs == 1.0 => {
                        fold.resolve_to_real_subexpressions(expr, lhs_id);
                        return None;
                    }

                    (Some(lhs), Some(rhs)) => lhs.powf(rhs),

                    (None, Some(rhs)) if rhs == 1.0 => {
                        fold.resolve_to_real_subexpressions(expr, lhs_id);
                        return None;
                    }

                    (_, _) => return None,
                },

                RealBinaryOperator::Modulus => {
                    let lhs = lhs?;
                    if lhs == 0.0 {
                        0.0
                    } else {
                        lhs % rhs?
                    }
                }
            }
        }

        RealExpression::Negate(_, val) => -fold.real_constant_fold(resolver, val)?,

        RealExpression::Condition(condition, _, true_val_id, _, false_val_id) => {
            let condition = fold.int_constant_fold(resolver, condition);
            let true_val = fold.real_constant_fold(resolver, true_val_id);
            let false_val = fold.real_constant_fold(resolver, false_val_id);

            if condition? != 0 {
                if let Some(true_val) = true_val {
                    true_val
                } else {
                    fold.resolve_to_real_subexpressions(expr, true_val_id);
                    return None;
                }
            } else if let Some(false_val) = false_val {
                false_val
            } else {
                fold.resolve_to_real_subexpressions(expr, false_val_id);
                return None;
            }
        }

        RealExpression::BuiltInFunctionCall1p(call, arg) => {
            let arg = fold.real_constant_fold(resolver, arg)?;
            match call {
                BuiltInFunctionCall1p::Ln => arg.ln(),
                BuiltInFunctionCall1p::Sqrt => arg.sqrt(),
                BuiltInFunctionCall1p::Exp(_) /* Whether this is a limexp or exp doesnt matter for constant eval*/ => arg.exp(),
                BuiltInFunctionCall1p::Log => arg.log10(),
                BuiltInFunctionCall1p::Abs => arg.abs(),
                BuiltInFunctionCall1p::Floor => arg.floor(),
                BuiltInFunctionCall1p::Ceil => arg.ceil(),
                BuiltInFunctionCall1p::Sin => arg.sin(),
                BuiltInFunctionCall1p::Cos => arg.cos(),
                BuiltInFunctionCall1p::Tan => arg.tan(),
                BuiltInFunctionCall1p::ArcSin => arg.asin(),
                BuiltInFunctionCall1p::ArcCos => arg.acos(),
                BuiltInFunctionCall1p::ArcTan => arg.atan(),
                BuiltInFunctionCall1p::SinH => arg.sinh(),
                BuiltInFunctionCall1p::CosH => arg.cosh(),
                BuiltInFunctionCall1p::TanH => arg.tanh(),
                BuiltInFunctionCall1p::ArcSinH => arg.asinh(),
                BuiltInFunctionCall1p::ArcCosH => arg.acosh(),
                BuiltInFunctionCall1p::ArcTanH => arg.atanh(),
            }
        }

        RealExpression::BuiltInFunctionCall2p(call, arg1_id, arg2_id) => {
            let arg1 = fold.real_constant_fold(resolver, arg1_id);
            let arg2 = fold.real_constant_fold(resolver, arg2_id);
            match call {
                BuiltInFunctionCall2p::Pow => {
                    match (arg1, arg2) {
                        //not IEEE-754 compliant
                        (Some(arg1), _) if arg1 == 0.0 => 0.0,

                        //not IEEE-754 compliant
                        (Some(arg1), _) if arg1 == 1.0 => 1.0,

                        //not IEEE-754 compliant
                        (None, Some(arg2)) if arg2 == 0.0 => 1.0,

                        (None, Some(arg2)) if arg2 == 1.0 => {
                            fold.resolve_to_real_subexpressions(expr, arg1_id);
                            return None;
                        }

                        (Some(arg1), Some(arg2)) => arg1.powf(arg2),

                        (_, _) => return None,
                    }
                }
                BuiltInFunctionCall2p::Hypot => arg1?.hypot(arg2?),
                BuiltInFunctionCall2p::Min => arg1?.min(arg2?),
                BuiltInFunctionCall2p::Max => arg1?.max(arg2?),
                BuiltInFunctionCall2p::ArcTan2 => arg1?.atan2(arg2?),
            }
        }

        RealExpression::IntegerConversion(expr) => fold.int_constant_fold(resolver, expr)? as f64,

        RealExpression::Vt(Some(_temp)) => {
            //TODO abstract over constants
            return None;
        }

        //Temperature/Sim parameters/Branches may be added in the future if there is any demand for it but it doesnt seem useful to me
        RealExpression::Temperature
        | RealExpression::SimParam(_, _)
        | RealExpression::Vt(None)
        | RealExpression::BranchAccess(_, _, _)
        | RealExpression::Noise(_, _) => return None,
    };
    Some(res)
}

pub fn int_constant_fold(
    fold: &mut impl ConstantFolder,
    resolver: &mut impl ConstantResolver,
    expr: IntegerExpressionId,
) -> Option<i64> {
    let res = match fold.mir()[expr].contents {
        IntegerExpression::Literal(val) => return Some(val),
        IntegerExpression::ParameterReference(param) => resolver.get_int_parameter_value(param)?,
        IntegerExpression::VariableReference(var) => resolver.get_int_variable_value(var)?,

        IntegerExpression::Abs(val) => fold.int_constant_fold(resolver, val)?.abs(),

        IntegerExpression::Min(arg1, arg2) => {
            let arg1 = fold.int_constant_fold(resolver, arg1);
            let arg2 = fold.int_constant_fold(resolver, arg2);
            arg1?.min(arg2?)
        }

        IntegerExpression::Max(arg1, arg2) => {
            let arg1 = fold.int_constant_fold(resolver, arg1);
            let arg2 = fold.int_constant_fold(resolver, arg2);
            arg1?.max(arg2?)
        }

        IntegerExpression::BinaryOperator(lhs_id, op, rhs_id) => {
            let lhs = fold.int_constant_fold(resolver, lhs_id);
            let rhs = fold.int_constant_fold(resolver, rhs_id);
            match op.contents {
                IntegerBinaryOperator::Sum => match (lhs, rhs) {
                    (Some(lhs), Some(rhs)) => lhs + rhs,
                    (Some(0), None) => {
                        fold.resolve_to_int_subexpressions(expr, rhs_id);
                        return None;
                    }
                    (None, Some(0)) => {
                        fold.resolve_to_int_subexpressions(expr, lhs_id);
                        return None;
                    }
                    (_, _) => return None,
                },
                IntegerBinaryOperator::Subtract => match (lhs, rhs) {
                    (Some(lhs), Some(rhs)) => lhs - rhs,

                    (None, Some(0)) => {
                        fold.resolve_to_int_subexpressions(expr, lhs_id);
                        return None;
                    }

                    (_, _) => return None,
                },
                IntegerBinaryOperator::Multiply => match (lhs, rhs) {
                    (Some(0), _) | (_, Some(0)) => 0,
                    (Some(lhs), Some(rhs)) => lhs * rhs,
                    (Some(1), None) => {
                        fold.resolve_to_int_subexpressions(expr, rhs_id);
                        return None;
                    }
                    (None, Some(1)) => {
                        fold.resolve_to_int_subexpressions(expr, lhs_id);
                        return None;
                    }
                    (_, _) => return None,
                },
                IntegerBinaryOperator::Divide => match (lhs, rhs) {
                    (Some(0), _) => 0,
                    (Some(lhs), Some(rhs)) => lhs / rhs,
                    (None, Some(1)) => {
                        fold.resolve_to_int_subexpressions(expr, lhs_id);
                        return None;
                    }
                    (_, _) => return None,
                },
                IntegerBinaryOperator::Exponent => match (lhs, rhs) {
                    (Some(0), _) => 0,
                    (Some(1), _) => 1,

                    (None, Some(0)) => 1,
                    (None, Some(1)) => {
                        fold.resolve_to_int_subexpressions(expr, lhs_id);
                        return None;
                    }

                    // TODO Proper error on overflow
                    (Some(lhs), Some(rhs)) if rhs >= 0 => lhs.pow(rhs as u32),

                    (Some(_), Some(_)) => 0,

                    (_, _) => return None,
                },

                IntegerBinaryOperator::Modulus => {
                    let lhs = lhs?;
                    if lhs == 0 {
                        0
                    } else {
                        lhs % rhs?
                    }
                }
                IntegerBinaryOperator::ShiftLeft => {
                    if lhs == Some(0) {
                        0
                    } else if rhs == Some(0) && lhs == None {
                        fold.resolve_to_int_subexpressions(expr, lhs_id);
                        return None;
                    } else {
                        lhs? << rhs?
                    }
                }
                IntegerBinaryOperator::ShiftRight => {
                    if lhs == Some(0) {
                        0
                    } else if rhs == Some(0) && lhs == None {
                        fold.resolve_to_int_subexpressions(expr, lhs_id);
                        return None;
                    } else {
                        lhs? >> rhs?
                    }
                }
                IntegerBinaryOperator::Xor => lhs? ^ rhs?,
                IntegerBinaryOperator::NXor => !(lhs? ^ rhs?),
                IntegerBinaryOperator::And => {
                    if lhs == Some(0) || rhs == Some(0) {
                        0
                    } else {
                        lhs? & rhs?
                    }
                }
                IntegerBinaryOperator::Or => {
                    if lhs == Some(0) && rhs.is_none() {
                        fold.resolve_to_int_subexpressions(expr, rhs_id);
                        return None;
                    } else if rhs == Some(0) && lhs.is_none() {
                        fold.resolve_to_int_subexpressions(expr, lhs_id);
                        return None;
                    } else {
                        lhs? | rhs?
                    }
                }
                IntegerBinaryOperator::LogicOr => match (lhs, rhs) {
                    (Some(0), Some(0)) => 0,
                    (Some(_), Some(_)) => 1,
                    (None, None) => return None,
                    (Some(0), None) => {
                        fold.resolve_to_int_subexpressions(expr, rhs_id);
                        return None;
                    }
                    (None, Some(0)) => {
                        fold.resolve_to_int_subexpressions(expr, lhs_id);
                        return None;
                    }
                    (Some(_), None) | (None, Some(_)) => 1,
                },
                IntegerBinaryOperator::LogicAnd => match (lhs, rhs) {
                    (Some(0), Some(0)) => 0,
                    (Some(0), _) | (_, Some(0)) => 0,
                    (Some(_), Some(_)) => 1,
                    (None, None) => return None,
                    (Some(_), None) => {
                        fold.resolve_to_int_subexpressions(expr, rhs_id);
                        return None;
                    }
                    (None, Some(_)) => {
                        fold.resolve_to_int_subexpressions(expr, lhs_id);
                        return None;
                    }
                },
            }
        }

        IntegerExpression::UnaryOperator(op, arg) => {
            let arg = fold.int_constant_fold(resolver, arg)?;
            match op.contents {
                UnaryOperator::BitNegate => !arg,
                UnaryOperator::LogicNegate => (arg == 0) as i64,
                UnaryOperator::ArithmeticNegate => -arg,
                UnaryOperator::ExplicitPositive => arg,
            }
        }

        IntegerExpression::IntegerComparison(lhs, op, rhs) => {
            let lhs = fold.int_constant_fold(resolver, lhs);
            let rhs = fold.int_constant_fold(resolver, rhs);
            let (lhs, rhs) = (lhs?, rhs?);
            let res = match op.contents {
                ComparisonOperator::LessThen => lhs < rhs,
                ComparisonOperator::LessEqual => lhs <= rhs,
                ComparisonOperator::GreaterThen => lhs > rhs,
                ComparisonOperator::GreaterEqual => lhs >= rhs,
                ComparisonOperator::LogicEqual => lhs == rhs,
                ComparisonOperator::LogicalNotEqual => lhs != rhs,
            };
            res as i64
        }

        IntegerExpression::RealComparison(lhs, op, rhs) => {
            let lhs = fold.real_constant_fold(resolver, lhs);
            let rhs = fold.real_constant_fold(resolver, rhs);
            let (lhs, rhs) = (lhs?, rhs?);
            let res = match op.contents {
                ComparisonOperator::LessThen => lhs < rhs,
                ComparisonOperator::LessEqual => lhs <= rhs,
                ComparisonOperator::GreaterThen => lhs > rhs,
                ComparisonOperator::GreaterEqual => lhs >= rhs,
                ComparisonOperator::LogicEqual => lhs == rhs,
                ComparisonOperator::LogicalNotEqual => lhs != rhs,
            };
            res as i64
        }

        IntegerExpression::StringEq(lhs, rhs) => {
            let lhs = fold.string_constant_fold(resolver, lhs);
            let rhs = fold.string_constant_fold(resolver, rhs);
            (lhs? == rhs?) as i64
        }

        IntegerExpression::StringNEq(lhs, rhs) => {
            let lhs = fold.string_constant_fold(resolver, lhs);
            let rhs = fold.string_constant_fold(resolver, rhs);
            (lhs? != rhs?) as i64
        }

        IntegerExpression::Condition(condition, _, true_val_id, _, false_val_id) => {
            let condition = fold.int_constant_fold(resolver, condition);
            let true_val = fold.int_constant_fold(resolver, true_val_id);
            let false_val = fold.int_constant_fold(resolver, false_val_id);

            if condition? != 0 {
                if let Some(true_val) = true_val {
                    true_val
                } else {
                    fold.resolve_to_int_subexpressions(expr, true_val_id);
                    return None;
                }
            } else if let Some(false_val) = false_val {
                false_val
            } else {
                fold.resolve_to_int_subexpressions(expr, false_val_id);
                return None;
            }
        }

        IntegerExpression::RealCast(val) => fold.real_constant_fold(resolver, val)?.round() as i64,

        //TODO system function call constant fold
        IntegerExpression::PortConnected(_)
        | IntegerExpression::ParamGiven(_)
        | IntegerExpression::NetReference(_)
        | IntegerExpression::PortReference(_)
        | IntegerExpression::FunctionCall(_, _) => return None,
    };

    Some(res)
}

pub fn string_constant_fold(
    fold: &mut impl ConstantFolder,
    resolver: &mut impl ConstantResolver,
    expr: StringExpressionId,
) -> Option<StringLiteral> {
    Some(match fold.mir()[expr].contents {
        StringExpression::Literal(val) => return Some(val),
        StringExpression::VariableReference(var) => resolver.get_str_variable_value(var)?,
        StringExpression::ParameterReference(param) => resolver.get_str_parameter_value(param)?,

        StringExpression::Condition(condition, _, true_val, _, false_val) => {
            let condition = fold.int_constant_fold(resolver, condition);
            let true_val = fold.string_constant_fold(resolver, true_val);
            let false_val = fold.string_constant_fold(resolver, false_val);

            if condition? == 0 {
                false_val?
            } else {
                true_val?
            }
        }
        // TODO system function call constant fold
        StringExpression::SimParam(_) => return None,
    })
}

pub struct ReadingConstantFold<'lt>(pub &'lt Mir);

impl<'lt> ConstantFolder for ReadingConstantFold<'lt> {
    fn resolve_to_string_subexpressions(
        &mut self,
        _dst: StringExpressionId,
        _newval: StringExpressionId,
    ) {
    }

    fn resolve_to_int_subexpressions(
        &mut self,
        _dst: IntegerExpressionId,
        _newval: IntegerExpressionId,
    ) {
    }

    fn resolve_to_real_subexpressions(
        &mut self,
        _dst: RealExpressionId,
        _newval: RealExpressionId,
    ) {
    }

    fn mir(&self) -> &Mir {
        self.0
    }
}

pub struct IntermediateWritingConstantFold<'lt>(pub &'lt mut Mir);

impl<'lt> ConstantFolder for IntermediateWritingConstantFold<'lt> {
    fn real_constant_fold(
        &mut self,
        resolver: &mut impl ConstantResolver,
        expr: RealExpressionId,
    ) -> Option<f64> {
        let res = real_constant_fold(self, resolver, expr)?;
        self.0[expr].contents = RealExpression::Literal(res);
        Some(res)
    }

    fn int_constant_fold(
        &mut self,
        resolver: &mut impl ConstantResolver,
        expr: IntegerExpressionId,
    ) -> Option<i64> {
        let res = int_constant_fold(self, resolver, expr)?;
        self.0[expr].contents = IntegerExpression::Literal(res);
        Some(res)
    }

    fn string_constant_fold(
        &mut self,
        resolver: &mut impl ConstantResolver,
        expr: StringExpressionId,
    ) -> Option<StringLiteral> {
        let res = string_constant_fold(self, resolver, expr)?;
        self.0[expr].contents = StringExpression::Literal(res);
        Some(res)
    }

    fn resolve_to_string_subexpressions(
        &mut self,
        dst: StringExpressionId,
        newval: StringExpressionId,
    ) {
        self.0[dst] = self.0[newval].clone();
    }

    fn resolve_to_int_subexpressions(
        &mut self,
        dst: IntegerExpressionId,
        newval: IntegerExpressionId,
    ) {
        self.0[dst] = self.0[newval].clone();
    }

    fn resolve_to_real_subexpressions(&mut self, dst: RealExpressionId, newval: RealExpressionId) {
        self.0[dst] = self.0[newval].clone();
    }

    fn mir(&self) -> &Mir {
        &self.0
    }
}

pub trait ConstantFolder: Sized {
    // We do defaults with external functions because specialization is nowhere near stable
    // This is similar to overwriting methods in OOP

    fn real_constant_fold(
        &mut self,
        resolver: &mut impl ConstantResolver,
        expr: RealExpressionId,
    ) -> Option<f64> {
        real_constant_fold(self, resolver, expr)
    }

    fn int_constant_fold(
        &mut self,
        resolver: &mut impl ConstantResolver,
        expr: IntegerExpressionId,
    ) -> Option<i64> {
        int_constant_fold(self, resolver, expr)
    }

    fn string_constant_fold(
        &mut self,
        resolver: &mut impl ConstantResolver,
        expr: StringExpressionId,
    ) -> Option<StringLiteral> {
        string_constant_fold(self, resolver, expr)
    }

    fn resolve_to_string_subexpressions(
        &mut self,
        dst: StringExpressionId,
        newval: StringExpressionId,
    );
    fn resolve_to_int_subexpressions(
        &mut self,
        dst: IntegerExpressionId,
        newval: IntegerExpressionId,
    );
    fn resolve_to_real_subexpressions(&mut self, dst: RealExpressionId, newval: RealExpressionId);

    fn mir(&self) -> &Mir;
}

impl ControlFlowGraph {
    pub fn constant_fold(
        &mut self,
        fold: &mut impl ConstantFolder,
        udg: &mut UseDefGraph,
        known_values: &mut ConstantFoldState,
        write_intermediate: bool,
    ) {
        let mut temporary_set = BitSet::new_empty(udg.len_stmd_idx());

        for (id, bb) in self.reverse_postorder_itermut() {
            Self::constant_fold_basic_block(
                id,
                bb,
                fold,
                udg,
                known_values,
                &mut temporary_set,
                write_intermediate,
            );

            if let Terminator::Split {
                condition,
                true_block,
                false_block,
                merge,
            } = bb.terminator
            {
                temporary_set
                    .as_mut_slice()
                    .copy_from_slice(udg.terminator_use_def_chains[id].as_slice());

                let folded_condition = fold.int_constant_fold(
                    &mut ConstantPropagator {
                        known_values,
                        dependencys_before: &udg.terminator_use_def_chains[id],
                        dependencys_after: &mut temporary_set,
                        variables_assignments: &udg.assignments,
                    },
                    condition,
                );

                if write_intermediate || folded_condition.is_some() {
                    std::mem::swap(&mut temporary_set, &mut udg.terminator_use_def_chains[id]);
                }

                match folded_condition {
                    Some(0) => {
                        debug!(
                            "{:?}->(false: {:?}, true: {:?}) always false (condition: {:?})",
                            id, false_block, true_block, condition
                        );

                        bb.terminator = Terminator::Goto(false_block);
                    }

                    Some(_) => {
                        if merge == id {
                            panic!("Found constant infinite loop!")
                        }

                        bb.terminator = Terminator::Goto(true_block);
                    }

                    None => (),
                }
            }
        }
    }

    fn constant_fold_basic_block(
        id: BasicBlockId,
        bb: &mut BasicBlock,
        fold: &mut impl ConstantFolder,
        udg: &mut UseDefGraph,
        known_values: &mut ConstantFoldState,
        temporary_set: &mut BitSet<StatementId>,
        write_intermediate: bool,
    ) {
        for &stmt in bb.statements.iter().rev() {
            temporary_set
                .as_mut_slice()
                .copy_from_slice(udg.stmt_use_def_chains[stmt].as_slice());

            let mut resolver = ConstantPropagator {
                known_values,
                dependencys_before: &udg.stmt_use_def_chains[stmt],
                dependencys_after: temporary_set,
                variables_assignments: &udg.assignments,
            };

            match fold.mir()[stmt] {
                Statement::Assignment(_, _, val) => {
                    let success = match val {
                        ExpressionId::Real(val) => {
                            if let Some(val) = fold.real_constant_fold(&mut resolver, val) {
                                let old = known_values.real_definitions.insert(stmt, val);
                                #[cfg(debug_assertions)]
                                match old{
                                    Some(new) if new != val => panic!(
                                        "Statement {} in block {} was assigned twice with different values (old={},new={})!",
                                        stmt,
                                        id,
                                        old.unwrap(),
                                        val
                                    ),
                                    _ => ()
                                }
                                true
                            } else {
                                false
                            }
                        }

                        ExpressionId::Integer(val) => {
                            if let Some(val) = fold.int_constant_fold(&mut resolver, val) {
                                let old = known_values.integer_definitions.insert(stmt, val);
                                #[cfg(debug_assertions)]
                                match old{
                                    Some(new) if new != val => panic!(
                                        "Statement {} in block {} was assigned twice with different values (old={},new={})!",
                                        stmt,
                                        id,
                                        old.unwrap(),
                                        val
                                    ),
                                    _ => ()
                                }
                                true
                            } else {
                                false
                            }
                        }
                        ExpressionId::String(val) => {
                            if let Some(val) = fold.string_constant_fold(&mut resolver, val) {
                                let old = known_values.string_definitions.insert(stmt, val);
                                #[cfg(debug_assertions)]
                                match old{
                                    Some(old) if old != val => panic!(
                                        "Statement {} in block {} was assigned twice with different values (old={},new={})!",
                                        stmt,
                                        id,
                                        old,
                                        val
                                    ),
                                    _ => ()
                                }
                                true
                            } else {
                                false
                            }
                        }
                    };

                    if success || write_intermediate {
                        std::mem::swap(temporary_set, &mut udg.stmt_use_def_chains[stmt])
                    }
                }

                Statement::Contribute(_, _, _, val) if write_intermediate => {
                    fold.real_constant_fold(&mut resolver, val);
                    std::mem::swap(temporary_set, &mut udg.stmt_use_def_chains[stmt])
                }

                _ => (),
            }
        }
    }
}
