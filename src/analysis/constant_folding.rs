use crate::analysis::data_flow::reaching_variables::{DefiningSet, UseDefGraph};
use crate::ast::UnaryOperator;
use crate::compact_arena::{CompressedRange, TinyHeapArena};
use crate::ir::mir::control_flow_graph::BasicBlockId;
use crate::ir::mir::{ControlFlowGraph, Mir};
use crate::ir::{
    BuiltInFunctionCall1p, BuiltInFunctionCall2p, IntegerExpressionId, ParameterId,
    RealExpressionId, StatementId, StringExpressionId, VariableId,
};
use crate::mir::control_flow_graph::Terminator;
use crate::mir::{
    ComparisonOperator, ExpressionId, IntegerBinaryOperator, IntegerExpression, RealBinaryOperator,
    RealExpression, Statement, StringExpression,
};
use fixedbitset::FixedBitSet as BitSet;
use log::*;
use rustc_hash::FxHashMap;

#[derive(Clone, Debug, Default)]
pub struct ConstantFoldState<'tag> {
    pub real_definitions: FxHashMap<StatementId<'tag>, f64>,
    pub integer_definitions: FxHashMap<StatementId<'tag>, i64>,
    pub string_definitions: FxHashMap<StatementId<'tag>, CompressedRange<'tag>>,
    pub real_parameters: FxHashMap<ParameterId<'tag>, f64>,
    pub int_parameters: FxHashMap<ParameterId<'tag>, i64>,
    pub string_parameters: FxHashMap<ParameterId<'tag>, CompressedRange<'tag>>,
}

impl<'tag> ConstantResolver<'tag> for () {
    #[inline(always)]
    fn get_real_variable_value(&mut self, var: VariableId<'tag>) -> Option<f64> {
        None
    }

    #[inline(always)]
    fn get_int_variable_value(&mut self, var: VariableId<'tag>) -> Option<i64> {
        None
    }

    #[inline(always)]
    fn get_str_variable_value(&mut self, var: VariableId<'tag>) -> Option<CompressedRange<'tag>> {
        None
    }

    #[inline(always)]
    fn get_real_parameter_value(&mut self, param: ParameterId<'tag>) -> Option<f64> {
        None
    }

    #[inline(always)]
    fn get_int_parameter_value(&mut self, param: ParameterId<'tag>) -> Option<i64> {
        None
    }

    #[inline(always)]
    fn get_str_parameter_value(
        &mut self,
        param: ParameterId<'tag>,
    ) -> Option<CompressedRange<'tag>> {
        None
    }
}

pub struct ConstantPropagator<'lt, 'tag> {
    known_values: &'lt ConstantFoldState<'tag>,
    dependencys_before: &'lt DefiningSet,
    dependencys_after: &'lt mut DefiningSet,
    variables_assignments: &'lt TinyHeapArena<'tag, DefiningSet>,
}

impl<'lt, 'tag> ConstantResolver<'tag> for ConstantPropagator<'lt, 'tag> {
    #[inline]
    fn get_real_variable_value(&mut self, var: VariableId<'tag>) -> Option<f64> {
        let mut definitions = self
            .dependencys_before
            .0
            .intersection(&self.variables_assignments[var.unwrap()].0)
            .map(|id| unsafe {
                let id = StatementId::from_raw_index(id);
                self.known_values.real_definitions.get(&id)
            });

        // TODO constant fold default values
        let value = *definitions.next().flatten()?;
        if definitions.any(|x| Some(&value) != x) {
            return None;
        }
        self.dependencys_after
            .0
            .difference_with(&self.variables_assignments[var.unwrap()].0);

        Some(value)
    }

    #[inline]
    fn get_int_variable_value(&mut self, var: VariableId<'tag>) -> Option<i64> {
        let mut definitions = self
            .dependencys_before
            .0
            .intersection(&self.variables_assignments[var.unwrap()].0)
            .map(|id| unsafe {
                let id = StatementId::from_raw_index(id);
                self.known_values.integer_definitions.get(&id)
            });

        // TODO constant fold default values
        let value = *definitions.next().flatten()?;
        if definitions.any(|x| Some(&value) != x) {
            return None;
        }
        self.dependencys_after
            .0
            .difference_with(&self.variables_assignments[var.unwrap()].0);

        Some(value)
    }

    #[inline]
    fn get_str_variable_value(&mut self, var: VariableId<'tag>) -> Option<CompressedRange<'tag>> {
        let mut definitions = self
            .dependencys_before
            .0
            .intersection(&self.variables_assignments[var.unwrap()].0)
            .map(|id| unsafe {
                let id = StatementId::from_raw_index(id);
                self.known_values.string_definitions.get(&id)
            });

        let value = *definitions.next().flatten()?;
        if definitions.any(|x| Some(&value) != x) {
            return None;
        }
        self.dependencys_after
            .0
            .difference_with(&self.variables_assignments[var.unwrap()].0);
        Some(value)
    }

    #[inline]
    fn get_real_parameter_value(&mut self, param: ParameterId<'tag>) -> Option<f64> {
        self.known_values.real_parameters.get(&param).copied()
    }

    #[inline]
    fn get_int_parameter_value(&mut self, param: ParameterId<'tag>) -> Option<i64> {
        self.known_values.int_parameters.get(&param).copied()
    }

    #[inline]
    fn get_str_parameter_value(
        &mut self,
        param: ParameterId<'tag>,
    ) -> Option<CompressedRange<'tag>> {
        self.known_values.string_parameters.get(&param).copied()
    }
}

pub trait ConstantResolver<'tag> {
    fn get_real_variable_value(&mut self, var: VariableId<'tag>) -> Option<f64>;
    fn get_int_variable_value(&mut self, var: VariableId<'tag>) -> Option<i64>;
    fn get_str_variable_value(&mut self, var: VariableId<'tag>) -> Option<CompressedRange<'tag>>;
    fn get_real_parameter_value(&mut self, param: ParameterId<'tag>) -> Option<f64>;
    fn get_int_parameter_value(&mut self, param: ParameterId<'tag>) -> Option<i64>;
    fn get_str_parameter_value(
        &mut self,
        param: ParameterId<'tag>,
    ) -> Option<CompressedRange<'tag>>;
}

pub fn real_constant_fold<'tag>(
    fold: &mut impl ConstantFolder<'tag>,
    resolver: &mut impl ConstantResolver<'tag>,
    expr: RealExpressionId<'tag>,
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
            } else {
                if let Some(false_val) = false_val {
                    false_val
                } else {
                    fold.resolve_to_real_subexpressions(expr, false_val_id);
                    return None;
                }
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

        RealExpression::Vt(Some(temp)) => {
            let temp = fold.real_constant_fold(resolver, temp);
            //TODO abstract over constants
            return None;
        }

        //Temperature/Sim parameters/Branches may be added in the future if there is any demand for it but it doesnt seem useful to me
        RealExpression::Temperature
        | RealExpression::SimParam(_, _)
        | RealExpression::Vt(None)
        | RealExpression::BranchAccess(_, _, _)
        | RealExpression::FunctionCall(_, _)
        | RealExpression::Noise(_, _) => return None,
    };
    Some(res)
}

pub fn int_constant_fold<'tag>(
    fold: &mut impl ConstantFolder<'tag>,
    resolver: &mut impl ConstantResolver<'tag>,
    expr: IntegerExpressionId<'tag>,
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

                    (Some(lhs), Some(rhs)) => 0,

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
                        fold.resolve_to_int_subexpressions(expr, rhs_id);
                        return None;
                    } else {
                        lhs? | rhs?
                    }
                }
                IntegerBinaryOperator::LogicOr => match (lhs, rhs) {
                    (Some(0), Some(0)) => 0,
                    (Some(lhs), Some(rhs)) => 1,
                    (None, None) => return None,
                    (Some(0), None) => {
                        fold.resolve_to_int_subexpressions(expr, rhs_id);
                        return None;
                    }
                    (None, Some(0)) => {
                        fold.resolve_to_int_subexpressions(expr, lhs_id);
                        return None;
                    }
                    (Some(val), None) | (None, Some(val)) => 1,
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
                UnaryOperator::LogicNegate => !(arg != 0) as i64,
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
            (fold.mir().string_literals[lhs?] == fold.mir().string_literals[rhs?]) as i64
        }

        IntegerExpression::StringNEq(lhs, rhs) => {
            let lhs = fold.string_constant_fold(resolver, lhs);
            let rhs = fold.string_constant_fold(resolver, rhs);
            (fold.mir().string_literals[lhs?] != fold.mir().string_literals[rhs?]) as i64
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
            } else {
                if let Some(false_val) = false_val {
                    false_val
                } else {
                    fold.resolve_to_int_subexpressions(expr, false_val_id);
                    return None;
                }
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

pub fn string_constant_fold<'tag>(
    fold: &mut impl ConstantFolder<'tag>,
    resolver: &mut impl ConstantResolver<'tag>,
    expr: StringExpressionId<'tag>,
) -> Option<CompressedRange<'tag>> {
    Some(match fold.mir()[expr].contents {
        StringExpression::Literal(val) => return Some(val),
        StringExpression::VariableReference(var) => resolver.get_str_variable_value(var)?,
        StringExpression::ParameterReference(param) => resolver.get_str_parameter_value(param)?,

        StringExpression::Condition(condition, _, true_val, _, false_val) => {
            let condition = fold.int_constant_fold(resolver, condition);
            let true_val = fold.string_constant_fold(resolver, true_val);
            let false_val = fold.string_constant_fold(resolver, false_val);

            if condition? != 0 {
                true_val?
            } else {
                false_val?
            }
        }
        // TODO system function call constant fold
        StringExpression::SimParam(_) => return None,
    })
}

pub struct ReadingConstantFold<'lt, 'tag>(pub &'lt Mir<'tag>);

impl<'lt, 'tag> ConstantFolder<'tag> for ReadingConstantFold<'lt, 'tag> {
    fn resolve_to_string_subexpressions(
        &mut self,
        _dst: StringExpressionId<'tag>,
        _newval: StringExpressionId<'tag>,
    ) {
    }

    fn resolve_to_int_subexpressions(
        &mut self,
        _dst: IntegerExpressionId<'tag>,
        _newval: IntegerExpressionId<'tag>,
    ) {
    }

    fn resolve_to_real_subexpressions(
        &mut self,
        _dst: RealExpressionId<'tag>,
        _newval: RealExpressionId<'tag>,
    ) {
    }

    fn mir(&self) -> &Mir<'tag> {
        self.0
    }
}

pub struct IntermediateWritingConstantFold<'lt, 'tag>(pub &'lt mut Mir<'tag>);

impl<'lt, 'tag> ConstantFolder<'tag> for IntermediateWritingConstantFold<'lt, 'tag> {
    fn real_constant_fold(
        &mut self,
        resolver: &mut impl ConstantResolver<'tag>,
        expr: RealExpressionId<'tag>,
    ) -> Option<f64> {
        let res = real_constant_fold(self, resolver, expr)?;
        self.0[expr].contents = RealExpression::Literal(res);
        Some(res)
    }

    fn int_constant_fold(
        &mut self,
        resolver: &mut impl ConstantResolver<'tag>,
        expr: IntegerExpressionId<'tag>,
    ) -> Option<i64> {
        let res = int_constant_fold(self, resolver, expr)?;
        self.0[expr].contents = IntegerExpression::Literal(res);
        Some(res)
    }

    fn string_constant_fold(
        &mut self,
        resolver: &mut impl ConstantResolver<'tag>,
        expr: StringExpressionId<'tag>,
    ) -> Option<CompressedRange<'tag>> {
        let res = string_constant_fold(self, resolver, expr)?;
        self.0[expr].contents = StringExpression::Literal(res);
        Some(res)
    }

    fn resolve_to_string_subexpressions(
        &mut self,
        dst: StringExpressionId<'tag>,
        newval: StringExpressionId<'tag>,
    ) {
        self.0[dst] = self.0[newval].clone();
    }

    fn resolve_to_int_subexpressions(
        &mut self,
        dst: IntegerExpressionId<'tag>,
        newval: IntegerExpressionId<'tag>,
    ) {
        self.0[dst] = self.0[newval].clone();
    }

    fn resolve_to_real_subexpressions(
        &mut self,
        dst: RealExpressionId<'tag>,
        newval: RealExpressionId<'tag>,
    ) {
        self.0[dst] = self.0[newval].clone();
    }

    fn mir(&self) -> &Mir<'tag> {
        &self.0
    }
}

pub trait ConstantFolder<'tag>: Sized {
    // We do defaults with external functions because specialization is nowhere near stable
    // This is similar to overwriting methods in OOP

    fn real_constant_fold(
        &mut self,
        resolver: &mut impl ConstantResolver<'tag>,
        expr: RealExpressionId<'tag>,
    ) -> Option<f64> {
        real_constant_fold(self, resolver, expr)
    }

    fn int_constant_fold(
        &mut self,
        resolver: &mut impl ConstantResolver<'tag>,
        expr: IntegerExpressionId<'tag>,
    ) -> Option<i64> {
        int_constant_fold(self, resolver, expr)
    }

    fn string_constant_fold(
        &mut self,
        resolver: &mut impl ConstantResolver<'tag>,
        expr: StringExpressionId<'tag>,
    ) -> Option<CompressedRange<'tag>> {
        string_constant_fold(self, resolver, expr)
    }

    fn resolve_to_string_subexpressions(
        &mut self,
        dst: StringExpressionId<'tag>,
        newval: StringExpressionId<'tag>,
    );
    fn resolve_to_int_subexpressions(
        &mut self,
        dst: IntegerExpressionId<'tag>,
        newval: IntegerExpressionId<'tag>,
    );
    fn resolve_to_real_subexpressions(
        &mut self,
        dst: RealExpressionId<'tag>,
        newval: RealExpressionId<'tag>,
    );

    fn mir(&self) -> &Mir<'tag>;
}

impl<'tag> Mir<'tag> {}

impl<'tag, 'mir> ControlFlowGraph<'tag, 'mir> {
    pub fn constant_fold<'newtag>(
        &mut self,
        fold: &mut impl ConstantFolder<'mir>,
        udg: &mut UseDefGraph<'mir, 'tag>,
        known_values: &mut ConstantFoldState<'mir>,
        write_intermidiate: bool,
    ) {
        self.constant_fold_internal(
            fold,
            udg,
            known_values,
            self.start(),
            None,
            &mut BitSet::with_capacity(udg.statement_count as usize),
            &mut DefiningSet(BitSet::with_capacity(udg.statement_count as usize)),
            write_intermidiate,
        );
    }

    fn constant_fold_internal(
        &mut self,
        fold: &mut impl ConstantFolder<'mir>,
        udg: &mut UseDefGraph<'mir, 'tag>,
        known_values: &mut ConstantFoldState<'mir>,
        start: BasicBlockId<'tag>,
        end: Option<BasicBlockId<'tag>>,
        removed_statements: &mut BitSet,
        temporary_set: &mut DefiningSet,
        write_intermediate: bool,
    ) {
        let mut block = start;
        loop {
            self.constant_fold_basic_block(
                block,
                fold,
                udg,
                known_values,
                removed_statements,
                temporary_set,
                write_intermediate,
            );

            loop {
                match self[block].terminator {
                    Terminator::Split {
                        condition,
                        true_block,
                        false_block,
                        merge,
                    } => {
                        udg.terminator_uses[block]
                            .0
                            .difference_with(removed_statements);
                        temporary_set
                            .0
                            .as_mut_slice()
                            .copy_from_slice(udg.terminator_uses[block].0.as_slice());

                        let folded_condition = fold.int_constant_fold(
                            &mut ConstantPropagator {
                                known_values,
                                dependencys_before: &udg.terminator_uses[block],
                                dependencys_after: temporary_set,
                                variables_assignments: &udg.variables,
                            },
                            condition,
                        );

                        if write_intermediate || folded_condition.is_some() {
                            std::mem::swap(temporary_set, &mut udg.terminator_uses[block]);
                        }

                        match folded_condition {
                            Some(0) => {
                                debug!(
                                    "{}->(false: {}, true: {}) always false (condition: {})",
                                    block, false_block, true_block, condition
                                );
                                self.partial_visit_mut_in_execution_order(
                                    true_block,
                                    Some(merge),
                                    &mut |cfg, block, _| {
                                        cfg.mark_block_dead(block);
                                        removed_statements.extend(
                                            cfg[block]
                                                .statements
                                                .iter()
                                                .map(|stmt| stmt.as_usize()),
                                        );
                                    },
                                );

                                self[block].terminator = self[false_block].terminator;
                                self.constant_fold_basic_block(
                                    false_block,
                                    fold,
                                    udg,
                                    known_values,
                                    removed_statements,
                                    temporary_set,
                                    write_intermediate,
                                );
                                {
                                    let (block, false_block) =
                                        self.blocks.double_mut_borrow(block, false_block);
                                    block.statements.append(&mut false_block.statements);
                                }
                                self.mark_block_dead(false_block)
                            }

                            Some(_) => {
                                if merge == block {
                                    panic!("Found constant infinite loop!")
                                }
                                debug!(
                                    "{}->(false: {}, true: {}) always true (condition: {})",
                                    block, false_block, true_block, condition
                                );
                                self.partial_visit_mut_in_execution_order(
                                    false_block,
                                    Some(merge),
                                    &mut |cfg, block, _| {
                                        cfg.mark_block_dead(block);
                                        removed_statements.extend(
                                            cfg[block]
                                                .statements
                                                .iter()
                                                .map(|stmt| stmt.as_usize()),
                                        );
                                    },
                                );
                                self[block].terminator = self[true_block].terminator;
                                self.constant_fold_basic_block(
                                    true_block,
                                    fold,
                                    udg,
                                    known_values,
                                    removed_statements,
                                    temporary_set,
                                    write_intermediate,
                                );
                                {
                                    let (block, true_block) =
                                        self.blocks.double_mut_borrow(block, true_block);
                                    block.statements.append(&mut true_block.statements);
                                }
                                self.mark_block_dead(true_block)
                            }
                            None => {
                                self.constant_fold_internal(
                                    fold,
                                    udg,
                                    known_values,
                                    true_block,
                                    Some(merge),
                                    removed_statements,
                                    temporary_set,
                                    write_intermediate,
                                );

                                if merge == block {
                                    block = false_block;
                                } else {
                                    self.constant_fold_internal(
                                        fold,
                                        udg,
                                        known_values,
                                        false_block,
                                        Some(merge),
                                        removed_statements,
                                        temporary_set,
                                        write_intermediate,
                                    );
                                    block = merge;
                                }

                                break;
                            }
                        }
                    }

                    Terminator::Merge(next) if Some(next) == end => return,

                    Terminator::End => return,

                    Terminator::Merge(next) => {
                        self[block].terminator = Terminator::Goto(next);
                        debug!("Overwriting merge {} -> {} with goto", block, next);
                        block = next;
                        break;
                    }

                    Terminator::Goto(next) => {
                        block = next;
                        break;
                    }
                }
            }
        }
    }

    fn constant_fold_basic_block(
        &mut self,
        block: BasicBlockId<'tag>,
        fold: &mut impl ConstantFolder<'mir>,
        udg: &mut UseDefGraph<'mir, 'tag>,
        known_values: &mut ConstantFoldState<'mir>,
        removed_statements: &mut BitSet,
        temporary_set: &mut DefiningSet,
        write_intermediate: bool,
    ) {
        for &stmt in self[block].statements.iter().rev() {
            udg.uses[stmt.unwrap()]
                .0
                .difference_with(removed_statements);
            temporary_set
                .0
                .as_mut_slice()
                .copy_from_slice(udg.uses[stmt.unwrap()].0.as_slice());

            let mut resolver = ConstantPropagator {
                known_values,
                dependencys_before: &udg.terminator_uses[block],
                dependencys_after: temporary_set,
                variables_assignments: &udg.variables,
            };

            match fold.mir()[stmt] {
                Statement::Assignment(_, _, val) => {
                    let success = match val {
                        ExpressionId::Real(val) => {
                            if let Some(val) = fold.real_constant_fold(&mut resolver, val) {
                                let old = known_values.real_definitions.insert(stmt, val);
                                if cfg!(debug_assertions) && old.is_some() {
                                    panic!(
                                        "Statement {} was assigned twice (old={},new={})!",
                                        stmt,
                                        old.unwrap(),
                                        val
                                    )
                                }
                                true
                            } else {
                                false
                            }
                        }

                        ExpressionId::Integer(val) => {
                            if let Some(val) = fold.int_constant_fold(&mut resolver, val) {
                                let old = known_values.integer_definitions.insert(stmt, val);
                                if cfg!(debug_assertions) && old.is_some() {
                                    panic!("Statement {} was assigned twice!", stmt)
                                }
                                true
                            } else {
                                false
                            }
                        }
                        ExpressionId::String(val) => {
                            if let Some(val) = fold.string_constant_fold(&mut resolver, val) {
                                let old = known_values.string_definitions.insert(stmt, val);
                                if cfg!(debug_assertions) && old.is_some() {
                                    panic!(
                                        "Statement {} was assigned twice (old={:?},new={:?})!",
                                        stmt,
                                        old.unwrap(),
                                        val
                                    )
                                }
                                true
                            } else {
                                false
                            }
                        }
                    };

                    if success || write_intermediate {
                        std::mem::swap(temporary_set, &mut udg.uses[stmt.unwrap()])
                    }
                }

                Statement::Contribute(_, _, _, val) if write_intermediate => {
                    fold.real_constant_fold(&mut resolver, val);
                    std::mem::swap(temporary_set, &mut udg.uses[stmt.unwrap()])
                }

                _ => (),
            }
        }
    }
}
