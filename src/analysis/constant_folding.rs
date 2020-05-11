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

impl<'tag> Mir<'tag> {
    pub fn real_constant_fold(
        &mut self,
        expr: RealExpressionId<'tag>,
        resolver: &mut impl ConstantResolver<'tag>,
        write_intermediate: bool,
    ) -> Option<f64> {
        let res = match self[expr].contents {
            RealExpression::Literal(val) => return Some(val),
            RealExpression::VariableReference(var) => resolver.get_real_variable_value(var)?,
            RealExpression::ParameterReference(param) => {
                resolver.get_real_parameter_value(param)?
            }

            RealExpression::BinaryOperator(lhs_id, op, rhs_id) => {
                let lhs = self.real_constant_fold(lhs_id, resolver, write_intermediate);
                let rhs = self.real_constant_fold(rhs_id, resolver, write_intermediate);
                match op.contents {
                    RealBinaryOperator::Sum => match (lhs, rhs) {
                        (Some(lhs), Some(rhs)) => lhs + rhs,
                        (Some(0.0), None) if write_intermediate => {
                            self[expr] = self[rhs_id].clone();
                            return None;
                        }
                        (None, Some(0.0)) if write_intermediate => {
                            self[expr] = self[lhs_id].clone();
                            return None;
                        }
                        (_, _) => return None,
                    },
                    RealBinaryOperator::Subtract => match (lhs, rhs) {
                        (Some(lhs), Some(rhs)) => lhs - rhs,
                        (Some(0.0), None) if write_intermediate => {
                            self[expr].contents = RealExpression::Negate(op.source, rhs_id);

                            return None;
                        }
                        (None, Some(0.0)) if write_intermediate => {
                            self[expr] = self[lhs_id].clone();
                            return None;
                        }
                        (_, _) => return None,
                    },
                    RealBinaryOperator::Multiply => match (lhs, rhs) {
                        (Some(0.0), _) | (_, Some(0.0)) => 0.0,
                        (Some(lhs), Some(rhs)) => lhs * rhs,
                        (Some(1.0), None) if write_intermediate => {
                            self[expr] = self[rhs_id].clone();
                            return None;
                        }
                        (None, Some(1.0)) if write_intermediate => {
                            self[expr] = self[lhs_id].clone();
                            return None;
                        }
                        (_, _) => return None,
                    },
                    RealBinaryOperator::Divide => match (lhs, rhs) {
                        (Some(0.0), _) => 0.0,
                        (Some(lhs), Some(rhs)) => lhs / rhs,
                        (None, Some(1.0)) if write_intermediate => {
                            self[expr] = self[lhs_id].clone();
                            return None;
                        }
                        (_, _) => return None,
                    },
                    RealBinaryOperator::Exponent => {
                        if rhs == Some(0.0) {
                            1.0
                        } else if lhs == Some(0.0) {
                            0.0
                        } else {
                            lhs?.powf(rhs?)
                        }
                    }
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

            RealExpression::Negate(_, val) => {
                -self.real_constant_fold(val, resolver, write_intermediate)?
            }

            RealExpression::Condition(condition, _, true_val_id, _, false_val_id) => {
                let condition = self.int_constant_fold(condition, resolver, write_intermediate);
                let true_val = self.real_constant_fold(true_val_id, resolver, write_intermediate);
                let false_val = self.real_constant_fold(false_val_id, resolver, write_intermediate);

                if condition? != 0 {
                    if let Some(true_val) = true_val {
                        true_val
                    } else {
                        if write_intermediate {
                            self[expr] = self[true_val_id].clone()
                        }
                        return None;
                    }
                } else {
                    if let Some(false_val) = false_val {
                        false_val
                    } else {
                        if write_intermediate {
                            self[expr] = self[false_val_id].clone()
                        }
                        return None;
                    }
                }
            }

            RealExpression::BuiltInFunctionCall1p(call, arg) => {
                let arg = self.real_constant_fold(arg, resolver, write_intermediate)?;
                match call {
                    BuiltInFunctionCall1p::Ln => arg.ln(),
                    BuiltInFunctionCall1p::Sqrt => arg.sqrt(),
                    BuiltInFunctionCall1p::Exp => arg.exp(),
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

            RealExpression::BuiltInFunctionCall2p(call, arg1, arg2) => {
                let arg1 = self.real_constant_fold(arg1, resolver, write_intermediate);
                let arg2 = self.real_constant_fold(arg2, resolver, write_intermediate);
                match call {
                    BuiltInFunctionCall2p::Pow => {
                        if arg2 == Some(0.0) {
                            1.0
                        } else if arg1 == Some(0.0) {
                            0.0
                        } else {
                            arg1?.powf(arg2?)
                        }
                    }
                    BuiltInFunctionCall2p::Hypot => arg1?.hypot(arg2?),
                    BuiltInFunctionCall2p::Min => arg1?.min(arg2?),
                    BuiltInFunctionCall2p::Max => arg1?.max(arg2?),
                    BuiltInFunctionCall2p::ArcTan2 => arg1?.atan2(arg2?),
                }
            }

            RealExpression::IntegerConversion(expr) => {
                self.int_constant_fold(expr, resolver, write_intermediate)? as f64
            }

            //definitely not doing constant functions. Temperature/Sim parameters/Branches may be an option in the future if there is any use for it
            RealExpression::SystemFunctionCall(_)
            | RealExpression::BranchAccess(_, _)
            | RealExpression::FunctionCall(_, _) => return None,
        };
        if write_intermediate {
            self[expr].contents = RealExpression::Literal(res);
        }
        Some(res)
    }

    pub fn int_constant_fold(
        &mut self,
        expr: IntegerExpressionId<'tag>,
        resolver: &mut impl ConstantResolver<'tag>,
        write_intermediate: bool,
    ) -> Option<i64> {
        let res = match self[expr].contents {
            IntegerExpression::Literal(val) => return Some(val),
            IntegerExpression::ParameterReference(param) => {
                resolver.get_int_parameter_value(param)?
            }
            IntegerExpression::VariableReference(var) => resolver.get_int_variable_value(var)?,

            IntegerExpression::Abs(val) => self
                .int_constant_fold(val, resolver, write_intermediate)?
                .abs(),

            IntegerExpression::Min(arg1, arg2) => {
                let arg1 = self.int_constant_fold(arg1, resolver, write_intermediate);
                let arg2 = self.int_constant_fold(arg2, resolver, write_intermediate);
                arg1?.min(arg2?)
            }

            IntegerExpression::Max(arg1, arg2) => {
                let arg1 = self.int_constant_fold(arg1, resolver, write_intermediate);
                let arg2 = self.int_constant_fold(arg2, resolver, write_intermediate);
                arg1?.max(arg2?)
            }

            IntegerExpression::BinaryOperator(lhs_id, op, rhs_id) => {
                let lhs = self.int_constant_fold(lhs_id, resolver, write_intermediate);
                let rhs = self.int_constant_fold(rhs_id, resolver, write_intermediate);
                match op.contents {
                    IntegerBinaryOperator::Sum => match (lhs, rhs) {
                        (Some(lhs), Some(rhs)) => lhs + rhs,
                        (Some(0), None) if write_intermediate => {
                            self[expr] = self[rhs_id].clone();
                            return None;
                        }
                        (None, Some(0)) if write_intermediate => {
                            self[expr] = self[lhs_id].clone();
                            return None;
                        }
                        (_, _) => return None,
                    },
                    IntegerBinaryOperator::Subtract => match (lhs, rhs) {
                        (Some(lhs), Some(rhs)) => lhs - rhs,
                        (Some(0), None) if write_intermediate => {
                            self[expr].contents = IntegerExpression::UnaryOperator(
                                op.clone_as(UnaryOperator::ArithmeticNegate),
                                rhs_id,
                            );

                            return None;
                        }
                        (None, Some(0)) if write_intermediate => {
                            self[expr] = self[lhs_id].clone();
                            return None;
                        }
                        (_, _) => return None,
                    },
                    IntegerBinaryOperator::Multiply => match (lhs, rhs) {
                        (Some(0), _) | (_, Some(0)) => 0,
                        (Some(lhs), Some(rhs)) => lhs * rhs,
                        (Some(1), None) if write_intermediate => {
                            self[expr] = self[rhs_id].clone();
                            return None;
                        }
                        (None, Some(1)) if write_intermediate => {
                            self[expr] = self[lhs_id].clone();
                            return None;
                        }
                        (_, _) => return None,
                    },
                    IntegerBinaryOperator::Divide => match (lhs, rhs) {
                        (Some(0), _) => 0,
                        (Some(lhs), Some(rhs)) => lhs / rhs,
                        (None, Some(1)) if write_intermediate => {
                            self[expr] = self[lhs_id].clone();
                            return None;
                        }
                        (_, _) => return None,
                    },
                    IntegerBinaryOperator::Exponent => {
                        if rhs == Some(0) {
                            1
                        } else if lhs == Some(0) {
                            0
                        } else {
                            lhs?.pow(rhs? as u32)
                        }
                    }
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
                        } else if rhs == Some(0) && lhs == None && write_intermediate {
                            self[expr] = self[lhs_id].clone();
                            return None;
                        } else {
                            lhs? << rhs?
                        }
                    }
                    IntegerBinaryOperator::ShiftRight => {
                        if lhs == Some(0) {
                            0
                        } else if rhs == Some(0) && lhs == None && write_intermediate {
                            self[expr] = self[lhs_id].clone();
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
                            self[expr] = self[rhs_id].clone();
                            return None;
                        } else if rhs == Some(0) && lhs.is_none() {
                            self[expr] = self[lhs_id].clone();
                            return None;
                        } else {
                            lhs? | rhs?
                        }
                    }
                    IntegerBinaryOperator::LogicOr => match (lhs, rhs) {
                        (Some(0), Some(0)) => 0,
                        (Some(lhs), Some(rhs)) => {
                            debug!("Or is true with {}||{}", lhs, rhs);
                            1
                        }
                        (None, None) => return None,
                        (Some(0), None) => {
                            if write_intermediate {
                                self[expr] = self[rhs_id].clone();
                            }
                            return None;
                        }
                        (None, Some(0)) => {
                            if write_intermediate {
                                self[expr] = self[lhs_id].clone();
                            }
                            return None;
                        }
                        (Some(val), None) | (None, Some(val)) => {
                            debug!("Or is true with {}||unevaluated", val);
                            1
                        }
                    },
                    IntegerBinaryOperator::LogicAnd => match (lhs, rhs) {
                        (Some(0), Some(0)) => 0,
                        (Some(0), _) | (_, Some(0)) => 0,
                        (Some(_), Some(_)) => 1,
                        (None, None) => return None,
                        (Some(_), None) => {
                            if write_intermediate {
                                self[expr] = self[rhs_id].clone();
                            }
                            return None;
                        }
                        (None, Some(_)) => {
                            if write_intermediate {
                                self[expr] = self[lhs_id].clone();
                            }
                            return None;
                        }
                    },
                }
            }

            IntegerExpression::UnaryOperator(op, arg) => {
                let arg = self.int_constant_fold(arg, resolver, write_intermediate)?;
                match op.contents {
                    UnaryOperator::BitNegate => !arg,
                    UnaryOperator::LogicNegate => !(arg != 0) as i64,
                    UnaryOperator::ArithmeticNegate => -arg,
                    UnaryOperator::ExplicitPositive => arg,
                }
            }

            IntegerExpression::IntegerComparison(lhs, op, rhs) => {
                let lhs = self.int_constant_fold(lhs, resolver, write_intermediate);
                let rhs = self.int_constant_fold(rhs, resolver, write_intermediate);
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
                let lhs = self.real_constant_fold(lhs, resolver, write_intermediate);
                let rhs = self.real_constant_fold(rhs, resolver, write_intermediate);
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
                let lhs = self.string_constant_fold(lhs, resolver, write_intermediate);
                let rhs = self.string_constant_fold(rhs, resolver, write_intermediate);
                (self.string_literals[lhs?] == self.string_literals[rhs?]) as i64
            }

            IntegerExpression::StringNEq(lhs, rhs) => {
                let lhs = self.string_constant_fold(lhs, resolver, write_intermediate);
                let rhs = self.string_constant_fold(rhs, resolver, write_intermediate);
                (self.string_literals[lhs?] != self.string_literals[rhs?]) as i64
            }

            IntegerExpression::Condition(condition, _, true_val_id, _, false_val_id) => {
                let condition = self.int_constant_fold(condition, resolver, write_intermediate);
                let true_val = self.int_constant_fold(true_val_id, resolver, write_intermediate);
                let false_val = self.int_constant_fold(false_val_id, resolver, write_intermediate);

                if condition? != 0 {
                    if let Some(true_val) = true_val {
                        true_val
                    } else {
                        if write_intermediate {
                            self[expr] = self[true_val_id].clone()
                        }
                        return None;
                    }
                } else {
                    if let Some(false_val) = false_val {
                        false_val
                    } else {
                        if write_intermediate {
                            self[expr] = self[false_val_id].clone()
                        }
                        return None;
                    }
                }
            }

            IntegerExpression::RealCast(val) => self
                .real_constant_fold(val, resolver, write_intermediate)?
                .round() as i64,

            IntegerExpression::NetReference(_)
            | IntegerExpression::PortReference(_)
            | IntegerExpression::FunctionCall(_, _) => return None,
        };
        if write_intermediate {
            self[expr].contents = IntegerExpression::Literal(res);
        }
        Some(res)
    }

    pub fn string_constant_fold(
        &mut self,
        expr: StringExpressionId<'tag>,
        resolver: &mut impl ConstantResolver<'tag>,
        write_intermediate: bool,
    ) -> Option<CompressedRange<'tag>> {
        let res = match self[expr].contents {
            StringExpression::Literal(val) => return Some(val),
            StringExpression::VariableReference(var) => resolver.get_str_variable_value(var)?,
            StringExpression::ParameterReference(param) => {
                resolver.get_str_parameter_value(param)?
            }

            StringExpression::Condition(condition, _, true_val, _, false_val) => {
                let condition = self.int_constant_fold(condition, resolver, write_intermediate);
                let true_val = self.string_constant_fold(true_val, resolver, write_intermediate);
                let false_val = self.string_constant_fold(false_val, resolver, write_intermediate);

                if condition? != 0 {
                    true_val?
                } else {
                    false_val?
                }
            }
        };
        if write_intermediate {
            self[expr].contents = StringExpression::Literal(res);
        }
        Some(res)
    }
}

impl<'tag, 'mir> ControlFlowGraph<'tag, 'mir> {
    pub fn constant_fold<'newtag>(
        &mut self,
        mir: &mut Mir<'mir>,
        udg: &mut UseDefGraph<'mir, 'tag>,
        known_values: &mut ConstantFoldState<'mir>,
        write_to_mir: bool,
    ) {
        self.constant_fold_internal(
            mir,
            udg,
            known_values,
            self.start(),
            None,
            write_to_mir,
            &mut BitSet::with_capacity(udg.statement_count as usize),
            &mut DefiningSet(BitSet::with_capacity(udg.statement_count as usize)),
        );
    }

    fn constant_fold_internal(
        &mut self,
        mir: &mut Mir<'mir>,
        udg: &mut UseDefGraph<'mir, 'tag>,

        known_values: &mut ConstantFoldState<'mir>,
        start: BasicBlockId<'tag>,
        end: Option<BasicBlockId<'tag>>,
        write_to_mir: bool,
        removed_statements: &mut BitSet,
        temporary_set: &mut DefiningSet,
    ) {
        let mut block = start;
        loop {
            self.constant_fold_basic_block(
                block,
                mir,
                udg,
                known_values,
                write_to_mir,
                removed_statements,
                temporary_set,
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

                        match mir.int_constant_fold(
                            condition,
                            &mut ConstantPropagator {
                                known_values,
                                dependencys_before: &udg.terminator_uses[block],
                                dependencys_after: temporary_set,
                                variables_assignments: &udg.variables,
                            },
                            write_to_mir,
                        ) {
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
                                    mir,
                                    udg,
                                    known_values,
                                    write_to_mir,
                                    removed_statements,
                                    temporary_set,
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
                                    mir,
                                    udg,
                                    known_values,
                                    write_to_mir,
                                    removed_statements,
                                    temporary_set,
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
                                    mir,
                                    udg,
                                    known_values,
                                    true_block,
                                    Some(merge),
                                    write_to_mir,
                                    removed_statements,
                                    temporary_set,
                                );

                                if merge == block {
                                    block = false_block;
                                } else {
                                    self.constant_fold_internal(
                                        mir,
                                        udg,
                                        known_values,
                                        false_block,
                                        Some(merge),
                                        write_to_mir,
                                        removed_statements,
                                        temporary_set,
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
        mir: &mut Mir<'mir>,
        udg: &mut UseDefGraph<'mir, 'tag>,
        known_values: &mut ConstantFoldState<'mir>,
        write_to_mir: bool,
        removed_statements: &mut BitSet,
        temporary_set: &mut DefiningSet,
    ) {
        for &stmt in self[block].statements.iter().rev() {
            match mir[stmt] {
                Statement::Assignment(_, _, val) => {
                    udg.uses[stmt.unwrap()]
                        .0
                        .difference_with(removed_statements);
                    temporary_set
                        .0
                        .as_mut_slice()
                        .copy_from_slice(udg.uses[stmt.unwrap()].0.as_slice());
                    match val {
                        ExpressionId::Real(val) => {
                            if let Some(val) = mir.real_constant_fold(
                                val,
                                &mut ConstantPropagator {
                                    known_values,
                                    dependencys_before: &udg.terminator_uses[block],
                                    dependencys_after: temporary_set,
                                    variables_assignments: &udg.variables,
                                },
                                write_to_mir,
                            ) {
                                let old = known_values.real_definitions.insert(stmt, val);
                                if cfg!(debug_assertions) && old.is_some() {
                                    panic!(
                                        "Statement {} was assigned twice (old={},new={})!",
                                        stmt,
                                        old.unwrap(),
                                        val
                                    )
                                }
                            }
                        }

                        ExpressionId::Integer(val) => {
                            if let Some(val) = mir.int_constant_fold(
                                val,
                                &mut ConstantPropagator {
                                    known_values,
                                    dependencys_before: &udg.terminator_uses[block],
                                    dependencys_after: temporary_set,
                                    variables_assignments: &udg.variables,
                                },
                                write_to_mir,
                            ) {
                                let old = known_values.integer_definitions.insert(stmt, val);
                                if cfg!(debug_assertions) && old.is_some() {
                                    panic!("Statement {} was assigned twice!", stmt)
                                }
                            }
                        }
                        ExpressionId::String(val) => {
                            if let Some(val) = mir.string_constant_fold(
                                val,
                                &mut ConstantPropagator {
                                    known_values,
                                    dependencys_before: &udg.terminator_uses[block],
                                    dependencys_after: temporary_set,
                                    variables_assignments: &udg.variables,
                                },
                                write_to_mir,
                            ) {
                                let old = known_values.string_definitions.insert(stmt, val);
                                debug_assert!(old.is_none())
                            }
                        }
                    }
                    std::mem::swap(temporary_set, &mut udg.uses[stmt.unwrap()])
                }

                Statement::Contribute(_, _, _, val) if write_to_mir => {
                    mir.real_constant_fold(
                        val,
                        &mut ConstantPropagator {
                            known_values,
                            dependencys_before: &udg.terminator_uses[block],
                            dependencys_after: temporary_set,
                            variables_assignments: &udg.variables,
                        },
                        true,
                    );
                }

                _ => (),
            }
        }
    }
}
