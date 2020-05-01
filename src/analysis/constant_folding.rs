use crate::ast::UnaryOperator;
use crate::compact_arena::{CompressedRange, InvariantLifetime};
use crate::hir_lowering::error::Error;
use crate::ir::mir::control_flow_graph::BasicBlockId;
use crate::ir::mir::{ControlFlowGraph, Mir};
use crate::ir::{
    BranchId, BuiltInFunctionCall1p, BuiltInFunctionCall2p, IntegerExpressionId, Node, ParameterId,
    RealExpressionId, SafeRangeCreation, StringExpressionId, VariableId,
};
use crate::mir::control_flow_graph::Terminator;
use crate::mir::{
    ComparisonOperator, ExpressionId, IntegerBinaryOperator, IntegerExpression, RealBinaryOperator,
    RealExpression, Statement, StringExpression, VariableType,
};
use crate::util::RefMut;
use ansi_term::Color;
use log::*;
use rustc_hash::{FxHashMap, FxHashSet};
use std::convert::TryFrom;

#[derive(Clone, Debug, Default)]
pub struct Constants<'tag> {
    pub real_variables: FxHashMap<VariableId<'tag>, f64>,
    pub int_variables: FxHashMap<VariableId<'tag>, i64>,
    pub string_variables: FxHashMap<VariableId<'tag>, CompressedRange<'tag>>,
    pub real_parameters: FxHashMap<ParameterId<'tag>, f64>,
    pub int_parameters: FxHashMap<ParameterId<'tag>, i64>,
    pub string_parameters: FxHashMap<ParameterId<'tag>, CompressedRange<'tag>>,
}
impl<'tag> Constants<'tag> {
    pub fn retain_unchanged_variables(&mut self, other: &mut Self, mir: &mut Mir<'tag>) {
        self.real_variables.retain(|var, &mut val| {
            if other.real_variables.get(var).copied() == Some(val) {
                if !mir[*var].contents.name.as_str().starts_with('∂') {
                    trace!(
                        "retaining constant fold {} = {}",
                        mir[*var].contents.name,
                        val
                    );
                }
                true
            } else {
                false
            }
        });
        self.int_variables.retain(|var, &mut val| {
            if other.int_variables.get(var).copied() == Some(val) {
                if !mir[*var].contents.name.as_str().starts_with('∂') {
                    trace!(
                        "retaining constant fold {} = {}",
                        mir[*var].contents.name,
                        val
                    );
                }
                true
            } else {
                false
            }
        });
        self.string_variables
            .retain(|var, &mut val| match other.string_variables.get(var) {
                Some(&range) if &mir.string_literals[range] == &mir.string_literals[val] => {
                    if !mir[*var].contents.name.as_str().starts_with('∂') {
                        trace!(
                            "retaining constant fold {} = {}",
                            mir[*var].contents.name,
                            &mir.string_literals[val]
                        );
                    }
                    true
                }
                _ => false,
            });
    }
}

impl<'tag> Mir<'tag> {
    pub fn try_real_constant_fold(
        &mut self,
        expr: RealExpressionId<'tag>,
        known_values: &Constants<'tag>,
        write_intermediate: bool,
    ) -> Option<f64> {
        let res = match self[expr].contents {
            RealExpression::Literal(val) => return Some(val),
            RealExpression::VariableReference(var) => *known_values.real_variables.get(&var)?,
            RealExpression::ParameterReference(param) => {
                *known_values.real_parameters.get(&param)?
            }

            RealExpression::BinaryOperator(lhs_id, op, rhs_id) => {
                let lhs = self.try_real_constant_fold(lhs_id, known_values, write_intermediate);
                let rhs = self.try_real_constant_fold(rhs_id, known_values, write_intermediate);
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
                -self.try_real_constant_fold(val, known_values, write_intermediate)?
            }

            RealExpression::Condition(condition, _, true_val_id, _, false_val_id) => {
                let condition =
                    self.try_int_constant_fold(condition, known_values, write_intermediate);
                let true_val =
                    self.try_real_constant_fold(true_val_id, known_values, write_intermediate);
                let false_val =
                    self.try_real_constant_fold(false_val_id, known_values, write_intermediate);

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
                let arg = self.try_real_constant_fold(arg, known_values, write_intermediate)?;
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
                let arg1 = self.try_real_constant_fold(arg1, known_values, write_intermediate);
                let arg2 = self.try_real_constant_fold(arg2, known_values, write_intermediate);
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
                self.try_int_constant_fold(expr, known_values, write_intermediate)? as f64
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

    pub fn try_int_constant_fold(
        &mut self,
        expr: IntegerExpressionId<'tag>,
        known_values: &Constants<'tag>,
        write_intermediate: bool,
    ) -> Option<i64> {
        let res = match self[expr].contents {
            IntegerExpression::Literal(val) => return Some(val),
            IntegerExpression::ParameterReference(param) => {
                *known_values.int_parameters.get(&param)?
            }
            IntegerExpression::VariableReference(var) => *known_values.int_variables.get(&var)?,

            IntegerExpression::Abs(val) => self
                .try_int_constant_fold(val, known_values, write_intermediate)?
                .abs(),

            IntegerExpression::Min(arg1, arg2) => {
                let arg1 = self.try_int_constant_fold(arg1, known_values, write_intermediate);
                let arg2 = self.try_int_constant_fold(arg2, known_values, write_intermediate);
                arg1?.min(arg2?)
            }

            IntegerExpression::Max(arg1, arg2) => {
                let arg1 = self.try_int_constant_fold(arg1, known_values, write_intermediate);
                let arg2 = self.try_int_constant_fold(arg2, known_values, write_intermediate);
                arg1?.max(arg2?)
            }

            IntegerExpression::BinaryOperator(lhs_id, op, rhs_id) => {
                let lhs = self.try_int_constant_fold(lhs_id, known_values, write_intermediate);
                let rhs = self.try_int_constant_fold(rhs_id, known_values, write_intermediate);
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
                let arg = self.try_int_constant_fold(arg, known_values, write_intermediate)?;
                match op.contents {
                    UnaryOperator::BitNegate => !arg,
                    UnaryOperator::LogicNegate => !(arg != 0) as i64,
                    UnaryOperator::ArithmeticNegate => -arg,
                    UnaryOperator::ExplicitPositive => arg,
                }
            }

            IntegerExpression::IntegerComparison(lhs, op, rhs) => {
                let lhs = self.try_int_constant_fold(lhs, known_values, write_intermediate);
                let rhs = self.try_int_constant_fold(rhs, known_values, write_intermediate);
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
                let lhs = self.try_real_constant_fold(lhs, known_values, write_intermediate);
                let rhs = self.try_real_constant_fold(rhs, known_values, write_intermediate);
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
                let lhs = self.try_string_constant_fold(lhs, known_values, write_intermediate);
                let rhs = self.try_string_constant_fold(rhs, known_values, write_intermediate);
                (self.string_literals[lhs?] == self.string_literals[rhs?]) as i64
            }

            IntegerExpression::StringNEq(lhs, rhs) => {
                let lhs = self.try_string_constant_fold(lhs, known_values, write_intermediate);
                let rhs = self.try_string_constant_fold(rhs, known_values, write_intermediate);
                (self.string_literals[lhs?] != self.string_literals[rhs?]) as i64
            }

            IntegerExpression::Condition(condition, _, true_val_id, _, false_val_id) => {
                let condition =
                    self.try_int_constant_fold(condition, known_values, write_intermediate);
                let true_val =
                    self.try_int_constant_fold(true_val_id, known_values, write_intermediate);
                let false_val =
                    self.try_int_constant_fold(false_val_id, known_values, write_intermediate);

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
                .try_real_constant_fold(val, known_values, write_intermediate)?
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

    pub fn try_string_constant_fold(
        &mut self,
        expr: StringExpressionId<'tag>,
        known_values: &Constants<'tag>,
        write_intermediate: bool,
    ) -> Option<CompressedRange<'tag>> {
        let res = match self[expr].contents {
            StringExpression::Literal(val) => return Some(val),
            StringExpression::VariableReference(var) => *known_values.string_variables.get(&var)?,
            StringExpression::ParameterReference(param) => {
                *known_values.string_parameters.get(&param)?
            }

            StringExpression::Condition(condition, _, true_val, _, false_val) => {
                let condition =
                    self.try_int_constant_fold(condition, known_values, write_intermediate);
                let true_val =
                    self.try_string_constant_fold(true_val, known_values, write_intermediate);
                let false_val =
                    self.try_string_constant_fold(false_val, known_values, write_intermediate);

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
#[macro_export]
macro_rules! constant_fold {
    ($cfg:ident,$mir:expr,$known_values:expr,$write_to_mir:expr) => {
        let tag = $crate::compact_arena::invariant_lifetime();
        let _guard;
        let mut $cfg = unsafe {
            // this is not per-se unsafe but we need it to be public and
            // calling it with a non-unique `tag` would allow arena mixups,
            // which may introduce UB in `Index`/`IndexMut`
            $cfg.constant_fold($mir, $known_values, $write_to_mir, tag)
        };
        // this doesn't make it to MIR, but ensures that borrowck will not
        // unify the lifetimes of two macro calls by binding the lifetime to
        // drop scope
        if false {
            struct Guard<'tag>(&'tag $crate::compact_arena::InvariantLifetime<'tag>);
            impl<'tag> ::core::ops::Drop for Guard<'tag> {
                fn drop(&mut self) {}
            }
            _guard = Guard(&tag);
        }
    };
}

impl<'tag, 'mir> ControlFlowGraph<'tag, 'mir> {
    pub unsafe fn constant_fold<'newtag>(
        mut self,
        mir: &mut Mir<'mir>,
        known_values: &mut Constants<'mir>,
        write_to_mir: bool,
        tag: InvariantLifetime<'newtag>,
    ) -> ControlFlowGraph<'newtag, 'mir> {
        let mut retain =
            FxHashSet::with_capacity_and_hasher(self.block_count() as usize, Default::default());
        self.constant_fold_internal(
            mir,
            known_values,
            self.start(),
            None,
            write_to_mir,
            &mut retain,
        );
        self.retain(tag, |block| retain.contains(&block))
    }

    fn constant_fold_internal<'values, 'loop_values: 'values>(
        &mut self,
        mir: &mut Mir<'mir>,
        known_values: &'values mut Constants<'mir>,
        start: BasicBlockId<'tag>,
        return_on_merge: Option<BasicBlockId<'tag>>,
        write_to_mir: bool,
        blocks_to_retain: &mut FxHashSet<BasicBlockId<'tag>>,
    ) {
        let mut current = start;
        loop {
            self.constant_fold_basic_block(mir, known_values, current, write_to_mir);
            blocks_to_retain.insert(current);

            // println!("Processing {:?}", current);
            loop {
                match self.blocks[current].terminator {
                    Terminator::Goto(next) => {
                        current = next;
                        break;
                    }

                    Terminator::End => return,
                    Terminator::Merge(next) if Some(next) == return_on_merge => return,

                    Terminator::Merge(next) => {
                        trace!(
                            "{} merge {:?} -> {:?} (returning on {:?})",
                            Color::Yellow.paint("Ignored"),
                            current,
                            next,
                            return_on_merge
                        );
                        self.constant_fold_basic_block(mir, known_values, next, write_to_mir);
                        let stmts_to_add = core::mem::take(&mut self.blocks[next].statements);
                        self.blocks[current].statements.extend(stmts_to_add);
                        self.blocks[current].terminator = self.blocks[next].terminator;
                    }

                    Terminator::Split {
                        condition,
                        true_block,
                        false_block,
                        merge,
                    } => {
                        if merge == current {
                            self.partial_visit_mut_in_execution_order(
                                true_block,
                                Some(merge),
                                &mut |cfg, block| {
                                    cfg.tracked_modified_variables(mir, known_values, block)
                                },
                            );
                            match mir.try_int_constant_fold(condition, known_values, write_to_mir) {
                                Some(0) => {
                                    warn!(
                                        "{} :found impossible loop!",
                                        Color::Yellow.paint("warning")
                                    );
                                    //TODO better warning infrastructure
                                    self.constant_fold_basic_block(
                                        mir,
                                        known_values,
                                        false_block,
                                        write_to_mir,
                                    );
                                    let stmts_to_add =
                                        core::mem::take(&mut self.blocks[false_block].statements);
                                    self.blocks[current].statements.extend(stmts_to_add);
                                    self.blocks[current].terminator =
                                        self.blocks[false_block].terminator;
                                }
                                Some(1) => {
                                    panic!(
                                        "{} :found constant infinite loop!",
                                        Color::Red.paint("error")
                                    );
                                }
                                _ => {
                                    let mut loop_values = known_values.clone();
                                    self.constant_fold_internal(
                                        mir,
                                        &mut loop_values,
                                        true_block,
                                        Some(merge),
                                        write_to_mir,
                                        blocks_to_retain,
                                    );
                                    current = false_block;
                                    break;
                                }
                            }
                        } else {
                            match mir.try_int_constant_fold(condition, known_values, write_to_mir) {
                                Some(0) => {
                                    trace!(
                                        "Constant at split {} evaluated to false_branch: {} merge at {}",
                                        current,
                                        Color::Blue.paint("Ignoring"),
                                        merge
                                    );
                                    self.constant_fold_basic_block(
                                        mir,
                                        known_values,
                                        false_block,
                                        write_to_mir,
                                    );
                                    let stmts_to_add =
                                        core::mem::take(&mut self.blocks[false_block].statements);
                                    self.blocks[current].statements.extend(stmts_to_add);
                                    self.blocks[current].terminator =
                                        self.blocks[false_block].terminator;
                                }
                                Some(_) => {
                                    trace!(
                                        "Constant at split {} evaluated to true_branch: {} merge at {:?}",
                                        current,
                                        Color::Blue.paint("Ignoring"),
                                        merge
                                    );
                                    self.constant_fold_basic_block(
                                        mir,
                                        known_values,
                                        true_block,
                                        write_to_mir,
                                    );
                                    let stmts_to_add =
                                        core::mem::take(&mut self.blocks[true_block].statements);
                                    self.blocks[current].statements.extend(stmts_to_add);
                                    self.blocks[current].terminator =
                                        self.blocks[true_block].terminator;
                                }
                                None => {
                                    let mut true_values = known_values.clone();
                                    trace!("entering branch");
                                    self.constant_fold_internal(
                                        mir,
                                        &mut true_values,
                                        true_block,
                                        Some(merge),
                                        write_to_mir,
                                        blocks_to_retain,
                                    );
                                    self.constant_fold_internal(
                                        mir,
                                        known_values,
                                        false_block,
                                        Some(merge),
                                        write_to_mir,
                                        blocks_to_retain,
                                    );
                                    known_values.retain_unchanged_variables(&mut true_values, mir);
                                    trace!("branch done");
                                    current = merge;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    fn tracked_modified_variables(
        &mut self,
        mir: &mut Mir<'mir>,
        known_values: &mut Constants<'mir>,
        basic_block: BasicBlockId<'tag>,
    ) {
        for stmt in self.blocks[basic_block].statements.iter().copied() {
            if let Statement::Assignment(_, var, expr) = mir[stmt] {
                match expr {
                    ExpressionId::Real(expr) => {
                        match mir.try_real_constant_fold(expr, known_values, false) {
                            Some(val) if known_values.real_variables.get(&var) == Some(&val) => {
                                trace!("Keeping constant fold {} = {}", mir[var].contents.name, val)
                            }
                            _ => {
                                known_values.real_variables.remove(&var);
                            }
                        }
                    }

                    ExpressionId::Integer(expr) => {
                        match mir.try_int_constant_fold(expr, known_values, false) {
                            Some(val) if known_values.int_variables.get(&var) == Some(&val) => {
                                trace!("Keeping constant fold {} = {}", mir[var].contents.name, val)
                            }
                            _ => {
                                known_values.int_variables.remove(&var);
                            }
                        }
                    }

                    ExpressionId::String(expr) => {
                        match mir.try_string_constant_fold(expr, known_values, false) {
                            Some(val) => match known_values.string_variables.get(&var) {
                                Some(&original_val)
                                    if &mir.string_literals[original_val]
                                        == &mir.string_literals[val] =>
                                {
                                    trace!(
                                        "Keeping constant fold {} = {}",
                                        mir[var].contents.name,
                                        &mir.string_literals[val]
                                    )
                                }
                                _ => {
                                    known_values.real_variables.remove(&var);
                                }
                            },
                            _ => {
                                known_values.string_variables.remove(&var);
                            }
                        }
                    }
                }
            }
        }
    }

    fn constant_fold_basic_block(
        &mut self,
        mir: &mut Mir<'mir>,
        known_values: &mut Constants<'mir>,
        basic_block: BasicBlockId<'tag>,
        write_to_mir: bool,
    ) {
        self.blocks[basic_block]
            .statements
            .retain(|&stmt| match mir[stmt] {
                Statement::Assignment(_, var, expr) => match expr {
                    ExpressionId::Real(expr) => {
                        match mir.try_real_constant_fold(expr, known_values, write_to_mir) {
                            Some(val) => {
                                if !mir[var].contents.name.as_str().starts_with('∂') {
                                    trace!(
                                        "Variable {} constant folded to {}",
                                        mir[var].contents.name,
                                        val
                                    );
                                }
                                known_values
                                    .real_variables
                                    .insert(var, val)
                                    .map_or(true, |old_val| old_val != val)
                            }
                            None => {
                                known_values.real_variables.remove(&var);
                                true
                            }
                        }
                    }

                    ExpressionId::Integer(expr) => {
                        match mir.try_int_constant_fold(expr, known_values, write_to_mir) {
                            Some(val) => {
                                trace!(
                                    "Variable {} constant folded to {}",
                                    mir[var].contents.name,
                                    val
                                );
                                known_values
                                    .int_variables
                                    .insert(var, val)
                                    .map_or(true, |old_val| old_val != val)
                            }
                            None => {
                                known_values.int_variables.remove(&var);
                                true
                            }
                        }
                    }

                    ExpressionId::String(expr) => {
                        match mir.try_string_constant_fold(expr, known_values, write_to_mir) {
                            Some(val) => {
                                trace!(
                                    "Variable {} constant folded to {}",
                                    mir[var].contents.name,
                                    &mir.string_literals[val]
                                );
                                match known_values.string_variables.insert(var, val) {
                                    Some(original_val)
                                        if &mir.string_literals[original_val]
                                            == &mir.string_literals[val] =>
                                    {
                                        false
                                    }
                                    _ => true,
                                }
                            }
                            None => {
                                known_values.string_variables.remove(&var);
                                true
                            }
                        }
                    }
                },
                Statement::Contribute(_, _, _, expr) if write_to_mir => {
                    mir.try_real_constant_fold(expr, known_values, true);
                    true
                }
                _ => true,
            });
    }
}
