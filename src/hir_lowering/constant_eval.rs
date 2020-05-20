/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

//! This module is responsible for evaluating expressions describing parameter bounds and default values a compile time
//! while also enforcing Type Conversion rules.
//!
//!
//!
//!
//!
//!
//!
use std::cmp::Ordering;
use std::convert::{TryFrom, TryInto};
use std::ops::Range;

use float_cmp::{ApproxEq, F64Margin};

use crate::ast;
use crate::ast::{BinaryOperator, UnaryOperator};
use crate::compact_arena::CompressedRange;
use crate::hir::{Expression, Primary};
use crate::hir_lowering::error::Error;
use crate::hir_lowering::error::Result;
use crate::hir_lowering::error::Type;
use crate::hir_lowering::HirToMirFold;
use crate::ir::mir::ParameterType;
use crate::ir::BuiltInFunctionCall1p::*;
use crate::ir::BuiltInFunctionCall2p::*;
use crate::ir::{BuiltInFunctionCall1p, BuiltInFunctionCall2p, ExpressionId, Node, ParameterId};
use crate::mir::*;
use crate::Span;

#[derive(Copy, Clone)]
pub enum Value<'tag> {
    Integer(i64),
    Real(f64),
    String(CompressedRange<'tag>),
}
impl<'tag> Value<'tag> {
    pub fn as_real(self, source: Span) -> Result<'tag, f64> {
        match self {
            Self::Real(val) => Ok(val),
            Self::Integer(val) => Ok(val as f64),
            Self::String(_) => Err(Error {
                error_type: Type::ExpectedNumber,
                source,
            }),
        }
    }
    pub fn as_integer(self, source: Span) -> Result<'tag, i64> {
        match self {
            Self::Real(val) => Ok(val.round() as i64),
            Self::Integer(val) => Ok(val),
            Self::String(_) => Err(Error {
                error_type: Type::ExpectedNumber,
                source,
            }),
        }
    }
    fn cast_to_bool(self, source: Span) -> Result<'tag, bool> {
        match self {
            Self::Integer(val) => match val {
                0 => Ok(false),
                _ => Ok(true),
            },
            Self::Real(_) | Self::String(_) => Err(Error {
                error_type: Type::ExpectedInteger,
                source,
            }),
        }
    }
}
impl<'tag> TryFrom<Node<Value<'tag>>> for i64 {
    type Error = Error<'tag>;

    fn try_from(val: Node<Value<'tag>>) -> Result<'tag, i64> {
        val.contents.as_integer(val.source)
    }
}

impl<'tag> TryFrom<Node<Value<'tag>>> for f64 {
    type Error = Error<'tag>;
    fn try_from(val: Node<Value<'tag>>) -> Result<'tag, f64> {
        val.contents.as_real(val.source)
    }
}
#[derive(PartialEq, Debug, Clone, Copy, PartialOrd)]
pub struct SortableFloat {
    val: f64,
}

impl From<SortableFloat> for f64 {
    fn from(sortable: SortableFloat) -> Self {
        sortable.val
    }
}
impl Ord for SortableFloat {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.val.partial_cmp(&other.val) {
            Some(res) => res,
            None => {
                unreachable_unchecked!("This struct exists to ensure that val can never be NAN")
            }
        }
    }
}
impl Eq for SortableFloat {}

pub trait IntoSortable: Sized {
    type T: Into<Self> + Ord + Eq + Clone;
    fn into_sortable<'tag>(self, source: Span) -> Result<'tag, Self::T>;
}

impl IntoSortable for f64 {
    type T = SortableFloat;
    fn into_sortable<'tag>(self, source: Span) -> Result<'tag, Self::T> {
        if self.is_nan() {
            Err(Error {
                source,
                error_type: Type::ExpectedNumber,
            })
        } else {
            Ok(SortableFloat { val: self })
        }
    }
}
impl IntoSortable for i64 {
    type T = i64;
    fn into_sortable<'tag>(self, _: Span) -> Result<'tag, Self> {
        Ok(self)
    }
}

impl<'tag, 'hirref> HirToMirFold<'tag, 'hirref> {
    pub fn eval_numerical_range_bound<
        T: TryFrom<Node<Value<'tag>>, Error = Error<'tag>> + PartialOrd,
    >(
        &mut self,
        current_parameter: ParameterId<'tag>,
        bound: &Range<ast::NumericalParameterRangeBound<'tag>>,
    ) -> Option<Range<NumericalParameterRangeBound<T>>> {
        let start = self
            .eval_constant_parameter_expression(current_parameter, bound.start.bound)
            .and_then(|res| {
                (Node::new(res, self.hir[bound.start.bound].source) as Node<Value<'tag>>).try_into()
            });
        let end = self
            .eval_constant_parameter_expression(current_parameter, bound.end.bound)
            .and_then(|res| Node::new(res, self.hir[bound.start.bound].source).try_into());
        match (start, end) {
            (Ok(start), Ok(end)) => {
                let res = Range {
                    start: NumericalParameterRangeBound {
                        inclusive: bound.start.inclusive,
                        bound: start,
                    },
                    end: NumericalParameterRangeBound {
                        inclusive: bound.end.inclusive,
                        bound: end,
                    },
                };

                if res.start.bound > res.end.bound {
                    self.errors.push(Error {
                        error_type: Type::InvalidParameterBound,
                        source: self.hir[bound.start.bound]
                            .source
                            .extend(self.hir[bound.end.bound].source),
                    });
                    None
                } else {
                    Some(res)
                }
            }
            (res1, res2) => {
                if let Err(error) = res1 {
                    self.errors.push(error);
                }

                if let Err(error) = res2 {
                    self.errors.push(error);
                }

                None
            }
        }
    }

    pub fn eval_parameter_type<
        T: TryFrom<Node<Value<'tag>>, Error = Error<'tag>>
            + Default
            + Clone
            + PartialOrd
            + IntoSortable,
    >(
        &mut self,
        parameter: ParameterId<'tag>,
        included_ranges: &[Range<ast::NumericalParameterRangeBound<'tag>>],
        excluded_ranges: &[ast::NumericalParameterRangeExclude<'tag>],
    ) -> Option<(Vec<Range<NumericalParameterRangeBound<T>>>, T)> {
        let mut included_ranges: Vec<_> = included_ranges
            .iter()
            .filter_map(
                |bound: &Range<ast::NumericalParameterRangeBound<'tag>>| -> Option<
                    Range<NumericalParameterRangeBound<<T as IntoSortable>::T>>
                > {
                    let (lower_src, upper_src) = (
                        self.hir[bound.start.bound].source,
                        self.hir[bound.end.bound].source,
                    );
                    let res: Range<NumericalParameterRangeBound<T>> =
                        self.eval_numerical_range_bound(parameter, bound)?;

                    Some(Range {
                        start: NumericalParameterRangeBound {
                            inclusive: res.start.inclusive,
                            bound: res
                                .start
                                .bound
                                .into_sortable(lower_src)
                                .map_err(|err| self.errors.push(err)).ok()?,
                        },
                        end: NumericalParameterRangeBound {
                            inclusive: res.end.inclusive,
                            bound: res
                                .end
                                .bound
                                .into_sortable(upper_src)
                                .map_err(|err| self.errors.push(err)).ok()?,
                        },
                    })
                },
            )
            .collect();

        included_ranges.sort_unstable_by(|lhs, rhs| lhs.start.bound.cmp(&rhs.end.bound));
        let mut iter = included_ranges.iter();
        let mut included_ranges: Vec<Range<NumericalParameterRangeBound<T>>> =
            if let Some(mut current) = iter.next().cloned() {
                let mut res = Vec::with_capacity(included_ranges.len());
                loop {
                    match iter.next() {
                        Some(range)
                            if range.start.bound <= current.end.bound
                                && range.end.bound > current.end.bound =>
                        {
                            current.end.bound = range.end.bound.clone(); //TODO warn
                            current.end.inclusive = range.end.inclusive || current.end.inclusive;
                        }
                        Some(range) if range.start > current.end => {
                            res.push(Range {
                                start: NumericalParameterRangeBound {
                                    bound: current.start.bound.into(),
                                    inclusive: current.start.inclusive,
                                },
                                end: NumericalParameterRangeBound {
                                    bound: current.end.bound.into(),
                                    inclusive: current.end.inclusive,
                                },
                            });
                            current = range.clone();
                        }
                        Some(_) => (), //todo warn
                        None => {
                            res.push(Range {
                                start: NumericalParameterRangeBound {
                                    bound: current.start.bound.into(),
                                    inclusive: current.start.inclusive,
                                },
                                end: NumericalParameterRangeBound {
                                    bound: current.end.bound.into(),
                                    inclusive: current.end.inclusive,
                                },
                            });
                            break;
                        }
                    }
                }
                res
            } else {
                Vec::new()
            };

        // This has a time complexiety of O(included*excluded) because we are linear serching each entry.
        //But in practice people use so few of these that any sophisticated algorithm is not worth the effort (and would probably even be slower)
        'outer: for exclude in excluded_ranges {
            match exclude {
                ast::NumericalParameterRangeExclude::Range(range_expr) => {
                    let range: Range<NumericalParameterRangeBound<T>> =
                        self.eval_numerical_range_bound(parameter, range_expr)?;

                    let mut i = 0;
                    while i < included_ranges.len() {
                        if included_ranges[i].contains(&range.start) {
                            included_ranges.insert(i, included_ranges[i].clone());
                            included_ranges[i].end = range.start;
                            i += 1;

                            loop {
                                if included_ranges[i].contains(&range.end) {
                                    included_ranges[i].start = range.end;
                                    continue 'outer;
                                }
                                included_ranges.remove(i);
                                if included_ranges.len() > i {
                                    self.errors.push(Error {
                                        error_type: Type::ParameterExcludeNotPartOfRange,
                                        source: self.hir[range_expr.end.bound].source,
                                    });
                                    continue 'outer;
                                }
                            }
                        }
                        i += 1;
                    }

                    self.errors.push(Error {
                        error_type: Type::ParameterExcludeNotPartOfRange,
                        source: self.hir[range_expr.start.bound]
                            .source
                            .extend(self.hir[range_expr.end.bound].source),
                    });
                }

                ast::NumericalParameterRangeExclude::Value(expr) => {
                    let res: Result<'tag, T> = self
                        .eval_constant_parameter_expression(parameter, *expr)
                        .and_then(|res| Node::new(res, self.hir[*expr].source).try_into());

                    match res {
                        Ok(res) => {
                            let bound = NumericalParameterRangeBound {
                                inclusive: true,
                                bound: res.clone(),
                            };

                            let mut i = 0;
                            while i < included_ranges.len() {
                                if included_ranges[i].contains(&bound) {
                                    included_ranges.insert(i, included_ranges[i].clone());

                                    included_ranges[i].end = NumericalParameterRangeBound {
                                        inclusive: false,
                                        bound: res.clone(),
                                    };

                                    included_ranges[i + 1].start = included_ranges[i].end.clone();
                                    continue 'outer;
                                }

                                i += 1;
                            }

                            self.errors.push(Error {
                                error_type: Type::ParameterExcludeNotPartOfRange,
                                source: self.hir[*expr].source,
                            });
                        }
                        Err(error) => {
                            self.errors.push(error);
                        }
                    }
                }
            }
        }

        let default_value = if let Some(expr) = self.hir[parameter].contents.default_value {
            match self
                .eval_constant_parameter_expression(parameter, expr)
                .and_then(|res| Node::new(res, self.hir[expr].source).try_into())
            {
                Ok(res) => res,
                Err(error) => {
                    self.errors.push(error);
                    return None;
                }
            }
        } else {
            Default::default()
        };

        Some((included_ranges, default_value))
    }

    pub fn eval_constant_parameter_expression(
        &mut self,
        current_parameter: ParameterId<'tag>,
        expr: ExpressionId<'tag>,
    ) -> Result<'tag, Value<'tag>> {
        let res = match self.hir[expr].contents {
            Expression::Condtion(condition, _, if_value, _, else_value) => {
                if self
                    .eval_constant_parameter_expression(current_parameter, condition)?
                    .cast_to_bool(self.hir[condition].source)?
                {
                    self.eval_constant_parameter_expression(current_parameter, if_value)?
                } else {
                    self.eval_constant_parameter_expression(current_parameter, else_value)?
                }
            }

            Expression::Primary(Primary::String(val)) => Value::String(val),

            Expression::Primary(Primary::Real(val)) => Value::Real(val),

            Expression::Primary(Primary::UnsignedInteger(val)) => Value::Integer(val as i64),

            Expression::Primary(Primary::Integer(val)) => Value::Integer(val),

            Expression::Primary(Primary::BuiltInFunctionCall1p(
                BuiltInFunctionCall1p::Abs,
                param,
            )) => match self.eval_constant_parameter_expression(current_parameter, param)? {
                Value::Real(val) => Value::Real(val.abs()),
                Value::Integer(val) => Value::Integer(val.abs()),
                Value::String(_) => {
                    return Err(Error {
                        error_type: Type::ExpectedNumber,
                        source: self.hir[param].source,
                    })
                }
            },

            Expression::Primary(Primary::BuiltInFunctionCall2p(
                BuiltInFunctionCall2p::Max,
                param1,
                param2,
            )) => {
                match (
                    self.eval_constant_parameter_expression(current_parameter, param1)?,
                    self.eval_constant_parameter_expression(current_parameter, param2)?,
                ) {
                    (Value::Integer(val1), Value::Integer(val2)) => Value::Integer(val1.max(val2)),
                    (val1, val2) => {
                        let val1 = val1.as_real(self.hir[param1].source);
                        let val2 = val2.as_real(self.hir[param2].source);
                        Value::Real(val1?.max(val2?))
                    }
                }
            }

            Expression::Primary(Primary::BuiltInFunctionCall2p(
                BuiltInFunctionCall2p::Min,
                param1,
                param2,
            )) => {
                match (
                    self.eval_constant_parameter_expression(current_parameter, param1)?,
                    self.eval_constant_parameter_expression(current_parameter, param2)?,
                ) {
                    (Value::Integer(val1), Value::Integer(val2)) => Value::Integer(val1.min(val2)),
                    (val1, val2) => {
                        let val1 = val1.as_real(self.hir[param1].source);
                        let val2 = val2.as_real(self.hir[param2].source);
                        Value::Real(val1?.min(val2?))
                    }
                }
            }

            Expression::Primary(Primary::BuiltInFunctionCall1p(call, arg)) => Value::Real({
                let arg = self
                    .eval_constant_parameter_expression(current_parameter, arg)?
                    .as_real(self.hir[arg].source)?;
                match call {
                    Sqrt => arg.sqrt(),
                    Exp(_)/* Whether this is a limexp or exp doesnt matter for constant eval*/ => arg.exp(),
                    Ln => arg.ln(),
                    Log => arg.log10(),
                    Abs => unreachable_unchecked!("Previous match"),
                    Floor => arg.floor(),
                    Ceil => arg.ceil(),
                    Sin => arg.sin(),
                    Cos => arg.cos(),
                    Tan => arg.tan(),
                    ArcSin => arg.asin(),
                    ArcCos => arg.acos(),
                    ArcTan => arg.atan(),
                    SinH => arg.sinh(),
                    CosH => arg.cosh(),
                    TanH => arg.tanh(),
                    ArcSinH => arg.asinh(),
                    ArcCosH => arg.acosh(),
                    ArcTanH => arg.atanh(),
                }
            }),

            Expression::Primary(Primary::BuiltInFunctionCall2p(call, arg1, arg2)) => Value::Real({
                let arg1 = self
                    .eval_constant_parameter_expression(current_parameter, arg1)
                    .and_then(|val| val.as_real(self.hir[arg1].source));
                let arg2 = self
                    .eval_constant_parameter_expression(current_parameter, arg2)
                    .and_then(|val| val.as_real(self.hir[arg2].source));
                let arg1 = arg1?;
                let arg2 = arg2?;
                match call {
                    Pow => arg1.powf(arg2),
                    Hypot => arg1.hypot(arg2),
                    Min => unreachable_unchecked!("Previous match"),
                    Max => unreachable_unchecked!("Previous match"),
                    ArcTan2 => arg1.atan2(arg2),
                }
            }),

            Expression::UnaryOperator(op, val) => match op.contents {
                UnaryOperator::ArithmeticNegate => {
                    match self.eval_constant_parameter_expression(current_parameter, val)? {
                        Value::Real(val) => Value::Real(-val),
                        Value::Integer(val) => Value::Integer(-val),
                        Value::String(_) => {
                            return Err(Error {
                                error_type: Type::ExpectedNumber,
                                source: self.hir[val].source,
                            })
                        }
                    }
                }

                UnaryOperator::ExplicitPositive => {
                    self.eval_constant_parameter_expression(current_parameter, val)?
                }

                UnaryOperator::LogicNegate => Value::Integer(
                    self.eval_constant_parameter_expression(current_parameter, val)?
                        .cast_to_bool(self.hir[val].source)? as i64,
                ),

                UnaryOperator::BitNegate => {
                    match self.eval_constant_parameter_expression(current_parameter, val)? {
                        Value::Real(_val) => {
                            return Err(Error {
                                error_type: Type::ExpectedInteger,
                                source: self.hir[expr].source,
                            })
                        }
                        Value::Integer(val) => Value::Integer(!val),
                        Value::String(_) => {
                            return Err(Error {
                                error_type: Type::ExpectedNumber,
                                source: self.hir[val].source,
                            })
                        }
                    }
                }
            },

            Expression::BinaryOperator(lhs_expr, op, rhs_expr) => {
                let lhs = self.eval_constant_parameter_expression(current_parameter, lhs_expr)?;
                let rhs = self.eval_constant_parameter_expression(current_parameter, rhs_expr)?;
                match op.contents {
                    BinaryOperator::LogicOr => Value::Integer(
                        (lhs.cast_to_bool(self.hir[lhs_expr].source)?
                            || rhs.cast_to_bool(self.hir[rhs_expr].source)?)
                            as i64,
                    ),
                    BinaryOperator::LogicAnd => Value::Integer(
                        (lhs.cast_to_bool(self.hir[lhs_expr].source)?
                            && rhs.cast_to_bool(self.hir[rhs_expr].source)?)
                            as i64,
                    ),
                    BinaryOperator::Xor => Value::Integer(
                        (lhs.cast_to_bool(self.hir[lhs_expr].source)?
                            ^ rhs.cast_to_bool(self.hir[rhs_expr].source)?)
                            as i64,
                    ),
                    BinaryOperator::NXor => Value::Integer(
                        !(lhs.cast_to_bool(self.hir[lhs_expr].source)?
                            ^ rhs.cast_to_bool(self.hir[rhs_expr].source)?)
                            as i64,
                    ),
                    BinaryOperator::And => Value::Integer(
                        (lhs.cast_to_bool(self.hir[lhs_expr].source)?
                            & rhs.cast_to_bool(self.hir[rhs_expr].source)?)
                            as i64,
                    ),
                    BinaryOperator::Or => Value::Integer(
                        (lhs.cast_to_bool(self.hir[lhs_expr].source)?
                            | rhs.cast_to_bool(self.hir[rhs_expr].source)?)
                            as i64,
                    ),
                    op => match (lhs, rhs) {
                        (Value::Integer(lhs), Value::Integer(rhs)) => match op {
                            BinaryOperator::Subtract => Value::Integer(lhs - rhs),
                            BinaryOperator::Sum => Value::Integer(lhs + rhs),
                            BinaryOperator::Multiply => Value::Integer(lhs * rhs),
                            BinaryOperator::Divide => Value::Integer(lhs / rhs),
                            BinaryOperator::Exponent => {
                                if rhs < 0 {
                                    Value::Integer(1 / rhs.pow(-lhs as u32))
                                } else {
                                    Value::Integer(lhs.pow(rhs as u32))
                                }
                            }
                            BinaryOperator::Modulus => Value::Integer(lhs % rhs),
                            BinaryOperator::ShiftLeft => Value::Integer(lhs << rhs),
                            BinaryOperator::ShiftRight => Value::Integer(lhs >> rhs),
                            BinaryOperator::LessThen => Value::Integer((lhs < rhs) as i64),
                            BinaryOperator::LessEqual => Value::Integer((lhs <= rhs) as i64),
                            BinaryOperator::GreaterThen => Value::Integer((lhs > rhs) as i64),
                            BinaryOperator::GreaterEqual => Value::Integer((lhs >= rhs) as i64),
                            BinaryOperator::LogicEqual => Value::Integer((lhs == rhs) as i64),
                            BinaryOperator::LogicalNotEqual => Value::Integer((lhs != rhs) as i64),
                            _ => unreachable_unchecked!("previous match"),
                        },
                        (Value::String(lhs), Value::String(rhs)) => match op {
                            BinaryOperator::LogicEqual => Value::Integer(
                                (self.mir.string_literals[lhs] == self.mir.string_literals[rhs])
                                    as i64,
                            ),
                            BinaryOperator::LogicalNotEqual => Value::Integer(
                                (self.mir.string_literals[lhs] != self.mir.string_literals[rhs])
                                    as i64,
                            ),
                            _ => {
                                self.errors.push(Error {
                                    error_type: Type::ExpectedNumber,
                                    source: self.hir[lhs_expr].source,
                                });
                                return Err(Error {
                                    error_type: Type::ExpectedNumber,
                                    source: self.hir[rhs_expr].source,
                                });
                            }
                        },
                        (lhs, rhs) => {
                            let lhs = lhs.as_real(self.hir[lhs_expr].source);
                            let rhs = rhs.as_real(self.hir[lhs_expr].source);
                            let lhs = lhs?;
                            let rhs = rhs?;
                            match op {
                                BinaryOperator::Subtract => Value::Real(lhs - rhs),
                                BinaryOperator::Sum => Value::Real(lhs + rhs),
                                BinaryOperator::Multiply => Value::Real(lhs * rhs),
                                BinaryOperator::Divide => Value::Real(lhs / rhs),
                                BinaryOperator::Exponent => Value::Real(lhs.powf(rhs)),
                                BinaryOperator::Modulus => Value::Real(lhs % rhs),
                                BinaryOperator::ShiftLeft | BinaryOperator::ShiftRight => {
                                    self.errors.push(Error {
                                        error_type: Type::ExpectedInteger,
                                        source: self.hir[lhs_expr].source,
                                    });
                                    return Err(Error {
                                        error_type: Type::ExpectedInteger,
                                        source: self.hir[rhs_expr].source,
                                    });
                                }
                                BinaryOperator::LessThen => Value::Integer((lhs < rhs) as i64),
                                BinaryOperator::LessEqual => Value::Integer((lhs <= rhs) as i64),
                                BinaryOperator::GreaterThen => Value::Integer((lhs > rhs) as i64),
                                BinaryOperator::GreaterEqual => Value::Integer((lhs >= rhs) as i64),
                                BinaryOperator::LogicEqual => Value::Integer(
                                    (lhs.approx_eq(rhs, F64Margin::default())) as i64,
                                ), //Todo discuss f64 comparison precision
                                BinaryOperator::LogicalNotEqual => Value::Integer(
                                    (lhs.approx_ne(rhs, F64Margin::default())) as i64,
                                ),
                                _ => unreachable_unchecked!("previous match"),
                            }
                        }
                    },
                }
            }

            Expression::Primary(Primary::VariableReference(_))
            | Expression::Primary(Primary::NetReference(_))
            | Expression::Primary(Primary::PortReference(_))
            | Expression::Primary(Primary::BranchAccess(_, _))
            | Expression::Primary(Primary::SystemFunctionCall(_))
            | Expression::Primary(Primary::Noise(_,_)) => {
                unreachable_unchecked!("constant checking")
            }

            Expression::Primary(Primary::ParameterReference(parameter)) => {
                if parameter.unwrap().index() < current_parameter.unwrap().index() {
                    match self.mir[parameter].contents.parameter_type {
                        ParameterType::Integer { default_value, .. } => {
                            Value::Integer(default_value)
                        }
                        ParameterType::Real { default_value, .. } => Value::Real(default_value),
                        ParameterType::String { .. } => {
                            return Err(Error {
                                error_type: Type::ExpectedNumericParameter(parameter),
                                source: self.hir[expr].source,
                            })
                        }
                    }
                } else {
                    return Err(Error {
                        error_type: Type::ParameterDefinedAfterConstantReference(parameter),
                        source: self.hir[expr].source,
                    });
                }
            }

            Expression::Primary(Primary::FunctionCall(_, _)) => todo!("function calls"),
            Expression::Primary(Primary::Derivative(_, _)) => {
                todo!("Migrate constant folding to work over MIR expressions for greater versatility -> Implement Constant derivatives")
            }
        };
        Ok(res)
    }
}
