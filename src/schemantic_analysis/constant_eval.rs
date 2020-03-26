use std::ops::Range;

use float_cmp::{ApproxEq, F64Margin};

use crate::ast;
use crate::ast::{BinaryOperator, BuiltInFunctionCall, UnaryOperator};
use crate::hir::{Expression, Primary};
use crate::ir::mir::ParameterType;
use crate::ir::{ExpressionId, ParameterId};
use crate::mir::*;
use crate::schemantic_analysis::error::Error;
use crate::schemantic_analysis::error::Result;
use crate::schemantic_analysis::error::Type;
use crate::schemantic_analysis::HirToMirFold;
use crate::Span;

#[derive(Copy, Clone)]
pub enum Value {
    Integer(i64),
    Real(f64),
}
impl Value {
    pub fn as_real(self) -> f64 {
        match self {
            Self::Real(val) => val,
            Self::Integer(val) => val as f64,
        }
    }
    pub fn as_integer(self) -> i64 {
        match self {
            Self::Real(val) => val.round() as i64,
            Self::Integer(val) => val,
        }
    }
    fn cast_to_bool<'tag>(self, source: Span) -> Result<'tag, bool> {
        match self {
            Value::Integer(val) => match val {
                0 => Ok(false),
                _ => Ok(true),
            },
            Value::Real(val) => Err(Error {
                error_type: Type::ExpectedInteger,
                source,
            }),
        }
    }
}
impl From<Value> for i64 {
    fn from(val: Value) -> i64 {
        val.as_integer()
    }
}

impl From<Value> for f64 {
    fn from(val: Value) -> f64 {
        val.as_real()
    }
}

impl<'tag, 'hirref> HirToMirFold<'tag, 'hirref> {
    pub fn eval_numerical_range_bound<T: From<Value>>(
        &mut self,
        current_parameter: ParameterId<'tag>,
        bound: &Range<ast::NumericalParameterRangeBound<'tag>>,
    ) -> Option<Range<NumericalParameterRangeBound<T>>> {
        let start = self.eval_constant_parameter_expression(current_parameter, bound.start.bound);
        let end = self.eval_constant_parameter_expression(current_parameter, bound.end.bound);
        match (start, end) {
            (Ok(start), Ok(end)) => Some(Range {
                start: NumericalParameterRangeBound {
                    inclusive: bound.start.inclusive,
                    bound: start.into(),
                },
                end: NumericalParameterRangeBound {
                    inclusive: bound.end.inclusive,
                    bound: end.into(),
                },
            }),
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
    pub fn eval_parameter_type<T: From<Value> + Default>(
        &mut self,
        parameter: ParameterId<'tag>,
        included_ranges: &[Range<ast::NumericalParameterRangeBound<'tag>>],
        excluded_ranges: &[ast::NumericalParameterRangeExclude<'tag>],
    ) -> std::result::Result<
        (
            Vec<Range<NumericalParameterRangeBound<T>>>,
            Vec<NumericalParameterRangeExclude<T>>,
            T,
        ),
        (),
    > {
        let included_ranges = included_ranges
            .iter()
            .filter_map(|bound| self.eval_numerical_range_bound(parameter, bound))
            .collect();
        let excluded_ranges = excluded_ranges
            .iter()
            .filter_map(|exclude| match exclude {
                ast::NumericalParameterRangeExclude::Range(range) => self
                    .eval_numerical_range_bound(parameter, range)
                    .map(NumericalParameterRangeExclude::Range),
                ast::NumericalParameterRangeExclude::Value(expr) => {
                    match self.eval_constant_parameter_expression(parameter, *expr) {
                        Ok(res) => Some(NumericalParameterRangeExclude::Value(res.into())),
                        Err(error) => {
                            self.errors.push(error);
                            None
                        }
                    }
                }
            })
            .collect();
        let default_value = if let Some(expr) = self.hir[parameter].contents.default_value {
            match self.eval_constant_parameter_expression(parameter, expr) {
                Ok(res) => res.into(),
                Err(error) => {
                    self.errors.push(error);
                    return Err(());
                }
            }
        } else {
            Default::default()
        };

        Ok((included_ranges, excluded_ranges, default_value))
    }

    pub fn eval_constant_parameter_expression(
        &mut self,
        current_parameter: ParameterId<'tag>,
        expr: ExpressionId<'tag>,
    ) -> Result<'tag, Value> {
        let res = match self.hir[expr].contents {
            Expression::Condtion(condition, question_span, if_value, colon_span, else_value) => {
                if self
                    .eval_constant_parameter_expression(current_parameter, condition)?
                    .cast_to_bool(self.hir[condition].source)?
                {
                    self.eval_constant_parameter_expression(current_parameter, if_value)?
                } else {
                    self.eval_constant_parameter_expression(current_parameter, else_value)?
                }
            }
            Expression::Primary(Primary::Real(val)) => Value::Real(val),
            Expression::Primary(Primary::UnsignedInteger(val)) => Value::Integer(val as i64),
            Expression::Primary(Primary::Integer(val)) => Value::Integer(val),
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Exp(param))) => {
                match self.eval_constant_parameter_expression(current_parameter, param)? {
                    Value::Real(val) => Value::Real(val.exp()),
                    Value::Integer(val) => Value::Real(std::f64::consts::E.powi(val as i32)), //faster than converting to float
                }
            }
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Ln(param))) => {
                Value::Real(
                    self.eval_constant_parameter_expression(current_parameter, param)?
                        .as_real()
                        .ln(),
                )
            }
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Log(param))) => {
                Value::Real(
                    self.eval_constant_parameter_expression(current_parameter, param)?
                        .as_real()
                        .log10(),
                )
            }
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Pow(
                param1,
                param2,
            ))) => {
                match (
                    self.eval_constant_parameter_expression(current_parameter, param1)?,
                    self.eval_constant_parameter_expression(current_parameter, param2)?,
                ) {
                    (Value::Real(val1), Value::Integer(val2)) => {
                        Value::Real(val1.powi(val2 as i32)) //faster than conversion
                    }
                    (val1, val2) => Value::Real(val1.as_real().powf(val2.as_real())),
                }
            }
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Sqrt(param))) => {
                Value::Real(
                    self.eval_constant_parameter_expression(current_parameter, param)?
                        .as_real()
                        .sqrt(),
                )
            }
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Hypot(
                param1,
                param2,
            ))) => Value::Real(
                self.eval_constant_parameter_expression(current_parameter, param1)?
                    .as_real()
                    .hypot(
                        self.eval_constant_parameter_expression(current_parameter, param2)?
                            .as_real(),
                    ),
            ),

            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::ArcCos(
                param,
            ))) => Value::Real(
                self.eval_constant_parameter_expression(current_parameter, param)?
                    .as_real()
                    .acos(),
            ),
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::ArcSin(
                param,
            ))) => Value::Real(
                self.eval_constant_parameter_expression(current_parameter, param)?
                    .as_real()
                    .asin(),
            ),
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Cos(param))) => {
                Value::Real(
                    self.eval_constant_parameter_expression(current_parameter, param)?
                        .as_real()
                        .cos(),
                )
            }
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Sin(param))) => {
                Value::Real(
                    self.eval_constant_parameter_expression(current_parameter, param)?
                        .as_real()
                        .sin(),
                )
            }
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Tan(param))) => {
                Value::Real(
                    self.eval_constant_parameter_expression(current_parameter, param)?
                        .as_real()
                        .tan(),
                )
            }
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::ArcTan(
                param,
            ))) => Value::Real(
                self.eval_constant_parameter_expression(current_parameter, param)?
                    .as_real()
                    .atan(),
            ),
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::ArcTan2(
                param1,
                param2,
            ))) => Value::Real(
                self.eval_constant_parameter_expression(current_parameter, param1)?
                    .as_real()
                    .atan2(
                        self.eval_constant_parameter_expression(current_parameter, param2)?
                            .as_real(),
                    ),
            ),

            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::ArcCosH(
                param,
            ))) => Value::Real(
                self.eval_constant_parameter_expression(current_parameter, param)?
                    .as_real()
                    .acosh(),
            ),
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::ArcSinH(
                param,
            ))) => Value::Real(
                self.eval_constant_parameter_expression(current_parameter, param)?
                    .as_real()
                    .asinh(),
            ),
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::CosH(param))) => {
                Value::Real(
                    self.eval_constant_parameter_expression(current_parameter, param)?
                        .as_real()
                        .cosh(),
                )
            }
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::SinH(param))) => {
                Value::Real(
                    self.eval_constant_parameter_expression(current_parameter, param)?
                        .as_real()
                        .sinh(),
                )
            }
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::TanH(param))) => {
                Value::Real(
                    self.eval_constant_parameter_expression(current_parameter, param)?
                        .as_real()
                        .tanh(),
                )
            }
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::ArcTanH(
                param,
            ))) => Value::Real(
                self.eval_constant_parameter_expression(current_parameter, param)?
                    .as_real()
                    .atanh(),
            ),

            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Abs(param))) => {
                match self.eval_constant_parameter_expression(current_parameter, param)? {
                    Value::Real(val) => Value::Real(val.abs()),
                    Value::Integer(val) => Value::Integer(val.abs()),
                }
            }
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Ceil(param))) => {
                match self.eval_constant_parameter_expression(current_parameter, param)? {
                    Value::Real(val) => Value::Real(val.ceil()),
                    Value::Integer(val) => Value::Integer(val),
                }
            }
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Floor(
                param,
            ))) => match self.eval_constant_parameter_expression(current_parameter, param)? {
                Value::Real(val) => Value::Real(val.floor()),
                Value::Integer(val) => Value::Integer(val),
            },
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Max(
                param1,
                param2,
            ))) => {
                match (
                    self.eval_constant_parameter_expression(current_parameter, param1)?,
                    self.eval_constant_parameter_expression(current_parameter, param2)?,
                ) {
                    (Value::Integer(val1), Value::Integer(val2)) => Value::Integer(val1.max(val2)),
                    (val1, val2) => Value::Real(val1.as_real().max(val2.as_real())),
                }
            }
            Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Min(
                param1,
                param2,
            ))) => {
                match (
                    self.eval_constant_parameter_expression(current_parameter, param1)?,
                    self.eval_constant_parameter_expression(current_parameter, param2)?,
                ) {
                    (Value::Integer(val1), Value::Integer(val2)) => Value::Integer(val1.min(val2)),
                    (val1, val2) => Value::Real(val1.as_real().min(val2.as_real())),
                }
            }
            Expression::UnaryOperator(op, val) => match op.contents {
                UnaryOperator::ArithmeticNegate => {
                    match self.eval_constant_parameter_expression(current_parameter, val)? {
                        Value::Real(val) => Value::Real(-val),
                        Value::Integer(val) => Value::Integer(-val),
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
                        Value::Real(val) => {
                            return Err(Error {
                                error_type: Type::ExpectedInteger,
                                source: self.hir[expr].source,
                            })
                        }
                        Value::Integer(val) => Value::Integer(!val),
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
                        (lhs, rhs) => {
                            let lhs = lhs.as_real();
                            let rhs = rhs.as_real();
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
            | Expression::Primary(Primary::SystemFunctionCall(_)) => {
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
        };
        Ok(res)
    }
}
