/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use std::convert::{TryFrom, TryInto};

use crate::ast::BinaryOperator;
use crate::hir::Primary;
use crate::hir_lowering::error::{Error, Type};
use crate::hir_lowering::HirToMirFold;
use crate::ir::{hir, IntegerExpressionId, Node, RealExpressionId};
use crate::ir::{BuiltInFunctionCall1p, BuiltInFunctionCall2p, Push};
use crate::mir::*;
use crate::{ast, ir, mir};

impl<'tag, 'hirref> HirToMirFold<'tag, 'hirref> {
    pub fn fold_real_expression(
        &mut self,
        expr: ir::ExpressionId<'tag>,
    ) -> Option<RealExpressionId<'tag>> {
        let source = self.hir[expr].source;
        let contents = match self.hir[expr].contents {
            hir::Expression::Condtion(condition, question_span, if_val, colon_span, else_val) => {
                let condition = self.fold_integer_expression(condition);
                let if_val = self.fold_real_expression(if_val);
                let else_val = self.fold_real_expression(else_val);
                RealExpression::Condition(condition?, question_span, if_val?, colon_span, else_val?)
            }

            hir::Expression::Primary(Primary::Real(val)) => RealExpression::Literal(val),
            hir::Expression::Primary(Primary::FunctionCall(function, ref args)) => {
                todo!("argument type checking")
                /*self.mir.push(Node{
                    source,
                    contents:RealExpression::FunctionCall(function, args.clone())
                })*/
            }

            hir::Expression::Primary(Primary::BranchAccess(discipline_access, branch)) => {
                RealExpression::BranchAccess(discipline_access, branch)
            }

            hir::Expression::Primary(Primary::ParameterReference(parameter))
                if matches!(
                    self.mir[parameter].contents.parameter_type,
                    ParameterType::Real {..}
                ) =>
            {
                RealExpression::ParameterReference(parameter)
            }

            hir::Expression::Primary(Primary::VariableReference(variable))
                if matches!(
                    self.mir[variable].contents.variable_type,
                    mir::VariableType::Real(..)
                ) =>
            {
                RealExpression::VariableReference(variable)
            }

            hir::Expression::UnaryOperator(
                Node {
                    contents: ast::UnaryOperator::ArithmeticNegate,
                    source: op,
                },
                parameter,
            ) => RealExpression::Negate(op, self.fold_real_expression(parameter)?),

            hir::Expression::BinaryOperator(lhs, op_node, rhs) => {
                let lhs = self.fold_real_expression(lhs);
                let rhs = self.fold_real_expression(rhs);
                let op = match op_node.contents {
                    BinaryOperator::Sum => RealBinaryOperator::Sum,
                    BinaryOperator::Subtract => RealBinaryOperator::Subtract,
                    BinaryOperator::Multiply => RealBinaryOperator::Multiply,
                    BinaryOperator::Divide => RealBinaryOperator::Divide,
                    BinaryOperator::Exponent => RealBinaryOperator::Exponent,
                    BinaryOperator::Modulus => RealBinaryOperator::Modulus,
                    _ => {
                        let integer_expr = self.fold_integer_expression(expr)?;
                        return Some(self.mir.push(Node {
                            contents: RealExpression::IntegerConversion(integer_expr),
                            source,
                        }));
                    }
                };
                RealExpression::BinaryOperator(
                    lhs?,
                    Node {
                        contents: op,
                        source: op_node.source,
                    },
                    rhs?,
                )
            }
            hir::Expression::Primary(Primary::SystemFunctionCall(ident)) => {
                RealExpression::SystemFunctionCall(ident) //TODO delegate type checking using closure
            }
            hir::Expression::Primary(Primary::BuiltInFunctionCall1p(call, arg)) => {
                RealExpression::BuiltInFunctionCall1p(call, self.fold_real_expression(arg)?)
            }
            hir::Expression::Primary(Primary::BuiltInFunctionCall2p(call, arg1, arg2)) => {
                let arg1 = self.fold_real_expression(arg1);
                let arg2 = self.fold_real_expression(arg2);
                RealExpression::BuiltInFunctionCall2p(call, arg1?, arg2?)
            }
            _ => RealExpression::IntegerConversion(self.fold_integer_expression(expr)?),
        };
        Some(self.mir.push(Node { contents, source }))
    }

    pub fn fold_integer_expression(
        &mut self,
        expr: ir::ExpressionId<'tag>,
    ) -> Option<IntegerExpressionId<'tag>> {
        let source = self.hir[expr].source;
        let contents = match self.hir[expr].contents {
            hir::Expression::Primary(Primary::Integer(val)) => {
                IntegerExpression::Literal(val as i64)
            }

            hir::Expression::Primary(Primary::UnsignedInteger(val)) => {
                IntegerExpression::Literal(val as i64)
            }

            hir::Expression::Primary(Primary::NetReference(net)) => {
                IntegerExpression::NetReference(net)
            }

            hir::Expression::Primary(Primary::PortReference(port)) => {
                IntegerExpression::PortReference(port)
            } //todo check that these are actually of the right size/treat integers as net arrays

            hir::Expression::UnaryOperator(op, arg) => {
                IntegerExpression::UnaryOperator(op, self.fold_integer_expression(arg)?)
            }

            hir::Expression::Primary(Primary::BuiltInFunctionCall2p(
                BuiltInFunctionCall2p::Min,
                arg1,
                arg2,
            )) => {
                let arg1 = self.fold_integer_expression(arg1);
                let arg2 = self.fold_integer_expression(arg2);
                let (arg1, arg2) = (arg1?, arg2?);
                IntegerExpression::Min(arg1, arg2)
            }

            hir::Expression::Primary(Primary::BuiltInFunctionCall2p(
                BuiltInFunctionCall2p::Max,
                arg1,
                arg2,
            )) => {
                let arg1 = self.fold_integer_expression(arg1);
                let arg2 = self.fold_integer_expression(arg2);
                let (arg1, arg2) = (arg1?, arg2?);
                IntegerExpression::Max(arg1, arg2)
            }

            hir::Expression::Primary(Primary::BuiltInFunctionCall1p(
                BuiltInFunctionCall1p::Abs,
                arg,
            )) => IntegerExpression::Abs(self.fold_integer_expression(arg)?),

            hir::Expression::Condtion(condition, question_span, if_val, colon_span, else_val) => {
                let condition = self.fold_integer_expression(condition);
                let if_val = self.fold_integer_expression(if_val);
                let else_val = self.fold_integer_expression(else_val);
                IntegerExpression::Condition(
                    condition?,
                    question_span,
                    if_val?,
                    colon_span,
                    else_val?,
                )
            }

            hir::Expression::BinaryOperator(lhs, op, rhs)
                if matches!(
                    op.contents,
                    BinaryOperator::LessEqual
                        | BinaryOperator::LessThen
                        | BinaryOperator::GreaterThen
                        | BinaryOperator::GreaterEqual
                        | BinaryOperator::LogicEqual
                        | BinaryOperator::LogicalNotEqual
                ) =>
            {
                let lhs = self.fold_expression(lhs);
                let rhs = self.fold_expression(rhs);
                let comparison_op = match op.contents {
                    BinaryOperator::LessEqual => ComparisonOperator::LessEqual,
                    BinaryOperator::LessThen => ComparisonOperator::LessThen,
                    BinaryOperator::GreaterThen => ComparisonOperator::GreaterThen,
                    BinaryOperator::GreaterEqual => ComparisonOperator::GreaterEqual,
                    BinaryOperator::LogicEqual => ComparisonOperator::LogicEqual,
                    BinaryOperator::LogicalNotEqual => ComparisonOperator::LogicalNotEqual,
                    _ => unreachable!(),
                };
                let op = Node::new(comparison_op, op.source);

                match (lhs?, rhs?) {
                    (ExpressionId::Integer(lhs), ExpressionId::Integer(rhs)) => {
                        IntegerExpression::IntegerComparison(lhs, op, rhs)
                    }

                    (ExpressionId::Real(lhs), ExpressionId::Real(rhs)) => {
                        IntegerExpression::RealComparison(lhs, op, rhs)
                    }

                    (ExpressionId::Integer(lhs), ExpressionId::Real(rhs)) => {
                        let lhs = self.mir.push(Node::new(
                            RealExpression::IntegerConversion(lhs),
                            self.mir[lhs].source,
                        ));
                        IntegerExpression::RealComparison(lhs, op, rhs)
                    }

                    (ExpressionId::Real(lhs), ExpressionId::Integer(rhs)) => {
                        let rhs = self.mir.push(Node::new(
                            RealExpression::IntegerConversion(rhs),
                            self.mir[rhs].source,
                        ));
                        IntegerExpression::RealComparison(lhs, op, rhs)
                    }

                    (ExpressionId::String(lhs), ExpressionId::String(rhs))
                        if op.contents == ComparisonOperator::LogicEqual =>
                    {
                        IntegerExpression::StringEq(lhs, rhs)
                    }

                    (ExpressionId::String(lhs), ExpressionId::String(rhs))
                        if op.contents == ComparisonOperator::LogicalNotEqual =>
                    {
                        IntegerExpression::StringNEq(lhs, rhs)
                    }

                    (ExpressionId::String(lhs), ExpressionId::String(rhs)) => {
                        self.errors.push(Error {
                            error_type: Type::ExpectedNumber,
                            source: self.mir[lhs].source,
                        });
                        self.errors.push(Error {
                            error_type: Type::ExpectedNumber,
                            source: self.mir[rhs].source,
                        });
                        return None;
                    }
                    (lhs, rhs) => {
                        self.errors.push(Error {
                            error_type: Type::CannotCompareStringToNumber,
                            source: lhs.source(&self.mir).extend(rhs.source(&self.mir)),
                        });
                        return None;
                    }
                }
            }

            hir::Expression::BinaryOperator(lhs, op_node, rhs) => {
                let lhs = self.fold_integer_expression(lhs);
                let rhs = self.fold_integer_expression(rhs);
                let op = match op_node.contents {
                    BinaryOperator::Sum => IntegerBinaryOperator::Sum,
                    BinaryOperator::Subtract => IntegerBinaryOperator::Subtract,
                    BinaryOperator::Multiply => IntegerBinaryOperator::Multiply,
                    BinaryOperator::Divide => IntegerBinaryOperator::Divide,
                    BinaryOperator::Exponent => IntegerBinaryOperator::Exponent,
                    BinaryOperator::Modulus => IntegerBinaryOperator::Modulus,
                    BinaryOperator::ShiftLeft => IntegerBinaryOperator::ShiftLeft,
                    BinaryOperator::ShiftRight => IntegerBinaryOperator::ShiftRight,
                    BinaryOperator::LogicOr => IntegerBinaryOperator::LogicOr,
                    BinaryOperator::LogicAnd => IntegerBinaryOperator::LogicAnd,
                    BinaryOperator::Xor => IntegerBinaryOperator::Xor,
                    BinaryOperator::NXor => IntegerBinaryOperator::NXor,
                    BinaryOperator::And => IntegerBinaryOperator::And,
                    BinaryOperator::Or => IntegerBinaryOperator::Or,
                    _ => unreachable!(),
                };
                IntegerExpression::BinaryOperator(
                    lhs?,
                    Node {
                        contents: op,
                        source: op_node.source,
                    },
                    rhs?,
                )
            }

            hir::Expression::Primary(Primary::FunctionCall(_, _)) => todo!("Function Calls"),
            hir::Expression::Primary(Primary::SystemFunctionCall(_)) => {
                todo!("System function calls")
            }
            hir::Expression::Primary(Primary::ParameterReference(parameter)) => {
                match self.mir[parameter].contents.parameter_type {
                    ParameterType::Integer { .. } => {
                        IntegerExpression::ParameterReference(parameter)
                    }
                    _ => {
                        self.errors.push(Error {
                            error_type: Type::ExpectedIntegerParameter(parameter),
                            source,
                        });
                        return None;
                    }
                }
            }

            hir::Expression::Primary(Primary::VariableReference(variable)) => {
                match self.mir[variable].contents.variable_type {
                    crate::mir::VariableType::Integer(..) => {
                        IntegerExpression::VariableReference(variable)
                    }
                    _ => {
                        self.errors.push(Error {
                            error_type: Type::ExpectedIntegerVariable(variable),
                            source,
                        });
                        return None;
                    }
                }
            }

            hir::Expression::Primary(Primary::String(_)) => {
                self.errors.push(Error {
                    error_type: Type::ExpectedInteger,
                    source,
                });
                return None;
            }
            _ => {
                self.errors.push(Error {
                    error_type: Type::ExpectedInteger,
                    source,
                });
                return None;
            }
        };

        Some(self.mir.push(Node { source, contents }))
    }

    pub fn fold_expression(&mut self, expr: ir::ExpressionId<'tag>) -> Option<ExpressionId<'tag>> {
        let source = self.hir[expr].source;
        let contents = match self.hir[expr].contents {
            hir::Expression::Condtion(condition, question_span, if_val, colon_span, else_val) => {
                let condition = self.fold_integer_expression(condition);
                let (if_val, else_val) = match (
                    self.fold_expression(if_val)?,
                    self.fold_expression(else_val)?,
                ) {
                    (ExpressionId::Real(if_val), ExpressionId::Real(else_val)) => {
                        (if_val, else_val)
                    }
                    (ExpressionId::Real(if_val), ExpressionId::Integer(else_val)) => {
                        let else_val = self.mir.push(Node {
                            source: self.mir[else_val].source,
                            contents: RealExpression::IntegerConversion(else_val),
                        });
                        (if_val, else_val)
                    }
                    (ExpressionId::Integer(if_val), ExpressionId::Real(else_val)) => {
                        let if_val = self.mir.push(Node {
                            source: self.mir[if_val].source,
                            contents: RealExpression::IntegerConversion(if_val),
                        });
                        (if_val, else_val)
                    }
                    (ExpressionId::Integer(if_val), ExpressionId::Integer(else_val)) => {
                        return Some(ExpressionId::Integer(self.mir.push(Node {
                            contents: IntegerExpression::Condition(
                                condition?,
                                question_span,
                                if_val,
                                colon_span,
                                else_val,
                            ),
                            source,
                        })))
                    }

                    (ExpressionId::String(if_val), ExpressionId::String(else_val)) => {
                        return Some(ExpressionId::String(self.mir.push(Node {
                            contents: StringExpression::Condition(
                                condition?,
                                question_span,
                                if_val,
                                colon_span,
                                else_val,
                            ),
                            source,
                        })))
                    }

                    (ExpressionId::String(_), _num) | (_num, ExpressionId::String(_)) => {
                        self.errors.push(Error {
                            error_type: Type::CondtionTypeMissmatch,
                            source,
                        });
                        return None;
                    }
                };
                RealExpression::Condition(condition?, question_span, if_val, colon_span, else_val)
            }

            hir::Expression::Primary(Primary::SystemFunctionCall(call)) => {
                RealExpression::SystemFunctionCall(call)
            }

            hir::Expression::Primary(Primary::String(val)) => {
                return Some(ExpressionId::String(self.mir.push(Node {
                    contents: StringExpression::Literal(val),
                    source,
                })))
            }
            hir::Expression::Primary(Primary::Real(val)) => RealExpression::Literal(val),

            hir::Expression::Primary(Primary::FunctionCall(function, ref args)) => {
                todo!("return type checking");
                // todo!("argument type checking")
                /*self.mir.push(Node{
                    source,
                    contents:RealExpression::FunctionCall(function, args.clone())
                })*/
            }

            hir::Expression::Primary(Primary::BranchAccess(discipline_access, branch)) => {
                RealExpression::BranchAccess(discipline_access, branch)
            }

            hir::Expression::Primary(Primary::ParameterReference(parameter))
                if matches!(
                    self.mir[parameter].contents.parameter_type,
                    ParameterType::Real {..}
                ) =>
            {
                RealExpression::ParameterReference(parameter)
            }

            hir::Expression::Primary(Primary::VariableReference(variable))
                if matches!(
                    self.mir[variable].contents.variable_type,
                    mir::VariableType::Real(..)
                ) =>
            {
                RealExpression::VariableReference(variable)
            }

            hir::Expression::UnaryOperator(
                Node {
                    contents: ast::UnaryOperator::ArithmeticNegate,
                    source: op,
                },
                parameter,
            ) => RealExpression::Negate(op, self.fold_real_expression(parameter)?),

            hir::Expression::BinaryOperator(lhs, op_node, rhs) => {
                let op = match op_node.contents {
                    BinaryOperator::Sum => RealBinaryOperator::Sum,
                    BinaryOperator::Subtract => RealBinaryOperator::Subtract,
                    BinaryOperator::Multiply => RealBinaryOperator::Multiply,
                    BinaryOperator::Divide => RealBinaryOperator::Divide,
                    BinaryOperator::Exponent => RealBinaryOperator::Exponent,
                    BinaryOperator::Modulus => RealBinaryOperator::Modulus,
                    _ => return Some(ExpressionId::Integer(self.fold_integer_expression(expr)?)),
                };

                let (lhs, rhs) = match (self.fold_expression(lhs)?, self.fold_expression(rhs)?) {
                    (ExpressionId::Real(lhs), ExpressionId::Real(rhs)) => (lhs, rhs),
                    (ExpressionId::Real(lhs), ExpressionId::Integer(rhs)) => {
                        let rhs = self.mir.push(Node {
                            source: self.mir[rhs].source,
                            contents: RealExpression::IntegerConversion(rhs),
                        });
                        (lhs, rhs)
                    }

                    (ExpressionId::Integer(lhs), ExpressionId::Real(rhs)) => {
                        let lhs = self.mir.push(Node {
                            source: self.mir[lhs].source,
                            contents: RealExpression::IntegerConversion(lhs),
                        });
                        (lhs, rhs)
                    }

                    (ExpressionId::Integer(lhs), ExpressionId::Integer(rhs)) => {
                        return Some(ExpressionId::Integer(self.mir.push(Node {
                            contents: IntegerExpression::BinaryOperator(
                                lhs,
                                Node {
                                    contents: op.into(),
                                    source: op_node.source,
                                },
                                rhs,
                            ),
                            source,
                        })))
                    }

                    (ExpressionId::String(val), other) | (other, ExpressionId::String(val)) => {
                        if let ExpressionId::String(other) = other {
                            self.errors.push(Error {
                                error_type: Type::ExpectedNumber,
                                source: self.mir[other].source,
                            });
                        }
                        self.errors.push(Error {
                            error_type: Type::ExpectedNumber,
                            source: self.mir[val].source,
                        });
                        return None;
                    }
                };

                RealExpression::BinaryOperator(
                    lhs,
                    Node {
                        contents: op,
                        source: op_node.source,
                    },
                    rhs,
                )
            }
            hir::Expression::Primary(Primary::BuiltInFunctionCall1p(call, arg)) => {
                RealExpression::BuiltInFunctionCall1p(call, self.fold_real_expression(arg)?)
            }
            hir::Expression::Primary(Primary::BuiltInFunctionCall2p(call, arg1, arg2)) => {
                let arg1 = self.fold_real_expression(arg1);
                let arg2 = self.fold_real_expression(arg2);
                RealExpression::BuiltInFunctionCall2p(call, arg1?, arg2?)
            }
            _ => return Some(ExpressionId::Integer(self.fold_integer_expression(expr)?)),
        };
        Some(ExpressionId::Real(self.mir.push(Node { contents, source })))
    }
}
