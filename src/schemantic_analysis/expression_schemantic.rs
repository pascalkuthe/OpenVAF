use std::convert::TryInto;

use crate::ast::{BinaryOperator, BuiltInFunctionCall, Node, VariableType};
use crate::hir::Primary;
use crate::ir::{hir, IntegerExpressionId, RealExpressionId};
use crate::mir::*;
use crate::schemantic_analysis::error::{Error, Type};
use crate::schemantic_analysis::HirToMirFold;
use crate::util::Push;
use crate::{ast, ir};

impl<'tag, 'hirref> HirToMirFold<'tag, 'hirref> {
    pub fn fold_real_expression(
        &mut self,
        expr: ir::ExpressionId<'tag>,
    ) -> Result<RealExpressionId<'tag>, ()> {
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
                    VariableType::REAL | VariableType::REALTIME
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
                        return Ok(self.mir.push(Node {
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
            hir::Expression::Primary(Primary::BuiltInFunctionCall(call)) => {
                RealExpression::BuiltInFunctionCall(
                    (call, |expr| self.fold_real_expression(expr)).try_into()?,
                )
            }
            _ => RealExpression::IntegerConversion(self.fold_integer_expression(expr)?),
        };
        Ok(self.mir.push(Node { contents, source }))
    }

    fn fold_integer_expression(
        &mut self,
        expr: ir::ExpressionId<'tag>,
    ) -> Result<IntegerExpressionId<'tag>, ()> {
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

            hir::Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Min(
                arg1,
                arg2,
            ))) => {
                let arg1 = self.fold_integer_expression(arg1);
                let arg2 = self.fold_integer_expression(arg2);
                let (arg1, arg2) = (arg1?, arg2?);
                IntegerExpression::Min(arg1, arg2)
            }

            hir::Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Max(
                arg1,
                arg2,
            ))) => {
                let arg1 = self.fold_integer_expression(arg1);
                let arg2 = self.fold_integer_expression(arg2);
                let (arg1, arg2) = (arg1?, arg2?);
                IntegerExpression::Max(arg1, arg2)
            }

            hir::Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Abs(
                arg,
            ))) => IntegerExpression::Abs(self.fold_integer_expression(arg)?),

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
                let op = match op.contents {
                    BinaryOperator::LessEqual => ComparisonOperator::LessEqual,
                    BinaryOperator::LessThen => ComparisonOperator::LessThen,
                    BinaryOperator::GreaterThen => ComparisonOperator::GreaterThen,
                    BinaryOperator::GreaterEqual => ComparisonOperator::GreaterEqual,
                    BinaryOperator::LogicEqual => ComparisonOperator::LogicEqual,
                    BinaryOperator::LogicalNotEqual => ComparisonOperator::LogicalNotEqual,
                    _ => unreachable!(),
                };
                todo!()
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
                        return Err(());
                    }
                }
            }

            hir::Expression::Primary(Primary::VariableReference(variable)) => {
                match self.mir[variable].contents.variable_type {
                    VariableType::INTEGER | VariableType::TIME => {
                        IntegerExpression::VariableReference(variable)
                    }
                    _ => {
                        self.errors.push(Error {
                            error_type: Type::ExpectedIntegerVariable(variable),
                            source,
                        });
                        return Err(());
                    }
                }
            }

            _ => {
                self.errors.push(Error {
                    error_type: Type::ExpectedInteger,
                    source,
                });
                return Err(());
            }
        };

        Ok(self.mir.push(Node { source, contents }))
    }

    pub fn fold_expression(
        &mut self,
        expr: ir::ExpressionId<'tag>,
    ) -> Result<ExpressionId<'tag>, ()> {
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
                        return Ok(ExpressionId::Integer(self.mir.push(Node {
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
                };
                RealExpression::Condition(condition?, question_span, if_val, colon_span, else_val)
            }

            hir::Expression::Primary(Primary::Real(val)) => RealExpression::Literal(val),
            hir::Expression::Primary(Primary::FunctionCall(function, ref args)) => {
                todo!("return type checking");
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
                    VariableType::REAL | VariableType::REALTIME
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
                    _ => return Ok(ExpressionId::Integer(self.fold_integer_expression(expr)?)),
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
                        return Ok(ExpressionId::Integer(self.mir.push(Node {
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
            hir::Expression::Primary(Primary::BuiltInFunctionCall(call)) => {
                RealExpression::BuiltInFunctionCall(
                    (call, |expr| self.fold_real_expression(expr)).try_into()?,
                )
            }
            _ => return Ok(ExpressionId::Integer(self.fold_integer_expression(expr)?)),
        };
        Ok(ExpressionId::Real(self.mir.push(Node { contents, source })))
    }
}
