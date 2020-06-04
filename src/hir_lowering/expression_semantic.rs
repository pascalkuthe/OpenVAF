/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::BinaryOperator;

use crate::cfg::Terminator;
use crate::cfg::{BasicBlock, BasicBlockId};
use crate::hir::Primary;
use crate::hir_lowering::derivatives::Unknown;
use crate::hir_lowering::error::{Error, Type};
use crate::hir_lowering::HirToMirFold;
use crate::ir::{
    hir, Attributes, FunctionId, IntegerExpressionId, Node, NoiseSource, RealExpressionId,
    StringExpressionId, SystemFunctionCall, VariableId,
};
use crate::ir::{BuiltInFunctionCall1p, BuiltInFunctionCall2p};
use crate::mir::*;
use crate::parser::error::Unsupported;
use crate::{ast, ir, mir, Span};
use index_vec::IndexVec;
use rustc_hash::FxHashMap;

pub struct ConstantSchematicAnalysis();
impl<'lt> SchematicAnalysis<'lt> for ConstantSchematicAnalysis {}

pub struct FunctionInline<'lt> {
    pub call_stack: Vec<FunctionId>,
    pub cfg_allocator: &'lt mut IndexVec<BasicBlockId, BasicBlock>,
    pub current_block: BasicBlockId,
    pub root_span: Span,
}
impl<'lt> FunctionInline<'lt> {
    pub fn new(
        cfg_allocator: &'lt mut IndexVec<BasicBlockId, BasicBlock>,
        current_block: BasicBlockId,
        root_span: Span,
    ) -> Self {
        Self {
            call_stack: Vec::new(),
            cfg_allocator,
            current_block,
            root_span,
        }
    }
}
impl<'lt> SchematicAnalysis<'lt> for FunctionInline<'_> {
    fn calculate_function_call(
        &mut self,
        fold: &mut HirToMirFold<'lt>,
        function: FunctionId,
        input: FxHashMap<VariableId, ExpressionId>,
        output: FxHashMap<VariableId, VariableId>,
        call_span: Span,
    ) -> Option<VariableId> {
        // Recursion is forbidden
        if self.call_stack.contains(&function) {
            fold.errors.push(Error {
                error_type: Type::Recursion(function, self.call_stack.clone(), self.root_span),
                source: call_span,
            });
            return None;
        }
        self.call_stack.push(function);

        // Write locals to output args
        for (local, dst) in output {
            let expression = match fold.mir[local].contents.variable_type {
                VariableType::Real(_) => ExpressionId::Real(fold.mir.real_expressions.push(Node {
                    source: fold.mir[dst].source,
                    contents: RealExpression::VariableReference(local),
                })),
                VariableType::Integer(_) => {
                    ExpressionId::Integer(fold.mir.integer_expressions.push(Node {
                        source: fold.mir[dst].source,
                        contents: IntegerExpression::VariableReference(local),
                    }))
                }
            };
            let stmt = fold.mir.statements.push(Statement::Assignment(
                Attributes::empty(),
                dst,
                expression,
            ));
            self.cfg_allocator[self.current_block].statements.push(stmt);
        }

        // creating a temporary variable this is equivalent to the original one
        let return_variable = fold
            .mir
            .variables
            .push(fold.mir[fold.hir[function].contents.return_variable]);

        let expr = match fold.mir[return_variable].contents.variable_type {
            VariableType::Real(_) => ExpressionId::Real(fold.mir.real_expressions.push(Node {
                source: call_span,
                contents: RealExpression::VariableReference(
                    fold.hir[function].contents.return_variable,
                ),
            })),
            VariableType::Integer(_) => {
                ExpressionId::Integer(fold.mir.integer_expressions.push(Node {
                    source: call_span,
                    contents: IntegerExpression::VariableReference(
                        fold.hir[function].contents.return_variable,
                    ),
                }))
            }
        };

        let stmt = fold.mir.statements.push(Statement::Assignment(
            Attributes::empty(),
            return_variable,
            expr,
        ));
        self.cfg_allocator[self.current_block].statements.push(stmt);


        // Add function body
        self.current_block = fold.fold_block_internal(
            fold.hir[function].contents.body.clone(),
            Terminator::Goto(self.current_block),
            self.cfg_allocator,
        );

        // Write inputs to local variables
        for (local, expr) in input {
            let stmt =
                fold.mir
                    .statements
                    .push(Statement::Assignment(Attributes::empty(), local, expr));
            self.cfg_allocator[self.current_block].statements.push(stmt);
        }

        // Init return value to 0
        let expr = match fold.mir[return_variable].contents.variable_type {
            VariableType::Real(_) => ExpressionId::Real(fold.mir.real_expressions.push(Node {
                source: call_span,
                contents: RealExpression::Literal(0.0),
            })),
            VariableType::Integer(_) => {
                ExpressionId::Integer(fold.mir.integer_expressions.push(Node {
                    source: call_span,
                    contents: IntegerExpression::Literal(0),
                }))
            }
        };

        let stmt = fold.mir.statements.push(Statement::Assignment(
            Attributes::empty(),
            fold.hir[function].contents.return_variable,
            expr,
        ));
        self.cfg_allocator[self.current_block].statements.push(stmt);

        // Now calling the function is allowed again
        self.call_stack.pop();
        Some(return_variable)
    }
}

pub struct AssignmentSchematicAnalysis<'lt> {
    pub dst: VariableId,
    pub inliner: FunctionInline<'lt>,
}
impl<'lt> AssignmentSchematicAnalysis<'lt> {
    pub fn new(
        dst: VariableId,
        assignment_span: Span,
        cfg_allocator: &'lt mut IndexVec<BasicBlockId, BasicBlock>,
        current_block: BasicBlockId,
    ) -> Self {
        Self {
            dst,
            inliner: FunctionInline::new(cfg_allocator, current_block, assignment_span),
        }
    }
}
impl<'lt> SchematicAnalysis<'lt> for AssignmentSchematicAnalysis<'_> {
    fn derivative_of_variable(
        &mut self,
        fold: &mut HirToMirFold<'lt>,
        var: VariableId,
        derive_by: Unknown,
    ) -> VariableId {
        if self.dst == var {
            fold.errors.push(Error {
                error_type: Type::Unsupported(Unsupported::SelfDerivingAssignments),
                source: self.inliner.root_span,
            })
        }
        fold.derivative_of_reference(var, derive_by)
    }

    fn calculate_function_call(
        &mut self,
        fold: &mut HirToMirFold<'lt>,
        function: FunctionId,
        input: FxHashMap<VariableId, ExpressionId>,
        output: FxHashMap<VariableId, VariableId>,
        call_span: Span,
    ) -> Option<VariableId> {
        self.inliner
            .calculate_function_call(fold, function, input, output, call_span)
    }
}

pub trait SchematicAnalysis<'lt>: Sized {
    fn fold_expression(
        &mut self,
        fold: &mut HirToMirFold<'lt>,
        expr: ir::ExpressionId,
    ) -> Option<ExpressionId> {
        fold.fold_expression(expr, self)
    }

    fn fold_real_expression(
        &mut self,
        fold: &mut HirToMirFold<'lt>,
        expr: ir::ExpressionId,
    ) -> Option<RealExpressionId> {
        fold.fold_real_expression(expr, self)
    }

    fn fold_string_expression(
        &mut self,
        fold: &mut HirToMirFold<'lt>,
        expr: ir::ExpressionId,
    ) -> Option<StringExpressionId> {
        fold.fold_string_expression(expr, self)
    }

    fn fold_integer_expression(
        &mut self,
        fold: &mut HirToMirFold<'lt>,
        expr: ir::ExpressionId,
    ) -> Option<IntegerExpressionId> {
        fold.fold_integer_expression(expr, self)
    }

    fn derivative_of_variable(
        &mut self,
        fold: &mut HirToMirFold<'lt>,
        var: VariableId,
        derive_by: Unknown,
    ) -> VariableId {
        fold.derivative_of_reference(var, derive_by)
    }

    fn calculate_function_call(
        &mut self,
        fold: &mut HirToMirFold<'lt>,
        _function: FunctionId,
        _input: FxHashMap<VariableId, ExpressionId>,
        _output: FxHashMap<VariableId, VariableId>,
        call_span: Span,
    ) -> Option<VariableId> {
        fold.errors.push(Error {
            error_type: Type::Unsupported(Unsupported::ConstantFunctionCalls),
            source: call_span,
        });
        None
    }
}

impl<'hirref> HirToMirFold<'hirref> {
    pub fn fold_function_args(
        &mut self,
        function: FunctionId,
        args: &[ir::ids::ExpressionId],
        source: Span,
        analysis: &mut impl SchematicAnalysis<'hirref>,
    ) -> Option<VariableId> {
        let mut input = FxHashMap::default();
        let mut output = FxHashMap::default();
        if args.len() == self.hir[function].contents.args.len() {
            for (&arg, &expected) in args.iter().zip(self.hir[function].contents.args.iter()) {
                if let Some(arg) = analysis.fold_expression(self, arg) {
                    if expected.input {
                        let arg = match (arg, self.mir[expected.local_var].contents.variable_type) {
                            (ExpressionId::Real(arg), VariableType::Integer(_)) => {
                                ExpressionId::Integer(
                                    self.mir.integer_expressions.push(
                                        self.mir[arg].clone_as(IntegerExpression::RealCast(arg)),
                                    ),
                                )
                            }
                            (ExpressionId::Integer(arg), VariableType::Real(_)) => {
                                ExpressionId::Real(self.mir.real_expressions.push(
                                    self.mir[arg].clone_as(RealExpression::IntegerConversion(arg)),
                                ))
                            }
                            (arg, _) => arg,
                        };
                        input.insert(expected.local_var, arg);
                    }
                    if expected.output {
                        let var = match arg {
                            ExpressionId::Real(arg) => {
                                if let RealExpression::VariableReference(var) =
                                    self.mir[arg].contents
                                {
                                    var
                                } else {
                                    self.errors.push(Error {
                                        error_type: Type::ExpectedVariableForFunctionOutput,
                                        source: self.mir[expected.local_var].source,
                                    });
                                    continue;
                                }
                            }
                            ExpressionId::Integer(arg) => {
                                if let IntegerExpression::VariableReference(var) =
                                    self.mir[arg].contents
                                {
                                    var
                                } else {
                                    self.errors.push(Error {
                                        error_type: Type::ExpectedVariableForFunctionOutput,
                                        source: self.mir[expected.local_var].source,
                                    });
                                    continue;
                                }
                            }
                            ExpressionId::String(arg) => {
                                if let StringExpression::VariableReference(var) =
                                    self.mir[arg].contents
                                {
                                    var
                                } else {
                                    self.errors.push(Error {
                                        error_type: Type::ExpectedVariableForFunctionOutput,
                                        source: self.mir[expected.local_var].source,
                                    });
                                    continue;
                                }
                            }
                        };
                        match (
                            self.mir[expected.local_var].contents.variable_type,
                            self.mir[var].contents.variable_type,
                        ) {
                            (VariableType::Real(_), VariableType::Real(_)) => (),
                            (VariableType::Integer(_), VariableType::Integer(_)) => (),
                            (VariableType::Real(_), _) => {
                                self.errors.push(Error {
                                    error_type: Type::ExpectedRealVariable(var),
                                    source: self.mir[expected.local_var].source,
                                });
                                continue;
                            }
                            (VariableType::Integer(_), _) => {
                                self.errors.push(Error {
                                    error_type: Type::ExpectedIntegerVariable(var),
                                    source: self.mir[expected.local_var].source,
                                });
                                continue;
                            }
                        }
                        output.insert(expected.local_var, var);
                    }
                }
            }
        } else {
            self.errors.push(Error {
                error_type: Type::WrongFunctionArgCount(
                    args.len(),
                    self.hir[function].contents.args.len(),
                ),
                source,
            })
        }
        analysis.calculate_function_call(self, function, input, output, source)
    }

    pub fn fold_real_expression(
        &mut self,
        expr: ir::ExpressionId,
        analysis: &mut impl SchematicAnalysis<'hirref>,
    ) -> Option<RealExpressionId> {
        let source = self.hir[expr].source;
        let contents = match self.hir[expr].contents {
            hir::Expression::Condtion(condition, question_span, if_val, colon_span, else_val) => {
                let condition = analysis.fold_integer_expression(self, condition);
                let if_val = analysis.fold_real_expression(self, if_val);
                let else_val = analysis.fold_real_expression(self, else_val);
                RealExpression::Condition(condition?, question_span, if_val?, colon_span, else_val?)
            }

            hir::Expression::Primary(Primary::Real(val)) => RealExpression::Literal(val),
            hir::Expression::Primary(Primary::FunctionCall(function, ref args))
                if self.hir[self.hir[function].contents.return_variable]
                    .contents
                    .variable_type
                    == ast::VariableType::REAL =>
            {
                RealExpression::VariableReference(self.fold_function_args(
                    function,
                    args.as_slice(),
                    source,
                    analysis,
                )?)
            }

            hir::Expression::Primary(Primary::SystemFunctionCall(
                SystemFunctionCall::Temperature,
            )) => RealExpression::Temperature,

            hir::Expression::Primary(Primary::SystemFunctionCall(SystemFunctionCall::Vt(arg))) => {
                RealExpression::Vt(
                    arg.map(|arg| analysis.fold_real_expression(self, arg))
                        .flatten(),
                )
            }

            hir::Expression::Primary(Primary::SystemFunctionCall(
                SystemFunctionCall::Simparam(name, default),
            )) => {
                let default = default
                    .map(|default| analysis.fold_real_expression(self, default))
                    .flatten();
                let name =
                    if let ExpressionId::String(str) = analysis.fold_expression(self, name)? {
                        str
                    } else {
                        self.errors.push(Error {
                            error_type: Type::ExpectedString,
                            source: self.hir[name].source,
                        });
                        return None;
                    };
                RealExpression::SimParam(name, default)
            }

            hir::Expression::Primary(Primary::BranchAccess(discipline_access, branch)) => {
                RealExpression::BranchAccess(discipline_access, branch, 0)
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
            ) => RealExpression::Negate(op, analysis.fold_real_expression(self, parameter)?),

            hir::Expression::BinaryOperator(lhs, op_node, rhs) => {
                let lhs = analysis.fold_real_expression(self, lhs);
                let rhs = analysis.fold_real_expression(self, rhs);
                let op = match op_node.contents {
                    BinaryOperator::Sum => RealBinaryOperator::Sum,
                    BinaryOperator::Subtract => RealBinaryOperator::Subtract,
                    BinaryOperator::Multiply => RealBinaryOperator::Multiply,
                    BinaryOperator::Divide => RealBinaryOperator::Divide,
                    BinaryOperator::Exponent => RealBinaryOperator::Exponent,
                    BinaryOperator::Modulus => RealBinaryOperator::Modulus,
                    _ => {
                        let integer_expr = analysis.fold_integer_expression(self, expr)?;
                        return Some(self.mir.real_expressions.push(Node {
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

            hir::Expression::Primary(Primary::BuiltInFunctionCall1p(call, arg)) => {
                RealExpression::BuiltInFunctionCall1p(
                    call,
                    analysis.fold_real_expression(self, arg)?,
                )
            }

            hir::Expression::Primary(Primary::BuiltInFunctionCall2p(call, arg1, arg2)) => {
                let arg1 = analysis.fold_real_expression(self, arg1);
                let arg2 = analysis.fold_real_expression(self, arg2);
                RealExpression::BuiltInFunctionCall2p(call, arg1?, arg2?)
            }

            hir::Expression::Primary(Primary::Derivative(expr_to_derive, derive_by)) => {
                let expr_to_derive = analysis.fold_real_expression(self, expr_to_derive)?;
                return Some(self.partial_derivative(
                    expr_to_derive,
                    derive_by,
                    &mut |fold, var| analysis.derivative_of_variable(fold, var, derive_by),
                )?);
            }

            hir::Expression::Primary(Primary::Noise(source, name)) => {
                let source = match source {
                    NoiseSource::White(expr) => {
                        NoiseSource::White(analysis.fold_real_expression(self, expr)?)
                    }
                    NoiseSource::Flicker(expr1, expr2) => {
                        let expr1 = analysis.fold_real_expression(self, expr1);
                        let expr2 = analysis.fold_real_expression(self, expr2);
                        NoiseSource::Flicker(expr1?, expr2?)
                    }
                    NoiseSource::Table(_) | NoiseSource::TableLog(_) => todo!(),
                };
                RealExpression::Noise(source, name)
            }

            _ => RealExpression::IntegerConversion(analysis.fold_integer_expression(self, expr)?),
        };
        Some(self.mir.real_expressions.push(Node { contents, source }))
    }

    pub fn fold_integer_expression(
        &mut self,
        expr: ir::ExpressionId,
        analysis: &mut impl SchematicAnalysis<'hirref>,
    ) -> Option<IntegerExpressionId> {
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
                IntegerExpression::UnaryOperator(op, analysis.fold_integer_expression(self, arg)?)
            }

            hir::Expression::Primary(Primary::BuiltInFunctionCall2p(
                BuiltInFunctionCall2p::Min,
                arg1,
                arg2,
            )) => {
                let arg1 = analysis.fold_integer_expression(self, arg1);
                let arg2 = analysis.fold_integer_expression(self, arg2);
                let (arg1, arg2) = (arg1?, arg2?);
                IntegerExpression::Min(arg1, arg2)
            }

            hir::Expression::Primary(Primary::BuiltInFunctionCall2p(
                BuiltInFunctionCall2p::Max,
                arg1,
                arg2,
            )) => {
                let arg1 = analysis.fold_integer_expression(self, arg1);
                let arg2 = analysis.fold_integer_expression(self, arg2);
                let (arg1, arg2) = (arg1?, arg2?);
                IntegerExpression::Max(arg1, arg2)
            }

            hir::Expression::Primary(Primary::BuiltInFunctionCall1p(
                BuiltInFunctionCall1p::Abs,
                arg,
            )) => IntegerExpression::Abs(analysis.fold_integer_expression(self, arg)?),

            hir::Expression::Condtion(condition, question_span, if_val, colon_span, else_val) => {
                let condition = analysis.fold_integer_expression(self, condition);
                let if_val = analysis.fold_integer_expression(self, if_val);
                let else_val = analysis.fold_integer_expression(self, else_val);
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
                let lhs = analysis.fold_expression(self, lhs);
                let rhs = analysis.fold_expression(self, rhs);
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
                        let lhs = self.mir.real_expressions.push(Node::new(
                            RealExpression::IntegerConversion(lhs),
                            self.mir[lhs].source,
                        ));
                        IntegerExpression::RealComparison(lhs, op, rhs)
                    }

                    (ExpressionId::Real(lhs), ExpressionId::Integer(rhs)) => {
                        let rhs = self.mir.real_expressions.push(Node::new(
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
                let lhs = analysis.fold_integer_expression(self, lhs);
                let rhs = analysis.fold_integer_expression(self, rhs);
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

            hir::Expression::Primary(Primary::FunctionCall(function, ref args))
                if self.hir[self.hir[function].contents.return_variable]
                    .contents
                    .variable_type
                    == ast::VariableType::INTEGER =>
            {
                IntegerExpression::VariableReference(self.fold_function_args(
                    function,
                    args.as_slice(),
                    source,
                    analysis,
                )?)
            }

            hir::Expression::Primary(Primary::SystemFunctionCall(
                SystemFunctionCall::ParameterGiven(param),
            )) => IntegerExpression::ParamGiven(param),

            hir::Expression::Primary(Primary::SystemFunctionCall(
                SystemFunctionCall::PortConnected(port),
            )) => IntegerExpression::PortConnected(port),

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

        Some(self.mir.integer_expressions.push(Node { source, contents }))
    }

    pub fn fold_string_expression(
        &mut self,
        expr: ir::ExpressionId,
        analysis: &mut impl SchematicAnalysis<'hirref>,
    ) -> Option<StringExpressionId> {
        //TODO make this into a real fold like the other ones for improved error reporting (then again strings are so rare who cares)
        if let ExpressionId::String(res) = analysis.fold_expression(self, expr)? {
            Some(res)
        } else {
            self.errors.push(Error {
                error_type: Type::ExpectedString,
                source: self.hir[expr].source,
            });
            None
        }
    }

    pub fn fold_expression(
        &mut self,
        expr: ir::ExpressionId,
        analysis: &mut impl SchematicAnalysis<'hirref>,
    ) -> Option<ExpressionId> {
        let source = self.hir[expr].source;
        let contents = match self.hir[expr].contents {
            hir::Expression::Condtion(condition, question_span, if_val, colon_span, else_val) => {
                let condition = analysis.fold_integer_expression(self, condition);
                let (if_val, else_val) = match (
                    analysis.fold_expression(self, if_val)?,
                    analysis.fold_expression(self, else_val)?,
                ) {
                    (ExpressionId::Real(if_val), ExpressionId::Real(else_val)) => {
                        (if_val, else_val)
                    }
                    (ExpressionId::Real(if_val), ExpressionId::Integer(else_val)) => {
                        let else_val = self.mir.real_expressions.push(Node {
                            source: self.mir[else_val].source,
                            contents: RealExpression::IntegerConversion(else_val),
                        });
                        (if_val, else_val)
                    }
                    (ExpressionId::Integer(if_val), ExpressionId::Real(else_val)) => {
                        let if_val = self.mir.real_expressions.push(Node {
                            source: self.mir[if_val].source,
                            contents: RealExpression::IntegerConversion(if_val),
                        });
                        (if_val, else_val)
                    }
                    (ExpressionId::Integer(if_val), ExpressionId::Integer(else_val)) => {
                        return Some(ExpressionId::Integer(self.mir.integer_expressions.push(
                            Node {
                                contents: IntegerExpression::Condition(
                                    condition?,
                                    question_span,
                                    if_val,
                                    colon_span,
                                    else_val,
                                ),
                                source,
                            },
                        )))
                    }

                    (ExpressionId::String(if_val), ExpressionId::String(else_val)) => {
                        return Some(ExpressionId::String(self.mir.string_expressions.push(
                            Node {
                                contents: StringExpression::Condition(
                                    condition?,
                                    question_span,
                                    if_val,
                                    colon_span,
                                    else_val,
                                ),
                                source,
                            },
                        )))
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

            hir::Expression::Primary(Primary::SystemFunctionCall(
                SystemFunctionCall::Temperature,
            )) => RealExpression::Temperature,

            hir::Expression::Primary(Primary::SystemFunctionCall(SystemFunctionCall::Vt(arg))) => {
                RealExpression::Vt(
                    arg.map(|arg| analysis.fold_real_expression(self, arg))
                        .flatten(),
                )
            }

            hir::Expression::Primary(Primary::SystemFunctionCall(
                SystemFunctionCall::Simparam(name, default),
            )) => {
                let default = default
                    .map(|default| analysis.fold_real_expression(self, default))
                    .flatten();
                let name =
                    if let ExpressionId::String(str) = analysis.fold_expression(self, name)? {
                        str
                    } else {
                        self.errors.push(Error {
                            error_type: Type::ExpectedString,
                            source: self.hir[name].source,
                        });
                        return None;
                    };
                RealExpression::SimParam(name, default)
            }

            hir::Expression::Primary(Primary::SystemFunctionCall(
                SystemFunctionCall::SimparamStr(name),
            )) => {
                let name =
                    if let ExpressionId::String(str) = analysis.fold_expression(self, name)? {
                        str
                    } else {
                        self.errors.push(Error {
                            error_type: Type::ExpectedString,
                            source: self.hir[name].source,
                        });
                        return None;
                    };
                return Some(ExpressionId::String(self.mir.string_expressions.push(
                    Node {
                        contents: StringExpression::SimParam(name),
                        source,
                    },
                )));
            }

            hir::Expression::Primary(Primary::String(val)) => {
                return Some(ExpressionId::String(self.mir.string_expressions.push(
                    Node {
                        contents: StringExpression::Literal(val),
                        source,
                    },
                )))
            }
            hir::Expression::Primary(Primary::Real(val)) => RealExpression::Literal(val),

            hir::Expression::Primary(Primary::BranchAccess(discipline_access, branch)) => {
                RealExpression::BranchAccess(discipline_access, branch, 0)
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

            hir::Expression::Primary(Primary::FunctionCall(function, ref args))
                if self.hir[self.hir[function].contents.return_variable]
                    .contents
                    .variable_type
                    == ast::VariableType::REAL =>
            {
                RealExpression::VariableReference(self.fold_function_args(
                    function,
                    args.as_slice(),
                    source,
                    analysis,
                )?)
            }

            hir::Expression::UnaryOperator(
                Node {
                    contents: ast::UnaryOperator::ArithmeticNegate,
                    source: op,
                },
                parameter,
            ) => RealExpression::Negate(op, analysis.fold_real_expression(self, parameter)?),

            hir::Expression::BinaryOperator(lhs, op_node, rhs) => {
                let op = match op_node.contents {
                    BinaryOperator::Sum => RealBinaryOperator::Sum,
                    BinaryOperator::Subtract => RealBinaryOperator::Subtract,
                    BinaryOperator::Multiply => RealBinaryOperator::Multiply,
                    BinaryOperator::Divide => RealBinaryOperator::Divide,
                    BinaryOperator::Exponent => RealBinaryOperator::Exponent,
                    BinaryOperator::Modulus => RealBinaryOperator::Modulus,
                    _ => {
                        return Some(ExpressionId::Integer(
                            analysis.fold_integer_expression(self, expr)?,
                        ))
                    }
                };

                let (lhs, rhs) = match (
                    analysis.fold_expression(self, lhs)?,
                    analysis.fold_expression(self, rhs)?,
                ) {
                    (ExpressionId::Real(lhs), ExpressionId::Real(rhs)) => (lhs, rhs),
                    (ExpressionId::Real(lhs), ExpressionId::Integer(rhs)) => {
                        let rhs = self.mir.real_expressions.push(Node {
                            source: self.mir[rhs].source,
                            contents: RealExpression::IntegerConversion(rhs),
                        });
                        (lhs, rhs)
                    }

                    (ExpressionId::Integer(lhs), ExpressionId::Real(rhs)) => {
                        let lhs = self.mir.real_expressions.push(Node {
                            source: self.mir[lhs].source,
                            contents: RealExpression::IntegerConversion(lhs),
                        });
                        (lhs, rhs)
                    }

                    (ExpressionId::Integer(lhs), ExpressionId::Integer(rhs)) => {
                        return Some(ExpressionId::Integer(self.mir.integer_expressions.push(
                            Node {
                                contents: IntegerExpression::BinaryOperator(
                                    lhs,
                                    Node {
                                        contents: op.into(),
                                        source: op_node.source,
                                    },
                                    rhs,
                                ),
                                source,
                            },
                        )))
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
                RealExpression::BuiltInFunctionCall1p(
                    call,
                    analysis.fold_real_expression(self, arg)?,
                )
            }
            hir::Expression::Primary(Primary::BuiltInFunctionCall2p(call, arg1, arg2)) => {
                let arg1 = analysis.fold_real_expression(self, arg1);
                let arg2 = analysis.fold_real_expression(self, arg2);
                RealExpression::BuiltInFunctionCall2p(call, arg1?, arg2?)
            }
            hir::Expression::Primary(Primary::Derivative(expr_to_derive, derive_by)) => {
                let expr_to_derive = analysis.fold_real_expression(self, expr_to_derive)?;
                return Some(ExpressionId::Real(self.partial_derivative(
                    expr_to_derive,
                    derive_by,
                    &mut |fold, var| analysis.derivative_of_variable(fold, var, derive_by),
                )?));
            }
            _ => {
                return Some(ExpressionId::Integer(
                    analysis.fold_integer_expression(self, expr)?,
                ))
            }
        };
        Some(ExpressionId::Real(
            self.mir.real_expressions.push(Node { contents, source }),
        ))
    }
}
