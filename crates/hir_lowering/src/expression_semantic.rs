/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use openvaf_hir::BinaryOperator;

use crate::error::Error::{
    CannotCompareStringToNumber, CondtionTypeMissmatch, ExpectedVariableForFunctionOutput,
    Recursion, TypeMissmatch, Unsupported, WrongFunctionArgCount,
};
use crate::error::{Error, MockType};
use crate::HirToMirFold;
use openvaf_constant_fold::{constant_eval_str_expr, NoConstResolution};
use openvaf_constants::Constants;
use openvaf_data_structures::index_vec::IndexVec;
use openvaf_data_structures::HashMap;
use openvaf_derivatives::AutoDiff;
use openvaf_diagnostics::Unsupported::ConstantFunctionCalls;
use openvaf_hir as hir;
use openvaf_hir::Primary;
use openvaf_ir::ids::ExpressionId as HirExpressionId;
use openvaf_ir::ids::{
    FunctionId, IntegerExpressionId, RealExpressionId, StringExpressionId, VariableId,
};
use openvaf_ir::{Attributes, Node, NoiseSource, Spanned, SystemFunctionCall};
use openvaf_ir::{DoubleArgMath, SingleArgMath};
use openvaf_mir::cfg::Terminator;
use openvaf_mir::cfg::{BasicBlock, BasicBlockId};
use openvaf_mir::{
    ComparisonOperator, ExpressionId, IntegerBinaryOperator, IntegerExpression, RealBinaryOperator,
    RealExpression, Statement, StringExpression, VariableType,
};
use openvaf_session::sourcemap::Span;

pub struct ConstantSchematicAnalysis;
impl<'lt> SchematicAnalysis<'lt> for ConstantSchematicAnalysis {
    fn calculate_function_call(
        &mut self,
        fold: &mut HirToMirFold<'lt>,
        _function: FunctionId,
        _input: HashMap<VariableId, ExpressionId>,
        _output: HashMap<VariableId, VariableId>,
        call_span: Span,
    ) -> Option<VariableId> {
        fold.errors
            .add(Unsupported(ConstantFunctionCalls, call_span));
        None
    }
}

pub struct InliningSchemanticAnalysis<'lt> {
    pub call_stack: Vec<(FunctionId, Span)>,
    pub cfg_allocator: &'lt mut IndexVec<BasicBlockId, BasicBlock>,
    pub current_block: BasicBlockId,
    pub root_span: Span,
}
impl<'lt> InliningSchemanticAnalysis<'lt> {
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
impl<'lt> SchematicAnalysis<'lt> for InliningSchemanticAnalysis<'_> {
    fn calculate_function_call(
        &mut self,
        fold: &mut HirToMirFold<'lt>,
        function: FunctionId,
        input: HashMap<VariableId, ExpressionId>,
        output: HashMap<VariableId, VariableId>,
        call_span: Span,
    ) -> Option<VariableId> {
        // Recursion is forbidden
        if self.call_stack.iter().any(|(x, _)| *x == function) {
            let function = &fold.hir[function];
            fold.errors.add(Recursion {
                function_name: function.contents.ident.name,
                recursion_span: function.span,
                recursion_traceback: self
                    .call_stack
                    .iter()
                    .map(|(id, call_span)| {
                        let function = &fold.hir[*id];
                        (function.contents.ident.name, *call_span)
                    })
                    .collect(),
            });
            return None;
        }
        self.call_stack.push((function, call_span));

        // Write locals to output args
        for (local, dst) in output {
            let expression = match fold.mir[local].contents.variable_type {
                VariableType::Real(_) => {
                    ExpressionId::Real(fold.mir.real_expressions.push(Spanned {
                        span: fold.mir[dst].span,
                        contents: RealExpression::VariableReference(local),
                    }))
                }
                VariableType::Integer(_) => {
                    ExpressionId::Integer(fold.mir.integer_expressions.push(Spanned {
                        span: fold.mir[dst].span,
                        contents: IntegerExpression::VariableReference(local),
                    }))
                }
                VariableType::String(_) => {
                    ExpressionId::String(fold.mir.string_expressions.push(Spanned {
                        span: fold.mir[dst].span,
                        contents: StringExpression::VariableReference(local),
                    }))
                }
            };
            let stmt = fold.mir.add_new_stmt(Node {
                attributes: Attributes::EMPTY,
                span: fold.mir[dst].span,
                contents: Statement::Assignment(dst, expression),
            });
            self.cfg_allocator[self.current_block].statements.push(stmt);
        }

        // creating a temporary variable this is equivalent to the original one
        let return_variable = fold
            .mir
            .variables
            .push(fold.mir[fold.hir[function].contents.return_variable]);

        let expr = match fold.mir[return_variable].contents.variable_type {
            VariableType::Real(_) => ExpressionId::Real(fold.mir.real_expressions.push(Spanned {
                span: call_span,
                contents: RealExpression::VariableReference(
                    fold.hir[function].contents.return_variable,
                ),
            })),
            VariableType::Integer(_) => {
                ExpressionId::Integer(fold.mir.integer_expressions.push(Spanned {
                    span: call_span,
                    contents: IntegerExpression::VariableReference(
                        fold.hir[function].contents.return_variable,
                    ),
                }))
            }
            VariableType::String(_) => {
                ExpressionId::String(fold.mir.string_expressions.push(Spanned {
                    span: call_span,
                    contents: StringExpression::VariableReference(
                        fold.hir[function].contents.return_variable,
                    ),
                }))
            }
        };

        let stmt = fold.mir.add_new_stmt(Node {
            attributes: Attributes::EMPTY,
            span: fold.hir[fold.hir[function].contents.return_variable].span,
            contents: Statement::Assignment(return_variable, expr),
        });

        self.cfg_allocator[self.current_block].statements.push(stmt);

        // Add function body
        self.current_block = fold.fold_block_internal(
            fold.hir[function].contents.body.clone(),
            Terminator::Goto(self.current_block),
            self.cfg_allocator,
        );

        // Write inputs to local variables
        for (local, expr) in input {
            let stmt = fold.mir.add_new_stmt(Node {
                span: expr.span(&fold.mir),
                attributes: Attributes::EMPTY,
                contents: Statement::Assignment(local, expr),
            });
            self.cfg_allocator[self.current_block].statements.push(stmt);
        }

        // Init return value to 0
        let expr = match fold.mir[return_variable].contents.variable_type {
            VariableType::Real(_) => ExpressionId::Real(fold.mir.real_expressions.push(Spanned {
                span: call_span,
                contents: RealExpression::Literal(0.0),
            })),
            VariableType::Integer(_) => {
                ExpressionId::Integer(fold.mir.integer_expressions.push(Spanned {
                    span: call_span,
                    contents: IntegerExpression::Literal(0),
                }))
            }
            VariableType::String(_) => unreachable!("Hir lowering should have errored here"),
        };

        let stmt = fold.mir.add_new_stmt(Node {
            contents: Statement::Assignment(fold.hir[function].contents.return_variable, expr),
            span: fold.hir[fold.hir[function].contents.return_variable].span,
            attributes: Attributes::EMPTY,
        });
        self.cfg_allocator[self.current_block].statements.push(stmt);

        // Now calling the function is allowed again
        self.call_stack.pop();
        Some(return_variable)
    }
}

pub trait SchematicAnalysis<'lt>: Sized {
    fn fold_expression(
        &mut self,
        fold: &mut HirToMirFold<'lt>,
        expr: HirExpressionId,
    ) -> Option<ExpressionId> {
        fold.fold_expression(expr, self)
    }

    fn fold_real_expression(
        &mut self,
        fold: &mut HirToMirFold<'lt>,
        expr: HirExpressionId,
    ) -> Option<RealExpressionId> {
        fold.fold_real_expression(expr, self)
    }

    fn fold_string_expression(
        &mut self,
        fold: &mut HirToMirFold<'lt>,
        expr: HirExpressionId,
    ) -> Option<StringExpressionId> {
        fold.fold_string_expression(expr, self)
    }

    fn fold_integer_expression(
        &mut self,
        fold: &mut HirToMirFold<'lt>,
        expr: HirExpressionId,
    ) -> Option<IntegerExpressionId> {
        fold.fold_integer_expression(expr, self)
    }

    fn calculate_function_call(
        &mut self,
        _fold: &mut HirToMirFold<'lt>,
        _function: FunctionId,
        _input: HashMap<VariableId, ExpressionId>,
        _output: HashMap<VariableId, VariableId>,
        _call_span: Span,
    ) -> Option<VariableId>;
}

impl<'hirref> HirToMirFold<'hirref> {
    pub fn fold_function_call(
        &mut self,
        function: FunctionId,
        args: &[HirExpressionId],
        span: Span,
        analysis: &mut impl SchematicAnalysis<'hirref>,
    ) -> Option<VariableId> {
        let mut input = HashMap::default();
        let mut output = HashMap::default();
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
                                    self.errors.add(ExpectedVariableForFunctionOutput(
                                        self.mir[expected.local_var].span,
                                    ));
                                    continue;
                                }
                            }
                            ExpressionId::Integer(arg) => {
                                if let IntegerExpression::VariableReference(var) =
                                    self.mir[arg].contents
                                {
                                    var
                                } else {
                                    self.errors.add(ExpectedVariableForFunctionOutput(
                                        self.mir[expected.local_var].span,
                                    ));
                                    continue;
                                }
                            }
                            ExpressionId::String(arg) => {
                                if let StringExpression::VariableReference(var) =
                                    self.mir[arg].contents
                                {
                                    var
                                } else {
                                    self.errors.add(ExpectedVariableForFunctionOutput(
                                        self.mir[expected.local_var].span,
                                    ));
                                    continue;
                                }
                            }
                        };
                        match (
                            self.mir[expected.local_var].contents.variable_type,
                            self.mir[var].contents.variable_type,
                        ) {
                            (VariableType::Real(_), VariableType::Real(_))
                            | (VariableType::Integer(_), VariableType::Integer(_))
                            | (VariableType::String(_), VariableType::String(_)) => (),
                            (VariableType::Real(_), _) => {
                                self.errors.add(Error::expected_variable_type(
                                    MockType::Real,
                                    &self.mir[var],
                                    self.mir[expected.local_var].span,
                                ));
                                continue;
                            }
                            (VariableType::Integer(_), _) => {
                                self.errors.add(Error::expected_variable_type(
                                    MockType::Integer,
                                    &self.mir[var],
                                    self.mir[expected.local_var].span,
                                ));
                                continue;
                            }
                            (VariableType::String(_), _) => {
                                self.errors.add(Error::expected_variable_type(
                                    MockType::String,
                                    &self.mir[var],
                                    self.mir[expected.local_var].span,
                                ));
                                continue;
                            }
                        }
                        output.insert(expected.local_var, var);
                    }
                }
            }
        } else {
            self.errors.add(WrongFunctionArgCount {
                expected: self.hir[function].contents.args.len() as u8,
                found: args.len(),
                span,
            })
        }
        analysis.calculate_function_call(self, function, input, output, span)
    }

    pub fn fold_real_expression(
        &mut self,
        expr: HirExpressionId,
        analysis: &mut impl SchematicAnalysis<'hirref>,
    ) -> Option<RealExpressionId> {
        let span = self.hir[expr].span;
        let contents = match self.hir[expr].contents {
            hir::Expression::Condtion(condition, if_val, else_val) => {
                let condition = analysis.fold_integer_expression(self, condition);
                let if_val = analysis.fold_real_expression(self, if_val);
                let else_val = analysis.fold_real_expression(self, else_val);
                RealExpression::Condition(condition?, if_val?, else_val?)
            }

            hir::Expression::Primary(Primary::Real(val)) => RealExpression::Literal(val),
            hir::Expression::Primary(Primary::FunctionCall(function, ref args))
                if self.hir[self.hir[function].contents.return_variable]
                    .contents
                    .var_type
                    == hir::VariableType::Real =>
            {
                RealExpression::VariableReference(self.fold_function_call(
                    function,
                    args.as_slice(),
                    span,
                    analysis,
                )?)
            }

            hir::Expression::Primary(Primary::SystemFunctionCall(
                SystemFunctionCall::Temperature,
            )) => RealExpression::Temperature,

            hir::Expression::Primary(Primary::SystemFunctionCall(SystemFunctionCall::Vt(arg))) => {
                let factor = Constants::kb(span) / Constants::q(span);
                let factor = RealExpression::Literal(factor);
                let factor = self.mir.real_expressions.push(Spanned::new(factor, span));
                let temp = arg
                    .and_then(|arg| analysis.fold_real_expression(self, arg))
                    .unwrap_or_else(|| {
                        self.mir
                            .real_expressions
                            .push(Spanned::new(RealExpression::Temperature, span))
                    });

                RealExpression::BinaryOperator(
                    factor,
                    Spanned::new(RealBinaryOperator::Multiply, span),
                    temp,
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
                        self.errors.add(TypeMissmatch {
                            span: self.hir[name].span,
                            expected_type: MockType::String,
                        });
                        return None;
                    };
                RealExpression::SimParam(name, default)
            }

            hir::Expression::Primary(Primary::BranchAccess(discipline_access, branch)) => {
                RealExpression::BranchAccess(discipline_access, branch, 0)
            }

            hir::Expression::Primary(Primary::PortFlowAccess(port)) => {
                RealExpression::PortFlowAccess(port, 0)
            }

            hir::Expression::Primary(Primary::ParameterReference(parameter))
                if matches!(
                    self.hir[parameter].contents.param_type,
                    hir::ParameterType::Real {..}
                ) =>
            {
                RealExpression::ParameterReference(parameter)
            }

            hir::Expression::Primary(Primary::VariableReference(variable))
                if matches!(
                    self.mir[variable].contents.variable_type,
                    VariableType::Real(..)
                ) =>
            {
                RealExpression::VariableReference(variable)
            }

            hir::Expression::UnaryOperator(
                Spanned {
                    contents: hir::UnaryOperator::ArithmeticNegate,
                    span: op,
                },
                parameter,
            ) => RealExpression::Negate(op, analysis.fold_real_expression(self, parameter)?),

            hir::Expression::BinaryOperator(lhs, op_node, rhs) => {
                let lhs = analysis.fold_real_expression(self, lhs);
                let rhs = analysis.fold_real_expression(self, rhs);
                let op = match op_node.contents {
                    BinaryOperator::Plus => RealBinaryOperator::Sum,
                    BinaryOperator::Minus => RealBinaryOperator::Subtract,
                    BinaryOperator::Multiply => RealBinaryOperator::Multiply,
                    BinaryOperator::Divide => RealBinaryOperator::Divide,
                    BinaryOperator::Exponent => RealBinaryOperator::Exponent,
                    BinaryOperator::Modulus => RealBinaryOperator::Modulus,
                    _ => {
                        let integer_expr = analysis.fold_integer_expression(self, expr)?;
                        return Some(self.mir.real_expressions.push(Spanned {
                            contents: RealExpression::IntegerConversion(integer_expr),
                            span,
                        }));
                    }
                };
                RealExpression::BinaryOperator(
                    lhs?,
                    Spanned {
                        contents: op,
                        span: op_node.span,
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
                let expr_to_derive = analysis.fold_expression(self, expr_to_derive)?;

                let mut ad = AutoDiff::new(&mut self.mir);
                let derivative = ad.partial_derivative(expr_to_derive, derive_by);
                self.errors.add_all(ad.errors);
                return Some(derivative);
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
                // no error here if const folding failed because that happens during ast lowering
                let name = name
                    .and_then(|name| self.fold_string_expression(name, analysis))
                    .map(|name| {
                        { constant_eval_str_expr(&self.mir, name, &mut NoConstResolution) }.unwrap()
                    });

                RealExpression::Noise(source, name)
            }

            _ => RealExpression::IntegerConversion(analysis.fold_integer_expression(self, expr)?),
        };
        Some(self.mir.real_expressions.push(Spanned { contents, span }))
    }

    pub fn fold_integer_expression(
        &mut self,
        expr: HirExpressionId,
        analysis: &mut impl SchematicAnalysis<'hirref>,
    ) -> Option<IntegerExpressionId> {
        let span = self.hir[expr].span;
        let contents = match self.hir[expr].contents {
            hir::Expression::Primary(Primary::Integer(val)) => {
                IntegerExpression::Literal(val as i64)
            }

            // Infinity can't be specified as a literal expect for parameter ranges
            // where infinity basically just means maximum and is also valid for integer
            hir::Expression::Primary(Primary::Real(x)) if x == f64::INFINITY => {
                IntegerExpression::Literal(i64::MAX)
            }

            hir::Expression::Primary(Primary::Real(x)) if x == f64::NEG_INFINITY => {
                IntegerExpression::Literal(i64::MIN)
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
                DoubleArgMath::Min,
                arg1,
                arg2,
            )) => {
                let arg1 = analysis.fold_integer_expression(self, arg1);
                let arg2 = analysis.fold_integer_expression(self, arg2);
                let (arg1, arg2) = (arg1?, arg2?);
                IntegerExpression::Min(arg1, arg2)
            }

            hir::Expression::Primary(Primary::BuiltInFunctionCall2p(
                DoubleArgMath::Max,
                arg1,
                arg2,
            )) => {
                let arg1 = analysis.fold_integer_expression(self, arg1);
                let arg2 = analysis.fold_integer_expression(self, arg2);
                let (arg1, arg2) = (arg1?, arg2?);
                IntegerExpression::Max(arg1, arg2)
            }

            hir::Expression::Primary(Primary::BuiltInFunctionCall1p(SingleArgMath::Abs, arg)) => {
                IntegerExpression::Abs(analysis.fold_integer_expression(self, arg)?)
            }

            hir::Expression::Condtion(condition, if_val, else_val) => {
                let condition = analysis.fold_integer_expression(self, condition);
                let if_val = analysis.fold_integer_expression(self, if_val);
                let else_val = analysis.fold_integer_expression(self, else_val);
                IntegerExpression::Condition(condition?, if_val?, else_val?)
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
                let op = Spanned::new(comparison_op, op.span);

                match (lhs?, rhs?) {
                    (ExpressionId::Integer(lhs), ExpressionId::Integer(rhs)) => {
                        IntegerExpression::IntegerComparison(lhs, op, rhs)
                    }

                    (ExpressionId::Real(lhs), ExpressionId::Real(rhs)) => {
                        IntegerExpression::RealComparison(lhs, op, rhs)
                    }

                    (ExpressionId::Integer(lhs), ExpressionId::Real(rhs)) => {
                        let lhs = self.mir.real_expressions.push(Spanned::new(
                            RealExpression::IntegerConversion(lhs),
                            self.mir[lhs].span,
                        ));
                        IntegerExpression::RealComparison(lhs, op, rhs)
                    }

                    (ExpressionId::Real(lhs), ExpressionId::Integer(rhs)) => {
                        let rhs = self.mir.real_expressions.push(Spanned::new(
                            RealExpression::IntegerConversion(rhs),
                            self.mir[rhs].span,
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
                        self.errors.add(TypeMissmatch {
                            expected_type: MockType::Numeric,
                            span: self.mir[lhs].span,
                        });
                        self.errors.add(TypeMissmatch {
                            expected_type: MockType::Numeric,
                            span: self.mir[rhs].span,
                        });

                        return None;
                    }
                    (lhs, rhs) => {
                        self.errors.add(CannotCompareStringToNumber(
                            lhs.span(&self.mir).extend(rhs.span(&self.mir)),
                        ));
                        return None;
                    }
                }
            }

            hir::Expression::BinaryOperator(lhs, op_node, rhs) => {
                let lhs = analysis.fold_integer_expression(self, lhs);
                let rhs = analysis.fold_integer_expression(self, rhs);
                let op = match op_node.contents {
                    BinaryOperator::Plus => IntegerBinaryOperator::Sum,
                    BinaryOperator::Minus => IntegerBinaryOperator::Subtract,
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
                    Spanned {
                        contents: op,
                        span: op_node.span,
                    },
                    rhs?,
                )
            }

            hir::Expression::Primary(Primary::FunctionCall(function, ref args))
                if self.hir[self.hir[function].contents.return_variable]
                    .contents
                    .var_type
                    == hir::VariableType::Integer =>
            {
                IntegerExpression::VariableReference(self.fold_function_call(
                    function,
                    args.as_slice(),
                    span,
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
                match self.hir[parameter].contents.param_type {
                    hir::ParameterType::Integer { .. } => {
                        IntegerExpression::ParameterReference(parameter)
                    }
                    _ => {
                        self.errors.add(Error::expected_parameter_type(
                            MockType::Integer,
                            &self.mir[parameter],
                            span,
                        ));
                        return None;
                    }
                }
            }

            hir::Expression::Primary(Primary::VariableReference(variable)) => {
                if let openvaf_mir::VariableType::Integer(..) =
                    self.mir[variable].contents.variable_type
                {
                    IntegerExpression::VariableReference(variable)
                } else {
                    self.errors.add(Error::expected_variable_type(
                        MockType::Integer,
                        &self.mir[variable],
                        span,
                    ));
                    return None;
                }
            }

            _ => {
                self.errors.add(TypeMissmatch {
                    expected_type: MockType::Integer,
                    span,
                });

                return None;
            }
        };

        Some(
            self.mir
                .integer_expressions
                .push(Spanned { span, contents }),
        )
    }

    pub fn fold_string_expression(
        &mut self,
        expr: HirExpressionId,
        analysis: &mut impl SchematicAnalysis<'hirref>,
    ) -> Option<StringExpressionId> {
        //TODO make this into a real fold like the other ones for improved error reporting (then again strings are so rare who cares)
        if let ExpressionId::String(res) = analysis.fold_expression(self, expr)? {
            Some(res)
        } else {
            self.errors.add(TypeMissmatch {
                expected_type: MockType::String,
                span: self.hir[expr].span,
            });

            None
        }
    }

    pub fn fold_expression(
        &mut self,
        expr: HirExpressionId,
        analysis: &mut impl SchematicAnalysis<'hirref>,
    ) -> Option<ExpressionId> {
        let span = self.hir[expr].span;
        let contents = match self.hir[expr].contents {
            hir::Expression::Condtion(condition, if_val, else_val) => {
                let condition = analysis.fold_integer_expression(self, condition);
                let (if_val, else_val) = match (
                    analysis.fold_expression(self, if_val)?,
                    analysis.fold_expression(self, else_val)?,
                ) {
                    (ExpressionId::Real(if_val), ExpressionId::Real(else_val)) => {
                        (if_val, else_val)
                    }
                    (ExpressionId::Real(if_val), ExpressionId::Integer(else_val)) => {
                        let else_val = self.mir.real_expressions.push(Spanned {
                            span: self.mir[else_val].span,
                            contents: RealExpression::IntegerConversion(else_val),
                        });
                        (if_val, else_val)
                    }
                    (ExpressionId::Integer(if_val), ExpressionId::Real(else_val)) => {
                        let if_val = self.mir.real_expressions.push(Spanned {
                            span: self.mir[if_val].span,
                            contents: RealExpression::IntegerConversion(if_val),
                        });
                        (if_val, else_val)
                    }
                    (ExpressionId::Integer(if_val), ExpressionId::Integer(else_val)) => {
                        return Some(ExpressionId::Integer(self.mir.integer_expressions.push(
                            Spanned {
                                contents: IntegerExpression::Condition(
                                    condition?, if_val, else_val,
                                ),
                                span,
                            },
                        )))
                    }

                    (ExpressionId::String(if_val), ExpressionId::String(else_val)) => {
                        return Some(ExpressionId::String(self.mir.string_expressions.push(
                            Spanned {
                                contents: StringExpression::Condition(condition?, if_val, else_val),
                                span,
                            },
                        )))
                    }

                    (ExpressionId::String(str), num) | (num, ExpressionId::String(str)) => {
                        self.errors.add(CondtionTypeMissmatch {
                            string: self.mir[str].span,
                            number: num.span(&self.mir),
                            span,
                        });
                        return None;
                    }
                };
                RealExpression::Condition(condition?, if_val, else_val)
            }

            hir::Expression::Primary(Primary::SystemFunctionCall(
                SystemFunctionCall::Temperature,
            )) => RealExpression::Temperature,

            hir::Expression::Primary(Primary::SystemFunctionCall(SystemFunctionCall::Vt(arg))) => {
                let factor = Constants::kb(span) / Constants::q(span);
                let factor = RealExpression::Literal(factor);
                let factor = self.mir.real_expressions.push(Spanned::new(factor, span));
                let temp = arg
                    .and_then(|arg| analysis.fold_real_expression(self, arg))
                    .unwrap_or_else(|| {
                        self.mir
                            .real_expressions
                            .push(Spanned::new(RealExpression::Temperature, span))
                    });

                RealExpression::BinaryOperator(
                    factor,
                    Spanned::new(RealBinaryOperator::Multiply, span),
                    temp,
                )
            }

            hir::Expression::Primary(Primary::SystemFunctionCall(
                SystemFunctionCall::Simparam(name, default),
            )) => {
                let default =
                    default.and_then(|default| analysis.fold_real_expression(self, default));

                let name =
                    if let ExpressionId::String(str) = analysis.fold_expression(self, name)? {
                        str
                    } else {
                        self.errors.add(TypeMissmatch {
                            expected_type: MockType::String,
                            span,
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
                        self.errors.add(TypeMissmatch {
                            expected_type: MockType::String,
                            span: self.hir[name].span,
                        });

                        return None;
                    };
                return Some(ExpressionId::String(self.mir.string_expressions.push(
                    Spanned {
                        contents: StringExpression::SimParam(name),
                        span,
                    },
                )));
            }

            hir::Expression::Primary(Primary::String(val)) => {
                return Some(ExpressionId::String(self.mir.string_expressions.push(
                    Spanned {
                        contents: StringExpression::Literal(val),
                        span,
                    },
                )))
            }
            hir::Expression::Primary(Primary::Real(val)) => RealExpression::Literal(val),

            hir::Expression::Primary(Primary::BranchAccess(discipline_access, branch)) => {
                RealExpression::BranchAccess(discipline_access, branch, 0)
            }

            hir::Expression::Primary(Primary::PortFlowAccess(port)) => {
                RealExpression::PortFlowAccess(port, 0)
            }

            hir::Expression::Primary(Primary::ParameterReference(parameter))
                if matches!(
                    self.hir[parameter].contents.param_type,
                    hir::ParameterType::Real {..}
                ) =>
            {
                RealExpression::ParameterReference(parameter)
            }

            hir::Expression::Primary(Primary::VariableReference(variable))
                if matches!(
                    self.mir[variable].contents.variable_type,
                    VariableType::Real(..)
                ) =>
            {
                RealExpression::VariableReference(variable)
            }

            hir::Expression::Primary(Primary::FunctionCall(function, ref args))
                if self.hir[self.hir[function].contents.return_variable]
                    .contents
                    .var_type
                    == hir::VariableType::Real =>
            {
                RealExpression::VariableReference(self.fold_function_call(
                    function,
                    args.as_slice(),
                    span,
                    analysis,
                )?)
            }

            hir::Expression::UnaryOperator(op, parameter)
                if op.contents == hir::UnaryOperator::ArithmeticNegate =>
            {
                let expr = analysis.fold_expression(self, parameter)?;
                let res = match expr {
                    ExpressionId::Real(arg) => {
                        let expr = Spanned::new(RealExpression::Negate(op.span, arg), span);
                        ExpressionId::Real(self.mir.real_expressions.push(expr))
                    }

                    ExpressionId::Integer(arg) => {
                        let expr = Spanned::new(IntegerExpression::UnaryOperator(op, arg), span);
                        ExpressionId::Integer(self.mir.integer_expressions.push(expr))
                    }
                    ExpressionId::String(arg) => {
                        self.errors.add(TypeMissmatch {
                            expected_type: MockType::Numeric,
                            span: self.mir[arg].span,
                        });
                        return None;
                    }
                };
                return Some(res);
            }

            hir::Expression::BinaryOperator(lhs, op_node, rhs) => {
                let op = match op_node.contents {
                    BinaryOperator::Plus => RealBinaryOperator::Sum,
                    BinaryOperator::Minus => RealBinaryOperator::Subtract,
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
                        let rhs = self.mir.real_expressions.push(Spanned {
                            span: self.mir[rhs].span,
                            contents: RealExpression::IntegerConversion(rhs),
                        });
                        (lhs, rhs)
                    }

                    (ExpressionId::Integer(lhs), ExpressionId::Real(rhs)) => {
                        let lhs = self.mir.real_expressions.push(Spanned {
                            span: self.mir[lhs].span,
                            contents: RealExpression::IntegerConversion(lhs),
                        });
                        (lhs, rhs)
                    }

                    (ExpressionId::Integer(lhs), ExpressionId::Integer(rhs)) => {
                        return Some(ExpressionId::Integer(self.mir.integer_expressions.push(
                            Spanned {
                                contents: IntegerExpression::BinaryOperator(
                                    lhs,
                                    Spanned {
                                        contents: op.into(),
                                        span: op_node.span,
                                    },
                                    rhs,
                                ),
                                span,
                            },
                        )))
                    }

                    (ExpressionId::String(val), other) | (other, ExpressionId::String(val)) => {
                        if let ExpressionId::String(other) = other {
                            self.errors.add(TypeMissmatch {
                                expected_type: MockType::Numeric,
                                span: self.mir[other].span,
                            });
                        }
                        self.errors.add(TypeMissmatch {
                            expected_type: MockType::Numeric,
                            span: self.mir[val].span,
                        });
                        return None;
                    }
                };

                RealExpression::BinaryOperator(
                    lhs,
                    Spanned {
                        contents: op,
                        span: op_node.span,
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
                let expr_to_derive = analysis.fold_expression(self, expr_to_derive)?;

                let mut ad = AutoDiff::new(&mut self.mir);
                let derivative = ad.partial_derivative(expr_to_derive, derive_by);
                self.errors.add_all(ad.errors);
                return Some(ExpressionId::Real(derivative));
            }
            _ => {
                return Some(ExpressionId::Integer(
                    analysis.fold_integer_expression(self, expr)?,
                ))
            }
        };
        Some(ExpressionId::Real(
            self.mir.real_expressions.push(Spanned { contents, span }),
        ))
    }
}
