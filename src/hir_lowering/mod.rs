/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

//! This module is responsible for lowering an [`Hir`](crate::hir::Hir) to an [`Mir`](crate::mir::Mir)
//!
//! This entails three main transformations
//!
//! * **Adding explicit type information** -
//!    Code generation generally requires explicit type information.
//!    This is represented in the MIR with distinct expression types for [real](crate::mir::RealExpression) and [integers](crate::mir::IntegerExpression).
//!    Most expressions have a distinct type input and output types. Those that do not are resolved based on type conversion rules
//!    Instead of being implicit type conversions are now distinct expressions that are added when required and legal
//!
//! * **folding constant expressions to values** -
//!    Constant expressions can generally not be evaluated as they may depend on parameters.
//!    However the constant expressions defining parameters may only depend on the default values of previously defined parameters.
//!    The evaluation of these expressions is done in the [`constant_eval`]  module.
//!
//! * **TypeChecking** -
//!     VerilogAMS only permits implicit type conversion under special circumstance and some operators are not defined for reals at all.
//!     During the other two transformations it is ensured that these rules are adhered (in fact without these rules the other transformation wouldn't be possible)
//!

use crate::ast;
use crate::ast::{AttributeNode, Node};
use crate::compact_arena::SafeRange;
use crate::hir::Hir;
use crate::hir_lowering::error::Error;
use crate::ir::hir::Block;
use crate::ir::mir::{ExpressionId, Mir, Parameter, ParameterType};
use crate::ir::{mir, ParameterId, UnsafeWrite, VariableId, Write};
use crate::mir::*;
use crate::util::{Push, SafeRangeCreation};
use crate::{hir, SourceMap};

pub mod error;

#[cfg(test)]
pub mod test;

struct HirToMirFold<'tag, 'hirref> {
    pub errors: Vec<Error<'tag>>,
    hir: &'hirref Hir<'tag>,
    mir: Box<Mir<'tag>>,
}
impl<'tag, 'hirref> HirToMirFold<'tag, 'hirref> {
    pub fn new(hir: &'hirref mut Hir<'tag>) -> Self {
        Self {
            errors: Vec::with_capacity(32),
            mir: unsafe { Mir::partial_initalize(hir) },
            hir: &*hir,
        }
    }

    pub fn fold(mut self) -> Result<Box<Mir<'tag>>, Vec<Error<'tag>>> {
        for parameter in self.hir.full_range() {
            self.fold_parameter(parameter)
        }

        for variable in self.mir.full_range() {
            self.fold_variable(variable)
        }

        self.fold_block(self.hir.full_range());

        if self.errors.is_empty() {
            Ok(self.mir)
        } else {
            Err(self.errors)
        }
    }

    /// folds a variable by foldings its default value (to a typed representation)
    fn fold_variable(&mut self, variable: VariableId<'tag>) {
        let variable_type = match self.hir[variable].contents.variable_type {
            ast::VariableType::REAL | ast::VariableType::REALTIME => {
                let default_value =
                    if let Some(default_value) = self.hir[variable].contents.default_value {
                        if let Ok(expr) = self.fold_real_expression(default_value) {
                            Some(expr)
                        } else {
                            return;
                        }
                    } else {
                        None
                    };
                VariableType::Real(default_value)
            }

            ast::VariableType::INTEGER | ast::VariableType::TIME => {
                let default_value =
                    if let Some(default_value) = self.hir[variable].contents.default_value {
                        match self.fold_expression(default_value) {
                            Ok(ExpressionId::Integer(expr)) => Some(expr),

                            Ok(ExpressionId::Real(real_expr)) => Some(self.mir.push(Node {
                                source: self.mir[real_expr].source,
                                contents: IntegerExpression::RealCast(real_expr),
                            })),

                            Err(()) => return,
                        }
                    } else {
                        None
                    };
                VariableType::Integer(default_value)
            }
        };

        self.mir.write(
            variable,
            AttributeNode {
                attributes: self.hir[variable].attributes,
                source: self.hir[variable].source,
                contents: Variable {
                    name: self.hir[variable].contents.name,
                    variable_type,
                },
            },
        );
    }

    /// folds a parameter by evaluating the default value and any range bounds
    /// # Safety
    /// This function HAS to be called for EVERY Parameter when folding an HIR to an MIR.
    /// Parameters are not initialized and when the MIR is dropped drop will be called on the parameters.
    /// This is UB since parameters do implement drop (they contain arrayS)
    /// Internally this function is very carefully written such that `self.mir[parameter]` is always initialized even when an error occurs
    fn fold_parameter(&mut self, parameter: ParameterId<'tag>) {
        let parameter_type = match self.hir[parameter].contents.parameter_type {
            ast::ParameterType::String() => todo!("String parameters"),
            ast::ParameterType::Numerical {
                parameter_type,
                ref included_ranges,
                ref excluded_ranges,
            } => match parameter_type {
                ast::VariableType::INTEGER | ast::VariableType::TIME => {
                    if let Ok(type_info) =
                        self.eval_parameter_type(parameter, included_ranges, excluded_ranges)
                    {
                        ParameterType::Integer {
                            included_ranges: type_info.0,
                            excluded_ranges: type_info.1,
                            default_value: type_info.2,
                        }
                    } else {
                        //dummy values in case of errors to ensure every parameter gets initalized to prevent UB during drop.
                        // This is okay since self.errors is not empty anymore and therefore self.mir won't be returned
                        ParameterType::Integer {
                            included_ranges: Vec::new(),
                            excluded_ranges: Vec::new(),
                            default_value: 0,
                        }
                    }
                }
                ast::VariableType::REAL | ast::VariableType::REALTIME => {
                    if let Ok(type_info) =
                        self.eval_parameter_type(parameter, included_ranges, excluded_ranges)
                    {
                        ParameterType::Real {
                            included_ranges: type_info.0,
                            excluded_ranges: type_info.1,
                            default_value: type_info.2,
                        }
                    } else {
                        //dummy values in case of errors to ensure every parameter gets initalized to prevent UB during drop.
                        // This is okay since self.errors is not empty anymore and therefore self.mir won't be returned
                        ParameterType::Real {
                            included_ranges: Vec::new(),
                            excluded_ranges: Vec::new(),
                            default_value: 0.0,
                        }
                    }
                }
            },
        };
        unsafe {
            //This is save since we write to all parameters
            self.mir.write_unsafe(
                parameter,
                AttributeNode {
                    contents: Parameter {
                        name: self.hir[parameter].contents.name,
                        parameter_type,
                    },
                    source: self.hir[parameter].source,
                    attributes: self.hir[parameter].attributes,
                },
            );
        }
    }

    fn fold_block(&mut self, statements: Block<'tag>) {
        for statement in statements {
            let res = match self.hir[statement] {
                hir::Statement::ConditionStart {
                    condition_info_and_end,
                } => Statement::ConditionStart {
                    condition_info_and_end,
                },
                hir::Statement::Contribute(attributes, discipline_access, branch, expr) => {
                    if let Ok(expr) = self.fold_real_expression(expr) {
                        Statement::Contribute(attributes, discipline_access, branch, expr)
                    } else {
                        continue;
                    }
                }
                hir::Statement::Assignment(attr, variable, expr)
                    if matches!(
                        self.mir[variable].contents.variable_type,
                        VariableType::Real(..)
                    ) =>
                {
                    if let Ok(expr) = self.fold_real_expression(expr) {
                        Statement::Assignment(attr, variable, ExpressionId::Real(expr))
                    } else {
                        continue;
                    }
                }
                hir::Statement::Assignment(attr, variable, expr) => {
                    match self.fold_expression(expr) {
                        Ok(ExpressionId::Integer(id)) => {
                            Statement::Assignment(attr, variable, ExpressionId::Integer(id))
                        }
                        Ok(ExpressionId::Real(id)) => {
                            let expr = self.mir.push(Node {
                                source: self.mir[id].source,
                                contents: IntegerExpression::RealCast(id),
                            });
                            Statement::Assignment(attr, variable, ExpressionId::Integer(expr))
                        }
                        Err(()) => continue,
                    }
                }
                hir::Statement::Condition(AttributeNode {
                    attributes,
                    source,
                    contents: ref condition,
                }) => {
                    let main_condition = self.fold_integer_expression(condition.main_condition);
                    self.fold_block(condition.main_condition_statements);
                    let else_ifs = condition
                        .else_ifs
                        .iter()
                        .copied()
                        .filter_map(|(condition, block)| {
                            self.fold_block(block);
                            if let Ok(condition) = self.fold_integer_expression(condition) {
                                Some((condition, block))
                            } else {
                                None
                            }
                        })
                        .collect();
                    self.fold_block(condition.else_statement);
                    let main_condition = if let Ok(main_condition) = main_condition {
                        main_condition
                    } else {
                        continue;
                    };
                    Statement::Condition(AttributeNode {
                        attributes,
                        source,
                        contents: Condition {
                            main_condition,
                            main_condition_statements: condition.main_condition_statements,
                            else_ifs,
                            else_statement: condition.else_statement,
                        },
                    })
                }
                hir::Statement::FunctionCall(_, _, _) => todo!("Function Calls"),
                hir::Statement::BuiltInFunctionCall(call) => todo!("warn useless function calls"),
            };
            self.mir.push(res);
        }
    }
}

mod constant_eval;
mod expression_semantic;

pub fn fold_hir_to_mir(mut hir: Box<Hir>) -> std::result::Result<Box<Mir>, (Vec<Error>, Box<Hir>)> {
    HirToMirFold::new(&mut hir)
        .fold()
        .map_err(|errors| (errors, hir))
}

pub fn fold_hir_to_mir_and_print_errors<'tag>(
    mut hir: Box<Hir<'tag>>,
    source_map: &SourceMap,
    translate_line: bool,
) -> std::result::Result<Box<Mir<'tag>>, ()> {
    fold_hir_to_mir(hir).map_err(|(errors, hir)| {
        errors
            .into_iter()
            .for_each(|error| error.print(source_map, &hir, translate_line))
    })
}
