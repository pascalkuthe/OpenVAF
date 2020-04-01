/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast;
use crate::ast::{AttributeNode, Node};
use crate::compact_arena::SafeRange;
use crate::hir::Hir;
use crate::ir::hir::Block;
use crate::ir::mir::{ExpressionId, Mir, Parameter, ParameterType};
use crate::ir::{mir, ParameterId, UnsafeWrite, VariableId, Write};
use crate::mir::*;
use crate::schemantic_analysis::error::Error;
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
        let parameters: SafeRange<ParameterId<'tag>> = self.hir.full_range();
        for parameter in parameters {
            let test = &self.hir[parameter].contents;
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
        let variables: SafeRange<VariableId> = self.mir.full_range();
        for variable in variables {
            match self.hir[variable].contents.variable_type {
                ast::VariableType::REAL | ast::VariableType::REALTIME => {
                    let default_value =
                        if let Some(default_value) = self.hir[variable].contents.default_value {
                            if let Ok(expr) = self.fold_real_expression(default_value) {
                                Some(expr)
                            } else {
                                continue;
                            }
                        } else {
                            None
                        };
                    self.mir.write(
                        variable,
                        AttributeNode {
                            attributes: self.hir[variable].attributes,
                            source: self.hir[variable].source,
                            contents: Variable {
                                name: self.hir[variable].contents.name,
                                variable_type: mir::VariableType::Real(default_value),
                            },
                        },
                    );
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

                                Err(()) => continue,
                            }
                        } else {
                            None
                        };
                    self.mir.write(
                        variable,
                        AttributeNode {
                            attributes: self.hir[variable].attributes,
                            source: self.hir[variable].source,
                            contents: Variable {
                                name: self.hir[variable].contents.name,
                                variable_type: mir::VariableType::Integer(default_value),
                            },
                        },
                    );
                }
            }
        }

        self.fold_block(self.hir.full_range());

        if self.errors.is_empty() {
            Ok(self.mir)
        } else {
            Err(self.errors)
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

pub fn run_semantic<'tag>(
    mut hir: Box<Hir<'tag>>,
    source_map: &SourceMap,
    translate_line: bool,
) -> std::result::Result<Box<Mir<'tag>>, ()> {
    let fold = HirToMirFold::new(&mut hir);
    fold.fold().map_err(|errors| {
        errors
            .iter()
            .for_each(|error| error.print(&source_map, &hir, translate_line))
    })
}
