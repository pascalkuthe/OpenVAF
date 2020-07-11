/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
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

use crate::analysis::constant_fold::NoConstResolution;
use crate::ast;
use crate::diagnostic::{DiagnosticSlicePrinter, MultiDiagnostic, UserResult};
use crate::hir::Hir;
use crate::hir_lowering::error::Error::TypeMissmatch;
use crate::hir_lowering::error::{Error, MockType};
use crate::hir_lowering::expression_semantic::{ConstantSchematicAnalysis, SchematicAnalysis};
use crate::ir::mir::{ExpressionId, Mir, Parameter, ParameterType};
use crate::ir::{hir, IntegerExpressionId, Node, RealExpressionId};
use crate::mir::Attribute;
use crate::mir::{IntegerExpression, Module, Nature, Variable, VariableType};
use crate::SourceMap;
use std::sync::Arc;

pub mod control_flow;
pub mod error;
mod expression_semantic;

pub struct HirToMirFold<'lt> {
    pub errors: MultiDiagnostic<Error>,
    hir: &'lt Hir,
    mir: Mir,
}
impl<'lt> HirToMirFold<'lt> {
    pub fn new(hir: &'lt mut Hir) -> Self {
        Self {
            errors: MultiDiagnostic(Vec::with_capacity(32)),
            mir: Mir::initalize(hir),
            hir: &*hir,
        }
    }

    fn fold(mut self) -> Result<Mir, MultiDiagnostic<Error>> {
        self.mir.natures = self
            .hir
            .natures
            .iter()
            .map(|nature| nature.map_with(|old| self.fold_nature(old)))
            .collect();

        self.mir.parameters = self
            .hir
            .parameters
            .iter()
            .map(|param| param.map_with(|old| self.fold_parameter(old)))
            .collect();

        self.mir.variables = self
            .hir
            .variables
            .iter()
            .map(|var| var.map_with(|old| self.fold_variable(old)))
            .collect();

        self.mir.attributes = self
            .hir
            .attributes
            .iter()
            .map(|attr| {
                let value = attr
                    .value
                    .and_then(|val| ConstantSchematicAnalysis.fold_expression(&mut self, val));

                Attribute {
                    name: attr.ident,
                    value,
                }
            })
            .collect();

        self.mir.modules = self
            .hir
            .modules
            .iter()
            .map(|module| {
                module.map_with(|old| Module {
                    name: old.ident,
                    port_list: old.port_list.clone(),
                    analog_cfg: self.fold_block_into_cfg(old.analog.clone()),
                })
            })
            .collect();

        if self.errors.is_empty() {
            Ok(self.mir)
        } else {
            Err(self.errors)
        }
    }

    /// folds a variable by foldings its default value (to a typed representation)
    fn fold_variable(&mut self, variable: &ast::Variable) -> Variable {
        let variable_type = match variable.variable_type {
            ast::VariableType::REAL => {
                let default_value = variable
                    .default_value
                    .and_then(|expr| ConstantSchematicAnalysis.fold_real_expression(self, expr));
                VariableType::Real(default_value)
            }

            ast::VariableType::INTEGER => {
                let default_value = if let Some(default_value) = variable.default_value {
                    match ConstantSchematicAnalysis.fold_expression(self, default_value) {
                        Some(ExpressionId::Integer(expr)) => Some(expr),

                        Some(ExpressionId::Real(real_expr)) => {
                            Some(self.mir.integer_expressions.push(Node {
                                span: self.mir[real_expr].span,
                                contents: IntegerExpression::RealCast(real_expr),
                            }))
                        }

                        Some(ExpressionId::String(_)) => {
                            self.errors.add(TypeMissmatch {
                                expected_type: MockType::Numeric,
                                span: self.hir[default_value].span,
                            });

                            None
                        }

                        None => None,
                    }
                } else {
                    None
                };
                VariableType::Integer(default_value)
            }
        };

        Variable {
            ident: variable.name,
            variable_type,
        }
    }

    /// folds a parameter by evaluating the default value and any range bounds
    fn fold_parameter(&mut self, parameter: &ast::Parameter) -> Parameter {
        let parameter_type = match parameter.parameter_type {
            ast::ParameterType::String() => todo!("String parameters"),

            ast::ParameterType::Numerical {
                parameter_type,
                ref from_ranges,
                ref excluded,
            } => match parameter_type {
                ast::VariableType::INTEGER => {
                    let from_ranges = from_ranges
                        .iter()
                        .filter_map(|range| {
                            let start = range.start.try_copy_with(|expr| {
                                ConstantSchematicAnalysis.fold_integer_expression(self, expr)
                            });
                            let end = range.end.try_copy_with(|expr| {
                                ConstantSchematicAnalysis.fold_integer_expression(self, expr)
                            });
                            Some(start?..end?)
                        })
                        .collect();

                    let excluded = excluded
                        .iter()
                        .filter_map(|exclude| {
                            exclude.try_clone_with(|expr| {
                                ConstantSchematicAnalysis.fold_integer_expression(self, expr)
                            })
                        })
                        .collect();

                    #[allow(clippy::or_fun_call)]
                    let default_value = ConstantSchematicAnalysis
                        .fold_integer_expression(self, parameter.default_value)
                        .unwrap_or(IntegerExpressionId::from_usize_unchecked(0));

                    ParameterType::Integer {
                        from_ranges,
                        excluded,
                        default_value,
                    }
                }

                ast::VariableType::REAL => {
                    let from_ranges = from_ranges
                        .iter()
                        .filter_map(|range| {
                            let start = range.start.try_copy_with(|expr| {
                                ConstantSchematicAnalysis.fold_real_expression(self, expr)
                            });
                            let end = range.end.try_copy_with(|expr| {
                                ConstantSchematicAnalysis.fold_real_expression(self, expr)
                            });
                            Some(start?..end?)
                        })
                        .collect();

                    let excluded = excluded
                        .iter()
                        .filter_map(|exclude| {
                            exclude.try_clone_with(|expr| {
                                ConstantSchematicAnalysis.fold_real_expression(self, expr)
                            })
                        })
                        .collect();

                    #[allow(clippy::or_fun_call)]
                    let default_value = ConstantSchematicAnalysis
                        .fold_real_expression(self, parameter.default_value)
                        .unwrap_or(RealExpressionId::from_usize_unchecked(0));
                    ParameterType::Real {
                        from_ranges,
                        excluded,
                        default_value,
                    }
                }
            },
        };

        Parameter {
            ident: parameter.name,
            parameter_type,
        }
    }

    pub fn fold_nature(&mut self, nature: &hir::Nature) -> Nature {
        let units = ConstantSchematicAnalysis
            .fold_string_expression(self, nature.units)
            .and_then(|expr| {
                self.mir
                    .constant_eval_str_expr(expr, &mut NoConstResolution)
            })
            .unwrap();
        let abstol = ConstantSchematicAnalysis
            .fold_real_expression(self, nature.abstol)
            .and_then(|expr| {
                self.mir
                    .constant_eval_real_expr(expr, &mut NoConstResolution)
            })
            .unwrap();

        Nature {
            name: nature.ident,
            abstol,
            units,
            access: nature.access,
            idt_nature: nature.idt_nature,
            ddt_nature: nature.ddt_nature,
        }
    }
}

impl Hir {
    /// Folds an hir to an mir by adding and checking type information
    /// Returns any errors that occur
    pub fn lower(mut self) -> Result<Mir, MultiDiagnostic<Error>> {
        HirToMirFold::new(&mut self).fold()
    }

    /// Folds an hir to an mir by adding and checking type information
    /// Returns any errors that occur
    pub fn lower_user_facing(
        self,
        sm: &Arc<SourceMap>,
        expansion_discalimer: &'static str,
    ) -> UserResult<Mir> {
        self.lower_user_facing_with_printer(sm, expansion_discalimer)
    }

    /// Folds an hir to an mir by adding and checking type information
    /// Returns any errors that occur
    pub fn lower_user_facing_with_printer<P: DiagnosticSlicePrinter>(
        self,
        sm: &Arc<SourceMap>,
        expansion_discalimer: &'static str,
    ) -> UserResult<Mir, P> {
        self.lower()
            .map_err(|error| error.user_facing(&sm, expansion_discalimer))
    }
}
