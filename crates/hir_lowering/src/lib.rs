/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

//! This module is responsible for lowering an [`Hir`](openvaf_hir::Hir) to an [`Mir`](openvaf_mir::Mir)
//!
//! This entails three main transformations
//!
//! ## Adding explicit type information
//!
//! Code generation generally requires explicit type information.
//! This is represented in the MIR with distinct expression types for [real](openvaf_mir::RealExpression) and [integers](openvaf_mir::IntegerExpression).
//! Most expressions have a distinct type input and output types. Those that do not are resolved based on type conversion rules
//! Instead of being implicit type conversions are now distinct expressions that are added when required and legal
//!
//! ## TypeChecking
//!
//! VerilogAMS only permits implicit type conversion under special circumstance and some operators are not defined for reals at all.
//! During the other two transformations it is ensured that these rules are adhered (in fact without these rules the other transformation wouldn't be possible)
//!

use crate::error::Error::TypeMissmatch;
use crate::error::{Error, MockType};
use crate::expression_semantic::{ConstantSchematicAnalysis, SchematicAnalysis};
use openvaf_constant_fold::{constant_eval_real_expr, constant_eval_str_expr, NoConstResolution};
use openvaf_data_structures::sync::{Lrc, RwLock};
use openvaf_diagnostics::{DiagnosticSlicePrinter, MultiDiagnostic, UserResult};
use openvaf_hir as hir;
use openvaf_hir::Hir;
use openvaf_ir::ids::{IntegerExpressionId, RealExpressionId, StringExpressionId};
use openvaf_ir::Spanned;
use openvaf_mir::{
    Attribute, ExpressionId, IntegerExpression, Mir, Module, Nature, Parameter, ParameterType,
    Variable, VariableType,
};
use openvaf_session::sourcemap::StringLiteral;
mod control_flow;
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
                    .iter()
                    .filter_map(|&val| ConstantSchematicAnalysis.fold_expression(&mut self, val))
                    .collect();

                Attribute {
                    ident: attr.ident,
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
                    ident: old.ident,
                    port_list: old.ports.clone(),
                    analog_cfg: Lrc::new(RwLock::new(self.fold_block_into_cfg(old.analog.clone()))),
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
    fn fold_variable(&mut self, variable: &hir::Variable) -> Variable {
        let variable_type = match variable.var_type {
            hir::VariableType::Real => {
                let default_value = variable
                    .default
                    .and_then(|expr| ConstantSchematicAnalysis.fold_real_expression(self, expr));
                VariableType::Real(default_value)
            }

            hir::VariableType::Integer => {
                let default_value = if let Some(default_value) = variable.default {
                    match ConstantSchematicAnalysis.fold_expression(self, default_value) {
                        Some(ExpressionId::Integer(expr)) => Some(expr),

                        Some(ExpressionId::Real(real_expr)) => {
                            Some(self.mir.integer_expressions.push(Spanned {
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
            hir::VariableType::String => {
                let default_value = variable
                    .default
                    .and_then(|expr| ConstantSchematicAnalysis.fold_string_expression(self, expr));
                VariableType::String(default_value)
            }
        };

        Variable {
            ident: variable.ident,
            variable_type,
        }
    }

    /// folds a parameter by evaluating the default value and any range bounds
    fn fold_parameter(&mut self, parameter: &hir::Parameter) -> Parameter {
        let parameter_type = match parameter.param_type {
            hir::ParameterType::String(ref included, ref excluded) => {
                let included = included
                    .iter()
                    .filter_map(|&expr| {
                        ConstantSchematicAnalysis.fold_string_expression(self, expr)
                    })
                    .collect();

                let excluded = excluded
                    .iter()
                    .filter_map(|&expr| {
                        ConstantSchematicAnalysis.fold_string_expression(self, expr)
                    })
                    .collect();

                #[allow(clippy::or_fun_call)]
                let default_value = ConstantSchematicAnalysis
                    .fold_string_expression(self, parameter.default)
                    .unwrap_or(StringExpressionId::from_usize_unchecked(0));

                ParameterType::String {
                    included,
                    excluded,
                    default_value,
                }
            }

            hir::ParameterType::Integer(ref included, ref excluded) => {
                let included = included
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
                    .fold_integer_expression(self, parameter.default)
                    .unwrap_or(IntegerExpressionId::from_usize_unchecked(0));

                ParameterType::Integer {
                    included,
                    excluded,
                    default_value,
                }
            }

            hir::ParameterType::Real(ref included, ref excluded) => {
                let included = included
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
                    .fold_real_expression(self, parameter.default)
                    .unwrap_or(RealExpressionId::from_usize_unchecked(0));
                ParameterType::Real {
                    included,
                    excluded,
                    default_value,
                }
            }
        };

        Parameter {
            ident: parameter.ident,
            parameter_type,
        }
    }

    pub fn fold_nature(&mut self, nature: &hir::Nature) -> Nature {
        let units = ConstantSchematicAnalysis
            .fold_string_expression(self, nature.units)
            .map_or(StringLiteral::DUMMY, |expr| {
                constant_eval_str_expr(&self.mir, expr, &mut NoConstResolution).unwrap()
            });

        let abstol = ConstantSchematicAnalysis
            .fold_real_expression(self, nature.abstol)
            .map(|expr| constant_eval_real_expr(&self.mir, expr, &mut NoConstResolution).unwrap())
            .unwrap_or(0.0);
        Nature {
            ident: nature.ident,
            abstol,
            units,
            access: nature.access,
            idt_nature: nature.idt_nature,
            ddt_nature: nature.ddt_nature,
        }
    }
}

/// Folds an hir to an mir by adding and checking type information
/// Returns any errors that occur
pub fn lower_hir(mut hir: Hir) -> Result<Mir, MultiDiagnostic<Error>> {
    HirToMirFold::new(&mut hir).fold()
}

/// Folds an hir to an mir by adding and checking type information
/// Returns any errors that occur
pub fn lower_hir_userfacing(hir: Hir) -> UserResult<Mir> {
    lower_hir_userfacing_with_printer(hir)
}

/// Folds an hir to an mir by adding and checking type information
/// Returns any errors that occur
pub fn lower_hir_userfacing_with_printer<P: DiagnosticSlicePrinter>(
    hir: Hir,
) -> UserResult<Mir, P> {
    lower_hir(hir).map_err(|error| error.user_facing())
}
