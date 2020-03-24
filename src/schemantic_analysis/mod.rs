use std::convert::TryInto;

use crate::ast::{AttributeNode, BuiltInFunctionCall, Node};
use crate::compact_arena::SafeRange;
use crate::hir;
use crate::hir::{Expression, Hir, Primary};
use crate::ir::ast::{BinaryOperator, VariableType};
use crate::ir::mir::NumericalParameterRangeExclude::Range;
use crate::ir::mir::{ExpressionId, Mir, Parameter, ParameterType};
use crate::ir::{IntegerExpressionId, ParameterId, RealExpressionId, StatementId};
use crate::mir::*;
use crate::schemantic_analysis::error::{Error, Type};
use crate::util::{Push, SafeRangeCreation};
use crate::{ast, ir};

pub mod error;
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
    pub fn fold(mut self) -> (Vec<Error<'tag>>, Box<Mir<'tag>>) {
        let parameters: SafeRange<ParameterId<'tag>> = self.hir.full_range();
        for parameter in parameters {
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
                            continue;
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
                            continue;
                        }
                    }
                },
            };
            self.mir[parameter] = AttributeNode {
                contents: Parameter {
                    name: self.hir[parameter].contents.name,
                    parameter_type,
                },
                source: self.hir[parameter].source,
                attributes: self.hir[parameter].attributes,
            }
        }
        let statements: SafeRange<StatementId<'tag>> = self.hir.full_range();
        for statement in statements {
            mself.mir[statement] = match self.hir[statement] {
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
                        VariableType::REALTIME | VariableType::TIME
                    ) =>
                {
                    if let Ok(expr) = self.fold_expression(expr) {
                        Statement::Assignment(attr, variable, expr)
                    } else {
                        continue;
                    }
                }
            }
        }
        (self.errors, self.mir)
    }
}
mod constant_eval;
mod expression_schemantic;
