/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use std::ops::Range;

use crate::ast::{BranchAccess, ModuleItem, Parameter, ParameterType, Variable};
use crate::ast_lowering::ast_to_hir_fold::expression::StatementExpressionFolder;
use crate::ast_lowering::ast_to_hir_fold::{
    DeclarationHandler, ExpressionFolder, Fold, VerilogContext,
};
use crate::ast_lowering::branch_resolution::BranchResolver;
use crate::ast_lowering::error::{Error, Type};
use crate::compact_arena::{NanoArena, TinyArena};
use crate::hir::{Condition, Module, Statement};
use crate::ir::hir::{DisciplineAccess, WhileLoop};
use crate::ir::*;
use crate::ir::{NumericalParameterRangeBound, NumericalParameterRangeExclude};
use crate::ir::{Push, SafeRangeCreation};
use crate::parser::error::Unsupported;
use crate::symbol::Ident;
use crate::symbol_table::SymbolDeclaration;
use crate::{ast, Hir};

/// The last fold folds all statements in textual order
pub struct Statements<'tag, 'lt, H: DeclarationHandler<'tag>> {
    pub(super) branch_resolver: BranchResolver<'tag>,
    pub(super) state: VerilogContext,
    pub(super) base: Fold<'tag, 'lt>,
    pub(super) declaration_handler: &'lt mut H,
}
impl<'tag, 'lt, H: DeclarationHandler<'tag>> Statements<'tag, 'lt, H> {
    pub fn fold(mut self) -> Result<Box<Hir<'tag>>, Vec<Error<'tag>>> {
        unsafe {
            //This is save since we get the ptrs using borrows and drop is never called since they are copy
            TinyArena::init_from(&mut self.base.hir.variables, &self.base.ast.variables);
            NanoArena::init_from(&mut self.base.hir.modules, &self.base.ast.modules);
        }

        for module_id in SafeRangeCreation::<ModuleId<'tag>>::full_range(self.base.ast) {
            let module = &self.base.ast[module_id];
            self.state.insert(VerilogContext::constant);

            self.base
                .resolver
                .enter_scope(&module.contents.symbol_table);

            for variable in module.contents.variables {
                self.fold_variable(variable);
            }

            for parameter in module.contents.parameter_list {
                self.fold_parameter(parameter);
            }
            self.state.remove(VerilogContext::constant);

            let analog_statements = self.base.hir.empty_range_from_end();

            for module_item in module.contents.children.iter() {
                match module_item {
                    ModuleItem::AnalogStmt(statement) => {
                        self.state.insert(VerilogContext::analog);
                        self.fold_statement(*statement);
                        self.state.remove(VerilogContext::analog);
                    }
                    ModuleItem::GenerateStatement => unimplemented!("Generate Statement"),
                }
            }

            self.base.resolver.exit_scope();
            self.base.hir.write(
                module_id,
                module.map_with(|old| Module {
                    name: old.name,
                    port_list: old.port_list,
                    parameter_list: old.parameter_list,
                    analog: self.base.hir.extend_range_to_end(analog_statements),
                }),
            );
        }

        if self.base.errors.is_empty() {
            unsafe {
                /* Save since the fold completeed without errors so all parameters were initialized
                   We do this here for safety reasons to avoid dropping unitized storage in some obscure case
                   and parameters are not read from or pushed to the hir during the entire fold so the hir.parameters doesn't need to be initialized
                */
                TinyArena::init_from(&mut self.base.hir.parameters, &self.base.ast.parameters)
            }
            Ok(self.base.hir)
        } else {
            Err(self.base.errors)
        }
    }

    /// Folds a statements. StatementIds are not stable because the amount of statements may change during this fold
    /// The way that Statement Blocks are stored also changes. Instead of an Vec<StatementId> we switch to a Range of StatementIds
    /// This is possible because this fold adds Statements in the order they are executed (conditions indicate themselves and their block as a statement before&after their block)
    /// As such this function doesn't return the new StatementId instead `empty_range_from_end` and `extend_range_to_end` are used to create the range of the folded block by the calle
    fn fold_statement(&mut self, statement: StatementId<'tag>) {
        match self.base.ast[statement] {
            ast::Statement::Block(id) => {
                if let Some(scope) = &self.base.ast[id].contents.scope {
                    self.base.resolver.enter_scope(&scope.symbols);
                    self.state.insert(VerilogContext::constant);

                    for decl in scope.symbols.values().copied() {
                        match decl {
                            SymbolDeclaration::Nature(_) => unreachable_unchecked!("Natures can't be declared inside blocks so the parser won't ever place this here"),
                            SymbolDeclaration::Module(_)=>unreachable_unchecked!("Module cant be declared inside blocks so the parser won't ever place this here"),
                            SymbolDeclaration::Discipline(_) => unreachable_unchecked!("Discipline can't be declared inside blocks so the parser won't ever place this here"),
                            SymbolDeclaration::Function(_) => unreachable_unchecked!("Functions can't be declared inside blocks so the parser won't ever place this here"),
                            SymbolDeclaration::Branch(_) => unreachable_unchecked!("Functions can't be declared inside blocks so the parser won't ever place this here"),
                            SymbolDeclaration::Block(_) => (),//Blocs are visited when the appropriate statements are reached
                            SymbolDeclaration::Port(_) =>unreachable_unchecked!("Port can't be declared inside blocks so the parser won't ever place this here"),
                            SymbolDeclaration::Net(_) =>unreachable_unchecked!("Net( can't be declared inside blocks so the parser won't ever place this here"),
                            SymbolDeclaration::Variable(variableid) => {self.fold_variable(variableid);},
                            SymbolDeclaration::Parameter(parameter_id) => {self.fold_parameter(parameter_id);},
                        }
                    }

                    self.state.remove(VerilogContext::constant);
                    self.fold_block(id);
                    self.base.resolver.exit_scope();
                } else {
                    self.fold_block(id);
                }
            }

            ast::Statement::Condition(ref condition) => {
                let start = self.base.hir.push(Statement::ConditionStart {
                    condition_info_and_end: statement, /*just a place holder*/
                });
                if let Some(contents) = self.fold_condition(&condition.contents) {
                    let end = self
                        .base
                        .hir
                        .push(Statement::Condition(condition.map(contents)));
                    self.base.hir[start] = Statement::ConditionStart {
                        condition_info_and_end: end,
                    };
                }
            }

            ast::Statement::While(while_loop) => {
                let start = self.base.hir.push(Statement::WhileStart {
                    while_info_and_start: statement, //just a place holder
                });

                let condition = self.fold_expression(while_loop.contents.condition);
                let body = self.base.hir.empty_range_from_end();
                self.fold_statement(while_loop.contents.body);
                if let Some(condition) = condition {
                    let end = self
                        .base
                        .hir
                        .push(Statement::While(while_loop.copy_with(|old| WhileLoop {
                            condition,
                            body: self.base.hir.extend_range_to_end(body),
                        })));
                    self.base.hir[start] = Statement::WhileStart {
                        while_info_and_start: end,
                    }
                }
            }

            ast::Statement::Assign(ref attr, ref ident, value) => {
                resolve_hierarchical!(self.base; ident as Variable(id) => {
                    if let Some(value) = self.fold_expression(value){
                        self.base.hir.push(Statement::Assignment(*attr, id, value));
                    }
                })
            }

            ast::Statement::Contribute(attr, ref nature_name, ref branch, value) => {
                if let Some((discipline_access, branch, value)) =
                    self.fold_contribute(nature_name, branch, value)
                {
                    self.base.hir.push(Statement::Contribute(
                        attr,
                        discipline_access,
                        branch,
                        value,
                    ));
                }
            }

            ast::Statement::FunctionCall(attr, ref name, ref parameters) => {
                let parameters = parameters
                    .iter()
                    .copied()
                    .filter_map(|expr| self.fold_expression(expr))
                    .collect();

                resolve_hierarchical!(self.base; name as
                        Function(fid) => {
                            self.base.hir.push(Statement::FunctionCall(attr,fid,parameters));
                    }
                )
            }
        }
    }

    fn fold_contribute(
        &mut self,
        nature_name: &Ident,
        branch: &Node<BranchAccess>,
        value: ExpressionId<'tag>,
    ) -> Option<(DisciplineAccess, BranchId<'tag>, ExpressionId<'tag>)> {
        let (branch, discipline) = self
            .branch_resolver
            .resolve_branch_access(&mut self.base, branch)?;

        let nature = self.branch_resolver.resolve_discipline_access(
            &mut self.base,
            nature_name,
            discipline,
        )?;

        let value = self.fold_expression(value)?;
        Some((nature, branch, value))
    }

    /// folds a condition/if statement
    fn fold_condition(&mut self, condition: &ast::Condition<'tag>) -> Option<Condition<'tag>> {
        let main_condition = self.fold_expression(condition.condition);

        let main_condition_statements = self.base.hir.empty_range_from_end();

        self.fold_statement(condition.if_statement);
        let main_condition_statements =
            self.base.hir.extend_range_to_end(main_condition_statements);

        let statements = self.base.hir.empty_range_from_end();
        if let Some(statement) = condition.else_statement {
            self.fold_statement(statement);
        }
        let else_statement = self.base.hir.extend_range_to_end(statements);

        Some(Condition {
            condition: main_condition?,
            if_statements: main_condition_statements,
            else_statement,
        })
    }

    /// Just a utility method that makes folding expressions a little more ergonomic
    fn fold_expression(&mut self, expr: ExpressionId<'tag>) -> Option<ExpressionId<'tag>> {
        StatementExpressionFolder {
            state: self.state,
            branch_resolver: &mut self.branch_resolver,
        }
        .fold(expr, &mut self.base)
    }

    fn fold_block(&mut self, block: BlockId<'tag>) {
        for statement in self.base.ast[block].contents.statements.iter().copied() {
            self.fold_statement(statement);
        }
    }

    /// Folds a variable
    /// This is just folds the default value if it exists and just copys thre rest
    fn fold_variable(&mut self, variable: VariableId<'tag>) {
        let default_value = self.base.ast[variable]
            .contents
            .default_value
            .and_then(|expr| self.fold_expression(expr));

        self.base.hir.write(
            variable,
            AttributeNode {
                contents: Variable {
                    default_value,
                    ..self.base.ast[variable].contents
                },
                ..self.base.ast[variable]
            },
        );

        self.declaration_handler
            .handle_declaration(&mut self.base, SymbolDeclaration::Variable(variable))
    }

    fn fold_parameter(&mut self, parameter_id: ParameterId<'tag>) {
        let default_value = self.base.ast[parameter_id]
            .contents
            .default_value
            .and_then(|expr| self.fold_expression(expr));

        if let ParameterType::Numerical {
            parameter_type,
            ref included_ranges,
            ref excluded_ranges,
        } = self.base.ast[parameter_id].contents.parameter_type
        {
            let included_ranges = included_ranges
                .iter()
                .filter_map(|range| {
                    Some(Range {
                        start: NumericalParameterRangeBound {
                            bound: self.fold_expression(range.start.bound)?,
                            ..range.start
                        },
                        end: NumericalParameterRangeBound {
                            bound: self.fold_expression(range.end.bound)?,
                            ..range.end
                        },
                    })
                })
                .collect();

            let excluded_ranges = excluded_ranges
                .iter()
                .filter_map(|exclude| match exclude {
                    NumericalParameterRangeExclude::Value(val) => Some(
                        NumericalParameterRangeExclude::Value(self.fold_expression(*val)?),
                    ),
                    NumericalParameterRangeExclude::Range(range) => {
                        Some(NumericalParameterRangeExclude::Range(
                            NumericalParameterRangeBound {
                                bound: self.fold_expression(range.start.bound)?,
                                ..range.start
                            }..NumericalParameterRangeBound {
                                bound: self.fold_expression(range.start.bound)?,
                                ..range.end
                            },
                        ))
                    }
                })
                .collect();

            unsafe {
                //this is save since it happens unconditionally for all parameters
                self.base.hir.write_unsafe(
                    parameter_id,
                    self.base.ast[parameter_id].map_with(|old| Parameter {
                        name: old.name,
                        default_value,
                        parameter_type: ParameterType::Numerical {
                            parameter_type,
                            included_ranges,
                            excluded_ranges,
                        },
                    }),
                )
            }
            self.declaration_handler
                .handle_declaration(&mut self.base, SymbolDeclaration::Parameter(parameter_id))
        } else {
            unsafe {
                self.base
                    .hir
                    .write_unsafe(parameter_id, self.base.ast[parameter_id].clone());
            } //required for safety
            self.base.error(Error {
                error_type: Type::Unsupported(Unsupported::StringParameters),
                source: self.base.ast[parameter_id].source,
            })
        }
    }
}
