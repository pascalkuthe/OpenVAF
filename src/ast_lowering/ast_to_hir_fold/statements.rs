/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use log::*;

use crate::ast::{BranchAccess, ModuleItem, ParameterType};
use crate::ast_lowering::ast_to_hir_fold::expression::StatementExpressionFolder;
use crate::ast_lowering::ast_to_hir_fold::{
    DeclarationHandler, ExpressionFolder, Fold, VerilogContext,
};
use crate::ast_lowering::branch_resolution::BranchResolver;
use crate::ast_lowering::error::{Error, NotAllowedInFunction, Type};

use crate::hir::{Condition, Module, Statement};
use crate::ir::hir::{DisciplineAccess, Function, FunctionArg, WhileLoop};
use crate::ir::ids::IdRange;
use crate::ir::NumericalParameterRangeExclude;
use crate::ir::*;
use crate::parser::error::Unsupported;
use crate::symbol::Ident;
use crate::symbol_table::SymbolDeclaration;
use crate::{ast, Hir};
use std::mem::take;

/// The last fold folds all statements in textual order
pub struct Statements<'lt, H: DeclarationHandler> {
    pub(super) branch_resolver: BranchResolver,
    pub(super) state: VerilogContext,
    pub(super) base: Fold<'lt>,
    pub(super) declaration_handler: &'lt mut H,
}

impl<'lt, H: DeclarationHandler> Statements<'lt, H> {
    pub fn fold(mut self) -> Result<Hir, Vec<Error>> {
        for module in self.base.ast.modules.iter() {
            self.base
                .resolver
                .enter_scope(&module.contents.symbol_table);

            for decl in module.contents.symbol_table.values().copied() {
                match decl {
                    SymbolDeclaration::Nature(_)
                    | SymbolDeclaration::Module(_)
                    | SymbolDeclaration::Discipline(_) => {
                        unreachable_unchecked!("Parser cant create this")
                    }

                    SymbolDeclaration::Branch(_)
                    | SymbolDeclaration::Port(_)
                    | SymbolDeclaration::Net(_)
                    | SymbolDeclaration::Block(_) => (), //Have already been visited or will be visited later

                    SymbolDeclaration::Function(function) => self.fold_function(function),
                    SymbolDeclaration::Variable(variable) => {
                        self.state.insert(VerilogContext::CONSTANT);
                        self.fold_variable(variable);
                        self.state.remove(VerilogContext::CONSTANT);
                    }
                    SymbolDeclaration::Parameter(parameter) => {
                        self.state.insert(VerilogContext::CONSTANT);
                        self.fold_parameter(parameter);
                        self.state.remove(VerilogContext::CONSTANT);
                    }
                }
            }

            let analog_stmts_start = self.base.hir.statements.len_idx();

            for module_item in module.contents.children.iter() {
                match module_item {
                    ModuleItem::AnalogStmt(statement) => {
                        self.fold_statement(*statement);
                    }
                    ModuleItem::GenerateStatement => unimplemented!("Generate Statement"),
                }
            }

            self.base.resolver.exit_scope();
            self.base.hir.modules.push(module.map_with(|old| Module {
                name: old.name,
                port_list: old.port_list.clone(),
                analog: IdRange(analog_stmts_start..self.base.hir.statements.len_idx()),
            }));
        }

        if self.base.errors.is_empty() {
            Ok(self.base.hir)
        } else {
            Err(self.base.errors)
        }
    }

    pub fn fold_function(&mut self, id: FunctionId) {
        let function = &self.base.ast[id].contents;
        let args = function
            .args
            .iter()
            .filter_map(|arg| {
                if let Some(SymbolDeclaration::Variable(local_var)) =
                    function.declarations.get(&arg.name.name).copied()
                {
                    Some(FunctionArg {
                        local_var,
                        input: arg.input,
                        output: arg.output,
                    })
                } else {
                    self.base.errors.push(Error {
                        error_type: Type::TypeDeclarationMissing(arg.name.name),
                        source: arg.name.span,
                    });
                    None
                }
            })
            .collect();

        self.state.insert(VerilogContext::FUNCTION);
        self.base.resolver.enter_function(&function.declarations);

        for declaration in function.declarations.values().copied() {
            match declaration {
                SymbolDeclaration::Block(_) => { /*error will be generated upon encounter*/ }
                SymbolDeclaration::Variable(var) => self.fold_variable(var),
                SymbolDeclaration::Parameter(param) => self.fold_parameter(param),
                SymbolDeclaration::Module(_)
                | SymbolDeclaration::Branch(_)
                | SymbolDeclaration::Net(_)
                | SymbolDeclaration::Port(_)
                | SymbolDeclaration::Function(_)
                | SymbolDeclaration::Discipline(_)
                | SymbolDeclaration::Nature(_) => {
                    unreachable_unchecked!("Parser doesn't allow this")
                }
            }
        }

        self.fold_variable(function.return_variable);

        let start = self.base.hir.statements.len_idx();
        self.fold_statement(function.body);
        self.base.hir[id] = self.base.ast[id].map(Function {
            name: function.name,
            args,
            return_variable: function.return_variable,
            body: IdRange(start..self.base.hir.statements.len_idx()),
        });
        self.base.resolver.exit_function();
        self.state.remove(VerilogContext::FUNCTION);
    }

    /// Folds a statements. StatementIds are not stable because the amount of statements may change during this fold
    /// The way that Statement Blocks are stored also changes. Instead of an Vec<StatementId> we switch to a Range of StatementIds
    /// This is possible because this fold adds Statements in the order they are executed (conditions indicate themselves and their block as a statement before&after their block)
    /// As such this function doesn't return the new StatementId instead `empty_range_from_end` and `extend_range_to_end` are used to create the range of the folded block by the calle
    fn fold_statement(&mut self, id: StatementId) {
        match self.base.ast[id] {
            ast::Statement::Block(id) => {
                if let Some(scope) = &self.base.ast[id].contents.scope {
                    if self.state.contains(VerilogContext::FUNCTION) {
                        self.base.error(Error {
                            source: scope.name.span,
                            error_type: Type::NotAllowedInFunction(
                                NotAllowedInFunction::NamedBlocks,
                            ),
                        });
                    }

                    self.base.resolver.enter_scope(&scope.symbols);
                    self.state.insert(VerilogContext::CONSTANT);

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

                    self.state.remove(VerilogContext::CONSTANT);
                    self.fold_block(id);
                    self.base.resolver.exit_scope();
                } else {
                    self.fold_block(id);
                }
            }

            ast::Statement::Condition(ref condition) => {
                let start = self.base.hir.statements.push(Statement::ConditionStart {
                    condition_info_and_end: id, /*just a place holder*/
                });
                if let Some(contents) = self.fold_condition(&condition.contents) {
                    let end = self
                        .base
                        .hir
                        .statements
                        .push(Statement::Condition(condition.map(contents)));
                    self.base.hir[start] = Statement::ConditionStart {
                        condition_info_and_end: end,
                    };
                }
            }

            ast::Statement::While(while_loop) => {
                let start = self.base.hir.statements.push(Statement::WhileStart {
                    while_info_and_start: id, //just a place holder
                });

                let condition = self.fold_expression(while_loop.contents.condition);
                let body_start = self.base.hir.statements.len_idx();
                self.fold_statement(while_loop.contents.body);
                if let Some(condition) = condition {
                    let end = self
                        .base
                        .hir
                        .statements
                        .push(Statement::While(while_loop.copy_as(WhileLoop {
                            condition,
                            body: IdRange(body_start..self.base.hir.statements.len_idx()),
                        })));
                    self.base.hir[start] = Statement::WhileStart {
                        while_info_and_start: end,
                    }
                }
            }

            ast::Statement::Assign(ref attr, ref ident, value) => {
                resolve_hierarchical!(self.base; ident as Variable(id) => {
                    if let Some(value) = self.fold_expression(value){
                        self.base.hir.statements.push(Statement::Assignment(*attr, id, value));
                    }
                })
            }

            ast::Statement::Contribute(attr, ref nature_name, ref branch, value) => {
                if self.state.contains(VerilogContext::FUNCTION) {
                    self.base.error(Error {
                        source: nature_name.span.extend(self.base.ast[value].source),
                        error_type: Type::NotAllowedInFunction(NotAllowedInFunction::Contribute),
                    });
                }

                if let Some((discipline_access, branch, value)) =
                    self.fold_contribute(nature_name, branch, value)
                {
                    self.base.hir.statements.push(Statement::Contribute(
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
                            self.base.hir.statements.push(Statement::FunctionCall(attr,fid,parameters));
                    }
                )
            }
            ast::Statement::DisplayTask(_, _) => {
                // TODO DisplayTask
                warn!("All display task such as $display are currently ignored. Display tasks will be implemented in the future");
            }
        }
    }

    fn fold_contribute(
        &mut self,
        nature_name: &Ident,
        branch: &Node<BranchAccess>,
        value: ExpressionId,
    ) -> Option<(DisciplineAccess, BranchId, ExpressionId)> {
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
    fn fold_condition(&mut self, node: &ast::Condition) -> Option<Condition> {
        let condition = self.fold_expression(node.condition);

        let if_body_start = self.base.hir.statements.len_idx();

        self.fold_statement(node.if_statement);
        let main_condition_statements = IdRange(if_body_start..self.base.hir.statements.len_idx());

        let else_statements_start = self.base.hir.statements.len_idx();
        if let Some(statement) = node.else_statement {
            self.fold_statement(statement);
        }

        Some(Condition {
            condition: condition?,
            if_statements: main_condition_statements,
            else_statements: IdRange(else_statements_start..self.base.hir.statements.len_idx()),
        })
    }

    /// Just a utility method that makes folding expressions a little more ergonomic
    fn fold_expression(&mut self, expr: ExpressionId) -> Option<ExpressionId> {
        StatementExpressionFolder {
            state: self.state,
            branch_resolver: &mut self.branch_resolver,
        }
        .fold(expr, &mut self.base)
    }

    fn fold_block(&mut self, block: BlockId) {
        for statement in self.base.ast[block].contents.statements.iter().copied() {
            self.fold_statement(statement);
        }
    }

    /// Folds a variable
    /// This is just folds the default value if it exists and just copys thre rest
    fn fold_variable(&mut self, id: VariableId) {
        self.base.hir[id].contents.default_value = self.base.hir[id]
            .contents
            .default_value
            .and_then(|expr| self.fold_expression(expr));

        self.declaration_handler
            .handle_declaration(&mut self.base, SymbolDeclaration::Variable(id))
    }

    fn fold_parameter(&mut self, id: ParameterId) {
        if let Some(expr) = self.fold_expression(self.base.hir[id].contents.default_value) {
            self.base.hir[id].contents.default_value = expr
        }

        if let ParameterType::Numerical {
            parameter_type,
            ref mut from_ranges,
            ref mut excluded,
        } = self.base.hir[id].contents.parameter_type
        {
            let mut from_ranges = take(from_ranges);
            let mut excluded = take(excluded);

            for range in from_ranges.iter_mut() {
                if let Some(expr) = self.fold_expression(range.start.bound) {
                    range.start.bound = expr;
                }
                if let Some(expr) = self.fold_expression(range.end.bound) {
                    range.end.bound = expr;
                }
            }

            for exclude in excluded.iter_mut() {
                match exclude {
                    NumericalParameterRangeExclude::Value(val) => {
                        if let Some(expr) = self.fold_expression(*val) {
                            *val = expr;
                        }
                    }

                    NumericalParameterRangeExclude::Range(range) => {
                        if let Some(expr) = self.fold_expression(range.start.bound) {
                            range.start.bound = expr;
                        }
                        if let Some(expr) = self.fold_expression(range.end.bound) {
                            range.end.bound = expr;
                        }
                    }
                }
            }

            self.base.hir[id].contents.parameter_type = ParameterType::Numerical {
                parameter_type,
                excluded,
                from_ranges,
            }
        } else {
            self.base.error(Error {
                error_type: Type::Unsupported(Unsupported::StringParameters),
                source: self.base.ast[id].source,
            })
        }

        self.declaration_handler
            .handle_declaration(&mut self.base, SymbolDeclaration::Parameter(id))
    }
}
