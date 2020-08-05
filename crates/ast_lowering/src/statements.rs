/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast;
use crate::ast::{NumericParameterConstraint, PortList};
use crate::branches::resolver::{BranchProbeKind, BranchResolver, NatureAccess};
use crate::error::Error::{
    ContributeToBranchPortProbe, ContributeToNonBranchProbe, FunctionArgTypeDeclarationMissing,
    MultipleDefaultDeclarations, PortNotPreDeclaredInModuleHead, PortPreDeclaredNotDefined,
    PortRedeclaration,
};
use crate::error::{Error, NotAllowedInFunction};
use crate::expression::{AllowedReferences, ConstantExpressionFolder, StatementExpressionFolder};
use crate::lints::IgnoredDisplayTask;
use crate::{ExpressionFolder, Fold, VerilogContext};
use openvaf_ast::symbol_table::SymbolDeclaration;
use openvaf_ast::StringParameterConstraint;
use openvaf_diagnostics::lints::Linter;
use openvaf_diagnostics::MultiDiagnostic;
use openvaf_hir::{Block, CaseItem, Cases, ForLoop, Parameter};
use openvaf_hir::{DisciplineAccess, Function, FunctionArg};
use openvaf_hir::{Hir, ParameterType};
use openvaf_hir::{Module, Statement};
use openvaf_ir::ids::IdRange;
use openvaf_ir::ids::{
    BlockId, BranchId, ExpressionId, FunctionId, ParameterId, StatementId, VariableId,
};
use openvaf_ir::{Node, ParameterExcludeConstraint, Spanned};
use openvaf_ir::{ParameterRangeConstraint, ParameterRangeConstraintBound};
use std::ops::Range;

/// The last fold folds all statements in textual order
pub struct Statements<'lt> {
    pub(super) branch_resolver: BranchResolver,
    pub(super) state: VerilogContext,
    pub(super) base: Fold<'lt>,
}

impl<'lt> Statements<'lt> {
    pub fn fold(mut self) -> Result<Hir, MultiDiagnostic<Error>> {
        for module in &self.base.ast.modules {
            self.base.fold_attributes(module.attributes);

            self.base
                .resolver
                .enter_scope(&module.contents.symbol_table);

            let mut ports = module.contents.ports.contents.clone();

            for decl in module.contents.symbol_table.values().copied() {
                match decl {
                    SymbolDeclaration::Nature(_)
                    | SymbolDeclaration::Module(_)
                    | SymbolDeclaration::Discipline(_) => unreachable!("Parser cant create this"),

                    SymbolDeclaration::Branch(id) => {
                        self.base.fold_attributes(self.base.hir[id].attributes)
                    }
                    SymbolDeclaration::Net(id) => {
                        self.base.fold_attributes(self.base.hir[id].attributes)
                    }
                    SymbolDeclaration::PortBranch(id) => {
                        self.base.fold_attributes(self.base.ast[id].attributes)
                    }

                    SymbolDeclaration::Block(_) => (), // Will visited or will be visited later

                    SymbolDeclaration::Port(id) => {
                        let attributes = self.base.ast[self.base.hir[id].net].attributes;
                        self.base.fold_attributes(attributes);
                        // LLVM should pull the match out of the loop
                        match ports {
                            PortList::Expected(ref mut idents) => {
                                let ident = self.base.hir[self.base.hir[id].net].contents.ident;
                                if let Some(i) = idents.iter().position(|&e| e == ident) {
                                    idents.remove(i);
                                } else {
                                    self.base.error(PortNotPreDeclaredInModuleHead {
                                        port_list: module.contents.ports.span,
                                        port: ident,
                                    })
                                }
                            }
                            PortList::Declarations(declarations)
                                if module.contents.body_ports.contains(declarations) =>
                            {
                                let span = self.base.hir[self.base.hir[id].net].span;
                                self.base.error(PortRedeclaration {
                                    module_head: module.contents.ports.span,
                                    body_declaration: span,
                                })
                            }

                            PortList::Declarations(_) => (),
                        }
                    }

                    SymbolDeclaration::Function(function) => self.fold_function(function),

                    SymbolDeclaration::Parameter(param) => {
                        self.state.insert(VerilogContext::CONSTANT);
                        self.fold_parameter(param);
                        self.state.remove(VerilogContext::CONSTANT);
                    }
                    SymbolDeclaration::Variable(variable) => {
                        self.state.insert(VerilogContext::CONSTANT);
                        self.fold_variable(variable);
                        self.state.remove(VerilogContext::CONSTANT);
                    }
                }
            }

            let ports = match ports {
                PortList::Expected(undeclared_ports) => {
                    for port in undeclared_ports {
                        self.base.error(PortPreDeclaredNotDefined(port))
                    }
                    module.contents.body_ports.clone()
                }
                PortList::Declarations(start) => IdRange(start..module.contents.body_ports.0.end),
            };

            let analog_stmts_start = self.base.hir.statements.len_idx();

            for &stmt in &module.contents.analog_stmts {
                self.fold_statement(stmt);
            }

            self.base.resolver.exit_scope();
            self.base.hir.modules.push(module.map_with(|old| Module {
                ident: old.ident,
                ports,
                parameters: old.parameters.clone(),
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
        self.base.fold_attributes(self.base.ast[id].attributes);

        let function = &self.base.ast[id].contents;
        let args = function
            .args
            .iter()
            .filter_map(|arg| {
                if let Some(SymbolDeclaration::Variable(local_var)) =
                    function.declarations.get(&arg.ident.name).copied()
                {
                    Some(FunctionArg {
                        local_var,
                        input: arg.input,
                        output: arg.output,
                    })
                } else {
                    self.base
                        .error(FunctionArgTypeDeclarationMissing(arg.ident));
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
                | SymbolDeclaration::PortBranch(_)
                | SymbolDeclaration::Net(_)
                | SymbolDeclaration::Port(_)
                | SymbolDeclaration::Function(_)
                | SymbolDeclaration::Discipline(_)
                | SymbolDeclaration::Nature(_) => unreachable!("Parser doesn't allow this"),
            }
        }

        self.fold_variable(function.return_variable);

        let start = self.base.hir.statements.len_idx();
        self.fold_statement(function.body);
        self.base.hir[id] = self.base.ast[id].map(Function {
            ident: function.ident,
            args,
            return_variable: function.return_variable,
            body: IdRange(start..self.base.hir.statements.len_idx()),
        });
        self.base.resolver.exit_function();
        self.state.remove(VerilogContext::FUNCTION);
    }

    /// Folds a statement. `StatementIds` are not stable because the amount of statements may change during this fold
    /// The way that Statement Blocks are stored also changes. Instead of an Vec<StatementId> we switch to a Range of `StatementId`s
    /// This is possible because this fold adds Statements in the order they are executed (conditions indicate themselves and their block as a statement before&after their block)
    /// As such this function doesn't return the new StatementId instead `empty_range_from_end` and `extend_range_to_end` are used to create the range of the folded block by the calle
    fn fold_statement(&mut self, id: StatementId) {
        let attributes = self.base.ast[id].attributes;
        self.base.fold_attributes(attributes);
        let span = self.base.ast[id].span;

        match self.base.ast[id].contents {
            ast::Statement::Block(id) => {
                if let Some(scope) = &self.base.ast[id].scope {
                    if self.state.contains(VerilogContext::FUNCTION) {
                        self.base.error(Error::NotAllowedInFunction(
                            NotAllowedInFunction::NamedBlocks,
                            scope.ident.span,
                        ));
                    }

                    self.base.resolver.enter_scope(&scope.symbols);
                    self.state.insert(VerilogContext::CONSTANT);

                    for decl in scope.symbols.values().copied() {
                        match decl {
                            SymbolDeclaration::Nature(_) => unreachable!("Natures can't be declared inside blocks so the parser won't ever place this here"),
                            SymbolDeclaration::Module(_)=>unreachable!("Module cant be declared inside blocks so the parser won't ever place this here"),
                            SymbolDeclaration::Discipline(_) => unreachable!("Discipline can't be declared inside blocks so the parser won't ever place this here"),
                            SymbolDeclaration::Function(_) => unreachable!("Functions can't be declared inside blocks so the parser won't ever place this here"),
                            SymbolDeclaration::Branch(_) => unreachable!("Functions can't be declared inside blocks so the parser won't ever place this here"),
                            SymbolDeclaration::PortBranch(_) => unreachable!("Functions can't be declared inside blocks so the parser won't ever place this here"),
                            SymbolDeclaration::Block(_) => (),//Blocs are visited when the appropriate statements are reached
                            SymbolDeclaration::Port(_) =>unreachable!("Port can't be declared inside blocks so the parser won't ever place this here"),
                            SymbolDeclaration::Net(_) =>unreachable!("Net( can't be declared inside blocks so the parser won't ever place this here"),
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

            ast::Statement::Condition(cond, true_block, false_block) => {
                if let Some((cond, true_block, false_block)) =
                    self.fold_condition(cond, true_block, false_block)
                {
                    self.base.hir.statements.push(Node {
                        attributes,
                        span,
                        contents: Statement::Condition(cond, true_block, false_block),
                    });
                }
            }

            ast::Statement::While(cond, body) => {
                let condition = self.fold_expression(cond);
                let body_start = self.base.hir.statements.len_idx();
                self.fold_statement(body);
                if let Some(condition) = condition {
                    self.base.hir.statements.push(Node {
                        attributes,
                        span,
                        contents: Statement::While(
                            condition,
                            IdRange(body_start..self.base.hir.statements.len_idx()),
                        ),
                    });
                }
            }

            ast::Statement::For(ref for_loop) => {
                let ast::ForLoop {
                    cond,
                    init: (ref init_var, init_expr),
                    incr: (ref incr_var, incr_expr),
                    body,
                } = *for_loop;

                let init_var = resolve_hierarchical!(self.base; init_var as  Variable(id) => id);

                let incr_var = resolve_hierarchical!(self.base; incr_var as  Variable(id) => id);

                let init_expr = self.fold_expression(init_expr);
                let incr_expr = self.fold_expression(incr_expr);
                let cond = self.fold_expression(cond);

                let body_start = self.base.hir.statements.len_idx();
                self.fold_statement(body);
                if let (
                    Some(cond),
                    Some(init_var),
                    Some(init_expr),
                    Some(incr_var),
                    Some(incr_expr),
                ) = (cond, init_var, init_expr, incr_var, incr_expr)
                {
                    self.base.hir.statements.push(Node {
                        attributes,
                        span,
                        contents: Statement::For(ForLoop {
                            cond,
                            body: IdRange(body_start..self.base.hir.statements.len_idx()),
                            init: (init_var, init_expr),
                            incr: (incr_var, incr_expr),
                        }),
                    });
                }
            }

            ast::Statement::Assignment(ref ident, value) => {
                let value = self.fold_expression(value);
                resolve_hierarchical!(self.base; ident as Variable(id) => {
                    if let Some(value) = value{
                        self.base.hir.statements.push(Node{attributes,span,contents: Statement::Assignment(id, value)});
                    }
                });
            }

            ast::Statement::Contribute(access, value) => {
                if self.state.contains(VerilogContext::FUNCTION) {
                    self.base.error(Error::NotAllowedInFunction(
                        NotAllowedInFunction::Contribute,
                        self.base.hir[access].span.extend(self.base.ast[value].span),
                    ));
                }

                if let Some((discipline_access, branch, value)) =
                    self.fold_contribute(access, value)
                {
                    self.base.hir.statements.push(Node {
                        attributes,
                        span,
                        contents: Statement::Contribute(discipline_access, branch, value),
                    });
                }
            }

            ast::Statement::DisplayTask(_task_kind, ref _args) => {
                Linter::dispatch_early(Box::new(IgnoredDisplayTask { span }))
            }

            ast::Statement::StopTask(kind, finish_number) => {
                let finish_number = finish_number.and_then(|expr| {
                    ConstantExpressionFolder(AllowedReferences::None).fold(expr, &mut self.base)
                });

                self.base.hir.statements.push(Node {
                    attributes,
                    span,
                    contents: Statement::StopTask(kind, finish_number),
                });
            }

            ast::Statement::Case(cond, ref cases) => {
                let expr = self.fold_expression(cond);
                let mut default: Option<Spanned<Block>> = None;
                let cases = cases
                    .iter()
                    .filter_map(|case_node| {
                        let case = &case_node.contents;
                        let is_default = case.values.is_empty();

                        let values = case
                            .values
                            .iter()
                            .filter_map(|&value| self.fold_expression(value))
                            .collect();

                        let start = self.base.hir.statements.len_idx();
                        self.fold_statement(case.stmt?);
                        let body = IdRange(start..self.base.hir.statements.len_idx());

                        if !is_default {
                            Some(CaseItem { values, body })
                        } else if let Some(old) = &default {
                            self.base.error(MultipleDefaultDeclarations {
                                new: case_node.span,
                                old: old.span,
                            });
                            None
                        } else {
                            default = Some(Spanned::new(body, case_node.span));
                            None
                        }
                    })
                    .collect();

                let end = self.base.hir.statements.len_idx();
                let default = default.map_or(IdRange(end..end), |default| default.contents);

                if let Some(expr) = expr {
                    self.base.hir.statements.push(Node {
                        attributes,
                        span,
                        contents: Statement::Case(Cases {
                            expr,
                            cases,
                            default,
                        }),
                    });
                }
            }

            ast::Statement::Error => unreachable!(),
        }
    }

    fn fold_contribute(
        &mut self,
        access_expr: ExpressionId,
        value: ExpressionId,
    ) -> Option<(DisciplineAccess, BranchId, ExpressionId)> {
        let value = self.fold_expression(value);
        match self.base.ast[access_expr].contents {
            ast::Expression::Primary(ast::Primary::FunctionCall(ident, ref args)) => {
                let access = NatureAccess::resolve_from_ident(ident, &mut self.base)?;
                let kind =
                    self.branch_resolver
                        .resolve_branch_probe_call(access, args, &mut self.base)?;
                match kind {
                    BranchProbeKind::Port(_) => {
                        self.base
                            .error(ContributeToBranchPortProbe(self.base.ast[access_expr].span));
                        None
                    }
                    BranchProbeKind::Branch(access, branch) => Some((access, branch, value?)),
                }
            }

            ast::Expression::Primary(ast::Primary::PortFlowProbe(_ident, ref _port)) => {
                self.base
                    .error(ContributeToBranchPortProbe(self.base.ast[access_expr].span));
                None
            }
            _ => {
                self.base
                    .error(ContributeToNonBranchProbe(self.base.ast[access_expr].span));
                None
            }
        }
    }

    /// folds a condition/if statement
    fn fold_condition(
        &mut self,
        cond: ExpressionId,
        true_block: StatementId,
        false_block: Option<StatementId>,
    ) -> Option<(ExpressionId, Block, Block)> {
        let condition = self.fold_expression(cond);

        let true_block_start = self.base.hir.statements.len_idx();

        self.fold_statement(true_block);
        let true_block = IdRange(true_block_start..self.base.hir.statements.len_idx());

        let false_block_start = self.base.hir.statements.len_idx();
        if let Some(statement) = false_block {
            self.fold_statement(statement);
        }

        Some((
            condition?,
            true_block,
            IdRange(false_block_start..self.base.hir.statements.len_idx()),
        ))
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
        for statement in self.base.ast[block].statements.iter().copied() {
            self.fold_statement(statement);
        }
    }

    /// Folds a variable
    /// This is just folds the default value if it exists and just copys thre rest
    fn fold_variable(&mut self, id: VariableId) {
        self.base.hir[id].contents.default = self.base.hir[id]
            .contents
            .default
            .and_then(|expr| self.fold_expression(expr));

        self.base.fold_attributes(self.base.hir[id].attributes)
    }

    #[allow(clippy::or_fun_call)]
    fn fold_parameter(&mut self, id: ParameterId) {
        let default = self.fold_expression(self.base.ast[id].contents.default);

        let param_type = match self.base.ast[id].contents.param_type {
            ast::ParameterType::Real(ref constraints) => {
                let (from_constraints, exlude_constraints) =
                    self.fold_numeric_parameter_constraints(constraints);
                ParameterType::Real(from_constraints, exlude_constraints)
            }

            ast::ParameterType::Integer(ref constraints) => {
                let (from_constraints, exlude_constraints) =
                    self.fold_numeric_parameter_constraints(constraints);
                ParameterType::Integer(from_constraints, exlude_constraints)
            }
            ast::ParameterType::String(ref constraints) => {
                let (from_constraints, exlucde_constraitns) =
                    self.fold_string_parameter_constraints(constraints);
                ParameterType::String(from_constraints, exlucde_constraitns)
            }
        };

        let attributes = self.base.ast[id].attributes;
        self.base.fold_attributes(attributes);

        let span = self.base.hir[id].span;
        self.base.hir[id] = Node {
            span,
            attributes,
            contents: Parameter {
                ident: self.base.ast[id].contents.ident,
                param_type,
                // dummy default value in case of error
                default: default.unwrap_or(ExpressionId::from_raw_unchecked(u32::MAX)),
            },
        };
    }

    fn fold_numeric_parameter_constraints(
        &mut self,
        valid_values: &[NumericParameterConstraint],
    ) -> (
        Vec<ParameterRangeConstraint<ExpressionId>>,
        Vec<ParameterExcludeConstraint<ExpressionId>>,
    ) {
        let mut from = Vec::with_capacity(valid_values.len());
        let mut excluded = Vec::with_capacity(valid_values.len());

        for contraints in valid_values {
            match contraints {
                NumericParameterConstraint::From(range) => {
                    if let Some(range) = self.fold_numeric_parameter_range_constraint_bound(range) {
                        from.push(range)
                    }
                }

                NumericParameterConstraint::Exclude(ParameterExcludeConstraint::Value(val)) => {
                    if let Some(val) = self.fold_expression(*val) {
                        excluded.push(ParameterExcludeConstraint::Value(val))
                    }
                }
                NumericParameterConstraint::Exclude(ParameterExcludeConstraint::Range(range)) => {
                    if let Some(range) = self.fold_numeric_parameter_range_constraint_bound(range) {
                        excluded.push(ParameterExcludeConstraint::Range(range))
                    }
                }
            }
        }
        (from, excluded)
    }

    fn fold_numeric_parameter_range_constraint_bound(
        &mut self,
        range: &ParameterRangeConstraint<ExpressionId>,
    ) -> Option<Range<ParameterRangeConstraintBound<ExpressionId>>> {
        let start = self.fold_expression(range.start.bound);
        let end = self.fold_expression(range.end.bound);
        let start = ParameterRangeConstraintBound {
            bound: start?,
            inclusive: range.start.inclusive,
        };

        let end = ParameterRangeConstraintBound {
            bound: end?,
            inclusive: range.end.inclusive,
        };

        Some(start..end)
    }

    fn fold_string_parameter_constraints(
        &mut self,
        valid_values: &[StringParameterConstraint],
    ) -> (Vec<ExpressionId>, Vec<ExpressionId>) {
        let mut included = Vec::with_capacity(valid_values.len());
        let mut excluded = Vec::with_capacity(valid_values.len());

        for contraints in valid_values {
            match contraints {
                StringParameterConstraint::From(values) => {
                    self.fold_string_parameter_constraint(values, &mut included)
                }

                StringParameterConstraint::Exclude(values) => {
                    self.fold_string_parameter_constraint(values, &mut excluded)
                }
            }
        }
        (included, excluded)
    }

    fn fold_string_parameter_constraint(
        &mut self,
        values: &[ExpressionId],
        dst: &mut Vec<ExpressionId>,
    ) {
        for expr in values {
            if let Some(expr) = self.fold_expression(*expr) {
                dst.push(expr)
            }
        }
    }
}
