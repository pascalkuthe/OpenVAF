/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::allowed_operations::VerilogAState;
use crate::ast;
use crate::ast::{OrderedParameterConstraint, PortList};
use crate::branches::resolver::{BranchResolver, NatureAccess};
use crate::error::Error::{
    ContributeToBranchPortProbe, ContributeToNonBranchProbe, FunctionArgTypeDeclarationMissing,
    MultipleDefaultDeclarations, PortNotPreDeclaredInModuleHead, PortPreDeclaredNotDefined,
    PortRedeclaration,
};
use crate::error::{Error, MockSymbolDeclaration};
use crate::expression::{ExpressionFolder, FunctionFoldResult};
use crate::Fold;
use ast::symbol_table::SymbolDeclaration;
use ast::UnorderedParameterConstraint;
use diagnostics::lints::Linter;
use diagnostics::MultiDiagnostic;
use hir::lowering::lints::UselessFunctionCall;
use hir::{
    AllowedOperation, AllowedOperations, Block, BranchKind, CaseItem, Cases, Expression, ForLoop,
    Parameter, Variable,
};
use hir::{FunctionArg, UserFunction};
use hir::{Hir, ParameterConstraint};
use hir::{Module, Statement};
use ir::ids::IdRange;
use ir::ids::{
    BlockId, BranchId, ExpressionId, FunctionId, ParameterId, StatementId, VariableId,
};
use ir::{
    Attributes, ConstVal, DisciplineAccess, ParameterExcludeConstraint, SimpleConstVal, Spanned,
};
use ir::{ParameterRangeConstraint, ParameterRangeConstraintBound};
use session::sourcemap::Span;
use session::symbols::Symbol;
use std::ops::Range;
use tracing::trace_span;

/// The last fold folds all statements in textual order
pub struct Statements<'lt, F: Fn(Symbol) -> AllowedOperations> {
    pub(super) branch_resolver: BranchResolver,
    pub(super) state: VerilogAState,
    pub(super) base: Fold<'lt, F>,
}

impl<'lt, F: Fn(Symbol) -> AllowedOperations> Statements<'lt, F> {
    pub fn fold(mut self) -> Result<Hir, MultiDiagnostic<Error>> {
        let span = trace_span!("statements fold");
        let _enter = span.enter();
        for module in &self.base.ast.modules {
            self.base.enter_sctxt(module.span, module.attributes);
            self.base.check_ident(module.contents.ident, MockSymbolDeclaration::Module);

            self.base.resolver.enter_scope(&module.contents.symbol_table);

            let mut ports = module.contents.ports.contents.clone();

            for decl in module.contents.symbol_table.values().copied() {
                match decl {
                    SymbolDeclaration::Nature(_)
                    | SymbolDeclaration::Module(_)
                    | SymbolDeclaration::Discipline(_) => unreachable!("Parser cant create this"),

                    SymbolDeclaration::Branch(id) => {
                        let sctx = self.base.hir[id].sctx;
                        self.base.hir[sctx].parent = Some(self.base.sctx);
                        self.base.fold_attributes(self.base.hir[sctx].attributes)
                    }

                    SymbolDeclaration::Net(id) => {
                        let sctx = self.base.hir[self.base.resolver.net_map[id]].sctx;
                        self.base.hir[sctx].parent = Some(self.base.sctx);
                        self.base.fold_attributes(self.base.hir[sctx].attributes)
                    }
                    SymbolDeclaration::PortBranch(id) => {
                        // TODO warn that these are ignored
                        self.base.fold_attributes(self.base.ast[id].attributes)
                    }

                    SymbolDeclaration::Block(_) => (), // Will visited or will be visited later

                    SymbolDeclaration::Port(id) => {
                        let sctx = self.base.hir[self.base.hir[id].node].sctx;
                        self.base.hir[sctx].parent = Some(self.base.sctx);
                        self.base.fold_attributes(self.base.hir[sctx].attributes);

                        // LLVM should pull the match out of the loop
                        match ports {
                            PortList::Expected(ref mut idents) => {
                                let ident = self.base.hir[self.base.hir[id].node].ident;
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
                                let span =
                                    self.base.hir[self.base.hir[self.base.hir[id].node].sctx].span;
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
                        let span = trace_span!(
                            "parameter",
                            id = param.index(),
                            name = display(self.base.hir[param].ident)
                        );
                        let enter = span.enter();
                        self.fold_parameter(param);
                        drop(enter);
                    }

                    SymbolDeclaration::Variable(variable) => {
                        let span = trace_span!(
                            "variable",
                            id = variable.index(),
                            name = display(self.base.hir[variable].ident)
                        );
                        let enter = span.enter();
                        self.fold_variable(variable);
                        drop(enter);
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

            let mut analog = Block::with_capacity(module.contents.analog_stmts.len());

            for &stmt in &module.contents.analog_stmts {
                self.fold_statement(stmt, &mut analog);
            }

            self.base.resolver.exit_scope();
            let sctx = self.base.exit_sctxt();
            self.base.hir.modules.push(Module {
                ident: module.contents.ident,
                ports,
                parameters: module.contents.parameters.clone(),
                analog,
                sctx,
            });
        }

        if self.base.errors.is_empty() {
            Ok(self.base.hir)
        } else {
            Err(self.base.errors)
        }
    }

    pub fn fold_function(&mut self, id: FunctionId) {
        self.base.enter_sctxt(self.base.ast[id].span, self.base.ast[id].attributes);

        let function = &self.base.ast[id].contents;

        self.base.check_ident(function.ident, MockSymbolDeclaration::Function);

        let args = function
            .args
            .iter()
            .filter_map(|arg| {
                if let Some(SymbolDeclaration::Variable(local_var)) =
                    function.declarations.get(&arg.ident.name).copied()
                {
                    Some(FunctionArg { local_var, input: arg.input, output: arg.output })
                } else {
                    self.base.error(FunctionArgTypeDeclarationMissing(arg.ident));
                    None
                }
            })
            .collect();

        let old_state = self.state.enter_function_decl();
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

        let mut body = Block::with_capacity(128);
        self.fold_statement(function.body, &mut body);
        self.base.hir[id] = UserFunction {
            ident: function.ident,
            args,
            return_variable: function.return_variable,
            body,
            sctx: self.base.exit_sctxt(),
        };
        self.base.resolver.exit_function();
        self.state = old_state;
    }

    /// Folds a statement. `StatementIds` are not stable because the amount of statements may change during this fold
    /// The way that Statement Blocks are stored also changes. Instead of an Vec<StatementId> we switch to a Range of `StatementId`s
    /// This is possible because this fold adds Statements in the order they are executed (conditions indicate themselves and their block as a statement before&after their block)
    /// As such this function doesn't return the new StatementId instead `empty_range_from_end` and `extend_range_to_end` are used to create the range of the folded block by the calle
    fn fold_statement(&mut self, id: StatementId, dst: &mut Block) {
        let span = trace_span!("statement", id = id.index(),);
        let _enter = span.enter();
        let attributes = self.base.ast[id].attributes;
        let span = self.base.ast[id].span;
        self.base.enter_sctxt(span, attributes);

        let stmnt = match self.base.ast[id].contents {
            ast::Statement::Block(id) => {
                if let Some(scope) = &self.base.ast[id].scope {
                    self.state.test_allowed(
                        &mut self.base,
                        AllowedOperation::NamedBlocks,
                        scope.ident.span,
                    );
                    self.base.check_ident(scope.ident, MockSymbolDeclaration::Block);

                    self.base.resolver.enter_scope(&scope.symbols);

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

                    self.fold_block(id, dst);
                    self.base.resolver.exit_scope();
                } else {
                    self.fold_block(id, dst);
                }

                return;
            }

            ast::Statement::Condition(cond, true_block, false_block) => {
                if let Some((cond, true_block, false_block)) =
                    self.fold_condition(cond, true_block, false_block)
                {
                    Statement::Condition(cond, true_block, false_block)
                } else {
                    return;
                }
            }

            ast::Statement::While(cond, body) => {
                let condition = self.fold_expression(cond);
                let mut new_body = Block::with_capacity(256);
                self.fold_statement(body, &mut new_body);
                if let Some(condition) = condition {
                    Statement::While(condition, new_body)
                } else {
                    return;
                }
            }

            ast::Statement::For(ref for_loop) => {
                let ast::ForLoop {
                    cond,
                    init: (ref init_var_ident, init_expr),
                    incr: (ref incr_var_ident, incr_expr),
                    body,
                } = *for_loop;

                let init_var =
                    resolve_hierarchical!(self.base; init_var_ident as  Variable(id) => id);

                self.base.enter_sctxt(
                    init_var_ident.span().extend(self.base.ast[init_expr].span),
                    Attributes::EMPTY,
                );
                let init_expr = self.fold_expression(init_expr);
                let init_sctx = self.base.exit_sctxt();

                let incr_var =
                    resolve_hierarchical!(self.base; incr_var_ident as  Variable(id) => id);

                self.base.enter_sctxt(
                    incr_var_ident.span().extend(self.base.ast[incr_expr].span),
                    Attributes::EMPTY,
                );
                let incr_expr = self.fold_expression(incr_expr);
                let incr_sctx = self.base.exit_sctxt();

                let cond = self.fold_expression(cond);

                let mut f_body = Block::with_capacity(32);

                self.fold_statement(body, &mut f_body);

                if let (
                    Some(cond),
                    Some(init_var),
                    Some(init_expr),
                    Some(incr_var),
                    Some(incr_expr),
                ) = (cond, init_var, init_expr, incr_var, incr_expr)
                {
                    let init = self
                        .base
                        .hir
                        .statements
                        .push((Statement::Assignment(init_var, init_expr), init_sctx));

                    let incr = self
                        .base
                        .hir
                        .statements
                        .push((Statement::Assignment(incr_var, incr_expr), incr_sctx));

                    Statement::For(ForLoop { cond, body: f_body, init, incr })
                } else {
                    return;
                }
            }

            ast::Statement::Assignment(ref ident, value) => {
                let value = self.fold_expression(value);
                let var = resolve_hierarchical!(self.base; ident as Variable(id) => id);
                if let (Some(var), Some(value)) = (var, value) {
                    Statement::Assignment(var, value)
                } else {
                    return;
                }
            }

            ast::Statement::Contribute(access, value) => {
                self.state.test_allowed(&mut self.base, AllowedOperation::Contribute, span);

                if let Some((discipline_access, branch, value)) =
                    self.fold_contribute(access, value, span)
                {
                    Statement::Contribute(discipline_access, branch, value)
                } else {
                    return;
                }
            }
            ast::Statement::FunctionCall(call, ref args) => {
                let fun = ExpressionFolder {
                    state: self.state,
                    branch_resolver: Some(&mut self.branch_resolver),
                    base: &mut self.base,
                }
                .fold_function(span, call, args);

                match fun {
                    Some(FunctionFoldResult::Function(fun, args, span)) => {
                        Statement::FunctionCall(fun, args, span)
                    }

                    Some(FunctionFoldResult::BranchAccess(_)) => {
                        Linter::dispatch_late(
                            Box::new(UselessFunctionCall { span, name: call.name, decl: None }),
                            self.base.sctx,
                        );
                        return;
                    }
                    None => {
                        return;
                    }
                }
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

                        let mut body = Block::with_capacity(32);
                        self.fold_statement(case.stmt?, &mut body);

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

                let default = default.map_or(Block::new(), |default| default.contents);

                if let Some(expr) = expr {
                    Statement::Case(Cases { expr, cases, default })
                } else {
                    return;
                }
            }

            ast::Statement::Error => unreachable!(),
            ast::Statement::NoOp => {
                return; //Nothing to do
            }
        };

        let sctx = self.base.exit_sctxt();
        let stmnt = self.base.hir.statements.push((stmnt, sctx));
        dst.push(stmnt)
    }

    fn fold_contribute(
        &mut self,
        access_expr: ExpressionId,
        value: ExpressionId,
        span: Span,
    ) -> Option<(DisciplineAccess, BranchId, ExpressionId)> {
        let value = self.fold_expression(value);
        match self.base.ast[access_expr].contents {
            ast::Expression::Primary(ast::Primary::FunctionCall(ident, ref args)) => {
                let access = NatureAccess::resolve_from_ident(ident, &mut self.base)?;
                let (access, branch) =
                    self.branch_resolver.resolve_branch_probe_call(access, args, &mut self.base)?;
                if self.base.hir[branch].kind == BranchKind::PortBranch {
                    self.base.error(ContributeToBranchPortProbe(self.base.ast[access_expr].span));
                }
                let value = value?;

                match access {
                    DisciplineAccess::Flow => {
                        self.base.hir[branch].current_contributions.push(span)
                    }
                    DisciplineAccess::Potential => {
                        // Collapse hints are always allowed
                        if self.base.hir[value].contents
                            != Expression::Constant(ConstVal::Scalar(SimpleConstVal::Real(0.0)))
                        {
                            self.base.hir[branch].voltage_access.push(span)
                        }
                    }
                }

                Some((access, branch, value))
            }

            ast::Expression::Primary(ast::Primary::PortFlowProbe(_ident, ref _port)) => {
                self.base.error(ContributeToBranchPortProbe(self.base.ast[access_expr].span));
                None
            }
            _ => {
                self.base.error(ContributeToNonBranchProbe(self.base.ast[access_expr].span));
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
        let mut f_true_block = Block::with_capacity(128);
        self.fold_statement(true_block, &mut f_true_block);

        let false_block = if let Some(statement) = false_block {
            let mut false_block = Block::with_capacity(128);
            self.fold_statement(statement, &mut false_block);
            false_block
        } else {
            Block::new()
        };

        Some((condition?, f_true_block, false_block))
    }

    /// Just a utility method that makes folding expressions a little more ergonomic
    fn fold_const_expression(&mut self, expr: ExpressionId) -> Option<ExpressionId> {
        self.fold_expression_with(expr, VerilogAState::new_runtime_const())
    }

    /// Just a utility method that makes folding expressions a little more ergonomic
    fn fold_expression(&mut self, expr: ExpressionId) -> Option<ExpressionId> {
        self.fold_expression_with(expr, self.state)
    }

    /// Just a utility method that makes folding expressions a little more ergonomic
    fn fold_expression_with(
        &mut self,
        expr: ExpressionId,
        state: VerilogAState,
    ) -> Option<ExpressionId> {
        ExpressionFolder {
            state,
            branch_resolver: Some(&mut self.branch_resolver),
            base: &mut self.base,
        }
        .fold(expr)
    }

    fn fold_block(&mut self, block: BlockId, dst: &mut Block) {
        for statement in self.base.ast[block].statements.iter().copied() {
            self.fold_statement(statement, dst);
        }
    }

    /// Folds a variable
    /// This is just folds the default value if it exists and just copys thre rest
    fn fold_variable(&mut self, id: VariableId) {
        let var = &self.base.ast[id];
        self.base.enter_sctxt(var.span, var.attributes);
        self.base.check_ident(var.contents.ident, MockSymbolDeclaration::Variable);

        let default =
            self.base.ast[id].contents.default.and_then(|expr| self.fold_const_expression(expr));

        self.base.hir[id] = Variable {
            ident: var.contents.ident,
            ty: var.contents.ty,
            default,
            sctx: self.base.exit_sctxt(),
        }
    }

    #[allow(clippy::or_fun_call)]
    fn fold_parameter(&mut self, id: ParameterId) {
        self.base.enter_sctxt(self.base.ast[id].span, self.base.ast[id].attributes);
        let param = &self.base.ast[id].contents;
        self.base.check_ident(param.ident, MockSymbolDeclaration::Parameter);

        let default = self.fold_const_expression(param.default);

        let param_type = match param.param_constraints {
            ast::ParameterConstraints::Ordered(ref constraints) => {
                let (from_constraints, exlude_constraints) =
                    self.fold_ordered_parameter_constraints(constraints);
                ParameterConstraint::Ordered(from_constraints, exlude_constraints)
            }

            ast::ParameterConstraints::Unordered(ref constraints) => {
                let (from_constraints, exclude_constrains) =
                    self.fold_unordered_parameter_constraints(constraints);
                ParameterConstraint::Unordered(from_constraints, exclude_constrains)
            }
        };

        self.base.hir[id] = Parameter {
            ident: param.ident,
            constraints: param_type,
            // dummy default value in case of error
            default: default.unwrap_or(ExpressionId::from_raw_unchecked(u32::MAX)),
            ty: param.ty,
            sctx: self.base.exit_sctxt(),
        };
    }

    fn fold_ordered_parameter_constraints(
        &mut self,
        valid_values: &[OrderedParameterConstraint],
    ) -> (Vec<ParameterRangeConstraint<ExpressionId>>, Vec<ParameterExcludeConstraint<ExpressionId>>)
    {
        let mut from = Vec::with_capacity(valid_values.len());
        let mut excluded = Vec::with_capacity(valid_values.len());

        for contraints in valid_values {
            match contraints {
                OrderedParameterConstraint::From(range) => {
                    if let Some(range) = self.fold_ordered_parameter_range_constraint_bound(range) {
                        from.push(range)
                    }
                }

                OrderedParameterConstraint::Exclude(ParameterExcludeConstraint::Value(val)) => {
                    if let Some(val) = self.fold_const_expression(*val) {
                        excluded.push(ParameterExcludeConstraint::Value(val))
                    }
                }
                OrderedParameterConstraint::Exclude(ParameterExcludeConstraint::Range(range)) => {
                    if let Some(range) = self.fold_ordered_parameter_range_constraint_bound(range) {
                        excluded.push(ParameterExcludeConstraint::Range(range))
                    }
                }
            }
        }
        (from, excluded)
    }

    fn fold_ordered_parameter_range_constraint_bound(
        &mut self,
        range: &ParameterRangeConstraint<ExpressionId>,
    ) -> Option<Range<ParameterRangeConstraintBound<ExpressionId>>> {
        let start = self.fold_const_expression(range.start.bound);
        let end = self.fold_const_expression(range.end.bound);
        let start =
            ParameterRangeConstraintBound { bound: start?, inclusive: range.start.inclusive };

        let end = ParameterRangeConstraintBound { bound: end?, inclusive: range.end.inclusive };

        Some(start..end)
    }

    fn fold_unordered_parameter_constraints(
        &mut self,
        valid_values: &[UnorderedParameterConstraint],
    ) -> (Vec<ExpressionId>, Vec<ExpressionId>) {
        let mut included = Vec::with_capacity(valid_values.len());
        let mut excluded = Vec::with_capacity(valid_values.len());

        for contraints in valid_values {
            match contraints {
                UnorderedParameterConstraint::From(values) => {
                    self.fold_unordered_parameter_constraint(*values, &mut included)
                }

                UnorderedParameterConstraint::Exclude(values) => {
                    self.fold_unordered_parameter_constraint(*values, &mut excluded)
                }
            }
        }
        (included, excluded)
    }

    fn fold_unordered_parameter_constraint(
        &mut self,
        expr: ExpressionId,
        dst: &mut Vec<ExpressionId>,
    ) {
        if let ast::Expression::Array(values) = &self.base.ast[expr].contents {
            for expr in values {
                if let Some(expr) = self.fold_const_expression(*expr) {
                    dst.push(expr)
                }
            }
        } else if let Some(expr) = self.fold_const_expression(expr) {
            dst.push(expr)
        }
    }
}
