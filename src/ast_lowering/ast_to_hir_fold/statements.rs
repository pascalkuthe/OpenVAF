use std::ops::Range;

use crate::ast::{
    AttributeNode, ModuleItem, Node, NumericalParameterRangeBound, NumericalParameterRangeExclude,
    Parameter, ParameterType, Variable,
};
use crate::ast_lowering::ast_to_hir_fold::{ExpressionFolder, Fold};
use crate::ast_lowering::branch_resolution::BranchResolver;
use crate::ast_lowering::error::{Error, Type};
use crate::ast_lowering::name_resolution::Resolver;
use crate::ast_lowering::VerilogContext;
use crate::compact_arena::{NanoArena, SafeRange, TinyArena};
use crate::hir::{Condition, Module, Statement};
use crate::ir::ast::{Attributes, BranchAccess};
use crate::ir::{BlockId, ModuleId, StatementId, Write};
use crate::ir::{ExpressionId, ParameterId, UnsafeWrite, VariableId};
use crate::parser::error::Unsupported;
use crate::symbol::Ident;
use crate::symbol_table::SymbolDeclaration;
use crate::util::{Push, SafeRangeCreation};
use crate::{ast, Ast, Hir};

/// The last fold folds all in textual order
pub struct Statements<'tag, 'lt> {
    pub(super) branch_resolver: BranchResolver<'tag, 'lt>,
    pub(super) state: VerilogContext,
    pub(super) base: Fold<'tag, 'lt>,
}
impl<'tag, 'lt> Statements<'tag, 'lt> {
    pub fn fold(mut self) -> Result<Box<Hir<'tag>>, Vec<Error<'tag>>> {
        unsafe {
            //This is save since we get the ptrs using borrows and drop is never called since they are copy
            TinyArena::init_from(&mut self.base.hir.variables, &self.base.ast.variables);
            NanoArena::init_from(&mut self.base.hir.modules, &self.base.ast.modules);
        }

        for module_id in SafeRangeCreation::<ModuleId<'tag>>::full_range(self.base.ast) {
            let module = &self.base.ast[module_id];
            self.state.insert(VerilogContext::constant);
            for variable in module.contents.variables {
                self.fold_variable(variable);
            }

            for parameter in module.contents.parameter_list {
                self.fold_parameter(parameter);
            }
            self.state.remove(VerilogContext::constant);

            let analog_statements = self.base.hir.empty_range_from_end();

            self.base
                .resolver
                .enter_scope(&module.contents.symbol_table);

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
                AttributeNode {
                    contents: Module {
                        name: module.contents.name,
                        port_list: module.contents.port_list,
                        analog: self.base.hir.extend_range_to_end(analog_statements),
                    },
                    source: module.source,
                    attributes: module.attributes,
                },
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
            return Ok(self.base.hir);
        } else {
            Err(self.base.errors)
        }
    }

    /// Folds a statements. StatementIds are not stable because the amount of statements may change during this fold
    /// The way that Statement Blocks are stored also changes. Instead of an Vec<StatementId> we switch to a Range of StatementIds
    /// This is possible because this fold adds Statements in the order they are executed (conditions indicate themselves and their block as a statement before&after their block)
    /// As such this function doesn't return the new StatementId instead [`empty_range_from_end`](VARF::util::SafeRangeCreation:empty_range_from_end) and [`extend_range_to_end`](VARF::util::SafeRangeCreation:extend_range_to_end) are used to create the range of the folded block by the calle
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
                if let Ok(contents) = self.fold_condition(&condition.contents) {
                    self.base.hir.push(Statement::Condition(AttributeNode {
                        contents,
                        source: condition.source,
                        attributes: condition.attributes,
                    }));
                }
            }

            ast::Statement::Assign(ref attr, ref ident, value) => {
                resolve_hierarchical!(self.base; ident as Variable(id) => {
                    if let Ok(value) = self.fold_expression(value){
                        self.base.hir.push(Statement::Assignment(*attr, id, value));
                    }
                })
            }

            ast::Statement::Contribute(attr, ref nature_name, ref branch, value) => {
                self.fold_contribute(attr, nature_name, branch, value);
            }

            ast::Statement::FunctionCall(attr, ref name, ref parameters) => {
                let parameters = parameters
                    .iter()
                    .copied()
                    .map(|expr| self.fold_expression(expr))
                    .filter_map(Result::ok)
                    .collect();

                resolve_hierarchical!(self.base; name as
                        Function(fid) => {
                            self.base.hir.push(Statement::FunctionCall(attr,fid,parameters));
                    }
                )
            }
            ast::Statement::BuiltInFunctionCall(function_call) => {
                //doesnt change the programm flow when we emit this. Might change in the future when stateful functions are introduced
            }
        }
    }

    fn fold_contribute(
        &mut self,
        attr: Attributes<'tag>,
        nature_name: &Ident,
        branch: &Node<BranchAccess>,
        value: ExpressionId<'tag>,
    ) -> Result<(), ()> {
        let (branch, discipline) = self
            .branch_resolver
            .resolve_branch_access(&mut self.base, branch)?;

        let nature = self.branch_resolver.resolve_discipline_access(
            &mut self.base,
            nature_name,
            discipline,
        )?;
        let value = self.fold_expression(value)?;
        self.base
            .hir
            .push(Statement::Contribute(attr, nature, branch, value));
        Ok(())
    }

    /// folds a condition/if statement
    fn fold_condition(&mut self, condition: &ast::Condition<'tag>) -> Result<Condition<'tag>, ()> {
        let main_condition = self.fold_expression(condition.main_condition);

        let main_condition_statements = self.base.hir.empty_range_from_end();
        self.fold_statement(condition.main_condition_statement);
        let main_condition_statements =
            self.base.hir.extend_range_to_end(main_condition_statements);

        let else_ifs: Vec<(ExpressionId<'tag>, SafeRange<StatementId<'tag>>)> = condition
            .else_ifs
            .iter()
            .copied()
            .filter_map(|(condition, statement)| {
                let condition = self.fold_expression(condition).ok()?;
                let statements = self.base.hir.empty_range_from_end();
                self.fold_statement(statement);
                Some((condition, self.base.hir.extend_range_to_end(statements)))
            })
            .collect();

        let statements = self.base.hir.empty_range_from_end();
        if let Some(statement) = condition.else_statement {
            self.fold_statement(statement);
        }
        let else_statement = self.base.hir.extend_range_to_end(statements);

        Ok(Condition {
            main_condition: main_condition?,
            main_condition_statements,
            else_ifs,
            else_statement,
        })
    }

    /// Just a utility method that makes folding expressions a little more ergonomic
    fn fold_expression(&mut self, expr: ExpressionId<'tag>) -> Result<ExpressionId<'tag>, ()> {
        let mut fold = ExpressionFolder {
            base: &mut self.base,
            state: self.state,
            branch_resolver: &mut self.branch_resolver,
        };
        fold.fold_expression(expr)
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
            .map(|expr| self.fold_expression(expr).ok())
            .flatten();
        self.base.hir.write(
            variable,
            AttributeNode {
                contents: Variable {
                    default_value,
                    ..self.base.ast[variable].contents
                },
                ..self.base.ast[variable]
            },
        )
    }

    fn fold_parameter(&mut self, parameter_id: ParameterId<'tag>) {
        let default_value = self.base.ast[parameter_id]
            .contents
            .default_value
            .map(|expr| self.fold_expression(expr).ok())
            .flatten();

        if let ParameterType::Numerical {
            parameter_type,
            ref included_ranges,
            ref excluded_ranges,
        } = self.base.ast[parameter_id].contents.parameter_type
        {
            let included_ranges = included_ranges
                .iter()
                .map(
                    |range| -> Result<Range<NumericalParameterRangeBound<'tag>>, ()> {
                        Ok(Range {
                            start: NumericalParameterRangeBound {
                                bound: self.fold_expression(range.start.bound)?,
                                ..range.start
                            },
                            end: NumericalParameterRangeBound {
                                bound: self.fold_expression(range.end.bound)?,
                                ..range.end
                            },
                        })
                    },
                )
                .filter_map(Result::ok)
                .collect();

            let excluded_ranges = excluded_ranges
                .iter()
                .map(
                    |exclude| -> Result<NumericalParameterRangeExclude<'tag>, ()> {
                        match exclude {
                            NumericalParameterRangeExclude::Value(val) => Ok(
                                NumericalParameterRangeExclude::Value(self.fold_expression(*val)?),
                            ),
                            NumericalParameterRangeExclude::Range(range) => {
                                Ok(NumericalParameterRangeExclude::Range(Range {
                                    start: NumericalParameterRangeBound {
                                        bound: self.fold_expression(range.start.bound)?,
                                        ..range.start
                                    },
                                    end: NumericalParameterRangeBound {
                                        bound: self.fold_expression(range.start.bound)?,
                                        ..range.end
                                    },
                                }))
                            }
                        }
                    },
                )
                .filter_map(Result::ok)
                .collect();

            unsafe {
                //this is save since it happens unconditionally for all parameters
                self.base.hir.write_unsafe(
                    parameter_id,
                    AttributeNode {
                        source: self.base.ast[parameter_id].source,
                        attributes: self.base.ast[parameter_id].attributes,
                        contents: Parameter {
                            name: self.base.ast[parameter_id].contents.name,
                            default_value,
                            parameter_type: ParameterType::Numerical {
                                parameter_type,
                                included_ranges,
                                excluded_ranges,
                            },
                        },
                    },
                )
            }
        } else {
            self.base.hir[parameter_id] = self.base.ast[parameter_id].clone(); //required for safety
            self.base.error(Error {
                error_type: Type::Unsupported(Unsupported::StringParameters),
                source: self.base.ast[parameter_id].source,
            })
        }
    }
}
