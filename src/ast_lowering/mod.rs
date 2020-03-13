use ahash::AHashMap;

use crate::ast::visitor::*;
use crate::ast::{Ast, AttributeNode, ModuleItem, Node};
use crate::ast::{HierarchicalId, Visitor};
use crate::ast_lowering::error::Type::{NotAScope, UnexpectedTokenInBranchAccess};
use crate::ast_lowering::error::{Error, NonConstantExpression, Type};
use crate::compact_arena::SafeRange;
use crate::ir::ast::{BuiltInFunctionCall, NetType, VariableType};
use crate::ir::hir::{
    Branch, BranchDeclaration, Condition, Discipline, DisciplineAccess, Expression, Net, Port,
    Primary, Statement,
};
use crate::ir::hir::{Hir, Module};
use crate::ir::{
    AttributeId, BlockId, BranchId, DisciplineId, ExpressionId, ModuleId, NatureId, NetId,
    ParameterId, PortId, StatementId, VariableId,
};
use crate::symbol::Ident;
use crate::symbol::{keywords, Symbol};
use crate::symbol_table::{SymbolDeclaration, SymbolTable};
use crate::util::{Push, SafeRangeCreation};
use crate::{ast, SourceMap};

#[cfg(test)]
mod test;

macro_rules! resolve {
    ($self:ident; $name:ident as $($declaration:ident($id:ident) => $block:block),+) => {
        match $self.resolve($name) {
            $(Ok($crate::symbol_table::SymbolDeclaration::$declaration($id)) => $block),+
            Err(error) => {
                $self.errors.push(error);
            }
            Ok(found) => {
                use $crate::ast_lowering::error;
                $self.errors.push(Error {
                    error_type: error::Type::DeclarationTypeMismatch {
                        found,
                        expected: vec![$(error::MockSymbolDeclaration::$declaration),+],
                    },
                    source: $name.span,
                });
            }
        }
    };
}
macro_rules! resolve_hierarchical {
    ($self:ident; $name:ident as  $($declaration:ident($id:ident) => $block:block),+) => {
        match $self.resolve_hierarchical_id($name) {
            $(Ok($crate::symbol_table::SymbolDeclaration::$declaration($id)) => $block),+
            Err(error) => {
                $self.errors.push(error);
            }
            Ok(found) => {
                use $crate::ast_lowering::error;
                $self.errors.push(Error {
                    error_type: error::Type::DeclarationTypeMismatch {
                        found,
                        expected:  vec![$(error::MockSymbolDeclaration::$declaration),+],
                    },
                    source: $name.span(),
                });
            }
        }
    };
}

//TODO input/output enforcement
//TODO type checking
pub mod error;
type Result<T = ()> = std::result::Result<T, ()>;
bitflags! {
    struct VerilogContext: u8{
        const constant = 0b00000001;
        const conditional = 0b00000010;
        const analog = 0b00000100;

    }
}
struct AstToHirFolder<'tag, 'astref> {
    scope_stack: Vec<&'astref SymbolTable<'tag>>,
    ast: &'astref Ast<'tag>,
    hir: Box<Hir<'tag>>,
    errors: Vec<Error<'tag>>,
    state: VerilogContext,
    unnamed_branches: AHashMap<(NetId<'tag>, NetId<'tag>), BranchId<'tag>>,
    unnamed_port_branches: AHashMap<PortId<'tag>, BranchId<'tag>>,
}
impl<'tag, 'astref> AstToHirFolder<'tag, 'astref> {
    fn resolve_branch(
        &mut self,
        branch: &ast::Branch,
    ) -> Result<(Branch<'tag>, DisciplineId<'tag>)> {
        match branch {
            ast::Branch::Port(ref port) => {
                resolve_hierarchical!(self; port as Port(port_id) => {
                    return Ok((Branch::Port(port_id),self.hir[self.hir[port_id].net].contents.discipline));
                });
            }
            ast::Branch::Nets(ref net1, ref net2) => {
                let mut first_net = Err(());
                resolve_hierarchical!(self; net1 as
                    Net(id) => {
                        first_net = Ok(id);
                    },
                    Port(id) => {
                        first_net = Ok(self.hir[id].net);
                    }
                );
                let mut second_net = Err(());
                resolve_hierarchical!(self; net2 as
                    Net(second_id) => {
                        second_net = Ok(second_id)
                    },
                    Port(second_id) => {
                        second_net = Ok(self.hir[second_id].net)
                    }
                );
                if let (Ok(first_net), Ok(second_net)) = (first_net, second_net) {
                    if self.hir[first_net].contents.discipline
                        != self.hir[second_net].contents.discipline
                    {
                        self.errors.push(Error {
                            error_type: Type::DisciplineMismatch(
                                self.hir[first_net].contents.discipline,
                                self.hir[second_net].contents.discipline,
                            ),
                            source: net1.span().extend(net2.span()),
                        });
                    } else {
                        //doesn't matter which nets discipline we use since we asserted that they are equal
                        return Ok((
                            Branch::Nets(first_net, second_net),
                            self.hir[first_net].contents.discipline,
                        ));
                    }
                }
            }
        }
        Err(())
    }
    fn resolve_branch_access(
        &mut self,
        branch_access: &Node<ast::BranchAccess>,
    ) -> Result<(BranchId<'tag>, DisciplineId<'tag>)> {
        match branch_access.contents {
            ast::BranchAccess::Implicit(ref branch) => {
                let (branch, discipline) = self.resolve_branch(branch)?;
                match branch {
                    Branch::Port(port) => match self.unnamed_port_branches.get(&port) {
                        Some(id) => return Ok((*id, discipline)),
                        None => {
                            return Ok((
                                self.hir.push(AttributeNode {
                                    attributes: self.hir.empty_range_from_end(), //TODO attributes
                                    source: branch_access.source,
                                    contents: BranchDeclaration {
                                        name: Ident::from_str_and_span(
                                            format!(
                                                "unnamed port branch {}",
                                                self.hir[self.hir[port].net].contents.name
                                            )
                                            .as_str(),
                                            branch_access.source,
                                        ),
                                        branch,
                                    },
                                }),
                                discipline,
                            ));
                        }
                    },
                    Branch::Nets(net1, net2) => match self.unnamed_branches.get(&(net1, net2)) {
                        Some(id) => return Ok((*id, discipline)),
                        None => {
                            return Ok((
                                self.hir.push(AttributeNode {
                                    attributes: self.hir.empty_range_from_end(), //TODO attributes
                                    source: branch_access.source,
                                    contents: BranchDeclaration {
                                        name: Ident::from_str_and_span(
                                            format!(
                                                "unnamed branch ({},{})",
                                                self.hir[net1].contents.name.name.as_str(),
                                                self.hir[net2].contents.name.name.as_str()
                                            )
                                            .as_str(),
                                            branch_access.source,
                                        ),
                                        branch,
                                    },
                                }),
                                discipline,
                            ));
                        }
                    },
                }
            }
            ast::BranchAccess::Explicit(ref name) => {
                resolve_hierarchical!(self; name as Branch(id) => {
                    let discipline = match self.hir[id].contents.branch {
                        Branch::Port(portid) => {
                            self.hir[self.hir[portid].net].contents.discipline
                        }
                        Branch::Nets(net1, _) => self.hir[net1].contents.discipline
                    };
                    return Ok((id,discipline))
                })
            }
        }
        Err(())
    }

    fn resolve_discipline_access(
        &mut self,
        nature_name: &Ident,
        discipline: DisciplineId<'tag>,
    ) -> Result<DisciplineAccess> {
        match nature_name.name {
            keywords::FLOW => Ok(DisciplineAccess::Flow),
            keywords::POTENTIAL => Ok(DisciplineAccess::Potential),
            _ => {
                resolve!(self; nature_name as
                    Nature(id) => {
                        return match id {
                            id if id == self.hir[discipline].contents.flow_nature => Ok(DisciplineAccess::Flow),
                            id if id == self.hir[discipline].contents.potential_nature => Ok(DisciplineAccess::Potential),
                            id => {
                                self.errors.push(Error{source:nature_name.span,error_type:Type::NatureNotPotentialOrFlow(nature_name.name,discipline)});
                                Err(())
                            },
                        };
                    }
                );
                Err(())
            }
        }
    }
    fn resolve_discipline(&mut self, ident: &Ident) -> Result<DisciplineId<'tag>> {
        match ident.name {
            keywords::EMPTY_SYMBOL => todo!("Implicit Disciplines are currently not supported"),
            _ => {
                resolve!(self; ident as Discipline(id) => {return Ok(id)});
                Err(())
            }
        }
    }

    fn resolve(&self, ident: &Ident) -> error::Result<'tag, SymbolDeclaration<'tag>> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(res) = scope.get(&ident.name) {
                return Ok(*res);
            }
        }
        Err(Error {
            error_type: Type::NotFound(ident.name),
            source: ident.span,
        })
    }

    fn resolve_hierarchical_id(
        &self,
        hierarchical_ident: &HierarchicalId,
    ) -> error::Result<'tag, SymbolDeclaration<'tag>> {
        let mut iter = hierarchical_ident.names.iter();
        let ident = iter.next().unwrap();
        let last_span = ident.span;
        let mut current = self.resolve(ident)?;
        for ident in iter {
            let symbol_table = match current {
                SymbolDeclaration::Module(module) => &self.ast[module].contents.symbol_table,
                SymbolDeclaration::Block(block_id) => {
                    if let Some(scope) = &self.ast[block_id].contents.scope {
                        &scope.symbols
                    } else {
                        return Err(Error {
                            error_type: NotAScope {
                                declaration: self.ast[block_id].source,
                            },
                            source: last_span,
                        });
                    }
                }
                found => {
                    return Err(Error {
                        error_type: NotAScope {
                            declaration: found.span(self.ast),
                        },
                        source: last_span,
                    });
                }
            };
            if let Some(found) = symbol_table.get(&ident.name) {
                current = *found;
            } else {
                return Err(Error {
                    error_type: Type::NotFound(ident.name),
                    source: ident.span,
                });
            }
        }
        Ok(current)
    }

    unsafe fn init(ast: &'astref mut Ast<'tag>) -> Self {
        Self {
            hir: Hir::partial_initalize(ast),
            ast,
            scope_stack: vec![&ast.top_symbols],
            errors: Vec::with_capacity(64),
            state: VerilogContext::analog,
            unnamed_branches: AHashMap::with_capacity(64),
            unnamed_port_branches: AHashMap::with_capacity(64),
        }
    }

    fn run(&mut self) {
        for nature in self.ast.full_range() {
            self.visit_nature(nature, self.ast);
        }

        for discipline in self.ast.full_range() {
            self.visit_discipline(discipline, self.ast);
        }
        for net in self.ast.full_range() {
            self.visit_net(net, self.ast);
        }
        for port in self.ast.full_range() {
            self.visit_port(port, self.ast);
        }
        if !self.errors.is_empty() {
            return;
        }

        self.state.insert(VerilogContext::constant);
        for module in SafeRangeCreation::<ModuleId<'tag>>::full_range(self.ast) {
            let module: &AttributeNode<ast::Module> = &self.ast[module];
            self.scope_stack.push(&module.contents.symbol_table);
            for decl in module.contents.symbol_table.values().copied() {
                match decl {
                    SymbolDeclaration::Nature(_) => unreachable_unchecked!("Natures can't be declared inside nature so the parser won't ever place this here"),
                    SymbolDeclaration::Module(_)=>unreachable_unchecked!("Module cant be declared inside modules so the parser won't ever place this here"),
                    SymbolDeclaration::Discipline(_) => unreachable_unchecked!("Discipline can't be declared inside modules so the parser won't ever place this here"),
                    SymbolDeclaration::Function(_) => unreachable_unchecked!("Functions can't be declared inside modules so the parser won't ever place this here"),
                    SymbolDeclaration::Branch(branch) => {self.visit_branch_declaration(branch,self.ast);},
                    SymbolDeclaration::Block(_) => (),//Blocks are visited later
                    SymbolDeclaration::Port(_) =>(), //ports had to be visited before
                    SymbolDeclaration::Net(_) =>(), //nets had to be visited before
                    SymbolDeclaration::Variable(variable_id) => {self.visit_variable(variable_id,self.ast);},
                    SymbolDeclaration::Parameter(parameter_id) => {self.visit_parameter(parameter_id,self.ast);},
                }
            }
            self.scope_stack.pop();
        }
        self.state.remove(VerilogContext::constant);

        if !self.errors.is_empty() {
            return;
        }

        //Depth first visit of all modules

        for module in self.ast.full_range() {
            self.visit_module(module, self.ast);
        }
    }

    fn done(self) -> Box<Hir<'tag>> {
        self.hir
    }
    fn reinterpret_expression_as_identifier<'lt>(
        &mut self,
        expression: &'lt Node<ast::Expression<'tag>>,
    ) -> Result<&'lt HierarchicalId> {
        if let ast::Expression::Primary(ast::Primary::VariableOrNetReference(ref name)) =
            expression.contents
        {
            Ok(name)
        } else {
            self.errors.push(Error {
                source: expression.source,
                error_type: UnexpectedTokenInBranchAccess,
            });
            Err(())
        }
    }
    fn reinterpret_function_call_as_branch_access(
        &mut self,
        parameters: Vec<ExpressionId<'tag>>,
    ) -> Result<(BranchId<'tag>, DisciplineId<'tag>)> {
        //this has quite a lot of code duplication from resolve_branch_access; This is unavoidable atm because we would need to clone the ident because we cant move out of the ast which is something I'd really like to avoid
        match parameters.len() {
            1 => {
                let ident = self.reinterpret_expression_as_identifier(&self.ast[parameters[0]])?;
                resolve_hierarchical!(self; ident as
                    Branch(bid) => {
                        let discipline = match self.hir[bid].contents.branch {
                            Branch::Port(portid) => {
                                self.hir[self.hir[portid].net].contents.discipline
                            }
                            Branch::Nets(net1, _) => self.hir[net1].contents.discipline
                         };
                        return Ok((bid,discipline));
                    }
                );
            }
            2 => {
                let first_net_ident =
                    self.reinterpret_expression_as_identifier(&self.ast[parameters[0]])?;
                let mut first_net = Err(());
                resolve_hierarchical!(self; first_net_ident as
                    Net(nid) => {
                        first_net = Ok(nid);
                    },
                    Port(pid) => {
                        first_net = Ok(self.hir[pid].net)
                    }
                );
                let second_net_ident =
                    self.reinterpret_expression_as_identifier(&self.ast[parameters[1]])?;
                let mut second_net = Err(());
                resolve_hierarchical!(self; second_net_ident as
                    Net(id) => {
                        second_net = Ok(id);
                    },
                    Port(pid) => {
                        first_net = Ok(self.hir[pid].net)
                    }
                );
                if let (Ok(first_net), Ok(second_net)) = (first_net, second_net) {
                    if self.hir[first_net].contents.discipline
                        != self.hir[second_net].contents.discipline
                    {
                        self.errors.push(Error {
                            error_type: Type::DisciplineMismatch(
                                self.hir[first_net].contents.discipline,
                                self.hir[second_net].contents.discipline,
                            ),
                            source: first_net_ident.span().extend(second_net_ident.span()),
                        });
                    } else {
                        //doesn't matter which nets discipline we use since we asserted that they are equal might get more complicated with proper discipline resolution
                        match self.unnamed_branches.get(&(first_net, second_net)) {
                            Some(id) => return Ok((*id, self.hir[first_net].contents.discipline)),
                            None => {
                                let span = first_net_ident.span().extend(second_net_ident.span());
                                return Ok((
                                    self.hir.push(AttributeNode {
                                        attributes: self.hir.empty_range_from_end(), //TODO attributes
                                        source: span,
                                        contents: BranchDeclaration {
                                            name: Ident::from_str_and_span(
                                                format!(
                                                    "unnamed branch ({},{})",
                                                    self.hir[first_net].contents.name.name.as_str(),
                                                    self.hir[second_net]
                                                        .contents
                                                        .name
                                                        .name
                                                        .as_str()
                                                )
                                                .as_str(),
                                                span,
                                            ),
                                            branch: Branch::Nets(first_net, second_net),
                                        },
                                    }),
                                    self.hir[first_net].contents.discipline,
                                ));
                            }
                        }
                    }
                }
            }
            _ => self.errors.push(Error {
                source: self.ast[parameters[1]]
                    .source
                    .extend(self.ast[*parameters.last().unwrap()].source),
                error_type: UnexpectedTokenInBranchAccess,
            }),
        }
        Err(())
    }
}

/// The Result in Visitor is there to allow error propagation.
/// However name resolution doesn't depend on the result of other name resolutions.
/// Therefore all errors are simply to the errors vector (size > 0 indicates a failure) and Ok(()) is always returned.
/// If a result of name resolution is required by a caller we use struct field last_resolved_declaration instead
impl<'tag, 'astref> Visitor<'tag> for AstToHirFolder<'tag, 'astref> {
    fn visit_declaration_name(&mut self, ident: &Ident, ast: &Ast<'tag>) {}

    fn visit_reference(&mut self, ident: &Ident, ast: &Ast<'tag>) {}

    fn visit_hierarchical_reference(&mut self, ident: &HierarchicalId, ast: &Ast<'tag>) {}

    fn visit_net_type(&mut self, net_type: NetType, ast: &Ast<'tag>) {}

    fn visit_variable_type(&mut self, variable_type: VariableType, ast: &Ast<'tag>) {}

    fn visit_module(&mut self, module_id: ModuleId<'tag>, ast: &Ast<'tag>) {
        let module = &self.ast[module_id];
        let analog_statements = self.hir.empty_range_from_end();
        self.scope_stack.push(&module.contents.symbol_table);

        //TODO parameters
        for module_item in module.contents.children.iter() {
            match module_item {
                ModuleItem::AnalogStmt(statement) => self.visit_statement(*statement, ast),
                ModuleItem::GenerateStatement => unimplemented!("Generate Statement"),
            }
        }
        self.scope_stack.pop();
        self.hir[module_id] = AttributeNode {
            contents: Module {
                name: module.contents.name,
                port_list: module.contents.port_list,
                analog: self.hir.extend_range_to_end(analog_statements),
            },
            source: module.source,
            attributes: module.attributes,
        };
    }

    fn visit_statement(&mut self, statement: StatementId<'tag>, ast: &Ast<'tag>) {
        match &ast[statement] {
            ast::Statement::Block(id) => {
                if let Some(scope) = &self.ast[*id].contents.scope {
                    self.scope_stack.push(&scope.symbols);
                    self.state.insert(VerilogContext::constant);
                    for decl in scope.symbols.values().copied() {
                        match decl {
                            SymbolDeclaration::Nature(_) => unreachable_unchecked!("Natures can't be declared inside nature so the parser won't ever place this here"),
                            SymbolDeclaration::Module(_)=>unreachable_unchecked!("Module cant be declared inside modules so the parser won't ever place this here"),
                            SymbolDeclaration::Discipline(_) => unreachable_unchecked!("Discipline can't be declared inside modules so the parser won't ever place this here"),
                            SymbolDeclaration::Function(_) => unreachable_unchecked!("Functions can't be declared inside modules so the parser won't ever place this here"),
                            SymbolDeclaration::Branch(branch) => (),
                            SymbolDeclaration::Block(_) => (),//Blocks are visited later
                            SymbolDeclaration::Port(_) =>(), //ports had to be visited before
                            SymbolDeclaration::Net(_) =>(), //nets had to be visited before
                            SymbolDeclaration::Variable(variableid) => {self.visit_variable(variableid,self.ast);},
                            SymbolDeclaration::Parameter(parameter_id) => {self.visit_parameter(parameter_id,self.ast);},
                        }
                    }
                    self.state.remove(VerilogContext::constant);
                    self.visit_block(*id, ast);
                    self.scope_stack.pop();
                } else {
                    self.visit_block(*id, ast);
                }
            }
            ast::Statement::Condition(ref condition) => {
                self.visit_expression(condition.contents.main_condition, ast);
                let main_condition_statements = self.hir.empty_range_from_end();
                self.visit_statement(condition.contents.main_condition_statement, ast);
                let main_condition_statements =
                    self.hir.extend_range_to_end(main_condition_statements);
                let mut else_ifs = Vec::with_capacity(condition.contents.else_ifs.len());
                for (condition, statement) in condition.contents.else_ifs.iter().copied() {
                    self.visit_expression(condition, ast);
                    let statements = self.hir.empty_range_from_end();
                    self.visit_statement(statement, ast);
                    else_ifs.push((condition, self.hir.extend_range_to_end(statements)))
                }
                let else_statement = if let Some(statement) = condition.contents.else_statement {
                    let statements = self.hir.empty_range_from_end();
                    self.visit_statement(statement, ast);
                    self.hir.extend_range_to_end(statements)
                } else {
                    self.hir.empty_range_from_end()
                };
                self.hir.push(Statement::Condition(AttributeNode {
                    contents: Condition {
                        main_condition: condition.contents.main_condition,
                        main_condition_statements,
                        else_ifs,
                        else_statement,
                    },
                    source: condition.source,
                    attributes: condition.attributes,
                }));
            }
            ast::Statement::Assign(ref attr, ref ident, value) => {
                resolve_hierarchical!(self; ident as Variable(id) => {
                    self.hir.push(Statement::Assignment(*attr, id, *value));
                });
                self.visit_assign(*attr, ident, *value, ast);
            }
            ast::Statement::Contribute(attr, ref nature_name, ref branch, value) => {
                if let Ok((branch, discipline)) = self.resolve_branch_access(branch) {
                    if let Ok(nature) = self.resolve_discipline_access(nature_name, discipline) {
                        self.hir
                            .push(Statement::Contribute(*attr, nature, branch, *value));
                    }
                }
                self.visit_expression(*value, ast);
            }
            ast::Statement::FunctionCall(ref attr, ref name, ref args) => {
                for expr in args.borrow().iter().copied() {
                    self.visit_expression(expr, ast);
                }
                resolve_hierarchical!(self; name as
                    Function(fid) => {
                        self.hir.push(Statement::FunctionCall(*attr,fid,args.replace(Vec::default())));
                    }
                )
            }
            ast::Statement::BuiltInFunctionCall(function_call) => {
                walk_builtin_function_call(self, &function_call.contents, ast);
                self.hir
                    .push(Statement::BuiltInFunctionCall(*function_call));
            }
        }
    }

    fn visit_block(&mut self, block: BlockId<'tag>, ast: &Ast<'tag>) {
        walk_block(self, &ast[block], ast)
    }

    fn visit_condition(
        &mut self,
        condition: &AttributeNode<'tag, ast::Condition<'tag>>,
        ast: &Ast<'tag>,
    ) {
        unimplemented!()
    }

    fn visit_assign(
        &mut self,
        attributes: SafeRange<AttributeId<'tag>>,
        ident: &HierarchicalId,
        value: ExpressionId<'tag>,
        ast: &Ast<'tag>,
    ) {
        walk_assign(self, ident, value, ast)
    }

    fn visit_contribute(
        &mut self,
        attributes: SafeRange<AttributeId<'tag>>,
        nature: &Ident,
        branch: &Node<ast::BranchAccess>,
        value: ExpressionId<'tag>,
        ast: &Ast<'tag>,
    ) {
        unimplemented!()
    }

    fn visit_branch_access(
        &mut self,
        nature: &Ident,
        branch: &Node<ast::BranchAccess>,
        ast: &Ast<'tag>,
    ) {
        unimplemented!()
    }

    fn visit_branch(&mut self, branch: &ast::Branch, ast: &Ast<'tag>) {
        unimplemented!()
    }

    fn visit_expression(&mut self, expression_id: ExpressionId<'tag>, ast: &Ast<'tag>) {
        let expression = &self.ast[expression_id];
        match expression.contents {
            ast::Expression::BinaryOperator(lhs, op, rhs) => {
                self.hir[expression_id] = Node {
                    source: expression.source,
                    contents: Expression::BinaryOperator(lhs, op, rhs),
                };
                self.visit_expression(lhs, ast);
                self.visit_expression(rhs, ast);
            }
            ast::Expression::UnaryOperator(unary_op, expr) => {
                self.hir[expression_id] = Node {
                    source: expression.source,
                    contents: Expression::UnaryOperator(unary_op, expr),
                };
                self.visit_expression(expr, ast);
            }
            ast::Expression::Primary(ast::Primary::BranchAccess(ref nature, ref branch_access)) => {
                if let Ok((branch_access, discipline)) = self.resolve_branch_access(branch_access) {
                    if let Ok(nature) = self.resolve_discipline_access(nature, discipline) {
                        self.hir[expression_id] = Node {
                            source: expression.source,
                            contents: Expression::Primary(Primary::BranchAccess(
                                nature,
                                branch_access,
                            )),
                        };
                    }
                }
            }
            ast::Expression::Primary(ast::Primary::VariableOrNetReference(ref ident)) => {
                resolve_hierarchical!(self; ident as
                    Variable(vid) => {
                        if self.state.contains(VerilogContext::constant){
                            self.errors.push(Error{
                                source:expression.source,
                                error_type:Type::NotAllowedInConstantContext(NonConstantExpression::VariableReference)
                                });
                        }else {
                            self.hir[expression_id]=Node{
                                source:expression.source,
                                contents:Expression::Primary(Primary::VariableReference(vid))
                            }
                        }
                    },
                    Parameter(pid) => {
                        self.hir[expression_id]=Node{
                            source:expression.source,
                            contents:Expression::Primary(Primary::ParameterReference(pid))
                        }
                    }
                    /*Port(pid) => {
                        self.hir[expression_id]=Node{
                            source:expression.source,
                            contents:Expression::Primary(Primary::PortReference(pid))
                        }
                    },
                    Net(nid) => {
                        self.hir[expression_id]=Node{
                            source:expression.source,
                            contents:Expression::Primary(Primary::NetReference(nid))
                        }
                    } TODO discrete net/por access */
                )
            }
            ast::Expression::Primary(ast::Primary::Integer(val)) => {
                self.hir[expression_id] = Node {
                    source: expression.source,
                    contents: Expression::Primary(Primary::Integer(val)),
                }
            }
            ast::Expression::Primary(ast::Primary::UnsignedInteger(val)) => {
                self.hir[expression_id] = Node {
                    source: expression.source,
                    contents: Expression::Primary(Primary::UnsignedInteger(val)),
                }
            }
            ast::Expression::Primary(ast::Primary::Real(val)) => {
                self.hir[expression_id] = Node {
                    source: expression.source,
                    contents: Expression::Primary(Primary::Real(val)),
                }
            }

            ast::Expression::Primary(ast::Primary::FunctionCall(ref ident, ref parameters)) => {
                if self.state.contains(VerilogContext::constant) {
                    self.errors.push(Error {
                        source: expression.source,
                        error_type: Type::NotAllowedInConstantContext(
                            NonConstantExpression::VariableReference,
                        ),
                    });
                }

                resolve_hierarchical!(self; ident as
                    Function(fid) => {
                        for expr in parameters.borrow().iter().copied() {
                            self.visit_expression(expr, ast);
                        }
                        self.hir[expression_id]=Node{
                            source:expression.source,
                            contents:Expression::Primary(Primary::FunctionCall(fid,parameters.replace(Vec::default())))
                        };
                    },
                    Nature(nid) => {
                        if let Ok((branch_access,discipline)) = self.reinterpret_function_call_as_branch_access(parameters.replace(Vec::default())){
                             let discipline_access = match nid {
                                id if id == self.hir[discipline].contents.flow_nature => Ok(DisciplineAccess::Flow),
                                id if id == self.hir[discipline].contents.potential_nature => Ok(DisciplineAccess::Potential),
                                id => {
                                    self.errors.push(Error{source:ident.span(),error_type:Type::NatureNotPotentialOrFlow(ident.names[0].name,discipline)});
                                    Err(())
                               },
                            };
                            if let Ok(discipline_access) = discipline_access{
                                self.hir[expression_id] = Node{
                                    source:expression.source,
                                    contents:Expression::Primary(Primary::BranchAccess(discipline_access,branch_access))
                                }
                            }
                        }
                    }
                )
            }
            ast::Expression::Primary(ast::Primary::BuiltInFunctionCall(
                ref primary_function_call,
            )) => {
                walk_builtin_function_call(self, primary_function_call, ast);
                self.hir[expression_id] = Node {
                    contents: Expression::Primary(Primary::BuiltInFunctionCall(
                        *primary_function_call,
                    )),
                    source: expression.source,
                }
            }
        }
    }

    fn visit_expression_primary(&mut self, primary: &ast::Primary<'tag>, ast: &Ast<'tag>) {
        unimplemented!()
    }

    fn visit_branch_declaration(&mut self, branch_declaration_id: BranchId<'tag>, ast: &Ast<'tag>) {
        let branch_declaration = &self.ast[branch_declaration_id];
        if let Ok((resolved_branch, _)) = self.resolve_branch(&branch_declaration.contents.branch) {
            self.hir[branch_declaration_id] = AttributeNode {
                attributes: branch_declaration.attributes,
                source: branch_declaration.source,
                contents: BranchDeclaration {
                    name: branch_declaration.contents.name,
                    branch: resolved_branch,
                },
            }
        }
    }
    fn visit_port(&mut self, port: PortId<'tag>, ast: &Ast<'tag>) {
        let unresolved_port = &ast[port];
        if let Ok(discipline) = self.resolve_discipline(&unresolved_port.contents.discipline) {
            self.hir[port] = Port {
                input: unresolved_port.contents.input,
                output: unresolved_port.contents.output,
                net: self.hir.push(AttributeNode {
                    attributes: unresolved_port.attributes,
                    source: unresolved_port.source,
                    contents: Net {
                        name: unresolved_port.contents.name,
                        discipline,
                        signed: unresolved_port.contents.signed,
                        net_type: unresolved_port.contents.net_type,
                    },
                }),
            };
        }
    }

    fn visit_net(&mut self, net: NetId<'tag>, ast: &Ast<'tag>) {
        let unresolved_net = &ast[net];
        if let Ok(discipline) = self.resolve_discipline(&unresolved_net.contents.discipline) {
            self.hir[net] = AttributeNode {
                attributes: unresolved_net.attributes,
                source: unresolved_net.source,
                contents: Net {
                    name: unresolved_net.contents.name,
                    discipline,
                    signed: unresolved_net.contents.signed,
                    net_type: unresolved_net.contents.net_type,
                },
            }
        }
    }

    fn visit_variable(&mut self, variable: VariableId<'tag>, ast: &Ast<'tag>) {
        walk_variable(self, variable, ast);
    }

    fn visit_nature(&mut self, nature: NatureId<'tag>, ast: &Ast<'tag>) {
        /*Nothing to do*/
        //TODO advanced natures
    }
    fn visit_discipline(&mut self, discipline: DisciplineId<'tag>, ast: &Ast<'tag>) {
        /*Nothing to do*/
        let unresolved_discipline = &ast[discipline];
        let mut flow_nature = Err(());
        let ident = &unresolved_discipline.contents.flow_nature;
        resolve!(self; ident as
            Nature(id) => {
                flow_nature = Ok(id)
            }
        );
        let mut pot_nature = Err(());
        let ident = &unresolved_discipline.contents.potential_nature;
        resolve!(self; ident as
            Nature(id) => {
                pot_nature = Ok(id)
            }
        );
        if let (Ok(potential_nature), Ok(flow_nature)) = (pot_nature, flow_nature) {
            self.hir[discipline] = AttributeNode {
                attributes: unresolved_discipline.attributes,
                source: unresolved_discipline.source,
                contents: Discipline {
                    name: unresolved_discipline.contents.name,
                    flow_nature,
                    potential_nature,
                },
            };
        }
    }

    fn visit_parameter(&mut self, parameter_id: ParameterId<'tag>, ast: &Ast<'tag>) {
        walk_parameter(self, parameter_id, ast)
    }

    fn visit_built_in_function_call(
        &mut self,
        function_call: &BuiltInFunctionCall<'tag>,
        ast: &Ast<'tag>,
    ) {
        unimplemented!()
    }
}

pub fn resolve<'tag>(mut ast: Box<Ast<'tag>>) -> std::result::Result<Box<Hir<'tag>>, Vec<Error>> {
    let mut fold = unsafe { AstToHirFolder::init(&mut ast) }; //this is save since the only thing that made this unsafe way calling only a part of this and expecting a valid hir
    fold.run();
    if fold.errors.is_empty() {
        let mut hir = fold.done();
        unsafe {
            hir.finish_partial_initalize(&mut ast);
        }
        Ok(hir)
    } else {
        Err(fold.errors)
    }
}
pub fn resolve_and_print<'tag>(
    mut ast: Box<Ast<'tag>>,
    source_map: &SourceMap,
) -> Result<Box<Hir<'tag>>> {
    let mut fold = unsafe { AstToHirFolder::init(&mut ast) }; //this is save since the only thing that made this unsafe way calling only a part of this and expecting a valid hir
    fold.run();
    if fold.errors.is_empty() {
        let mut hir = fold.done();
        unsafe {
            hir.finish_partial_initalize(&mut ast);
        }
        Ok(hir)
    } else {
        fold.errors
            .drain(..)
            .for_each(|err| err.print(source_map, &ast, true));
        Err(())
    }
}
