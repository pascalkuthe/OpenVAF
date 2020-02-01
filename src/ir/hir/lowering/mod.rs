use crate::ast::visitor::*;
use crate::ast::{HierarchicalId, TopNode, Visitor};
use crate::compact_arena::{Idx, Idx16};
use crate::ir::ast::{Ast, AttributeNode};
use crate::ir::hir::lowering::error::Type::NotAScope;
use crate::ir::hir::lowering::error::{Error, Type};
use crate::ir::hir::{Hir, Module};
use crate::ir::{ModuleId, StatementId};
use crate::symbol::Ident;
use crate::symbol_table::{SymbolDeclaration, SymbolTable};
use crate::util::SafeRange;

mod error;
type Result<T = ()> = std::result::Result<T, ()>;
struct AstToHirFolder<'tag, 'astref> {
    scope_stack: Vec<&'astref SymbolTable<'tag>>,
    ast: &'astref Ast<'tag>,
    hir: Box<Hir<'tag>>,
    errors: Vec<Error>,
    last_resolved_declaration: Result<SymbolDeclaration<'tag>>,
}
impl<'tag, 'astref> AstToHirFolder<'tag, 'astref> {
    fn resolve(&self, ident: &Ident) -> error::Result<SymbolDeclaration<'tag>> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(res) = scope.get(&ident.name) {
                return Ok(*res);
            }
        }
        Err(Error {
            error_type: Type::NotFound,
            source: ident.span,
        })
    }
    unsafe fn init(ast: &'astref mut Ast<'tag>) -> Self {
        Self {
            hir: Hir::partial_initalize(ast),
            ast,
            scope_stack: vec![&ast.top_symbols],
            errors: Vec::with_capacity(64),
            last_resolved_declaration: Err(()), //we initalize this as an error because that is the default value for this and we only change this when we have a positive hit
        }
    }
    fn run(&mut self) {
        for top_node in self.ast.top_nodes.iter() {
            match top_node {
                TopNode::Module(module) => {
                    self.visit_module(*module, self.ast);
                }
                TopNode::Nature | TopNode::Discipline => unimplemented!(),
            }
        }
    }
    fn done(self) -> Box<Hir<'tag>> {
        self.hir
    }
    fn take_resolved_declaration(&mut self) -> Result<SymbolDeclaration<'tag>> {
        std::mem::replace(&mut self.last_resolved_declaration, Err(()))
    }
}
/// The Result in Visitor is there to allow error propagation.
/// However name resolution doesn't depend on the result of other name resolutions.
/// Therefore all errors are simply to the errors vector (size > 0 indicates a failure) and Ok(()) is always returned.
/// If a result of name reolution is required by a caller we use struct field last_resolved_declaration instead
impl<'tag, 'astref> Visitor<'tag> for AstToHirFolder<'tag, 'astref> {
    fn visit_reference(&mut self, ident: &Ident, ast: &Ast<'tag>) -> Result {
        match self.resolve(ident) {
            Err(error) => self.errors.push(error),
            Ok(res) => self.last_resolved_declaration = Ok(res),
        }
        Ok(())
    }
    fn visit_hierarchical_reference(
        &mut self,
        hieraichal_ident: &HierarchicalId,
        ast: &Ast<'tag>,
    ) -> Result {
        //this needs to ret
        let mut iter = hieraichal_ident.names.iter();
        let ident = iter.next().unwrap();
        let mut last_span = ident.span;
        let mut current = match self.resolve(ident) {
            Err(error) => {
                self.errors.push(error);
                return Ok(());
            }
            Ok(res) => res,
        };
        for ident in iter {
            let symbol_table = match current {
                SymbolDeclaration::Module(module) => &ast[module].contents.symbol_table,
                SymbolDeclaration::Block(block_id) => {
                    if let Some(scope) = &ast[block_id].contents.scope {
                        &scope.symbols
                    } else {
                        self.errors.push(Error {
                            error_type: NotAScope {
                                declaration: ast[block_id].source,
                            },
                            source: last_span,
                        });
                        return Ok(());
                    }
                }
                found => {
                    self.errors.push(Error {
                        error_type: NotAScope {
                            declaration: found.span(ast),
                        },
                        source: last_span,
                    });
                    return Ok(());
                }
            };
            if let Some(found) = symbol_table.get(&ident.name) {
                current = *found;
                last_span = ident.span
            } else {
                self.errors.push(Error {
                    error_type: Type::NotFound,
                    source: ident.span,
                });
                return Ok(());
            }
        }
        self.last_resolved_declaration = Ok(current);
        Ok(())
    }
    fn visit_module(&mut self, module_id: ModuleId<'tag>, ast: &Ast<'tag>) -> Result {
        let module = &ast[module_id];
        let fist_analog_statement = StatementId(unsafe { self.hir.statements.next_id() });
        walk_module(self, module, ast)?;
        self.hir[module_id] = AttributeNode {
            contents: Module {
                name: module.contents.name,
                port_list: module.contents.port_list,
                analog: SafeRange {
                    start: fist_analog_statement,
                    end: StatementId(unsafe { self.hir.statements.next_id() }),
                },
            },
            source: module.source,
            attributes: module.attributes,
        };
        Ok(())
    }
}

pub fn resolve(mut ast: Ast) -> Box<Hir> {
    let mut fold = unsafe { AstToHirFolder::init(&mut ast) }; //this is save since the only thing that made this unsafe way calling only a part of this and expecting a valid hir
    fold.run();
    let mut hir = fold.done();
    std::mem::swap(&mut ast.top_nodes, &mut hir.top_nodes);
    hir
}
