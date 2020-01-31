use crate::ast::Visitor;
use crate::ir::ast::Ast;
use crate::ir::hir::Hir;
use crate::schemantic::error::Result;
use crate::schemantic::error::{Error, Type};
use crate::symbol::Ident;
use crate::symbol_table::{SymbolDeclaration, SymbolTable};

mod error;

pub struct SchemanticPass<'tag, 'astref> {
    scope_stack: Vec<&'astref SymbolTable<'tag>>,
    ast: &'astref mut Ast<'tag>,
    hir: Box<Hir<'tag>>,
    errors: Vec<Error>,
}
impl<'tag, 'astref> SchemanticPass<'tag, 'astref> {
    fn resolve(&self, ident: Ident) -> Result<SymbolDeclaration<'tag>> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(res) = scope.get(&ident.name) {
                return Ok(*res);
            }
        }
        return Err(Error {
            error_type: Type::NotFound,
            source: ident.span,
        });
    }
    pub unsafe fn init(ast: &'tag mut Ast<'tag>) -> Self {
        Self {
            scope_stack: vec![&ast.top_symbols],
            ast,
            hir: Hir::partial_initalize(ast),
            errors: Vec::with_capacity(64),
        }
    }
}
impl Visitor<Error> for SchemanticPass {}
