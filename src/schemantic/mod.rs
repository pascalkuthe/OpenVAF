use crate::ast::Visitor;
use crate::schemantic::error::Result;
use crate::schemantic::error::{Error, Type};
use crate::symbol::Ident;
use crate::symbol_table::{SymbolDeclaration, SymbolTable};
use crate::Ast;

mod error;

struct SchemanticPass<'ast, 'astref> {
    scope_stack: Vec<&'astref SymbolTable<'ast>>,
    ast: &'astref Ast<'ast>,
    errors: Vec<Error>,
}
impl<'ast, 'astref> SchemanticPass<'ast, 'astref> {
    fn resolve(&self, ident: Ident) -> Result<SymbolDeclaration<'ast>> {
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
}
impl Visitor<Error> for SchemanticPass {}
