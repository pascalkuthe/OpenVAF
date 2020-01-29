use crate::schemantic::error::Result;
use crate::schemantic::error::{Error, Type};
use crate::symbol::Ident;
use crate::symbol_table::{SymbolDeclaration, SymbolTable};
use crate::Ast;

mod error;
mod name_resolution;

struct SchemanticPass<'ast> {
    scope_stack: Vec<SymbolTable<'ast>>,
    ast: Box<Ast<'ast>>,
    errors: Vec<Error>,
}
impl<'ast> SchemanticPass<'ast> {
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
