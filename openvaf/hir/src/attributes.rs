use std::sync::Arc;

use basedb::{AstIdMap, BaseDB, ErasedAstId, FileId};
use syntax::ast::{self};
use syntax::AstNode;

use crate::CompilationDB;

pub struct AstCache {
    ast: syntax::SourceFile,
    id_map: Arc<AstIdMap>,
}

impl AstCache {
    pub(crate) fn new(db: &CompilationDB, root_file: FileId) -> AstCache {
        AstCache { ast: db.parse(root_file).tree(), id_map: db.ast_id_map(root_file) }
    }
    /// Tries to resolve an attr as a string if it exists.  Emits an error to `sink`
    ///if the attribute exists but is not a string literal.
    ///
    /// # Returns
    ///
    /// The (unescaped) string literal assigned to `attribute`. If `attribute`
    /// doesn't exits or is not a string literal `None` is returned instead
    pub(crate) fn resolve_attribute(&self, attribute: &str, id: ErasedAstId) -> Option<ast::Attr> {
        let idx = self.id_map.get_attr(id, attribute)?;
        let ast = self.id_map.get_syntax(id).to_node(self.ast.syntax());
        let mut attrs = if ast::Var::can_cast(ast.kind()) || ast::Param::can_cast(ast.kind()) {
            ast::attrs(&ast.parent().unwrap())
        } else {
            ast::attrs(&ast)
        };
        attrs.nth(idx)
    }
}
