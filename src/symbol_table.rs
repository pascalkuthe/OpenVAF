use std::collections::HashMap;

use sr_alloc::{NodeId, StrId};

use super::ast::*;

pub type SymbolTable = HashMap<&'static str, SymbolDeclaration>; //Todo avoid copy
pub enum SymbolDeclaration {
    Module(NodeId<Module>, SymbolTable),
    Block(NodeId<SeqBlock>, SymbolTable),
    Variable(NodeId<Variable>),
    Net(NodeId<Variable>),
    Port(NodeId<Port>),
    Function(NodeId<Function>),
    Discipline(NodeId<Discipline>),
}
pub struct UnresolvedAst {
    ast: Ast,
    root_symbol_table: SymbolTable,
}
impl UnresolvedAst {
    pub fn new(ast: Ast, root_symbol_table: SymbolTable) -> Self {
        Self {
            ast,
            root_symbol_table,
        }
    }
    /// Should be called when all identifier have been resolved.
    /// This drops the symbol table and returns the (resolved) Ast
    pub fn to_resolved(self) -> Ast {
        self.ast
    }
}
