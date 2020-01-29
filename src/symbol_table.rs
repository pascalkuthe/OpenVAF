use ahash::AHashMap as HashMap;

use crate::symbol::Symbol;
use crate::Span;

use super::ast::*;

pub type SymbolTable<'ast> = HashMap<Symbol, SymbolDeclaration<'ast>>;
#[derive(Clone, Copy)]
pub enum SymbolDeclaration<'ast> {
    Module(ModuleId<'ast>),
    Block(BlockId<'ast>),
    Variable(VariableId<'ast>),
    Branch(BranchId<'ast>),
    Net(NetId<'ast>),
    Port(PortId<'ast>),
    Function(FunctionId<'ast>),
    Discipline(DisciplineId<'ast>),
    Nature,
}
impl<'ast> SymbolDeclaration<'ast> {
    pub fn span(&self, ast: &Ast<'ast>) -> Span {
        match self {
            Self::Module(id) => ast[*id].source,
            Self::Block(id) => ast[*id].source,
            Self::Variable(id) => ast[*id].source,
            Self::Net(id) => ast[*id].source,
            Self::Branch(id) => ast[*id].source,
            Self::Port(id) => ast[*id].source,
            Self::Function(id) => ast[*id].source,
            Self::Discipline(id) => ast[*id].source,
            Self::Nature => unimplemented!(), //ast[id].source,
        }
    }
}
