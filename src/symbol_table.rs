use ahash::AHashMap as HashMap;

use crate::ast_lowering::error::MockSymbolDeclaration;
use crate::ir::{
    BlockId, BranchId, DisciplineId, FunctionId, ModuleId, NatureId, NetId, ParameterId, PortId,
    VariableId,
};
use crate::symbol::{Symbol, SymbolStr};
use crate::Span;

use super::ast::*;

pub type SymbolTable<'ast> = HashMap<Symbol, SymbolDeclaration<'ast>>;
#[derive(Clone, Copy, Debug)]
pub enum SymbolDeclaration<'ast> {
    Module(ModuleId<'ast>),
    Block(BlockId<'ast>),
    Variable(VariableId<'ast>),
    Branch(BranchId<'ast>),
    Net(NetId<'ast>),
    Port(PortId<'ast>),
    Function(FunctionId<'ast>),
    Discipline(DisciplineId<'ast>),
    Nature(NatureId<'ast>),
    Parameter(ParameterId<'ast>),
}
impl<'ast> SymbolDeclaration<'ast> {
    pub fn span(self, ast: &Ast<'ast>) -> Span {
        match self {
            Self::Module(id) => ast[id].source,
            Self::Block(id) => ast[id].source,
            Self::Variable(id) => ast[id].source,
            Self::Net(id) => ast[id].source,
            Self::Branch(id) => ast[id].source,
            Self::Port(id) => ast[id].source,
            Self::Function(id) => ast[id].source,
            Self::Discipline(id) => ast[id].source,
            Self::Nature(id) => ast[id].source,
            Self::Parameter(id) => ast[id].source,
        }
    }
    pub fn name<'lt>(self, ast: &'lt Ast<'ast>) -> SymbolStr {
        match self {
            Self::Module(id) => ast[id].contents.name.name.as_str(),
            Self::Block(id) => ast[id].contents.scope.as_ref().unwrap().name.name.as_str(),
            Self::Variable(id) => ast[id].contents.name.name.as_str(),
            Self::Net(id) => ast[id].contents.name.name.as_str(),
            Self::Branch(id) => ast[id].contents.name.name.as_str(),
            Self::Port(id) => ast[id].contents.name.name.as_str(),
            Self::Function(id) => ast[id].contents.name.name.as_str(),
            Self::Discipline(id) => ast[id].contents.name.name.as_str(),
            Self::Nature(id) => ast[id].contents.name.name.as_str(),
            Self::Parameter(id) => ast[id].contents.name.name.as_str(),
        }
    }
    pub fn mock(self) -> MockSymbolDeclaration {
        match self {
            Self::Module(_) => MockSymbolDeclaration::Module,
            Self::Block(_) => MockSymbolDeclaration::Block,
            Self::Variable(_) => MockSymbolDeclaration::Variable,
            Self::Net(_) => MockSymbolDeclaration::Net,
            Self::Branch(_) => MockSymbolDeclaration::Branch,
            Self::Port(_) => MockSymbolDeclaration::Port,
            Self::Function(_) => MockSymbolDeclaration::Function,
            Self::Discipline(_) => MockSymbolDeclaration::Discipline,
            Self::Nature(_) => MockSymbolDeclaration::Nature,
            Self::Parameter(_) => MockSymbolDeclaration::Parameter,
        }
    }
}
