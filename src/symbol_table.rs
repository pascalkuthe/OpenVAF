use std::collections::HashMap;

use crate::symbol::Symbol;
use crate::Span;

use super::ast::*;

pub type SymbolTable<'ast> = HashMap<Symbol, SymbolDeclaration<'ast>>; //Todo avoid copy
#[derive(Debug, Clone)]
pub enum SymbolDeclaration<'ast> {
    Module(&'ast Module<'ast>, SymbolTable<'ast>, Span),
    Block(&'ast SeqBlock<'ast>, SymbolTable<'ast>, Span),
    Variable(&'ast AttributeNode<'ast, Variable<'ast>>),
    Branch(&'ast AttributeNode<'ast, BranchDeclaration<'ast>>),
    Net(&'ast AttributeNode<'ast, Net>),
    Port(&'ast AttributeNode<'ast, Port>),
    Function(&'ast AttributeNode<'ast, Function<'ast>>),
    Discipline(&'ast AttributeNode<'ast, Discipline>),
    Nature(&'ast AttributeNode<'ast, Discipline>),
}
impl<'ast> SymbolDeclaration<'ast> {
    pub fn span(&self) -> Span {
        match self {
            Self::Module(_, _, span)
            | Self::Block(_, _, span)
            | Self::Variable(AttributeNode { source: span, .. })
            | Self::Net(AttributeNode { source: span, .. })
            | Self::Branch(AttributeNode { source: span, .. })
            | Self::Port(AttributeNode { source: span, .. })
            | Self::Function(AttributeNode { source: span, .. })
            | Self::Discipline(AttributeNode { source: span, .. })
            | Self::Nature(AttributeNode { source: span, .. }) => *span,
        }
    }
}
