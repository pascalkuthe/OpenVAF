use std::collections::HashMap;

use crate::Span;
use crate::symbol::Symbol;

use super::ast::*;

pub type SymbolTable = HashMap<Symbol, SymbolDeclaration>; //Todo avoid copy
#[derive(Debug, Clone)]
pub enum SymbolDeclaration {
    Module(& Module, SymbolTable, Span),
    Block(& SeqBlock, SymbolTable, Span),
    Variable(& AttributeNode<, Variable>),
    Branch(& AttributeNode<, BranchDeclaration>),
    Net(& AttributeNode<, Net>),
    Port(& AttributeNode<, Port>),
    Function(& AttributeNode<, Function>),
    Discipline(& AttributeNode<, Discipline>),
    Nature(& AttributeNode<, Discipline>),
}
impl SymbolDeclaration {
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
