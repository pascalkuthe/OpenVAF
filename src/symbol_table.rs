use std::collections::HashMap;

use crate::symbol::Symbol;

use super::ast::*;

pub type SymbolTable<'ast> = HashMap<Symbol, SymbolDeclaration<'ast>>; //Todo avoid copy
#[derive(Debug, Clone)]
pub enum SymbolDeclaration<'ast> {
    Module(&'ast Module<'ast>, SymbolTable<'ast>),
    Block(&'ast SeqBlock<'ast>, SymbolTable<'ast>),
    Variable(&'ast AttributeNode<'ast, Variable<'ast>>),
    Branch(&'ast AttributeNode<'ast, BranchDeclaration<'ast>>),
    Net(&'ast AttributeNode<'ast, Net>),
    Port(&'ast AttributeNode<'ast, Port>),
    Function(&'ast Node<Function<'ast>>),
    Discipline(&'ast Node<Discipline>),
    Nature(&'ast Node<Discipline>),
}
