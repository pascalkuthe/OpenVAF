//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use intrusive_collections::__core::fmt::Debug;

use crate::symbol::Ident;
use crate::Span;

//mod visitor;

#[derive(Clone, Copy, Debug)]
pub struct Node<T: Clone> {
    pub source: Span,
    pub contents: T,
}
impl<T: Clone> Node<T> {
    pub fn new(contents: T, source: Span) -> Self {
        Self { contents, source }
    }
}
pub type Attribute = ();
pub type Attributes<'ast> = &'ast [Attribute];
#[derive(Clone, Copy, Debug)]
pub struct AttributeNode<'ast, T: Clone> {
    pub attributes: Attributes<'ast>,
    pub source: Span,
    pub contents: T,
}

#[derive(Clone, Copy, Debug)]
pub enum TopNode<'ast> {
    Module(Module<'ast>),
    Nature,
}

#[derive(Clone, Copy, Debug)]
pub struct Module<'ast> {
    pub name: Ident,
    pub port_list: &'ast [AttributeNode<'ast, Port>],
    //parameter_list: SliceId<Parameter>,TODO Parameter List
    pub children: &'ast [AttributeNode<'ast, ModuleItem<'ast>>],
}
#[derive(Clone, Copy, Debug)]
pub struct Port {
    pub name: Ident,
    pub input: bool,
    pub output: bool,
    pub discipline: Ident, //TODO discipline
    pub signed: bool,
    pub net_type: NetType,
}

impl Default for Port {
    fn default() -> Self {
        Self {
            name: Ident::empty(),
            input: false,
            output: false,
            discipline: Ident::empty(),
            signed: false,
            net_type: NetType::UNDECLARED,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Branch<'ast> {
    Port(HierarchicalId<'ast>),
    Nets(HierarchicalId<'ast>, HierarchicalId<'ast>),
}
#[derive(Debug, Clone, Copy)]
pub struct BranchDeclaration<'ast> {
    pub name: Ident,
    pub branch: Branch<'ast>,
}

#[derive(Debug, Clone, Copy)]
pub enum ModuleItem<'ast> {
    AnalogStmt(Node<Statement<'ast>>),
    BranchDecl(BranchDeclaration<'ast>),
    NetDecl(Net),
    VariableDecl(Variable<'ast>),
    ParameterDecl,
}
#[derive(Clone, Copy, Debug)]
pub struct Discipline {
    pub name: Ident,
}
#[derive(Clone, Copy, Debug)]
pub struct Function<'ast> {
    pub name: Ident,
    pub args: &'ast [Ident],
    pub body: Node<Statement<'ast>>,
}
#[derive(Debug, Clone, Copy)]
pub struct Net {
    pub name: Ident,
    pub discipline: Ident,
    pub signed: bool,
    pub net_type: NetType,
}
#[derive(Debug, Clone, Copy)]
pub struct Variable<'ast> {
    pub name: Ident,
    pub variable_type: VariableType,
    pub default_value: Option<&'ast Node<Expression<'ast>>>, //a reference because this used rarely in practice
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NetType {
    UNDECLARED,
    REG,
    WREAL,
    SUPPLY0,
    SUPPLY1,
    TRI,
    TRIAND,
    TRIOR,
    TRI0,
    TRI1,
    WIRE,
    UWIRE,
    WAND,
    WOR,
}

#[derive(Clone, Copy, Debug)]
pub enum Statement<'ast> {
    Block(&'ast SeqBlock<'ast>),
    Condition(Condition<'ast>),
    Contribute(Ident, BranchAccess<'ast>, Node<Expression<'ast>>),
    //  TODO IndirectContribute(),
    Assign(HierarchicalId<'ast>, Node<Expression<'ast>>),
    FunctionCall(HierarchicalId<'ast>, &'ast [Node<Expression<'ast>>]),
}
#[derive(Clone, Copy, Debug)]
pub struct SeqBlock<'ast> {
    pub scope: Option<BlockScope<'ast>>,
    pub statements: &'ast [Node<Statement<'ast>>],
    //    parameters:Parameters, TODO parameters
}
#[derive(Clone, Copy, Debug)]
pub struct BlockScope<'ast> {
    pub name: Ident,
    pub variables: &'ast [AttributeNode<'ast, Variable<'ast>>],
}

#[derive(Clone, Copy, Debug)]
pub struct Condition<'ast> {
    pub main_condition: Node<Expression<'ast>>,
    pub main_condition_statement: &'ast Node<Statement<'ast>>,
    pub else_ifs: &'ast [(Node<Expression<'ast>>, Node<Statement<'ast>>)],
    pub else_statement: Option<&'ast Node<Statement<'ast>>>,
}

#[derive(Clone, Copy, Debug)]
pub enum Expression<'ast> {
    BinaryOperator(
        &'ast Node<Expression<'ast>>,
        Node<BinaryOperator>,
        &'ast Node<Expression<'ast>>,
    ),
    UnaryOperator(Node<UnaryOperator>, &'ast Node<Expression<'ast>>),
    Primary(Primary<'ast>),
}

#[derive(Clone, Copy, Debug)]
pub enum BranchAccess<'ast> {
    Explicit(HierarchicalId<'ast>),
    Implicit(Branch<'ast>),
}

#[derive(Clone, Copy, Debug)]
pub enum Primary<'ast> {
    Integer(i64),
    UnsignedInteger(u32),
    Real(f64),
    VariableOrNetReference(HierarchicalId<'ast>),
    FunctionCall(HierarchicalId<'ast>, &'ast [Node<Expression<'ast>>]),
    BranchAccess(Ident, BranchAccess<'ast>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    Condition,
    Either,
    Sum,
    Subtract,
    Multiply,
    Divide,
    Exponent,
    Modulus,

    ShiftLeft,
    ShiftRight,

    LessThen,
    LessEqual,
    GreaterThen,
    GreaterEqual,
    LogicEqual,
    LogicalNotEqual,

    LogicOr,
    LogicAnd,

    Xor,
    NXor,
    And,
    Or,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
    BitNegate,
    LogicNegate,
    ArithmeticNegate,
    ExplicitPositive,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VariableType {
    TIME,
    INTEGER,
    REAL,
    REALTIME,
}
#[derive(Clone, Copy, Debug)]
pub struct HierarchicalId<'ast> {
    pub names: &'ast [Ident],
}
impl<'ast> From<bumpalo::collections::Vec<'ast, Ident>> for HierarchicalId<'ast> {
    fn from(vec: bumpalo::collections::Vec<'ast, Ident>) -> Self {
        Self {
            names: vec.into_bump_slice(),
        }
    }
}
