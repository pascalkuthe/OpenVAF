//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use sr_alloc::{Allocator, Immutable, NodeId, SliceId, StrId};

use crate::Span;

/// This is an Ast. Once created is it completely immutable
pub struct Ast {
    pub data: Immutable,
    top_nodes: SliceId<AttributeNode<TopNode>>,
}
impl Ast {
    pub fn new(data: Allocator, top_nodes: SliceId<AttributeNode<TopNode>>) -> Self {
        Self {
            data: Immutable::new(data),
            top_nodes,
        }
    }
    pub fn top_nodes(&self) -> &[AttributeNode<TopNode>] {
        &self.data.get_slice(self.top_nodes)
    }
}
pub type AstNodeId<T> = NodeId<Node<T>>;
pub type AstAttributeNodeId<T> = NodeId<AttributeNode<T>>;
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
pub type Attributes = SliceId<Attribute>;
#[derive(Clone, Copy, Debug)]
pub struct AttributeNode<T: Clone> {
    pub attributes: Attributes,
    pub contents: Node<T>,
}
impl<T: Clone> AttributeNode<T> {
    pub fn new(source: Span, attributes: Attributes, contents: T) -> Self {
        Self {
            attributes,
            contents: Node { source, contents },
        }
    }
}
#[derive(Clone, Copy, Debug)]
pub enum TopNode {
    Module(Module),
    Nature,
}

#[derive(Clone, Copy, Debug)]
pub struct Module {
    pub name: StrId,
    pub port_list: SliceId<AttributeNode<Port>>,
    //parameter_list: SliceId<Parameter>,TODO Parameter List
    pub children: SliceId<AttributeNode<ModuleItem>>,
}
#[derive(Clone, Copy, Debug)]
pub struct Port {
    pub name: StrId,
    pub input: bool,
    pub output: bool,
    pub discipline: Discipline, //TODO discipline
    pub signed: bool,
    pub net_type: NetType,
}

impl Default for Port {
    fn default() -> Self {
        Self {
            name: StrId::dangling(),
            input: false,
            output: false,
            discipline: None,
            signed: false,
            net_type: NetType::UNDECLARED,
        }
    }
}
#[derive(Debug, Clone, Copy)]
pub enum Branch {
    Port(Reference<Port>),
    Nets(Reference<Net>, Reference<Net>),
}
#[derive(Debug, Clone, Copy)]
pub struct BranchDeclaration {
    pub name: StrId,
    pub branch: Branch,
}

#[derive(Debug, Clone, Copy)]
pub enum ModuleItem {
    AnalogStmt,
    BranchDecl(BranchDeclaration),
    NetDecl(Net),
    VariableDecl(Variable),
    ParameterDecl,
}
#[derive(Clone, Copy, Debug)]
pub struct Reference<T: Clone> {
    pub name: StrId,
    pub declaration: Option<AstNodeId<T>>,
}
pub type Discipline = Option<Reference<()>>;
#[derive(Debug, Clone, Copy)]
pub struct Net {
    pub name: StrId,
    pub discipline: Discipline, //TODO discipline
    pub signed: bool,
    pub net_type: NetType,
    //TODO defaut value
}
#[derive(Debug, Clone, Copy)]
pub struct Variable {
    pub name: StrId,
    //TODO defaut value
    pub variable_type: VariableType,
    pub default_value: Option<Node<Expression>>,
}
impl<T: Clone> Reference<T> {
    pub fn new(name: StrId) -> Self {
        Self {
            name,
            declaration: None,
        }
    }
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
pub enum Statement {
    Block(SeqBlock),
    Condition(Condition),
    Contribute(NatureAccess, BranchAccess, Node<Expression>),
    //  TODO IndirectContribute(),
    Assign(Reference<Variable>, Node<Expression>),
    FunctionCall(Reference<Variable>, SliceId<Node<Expression>>),
}
#[derive(Clone, Copy, Debug)]
pub struct SeqBlock {
    name: Option<StrId>,
    variables: SliceId<Variable>,
    statements: SliceId<Statement>, //    parameters:Parameters, TODO parameters
}

#[derive(Clone, Copy, Debug)]
pub struct Condition {
    main_condition: Node<Expression>,
    main_condition_statement: AstNodeId<Statement>,
    else_ifs: SliceId<(Node<Expression>, Node<Statement>)>,
    else_statement: Option<AstNodeId<Statement>>,
}

#[derive(Clone, Copy, Debug, EnumAsInner)]
pub enum Expression {
    BinaryOperator(
        AstNodeId<Expression>,
        Node<BinaryOperator>,
        AstNodeId<Expression>,
    ),
    UnaryOperator(Node<UnaryOperator>, AstNodeId<Expression>),
    Primary(Primary),
}

#[derive(Clone, Copy, Debug)]
pub enum BranchAccess {
    Explicit(Reference<BranchDeclaration>),
    Implicit(Branch),
}
#[derive(Clone, Copy, Debug, EnumAsInner)]
pub enum Primary {
    Integer(i64),
    UnsignedInteger(u32),
    Real(f64),
    NetReference(Reference<Net>),
    VariableReference(Reference<Variable>),
    FunctionCall(Reference<Variable>, SliceId<Node<Expression>>),
    BranchAccess(NatureAccess, BranchAccess),
    ImplictBranch(NatureAccess, Branch),
}
#[derive(Clone, Copy, Debug)]
pub enum NatureAccess {
    Potential,
    Flow,
    Unresolved(StrId),
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
