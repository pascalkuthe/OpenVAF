//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use core::ops::Index;
use std::ops::Range;

use compact_arena::{Idx16, TinyArena};
use intrusive_collections::__core::fmt::Debug;

use crate::symbol::Ident;
use crate::symbol_table::SymbolTable;
use crate::{FrozenBox, Span};

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
pub struct Ast {
    //Declarations
    //    parameters: Vec<Parameter>,
    //    nature: Vec<Nature>
    branches: Vec<AttributeNode<BranchDeclaration>>,
    nets: Vec<AttributeNode<Net>>,
    ports: Vec<AttributeNode<Port>>,
    variables: Vec<AttributeNode<Variable>>,
    modules: Vec<AttributeNode<Module>>,
    functions: Vec<AttributeNode<Function>>,
    disciplines: Vec<AttributeNode<Discipline>>,
    //Ast Items
    expressions: Vec<Node<Expression>>,
    blocks: Vec<SeqBlock>,
    attributes: Vec<Attribute>,
}
impl Ast {
    //TODO allow configuration; Values currently based on HICUM plus quite a bit of leanyway rounded (up) to powers of two
    //These values are here to allow preallocation of data for higher performance makes a lot of sense since compact models have less size variations than general purpose programming languages
    const PARAMETER_DEFAULT_CAPACITY: usize = 128;
    const NATURE_DEFAULT_CAPACITY: usize = 16;
    const NETS_DEFAULT_CAPACITY: usize = 32;
    const BRANCHES_DEFAULT_CAPACITY: usize = 64;
    const PORTS_DEFAULT_CAPACITY: usize = 32;
    const VARIABLES_DEFAULT_CAPACITY: usize = 512;
    const MODULES_DEFAULT_CAPACITY: usize = 8;
    const FUNCTIONS_DEFAULT_CAPACITY: usize = 16;
    const DISCIPLINE_DEFAULT_CAPACITY: usize = 16;
    const EXPRESSION_DEFAULT_CAPACITY: usize = 1024;
    const BLOCK_DEFAULT_CAPACITY: usize = 32;
    const ATTRIBUTE_DEFAULT_CAPACITY: usize = 512;

    pub fn new() -> Self {
        Self {
            branches: Vec::with_capacity(Self::BRANCHES_DEFAULT_CAPACITY),
            nets: Vec::with_capacity(Self::NETS_DEFAULT_CAPACITY),
            ports: Vec::with_capacity(Self::PORTS_DEFAULT_CAPACITY),
            variables: Vec::with_capacity(Self::VARIABLES_DEFAULT_CAPACITY),
            modules: Vec::with_capacity(Self::MODULES_DEFAULT_CAPACITY),
            functions: Vec::with_capacity(Self::FUNCTIONS_DEFAULT_CAPACITY),
            disciplines: Vec::with_capacity(Self::DISCIPLINE_DEFAULT_CAPACITY),
            expressions: Vec::with_capacity(Self::EXPRESSION_DEFAULT_CAPACITY),
            blocks: Vec::with_capacity(Self::BLOCK_DEFAULT_CAPACITY),
            attributes: Vec::with_capacity(Self::ATTRIBUTE_DEFAULT_CAPACITY),
        }
    }
}
macro_rules! impl_id_type {
    ($name:ident($id_type:ident): $type:ident in $container:ident::$sub_container:ident) => {
        impl Index<$name> for $container {
            type Output = $type;
            fn index(&self, index: $name) -> &Self::Output {
                self.$sub_container[index.0]
            }
        }
        impl Index<Range<$name>> for $container {
            type Output = [$type];
            fn index(&self, range: Range<$name>) -> &Self::Output {
                self.$sub_container[range.start.0..range.end.0]
            }
        }
        impl Push for $container {
            type Value = $type;
            type Key = $name;
            fn push(& mut self;val: Self::Value) -> &Self::Key {
                self.$sub_container.push(val);
                &name(self.$sub_container.len() as $id_type -1)
            }
        }
    }
}
pub trait Push {
    type Value;
    type Key;
    fn push(&mut self, key: Self::Value) -> Self::Key;
}

//TODO cfg options for different id sizes

#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct BranchId(u8);
impl_id_type!(BranchId(u8): AttributeNode<Branch> in Ast::branches);
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct NetId(u8);
impl_id_type!(NetId(u8): AttributeNode<Net> in Ast::nets);
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct PortId(u8);
impl_id_type!(PortId(u8): AttributeNode<Port> in Ast::ports);
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct VariableId(u16);
impl_id_type!(VariableId(u16): AttributeNode<Variable> in Ast::variables);
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct ModuleId(u8);
impl_id_type!(ModuleId(u8): AttributeNode<Module> in Ast::modules);
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct FunctionId(u8);
impl_id_type!(FunctionId(u8): AttributeNode<Function> in Ast::functions);
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct DisciplineId(u8);
impl_id_type!(DisciplineId(u8): AttributeNode<Discipline> in Ast::disciplines);
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct ExpressionId(u16);
impl_id_type!(ExpressionId(u16): Node<Expression> in Ast::expressions);
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct BlockId(u8);
impl_id_type!(BlockId(u8): SeqBlock in Ast::blocks);
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct AttributeId(u16);
impl_id_type!(AttributeId(u16): Attribute in Ast::attributes);

pub type AstPtr<T> = FrozenBox<T>;
pub type Attribute = ();
#[derive(Clone, Copy, Debug)]
pub struct AttributeNode<T: Clone> {
    pub attributes: Range<AttributeId>,
    pub source: Span,
    pub contents: T,
}

#[derive(Clone, Copy, Debug)]
pub enum TopNode {
    Module(Module),
    Nature,
}

#[derive(Clone, Copy, Debug)]
pub struct Module {
    pub name: Ident,
    pub port_list: Range<PortId>,
    pub symbol_table: SymbolTable,
    pub children: Vec<ModuleItem>,
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
pub struct BranchDeclaration {
    pub name: Ident,
    pub branch: Branch,
}

#[derive(Debug, Clone, Copy)]
pub enum Branch {
    Port(PortId),
    Nets(NetId, NetId),
}

#[derive(Debug, Clone, Copy)]
pub enum ModuleItem {
    AnalogStmt(Statement),
    BranchDecl(BranchId),
    NetDecl(NetId),
    VariableDecl(VariableId),
    //ParameterDecl,
}
#[derive(Clone, Copy, Debug)]
pub struct Discipline {
    pub name: Ident,
}
#[derive(Clone, Copy, Debug)]
pub struct Function {
    pub name: Ident,
    pub args: Vec<Ident>,
    pub body: Statement,
}
#[derive(Debug, Clone, Copy)]
pub struct Net {
    pub name: Ident,
    pub discipline: Ident,
    pub signed: bool,
    pub net_type: NetType,
}
#[derive(Debug, Clone, Copy)]
pub struct Variable {
    pub name: Ident,
    pub variable_type: VariableType,
    pub default_value: Option<ExpressionId>,
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
    Block(BlockId),
    Condition(Condition),
    Contribute(Ident, BranchAccess, Node<Expression>),
    //  TODO IndirectContribute(),
    Assign(HierarchicalId, Node<Expression>),
    FunctionCall(HierarchicalId, Vec<ExpressionId>),
}
#[derive(Clone, Copy, Debug)]
pub struct SeqBlock {
    pub scope: Option<BlockScope>,
    pub statements: Vec<Statement>, //TODO statement id
                                    //    parameters:Parameters, TODO parameters
}
#[derive(Clone, Copy, Debug)]
pub struct BlockScope {
    pub name: Ident,
    pub symbols: SymbolTable,
}

#[derive(Clone, Copy, Debug)]
pub struct Condition {
    pub main_condition: Node<Expression>,
    pub main_condition_statement: Node<Statement>,
    pub else_ifs: Vec<(ExpressionId, Node<Statement>)>, //TODO statement id
    pub else_statement: Option<Node<Statement>>,
}

#[derive(Clone, Copy, Debug)]
pub enum Expression {
    BinaryOperator(ExpressionId, Node<BinaryOperator>, ExpressionId),
    UnaryOperator(Node<UnaryOperator>, ExpressionId),
    Primary(Primary),
}

#[derive(Clone, Copy, Debug)]
pub enum BranchAccess {
    Explicit(HierarchicalId),
    Implicit(Branch),
}

#[derive(Clone, Copy, Debug)]
pub enum Primary {
    Integer(i64),
    UnsignedInteger(u32),
    Real(f64),
    VariableOrNetReference(HierarchicalId),
    FunctionCall(HierarchicalId, ExpressionId),
    BranchAccess(Ident, BranchAccess),
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
pub struct HierarchicalId {
    pub names: Vec<Ident>,
}
