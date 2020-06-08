//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

#![allow(clippy::wildcard_imports)]
use crate::ir::*;

use core::fmt::Debug;
use std::ops::Range;
use crate::symbol::Ident;
use crate::symbol_table::SymbolTable;
use crate::Span;
use index_vec::IndexVec;
use crate::literals::StringLiteral;
use crate::ir::ids::IdRange;

// pub use visitor::Visitor;

/*The FOLLOWING MACRO is adapted from https://github.com/llogiq/compact_arena (mk_tiny_arena!) under MIT-License:

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
    ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
    TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
    PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
    SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
    IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    DEALINGS IN THE SOFTWARE.
*/

//pub mod printer;
// pub mod visitor;

/// An Ast representing a parsed Verilog-AMS project (root file);
/// It provides stable indicies for every Node because the entire Tree is immutable once created;
/// It uses preallocated constant size arrays for performance

//TODO configure to use different arena sizes
pub struct Ast {
    //Declarations
    pub branches: IndexVec<BranchId, AttributeNode<BranchDeclaration>>,
    pub nets: IndexVec<NetId, AttributeNode<Net>>,
    pub ports: IndexVec<PortId, AttributeNode<Port>>,
    pub variables: IndexVec<VariableId, AttributeNode<Variable>>,
    pub parameters: IndexVec<ParameterId, AttributeNode<Parameter>>,
    pub modules: IndexVec<ModuleId, AttributeNode<Module>>,
    pub functions: IndexVec<FunctionId, AttributeNode<Function>>,
    pub disciplines: IndexVec<DisciplineId, AttributeNode<Discipline>>,
    pub natures: IndexVec<NatureId, AttributeNode<Nature>>,
    //Ast Items
    pub expressions: IndexVec<ExpressionId, Node<Expression>>,
    pub blocks: IndexVec<BlockId, AttributeNode<SeqBlock>>,
    pub attributes: IndexVec<AttributeId, Attribute>,
    pub statements: IndexVec<StatementId, Statement>,
    pub top_symbols: SymbolTable,
}

impl Ast {
    #[must_use]
    pub fn new() -> Self {
        // Large initial capacity to avoid reallocation
        // TODO configure this smh
        Self {
            branches: IndexVec::with_capacity(32),
            nets: IndexVec::with_capacity(32),
            ports: IndexVec::with_capacity(32),
            variables: IndexVec::with_capacity(512),
            parameters: IndexVec::with_capacity(512),
            modules: IndexVec::with_capacity(1),
            functions: IndexVec::with_capacity(16),
            disciplines: IndexVec::with_capacity(8),
            natures: IndexVec::with_capacity(8),
            expressions: IndexVec::with_capacity(u16::MAX as usize),
            blocks: IndexVec::with_capacity(32),
            attributes: IndexVec::with_capacity(128),
            statements: IndexVec::with_capacity(4096),
            top_symbols: SymbolTable::with_capacity_and_hasher(32, Default::default()),
        }
    }
}

impl_id_type!(BranchId in Ast::branches -> AttributeNode<BranchDeclaration>);

impl_id_type!(NetId in Ast::nets -> AttributeNode<Net>);

impl_id_type!(PortId in Ast::ports -> AttributeNode<Port>);

impl_id_type!(ParameterId in Ast::parameters -> AttributeNode<Parameter>);

impl_id_type!(VariableId in Ast::variables -> AttributeNode<Variable>);

impl_id_type!(ModuleId in Ast::modules -> AttributeNode<Module>);

impl_id_type!(FunctionId in Ast::functions -> AttributeNode<Function>);

impl_id_type!(DisciplineId in Ast::disciplines -> AttributeNode<Discipline>);

impl_id_type!(ExpressionId in Ast::expressions -> Node<Expression>);

impl_id_type!(AttributeId in Ast::attributes -> Attribute);

impl_id_type!(StatementId in Ast::statements -> Statement);

impl_id_type!(BlockId in Ast::blocks -> AttributeNode<SeqBlock>);

impl_id_type!(NatureId in Ast::natures -> AttributeNode<Nature>);

#[derive(Clone, Debug)]
pub enum TopNode {
    Module(ModuleId),
    Nature(NatureId),
    Discipline(DisciplineId),
}

#[derive(Copy, Clone)]
pub struct Nature {
    pub name: Ident,
    pub abstol: ExpressionId,
    pub units: ExpressionId,
    pub access: Ident,
    pub idt_nature: Option<Ident>,
    pub ddt_nature: Option<Ident>,
}

pub enum NatureParentType {
    Nature,
    DisciplineFlow,
    DisciplinePotential,
}

#[derive(Clone)]
pub struct Module {
    pub name: Ident,
    pub port_list: IdRange<PortId>,
    pub branch_list: IdRange<BranchId>,
    pub symbol_table: SymbolTable,
    pub children: Vec<ModuleItem>,
}

#[derive(Clone, Debug)]
pub struct Parameter {
    pub name: Ident,
    pub parameter_type: ParameterType,
    pub default_value: ExpressionId,
}

#[derive(Clone, Debug)]
pub enum ParameterType {
    Numerical {
        parameter_type: VariableType,
        from_ranges: Vec<Range<NumericalParameterRangeBound<ExpressionId>>>,
        excluded: Vec<NumericalParameterRangeExclude<ExpressionId>>,
    },
    String(
        //TODO string parameter from/exlude
    ),
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

#[derive(Clone, Debug)]
pub struct BranchDeclaration {
    pub name: Ident,
    pub branch: Branch,
}

#[derive(Clone, Debug)]
pub enum Branch {
    Port(HierarchicalId),
    NetToGround(HierarchicalId),
    Nets(HierarchicalId, HierarchicalId),
}

#[derive(Clone, Copy, Debug)]
pub enum ModuleItem {
    AnalogStmt(StatementId),
    GenerateStatement, //TODO
}

#[derive(Clone, Copy, Debug)]
pub struct Discipline {
    pub name: Ident,
    pub flow_nature: Option<Ident>,
    pub potential_nature: Option<Ident>,
    pub continuous: Option<bool>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Ident,
    pub args: Vec<FunctionArg>,
    pub declarations: SymbolTable,
    pub return_variable: VariableId,
    pub body: StatementId,
}

#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub struct FunctionArg {
    pub name: Ident,
    pub input: bool,
    pub output: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct Net {
    pub name: Ident,
    pub discipline: Ident,
    pub signed: bool,
    pub net_type: NetType,
}

#[derive(Clone, Copy, Debug)]
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
    GROUND,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Block(BlockId),
    Condition(AttributeNode<Condition>),
    Contribute(Attributes, Ident, Node<BranchAccess>, ExpressionId),
    //  TODO IndirectContribute(),
    Assign(Attributes, HierarchicalId, ExpressionId),
    FunctionCall(Attributes, HierarchicalId, Vec<ExpressionId>),
    While(AttributeNode<WhileLoop>),
    DisplayTask(DisplayTaskKind, Vec<ExpressionId>),
}

#[derive(Clone)]
pub struct SeqBlock {
    pub scope: Option<BlockScope>,
    pub statements: Vec<StatementId>,
}

#[derive(Clone, Debug)]
pub struct BlockScope {
    pub name: Ident,
    pub symbols: SymbolTable,
}

#[derive(Clone, Copy, Debug)]
pub struct WhileLoop {
    pub condition: ExpressionId,
    pub body: StatementId,
}

#[derive(Clone, Debug)]
pub struct Condition {
    pub condition: ExpressionId,
    pub if_statement: StatementId,
    pub else_statement: Option<StatementId>,
}

#[derive(Clone, Debug)]
pub enum Expression {
    BinaryOperator(ExpressionId, Node<BinaryOperator>, ExpressionId),
    UnaryOperator(Node<UnaryOperator>, ExpressionId),
    Condtion(ExpressionId, Span, ExpressionId, Span, ExpressionId),
    Primary(Primary),
}

#[derive(Clone, Debug)]
pub enum BranchAccess {
    BranchOrNodePotential(HierarchicalId),
    Implicit(Branch),
}

#[derive(Clone, Debug)]
pub enum Primary {
    Integer(i64),
    UnsignedInteger(u32),
    Real(f64),
    String(StringLiteral),
    VariableOrNetReference(HierarchicalId),
    FunctionCall(HierarchicalId, Vec<ExpressionId>),
    SystemFunctionCall(
        SystemFunctionCall<ExpressionId, ExpressionId, HierarchicalId, HierarchicalId>,
    ),
    BranchAccess(Ident, Node<BranchAccess>),
    BuiltInFunctionCall1p(BuiltInFunctionCall1p, ExpressionId),
    BuiltInFunctionCall2p(BuiltInFunctionCall2p, ExpressionId, ExpressionId),
    Noise(NoiseSource<ExpressionId, ()>, Option<StringLiteral>),
    DerivativeByBranch(ExpressionId, Ident, Node<BranchAccess>),
    DerivativeByTime(ExpressionId),
    DerivativeByTemperature(ExpressionId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOperator {
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
    INTEGER,
    REAL,
}

#[derive(Clone, Debug)]
pub struct HierarchicalId {
    pub names: Vec<Ident>,
}

impl HierarchicalId {
    #[must_use]
    pub fn span(&self) -> Span {
        self.names[0]
            .span
            .extend(self.names[self.names.len() - 1].span)
    }
}
impl From<Vec<Ident>> for HierarchicalId {
    fn from(raw: Vec<Ident>) -> Self {
        Self { names: raw }
    }
}
