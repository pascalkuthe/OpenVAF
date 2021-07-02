/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

pub use crate::symbol_table::SymbolTable;
use core::fmt::Debug;

use openvaf_data_structures::index_vec::IndexVec;

use openvaf_ir::ids::{
    AttributeId, BlockId, BranchId, DisciplineId, ExpressionId, FunctionId, IdRange, ModuleId,
    NatureId, NetId, ParameterId, PortBranchId, PortId, StatementId, VariableId,
};

pub use openvaf_ir::{
    impl_id_type, Attribute, ConstVal, DisplayTaskKind, DoubleArgMath, Node,
    ParameterExcludeConstraint, ParameterRangeConstraint, ParameterRangeConstraintBound, Port,
    SingleArgMath, Spanned, StopTaskKind, Type, UnaryOperator,
};

pub type SystemFunctionCall = openvaf_ir::SystemFunctionCall<ExpressionId, ExpressionId, (), ()>;

use openvaf_ir::SimpleConstVal;
use openvaf_session::sourcemap::Span;
use openvaf_session::sourcemap::StringLiteral;
use openvaf_session::symbols::Ident;
use std::ops::Range;

pub mod symbol_table;

/// An Ast representing a parsed Verilog-AMS project (root file);
/// It provides stable indices for every Node because the entire Tree is immutable once created;
#[derive(Default, Debug, Clone)]
pub struct Ast {
    //Declarations
    pub branches: IndexVec<BranchId, Node<Branch>>,
    pub port_branches: IndexVec<PortBranchId, Node<PortBranch>>,
    pub nets: IndexVec<NetId, Node<Net>>,
    pub ports: IndexVec<PortId, Port>,
    pub variables: IndexVec<VariableId, Node<Variable>>,
    pub parameters: IndexVec<ParameterId, Node<Parameter>>,
    pub modules: IndexVec<ModuleId, Node<Module>>,
    pub functions: IndexVec<FunctionId, Node<Function>>,
    pub disciplines: IndexVec<DisciplineId, Node<Discipline>>,
    pub natures: IndexVec<NatureId, Node<Nature>>,
    //Ast Items
    pub expressions: IndexVec<ExpressionId, Spanned<Expression>>,
    pub blocks: IndexVec<BlockId, Block>,
    pub attributes: IndexVec<AttributeId, Attribute>,
    pub statements: IndexVec<StatementId, Node<Statement>>,
    pub top_symbols: SymbolTable,
}

impl Ast {
    #[must_use]
    pub fn new() -> Self {
        // Large initial capacity to avoid reallocation
        // TODO configure this smh
        Self {
            branches: IndexVec::with_capacity(32),
            port_branches: IndexVec::with_capacity(32),
            nets: IndexVec::with_capacity(32),
            ports: IndexVec::with_capacity(32),
            variables: IndexVec::with_capacity(512),
            parameters: IndexVec::with_capacity(512),
            modules: IndexVec::with_capacity(4),
            functions: IndexVec::with_capacity(16),
            disciplines: IndexVec::with_capacity(8),
            natures: IndexVec::with_capacity(8),
            expressions: IndexVec::with_capacity(u16::MAX as usize),
            blocks: IndexVec::with_capacity(32),
            attributes: IndexVec::with_capacity(128),
            statements: IndexVec::with_capacity(4096),
            top_symbols: SymbolTable::with_capacity(32),
        }
    }
}

impl_id_type!(BranchId in Ast => branches as Node<Branch>);

impl_id_type!(PortBranchId in Ast => port_branches as Node<PortBranch>);

impl_id_type!(NetId in Ast => nets as Node<Net>);

impl_id_type!(PortId in Ast => ports as Port);

impl_id_type!(ParameterId in Ast => parameters as Node<Parameter>);

impl_id_type!(VariableId in Ast => variables as Node<Variable>);

impl_id_type!(ModuleId in Ast => modules as Node<Module>);

impl_id_type!(FunctionId in Ast => functions as Node<Function>);

impl_id_type!(DisciplineId in Ast => disciplines as Node<Discipline>);

impl_id_type!(ExpressionId in Ast => expressions as Spanned<Expression>);

impl_id_type!(AttributeId in Ast => attributes as Attribute);

impl_id_type!(StatementId in Ast => statements as Node<Statement>);

impl_id_type!(BlockId in Ast => blocks as Block);

impl_id_type!(NatureId in Ast => natures as Node<Nature>);

#[derive(Clone, Debug)]
pub struct Nature {
    pub ident: Ident,
    pub parent: Option<Ident>,
    pub attributes: Vec<Option<Spanned<(NatureAttribute, ExpressionId)>>>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum NatureAttribute {
    Access,
    Abstol,
    DerivativeNature,
    AntiDerivativeNature,
    Units,
    User(Ident),
}

#[derive(Debug, Clone)]
pub enum PortList {
    Expected(Vec<Ident>),
    Declarations(PortId),
}

impl From<Vec<Ident>> for PortList {
    fn from(expected: Vec<Ident>) -> Self {
        Self::Expected(expected)
    }
}

impl From<PortId> for PortList {
    fn from(ports: PortId) -> Self {
        Self::Declarations(ports)
    }
}

#[derive(Clone, Debug)]
pub struct Module {
    pub ident: Ident,
    pub branches: IdRange<BranchId>,
    pub port_branches: IdRange<PortBranchId>,
    pub ports: Spanned<PortList>,
    pub parameters: IdRange<ParameterId>,
    pub body_ports: IdRange<PortId>,
    pub symbol_table: SymbolTable,
    pub analog_stmts: Vec<StatementId>,
}

#[derive(Clone, Debug)]
pub struct Parameter {
    pub ident: Ident,
    pub param_constraints: ParameterConstraints,
    pub default: ExpressionId,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub enum ParameterConstraint<F, E> {
    From(F),
    Exclude(E),
}

pub type OrderedParameterConstraint = ParameterConstraint<
    Range<ParameterRangeConstraintBound<ExpressionId>>,
    ParameterExcludeConstraint<ExpressionId>,
>;

pub type UnorderedParameterConstraint = ParameterConstraint<ExpressionId, ExpressionId>;

#[derive(Clone, Debug)]
pub enum ParameterConstraints {
    Ordered(Vec<OrderedParameterConstraint>),
    Unordered(Vec<UnorderedParameterConstraint>),
}

#[derive(Clone, Debug)]
pub struct Branch {
    pub ident: Ident,
    pub hi_net: HierarchicalId,
    pub lo_net: Option<HierarchicalId>,
}

#[derive(Clone, Debug)]
pub struct PortBranch {
    pub ident: Ident,
    pub port: HierarchicalId,
}

#[derive(Clone, Debug)]
pub struct Discipline {
    pub ident: Ident,
    pub items: Vec<Spanned<DisciplineItem>>,
}

#[derive(Clone, Copy, Debug)]
pub enum DisciplineItem {
    Potential(Ident),
    Flow(Ident),
    Domain(bool),
    Error,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub ident: Ident,
    pub args: Vec<FunctionArg>,
    pub declarations: SymbolTable,
    pub return_variable: VariableId,
    pub body: StatementId,
}

#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub struct FunctionArg {
    pub ident: Ident,
    pub input: bool,
    pub output: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct Net {
    pub ident: Ident,
    pub discipline: Option<Ident>,
    pub net_type: NetType,
}

impl Default for Net {
    fn default() -> Self {
        Self {
            ident: Ident::DUMMY,
            discipline: None,
            net_type: NetType::UNDECLARED,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Variable {
    pub ident: Ident,
    pub ty: Type,
    pub default: Option<ExpressionId>,
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
    Condition(ExpressionId, StatementId, Option<StatementId>),
    Case(ExpressionId, Vec<Spanned<CaseItem>>),
    Contribute(ExpressionId, ExpressionId),
    //  TODO IndirectContribute(),
    Assignment(HierarchicalId, ExpressionId),
    While(ExpressionId, StatementId),
    For(ForLoop),
    DisplayTask(DisplayTaskKind, Vec<ExpressionId>),
    StopTask(StopTaskKind, Option<ExpressionId>),
    Error,
    NoOp,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub scope: Option<BlockScope>,
    pub statements: Vec<StatementId>,
}

#[derive(Clone, Debug)]
pub struct BlockScope {
    pub ident: Ident,
    pub symbols: SymbolTable,
}

#[derive(Clone, Copy, Debug)]
pub struct WhileLoop {
    pub condition: ExpressionId,
    pub body: StatementId,
}

#[derive(Clone, Debug)]
pub struct ForLoop {
    pub cond: ExpressionId,
    pub init: (HierarchicalId, ExpressionId),
    pub incr: (HierarchicalId, ExpressionId),
    pub body: StatementId,
}

#[derive(Clone, Debug)]
pub struct Condition {
    pub condition: ExpressionId,
    pub if_statement: StatementId,
    pub else_statement: Option<StatementId>,
}

#[derive(Clone, Debug)]
pub struct CaseItem {
    pub values: Vec<ExpressionId>,
    pub stmt: Option<StatementId>,
}

#[derive(Clone, Debug)]
pub enum Expression {
    BinaryOperator(ExpressionId, Spanned<BinaryOperator>, ExpressionId),
    UnaryOperator(Spanned<UnaryOperator>, ExpressionId),
    Condition(ExpressionId, ExpressionId, ExpressionId),
    Primary(Primary),
    Array(Vec<ExpressionId>),
    Error,
}

pub type NoiseSource = openvaf_ir::NoiseSource<ExpressionId, ()>;

#[derive(Clone, Debug)]
pub enum Primary {
    Constant(ConstVal),

    Reference(HierarchicalId),

    SystemFunctionCall(SystemFunctionCall),
    FunctionCall(Ident, Vec<ExpressionId>),

    PortFlowProbe(Ident, HierarchicalId),
    SingleArgMath(SingleArgMath, ExpressionId),

    DoubleArgMath(DoubleArgMath, ExpressionId, ExpressionId),

    Noise(NoiseSource, Option<ExpressionId>),

    PartialDerivative(ExpressionId, ExpressionId),
    DerivativeByTime(ExpressionId),
}

impl From<i64> for Primary {
    fn from(val: i64) -> Self {
        Self::Constant(ConstVal::Scalar(SimpleConstVal::Integer(val)))
    }
}

impl From<u32> for Primary {
    fn from(val: u32) -> Self {
        Self::Constant(ConstVal::Scalar(SimpleConstVal::Integer(val.into())))
    }
}

impl From<f64> for Primary {
    fn from(val: f64) -> Self {
        Self::Constant(ConstVal::Scalar(SimpleConstVal::Real(val)))
    }
}

impl From<StringLiteral> for Primary {
    fn from(val: StringLiteral) -> Self {
        Self::Constant(ConstVal::Scalar(SimpleConstVal::String(val)))
    }
}

impl From<HierarchicalId> for Primary {
    fn from(ident: HierarchicalId) -> Self {
        Self::Reference(ident)
    }
}

impl From<SystemFunctionCall> for Primary {
    fn from(call: SystemFunctionCall) -> Self {
        Self::SystemFunctionCall(call)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    Plus,
    Minus,
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
