/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use std::mem::take;
use std::ops::Range;

use index_vec::IndexVec;

use crate::ast::UnaryOperator;
use crate::data_structures::sync::{Lrc, RwLock};
use crate::derivatives::DerivativeMap;
use crate::hir::{Branch, Discipline, DisciplineAccess, Net};
use crate::ir::cfg::ControlFlowGraph;
use crate::ir::hir::Hir;
use crate::ir::ids::{
    AttributeId, BranchId, DisciplineId, IntegerExpressionId, ModuleId, NatureId, NetId,
};
use crate::ir::ids::{
    IdRange, ParameterId, PortId, RealExpressionId, StatementId, StringExpressionId, VariableId,
};
use crate::ir::Port;
use crate::ir::{
    DoubleArgMath, Node, NoiseSource, ParameterExcludeConstraint, ParameterRangeConstraintBound,
    PrintOnFinish, SingleArgMath, Spanned, StopTaskKind,
};
use crate::literals::StringLiteral;
use crate::sourcemap::Span;
use crate::symbol::Ident;
use crate::HashMap;

pub mod fold;
pub mod visit;

#[derive(Debug, Clone, Default)]
pub struct Mir {
    pub branches: IndexVec<BranchId, Node<Branch>>,
    pub nets: IndexVec<NetId, Node<Net>>,
    pub ports: IndexVec<PortId, Port>,
    pub disciplines: IndexVec<DisciplineId, Node<Discipline>>,

    // The only thing that changes about these types is that type checking happens
    pub modules: IndexVec<ModuleId, Node<Module>>,
    pub parameters: IndexVec<ParameterId, Node<Parameter>>,
    pub variables: IndexVec<VariableId, Node<Variable>>,
    pub natures: IndexVec<NatureId, Node<Nature>>,
    pub attributes: IndexVec<AttributeId, Attribute>,

    // Expressions
    pub real_expressions: IndexVec<RealExpressionId, Spanned<RealExpression>>,
    pub integer_expressions: IndexVec<IntegerExpressionId, Spanned<IntegerExpression>>,
    pub string_expressions: IndexVec<StringExpressionId, Spanned<StringExpression>>,

    // Statements
    statements: IndexVec<StatementId, Node<Statement>>,

    pub(crate) derivatives: DerivativeMap,
    pub(crate) derivative_origins: HashMap<VariableId, VariableId>,

    pub(crate) statement_origins: HashMap<StatementId, StatementId>,
}

impl Mir {
    pub fn statements(&self) -> &IndexVec<StatementId, Node<Statement>> {
        &self.statements
    }

    pub fn add_modified_stmt(&mut self, stmt: Statement, origin: StatementId) -> StatementId {
        let id = self.add_new_stmt(Node {
            attributes: self.statements[origin].attributes,
            span: self.statements[origin].span,
            contents: stmt,
        });
        self.statement_origins.insert(id, origin);
        id
    }

    pub fn add_new_stmt(&mut self, stmt: Node<Statement>) -> StatementId {
        self.statements.push(stmt)
    }

    pub(crate) fn initalize(hir: &mut Hir) -> Self {
        Self {
            // Nothing about these changes during HIR lowering we can just copy
            branches: take(&mut hir.branches),
            nets: take(&mut hir.nets),
            ports: take(&mut hir.ports),
            disciplines: take(&mut hir.disciplines),
            ..Self::default()
        }
    }
}

impl_id_type!(BranchId in Mir::branches -> Node<Branch>);
impl_id_type!(NetId in Mir::nets -> Node<Net>);
impl_id_type!(PortId in Mir::ports -> Port);
impl_id_type!(VariableId in Mir::variables ->  Node<Variable>);
impl_id_type!(ModuleId in Mir::modules -> Node<Module>);
impl_id_type!(DisciplineId in Mir::disciplines -> Node<Discipline>);
impl_id_type!(RealExpressionId in Mir::real_expressions -> Spanned<RealExpression>);
impl_id_type!(StringExpressionId in Mir::string_expressions -> Spanned<StringExpression>);
impl_id_type!(IntegerExpressionId in Mir::integer_expressions -> Spanned<IntegerExpression>);
impl_id_type!(AttributeId in Mir::attributes -> Attribute);
impl_id_type!(StatementId in Mir::statements -> Node<Statement>);
impl_id_type!(NatureId in Mir::natures -> Node<Nature>);
impl_id_type!(ParameterId in Mir::parameters -> Node<Parameter>);

#[derive(Clone, Copy, Debug)]
pub struct Variable {
    pub ident: Ident,
    pub variable_type: VariableType,
}

#[derive(Clone, Debug)]
pub struct Attribute {
    pub ident: Ident,
    pub value: Vec<ExpressionId>,
}

#[derive(Copy, Clone, Debug)]
pub struct Nature {
    pub ident: Ident,
    pub abstol: f64,
    pub units: StringLiteral,
    pub access: Ident,
    pub idt_nature: NatureId,
    pub ddt_nature: NatureId,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub ident: Ident,
    pub port_list: IdRange<PortId>,
    pub analog_cfg: Lrc<RwLock<ControlFlowGraph>>,
}

#[derive(Clone, Copy, Debug)]
pub enum VariableType {
    Real(Option<RealExpressionId>),
    Integer(Option<IntegerExpressionId>),
    String(Option<StringExpressionId>),
}

#[derive(Clone, Debug)]
pub enum Statement {
    Contribute(DisciplineAccess, BranchId, RealExpressionId),
    //  TODO IndirectContribute
    Assignment(VariableId, ExpressionId),
    StopTask(StopTaskKind, PrintOnFinish),
}

#[derive(Clone, Debug)]
pub struct Parameter {
    pub ident: Ident,
    pub parameter_type: ParameterType,
}

impl ParameterType {
    #[must_use]
    pub fn is_numeric(&self) -> bool {
        !matches!(self, ParameterType::String{..})
    }
}

#[derive(Clone, Debug)]
pub enum ParameterType {
    Integer {
        included: Vec<Range<ParameterRangeConstraintBound<IntegerExpressionId>>>,
        excluded: Vec<ParameterExcludeConstraint<IntegerExpressionId>>,
        default_value: IntegerExpressionId,
    },
    Real {
        included: Vec<Range<ParameterRangeConstraintBound<RealExpressionId>>>,
        excluded: Vec<ParameterExcludeConstraint<RealExpressionId>>,
        default_value: RealExpressionId,
    },
    String {
        included: Vec<StringExpressionId>,
        excluded: Vec<StringExpressionId>,
        default_value: StringExpressionId,
    },
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum ExpressionId {
    Real(RealExpressionId),
    Integer(IntegerExpressionId),
    String(StringExpressionId),
}

impl ExpressionId {
    #[must_use]
    pub fn span(self, mir: &Mir) -> Span {
        match self {
            Self::Real(id) => mir[id].span,
            Self::Integer(id) => mir[id].span,
            Self::String(id) => mir[id].span,
        }
    }

    #[must_use]
    pub fn is_real(self) -> bool {
        matches!(self,Self::Real{..})
    }

    #[must_use]
    pub fn is_integer(self) -> bool {
        matches!(self,Self::Integer{..})
    }
}

impl From<RealExpressionId> for ExpressionId {
    #[allow(clippy::inline_always)]
    #[inline(always)]
    fn from(expr: RealExpressionId) -> Self {
        Self::Real(expr)
    }
}

impl From<IntegerExpressionId> for ExpressionId {
    #[allow(clippy::inline_always)]
    #[inline(always)]
    fn from(expr: IntegerExpressionId) -> Self {
        Self::Integer(expr)
    }
}

impl From<StringExpressionId> for ExpressionId {
    #[allow(clippy::inline_always)]
    #[inline(always)]
    fn from(expr: StringExpressionId) -> Self {
        Self::String(expr)
    }
}

#[derive(Clone, Debug)]
pub enum IntegerExpression {
    BinaryOperator(
        IntegerExpressionId,
        Spanned<IntegerBinaryOperator>,
        IntegerExpressionId,
    ),
    UnaryOperator(Spanned<UnaryOperator>, IntegerExpressionId),
    IntegerComparison(
        IntegerExpressionId,
        Spanned<ComparisonOperator>,
        IntegerExpressionId,
    ),
    RealComparison(
        RealExpressionId,
        Spanned<ComparisonOperator>,
        RealExpressionId,
    ),
    StringEq(StringExpressionId, StringExpressionId),
    StringNEq(StringExpressionId, StringExpressionId),
    Condition(
        IntegerExpressionId,
        IntegerExpressionId,
        IntegerExpressionId,
    ),
    RealCast(RealExpressionId),
    Literal(i64),
    VariableReference(VariableId),
    NetReference(NetId),
    PortReference(PortId),
    ParameterReference(ParameterId),
    Min(IntegerExpressionId, IntegerExpressionId),
    Max(IntegerExpressionId, IntegerExpressionId),
    Abs(IntegerExpressionId),

    ParamGiven(ParameterId),
    PortConnected(PortId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntegerBinaryOperator {
    Sum,
    Subtract,
    Multiply,
    Divide,
    Exponent,
    Modulus,

    ShiftLeft,
    ShiftRight,

    Xor,
    NXor,
    And,
    Or,

    LogicOr,
    LogicAnd,
}
impl From<RealBinaryOperator> for IntegerBinaryOperator {
    fn from(op: RealBinaryOperator) -> Self {
        match op {
            RealBinaryOperator::Sum => IntegerBinaryOperator::Sum,
            RealBinaryOperator::Subtract => IntegerBinaryOperator::Subtract,
            RealBinaryOperator::Multiply => IntegerBinaryOperator::Multiply,
            RealBinaryOperator::Divide => IntegerBinaryOperator::Divide,
            RealBinaryOperator::Exponent => IntegerBinaryOperator::Exponent,
            RealBinaryOperator::Modulus => IntegerBinaryOperator::Modulus,
        }
    }
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ComparisonOperator {
    LessThen,
    LessEqual,
    GreaterThen,
    GreaterEqual,
    LogicEqual,
    LogicalNotEqual,
}

#[derive(Clone, Debug)]
pub enum RealExpression {
    BinaryOperator(
        RealExpressionId,
        Spanned<RealBinaryOperator>,
        RealExpressionId,
    ),
    Negate(Span, RealExpressionId),
    Condition(IntegerExpressionId, RealExpressionId, RealExpressionId),
    Literal(f64),
    VariableReference(VariableId),
    ParameterReference(ParameterId),
    BranchAccess(DisciplineAccess, BranchId, u8),
    PortFlowAccess(PortId, u8),
    Noise(NoiseSource<RealExpressionId, ()>, Option<StringLiteral>),
    BuiltInFunctionCall1p(SingleArgMath, RealExpressionId),
    BuiltInFunctionCall2p(DoubleArgMath, RealExpressionId, RealExpressionId),
    IntegerConversion(IntegerExpressionId),

    Temperature,
    SimParam(StringExpressionId, Option<RealExpressionId>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RealBinaryOperator {
    Sum,
    Subtract,
    Multiply,
    Divide,
    Exponent,
    Modulus,
}

#[derive(Clone, Debug)]
pub enum StringExpression {
    Condition(IntegerExpressionId, StringExpressionId, StringExpressionId),

    Literal(StringLiteral),
    SimParam(StringExpressionId),

    VariableReference(VariableId),

    ParameterReference(ParameterId),
}
