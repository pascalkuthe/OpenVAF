/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::UnaryOperator;

use crate::hir::{BranchDeclaration, Discipline, DisciplineAccess, Net, Port};
use crate::ir::cfg::ControlFlowGraph;
use crate::ir::hir::Hir;
use crate::ir::ids::{IdRange, StringExpressionId};
use crate::literals::StringLiteral;
use crate::sourcemap::Span;
use crate::symbol::Ident;
use index_vec::IndexVec;

use std::mem::take;
use std::ops::Range;

use crate::derivatives::DerivativeMap;
use crate::ir::{
    AttributeId, AttributeNode, Attributes, BranchId, BuiltInFunctionCall1p, BuiltInFunctionCall2p,
    DisciplineId, IntegerExpressionId, ModuleId, NatureId, NetId, Node, NoiseSource,
    NumericalParameterRangeBound, NumericalParameterRangeExclude, ParameterId, PortId,
    PrintOnFinish, RealExpressionId, StatementId, StopTaskKind, VariableId,
};

pub mod fold;
pub mod visit;

#[derive(Debug, Clone, Default)]
pub struct Mir {
    pub branches: IndexVec<BranchId, AttributeNode<BranchDeclaration>>,
    pub nets: IndexVec<NetId, AttributeNode<Net>>,
    pub ports: IndexVec<PortId, Port>,
    pub disciplines: IndexVec<DisciplineId, AttributeNode<Discipline>>,

    // The only thing that changes about these types is that type checking happens
    pub modules: IndexVec<ModuleId, AttributeNode<Module>>,
    pub parameters: IndexVec<ParameterId, AttributeNode<Parameter>>,
    pub variables: IndexVec<VariableId, AttributeNode<Variable>>,
    pub natures: IndexVec<NatureId, AttributeNode<Nature>>,
    pub attributes: IndexVec<AttributeId, Attribute>,

    // Expressions
    pub real_expressions: IndexVec<RealExpressionId, Node<RealExpression>>,
    pub integer_expressions: IndexVec<IntegerExpressionId, Node<IntegerExpression>>,
    pub string_expressions: IndexVec<StringExpressionId, Node<StringExpression>>,

    // Statements
    pub statements: IndexVec<StatementId, Statement>,

    pub(crate) derivatives: DerivativeMap,
}

impl Mir {
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

impl_id_type!(BranchId in Mir::branches -> AttributeNode<BranchDeclaration>);
impl_id_type!(NetId in Mir::nets -> AttributeNode<Net>);
impl_id_type!(PortId in Mir::ports -> Port);
impl_id_type!(VariableId in Mir::variables ->  AttributeNode<Variable>);
impl_id_type!(ModuleId in Mir::modules -> AttributeNode<Module>);
impl_id_type!(DisciplineId in Mir::disciplines -> AttributeNode<Discipline>);
impl_id_type!(RealExpressionId in Mir::real_expressions -> Node<RealExpression>);
impl_id_type!(StringExpressionId in Mir::string_expressions -> Node<StringExpression>);
impl_id_type!(IntegerExpressionId in Mir::integer_expressions -> Node<IntegerExpression>);
impl_id_type!(AttributeId in Mir::attributes -> Attribute);
impl_id_type!(StatementId in Mir::statements -> Statement);
impl_id_type!(NatureId in Mir::natures -> AttributeNode<Nature>);
impl_id_type!(ParameterId in Mir::parameters -> AttributeNode<Parameter>);

#[derive(Clone, Copy, Debug)]
pub struct Variable {
    pub ident: Ident,
    pub variable_type: VariableType,
}

#[derive(Copy, Clone, Debug)]
pub struct Attribute {
    pub name: Ident,
    pub value: Option<ExpressionId>,
}

#[derive(Copy, Clone, Debug)]
pub struct Nature {
    pub name: Ident,
    pub abstol: f64,
    pub units: StringLiteral,
    pub access: Ident,
    pub idt_nature: NatureId,
    pub ddt_nature: NatureId,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub name: Ident,
    pub port_list: IdRange<PortId>,
    pub analog_cfg: ControlFlowGraph,
}

#[derive(Clone, Copy, Debug)]
pub enum VariableType {
    Real(Option<RealExpressionId>),
    Integer(Option<IntegerExpressionId>),
}

#[derive(Clone, Debug)]
pub enum Statement {
    Contribute(Attributes, DisciplineAccess, BranchId, RealExpressionId),
    //  TODO IndirectContribute
    Assignment(Attributes, VariableId, ExpressionId),
    StopTask(AttributeNode<StopTaskKind>, PrintOnFinish),
}

#[derive(Clone, Debug)]
pub struct Parameter {
    pub ident: Ident,
    pub parameter_type: ParameterType,
}

impl ParameterType {
    #[must_use]
    pub fn is_numeric(&self) -> bool {
        !matches!(self, ParameterType::String(_))
    }
}

#[derive(Clone, Debug)]
pub enum ParameterType {
    Integer {
        from_ranges: Vec<Range<NumericalParameterRangeBound<IntegerExpressionId>>>,
        excluded: Vec<NumericalParameterRangeExclude<IntegerExpressionId>>,
        default_value: IntegerExpressionId,
    },
    Real {
        from_ranges: Vec<Range<NumericalParameterRangeBound<RealExpressionId>>>,
        excluded: Vec<NumericalParameterRangeExclude<RealExpressionId>>,
        default_value: RealExpressionId,
    },
    String(StringExpressionId),
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
        Node<IntegerBinaryOperator>,
        IntegerExpressionId,
    ),
    UnaryOperator(Node<UnaryOperator>, IntegerExpressionId),
    IntegerComparison(
        IntegerExpressionId,
        Node<ComparisonOperator>,
        IntegerExpressionId,
    ),
    RealComparison(RealExpressionId, Node<ComparisonOperator>, RealExpressionId),
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
    BinaryOperator(RealExpressionId, Node<RealBinaryOperator>, RealExpressionId),
    Negate(Span, RealExpressionId),
    Condition(IntegerExpressionId, RealExpressionId, RealExpressionId),
    Literal(f64),
    VariableReference(VariableId),
    ParameterReference(ParameterId),
    BranchAccess(DisciplineAccess, BranchId, u8),
    Noise(NoiseSource<RealExpressionId, ()>, Option<StringLiteral>),
    BuiltInFunctionCall1p(BuiltInFunctionCall1p, RealExpressionId),
    BuiltInFunctionCall2p(BuiltInFunctionCall2p, RealExpressionId, RealExpressionId),
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
