/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use std::ops::Range;
use std::ptr::NonNull;

use intrusive_collections::__core::cmp::Ordering;
use intrusive_collections::__core::convert::TryFrom;

use crate::ast::{
    Attribute, AttributeNode, Attributes, BuiltInFunctionCall, Function, Nature, Node,
    UnaryOperator,
};
use crate::compact_arena::{NanoArena, SafeRange, TinyArena};
use crate::hir::{Block, BranchDeclaration, Discipline, DisciplineAccess, Module, Net, Port};
use crate::ir::hir::Hir;
use crate::ir::*;
use crate::ir::{
    BranchId, FunctionId, IntegerExpressionId, NetId, ParameterId, PortId, RealExpressionId,
    StatementId, VariableId,
};
use crate::symbol::Ident;
use crate::{ir, Span};

pub struct Mir<'tag> {
    //TODO unsized
    //TODO configure to use different arena sizes
    //Declarations
    parameters: TinyArena<'tag, AttributeNode<'tag, Parameter>>,
    //    nature: NanoArena<'tag,Nature>
    branches: NanoArena<'tag, AttributeNode<'tag, BranchDeclaration<'tag>>>,
    nets: TinyArena<'tag, AttributeNode<'tag, Net<'tag>>>,
    ports: NanoArena<'tag, Port<'tag>>,
    variables: TinyArena<'tag, AttributeNode<'tag, Variable<'tag>>>,
    modules: NanoArena<'tag, AttributeNode<'tag, Module<'tag>>>,
    functions: NanoArena<'tag, AttributeNode<'tag, Function<'tag>>>,
    disciplines: NanoArena<'tag, AttributeNode<'tag, Discipline<'tag>>>,
    natures: NanoArena<'tag, AttributeNode<'tag, Nature>>,
    real_expressions: TinyArena<'tag, Node<RealExpression<'tag>>>,
    integer_expressions: TinyArena<'tag, Node<IntegerExpression<'tag>>>,
    attributes: TinyArena<'tag, Attribute<'tag>>,
    statements: TinyArena<'tag, Statement<'tag>>,
}

impl<'tag> Mir<'tag> {
    /// # Safety
    /// You should never call this yourself
    pub(crate) unsafe fn partial_initalize<'hirref>(hir: &'hirref mut Hir<'tag>) -> Box<Self> {
        let layout = std::alloc::Layout::new::<Self>();
        #[allow(clippy::cast_ptr_alignment)]
        //the ptr cast below has the right alignment since we are allocation using the right layout
        let mut res: NonNull<Self> = NonNull::new(std::alloc::alloc(layout) as *mut Self)
            .unwrap_or_else(|| std::alloc::handle_alloc_error(layout));
        NanoArena::copy_to(&mut res.as_mut().natures, &hir.natures);
        TinyArena::copy_to(&mut res.as_mut().attributes, &hir.attributes);
        NanoArena::copy_to(&mut res.as_mut().branches, &hir.branches);
        TinyArena::copy_to(&mut res.as_mut().nets, &hir.nets);
        NanoArena::copy_to(&mut res.as_mut().ports, &hir.ports);
        NanoArena::copy_to(&mut res.as_mut().modules, &hir.modules);
        NanoArena::copy_to(&mut res.as_mut().disciplines, &hir.disciplines);
        NanoArena::copy_to(&mut res.as_mut().natures, &hir.natures);
        NanoArena::move_to(&mut res.as_mut().functions, &mut hir.functions);
        TinyArena::init_from(&mut res.as_mut().parameters, &hir.parameters);
        TinyArena::init_from(&mut res.as_mut().variables, &hir.variables);
        TinyArena::init(&mut res.as_mut().integer_expressions);
        TinyArena::init(&mut res.as_mut().real_expressions);
        TinyArena::init(&mut res.as_mut().statements);
        Box::from_raw(res.as_ptr())
    }
}

impl_id_type!(BranchId in Mir::branches -> AttributeNode<'tag,BranchDeclaration<'tag>>);
impl_id_type!(NetId in Mir::nets -> AttributeNode<'tag,Net<'tag>>);
impl_id_type!(PortId in Mir::ports -> Port<'tag>);
impl_id_type!(VariableId in Mir::variables ->  AttributeNode<'tag,Variable<'tag>>);
impl<'tag> Write<VariableId<'tag>> for Mir<'tag> {
    type Data = AttributeNode<'tag, Variable<'tag>>;
    fn write(&mut self, index: VariableId<'tag>, value: Self::Data) {
        unsafe {
            self.variables
                .write(index.0, ::core::mem::MaybeUninit::new(value))
        }
    }
}
impl_id_type!(ModuleId in Mir::modules -> AttributeNode<'tag,Module<'tag>>);
impl_id_type!(FunctionId in Mir::functions -> AttributeNode<'tag,Function<'tag>>);
impl_id_type!(DisciplineId in Mir::disciplines -> AttributeNode<'tag,Discipline<'tag>>);
impl_id_type!(RealExpressionId in Mir::real_expressions -> Node<RealExpression<'tag>>);
impl_id_type!(IntegerExpressionId in Mir::integer_expressions -> Node<IntegerExpression<'tag>>);
impl_id_type!(AttributeId in Mir::attributes -> Attribute<'tag>);
impl_id_type!(StatementId in Mir::statements -> Statement<'tag>);
impl_id_type!(NatureId in Mir::natures -> AttributeNode<'tag,Nature>);
impl_id_type!(ParameterId in Mir::parameters -> AttributeNode<'tag,Parameter>);

#[derive(Clone, Copy, Debug)]
pub struct Variable<'mir> {
    pub name: Ident,
    pub variable_type: VariableType<'mir>,
}
#[derive(Clone, Copy, Debug)]
pub enum VariableType<'mir> {
    Real(Option<RealExpressionId<'mir>>),
    Integer(Option<IntegerExpressionId<'mir>>),
}
#[derive(Clone)]
pub enum Statement<'mir> {
    Condition(AttributeNode<'mir, Condition<'mir>>),
    ConditionStart {
        condition_info_and_end: StatementId<'mir>,
    },
    Contribute(
        Attributes<'mir>,
        DisciplineAccess,
        BranchId<'mir>,
        RealExpressionId<'mir>,
    ),
    //  TODO IndirectContribute(),
    Assignment(Attributes<'mir>, VariableId<'mir>, ExpressionId<'mir>),
    FunctionCall(Attributes<'mir>, FunctionId<'mir>, Vec<ExpressionId<'mir>>),
}

#[derive(Clone)]
pub struct Condition<'mir> {
    pub main_condition: IntegerExpressionId<'mir>,
    pub main_condition_statements: Block<'mir>,
    pub else_ifs: Vec<(IntegerExpressionId<'mir>, SafeRange<StatementId<'mir>>)>,
    pub else_statement: SafeRange<StatementId<'mir>>,
}

#[derive(Clone)]
pub struct Parameter {
    pub name: Ident,
    pub parameter_type: ParameterType,
}

#[derive(Clone, Debug)]
pub enum ParameterType {
    Integer {
        included_ranges: Vec<Range<NumericalParameterRangeBound<i64>>>,
        excluded_ranges: Vec<NumericalParameterRangeExclude<i64>>,
        default_value: i64,
    },
    Real {
        included_ranges: Vec<Range<NumericalParameterRangeBound<f64>>>,
        excluded_ranges: Vec<NumericalParameterRangeExclude<f64>>,
        default_value: f64,
    },
    String(
        //TODO string parameters
    ),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NumericalParameterRangeBound<T> {
    pub inclusive: bool,
    pub bound: T,
}
impl<T: PartialOrd> PartialOrd for NumericalParameterRangeBound<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self.bound.partial_cmp(&other.bound) {
            Some(Ordering::Equal) if self.inclusive != other.inclusive => None,
            order => order,
        }
    }
}
#[derive(Clone, Debug)]
pub enum NumericalParameterRangeExclude<T> {
    Value(T),
    Range(Range<NumericalParameterRangeBound<T>>),
}
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum ExpressionId<'mir> {
    Real(RealExpressionId<'mir>),
    Integer(IntegerExpressionId<'mir>),
}
impl<'mir> ExpressionId<'mir> {
    pub fn is_real(self) -> bool {
        matches!(self,Self::Real{..})
    }

    pub fn is_integer(self) -> bool {
        matches!(self,Self::Integer{..})
    }
}

#[derive(Clone, Debug)]
pub enum IntegerExpression<'mir> {
    BinaryOperator(
        IntegerExpressionId<'mir>,
        Node<IntegerBinaryOperator>,
        IntegerExpressionId<'mir>,
    ),
    UnaryOperator(Node<UnaryOperator>, IntegerExpressionId<'mir>),
    IntegerComparison(
        IntegerExpressionId<'mir>,
        Node<ComparisonOperator>,
        IntegerExpressionId<'mir>,
    ),
    RealComparison(
        RealExpressionId<'mir>,
        Node<ComparisonOperator>,
        RealExpressionId<'mir>,
    ),
    Condition(
        IntegerExpressionId<'mir>,
        Span,
        IntegerExpressionId<'mir>,
        Span,
        IntegerExpressionId<'mir>,
    ),
    RealCast(RealExpressionId<'mir>),
    Literal(i64),
    VariableReference(VariableId<'mir>),
    NetReference(NetId<'mir>),
    PortReference(PortId<'mir>),
    ParameterReference(ParameterId<'mir>),
    FunctionCall(FunctionId<'mir>, Vec<ExpressionId<'mir>>),
    Min(IntegerExpressionId<'mir>, IntegerExpressionId<'mir>),
    Max(IntegerExpressionId<'mir>, IntegerExpressionId<'mir>),
    Abs(IntegerExpressionId<'mir>),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntegerUnaryOperator {
    BitNegate,
    LogicNegate,
    ArithmeticNegate,
    ExplicitPositive,
}

#[derive(Clone, Debug)]
pub enum RealExpression<'mir> {
    BinaryOperator(
        RealExpressionId<'mir>,
        Node<RealBinaryOperator>,
        RealExpressionId<'mir>,
    ),
    Negate(Span, RealExpressionId<'mir>),
    Condition(
        IntegerExpressionId<'mir>,
        Span,
        RealExpressionId<'mir>,
        Span,
        RealExpressionId<'mir>,
    ),
    Literal(f64),
    VariableReference(VariableId<'mir>),
    ParameterReference(ParameterId<'mir>),
    FunctionCall(FunctionId<'mir>, Vec<ExpressionId<'mir>>),
    BranchAccess(DisciplineAccess, BranchId<'mir>),
    BuiltInFunctionCall(RealBuiltInFunctionCall<'mir>),
    IntegerConversion(IntegerExpressionId<'mir>),
    SystemFunctionCall(Ident),
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

#[derive(Copy, Clone, Debug)]
pub enum RealBuiltInFunctionCall<'mir> {
    Pow(RealExpressionId<'mir>, RealExpressionId<'mir>),
    Sqrt(RealExpressionId<'mir>),

    Hypot(RealExpressionId<'mir>, RealExpressionId<'mir>),
    Exp(RealExpressionId<'mir>),
    Ln(RealExpressionId<'mir>),
    Log(RealExpressionId<'mir>),

    Min(RealExpressionId<'mir>, RealExpressionId<'mir>),
    Max(RealExpressionId<'mir>, RealExpressionId<'mir>),
    Abs(RealExpressionId<'mir>),
    Floor(RealExpressionId<'mir>),
    Ceil(RealExpressionId<'mir>),

    Sin(RealExpressionId<'mir>),
    Cos(RealExpressionId<'mir>),
    Tan(RealExpressionId<'mir>),

    ArcSin(RealExpressionId<'mir>),
    ArcCos(RealExpressionId<'mir>),
    ArcTan(RealExpressionId<'mir>),
    ArcTan2(RealExpressionId<'mir>, RealExpressionId<'mir>),

    SinH(RealExpressionId<'mir>),
    CosH(RealExpressionId<'mir>),
    TanH(RealExpressionId<'mir>),

    ArcSinH(RealExpressionId<'mir>),
    ArcCosH(RealExpressionId<'mir>),
    ArcTanH(RealExpressionId<'mir>),
}

impl<'tag, F: FnMut(ir::ExpressionId<'tag>) -> Result<RealExpressionId<'tag>, ()>>
    TryFrom<(BuiltInFunctionCall<'tag>, F)> for RealBuiltInFunctionCall<'tag>
{
    type Error = ();
    fn try_from(
        (call, mut parameter_map): (BuiltInFunctionCall<'tag>, F),
    ) -> Result<Self, Self::Error> {
        let res = match call {
            BuiltInFunctionCall::Sqrt(param) => {
                RealBuiltInFunctionCall::Sqrt(parameter_map(param)?)
            }
            BuiltInFunctionCall::Exp(param) => RealBuiltInFunctionCall::Exp(parameter_map(param)?),
            BuiltInFunctionCall::Ln(param) => RealBuiltInFunctionCall::Ln(parameter_map(param)?),
            BuiltInFunctionCall::Log(param) => RealBuiltInFunctionCall::Log(parameter_map(param)?),
            BuiltInFunctionCall::Abs(param) => RealBuiltInFunctionCall::Abs(parameter_map(param)?),
            BuiltInFunctionCall::Floor(param) => {
                RealBuiltInFunctionCall::Floor(parameter_map(param)?)
            }
            BuiltInFunctionCall::Ceil(param) => {
                RealBuiltInFunctionCall::Ceil(parameter_map(param)?)
            }
            BuiltInFunctionCall::Sin(param) => RealBuiltInFunctionCall::Sin(parameter_map(param)?),
            BuiltInFunctionCall::Cos(param) => RealBuiltInFunctionCall::Cos(parameter_map(param)?),
            BuiltInFunctionCall::Tan(param) => RealBuiltInFunctionCall::Tan(parameter_map(param)?),
            BuiltInFunctionCall::ArcSin(param) => {
                RealBuiltInFunctionCall::ArcSin(parameter_map(param)?)
            }
            BuiltInFunctionCall::ArcCos(param) => {
                RealBuiltInFunctionCall::ArcCos(parameter_map(param)?)
            }
            BuiltInFunctionCall::ArcTan(param) => {
                RealBuiltInFunctionCall::ArcTan(parameter_map(param)?)
            }
            BuiltInFunctionCall::SinH(param) => {
                RealBuiltInFunctionCall::SinH(parameter_map(param)?)
            }
            BuiltInFunctionCall::CosH(param) => {
                RealBuiltInFunctionCall::CosH(parameter_map(param)?)
            }
            BuiltInFunctionCall::TanH(param) => {
                RealBuiltInFunctionCall::TanH(parameter_map(param)?)
            }
            BuiltInFunctionCall::ArcSinH(param) => {
                RealBuiltInFunctionCall::ArcSinH(parameter_map(param)?)
            }
            BuiltInFunctionCall::ArcCosH(param) => {
                RealBuiltInFunctionCall::ArcCosH(parameter_map(param)?)
            }
            BuiltInFunctionCall::ArcTanH(param) => {
                RealBuiltInFunctionCall::ArcTanH(parameter_map(param)?)
            }
            BuiltInFunctionCall::Min(arg1, arg2) => {
                RealBuiltInFunctionCall::Min(parameter_map(arg1)?, parameter_map(arg2)?)
            }
            BuiltInFunctionCall::Max(arg1, arg2) => {
                RealBuiltInFunctionCall::Max(parameter_map(arg1)?, parameter_map(arg2)?)
            }
            BuiltInFunctionCall::ArcTan2(arg1, arg2) => {
                RealBuiltInFunctionCall::ArcTan2(parameter_map(arg1)?, parameter_map(arg2)?)
            }
            BuiltInFunctionCall::Hypot(arg1, arg2) => {
                RealBuiltInFunctionCall::Hypot(parameter_map(arg1)?, parameter_map(arg2)?)
            }
            BuiltInFunctionCall::Pow(arg1, arg2) => {
                RealBuiltInFunctionCall::Pow(parameter_map(arg1)?, parameter_map(arg2)?)
            }
        };
        Ok(res)
    }
}
