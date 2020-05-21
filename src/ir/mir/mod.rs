/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

pub mod control_flow_graph;

pub use control_flow_graph::ControlFlowGraph;

use crate::analysis::DominatorTree;
use crate::ast::{Function, UnaryOperator};
use crate::compact_arena::{CompressedRange, NanoArena, SafeRange, StringArena, TinyArena};
use crate::hir::{BranchDeclaration, Discipline, DisciplineAccess, Net, Port};
use crate::ir::hir::Hir;
use crate::ir::ids::StringExpressionId;
use crate::ir::*;
use crate::symbol::Ident;
use crate::Span;
use bitflags::_core::mem::MaybeUninit;
use rustc_hash::FxHashMap;
use std::ops::Range;
use std::ptr::NonNull;

pub struct Mir<'tag> {
    //TODO unsized
    //TODO configure to use different arena sizes
    //Declarations
    parameters: TinyArena<'tag, AttributeNode<'tag, Parameter<'tag>>>,
    //    nature: NanoArena<'tag,Nature>
    branches: NanoArena<'tag, AttributeNode<'tag, BranchDeclaration<'tag>>>,
    nets: TinyArena<'tag, AttributeNode<'tag, Net<'tag>>>,
    ports: NanoArena<'tag, Port<'tag>>,
    variables: TinyArena<'tag, AttributeNode<'tag, Variable<'tag>>>,
    modules: NanoArena<'tag, AttributeNode<'tag, Module<'tag>>>,
    functions: NanoArena<'tag, AttributeNode<'tag, Function<'tag>>>,
    disciplines: NanoArena<'tag, AttributeNode<'tag, Discipline<'tag>>>,
    natures: NanoArena<'tag, AttributeNode<'tag, Nature<'tag>>>,
    real_expressions: TinyArena<'tag, Node<RealExpression<'tag>>>,
    integer_expressions: TinyArena<'tag, Node<IntegerExpression<'tag>>>,
    string_expressions: NanoArena<'tag, Node<StringExpression<'tag>>>,
    attributes: TinyArena<'tag, Attribute<'tag>>,
    statements: TinyArena<'tag, Statement<'tag>>,
    pub(crate) string_literals: StringArena<'tag>,
}

impl<'tag> Mir<'tag> {
    /// # Safety
    /// You should never call this yourself
    pub(crate) unsafe fn partial_initalize<'lt>(hir: &'lt mut Hir<'tag>) -> Box<Self> {
        let layout = std::alloc::Layout::new::<Self>();
        #[allow(clippy::cast_ptr_alignment)]
        //the ptr cast below has the right alignment since we are allocation using the right layout
        let mut res: NonNull<Self> = NonNull::new(std::alloc::alloc(layout) as *mut Self)
            .unwrap_or_else(|| std::alloc::handle_alloc_error(layout));
        NanoArena::copy_to(&mut res.as_mut().branches, &hir.branches);
        TinyArena::copy_to(&mut res.as_mut().nets, &hir.nets);
        NanoArena::copy_to(&mut res.as_mut().ports, &hir.ports);
        NanoArena::copy_to(&mut res.as_mut().disciplines, &hir.disciplines);

        NanoArena::init(&mut res.as_mut().modules);
        NanoArena::init_from(&mut res.as_mut().natures, &hir.natures);
        TinyArena::init_from(&mut res.as_mut().parameters, &hir.parameters);
        TinyArena::init_from(&mut res.as_mut().variables, &hir.variables);

        NanoArena::move_to(&mut res.as_mut().functions, &mut hir.functions);
        StringArena::move_to(&mut res.as_mut().string_literals, &mut hir.string_literals);

        TinyArena::init(&mut res.as_mut().integer_expressions);
        TinyArena::init(&mut res.as_mut().real_expressions);

        TinyArena::init(&mut res.as_mut().statements);
        Box::from_raw(res.as_ptr())
    }

    pub fn get_str(&self, range: CompressedRange<'tag>) -> &str {
        &self.string_literals[range]
    }
    pub fn get_str_mut(&mut self, range: CompressedRange<'tag>) -> &mut str {
        &mut self.string_literals[range]
    }
    pub fn parameter_count(&self) -> u32 {
        self.parameters.len
    }
    pub fn variable_count(&self) -> u32 {
        self.variables.len
    }
    pub fn branch_count(&self) -> u16 {
        self.branches.len
    }
    pub fn net_count(&self) -> u32 {
        self.nets.len
    }
    pub fn port_count(&self) -> u16 {
        self.ports.len
    }
    pub fn module_count(&self) -> u16 {
        self.modules.len
    }
    pub fn natures_count(&self) -> u16 {
        self.natures.len
    }
    pub fn discipline_count(&self) -> u16 {
        self.disciplines.len
    }
}

impl_id_type!(BranchId in Mir::branches -> AttributeNode<'tag,BranchDeclaration<'tag>>);
impl_id_type!(NetId in Mir::nets -> AttributeNode<'tag,Net<'tag>>);
impl_id_type!(PortId in Mir::ports -> Port<'tag>);
impl_id_type!(VariableId in Mir::variables ->  AttributeNode<'tag,Variable<'tag>>);
impl<'tag> Write<VariableId<'tag>> for Mir<'tag> {
    type Data = AttributeNode<'tag, Variable<'tag>>;
    fn write(&mut self, index: VariableId<'tag>, value: Self::Data) {
        unsafe { self.variables.write(index.0, MaybeUninit::new(value)) }
    }
}
impl_id_type!(ModuleId in Mir::modules -> AttributeNode<'tag,Module<'tag>>);
impl_id_type!(FunctionId in Mir::functions -> AttributeNode<'tag,Function<'tag>>);
impl_id_type!(DisciplineId in Mir::disciplines -> AttributeNode<'tag,Discipline<'tag>>);
impl_id_type!(RealExpressionId in Mir::real_expressions -> Node<RealExpression<'tag>>);
impl_id_type!(StringExpressionId in Mir::string_expressions -> Node<StringExpression<'tag>>);
impl_id_type!(IntegerExpressionId in Mir::integer_expressions -> Node<IntegerExpression<'tag>>);
impl_id_type!(AttributeId in Mir::attributes -> Attribute<'tag>);
impl_id_type!(StatementId in Mir::statements -> Statement<'tag>);
impl_id_type!(NatureId in Mir::natures -> AttributeNode<'tag,Nature<'tag>>);
impl<'tag> Write<NatureId<'tag>> for Mir<'tag> {
    type Data = AttributeNode<'tag, Nature<'tag>>;
    fn write(&mut self, index: NatureId<'tag>, value: Self::Data) {
        unsafe { self.natures.write(index.0, MaybeUninit::new(value)) }
    }
}
impl_id_type!(ParameterId in Mir::parameters -> AttributeNode<'tag,Parameter<'tag>>);

#[derive(Clone, Copy, Debug)]
pub struct Variable<'mir> {
    pub name: Ident,
    pub variable_type: VariableType<'mir>,
}

#[derive(Copy, Clone, Debug)]
pub struct Attribute<'tag> {
    pub name: Ident,
    pub value: Option<ExpressionId<'tag>>,
}

#[derive(Copy, Clone)]
pub struct Nature<'mir> {
    pub name: Ident,
    pub abstol: f64,
    pub units: CompressedRange<'mir>,
    pub access: Ident,
    pub idt_nature: NatureId<'mir>,
    pub ddt_nature: NatureId<'mir>,
}
#[derive(Debug)]
pub struct Module<'mir> {
    pub name: Ident,
    pub port_list: SafeRange<PortId<'mir>>,
    pub parameter_list: SafeRange<ParameterId<'mir>>,
    pub analog_cfg: ControlFlowGraph<'mir, 'mir>,
    pub analog_dtree: DominatorTree<'mir>,
}

#[derive(Clone, Copy, Debug)]
pub enum VariableType<'mir> {
    Real(Option<RealExpressionId<'mir>>),
    Integer(Option<IntegerExpressionId<'mir>>),
}

#[derive(Clone)]
pub enum Statement<'mir> {
    Contribute(
        Attributes<'mir>,
        DisciplineAccess,
        BranchId<'mir>,
        RealExpressionId<'mir>,
    ),
    //  TODO IndirectContribute
    Assignment(Attributes<'mir>, VariableId<'mir>, ExpressionId<'mir>),
}

#[derive(Clone, Debug)]
pub struct Parameter<'tag> {
    pub name: Ident,
    pub parameter_type: ParameterType<'tag>,
}

#[derive(Clone, Debug)]
pub enum ParameterType<'tag> {
    Integer {
        included_ranges: Vec<Range<NumericalParameterRangeBound<IntegerExpressionId<'tag>>>>,
        excluded_ranges: Vec<NumericalParameterRangeExclude<IntegerExpressionId<'tag>>>,
        default_value: Option<IntegerExpressionId<'tag>>,
    },
    Real {
        included_ranges: Vec<Range<NumericalParameterRangeBound<RealExpressionId<'tag>>>>,
        excluded_ranges: Vec<NumericalParameterRangeExclude<RealExpressionId<'tag>>>,
        default_value: Option<RealExpressionId<'tag>>,
    },
    String(Option<StringExpressionId<'tag>>),
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum ExpressionId<'mir> {
    Real(RealExpressionId<'mir>),
    Integer(IntegerExpressionId<'mir>),
    String(StringExpressionId<'mir>),
}

impl<'mir> ExpressionId<'mir> {
    pub fn source(self, mir: &Mir<'mir>) -> Span {
        match self {
            Self::Real(id) => mir[id].source,
            Self::Integer(id) => mir[id].source,
            Self::String(id) => mir[id].source,
        }
    }
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
    StringEq(StringExpressionId<'mir>, StringExpressionId<'mir>),
    StringNEq(StringExpressionId<'mir>, StringExpressionId<'mir>),
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

    ParamGiven(ParameterId<'mir>),
    PortConnected(PortId<'mir>),
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
    BranchAccess(DisciplineAccess, BranchId<'mir>, u8),
    Noise(
        NoiseSource<RealExpressionId<'mir>, ()>,
        Option<CompressedRange<'mir>>,
    ),
    BuiltInFunctionCall1p(BuiltInFunctionCall1p, RealExpressionId<'mir>),
    BuiltInFunctionCall2p(
        BuiltInFunctionCall2p,
        RealExpressionId<'mir>,
        RealExpressionId<'mir>,
    ),
    IntegerConversion(IntegerExpressionId<'mir>),

    Temperature,
    Vt(Option<RealExpressionId<'mir>>),
    SimParam(StringExpressionId<'mir>, Option<RealExpressionId<'mir>>),
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
pub enum StringExpression<'mir> {
    Condition(
        IntegerExpressionId<'mir>,
        Span,
        StringExpressionId<'mir>,
        Span,
        StringExpressionId<'mir>,
    ),

    Literal(CompressedRange<'mir>),
    SimParam(StringExpressionId<'mir>),

    VariableReference(VariableId<'mir>),

    ParameterReference(ParameterId<'mir>),
}

impl<'tag> Mir<'tag> {
    pub fn map_real_expr(
        &mut self,
        expr: RealExpressionId<'tag>,
        variables_to_replace: &FxHashMap<VariableId<'tag>, VariableId<'tag>>,
    ) -> Option<RealExpressionId<'tag>> {
        let new_expr = match self[expr].contents {
            RealExpression::VariableReference(var) => {
                let new_var = *variables_to_replace.get(&var)?;
                RealExpression::VariableReference(new_var)
            }

            RealExpression::BinaryOperator(lhs, op, rhs) => {
                let mapped_lhs = self.map_real_expr(lhs, variables_to_replace);
                let mapped_rhs = self.map_real_expr(rhs, variables_to_replace);
                let (lhs, rhs) = match (mapped_lhs, mapped_rhs) {
                    (None, None) => return None,
                    (Some(lhs), None) => (lhs, rhs),
                    (None, Some(rhs)) => (lhs, rhs),
                    (Some(lhs), Some(rhs)) => (lhs, rhs),
                };
                RealExpression::BinaryOperator(lhs, op, rhs)
            }

            RealExpression::Negate(span, expr) => {
                let expr = self.map_real_expr(expr, variables_to_replace)?;
                RealExpression::Negate(span, expr)
            }

            RealExpression::Vt(arg) => {
                let arg =
                    arg.map(|arg| self.map_real_expr(arg, variables_to_replace).unwrap_or(arg));
                RealExpression::Vt(arg)
            }

            RealExpression::Condition(condition, question_span, arg1, colon_span, arg2) => {
                let mapped_arg1 = self.map_real_expr(arg1, variables_to_replace);
                let mapped_arg2 = self.map_real_expr(arg2, variables_to_replace);
                let mapped_condition = self.map_int_expr(condition, variables_to_replace);
                if mapped_arg1.is_none() && mapped_arg2.is_none() && mapped_condition.is_none() {
                    return None;
                }
                let arg1 = mapped_arg1.unwrap_or(arg1);
                let arg2 = mapped_arg2.unwrap_or(arg2);
                let condition = mapped_condition.unwrap_or(condition);
                RealExpression::Condition(condition, question_span, arg1, colon_span, arg2)
            }

            RealExpression::FunctionCall(_, _) => todo!("Function Calls"),

            RealExpression::BuiltInFunctionCall1p(call, expr) => {
                let expr = self.map_real_expr(expr, variables_to_replace)?;
                RealExpression::BuiltInFunctionCall1p(call, expr)
            }

            RealExpression::BuiltInFunctionCall2p(call, arg1, arg2) => {
                let mapped_arg1 = self.map_real_expr(arg1, variables_to_replace);
                let mapped_arg2 = self.map_real_expr(arg2, variables_to_replace);
                let (arg1, arg2) = match (mapped_arg1, mapped_arg2) {
                    (None, None) => return None,
                    (Some(arg1), None) => (arg1, arg2),
                    (None, Some(arg2)) => (arg1, arg2),
                    (Some(arg1), Some(arg2)) => (arg1, arg2),
                };
                RealExpression::BuiltInFunctionCall2p(call, arg1, arg2)
            }

            RealExpression::Noise(source, src) => {
                let source = match source {
                    NoiseSource::White(expr) => {
                        NoiseSource::White(self.map_real_expr(expr, variables_to_replace)?)
                    }
                    NoiseSource::Flicker(expr1, expr2) => NoiseSource::Flicker(
                        self.map_real_expr(expr1, variables_to_replace)?,
                        self.map_real_expr(expr2, variables_to_replace)?,
                    ),
                    NoiseSource::Table(_) | NoiseSource::TableLog(_) => todo!(),
                };
                RealExpression::Noise(source, src)
            }

            RealExpression::IntegerConversion(expr) => {
                let expr = self.map_int_expr(expr, variables_to_replace)?;
                RealExpression::IntegerConversion(expr)
            }

            RealExpression::Literal(_)
            | RealExpression::ParameterReference(_)
            | RealExpression::BranchAccess(_, _, _)
            | RealExpression::Temperature => return None,

            RealExpression::SimParam(name, default) => {
                let new_name = self.map_str_expr(name, variables_to_replace);
                if let Some(default) = default {
                    if let Some(new_default) = self.map_real_expr(default, variables_to_replace) {
                        RealExpression::SimParam(new_name.unwrap_or(name), Some(new_default))
                    } else {
                        RealExpression::SimParam(new_name?, Some(default))
                    }
                } else {
                    RealExpression::SimParam(new_name?, None)
                }
            }
        };
        Some(self.push(self[expr].clone_as(new_expr)))
    }

    pub fn map_int_expr(
        &mut self,
        expr: IntegerExpressionId<'tag>,
        variables_to_replace: &FxHashMap<VariableId<'tag>, VariableId<'tag>>,
    ) -> Option<IntegerExpressionId<'tag>> {
        let new_expr = match self[expr].contents {
            IntegerExpression::VariableReference(var) => {
                let new_var = *variables_to_replace.get(&var)?;
                IntegerExpression::VariableReference(new_var)
            }

            IntegerExpression::BinaryOperator(lhs, op, rhs) => {
                let mapped_lhs = self.map_int_expr(lhs, variables_to_replace);
                let mapped_rhs = self.map_int_expr(rhs, variables_to_replace);
                let (lhs, rhs) = match (mapped_lhs, mapped_rhs) {
                    (None, None) => return None,
                    (Some(lhs), None) => (lhs, rhs),
                    (None, Some(rhs)) => (lhs, rhs),
                    (Some(lhs), Some(rhs)) => (lhs, rhs),
                };
                IntegerExpression::BinaryOperator(lhs, op, rhs)
            }

            IntegerExpression::UnaryOperator(op, expr) => {
                let expr = self.map_int_expr(expr, variables_to_replace)?;
                IntegerExpression::UnaryOperator(op, expr)
            }

            IntegerExpression::Condition(condition, question_span, arg1, colon_span, arg2) => {
                let mapped_arg1 = self.map_int_expr(arg1, variables_to_replace);
                let mapped_arg2 = self.map_int_expr(arg2, variables_to_replace);
                let mapped_condition = self.map_int_expr(condition, variables_to_replace);
                if mapped_arg1.is_none() && mapped_arg2.is_none() && mapped_condition.is_none() {
                    return None;
                }
                let arg1 = mapped_arg1.unwrap_or(arg1);
                let arg2 = mapped_arg2.unwrap_or(arg2);
                let condition = mapped_condition.unwrap_or(condition);
                IntegerExpression::Condition(condition, question_span, arg1, colon_span, arg2)
            }

            IntegerExpression::FunctionCall(_, _) => todo!("Function Calls"),

            IntegerExpression::Abs(expr) => {
                let expr = self.map_int_expr(expr, variables_to_replace)?;
                IntegerExpression::Abs(expr)
            }

            IntegerExpression::Min(arg1, arg2) => {
                let mapped_arg1 = self.map_int_expr(arg1, variables_to_replace);
                let mapped_arg2 = self.map_int_expr(arg2, variables_to_replace);
                let (arg1, arg2) = match (mapped_arg1, mapped_arg2) {
                    (None, None) => return None,
                    (Some(arg1), None) => (arg1, arg2),
                    (None, Some(arg2)) => (arg1, arg2),
                    (Some(arg1), Some(arg2)) => (arg1, arg2),
                };
                IntegerExpression::Min(arg1, arg2)
            }

            IntegerExpression::Max(arg1, arg2) => {
                let mapped_arg1 = self.map_int_expr(arg1, variables_to_replace);
                let mapped_arg2 = self.map_int_expr(arg2, variables_to_replace);
                let (arg1, arg2) = match (mapped_arg1, mapped_arg2) {
                    (None, None) => return None,
                    (Some(arg1), None) => (arg1, arg2),
                    (None, Some(arg2)) => (arg1, arg2),
                    (Some(arg1), Some(arg2)) => (arg1, arg2),
                };
                IntegerExpression::Max(arg1, arg2)
            }

            IntegerExpression::IntegerComparison(lhs, op, rhs) => {
                let mapped_lhs = self.map_int_expr(lhs, variables_to_replace);
                let mapped_rhs = self.map_int_expr(rhs, variables_to_replace);
                let (lhs, rhs) = match (mapped_lhs, mapped_rhs) {
                    (None, None) => return None,
                    (Some(lhs), None) => (lhs, rhs),
                    (None, Some(rhs)) => (lhs, rhs),
                    (Some(lhs), Some(rhs)) => (lhs, rhs),
                };
                IntegerExpression::IntegerComparison(lhs, op, rhs)
            }

            IntegerExpression::RealComparison(lhs, op, rhs) => {
                let mapped_lhs = self.map_real_expr(lhs, variables_to_replace);
                let mapped_rhs = self.map_real_expr(rhs, variables_to_replace);
                let (lhs, rhs) = match (mapped_lhs, mapped_rhs) {
                    (None, None) => return None,
                    (Some(lhs), None) => (lhs, rhs),
                    (None, Some(rhs)) => (lhs, rhs),
                    (Some(lhs), Some(rhs)) => (lhs, rhs),
                };
                IntegerExpression::RealComparison(lhs, op, rhs)
            }

            IntegerExpression::StringEq(arg1, arg2) => {
                let mapped_arg1 = self.map_str_expr(arg1, variables_to_replace);
                let mapped_arg2 = self.map_str_expr(arg2, variables_to_replace);
                let (arg1, arg2) = match (mapped_arg1, mapped_arg2) {
                    (None, None) => return None,
                    (Some(arg1), None) => (arg1, arg2),
                    (None, Some(arg2)) => (arg1, arg2),
                    (Some(arg1), Some(arg2)) => (arg1, arg2),
                };
                IntegerExpression::StringEq(arg1, arg2)
            }
            IntegerExpression::StringNEq(arg1, arg2) => {
                let mapped_arg1 = self.map_str_expr(arg1, variables_to_replace);
                let mapped_arg2 = self.map_str_expr(arg2, variables_to_replace);
                let mapped_arg2 = self.map_str_expr(arg2, variables_to_replace);
                let (arg1, arg2) = match (mapped_arg1, mapped_arg2) {
                    (None, None) => return None,
                    (Some(arg1), None) => (arg1, arg2),
                    (None, Some(arg2)) => (arg1, arg2),
                    (Some(arg1), Some(arg2)) => (arg1, arg2),
                };
                IntegerExpression::StringNEq(arg1, arg2)
            }

            IntegerExpression::RealCast(expr) => {
                let expr = self.map_real_expr(expr, variables_to_replace)?;
                IntegerExpression::RealCast(expr)
            }

            IntegerExpression::Literal(_)
            | IntegerExpression::ParamGiven(_)
            | IntegerExpression::PortConnected(_)
            | IntegerExpression::ParameterReference(_)
            | IntegerExpression::PortReference(_)
            | IntegerExpression::NetReference(_) => return None,
        };
        Some(self.push(self[expr].clone_as(new_expr)))
    }

    pub fn map_str_expr(
        &mut self,
        expr: StringExpressionId<'tag>,
        variables_to_replace: &FxHashMap<VariableId<'tag>, VariableId<'tag>>,
    ) -> Option<StringExpressionId<'tag>> {
        let new_expr = match self[expr].contents {
            StringExpression::VariableReference(var) => {
                let new_var = *variables_to_replace.get(&var)?;
                StringExpression::VariableReference(new_var)
            }

            StringExpression::Condition(condition, question_span, arg1, colon_span, arg2) => {
                let mapped_arg1 = self.map_str_expr(arg1, variables_to_replace);
                let mapped_arg2 = self.map_str_expr(arg2, variables_to_replace);
                let mapped_condition = self.map_int_expr(condition, variables_to_replace);
                if mapped_arg1.is_none() && mapped_arg2.is_none() && mapped_condition.is_none() {
                    return None;
                }
                let arg1 = mapped_arg1.unwrap_or(arg1);
                let arg2 = mapped_arg2.unwrap_or(arg2);
                let condition = mapped_condition.unwrap_or(condition);
                StringExpression::Condition(condition, question_span, arg1, colon_span, arg2)
            }

            StringExpression::SimParam(name) => {
                StringExpression::SimParam(self.map_str_expr(expr, variables_to_replace)?)
            }
            StringExpression::Literal(_) | StringExpression::ParameterReference(_) => return None,
        };
        Some(self.push(self[expr].clone_as(new_expr)))
    }
}
