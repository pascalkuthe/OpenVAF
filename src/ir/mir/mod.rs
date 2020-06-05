/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::UnaryOperator;

use crate::hir::{BranchDeclaration, Discipline, DisciplineAccess, Net, Port};
use crate::ir::cfg::ControlFlowGraph;
use crate::ir::hir::Hir;
use crate::ir::ids::{IdRange, StringExpressionId};
use crate::ir::*;
use crate::literals::StringLiteral;
use crate::symbol::Ident;
use crate::Span;
use index_vec::IndexVec;
use rustc_hash::FxHashMap;
use std::mem::take;
use std::ops::Range;

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
}

impl Mir {
    /// # Safety
    /// You should never call this yourself
    pub(crate) fn initalize(hir: &mut Hir) -> Self {
        Self {
            // Nothing about these changes during HIR lowering we can just copy
            branches: take(&mut hir.branches),
            nets: take(&mut hir.nets),
            ports: take(&mut hir.ports),
            disciplines: take(&mut hir.disciplines),

            parameters: Default::default(),
            variables: Default::default(),
            modules: Default::default(),
            natures: Default::default(),
            real_expressions: Default::default(),
            integer_expressions: Default::default(),
            string_expressions: Default::default(),
            attributes: Default::default(),
            statements: Default::default(),
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
    pub name: Ident,
    pub variable_type: VariableType,
}

#[derive(Copy, Clone, Debug)]
pub struct Attribute {
    pub name: Ident,
    pub value: Option<ExpressionId>,
}

#[derive(Copy, Clone)]
pub struct Nature {
    pub name: Ident,
    pub abstol: f64,
    pub units: StringLiteral,
    pub access: Ident,
    pub idt_nature: NatureId,
    pub ddt_nature: NatureId,
}

#[derive(Debug)]
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

#[derive(Clone)]
pub enum Statement {
    Contribute(Attributes, DisciplineAccess, BranchId, RealExpressionId),
    //  TODO IndirectContribute
    Assignment(Attributes, VariableId, ExpressionId),
}

#[derive(Clone, Debug)]
pub struct Parameter {
    pub name: Ident,
    pub parameter_type: ParameterType,
}

impl ParameterType {
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
    pub fn source(self, mir: &Mir) -> Span {
        match self {
            Self::Real(id) => mir[id].source,
            Self::Integer(id) => mir[id].source,
            Self::String(id) => mir[id].source,
        }
    }
}

impl ExpressionId {
    pub fn is_real(self) -> bool {
        matches!(self,Self::Real{..})
    }

    pub fn is_integer(self) -> bool {
        matches!(self,Self::Integer{..})
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
        Span,
        IntegerExpressionId,
        Span,
        IntegerExpressionId,
    ),
    RealCast(RealExpressionId),
    Literal(i64),
    VariableReference(VariableId),
    NetReference(NetId),
    PortReference(PortId),
    ParameterReference(ParameterId),
    FunctionCall(FunctionId, Vec<ExpressionId>),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntegerUnaryOperator {
    BitNegate,
    LogicNegate,
    ArithmeticNegate,
    ExplicitPositive,
}

#[derive(Clone, Debug)]
pub enum RealExpression {
    BinaryOperator(RealExpressionId, Node<RealBinaryOperator>, RealExpressionId),
    Negate(Span, RealExpressionId),
    Condition(
        IntegerExpressionId,
        Span,
        RealExpressionId,
        Span,
        RealExpressionId,
    ),
    Literal(f64),
    VariableReference(VariableId),
    ParameterReference(ParameterId),
    BranchAccess(DisciplineAccess, BranchId, u8),
    Noise(NoiseSource<RealExpressionId, ()>, Option<StringLiteral>),
    BuiltInFunctionCall1p(BuiltInFunctionCall1p, RealExpressionId),
    BuiltInFunctionCall2p(BuiltInFunctionCall2p, RealExpressionId, RealExpressionId),
    IntegerConversion(IntegerExpressionId),

    Temperature,
    Vt(Option<RealExpressionId>),
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
    Condition(
        IntegerExpressionId,
        Span,
        StringExpressionId,
        Span,
        StringExpressionId,
    ),

    Literal(StringLiteral),
    SimParam(StringExpressionId),

    VariableReference(VariableId),

    ParameterReference(ParameterId),
}

impl Mir {
    pub fn map_real_expr(
        &mut self,
        expr: RealExpressionId,
        variables_to_replace: &FxHashMap<VariableId, VariableId>,
    ) -> Option<RealExpressionId> {
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
        Some(self.real_expressions.push(self[expr].clone_as(new_expr)))
    }

    pub fn map_int_expr(
        &mut self,
        expr: IntegerExpressionId,
        variables_to_replace: &FxHashMap<VariableId, VariableId>,
    ) -> Option<IntegerExpressionId> {
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
        Some(self.integer_expressions.push(self[expr].clone_as(new_expr)))
    }

    pub fn map_str_expr(
        &mut self,
        expr: StringExpressionId,
        variables_to_replace: &FxHashMap<VariableId, VariableId>,
    ) -> Option<StringExpressionId> {
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
                StringExpression::SimParam(self.map_str_expr(name, variables_to_replace)?)
            }
            StringExpression::Literal(_) | StringExpression::ParameterReference(_) => return None,
        };
        Some(self.string_expressions.push(self[expr].clone_as(new_expr)))
    }
}
