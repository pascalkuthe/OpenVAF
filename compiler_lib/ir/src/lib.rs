/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

pub use crate::ids::IdRange;

use crate::ids::{AttributeId, BranchId, ExpressionId, NetId, ParameterId};
use core::convert::TryFrom;
use core::fmt::Debug;
use openvaf_session::sourcemap::{Span, StringLiteral};
use openvaf_session::symbols::Ident;
use std::ops::Range;

pub type ConstVal = osdi_types::ConstVal<StringLiteral>;
pub use osdi_types::{SimpleConstVal, SimpleType, Type, TypeInfo};
use std::fmt;
use std::fmt::{Display, Formatter};

#[macro_use]
pub mod ids;
pub mod convert;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Spanned<T> {
    pub span: Span,
    pub contents: T,
}

impl<T> Spanned<T> {
    pub fn new(contents: T, source: Span) -> Self {
        Self {
            contents,
            span: source,
        }
    }
}

impl<T: Copy> Spanned<T> {
    pub fn copy_as<X>(self, contents: X) -> Spanned<X> {
        Spanned {
            span: self.span,
            contents,
        }
    }
}
impl<T: Clone> Spanned<T> {
    pub fn clone_as<X>(&self, contents: X) -> Spanned<X> {
        Spanned {
            span: self.span,
            contents,
        }
    }
}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.contents.fmt(f)
    }
}

#[derive(Clone, Debug)]
pub struct Attribute {
    pub ident: Ident,
    pub value: Option<ExpressionId>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Attributes {
    pub start: AttributeId,
    pub len: u8,
}

impl Attributes {
    #[must_use]
    pub fn new(start: AttributeId, end: AttributeId) -> Self {
        let len = end.index() - start.index();
        assert!(
            len < u8::MAX as usize,
            "Only up to 255 attributes per object are supported"
        );
        #[allow(clippy::cast_possible_truncation)]
        Self {
            start,
            len: len as u8,
        }
    }

    #[inline]
    #[must_use]
    pub fn as_range(self) -> Range<AttributeId> {
        self.start..self.end()
    }

    #[inline]
    #[must_use]
    pub fn end(self) -> AttributeId {
        self.start + (self.len as usize)
    }

    pub const EMPTY: Self = Self {
        start: AttributeId::from_raw_unchecked(0),
        len: 0,
    };
}

impl IntoIterator for Attributes {
    type Item = AttributeId;
    type IntoIter = IdRange<AttributeId>;

    fn into_iter(self) -> Self::IntoIter {
        IdRange(self.as_range())
    }
}

impl Default for Attributes {
    fn default() -> Self {
        Self::EMPTY
    }
}

/// A special type of IR Node. Contains a Span and attributes in addition to whatever that node holds
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Node<T> {
    pub attributes: Attributes,
    pub span: Span,
    pub contents: T,
}

impl<T: Copy + Clone> Node<T> {
    #[inline]
    pub fn copy_with<X: Clone>(self, f: impl FnOnce(T) -> X) -> Node<X> {
        Node {
            attributes: self.attributes,
            span: self.span,
            contents: f(self.contents),
        }
    }

    #[inline]
    pub fn copy_as<X: Clone>(self, contents: X) -> Node<X> {
        Node {
            attributes: self.attributes,
            span: self.span,
            contents,
        }
    }
}
impl<T> Node<T> {
    #[inline]
    pub fn map_with<X>(&self, f: impl FnOnce(&T) -> X) -> Node<X> {
        Node {
            attributes: self.attributes,
            span: self.span,
            contents: f(&self.contents),
        }
    }

    #[inline]
    pub fn map<X>(&self, contents: X) -> Node<X> {
        Node {
            attributes: self.attributes,
            span: self.span,
            contents,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
    BitNegate,
    LogicNegate,
    ArithmeticNegate,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::BitNegate => f.write_str("!"),
            UnaryOperator::LogicNegate => f.write_str("^"),
            UnaryOperator::ArithmeticNegate => f.write_str("-"),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Unknown {
    Parameter(ParameterId),
    NodePotential(NetId),
    Flow(BranchId),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum SingleArgMath {
    Sqrt,
    Exp(bool),
    Ln,
    Log,
    Abs,
    Floor,
    Ceil,

    Sin,
    Cos,
    Tan,

    ArcSin,
    ArcCos,
    ArcTan,

    SinH,
    CosH,
    TanH,

    ArcSinH,
    ArcCosH,
    ArcTanH,
}

impl Display for SingleArgMath {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

impl SingleArgMath {
    pub fn name(self) -> &'static str {
        match self {
            Self::Sqrt => "sqrt",
            Self::Exp(_) => "exp",
            Self::Ln => "ln",
            Self::Log => "log",
            Self::Abs => "abs",
            Self::Floor => "floor",
            Self::Ceil => "ceil",
            Self::Sin => "floor",
            Self::Cos => "cos",
            Self::Tan => "tan",
            Self::ArcSin => "asin",
            Self::ArcCos => "acos",
            Self::ArcTan => "atan",
            Self::SinH => "sinh",
            Self::CosH => "cosh",
            Self::TanH => "tanh",
            Self::ArcSinH => "asinh",
            Self::ArcCosH => "acosh",
            Self::ArcTanH => "atanh",
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum DoubleArgMath {
    Pow,
    Hypot,
    Min,
    Max,
    ArcTan2,
}

impl DoubleArgMath {
    pub const fn name(self) -> &'static str {
        match self {
            Self::Pow => "pow",
            Self::Hypot => "hypot",
            Self::Min => "min",
            Self::Max => "max",
            Self::ArcTan2 => "arctan2",
        }
    }
}

impl Display for DoubleArgMath {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

#[derive(Copy, Clone, Debug)]
pub enum NoiseSource<Expr, Table> {
    White(Expr),
    Flicker(Expr, Expr),
    Table(Table),
    TableLog(Table),
}

impl<Expr, Table> NoiseSource<Expr, Table> {
    pub fn fold<NewExpr, NewTable>(
        self,
        mut fold_expr: impl FnMut(Expr) -> NewExpr,
        mut fold_table: impl FnMut(Table) -> NewTable,
    ) -> NoiseSource<NewExpr, NewTable> {
        match self {
            NoiseSource::White(expr) => NoiseSource::White(fold_expr(expr)),
            NoiseSource::Flicker(expr1, expr2) => {
                NoiseSource::Flicker(fold_expr(expr1), fold_expr(expr2))
            }
            NoiseSource::Table(table) => NoiseSource::Table(fold_table(table)),
            NoiseSource::TableLog(table) => NoiseSource::TableLog(fold_table(table)),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Port {
    pub input: bool,
    pub output: bool,
    pub net: NetId,
}

// TODO add system to generalise (dynamically add more)
#[derive(Clone, Debug)]
pub enum SystemFunctionCall<Port, Parameter, BranchAccess, Fun> {
    Temperature,
    Vt(Option<ExpressionId>),
    Simparam(ExpressionId, Option<ExpressionId>),
    SimparamStr(ExpressionId),
    PortConnected(Port),
    ParameterGiven(Parameter),
    Lim {
        access: BranchAccess,
        fun: Fun,
        args: Vec<ExpressionId>,
    },
}

impl<Port, Parameter, BranchAccess, Fun> SystemFunctionCall<Port, Parameter, BranchAccess, Fun> {
    pub fn ty(&self) -> Type {
        match self {
            Self::Temperature | Self::Vt(_) | Self::Simparam(_, _) | Self::Lim { .. } => Type::REAL,
            Self::SimparamStr(_) => Type::STRING,
            Self::PortConnected(_) | Self::ParameterGiven(_) => Type::BOOL,
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum StopTaskKind {
    Stop,
    Finish,
}

impl Display for StopTaskKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

impl Debug for StopTaskKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl StopTaskKind {
    pub fn name(self) -> &'static str {
        match self {
            Self::Stop => "$stop",
            Self::Finish => "$finish",
        }
    }
}

#[derive(Clone, Debug, Copy, Eq, PartialEq)]
pub enum PrintOnFinish {
    Nothing,
    Location,
    LocationAndResourceUsage,
}

impl TryFrom<i64> for PrintOnFinish {
    type Error = ();

    fn try_from(finish_number: i64) -> Result<Self, Self::Error> {
        Ok(match finish_number {
            0 => Self::Nothing,
            1 => Self::Location,
            2 => Self::LocationAndResourceUsage,
            _ => return Err(()),
        })
    }
}
#[derive(Clone, Debug, Copy)]
pub enum DisplayTaskKind {
    // stprob, display and write (write does not have a newline after are equivalent. write
    Convergence(bool),
    Debug,
    Info,
    Warn,
    Error,
    Fatal(PrintOnFinish),
}

pub type ParameterRangeConstraint<T> = Range<ParameterRangeConstraintBound<T>>;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ParameterRangeConstraintBound<T> {
    pub inclusive: bool,
    pub bound: T,
}

impl<T: Copy> ParameterRangeConstraintBound<T> {
    pub fn copy_with<N>(self, f: impl FnOnce(T) -> N) -> ParameterRangeConstraintBound<N> {
        ParameterRangeConstraintBound {
            inclusive: self.inclusive,
            bound: f(self.bound),
        }
    }

    pub fn copy_with_ref<N>(self, f: &mut impl FnMut(T) -> N) -> ParameterRangeConstraintBound<N> {
        ParameterRangeConstraintBound {
            inclusive: self.inclusive,
            bound: f(self.bound),
        }
    }

    pub fn try_copy_with<N>(
        self,
        f: impl FnOnce(T) -> Option<N>,
    ) -> Option<ParameterRangeConstraintBound<N>> {
        Some(ParameterRangeConstraintBound {
            inclusive: self.inclusive,
            bound: f(self.bound)?,
        })
    }

    pub fn try_copy_with_ref<N>(
        self,
        f: &mut impl FnMut(T) -> Option<N>,
    ) -> Option<ParameterRangeConstraintBound<N>> {
        Some(ParameterRangeConstraintBound {
            inclusive: self.inclusive,
            bound: f(self.bound)?,
        })
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum ParameterExcludeConstraint<T> {
    Value(T),
    Range(Range<ParameterRangeConstraintBound<T>>),
}

pub enum FunctionType {
    Real,
    Integer,
}

impl<T> From<T> for ParameterExcludeConstraint<T> {
    fn from(val: T) -> Self {
        Self::Value(val)
    }
}

impl<T> From<Range<ParameterRangeConstraintBound<T>>> for ParameterExcludeConstraint<T> {
    fn from(val: Range<ParameterRangeConstraintBound<T>>) -> Self {
        Self::Range(val)
    }
}

impl<T: Copy> ParameterExcludeConstraint<T> {
    pub fn clone_with<N>(&self, mut f: impl FnMut(T) -> N) -> ParameterExcludeConstraint<N> {
        match self {
            ParameterExcludeConstraint::Value(val) => ParameterExcludeConstraint::Value(f(*val)),
            ParameterExcludeConstraint::Range(range) => ParameterExcludeConstraint::Range(
                range.start.copy_with_ref(&mut f)..range.end.copy_with_ref(&mut f),
            ),
        }
    }
    pub fn try_clone_with<N>(
        &self,
        mut f: impl FnMut(T) -> Option<N>,
    ) -> Option<ParameterExcludeConstraint<N>> {
        Some(match self {
            ParameterExcludeConstraint::Value(val) => ParameterExcludeConstraint::Value(f(*val)?),
            ParameterExcludeConstraint::Range(range) => ParameterExcludeConstraint::Range(
                range.start.try_copy_with_ref(&mut f)?..range.end.try_copy_with_ref(&mut f)?,
            ),
        })
    }
}
