/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

pub use crate::ids::IdRange;

use crate::ids::{AttributeId, BranchId, ExpressionId, NodeId, ParameterId};
use core::fmt::Debug;
use session::sourcemap::{Span, StringLiteral};
use session::symbols::{kw, sysfun, Ident, Symbol};
use std::ops::Range;

pub type ConstVal = osdi_types::ConstVal<StringLiteral>;

pub use osdi_types::{SimpleConstVal, SimpleType, Type, TypeInfo};
use std::fmt;
use std::fmt::{Display, Formatter};

#[macro_use]
pub mod ids;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum Math1 {
    Sqrt,
    Exp(bool),
    Ln,
    Log,
    Clog2,

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

impl Math1 {
    pub const fn symbol(&self) -> Symbol {
        match self {
            Self::Sqrt => kw::sqrt,
            Self::Exp(false) => kw::exp,
            Self::Exp(true) => kw::limexp,
            Self::Ln => kw::ln,
            Self::Log => kw::log,
            Self::Clog2 => sysfun::clog2,
            Self::Abs => kw::abs,
            Self::Floor => kw::floor,
            Self::Ceil => kw::ceil,
            Self::Sin => kw::sin,
            Self::Cos => kw::cos,
            Self::Tan => kw::tan,
            Self::ArcSin => kw::asin,
            Self::ArcCos => kw::acos,
            Self::ArcTan => kw::atan,
            Self::SinH => kw::sinh,
            Self::CosH => kw::cosh,
            Self::TanH => kw::tanh,
            Self::ArcSinH => kw::asinh,
            Self::ArcCosH => kw::acosh,
            Self::ArcTanH => kw::atanh,
        }
    }

    pub const fn ty(&self) -> Type {
        match self {
            Self::Clog2 => Type::INT,
            _ => Type::REAL,
        }
    }
}

impl Display for Math1 {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.symbol())
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum Math2 {
    Pow,
    Hypot,
    Min,
    Max,
    ArcTan2,
}

impl Math2 {
    pub const fn symbol(&self) -> Symbol {
        match self {
            Self::Pow => kw::pow,
            Self::Hypot => kw::hypot,
            Self::Min => kw::min,
            Self::Max => kw::max,
            Self::ArcTan2 => kw::atan2,
        }
    }
}

impl Display for Math2 {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.symbol())
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum StopTask {
    Stop,
    Finish,
}

impl StopTask {
    pub const fn symbol(&self) -> Symbol {
        match self {
            Self::Stop => sysfun::stop,
            Self::Finish => sysfun::finish,
        }
    }
}

impl Display for StopTask {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.symbol())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Copy)]
pub enum Print {
    // strobe, display and write (write does not have a newline after are equivalent. write
    Strobe,
    Display,
    Write,
    Debug,
    Info,
    Warn,
    Err,
    Fatal,
}

impl Print {
    pub const fn symbol(&self) -> Symbol {
        match self {
            Self::Strobe => sysfun::strobe,
            Self::Display => sysfun::display,
            Self::Write => sysfun::write,
            Self::Debug => sysfun::debug,
            Self::Info => sysfun::info,
            Self::Warn => sysfun::warning,
            Self::Err => sysfun::error,
            Self::Fatal => sysfun::fatal,
        }
    }
}

impl Display for Print {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.symbol())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Spanned<T> {
    pub span: Span,
    pub contents: T,
}

impl<T> Spanned<T> {
    pub fn new(contents: T, source: Span) -> Self {
        Self { contents, span: source }
    }
}

impl<T: Copy> Spanned<T> {
    pub fn copy_as<X>(self, contents: X) -> Spanned<X> {
        Spanned { span: self.span, contents }
    }
}
impl<T> Spanned<T> {
    pub fn clone_as<X>(&self, contents: X) -> Spanned<X> {
        Spanned { span: self.span, contents }
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
        assert!(len < u8::MAX as usize, "Only up to 255 attributes per object are supported");
        #[allow(clippy::cast_possible_truncation)]
        Self { start, len: len as u8 }
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

    pub const EMPTY: Self = Self { start: AttributeId::from_raw_unchecked(0), len: 0 };
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
pub struct AttrSpanned<T> {
    pub attributes: Attributes,
    pub span: Span,
    pub contents: T,
}

impl<T: Copy + Clone> AttrSpanned<T> {
    #[inline]
    pub fn copy_with<X: Clone>(self, f: impl FnOnce(T) -> X) -> AttrSpanned<X> {
        AttrSpanned { attributes: self.attributes, span: self.span, contents: f(self.contents) }
    }

    #[inline]
    pub fn copy_as<X: Clone>(self, contents: X) -> AttrSpanned<X> {
        AttrSpanned { attributes: self.attributes, span: self.span, contents }
    }
}
impl<T> AttrSpanned<T> {
    #[inline]
    pub fn map_with<X>(&self, f: impl FnOnce(&T) -> X) -> AttrSpanned<X> {
        AttrSpanned { attributes: self.attributes, span: self.span, contents: f(&self.contents) }
    }

    #[inline]
    pub fn map<X>(&self, contents: X) -> AttrSpanned<X> {
        AttrSpanned { attributes: self.attributes, span: self.span, contents }
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
            UnaryOperator::BitNegate => f.write_str("^"),
            UnaryOperator::LogicNegate => f.write_str("!"),
            UnaryOperator::ArithmeticNegate => f.write_str("-"),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Unknown {
    Parameter(ParameterId),
    NodePotential(NodeId),
    BranchPotential(NodeId, NodeId),
    Flow(BranchId),
    Temperature,
}

impl Display for Unknown {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Unknown::Parameter(param) => write!(f, "{:?}", param),
            Unknown::NodePotential(node) => write!(f, "V({:?})", node),
            Unknown::BranchPotential(hi, lo) => write!(f, "V({:?},{:?})", hi, lo),
            Unknown::Flow(branch) => write!(f, "I({:?})", branch),
            Unknown::Temperature => write!(f, "$temperature"),
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum DisciplineAccess {
    Potential,
    Flow,
}

impl Display for DisciplineAccess {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Potential => f.write_str("pot"),
            Self::Flow => f.write_str("flow"),
        }
    }
}

pub type ParameterRangeConstraint<T> = Range<ParameterRangeConstraintBound<T>>;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ParameterRangeConstraintBound<T> {
    pub inclusive: bool,
    pub bound: T,
}

impl<T: Copy> ParameterRangeConstraintBound<T> {
    pub fn copy_with<N>(self, f: impl FnOnce(T) -> N) -> ParameterRangeConstraintBound<N> {
        ParameterRangeConstraintBound { inclusive: self.inclusive, bound: f(self.bound) }
    }

    pub fn copy_with_ref<N>(self, f: &mut impl FnMut(T) -> N) -> ParameterRangeConstraintBound<N> {
        ParameterRangeConstraintBound { inclusive: self.inclusive, bound: f(self.bound) }
    }

    pub fn try_copy_with<N>(
        self,
        f: impl FnOnce(T) -> Option<N>,
    ) -> Option<ParameterRangeConstraintBound<N>> {
        Some(ParameterRangeConstraintBound { inclusive: self.inclusive, bound: f(self.bound)? })
    }

    pub fn try_copy_with_ref<N>(
        self,
        f: &mut impl FnMut(T) -> Option<N>,
    ) -> Option<ParameterRangeConstraintBound<N>> {
        Some(ParameterRangeConstraintBound { inclusive: self.inclusive, bound: f(self.bound)? })
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
