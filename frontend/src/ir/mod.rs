/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

//here so that ids don't spam the struct section of this module in docs but can still be imported under the normal path
#[doc(no_inline)]
pub use ids::AttributeId;
#[doc(no_inline)]
pub use ids::BlockId;
#[doc(no_inline)]
pub use ids::BranchId;
#[doc(no_inline)]
pub use ids::DisciplineId;
#[doc(no_inline)]
pub use ids::ExpressionId;
#[doc(no_inline)]
pub use ids::FunctionId;
#[doc(no_inline)]
pub use ids::IntegerExpressionId;
#[doc(no_inline)]
pub use ids::ModuleId;
#[doc(no_inline)]
pub use ids::NatureId;
#[doc(no_inline)]
pub use ids::NetId;
#[doc(no_inline)]
pub use ids::ParameterId;
#[doc(no_inline)]
pub use ids::PortId;
#[doc(no_inline)]
pub use ids::RealExpressionId;
#[doc(no_inline)]
pub use ids::StatementId;
#[doc(no_inline)]
pub use ids::StringExpressionId;
#[doc(no_inline)]
pub use ids::VariableId;

use crate::ir::ids::IdRange;
use crate::sourcemap::Span;
use crate::symbol::Ident;
use bitflags::_core::convert::TryFrom;
use core::fmt::Debug;
use std::ops::Range;

#[macro_use]
pub mod ids;

#[macro_use]
pub mod ast;

pub mod hir;

pub mod mir;

#[macro_use]
pub mod cfg;

/// A Node of an IR. Contains a Span an addition to whatever that node holds
#[derive(Clone, Copy, Debug)]
pub struct Node<T> {
    pub span: Span,
    pub contents: T,
}

impl<T> Node<T> {
    pub fn new(contents: T, source: Span) -> Self {
        Self {
            contents,
            span: source,
        }
    }
}

impl<T: Copy> Node<T> {
    pub fn copy_as<X>(self, contents: X) -> Node<X> {
        Node {
            span: self.span,
            contents,
        }
    }
}
impl<T: Clone> Node<T> {
    pub fn clone_as<X>(&self, contents: X) -> Node<X> {
        Node {
            span: self.span,
            contents,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Attribute {
    pub ident: Ident,
    pub value: Option<ExpressionId>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
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
#[derive(Clone, Copy, Debug)]
pub struct AttributeNode<T> {
    pub attributes: Attributes,
    pub span: Span,
    pub contents: T,
}

impl<T: Copy + Clone> AttributeNode<T> {
    #[inline]
    pub fn copy_with<X: Clone>(self, f: impl FnOnce(T) -> X) -> AttributeNode<X> {
        AttributeNode {
            attributes: self.attributes,
            span: self.span,
            contents: f(self.contents),
        }
    }

    #[inline]
    pub fn copy_as<X: Clone>(self, contents: X) -> AttributeNode<X> {
        AttributeNode {
            attributes: self.attributes,
            span: self.span,
            contents,
        }
    }
}
impl<T> AttributeNode<T> {
    #[inline]
    pub fn map_with<X>(&self, f: impl FnOnce(&T) -> X) -> AttributeNode<X> {
        AttributeNode {
            attributes: self.attributes,
            span: self.span,
            contents: f(&self.contents),
        }
    }

    #[inline]
    pub fn map<X>(&self, contents: X) -> AttributeNode<X> {
        AttributeNode {
            attributes: self.attributes,
            span: self.span,
            contents,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum BuiltInFunctionCall1p {
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

#[derive(Copy, Clone, Debug)]
pub enum BuiltInFunctionCall2p {
    Pow,
    Hypot,
    Min,
    Max,
    ArcTan2,
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

// TODO add system to generalise (dynamically add more)
// TODO add a way to constant fold these
#[derive(Clone, Debug)]
pub enum SystemFunctionCall<RealExpr, StrExpr, Port, Parameter> {
    Temperature,
    Vt(Option<RealExpr>),
    Simparam(StrExpr, Option<RealExpr>),
    SimparamStr(StrExpr),
    PortConnected(Port),
    ParameterGiven(Parameter),
}

#[derive(Clone, Debug, Copy)]
pub enum StopTaskKind {
    Stop,
    Finish,
}

#[derive(Clone, Debug, Copy)]
pub enum PrintOnFinish {
    Nothing,
    Location,
    LocationAndResourceUsage,
}
impl TryFrom<u32> for PrintOnFinish {
    type Error = ();

    fn try_from(finish_number: u32) -> Result<Self, Self::Error> {
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NumericalParameterRangeBound<T> {
    pub inclusive: bool,
    pub bound: T,
}
impl<T: Copy> NumericalParameterRangeBound<T> {
    pub fn copy_with<N>(self, f: impl FnOnce(T) -> N) -> NumericalParameterRangeBound<N> {
        NumericalParameterRangeBound {
            inclusive: self.inclusive,
            bound: f(self.bound),
        }
    }

    pub fn copy_with_ref<N>(self, f: &mut impl FnMut(T) -> N) -> NumericalParameterRangeBound<N> {
        NumericalParameterRangeBound {
            inclusive: self.inclusive,
            bound: f(self.bound),
        }
    }

    pub fn try_copy_with<N>(
        self,
        f: impl FnOnce(T) -> Option<N>,
    ) -> Option<NumericalParameterRangeBound<N>> {
        Some(NumericalParameterRangeBound {
            inclusive: self.inclusive,
            bound: f(self.bound)?,
        })
    }

    pub fn try_copy_with_ref<N>(
        self,
        f: &mut impl FnMut(T) -> Option<N>,
    ) -> Option<NumericalParameterRangeBound<N>> {
        Some(NumericalParameterRangeBound {
            inclusive: self.inclusive,
            bound: f(self.bound)?,
        })
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum NumericalParameterRangeExclude<T> {
    Value(T),
    Range(Range<NumericalParameterRangeBound<T>>),
}

pub enum FunctionType {
    Real,
    Integer,
}

impl<T: Copy> NumericalParameterRangeExclude<T> {
    pub fn clone_with<N>(&self, mut f: impl FnMut(T) -> N) -> NumericalParameterRangeExclude<N> {
        match self {
            NumericalParameterRangeExclude::Value(val) => {
                NumericalParameterRangeExclude::Value(f(*val))
            }
            NumericalParameterRangeExclude::Range(range) => NumericalParameterRangeExclude::Range(
                range.start.copy_with_ref(&mut f)..range.end.copy_with_ref(&mut f),
            ),
        }
    }
    pub fn try_clone_with<N>(
        &self,
        mut f: impl FnMut(T) -> Option<N>,
    ) -> Option<NumericalParameterRangeExclude<N>> {
        Some(match self {
            NumericalParameterRangeExclude::Value(val) => {
                NumericalParameterRangeExclude::Value(f(*val)?)
            }
            NumericalParameterRangeExclude::Range(range) => NumericalParameterRangeExclude::Range(
                range.start.try_copy_with_ref(&mut f)?..range.end.try_copy_with_ref(&mut f)?,
            ),
        })
    }
}
