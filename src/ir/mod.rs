/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
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

use crate::compact_arena::{Idx16, Idx8, SafeRange};
use crate::symbol::Ident;
use crate::Span;

#[macro_use]
pub mod ids;

#[macro_use]
pub mod ast;

pub mod hir;

pub mod mir;

/// Allows adding elements to IRS
///
/// # Examples
///
/// ## Adding an attriubte to an ast and reading it using its ID
///
/// ```
/// # use VARF::ir::{Node, Attribute};
/// # use VARF::ir::ast::Statement;
/// # use VARF::symbol::Ident;
/// mk_ast!(ast);
/// let id = ast.push(Attribute{name: Ident::from_str("foo"),value:None });
/// assert_eq!(ast[id].name,Ident::from_str("foo"));
///
/// ```
///
///
pub trait Push<T> {
    type Key;
    fn push(&mut self, value: T) -> Self::Key;
}

/// Allows creating ranges of IDs for an IR. [`SafeRange`](crate::compact_arena::SafeRange)s are returned instead of normal [`Range`](std::ops::Range)s.
/// See the documentation of [`SafeRange`](crate::compact_arena::SafeRange) for more details.
pub trait SafeRangeCreation<Key: Copy + Clone> {
    ///  Creates a range from `start` to the last Element of this type
    ///
    /// # Examples
    /// ```
    /// # use VARF::ir::{Node, Attribute};
    /// # use VARF::ir::ast::Statement;
    /// # use VARF::symbol::Ident;
    /// mk_ast!(ast);
    /// let foo = ast.push(Attribute{name: Ident::from_str("foo"),value:None });
    /// let bar = ast.push(Attribute{name: Ident::from_str("bar"),value:None });
    /// let range = ast.range_to_end(foo);
    /// assert_eq!(bar,range.next());
    /// assert_eq!(None,range.next());
    ///
    /// ```
    fn range_to_end(&self, start: Key) -> SafeRange<Key>;

    /// Extends a range to to the last Element of this type: `x..y`-> `x..end`
    #[inline]
    fn extend_range_to_end(&self, range: SafeRange<Key>) -> SafeRange<Key> {
        //this is save since the result will be a SafeRange once again
        self.range_to_end(unsafe { range.get_start() })
    }
    /// Creates an empty range (start = end) starting at the last Element of the IR
    fn empty_range_from_end(&self) -> SafeRange<Key>;

    /// Returns a range over all elements of the IR
    ///
    /// # Examples
    /// ```
    /// # use VARF::ir::{Node, Attribute};
    /// # use VARF::ir::ast::Statement;
    /// # use VARF::symbol::Ident;
    /// mk_ast!(ast);
    /// let id = ast.push(Attribute{name: Ident::from_str("foo"),value:None });
    /// let range = ast.full_range();
    /// assert_eq!(id,range.next());
    /// assert_eq!(None,range.next());
    ///
    /// ```
    fn full_range(&self) -> SafeRange<Key>;
}

/// A trait implemented for all id types to abstract over writing to unitized memory (the = operator is not save since drop will be called)
/// This is an unsafe trait since extra care has to be taken when using this during the folding process for types that implement drop:
/// * This has to be called for every ID that will be assumed initialized later (even when errors occur)
/// * This may cause memory leaks if this is used to write using an old id
pub(crate) trait UnsafeWrite<Idx> {
    type Data;
    unsafe fn write_unsafe(&mut self, idx: Idx, val: Self::Data);
}

/// For most types in this crate drop is not implemented. For those types `UnsafeWrite` is always save.
/// This crate is implimented for those types.
pub(crate) trait Write<Idx> {
    //This should be !Drop but negative trait bounds aren't implemented yet.
    // While there could be drop implementations for copy types that's very rarely the case (not at all in this crate) so this will do for now
    type Data: Copy;
    fn write(&mut self, idx: Idx, val: Self::Data);
}

/// A Node of an IR. Contains a Span an addition to whatever that node holds
#[derive(Clone, Copy, Debug)]
pub struct Node<T> {
    pub source: Span,
    pub contents: T,
}

impl<T> Node<T> {
    pub fn new(contents: T, source: Span) -> Self {
        Self { contents, source }
    }
}

impl<T: Copy> Node<T> {
    pub fn copy_as<X>(self, contents: X) -> Node<X> {
        Node {
            source: self.source,
            contents,
        }
    }
}
impl<T: Clone> Node<T> {
    pub fn clone_as<X>(&self, contents: X) -> Node<X> {
        Node {
            source: self.source,
            contents,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Attribute<'tag> {
    pub name: Ident,
    pub value: Option<ExpressionId<'tag>>,
}

pub type Attributes<'ast> = SafeRange<AttributeId<'ast>>;

/// A special type of IR Node. Contains a Span and attributes in addition to whatever that node holds
#[derive(Clone, Copy, Debug)]
pub struct AttributeNode<'ast, T> {
    pub attributes: Attributes<'ast>,
    pub source: Span,
    pub contents: T,
}

impl<'tag, T: Copy + Clone> AttributeNode<'tag, T> {
    #[inline]
    pub fn copy_with<X: Clone>(self, f: impl FnOnce(T) -> X) -> AttributeNode<'tag, X> {
        AttributeNode {
            attributes: self.attributes,
            source: self.source,
            contents: f(self.contents),
        }
    }

    #[inline]
    pub fn copy_as<X: Clone>(self, contents: X) -> AttributeNode<'tag, X> {
        AttributeNode {
            attributes: self.attributes,
            source: self.source,
            contents,
        }
    }
}
impl<'tag, T> AttributeNode<'tag, T> {
    #[inline]
    pub fn map_with<X>(&self, f: impl FnOnce(&T) -> X) -> AttributeNode<'tag, X> {
        AttributeNode {
            attributes: self.attributes,
            source: self.source,
            contents: f(&self.contents),
        }
    }

    #[inline]
    pub fn map<X>(&self, contents: X) -> AttributeNode<'tag, X> {
        AttributeNode {
            attributes: self.attributes,
            source: self.source,
            contents,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum BuiltInFunctionCall1p {
    Sqrt,
    Exp,
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
