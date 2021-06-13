/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

//! All IRS use preallocated Arenas for each node type. These Ids and their implementations are generated using the `id_type!` macro in this module.
//! The [`impl_id_type!`](impl_id_type) is also defined here which provides the implementation necessary for an ID type to interact with an IR

// IndexVec macro is a bit unpretty atm
#![allow(clippy::doc_markdown)]
#![allow(clippy::used_underscore_binding)]

use more_asserts::assert_ge;
use openvaf_data_structures::index_vec::Idx;
use std::fmt::Debug;
use std::iter;
use std::ops::Range;

pub type IdxRangeIter<I> = iter::Map<Range<usize>, fn(usize) -> I>;

#[macro_export]
macro_rules! id_type {
    ($name:ident($type:ident)) => {
        // see the index_vec documentation
        ::openvaf_data_structures::index_vec::define_index_type! {
            pub struct $name = $type;

            DISPLAY_FORMAT = "{}";

            DEBUG_FORMAT = stringify!(<$name {}>);

            IMPL_RAW_CONVERSIONS = true;
        }
    };
}

id_type!(SyntaxCtx(u32));

impl SyntaxCtx {
    pub const ROOT: Self = Self::from_raw_unchecked(0);
}

id_type!(BranchId(u16));
id_type!(PortBranchId(u16));

id_type!(NetId(u16));

id_type!(PortId(u16));

id_type!(ParameterId(u32));

id_type!(VariableId(u32));

id_type!(ModuleId(u16));

id_type!(FunctionId(u16));

id_type!(DisciplineId(u16));

id_type!(ExpressionId(u32));
id_type!(RealExpressionId(u32));
id_type!(IntegerExpressionId(u32));
id_type!(StringExpressionId(u32));

id_type!(BlockId(u16));

id_type!(AttributeId(u16));

id_type!(StatementId(u32));

id_type!(NatureId(u16));

#[derive(Clone, Debug)]
pub struct IdRange<I: Idx>(pub Range<I>);

impl<I: Idx> IdRange<I> {
    pub fn enter_back(&mut self, sub_range: &Self) -> Self {
        if cfg!(debug_assertions) && self.0.start != self.0.end {
            assert_eq!(self.0.end, sub_range.0.end);
            assert_ge!(sub_range.0.start, self.0.start);
        }

        self.0.end = sub_range.0.start;

        sub_range.clone()
    }

    pub fn contains(&self, id: I) -> bool {
        self.0.start <= id && id < self.0.end
    }

    pub fn is_empty(&self) -> bool {
        self.0.start == self.0.end
    }
}

impl<I: Idx> Iterator for IdRange<I> {
    type Item = I;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.0.start < self.0.end {
            let res = self.0.start;
            self.0.start = I::from_usize(self.0.start.index() + 1);
            Some(res)
        } else {
            None
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }

    #[inline]
    fn count(self) -> usize {
        self.len()
    }

    #[inline]
    fn last(mut self) -> Option<Self::Item> {
        self.next_back()
    }

    #[inline]
    fn max(self) -> Option<Self::Item> {
        self.last()
    }

    #[inline]
    fn min(self) -> Option<Self::Item> {
        Some(self.0.start)
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        let new_start = I::from_usize(self.0.start.index() + n);
        if new_start < self.0.end {
            self.0.start = I::from_usize(new_start.index() + 1);
            Some(new_start)
        } else {
            self.0.start = self.0.end;
            None
        }
    }
}

impl<I: Idx> ExactSizeIterator for IdRange<I> {
    fn len(&self) -> usize {
        self.0.end.index() - self.0.start.index()
    }
}

impl<I: Idx> DoubleEndedIterator for IdRange<I> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.0.start < self.0.end {
            self.0.end = I::from_usize(self.0.end.index() - 1);
            Some(self.0.end)
        } else {
            None
        }
    }

    fn nth_back(&mut self, n: usize) -> Option<Self::Item> {
        let new_end = I::from_usize(self.0.end.index() - n);
        if self.0.start < new_end {
            self.0.end = I::from_usize(new_end.index() - 1);
            Some(new_end)
        } else {
            self.0.end = self.0.start;
            None
        }
    }
}

/// Provides the implementation which allows the data of an IR to be accessed using an ID type generated using the `id_type!` macro.
///
///
/// # Arguments
///
/// * `$name` - the identifier of the id type
///
/// * `$container` - the identifier of the IR
///
/// * `$sub_container` - the field of the ir which contains the [Arena](openvaf_data_structures::index_vec::IndexVec) this ID indexes
///
/// * `$type` - The type of the values that are indexed by this id
///
/// # Examples
///
/// ``` compile_fail
/// id_type!(NetId(u8));
/// impl_id_type!(NetId in Ast::nets -> AttributeNode<Net>);
/// ```

#[macro_export]
macro_rules! impl_id_type {
    ($name:ident in $container:ty => $sub_container:ident as $type:ty) => {
        impl_id_type!($name in $container => $sub_container as $type where);
    };

    ($name:ident in $container:ty => $sub_container:ident as $type:ty where $($generics:tt)*) => {
        impl$($generics)* ::std::ops::Index<$name> for $container {
            type Output = $type;
            fn index(&self, index: $name) -> &Self::Output {
                &self.$sub_container[index]
            }
        }

        impl$($generics)* ::std::ops::IndexMut<$name> for $container {
            fn index_mut(&mut self, index: $name) -> &mut Self::Output {
                &mut self.$sub_container[index]
            }
        }

        impl$($generics)* ::std::ops::Index<::core::ops::Range<$name>> for $container {
            type Output = openvaf_data_structures::index_vec::IndexSlice<$name, [$type]>;
            fn index(&self, range: ::core::ops::Range<$name>) -> &Self::Output {
                &self.$sub_container[range]
            }
        }

        impl$($generics)* ::std::ops::IndexMut<::core::ops::Range<$name>> for $container {
            fn index_mut(&mut self, range: ::core::ops::Range<$name>) -> &mut Self::Output {
                &mut self.$sub_container[range]
            }
        }

        impl$($generics)* ::std::ops::Index<$crate::ids::IdRange<$name>> for $container {
            type Output = ::openvaf_data_structures::index_vec::IndexSlice<$name, [$type]>;
            fn index(&self, range: $crate::ids::IdRange<$name>) -> &Self::Output {
                &self.$sub_container[range.0]
            }
        }

        impl$($generics)* ::std::ops::IndexMut<$crate::ids::IdRange<$name>> for $container {
            fn index_mut(&mut self, range: $crate::ids::IdRange<$name>) -> &mut Self::Output {
                &mut self.$sub_container[range.0]
            }
        }
    };
}
