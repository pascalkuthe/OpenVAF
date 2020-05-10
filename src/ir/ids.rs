//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

//! All IRS use preallocated Arenas for each node type. These Ids and their implementations are generated using the `id_type!` macro in this module.
//! The [`impl_id_type!`](impl_id_type) is also defined here which provides the implementation necessary for an ID type to interact with an IR

use super::*;

macro_rules! id_type {
    ($name:ident($type:ident)) => {
        #[derive(Copy, Clone, PartialOrd, PartialEq, Eq, Debug, Hash)]
        #[repr(transparent)]
        pub struct $name<'tag>(pub(super) $type<'tag>);

        impl<'tag> $crate::compact_arena::Step for $name<'tag> {
            type I = <$type<'tag> as $crate::compact_arena::Step>::I;
            #[inline]
            unsafe fn step(&mut self, distance: Self::I) {
                self.0.step(distance)
            }
            #[inline]
            unsafe fn step_back(&mut self, distance: Self::I) {
                self.0.step_back(distance)
            }
        }
        impl<'tag> ::std::convert::Into<$type<'tag>> for $name<'tag> {
            fn into(self) -> $type<'tag> {
                self.0
            }
        }
        impl<'tag> $name<'tag> {
            pub fn unwrap(self) -> $type<'tag> {
                self.0
            }

            pub unsafe fn from_raw_index(id: usize) -> Self {
                Self($type::new_from_usize(id))
            }

            pub fn as_usize(self) -> usize {
                self.0.index() as usize
            }
        }
        impl<'tag> $crate::compact_arena::SafeRange<$name<'tag>> {
            pub fn unwrap(self) -> $crate::compact_arena::SafeRange<$type<'tag>> {
                self.into()
            }
        }
        impl<'tag> ::core::fmt::Display for $name<'tag> {
            fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> std::fmt::Result {
                self.0.fmt(f)
            }
        }
        impl<'tag> ::std::convert::Into<$crate::compact_arena::SafeRange<$type<'tag>>>
            for $crate::compact_arena::SafeRange<$name<'tag>>
        {
            fn into(self) -> $crate::compact_arena::SafeRange<$type<'tag>> {
                $crate::compact_arena::SafeRange::new(
                    unsafe { self.get_start() }.0,
                    unsafe { self.get_end() }.0,
                )
            }
        }
    };
}

id_type!(BranchId(Idx8));

id_type!(NetId(Idx16));

id_type!(PortId(Idx8));

id_type!(ParameterId(Idx16));

id_type!(VariableId(Idx16));

id_type!(ModuleId(Idx8));

id_type!(FunctionId(Idx8));

id_type!(DisciplineId(Idx8));

id_type!(ExpressionId(Idx16));
id_type!(RealExpressionId(Idx16));
id_type!(IntegerExpressionId(Idx16));
id_type!(StringExpressionId(Idx8));

id_type!(BlockId(Idx16));

id_type!(AttributeId(Idx16));

id_type!(StatementId(Idx16));

id_type!(NatureId(Idx8));

/// Provides the implementation which allows the data of an IR to be accessed using an ID type generated using the `id_type!` macro.
///
///
/// # Arguments
///
/// * `$name` - the identifier of the id type
///
/// * `$container` - the identifier of the IR
///
/// * `$sub_container` - the field of the ir which contains the [Arena](crate::compact_arena) this ID indexes
///
/// * `$type` - The type of the values that are indexed by this id
///
/// # Examples
///
/// ```
/// id_type!(NetId(Idx8));
/// impl_id_type!(NetId in Ast::nets -> AttributeNode<'tag,Net>);
/// ```

#[macro_export]
macro_rules! impl_id_type {
    ($name:ident in $container:ident::$sub_container:ident -> $type:ty) => {
        impl<'tag> ::std::ops::Index<$name<'tag>> for $container<'tag> {
            type Output = $type;
            fn index(&self, index: $name<'tag>) -> &Self::Output {
                &self.$sub_container[index.0]
            }
        }
        impl<'tag> ::std::ops::Index<Range<$name<'tag>>> for $container<'tag> {
            type Output = [$type];
            fn index(&self, range: Range<$name<'tag>>) -> &Self::Output {
                let range = $crate::compact_arena::SafeRange::new(range.start.0, range.end.0);
                &self.$sub_container[range]
            }
        }
        impl<'tag> ::std::ops::Index<$crate::compact_arena::SafeRange<$name<'tag>>>
            for $container<'tag>
        {
            type Output = [$type];
            fn index(&self, range: $crate::compact_arena::SafeRange<$name<'tag>>) -> &Self::Output {
                let range = unsafe {
                    $crate::compact_arena::SafeRange::new(range.get_start().0, range.get_end().0)
                };
                &self.$sub_container[range]
            }
        }

        impl<'tag> ::std::ops::IndexMut<$name<'tag>> for $container<'tag> {
            fn index_mut(&mut self, index: $name<'tag>) -> &mut Self::Output {
                &mut self.$sub_container[index.0]
            }
        }
        impl<'tag> $crate::ir::UnsafeWrite<$name<'tag>> for $container<'tag> {
            type Data = $type;
            unsafe fn write_unsafe(&mut self, index: $name<'tag>, value: Self::Data) {
                self.$sub_container
                    .write(index.0, ::core::mem::MaybeUninit::new(value))
            }
        }
        impl<'tag> ::std::ops::IndexMut<Range<$name<'tag>>> for $container<'tag> {
            fn index_mut(&mut self, range: Range<$name<'tag>>) -> &mut Self::Output {
                let range = $crate::compact_arena::SafeRange::new(range.start.0, range.end.0);
                &mut self.$sub_container[range]
            }
        }
        impl<'tag> ::std::ops::IndexMut<$crate::compact_arena::SafeRange<$name<'tag>>>
            for $container<'tag>
        {
            fn index_mut(
                &mut self,
                range: $crate::compact_arena::SafeRange<$name<'tag>>,
            ) -> &mut Self::Output {
                let range = unsafe {
                    $crate::compact_arena::SafeRange::new(range.get_start().0, range.get_end().0)
                };
                &mut self.$sub_container[range]
            }
        }
        impl<'tag> $crate::ir::Push<$type> for $container<'tag> {
            type Key = $name<'tag>;
            fn push(&mut self, val: $type) -> Self::Key {
                $name(self.$sub_container.add(val))
            }
        }
        impl<'tag> $crate::ir::SafeRangeCreation<$name<'tag>> for $container<'tag> {
            fn range_to_end(&self, from: $name<'tag>) -> SafeRange<$name<'tag>> {
                let range = self.$sub_container.range_to_end(from.0);
                unsafe {
                    $crate::compact_arena::SafeRange::new(
                        $name(range.get_start()),
                        $name(range.get_end()),
                    )
                }
            }
            fn empty_range_from_end(&self) -> SafeRange<$name<'tag>> {
                let range = self.$sub_container.empty_range_from_end();
                unsafe {
                    $crate::compact_arena::SafeRange::new(
                        $name(range.get_start()),
                        $name(range.get_end()),
                    )
                }
            }
            fn extend_range_to_end(&self, range: SafeRange<$name<'tag>>) -> SafeRange<$name<'tag>> {
                let range = self.$sub_container.extend_range_to_end(range.into());
                unsafe {
                    $crate::compact_arena::SafeRange::new(
                        $name(range.get_start()),
                        $name(range.get_end()),
                    )
                }
            }
            fn full_range(&self) -> SafeRange<$name<'tag>> {
                let range = self.$sub_container.full_range();
                unsafe {
                    $crate::compact_arena::SafeRange::new(
                        $name(range.get_start()),
                        $name(range.get_end()),
                    )
                }
            }
        }
    };
}
