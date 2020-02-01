use crate::compact_arena::{Idx16, Idx8};
use crate::util::SafeRange;

macro_rules! impl_id_type_for_container {
    ($name:ident($index_type:ident): $type:ty; in $container:ident::$sub_container:ident) => {
        impl<'tag> ::std::ops::Index<$name<'tag>> for $container<'tag> {
            type Output = $type;
            fn index(&self, index: $name<'tag>) -> &Self::Output {
                &self.$sub_container[index.0]
            }
        }
        impl<'tag> ::std::ops::Index<&Range<$name<'tag>>> for $container<'tag> {
            type Output = [$type];
            fn index(&self, range: &Range<$name<'tag>>) -> &Self::Output {
                let range = range as *const Range<$name<'tag>>;
                let range = unsafe { &*(range as *const Range<$index_type<'tag>>) };
                &self.$sub_container[range]
            }
        }
        impl<'tag> ::std::ops::Index<&Option<Range<$name<'tag>>>> for $container<'tag> {
            type Output = [$type];
            fn index(&self, range: &Option<Range<$name<'tag>>>) -> &Self::Output {
                if let Some(range) = range {
                    let range = range as *const Range<$name<'tag>>;
                    let range = unsafe { &*(range as *const Range<$index_type<'tag>>) };
                    &self.$sub_container[range]
                } else {
                    &[]
                }
            }
        }
        impl<'tag> ::std::ops::IndexMut<$name<'tag>> for $container<'tag> {
            fn index_mut(&mut self, index: $name<'tag>) -> &mut Self::Output {
                &mut self.$sub_container[index.0]
            }
        }
        impl<'tag> ::std::ops::IndexMut<&Range<$name<'tag>>> for $container<'tag> {
            fn index_mut(&mut self, range: &Range<$name<'tag>>) -> &mut Self::Output {
                let range = range as *const Range<$name<'tag>>;
                let range = unsafe { &*(range as *const Range<$index_type<'tag>>) };
                &mut self.$sub_container[range]
            }
        }
        impl<'tag> ::std::ops::IndexMut<&Option<Range<$name<'tag>>>> for $container<'tag> {
            fn index_mut(&mut self, range: &Option<Range<$name<'tag>>>) -> &mut Self::Output {
                if let Some(range) = range {
                    let range = range as *const Range<$name<'tag>>;
                    let range = unsafe { &*(range as *const Range<$index_type<'tag>>) };
                    &mut self.$sub_container[range]
                } else {
                    &mut []
                }
            }
        }
        impl<'tag> $crate::util::Push<$type> for $container<'tag> {
            type Key = $name<'tag>;
            fn push(&mut self, val: $type) -> Self::Key {
                $name(self.$sub_container.add(val))
            }
        }
    };
}

macro_rules! impl_id_type {
    ($name:ident) => {
        impl<'tag> $crate::util::Step for $name<'tag> {
            unsafe fn step(&mut self) {
                self.0.add(1)
            }
        }
    };
}
#[macro_use]
pub mod ast;
#[macro_use]
pub mod hir;

#[derive(Copy, Clone, PartialOrd, PartialEq, Eq)]
#[repr(transparent)]
pub struct BranchId<'tag>(Idx8<'tag>);
impl_id_type!(BranchId);
#[derive(Clone, Copy, PartialOrd, Eq, PartialEq)]
#[repr(transparent)]
pub struct NetId<'tag>(Idx16<'tag>);
impl_id_type!(NetId);
#[derive(Clone, Copy, PartialOrd, Eq, PartialEq)]
#[repr(transparent)]
pub struct PortId<'tag>(Idx8<'tag>);
impl_id_type!(PortId);
#[derive(Clone, Copy, PartialOrd, Eq, PartialEq)]
#[repr(transparent)]
pub struct VariableId<'tag>(Idx16<'tag>);
impl_id_type!(VariableId);
#[derive(Clone, Copy, PartialOrd, Eq, PartialEq)]
#[repr(transparent)]
pub struct ModuleId<'tag>(Idx8<'tag>);
impl_id_type!(ModuleId);
#[derive(Clone, Copy, PartialOrd, Eq, PartialEq)]
#[repr(transparent)]
pub struct FunctionId<'tag>(Idx8<'tag>);
impl_id_type!(FunctionId);
#[derive(Clone, Copy, PartialOrd, Eq, PartialEq)]
#[repr(transparent)]
pub struct DisciplineId<'tag>(Idx8<'tag>);
impl_id_type!(DisciplineId);
#[derive(Clone, Copy, PartialOrd, Eq, PartialEq)]
#[repr(transparent)]
pub struct ExpressionId<'tag>(Idx16<'tag>);
impl_id_type!(ExpressionId);
#[derive(Clone, Copy, PartialOrd, Eq, PartialEq)]
#[repr(transparent)]
pub struct BlockId<'tag>(Idx8<'tag>);
impl_id_type!(BlockId);
#[derive(Clone, Copy, PartialOrd, Eq, PartialEq)]
#[repr(transparent)]
pub struct AttributeId<'tag>(Idx16<'tag>);
impl_id_type!(AttributeId);
#[derive(Clone, Copy, PartialOrd, Eq, PartialEq)]
#[repr(transparent)]
pub struct StatementId<'tag>(Idx16<'tag>);
impl_id_type!(StatementId);
