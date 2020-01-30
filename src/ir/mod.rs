use crate::compact_arena::{Idx16, Idx8};

#[macro_use]
pub mod ast;
#[macro_use]
pub mod hir;

macro_rules! impl_id_type {
    ($name:ident($index_type:ident): $type:ty; in $container:ident::$sub_container:ident) => {
        impl<'tag> ::std::ops::Index<$name<'tag>> for $container<'tag> {
            type Output = $type;
            fn index(&self, index: $name<'tag>) -> &Self::Output {
                &self.$sub_container[index.0]
            }
        }
        impl<'tag> Index<&Range<$name<'tag>>> for $container<'tag> {
            type Output = [$type];
            fn index(&self, range: &Range<$name<'tag>>) -> &Self::Output {
                let range = range as *const Range<$name<'tag>>;
                let range = unsafe { &*(range as *const Range<$index_type<'tag>>) };
                &self.$sub_container[range]
            }
        }
        impl<'tag> Index<&Option<Range<$name<'tag>>>> for $container<'tag> {
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
        impl<'tag> Push<$type> for $container<'tag> {
            type Key = $name<'tag>;
            fn push(&mut self, val: $type) -> Self::Key {
                $name(self.$sub_container.add(val))
            }
        }
    };
}
pub trait Push<T> {
    type Key;
    fn push(&mut self, value: T) -> Self::Key;
}

#[derive(Copy, Clone, PartialOrd, PartialEq, Eq)]
#[repr(transparent)]
pub struct BranchId<'tag>(Idx8<'tag>);
#[derive(Clone, Copy, PartialOrd, Eq, PartialEq)]
#[repr(transparent)]
pub struct NetId<'tag>(Idx16<'tag>);
#[derive(Clone, Copy, PartialOrd, Eq, PartialEq)]
#[repr(transparent)]
pub struct PortId<'tag>(Idx8<'tag>);
#[derive(Clone, Copy, PartialOrd, Eq, PartialEq)]
#[repr(transparent)]
pub struct VariableId<'tag>(Idx16<'tag>);
#[derive(Clone, Copy, PartialOrd, Eq, PartialEq)]
#[repr(transparent)]
pub struct ModuleId<'tag>(Idx8<'tag>);
#[derive(Clone, Copy, PartialOrd, Eq, PartialEq)]
#[repr(transparent)]
pub struct FunctionId<'tag>(Idx8<'tag>);
#[derive(Clone, Copy, PartialOrd, Eq, PartialEq)]
#[repr(transparent)]
pub struct DisciplineId<'tag>(Idx8<'tag>);
#[derive(Clone, Copy, PartialOrd, Eq, PartialEq)]
#[repr(transparent)]
pub struct ExpressionId<'tag>(Idx16<'tag>);
#[derive(Clone, Copy, PartialOrd, Eq, PartialEq)]
#[repr(transparent)]
pub struct BlockId<'tag>(Idx8<'tag>);
#[derive(Clone, Copy, PartialOrd, Eq, PartialEq)]
#[repr(transparent)]
pub struct AttributeId<'tag>(Idx16<'tag>);
#[derive(Clone, Copy, PartialOrd, Eq, PartialEq)]
#[repr(transparent)]
pub struct StatementId<'tag>(Idx16<'tag>);
