use crate::compact_arena::{Idx16, Idx8};
use crate::util::SafeRange;

macro_rules! impl_id_type_for_container {
    ($name:ident($index_type:ident): $type:ty; in $container:ident::$sub_container:ident) => {
        impl<'tag> ::std::ops::Index<$name<'tag>> for $container<'tag> {
            type Output = $type;
            fn index(&self, index: $name<'tag>) -> &Self::Output {
                &self.$sub_container[index.into()]
            }
        }
        impl<'tag> ::std::ops::Index<Range<$name<'tag>>> for $container<'tag> {
            type Output = [$type];
            fn index(&self, range: &Range<$name<'tag>>) -> &Self::Output {
                &self.$sub_container[range.into()]
            }
        }
        impl<'tag> ::std::ops::Index<$crate::compact_arena::SafeRange<$name<'tag>>>
            for $container<'tag>
        {
            type Output = [$type];
            fn index(
                &self,
                range: &$crate::compact_arena::SafeRange<$name<'tag>>,
            ) -> &Self::Output {
                &self.$sub_container[range.into()]
            }
        }

        impl<'tag> ::std::ops::IndexMut<$name<'tag>> for $container<'tag> {
            fn index_mut(&mut self, index: $name<'tag>) -> &mut Self::Output {
                &mut self.$sub_container[index.into()]
            }
        }
        impl<'tag> ::std::ops::IndexMut<Range<$name<'tag>>> for $container<'tag> {
            fn index(&self, range: &Range<$name<'tag>>) -> &mut Self::Output {
                &mut self.$sub_container[range.into()]
            }
        }
        impl<'tag> ::std::ops::IndexMut<$crate::compact_arena::SafeRange<$name<'tag>>>
            for $container<'tag>
        {
            fn index(
                &self,
                range: &$crate::compact_arena::SafeRange<$name<'tag>>,
            ) -> &mut Self::Output {
                &mut self.$sub_container[range.into()]
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

macro_rules! id_type {
    ($name:ident($type:ident)) => {
        #[derive(Copy, Clone, PartialOrd, PartialEq, Eq)]
        #[repr(transparent)]
        pub struct $name<'tag>($type<'tag>);

        impl<'tag> $crate::util::Step for $name<'tag> {
            unsafe fn step(&mut self) {
                self.0.add(1)
            }
        }
        impl<'tag> ::std::convert::Into<$type<'tag>> for $name<'tag> {
            fn into(self) -> $type<'tag> {
                self.0
            }
        }
    };
}

#[macro_use]
pub mod ast;
#[macro_use]
pub mod hir;

id_type!(BranchId(Idx8));
id_type!(NetId(Idx16));
id_type!(PortId(Idx8));
id_type!(VariableId(Idx16));
id_type!(ModuleId(Idx8));
id_type!(FunctionId(Idx8));
id_type!(DisciplineId(Idx8));
id_type!(ExpressionId(Idx16));
id_type!(BlockId(Idx8));
id_type!(AttributeId(Idx16));
id_type!(StatementId(Idx16));
