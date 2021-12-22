use std::marker::PhantomData;

enum Opaque {}

#[repr(C)]
pub(crate) struct InvariantOpaque<'a> {
    _marker: PhantomData<&'a mut &'a ()>,
    _opaque: Opaque,
}
