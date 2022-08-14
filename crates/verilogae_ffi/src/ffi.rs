use std::marker::PhantomData;
use std::{ptr, slice};

pub use generated::*;

mod generated;

#[cfg(not(windows))]
pub type NativePath = Slice<u8>;

#[cfg(windows)]
pub type NativePath = Slice<u16>;

impl<T> From<Box<[T]>> for Slice<T> {
    fn from(raw: Box<[T]>) -> Self {
        let len = raw.len();
        let ptr = Box::into_raw(raw);
        Self { ptr: ptr as *mut T, len, _phantom_0: PhantomData }
    }
}

impl<T> Default for Slice<T> {
    fn default() -> Self {
        Self { ptr: ptr::null_mut(), len: 0, _phantom_0: PhantomData }
    }
}

impl<T> Slice<T> {
    pub fn from_raw_parts(ptr: *const T, len: usize) -> Self {
        Self { ptr: ptr as *mut T, len, _phantom_0: PhantomData }
    }
    /// # Safety
    /// Pointer must be valid for reads
    /// Pointer must be allocated by rust
    pub unsafe fn into_box(self) -> Box<[T]> {
        let raw = std::ptr::slice_from_raw_parts_mut(self.ptr, self.len);
        Box::from_raw(raw)
    }

    /// # Safety
    /// Pointer must be valid for reads or null
    /// Pointer must be allocated by rust
    pub unsafe fn into_box_opt(self) -> Option<Box<[T]>> {
        if self.ptr.is_null() {
            None
        } else {
            Some(self.into_box())
        }
    }

    /// # Safety
    /// Pointer must be valid for reads
    pub unsafe fn read(&self) -> &[T] {
        slice::from_raw_parts(self.ptr, self.len)
    }
}

impl<T: 'static> From<&'_ [T]> for Slice<T> {
    fn from(src: &'_ [T]) -> Self {
        Self { ptr: src.as_ptr() as *mut T, len: src.len(), _phantom_0: PhantomData }
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
pub union Meta<T: Copy> {
    pub stride: u64,
    pub scalar: T,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct FatPtr<T: Copy> {
    pub ptr: *mut T,
    pub meta: Meta<T>,
}

impl<T: Copy> FatPtr<T> {
    #[inline(always)]
    pub fn set_scalar(&mut self, val: T) {
        self.ptr = std::ptr::null_mut();
        self.meta.scalar = val
    }

    #[inline(always)]
    pub fn set_ptr(&mut self, ptr: *mut T, stride: isize) {
        self.ptr = ptr;
        self.meta.stride = stride as u64;
    }
}
