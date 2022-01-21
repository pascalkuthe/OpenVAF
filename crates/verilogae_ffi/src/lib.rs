pub mod ffi;
#[cfg(test)]
mod tests;

use core::slice;
use std::marker::PhantomData;
use std::ptr;

pub use ffi::*;

#[cfg(not(windows))]
pub type NativePath = Slice<u8>;

#[cfg(windows)]
pub type NativePath = Slice<u16>;

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

pub struct VfsExport(ffi::Vfs);

impl VfsExport {
    #[inline]
    pub fn new(path: NativePath, opts: &Opts) -> Option<VfsExport> {
        let raw = unsafe { verilogae_export_vfs(path, opts.to_ffi()) };
        if raw.ptr.is_null() {
            return None;
        }
        Some(VfsExport(raw))
    }

    pub fn entries(&self) -> impl Iterator<Item = (&str, &str)> + '_ {
        let raw = unsafe { slice::from_raw_parts(self.0.ptr, self.0.len) };
        raw.iter().map(|entry| unsafe {
            let path = slice::from_raw_parts(entry.name.ptr, entry.name.len);
            let path = std::str::from_utf8_unchecked(path);

            let contents = slice::from_raw_parts(entry.data.ptr, entry.data.len);
            let contents = std::str::from_utf8_unchecked(contents);

            (path, contents)
        })
    }
}

impl Drop for VfsExport {
    #[inline]
    fn drop(&mut self) {
        unsafe { verilogae_free_vfs(self.0) }
    }
}

#[derive(Default)]
pub struct Opts(Option<&'static mut ffi::Opts>);

impl Opts {
    pub fn to_ffi(&self) -> *mut ffi::Opts {
        match &self.0 {
            Some(opts) => *opts as *const ffi::Opts as *mut ffi::Opts,
            None => ptr::null_mut(),
        }
    }

    /// # Safety
    /// Only valid pointers may be written into the return value
    ///
    /// # Note
    /// The slices themselves are deallocated on drop (owned by this struct)
    /// Their contents (strings) are owned by the calle. Its his responsability to deallocate them
    pub unsafe fn write(&mut self) -> &mut ffi::Opts {
        match &mut self.0 {
            Some(res) => res,
            dst => {
                let opts = ffi::verilogae_new_opts();
                *dst = Some(&mut *opts);
                &mut *opts
            }
        }
    }
}

impl Drop for Opts {
    fn drop(&mut self) {
        if let Some(opts) = self.0.take() {
            unsafe {
                opts.cache_dir.into_box_opt();
                opts.include_dirs.into_box_opt();
                opts.macro_flags.into_box_opt();
                opts.allow_lints.into_box_opt();
                opts.warn_lints.into_box_opt();
                opts.deny_lints.into_box_opt();
                opts.cg_flags.into_box_opt();
                opts.vfs.into_box_opt();
            }
            unsafe { ffi::verilogae_free_opts(opts as *mut ffi::Opts) }
        }
    }
}

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
