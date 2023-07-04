#[cfg(not(feature = "static"))]
mod ffi;
#[cfg(feature = "static")]
use verilogae::api as ffi;

pub use ffi::*;

#[cfg(feature = "static")]
pub const PARAM_FLAGS_MIN_INCLUSIVE: ParamFlags = 1;
#[cfg(feature = "static")]
pub const PARAM_FLAGS_MAX_INCLUSIVE: ParamFlags = 2;
#[cfg(feature = "static")]
pub const PARAM_FLAGS_INVALID: ParamFlags = 4;
#[cfg(feature = "static")]
pub const PARAM_FLAGS_GIVEN: ParamFlags = 8;

use core::slice;
use std::ptr;

pub struct VfsExport(ffi::Vfs);

impl VfsExport {
    #[inline]
    pub fn new(path: &[u8], opts: &Opts) -> Option<VfsExport> {
        let raw = unsafe { verilogae_export_vfs(path.into(), opts.to_ffi()) };
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
    /// Their contents (strings) are owned by the caller. Its his responsibility to deallocate them
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
