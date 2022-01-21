use core::fmt;
use std::error::Error;
use std::ffi::{CStr, CString};
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;

use libc::c_char;

use crate::{LLVMCreateMessage, LLVMDisposeMessage};

/// An owned LLVM String. Also known as a LLVM Message
#[derive(Eq)]
#[repr(transparent)]
pub struct LLVMString {
    pub(crate) ptr: *const c_char,
}

impl LLVMString {
    /// # Safety
    /// This functions requires a string that was allocated by LLVM!
    pub unsafe fn new(ptr: *const c_char) -> Self {
        LLVMString { ptr }
    }

    // /// This is a convenience method for creating a Rust `String`,
    // /// however; it *will* reallocate. `LLVMString` should be used
    // /// as much as possible to save memory since it is allocated by
    // /// LLVM. It's essentially a `CString` with a custom LLVM
    // /// deallocator
    // pub fn to_string(&self) -> String {
    //     (*self).to_string_lossy().into_owned()
    // }

    // /// This method will allocate a c string through LLVM
    pub(crate) fn create_from_str(string: &str) -> LLVMString {
        let msg = CString::new(string).unwrap();
        unsafe { LLVMString::new(LLVMCreateMessage(msg.as_ptr() as *const _)) }
    }

    /// This method will allocate a c string through LLVM
    pub fn create_from_c_str(string: &CStr) -> LLVMString {
        unsafe { LLVMString::new(LLVMCreateMessage(string.as_ptr() as *const _)) }
    }
}

impl Deref for LLVMString {
    type Target = CStr;

    fn deref(&self) -> &Self::Target {
        unsafe { CStr::from_ptr(self.ptr) }
    }
}

impl Debug for LLVMString {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self.deref())
    }
}

impl Display for LLVMString {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.deref().to_string_lossy())
    }
}

impl PartialEq for LLVMString {
    fn eq(&self, other: &LLVMString) -> bool {
        **self == **other
    }
}

impl Error for LLVMString {
    fn description(&self) -> &str {
        self.to_str().expect("Could not convert LLVMString to str (likely invalid unicode)")
    }

    fn cause(&self) -> Option<&dyn Error> {
        None
    }
}

impl Drop for LLVMString {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeMessage(self.ptr as *mut _);
        }
    }
}
