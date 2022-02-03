// SPDX-License-Identifier: (Apache-2.0 OR MIT)

use std::fmt::Debug;
use std::marker::PhantomData;

#[cfg(windows)]
use libc::wchar_t;
use pyo3_ffi::*;
use verilogae_ffi::NativePath;

use crate::ffi::{PyBytes_AS_STRING, PyBytes_GET_SIZE};
use crate::typeref::PATHLIB_PATH;

// #[derive(Debug)]
pub struct OsStr {
    #[cfg(not(windows))]
    py: *mut PyObject,
    pub data: NativePath,
}

impl OsStr {
    pub fn new_path(py: *mut PyObject) -> Option<Option<OsStr>> {
        match unsafe { PyObject_IsInstance(py, PATHLIB_PATH as *mut PyObject) } {
            1 => {
                let py = unsafe { PyObject_Str(py) };
                let res = OsStr::new(py);
                unsafe { Py_XDECREF(py) };
                Some(res)
            }

            0 => Some(OsStr::new(py)),
            _ => None,
        }
    }

    #[cfg(not(windows))]
    pub fn new(py: *mut PyObject) -> Option<OsStr> {
        // Decode from Python's lossless bytes string representation back into raw bytes
        let fs_encoded_bytes = unsafe { PyUnicode_EncodeFSDefault(py) };
        if fs_encoded_bytes.is_null() {
            return None;
        }
        let ptr = unsafe { PyBytes_AS_STRING(fs_encoded_bytes) as *mut u8 };
        let len = unsafe { PyBytes_GET_SIZE(fs_encoded_bytes) as usize };

        Some(OsStr { py: fs_encoded_bytes, data: NativePath { ptr, len, _phantom_0: PhantomData } })
    }

    #[cfg(windows)]
    pub fn new(py: *mut PyObject) -> Option<OsStr> {
        // Get an owned allocated wide char buffer from PyString, which we have to deallocate
        // ourselves
        let size = unsafe { PyUnicode_AsWideChar(py, std::ptr::null_mut(), 0) };
        if size == -1 {
            return None;
        }

        let mut buffer = vec![0; size as usize];
        let bytes_read = unsafe { PyUnicode_AsWideChar(py, buffer.as_mut_ptr(), size) };
        assert_eq!(bytes_read, size);

        let len = buffer.len();
        let ptr = Box::into_raw(buffer.into_boxed_slice()) as *mut wchar_t;

        // Copy wide char buffer into OsString

        Some(OsStr { data: NativePath { ptr, len, _phantom_0: PhantomData } })
    }
}

impl Drop for OsStr {
    fn drop(&mut self) {
        #[cfg(not(windows))]
        unsafe {
            Py_DecRef(self.py)
        }
    }
}

impl Debug for OsStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let slice = unsafe {
            std::str::from_utf8(std::slice::from_raw_parts(self.data.ptr, self.data.len)).unwrap()
        };
        write!(f, "{}", slice)
    }
}
