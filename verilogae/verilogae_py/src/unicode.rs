use std::fmt::Debug;

use pyo3_ffi::*;
use verilogae_ffi::Slice;

use crate::typeref::PATHLIB_PATH;

pub struct OsStr {
    py: *mut PyObject,
    pub data: Slice<u8>,
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

    fn new(py: *mut PyObject) -> Option<OsStr> {
        unsafe {
            Py_INCREF(py);
            let mut size = 0;
            let ptr = PyUnicode_AsUTF8AndSize(py, &mut size) as *const u8;
            if ptr.is_null() {
                return None;
            }
            let data = Slice::from_raw_parts(ptr, size as usize);
            Some(OsStr { py, data })
        }
    }

    // #[cfg(windows)]
    // pub fn new(py: *mut PyObject) -> Option<OsStr> {
    //     // Get an owned allocated wide char buffer from PyString, which we have to deallocate
    //     // ourselves
    //     let size = unsafe { PyUnicode_AsWideChar(py, std::ptr::null_mut(), 0) };
    //     if size == -1 {
    //         return None;
    //     }

    //     let mut buffer = vec![0; size as usize];
    //     let bytes_read = unsafe { PyUnicode_AsWideChar(py, buffer.as_mut_ptr(), size) };
    //     assert_eq!(bytes_read, size);

    //     let len = buffer.len();
    //     let ptr = Box::into_raw(buffer.into_boxed_slice()) as *mut u16;

    //     // Copy wide char buffer into OsString

    //     Some(OsStr { data: NativePath::from_raw_parts(ptr, len) })
    // }
}

impl Drop for OsStr {
    fn drop(&mut self) {
        unsafe { Py_DecRef(self.py) }
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
