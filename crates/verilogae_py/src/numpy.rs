use std::ffi::c_void;
use std::fmt::Display;
use std::os::raw::{c_char, c_int};

use pyo3_ffi::{PyObject, PyObject_GetAttr, PyTypeObject, Py_DECREF, Py_intptr_t, Py_ssize_t};

use crate::typeref::ARRAY_STRUCT_STR;

// https://docs.scipy.org/doc/numpy/reference/arrays.interface.html#c.__array_struct__

#[repr(C)]
struct PyArrayInterface {
    pub two: c_int,
    pub nd: c_int,
    pub typekind: c_char,
    pub itemsize: c_int,
    pub flags: c_int,
    pub shape: *mut Py_intptr_t,
    pub strides: *mut Py_intptr_t,
    pub data: *mut c_void,
    pub descr: *mut PyObject,
}

#[repr(C)]
struct PyCapsule {
    pub ob_refcnt: Py_ssize_t,
    pub ob_type: *mut PyTypeObject,
    pub pointer: *mut c_void,
    pub name: *const c_char,
    pub context: *mut c_void,
    pub destructor: *mut c_void, // should be typedef void (*PyCapsule_Destructor)(PyObject *);
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ItemType {
    Float,
    Int,
    Long,
}

impl Display for ItemType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ItemType::Float => write!(f, "float64"),
            ItemType::Int => write!(f, "int32"),
            ItemType::Long => write!(f, "int64"),
        }
    }
}

impl ItemType {
    fn find(array: *mut PyArrayInterface) -> Option<ItemType> {
        match unsafe { ((*array).typekind, (*array).itemsize) } {
            (102, 8) => Some(ItemType::Float),
            (105 | 117, 4) => Some(ItemType::Int),
            (105 | 117, 8) => Some(ItemType::Long),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum PyArrayError {
    Malformed,
    UnsupportedDataType,
}

pub struct NumpyArray {
    array: *mut PyArrayInterface,
    capsule: *mut PyCapsule,
    pub kind: ItemType,
}

impl<'a> NumpyArray {
    // #[inline(never)]
    pub fn new(ptr: *mut PyObject) -> Result<Self, PyArrayError> {
        let capsule = unsafe { PyObject_GetAttr(ptr, ARRAY_STRUCT_STR) };
        let array = unsafe { (*(capsule as *mut PyCapsule)).pointer as *mut PyArrayInterface };
        if unsafe { (*array).two != 2 } {
            unsafe { Py_DECREF(capsule) };
            Err(PyArrayError::Malformed)
        } else {
            let num_dimensions = unsafe { (*array).nd as usize };
            if num_dimensions != 1 {
                unsafe { Py_DECREF(capsule) };
                return Err(PyArrayError::UnsupportedDataType);
            }

            let stride = unsafe { *(*array).strides };
            let itemsize = unsafe { (*array).itemsize };

            if stride % itemsize as isize != 0 {
                return Err(PyArrayError::UnsupportedDataType);
            }

            match ItemType::find(array) {
                None => Err(PyArrayError::UnsupportedDataType),
                Some(kind) => {
                    let pyarray = NumpyArray { array, capsule: capsule as *mut PyCapsule, kind };
                    Ok(pyarray)
                }
            }
        }
    }

    pub fn data(&self) -> *mut c_void {
        unsafe { (*self.array).data }
    }

    pub fn len(&self) -> isize {
        unsafe { *(*self.array).shape }
    }

    pub fn stride(&self) -> isize {
        unsafe { *(*self.array).strides / (*self.array).itemsize as isize }
    }
}

impl Drop for NumpyArray {
    fn drop(&mut self) {
        // unsafe { Py_DECREF(self.array as *mut pyo3_ffi::PyObject) } orjson does this but i am
        // 99% sure this is wrong
        unsafe { Py_DECREF(self.capsule as *mut pyo3_ffi::PyObject) }
    }
}
