use std::mem::size_of;
use std::os::raw::c_ulong;

use pyo3_ffi::*;

#[allow(non_snake_case)]
#[inline(always)]
pub unsafe fn PyDict_GET_SIZE(op: *mut PyObject) -> Py_ssize_t {
    (*op.cast::<PyDictObject>()).ma_used
}

// #[cfg(Py_3_8)]
// const PY_TPFLAGS_HAVE_VECTORCALL: c_ulonglong = pyo3_ffi::Py_TPFLAGS_HAVE_VECTORCALL;
// #[cfg(not(Py_3_8))]
// const Py_TPFLAGS_HAVE_VECTORCALL: c_ulonglong = 0;

#[cfg(Py_3_10)]
const PY_TPFLAGS_IMMUTABLETYPE: c_ulong = pyo3_ffi::Py_TPFLAGS_IMMUTABLETYPE;
#[cfg(not(Py_3_10))]
const PY_TPFLAGS_IMMUTABLETYPE: c_ulong = 0;

const TY_FLAGS: c_ulong = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE | PY_TPFLAGS_IMMUTABLETYPE;
// | Py_TPFLAGS_HAVE_VECTORCALL;

macro_rules! zero {
    ($ty:ty) => {{
        union Init {
            data: $ty,
            raw: [u8; ::std::mem::size_of::<$ty>()],
        }
        Init { raw: [0; ::std::mem::size_of::<$ty>()] }.data
    }};
}

// manual implemtation of PyVarObject_HEAD_INIT macro
pub const fn new_type<T>() -> PyTypeObject {
    let mut res = unsafe { zero!(PyTypeObject) };
    res.ob_base.ob_base.ob_refcnt = 1;
    res.tp_basicsize = size_of::<T>() as isize;
    res.tp_flags = TY_FLAGS;

    res
}

macro_rules! ob_type {
    ($obj:expr) => {
        (*$obj).ob_type
    };
}
