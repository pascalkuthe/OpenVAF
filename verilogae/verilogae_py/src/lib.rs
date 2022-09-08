#[macro_use]
mod offsets;
#[macro_use]
mod ffi;
mod load;
mod model;
mod numpy;
mod typeref;
mod unicode;
mod util;

use std::os::raw::{c_char, c_int};
use std::ptr;

use crate::load::{load_info_py, load_py, load_vfs};
use crate::model::{VAE_FUNCTION_TY, VAE_MODEL_TY, VAE_PARAM_TY};
use crate::typeref::init_typerefs;
use pyo3_ffi::*;

#[cfg(Py_3_8)]
const FUN_FLAG: c_int = METH_FASTCALL;
#[cfg(not(Py_3_8))]
const FUN_FLAG: c_int = METH_VARARGS;

static mut FUNCTIONS: [PyMethodDef; 4] = unsafe {
    [
    PyMethodDef {
            ml_name: "load\0".as_ptr() as *const c_char,
            #[cfg(Py_3_8)]
            ml_meth: PyMethodDefPointer{_PyCFunctionFastWithKeywords: load_py},
            #[cfg(not(Py_3_8))]
            ml_meth: PyMethodDefPointer{PyCFunctionWithKeywords: load_py},
            ml_flags: FUN_FLAG | METH_KEYWORDS,
            ml_doc: "loads a Verilog-A model by either loading it from the object cache or compiling it\0".as_ptr() as *const c_char,
    },
    PyMethodDef {
            ml_name: "load_info\0".as_ptr() as *const c_char,
            #[cfg(Py_3_8)]
            ml_meth: PyMethodDefPointer{_PyCFunctionFastWithKeywords: load_info_py},
            #[cfg(not(Py_3_8))]
            ml_meth: PyMethodDefPointer{PyCFunctionWithKeywords: load_info_py},
            ml_flags: FUN_FLAG | METH_KEYWORDS,
            ml_doc: "loads information about Verilog-A model by either loading it from the object cache or compiling it\nThis funciton does not compile retrieved funcitons.\nThis allows for much faster compile times.\nModelsCompiled with this function lack the `functions` attribute.\0".as_ptr() as *const c_char,
    },
    PyMethodDef {
            ml_name: "export_vfs\0".as_ptr() as *const c_char,
            #[cfg(Py_3_8)]
            ml_meth: PyMethodDefPointer{_PyCFunctionFastWithKeywords: load_vfs},
            #[cfg(not(Py_3_8))]
            ml_meth: PyMethodDefPointer{PyCFunctionWithKeywords: load_vfs},
            ml_flags: FUN_FLAG | METH_KEYWORDS,
            ml_doc: "runs the preprocessor on a Verilog-A file and exports a dict with all files.\nThe result of this functions can be passed to other functions `vfs` argument\0".as_ptr() as *const c_char,
    },
    zero!(PyMethodDef)
]
};

#[allow(clippy::missing_safety_doc)]
#[allow(non_snake_case)]
#[no_mangle]
#[cold]
pub unsafe extern "C" fn PyInit_verilogae() -> *mut PyObject {
    let init = PyModuleDef {
        m_base: PyModuleDef_HEAD_INIT,
        m_name: "verilogae\0".as_ptr() as *const c_char,
        m_doc: std::ptr::null(),
        m_size: 0,
        m_methods: FUNCTIONS.as_mut_ptr(),
        m_slots: std::ptr::null_mut(),
        m_traverse: None,
        m_clear: None,
        m_free: None,
    };

    if PyType_Ready(&mut VAE_MODEL_TY) < 0 {
        return ptr::null_mut();
    }

    if PyType_Ready(&mut VAE_FUNCTION_TY) < 0 {
        return ptr::null_mut();
    }

    if PyType_Ready(&mut VAE_PARAM_TY) < 0 {
        return ptr::null_mut();
    }

    let mptr = PyModule_Create(Box::into_raw(Box::new(init)));
    init_typerefs();
    let version = env!("CARGO_PKG_VERSION");
    PyModule_AddObject(
        mptr,
        "__version__\0".as_ptr() as *const c_char,
        PyUnicode_FromStringAndSize(version.as_ptr() as *const c_char, version.len() as isize),
    );

    let all = ["__all__\0", "__version__\0", "load\0", "load_info\0", "export_vfs\0"];

    let pyall = PyTuple_New(all.len() as isize);
    for (i, obj) in all.iter().enumerate() {
        PyTuple_SET_ITEM(
            pyall,
            i as isize,
            PyUnicode_InternFromString(obj.as_ptr() as *const c_char),
        )
    }

    PyModule_AddObject(mptr, "__all__\0".as_ptr() as *const c_char, pyall);

    mptr
}
