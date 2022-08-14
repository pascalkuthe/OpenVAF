use libc::c_char;
use pyo3_ffi::*;
use verilogae_ffi::{verilogae_load, Opts, Slice, Vfs, VfsEntry, VfsExport};

use crate::ffi::PyDict_GET_SIZE;
use crate::model::VaeModel;
use crate::typeref;
use crate::unicode::OsStr;
use crate::util::unlikely;

use std::ptr;

macro_rules! handle_opt {
    ($fun: literal, $dst: expr, $arg: expr, $val: expr) => {
        if $arg == typeref::MODULE_STR {
            let mut size = 0;
            let model = PyUnicode_AsUTF8AndSize($val, &mut size) as *const u8;
            if unlikely(model.is_null()) {
                return raise_type_exception(concat!($fun, "() module kwarg is not a str"));
            }
            $dst.write().model = Slice::from_raw_parts(model, size as usize);
            true
        } else if $arg == typeref::VFS_STR {
            match py_to_vfs($fun, $val) {
                Some(vfs) => $dst.write().vfs = vfs,
                None => return ptr::null_mut(),
            }
            true
        } else {
            false
        }
    };
}

#[cfg(Py_3_8)]
macro_rules! parse_args {
    ($fun: literal, $args: ident, $nargs: ident, $kwnames: ident, $path: ident, $opts: ident) => {
        let num_args = pyo3_ffi::PyVectorcall_NARGS($nargs as usize);
        if unlikely(num_args == 0) {
            return raise_type_exception(concat!(
                $fun,
                "() missing 1 required positional argument: 'path'"
            ));
        }

        if unlikely(num_args != 1) {
            return raise_type_exception(concat!(
                $fun,
                "() found unexpected positional argument (expected 1)"
            ));
        }

        let $path = match OsStr::new_path(*$args) {
            Some(Some(path)) => path,
            Some(None) => {
                return raise_type_exception(concat!(
                    $fun,
                    "() positional argument 'path' must be a pathlib Path or str"
                ))
            }
            None => return ptr::null_mut(),
        };

        let mut $opts = Opts::default();

        if !$kwnames.is_null() {
            let len = PyTuple_GET_SIZE($kwnames).saturating_sub(1);
            for i in 0..=len {
                let arg = PyTuple_GET_ITEM($kwnames, i as Py_ssize_t);
                if !handle_opt!($fun, $opts, arg, *$args.offset(num_args + i)) {
                    return raise_type_exception(concat!(
                        $fun,
                        "() got an unexpected keyword argument"
                    ));
                }
            }
        }
    };
}

#[cfg(not(Py_3_8))]
macro_rules! parse_args {
    ($fun: literal, $args: ident, $kwargs: ident, $path: ident, $opts: ident) => {
        let path = PyTuple_GET_ITEM($args, 0);

        let num_args = PyTuple_GET_SIZE($args);
        if unlikely(num_args == 0) {
            return raise_type_exception(concat!(
                $fun,
                "() missing 1 required positional argument: 'path'"
            ));
        }

        if unlikely(num_args != 1) {
            return raise_type_exception(concat!(
                $fun,
                "() found unexpected positional argument (expected 1)"
            ));
        }

        let $path = match OsStr::new_path(path) {
            Some(Some(path)) => path,
            Some(None) => {
                return raise_type_exception(concat!(
                    $fun,
                    "() positional argument 'path' must be a pathlib Path or str"
                ))
            }
            None => return ptr::null_mut(),
        };

        let mut $opts = Opts::default();

        if !$kwargs.is_null() {
            let len = crate::ffi::PyDict_GET_SIZE($kwargs);
            let mut pos = 0isize;
            let mut arg: *mut PyObject = std::ptr::null_mut();
            let mut val: *mut PyObject = std::ptr::null_mut();
            for _i in 0..=len {
                _PyDict_Next($kwargs, &mut pos, &mut arg, &mut val, std::ptr::null_mut());
                if !handle_opt!($fun, $opts, arg, val) {
                    if arg.is_null() {
                        break;
                    } else {
                        return raise_type_exception(concat!(
                            $fun,
                            "() got an unexpected keyword argument"
                        ));
                    }
                }
            }
        }
    };
}

#[cold]
#[inline(never)]
fn raise_type_exception(msg: &str) -> *mut PyObject {
    unsafe {
        let err_msg =
            PyUnicode_FromStringAndSize(msg.as_ptr() as *const c_char, msg.len() as isize);
        PyErr_SetObject(PyExc_TypeError, err_msg);
        Py_DECREF(err_msg);
    };
    std::ptr::null_mut()
}

#[cold]
#[inline(never)]
fn raise_runtime_runtime_exception(msg: &str) -> *mut PyObject {
    unsafe {
        let err_msg =
            PyUnicode_FromStringAndSize(msg.as_ptr() as *const c_char, msg.len() as isize);
        PyErr_SetObject(PyExc_RuntimeError, err_msg);
        Py_DECREF(err_msg);
    };
    std::ptr::null_mut()
}

#[cfg(not(Py_3_8))]
#[no_mangle]
pub unsafe extern "C" fn load_py(
    _self: *mut PyObject,
    args: *mut PyObject,
    kwds: *mut PyObject,
) -> *mut PyObject {
    parse_args!("load", args, kwds, path, opts);
    let model = verilogae_load(path.data, true, opts.to_ffi());

    if model.is_null() {
        return raise_runtime_runtime_exception("load() compilation failed");
    }

    VaeModel::new(model, true)
}

#[cfg(Py_3_8)]
#[no_mangle]
pub unsafe extern "C" fn load_py(
    _self: *mut PyObject,
    args: *const *mut PyObject,
    nargs: Py_ssize_t,
    kwnames: *mut PyObject,
) -> *mut PyObject {
    parse_args!("load", args, nargs, kwnames, path, opts);
    let model = verilogae_load(path.data, true, opts.to_ffi());

    if model.is_null() {
        return raise_runtime_runtime_exception("load() compilation failed");
    }

    VaeModel::new(model, true)
}

#[cfg(not(Py_3_8))]
#[no_mangle]
pub unsafe extern "C" fn load_info_py(
    _self: *mut PyObject,
    args: *mut PyObject,
    kwds: *mut PyObject,
) -> *mut PyObject {
    parse_args!("load_info", args, kwds, path, opts);
    let model = verilogae_load(path.data, false, opts.to_ffi());

    if model.is_null() {
        return raise_runtime_runtime_exception("load_info() compilation failed");
    }

    VaeModel::new(model, false)
}

#[cfg(Py_3_8)]
#[no_mangle]
pub unsafe extern "C" fn load_info_py(
    _self: *mut PyObject,
    args: *const *mut PyObject,
    nargs: Py_ssize_t,
    kwnames: *mut PyObject,
) -> *mut PyObject {
    parse_args!("load_info", args, nargs, kwnames, path, opts);
    let model = verilogae_load(path.data, false, opts.to_ffi());

    if model.is_null() {
        return raise_runtime_runtime_exception("load_info() compilation failed");
    }

    VaeModel::new(model, false)
}

#[cfg(not(Py_3_8))]
#[no_mangle]
pub unsafe extern "C" fn load_vfs(
    _self: *mut PyObject,
    args: *mut PyObject,
    kwds: *mut PyObject,
) -> *mut PyObject {
    parse_args!("load_vfs", args, kwds, path, opts);
    let vfs = VfsExport::new(path.data, &opts);
    match vfs {
        Some(vfs) => vfs_to_py(vfs),
        None => raise_runtime_runtime_exception("load_vfs() failed to create vfs"),
    }
}

#[cfg(Py_3_8)]
#[no_mangle]
pub unsafe extern "C" fn load_vfs(
    _self: *mut PyObject,
    args: *const *mut PyObject,
    nargs: Py_ssize_t,
    kwnames: *mut PyObject,
) -> *mut PyObject {
    parse_args!("load_vfs", args, nargs, kwnames, path, opts);
    let vfs = VfsExport::new(path.data, &opts);
    match vfs {
        Some(vfs) => vfs_to_py(vfs),
        None => raise_runtime_runtime_exception("load_vfs() failed to create vfs"),
    }
}

unsafe fn vfs_to_py(src: VfsExport) -> *mut PyObject {
    let dict = PyDict_New();
    for (path, contents) in src.entries() {
        let path = PyUnicode_FromStringAndSize(path.as_ptr() as *const c_char, path.len() as isize);
        let contents = PyUnicode_FromStringAndSize(
            contents.as_ptr() as *const c_char,
            contents.len() as isize,
        );
        PyDict_SetItem(dict, path, contents);
    }
    dict
}

unsafe fn py_to_vfs(fun: &str, obj: *mut PyObject) -> Option<Vfs> {
    if PyDict_Check(obj) == 0 {
        raise_type_exception(&format!("{}() arguments 'vfs' must have type dict(str,str) ", fun));
        return None;
    }
    let len = PyDict_GET_SIZE(obj);
    let mut pos = 0isize;
    let mut arg: *mut PyObject = std::ptr::null_mut();
    let mut val: *mut PyObject = std::ptr::null_mut();

    let mut vfs = Vec::with_capacity(len as usize);

    for _i in 0..=len {
        _PyDict_Next(obj, &mut pos, &mut arg, &mut val, std::ptr::null_mut());
        let mut path_size = 0;
        let path = PyUnicode_AsUTF8AndSize(arg, &mut path_size);
        if unlikely(path.is_null()) {
            raise_type_exception(&format!(
                "{}() arguments 'vfs' must have type dict(str,str) ",
                fun
            ));
            return None;
        }

        let mut data_size = 0;
        let data = PyUnicode_AsUTF8AndSize(val, &mut data_size);
        if unlikely(path.is_null()) {
            raise_type_exception(&format!(
                "{}() arguments 'vfs' must have type dict(str,str) ",
                fun
            ));
            return None;
        }

        vfs.push(VfsEntry {
            name: Slice::from_raw_parts(path as *const u8, path_size as usize),
            data: Slice::from_raw_parts(data as *const u8, data_size as usize),
        })
    }

    Some(vfs.into_boxed_slice().into())
}
