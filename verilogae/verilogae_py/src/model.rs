use std::ffi::CStr;
use std::mem::take;
use std::os::raw::c_long;
use std::ptr;
use std::slice;

use libc::{c_char, c_void};
use pyo3_ffi::structmember::{PyMemberDef, READONLY, T_OBJECT, T_OBJECT_EX};
use pyo3_ffi::*;
use verilogae_ffi::{
    verilogae_call_fun_parallel, verilogae_fun_current_cnt, verilogae_fun_current_default_cnt,
    verilogae_fun_current_defaults, verilogae_fun_currents, verilogae_fun_ptr,
    verilogae_fun_voltage_cnt, verilogae_fun_voltage_default_cnt, verilogae_fun_voltage_defaults,
    verilogae_fun_voltages, verilogae_function_cnt, verilogae_function_symbols,
    verilogae_functions, verilogae_init_modelcard, verilogae_int_fun_depbreak,
    verilogae_int_fun_depbreak_cnt, verilogae_int_fun_param_cnt, verilogae_int_fun_params,
    verilogae_int_param_cnt, verilogae_int_param_descriptions, verilogae_int_param_groups,
    verilogae_int_param_units, verilogae_int_params, verilogae_module_name, verilogae_node_cnt,
    verilogae_nodes, verilogae_opvars, verilogae_opvars_cnt, verilogae_real_fun_depbreak,
    verilogae_real_fun_depbreak_cnt, verilogae_real_fun_param_cnt, verilogae_real_fun_params,
    verilogae_real_param_cnt, verilogae_real_param_descriptions, verilogae_real_param_groups,
    verilogae_real_param_units, verilogae_real_params, verilogae_str_fun_param_cnt,
    verilogae_str_fun_params, verilogae_str_param_cnt, verilogae_str_param_descriptions,
    verilogae_str_param_groups, verilogae_str_param_units, verilogae_str_params, FatPtr, Meta,
    ParamFlags, PARAM_FLAGS_INVALID, PARAM_FLAGS_MAX_INCLUSIVE, PARAM_FLAGS_MIN_INCLUSIVE,
};

use crate::ffi::new_type;
use crate::numpy::{ItemType, NumpyArray, PyArrayError};
use crate::typeref::NUMPY_API;
use crate::typeref::NUMPY_ARR_TYPE;
use crate::typeref::TEMPERATURE_STR;
use crate::typeref::VOLTAGES_STR;
use crate::typeref::{CURRENTS_STR, NUMPY_CDOUBLE_DESCR};
use crate::util::likely;
use crate::util::unlikely;

pub static mut VAE_MODEL_TY: PyTypeObject = {
    let mut res = new_type::<VaeModel>();
    res.tp_name = "verilogae.VaeModel\0".as_ptr() as *const c_char;
    res.tp_doc =
        "A Verilog-A module compiled and loaded with Verilog-AE\0".as_ptr() as *const c_char;
    res.tp_members = unsafe { &mut VAE_MODEL_MEMBERS } as *mut _;
    res.tp_dealloc = Some(VaeModel::dealloc);
    res
};

static mut VAE_MODEL_MEMBERS: [PyMemberDef; 6] = [
    PyMemberDef {
        name: "functions\0".as_ptr() as *mut c_char,
        type_code: T_OBJECT_EX,
        offset: VaeModel::offset_to.functions as isize,
        flags: READONLY,
        doc: "all functions defined within this module\0".as_ptr() as *mut c_char,
    },
    PyMemberDef {
        name: "modelcard\0".as_ptr() as *mut c_char,
        type_code: T_OBJECT_EX,
        offset: VaeModel::offset_to.modelcard as isize,
        flags: READONLY,
        doc: "all parameters defined within this module\0".as_ptr() as *mut c_char,
    },
    PyMemberDef {
        name: "op_vars\0".as_ptr() as *mut c_char,
        type_code: T_OBJECT_EX,
        offset: VaeModel::offset_to.op_vars as isize,
        flags: READONLY,
        doc: "all variables marked with (*op_var*)\0".as_ptr() as *mut c_char,
    },
    PyMemberDef {
        name: "module_name\0".as_ptr() as *mut c_char,
        type_code: T_OBJECT_EX,
        offset: VaeModel::offset_to.module_name as isize,
        flags: READONLY,
        doc: "the name of the compiled model\0".as_ptr() as *mut c_char,
    },
    PyMemberDef {
        name: "nodes\0".as_ptr() as *mut c_char,
        type_code: T_OBJECT_EX,
        offset: VaeModel::offset_to.nodes as isize,
        flags: READONLY,
        doc: "Verilog-A ports of the compiled module\0".as_ptr() as *mut c_char,
    },
    unsafe { zero!(PyMemberDef) },
];

with_offsets! {
    #[repr(C)]
    pub struct VaeModel {
        ob_base: PyObject,
        functions: *mut PyObject,
        modelcard: *mut PyObject,
        op_vars: *mut PyObject,
        module_name: *mut PyObject,
        nodes: *mut PyObject,
    }
}

impl VaeModel {
    #[allow(clippy::new_ret_no_self)]
    pub unsafe fn new(handle: *const c_void, full: bool) -> *mut PyObject {
        let ptr = VAE_MODEL_TY.tp_alloc.unwrap()(&mut VAE_MODEL_TY, 0);
        if ptr.is_null() {
            return ptr::null_mut();
        }

        let res = &mut *(ptr as *mut Self);
        if full {
            let functions = VaeFun::new_dict(handle);
            if functions.is_null() {
                Py_DECREF(ptr);
                return ptr::null_mut();
            }
            res.functions = functions
        }

        res.modelcard = VaeParam::new_mcard(handle);
        if res.modelcard.is_null() {
            Py_DECREF(ptr);
            return ptr::null_mut();
        }

        let opvar_cnt = verilogae_opvars_cnt(handle);
        let opvars = verilogae_opvars(handle);
        res.op_vars = PyList_New(opvar_cnt as isize);
        if res.op_vars.is_null() {
            Py_DECREF(ptr);
            return ptr::null_mut();
        }
        for i in 0..opvar_cnt {
            let var = *opvars.add(i);
            let var = PyUnicode_InternFromString(var);
            PyList_SetItem(res.op_vars, i as isize, var);
        }

        let module_name = verilogae_module_name(handle);
        res.module_name = PyUnicode_InternFromString(module_name);

        let nodes = verilogae_nodes(handle);
        let node_cnt = verilogae_node_cnt(handle);

        res.nodes = PyList_New(node_cnt as isize);
        for i in 0..node_cnt {
            let node = PyUnicode_InternFromString(*nodes.add(i));
            PyList_SetItem(res.nodes, i as isize, node);
        }

        ptr
    }

    unsafe extern "C" fn dealloc(sel: *mut PyObject) {
        let sel = &mut *(sel as *mut Self);
        Py_XDECREF(sel.functions);
        Py_XDECREF(sel.modelcard);
        Py_XDECREF(sel.op_vars);
    }
}

pub static mut VAE_PARAM_TY: PyTypeObject = {
    let mut res = new_type::<VaeParam>();
    res.tp_name = "verilogae.VaeParam\0".as_ptr() as *const c_char;
    res.tp_doc = "A parameter belonging to Verilog-A module compiled and loaded with Verilog-AE\0"
        .as_ptr() as *const c_char;
    res.tp_members = unsafe { &mut VAE_PARAM_MEMBERS } as *mut _;
    res.tp_dealloc = Some(VaeParam::dealloc);
    res
};

static mut VAE_PARAM_MEMBERS: [PyMemberDef; 10] = [
    PyMemberDef {
        name: "name\0".as_ptr() as *mut c_char,
        type_code: T_OBJECT,
        offset: VaeParam::offset_to.name as isize,
        flags: READONLY,
        doc: "The name of the parameter\0".as_ptr() as *mut c_char,
    },
    PyMemberDef {
        name: "default\0".as_ptr() as *mut c_char,
        type_code: T_OBJECT,
        offset: VaeParam::offset_to.default as isize,
        flags: READONLY,
        doc: "The default value of the parameter\0".as_ptr() as *mut c_char,
    },
    PyMemberDef {
        name: "min\0".as_ptr() as *mut c_char,
        type_code: T_OBJECT_EX,
        offset: VaeParam::offset_to.min as isize,
        flags: READONLY,
        doc: "The lowest bounds of the patamer\0".as_ptr() as *mut c_char,
    },
    PyMemberDef {
        name: "max\0".as_ptr() as *mut c_char,
        type_code: T_OBJECT_EX,
        offset: VaeParam::offset_to.max as isize,
        flags: READONLY,
        doc: "The higest bound of the parameter\0".as_ptr() as *mut c_char,
    },
    PyMemberDef {
        name: "min_inclusive\0".as_ptr() as *mut c_char,
        type_code: T_OBJECT_EX,
        offset: VaeParam::offset_to.min_inclusive as isize,
        flags: READONLY,
        doc: "Whether the lowest bound is inclusive\0".as_ptr() as *mut c_char,
    },
    PyMemberDef {
        name: "max_inclusive\0".as_ptr() as *mut c_char,
        type_code: T_OBJECT_EX,
        offset: VaeParam::offset_to.max_inclusive as isize,
        flags: READONLY,
        doc: "Whether the higest bound is inclusive\0".as_ptr() as *mut c_char,
    },
    PyMemberDef {
        name: "description\0".as_ptr() as *mut c_char,
        type_code: T_OBJECT_EX,
        offset: VaeParam::offset_to.description as isize,
        flags: READONLY,
        doc: "Contents of the descr attribute of the parameter\0".as_ptr() as *mut c_char,
    },
    PyMemberDef {
        name: "unit\0".as_ptr() as *mut c_char,
        type_code: T_OBJECT_EX,
        offset: VaeParam::offset_to.unit as isize,
        flags: READONLY,
        doc: "Contents of the units attribute of the parameter\0".as_ptr() as *mut c_char,
    },
    PyMemberDef {
        name: "group\0".as_ptr() as *mut c_char,
        type_code: T_OBJECT_EX,
        offset: VaeParam::offset_to.group as isize,
        flags: READONLY,
        doc: "Contents of the group attribute of the parameter\0".as_ptr() as *mut c_char,
    },
    unsafe { zero!(PyMemberDef) },
];

with_offsets! {
    #[repr(C)]
    pub struct VaeParam {
        ob_base: PyObject,
        name: *mut PyObject,
        default: *mut PyObject,
        min: *mut PyObject,
        max: *mut PyObject,
        min_inclusive: *mut PyObject,
        max_inclusive: *mut PyObject,
        description: *mut PyObject,
        unit: *mut PyObject,
        group: *mut PyObject,
    }
}

impl VaeParam {
    pub unsafe fn new_mcard(handle: *const c_void) -> *mut PyObject {
        let res = PyDict_New();
        if res.is_null() {
            return ptr::null_mut();
        }

        let real_param_cnt = verilogae_real_param_cnt(handle);
        let int_param_cnt = verilogae_int_param_cnt(handle);
        let str_param_cnt = verilogae_str_param_cnt(handle);

        let mut flags = vec![0u8; real_param_cnt + int_param_cnt + str_param_cnt];
        let mut real_data = vec![0f64; 3 * real_param_cnt];
        let mut int_data = vec![0; 3 * int_param_cnt];
        let mut str_data = vec![ptr::null(); str_param_cnt];
        let modelcard_init = verilogae_init_modelcard(handle).expect("invalid model handle");
        modelcard_init(
            real_data.as_mut_ptr(),
            int_data.as_mut_ptr(),
            str_data.as_mut_ptr(),
            real_data.as_mut_ptr().add(real_param_cnt),
            int_data.as_mut_ptr().add(int_param_cnt),
            real_data.as_mut_ptr().add(2 * real_param_cnt),
            int_data.as_mut_ptr().add(2 * int_param_cnt),
            flags.as_mut_ptr(),
        );

        let param_names = verilogae_real_params(handle);
        let param_units = verilogae_real_param_units(handle);
        let param_descr = verilogae_real_param_descriptions(handle);
        let param_groups = verilogae_real_param_groups(handle);

        for (i, (((val, min), max), flags)) in real_data[..real_param_cnt]
            .iter()
            .zip(&real_data[real_param_cnt..])
            .zip(&real_data[2 * real_param_cnt..])
            .zip(&flags)
            .enumerate()
        {
            let name = *param_names.add(i);
            let unit = *param_units.add(i);
            let description = *param_descr.add(i);
            let group = *param_groups.add(i);

            let (param, name) =
                VaeParam::new_real(name, *val, *min, *max, *flags, description, unit, group);
            if param.is_null() {
                return ptr::null_mut();
            }

            PyDict_SetItem(res, name, param);

            Py_DECREF(param);
        }

        let param_names = verilogae_int_params(handle);
        let param_units = verilogae_int_param_units(handle);
        let param_descr = verilogae_int_param_descriptions(handle);
        let param_groups = verilogae_int_param_groups(handle);

        for (i, (((val, min), max), flags)) in int_data[..int_param_cnt]
            .iter()
            .zip(&int_data[int_param_cnt..])
            .zip(&int_data[2 * int_param_cnt..])
            .zip(&flags[real_param_cnt..])
            .enumerate()
        {
            let name = *param_names.add(i);
            let unit = *param_units.add(i);
            let description = *param_descr.add(i);
            let group = *param_groups.add(i);

            let (param, name) =
                VaeParam::new_int(name, *val, *min, *max, *flags, description, unit, group);
            if param.is_null() {
                return ptr::null_mut();
            }

            PyDict_SetItem(res, name, param);
            Py_DECREF(param);
        }

        let param_names = verilogae_str_params(handle);
        let param_units = verilogae_str_param_units(handle);
        let param_descr = verilogae_str_param_descriptions(handle);
        let param_groups = verilogae_str_param_groups(handle);

        for (i, (val, flags)) in
            str_data.iter().zip(&flags[(real_param_cnt + int_param_cnt)..]).enumerate()
        {
            let name = *param_names.add(i);
            let unit = *param_units.add(i);
            let description = *param_descr.add(i);
            let group = *param_groups.add(i);

            let (param, name) = VaeParam::new_str(name, *val, *flags, description, unit, group);
            if param.is_null() {
                return ptr::null_mut();
            }

            PyDict_SetItem(res, name, param);
            Py_DECREF(param);
        }
        res
    }

    unsafe fn new_str(
        name: *const c_char,
        default: *const c_char,
        flags: ParamFlags,
        description: *const c_char,
        unit: *const c_char,
        group: *const c_char,
    ) -> (*mut PyObject, *mut PyObject) {
        let default = PyUnicode_FromString(default);
        VaeParam::new(
            name,
            default,
            ptr::null_mut(),
            ptr::null_mut(),
            flags,
            description,
            unit,
            group,
        )
    }

    #[allow(clippy::too_many_arguments)]
    unsafe fn new_real(
        name: *const c_char,
        default: f64,
        min: f64,
        max: f64,
        flags: ParamFlags,
        description: *const c_char,
        unit: *const c_char,
        group: *const c_char,
    ) -> (*mut PyObject, *mut PyObject) {
        let min = PyFloat_FromDouble(min);
        let max = PyFloat_FromDouble(max);
        let default = PyFloat_FromDouble(default);
        VaeParam::new(name, default, min, max, flags, description, unit, group)
    }

    #[allow(clippy::too_many_arguments)]
    unsafe fn new_int(
        name: *const c_char,
        default: i32,
        min: i32,
        max: i32,
        flags: ParamFlags,
        description: *const c_char,
        unit: *const c_char,
        group: *const c_char,
    ) -> (*mut PyObject, *mut PyObject) {
        let min = PyLong_FromLong(min as c_long);
        let max = PyLong_FromLong(max as c_long);
        let default = PyLong_FromLong(default as c_long);
        VaeParam::new(name, default, min, max, flags, description, unit, group)
    }

    #[allow(clippy::too_many_arguments)]
    #[allow(clippy::new_ret_no_self)]
    unsafe fn new(
        name: *const c_char,
        default: *mut PyObject,
        min: *mut PyObject,
        max: *mut PyObject,
        flags: ParamFlags,
        description: *const c_char,
        unit: *const c_char,
        group: *const c_char,
    ) -> (*mut PyObject, *mut PyObject) {
        let ptr = VAE_PARAM_TY.tp_alloc.unwrap()(&mut VAE_PARAM_TY, 0);
        if ptr.is_null() {
            Py_XDECREF(default);
            Py_XDECREF(min);
            Py_XDECREF(max);
            return (ptr::null_mut(), ptr::null_mut());
        }

        if unlikely((flags & PARAM_FLAGS_INVALID) != 0) {
            let name = CStr::from_ptr(name).to_str().unwrap();
            eprintln!("warning: default value for {} is out of bounds!", name);
        }

        let min_inclusive = PyBool_FromLong((flags & PARAM_FLAGS_MIN_INCLUSIVE) as c_long);
        let max_inclusive = PyBool_FromLong((flags & PARAM_FLAGS_MAX_INCLUSIVE) as c_long);

        let name = PyUnicode_InternFromString(name);
        let description = PyUnicode_FromString(description);
        let unit = PyUnicode_InternFromString(unit);
        let group = PyUnicode_InternFromString(group);

        let res = &mut *(ptr as *mut Self);
        res.name = name;
        res.default = default;
        res.min = min;
        res.max = max;
        res.min_inclusive = min_inclusive;
        res.max_inclusive = max_inclusive;
        res.unit = unit;
        res.description = description;
        res.group = group;

        (ptr, name)
    }

    unsafe extern "C" fn dealloc(sel: *mut PyObject) {
        let sel = &mut *(sel as *mut Self);
        Py_XDECREF(sel.name);
        Py_XDECREF(sel.default);
        Py_XDECREF(sel.min);
        Py_XDECREF(sel.max);
        Py_XDECREF(sel.min_inclusive);
        Py_XDECREF(sel.max_inclusive);
        Py_XDECREF(sel.description);
        Py_XDECREF(sel.unit);
    }
}

pub static mut VAE_FUNCTION_TY: PyTypeObject = {
    let mut res = new_type::<VaeFun>();
    res.tp_name = "verilogae.VaeFun\0".as_ptr() as *const c_char;
    res.tp_doc =
        "A function (compiled with VerilogAE) to calculate a Verilog-A variable marked with `retrieve`\0".as_ptr() as *const c_char;
    res.tp_members = unsafe { &mut VAE_FUNCTION_MEMBERS } as *mut _;
    res.tp_methods = unsafe { &mut VAE_FUNCTION_METHODS } as *mut _;
    res.tp_dealloc = Some(VaeFun::dealloc);
    res
};

static mut VAE_FUNCTION_MEMBERS: [PyMemberDef; 6] = [
    PyMemberDef {
        name: "name\0".as_ptr() as *mut c_char,
        type_code: T_OBJECT,
        offset: VaeFun::offset_to.name as isize,
        flags: READONLY,
        doc: "The name of the retrieved function".as_ptr() as *mut c_char,
    },
    PyMemberDef {
        name: "voltages\0".as_ptr() as *mut c_char,
        type_code: T_OBJECT,
        offset: VaeFun::offset_to.voltages as isize,
        flags: READONLY,
        doc: "The names of all voltages the function requires".as_ptr() as *mut c_char,
    },
    PyMemberDef {
        name: "currents\0".as_ptr() as *mut c_char,
        type_code: T_OBJECT,
        offset: VaeFun::offset_to.currents as isize,
        flags: READONLY,
        doc: "The names of all currents the function requires".as_ptr() as *mut c_char,
    },
    PyMemberDef {
        name: "parameters\0".as_ptr() as *mut c_char,
        type_code: T_OBJECT,
        offset: VaeFun::offset_to.parameters as isize,
        flags: READONLY,
        doc: "The names of all parameters the function requires".as_ptr() as *mut c_char,
    },
    PyMemberDef {
        name: "depbreak\0".as_ptr() as *mut c_char,
        type_code: T_OBJECT,
        offset: VaeFun::offset_to.depbreak as isize,
        flags: READONLY,
        doc: "The names of all variables the function requires for dependency breaking".as_ptr()
            as *mut c_char,
    },
    unsafe { zero!(PyMemberDef) },
];

const EVAL_DOC: &str = "\0";

static mut VAE_FUNCTION_METHODS: [PyMethodDef; 2] = [
    // #[cfg(Py_3_8)]
    // PyMethodDef {
    //     ml_name: "eval\0".as_ptr() as *const c_char,
    //     ml_meth: unsafe {
    //         transmute::<_PyCFunctionFastWithKeywords, Option<PyCFunction>>(VaeFun::eval)
    //     },
    //     ml_flags: METH_FASTCALL | METH_KEYWORDS,
    //     ml_doc: EVAL_DOC.as_ptr() as *const c_char,
    // },
    // #[cfg(not(Py_3_8))]
    PyMethodDef {
        ml_name: "eval\0".as_ptr() as *const c_char,
        ml_meth: PyMethodDefPointer { PyCFunctionWithKeywords: VaeFun::eval },
        ml_flags: METH_VARARGS | METH_KEYWORDS,
        ml_doc: EVAL_DOC.as_ptr() as *const c_char,
    },
    unsafe { zero!(PyMethodDef) },
];

#[derive(Clone, Copy)]
union ErasedFatPtr {
    float: FatPtr<f64>,
    int: FatPtr<i32>,
}

// TODO benchmark: use vectorcall and custom internal tracking what parameters were specified

with_offsets! {
    #[repr(C)]
    struct VaeFun {
        ob_base: PyObject,

        // exposed API
        name: *mut PyObject,
        voltages: *mut PyObject,
        currents: *mut PyObject,
        parameters: *mut PyObject,
        depbreak: *mut PyObject,

        int_depbreak_offset: usize,
        real_depbreak_offset: usize,

        real_params: Box<[(*mut PyObject, &'static str)]>,
        int_params:  Box<[(*mut PyObject, &'static str)]>,
        str_params:  Box<[(*mut PyObject, &'static str)]>,
        voltages_:   Box<[(*mut PyObject, &'static str, f64)]>,
        currents_:   Box<[(*mut PyObject, &'static str, f64)]>,

        required_kwargs: usize,

        ffi_data: Box<[ErasedFatPtr]>,
        ffi_str_data: Box<[*const c_char]>,
        ffi: verilogae_ffi::VaeFun,
    }
}
macro_rules! read_array {
    ($name: expr, $val: expr,  $kind: ident, $len: expr,  $dst: expr $(, convert $convert_kind: ident)?) => {
        match NumpyArray::new($val) {
            Ok(arr) if likely(arr.kind == ItemType::$kind) => {
                if unlikely(arr.len() == 1){
                    $dst.set_scalar(*(arr.data() as *mut _));
                } else if $len == arr.len(){
                    $dst.set_ptr(arr.data() as *mut _, arr.stride());
                } else if $len == 1 {
                    $len = arr.len();
                    $dst.set_ptr(arr.data() as *mut _, arr.stride());
                } else {
                    return raise_eval_illegal_arr_len_exception($name, arr.len(), $len)
                };
            }
            $(Ok(arr) if likely(arr.kind == ItemType::$convert_kind) => {
                if unlikely(arr.len() == 1){
                    $dst.set_scalar(*(arr.data() as *mut _));
                } else if $len == arr.len(){
                    // Stide times two since we are reading i32 (half size)
                    $dst.set_ptr(arr.data() as *mut _, arr.stride() * 2);
                } else if $len == 1 {
                    $len = arr.len();
                    // Stide times two since we are reading i32 (half size)
                    $dst.set_ptr(arr.data() as *mut _, arr.stride() * 2);
                } else {
                    return raise_eval_illegal_arr_len_exception($name, arr.len(), $len)
                };
            })?

            res => return raise_eval_illegal_array_exception(
                    res,
                    $name,
                    ItemType::$kind,
            ),
        }
    };
}

macro_rules! read_branch_val {
    ( $expected: expr, $found: expr, $len: ident, $dst: expr) => {
        for ((name_, name, default_val), dst) in $expected.iter().copied().zip(&mut $dst) {
            let dst = &mut dst.float;

            let val = PyDict_GetItem($found, name_);
            if val.is_null() {
                if unlikely(default_val.is_nan()) {
                    return raise_eval_exception(&format!(
                        "eval() missing required {} '{}'",
                        stringify!($found),
                        name
                    ));
                } else {
                    dst.set_scalar(default_val);
                    continue;
                }
            }

            let ty = ob_type!(val);

            if likely(is_array(ty)) {
                read_array!(name, val, Float, $len, dst)
            } else if is_float(ty) {
                dst.set_scalar(PyFloat_AS_DOUBLE(val));
            } else if is_int(ty) {
                // allow conversion of scalars
                dst.set_scalar(PyLong_AsLong(val) as f64);
                if unlikely(!PyErr_Occurred().is_null()) {
                    return ptr::null_mut();
                }
            } else {
                return raise_eval_illegal_data_type_exception(name);
            }
        }
    };
}

macro_rules! populate_default_branches {
    ( $expected: expr, $found: expr, $len: ident, $dst: expr) => {
        for ((_, name, default_val), dst) in $expected.iter().copied().zip(&mut $dst) {
            if default_val.is_nan() {
                return raise_eval_exception(&format!(
                    "eval() missing required {} '{}'",
                    stringify!($found),
                    name
                ));
            } else {
                let dst = &mut dst.float;
                dst.set_scalar(default_val)
            }
        }
    };
}

impl VaeFun {
    unsafe fn new_dict(handle: *const c_void) -> *mut PyObject {
        let functions = PyDict_New();
        if functions.is_null() {
            return ptr::null_mut();
        }

        let fun_cnt = verilogae_function_cnt(handle);
        let fun_names = verilogae_functions(handle);
        let fun_symbols = verilogae_function_symbols(handle);

        let fun_handles = slice::from_raw_parts(fun_symbols, fun_cnt as usize);
        let fun_names = slice::from_raw_parts(fun_names, fun_cnt as usize);

        for (name, sym) in fun_names.iter().copied().zip(fun_handles.iter().copied()) {
            // intern for faster lookups with constants (so all the time)
            let name = PyUnicode_InternFromString(name);
            let fun = VaeFun::new(handle, name, sym);
            if fun.is_null() {
                Py_DECREF(functions);
                Py_DECREF(name);
                return ptr::null_mut();
            }
            let code = PyDict_SetItem(functions, name, fun);
            Py_DECREF(fun);
            if code != 0 {
                Py_DECREF(functions);
                return ptr::null_mut();
            }
        }

        functions
    }
    #[allow(clippy::new_ret_no_self)]
    unsafe fn new(handle: *const c_void, name: *mut PyObject, sym: *const c_char) -> *mut PyObject {
        let ptr = PyType_GenericAlloc(&mut VAE_FUNCTION_TY, 0);
        if ptr.is_null() {
            return ptr::null_mut();
        }

        let voltage_names = verilogae_fun_voltages(handle, sym);
        let voltage_cnt = verilogae_fun_voltage_cnt(handle, sym);
        let voltage_default_cnt = verilogae_fun_voltage_default_cnt(handle, sym);
        let voltage_defaults = verilogae_fun_voltage_defaults(handle, sym);

        let voltages = PyList_New(voltage_cnt as isize);

        let voltages_: Box<[_]> = (0..voltage_cnt)
            .map(|i| {
                let name = *voltage_names.add(i);
                let name_py = PyUnicode_InternFromString(name);
                PyList_SetItem(voltages, i as isize, name_py);
                let name = std::str::from_utf8(CStr::from_ptr(name).to_bytes()).unwrap();
                let default =
                    if i < voltage_default_cnt { *voltage_defaults.add(i) } else { f64::NAN };
                (name_py, name, default)
            })
            .collect();

        let current_names = verilogae_fun_currents(handle, sym);
        let currents_cnt = verilogae_fun_current_cnt(handle, sym);
        let current_default_cnt = verilogae_fun_current_default_cnt(handle, sym);
        let current_defaults = verilogae_fun_current_defaults(handle, sym);

        let currents = PyList_New(currents_cnt as isize);

        let currents_: Box<[_]> = (0..currents_cnt)
            .map(|i| {
                let name = *current_names.add(i);
                let name_py = PyUnicode_InternFromString(name);
                PyList_SetItem(currents, i as isize, name_py);
                let name = std::str::from_utf8(CStr::from_ptr(name).to_bytes()).unwrap();
                let default =
                    if i < current_default_cnt { *current_defaults.add(i) } else { f64::NAN };
                (name_py, name, default)
            })
            .collect();

        let real_param_names = verilogae_real_fun_params(handle, sym);
        let real_param_cnt = verilogae_real_fun_param_cnt(handle, sym);

        let int_param_names = verilogae_int_fun_params(handle, sym);
        let int_param_cnt = verilogae_int_fun_param_cnt(handle, sym);

        let str_param_names = verilogae_str_fun_params(handle, sym);
        let str_param_cnt = verilogae_str_fun_param_cnt(handle, sym);

        let parameters = PyList_New((real_param_cnt + int_param_cnt) as isize);

        let real_params = (0..real_param_cnt).map(|i| {
            let name = *real_param_names.add(i);
            let name_py = PyUnicode_InternFromString(name);
            PyList_SetItem(parameters, i as isize, name_py);
            let name = std::str::from_utf8(CStr::from_ptr(name).to_bytes()).unwrap();
            (name_py, name)
        });

        let int_params = (0..int_param_cnt).map(|i| {
            let name = *int_param_names.add(i);
            let name_py = PyUnicode_InternFromString(name);
            PyList_SetItem(parameters, (i + real_param_cnt) as isize, name_py);
            let name = std::str::from_utf8(CStr::from_ptr(name).to_bytes()).unwrap();
            (name_py, name)
        });

        let str_params = (0..str_param_cnt).map(|i| {
            let name = *str_param_names.add(i);
            let name_py = PyUnicode_InternFromString(name);
            PyList_SetItem(parameters, (i + real_param_cnt) as isize, name_py);
            let name = std::str::from_utf8(CStr::from_ptr(name).to_bytes()).unwrap();
            (name_py, name)
        });

        let real_depbreak_names = verilogae_real_fun_depbreak(handle, sym);
        let real_depbreak_cnt = verilogae_real_fun_depbreak_cnt(handle, sym);
        let int_depbreak_names = verilogae_int_fun_depbreak(handle, sym);
        let int_depbreak_cnt = verilogae_int_fun_depbreak_cnt(handle, sym);

        let depbreak = PyList_New((real_depbreak_cnt + int_depbreak_cnt) as isize);

        let real_depbreak = (0..real_depbreak_cnt).map(|i| {
            let name = *real_depbreak_names.add(i);
            let name_py = PyUnicode_InternFromString(name);
            PyList_SetItem(depbreak, i as isize, name_py);
            let name = std::str::from_utf8(CStr::from_ptr(name).to_bytes()).unwrap();
            (name_py, name)
        });

        let int_depbreak = (0..int_depbreak_cnt).map(|i| {
            let name = *int_depbreak_names.add(i);
            let name_py = PyUnicode_InternFromString(name);
            PyList_SetItem(depbreak, (i + real_depbreak_cnt) as isize, name_py);
            let name = std::str::from_utf8(CStr::from_ptr(name).to_bytes()).unwrap();
            (name_py, name)
        });

        let ffi = verilogae_fun_ptr(handle, sym);
        assert!(ffi.is_some(), "failed to read verilogae function");

        let res = VaeFun {
            ob_base: ptr::read(ptr),
            name,
            voltages,
            currents,
            parameters,
            depbreak,
            real_depbreak_offset: real_param_cnt,
            int_depbreak_offset: int_param_cnt,
            real_params: real_params.chain(real_depbreak).collect(),
            int_params: int_params.chain(int_depbreak).collect(),
            str_params: str_params.collect(),
            voltages_,
            currents_,

            required_kwargs: 1
                + real_depbreak_cnt
                + int_depbreak_cnt
                + real_param_cnt
                + int_param_cnt
                + str_param_cnt,

            ffi_data: vec![
                ErasedFatPtr {
                    float: FatPtr { ptr: ptr::null_mut(), meta: Meta { stride: 0 } }
                };
                real_depbreak_cnt
                    + int_depbreak_cnt
                    + voltage_cnt
                    + currents_cnt
                    + real_param_cnt
                    + int_param_cnt
            ]
            .into_boxed_slice(),
            ffi,

            ffi_str_data: vec![ptr::null(); str_param_cnt].into_boxed_slice(),
        };

        ptr::write(ptr as *mut VaeFun, res);

        ptr
    }

    unsafe extern "C" fn dealloc(self_: *mut PyObject) {
        let self_ = &mut *(self_ as *mut Self);
        Py_XDECREF(self_.name);

        Py_XDECREF(self_.voltages);
        Py_XDECREF(self_.currents);
        Py_XDECREF(self_.parameters);
        Py_XDECREF(self_.depbreak);

        // make drop a noop
        take(&mut self_.real_params);
        take(&mut self_.int_params);
        take(&mut self_.str_params);
        take(&mut self_.voltages_);
        take(&mut self_.currents_);
        take(&mut self_.ffi_data);
        take(&mut self_.ffi_str_data);
    }

    // #[cfg(not(Py_3_8))]
    unsafe extern "C" fn eval(
        self_: *mut PyObject,
        args: *mut PyObject,
        kwds: *mut PyObject,
    ) -> *mut PyObject {
        let self_ = &mut *(self_ as *mut Self);

        let arg_cnt = PyTuple_GET_SIZE(args);
        if unlikely(arg_cnt != 0) {
            return raise_eval_exception(&format!(
                "eval() {} unexpected positional arguments",
                arg_cnt
            ));
        }

        if unlikely(kwds.is_null()) && self_.required_kwargs != 0 {
            return raise_eval_exception(&format!(
                "eval() missing required {} keyword arguments",
                self_.required_kwargs,
            ));
        }

        let temperature = PyDict_GetItem(kwds, TEMPERATURE_STR);

        if unlikely(temperature.is_null()) {
            return raise_eval_exception("eval() missing required keyword argument 'temperature'");
        }

        let mut len = 1;

        #[allow(unused_unsafe)]
        let ty = ob_type!(temperature);

        let mut temp = FatPtr { ptr: std::ptr::null_mut(), meta: Meta { stride: 0 } };
        if likely(is_array(ty)) {
            read_array!("temperature", temperature, Float, len, temp);
        } else if is_float(ty) {
            temp.set_scalar(PyFloat_AS_DOUBLE(temperature));
        } else if is_int(ty) {
            // allow conversion of scalars
            temp.set_scalar(PyLong_AsLong(temperature) as f64);
            if unlikely(!PyErr_Occurred().is_null()) {
                return ptr::null_mut();
            }
        } else {
            return raise_eval_illegal_data_type_exception("temperature");
        }

        let mut dst = self_.ffi_data.iter_mut();
        for ((name_, name), dst) in self_.real_params.iter().copied().zip(&mut dst) {
            let dst = &mut dst.float;
            // This is somewhat of an hotloop
            // There are easily hundreds of parameters
            // Therefore we do not support implicit float32 -> float64 conversions. This is not important
            // because float64 is default anyway
            let val = PyDict_GetItem(kwds, name_);
            if unlikely(val.is_null()) {
                return raise_eval_exception(&format!(
                    "eval() missing required keyword argument '{}'",
                    name
                ));
            }

            let ty = ob_type!(val);

            if likely(is_float(ty)) {
                dst.set_scalar(PyFloat_AS_DOUBLE(val));
            } else if is_array(ty) {
                read_array!(name, val, Float, len, dst)
            } else if is_int(ty) {
                // allow conversion of scalars
                dst.set_scalar(PyLong_AsLong(val) as f64);
                if unlikely(!PyErr_Occurred().is_null()) {
                    return ptr::null_mut();
                }
            } else {
                return raise_eval_illegal_data_type_exception(name);
            }
        }

        for ((name_, name), dst) in self_.int_params.iter().copied().zip(&mut dst) {
            let dst = &mut dst.int;
            // There are usually very few integer parameters and they are usually flags/single
            // values
            let val = PyDict_GetItem(kwds, name_);
            if unlikely(val.is_null()) {
                return raise_eval_exception(&format!(
                    "eval() missing required keyword argument '{}'",
                    name
                ));
            }

            let ty = ob_type!(val);

            if likely(is_int(ty)) {
                dst.set_scalar(PyLong_AsLong(val) as i32);
                if unlikely(!PyErr_Occurred().is_null()) {
                    return ptr::null_mut();
                }
            } else if is_array(ty) {
                read_array!(name, val, Int, len, dst, convert  Long)
            } else {
                return raise_eval_illegal_data_type_exception(name);
            }
        }

        for ((name_, name), dst) in self_.str_params.iter().copied().zip(&mut *self_.ffi_str_data) {
            // There are usually very few integer parameters and they are usually flags/single
            // values
            let val = PyDict_GetItem(kwds, name_);
            if unlikely(val.is_null()) {
                return raise_eval_exception(&format!(
                    "eval() missing required keyword argument '{}'",
                    name
                ));
            }

            if likely(PyUnicode_Check(val) != 0) {
                let val = PyUnicode_AsUTF8(val);
                if unlikely(val.is_null()) {
                    return ptr::null_mut();
                }
                *dst = val;
            } else {
                return raise_eval_illegal_data_type_exception(name);
            }
        }

        let voltages = PyDict_GetItem(kwds, VOLTAGES_STR);
        if unlikely(voltages.is_null()) {
            populate_default_branches!(self_.voltages_, voltages, len, dst);
        } else {
            read_branch_val!(self_.voltages_, voltages, len, dst);
        }

        let currents = PyDict_GetItem(kwds, CURRENTS_STR);
        if unlikely(currents.is_null()) {
            populate_default_branches!(self_.currents_, currents, len, dst);
        } else {
            read_branch_val!(self_.currents_, currents, len, dst);
        }

        let ptr = self_.ffi_data.as_mut_ptr();
        if likely(len != 1) {
            let new_arr = NUMPY_API.unwrap();
            Py_INCREF(NUMPY_CDOUBLE_DESCR);
            let dst = new_arr(
                NUMPY_ARR_TYPE.unwrap(), // base_type (normal numpy array)
                NUMPY_CDOUBLE_DESCR,     // type descriptor
                1,                       //nd
                &mut len,                //dims
                &mut 8,                  // strides
                ptr::null_mut(),         //data (to be allocated)
                0,                       // flags
                ptr::null_mut(),         // obj (to be created)
            );
            let arr = NumpyArray::new(dst).unwrap();
            verilogae_call_fun_parallel(
                self_.ffi,
                len as usize,
                &mut (*ptr.add(self_.int_params.len() + self_.real_params.len())).float,
                &mut (*ptr
                    .add(self_.int_params.len() + self_.real_params.len() + self_.voltages_.len()))
                .float,
                &mut (*ptr).float,
                &mut (*ptr.add(self_.real_params.len())).int,
                self_.ffi_str_data.as_mut_ptr(),
                &mut (*ptr.add(self_.real_depbreak_offset)).float,
                &mut (*ptr.add(self_.int_depbreak_offset + self_.real_params.len())).int,
                &mut temp,
                arr.data(),
            );
            dst
        } else {
            let mut val = 0f64;
            let val_ptr: *mut f64 = &mut val;
            verilogae_call_fun_parallel(
                self_.ffi,
                len as usize,
                &mut (*ptr.add(self_.int_params.len() + self_.real_params.len())).float,
                &mut (*ptr
                    .add(self_.int_params.len() + self_.real_params.len() + self_.voltages_.len()))
                .float,
                &mut (*ptr).float,
                &mut (*ptr.add(self_.real_params.len())).int,
                self_.ffi_str_data.as_mut_ptr(),
                &mut (*ptr.add(self_.real_depbreak_offset)).float,
                &mut (*ptr.add(self_.int_depbreak_offset + self_.real_params.len())).int,
                &mut temp,
                &mut val as *mut f64 as *mut _,
            );
            PyFloat_FromDouble(*val_ptr)
        }
    }
}

#[cold]
#[inline(never)]
fn raise_eval_exception(msg: &str) -> *mut PyObject {
    unsafe {
        let err_msg =
            PyUnicode_FromStringAndSize(msg.as_ptr() as *const c_char, msg.len() as isize);
        PyErr_SetObject(PyExc_TypeError, err_msg);
        Py_DECREF(err_msg);
    };
    std::ptr::null_mut()
}

#[inline(always)]
fn is_array(ty: *mut PyTypeObject) -> bool {
    let arr_type = unsafe { NUMPY_ARR_TYPE };
    match arr_type {
        Some(arr_type) => ty == arr_type,
        None => unlikely(false),
    }
}

#[inline(always)]
unsafe fn is_float(ty: *mut PyTypeObject) -> bool {
    ty == &mut PyFloat_Type || PyType_IsSubtype(ty, &mut PyFloat_Type) != 0
}

#[inline(always)]
unsafe fn is_int(ty: *mut PyTypeObject) -> bool {
    PyType_FastSubclass(ty, Py_TPFLAGS_LONG_SUBCLASS) != 0
}

#[cold]
#[inline(never)]
fn raise_eval_illegal_array_exception(
    res: Result<NumpyArray, PyArrayError>,
    name: &str,
    kind: ItemType,
) -> *mut PyObject {
    let msg = match res {
        Ok(arr) => {
            format!("eval() expected {} for '{}' (found {} array)", kind, name, arr.kind)
        }

        Err(PyArrayError::Malformed) => {
            format!("eval() recived mallformed numpy array for '{}'", name)
        }

        // Err(PyArrayError::NotContiguous) => {
        //     return raise_eval_exception(&format!(
        //         "eval() recived non contigous numpy array for '{}'",
        //         $name,
        //     ))
        // }
        Err(PyArrayError::UnsupportedDataType) => {
            format!("eval() recived numpy array with unsupprted data type for '{}'", name,)
        }
    };

    raise_eval_exception(&msg)
}

#[cold]
#[inline(never)]
fn raise_eval_illegal_data_type_exception(name: &str) -> *mut PyObject {
    raise_eval_exception(&format!("eval() recived unsupprted data type for '{}'", name,))
}

#[cold]
#[inline(never)]
fn raise_eval_illegal_arr_len_exception(
    name: &str,
    found: isize,
    expected: isize,
) -> *mut PyObject {
    raise_eval_exception(&format!(
        "eval() all arguments must have the same length but {} has length {}\n\thelp: all previous argument have len {}",
        name,
        found,
        expected
    ))
}
