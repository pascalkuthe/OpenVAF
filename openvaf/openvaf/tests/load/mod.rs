use std::alloc::{alloc_zeroed, handle_alloc_error, Layout};
use std::cell::Cell;
use std::ffi::{c_char, CStr};
use std::fmt::Debug;
use std::mem::{align_of, swap};
use std::panic::catch_unwind;
use std::process::exit;
use std::{ptr, slice};

use anyhow::{bail, Result};
use bitflags::bitflags;
use camino::Utf8Path;
use libc::c_void;
use libloading::Library;
use stdx::format_to;
use stdx::iter::zip;

#[allow(warnings)]
mod osdi_0_3;

pub use osdi_0_3::*;

impl OsdiDescriptor {
    pub fn nodes(&self) -> &[OsdiNode] {
        // # SAFETY: OsdiDescriptor can only be constructed from FFI and is assumed to contain
        // valid data
        unsafe { slice::from_raw_parts(self.nodes, self.num_nodes as usize) }
    }

    pub fn params(&self) -> &[OsdiParamOpvar] {
        // # SAFETY: OsdiDescriptor can only be constructed from FFI and is assumed to contain
        // valid data
        unsafe { slice::from_raw_parts(self.param_opvar, self.num_params as usize) }
    }

    pub fn collapsible(&self) -> &[OsdiNodePair] {
        // SAFETY: self.data is a valid allocation and the descriptor is assumed valid
        unsafe { slice::from_raw_parts(self.collapsible, self.num_collapsible as usize) }
    }

    pub fn matrix_entries(&self) -> &[OsdiJacobianEntry] {
        // SAFETY: self.data is a valid allocation and the descriptor is assumed valid
        unsafe { slice::from_raw_parts(self.jacobian_entries, self.num_jacobian_entries as usize) }
    }

    pub fn check_init_result(&self, res: OsdiInitInfo) -> Result<()> {
        if (res.flags & EVAL_RET_FLAG_FATAL) != 0 {
            bail!("Verilog-A $fatal was called")
        }

        if res.num_errors != 0 {
            let mut msg = String::default();

            for i in 0..res.num_errors as usize {
                let err = unsafe { &*res.errors.add(i) };

                match err.code {
                    INIT_ERR_OUT_OF_BOUNDS => {
                        let param = unsafe { err.payload.parameter_id };
                        let param = unsafe { *self.params()[param as usize].name };
                        let param = unsafe { osdi_str(param) };
                        format_to!(msg, "value supplied for parameter '{param}' is out of bounds\n")
                    }

                    code => format_to!(msg, "unknown error: {code}\n"),
                }
            }

            msg.pop();
            bail!(msg)
        }
        Ok(())
    }

    pub fn new_model(&'static self) -> OsdiModel {
        OsdiModel { data: alloc(self.model_size as usize), descriptor: self }
    }
}

impl Drop for OsdiInitInfo {
    fn drop(&mut self) {
        // # SAFETY: this is save because OSDI api promises malloc allocated data and the struct can
        // only be constructed by FFI
        if self.num_errors != 0 && !self.errors.is_null() {
            unsafe {
                libc::free(self.errors as *mut c_void);
            }
        }
    }
}

pub(super) unsafe fn osdi_str(raw: *mut c_char) -> &'static str {
    CStr::from_ptr(raw).to_str().expect("All OSDI strings must be encoded in UTF-8")
}

#[allow(non_camel_case_types)]
type max_align_t = u128;
const MAX_ALIGN: usize = align_of::<max_align_t>();

fn aligned_size(size: usize) -> usize {
    (size + (MAX_ALIGN - 1)) / MAX_ALIGN
}

fn max_align_layout(size: usize) -> Layout {
    Layout::array::<max_align_t>(aligned_size(size)).unwrap()
}

fn alloc(size: usize) -> *mut c_void {
    if size == 0 {
        // create dangeling pointer for zero sized types
        return ptr::null_mut();
    }
    let layout = max_align_layout(size);
    // # Safety: this is save because we check for zst above
    let data = unsafe { alloc_zeroed(layout) } as *mut c_void;
    if data.is_null() {
        handle_alloc_error(layout)
    } else {
        data
    }
}

/// # Safety
/// `ptr` must be a pointer allocated by [`alloc`]
unsafe fn dealloc(ptr: *mut c_void, size: usize) {
    if ptr.is_null() {
        return;
    }

    let layout = max_align_layout(size);
    std::alloc::dealloc(ptr as *mut u8, layout)
}

pub struct OsdiModel {
    pub descriptor: &'static OsdiDescriptor,
    pub data: *mut c_void,
}

impl Drop for OsdiModel {
    fn drop(&mut self) {
        // SAFETY: this is save because we obtain data from `alloc`
        unsafe { dealloc(self.data, self.descriptor.model_size as usize) }
    }
}

impl OsdiModel {
    pub fn process_params(&self) -> Result<()> {
        let mut sim_params = OsdiSimParas {
            names: &mut ptr::null_mut(),
            vals: ptr::null_mut(),
            names_str: &mut ptr::null_mut(),
            vals_str: ptr::null_mut(),
        };

        let mut res = OsdiInitInfo { flags: 0, num_errors: 0, errors: ptr::null_mut() };
        unsafe {
            self.descriptor.setup_model(
                b"foo\0".as_ptr() as *mut c_void,
                self.data,
                &mut sim_params,
                &mut res,
            )
        };
        self.descriptor.check_init_result(res)
    }

    pub fn set_real_param(&self, param: u32, val: f64) {
        let ptr =
            unsafe { self.descriptor.access(ptr::null_mut(), self.data, param, ACCESS_FLAG_SET) };
        let ptr = ptr as *mut f64;
        if ptr.is_null() {
            unreachable!("invalid parameter access")
        }
        unsafe { ptr.write(val) };
    }

    pub fn new_instance(&self) -> OsdiInstance {
        OsdiInstance {
            descriptor: self.descriptor,
            data: alloc(self.descriptor.instance_size as usize),
        }
    }
}

pub struct OsdiInstance {
    pub descriptor: &'static OsdiDescriptor,
    pub data: *mut c_void,
}

impl Drop for OsdiInstance {
    fn drop(&mut self) {
        unsafe { dealloc(self.data, self.descriptor.instance_size as usize) }
    }
}

impl OsdiInstance {
    pub fn matrix_ptrs_resist(&self) -> &[Cell<*mut f64>] {
        let ptr = self.data as *mut u8;
        // SAFETY: self.data is a valid allocation and the descriptor is assumed valid
        unsafe {
            let ptr =
                ptr.add(self.descriptor.jacobian_ptr_resist_offset as usize) as *mut Cell<*mut f64>;
            slice::from_raw_parts_mut(ptr, self.descriptor.num_jacobian_entries as usize)
        }
    }

    pub fn node_mapping(&self) -> &[Cell<u32>] {
        let ptr = self.data as *mut u8;
        // SAFETY: self.data is a valid allocation and the descriptor is assumed valid
        unsafe {
            let ptr = ptr.add(self.descriptor.node_mapping_offset as usize) as *mut Cell<u32>;
            slice::from_raw_parts_mut(ptr, self.descriptor.num_nodes as usize)
        }
    }

    pub fn collapsed(&self) -> &[bool] {
        let ptr = self.data as *mut u8;
        // SAFETY: self.data is a valid allocation and the descriptor is assumed valid
        unsafe {
            let ptr = ptr.add(self.descriptor.collapsed_offset as usize) as *mut bool;
            slice::from_raw_parts(ptr, self.descriptor.num_collapsible as usize)
        }
    }

    pub fn collapse_nodes(&self, connected_terminals: u32) -> Vec<u32> {
        let collapsed = self.collapsed();
        let node_mapping = self.node_mapping();
        let collapsible = self.descriptor.collapsible();

        let mut back_map: Vec<u32> = (connected_terminals..self.descriptor.num_nodes).collect();

        //  populate nodes with themselves
        for (node, node_mapping) in node_mapping.iter().enumerate() {
            node_mapping.set(node as u32)
        }

        for (candidate, is_collapsed) in zip(collapsible, collapsed) {
            if !is_collapsed {
                continue;
            }

            let from = candidate.node_1;
            let to = candidate.node_2;

            let mut mapped_from = node_mapping[from as usize].get();
            let mut collapse_to_gnd = to == u32::MAX;
            let mut mapped_to = if collapse_to_gnd {
                u32::MAX
            } else {
                let mapped = node_mapping[to as usize].get();
                collapse_to_gnd |= mapped == u32::MAX;
                mapped
            };

            // terminals cannot be collapsed
            if mapped_from < connected_terminals
                && (collapse_to_gnd || mapped_to < connected_terminals)
            {
                continue;
            }

            // ensure that to is always the smaller node
            if !collapse_to_gnd && mapped_from < mapped_to {
                swap(&mut mapped_from, &mut mapped_to)
            }

            // replace nodes mapped to from with to and reduce the number of nodes
            for dst in node_mapping {
                let mapping = dst.get();
                if mapping == mapped_from {
                    dst.set(mapped_to)
                } else if mapping > mapped_from && mapping != u32::MAX {
                    dst.set(mapping - 1)
                }
            }
            // public nodes can not be removed to no need to track them
            back_map.remove((mapped_from - connected_terminals) as usize);
        }

        back_map
    }

    pub fn process_params(
        &mut self,
        model: &OsdiModel,
        connected_terminals: u32,
        temp: f64,
    ) -> Result<Vec<u32>> {
        let mut sim_params = OsdiSimParas {
            names: &mut ptr::null_mut(),
            vals: ptr::null_mut(),
            names_str: &mut ptr::null_mut(),
            vals_str: ptr::null_mut(),
        };

        let mut res = OsdiInitInfo { flags: 0, num_errors: 0, errors: ptr::null_mut() };
        unsafe {
            self.descriptor.setup_instance(
                b"foo\0".as_ptr() as *mut c_void,
                self.data,
                model.data,
                temp,
                connected_terminals,
                &mut sim_params,
                &mut res,
            )
        };
        self.descriptor.check_init_result(res)?;
        let internal_nodes = self.collapse_nodes(connected_terminals);
        Ok(internal_nodes)
    }

    // pub fn set_real_param(&mut self, param: u32, val: f64) {
    //     let ptr =
    //         unsafe { self.descriptor.access(ptr::null_mut(), self.data, param, ACCESS_FLAG_SET) };
    //     let ptr = ptr as *mut f64;
    //     if ptr.is_null() {
    //         unreachable!("invalid parameter access")
    //     }
    //     unsafe { ptr.write(val) };
    // }
}

pub unsafe fn load_osdi_lib(path: &Utf8Path) -> Result<&'static [OsdiDescriptor]> {
    let lib = Library::new(path)?;
    let lib = Box::leak(Box::new(lib));

    let major_version: &u32 = *lib.get(b"OSDI_VERSION_MAJOR\0")?;
    let minor_version: &u32 = *lib.get(b"OSDI_VERSION_MINOR\0")?;

    if *major_version != 0 || *minor_version != 3 {
        bail!("invalid version v{major_version}.{minor_version}",);
    }

    let num_descriptors: &u32 = *lib.get(b"OSDI_NUM_DESCRIPTORS\0")?;
    let descriptors: *const OsdiDescriptor = *lib.get(b"OSDI_DESCRIPTORS\0")?;

    let descriptors: &[OsdiDescriptor] =
        slice::from_raw_parts(descriptors, *num_descriptors as usize);

    if let Ok(osdi_log_ptr) =
        lib.get::<*mut unsafe extern "C" fn(*mut c_void, *const c_char, u32)>(b"osdi_log\0")
    {
        osdi_log_ptr.write(osdi_log)
    }
    if let Ok(osdi_lim_table) = lib.get(b"OSDI_LIM_TABLE\0") {
        let lim_table_base: *mut OsdiLimFunction = *osdi_lim_table;
        let lim_table_len: &u32 = *lib.get(b"OSDI_LIM_TABLE_LEN\0")?;
        let lim_table = slice::from_raw_parts_mut(lim_table_base, *lim_table_len as usize);
        for lim_func in lim_table {
            if osdi_str(lim_func.name) == "pnjlim" {
                assert_eq!(lim_func.num_args, 2);
                let ptr: unsafe extern "C" fn(bool, *mut bool, f64, f64, f64, f64) -> f64 =
                    osdi_pnjlim;
                lim_func.func_ptr = ptr as *mut c_void;
            }
        }
    }
    Ok(descriptors)
}

unsafe extern "C" fn osdi_log(handle: *mut c_void, msg: *const c_char, lvl: u32) {
    let _ = catch_unwind(|| osdi_log_impl(handle, msg, lvl));
}

unsafe extern "C" fn osdi_pnjlim(
    init: bool,
    check: *mut bool,
    vnew: f64,
    vold: f64,
    vt: f64,
    vcrit: f64,
) -> f64 {
    if let Ok((res, check_)) = catch_unwind(|| osdi_pnjlim_impl(init, vnew, vold, vt, vcrit)) {
        if check_ {
            *check = true;
        }
        res
    } else {
        exit(-1)
    }
}

// an incorrect implementation of pnjlim that makes testing easy
fn osdi_pnjlim_impl(init: bool, vnew: f64, vold: f64, vt: f64, vcrit: f64) -> (f64, bool) {
    if init {
        (vcrit, true)
    } else if vnew > vcrit && (vnew - vold).abs() > vt + vt {
        (vold + (vnew - vold) / 2.0, true)
    } else {
        (vnew, false)
    }
}

unsafe fn osdi_log_impl(handle: *mut c_void, msg: *const c_char, lvl: u32) {
    let instance = handle as *const c_char;
    let instance = CStr::from_ptr(instance).to_str().expect("all OSDI strings must be valid utf-8");
    let msg = CStr::from_ptr(msg).to_str().expect("all OSDI strings must be valid utf-8");

    if (lvl & LOG_FMT_ERR) == 0 {
        match lvl & LOG_LVL_MASK {
            LOG_LVL_DEBUG => println!("debug {instance} - {msg}"),
            LOG_LVL_DISPLAY => println!("display {instance} - {msg}"),
            LOG_LVL_INFO => println!("info {instance} - {msg}"),
            LOG_LVL_WARN => println!("warn {instance} - {msg}"),
            LOG_LVL_ERR => println!("err {instance} - {msg}"),
            LOG_LVL_FATAL => println!("fatal {instance} - FATAL {msg}"),
            _ => println!("{instance} - UNKNOWN_LOG_LVL {msg}"),
        }
    } else {
        println!("{instance} - failed to format\"{msg}\"")
    }
}

impl Debug for OsdiDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        macro_rules! w {
            ($($tt: tt)*) => {
                write!(f, $($tt)*)?;
            };
        }
        macro_rules! wn {
            ($($tt: tt)*) => {
                writeln!(f, $($tt)*)?;
            };
        }

        unsafe {
            for param in self.params() {
                assert_eq!(param.len, 0);
                w!("param ");
                for i in 0..=param.num_alias {
                    if i != 0 {
                        w!(", ");
                    }
                    w!("{:?}", osdi_str(*param.name.add(i as usize)));
                }
                wn!();
                let desc = osdi_str(param.description);
                let units = osdi_str(param.units);
                let ty = ParameterFlags::from_bits(param.flags).unwrap();
                wn!("units = {units:?}, desc = {desc:?}, flags = {ty:?}");
            }

            wn!();
            wn!("{} terminals", self.num_terminals);
            for node in self.nodes() {
                let flow = if node.is_flow { "(flow)" } else { "" };
                wn!(
                    "node{flow} {:?} units = {:?}, runits = {:?}",
                    osdi_str(node.name),
                    osdi_str(node.units),
                    osdi_str(node.residual_units)
                );
                wn!(
                    "residual {} {} {} {}",
                    node.resist_residual_off,
                    node.react_residual_off,
                    node.resist_limit_rhs_off,
                    node.react_limit_rhs_off
                );
            }
            for matrix_entry in self.matrix_entries() {
                let hi = self.nodes()[matrix_entry.nodes.node_1 as usize].name;
                let lo = self.nodes()[matrix_entry.nodes.node_2 as usize].name;
                wn!(
                    "jacobian ({}, {}) {:?} react_ptr = {}",
                    osdi_str(hi),
                    osdi_str(lo),
                    JacobianFlags::from_bits(matrix_entry.flags).unwrap(),
                    matrix_entry.react_ptr_off,
                );
            }
            for OsdiNodePair { node_1, node_2 } in self.collapsible() {
                let hi = self.nodes()[*node_1 as usize].name;
                let lo = if *node_2 == u32::MAX {
                    "gnd"
                } else {
                    osdi_str(self.nodes()[*node_2 as usize].name)
                };
                wn!("collapsible ({}, {})", osdi_str(hi), lo);
            }
            wn!("{} states", self.num_states);
            wn!("has bound_step {}", self.bound_step_offset != u32::MAX);
            wn!("instance size {}", self.instance_size);
            wn!("model size {}", self.model_size);
            Ok(())
        }
    }
}

bitflags! {
    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    pub struct JacobianFlags: u32 {
        const JACOBIAN_ENTRY_RESIST = JACOBIAN_ENTRY_RESIST;
        const JACOBIAN_ENTRY_REACT = JACOBIAN_ENTRY_REACT;
        const JACOBIAN_ENTRY_RESIST_CONST = JACOBIAN_ENTRY_RESIST_CONST;
        const JACOBIAN_ENTRY_REACT_CONST = JACOBIAN_ENTRY_REACT_CONST;
    }
}

bitflags! {
    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    pub struct ParameterFlags: u32 {
        const PARA_TY_REAL  = PARA_TY_REAL;
        const PARA_TY_INT  = PARA_TY_INT;
        const PARA_TY_STR  = PARA_TY_STR;
        const PARA_KIND_MODEL  = PARA_KIND_MODEL;
        const PARA_KIND_INST  = PARA_KIND_INST;
        const PARA_KIND_OPVAR  = PARA_KIND_OPVAR;
    }
}

bitflags! {
    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    pub struct EvalFlags: u32 {
        const CALC_RESIST_RESIDUAL = CALC_RESIST_RESIDUAL;
        const CALC_REACT_RESIDUAL = CALC_REACT_RESIDUAL;
        const CALC_RESIST_JACOBIAN = CALC_RESIST_JACOBIAN;
        const CALC_REACT_JACOBIAN = CALC_REACT_JACOBIAN;
        const CALC_NOISE = CALC_NOISE;
        const CALC_OP = CALC_OP;
        const CALC_RESIST_LIM_RHS = CALC_RESIST_LIM_RHS;
        const CALC_REACT_LIM_RHS = CALC_REACT_LIM_RHS;
        const ENABLE_LIM = ENABLE_LIM;
        const INIT_LIM = INIT_LIM;
        const ANALYSIS_NOISE = ANALYSIS_NOISE;
        const ANALYSIS_DC = ANALYSIS_DC;
        const ANALYSIS_AC = ANALYSIS_AC;
        const ANALYSIS_TRAN = ANALYSIS_TRAN;
        const ANALYSIS_IC = ANALYSIS_IC;
        const ANALYSIS_STATIC = ANALYSIS_STATIC;
        const ANALYSIS_NODESET = ANALYSIS_NODESET;
    }
}

bitflags! {
    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    pub struct EvalRetFlags: u32 {
        const EVAL_RET_FLAG_LIM = EVAL_RET_FLAG_LIM;
        const EVAL_RET_FLAG_FATAL = EVAL_RET_FLAG_FATAL;
        const EVAL_RET_FLAG_FINISH = EVAL_RET_FLAG_FINISH;
        const EVAL_RET_FLAG_STOP = EVAL_RET_FLAG_STOP;
    }
}
