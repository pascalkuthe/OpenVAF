use anyhow::{bail, Result};
use std::alloc::{alloc_zeroed, handle_alloc_error, Layout};
use std::cell::Cell;
use std::ffi::{c_void, CStr, CString};
use std::mem::{align_of, swap};
use std::os::raw::c_char;
use std::rc::Rc;
use std::{ptr, slice};
use stdx::format_to;
use stdx::iter::zip;
use typed_index_collections::TiSlice;

use crate::circuit::Node;
use crate::devices::{DeviceImpl, DeviceParams, InstanceImpl, ModelImpl, ParamId, Type};
use crate::simulation::{MatrixEntryIter, SimBuilder, SimInfo};
use crate::veriloga::osdi_0_3::{
    OsdiDescriptor, OsdiInitInfo, OsdiJacobianEntry, OsdiNode, OsdiNodePair, OsdiParamOpvar,
    OsdiSimInfo, OsdiSimParas, ACCESS_FLAG_SET, EVAL_RET_FLAG_FATAL, INIT_ERR_OUT_OF_BOUNDS,
    PARA_KIND_INST, PARA_TY_INT, PARA_TY_MASK, PARA_TY_REAL, PARA_TY_STR,
};

impl OsdiDescriptor {
    fn nodes(&self) -> &[OsdiNode] {
        // # SAFETY: OsdiDescriptor can only be constructed from FFI and is assumed to contain
        // valid data
        unsafe { slice::from_raw_parts(self.nodes, self.num_nodes as usize) }
    }

    fn terminals(&self) -> &[OsdiNode] {
        &self.nodes()[..self.num_terminals as usize]
    }

    fn params(&self) -> &[OsdiParamOpvar] {
        // # SAFETY: OsdiDescriptor can only be constructed from FFI and is assumed to contain
        // valid data
        unsafe { slice::from_raw_parts(self.param_opvar, self.num_params as usize) }
    }

    fn collapsible(&self) -> &[OsdiNodePair] {
        // SAFETY: self.data is a valid allocation and the descriptor is assumed valid
        unsafe { slice::from_raw_parts(self.collapsible, self.num_collapsible as usize) }
    }

    fn matrix_entries(&self) -> &[OsdiJacobianEntry] {
        // SAFETY: self.data is a valid allocation and the descriptor is assumed valid
        unsafe { slice::from_raw_parts(self.jacobian_entries, self.num_jacobian_entries as usize) }
    }

    fn check_init_result(&self, res: OsdiInitInfo) -> Result<()> {
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
                        format_to!(msg, "vale supplied for parameter '{param}' is out of bounds\n")
                    }

                    code => format_to!(msg, "unknown error: {code}\n"),
                }
            }

            msg.pop();
            bail!(msg)
        }
        Ok(())
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

unsafe fn osdi_str(raw: *mut c_char) -> &'static str {
    CStr::from_ptr(raw).to_str().expect("All OSDI strings must be encoded in UTF-8")
}

fn osdi_param_ty(flags: u32) -> Type {
    match flags & PARA_TY_MASK {
        PARA_TY_REAL => Type::Real,
        PARA_TY_INT => Type::Int,
        PARA_TY_STR => Type::String,
        _ => unreachable!("unknown osdi type {}", flags & PARA_TY_MASK),
    }
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

pub(super) struct OsdiDevice {
    pub descriptor: &'static OsdiDescriptor,
}
impl DeviceImpl for OsdiDevice {
    fn get_name(&self) -> &'static str {
        unsafe { osdi_str(self.descriptor.name) }
    }

    fn get_terminals(&self) -> Box<[&'static str]> {
        unsafe {
            let terminals = &self.descriptor.nodes()[..self.descriptor.num_terminals as usize];
            terminals.iter().map(|node| osdi_str(node.name)).collect()
        }
    }

    fn get_params(&self) -> DeviceParams {
        let mut res = DeviceParams::default();
        unsafe {
            let params = self.descriptor.params();
            for param in params {
                let name = osdi_str(*param.name);
                let is_instance_param = (param.flags & PARA_KIND_INST) != 0;
                let ty = osdi_param_ty(param.flags);
                let param_id = res.insert_param(name, ty, is_instance_param);
                let aliases = slice::from_raw_parts(param.name.add(1), param.num_alias as usize);
                for &alias in aliases {
                    res.insert_alias(osdi_str(alias), param_id);
                }
            }
        }

        res
    }

    fn new_model(&self) -> Rc<dyn ModelImpl> {
        Rc::new(OsdiModel {
            data: alloc(self.descriptor.model_size as usize),
            descriptor: self.descriptor,
        })
    }
}

struct OsdiModel {
    descriptor: &'static OsdiDescriptor,
    data: *mut c_void,
}

impl Drop for OsdiModel {
    fn drop(&mut self) {
        // SAFETY: this is save because we obtain data from `alloc`
        unsafe { dealloc(self.data, self.descriptor.model_size as usize) }
    }
}

impl ModelImpl for OsdiModel {
    fn process_params(&self) -> Result<()> {
        let mut sim_params = OsdiSimParas {
            names: &mut ptr::null_mut(),
            vals: ptr::null_mut(),
            names_str: &mut ptr::null_mut(),
            vals_str: ptr::null_mut(),
        };

        let mut res = OsdiInitInfo { flags: 0, num_errors: 0, errors: ptr::null_mut() };
        self.descriptor.setup_model(
            b"foo\0".as_ptr() as *mut c_void,
            self.data,
            &mut sim_params,
            &mut res,
        );
        self.descriptor.check_init_result(res)
    }

    fn set_real_param(&self, param: ParamId, val: f64) {
        let ptr = self.descriptor.access(ptr::null_mut(), self.data, param.into(), ACCESS_FLAG_SET);
        let ptr = ptr as *mut f64;
        if ptr.is_null() {
            unreachable!("invalid parameter access")
        }
        unsafe { ptr.write(val) };
    }

    fn set_str_param(&self, param: ParamId, val: &str) {
        let val = CString::new(val).expect("string may not contain null terminators");
        let ptr = self.descriptor.access(ptr::null_mut(), self.data, param.into(), ACCESS_FLAG_SET);
        let ptr = ptr as *mut *mut c_char;
        if ptr.is_null() {
            unreachable!("invalid parameter access")
        }
        // TODO do not leak memory
        unsafe { ptr.write(val.into_raw()) };
    }

    fn set_int_param(&self, param: ParamId, val: i32) {
        let ptr = self.descriptor.access(ptr::null_mut(), self.data, param.into(), ACCESS_FLAG_SET);
        let ptr = ptr as *mut i32;
        if ptr.is_null() {
            unreachable!("invalid parameter access")
        }
        unsafe { ptr.write(val) };
    }

    fn new_instance(self: Rc<Self>) -> Box<dyn crate::devices::InstanceImpl> {
        Box::new(OsdiInstance {
            descriptor: self.descriptor,
            data: alloc(self.descriptor.instance_size as usize),
            model_data: self.data,
            _model: self,
        })
    }
}

struct OsdiInstance {
    descriptor: &'static OsdiDescriptor,
    data: *mut c_void,
    model_data: *mut c_void,
    _model: Rc<OsdiModel>, // only kept to ensure the data stays live
}

impl Drop for OsdiInstance {
    fn drop(&mut self) {
        unsafe { dealloc(self.data, self.descriptor.instance_size as usize) }
    }
}

impl OsdiInstance {
    fn matrix_ptrs_resist(&self) -> &[Cell<*mut f64>] {
        let ptr = self.data as *mut u8;
        // SAFETY: self.data is a valid allocation and the descriptor is assumed valid
        unsafe {
            let ptr =
                ptr.add(self.descriptor.jacobian_ptr_resist_offset as usize) as *mut Cell<*mut f64>;
            slice::from_raw_parts_mut(ptr, self.descriptor.num_jacobian_entries as usize)
        }
    }
    fn node_mapping(&self) -> &[Cell<u32>] {
        let ptr = self.data as *mut u8;
        // SAFETY: self.data is a valid allocation and the descriptor is assumed valid
        unsafe {
            let ptr = ptr.add(self.descriptor.node_mapping_offset as usize) as *mut Cell<u32>;
            slice::from_raw_parts_mut(ptr, self.descriptor.num_nodes as usize)
        }
    }

    fn collapsed(&self) -> &[bool] {
        let ptr = self.data as *mut u8;
        // SAFETY: self.data is a valid allocation and the descriptor is assumed valid
        unsafe {
            let ptr = ptr.add(self.descriptor.collapsed_offset as usize) as *mut bool;
            slice::from_raw_parts(ptr, self.descriptor.num_collapsible as usize)
        }
    }

    fn collapse_nodes(&self, connected_terminals: u32) -> Vec<u32> {
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
}

impl InstanceImpl for OsdiInstance {
    fn process_params(
        &mut self,
        temp: f64,
        sim_builder: &mut SimBuilder,
        terminals: &[Node],
    ) -> Result<()> {
        let mut sim_params = OsdiSimParas {
            names: &mut ptr::null_mut(),
            vals: ptr::null_mut(),
            names_str: &mut ptr::null_mut(),
            vals_str: ptr::null_mut(),
        };

        let mut res = OsdiInitInfo { flags: 0, num_errors: 0, errors: ptr::null_mut() };
        self.descriptor.setup_instance(
            b"foo\0".as_ptr() as *mut c_void,
            self.data,
            self.model_data,
            temp,
            terminals.len() as u32,
            &mut sim_params,
            &mut res,
        );
        self.descriptor.check_init_result(res)?;
        let mut internal_nodes = self.collapse_nodes(terminals.len() as u32);

        // create internal nodes
        for node in &mut internal_nodes {
            let node_info = &self.descriptor.nodes()[*node as usize];
            // TODO: tolerance based upon natures: #2
            let tol = if node_info.is_flow {
                sim_builder.config.voltage_atol
            } else {
                sim_builder.config.current_atol
            };

            let name = unsafe { osdi_str(node_info.name) };
            let units = unsafe { osdi_str(node_info.units) };
            let residual_units = unsafe { osdi_str(node_info.residual_units) };
            *node = sim_builder.new_internal_unknown(name, tol, units, residual_units).into();
        }

        let node_mapping = self.node_mapping();
        for node in node_mapping {
            let idx = node.get();
            if let Some(&terminal) = terminals.get(idx as usize) {
                node.set(terminal.into())
            } else if idx == u32::MAX {
                node.set(0)
            } else {
                node.set(internal_nodes[idx as usize - terminals.len()])
            }
        }

        for entry in self.descriptor.matrix_entries() {
            let column = node_mapping[entry.nodes.node_1 as usize].get().into();
            let row = node_mapping[entry.nodes.node_2 as usize].get().into();
            sim_builder.ensure_matrix_entry(column, row)
        }

        Ok(())
    }

    fn set_real_param(&mut self, param: ParamId, val: f64) {
        let ptr = self.descriptor.access(ptr::null_mut(), self.data, param.into(), ACCESS_FLAG_SET);
        let ptr = ptr as *mut f64;
        if ptr.is_null() {
            unreachable!("invalid parameter access")
        }
        unsafe { ptr.write(val) };
    }

    fn set_str_param(&mut self, param: ParamId, val: &str) {
        let val = CString::new(val).expect("string may not contain null terminators");
        let ptr = self.descriptor.access(ptr::null_mut(), self.data, param.into(), ACCESS_FLAG_SET);
        let ptr = ptr as *mut *mut c_char;
        if ptr.is_null() {
            unreachable!("invalid parameter access")
        }
        // TODO do not leak memory
        unsafe { ptr.write(val.into_raw()) };
    }

    fn set_int_param(&mut self, param: ParamId, val: i32) {
        let ptr = self.descriptor.access(ptr::null_mut(), self.data, param.into(), ACCESS_FLAG_SET);
        let ptr = ptr as *mut i32;
        if ptr.is_null() {
            unreachable!("invalid parameter access")
        }
        unsafe { ptr.write(val) };
    }

    fn populate_matrix_ptrs(&mut self, matrix_entries: MatrixEntryIter) {
        for (ptrs, (resist_ptr, entry)) in
            zip(matrix_entries, zip(self.matrix_ptrs_resist(), self.descriptor.matrix_entries()))
        {
            resist_ptr.set(ptrs.resist_ffi_ptr());
            if entry.react_ptr_off != u32::MAX {
                unsafe {
                    let react_ptr =
                        (self.data as *mut u8).add(entry.react_ptr_off as usize) as *mut *mut f64;
                    react_ptr.write(ptrs.react_ffi_ptr())
                }
            }
        }
    }

    fn eval(&mut self, sim_info: SimInfo<'_>) -> Result<()> {
        let sim_params = OsdiSimParas {
            names: &mut ptr::null_mut(),
            vals: ptr::null_mut(),
            names_str: &mut ptr::null_mut(),
            vals_str: ptr::null_mut(),
        };

        let mut info = OsdiSimInfo {
            paras: sim_params,
            abstime: sim_info.abstime,
            prev_solve: sim_info.prev_solve.as_ptr() as *mut f64,
            prev_state: ptr::null_mut(),
            next_state: ptr::null_mut(),
            flags: sim_info.flags.bits(),
        };

        let ret_flags = self.descriptor.eval(
            b"foo\0".as_ptr() as *mut c_void,
            self.data,
            self.model_data,
            &mut info,
        );
        if (ret_flags & EVAL_RET_FLAG_FATAL) != 0 {
            bail!("Simulation aborted with $fatal")
        }

        // TODO only during tran
        // if (ret_flags & EVAL_RET_FLAG_FINISH) != 0 {
        //     bail!("Simulation aborted with $finish")
        // }

        Ok(())
    }

    unsafe fn load_matrix_resist(&self) {
        self.descriptor.load_jacobian_resist(self.data, self.model_data)
    }

    unsafe fn load_matrix_react(&self, alpha: f64) {
        self.descriptor.load_jacobian_react(self.data, self.model_data, alpha)
    }

    fn load_residual_react(
        &self,
        _prev_solve: &TiSlice<Node, f64>,
        residual: &mut TiSlice<Node, f64>,
    ) {
        self.descriptor.load_residual_react(self.data, self.model_data, residual.as_mut_ptr())
    }

    fn load_residual_resist(
        &self,
        _prev_solve: &TiSlice<Node, f64>,
        residual: &mut TiSlice<Node, f64>,
    ) {
        self.descriptor.load_residual_resist(self.data, self.model_data, residual.as_mut_ptr())
    }

    fn load_lead_current_resist(&self, _dc_solve: &TiSlice<Node, f64>, dst: &mut [f64]) {
        for (node, dst) in zip(self.descriptor.terminals(), dst) {
            unsafe {
                let src =
                    (self.data as *mut u8).add(node.resist_residual_off as usize) as *const f64;
                ptr::copy_nonoverlapping(src, dst, 1);
            };
        }
    }

    fn load_lead_current_react(&self, _dc_solve: &TiSlice<Node, f64>, dst: &mut [f64]) {
        for (node, dst) in zip(self.descriptor.terminals(), dst) {
            unsafe {
                let src =
                    (self.data as *mut u8).add(node.react_residual_off as usize) as *const f64;
                ptr::copy_nonoverlapping(src, dst, 1);
            };
        }
    }
}
