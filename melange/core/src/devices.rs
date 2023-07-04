use std::cell::Cell;
use std::ptr::NonNull;
use std::rc::Rc;

use anyhow::Result;
use camino::Utf8PathBuf;
use num_complex::Complex64;
use stdx::{impl_debug_display, impl_idx_from};
use typed_index_collections::TiSlice;

use crate::circuit::Node;
pub use crate::devices::params::{DeviceParams, ParamId, Type};
use crate::devices::resistor::Resistor;
use crate::devices::vsource::VoltageSrc;
use crate::simulation::{MatrixEntryIter, SimBuilder, SimInfo};

mod params;
mod resistor;
mod vsource;

pub trait DeviceImpl {
    fn get_name(&self) -> &'static str;
    fn get_terminals(&self) -> Box<[&'static str]>;
    fn get_params(&self) -> DeviceParams;
    fn new_model(&self) -> Rc<dyn ModelImpl>;
}

pub trait ModelImpl {
    fn process_params(&self) -> Result<()>;
    fn set_real_param(&self, param: ParamId, val: f64);
    fn set_int_param(&self, param: ParamId, _val: i32) {
        unreachable!("unknown int param {param:?}")
    }

    fn set_str_param(&self, param: ParamId, _val: &str) {
        unreachable!("unknown str param {param:?}")
    }
    fn new_instance(self: Rc<Self>) -> Box<dyn InstanceImpl>;
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DeviceMatrixId(u32);
impl_debug_display!(match DeviceMatrixId{ DeviceMatrixId(id) => "matrix_entry{:?}", id;});
impl_idx_from!(DeviceMatrixId(u32));

pub struct MatrixEntry<'a> {
    pub(crate) resist: &'a Cell<f64>,
    pub(crate) react: &'a Cell<f64>,
}

impl MatrixEntry<'_> {
    pub fn resist_ffi_ptr(&self) -> *mut f64 {
        self.resist.as_ptr()
    }
    pub fn react_ffi_ptr(&self) -> *mut f64 {
        self.react.as_ptr()
    }

    pub fn resist(&self) -> NonNull<Cell<f64>> {
        self.resist.into()
    }

    pub fn react(&self) -> NonNull<Cell<f64>> {
        self.react.into()
    }
}

pub fn update_matrix_entry(dst: &Cell<f64>, val: f64) {
    let res = dst.get() + val;
    dst.set(res)
}

pub trait InstanceImpl {
    fn process_params(
        &mut self,
        temp: f64,
        sim_builder: &mut SimBuilder,
        terminals: &[Node],
    ) -> Result<()>;
    fn set_real_param(&mut self, param: ParamId, val: f64);

    fn set_int_param(&mut self, param: ParamId, _val: i32) {
        unreachable!("unknown int param {param:?}")
    }

    fn set_str_param(&mut self, param: ParamId, _val: &str) {
        unreachable!("unknown str param {param:?}")
    }

    fn populate_matrix_ptrs(&mut self, matrix_entries: MatrixEntryIter);

    fn eval(&mut self, sim_info: SimInfo<'_>) -> Result<()>;

    unsafe fn load_matrix_resist(&self);
    unsafe fn load_matrix_react(&self, alpha: f64);

    fn load_residual_react(&self, prev_solve: &TiSlice<Node, f64>, rhs: &mut TiSlice<Node, f64>);
    fn load_residual_resist(&self, prev_solve: &TiSlice<Node, f64>, rhs: &mut TiSlice<Node, f64>);
    fn load_ac_residual(
        &self,
        _dc_solve: &TiSlice<Node, f64>,
        _rhs: &mut TiSlice<Node, Complex64>,
    ) {
    }

    fn load_lead_current_resist(&self, _dc_solve: &TiSlice<Node, f64>, dst: &mut [f64]);
    fn load_lead_current_react(&self, _dc_solve: &TiSlice<Node, f64>, dst: &mut [f64]);
    fn load_ac_lead_current(&self, _ac_solve: &TiSlice<Node, Complex64>, _dst: &mut [Complex64]) {}
}

pub struct DeviceInfo {
    pub name: &'static str,
    pub dev_impl: Box<dyn DeviceImpl>,
    pub va_file: Option<Utf8PathBuf>,
    pub terminals: Box<[&'static str]>,
    pub parameters: DeviceParams,
}

pub(crate) fn default_devices() -> impl Iterator<Item = Box<dyn DeviceImpl>> {
    [VoltageSrc::init_dev(), Resistor::init_dev()].into_iter()
}
