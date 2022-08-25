use std::cell::Cell;
use std::path::Path;
use std::ptr::NonNull;
use std::rc::Rc;

use anyhow::Result;
use camino::Utf8PathBuf;
use num_complex::Complex64;
use stdx::{impl_debug_display, impl_idx_from};
use typed_index_collections::TiSlice;
use typed_indexmap::TiMap;

use crate::circuit::Node;
use crate::devices::resistor::Resistor;
use crate::devices::vsource::VoltageSrc;
use crate::expr::ExprEvalCtxRef;
use crate::simulation::{MatrixEntryIter, SimBuilder, SimInfo};
use crate::Expr;

mod resistor;
mod vsource;

pub trait DeviceImpl {
    fn get_name(&self) -> &'static str;
    fn get_ports(&self) -> &'static [&'static str];
    fn get_params(&self) -> TiMap<ParamId, &'static str, ParamInfo>;

    fn new_model(
        &self,
        eval_ctx: ExprEvalCtxRef,
        params: &[(ParamId, Expr)],
    ) -> Result<Rc<dyn ModelImpl>>;
}

pub trait ModelImpl {
    fn new_instance(
        self: Rc<Self>,
        builder: &mut SimBuilder,
        eval_ctx: ExprEvalCtxRef,
        params: &[(ParamId, Expr)],
        ports: &[Node],
    ) -> Result<Box<dyn InstanceImpl>>;
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
    fn populate_matrix_ptrs(&mut self, matrix_entries: MatrixEntryIter);
    fn eval(&mut self, sim_info: SimInfo<'_>);
    unsafe fn load_matrix_resist(&mut self);
    unsafe fn load_matrix_react(&mut self, alpha: f64);
    fn load_residual_react(
        &mut self,
        prev_solve: &TiSlice<Node, f64>,
        rhs: &mut TiSlice<Node, f64>,
    );
    fn load_residual_resist(
        &mut self,
        prev_solve: &TiSlice<Node, f64>,
        rhs: &mut TiSlice<Node, f64>,
    );
    fn load_ac_residual(
        &mut self,
        _dc_solve: &TiSlice<Node, f64>,
        _rhs: &mut TiSlice<Node, Complex64>,
    ) {
    }

    fn load_lead_current_resist(&self, _dc_solve: &TiSlice<Node, f64>, dst: &mut [f64]);
    fn load_lead_current_react(&self, _dc_solve: &TiSlice<Node, f64>, dst: &mut [f64]);
    fn load_ac_lead_current(&self, _ac_solve: &TiSlice<Node, Complex64>, _dst: &mut [Complex64]) {}
}

// TODO advanced options => tight OpenVAF integration
pub trait VaCompiler {
    fn build(&mut self, file: &Path) -> Result<Vec<Box<dyn DeviceImpl>>>;
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ParamId(pub u32);
impl_debug_display!(match ParamId{ ParamId(id) => "dev{:?}", id;});
impl_idx_from!(ParamId(u32));

pub struct ParamInfo {
    pub ty: Type,
    pub is_instance_param: bool,
}

pub enum Type {
    Real,
    Int,
    String,
}

pub struct DeviceInfo {
    pub name: &'static str,
    pub dev_impl: Box<dyn DeviceImpl>,
    pub va_file: Option<Utf8PathBuf>,
    pub ports: &'static [&'static str],
    pub parameters: TiMap<ParamId, &'static str, ParamInfo>,
}

pub(crate) fn default_devices() -> impl Iterator<Item = Box<dyn DeviceImpl>> {
    [VoltageSrc::init_dev(), Resistor::init_dev()].into_iter()
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Tolerance {
    Current,
    Voltage,
    Other(f64),
}
