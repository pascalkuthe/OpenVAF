use std::cell::Cell;

use std::ptr::NonNull;
use std::rc::Rc;

use anyhow::{bail, Result};
use num_complex::Complex64;
use stdx::iter::zip;
use typed_index_collections::TiSlice;

use crate::circuit::Node;
use crate::devices::{update_matrix_entry, DeviceImpl, DeviceParams, InstanceImpl, Type};
use crate::simulation::{MatrixEntryIter, SimBuilder};

use super::{ModelImpl, ParamId, SimInfo};

pub struct VoltageSrc;

impl VoltageSrc {
    pub fn init_dev() -> Box<dyn DeviceImpl> {
        Box::new(Self)
    }
}

const DC: ParamId = ParamId(0u32);
const MAG: ParamId = ParamId(1u32);
const PHASE: ParamId = ParamId(2u32);

const MATRIX_ANODE_BR: usize = 0;
const MATRIX_BR_ANODE: usize = 1;
const MATRIX_CATHODE_BR: usize = 2;
const MATRIX_BR_CATHODE: usize = 3;

impl DeviceImpl for VoltageSrc {
    fn get_name(&self) -> &'static str {
        "vsource"
    }

    fn get_terminals(&self) -> Box<[&'static str]> {
        vec!["A", "C"].into_boxed_slice()
    }

    fn get_params(&self) -> DeviceParams {
        let mut res = DeviceParams::default();
        res.insert_instance_param("dc", Type::Real);
        res.insert_instance_param("mag", Type::Real);
        res.insert_instance_param("phase", Type::Real);
        res
    }

    fn new_model(&self) -> Rc<dyn ModelImpl> {
        Rc::new(VoltageSrcModel::default())
    }
}

#[derive(Default)]
struct VoltageSrcModel {
    dc: Cell<f64>,
    mag: Cell<f64>,
    phase: Cell<f64>,
}

impl ModelImpl for VoltageSrcModel {
    fn process_params(&self) -> Result<()> {
        Ok(())
    }

    fn set_real_param(&self, param: ParamId, val: f64) {
        let dst = match param {
            DC => &self.dc,
            MAG => &self.mag,
            PHASE => &self.phase,
            _ => unreachable!("vsource: unkown num param {param:?}"),
        };
        dst.set(val);
    }

    fn new_instance(self: Rc<Self>) -> Box<dyn super::InstanceImpl> {
        Box::new(VoltageSrcInstance {
            anode: Node::GROUND,
            cathode: Node::GROUND,
            branch: Node::GROUND,
            dc: self.dc.get(),
            ac: Complex64::from_polar(self.mag.get(), self.phase.get()),
            matrix_entries: [NonNull::dangling(); 4],
        })
    }
}

struct VoltageSrcInstance {
    anode: Node,
    cathode: Node,
    branch: Node,
    dc: f64,
    ac: Complex64,
    matrix_entries: [NonNull<Cell<f64>>; 4],
}

impl InstanceImpl for VoltageSrcInstance {
    fn process_params(
        &mut self,
        _temp: f64,
        builder: &mut SimBuilder,
        terminals: &[Node],
    ) -> Result<()> {
        let [anode, cathode] = if let &[anode, cathode] = terminals {
            [anode, cathode]
        } else {
            bail!("expected at least 2 connections")
        };

        let branch = builder.new_internal_branch("branch");
        self.anode = anode;
        self.cathode = cathode;
        self.branch = branch;

        // IMPORTANT: keep the order here in syn with the MATRIX_ indicies
        builder.ensure_matrix_entry(anode, branch);
        builder.ensure_matrix_entry(branch, anode);
        builder.ensure_matrix_entry(cathode, branch);
        builder.ensure_matrix_entry(branch, cathode);

        Ok(())
    }

    fn set_real_param(&mut self, param: ParamId, val: f64) {
        match param {
            DC => self.dc = val,
            MAG => self.ac = Complex64::from_polar(val, self.ac.arg()),
            PHASE => self.ac = Complex64::from_polar(self.ac.norm_sqr(), val),
            _ => unreachable!("vsource: unkown num param {param:?}"),
        };
    }

    fn populate_matrix_ptrs(&mut self, matrix_entries: MatrixEntryIter) {
        for (dst, entry) in zip(&mut self.matrix_entries, matrix_entries) {
            *dst = entry.resist();
        }
    }

    fn eval(&mut self, _sim_info: SimInfo<'_>) -> Result<()> {
        Ok(())
    }

    unsafe fn load_matrix_resist(&self) {
        update_matrix_entry(self.matrix_entries[MATRIX_ANODE_BR].as_ref(), 1.0);
        update_matrix_entry(self.matrix_entries[MATRIX_BR_ANODE].as_ref(), 1.0);
        update_matrix_entry(self.matrix_entries[MATRIX_CATHODE_BR].as_ref(), -1.0);
        update_matrix_entry(self.matrix_entries[MATRIX_BR_CATHODE].as_ref(), -1.0);
    }

    unsafe fn load_matrix_react(&self, _alpha: f64) {}

    fn load_residual_react(&self, _prev_solve: &TiSlice<Node, f64>, _rhs: &mut TiSlice<Node, f64>) {
    }

    fn load_residual_resist(&self, prev_solve: &TiSlice<Node, f64>, rhs: &mut TiSlice<Node, f64>) {
        rhs[self.anode] += prev_solve[self.branch];
        rhs[self.cathode] -= prev_solve[self.branch];
        rhs[self.branch] -= self.dc;
        rhs[self.branch] += prev_solve[self.anode] - prev_solve[self.cathode];
    }

    fn load_ac_residual(
        &self,
        _prev_solve: &TiSlice<Node, f64>,
        rhs: &mut TiSlice<Node, Complex64>,
    ) {
        rhs[self.branch] += self.ac;
    }

    fn load_lead_current_resist(&self, dc_solve: &TiSlice<Node, f64>, dst: &mut [f64]) {
        dst[0] = dc_solve[self.branch];
        dst[1] = -dc_solve[self.branch];
    }

    fn load_lead_current_react(&self, _dc_solve: &TiSlice<Node, f64>, _dst: &mut [f64]) {}

    fn load_ac_lead_current(&self, ac_solve: &TiSlice<Node, Complex64>, dst: &mut [Complex64]) {
        dst[0] = ac_solve[self.branch];
        dst[1] = -ac_solve[self.branch];
    }
}
