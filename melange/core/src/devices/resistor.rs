use std::cell::Cell;
use std::ptr::NonNull;
use std::rc::Rc;

use anyhow::{bail, Result};
use stdx::iter::zip;
use typed_index_collections::TiSlice;

use super::{ModelImpl, ParamId, SimInfo};
use crate::circuit::Node;
use crate::devices::{update_matrix_entry, DeviceImpl, DeviceParams, InstanceImpl, Type};
use crate::simulation::{MatrixEntryIter, SimBuilder};

pub struct Resistor;

impl Resistor {
    pub fn init_dev() -> Box<dyn DeviceImpl> {
        Box::new(Self)
    }
}

impl DeviceImpl for Resistor {
    fn get_name(&self) -> &'static str {
        "resistor"
    }

    fn get_terminals(&self) -> Box<[&'static str]> {
        vec!["A", "C"].into_boxed_slice()
    }

    fn get_params(&self) -> DeviceParams {
        let mut res = DeviceParams::default();
        res.insert_instance_param("r", Type::Real);
        res
    }

    fn new_model(&self) -> Rc<dyn ModelImpl> {
        Rc::new(ResistorModel::default())
    }
}

const R: ParamId = ParamId(0u32);

const MATRIX_ANODE_ANODE: usize = 0;
const MATRIX_ANODE_CATHODE: usize = 1;
const MATRIX_CATHODE_ANODE: usize = 2;
const MATRIX_CATHODE_CATHODE: usize = 3;

#[derive(Default, Clone)]
struct ResistorModel {
    res: Cell<Option<f64>>,
}

impl ModelImpl for ResistorModel {
    fn process_params(&self) -> Result<()> {
        Ok(())
    }

    fn set_real_param(&self, param: ParamId, val: f64) {
        match param {
            R => self.res.set(Some(val)),
            _ => unreachable!("vsource: unknown numeric parameter {param:?}"),
        };
    }

    fn new_instance(self: Rc<Self>) -> Box<dyn super::InstanceImpl> {
        Box::new(ResistorInstance {
            anode: Node::GROUND,
            cathode: Node::GROUND,
            res: self.res.get(),
            matrix_entries: [NonNull::dangling(); 4],
            conductance: 0.0,
        })
    }
}

struct ResistorInstance {
    anode: Node,
    cathode: Node,
    conductance: f64,
    res: Option<f64>,
    matrix_entries: [NonNull<Cell<f64>>; 4],
}

impl InstanceImpl for ResistorInstance {
    fn populate_matrix_ptrs(&mut self, matrix_entries: MatrixEntryIter) {
        for (dst, entry) in zip(&mut self.matrix_entries, matrix_entries) {
            *dst = entry.resist();
        }
    }

    fn eval(&mut self, _sim_info: SimInfo<'_>) -> Result<()> {
        Ok(())
    }

    unsafe fn load_matrix_resist(&self) {
        update_matrix_entry(self.matrix_entries[MATRIX_ANODE_ANODE].as_ref(), self.conductance);
        update_matrix_entry(self.matrix_entries[MATRIX_ANODE_CATHODE].as_ref(), -self.conductance);
        update_matrix_entry(self.matrix_entries[MATRIX_CATHODE_ANODE].as_ref(), -self.conductance);
        update_matrix_entry(self.matrix_entries[MATRIX_CATHODE_CATHODE].as_ref(), self.conductance);
    }

    unsafe fn load_matrix_react(&self, _alpha: f64) {}

    fn load_residual_react(&self, _prev_solve: &TiSlice<Node, f64>, _rhs: &mut TiSlice<Node, f64>) {
    }

    fn load_residual_resist(&self, prev_solve: &TiSlice<Node, f64>, rhs: &mut TiSlice<Node, f64>) {
        let voltage = prev_solve[self.anode] - prev_solve[self.cathode];
        rhs[self.anode] += voltage * self.conductance;
        rhs[self.cathode] -= voltage * self.conductance;
    }

    fn load_lead_current_resist(&self, dc_solve: &TiSlice<Node, f64>, dst: &mut [f64]) {
        let voltage = dc_solve[self.anode] - dc_solve[self.cathode];
        dst[0] = voltage * self.conductance;
        dst[1] = -voltage * self.conductance;
    }

    fn load_lead_current_react(&self, _dc_solve: &TiSlice<Node, f64>, _dst: &mut [f64]) {}

    fn process_params(
        &mut self,
        _temp: f64,
        sim_builder: &mut SimBuilder,
        terminals: &[Node],
    ) -> Result<()> {
        let (anode, cathode) = if let &[anode, cathode] = terminals {
            (anode, cathode)
        } else {
            bail!("resistor: all terminals must be connected")
        };

        self.anode = anode;
        self.cathode = cathode;

        sim_builder.ensure_matrix_entry(anode, anode);
        sim_builder.ensure_matrix_entry(anode, cathode);
        sim_builder.ensure_matrix_entry(cathode, anode);
        sim_builder.ensure_matrix_entry(cathode, cathode);

        match self.res {
            Some(res) => self.conductance = 1.0 / res,
            None => bail!("resistor: resistance must be set"),
        };
        Ok(())
    }

    fn set_real_param(&mut self, param: ParamId, val: f64) {
        match param {
            R => self.res = Some(val),
            _ => unreachable!("vsource: unknown numeric parameter {param:?}"),
        };
    }
}
