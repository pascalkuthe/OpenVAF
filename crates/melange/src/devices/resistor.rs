use std::cell::Cell;
use std::ptr::NonNull;
use std::rc::Rc;

use anyhow::{bail, Result};
use stdx::iter::zip;
use typed_index_collections::TiSlice;
use typed_indexmap::TiMap;

use crate::circuit::Node;
use crate::devices::{update_matrix_entry, DeviceImpl, InstanceImpl, Type};
use crate::expr::ExprEvalCtxRef;
use crate::simulation::{MatrixEntryIter, SimBuilder};
use crate::{Expr, Value};

use super::{ModelImpl, ParamId, ParamInfo, SimInfo};

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

    fn get_ports(&self) -> &'static [&'static str] {
        &["A", "C"]
    }

    fn get_params(&self) -> TiMap<ParamId, &'static str, ParamInfo> {
        let mut res = TiMap::default();
        res.insert("r", ParamInfo { ty: Type::Real, is_instance_param: true });
        res
    }

    fn new_model(
        &self,
        mut eval_ctx: ExprEvalCtxRef,
        params: &[(ParamId, Expr)],
    ) -> Result<Rc<dyn ModelImpl>> {
        let mut res = None;
        for &(param, val) in params {
            let dst = match param {
                R => &mut res,
                _ => unreachable!("vsource: unkown parameter"),
            };

            match val.eval(eval_ctx.borrow()) {
                Value::Num(val) => *dst = Some(val),
                Value::Str(_) | Value::UNDEF => bail!("expected real value"),
            }
        }
        Ok(Rc::new(ResistorModel { res }))
    }
}

const R: ParamId = ParamId(0u32);

const MATRIX_ANODE_ANODE: usize = 0;
const MATRIX_ANODE_CATHODE: usize = 1;
const MATRIX_CATHODE_ANODE: usize = 2;
const MATRIX_CATHODE_CATHODE: usize = 3;

#[derive(Default, Clone)]
struct ResistorModel {
    res: Option<f64>,
}

impl ModelImpl for ResistorModel {
    fn new_instance(
        self: Rc<Self>,
        builder: &mut SimBuilder,
        mut eval_ctx: ExprEvalCtxRef,
        params: &[(ParamId, Expr)],
        ports: &[Node],
    ) -> Result<Box<dyn super::InstanceImpl>> {
        let [anode, cathode] = if let &[anode, cathode] = ports {
            [anode, cathode]
        } else {
            bail!("expected at least 2 connections")
        };

        // IMPORTANT: keep the order here in syn with the MATRIX_ indicies
        builder.ensure_matrix_entry(anode, anode);
        builder.ensure_matrix_entry(anode, cathode);
        builder.ensure_matrix_entry(cathode, anode);
        builder.ensure_matrix_entry(cathode, cathode);

        let mut res = self.res;
        for &(param, val) in params {
            let dst = match param {
                R => &mut res,
                _ => unreachable!("vsource: unkown parameter"),
            };

            match val.eval(eval_ctx.borrow()) {
                Value::Num(val) => *dst = Some(val),
                Value::Str(_) | Value::UNDEF => bail!("expected real value"),
            }
        }

        let res = if let Some(res) = res { res } else { bail!("parameter 'r' is required") };

        let inst = Box::new(ResistorInstance {
            anode,
            cathode,
            conductance: 1.0 / res,
            matrix_entries: [NonNull::dangling(); 4],
        });

        Ok(inst)
    }
}

struct ResistorInstance {
    anode: Node,
    cathode: Node,
    conductance: f64,
    matrix_entries: [NonNull<Cell<f64>>; 4],
}

impl InstanceImpl for ResistorInstance {
    fn populate_matrix_ptrs(&mut self, matrix_entries: MatrixEntryIter) {
        for (dst, entry) in zip(&mut self.matrix_entries, matrix_entries) {
            *dst = entry.resist();
        }
    }

    fn eval(&mut self, _sim_info: SimInfo<'_>) {}

    unsafe fn load_matrix_resist(&mut self) {
        update_matrix_entry(self.matrix_entries[MATRIX_ANODE_ANODE].as_ref(), self.conductance);
        update_matrix_entry(self.matrix_entries[MATRIX_ANODE_CATHODE].as_ref(), -self.conductance);
        update_matrix_entry(self.matrix_entries[MATRIX_CATHODE_ANODE].as_ref(), -self.conductance);
        update_matrix_entry(self.matrix_entries[MATRIX_CATHODE_CATHODE].as_ref(), self.conductance);
    }

    unsafe fn load_matrix_react(&mut self, _alpha: f64) {}

    fn load_residual_react(
        &mut self,
        _prev_solve: &TiSlice<Node, f64>,
        _rhs: &mut TiSlice<Node, f64>,
    ) {
    }

    fn load_residual_resist(
        &mut self,
        prev_solve: &TiSlice<Node, f64>,
        rhs: &mut TiSlice<Node, f64>,
    ) {
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
}
