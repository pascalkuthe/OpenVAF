use std::cell::Cell;
use std::ptr::NonNull;
use std::rc::Rc;

use anyhow::{bail, Result};
use num_complex::Complex64;
use stdx::iter::zip;
use typed_index_collections::TiSlice;
use typed_indexmap::TiMap;

use crate::circuit::Node;
use crate::devices::{update_matrix_entry, DeviceImpl, InstanceImpl, Tolerance, Type};
use crate::expr::ExprEvalCtxRef;
use crate::simulation::{MatrixEntryIter, SimBuilder};
use crate::{Expr, Value};

use super::{ModelImpl, ParamId, ParamInfo, SimInfo};

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

    fn get_ports(&self) -> &'static [&'static str] {
        &["A", "C"]
    }

    fn get_params(&self) -> TiMap<ParamId, &'static str, ParamInfo> {
        let mut res = TiMap::default();
        res.insert("dc", ParamInfo { ty: Type::Real, is_instance_param: true });
        res.insert("mag", ParamInfo { ty: Type::Real, is_instance_param: true });
        res.insert("phase", ParamInfo { ty: Type::Real, is_instance_param: true });
        res
    }

    fn new_model(
        &self,
        mut eval_ctx: ExprEvalCtxRef,
        params: &[(ParamId, Expr)],
    ) -> Result<Rc<dyn ModelImpl>> {
        let mut dc = 0f64;
        let mut mag = 0f64;
        let mut phase = 0f64;
        for &(param, val) in params {
            let dst = match param {
                DC => &mut dc,
                MAG => &mut mag,
                PHASE => &mut phase,
                _ => unreachable!("vsource: unkown parameter"),
            };

            match val.eval(eval_ctx.borrow()) {
                Value::Num(val) => *dst = val,
                Value::Str(_) | Value::UNDEF => bail!("expected real value"),
            }
        }

        let src = VoltageSrcModel { dc, mag, phase };

        Ok(Rc::new(src))
    }
}

struct VoltageSrcModel {
    dc: f64,
    mag: f64,
    phase: f64,
}

impl ModelImpl for VoltageSrcModel {
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

        let branch = builder.new_internal_node(Tolerance::Voltage);
        // IMPORTANT: keep the order here in syn with the MATRIX_ indicies
        builder.ensure_matrix_entry(anode, branch);
        builder.ensure_matrix_entry(branch, anode);
        builder.ensure_matrix_entry(cathode, branch);
        builder.ensure_matrix_entry(branch, cathode);

        let mut dc = self.dc;
        let mut mag = self.mag;
        let mut phase = self.phase;
        for &(param, val) in params {
            let dst = match param {
                DC => &mut dc,
                MAG => &mut mag,
                PHASE => &mut phase,
                _ => unreachable!("vsource: unkown parameter"),
            };

            match val.eval(eval_ctx.borrow()) {
                Value::Num(val) => *dst = val,
                Value::Str(_) | Value::UNDEF => bail!("expected real value"),
            }
        }

        let inst = Box::new(VoltageSrcInstance {
            anode,
            cathode,
            branch,
            dc,
            ac: Complex64::from_polar(mag, phase),
            matrix_entries: [NonNull::dangling(); 4],
        });

        Ok(inst)
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
    fn populate_matrix_ptrs(&mut self, matrix_entries: MatrixEntryIter) {
        for (dst, entry) in zip(&mut self.matrix_entries, matrix_entries) {
            *dst = entry.resist();
        }
    }

    fn eval(&mut self, _sim_info: SimInfo<'_>) {}

    unsafe fn load_matrix_resist(&mut self) {
        update_matrix_entry(self.matrix_entries[MATRIX_ANODE_BR].as_ref(), 1.0);
        update_matrix_entry(self.matrix_entries[MATRIX_BR_ANODE].as_ref(), 1.0);
        update_matrix_entry(self.matrix_entries[MATRIX_CATHODE_BR].as_ref(), -1.0);
        update_matrix_entry(self.matrix_entries[MATRIX_BR_CATHODE].as_ref(), -1.0);
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
        rhs[self.anode] += prev_solve[self.branch];
        rhs[self.cathode] -= prev_solve[self.branch];
        rhs[self.branch] += self.dc;
        rhs[self.branch] -= prev_solve[self.anode] - prev_solve[self.cathode];
    }

    fn load_ac_residual(
        &mut self,
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
