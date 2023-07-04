use std::mem::replace;
use std::rc::Rc;

use anyhow::{bail, Context, Result};
use cli_table::{print_stdout, Cell, Style, Table, TableStruct};
use klu_rs::{FixedKluMatrix, KluData};
use num_complex::Complex64;
use stdx::iter::zip;
use typed_index_collections::{TiSlice, TiVec};

use crate::circuit::{CircuitModelSrc, InstanceId, ModelId, Node};
use crate::devices::{InstanceImpl, ModelImpl, Type};
use crate::expr::{CircuitParam, ExprEvalCtxRef};
use crate::simulation::flags::{EvalFlags, OperatingPointAnalysis, SimulationState};
pub use crate::simulation::matrix::MatrixEntryIter;
use crate::simulation::matrix::{MatrixBuilder, SimulationMatrix};
use crate::utils::PrettyPrint;
use crate::{Arena, Circuit, Value};

mod flags;
mod matrix;

pub struct Simulation<'a> {
    circ: &'a Circuit,
    model_data: Box<TiSlice<ModelId, Rc<dyn ModelImpl>>>,
    instance_data: Box<TiSlice<InstanceId, Box<dyn InstanceImpl>>>,

    matrix_builder: MatrixBuilder,
    matrix: Option<SimulationMatrix>,

    nodes: TiVec<Node, NodeInfo>,

    solution: TiVec<Node, f64>,
    ac_solution: TiVec<Node, Complex64>,
    residual_resist: TiVec<Node, f64>,
    residual_react: TiVec<Node, f64>,
    pub config: SimConfig,
    state: SimulationState,

    omega: f64,
}

#[derive(Debug, Clone)]
pub struct NodeInfo {
    pub name: String,
    pub atol: f64,
    pub units: &'static str,
    pub residual_units: &'static str,
}

pub struct SimBuilder<'a> {
    instance: InstanceId,
    matrix_builder: &'a mut MatrixBuilder,
    node_info: &'a mut TiVec<Node, NodeInfo>,
    circ: &'a Circuit,
    pub config: &'a SimConfig,
}

impl<'a> SimBuilder<'a> {
    fn process_instance(&mut self, instance: InstanceId) {
        self.matrix_builder.clear_instance(instance);
        self.instance = instance;
    }

    pub fn new_internal_branch(&mut self, name: &'static str) -> Node {
        self.new_internal_unknown(name, self.config.voltage_atol, "V", "A")
    }

    pub fn new_internal_node(&mut self, name: &'static str) -> Node {
        self.new_internal_unknown(name, self.config.current_atol, "A", "V")
    }
    pub fn new_internal_unknown(
        &mut self,
        name: &'static str,
        atol: f64,
        units: &'static str,
        residual_units: &'static str,
    ) -> Node {
        let name = format!("{}::{name}", self.circ[self.instance].name);
        self.node_info.push_and_get_key(NodeInfo { atol, name, units, residual_units })
    }

    pub fn ensure_matrix_entry(&mut self, column: Node, row: Node) {
        self.matrix_builder.insert(self.instance, column, row)
    }
}

impl Circuit {
    pub fn prepare_simulation(
        &self,
        eval_ctx: ExprEvalCtxRef,
        arena: &Arena,
        config: SimConfig,
    ) -> Result<Simulation> {
        let mut res = self.setup_simulation(config)?;
        res.prepare_solver(eval_ctx, arena)?;
        Ok(res)
    }

    pub fn setup_simulation(&self, config: SimConfig) -> Result<Simulation> {
        let model_data: Box<TiSlice<_, _>> = self
            .models()
            .map(|model| {
                let dev = self[model].device;
                self[dev].dev_impl.new_model()
            })
            .collect();

        let instance_data = self
            .instances()
            .map(|instance| {
                let model = self[instance].model;
                model_data[model].clone().new_instance()
            })
            .collect();

        let nodes = self
            .nodes()
            .map(|node| NodeInfo {
                name: self.node_name(node).to_owned(),
                atol: config.current_atol,
                units: "V",
                residual_units: "A",
            })
            .collect();

        let res = Simulation {
            model_data,
            instance_data,
            matrix_builder: MatrixBuilder::new(self),
            matrix: None,
            nodes,
            solution: vec![0f64; self.num_nodes() as usize].into(),
            circ: self,
            config,
            state: SimulationState::empty(),
            ac_solution: vec![Complex64::default(); self.num_nodes() as usize].into(),
            residual_resist: vec![0f64; self.num_nodes() as usize].into(),
            residual_react: vec![0f64; self.num_nodes() as usize].into(),
            omega: 1.0,
        };

        Ok(res)
    }
}

impl Simulation<'_> {
    fn vec_table<D: PrettyPrint>(solution: &[D], nodes: &[NodeInfo]) -> TableStruct {
        let mut res = vec![];
        for (&val, node) in zip(solution, nodes) {
            res.push(vec![node.name.clone().cell().bold(true), val.pretty_str().cell()]);
        }

        res.table()
    }

    pub fn print_solution(&self) {
        print_stdout(Self::vec_table(&self.solution.raw, &self.nodes.raw)).unwrap()
    }

    pub fn print_nonlinear_matrix(&self) -> Result<()> {
        print_stdout(self.nonlinear_matrix_table()?).unwrap();
        Ok(())
    }

    fn matrix_table<D: KluData + PrettyPrint>(
        nodes: &TiSlice<Node, NodeInfo>,
        matrix: &FixedKluMatrix<i32, D>,
    ) -> TableStruct {
        let mut header = vec!["".cell()];
        header.reserve(nodes.len() - 1);
        for node in nodes.iter().skip(1) {
            let name = node.name.clone().cell().bold(true).intense(true);
            header.push(name)
        }
        let mut table = vec![header];
        table.reserve(nodes.len());
        for (row_node, node_info) in nodes.iter_enumerated().skip(1) {
            let mut row = vec![node_info.name.clone().cell().bold(true).intense(true)];
            row.reserve(nodes.len());
            for col_node in nodes.keys().skip(1) {
                if let Some(val) = matrix.get(col_node.matrix_idx(), row_node.matrix_idx()) {
                    row.push(val.get().pretty_str().cell())
                } else {
                    row.push("".cell())
                }
            }
            table.push(row);
        }

        table.table()
    }

    fn nonlinear_matrix_table(&self) -> Result<TableStruct> {
        let matrix = &self
            .matrix
            .as_ref()
            .context("simulation must run before matrix can be printed")?
            .nonlinear_matrix;

        Ok(Self::matrix_table(&self.nodes, matrix))
    }

    pub fn prepare_solver(&mut self, mut eval_ctx: ExprEvalCtxRef, arena: &Arena) -> Result<()> {
        self.wipe_solution();
        for param in arena.ctx_params(self.circ.ctx) {
            if self.circ.param_assignments.contains_key(&param) {
                continue;
            }

            if eval_ctx[param] == Value::UNDEF {
                let (name, _) =
                    arena.lookup_param_info(param).expect("parameter belongs to the same arena");
                bail!("required parameter {name} was not provided")
            }
        }

        for (&param, &val) in self.circ.param_assignments.iter() {
            let val = val.eval(eval_ctx.borrow());
            eval_ctx.set_param(param, val?);
        }

        for model in self.circ.models() {
            let model_info = &self.circ[model];
            let model_data = &self.model_data[model];
            let dev = &self.circ[model_info.device];
            for &(param, val) in &model_info.parameters {
                let context = || match model_info.src {
                    CircuitModelSrc::Explicit(ref name) => {
                        format!(
                            "while evaluating parameter '{}' of model '{name}'",
                            dev.parameters[param].name,
                        )
                    }
                    CircuitModelSrc::Implicit(instance) => format!(
                        "while evaluating parameter '{}' of instance '{}'",
                        dev.parameters[param].name, self.circ[instance].name
                    ),
                };
                match dev.parameters[param].ty {
                    Type::Real => {
                        let val = val.eval_num(eval_ctx.borrow()).with_context(context)?;
                        model_data.set_real_param(param, val);
                    }

                    Type::Int => {
                        let val = val
                            .eval(eval_ctx.borrow())
                            .and_then(|res| res.to_int())
                            .with_context(context)?;
                        model_data.set_int_param(param, val);
                    }
                    Type::String => {
                        let val = val.eval_str(eval_ctx.borrow()).with_context(context)?;
                        model_data.set_str_param(param, val);
                    }
                }
            }
            model_data.process_params()?;
        }

        self.matrix_builder.reset(self.circ);
        self.nodes.truncate(self.circ.num_nodes() as usize);

        let mut builder = SimBuilder {
            circ: self.circ,
            instance: 0u32.into(),
            matrix_builder: &mut self.matrix_builder,
            node_info: &mut self.nodes,
            config: &self.config,
        };

        let temp = eval_ctx[CircuitParam::TEMPERATURE].to_num().context("invalid tempetaure")?;

        for inst in self.circ.instances() {
            let instance_info = &self.circ[inst];
            let instance_data = &mut self.instance_data[inst];
            let dev = &self.circ[self.circ[instance_info.model].device];
            for &(param, val) in &instance_info.parameters {
                let context = || {
                    format!(
                        "while evaluating parameter '{}' of instance '{}'",
                        dev.parameters[param].name, instance_info.name
                    )
                };
                match dev.parameters[param].ty {
                    Type::Real => {
                        let val = val.eval_num(eval_ctx.borrow()).with_context(context)?;
                        instance_data.set_real_param(param, val);
                    }
                    Type::Int => {
                        let val = val
                            .eval(eval_ctx.borrow())
                            .and_then(|res| res.to_int())
                            .with_context(context)?;
                        instance_data.set_int_param(param, val);
                    }
                    Type::String => {
                        let val = val.eval_str(eval_ctx.borrow()).with_context(context)?;
                        instance_data.set_str_param(param, val);
                    }
                }
            }

            builder.process_instance(inst);
            instance_data.process_params(temp, &mut builder, &self.circ[inst].connections)?;
        }

        let num_nodes = self.nodes.len();
        self.solution.resize(num_nodes, 0f64);
        self.residual_resist.resize(num_nodes, 0f64);
        self.residual_react.resize(num_nodes, 0f64);
        self.ac_solution.resize(num_nodes, Complex64::default());

        let matrix = SimulationMatrix::new_or_reset(self.matrix.take(), &self.matrix_builder);
        for (instance, data) in self.instance_data.iter_mut_enumerated() {
            let instance_entries = MatrixEntryIter::new(&matrix, &self.matrix_builder, instance);
            data.populate_matrix_ptrs(instance_entries)
        }
        self.matrix = Some(matrix);

        Ok(())
    }

    pub fn wipe_solution(&mut self) {
        self.solution.raw.fill(0.0);
        self.state.clear()
    }

    pub fn dc_op(&mut self) -> Result<&TiSlice<Node, f64>> {
        self.solve_op(OperatingPointAnalysis::DC)?;
        Ok(&self.solution)
    }

    pub fn restore_dc_op(&mut self, op: &TiSlice<Node, f64>) {
        self.solution.copy_from_slice(op);
        self.state = SimulationState::AT_DC_OP;
    }

    pub fn ac_op(&mut self) -> Result<&TiSlice<Node, f64>> {
        self.solve_op(OperatingPointAnalysis::AC)?;
        Ok(&self.solution)
    }

    pub fn restore_ac_op(&mut self, op: &TiSlice<Node, f64>) {
        self.solution.copy_from_slice(op);
        self.state = SimulationState::AT_AC_OP;
    }

    // pub fn noise_op(&mut self) -> Result<&TiSlice<Node, f64>> {
    //     self.solve_op(OperatingPointAnalysis::Noise)?;
    //     Ok(&self.solution)
    // }

    // pub fn restore_noise_op(&mut self, op: &TiSlice<Node, f64>) {
    //     self.solution.copy_from_slice(op);
    //     self.stat = SimulationState::AT_NOISE_OP;
    // }

    pub fn set_initial_guess(&mut self, guess: &TiSlice<Node, f64>) {
        self.solution.copy_from_slice(guess);
        self.state.clear();
    }

    fn solve_op(&mut self, analysis: OperatingPointAnalysis) -> Result<()> {
        let op_flag = analysis.solution_flags();

        if self.state.contains(op_flag) {
            return Ok(());
        }

        let flags = analysis.eval_flags();

        let debug = self.config.debug;
        let matrix =
            self.matrix.as_mut().context("Simulation must be populated before it can run")?;

        let mut i = 0;
        loop {
            let sim_info = SimInfo { abstime: 0f64, prev_solve: &self.solution, flags };
            for inst in &mut *self.instance_data {
                inst.eval(sim_info)?;

                // this is save because we call populate_matrix_ptrs during Simulation construction
                unsafe { inst.load_matrix_resist() }
                inst.load_residual_resist(&self.solution, &mut self.residual_resist);

                if analysis.time_integration() {
                    let alpha = 0.0;
                    unsafe { inst.load_matrix_react(alpha) }
                    inst.load_residual_react(&self.solution, &mut self.residual_react);
                }
            }

            // TODO time integration for tran analysis

            if debug {
                print_stdout(Self::matrix_table(&self.nodes, &matrix.nonlinear_matrix)).unwrap();
            }

            let singular = matrix.nonlinear_matrix.lu_factorize(None);
            if singular {
                bail!("matrix is singular")
            }

            matrix.nonlinear_matrix.solve_linear_system(&mut self.residual_resist.raw[1..]);

            if debug {
                print_stdout(Self::vec_table(&self.residual_resist.raw, &self.nodes.raw)).unwrap();
            }

            // reset matrix
            matrix.nonlinear_matrix.write_zero();
            let mut found_solution = true;
            for ((dst, delta), node_info) in
                zip(&mut self.solution.raw[1..], &mut self.residual_resist.raw[1..])
                    .zip(&self.nodes.raw[1..])
            {
                let delta = replace(delta, 0f64);
                let new_val = *dst - delta;
                let atol = node_info.atol;
                let tol = atol.max(new_val * self.config.rtol);
                if delta > tol {
                    found_solution = false;
                }
                *dst = new_val;
            }

            if debug {
                print_stdout(Self::vec_table(&self.solution.raw, &self.nodes.raw)).unwrap();
            }

            if found_solution && i > 0 {
                break;
            }
            i += 1;

            if i == self.config.maxiters {
                bail!("Simulation failed to converge after {i} iterations")
            }
        }

        self.state = op_flag;
        Ok(())
    }

    pub fn set_omega(&mut self, omega: f64) {
        if (self.omega - omega).abs() > f64::EPSILON {
            self.state.remove(SimulationState::AT_AC)
        }
        self.omega = omega;
    }

    pub fn ac(&mut self) -> Result<&TiSlice<Node, Complex64>> {
        self.ac_op()?;
        if self.state.contains(SimulationState::AT_AC) {
            return Ok(&self.ac_solution);
        }

        let matrix =
            self.matrix.as_mut().context("simulation must be setup before ac() is called")?;
        matrix.nonlinear_matrix.write_zero();
        matrix.ac_matrix.write_zero();
        self.ac_solution.raw.fill(Complex64::default());

        if self.state.contains(SimulationState::HAS_AC_EVAL) {
            for inst in &mut *self.instance_data {
                unsafe {
                    inst.load_matrix_react(self.omega);
                }
                inst.load_ac_residual(&self.solution, &mut self.ac_solution);
            }
        } else {
            let sim_info =
                SimInfo { abstime: 0f64, prev_solve: &self.solution, flags: EvalFlags::AC };
            for inst in &mut *self.instance_data {
                inst.eval(sim_info)?;

                unsafe {
                    inst.load_matrix_resist();
                    inst.load_matrix_react(self.omega);
                }
                inst.load_ac_residual(&self.solution, &mut self.ac_solution);
            }
            self.state.insert(SimulationState::HAS_AC_EVAL);

            for (dst, src) in zip(matrix.ac_matrix.data(), matrix.nonlinear_matrix.data()) {
                let val = Complex64::new(src.get(), dst.get().im);
                dst.set(val);
            }
        }

        if self.config.debug {
            println!("omega = {}", self.omega);
            print_stdout(Self::vec_table(&self.ac_solution.raw, &self.nodes.raw)).unwrap();
            print_stdout(Self::matrix_table(&self.nodes, &matrix.ac_matrix)).unwrap();
        }

        let is_singular = matrix.ac_matrix.lu_factorize(None);
        if is_singular {
            bail!("ac matrix is singular!")
        }
        matrix.ac_matrix.solve_linear_system(&mut self.ac_solution.raw[1..]);

        self.state.insert(SimulationState::AT_AC);
        Ok(&self.ac_solution)
    }

    pub fn ac_lead_current(&mut self, inst: InstanceId) -> Result<Vec<Complex64>> {
        self.ac()?;
        let mut dst = vec![Complex64::default(); self.circ[inst].connections.len()];
        self.instance_data[inst].load_ac_lead_current(&self.ac_solution, &mut dst);
        Ok(dst)
    }

    pub fn dc_lead_current(&mut self, inst: InstanceId) -> Result<Vec<f64>> {
        self.dc_op()?;
        let mut dst = vec![0f64; self.circ[inst].connections.len()];
        self.instance_data[inst].load_lead_current_resist(&self.solution, &mut dst);
        Ok(dst)
    }
}

pub struct SimConfig {
    pub debug: bool,
    pub maxiters: u32,
    pub voltage_atol: f64,
    pub current_atol: f64,
    pub rtol: f64,
}

impl Default for SimConfig {
    fn default() -> Self {
        SimConfig {
            debug: false,
            maxiters: 100,
            voltage_atol: 1e-6,
            current_atol: 1e-12,
            rtol: 1e-3,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SimInfo<'a> {
    pub abstime: f64,
    pub prev_solve: &'a TiSlice<Node, f64>,
    pub flags: EvalFlags,
}
