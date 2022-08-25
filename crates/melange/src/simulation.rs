use std::cell::Cell;
use std::ptr::NonNull;
use std::rc::Rc;
use std::slice;

use anyhow::{bail, Result};
use klu_rs::KluSettings;
use num_complex::Complex64;
use stdx::iter::zip;
use typed_index_collections::{TiSlice, TiVec};

use crate::circuit::{InstanceId, ModelId, Node};
use crate::devices::{InstanceImpl, MatrixEntry, ModelImpl, Tolerance};
use crate::expr::ExprEvalCtx;
use crate::solver::{ComplexMatrix, MatrixBuilder, MatrixSpec, RealMatrix};
use crate::{Circuit, ExprArena, Value};

pub struct Simulation<'a> {
    circ: &'a Circuit,
    atol: Box<TiSlice<Node, f64>>,
    model_data: TiVec<ModelId, Rc<dyn ModelImpl>>,
    instance_data: TiVec<InstanceId, Box<dyn InstanceImpl>>,
    matrix_spec: MatrixSpec,
    solution: Box<TiSlice<Node, f64>>,
    at_dc_solution: bool,
    residual: Box<[Complex64]>,
    nonlinear_matrix: RealMatrix,
    ac_matrix: ComplexMatrix,
    rtol: f64,
    dump: NonNull<Cell<f64>>,
}

pub struct SimBuilder {
    matrix_entries: TiVec<InstanceId, Vec<(Node, Node)>>,
    instance: InstanceId,
    builder: MatrixBuilder,
    atol: TiVec<Node, f64>,
}

impl SimBuilder {
    pub fn new_internal_node(&mut self, atol: Tolerance) -> Node {
        let atol = match atol {
            Tolerance::Current => 1e-12,
            Tolerance::Voltage => 1e-6,
            Tolerance::Other(atol) => atol,
        };
        self.atol.push_and_get_key(atol)
    }

    pub fn ensure_matrix_entry(&mut self, col: Node, row: Node) {
        if col == Node::GROUND || row == Node::GROUND {
            self.matrix_entries[self.instance].push((Node::GROUND, Node::GROUND))
        } else {
            self.builder.add_entry(col.matrix_idx(), row.matrix_idx());
            self.matrix_entries[self.instance].push((col, row))
        }
    }
}

pub struct MatrixEntryIter<'a> {
    iter: std::slice::Iter<'a, (Node, Node)>,
    nonlinear_matrix: &'a RealMatrix,
    ac_matrix: &'a ComplexMatrix,
    dump: &'a Cell<f64>,
}

impl<'a> Iterator for MatrixEntryIter<'a> {
    type Item = MatrixEntry<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|&(col, row)| {
            if row == Node::GROUND {
                return MatrixEntry { resist: self.dump, react: self.dump };
            }
            let resist = &self.nonlinear_matrix[(col.matrix_idx(), row.matrix_idx())];
            let complex = &self.ac_matrix[(col.matrix_idx(), row.matrix_idx())]
                as *const Cell<Complex64> as *const Cell<[f64; 2]>;

            // this is save because the original cell is still valid and because Complex64 is
            // garunteed to be layout compatible with [f64;2]
            // FIXME us as_array_of_cells instead when it becomes stable
            // SAFETY: `Cell<T>` has the same memory layout as `T`.
            let complex = unsafe { &*(complex as *const [Cell<f64>; 2]) };
            let react = &complex[1];
            MatrixEntry { resist, react }
        })
    }
}

impl Circuit {
    pub fn prepare_simulation(
        &self,
        mut eval_ctx: ExprEvalCtx,
        earena: &ExprArena,
    ) -> Result<Simulation> {
        for param in earena.ctx_params(self.ctx) {
            if self.param_assignments.contains_key(&param) {
                continue;
            }

            if eval_ctx[param] == Value::UNDEF {
                let (name, _) =
                    earena.lookup_param_info(param).expect("parameter belongs to the same arena");
                bail!("required parameter {name} was not provied")
            }
        }

        for (&param, &val) in self.param_assignments.iter() {
            let val = val.eval(eval_ctx.borrow());
            eval_ctx.set_param(param, val);
        }

        let model_data: Result<_> = self
            .models()
            .map(|model| {
                let dev = self[model].device;
                let data = self[dev].dev_impl.new_model(eval_ctx.borrow(), &self[model].parameters);
                data
            })
            .collect();
        let model_data: TiVec<_, _> = model_data?;

        let mut builder = SimBuilder {
            matrix_entries: vec![Vec::with_capacity(8); self.num_instances() as usize].into(),
            instance: 0u32.into(),
            builder: MatrixBuilder::new(self.nodes.len() as i32 - 1),
            atol: vec![1e-6; self.nodes.len()].into(),
        };

        let instance_data: Result<_> = self
            .instances()
            .map(|instance| {
                builder.instance = instance;
                let model = self[instance].model;
                let data = model_data[model].clone().new_instance(
                    &mut builder,
                    eval_ctx.borrow(),
                    &self[instance].parameters,
                    &self[instance].port_connections,
                );
                data
            })
            .collect();

        let mut instance_data: TiVec<InstanceId, _> = instance_data?;
        let SimBuilder { matrix_entries, builder, atol, .. } = builder;
        let matrix_spec = builder.finish(KluSettings::new());
        let nonlinear_matrix = matrix_spec.clone().create_matrix().unwrap();
        let ac_matrix = matrix_spec.clone().create_matrix().unwrap();
        let dump = Box::into_raw(Box::new(Cell::new(0f64)));
        let dump = NonNull::new(dump).expect("Box::into_raw always returns a valid pointer");

        for (instance, data) in instance_data.iter_mut_enumerated() {
            let matrix_entries = MatrixEntryIter {
                iter: matrix_entries[instance].iter(),
                nonlinear_matrix: &nonlinear_matrix,
                ac_matrix: &ac_matrix,
                // This is save because dump is a valid box allocation (see above)
                dump: unsafe { dump.as_ref() },
            };
            data.populate_matrix_ptrs(matrix_entries)
        }

        let num_nodes = atol.len() as usize;
        let res = Simulation {
            atol: atol.into_boxed_slice(),
            model_data,
            instance_data,
            matrix_spec,
            nonlinear_matrix,
            ac_matrix,
            solution: vec![0f64; num_nodes].into_boxed_slice().into(),
            at_dc_solution: false,
            residual: vec![Complex64::default(); num_nodes * 2].into_boxed_slice(),
            rtol: 1e-3,
            dump,
            circ: self,
        };

        Ok(res)
    }
}

fn split_residual(
    residual: &mut [Complex64],
) -> (&mut TiSlice<Node, f64>, &mut TiSlice<Node, f64>) {
    let len = residual.len();
    let ptr = residual.as_ptr() as *mut Complex64 as *mut f64;
    let data = unsafe {
        // this is save because Complex64 is garunteed to have the same size/alginment as [f64; 2]
        slice::from_raw_parts_mut(ptr, 2 * len)
    };
    let (resist, react) = data.split_at_mut(len);
    (TiSlice::from_mut(resist), TiSlice::from_mut(react))
}

impl Simulation<'_> {
    pub fn find_dc_solution(&mut self) -> Result<&TiSlice<Node, f64>> {
        let (residual, _) = split_residual(&mut self.residual);

        let mut i = 0;
        loop {
            let sim_info = SimInfo {
                abstime: 0f64,
                calc_react: false,
                calc_resist: true,
                prev_solve: &self.solution,
            };
            for inst in &mut self.instance_data {
                inst.eval(sim_info);
                // this is save because we call populate_matrix_ptrs during Simulation construction
                unsafe { inst.load_matrix_resist() }
                inst.load_residual_resist(&self.solution, residual);
            }

            let singular = self.nonlinear_matrix.lu_factorize(Some(1e-12));
            if singular {
                bail!("matrix is singular")
            }
            self.nonlinear_matrix.solve_linear_system(&mut residual.raw[1..]);
            let mut found_solution = true;
            for ((dst, delta), &atol) in
                zip(&mut self.solution.raw[1..], &mut residual.raw[1..]).zip(&self.atol.raw[1..])
            {
                let new_val = *dst + *delta;
                let tol = atol.max(new_val * self.rtol);
                if *delta > atol && tol > *delta {
                    found_solution = false;
                }
                *dst = new_val;
            }

            if found_solution {
                break;
            }
            i += 1;

            // TODO configure iters
            if i == 100 {
                // TODO better error
                bail!("Simulation failed to converge after 100 iterations")
            }
        }

        self.at_dc_solution = true;

        Ok(&*self.solution)
    }

    pub fn dc_lead_current(&self, inst: InstanceId) -> Vec<f64> {
        let mut dst = vec![0f64; self.circ[inst].port_connections.len()];
        self.instance_data[inst].load_lead_current_resist(&self.solution, &mut dst);
        dst
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SimInfo<'a> {
    pub abstime: f64,
    pub prev_solve: &'a TiSlice<Node, f64>,
    pub calc_react: bool,
    pub calc_resist: bool,
}
