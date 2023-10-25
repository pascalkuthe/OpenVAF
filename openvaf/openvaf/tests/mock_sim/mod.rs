use std::cell::UnsafeCell;
use std::mem::swap;
use std::ptr;

use anyhow::Result;
use indexmap::IndexSet;
use libc::c_void;
use stdx::iter::zip;

pub const ALPHA: f64 = 0.172;

use crate::load::{
    osdi_str, EvalFlags, EvalRetFlags, OsdiInstance, OsdiModel, OsdiSimInfo, OsdiSimParas,
};

#[derive(Debug, Default)]
pub struct MockSimulation {
    pub nodes: IndexSet<&'static str>,
    pub residual_resist: Vec<f64>,
    pub residual_react: Vec<f64>,
    pub solve: Vec<f64>,
    pub jacobian_info: IndexSet<(u32, u32)>,
    pub jacobian_resist: &'static [UnsafeCell<f64>],
    pub jacobian_react: &'static [UnsafeCell<f64>],
    pub state_1: Vec<f64>,
    pub state_2: Vec<f64>,
}
impl MockSimulation {
    fn new() -> MockSimulation {
        MockSimulation {
            nodes: {
                let mut set = IndexSet::new();
                set.insert("gnd");
                set
            },
            residual_resist: vec![0.0],
            residual_react: vec![0.0],
            solve: vec![0.0],
            jacobian_info: {
                let mut set = IndexSet::new();
                set.insert((0, 0));
                set
            },
            jacobian_resist: &[],
            jacobian_react: &[],
            state_1: Vec::new(),
            state_2: Vec::new(),
        }
    }

    fn register_node(&mut self, name: &'static str) -> u32 {
        let (idx, changed) = self.nodes.insert_full(name);
        assert!(changed);
        self.residual_resist.push(0.0);
        self.residual_react.push(0.0);
        self.solve.push(0.0);
        idx as u32
    }

    fn register_jacobian_entry(&mut self, hi: u32, lo: u32) {
        if hi != 0 && lo != 0 {
            self.jacobian_info.insert((hi, lo));
        }
    }

    fn get_jacobian_entry(&mut self, hi: u32, lo: u32) -> usize {
        if hi == 0 || lo == 0 {
            0
        } else {
            self.jacobian_info.get_index_of(&(hi, lo)).unwrap()
        }
    }

    pub fn set_voltage(&mut self, node: &str, voltage: f64) {
        let i = self.nodes.get_index_of(node).unwrap();
        self.solve[i] = voltage
    }

    pub fn read_residual(&self, node: &str) -> (f64, f64) {
        let i = self.nodes.get_index_of(node).unwrap();
        (self.residual_resist[i], self.residual_react[i])
    }

    pub fn read_jacobian(&self, hi: &str, lo: &str) -> (f64, f64) {
        let hi = self.nodes.get_index_of(hi).unwrap() as u32;
        let lo = self.nodes.get_index_of(lo).unwrap() as u32;
        let i = self.jacobian_info.get_index_of(&(hi, lo)).unwrap();
        unsafe { (self.jacobian_resist[i].get().read(), self.jacobian_react[i].get().read()) }
    }

    fn build_jacobian(&mut self) {
        self.jacobian_resist =
            (0..self.jacobian_info.len()).map(|_| UnsafeCell::new(0.0)).collect::<Vec<_>>().leak();
        self.jacobian_react =
            (0..self.jacobian_info.len()).map(|_| UnsafeCell::new(0.0)).collect::<Vec<_>>().leak();
    }

    pub(crate) fn clear(&mut self) {
        self.residual_resist.fill(0.0);
        self.residual_react.fill(0.0);
        for entry in self.jacobian_resist {
            unsafe { entry.get().write(0.0) };
        }
        for entry in self.jacobian_react {
            unsafe { entry.get().write(0.0) };
        }
    }
    pub(crate) fn next_iter(&mut self) {
        self.solve.fill(0.0);
        swap(&mut self.state_1, &mut self.state_2);
        self.clear();
    }
}

impl OsdiInstance {
    pub(super) fn mock_simulation(
        &mut self,
        model: &OsdiModel,
        connected_terminals: u32,
        temp: f64,
    ) -> Result<MockSimulation> {
        let mut internal_nodes = self.process_params(model, connected_terminals, temp)?;
        let mut sim = MockSimulation::new();
        // create internal nodes
        let terminals: Vec<_> = self.descriptor.nodes()[..connected_terminals as usize]
            .iter()
            .map(|node| unsafe { sim.register_node(osdi_str(node.name)) })
            .collect();

        for node_idx in &mut internal_nodes {
            let node = &self.descriptor.nodes()[*node_idx as usize];
            *node_idx = unsafe { sim.register_node(osdi_str(node.name)) };
        }

        let node_mapping = self.node_mapping();
        for node in node_mapping {
            let idx = node.get();
            if let Some(&terminal) = terminals.get(idx as usize) {
                node.set(terminal)
            } else if idx == u32::MAX {
                node.set(0)
            } else {
                node.set(internal_nodes[idx as usize - terminals.len()])
            }
        }

        // create jacobian
        for entry in self.descriptor.matrix_entries() {
            let column = node_mapping[entry.nodes.node_1 as usize].get();
            let row = node_mapping[entry.nodes.node_2 as usize].get();
            sim.register_jacobian_entry(row, column);
        }
        sim.build_jacobian();

        // populate matrix ptrs
        for (entry, ptr_resist) in zip(self.descriptor.matrix_entries(), self.matrix_ptrs_resist())
        {
            let column = node_mapping[entry.nodes.node_1 as usize].get();
            let row = node_mapping[entry.nodes.node_2 as usize].get();
            let i = sim.get_jacobian_entry(row, column);
            ptr_resist.set(sim.jacobian_resist[i].get());
            if entry.react_ptr_off != u32::MAX {
                let data = self.data as *mut u8;
                unsafe {
                    let react_ptr_ptr: *mut *mut f64 =
                        data.add(entry.react_ptr_off as usize).cast();
                    *react_ptr_ptr = sim.jacobian_react[i].get();
                }
            }
        }
        sim.state_1.resize(self.descriptor.num_states as usize, 0.0);
        sim.state_2.resize(self.descriptor.num_states as usize, 0.0);
        Ok(sim)
    }

    pub fn load_spice(&self, model: &OsdiModel, sim: &mut MockSimulation) {
        self.descriptor.load_spice_rhs_tran(
            self.data,
            model.data,
            sim.residual_resist.as_mut_ptr(),
            sim.solve.as_mut_ptr(),
            ALPHA,
        );
        self.descriptor.load_jacobian_tran(self.data, self.data, ALPHA);
    }

    pub fn load_dae(&self, model: &OsdiModel, sim: &mut MockSimulation) {
        self.descriptor.load_residual_resist(
            self.data,
            model.data,
            sim.residual_resist.as_mut_ptr(),
        );
        self.descriptor.load_limit_rhs_resist(
            self.data,
            model.data,
            sim.residual_resist.as_mut_ptr(),
        );

        self.descriptor.load_residual_react(self.data, model.data, sim.residual_react.as_mut_ptr());
        self.descriptor.load_limit_rhs_react(
            self.data,
            model.data,
            sim.residual_react.as_mut_ptr(),
        );

        self.descriptor.load_jacobian_resist(self.data, model.data);
        self.descriptor.load_jacobian_react(self.data, model.data, 1.0);
    }
    pub fn eval(
        &self,
        model: &OsdiModel,
        sim: &mut MockSimulation,
        mut flags: EvalFlags,
    ) -> EvalRetFlags {
        // always calculate everything
        flags |= EvalFlags::CALC_RESIST_JACOBIAN
            | EvalFlags::CALC_RESIST_RESIDUAL
            | EvalFlags::CALC_RESIST_LIM_RHS
            | EvalFlags::CALC_REACT_JACOBIAN
            | EvalFlags::CALC_REACT_RESIDUAL
            | EvalFlags::CALC_REACT_LIM_RHS
            | EvalFlags::CALC_NOISE;
        let sim_params = OsdiSimParas {
            names: &mut ptr::null_mut(),
            vals: ptr::null_mut(),
            names_str: &mut ptr::null_mut(),
            vals_str: ptr::null_mut(),
        };
        let mut sim_info = OsdiSimInfo {
            paras: sim_params,
            abstime: 0.0,
            prev_solve: sim.solve.as_ptr() as *mut f64,
            prev_state: sim.state_1.as_mut_ptr(),
            next_state: sim.state_2.as_mut_ptr(),
            flags: flags.bits(),
        };
        let flags = self.descriptor.eval(
            b"foo\0".as_ptr() as *mut c_void,
            self.data,
            model.data,
            &mut sim_info,
        );
        EvalRetFlags::from_bits(flags).unwrap()
    }
}
