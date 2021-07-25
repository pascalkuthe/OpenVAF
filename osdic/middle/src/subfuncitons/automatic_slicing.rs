/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

//! This module contains most of the analysis functions that are used to automatically
//! slice the analog block into subfunctions.

use crate::frontend::{GeneralOsdiCall, GeneralOsdiInput};
use crate::optimize_cfg;
use crate::storage_locations::{StorageLocation, StorageLocationValue, StorageLocations};
use crate::subfuncitons::instance_init::InstanceInitFunction;
use crate::subfuncitons::instance_temp_update::InstanceTempUpdateFunction;
use crate::subfuncitons::model_init::ModelInitFunction;
use crate::subfuncitons::model_temp_update::ModelTempUpdateFunction;
use crate::subfuncitons::OsdiFunctions;
use crate::topology::CircuitTopology;
use itertools::Itertools;
use data_structures::{bit_set::BitSet, HashMap};
use hir::DisciplineAccess;
use ir::ids::VariableId;
use middle::cfg::{
    ControlFlowGraph, IntLocation, InternedLocations, LocationKind, PhiData, START_BLOCK,
};
use middle::{
    COperand, CfgFunctions, Local, LocalKind, Mir, OperandData, RValue, StmntKind,
    VariableLocalKind,
};
use pass::{
    program_dependence::{InvProgramDependenceGraph, ProgramDependenceGraph},
    visit::CfgVisitor,
    BackwardSlice, ForwardSlice, LiveLocalAnalysis, Strip, Visit,
};

/// On create the CFG of the full analog block is walked.
/// Any instructions that are tainted for a certain subfunction are added to the appropriately named fields.
/// A instruction is tainted if it reads data not yet available during the function.
///
/// # Examples
///
/// * Branch access (`V(a,b)`) is only available during the load function is therefore always marked as tainted.
/// * Temperature (`$temperature`) is not available during model and instance init
pub struct TaintedLocations {
    pub model_temp_update: BitSet<IntLocation>,
    pub model_init: BitSet<IntLocation>,
    pub instance_init: BitSet<IntLocation>,
    pub instance_temp_update: BitSet<IntLocation>,
    pub by_time_derivative: BitSet<IntLocation>,
    pub by_branch_write: BitSet<IntLocation>,

    pub by_stamp_write: BitSet<IntLocation>,
    // pub by_noise: BitSet<IntLocation> TODO NOISE
}

impl TaintedLocations {
    pub fn new(
        cfg: &mut ControlFlowGraph<GeneralOsdiCall>,
        locations: &InternedLocations,
        topology: &CircuitTopology,
    ) -> Self {
        let mut res = TaintedLocations {
            model_temp_update: BitSet::new_empty(locations.len_idx()),
            model_init: BitSet::new_empty(locations.len_idx()),
            instance_init: BitSet::new_empty(locations.len_idx()),
            instance_temp_update: BitSet::new_empty(locations.len_idx()),
            by_time_derivative: BitSet::new_empty(locations.len_idx()),
            by_branch_write: BitSet::new_empty(locations.len_idx()),
            by_stamp_write: BitSet::new_empty(locations.len_idx()),
        };

        cfg.modify(Visit {
            visitor: &mut TaintedLocationFinder {
                dst: &mut res,
                topology,
            },
            locations: &locations,
        });

        res
    }

    pub fn taint_variable_access_for_init_functions(
        &mut self,
        instance_storage: &BitSet<StorageLocation>,
        instance_temp_tainted: &BitSet<StorageLocation>,
        instance_init_tainted: &BitSet<StorageLocation>,
        model_init_tainted: &BitSet<StorageLocation>,
        storage: &StorageLocations,
        pdg: &ProgramDependenceGraph,
    ) {
        for storage_loc in instance_storage.ones() {
            let assignments = &pdg.data_dependencies.assignments[storage[storage_loc].local];
            if let Some(assignments) = assignments {
                self.model_temp_update.union_with(assignments);
                self.model_init.union_with(assignments);
            }
        }

        for storage_loc in model_init_tainted.ones() {
            let assignments = &pdg.data_dependencies.assignments[storage[storage_loc].local];
            if let Some(assignments) = assignments {
                self.model_init.union_with(assignments);
            }
        }

        for storage_loc in instance_init_tainted.ones() {
            let assignments = &pdg.data_dependencies.assignments[storage[storage_loc].local];
            if let Some(assignments) = assignments {
                self.instance_init.union_with(assignments);
            }
        }

        for storage_loc in instance_temp_tainted.ones() {
            let assignments = &pdg.data_dependencies.assignments[storage[storage_loc].local];
            if let Some(assignments) = assignments {
                self.instance_temp_update.union_with(assignments);
            }
        }
    }
}

struct TaintedLocationFinder<'a> {
    dst: &'a mut TaintedLocations,
    topology: &'a CircuitTopology,
}

impl<'a> CfgVisitor<GeneralOsdiCall> for TaintedLocationFinder<'a> {
    fn visit_stmnt(
        &mut self,
        stmnt: &StmntKind<GeneralOsdiCall>,
        loc: IntLocation,
        cfg: &ControlFlowGraph<GeneralOsdiCall>,
    ) {
        match *stmnt {
            StmntKind::Assignment(dst, ref val) => {
                match cfg.locals[dst].kind {
                    LocalKind::Variable(_, VariableLocalKind::Derivative(_)) => {
                        // Just doesnt make sense to do this
                        // Probably doesnt occur in practice of the other algorithems (const prop) do their job
                        // This is done just to be save!

                        self.dst.model_init.insert(loc);
                        self.dst.model_temp_update.insert(loc);
                        self.dst.instance_temp_update.insert(loc);
                        self.dst.instance_init.insert(loc);
                    }
                    LocalKind::Branch(_, _, ref kind) => {
                        if kind == &VariableLocalKind::User {
                            self.dst.by_branch_write.insert(loc);
                        } else if self.topology.matrix_stamp_locals.contains(dst) {
                            self.dst.by_branch_write.insert(loc);
                            self.dst.by_stamp_write.insert(loc);
                        }

                        self.dst.model_init.insert(loc);
                        self.dst.model_temp_update.insert(loc);
                        self.dst.instance_temp_update.insert(loc);
                        self.dst.instance_init.insert(loc);
                    }
                    _ => {}
                }
                self.visit_rvalue(val, loc, cfg)
            }
            StmntKind::Call(ref call, ref args, _) => {
                if matches!(call, GeneralOsdiCall::NodeCollapse(_, _)) {
                    self.dst.model_init.insert(loc);
                    self.dst.model_temp_update.insert(loc);
                    self.dst.instance_temp_update.insert(loc);
                }
                for arg in args {
                    self.visit_operand(arg, loc, cfg)
                }
            }
            StmntKind::NoOp => {}
        }
    }

    fn visit_phi(
        &mut self,
        phi: &PhiData,
        loc: IntLocation,
        cfg: &ControlFlowGraph<GeneralOsdiCall>,
    ) {
        if matches!(cfg.locals[phi.dst].kind, LocalKind::Branch(_, _, _)) {
            self.dst.model_init.insert(loc);
            self.dst.model_temp_update.insert(loc);
            self.dst.instance_temp_update.insert(loc);
            self.dst.instance_init.insert(loc);
        }
    }
    fn visit_input(
        &mut self,
        input: &GeneralOsdiInput,
        loc: IntLocation,
        _cfg: &ControlFlowGraph<GeneralOsdiCall>,
    ) {
        match input {
            GeneralOsdiInput::SimParam(_, _) | GeneralOsdiInput::Voltage(_, _) => {
                self.dst.model_init.insert(loc);
                self.dst.model_temp_update.insert(loc);
                self.dst.instance_temp_update.insert(loc);
                self.dst.instance_init.insert(loc);
            }

            GeneralOsdiInput::Temperature => {
                self.dst.model_init.insert(loc);
                self.dst.instance_init.insert(loc);
            }

            GeneralOsdiInput::PortConnected(_) => {
                self.dst.model_temp_update.insert(loc);
                self.dst.model_init.insert(loc);
            }

            GeneralOsdiInput::Parameter(_) => {}
        }
    }

    fn visit_rvalue(
        &mut self,
        rval: &RValue<GeneralOsdiCall>,
        loc: IntLocation,
        cfg: &ControlFlowGraph<GeneralOsdiCall>,
    ) {
        if matches!(
            rval,
            RValue::Call(GeneralOsdiCall::TimeDerivative, _, _)
                | RValue::Call(GeneralOsdiCall::SymbolicDerivativeOfTimeDerivative, _, _)
        ) {
            self.dst.model_init.insert(loc);
            self.dst.model_temp_update.insert(loc);
            self.dst.instance_temp_update.insert(loc);
            self.dst.instance_init.insert(loc);
            self.dst.by_time_derivative.insert(loc)
        }

        if matches!(rval, RValue::Call(GeneralOsdiCall::Lim { .. }, _, _)) {
            self.dst.model_init.insert(loc);
            self.dst.model_temp_update.insert(loc);
            self.dst.instance_temp_update.insert(loc);
            self.dst.instance_init.insert(loc);
        }

        // if matches!(rval, RValue::Call(GeneralOsdiCall::Noise,__)){
        //     self.by_noise.insert(loc)
        // } TODO NOISE

        for operand in rval.operands() {
            self.visit_operand(operand, loc, cfg)
        }
    }
}

/// CFG visit that finds all Instructions that write to a variable or branch (and both of their derivatives) or are a call stmnt (print, collapse hints).
/// The instructions regarded as the output of functions are a subset of these statements.
pub struct OutputLocations<'a> {
    dst: BitSet<IntLocation>,
    topology: &'a CircuitTopology,
}

impl<'a, C: CfgFunctions> CfgVisitor<C> for OutputLocations<'a> {
    fn visit_stmnt(&mut self, stmnt: &StmntKind<C>, loc: IntLocation, cfg: &ControlFlowGraph<C>) {
        match *stmnt {
            // TODO treat derivatives as temproaries?
            StmntKind::Assignment(dst, _)
                if matches!(cfg.locals[dst].kind, LocalKind::Variable(_, _)) =>
            {
                self.dst.insert(loc)
            }
            StmntKind::Assignment(dst, _) if self.topology.matrix_stamp_locals.contains(dst) => {
                self.dst.insert(loc)
            }
            StmntKind::Call(_, _, _) => self.dst.insert(loc),
            _ => {}
        }
    }
}

impl<'a> OutputLocations<'a> {
    pub fn find_in_cfg(
        cfg: &mut ControlFlowGraph<GeneralOsdiCall>,
        locations: &InternedLocations,
        topology: &CircuitTopology,
    ) -> BitSet<IntLocation> {
        let mut find_output_stmnnts = OutputLocations {
            dst: BitSet::new_empty(locations.len_idx()),
            topology,
        };

        cfg.modify(Visit {
            visitor: &mut find_output_stmnnts,
            locations: &locations,
        });

        find_output_stmnnts.dst
    }
}

/// CFG visit that finds all variables that are written to in a CFG
pub struct WrittenStorage<'a> {
    dst: BitSet<StorageLocation>,
    storage: &'a StorageLocations,
}

impl<'a, C: CfgFunctions> CfgVisitor<C> for WrittenStorage<'a> {
    fn visit_stmnt(&mut self, stmnt: &StmntKind<C>, _loc: IntLocation, cfg: &ControlFlowGraph<C>) {
        if let StmntKind::Assignment(dst, _) = *stmnt {
            let written_storage = match cfg.locals[dst].kind {
                LocalKind::Variable(var, ref kind) => {
                    self.storage.find_variable_location(var, kind.clone())
                }
                LocalKind::Branch(DisciplineAccess::Flow, branch, ref kind) => {
                    self.storage.find_branch_location(branch, kind.clone())
                }
                _ => return,
            };
            self.dst.insert(written_storage)
        }
    }
}

impl<'a> WrittenStorage<'a> {
    pub fn find_in_cfg<C: CfgFunctions>(
        storage: &'a StorageLocations,
        cfg: &mut ControlFlowGraph<C>,
        locations: &InternedLocations,
    ) -> BitSet<StorageLocation> {
        let mut res = Self {
            dst: BitSet::new_empty(storage.len_idx()),
            storage,
        };

        cfg.modify(Visit {
            visitor: &mut res,
            locations,
        });

        res.dst
    }
}

/// Function used internally to create a cfg for an osdi function from the cfg of the full analog block
///
/// # Parameters
///
/// * `full_cfg` The full ControlFlowGraph of the analog block
/// * `tainted_locations` Instructions that read data that is not available during the function (example: voltages during temperature update)
/// * `assumed_locations` Instructions whose value is computed in other functions and can be assumed here
/// * `output_statements` All Instructions that write to a variable (therefore considered the results of functions)
///
/// # Returns
///
/// * ControlFlowGraph of the function
/// * All instructions which are part of this function and write to a variable. These are to be considered the "output" of this function
pub(super) fn function_cfg_from_full_cfg(
    full_cfg: &ControlFlowGraph<GeneralOsdiCall>,
    tainted_locations: &BitSet<IntLocation>,
    assumed_locations: Option<&BitSet<IntLocation>>,
    all_output_locations: &BitSet<IntLocation>,
    locations: &InternedLocations,
    inv_pdg: &InvProgramDependenceGraph,
    pdg: &ProgramDependenceGraph,
    storage: &StorageLocations,
) -> (
    ControlFlowGraph<GeneralOsdiCall>,
    BitSet<IntLocation>,
    BitSet<StorageLocation>,
    BitSet<StorageLocation>,
) {
    let mut cfg = full_cfg.clone();

    // println!(
    //     "{:?}",
    //     tainted_locations
    //         .ones()
    //         .filter_map(
    //             |loc| if let LocationKind::Statement(stmnt) = locations[loc].kind {
    //                 Some((locations[loc].block, stmnt))
    //             } else {
    //                 None
    //             }
    //         )
    //         .collect_vec()
    // );

    let mut allowed_locations = cfg.modify(ForwardSlice {
        tainted_locations: tainted_locations.clone(),
        pdg: inv_pdg,
        locations,
        taint_control_dependencies: true,
    });

    let mut relevant_locations = all_output_locations.clone();
    if let Some(assumed_locations) = assumed_locations {
        relevant_locations.difference_with(assumed_locations);
    }

    let relevant_locations = cfg.modify(BackwardSlice {
        relevant_locations,
        assumed_locations: assumed_locations
            .cloned()
            .unwrap_or_else(|| BitSet::new_empty(locations.len_idx())),
        pdg,
        locations,
    });

    let mut retained_locations = {
        allowed_locations.intersect_with(&relevant_locations);
        allowed_locations
    };

    cfg.modify(Strip {
        retain: &retained_locations,
        locations,
    });

    let output_locations = {
        retained_locations.intersect_with(all_output_locations);
        retained_locations
    };

    optimize_cfg(&mut cfg);

    let written_vars = WrittenStorage::find_in_cfg(storage, &mut cfg, locations);

    let read_vars = ReadVars::find_in_cfg(storage, &mut cfg);

    (cfg, output_locations, written_vars, read_vars)
}

pub struct ReadVars;

impl ReadVars {
    pub fn find_in_cfg<C: CfgFunctions>(
        storage: &StorageLocations,
        cfg: &mut ControlFlowGraph<C>,
    ) -> BitSet<StorageLocation> {
        let mut res = BitSet::new_empty(storage.len_idx());
        for local in cfg.modify(LiveLocalAnalysis(None)).out_sets[START_BLOCK].ones() {
            let written_storage = match cfg.locals[local].kind {
                LocalKind::Variable(var, ref kind) => {
                    storage.find_variable_location(var, kind.clone())
                }
                LocalKind::Branch(DisciplineAccess::Flow, branch, ref kind) => {
                    storage.find_branch_location(branch, kind.clone())
                }
                _ => continue,
            };
            res.insert(written_storage)
        }
        res
    }
}

impl OsdiFunctions {
    pub(super) fn init_functions_from_tainted_locs(
        mir: &Mir<GeneralOsdiCall>,
        cfg: &mut ControlFlowGraph<GeneralOsdiCall>,
        locations: &InternedLocations,
        pdg: &ProgramDependenceGraph,
        inverse_pdg: &InvProgramDependenceGraph,
        tainted_locations: &TaintedLocations,
        output_locations: &BitSet<IntLocation>,
        storage: &StorageLocations,
    ) -> (
        ModelInitFunction,
        ModelTempUpdateFunction,
        InstanceInitFunction,
        InstanceTempUpdateFunction,
        BitSet<IntLocation>,
    ) {
        let (model_init, model_init_output) = ModelInitFunction::new(
            mir,
            &cfg,
            &tainted_locations.model_init,
            locations,
            pdg,
            inverse_pdg,
            &output_locations,
            storage,
        );

        let mut assumed_locations = model_init_output;

        let (model_temp_update, model_temp_update_output) = ModelTempUpdateFunction::new(
            &cfg,
            &tainted_locations.model_temp_update,
            &assumed_locations,
            locations,
            pdg,
            inverse_pdg,
            &output_locations,
            storage,
        );

        assumed_locations.union_with(&model_temp_update_output);

        let (instance_init, instance_init_output) = InstanceInitFunction::new(
            &cfg,
            &tainted_locations.instance_init,
            &assumed_locations,
            &locations,
            pdg,
            inverse_pdg,
            &output_locations,
            storage,
        );

        assumed_locations.union_with(&instance_init_output);

        let (instance_temp_update, instance_temp_update_output) = InstanceTempUpdateFunction::new(
            &cfg,
            &tainted_locations.instance_temp_update,
            &assumed_locations,
            &locations,
            pdg,
            inverse_pdg,
            &output_locations,
            storage,
        );
        assumed_locations.union_with(&instance_temp_update_output);

        (
            model_init,
            model_temp_update,
            instance_init,
            instance_temp_update,
            assumed_locations,
        )
    }
}
