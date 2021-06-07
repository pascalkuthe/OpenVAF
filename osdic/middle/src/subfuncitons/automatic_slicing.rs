//! This module contains most of the analysis functions that are used to automatically
//! slice the analog block into subfunctions.

use crate::frontend::{GeneralOsdiCall, GeneralOsdiInput};
use crate::subfuncitons::instance_init::InstanceInitFunction;
use crate::subfuncitons::instance_temp_update::InstanceTempUpdateFunction;
use crate::subfuncitons::model_init::ModelInitFunction;
use crate::subfuncitons::model_temp_update::ModelTempUpdateFunction;
use crate::subfuncitons::{optimize_cfg, OsdiFunctions};
use openvaf_data_structures::BitSet;
use openvaf_ir::ids::VariableId;
use openvaf_middle::cfg::{ControlFlowGraph, IntLocation, InternedLocations, PhiData};
use openvaf_middle::{COperand, CallType, Local, LocalKind, Mir, OperandData, RValue, StmntKind};
use openvaf_transformations::{CfgVisitor, ForwardSlice, InvProgramDependenceGraph, Strip, Visit};

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
    // pub by_noise: BitSet<IntLocation> TODO NOISE
}

impl CfgVisitor<GeneralOsdiCall> for TaintedLocations {
    fn visit_input(
        &mut self,
        input: &GeneralOsdiInput,
        loc: IntLocation,
        _cfg: &ControlFlowGraph<GeneralOsdiCall>,
    ) {
        match input {
            GeneralOsdiInput::SimParam(_, _)
            | GeneralOsdiInput::Voltage(_, _)
            | GeneralOsdiInput::Current(_)
            | GeneralOsdiInput::PortFlow(_)
            | GeneralOsdiInput::Lim { .. } => {
                self.model_init.insert(loc);
                self.model_temp_update.insert(loc);
                self.instance_temp_update.insert(loc);
                self.instance_init.insert(loc);
            }

            GeneralOsdiInput::Temperature => {
                self.model_init.insert(loc);
                self.instance_init.insert(loc);
            }

            GeneralOsdiInput::PortConnected(_) => {
                self.model_temp_update.insert(loc);
                self.model_init.insert(loc);
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
        if matches!(rval, RValue::Call(GeneralOsdiCall::TimeDerivative, _, _)) {
            self.by_time_derivative.insert(loc)
        }

        // if matches!(rval, RValue::Call(GeneralOsdiCall::Noise,__)){
        //     self.by_noise.insert(loc)
        // } TODO NOISE

        for operand in rval.operands() {
            self.visit_operand(operand, loc, cfg)
        }
    }
}

impl TaintedLocations {
    pub fn new(cfg: &mut ControlFlowGraph<GeneralOsdiCall>, locations: &InternedLocations) -> Self {
        let mut res = TaintedLocations {
            model_temp_update: BitSet::new_empty(locations.len_idx()),
            model_init: BitSet::new_empty(locations.len_idx()),
            instance_init: BitSet::new_empty(locations.len_idx()),
            instance_temp_update: BitSet::new_empty(locations.len_idx()),
            by_time_derivative: BitSet::new_empty(locations.len_idx()),
        };

        cfg.run_pass(Visit {
            visitor: &mut res,
            locations: &locations,
        });

        res
    }

    pub fn taint_instance_variable_access_for_model_function(
        &mut self,
        instance_vars: &BitSet<VariableId>,
        locations: &InternedLocations,
        cfg: &mut ControlFlowGraph<GeneralOsdiCall>,
    ) {
        cfg.run_pass(Visit {
            visitor: &mut TaintInstanceVariableLocations {
                tainted_locations: self,
                instance_vars,
            },
            locations,
        })
    }
}

struct TaintInstanceVariableLocations<'a> {
    tainted_locations: &'a mut TaintedLocations,
    instance_vars: &'a BitSet<VariableId>,
}

impl<'a> CfgVisitor<GeneralOsdiCall> for TaintInstanceVariableLocations<'a> {
    fn visit_operand(
        &mut self,
        operand: &COperand<GeneralOsdiCall>,
        loc: IntLocation,
        cfg: &ControlFlowGraph<GeneralOsdiCall>,
    ) {
        if let OperandData::Copy(local) = operand.contents {
            self.visit_local(local, loc, cfg)
        }
    }

    fn visit_stmnt(
        &mut self,
        stmnt: &StmntKind<GeneralOsdiCall>,
        loc: IntLocation,
        cfg: &ControlFlowGraph<GeneralOsdiCall>,
    ) {
        if let StmntKind::Assignment(local, _) = stmnt {
            self.visit_local(*local, loc, cfg)
        }
    }

    fn visit_phi(
        &mut self,
        phi: &PhiData,
        loc: IntLocation,
        cfg: &ControlFlowGraph<GeneralOsdiCall>,
    ) {
        self.visit_local(phi.dst, loc, cfg)
    }
}

impl<'a> TaintInstanceVariableLocations<'a> {
    pub fn visit_local(
        &mut self,
        local: Local,
        loc: IntLocation,
        cfg: &ControlFlowGraph<GeneralOsdiCall>,
    ) {
        if matches!(cfg.locals[local].kind, LocalKind::Variable(var,_) if self.instance_vars.contains(var))
        {
            self.tainted_locations.model_temp_update.insert(loc);
            self.tainted_locations.model_init.insert(loc);
        }
    }
}

/// CFG visit that finds all Instructons that write to a variable.
/// The instructions regarded as the output of functions are a subset of these statements.
pub struct OutputLocations(BitSet<IntLocation>);

impl<C: CallType> CfgVisitor<C> for OutputLocations {
    fn visit_stmnt(&mut self, stmnt: &StmntKind<C>, loc: IntLocation, cfg: &ControlFlowGraph<C>) {
        match *stmnt {
            // TODO treat derivatives as temproaries?
            StmntKind::Assignment(dst, _) if cfg.locals[dst].kind != LocalKind::Temporary => {
                self.0.insert(loc)
            }
            StmntKind::Call(_, _, _) => self.0.insert(loc),
            _ => {}
        }
    }
}

impl OutputLocations {
    pub fn find_in_cfg(
        cfg: &mut ControlFlowGraph<GeneralOsdiCall>,
        locations: &InternedLocations,
    ) -> BitSet<IntLocation> {
        let mut find_output_stmnnts = OutputLocations(BitSet::new_empty(locations.len_idx()));

        cfg.run_pass(Visit {
            visitor: &mut find_output_stmnnts,
            locations: &locations,
        });

        find_output_stmnnts.0
    }
}

/// CFG visit that finds all variables that are written to in a CFG
pub struct WrittenVars(BitSet<VariableId>);

impl<C: CallType> CfgVisitor<C> for WrittenVars {
    fn visit_stmnt(&mut self, stmnt: &StmntKind<C>, _loc: IntLocation, cfg: &ControlFlowGraph<C>) {
        if let StmntKind::Assignment(dst, _) = *stmnt {
            if let LocalKind::Variable(var, _) = cfg.locals[dst].kind {
                self.0.insert(var)
            }
        }
    }
}

impl WrittenVars {
    pub fn find_in_cfg(
        var_cnt: VariableId,
        cfg: &mut ControlFlowGraph<GeneralOsdiCall>,
        locations: &InternedLocations,
    ) -> BitSet<VariableId> {
        let mut res = Self(BitSet::new_empty(var_cnt));

        cfg.run_pass(Visit {
            visitor: &mut res,
            locations,
        });

        res.0
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
/// * `written_vars` All variables that are written to inside of this function are added to this set
///
/// # Returns
///
/// * ControlFlowGraph of the function
/// * All instructions which are part of this function and write to a variable. These are to be considered the "output" of this function
pub(super) fn function_cfg_from_full_cfg(
    mir: &Mir<GeneralOsdiCall>,
    full_cfg: &ControlFlowGraph<GeneralOsdiCall>,
    tainted_locations: &BitSet<IntLocation>,
    assumed_locations: Option<&BitSet<IntLocation>>,
    all_output_locations: &BitSet<IntLocation>,
    locations: &InternedLocations,
    pdg: &InvProgramDependenceGraph,
) -> (
    ControlFlowGraph<GeneralOsdiCall>,
    BitSet<IntLocation>,
    BitSet<VariableId>,
) {
    let mut cfg = full_cfg.clone();
    let mut init_locations = cfg.run_pass(ForwardSlice {
        tainted_locations: tainted_locations.clone(),
        pdg,
        locations,
    });

    if let Some(assumed_locations) = assumed_locations {
        init_locations.difference_with(assumed_locations);
    }

    cfg.run_pass(Strip {
        retain: &init_locations,
        locations,
    });

    let init_output_locations = {
        init_locations.intersect_with(all_output_locations);
        init_locations
    };

    optimize_cfg(&mut cfg);

    let written_vars = WrittenVars::find_in_cfg(mir.variables.len_idx(), &mut cfg, locations);

    (cfg, init_output_locations, written_vars)
}

impl OsdiFunctions {
    pub(super) fn init_functions_from_tainted_locs(
        mir: &Mir<GeneralOsdiCall>,
        cfg: &mut ControlFlowGraph<GeneralOsdiCall>,
        locations: &InternedLocations,
        inverse_pdg: &InvProgramDependenceGraph,
        tainted_locations: &TaintedLocations,
        output_locations: &BitSet<IntLocation>,
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
            &locations,
            inverse_pdg,
            &output_locations,
        );

        let mut assumed_locations = model_init_output;

        let (model_temp_update, model_temp_update_output) = ModelTempUpdateFunction::new(
            mir,
            &cfg,
            &tainted_locations.model_temp_update,
            &assumed_locations,
            &locations,
            inverse_pdg,
            &output_locations,
        );

        assumed_locations.union_with(&model_temp_update_output);

        let (instance_init, instance_init_output) = InstanceInitFunction::new(
            mir,
            &cfg,
            &tainted_locations.instance_init,
            &assumed_locations,
            &locations,
            inverse_pdg,
            &output_locations,
        );

        assumed_locations.union_with(&instance_init_output);

        let (instance_temp_update, instance_temp_update_output) = InstanceTempUpdateFunction::new(
            mir,
            &cfg,
            &tainted_locations.instance_temp_update,
            &assumed_locations,
            &locations,
            inverse_pdg,
            &output_locations,
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
