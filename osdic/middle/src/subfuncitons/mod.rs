use crate::frontend::{GeneralOsdiCall, GeneralOsdiInput};
use crate::subfuncitons::instance_init::InstanceInit;
use crate::subfuncitons::instance_temp_update::InstanceTempUpdate;
use crate::subfuncitons::model_init::InitModelFunction;
use crate::subfuncitons::model_temp_update::ModelTempUpdate;
use openvaf_data_structures::BitSet;
use openvaf_hir::VariableId;
use openvaf_middle::cfg::{ControlFlowGraph, IntLocation, InternedLocations};
use openvaf_middle::const_fold::ConstantPropagation;
use openvaf_middle::{CallType, LocalKind, Mir, StmntKind};
use openvaf_transformations::{
    BuildPDG, CalculateDataDependence, CfgVisitor, ForwardSlice, InvProgramDependenceGraph,
    RemoveDeadLocals, Simplify, SimplifyBranches, Strip, Visit,
};

//mod ac_load;
pub mod model_init;
//mod load;
mod instance_init;
mod instance_temp_update;
mod model_temp_update;

pub struct FindTaintedStmnts {
    model_temp_update_tainted: BitSet<IntLocation>,
    model_init_tainted: BitSet<IntLocation>,
    instance_init_tainted: BitSet<IntLocation>,
    instance_temp_update_tainted: BitSet<IntLocation>,
}

impl CfgVisitor<GeneralOsdiCall> for FindTaintedStmnts {
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
                self.model_init_tainted.insert(loc);
                self.model_temp_update_tainted.insert(loc);
                self.instance_temp_update_tainted.insert(loc);
                self.instance_init_tainted.insert(loc);
            }

            GeneralOsdiInput::Temperature => {
                self.model_init_tainted.insert(loc);
                self.instance_init_tainted.insert(loc);
            }

            GeneralOsdiInput::PortConnected(_) => {
                self.model_temp_update_tainted.insert(loc);
                self.model_init_tainted.insert(loc);
            }

            GeneralOsdiInput::Parameter(_) => {}
        }
    }
}

pub struct FindOutputStmnts(BitSet<IntLocation>);

impl<C: CallType> CfgVisitor<C> for FindOutputStmnts {
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

pub struct FindWrittenVars<'a>(&'a mut BitSet<VariableId>);

impl<'a, C: CallType> CfgVisitor<C> for FindWrittenVars<'a> {
    fn visit_stmnt(&mut self, stmnt: &StmntKind<C>, _loc: IntLocation, cfg: &ControlFlowGraph<C>) {
        if let StmntKind::Assignment(dst, _) = *stmnt {
            if let LocalKind::Variable(var, _) = cfg.locals[dst].kind {
                self.0.insert(var)
            }
        }
    }
}

pub struct DividedAnalogBlock {
    model_init: InitModelFunction,
    model_temp_update: ModelTempUpdate,
}

pub fn divide_analog_block(
    mir: &Mir<GeneralOsdiCall>,
) -> (
    InitModelFunction,
    ControlFlowGraph<GeneralOsdiCall>,
    ControlFlowGraph<GeneralOsdiCall>,
) {
    let mut cfg = mir.modules[0].analog_cfg.borrow_mut();

    // optimize a few times so we catch most things

    optimize_cfg(&mut cfg);
    cfg.run_pass(ConstantPropagation::default());
    optimize_cfg(&mut cfg);

    // analyse the cfg

    let locations = cfg.intern_locations();

    let mut tainted_locations = FindTaintedStmnts {
        model_temp_update_tainted: BitSet::new_empty(locations.len_idx()),
        model_init_tainted: BitSet::new_empty(locations.len_idx()),
        instance_init_tainted: BitSet::new_empty(locations.len_idx()),
        instance_temp_update_tainted: BitSet::new_empty(locations.len_idx()),
    };

    cfg.run_pass(Visit {
        visitor: &mut tainted_locations,
        locations: &locations,
    });

    let mut find_output_stmnnts = FindOutputStmnts(BitSet::new_empty(locations.len_idx()));
    cfg.run_pass(Visit {
        visitor: &mut find_output_stmnnts,
        locations: &locations,
    });
    let output_stmnts = find_output_stmnnts.0;

    let pdg = cfg.run_pass(BuildPDG {
        locations: &locations,
        data_dependence: CalculateDataDependence,
    });
    let inverse_pdg = pdg.inverse();

    // create subslices

    let (mut model_init, model_init_output) = InitModelFunction::new(
        mir,
        &cfg,
        tainted_locations.model_init_tainted,
        &locations,
        &inverse_pdg,
        &output_stmnts,
    );

    let (model_temp_update, model_temp_update_output) = ModelTempUpdate::new(
        &cfg,
        tainted_locations.model_temp_update_tainted,
        &model_init_output,
        &locations,
        &inverse_pdg,
        &output_stmnts,
        &mut model_init.model_vars,
    );

    let (mut instance_init, instance_init_output) = InstanceInit::new(
        mir,
        &cfg,
        tainted_locations.instance_init_tainted,
        &model_temp_update_output,
        &locations,
        &inverse_pdg,
        &output_stmnts,
    );

    let (mut instance_init, instance_init_output) = InstanceTempUpdate::new(
        &cfg,
        tainted_locations.instance_temp_update_tainted,
        &model_temp_update_output,
        &locations,
        &inverse_pdg,
        &output_stmnts,
        &mut instance_init.instance_vars,
    );
}

fn optimize_cfg<C: CallType + 'static>(cfg: &mut ControlFlowGraph<C>) {
    cfg.run_pass(Simplify);
    cfg.run_pass(SimplifyBranches);
    cfg.run_pass(RemoveDeadLocals);
    cfg.run_pass(ConstantPropagation::default());

    cfg.run_pass(Simplify);
    cfg.run_pass(SimplifyBranches);
    cfg.run_pass(RemoveDeadLocals);
}

fn create_subfunction_cfg(
    src: &ControlFlowGraph<GeneralOsdiCall>,
    tainted_locations: BitSet<IntLocation>,
    assumed_locations: &BitSet<IntLocation>,

    locations: &InternedLocations,
    pdg: &InvProgramDependenceGraph,
    output_stmnts: &BitSet<IntLocation>,

    written_vars: &mut BitSet<VariableId>,
) -> (ControlFlowGraph<GeneralOsdiCall>, BitSet<IntLocation>) {
    let mut cfg = src.clone();
    let mut init_locations = cfg.run_pass(ForwardSlice {
        tainted_locations,
        pdg,
        locations,
    });

    init_locations.difference_with(assumed_locations);

    cfg.run_pass(Strip {
        retain: &init_locations,
        locations,
    });

    let init_output_locations = {
        init_locations.intersect_with(output_stmnts);
        init_locations
    };

    optimize_cfg(&mut cfg);

    cfg.run_pass(Visit {
        visitor: &mut FindWrittenVars(written_vars),
        locations,
    });

    (cfg, init_output_locations)
}
