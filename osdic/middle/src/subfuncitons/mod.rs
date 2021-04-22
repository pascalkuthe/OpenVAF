use crate::frontend::{GeneralOsdiCall, GeneralOsdiInput};
use crate::subfuncitons::model_init::InitModelFunction;
use openvaf_data_structures::BitSet;
use openvaf_hir::VariableId;
use openvaf_middle::cfg::{ControlFlowGraph, IntLocation};
use openvaf_middle::const_fold::ConstantPropagation;
use openvaf_middle::{CallType, Local, LocalKind, Mir, StmntKind};
use openvaf_transformations::{
    BuildPDG, CalculateDataDependence, CfgVisitor, ForwardSlice, RemoveDeadLocals, Simplify,
    SimplifyBranches, Strip, Visit,
};

//mod ac_load;
pub mod model_init;
//mod load;
mod instance_init;
mod temp_update;

pub struct FindTaintedStmnts {
    instance_init_or_later: BitSet<IntLocation>,
    temp_update_or_later: BitSet<IntLocation>,
    load_or_later: BitSet<IntLocation>,
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
                self.instance_init_or_later.insert(loc);
                self.temp_update_or_later.insert(loc);
                self.load_or_later.insert(loc);
            }

            GeneralOsdiInput::Temperature => {
                self.instance_init_or_later.insert(loc);
                self.temp_update_or_later.insert(loc)
            }

            GeneralOsdiInput::PortConnected(_) => self.instance_init_or_later.insert(loc),

            GeneralOsdiInput::Parameter(_) | GeneralOsdiInput::ParamGiven(_) => {}
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

pub struct FindWrittenVars(BitSet<VariableId>);

impl<C: CallType> CfgVisitor<C> for FindWrittenVars {
    fn visit_stmnt(&mut self, stmnt: &StmntKind<C>, _loc: IntLocation, cfg: &ControlFlowGraph<C>) {
        if let StmntKind::Assignment(dst, _) = *stmnt {
            if let LocalKind::Variable(var, _) = cfg.locals[dst].kind {
                self.0.insert(var)
            }
        }
    }
}

pub fn divide_analog_block_into_cfgs(
    mir: &Mir<GeneralOsdiCall>,
) -> (
    ControlFlowGraph<GeneralOsdiCall>,
    ControlFlowGraph<GeneralOsdiCall>,
    ControlFlowGraph<GeneralOsdiCall>,
) {
    let mut cfg = mir.modules[0].analog_cfg.borrow_mut();

    // optimize a few times so we catch most things

    cfg.run_pass(Simplify);
    cfg.run_pass(SimplifyBranches);
    cfg.run_pass(ConstantPropagation::default());

    cfg.run_pass(Simplify);
    cfg.run_pass(SimplifyBranches);
    cfg.run_pass(RemoveDeadLocals);

    cfg.run_pass(ConstantPropagation::default());

    cfg.run_pass(Simplify);
    cfg.run_pass(SimplifyBranches);
    cfg.run_pass(RemoveDeadLocals);

    // analyse the cfg

    let locations = cfg.intern_locations();

    let mut visitor = FindTaintedStmnts {
        instance_init_or_later: BitSet::new_empty(locations.len_idx()),
        temp_update_or_later: BitSet::new_empty(locations.len_idx()),
        load_or_later: BitSet::new_empty(locations.len_idx()),
    };
    cfg.run_pass(Visit {
        visitor: &mut visitor,
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

    let (model_init, mut model_init_output) = InitModelFunction::new(
        mir,
        cfg.clone(),
        visitor.instance_init_or_later,
        &locations,
        &inverse_pdg,
        &output_stmnts,
    );

    let mut temp_update_cfg = cfg.clone();
    let mut temp_update_locations = temp_update_cfg.run_pass(ForwardSlice {
        tainted_locations: visitor.load_or_later,
        pdg: &inverse_pdg,
        locations: &locations,
    });
    temp_update_locations.difference_with(&model_init_output);
    temp_update_cfg.run_pass(Strip {
        retain: &temp_update_locations,
        locations: &locations,
    });

    let mut load_assumed_locations = {
        temp_update_locations.intersect_with(&output_stmnts);
        temp_update_locations.union_with(&init_output_locations);
        temp_update_locations
    };

    let load_relevant_locations = {
        load_assumed_locations.toggle_all();
        load_assumed_locations
    };

    let mut load_cfg = cfg.clone();
    load_cfg.run_pass(Strip {
        retain: &load_relevant_locations,
        locations: &locations,
    });
    // todo remove noise

    temp_update_cfg.run_pass(Simplify);
    temp_update_cfg.run_pass(SimplifyBranches);
    temp_update_cfg.run_pass(ConstantPropagation::default());

    temp_update_cfg.run_pass(Simplify);
    temp_update_cfg.run_pass(SimplifyBranches);
    temp_update_cfg.run_pass(RemoveDeadLocals);

    load_cfg.run_pass(Simplify);
    load_cfg.run_pass(SimplifyBranches);
    load_cfg.run_pass(ConstantPropagation::default());

    load_cfg.run_pass(Simplify);
    load_cfg.run_pass(SimplifyBranches);
    load_cfg.run_pass(RemoveDeadLocals);

    (init_cfg, temp_update_cfg, load_cfg)
}

pub fn divide_analog_block(mir: &Mir<GeneralOsdiCall>) -> InitModelFunction {
    let (init_cfg, _, _) = divide_analog_block_into_cfgs(mir);
    let mut fun = InitModelFunction::new(mir);
    fun.insert_model_init(init_cfg);
    fun
}
