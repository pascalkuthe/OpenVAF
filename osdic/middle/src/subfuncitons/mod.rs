use crate::frontend::{GeneralOsdiCall, GeneralOsdiInput};
use crate::subfuncitons::init::InitFunction;
use openvaf_data_structures::BitSet;
use openvaf_middle::cfg::{ControlFlowGraph, LocationId};
use openvaf_middle::const_fold::ConstantPropagation;
use openvaf_middle::{CallType, LocalKind, Mir, StmntKind};
use openvaf_transformations::{
    BuildPDG, CalculateDataDependence, CfgVisitor, ForwardSlice, RemoveDeadLocals, Simplify,
    SimplifyBranches, Strip, Visit,
};

//mod ac_load;
pub mod init;
//mod load;
mod temp_update;
mod node_collapse;

pub struct FindTaintedStmnts {
    temp_update_or_later: BitSet<LocationId>,
    load_or_later: BitSet<LocationId>,
}

impl CfgVisitor<GeneralOsdiCall> for FindTaintedStmnts {
    fn visit_input(
        &mut self,
        input: &GeneralOsdiInput,
        loc: LocationId,
        _cfg: &ControlFlowGraph<GeneralOsdiCall>,
    ) {
        match input {
            GeneralOsdiInput::SimParam(_, _)
            | GeneralOsdiInput::Voltage(_, _)
            | GeneralOsdiInput::Current(_)
            | GeneralOsdiInput::PortFlow(_)
            | GeneralOsdiInput::PortConnected(_)
            | GeneralOsdiInput::Lim { .. } => {
                self.temp_update_or_later.insert(loc);
                self.load_or_later.insert(loc);
            }

            GeneralOsdiInput::Temperature => self.temp_update_or_later.insert(loc),

            GeneralOsdiInput::Parameter(_) | GeneralOsdiInput::ParamGiven(_) => {}
        }
    }
}

pub struct FindOutputStmnts<'a> {
    relevant: BitSet<LocationId>,
    mir: &'a Mir<GeneralOsdiCall>,
}

impl<'a, C: CallType> CfgVisitor<C> for FindOutputStmnts<'a> {
    fn visit_stmnt(&mut self, stmnt: &StmntKind<C>, loc: LocationId, cfg: &ControlFlowGraph<C>) {
        match *stmnt {
            // TODO teat derivatives as temproaries?
            StmntKind::Assignment(dst, _) if cfg.locals[dst].kind != LocalKind::Temporary => {
                self.relevant.insert(loc)
            }
            StmntKind::Call(_, _, _) => self.relevant.insert(loc),
            _ => {}
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
        temp_update_or_later: BitSet::new_empty(locations.len_idx()),
        load_or_later: BitSet::new_empty(locations.len_idx()),
    };
    cfg.run_pass(Visit {
        visitor: &mut visitor,
        locations: &locations,
    });

    let mut find_output_stmnnts = FindOutputStmnts {
        relevant: BitSet::new_empty(locations.len_idx()),
        mir,
    };
    cfg.run_pass(Visit {
        visitor: &mut find_output_stmnnts,
        locations: &locations,
    });

    let pdg = cfg.run_pass(BuildPDG {
        locations: &locations,
        data_dependence: CalculateDataDependence,
    });
    let inverse_pdg = pdg.inverse();

    // create subslices

    let mut init_cfg = cfg.clone();

    let mut init_locations = init_cfg.run_pass(ForwardSlice {
        tainted_locations: visitor.temp_update_or_later,
        pdg: &inverse_pdg,
        locations: &locations,
    });
    init_cfg.run_pass(Strip {
        retain: &init_locations,
        locations: &locations,
    });
    let init_output_locations = {
        init_locations.intersect_with(&find_output_stmnnts.relevant);
        init_locations
    };

    let mut temp_update_cfg = cfg.clone();
    let mut temp_update_locations = temp_update_cfg.run_pass(ForwardSlice {
        tainted_locations: visitor.load_or_later,
        pdg: &inverse_pdg,
        locations: &locations,
    });
    temp_update_locations.difference_with(&init_output_locations);
    temp_update_cfg.run_pass(Strip {
        retain: &temp_update_locations,
        locations: &locations,
    });

    let mut load_assumed_locations = {
        temp_update_locations.intersect_with(&find_output_stmnnts.relevant);
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

    init_cfg.run_pass(Simplify);
    init_cfg.run_pass(SimplifyBranches);
    init_cfg.run_pass(ConstantPropagation::default());

    init_cfg.run_pass(Simplify);
    init_cfg.run_pass(SimplifyBranches);
    init_cfg.run_pass(RemoveDeadLocals);

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

pub fn divide_analog_block(mir: &Mir<GeneralOsdiCall>) -> InitFunction {
    let (init_cfg, _, _) = divide_analog_block_into_cfgs(mir);
    let mut fun = InitFunction::new(mir);
    fun.insert_model_init(init_cfg);
    fun
}