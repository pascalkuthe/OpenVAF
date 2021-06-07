use openvaf_data_structures::BitSet;
use openvaf_hir::VariableId;
use openvaf_middle::cfg::{ControlFlowGraph, IntLocation, InternedLocations};
use openvaf_middle::const_fold::ConstantPropagation;
use openvaf_middle::{CallType, Mir};
use openvaf_transformations::{
    BuildPDG, CalculateDataDependence, InvProgramDependenceGraph, RemoveDeadLocals, Simplify,
    SimplifyBranches,
};

use crate::frontend::GeneralOsdiCall;
use crate::subfuncitons::automatic_slicing::{OutputLocations, TaintedLocations};
use crate::subfuncitons::instance_init::InstanceInitFunction;
use crate::subfuncitons::instance_temp_update::InstanceTempUpdateFunction;
use crate::subfuncitons::load_functions::LoadFunctions;
use crate::subfuncitons::model_init::ModelInitFunction;
use crate::subfuncitons::model_temp_update::ModelTempUpdateFunction;
use itertools::multizip;

//mod ac_load;
mod automatic_slicing;
mod instance_init;
mod instance_temp_update;
mod load_functions;
pub mod model_init;
mod model_temp_update;

pub struct OsdiFunctions {
    pub model_init: ModelInitFunction,
    pub model_temp_update: ModelTempUpdateFunction,

    pub instance_init: InstanceInitFunction,
    pub instance_temp_update: InstanceTempUpdateFunction,

    pub model_vars: BitSet<VariableId>,
    pub instance_vars: BitSet<VariableId>,
}

impl OsdiFunctions {
    pub fn create_from_analog_block_by_automatic_division(mir: &Mir<GeneralOsdiCall>) -> Self {
        let mut cfg = mir.modules[0].analog_cfg.borrow_mut();

        // optimize a few times so we catch most things

        optimize_cfg(&mut cfg);
        cfg.run_pass(ConstantPropagation::default());
        optimize_cfg(&mut cfg);

        // analyse the cfg

        let locations = cfg.intern_locations();

        let mut tainted_locations = TaintedLocations::new(&mut cfg, &locations);
        let output_locations = OutputLocations::find_in_cfg(&mut cfg, &locations);

        let pdg = cfg.run_pass(BuildPDG {
            locations: &locations,
            data_dependence: CalculateDataDependence,
        });
        let inverse_pdg = pdg.inverse();

        // create subslices

        let (
            mut model_init,
            mut model_temp_update,
            mut instance_init,
            mut instance_temp_update,
            init_output_locations,
        ) = Self::init_functions_from_tainted_locs(
            mir,
            &mut cfg,
            &locations,
            &inverse_pdg,
            &tainted_locations,
            &output_locations,
        );

        let load_functions = LoadFunctions::new(
            mir,
            &mut cfg,
            &tainted_locations,
            &init_output_locations,
            &locations,
            &output_locations,
        );

        let mut model_vars = model_init.written_vars.clone();
        model_vars.union_with(&model_temp_update.written_vars);

        let mut instance_vars = instance_init.written_vars.clone();
        instance_vars.union_with(&instance_temp_update.written_vars);
        instance_vars.union_with(&load_functions.written_vars);

        // TODO temporary vars

        let mut res = Self {
            model_init,
            model_temp_update,
            instance_init,
            instance_temp_update,
            model_vars,
            instance_vars,
        };

        res.remove_instance_vars_from_model_functions(
            &locations,
            &inverse_pdg,
            &mut tainted_locations,
            &output_locations,
            &mut cfg,
            mir,
        );

        // TODO dont reserve space for temporaries
        // let mut temp_model_vars = model_init.written_vars.clone();
        // for (dst, model_temp_update_written,read_in_model_temp_update, read_in_load, read_in_instance_init, read_in_instance_temp_update) in multizip((temp_model_vars.as_mut_slice(),model_init.written_vars.as_slice())) {
        //     *dst = ((*dst & !*read_in_model_temp_update) ^ *model_temp_update_written) & !(*read_in_load | *read_in_instance_init | *read_in_instance_temp_update);
        // }

        res
    }

    /// Some functions may be initalized on a per model basis but may later be modified per instance
    /// These variable are then present in both sets of variables
    /// They are then removed from the model_vars and the associated functions
    fn remove_instance_vars_from_model_functions(
        &mut self,
        locations: &InternedLocations,
        inverse_pdg: &InvProgramDependenceGraph,
        tainted_locations: &mut TaintedLocations,
        output_locations: &BitSet<IntLocation>,
        cfg: &mut ControlFlowGraph<GeneralOsdiCall>,
        mir: &Mir<GeneralOsdiCall>,
    ) {
        if self.model_vars.has_intersection_with(&self.instance_vars) {
            self.model_vars.difference_with(&self.instance_vars);
            tainted_locations.taint_instance_variable_access_for_model_function(
                &self.instance_vars,
                &locations,
                cfg,
            );
            let (
                model_init,
                model_temp_update,
                instance_init,
                instance_temp_update,
                _output_locations,
            ) = Self::init_functions_from_tainted_locs(
                mir,
                cfg,
                &locations,
                &inverse_pdg,
                &tainted_locations,
                &output_locations,
            );
            self.model_init = model_init;
            self.model_temp_update = model_temp_update;
            self.instance_init = instance_init;
            self.instance_temp_update = instance_temp_update;

            debug_assert_eq!(output_locations, &_output_locations)
        }
    }
}

/// Optimization pass applied to all functions
fn optimize_cfg<C: CallType + 'static>(cfg: &mut ControlFlowGraph<C>) {
    cfg.run_pass(Simplify);
    cfg.run_pass(SimplifyBranches);
    cfg.run_pass(RemoveDeadLocals);
    cfg.run_pass(ConstantPropagation::default());

    cfg.run_pass(Simplify);
    cfg.run_pass(SimplifyBranches);
    cfg.run_pass(RemoveDeadLocals);
}
