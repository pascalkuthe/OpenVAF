/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use std::fs::File;

use itertools::Itertools;

use data_structures::{bit_set::BitSet, index_vec::define_index_type, HashMap};
use hir::{BranchId, VariableId};
use middle::cfg::{ControlFlowGraph, IntLocation, InternedLocations, Location};
use middle::{LocalKind, Mir, VariableLocalKind};
use pass::program_dependence::{
    BuildPDG, InvProgramDependenceGraph, ProgramDependenceGraph,
};
use session::symbols::SymbolStr;

use crate::frontend::GeneralOsdiCall;
use crate::storage_locations::{StorageLocation, StorageLocations};
use crate::subfuncitons::automatic_slicing::{OutputLocations, TaintedLocations};
use crate::subfuncitons::instance_init::InstanceInitFunction;
use crate::subfuncitons::instance_temp_update::InstanceTempUpdateFunction;
use crate::subfuncitons::load_functions::LoadFunctions;
use crate::subfuncitons::model_init::ModelInitFunction;
use crate::subfuncitons::model_temp_update::ModelTempUpdateFunction;
use crate::topology::CircuitTopology;

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

    pub load: LoadFunctions,

    pub model_storage: BitSet<StorageLocation>,
    pub instance_storage: BitSet<StorageLocation>,
}

impl OsdiFunctions {
    pub fn create_from_analog_block_by_automatic_division(
        mir: &Mir<GeneralOsdiCall>,
        cfg: &mut ControlFlowGraph<GeneralOsdiCall>,
        topology: &CircuitTopology,
        storage: &StorageLocations,
    ) -> Self {
        // analyse the cfg

        let locations = cfg.intern_locations();

        let mut tainted_locations = TaintedLocations::new(cfg, &locations, topology);
        let output_locations = OutputLocations::find_in_cfg(cfg, &locations, &topology);

        let pdg = cfg.modify(BuildPDG {
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
            cfg,
            &locations,
            &pdg,
            &inverse_pdg,
            &tainted_locations,
            &output_locations,
            storage,
        );

        let load_functions = LoadFunctions::new(
            cfg,
            &tainted_locations,
            &init_output_locations,
            &locations,
            &output_locations,
            &pdg,
            &inverse_pdg,
            topology,
            storage,
        );

        let mut model_vars = model_init.written_storage.clone();
        model_vars.union_with(&model_temp_update.written_storage);

        let mut instance_vars = instance_init.written_storage.clone();
        instance_vars.union_with(&instance_temp_update.written_storage);
        instance_vars.union_with(&load_functions.written_storage);

        let model_var_names = model_vars.ones().map(|loc| &storage[loc].val).collect_vec();

        let instance_var_names = instance_vars
            .ones()
            .map(|loc| &storage[loc].val)
            .collect_vec();

        println!("Model Vars: {:?}", model_var_names);
        println!("Instance Vars: {:?}", instance_var_names);

        // Only consider variables that are actually read somewhere to remove temporaries

        let mut read_vars = instance_init.read_storage.clone();
        read_vars.union_with(&instance_temp_update.read_storage);
        read_vars.union_with(&load_functions.read_storage);
        read_vars.union_with(&model_temp_update.read_storage);

        let read_vars_names = read_vars.ones().map(|loc| &storage[loc].val).collect_vec();

        println!("Read Vars: {:?}", read_vars_names);

        instance_vars.intersect_with(&read_vars);
        model_vars.intersect_with(&read_vars);

        let mut res = Self {
            model_init,
            model_temp_update,
            instance_init,
            instance_temp_update,
            load: load_functions,
            model_storage: model_vars,
            instance_storage: instance_vars,
        };

        res.remove_instance_vars_from_model_functions(
            &locations,
            &inverse_pdg,
            &pdg,
            &mut tainted_locations,
            &output_locations,
            init_output_locations,
            cfg,
            mir,
            &storage,
            &topology,
        );

        let circular_vars = res
            .model_temp_update
            .read_storage
            .intersection(&res.instance_storage)
            .collect_vec();

        debug_assert_eq!(circular_vars, Vec::<StorageLocation>::new());

        let model_var_names = res
            .model_storage
            .ones()
            .map(|loc| &storage[loc].val)
            .collect_vec();

        let instance_var__names = res
            .instance_storage
            .ones()
            .map(|loc| &storage[loc].val)
            .collect_vec();

        println!("Model Vars: {:?}", model_var_names);
        println!("Instance Vars: {:?}", instance_var__names);

        res
    }

    /// Some functions may be initalized on a per model basis but may later be modified per instance
    /// These variable are then present in both sets of variables
    /// They are then removed from the model_vars and the associated functions
    fn remove_instance_vars_from_model_functions(
        &mut self,
        locations: &InternedLocations,
        inverse_pdg: &InvProgramDependenceGraph,
        pdg: &ProgramDependenceGraph,
        tainted_locations: &mut TaintedLocations,
        all_output_locations: &BitSet<IntLocation>,
        init_output_locations: BitSet<IntLocation>,
        cfg: &mut ControlFlowGraph<GeneralOsdiCall>,
        mir: &Mir<GeneralOsdiCall>,
        storage: &StorageLocations,
        topology: &CircuitTopology,
    ) {
        let mut model_init_tainted = self.model_temp_update.written_storage.clone();
        model_init_tainted.intersect_with(&self.model_init.written_storage);
        model_init_tainted.intersect_with(&self.model_temp_update.read_storage);

        let mut instance_init_tainted = self.instance_temp_update.written_storage.clone();
        instance_init_tainted.intersect_with(&self.instance_init.written_storage);
        instance_init_tainted.intersect_with(&self.instance_temp_update.read_storage);

        let mut instance_temp_update_tainted = self.load.written_storage.clone();
        instance_temp_update_tainted.intersect_with(&self.instance_temp_update.written_storage);
        instance_temp_update_tainted.intersect_with(&self.load.read_storage);

        instance_init_tainted.union_with(&instance_temp_update_tainted);

        if self
            .model_storage
            .has_intersection_with(&self.instance_storage)
            || !model_init_tainted.is_empty()
            || !instance_init_tainted.is_empty()
            || instance_temp_update_tainted.is_empty()
        {
            self.model_storage.difference_with(&self.instance_storage);
            tainted_locations.taint_variable_access_for_init_functions(
                &self.instance_storage,
                &instance_temp_update_tainted,
                &instance_init_tainted,
                &model_init_tainted,
                storage,
                &pdg,
            );

            let (
                model_init,
                model_temp_update,
                instance_init,
                instance_temp_update,
                new_init_output_locations,
            ) = Self::init_functions_from_tainted_locs(
                mir,
                cfg,
                &locations,
                &pdg,
                &inverse_pdg,
                &tainted_locations,
                &all_output_locations,
                storage,
            );
            self.model_init = model_init;
            self.model_temp_update = model_temp_update;
            self.instance_init = instance_init;
            self.instance_temp_update = instance_temp_update;

            if new_init_output_locations != init_output_locations {
                self.load = LoadFunctions::new(
                    cfg,
                    &tainted_locations,
                    &init_output_locations,
                    &locations,
                    &all_output_locations,
                    &pdg,
                    &inverse_pdg,
                    topology,
                    storage,
                );
            }
        }
    }
}
