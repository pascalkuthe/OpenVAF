/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::storage_locations::StorageLocationValue::{
    Branch, BranchDerivatives, Variable, VariableDerivative,
};
use crate::GeneralOsdiCall;
use data_structures::arrayvec::ArrayVec;
use data_structures::index_vec::{define_index_type, IndexVec};
use data_structures::{bit_set::BitSet, HashMap};
use diagnostics::ListPrettyPrinter;
use hir::{Unknown, VariableId};
use ir::ids::BranchId;
use middle::cfg::ControlFlowGraph;
use middle::{DisciplineAccess, Local, LocalKind, VariableLocalKind};
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Index;

define_index_type! {
    pub struct StorageLocation = u32;
    DISPLAY_FORMAT = "store_loc_{}";
    DISABLE_MAX_INDEX_CHECK = cfg!(not(debug_assertions));
}

#[derive(PartialEq, Eq, Clone)]
pub enum StorageLocationValue {
    BranchDerivatives(BranchId, ArrayVec<Unknown, 7>),
    Branch(BranchId),
    VariableDerivative(VariableId, ArrayVec<Unknown, 7>),
    Variable(VariableId),
}

impl Debug for StorageLocationValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            BranchDerivatives(branch, unkowns) => {
                let mut unkowns = ListPrettyPrinter {
                    list: unkowns.as_slice(),
                    prefix: "d/d",
                    postfix: " ",
                };

                write!(f, "{} {:?}", unkowns, branch)
            }
            Branch(branch) => write!(f, "{:?}", branch),

            VariableDerivative(var, unkowns) => {
                let mut unkowns = ListPrettyPrinter {
                    list: unkowns.as_slice(),
                    prefix: "d/d",
                    postfix: " ",
                };

                write!(f, "{} {:?}", unkowns, var)
            }
            Variable(var) => write!(f, "{:?}", var),
        }
    }
}

impl Display for StorageLocationValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self, f)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum StorageContainer {
    Model,
    Instance,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct StorageLocationInfo {
    pub offset: usize,
    pub val: StorageLocationValue,
    /// Local in the cfg of the entire analog block
    /// Used internally during the partitioning process
    pub(crate) local: Local,
    pub container: StorageContainer,
}

impl StorageLocationInfo {
    pub fn new(val: StorageLocationValue, local: Local) -> Self {
        Self {
            offset: 0,
            val,
            container: StorageContainer::Instance,
            local,
        }
    }
}

pub struct StorageLocations {
    locations: IndexVec<StorageLocation, StorageLocationInfo>,
    var_info_to_location: HashMap<(VariableId, VariableLocalKind), StorageLocation>,
    branch_info_to_location: HashMap<(BranchId, VariableLocalKind), StorageLocation>,
}

impl StorageLocations {
    pub fn new(cfg: &ControlFlowGraph<GeneralOsdiCall>) -> Self {
        let mut locations = IndexVec::with_capacity(32);

        // TODO benchmark: Is a vector of hashmaps faster?
        let mut var_info_to_location = HashMap::with_capacity(32);
        let mut branch_info_to_location = HashMap::with_capacity(32);

        for (local, local_decl) in cfg.locals.iter_enumerated() {
            match local_decl.kind {
                LocalKind::Variable(var, VariableLocalKind::User) => {
                    let storage = locations.push(StorageLocationInfo::new(Variable(var), local));
                    var_info_to_location.insert((var, VariableLocalKind::User), storage);
                }
                LocalKind::Variable(var, VariableLocalKind::Derivative(ref unkowns)) => {
                    let storage = locations.push(StorageLocationInfo::new(
                        VariableDerivative(var, unkowns.clone()),
                        local,
                    ));
                    var_info_to_location.insert(
                        (var, VariableLocalKind::Derivative(unkowns.clone())),
                        storage,
                    );
                }

                LocalKind::Branch(
                    DisciplineAccess::Flow,
                    branch,
                    VariableLocalKind::Derivative(ref unkowns),
                ) => {
                    let storage = locations.push(StorageLocationInfo::new(
                        BranchDerivatives(branch, unkowns.clone()),
                        local,
                    ));
                    branch_info_to_location.insert(
                        (branch, VariableLocalKind::Derivative(unkowns.clone())),
                        storage,
                    );
                }

                LocalKind::Branch(DisciplineAccess::Flow, branch, VariableLocalKind::User) => {
                    let storage = locations.push(StorageLocationInfo::new(Branch(branch), local));
                    branch_info_to_location.insert((branch, VariableLocalKind::User), storage);
                }

                _ => (),
            }
        }

        Self {
            locations,
            var_info_to_location,
            branch_info_to_location,
        }
    }

    pub fn init_locations_positions(
        &mut self,
        model_storage: &BitSet<StorageLocation>,
        instance_storage: &BitSet<StorageLocation>,
    ) {
        debug_assert!(model_storage.is_disjoint(&instance_storage));

        for (offset, storage_loc) in model_storage.ones().enumerate() {
            self.locations[storage_loc].container = StorageContainer::Model;
            self.locations[storage_loc].offset = offset;
        }

        for (offset, storage_loc) in instance_storage.ones().enumerate() {
            self.locations[storage_loc].container = StorageContainer::Instance;
            self.locations[storage_loc].offset = offset;
        }
    }

    pub fn len(&self) -> usize {
        self.locations.len()
    }

    pub fn len_idx(&self) -> StorageLocation {
        self.locations.len_idx()
    }

    pub fn find_variable_location(
        &self,
        var: VariableId,
        kind: VariableLocalKind,
    ) -> StorageLocation {
        self.var_info_to_location[&(var, kind)]
    }

    pub fn find_branch_location(
        &self,
        branch: BranchId,
        kind: VariableLocalKind,
    ) -> StorageLocation {
        self.branch_info_to_location[&(branch, kind)]
    }
}

impl Index<StorageLocation> for StorageLocations {
    type Output = StorageLocationInfo;

    fn index(&self, index: StorageLocation) -> &Self::Output {
        &self.locations[index]
    }
}
