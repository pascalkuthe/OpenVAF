/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use openvaf_data_structures::bit_set::BitSet;
use openvaf_data_structures::index_vec::IndexVec;
use openvaf_middle::cfg::{ControlFlowGraph, ModificationPass};
use openvaf_middle::{impl_pass_span, CallType, Local};

pub struct RemoveDeadLocals;

impl<C: CallType> ModificationPass<'_, C> for RemoveDeadLocals {
    type Result = IndexVec<Local, Local>;

    fn run(self, cfg: &mut ControlFlowGraph<C>) -> IndexVec<Local, Local> {
        let mut alive_locals = BitSet::new_empty(cfg.locals.len());
        cfg.for_locals(|local| {
            alive_locals.insert(local);
            //debug!("{} is alive", local)
        });
        let replacements = delete_dead_local_declarations(cfg, alive_locals);
        cfg.for_locals_mut(|local| {
            // debug!(
            //     old = local.index(),
            //     new = replacements[*local].index(),
            //     "replacing"
            // );
            *local = replacements[*local];
        });
        replacements
    }

    impl_pass_span!("remove_dead_locals");
}

/// Removes all locals in `dead_locals` from `cfg.locals`
fn delete_dead_local_declarations<C: CallType>(
    cfg: &mut ControlFlowGraph<C>,
    alive_locals: BitSet<Local>,
) -> IndexVec<Local, Local> {
    let mut replacements: IndexVec<Local, Local> = cfg.locals.indices().collect();

    let mut new_local = Local::new(0);
    for old_local in alive_locals.iter() {
        replacements[old_local] = new_local;
        if old_local != new_local {
            // Swap the next alive block data with the current available slot. Since
            // alive_index is non-decreasing this is a valid operation.
            cfg.locals.swap(old_local, new_local);
        }
        new_local += 1;
    }

    cfg.locals.truncate(new_local.index());
    replacements
}
