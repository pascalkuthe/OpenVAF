/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use openvaf_data_structures::BitSet;
use openvaf_middle::cfg::{CfgPass, ControlFlowGraph, IntLocation, InternedLocations};
use openvaf_middle::{impl_pass_span, CallType};

pub struct Strip<'a> {
    pub retain: &'a BitSet<IntLocation>,
    pub locations: &'a InternedLocations,
}

impl<'a, C> CfgPass<'_, C> for Strip<'a>
where
    C: CallType,
{
    type Result = ();

    fn run(self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
        for (data, block_location) in cfg.blocks.iter_mut().zip(self.locations.blocks.iter()) {
            let mut loc = block_location.phi_start;
            data.phi_statements.retain(|_| {
                let res = self.retain.contains(loc);
                loc += 1;
                res
            });

            let mut loc = block_location.stmnt_start;
            data.statements.retain(|_| {
                let res = self.retain.contains(loc);
                loc += 1;
                res
            })
        }
    }

    impl_pass_span!("cfg_strip");
}
