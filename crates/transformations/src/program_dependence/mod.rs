/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

pub use crate::program_dependence::control_dependence::ControlDependenceGraph;
pub use crate::program_dependence::reaching_definitions::{
    ReachingDefinitionsAnalysis, UseDefGraph,
};
use openvaf_middle::cfg::{CfgPass, ControlFlowGraph, InternedLocations};
use openvaf_middle::{impl_pass_span, CallType};

mod control_dependence;
mod post_dominance;
mod reaching_definitions;

pub struct ProgramDependenceGraph {
    pub data_dependencies: UseDefGraph,
    pub control_dependencies: ControlDependenceGraph,
}

pub struct BuildPDG<'a>(pub &'a InternedLocations);

impl<'a, C: CallType> CfgPass<'_, C> for BuildPDG<'a> {
    type Result = ProgramDependenceGraph;

    fn run(self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
        let control_dependencies = ControlDependenceGraph::from_cfg(cfg);
        let data_dependencies = ReachingDefinitionsAnalysis::new(cfg, self.0).run(cfg);
        ProgramDependenceGraph {
            data_dependencies,
            control_dependencies,
        }
    }

    impl_pass_span!("build_pdg");
}
