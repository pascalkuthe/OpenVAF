/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

pub use crate::program_dependence::control_dependence::{
    ControlDependenceGraph, InvControlDependenceGraph,
};
pub use crate::program_dependence::reaching_definitions::{
    DefUserGraph, ReachingDefinitionsAnalysis, UseDefGraph,
};
use openvaf_data_structures::BitSet;
use openvaf_middle::cfg::{CfgPass, ControlFlowGraph, IntLocation, InternedLocations};
use openvaf_middle::dfa::DfGraph;
use openvaf_middle::{impl_pass_span, CallType};

mod control_dependence;
mod post_dominance;
mod reaching_definitions;

pub struct ProgramDependenceGraph {
    pub data_dependencies: UseDefGraph,
    pub control_dependencies: ControlDependenceGraph,
}

pub struct InvProgramDependenceGraph {
    pub data_dependencies: DefUserGraph,
    pub control_dependencies: InvControlDependenceGraph,
}

impl ProgramDependenceGraph {
    pub fn inverse(&self) -> InvProgramDependenceGraph {
        InvProgramDependenceGraph {
            data_dependencies: self.data_dependencies.inverse(),
            control_dependencies: self.control_dependencies.inverse(),
        }
    }
}

pub struct CalculateDataDependence;

pub struct BuildPDG<'a, D> {
    pub locations: &'a InternedLocations,
    pub data_dependence: D,
}

impl<'a, C: CallType> CfgPass<'_, C> for BuildPDG<'a, CalculateDataDependence> {
    type Result = ProgramDependenceGraph;

    fn run(self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
        let data_dependence = BuildReachingDefinitions(self.locations).run(cfg);
        BuildPDG {
            locations: self.locations,
            data_dependence,
        }
        .run(cfg)
    }

    impl_pass_span!("build_pdg");
}

impl<'a, C: CallType> CfgPass<'_, C>
    for BuildPDG<
        'a,
        (
            ReachingDefinitionsAnalysis<'a>,
            DfGraph<BitSet<IntLocation>>,
        ),
    >
{
    type Result = ProgramDependenceGraph;

    fn run(self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
        let control_dependencies = ControlDependenceGraph::from_cfg(cfg);
        ProgramDependenceGraph {
            data_dependencies: self
                .data_dependence
                .0
                .to_use_def(cfg, self.data_dependence.1),
            control_dependencies,
        }
    }

    impl_pass_span!("build_pdg");
}

pub struct BuildReachingDefinitions<'a>(pub &'a InternedLocations);

impl<'a: 'c, 'c, C: CallType> CfgPass<'c, C> for BuildReachingDefinitions<'a> {
    type Result = (
        ReachingDefinitionsAnalysis<'a>,
        DfGraph<BitSet<IntLocation>>,
    );

    fn run(self, cfg: &'c mut ControlFlowGraph<C>) -> Self::Result {
        let mut reaching_definitions = ReachingDefinitionsAnalysis::new(&*cfg, self.0);
        let dfg = reaching_definitions.solve(&*cfg);
        (reaching_definitions, dfg)
    }

    impl_pass_span!("build_reaching_definitions");
}
