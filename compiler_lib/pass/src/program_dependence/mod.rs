/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use std::borrow::Borrow;

use openvaf_data_structures::bit_set::{HybridBitSet, SparseBitMatrix};
use openvaf_middle::cfg::{
    AnalysisPass, BasicBlock, ControlFlowGraph, IntLocation, InternedLocations,
};
use openvaf_middle::{impl_pass_span, CfgFunctions, Local};

use crate::post_dominance::PostDominators;
use crate::program_dependence::control_dependence::BuildControlDependenceGraph;
pub use crate::program_dependence::control_dependence::{
    ControlDependenceGraph, InvControlDependenceGraph,
};
use crate::program_dependence::data_dependence::{BuildUseDefGraph, CalculateReachingDefinitions};
pub use crate::program_dependence::data_dependence::{DefUserGraph, UseDefGraph};
use crate::reaching_definitions::FindAssignments;
use crate::BuildPostDominators;

pub mod control_dependence;
pub mod data_dependence;

pub struct ProgramDependenceGraph<A: Borrow<SparseBitMatrix<Local, IntLocation>>> {
    pub data_dependencies: UseDefGraph,
    pub control_dependencies: ControlDependenceGraph,
    pub assignments: A,
}

impl<A: Borrow<SparseBitMatrix<Local, IntLocation>>> ProgramDependenceGraph<A> {
    pub fn get_data_dependencies(&self, loc: IntLocation) -> Option<&HybridBitSet<IntLocation>> {
        self.data_dependencies.0.row(loc)
    }
    pub fn get_control_dependencies(&self, block: BasicBlock) -> Option<&HybridBitSet<BasicBlock>> {
        self.control_dependencies.0.row(block)
    }
    pub fn get_assignments(&self, local: Local) -> Option<&HybridBitSet<IntLocation>> {
        self.assignments.borrow().row(local)
    }
}

impl<A: Borrow<SparseBitMatrix<Local, IntLocation>>> InvProgramDependenceGraph<A> {
    pub fn get_data_dependencies(&self, loc: IntLocation) -> Option<&HybridBitSet<IntLocation>> {
        self.data_dependencies.0.row(loc)
    }
    pub fn get_control_dependencies(&self, block: BasicBlock) -> Option<&HybridBitSet<BasicBlock>> {
        self.control_dependencies.0.row(block)
    }
    pub fn get_assignments(&self, local: Local) -> Option<&HybridBitSet<IntLocation>> {
        self.assignments.borrow().row(local)
    }
}

impl<A> ProgramDependenceGraph<A>
where
    A: ToOwned<Owned = SparseBitMatrix<Local, IntLocation>>
        + Borrow<SparseBitMatrix<Local, IntLocation>>,
{
    #[allow(clippy::redundant_clone)]
    pub fn into_owned(self) -> ProgramDependenceGraph<SparseBitMatrix<Local, IntLocation>> {
        ProgramDependenceGraph {
            data_dependencies: self.data_dependencies,
            control_dependencies: self.control_dependencies,
            assignments: self.assignments.to_owned(),
        }
    }
}

pub struct InvProgramDependenceGraph<A: Borrow<SparseBitMatrix<Local, IntLocation>>> {
    pub data_dependencies: DefUserGraph,
    pub control_dependencies: InvControlDependenceGraph,
    pub assignments: A,
}

impl<A> InvProgramDependenceGraph<A>
where
    A: ToOwned<Owned = SparseBitMatrix<Local, IntLocation>>
        + Borrow<SparseBitMatrix<Local, IntLocation>>,
{
    #[allow(clippy::redundant_clone)]
    pub fn into_owned(self) -> InvProgramDependenceGraph<SparseBitMatrix<Local, IntLocation>> {
        InvProgramDependenceGraph {
            data_dependencies: self.data_dependencies,
            control_dependencies: self.control_dependencies,
            assignments: self.assignments.to_owned(),
        }
    }
}

impl<A: Borrow<SparseBitMatrix<Local, IntLocation>>> ProgramDependenceGraph<A> {
    pub fn inverse(&self) -> InvProgramDependenceGraph<&SparseBitMatrix<Local, IntLocation>> {
        InvProgramDependenceGraph {
            data_dependencies: self.data_dependencies.inverse(),
            control_dependencies: self.control_dependencies.inverse(),
            assignments: self.assignments.borrow(),
        }
    }
}

pub struct CalculateUseDefGraph;

pub struct BuildPDG<'a, D, A, C> {
    pub locations: &'a InternedLocations,
    pub use_def_graph: D,
    pub assignments: A,
    pub control_dependence: C,
}

impl<'a, C: CfgFunctions> AnalysisPass<'_, C>
    for BuildPDG<'a, CalculateUseDefGraph, FindAssignments<'a>, ControlDependenceGraph>
{
    type Result = ProgramDependenceGraph<SparseBitMatrix<Local, IntLocation>>;

    fn run(self, cfg: &ControlFlowGraph<C>) -> Self::Result {
        let assignments = cfg.analyse(self.assignments);

        cfg.analyse(BuildPDG {
            locations: self.locations,
            use_def_graph: CalculateUseDefGraph,
            assignments,
            control_dependence: self.control_dependence,
        })
    }

    impl_pass_span!("Build PDG");
}

impl<'a, C: CfgFunctions> AnalysisPass<'_, C>
    for BuildPDG<
        'a,
        CalculateUseDefGraph,
        SparseBitMatrix<Local, IntLocation>,
        ControlDependenceGraph,
    >
{
    type Result = ProgramDependenceGraph<SparseBitMatrix<Local, IntLocation>>;

    fn run(self, cfg: &ControlFlowGraph<C>) -> Self::Result {
        let use_def_graph = cfg.analyse(BuildUseDefGraph {
            locations: self.locations,
            assignments: &self.assignments,
            reaching_definitions: CalculateReachingDefinitions,
        });

        cfg.analyse(BuildPDG {
            locations: self.locations,
            use_def_graph,
            assignments: self.assignments,
            control_dependence: self.control_dependence,
        })
    }

    impl_pass_span!("Build PDG");
}

impl<'a, C: CfgFunctions> AnalysisPass<'_, C>
    for BuildPDG<'a, UseDefGraph, SparseBitMatrix<Local, IntLocation>, ControlDependenceGraph>
{
    type Result = ProgramDependenceGraph<SparseBitMatrix<Local, IntLocation>>;

    fn run(self, _cfg: &ControlFlowGraph<C>) -> Self::Result {
        ProgramDependenceGraph {
            data_dependencies: self.use_def_graph,
            control_dependencies: self.control_dependence,
            assignments: self.assignments,
        }
    }

    impl_pass_span!("Build PDG");
}

impl<'a, C: CfgFunctions> AnalysisPass<'_, C>
    for BuildPDG<
        'a,
        CalculateUseDefGraph,
        FindAssignments<'a>,
        BuildControlDependenceGraph<BuildPostDominators>,
    >
{
    type Result = ProgramDependenceGraph<SparseBitMatrix<Local, IntLocation>>;

    fn run(self, cfg: &ControlFlowGraph<C>) -> Self::Result {
        let assignments = cfg.analyse(self.assignments);

        cfg.analyse(BuildPDG {
            locations: self.locations,
            use_def_graph: CalculateUseDefGraph,
            assignments,
            control_dependence: self.control_dependence,
        })
    }

    impl_pass_span!("Build PDG");
}

impl<'a, C: CfgFunctions> AnalysisPass<'_, C>
    for BuildPDG<
        'a,
        CalculateUseDefGraph,
        SparseBitMatrix<Local, IntLocation>,
        BuildControlDependenceGraph<BuildPostDominators>,
    >
{
    type Result = ProgramDependenceGraph<SparseBitMatrix<Local, IntLocation>>;

    fn run(self, cfg: &ControlFlowGraph<C>) -> Self::Result {
        let use_def_graph = cfg.analyse(BuildUseDefGraph {
            locations: self.locations,
            assignments: &self.assignments,
            reaching_definitions: CalculateReachingDefinitions,
        });

        cfg.analyse(BuildPDG {
            locations: self.locations,
            use_def_graph,
            assignments: self.assignments,
            control_dependence: self.control_dependence,
        })
    }

    impl_pass_span!("Build PDG");
}

impl<'a, C: CfgFunctions> AnalysisPass<'_, C>
    for BuildPDG<
        'a,
        UseDefGraph,
        SparseBitMatrix<Local, IntLocation>,
        BuildControlDependenceGraph<BuildPostDominators>,
    >
{
    type Result = ProgramDependenceGraph<SparseBitMatrix<Local, IntLocation>>;

    fn run(self, cfg: &ControlFlowGraph<C>) -> Self::Result {
        let control_dependencies = cfg.analyse(self.control_dependence);

        ProgramDependenceGraph {
            data_dependencies: self.use_def_graph,
            control_dependencies,
            assignments: self.assignments,
        }
    }

    impl_pass_span!("Build PDG");
}

impl<'a, C: CfgFunctions, PD: Borrow<PostDominators>> AnalysisPass<'_, C>
    for BuildPDG<'a, CalculateUseDefGraph, FindAssignments<'a>, BuildControlDependenceGraph<PD>>
{
    type Result = ProgramDependenceGraph<SparseBitMatrix<Local, IntLocation>>;

    fn run(self, cfg: &ControlFlowGraph<C>) -> Self::Result {
        let assignments = cfg.analyse(self.assignments);

        cfg.analyse(BuildPDG {
            locations: self.locations,
            use_def_graph: CalculateUseDefGraph,
            assignments,
            control_dependence: self.control_dependence,
        })
    }

    impl_pass_span!("Build PDG");
}

impl<'a, C: CfgFunctions, PD: Borrow<PostDominators>> AnalysisPass<'_, C>
    for BuildPDG<
        'a,
        CalculateUseDefGraph,
        SparseBitMatrix<Local, IntLocation>,
        BuildControlDependenceGraph<PD>,
    >
{
    type Result = ProgramDependenceGraph<SparseBitMatrix<Local, IntLocation>>;

    fn run(self, cfg: &ControlFlowGraph<C>) -> Self::Result {
        let use_def_graph = cfg.analyse(BuildUseDefGraph {
            locations: self.locations,
            assignments: &self.assignments,
            reaching_definitions: CalculateReachingDefinitions,
        });

        cfg.analyse(BuildPDG {
            locations: self.locations,
            use_def_graph,
            assignments: self.assignments,
            control_dependence: self.control_dependence,
        })
    }

    impl_pass_span!("Build PDG");
}

impl<'a, C: CfgFunctions, PD: Borrow<PostDominators>> AnalysisPass<'_, C>
    for BuildPDG<
        'a,
        UseDefGraph,
        SparseBitMatrix<Local, IntLocation>,
        BuildControlDependenceGraph<PD>,
    >
{
    type Result = ProgramDependenceGraph<SparseBitMatrix<Local, IntLocation>>;

    fn run(self, cfg: &ControlFlowGraph<C>) -> Self::Result {
        let control_dependencies = cfg.analyse(self.control_dependence);

        ProgramDependenceGraph {
            data_dependencies: self.use_def_graph,
            control_dependencies,
            assignments: self.assignments,
        }
    }

    impl_pass_span!("Build PDG");
}
