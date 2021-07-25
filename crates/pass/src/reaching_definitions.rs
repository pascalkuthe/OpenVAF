/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use data_structures::{
    bit_set::{BitSet, SparseBitMatrix},
    iter::izip,
};
use ir::ids::StatementId;
use middle::cfg::{
    AnalysisPass, BasicBlock, ControlFlowGraph, IntLocation, InternedLocations, Phi, PhiData,
};
use middle::dfa::{
    self, direciton::Forward, GenKill, GenKillAnalysis, GenKillAnalysisDomain,
};
use middle::{impl_pass_span, CfgFunctions, Local, Statement, StmntKind};
use std::borrow::Borrow;

pub struct FindAssignments<'a>(pub &'a InternedLocations);

impl<'a, C: CfgFunctions> AnalysisPass<'_, C> for FindAssignments<'a> {
    type Result = SparseBitMatrix<Local, IntLocation>;
    impl_pass_span!("Find Assignments");

    fn run(self, cfg: &ControlFlowGraph<C>) -> Self::Result {
        let locations = self.0;
        let mut assignments = SparseBitMatrix::new(cfg.locals.len(), locations.len());

        for (block, block_locations) in izip!(&cfg.blocks, &locations.blocks) {
            for (phi, data) in block.phi_statements.iter_enumerated() {
                assignments.insert(data.dst, block_locations.phi_start + phi.index());
            }

            for (stmnt, (kind, _)) in block.statements.iter_enumerated() {
                if let StmntKind::Assignment(dst, _) = *kind {
                    assignments.insert(dst, block_locations.stmnt_start + stmnt.index());
                }
            }
        }

        assignments
    }
}

impl<'a, C, B> AnalysisPass<'a, C> for ReachingDefinitionsAnalysis<'a, B>
where
    C: CfgFunctions + 'a,
    B: Borrow<SparseBitMatrix<Local, IntLocation>> + 'a,
{
    type Result = dfa::GenKillResults<C, Self>;
    impl_pass_span!("Reaching Definitions Analysis");

    fn run(self, cfg: &'a ControlFlowGraph<C>) -> Self::Result {
        self.into_engine(cfg).iterate_to_fixpoint()
    }
}

impl<'a, C: CfgFunctions + 'a> AnalysisPass<'a, C>
    for ReachingDefinitionsAnalysis<'a, FindAssignments<'a>>
{
    type Result = dfa::GenKillResults<
        C,
        ReachingDefinitionsAnalysis<'a, SparseBitMatrix<Local, IntLocation>>,
    >;
    impl_pass_span!("Reaching Definitions Analysis");

    fn run(self, cfg: &'a ControlFlowGraph<C>) -> Self::Result {
        let assignments = cfg.analyse(self.assignments);
        cfg.analyse(ReachingDefinitionsAnalysis { assignments, locations: self.locations })
    }
}

pub struct ReachingDefinitionsAnalysis<'a, A> {
    pub assignments: A,
    pub locations: &'a InternedLocations,
}

impl<'a, C, B> GenKillAnalysisDomain<C> for ReachingDefinitionsAnalysis<'a, B>
where
    C: CfgFunctions,
    B: Borrow<SparseBitMatrix<Local, IntLocation>>,
{
    type Domain = BitSet<IntLocation>;
    type Idx = IntLocation;
    type Direction = Forward;
    const NAME: &'static str = "Reaching Definition Analysis";

    fn bottom_value(&self, _cfg: &ControlFlowGraph<C>) -> Self::Domain {
        BitSet::new_empty(self.locations.len())
    }

    fn initialize_start_block(&self, _cfg: &ControlFlowGraph<C>, _state: &mut Self::Domain) {}

    fn domain_size(&self, _cfg: &ControlFlowGraph<C>) -> usize {
        self.locations.len()
    }
}

impl<'a, B> ReachingDefinitionsAnalysis<'a, B>
where
    B: Borrow<SparseBitMatrix<Local, IntLocation>>,
{
    fn assignment_effect(
        &self,
        state: &mut impl GenKill<IntLocation>,
        dst: Local,
        pos: IntLocation,
    ) {
        if let Some(assignments) = self.assignments.borrow().row(dst) {
            state.kill_set(assignments);
        }
        state.gen(pos);
    }
}

impl<'a, C, B> GenKillAnalysis<C> for ReachingDefinitionsAnalysis<'a, B>
where
    C: CfgFunctions,
    B: Borrow<SparseBitMatrix<Local, IntLocation>>,
{
    fn phi_effect(
        &self,
        _cfg: &ControlFlowGraph<C>,
        trans: &mut impl GenKill<Self::Idx>,
        phi: &PhiData,
        bb: BasicBlock,
        idx: Phi,
    ) {
        self.assignment_effect(trans, phi.dst, self.locations[bb].phi_start + idx.index())
    }

    fn statement_effect(
        &self,
        _cfg: &ControlFlowGraph<C>,
        trans: &mut impl GenKill<Self::Idx>,
        statement: &Statement<C>,
        idx: StatementId,
        bb: BasicBlock,
    ) {
        if let StmntKind::Assignment(dst, _) = statement.0 {
            self.assignment_effect(trans, dst, self.locations[bb].stmnt_start + idx.index())
        }
    }
}
