/*
 *  ******************************************************************************************
 *   Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *   It is subject to the license terms in the LICENSE file found in the top-level directory
 *   of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *   No part of frontend, including this file, may be copied, modified, propagated, or
 *   distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::Strip;
use middle::cfg::{
    AnalysisPass, BasicBlock, ControlFlowGraph, IntLocation, InternedLocations, ModificationPass,
    Phi, PhiData, Terminator, TerminatorKind,
};
use middle::dfa::{self, GenKill, GenKillAnalysis, GenKillAnalysisDomain, ResultsVisitor};

use data_structures::bit_set::{BitSet, HybridBitSet};
use ir::ids::{StatementId, SyntaxCtx};
use middle::dfa::direciton::Backward;
use middle::{impl_pass_span, CfgFunctions, Local, StmntKind};
use std::borrow::Borrow;

#[derive(Default)]
pub struct LiveLocalAnalysis(pub HybridBitSet<Local>);

impl<'a, C: CfgFunctions + 'a> AnalysisPass<'a, C> for LiveLocalAnalysis {
    type Result = dfa::GenKillResults<C, Self>;
    impl_pass_span!("live_variable_analysis");

    fn run(self, cfg: &'a ControlFlowGraph<C>) -> Self::Result {
        self.into_engine(cfg).iterate_to_fixpoint()
    }
}

impl<C: CfgFunctions> GenKillAnalysisDomain<C> for LiveLocalAnalysis {
    type Domain = BitSet<Local>;
    type Idx = Local;
    type Direction = Backward;
    const NAME: &'static str = "LiveLocalAnalysis";

    fn bottom_value(&self, cfg: &ControlFlowGraph<C>) -> Self::Domain {
        BitSet::new_empty(cfg.locals.len())
    }

    fn initialize_start_block(&self, _cfg: &ControlFlowGraph<C>, state: &mut Self::Domain) {
        state.union(&self.0);
    }

    fn domain_size(&self, cfg: &ControlFlowGraph<C>) -> usize {
        cfg.locals.len()
    }
}

impl<C: CfgFunctions> GenKillAnalysis<C> for LiveLocalAnalysis {
    fn phi_effect(
        &self,
        _cfg: &ControlFlowGraph<C>,
        trans: &mut impl GenKill<Self::Idx>,
        phi: &PhiData,
        _bb: BasicBlock,
        _idx: Phi,
    ) {
        trans.kill(phi.dst);
        for src in phi.sources.values() {
            trans.gen(*src)
        }
    }

    fn statement_effect(
        &self,
        _cfg: &ControlFlowGraph<C>,
        trans: &mut impl GenKill<Self::Idx>,
        statement: &(StmntKind<C>, SyntaxCtx),
        _idx: StatementId,
        _bb: BasicBlock,
    ) {
        if let StmntKind::Assignment(dst, ref val) = statement.0 {
            trans.kill(dst);
            val.for_locals(|local| trans.gen(local))
        }
    }

    fn terminator_effect(
        &self,
        _cfg: &ControlFlowGraph<C>,
        trans: &mut impl GenKill<Self::Idx>,
        terminator: &Terminator<C>,
        _bb: BasicBlock,
    ) {
        if let TerminatorKind::Split { ref condition, .. } = terminator.kind {
            condition.for_locals(|local| trans.gen(local))
        }
    }
}

pub struct DeadCodeScan<'a, L> {
    pub live_locals: L,
    pub locations: &'a InternedLocations,
}

impl<'a, C, L> AnalysisPass<'_, C> for DeadCodeScan<'a, L>
where
    C: CfgFunctions,
    L: Borrow<dfa::GenKillResults<C, LiveLocalAnalysis>>,
{
    type Result = BitSet<IntLocation>;

    fn run(self, cfg: &ControlFlowGraph<C>) -> Self::Result {
        let Self { live_locals, locations } = self;

        let mut visit = LiveCodeFinder::new(locations);
        live_locals.borrow().visit_with(cfg, &mut visit);
        visit.live_code
    }

    impl_pass_span!("dead_code_scan");
}

impl<'a, C: CfgFunctions> AnalysisPass<'_, C> for DeadCodeScan<'a, LiveLocalAnalysis> {
    type Result = BitSet<IntLocation>;

    fn run(self, cfg: &ControlFlowGraph<C>) -> Self::Result {
        let Self { live_locals, locations } = self;

        let live_locals = cfg.analyse(live_locals);

        let mut visit = LiveCodeFinder::new(locations);
        live_locals.borrow().visit_with(cfg, &mut visit);
        visit.live_code
    }

    impl_pass_span!("dead_code_scan");
}

impl<'a> DeadCodeScan<'a, LiveLocalAnalysis> {
    pub fn new(locations: &'a InternedLocations) -> Self {
        Self { live_locals: LiveLocalAnalysis::default(), locations }
    }

    pub fn with_output_locals(
        locations: &'a InternedLocations,
        outputs_locals: HybridBitSet<Local>,
    ) -> Self {
        Self { live_locals: LiveLocalAnalysis(outputs_locals), locations }
    }
}

struct LiveCodeFinder<'a> {
    live_code: BitSet<IntLocation>,
    locations: &'a InternedLocations,
}

impl<'a> LiveCodeFinder<'a> {
    pub fn new(locations: &'a InternedLocations) -> Self {
        Self { live_code: BitSet::new_empty(locations.len()), locations }
    }
}

impl<'a, C: CfgFunctions> ResultsVisitor<C> for LiveCodeFinder<'a> {
    type FlowState = BitSet<Local>;

    #[inline]
    fn visit_phi_before_effect(
        &mut self,
        state: &Self::FlowState,
        phi: &PhiData,
        block: BasicBlock,
        id: Phi,
    ) {
        if state.contains(phi.dst) {
            self.live_code.insert(self.locations[block].phi_start + id.index());
        }
    }

    fn visit_statement_before_effect(
        &mut self,
        state: &Self::FlowState,
        stmnt: &(StmntKind<C>, SyntaxCtx),
        block: BasicBlock,
        id: StatementId,
    ) {
        if matches!(stmnt.0, StmntKind::Assignment(dst,_) if state.contains(dst)) {
            self.live_code.insert(self.locations[block].stmnt_start + id.index());
        }
    }
}

pub struct DeadCodeElimination<'a, L> {
    pub live_locals: L,
    pub locations: &'a InternedLocations,
}

impl<'a, C, L> ModificationPass<'_, C> for DeadCodeElimination<'a, L>
where
    C: CfgFunctions,
    L: Borrow<dfa::GenKillResults<C, LiveLocalAnalysis>>,
{
    type Result = ();

    fn run(self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
        let Self { live_locals, locations } = self;

        let alive_code = cfg.analyse(DeadCodeScan { live_locals, locations });

        cfg.modify(Strip { retain: &alive_code, locations })
    }

    impl_pass_span!("dead_code_elimination");
}

impl<'a, C: CfgFunctions> ModificationPass<'_, C> for DeadCodeElimination<'a, LiveLocalAnalysis> {
    type Result = ();

    fn run(self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
        let Self { live_locals, locations } = self;

        let alive_code = cfg.analyse(DeadCodeScan { live_locals, locations });

        cfg.modify(Strip { retain: &alive_code, locations })
    }

    impl_pass_span!("dead_code_elimination");
}

impl<'a> DeadCodeElimination<'a, LiveLocalAnalysis> {
    pub fn new(locations: &'a InternedLocations) -> Self {
        Self { live_locals: LiveLocalAnalysis::default(), locations }
    }

    pub fn with_output_locals(
        locations: &'a InternedLocations,
        outputs_locals: HybridBitSet<Local>,
    ) -> Self {
        Self { live_locals: LiveLocalAnalysis(outputs_locals), locations }
    }
}
