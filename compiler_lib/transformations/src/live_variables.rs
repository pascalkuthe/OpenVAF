/*
 *  ******************************************************************************************
 *   Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *   It is subject to the license terms in the LICENSE file found in the top-level directory
 *   of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *   No part of frontend, including this file, may be copied, modified, propagated, or
 *   distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::program_dependence::InvProgramDependenceGraph;
use crate::Strip;
use itertools::izip;
use openvaf_data_structures::index_vec::IndexVec;
use openvaf_data_structures::{BitSet, WorkQueue};
use openvaf_middle::cfg::{
    BasicBlock, CfgPass, ControlFlowGraph, IntLocation, InternedLocations, TerminatorKind,
};
use openvaf_middle::dfa::{Backward, DfGraph, Engine, GenKillAnalysis, GenKillEngine, GenKillSet};
use openvaf_middle::{impl_pass_span, CallType, Local, StmntKind};
use std::collections::VecDeque;
use std::iter::FromIterator;
use tracing::{debug, trace, trace_span};

pub struct LiveLocalAnalysis(pub Option<BitSet<Local>>);

impl<C: CallType> CfgPass<'_, C> for LiveLocalAnalysis {
    type Result = DfGraph<BitSet<Local>>;

    fn run(self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
        let mut analysis = &mut LiveVariablesAnalysisImpl {
            cfg,
            outputs: self.0,
        };
        let mut gen_kill_engine = GenKillEngine::new(cfg, analysis);
        let res = Engine::new(&cfg, &mut gen_kill_engine).iterate_to_fixpoint();

        res
    }

    impl_pass_span!("live_variable_analysis");
}

struct LiveVariablesAnalysisImpl<'a, C: CallType> {
    cfg: &'a ControlFlowGraph<C>,
    outputs: Option<BitSet<Local>>,
}

impl<'a, C: CallType> GenKillAnalysis<C> for LiveVariablesAnalysisImpl<'a, C> {
    type SetType = Local;
    type Direction = Backward;

    fn transfer_function(
        &mut self,
        gen_kill_set: &mut GenKillSet<Local>,
        basic_bock: BasicBlock,
        cfg: &ControlFlowGraph<C>,
    ) {
        if let Some(ref term) = cfg[basic_bock].terminator {
            if let TerminatorKind::Split { ref condition, .. } = term.kind {
                condition.for_locals(|local| gen_kill_set.gen(local))
            }
        }

        for (stmnt, _) in cfg[basic_bock].statements.iter().rev() {
            if let StmntKind::Assignment(dst, val) = stmnt {
                gen_kill_set.kill(*dst);
                val.for_locals(|local| gen_kill_set.gen(local))
            }
        }

        for phi in cfg[basic_bock].phi_statements.iter().rev() {
            gen_kill_set.kill(phi.dst);
            for src in phi.sources.values() {
                gen_kill_set.gen(*src)
            }
        }
    }

    fn max_idx(&self) -> Self::SetType {
        self.cfg.locals.len_idx()
    }

    fn setup_entry(&mut self, block: BasicBlock, graph: &mut DfGraph<BitSet<Self::SetType>>) {
        if let Some(outputs) = &self.outputs {
            graph.in_sets[block].union_with(outputs)
        }
    }
}

pub struct DeadCodeScan<'a> {
    pub live_variables: Option<IndexVec<BasicBlock, BitSet<Local>>>,
    pub extra_locals: Option<BitSet<Local>>,
    pub locations: &'a InternedLocations,
}

impl<'a, C: CallType> CfgPass<'_, C> for DeadCodeScan<'a> {
    type Result = BitSet<IntLocation>;

    fn run(self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
        let Self {
            live_variables,
            extra_locals,
            locations,
        } = self;

        let live_variables =
            live_variables.unwrap_or_else(|| cfg.run_pass(LiveLocalAnalysis(extra_locals)).in_sets);

        let mut retain = BitSet::new_empty(self.locations.len_idx());
        for (bb, mut live_vars, block_locations) in
            izip!(&cfg.blocks, live_variables, locations.blocks.iter())
        {
            if let Some(ref term) = bb.terminator {
                if let TerminatorKind::Split { ref condition, .. } = term.kind {
                    condition.for_locals(|local| live_vars.insert(local))
                }
            }

            for (id, (stmnt, _)) in bb.statements.iter_enumerated().rev() {
                match stmnt {
                    StmntKind::Assignment(dst, val) => {
                        if live_vars.remove(*dst) {
                            val.for_locals(|local| live_vars.insert(local));
                            retain.insert(block_locations.stmnt_start + id.index())
                        }
                    }

                    StmntKind::Call(_, _, _) => {
                        retain.insert(block_locations.stmnt_start + id.index())
                    }

                    StmntKind::NoOp => {}
                }
            }

            for (id, phi) in bb.phi_statements.iter_enumerated().rev() {
                if live_vars.remove(phi.dst) {
                    for src in phi.sources.values() {
                        live_vars.insert(*src)
                    }
                    retain.insert(block_locations.phi_start + id.index())
                }
            }
        }
        retain
    }

    impl_pass_span!("dead_code_scan");
}

impl<'a> DeadCodeScan<'a> {
    pub fn new(locations: &'a InternedLocations) -> Self {
        Self {
            live_variables: None,
            extra_locals: None,
            locations,
        }
    }

    pub fn with_output_locals(
        locations: &'a InternedLocations,
        outputs_locals: BitSet<Local>,
    ) -> Self {
        Self {
            live_variables: None,
            extra_locals: Some(outputs_locals),
            locations,
        }
    }

    pub fn with_live_locals(
        locations: &'a InternedLocations,
        live_locals_in_set: IndexVec<BasicBlock, BitSet<Local>>,
    ) -> Self {
        Self {
            live_variables: Some(live_locals_in_set),
            extra_locals: None,
            locations,
        }
    }
}

pub struct DeadCodeElimination<'a> {
    pub live_variables: Option<IndexVec<BasicBlock, BitSet<Local>>>,
    pub extra_locals: Option<BitSet<Local>>,
    pub locations: &'a InternedLocations,
}

impl<'a, C: CallType> CfgPass<'_, C> for DeadCodeElimination<'a> {
    type Result = ();

    fn run(self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
        let retain = cfg.run_pass(DeadCodeScan {
            live_variables: self.live_variables,
            locations: self.locations,
            extra_locals: self.extra_locals,
        });
        cfg.run_pass(Strip {
            retain: &retain,
            locations: self.locations,
        })
    }

    impl_pass_span!("dead_code_elimination");
}

impl<'a> DeadCodeElimination<'a> {
    pub fn new(locations: &'a InternedLocations) -> Self {
        Self {
            live_variables: None,
            extra_locals: None,
            locations,
        }
    }

    pub fn with_output_locals(
        locations: &'a InternedLocations,
        outputs_locals: BitSet<Local>,
    ) -> Self {
        Self {
            live_variables: None,
            extra_locals: Some(outputs_locals),
            locations,
        }
    }

    pub fn with_live_locals(
        locations: &'a InternedLocations,
        live_locals_out_set: IndexVec<BasicBlock, BitSet<Local>>,
    ) -> Self {
        Self {
            live_variables: Some(live_locals_out_set),
            extra_locals: None,
            locations,
        }
    }
}
