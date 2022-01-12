use bitset::BitSet;
use cfg::InstrDst;
use data_flow::{direction, GenKillAnalysis, GenKillAnalysisDomain, GenKillAnalysisImpl, Results};

use crate::{AssigmentInterner, AssigmentLoc, Assignment};

pub type ReachingDefinitions<'a> = Results<GenKillAnalysisImpl<ReachingDefintionsAnalysis<'a>>>;

#[derive(Debug, Clone)]
pub struct ReachingDefintionsAnalysis<'a> {
    pub intern: &'a AssigmentInterner,
}

impl GenKillAnalysisDomain for ReachingDefintionsAnalysis<'_> {
    type Domain = BitSet<Assignment>;

    type Idx = Assignment;

    type Direction = direction::Forward;

    const NAME: &'static str = "Reaching Definitions";

    fn bottom_value(&self, _cfg: &cfg::ControlFlowGraph) -> Self::Domain {
        BitSet::new_empty(self.intern.assigment_locations.len())
    }

    #[inline(always)]
    fn initialize_start_block(&self, _cfg: &cfg::ControlFlowGraph, _state: &mut Self::Domain) {}

    fn domain_size(&self, _cfg: &cfg::ControlFlowGraph) -> usize {
        self.intern.assigment_locations.len()
    }
}

impl GenKillAnalysis for ReachingDefintionsAnalysis<'_> {
    fn instruction_effect(
        &self,
        _cfg: &cfg::ControlFlowGraph,
        trans: &mut impl data_flow::GenKill<Self::Idx>,
        instr: &cfg::Instruction,
        idx: cfg::InstIdx,
        bb: cfg::BasicBlock,
    ) {
        if let InstrDst::Place(place) = instr.dst {
            trans.kill_set(self.intern.place_assigments.row(place).unwrap());
            trans
                .gen(self.intern.assigment_locations.unwrap_index(&AssigmentLoc { bb, instr: idx }))
        }
    }
}
