use bitset::BitSet;
use cfg::{
    BasicBlock, Callback, ControlFlowGraph, InstIdx, InstrDst, Instruction, Local, Op, Operand,
    Phi, PhiIdx, Place, Terminator,
};
use data_flow::{direction, GenKill, GenKillAnalysis, GenKillAnalysisDomain, ResultsVisitorMut};

pub struct LivePlacesAnalysis<'a> {
    pub outputs: &'a BitSet<Place>,
}

impl GenKillAnalysisDomain for LivePlacesAnalysis<'_> {
    type Domain = BitSet<Place>;
    type Idx = Place;
    type Direction = direction::Backward;
    const NAME: &'static str = "LiveLocalAnalysis";

    fn bottom_value(&self, cfg: &ControlFlowGraph) -> Self::Domain {
        BitSet::new_empty(cfg.next_place.into())
    }

    fn initialize_start_block(&self, _cfg: &ControlFlowGraph, state: &mut Self::Domain) {
        state.union(self.outputs);
    }

    fn domain_size(&self, cfg: &ControlFlowGraph) -> usize {
        cfg.next_place.into()
    }
}

impl GenKillAnalysis for LivePlacesAnalysis<'_> {
    fn instruction_effect(
        &self,
        _cfg: &ControlFlowGraph,
        trans: &mut impl GenKill<Self::Idx>,
        instr: &Instruction,
        _idx: InstIdx,
        _bb: BasicBlock,
    ) {
        if let InstrDst::Place(place) = instr.dst {
            trans.kill(place)
        }
        instr.visit_operands(|op| {
            if let Operand::Place(place) = op {
                trans.gen(*place)
            }
        })
    }

    fn terminator_effect(
        &self,
        _cfg: &ControlFlowGraph,
        trans: &mut impl GenKill<Self::Idx>,
        terminator: &Terminator,
        _bb: BasicBlock,
    ) {
        if let Terminator::Split { condition: Operand::Place(place), .. } = terminator {
            trans.gen(*place)
        }
    }
}

pub struct DeadCodeElimination<'a> {
    pub live_locals: BitSet<Local>,
    pub callbacks_with_sideeffect: &'a BitSet<Callback>,
}

impl<'a> ResultsVisitorMut for DeadCodeElimination<'_> {
    type FlowState = BitSet<Place>;

    fn visit_phi_before_effect(
        &mut self,
        _state: &Self::FlowState,
        phi: &mut Phi,
        _block: BasicBlock,
        _id: PhiIdx,
    ) {
        // There are no noops for phis but we treat an empty phi as such
        if self.live_locals.contains(phi.dst) {
            for src in phi.sources.values() {
                self.live_locals.insert(*src);
            }
        } else {
            phi.sources.clear()
        }
    }

    fn visit_instruction_before_effect(
        &mut self,
        state: &Self::FlowState,
        instr: &mut Instruction,
        _block: BasicBlock,
        _id: InstIdx,
    ) {
        let relevant = match instr.dst {
            InstrDst::Local(local) => self.live_locals.contains(local),
            InstrDst::Place(place) => state.contains(place),
            InstrDst::Ignore => false,
        };

        let has_side_effects =
            matches!(instr.op, Op::Call(call) if self.callbacks_with_sideeffect.contains(call));

        if relevant || has_side_effects {
            instr.visit_operands(|op| {
                if let Operand::Local(local) = op {
                    self.live_locals.insert(*local);
                }
            });
        } else {
            *instr = Instruction::default()
        }
    }

    fn visit_terminator_before_effect(
        &mut self,
        _state: &Self::FlowState,
        term: &mut Terminator,
        _block: BasicBlock,
    ) {
        if let Terminator::Split { condition: Operand::Local(local), .. } = term {
            self.live_locals.insert(*local);
        }
    }
}

pub fn dead_code_elimination(
    cfg: &mut ControlFlowGraph,
    outputs: &BitSet<Place>,
    callbacks_with_sideeffect: &BitSet<Callback>,
) {
    cfg.cannonicalize_ret();

    let live_places = LivePlacesAnalysis { outputs }.into_engine(cfg).iterate_to_fixpoint();
    let postorder: Vec<_> = cfg.postorder_iter().map(|(bb, _)| bb).collect();
    live_places.visit_in_blocks_with_mut(
        cfg,
        postorder,
        &mut DeadCodeElimination {
            live_locals: BitSet::new_empty(cfg.next_local.into()),
            callbacks_with_sideeffect,
        },
    )
}
