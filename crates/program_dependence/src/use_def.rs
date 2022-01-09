use ahash::AHashMap;
use bitset::{BitSet, HybridBitSet, SparseBitMatrix};
use cfg::{
    BasicBlock, Callback, ControlFlowGraph, InstIdx, Instruction, Local, LocationKind, Op, Operand,
    Place, Terminator,
};
use data_flow::ResultsVisitor;

use crate::def_use::Def;
use crate::{
    AssigmentInterner, AssigmentLoc, Assignment, ProgramDependenGraph, ReachingDefinitions,
};

#[derive(Debug, Clone)]
pub struct UseDefGraph {
    pub assign_local: SparseBitMatrix<Assignment, Local>,
    pub assign_assign: SparseBitMatrix<Assignment, Assignment>,
    pub local_local: SparseBitMatrix<Local, Local>,
    pub local_assign: SparseBitMatrix<Local, Assignment>,
    pub term_assign: SparseBitMatrix<BasicBlock, Assignment>,
    pub term_local: AHashMap<BasicBlock, Local>,
}

impl UseDefGraph {
    pub fn build(
        cfg: &ControlFlowGraph,
        reaching_definitions: &ReachingDefinitions,
    ) -> UseDefGraph {
        let local_cnt = cfg.next_local.into();
        let intern = reaching_definitions.analysis.0.intern;
        let assign_cnt = intern.assigment_locations.len();
        let mut res = UseDefGraph {
            assign_local: SparseBitMatrix::new(assign_cnt, local_cnt),
            assign_assign: SparseBitMatrix::new(assign_cnt, assign_cnt),
            local_local: SparseBitMatrix::new(local_cnt, local_cnt),
            local_assign: SparseBitMatrix::new(local_cnt, assign_cnt),
            term_assign: SparseBitMatrix::new(cfg.blocks.len(), assign_cnt),
            term_local: AHashMap::new(),
        };

        reaching_definitions.visit_with(
            cfg,
            &mut UseDefBuilder { dst: &mut res, intern, tmp: BitSet::new_empty(assign_cnt) },
        );

        res
    }
}

struct UseDefBuilder<'a> {
    dst: &'a mut UseDefGraph,
    intern: &'a AssigmentInterner,
    tmp: BitSet<Assignment>,
}

impl ResultsVisitor for UseDefBuilder<'_> {
    type FlowState = BitSet<Assignment>;

    fn visit_phi_before_effect(
        &mut self,
        _state: &Self::FlowState,
        phi: &cfg::Phi,
        _bb: BasicBlock,
        _id: cfg::PhiIdx,
    ) {
        for src in phi.sources.values() {
            self.dst.local_local.insert(phi.dst, *src);
        }
    }

    fn visit_instruction_before_effect(
        &mut self,
        state: &Self::FlowState,
        instr: &cfg::Instruction,
        bb: BasicBlock,
        idx: InstIdx,
    ) {
        let mut locals = HybridBitSet::new_empty();
        let mut read_place = false;

        for operand in &instr.args {
            match operand {
                cfg::Operand::Local(local) => {
                    locals.insert(*local, self.dst.local_local.num_rows());
                }
                cfg::Operand::Place(place) => {
                    read_place = true;
                    if let Some(assings) = self.intern.place_assigments.row(*place) {
                        self.tmp.union(assings);
                    }
                }
                _ => (),
            }
        }

        match instr.dst {
            cfg::InstrDst::Local(local) => {
                if !locals.is_empty() {
                    *self.dst.local_local.ensure_row(local) = locals;
                }
                if read_place {
                    self.tmp.intersect(state);
                    self.dst
                        .local_assign
                        .ensure_row(local)
                        .union(&self.tmp, self.tmp.domain_size());

                    self.tmp.clear();
                }
            }
            cfg::InstrDst::Place(_) => {
                let assing =
                    self.intern.assigment_locations.unwrap_index(&AssigmentLoc { bb, instr: idx });
                if !locals.is_empty() {
                    *self.dst.assign_local.ensure_row(assing) = locals;
                }
                if read_place {
                    self.tmp.intersect(state);
                    self.dst
                        .assign_assign
                        .ensure_row(assing)
                        .union(&self.tmp, self.tmp.domain_size());

                    self.tmp.clear();
                }
            }
            _ => (),
        }
    }

    fn visit_terminator_before_effect(
        &mut self,
        state: &Self::FlowState,
        term: &Terminator,
        bb: BasicBlock,
    ) {
        match term {
            Terminator::Split { condition: Operand::Local(local), .. } => {
                self.dst.term_local.insert(bb, *local);
            }
            Terminator::Split { condition: Operand::Place(place), .. } => {
                if let Some(assings) = self.intern.place_assigments.row(*place) {
                    self.tmp.union(assings);
                    self.tmp.intersect(state);
                    self.dst.term_assign.ensure_row(bb).union(&self.tmp, self.tmp.domain_size());
                    self.tmp.clear();
                }
            }
            _ => (),
        }
    }
}

/// Perfroms recrusive depth first search on a use_def_graph optionally taking into account control
/// depenendence. The This allows extremly effective dead code elemination / program slicing.
/// All visited locations are stored in the `visited_assigments` and `visited_locals` set
#[derive(Debug, Clone)]
pub struct DepthFirstSearch {
    pub visited_locals: BitSet<Local>,
    pub visited_assigments: BitSet<Assignment>,
    visited_basic_blocks: BitSet<BasicBlock>,
}

impl DepthFirstSearch {
    pub fn new(pdg: &ProgramDependenGraph) -> DepthFirstSearch {
        DepthFirstSearch {
            visited_locals: BitSet::new_empty(pdg.use_def_graph.local_local.num_rows()),
            visited_assigments: BitSet::new_empty(pdg.use_def_graph.assign_assign.num_rows()),
            visited_basic_blocks: BitSet::new_empty(pdg.use_def_graph.term_assign.num_rows()),
        }
    }

    pub fn clear(&mut self) {
        self.visited_locals.clear();
        self.visited_assigments.clear();
        self.visited_basic_blocks.clear();
    }

    pub fn remove_unvisited_from_cfg(
        &self,
        cfg: &mut ControlFlowGraph,
        pdg: &mut ProgramDependenGraph,
    ) {
        for (assign, loc) in pdg.interner().assigment_locations.iter_enumerated() {
            if !self.visited_assigments.contains(assign) {
                cfg.blocks[loc.bb].instructions[loc.instr] = Instruction::default();
                pdg.use_def_graph.assign_local.take_row(assign);
                pdg.use_def_graph.assign_assign.take_row(assign);
            }
        }

        for (local, loc) in pdg.interner().local_defs.iter_enumerated() {
            if !self.visited_locals.contains(local) {
                match loc.kind {
                    cfg::LocationKind::Phi(phi) => cfg[loc.bb].phis[phi].sources.clear(),
                    cfg::LocationKind::Instruction(instr) => {
                        cfg.blocks[loc.bb].instructions[instr] = Instruction::default()
                    }
                    cfg::LocationKind::Terminator => (), // UNDEF local is dead... good
                }

                pdg.use_def_graph.local_local.take_row(local);
                pdg.use_def_graph.local_assign.take_row(local);
            }
        }
    }

    pub fn visited_instructions(
        &self,
        pdg: &ProgramDependenGraph,
        cfg: &ControlFlowGraph,
        mut f: impl FnMut(Def, &Instruction),
    ) {
        for (assign, loc) in pdg.interner().assigment_locations.iter_enumerated() {
            if self.visited_assigments.contains(assign) {
                f(assign.into(), &cfg.blocks[loc.bb].instructions[loc.instr])
            }
        }

        for (local, loc) in pdg.interner().local_defs.iter_enumerated() {
            if self.visited_locals.contains(local) {
                if let cfg::LocationKind::Instruction(instr) = loc.kind {
                    f(local.into(), &cfg.blocks[loc.bb].instructions[instr])
                }
            }
        }
    }

    pub fn walk_places<I: IntoIterator<Item = Place>, const WALK_CONTROL: bool>(
        &mut self,
        places: I,
        pdg: &ProgramDependenGraph,
        cfg: &ControlFlowGraph,
    ) {
        for place in places {
            self.walk_place::<WALK_CONTROL>(place, pdg, cfg)
        }
    }

    pub fn walk_sideffects<const WALK_CONTROL: bool>(
        &mut self,
        side_effect: &BitSet<Callback>,
        pdg: &ProgramDependenGraph,
        cfg: &ControlFlowGraph,
    ) {
        // This is not very performant as we fallback onto reaching_definitions but probably doesn't
        // matter as stmts with sideeffects are very rare
        let mut cursor = pdg.reaching_definitions.as_results_cursor(cfg);
        let intern = pdg.interner();
        for (bb, block) in cfg.blocks.iter_enumerated() {
            for (idx, instr) in block.instructions.iter_enumerated() {
                if matches!(instr.op, Op::Call(call) if side_effect.contains(call)) {
                    cursor.seek_before_effect(
                        cfg::Location { bb, kind: LocationKind::Instruction(idx) },
                        cfg,
                    );
                    instr.visit_operands(|op| match *op {
                        Operand::Local(local) => self.walk_local::<WALK_CONTROL>(local, pdg, cfg),
                        Operand::Place(place) => {
                            if let Some(assigns) = intern.place_assigments.row(place) {
                                for assing in assigns.iter() {
                                    if cursor.get().contains(assing) {
                                        self.walk_assign::<WALK_CONTROL>(assing, pdg, cfg)
                                    }
                                }
                            }
                        }
                        _ => (),
                    })
                }
            }
        }
    }

    pub fn walk_place<const WALK_CONTROL: bool>(
        &mut self,
        place: Place,
        pdg: &ProgramDependenGraph,
        cfg: &ControlFlowGraph,
    ) {
        let mut cursor = pdg.reaching_definitions.as_results_cursor(cfg);
        cursor.seek_to_block_end(cfg.blocks.last_key().unwrap(), cfg);
        if let Some(defs) = pdg.interner().place_assigments.row(place) {
            for assignment in defs.iter() {
                if cursor.get().contains(assignment) {
                    self.walk_assign::<WALK_CONTROL>(assignment, pdg, cfg)
                }
            }
        }
    }

    pub fn walk_assign<const WALK_CONTROL: bool>(
        &mut self,
        assignment: Assignment,
        pdg: &ProgramDependenGraph,
        cfg: &ControlFlowGraph,
    ) {
        if self.visited_assigments.insert(assignment) {
            if WALK_CONTROL {
                let bb = pdg.interner().assigment_locations[assignment].bb;
                self.walk_control_dependence(bb, pdg, cfg);
            }

            if let Some(deps) = pdg.use_def_graph.assign_assign.row(assignment) {
                for dep in deps.iter() {
                    self.walk_assign::<WALK_CONTROL>(dep, pdg, cfg);
                }
            }

            if let Some(deps) = pdg.use_def_graph.assign_local.row(assignment) {
                for dep in deps.iter() {
                    self.walk_local::<WALK_CONTROL>(dep, pdg, cfg);
                }
            }
        }
    }

    pub fn walk_local<const WALK_CONTROL: bool>(
        &mut self,
        local: Local,
        pdg: &ProgramDependenGraph,
        cfg: &ControlFlowGraph,
    ) {
        if self.visited_locals.insert(local) {
            if WALK_CONTROL {
                let bb = pdg.interner().local_defs[local].bb;
                self.walk_control_dependence(bb, pdg, cfg);
                if matches!(pdg.interner().local_defs[local].kind, LocationKind::Phi(_)) {
                    for predecessor in cfg.predecessors(bb) {
                        self.walk_control_dependence(*predecessor, pdg, cfg)
                    }
                }
            }

            if let Some(deps) = pdg.use_def_graph.local_assign.row(local) {
                for dep in deps.iter() {
                    self.walk_assign::<WALK_CONTROL>(dep, pdg, cfg);
                }
            }

            if let Some(deps) = pdg.use_def_graph.local_local.row(local) {
                for dep in deps.iter() {
                    self.walk_local::<WALK_CONTROL>(dep, pdg, cfg);
                }
            }
        }
    }

    fn walk_control_dependence(
        &mut self,
        bb: BasicBlock,
        pdg: &ProgramDependenGraph,
        cfg: &ControlFlowGraph,
    ) {
        if self.visited_basic_blocks.insert(bb) {
            if let Some(control_deps) = pdg.control_dependence.row(bb) {
                for dep in control_deps.iter() {
                    // self.visited_basic_blocks.insert(dep);
                    if let Some(deps) = pdg.use_def_graph.term_assign.row(dep) {
                        for dep in deps.iter() {
                            self.walk_assign::<true>(dep, pdg, cfg);
                        }
                    }

                    if let Some(dep) = pdg.use_def_graph.term_local.get(&dep) {
                        self.walk_local::<true>(*dep, pdg, cfg);
                    }
                }
            }
        }
    }
}
