use std::collections::VecDeque;

use bitset::BitSet;
use cfg::{
    BasicBlock, Callback, ControlFlowGraph, Instruction, Local, LocationKind, Op, Operand, Place,
    Terminator,
};
use data_flow::GenKillResultsRefCursor;
use stdx::impl_from;

use crate::{Assigment, ProgramDependenGraph, ReachingDefintionsAnalysis};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Work {
    Local(Local),
    Assigment(Assigment),
    BasicBlock(BasicBlock),
}

impl_from!(
    Local,Assigment,BasicBlock for Work
);

#[derive(Debug, Clone)]
pub struct ProgramDependenceWorkQueue<'a, 'i> {
    pub work_queue: VecDeque<Work>,
    pub visited_locals: BitSet<Local>,
    pub visited_assigns: BitSet<Assigment>,
    pub visited_blocks: BitSet<BasicBlock>,
    pub tmp: BitSet<Assigment>,
    pub pdg: &'a ProgramDependenGraph<'i>,
    pub cursor: GenKillResultsRefCursor<'a, ReachingDefintionsAnalysis<'i>>,
}

impl<'a, 'i> ProgramDependenceWorkQueue<'a, 'i> {
    pub fn new(
        cfg: &ControlFlowGraph,
        pdg: &'a ProgramDependenGraph<'i>,
    ) -> ProgramDependenceWorkQueue<'a, 'i> {
        ProgramDependenceWorkQueue {
            work_queue: VecDeque::new(),
            visited_locals: BitSet::new_empty(cfg.next_local.into()),
            visited_assigns: BitSet::new_empty(pdg.interner().local_defs.len()),
            visited_blocks: BitSet::new_empty(cfg.blocks.len()),
            tmp: BitSet::new_empty(pdg.interner().local_defs.len()),
            pdg,
            cursor: pdg.reaching_definitions.as_results_cursor(cfg),
        }
    }

    pub fn insert_stmts_with_sideffects(
        &mut self,
        cfg: &ControlFlowGraph,
        sideeffects: &BitSet<Callback>,
    ) {
        for bb in &cfg.blocks {
            for instr in &bb.instructions {
                if matches!(instr.op, Op::Call(call) if sideeffects.contains(call)) {
                    self.walk_instr(instr);
                    self.tmp.clear();
                }
            }
        }
    }

    pub fn run(&mut self, visitor: &mut impl UseDefVisitor) {
        while let Some(work) = self.take() {
            match work {
                Work::Local(local) => visitor.visit_local(self, local),
                Work::Assigment(assign) => visitor.visit_assign(self, assign),
                Work::BasicBlock(bb) => visitor.visit_bb(self, bb),
            }
        }
    }

    pub fn insert_places(
        &mut self,
        cfg: &ControlFlowGraph,
        pdg: &ProgramDependenGraph,
        places: &BitSet<Place>,
    ) {
        let intern = pdg.interner();
        for place in places.iter() {
            if let Some(row) = intern.place_assigments.row(place) {
                self.tmp.union(row);
            }
        }

        self.cursor.seek_to_block_end(cfg.blocks.last_key().unwrap(), cfg);
        self.tmp.intersect(self.cursor.get());
        self.insert_tmp_assigments();
        self.tmp.clear();
    }

    pub fn insert_tmp_assigments(&mut self) {
        for assign in self.tmp.iter() {
            if self.visited_assigns.insert(assign) {
                self.work_queue.push_back(assign.into())
            }
        }
    }
    pub fn insert(&mut self, work: Work) {
        let changed = match work {
            Work::Local(local) => self.visited_locals.insert(local),
            Work::Assigment(assign) => self.visited_assigns.insert(assign),
            Work::BasicBlock(bb) => self.visited_blocks.insert(bb),
        };

        if changed {
            self.work_queue.push_back(work)
        }
    }

    pub fn take(&mut self) -> Option<Work> {
        self.work_queue.pop_front()
    }

    pub fn pop(&mut self) -> Option<Work> {
        let work = self.take();
        match work {
            Some(Work::Assigment(assign)) => {
                self.visited_assigns.remove(assign);
            }
            Some(Work::BasicBlock(bb)) => {
                self.visited_blocks.remove(bb);
            }
            Some(Work::Local(local)) => {
                self.visited_locals.remove(local);
            }
            None => (),
        }
        work
    }

    pub fn walk_local<const WALK_CONTROL: bool>(&mut self, cfg: &ControlFlowGraph, local: Local) {
        let intern = self.pdg.interner();
        let loc = intern.local_defs[local];
        if WALK_CONTROL {
            self.walk_control_dependence(loc.block);
        }
        match loc.kind {
            LocationKind::Phi(phi) => {
                for src in cfg[loc.block].phis[phi].sources.values().copied() {
                    self.insert(src.into());
                }
            }
            LocationKind::Instruction(instr) => {
                self.cursor.seek_before_effect(loc, cfg);
                self.walk_instr(&cfg[loc.block].instructions[instr])
            }
            LocationKind::Terminator => unreachable!("UNDEF local {:?}", local),
        };
        // self.insert(loc.block.into());
    }

    pub fn walk_instr(&mut self, instr: &Instruction) {
        instr.visit_operands(|arg| self.walk_operand(*arg));

        self.tmp.intersect(self.cursor.get());
        self.insert_tmp_assigments();
    }

    pub fn walk_operand(&mut self, op: Operand) {
        match op {
            Operand::Local(local) => {
                self.insert(local.into());
            }
            Operand::Place(place) => {
                if let Some(assigments) = self.pdg.interner().place_assigments.row(place) {
                    self.tmp.union(assigments);
                }
            }
            _ => (),
        }
    }

    pub fn walk_assign<const WALK_CONTROL: bool>(
        &mut self,
        cfg: &ControlFlowGraph,
        assign: Assigment,
    ) {
        let loc = self.pdg.interner().assigment_locations[assign];

        if WALK_CONTROL {
            self.walk_control_dependence(loc.bb);
        }
        self.cursor.seek_before_effect(loc.into(), cfg);
        self.walk_instr(&cfg[loc.bb].instructions[loc.instr])
    }

    pub fn walk_terminator<const WALK_CONTROL: bool>(
        &mut self,
        cfg: &ControlFlowGraph,
        bb: BasicBlock,
    ) {
        if WALK_CONTROL {
            self.walk_control_dependence(bb);
        }

        if let Some(Terminator::Split { condition, .. }) = cfg.blocks[bb].terminator {
            self.walk_operand(condition);
            if matches!(condition, Operand::Place(_)) {
                self.insert_tmp_assigments();
                self.tmp.clear();
            }
        }
    }

    pub fn walk_control_dependence(&mut self, bb: BasicBlock) {
        if let Some(control_dependence) = self.pdg.control_dependence.row(bb) {
            for bb in control_dependence.iter() {
                self.insert(bb.into())
            }
        }
    }
}

pub trait UseDefVisitor {
    fn visit_bb(&mut self, work_queue: &mut ProgramDependenceWorkQueue, bb: BasicBlock);
    fn visit_assign(&mut self, work_queue: &mut ProgramDependenceWorkQueue, assing: Assigment);
    fn visit_local(&mut self, work_queue: &mut ProgramDependenceWorkQueue, local: Local);
}
