use bitset::SparseBitMatrix;
use cfg::{BasicBlock, ControlFlowGraph, Local, Operand};

use crate::use_def::ProgramDependenceWorkQueue;
use crate::{Assigment, ProgramDependenGraph, UseDefVisitor};

#[derive(Debug, Clone)]
pub struct DefUseGraph {
    assign_local: SparseBitMatrix<Assigment, Local>,
    assign_assign: SparseBitMatrix<Assigment, Assigment>,
    assign_term: SparseBitMatrix<Assigment, BasicBlock>,
    local_local: SparseBitMatrix<Local, Local>,
    local_assign: SparseBitMatrix<Local, Assigment>,
    local_term: SparseBitMatrix<Local, BasicBlock>,
}

impl ProgramDependenGraph<'_> {
    pub fn def_use_graph(&self) -> &DefUseGraph {
        self.def_use_graph.as_ref().unwrap()
    }
}

impl ProgramDependenceWorkQueue<'_, '_> {
    pub fn build_use_def_graph(&mut self, cfg: &ControlFlowGraph) -> DefUseGraph {
        let assign_cnt = self.pdg.interner().assigment_locations.len();
        let mut def_use_graph = DefUseGraph {
            assign_local: SparseBitMatrix::new(assign_cnt, cfg.next_local.into()),
            assign_assign: SparseBitMatrix::new(assign_cnt, assign_cnt),
            assign_term: SparseBitMatrix::new(assign_cnt, cfg.blocks.len()),
            local_local: SparseBitMatrix::new(cfg.next_local.into(), cfg.next_local.into()),
            local_assign: SparseBitMatrix::new(cfg.next_local.into(), assign_cnt),
            local_term: SparseBitMatrix::new(cfg.next_local.into(), cfg.blocks.len()),
        };

        self.run(&mut DefUseBuilder { dst: &mut def_use_graph, cfg });

        def_use_graph
    }
}

pub struct DefUseBuilder<'a> {
    pub dst: &'a mut DefUseGraph,
    pub cfg: &'a ControlFlowGraph,
}

// TODO reduce code duplication
impl UseDefVisitor for DefUseBuilder<'_> {
    fn visit_bb(
        &mut self,
        work_queue: &mut crate::use_def::ProgramDependenceWorkQueue,
        bb: BasicBlock,
    ) {
        work_queue.walk_terminator::<true>(self.cfg, bb);
        self.cfg[bb].terminator().visit_operands(|op| match op {
            Operand::Local(local) => {
                self.dst.local_term.insert(*local, bb);
            }
            Operand::Place(_) => {
                for assign in work_queue.tmp.iter() {
                    self.dst.assign_term.insert(assign, bb);
                }
                work_queue.tmp.clear();
            }
            _ => (),
        });
    }

    fn visit_assign(
        &mut self,
        work_queue: &mut crate::use_def::ProgramDependenceWorkQueue,
        use_: Assigment,
    ) {
        work_queue.walk_assign::<true>(self.cfg, use_);
        let loc = work_queue.pdg.interner().assigment_locations[use_];
        self.cfg[loc.bb].instructions[loc.instr].visit_operands(|op| {
            if let Operand::Local(def) = op {
                self.dst.local_assign.insert(*def, use_);
            }
        });

        for def in work_queue.tmp.iter() {
            self.dst.assign_assign.insert(def, use_);
        }
        work_queue.tmp.clear();
    }

    fn visit_local(
        &mut self,
        work_queue: &mut crate::use_def::ProgramDependenceWorkQueue,
        use_: Local,
    ) {
        work_queue.walk_local::<true>(self.cfg, use_);
        let loc = work_queue.pdg.interner().local_defs[use_];
        match loc.kind {
            cfg::LocationKind::Phi(phi) => {
                for def in self.cfg.blocks[loc.block].phis[phi].sources.values() {
                    self.dst.local_local.insert(*def, use_);
                }
            }
            cfg::LocationKind::Instruction(instr) => {
                self.cfg[loc.block].instructions[instr].visit_operands(|op| {
                    if let Operand::Local(def) = op {
                        self.dst.local_local.insert(*def, use_);
                    }
                });
            }
            cfg::LocationKind::Terminator => unreachable!(),
        }

        for def in work_queue.tmp.iter() {
            self.dst.assign_local.insert(def, use_);
        }
        work_queue.tmp.clear();
    }
}
