use cfg::{ControlFlowGraph, Instruction};

use crate::use_def::ProgramDependenceWorkQueue;

impl ProgramDependenceWorkQueue<'_, '_> {
    pub fn remove_unvisited_from_cfg(&self, cfg: &mut ControlFlowGraph) {
        for (assign, loc) in self.pdg.interner().assigment_locations.iter_enumerated() {
            if !self.visited_assigns.contains(assign) {
                cfg.blocks[loc.bb].instructions[loc.instr] = Instruction::default();
            }
        }

        for (local, loc) in self.pdg.interner().local_defs.iter_enumerated() {
            if !self.visited_locals.contains(local) {
                match loc.kind {
                    cfg::LocationKind::Phi(phi) => cfg[loc.block].phis[phi].sources.clear(),
                    cfg::LocationKind::Instruction(instr) => {
                        cfg.blocks[loc.block].instructions[instr] = Instruction::default()
                    }
                    cfg::LocationKind::Terminator => (), // UNDEF local is dead... good
                }
            }
        }
    }
}
