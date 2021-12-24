//! Editing/Constructing a ControlFlowGraph can be cumbersome.
//! Therefore a builder is provided here to make that task more ergnomic.

use ahash::AHashMap;
use smallvec::smallvec;

use crate::{
    BasicBlock, BasicBlockData, ControlFlowGraph, InstrDst, Instruction, Local, Op, Operand,
    Operands, Place, Terminator,
};

pub struct CfgBuilder<'a> {
    pub cfg: &'a mut ControlFlowGraph,
    pub current: BasicBlock,
}

impl<'a> CfgBuilder<'a> {
    pub fn new(cfg: &'a mut ControlFlowGraph) -> CfgBuilder {
        let current = cfg.blocks.push_and_get_key(BasicBlockData::default());
        CfgBuilder { cfg, current }
    }

    pub fn build_phi(&mut self, sources: impl IntoIterator<Item = (BasicBlock, Local)>) -> Local {
        let dst = self.cfg.new_local();
        self.add_phi(dst, sources.into_iter().collect());
        dst
    }

    pub fn build_select(
        &mut self,
        cond: Operand,
        lower_branch: impl FnMut(&mut Self, bool) -> Local,
    ) -> Local {
        let (then_src, else_src) = self.build_cond(cond, lower_branch);

        self.build_phi([then_src, else_src])
    }

    pub fn build_cond<T>(
        &mut self,
        cond: Operand,
        mut lower_branch: impl FnMut(&mut Self, bool) -> T,
    ) -> ((BasicBlock, T), (BasicBlock, T)) {
        let start = self.current;

        let then_head_bb = self.enter_new_block();
        let then_val = lower_branch(self, true);
        let then_tail_bb = self.current;

        let else_head_bb = self.enter_new_block();
        let else_val = lower_branch(self, false);
        let else_tail_bb = self.current;

        let end = self.enter_new_block();
        self.terminate_bb(else_tail_bb, Terminator::Goto(end));
        self.terminate_bb(then_tail_bb, Terminator::Goto(end));

        self.terminate_bb(
            start,
            Terminator::Split {
                condition: cond,
                true_block: then_head_bb,
                false_block: else_head_bb,
                loop_head: false,
            },
        );

        ((then_tail_bb, then_val), (else_tail_bb, else_val))
    }

    pub fn add_phi(&mut self, dst: Local, sources: AHashMap<BasicBlock, Local>) -> Local {
        self.cfg.blocks[self.current]
            .phis
            .push(crate::Phi { dst, sources: sources.into_iter().collect() });
        dst
    }

    pub fn build_val(&mut self, op: Op, operands: Operands, src: i32) -> Local {
        let dst = self.cfg.new_local();
        self.cfg.blocks[self.current].instructions.push(crate::Instruction {
            dst: InstrDst::Local(dst),
            op,
            args: operands,
            src,
        });
        dst
    }

    pub fn to_local(&mut self, val: Operand, src: i32) -> Local {
        match val {
            Operand::Local(local) => local,
            _ => self.build_val(Op::Copy, smallvec![val], src),
        }
    }

    pub fn build_write(&mut self, dst: Place, src: i32, val: Operand) {
        self.cfg.blocks[self.current].instructions.push(crate::Instruction {
            dst: InstrDst::Place(dst),
            op: Op::Copy,
            args: smallvec![val],
            src: src as i32,
        });
    }

    pub fn build_assign(&mut self, dst: InstrDst, op: Op, operands: Operands, src: i32) {
        self.cfg.blocks[self.current].instructions.push(crate::Instruction {
            dst,
            op,
            args: operands,
            src: src as i32,
        });
    }

    pub fn build_stmt(&mut self, op: Op, operands: Operands, src: i32) {
        self.cfg.blocks[self.current].instructions.push(crate::Instruction {
            dst: InstrDst::Ignore,
            op,
            args: operands,
            src: src as i32,
        });
    }

    pub fn add_instr(&mut self, instr: Instruction) {
        self.cfg.blocks[self.current].instructions.push(instr);
    }

    pub fn terminate_bb(&mut self, block: BasicBlock, term: Terminator) {
        debug_assert!(
            self.cfg[block].terminator.is_none(),
            "terminate: block {:?} already has a terminator set: {:#?}",
            block,
            self.cfg[block]
        );

        self.cfg[block].terminator = Some(term);
    }

    pub fn terminate(&mut self, term: Terminator) {
        self.terminate_bb(self.current, term)
    }

    pub fn create_block(&mut self) -> BasicBlock {
        self.cfg.blocks.push_and_get_key(BasicBlockData::default())
    }

    pub fn enter_new_block(&mut self) -> BasicBlock {
        let bb = self.cfg.blocks.push_and_get_key(BasicBlockData::default());
        self.current = bb;
        bb
    }

    pub fn current_block_mut(&mut self) -> &mut BasicBlockData {
        &mut self.cfg[self.current]
    }

    pub fn current_block(&self) -> &BasicBlockData {
        &self.cfg[self.current]
    }

    pub fn prepend_entry(&mut self, entry: BasicBlock) {
        self.terminate(Terminator::Goto(self.cfg.entry()));
        self.cfg.entry = entry;
    }
}
