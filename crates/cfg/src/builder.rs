//! Editing/Constructing a ControlFlowGraph can be cumbersome.
//! Therefore a builder is provided here to make that task more ergnomic.

use crate::{
    BasicBlock, BasicBlockData, ControlFlowGraph, InstrDst, Instruction, Local, Op, Operand, Place,
    Terminator,
};

pub struct CfgBuilder<'a> {
    pub cfg: &'a mut ControlFlowGraph,
    pub current: BasicBlock,
}

impl<'a> CfgBuilder<'a> {
    pub fn new_cfg(cfg: &'a mut ControlFlowGraph) -> CfgBuilder {
        let current = cfg.blocks.push_and_get_key(BasicBlockData::default());
        CfgBuilder { cfg, current }
    }
    pub fn build_phi(&mut self, sources: impl IntoIterator<Item = (BasicBlock, Local)>) -> Local {
        let dst = self.cfg.new_local();
        self.cfg.blocks[self.current]
            .phis
            .push(crate::Phi { dst, sources: sources.into_iter().collect() });
        dst
    }

    pub fn build_val(&mut self, op: Op, operands: Vec<Operand>, src: u32) -> Local {
        let dst = self.cfg.new_local();
        self.cfg.blocks[self.current].instructions.push(crate::Instruction {
            dst: InstrDst::Local(dst),
            op,
            args: operands.into_boxed_slice(),
            src: src as i32,
        });
        dst
    }

    pub fn to_local(&mut self, val: Operand, src: u32) -> Local {
        match val {
            Operand::Local(local) => local,
            _ => self.build_val(Op::Copy, vec![val], src),
        }
    }

    pub fn build_write(&mut self, dst: Place, src: u32, val: Operand) {
        self.cfg.blocks[self.current].instructions.push(crate::Instruction {
            dst: InstrDst::Place(dst),
            op: Op::Copy,
            args: vec![val].into_boxed_slice(),
            src: src as i32,
        });
    }

    pub fn build_assign(&mut self, dst: InstrDst, op: Op, operands: Vec<Operand>, src: u32) {
        self.cfg.blocks[self.current].instructions.push(crate::Instruction {
            dst,
            op,
            args: operands.into_boxed_slice(),
            src: src as i32,
        });
    }

    pub fn build_stmt(&mut self, op: Op, operands: Vec<Operand>, src: u32) {
        self.cfg.blocks[self.current].instructions.push(crate::Instruction {
            dst: InstrDst::Ignore,
            op,
            args: operands.into_boxed_slice(),
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
}
