use std::mem;

use crate::instructions::{PhiEdges, PhiNode};
use crate::{Block, DataFlowGraph, Inst, InstructionData, Value, GRAVESTONE};

impl DataFlowGraph {
    #[inline]
    pub fn insert_phi_edge(&mut self, inst: Inst, block: Block, val: Value) {
        let PhiNode { mut args, mut blocks } = self.insts.declarations[inst].unwrap_phi();
        blocks.update_or_insert_with(
            block,
            |arg| {
                if let Some(arg) = arg {
                    let use_ = self.insts.operands(inst)[*arg as usize];
                    self.values.detach_use(use_, &self.insts);
                    args.as_mut_slice(&mut self.insts.value_lists)[*arg as usize] = val;
                    self.values.attach_use(use_, val);
                    *arg
                } else {
                    let arg = args.push(val, &mut self.insts.value_lists) as u32;
                    let use_ = self.values.make_use(val, inst, arg as u16);
                    self.insts.uses[inst].push(use_, &mut self.insts.use_lists);
                    arg
                }
            },
            &mut self.phi_forest,
            &(),
        );

        self.insts[inst] = PhiNode { blocks, args }.into();
    }

    #[inline]
    pub fn try_remove_phi_edge_at(&mut self, inst: Inst, block: Block) -> Option<(Value, u32)> {
        if let InstructionData::PhiNode(PhiNode { mut blocks, args }) = self.insts[inst] {
            if let Some(pos) = blocks.remove(block, &mut self.phi_forest, &()) {
                self.detach_operand(inst, pos as u16);
                // this use might be reattched again so we replace the value with a constant where
                // uses currently don't matter that much
                // TODO introduce dedicated gravestone value
                let val = mem::replace(&mut self.instr_args_mut(inst)[pos as usize], GRAVESTONE);
                self.insts.declarations[inst] = PhiNode { blocks, args }.into();
                return Some((val, pos));
            }
        }

        None
    }

    #[inline]
    pub fn try_remove_phi_edge(
        &mut self,
        PhiNode { args, blocks }: &mut PhiNode,
        inst: Inst,
        block: Block,
    ) -> Option<(Value, u32)> {
        if let Some(pos) = blocks.remove(block, &mut self.phi_forest, &()) {
            self.detach_operand(inst, pos as u16);
            // this use might be reattched again so we replace the value with a constant where
            // uses currently don't matter that much
            // TODO introduce dedicated gravestone value
            let val = mem::replace(
                &mut args.as_mut_slice(&mut self.insts.value_lists)[pos as usize],
                GRAVESTONE,
            );
            Some((val, pos))
        } else {
            None
        }
    }

    pub fn phi_edges(&self, phi: PhiNode) -> PhiEdges {
        phi.edges(&self.insts.value_lists, &self.phi_forest)
    }

    pub fn phi_edge_val(&self, phi: PhiNode, pred: Block) -> Option<Value> {
        phi.edge_val(pred, &self.insts.value_lists, &self.phi_forest)
    }

    pub fn phi_eq(&self, phi1: PhiNode, phi2: PhiNode) -> bool {
        phi1.eq(phi2, &self.insts.value_lists, &self.phi_forest)
    }
}
