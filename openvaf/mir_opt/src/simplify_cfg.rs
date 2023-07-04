use std::hash::{Hash, Hasher};
use std::iter::repeat;

use bitset::BitSet;
use mir::{Block, ControlFlowGraph, Function, InstructionData, Value, ValueDef, FALSE, TRUE};

#[cfg(test)]
mod tests;

pub fn simplify_cfg(func: &mut Function, cfg: &mut ControlFlowGraph) {
    let mut simplify = SimplifyCfg {
        cfg,
        merge_phis: true,
        vals_changed: BitSet::new_filled(func.layout.num_blocks()),
        func,
        local_changed: false,
        // table: RawTable::with_capacity(8),
        // hash_builer: ahash::RandomState::new(),
        // unconditional_preds: Vec::with_capacity(4),
    };
    simplify.iteratively_simplify_cfg();
}

pub fn simplify_cfg_no_phi_merge(func: &mut Function, cfg: &mut ControlFlowGraph) {
    let mut simplify = SimplifyCfg {
        cfg,
        merge_phis: false,
        vals_changed: BitSet::new_filled(func.layout.num_blocks()),
        func,
        local_changed: false,
        // table: RawTable::with_capacity(8),
        // hash_builer: ahash::RandomState::new(),
        // unconditional_preds: Vec::with_capacity(4),
    };
    simplify.iteratively_simplify_cfg();
}

struct SimplifyCfg<'a> {
    cfg: &'a mut ControlFlowGraph,
    func: &'a mut Function,
    merge_phis: bool,
    local_changed: bool,
    // table: RawTable<Inst>,
    // hash_builer: ahash::RandomState,
    vals_changed: BitSet<Block>,
    // unconditional_preds: Vec<(Block, InstCursor)>,
}

impl<'a> SimplifyCfg<'a> {
    /// Call SimplifyCFG on all the blocks in the function,
    fn iteratively_simplify_cfg(&mut self) -> bool {
        let mut changed = false;

        loop {
            self.local_changed = false;
            let mut cursor = self.func.layout.blocks_cursor();
            // Loop over all of the basic blocks and remove them if they are unneeded.
            while let Some(bb) = cursor.next {
                self.simplify_bb(bb);
                // only advance after simplification to avoid visiting dead blocks
                cursor.next(&self.func.layout);
            }
            if !self.local_changed {
                break;
            }
            changed = true
        }

        changed
    }

    fn const_fold_terminator(&mut self, bb: Block) {
        if let Some(inst) = self.func.layout.last_inst(bb) {
            if let InstructionData::Branch { cond, then_dst, else_dst, .. } =
                self.func.dfg.insts[inst]
            {
                if then_dst == else_dst {
                    self.local_changed = true;
                    self.func.dfg.zap_inst(inst);
                    self.func.dfg.insts[inst] = InstructionData::Jump { destination: then_dst };
                    // self.cfg.recompute_block(&self.func, bb);
                } else {
                    let (destination, dead_dst) = match cond {
                        FALSE => (else_dst, then_dst),
                        TRUE => (then_dst, else_dst),
                        _ => return,
                    };

                    if let Some(bb) = self.func.layout.inst_block(inst) {
                        self.func.dfg.detach_operand(inst, 0);
                        self.func.dfg.insts[inst] = InstructionData::Jump { destination };
                        self.cfg.recompute_block(self.func, bb);
                        self.remove_phi_edges(dead_dst, bb);
                        self.vals_changed.insert(dead_dst);
                    }
                }

                //self.func.dfg.value_def(v)
            }
        }
    }

    // TODO does this actually belong here?... Probably not..
    fn simplify_trivial_phis(&mut self, bb: Block) {
        if Some(bb) == self.func.layout.entry_block() {
            return;
        }

        let mut cursor = self.func.layout.block_inst_cursor(bb);
        while let Some(inst) = cursor.next(&self.func.layout) {
            if let InstructionData::PhiNode(phi) = self.func.dfg.insts[inst].clone() {
                let mut edges = self.func.dfg.phi_edges(&phi);
                let phi_val = self.func.dfg.first_result(inst);
                if let Some((_, first_val)) = edges.find(|it| it.1 != phi_val) {
                    if edges.all(|(_, val)| val == first_val || val == phi_val) {
                        for use_ in self.func.dfg.uses(phi_val) {
                            let inst = self.func.dfg.use_to_operand(use_).0;
                            if let Some(inst) = self.func.layout.inst_block(inst) {
                                self.vals_changed.insert(inst);
                            }
                        }
                        self.func.dfg.replace_uses(phi_val, first_val);
                    } else {
                        continue;
                    }
                } else {
                    debug_assert_eq!(self.cfg.pred_iter(bb).count(), 0);
                    continue;
                }

                self.local_changed = true;
                self.func.dfg.zap_inst(inst);
                self.func.layout.remove_inst(inst);
            } else {
                break;
            }
        }
    }

    fn simplify_duplicates_phis_naive(&mut self, bb: Block) {
        // This is worstcase O(n_phis^2) which is faster for smaller phis
        let mut cursor = self.func.layout.block_inst_cursor(bb);
        while let Some(inst) = cursor.head {
            if let InstructionData::PhiNode(phi1) = self.func.dfg.insts[inst].clone() {
                let val = self.func.dfg.first_result(inst);
                let mut cursor2 = cursor;
                cursor2.next(&self.func.layout);
                while let Some(inst2) = cursor2.next(&self.func.layout) {
                    // cursor2.next(&self.func.layout);
                    if let InstructionData::PhiNode(phi2) = &self.func.dfg.insts[inst2] {
                        if self.func.dfg.phi_eq(&phi1, phi2) {
                            let duplicate_val = self.func.dfg.first_result(inst2);
                            for use_ in self.func.dfg.uses(duplicate_val) {
                                let inst = self.func.dfg.use_to_operand(use_).0;
                                if let Some(inst) = self.func.layout.inst_block(inst) {
                                    self.vals_changed.insert(inst);
                                }
                            }
                            self.func.dfg.zap_inst(inst2);
                            self.func.dfg.replace_uses(duplicate_val, val);
                            self.func.layout.remove_inst(inst2);
                            self.local_changed = true;
                        }
                    } else {
                        break;
                    }
                }
            } else {
                break;
            }

            cursor.next(&self.func.layout);
        }
    }

    // fn simplify_duplicates_phis_large(&mut self, bb: Block) {
    //     let mut cursor = self.func.layout.block_inst_cursor(bb);

    //     // TODO benchmark reserve on each insert instead
    //     let len = self.func.layout.block_insts(bb).count();
    //     self.table.reserve(len, |_| unreachable!());

    //     while let Some(inst) = cursor.next(&self.func.layout) {
    //         if let InstructionData::PhiNode(phi) = self.func.dfg.insts[inst] {
    //             let mut hasher = self.hash_builer.build_hasher();
    //             self.func.dfg.phi_edges(phi).for_each(|(_, val)| val.hash(&mut hasher));
    //             let hash = hasher.finish();

    //             let eq_phi = self.table.get(hash, |inst| {
    //                 let other = self.func.dfg.insts[*inst].unwrap_phi();
    //                 let edge1 = self.func.dfg.phi_edges(other).map(|(_, val)| val);
    //                 let edge2 = self.func.dfg.phi_edges(other).map(|(_, val)| val);
    //                 edge1.eq(edge2)
    //             });

    //             if let Some(eq_phi) = eq_phi {
    //                 let val = self.func.dfg.first_result(inst);
    //                 let eq_val = self.func.dfg.first_result(*eq_phi);
    //                 for use_ in self.func.dfg.uses(val) {
    //                     let inst = self.func.dfg.use_to_operand(use_).0;
    //                     if let Some(inst) = self.func.layout.inst_block(inst) {
    //                         self.vals_changed.insert(inst);
    //                     }
    //                 }
    //                 self.func.dfg.zap_inst(inst);
    //                 self.func.dfg.replace_uses(val, eq_val);
    //                 self.func.layout.remove_inst(inst);
    //                 self.local_changed = true;
    //             } else {
    //                 unsafe { self.table.insert_no_grow(hash, inst) };
    //             }
    //         }
    //     }

    //     self.table.clear_no_drop();
    // }

    fn simplify_duplicates_phis(&mut self, bb: Block) {
        // // TODO benchmark: is this worth it? Is 32 the right number
        // if self
        //     .func
        //     .layout
        //     .block_insts(bb)
        //     .enumerate()
        //     .filter(|(_, inst)| matches!(self.func.dfg.insts[*inst], InstructionData::PhiNode(..)))
        //     .all(|(i, _)| i < 32)
        // {
        self.simplify_duplicates_phis_naive(bb)
        // } else {
        // self.simplify_duplicates_phis_large(bb)
        // }
    }

    fn merge_block_into_predecessor(&mut self, bb: Block) -> bool {
        let pred =
            if let Some(pred) = self.cfg.single_predecessor(bb) { pred } else { return false };

        if self.cfg.self_loop(bb)
            || self.cfg.unique_succ(pred).is_none()
            || self
                .func
                .layout
                .block_insts(bb)
                .next()
                .map_or(false, |inst| self.func.dfg.insts[inst].is_phi())
        {
            return false;
        }

        // in case the terminator is an unoptimized br _, bb, bb
        if let Some(terminator) = self.func.layout.last_inst(pred) {
            debug_assert!(self.func.dfg.insts[terminator].is_terminator());
            if let InstructionData::Branch { then_dst, else_dst, .. } =
                self.func.dfg.insts[terminator]
            {
                debug_assert_eq!(then_dst, else_dst);
                self.func.dfg.zap_inst(terminator);
            }
        }

        // update phis in successors
        for succ in self.cfg.succ_iter(bb) {
            self.vals_changed.insert(succ);
            self.func.update_phi_edges(succ, bb, pred);
        }

        self.func.layout.merge_blocks(pred, bb);
        self.local_changed = true;

        // update sucessors/predecessors
        self.cfg.recompute_block(self.func, pred);
        self.cfg.recompute_block(self.func, bb);

        true
    }

    fn remove_phi_edges(&mut self, bb: Block, dead_pred: Block) {
        for inst in self.func.layout.block_insts(bb) {
            if self.func.dfg.try_remove_phi_edge_at(inst, dead_pred).is_none() {
                break;
            }
        }
    }

    // TODO porperly implement this... its a bit tricky and not high prio right now
    //    fn sink_common_code_from_predecessors(&mut self, bb: Block) -> bool {
    //        // We support two situations:
    //        //   (1) all incoming arcs are unconditional
    //        //   (2) there are non-unconditional incoming arcs
    //        //
    //        // (2) is very common in switch defaults and
    //        // else-if patterns;
    //        //
    //        //   if (a) f(1);
    //        //   else if (b) f(2);
    //        //
    //        // produces:
    //        //
    //        //       [if]
    //        //      /    \
    //        //    [f(1)] [if]
    //        //      |     | \
    //        //      |     |  |
    //        //      |  [f(2)]|
    //        //       \    | /
    //        //        [ end ]
    //        //
    //        // [end] has two unconditional predecessor arcs and one conditional. The
    //        // conditional refers to the implicit empty 'else' arc. This conditional
    //        // arc can also be caused by an empty default block in a switch.
    //        //
    //        // In this case, we attempt to sink code from all *unconditional* arcs.
    //        // If we can sink instructions from these arcs (determined during the scan
    //        // phase below) we insert a common successor for all unconditional arcs and
    //        // connect that to [end], to enable sinking:
    //        //
    //        //       [if]
    //        //      /    \
    //        //    [x(1)] [if]
    //        //      |     | \
    //        //      |     |  \
    //        //      |  [x(2)] |
    //        //       \   /    |
    //        //   [sink.split] |
    //        //         \     /
    //        //         [ end ]
    //        //

    //        let unconditional_preds = &mut self.unconditional_preds;
    //        let mut has_unconditional_pred = false;
    //        unconditional_preds.extend(self.cfg.pred_iter(bb).filter_map(|pred| {
    //            if self.cfg.succ_iter(pred).count() > 1 {
    //                has_unconditional_pred = true;
    //                None
    //            } else {
    //                let mut cursor = self.func.layout.block_inst_cursor(pred);
    //                cursor.next_back(&self.func.layout);
    //                Some((pred, cursor))
    //            }
    //        }));

    //        let res = match unconditional_preds.as_mut_slice() {
    //            [] | [_] => false,
    //            [first_uncoditional, unconditional_preds @ ..] => {
    //                let mut dst = None;
    //                let mut changed = false;

    //                // in the future we may create phis for mismatched operands however this becomes a cost
    //                // tradeoff that could actually impead some optimizations (aka const prop)
    //                while let Some(inst) = first_uncoditional.1.next_back(&self.func.layout) {
    //                    let inst_data = self.func.dfg.insts[inst];
    //                    let all_eq = unconditional_preds.iter().all(|(_, cursor)| {
    //                        cursor.tail.map_or(false, |it| {
    //                            self.func.dfg.insts[it].eq(
    //                                &inst_data,
    //                                &self.func.dfg.insts.value_lists,
    //                                &self.func.dfg.phi_forest,
    //                            )
    //                        })
    //                    });

    //                    if !all_eq {
    //                        break;
    //                    }

    //                    changed = true;

    //                    for (_, cursor) in unconditional_preds.iter_mut() {
    //                        let merged_inst = cursor.next_back(&self.func.layout).unwrap();
    //                        for i in 0..self.func.dfg.inst_results(inst).len() {
    //                            let obsolete = self.func.dfg.inst_results(merged_inst)[i];
    //                            let val = self.func.dfg.inst_results(inst)[i];
    //                            self.func.dfg.replace_uses(obsolete, val);
    //                        }
    //                        self.func.dfg.zap_inst(merged_inst);
    //                        self.func.layout.remove_inst(merged_inst);
    //                    }

    //                    self.func.layout.remove_inst(inst);

    //                    let (dst_block, dst) = match dst {
    //                        Some((block, inst)) => (block, inst),
    //                        None => {
    //                            let (block, inst) = if has_unconditional_pred {
    //                                let tmp_block = self.func.layout.append_new_block();
    //                                for (pred, _) in &*unconditional_preds {
    //                                    let term = self.func.layout.last_inst(*pred).unwrap();
    //                                    debug_assert!(matches!(
    //                                        self.func.dfg.insts[term],
    //                                        InstructionData::Jump { destination } if destination == bb
    //                                    ));
    //                                    self.func.dfg.insts[term] =
    //                                        InstructionData::Jump { destination: tmp_block };
    //                                }

    //                                let tmp_term = self
    //                                    .func
    //                                    .dfg
    //                                    .make_inst(InstructionData::Jump { destination: bb });
    //                                self.func.layout.append_inst(tmp_term, tmp_block);
    //                                (tmp_block, Some(tmp_term))
    //                            } else {
    //                                (bb, self.func.layout.last_inst(bb))
    //                            };
    //                            dst = Some((block, inst));
    //                            (block, inst)
    //                        }
    //                    };

    //                    if let Some(dst) = dst {
    //                        self.func.layout.insert_inst(inst, dst);
    //                    } else {
    //                        self.func.layout.append_inst(inst, dst_block);
    //                    }
    //                }

    //                changed
    //            }
    //        };
    //        unconditional_preds.clear();
    //        res
    //    }

    /// If a block only contains phis and a unconditional jump the used phis can be merged with
    /// their
    fn simplify_unconditional_jmp_term(&mut self, src: Block, dst: Block) {
        if self.cfg.single_predecessor(dst).is_some() {
            // trivial case let `merge_block_into_predecessor` handle this
            return;
        }

        // check that the block only contains phi and a terminator
        let mut insts = self.func.layout.block_insts(src);
        insts.next_back();
        for inst in insts.clone() {
            if !matches!(self.func.dfg.insts[inst], InstructionData::PhiNode(_)) {
                return;
            }

            for use_ in self.func.dfg.inst_uses(inst) {
                let inst = self.func.dfg.use_to_operand(use_).0;
                // check that all uses are phi nodes (otherwise we produce invalid code in loops
                // where we dominate a block with multiple predecessor
                if self.func.layout.inst_block(inst).is_none() {
                    unreachable!(
                        "found use in detachted inst {}",
                        self.func.dfg.display_inst(inst)
                    );
                }
                if !matches!(self.func.dfg.insts[inst], InstructionData::PhiNode(_))
                    || self.func.layout.inst_block(inst).unwrap() != dst
                {
                    return;
                }
            }
        }

        // check that the successors phis can be merged
        for inst in self.func.layout.block_insts(dst) {
            if let InstructionData::PhiNode(phi) = self.func.dfg.insts[inst].clone() {
                // prepare for update
                let val = self.func.dfg.phi_edge_val(&phi, src).unwrap();

                if let ValueDef::Result(src_inst, _) = self.func.dfg.value_def(val) {
                    if let InstructionData::PhiNode(src_phi) = self.func.dfg.insts[src_inst].clone()
                    {
                        if self.func.layout.inst_block(src_inst) == Some(src) {
                            // the value depends on the predecessor (its a phi)

                            let can_merge = self.func.dfg.phi_edges(&src_phi).all(|(bb, val)| {
                                self.func.dfg.phi_edge_val(&phi, bb).map_or(true, |it| it == val)
                            });
                            if can_merge {
                                continue;
                            } else {
                                return;
                            }
                        }
                    }
                }

                // value is the same for all predecessors
                let can_merge = self
                    .cfg
                    .pred_iter(src)
                    .all(|bb| self.func.dfg.phi_edge_val(&phi, bb).map_or(true, |it| it == val));

                if !can_merge {
                    return;
                }
            } else {
                break;
            }
        }

        // actually merge the phis
        for inst in self.func.layout.block_insts(dst) {
            if let InstructionData::PhiNode(mut phi) = self.func.dfg.insts[inst].clone() {
                // prepare for update
                self.func.dfg.zap_inst(inst);
                let (old_val, _) = self.func.dfg.try_remove_phi_edge(&mut phi, inst, src).unwrap();

                if let ValueDef::Result(src_inst, _) = self.func.dfg.value_def(old_val) {
                    if let InstructionData::PhiNode(src_phi) = self.func.dfg.insts[src_inst].clone()
                    {
                        if self.func.layout.inst_block(src_inst) == Some(src) {
                            // merge phis...

                            phi.blocks.merge(
                                src_phi.blocks,
                                &mut self.func.dfg.phi_forest,
                                &(),
                                |old, new| {
                                    if let Some(old) = old {
                                        old
                                    } else {
                                        let val =
                                            src_phi.args.as_slice(&self.func.dfg.insts.value_lists)
                                                [new as usize];
                                        phi.args.push(val, &mut self.func.dfg.insts.value_lists)
                                            as u32
                                    }
                                },
                            );

                            self.func.dfg.insts[inst] = phi.into();
                            self.func.dfg.update_inst_uses(inst);
                            continue;
                        }
                    }
                }

                phi.blocks.insert_sorted_iter(
                    self.cfg.pred_iter(src).zip(repeat(())),
                    &mut self.func.dfg.phi_forest,
                    &(),
                    |old, _| {
                        if let Some(old) = old {
                            old
                        } else {
                            phi.args.push(old_val, &mut self.func.dfg.insts.value_lists) as u32
                        }
                    },
                );

                self.func.dfg.insts[inst] = phi.into();
                self.func.dfg.update_inst_uses(inst);
            } else {
                break;
            }
        }

        // zap phis in old block
        for inst in insts {
            self.func.dfg.zap_inst(inst)
        }

        for pred in self.cfg.pred_iter(src) {
            let term = self.func.layout.last_inst(pred).unwrap();
            match &mut self.func.dfg.insts[term] {
                InstructionData::Branch { then_dst: ref mut destination, .. }
                | InstructionData::Branch { else_dst: ref mut destination, .. }
                    if *destination == src =>
                {
                    *destination = dst;
                }
                InstructionData::Jump { ref mut destination } => {
                    debug_assert_eq!(*destination, src);
                    *destination = dst;
                }
                _ => unreachable!(),
            }
        }

        self.vals_changed.insert(dst);

        self.cfg.replace(src, dst);
        self.func.layout.remove_and_clear_block(src);

        // self.cfg.recompute_block(self.func, src);
        self.local_changed = true;
    }

    fn simplify_bb(&mut self, bb: Block) {
        // Remove basic blocks that have no predecessors (except the entry block)...
        // or that just have themself as a predecessor.  These are unreachable.
        if (self.cfg[bb].predecessors.is_empty() || self.cfg.self_loop(bb))
            && Some(bb) != self.func.layout.entry_block()
        {
            // remove phi phi_edges
            for succ in self.cfg.succ_iter(bb) {
                if succ != bb {
                    self.remove_phi_edges(succ, bb);
                }
            }
            // zap just to be sure
            for inst in self.func.layout.block_insts(bb) {
                self.func.dfg.zap_inst(inst)
            }

            self.func.layout.remove_and_clear_block(bb);
            self.local_changed = true;
            self.cfg.recompute_block(self.func, bb);
            return;
        }

        self.const_fold_terminator(bb);
        if self.vals_changed.remove(bb) {
            self.simplify_trivial_phis(bb);
            self.simplify_duplicates_phis(bb);
        }

        // Merge basic blocks into their predecessor if there is only one distinct
        // pred, and if there is only one distinct successor of the predecessor, and
        // if there are no PHI nodes.
        if self.merge_block_into_predecessor(bb) {
            // nothing to do for this blog anymore its removed
            return;
        }

        // if self.sink_common_code_from_predecessors(bb) {
        //     self.local_changed = true;
        //     return;
        // }

        if self.merge_phis {
            if let Some(term) = self.func.layout.last_inst(bb) {
                if let InstructionData::Jump { destination } = self.func.dfg.insts[term] {
                    self.simplify_unconditional_jmp_term(bb, destination)
                }

                // TODO merge common code in successor (for branch)
            }
        }
    }
}

#[derive(Clone)]
struct ResolvedPhi<I> {
    vals: I,
}
impl<I: Iterator<Item = Value> + Clone> Hash for ResolvedPhi<I> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for val in self.vals.clone() {
            val.hash(state)
        }
    }
}
impl<I: Iterator<Item = Value> + Clone> PartialEq for ResolvedPhi<I> {
    fn eq(&self, other: &Self) -> bool {
        self.vals.clone().eq(other.vals.clone())
    }
}
impl<I: Iterator<Item = Value> + Clone> Eq for ResolvedPhi<I> {}
