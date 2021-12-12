use bitset::BitSet;
use cfg::{BasicBlock, BasicBlockData, ControlFlowGraph, Instruction, Op, Terminator};
use typed_index_collections::TiVec;

pub fn simplify_cfg(cfg: &mut ControlFlowGraph) {
    strip_nops(cfg);
    let mut simplify = SimplifyTerminators::new(cfg);
    simplify.run();
    remove_dead_blocks(cfg);
    cfg.predecessor_cache.invalidate();
    cfg.is_cyclic.invalidate();
}

struct SimplifyTerminators<'a> {
    cfg: &'a mut ControlFlowGraph,
    pred_count: TiVec<BasicBlock, u16>,
}

impl<'a> SimplifyTerminators<'a> {
    fn new(cfg: &'a mut ControlFlowGraph) -> Self {
        let mut pred_count: TiVec<_, _> = vec![0u16; cfg.blocks.len()].into();
        pred_count[cfg.entry()] = 1u16;
        for (_, data) in cfg.postorder_iter() {
            if let Some(ref term) = data.terminator {
                for succ in term.successors() {
                    pred_count[succ] += 1;
                }
            }
        }

        Self { cfg, pred_count }
    }

    /// Collapse a goto chain starting from `start`
    fn collapse_goto_chain(&mut self, start: &mut BasicBlock, changed: &mut bool) {
        // let tspan = trace_span!("try collapse goto chain", start = start.index());
        // let _enter = tspan.enter();
        let mut chain = Vec::with_capacity(8);
        let mut bb = *start;
        loop {
            match self.cfg[bb] {
                BasicBlockData {
                    ref phis,
                    ref instructions,
                    terminator: Some(Terminator::Goto(dst)),
                } if instructions.is_empty() && phis.is_empty() => {
                    chain.push((bb, dst));
                    bb = dst;
                }

                _ => break,
            }
        }

        let last = bb;
        let chain_end = chain.last().map(|(bb, _)| *bb);

        for (bb, dst) in chain.into_iter().rev() {
            *changed |= dst != last;

            if self.pred_count[bb] == 1 {
                // This is the last reference to bb, so the pred-count to
                // to target is moved into the bb block.
                self.pred_count[bb] = 0;
            } else {
                self.pred_count[last] += 1;
                self.pred_count[bb] -= 1;
            }
            *self.cfg[bb].terminator_mut() = Terminator::Goto(last);
        }

        if let Some(chain_end) = chain_end {
            for phi in &mut self.cfg[last].phis {
                let local = phi.sources.remove(&chain_end).unwrap();
                phi.sources.insert(*start, local);
            }
        }
        *start = last;
    }

    fn collapse_phis(&mut self, bb: BasicBlock, predesssor: BasicBlock) {
        let block = &mut self.cfg[bb];
        let instructions = block.phis.iter().map(|phi| {
            let src = *phi.sources.get(&predesssor).unwrap();
            Instruction {
                dst: phi.dst.into(),
                op: Op::Copy,
                args: vec![src.into()].into_boxed_slice(),
                src: 0,
            }
        });
        block.instructions.raw.splice(0..0, instructions);
    }

    fn run(&mut self) {
        let entry = self.cfg.entry();
        let mut start = entry;

        // Vec of the blocks that should be merged. We store the indices here, instead of the
        // statements itself to avoid moving the (relatively) large statements twice.
        // We do not push the statements directly into the target block (`bb`) as that is slower
        // due to additional reallocations
        let mut merged_blocks = Vec::new();
        loop {
            let mut changed = false;

            self.collapse_goto_chain(&mut start, &mut changed);

            for bb in self.cfg.blocks.keys() {
                if self.pred_count[bb] == 0 {
                    continue;
                }

                let mut terminator =
                    self.cfg[bb].terminator.take().expect("invalid terminator state");

                for successor in terminator.successors_mut() {
                    self.collapse_goto_chain(successor, &mut changed);
                }

                merged_blocks.clear();

                let mut current = bb;

                loop {
                    let dst = match terminator {
                        Terminator::Split { true_block, false_block, .. }
                            if true_block == false_block =>
                        {
                            changed = true;

                            if self.pred_count[true_block] == 2 {
                                self.pred_count[true_block] = 0;
                                true_block
                            } else {
                                terminator = Terminator::Goto(true_block);
                                break;
                            }
                        }

                        Terminator::Goto(dst) if self.pred_count[dst] == 1 => {
                            changed = true;

                            self.pred_count[dst] = 0;
                            dst
                        }

                        // TODO difference between infinite loop and dead code result (not that important because the former is just bullshit we should just shoot that down in some pass beforehand)
                        Terminator::Split { true_block, false_block, loop_head: true, .. }
                            if true_block == current
                                && self.cfg[current].instructions.is_empty()
                                && self.cfg[current].phis.is_empty() =>
                        {
                            changed = true;
                            if self.pred_count[false_block] == 1 {
                                self.pred_count[false_block] = 0;
                                false_block
                            } else {
                                terminator = Terminator::Goto(false_block);
                                break;
                            }
                        }

                        _ => break,
                    };
                    terminator = self.cfg[dst].terminator.take().expect("invalid terminator state");
                    self.collapse_phis(dst, current);
                    merged_blocks.push(dst);
                    current = dst
                }

                let instructions_to_merge =
                    merged_blocks.iter().map(|&i| self.cfg.blocks[i].instructions.len()).sum();

                if instructions_to_merge > 0 {
                    let mut statements = std::mem::take(&mut self.cfg[bb].instructions);
                    statements.reserve(instructions_to_merge);
                    for &from in &merged_blocks {
                        statements.append(&mut self.cfg[from].instructions);
                    }

                    self.cfg[bb].instructions = statements;
                }

                self.cfg[bb].terminator = Some(terminator);
            }

            if !changed {
                break;
            }
        }

        if start != entry {
            debug_assert!(self.pred_count[entry] == 0);
            self.cfg.blocks.swap(entry, start);
            self.pred_count.swap(entry, start);

            // pred_count == 1 if the start block has no predecessor _blocks_.
            if self.pred_count[entry] > 1 {
                for (bb, data) in self.cfg.blocks.iter_mut_enumerated() {
                    if self.pred_count[bb] == 0 {
                        continue;
                    }

                    data.terminator_mut().visit_bb_mut(|target| {
                        if *target == start {
                            *target = entry
                        }
                    })
                }
            }
        }
    }
}

pub fn strip_nops(cfg: &mut ControlFlowGraph) {
    for block in &mut cfg.blocks {
        block.instructions.retain(|instr| !matches!(instr.op, Op::NoOp))
    }
}

pub fn remove_dead_blocks(cfg: &mut ControlFlowGraph) {
    let alive_blocks: BitSet<_> = {
        let mut seen = BitSet::new_empty(cfg.blocks.len());
        seen.extend(cfg.postorder_iter().map(|(id, _)| id));
        seen
    };

    let mut replacements: TiVec<BasicBlock, BasicBlock> = cfg.blocks.keys().collect();

    let mut next_idx = 0u32;
    for current_idx in alive_blocks.iter() {
        replacements[current_idx] = next_idx.into();
        if current_idx != next_idx.into() {
            // Swap the next alive block data with the current available slot. Since
            // alive_index is non-decreasing this is a valid operation.
            cfg.blocks.swap(current_idx, next_idx.into());
        }
        next_idx += 1;
    }

    cfg.blocks.truncate(next_idx as usize);

    for data in cfg.blocks.iter_mut() {
        for phi in data.phis.iter_mut() {
            phi.sources =
                phi.sources.iter().map(|(block, local)| (replacements[*block], *local)).collect();
        }
        data.terminator_mut().visit_bb_mut(|succ| *succ = replacements[*succ])
    }
}
