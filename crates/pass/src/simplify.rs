/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use data_structures::bit_set::BitSet;
use data_structures::index_vec::{index_vec, IndexVec};
use middle::cfg::{
    BasicBlock, BasicBlockData, ControlFlowGraph, ModificationPass, Terminator, TerminatorKind,
    START_BLOCK,
};
use middle::{impl_pass_span, CfgFunctions, Operand, OperandData, RValue, StmntKind};
use session::sourcemap::span::DUMMY_SP;
use tracing::{debug, trace_span};

pub struct Simplify;

impl<C: CfgFunctions> ModificationPass<'_, C> for Simplify {
    type Result = ();

    fn run(self, cfg: &mut ControlFlowGraph<C>) {
        strip_nops(cfg);
        let mut simplify = SimplifyTerminators::new(cfg);
        simplify.run();
        remove_dead_blocks(cfg);
        cfg.predecessor_cache.invalidate();
        cfg.is_cyclic.invalidate();
    }

    impl_pass_span!("simplify");
}

struct SimplifyTerminators<'a, C: CfgFunctions> {
    cfg: &'a mut ControlFlowGraph<C>,
    pred_count: IndexVec<BasicBlock, u16>,
}

impl<'a, C: CfgFunctions> SimplifyTerminators<'a, C> {
    fn new(cfg: &'a mut ControlFlowGraph<C>) -> Self {
        let mut pred_count = index_vec![0; cfg.blocks.len()];
        pred_count[START_BLOCK] = 1u16;
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
        let tspan = trace_span!("try collapse goto chain", start = start.index());
        let _enter = tspan.enter();
        let mut chain = Vec::with_capacity(8);
        let mut bb = *start;
        loop {
            match self.cfg[bb] {
                BasicBlockData {
                    ref phi_statements,
                    ref statements,
                    terminator: Some(Terminator { kind: TerminatorKind::Goto(dst), .. }),
                } if statements.is_empty() && phi_statements.is_empty() => {
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
            debug!(
                collapsed_block = bb.index(),
                chain.start = start.index(),
                chain.end = last.index(),
                changed = *changed,
                "collapsing goto chain"
            );

            if self.pred_count[bb] == 1 {
                // This is the last reference to bb, so the pred-count to
                // to target is moved into the bb block.
                self.pred_count[bb] = 0;
            } else {
                self.pred_count[last] += 1;
                self.pred_count[bb] -= 1;
            }
            self.cfg[bb].terminator_mut().kind = TerminatorKind::Goto(last);
        }

        if let Some(chain_end) = chain_end {
            for phi in &mut self.cfg[last].phi_statements {
                let local = phi.sources.remove(&chain_end).unwrap();
                phi.sources.insert(*start, local);
            }
        }
        *start = last;
    }

    fn collapse_phis(&mut self, bb: BasicBlock, predesssor: BasicBlock) {
        let block = &mut self.cfg[bb];
        let statements = block.phi_statements.iter().map(|phi| {
            let src = *phi.sources.get(&predesssor).unwrap();
            (
                StmntKind::Assignment(
                    phi.dst,
                    RValue::Use(Operand::new(OperandData::Copy(src), DUMMY_SP)),
                ),
                phi.sctx,
            )
        });
        block.statements.raw.splice(0..0, statements);
    }

    fn run(&mut self) {
        let mut start = START_BLOCK;

        // Vec of the blocks that should be merged. We store the indices here, instead of the
        // statements itself to avoid moving the (relatively) large statements twice.
        // We do not push the statements directly into the target block (`bb`) as that is slower
        // due to additional reallocations
        let mut merged_blocks = Vec::new();
        loop {
            let mut changed = false;

            self.collapse_goto_chain(&mut start, &mut changed);

            for bb in self.cfg.blocks.indices() {
                if self.pred_count[bb] == 0 {
                    continue;
                }

                let span = trace_span!("simplifying", block = bb.index());
                let _enter = span.enter();

                let mut terminator =
                    self.cfg[bb].terminator.take().expect("invalid terminator state");

                for successor in terminator.successors_mut() {
                    self.collapse_goto_chain(successor, &mut changed);
                }

                merged_blocks.clear();

                let mut current = bb;

                loop {
                    let dst = match terminator.kind {
                        TerminatorKind::Split { true_block, false_block, .. }
                            if true_block == false_block =>
                        {
                            changed = true;

                            debug!(
                                from = debug(bb),
                                to = debug(true_block),
                                "Merging Split with equivalent targets"
                            );

                            if self.pred_count[true_block] == 2 {
                                self.pred_count[true_block] = 0;
                                true_block
                            } else {
                                terminator.kind = TerminatorKind::Goto(true_block);
                                break;
                            }
                        }

                        TerminatorKind::Goto(dst) if self.pred_count[dst] == 1 => {
                            changed = true;
                            debug!(
                                from = debug(bb),
                                to = debug(dst),
                                "Collapsing goto with single predecessor"
                            );
                            self.pred_count[dst] = 0;
                            dst
                        }

                        // TODO difference between infinite loop and dead code result (not that important because the former is just bullshit we should just shoot that down in some pass beforehand)
                        TerminatorKind::Split {
                            true_block, false_block, loop_head: true, ..
                        } if true_block == current
                            && self.cfg[current].statements.is_empty()
                            && self.cfg[current].phi_statements.is_empty() =>
                        {
                            changed = true;
                            if self.pred_count[false_block] == 1 {
                                self.pred_count[false_block] = 0;
                                false_block
                            } else {
                                terminator.kind = TerminatorKind::Goto(false_block);
                                break;
                            }
                        }

                        _ => break,
                    };
                    debug!(src = dst.index(), dst = bb.index(), "Appending basic block");
                    terminator = self.cfg[dst].terminator.take().expect("invalid terminator state");
                    self.collapse_phis(dst, current);
                    merged_blocks.push(dst);
                    current = dst
                }

                let statements_to_merge =
                    merged_blocks.iter().map(|&i| self.cfg.blocks[i].statements.len()).sum();

                if statements_to_merge > 0 {
                    let mut statements = std::mem::take(&mut self.cfg[bb].statements);
                    statements.reserve(statements_to_merge);
                    for &from in &merged_blocks {
                        statements.append(&mut self.cfg[from].statements);
                    }

                    self.cfg[bb].statements = statements;
                }

                self.cfg[bb].terminator = Some(terminator);
            }

            if !changed {
                break;
            }
        }

        if start != START_BLOCK {
            debug_assert!(self.pred_count[START_BLOCK] == 0);
            self.cfg.blocks.swap(START_BLOCK, start);
            self.pred_count.swap(START_BLOCK, start);

            // pred_count == 1 if the start block has no predecessor _blocks_.
            if self.pred_count[START_BLOCK] > 1 {
                for (bb, data) in self.cfg.blocks.iter_mut_enumerated() {
                    if self.pred_count[bb] == 0 {
                        continue;
                    }

                    data.terminator_mut().map(
                        |target| {
                            if target == start {
                                START_BLOCK
                            } else {
                                target
                            }
                        },
                    )
                }
            }
        }
    }
}

pub fn strip_nops<C: CfgFunctions>(cfg: &mut ControlFlowGraph<C>) {
    for block in &mut cfg.blocks {
        block.statements.retain(|(x, _)| !matches!(x, StmntKind::NoOp))
    }
}

pub fn remove_dead_blocks<C: CfgFunctions>(cfg: &mut ControlFlowGraph<C>) {
    let seen: BitSet<_> = {
        let mut seen = BitSet::new_empty(cfg.blocks.len());
        seen.extend(cfg.postorder_iter().map(|(id, _)| id));
        seen
    };

    let mut replacements: IndexVec<BasicBlock, BasicBlock> = cfg.blocks.indices().collect();

    let mut used_blocks = BasicBlock::new(0);
    for alive_index in seen.iter() {
        replacements[alive_index] = used_blocks;
        if alive_index != used_blocks {
            // Swap the next alive block data with the current available slot. Since
            // alive_index is non-decreasing this is a valid operation.
            cfg.blocks.swap(alive_index, used_blocks);
        }
        used_blocks += 1;
    }

    cfg.blocks.truncate(used_blocks.index());

    for data in cfg.blocks.iter_mut() {
        for phi in data.phi_statements.iter_mut() {
            phi.sources =
                phi.sources.iter().map(|(block, local)| (replacements[*block], *local)).collect();
        }
        data.terminator_mut().map(|succ| replacements[succ])
    }
}
