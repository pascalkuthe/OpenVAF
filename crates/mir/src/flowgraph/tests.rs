use super::*;

use mir::builder::InstBuilder;
use mir::cursor::{Cursor, FuncCursor};
use mir::Function;

#[test]
fn empty() {
    let func = Function::new();
    ControlFlowGraph::with_function(&func);
}

#[test]
fn no_predecessors() {
    let mut func = Function::new();
    let block0 = func.dfg.make_block();
    let block1 = func.dfg.make_block();
    let block2 = func.dfg.make_block();
    func.layout.append_block(block0);
    func.layout.append_block(block1);
    func.layout.append_block(block2);

    let cfg = ControlFlowGraph::with_function(&func);

    let mut fun_blocks = func.layout.blocks();
    for block in func.layout.blocks() {
        assert_eq!(block, fun_blocks.next().unwrap());
        assert_eq!(cfg.pred_iter(block).count(), 0);
        assert_eq!(cfg.succ_iter(block).count(), 0);
    }
}

#[test]
fn branches_and_jumps() {
    let mut func = Function::new();
    let block0 = func.dfg.make_block();
    let cond = func.dfg.append_block_param(block0);
    let block1 = func.dfg.make_block();
    let block2 = func.dfg.make_block();
    let block3 = func.dfg.make_block();

    let br_block0_block2;
    let br_block1_block1;
    let jmp_block0_block1;
    let jmp_block1_block2;

    {
        let mut cur = FuncCursor::new(&mut func);

        cur.insert_block(block0);
        br_block0_block2 = cur.ins().brnz(cond, block2, &[]);
        jmp_block0_block1 = cur.ins().jump(block1, &[]);

        cur.insert_block(block1);
        br_block1_block1 = cur.ins().brnz(cond, block1, &[]);
        jmp_block1_block2 = cur.ins().jump(block2, &[]);

        cur.insert_block(block2);
        cur.insert_block(block3);
    }

    let mut cfg = ControlFlowGraph::with_function(&func);

    {
        let block0_predecessors = cfg.pred_iter(block0).count();
        let block1_predecessors = cfg.pred_iter(block1).collect::<Vec<_>>();
        let block2_predecessors = cfg.pred_iter(block2).collect::<Vec<_>>();

        let block0_successors = cfg.succ_iter(block0).collect::<Vec<_>>();
        let block1_successors = cfg.succ_iter(block1).collect::<Vec<_>>();
        let block2_successors = cfg.succ_iter(block2).collect::<Vec<_>>();

        assert_eq!(block0_predecessors, 0);
        assert_eq!(block1_predecessors.len(), 2);
        assert_eq!(block2_predecessors.len(), 2);

        assert!(block1_predecessors.contains(&BlockPredecessor::new(block0, jmp_block0_block1)),);
        assert!(block1_predecessors.contains(&BlockPredecessor::new(block1, br_block1_block1)),);
        assert!(block2_predecessors.contains(&BlockPredecessor::new(block0, br_block0_block2)),);
        assert!(block2_predecessors.contains(&BlockPredecessor::new(block1, jmp_block1_block2)),);

        assert_eq!(block0_successors, [block1, block2]);
        assert_eq!(block1_successors, [block1, block2]);
        assert_eq!(block2_successors, []);
    }

    // Change some instructions and recompute block0
    func.dfg.replace(br_block0_block2).brnz(cond, block1, &[]);
    func.dfg.replace(jmp_block0_block1).jump(block3, &[]);
    cfg.recompute_block(&func, block0);
    let br_block0_block1 = br_block0_block2;

    {
        let block0_predecessors = cfg.pred_iter(block0).count();
        let block1_predecessors = cfg.pred_iter(block1).collect::<Vec<_>>();
        let block2_predecessors = cfg.pred_iter(block2).collect::<Vec<_>>();

        let block0_successors = cfg.succ_iter(block0);
        let block1_successors = cfg.succ_iter(block1);
        let block2_successors = cfg.succ_iter(block2);

        assert!(block1_predecessors.contains(&BlockPredecessor::new(block0, br_block0_block1)),);
        assert!(block1_predecessors.contains(&BlockPredecessor::new(block1, br_block1_block1)),);
        assert!(!block2_predecessors.contains(&BlockPredecessor::new(block0, br_block0_block2)),);
        assert!(block2_predecessors.contains(&BlockPredecessor::new(block1, jmp_block1_block2)),);

        assert_eq!(block0_predecessors, 0);
        assert_eq!(block1_predecessors.len(), 2);
        assert_eq!(block2_predecessors.len(), 1);

        assert_eq!(block0_successors.collect::<Vec<_>>(), [block1, block3]);
        assert_eq!(block1_successors.collect::<Vec<_>>(), [block1, block2]);
        assert_eq!(block2_successors.collect::<Vec<_>>(), []);
    }
}
