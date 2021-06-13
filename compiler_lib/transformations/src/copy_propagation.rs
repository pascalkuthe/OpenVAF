/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::UseDefGraph;
use openvaf_data_structures::HashMap;
use openvaf_middle::cfg::{CfgPass, ControlFlowGraph, IntLocation, InternedLocations, BasicBlock};
use openvaf_middle::{impl_pass_span, COperand, COperandData, CallType, OperandData, Local};
use openvaf_middle::dfa::{Analysis, Forward};
use openvaf_middle::const_fold::DiamondLattice;

// struct CopyPropagation<'a> {
//     data_dependence: &'a UseDefGraph,
//     locations: &'a InternedLocations,
// }
// 
// impl<'a, C: CallType> CfgPass<'_, C> for CopyPropagation<'a> {
//     type Result = ();
// 
//     fn run(self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
//         let Self {
//             data_dependence,
//             locations,
//         } = self;
// 
//         let mut copies = HashMap::with_capacity(256);
// 
//         for (bb, data) in cfg.blocks.iter_mut_enumerated() {
//             let block_location = &locations[bb];
// 
//             let mut loc = block_location.phi_start;
//             data.phi_statements.retain(|_| {
//                 let res = self.retain.contains(loc);
//                 loc += 1;
//                 res
//             });
// 
//             let mut loc = block_location.stmnt_start;
//             data.statements.retain(|_| {
//                 let res = self.retain.contains(loc);
//                 loc += 1;
//                 res
//             })
//         }
//     }
// 
//     impl_pass_span!("copy_propagation");
// }

pub struct CopyPropagation<'lt, C: CallType> {
    pub cfg: &'lt ControlFlowGraph<C>,
}




impl<'lt, C: CallType> Analysis<C> for CopyPropagation<'lt, C> {
    type Set = HashMap<Local,COperand<C>>;
    type Direction = Forward;

    fn transfer_function(
        &mut self,
        in_set: &Self::Set,
        out_set: &mut Self::Set,
        basic_bock: BasicBlock,
        cfg: &ControlFlowGraph<C>,
    ) {
        
        let bb = &cfg[basic_bock];
        
        for phi in bb.phi_statements{
            if phi.sources.iter()
        }
        
    }

    fn join(&mut self, src_bb: BasicBlock, dst_bb: BasicBlock, graph: &mut DfGraph<Self::Set>) {
        if !graph.out_sets[src_bb].unreachable {
            // try to constant fold terminator and mark as unreachable
            if let TerminatorKind::Split {
                ref condition,
                false_block,
                ..
            } = self.cfg[src_bb].terminator().kind
            {
                let mut fold = ConstantFold {
                    locals: &graph.out_sets[src_bb],
                    resolver: self.resolver,
                };

                if let DiamondLattice::Val(val) = fold_rvalue(&mut fold, condition, Type::INT) {
                    if (val == Scalar(Integer(0))) != (dst_bb == false_block) {
                        // unreachable don't propagate constants
                        return;
                    }
                }
            }

            graph.in_sets[dst_bb].unreachable = false;
            graph.in_sets[dst_bb].meet(&graph.out_sets[src_bb]);
        }
    }

    fn new_set(&self) -> Self::Set {
        BasicBlockConstants {
            unreachable: true,
            constants: HashMap::new(),
            not_a_constant: BitSet::new_empty(self.cfg.locals.len_idx()),
        }
    }

    fn setup_entry(&mut self, block: BasicBlock, graph: &mut DfGraph<Self::Set>) {
        graph.in_sets[block].unreachable = false
    }
}
