/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use super::{Analysis, Direction, Results};
use crate::cfg::{BasicBlock, BasicBlockData, ControlFlowGraph, Phi, PhiData, Terminator};
use crate::{CallType, Statement, StatementId};

/// Calls the corresponding method in `ResultsVisitor` for every location in a ControlFlow Graph with the
/// dataflow state at that location.
pub fn visit_results<'a, C, F, V>(
    cfg: &'a ControlFlowGraph<C>,
    blocks: impl IntoIterator<Item = (BasicBlock, &'a BasicBlockData<C>)>,
    results: &V,
    vis: &mut impl ResultsVisitor<C, FlowState = F>,
) where
    C: CallType,
    V: ResultsVisitable<C, FlowState = F>,
{
    let mut state = results.new_flow_state(cfg);

    for (block, block_data) in blocks {
        V::Direction::visit_results_in_block(&mut state, cfg, block, block_data, results, vis);
    }
}

/// Calls the corresponding method in `ResultsVisitor` for every location in a ControlFlow Graph with the
/// dataflow state at that location.
pub fn visit_results_mut<C, F, V>(
    cfg: &mut ControlFlowGraph<C>,
    blocks: impl IntoIterator<Item = BasicBlock>,
    results: &V,
    vis: &mut impl ResultsVisitorMut<C, FlowState = F>,
) where
    C: CallType,
    V: ResultsVisitable<C, FlowState = F>,
{
    let mut state = results.new_flow_state(cfg);

    for block in blocks {
        V::Direction::visit_results_in_block_mut(&mut state, cfg, block, results, vis);
    }
}

pub trait ResultsVisitor<C: CallType> {
    type FlowState;

    #[inline(always)]
    fn visit_block_start(
        &mut self,
        _state: &Self::FlowState,
        _block_data: &BasicBlockData<C>,
        _block: BasicBlock,
    ) {
    }

    #[inline(always)]
    fn visit_phi_before_effect(
        &mut self,
        _state: &Self::FlowState,
        _stmnt: &PhiData,
        _block: BasicBlock,
        _id: Phi,
    ) {
    }

    #[inline(always)]
    fn visit_phi_after_effect(
        &mut self,
        _state: &Self::FlowState,
        _stmnt: &PhiData,
        _block: BasicBlock,
        _id: Phi,
    ) {
    }

    #[inline(always)]
    fn visit_statement_before_effect(
        &mut self,
        _state: &Self::FlowState,
        _stmnt: &Statement<C>,
        _block: BasicBlock,
        _id: StatementId,
    ) {
    }

    #[inline(always)]
    fn visit_statement_after_effect(
        &mut self,
        _state: &Self::FlowState,
        _stmnt: &Statement<C>,
        _block: BasicBlock,
        _id: StatementId,
    ) {
    }

    #[inline(always)]
    fn visit_terminator_after_effect(
        &mut self,
        _state: &Self::FlowState,
        _term: &Terminator<C>,
        _block: BasicBlock,
    ) {
    }

    #[inline(always)]
    fn visit_terminator_before_effect(
        &mut self,
        _state: &Self::FlowState,
        _term: &Terminator<C>,
        _block: BasicBlock,
    ) {
    }

    #[inline(always)]
    fn visit_block_end(
        &mut self,
        _state: &Self::FlowState,
        _block_data: &BasicBlockData<C>,
        _block: BasicBlock,
    ) {
    }
}

pub trait ResultsVisitorMut<C: CallType> {
    type FlowState;

    #[inline(always)]
    fn visit_block_start(
        &mut self,
        _state: &Self::FlowState,
        _block_data: &mut BasicBlockData<C>,
        _block: BasicBlock,
    ) {
    }

    #[inline(always)]
    fn visit_phi_before_effect(
        &mut self,
        _state: &Self::FlowState,
        _stmnt: &mut PhiData,
        _block: BasicBlock,
        _id: Phi,
    ) {
    }

    #[inline(always)]
    fn visit_phi_after_effect(
        &mut self,
        _state: &Self::FlowState,
        _stmnt: &mut PhiData,
        _block: BasicBlock,
        _id: Phi,
    ) {
    }

    #[inline(always)]
    fn visit_statement_before_effect(
        &mut self,
        _state: &Self::FlowState,
        _stmnt: &mut Statement<C>,
        _block: BasicBlock,
        _id: StatementId,
    ) {
    }

    #[inline(always)]
    fn visit_statement_after_effect(
        &mut self,
        _state: &Self::FlowState,
        _stmnt: &mut Statement<C>,
        _block: BasicBlock,
        _id: StatementId,
    ) {
    }

    #[inline(always)]
    fn visit_terminator_before_effect(
        &self,
        _state: &Self::FlowState,
        _term: &mut Terminator<C>,
        _block: BasicBlock,
    ) {
    }

    #[inline(always)]
    fn visit_terminator_after_effect(
        &self,
        _state: &Self::FlowState,
        _term: &mut Terminator<C>,
        _block: BasicBlock,
    ) {
    }

    #[inline(always)]
    fn visit_block_end(
        &mut self,
        _state: &Self::FlowState,
        _block_data: &mut BasicBlockData<C>,
        _block: BasicBlock,
    ) {
    }
}

/// Things that can be visited by a `ResultsVisitor`.
///
/// This trait exists so that we can visit the results of multiple dataflow analyses simultaneously.
/// DO NOT IMPLEMENT MANUALLY. Instead, use the `impl_visitable` macro below.
pub trait ResultsVisitable<C: CallType> {
    type Direction: Direction;
    type FlowState;

    /// Creates an empty `FlowState` to hold the transient state for these dataflow results.
    ///
    /// The value of the newly created `FlowState` will be overwritten by `reset_to_block_entry`
    /// before it can be observed by a `ResultsVisitor`.
    fn new_flow_state(&self, cfg: &ControlFlowGraph<C>) -> Self::FlowState;

    fn reset_to_block_entry(&self, state: &mut Self::FlowState, block: BasicBlock);

    fn reconstruct_phi_effect(
        &self,
        cfg: &ControlFlowGraph<C>,
        state: &mut Self::FlowState,
        phi: &PhiData,
        bb: BasicBlock,
        id: Phi,
    );

    fn reconstruct_statement_effect(
        &self,
        cfg: &ControlFlowGraph<C>,
        state: &mut Self::FlowState,
        statement: &Statement<C>,
        bb: BasicBlock,
        id: StatementId,
    );

    fn reconstruct_terminator_effect(
        &self,
        cfg: &ControlFlowGraph<C>,
        state: &mut Self::FlowState,
        terminator: &Terminator<C>,
        bb: BasicBlock,
    );
}

impl<C: CallType, A> ResultsVisitable<C> for Results<C, A>
where
    A: Analysis<C>,
{
    type Direction = A::Direction;

    type FlowState = A::Domain;

    fn new_flow_state(&self, cfg: &ControlFlowGraph<C>) -> Self::FlowState {
        self.analysis.bottom_value(cfg)
    }

    fn reset_to_block_entry(&self, state: &mut Self::FlowState, block: BasicBlock) {
        state.clone_from(&self.entry_set_for_block(block));
    }

    fn reconstruct_phi_effect(
        &self,
        cfg: &ControlFlowGraph<C>,
        state: &mut Self::FlowState,
        phi: &PhiData,
        bb: BasicBlock,
        id: Phi,
    ) {
        self.analysis.apply_phi_effect(cfg, state, phi, bb, id);
    }

    fn reconstruct_statement_effect(
        &self,
        cfg: &ControlFlowGraph<C>,
        state: &mut Self::FlowState,
        statement: &Statement<C>,
        bb: BasicBlock,
        id: StatementId,
    ) {
        self.analysis
            .apply_statement_effect(cfg, state, statement, id, bb);
    }

    fn reconstruct_terminator_effect(
        &self,
        cfg: &ControlFlowGraph<C>,
        state: &mut Self::FlowState,
        terminator: &Terminator<C>,
        bb: BasicBlock,
    ) {
        self.analysis
            .apply_terminator_effect(cfg, state, terminator, bb);
    }
}
//
// /// A tuple with named fields that can hold either the results or the transient state of the
// /// dataflow analyses used by the borrow checker.
// #[derive(Debug)]
// pub struct BorrowckAnalyses<B, U, E> {
//     pub borrows: B,
//     pub uninits: U,
//     pub ever_inits: E,
// }
//
// /// The results of the dataflow analyses used by the borrow checker.
// pub type BorrowckResults<'mir, 'tcx> = BorrowckAnalyses<
//     Results<'tcx, Borrows<'mir, 'tcx>>,
//     Results<'tcx, MaybeUninitializedPlaces<'mir, 'tcx>>,
//     Results<'tcx, EverInitializedPlaces<'mir, 'tcx>>,
// >;
//
// /// The transient state of the dataflow analyses used by the borrow checker.
// pub type BorrowckFlowState<'mir, 'tcx> =
// <BorrowckResults<'mir, 'tcx> as ResultsVisitable<'tcx>>::FlowState;
//
// macro_rules! impl_visitable {
//     ( $(
//         $T:ident { $( $field:ident : $A:ident ),* $(,)? }
//     )* ) => { $(
//         impl<'tcx, $($A),*, D: Direction> ResultsVisitable<'tcx> for $T<$( Results<'tcx, $A> ),*>
//         where
//             $( $A: Analysis<'tcx, Direction = D>, )*
//         {
//             type Direction = D;
//             type FlowState = $T<$( $A::Domain ),*>;
//
//             fn new_flow_state(&self, body: &mir::Body<'tcx>) -> Self::FlowState {
//                 $T {
//                     $( $field: self.$field.analysis.bottom_value(body) ),*
//                 }
//             }
//
//             fn reset_to_block_entry(
//                 &self,
//                 state: &mut Self::FlowState,
//                 block: BasicBlock,
//             ) {
//                 $( state.$field.clone_from(&self.$field.entry_set_for_block(block)); )*
//             }
//
//             fn reconstruct_before_statement_effect(
//                 &self,
//                 state: &mut Self::FlowState,
//                 stmt: &mir::Statement<'tcx>,
//                 loc: Location,
//             ) {
//                 $( self.$field.analysis
//                     .apply_before_statement_effect(&mut state.$field, stmt, loc); )*
//             }
//
//             fn reconstruct_statement_effect(
//                 &self,
//                 state: &mut Self::FlowState,
//                 stmt: &mir::Statement<'tcx>,
//                 loc: Location,
//             ) {
//                 $( self.$field.analysis
//                     .apply_statement_effect(&mut state.$field, stmt, loc); )*
//             }
//
//             fn reconstruct_before_terminator_effect(
//                 &self,
//                 state: &mut Self::FlowState,
//                 term: &mir::Terminator<'tcx>,
//                 loc: Location,
//             ) {
//                 $( self.$field.analysis
//                     .apply_before_terminator_effect(&mut state.$field, term, loc); )*
//             }
//
//             fn reconstruct_terminator_effect(
//                 &self,
//                 state: &mut Self::FlowState,
//                 term: &mir::Terminator<'tcx>,
//                 loc: Location,
//             ) {
//                 $( self.$field.analysis
//                     .apply_terminator_effect(&mut state.$field, term, loc); )*
//             }
//         }
//     )* }
// }
//
// impl_visitable! {
//     BorrowckAnalyses { borrows: B, uninits: U, ever_inits: E }
// }
