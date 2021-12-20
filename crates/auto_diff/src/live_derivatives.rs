//! This module implements a data flow analysis similar to live variable analysis.
//! It tracks which places and locals require a derivative at a
//! certain point in the program. This is required to properly generate derivatives for loops.
//!
//! The algorithm is nearly identical to live variable analysis with a two key differences:
//!
//! * Instead of just tracking one state (live/not live) for each place/local a set of states is
//! tracked instead (live/not live for each unkown) -> `SparseBitMatrix` instead of `BitSet` as Domain
//!
//! * Higher Order Derivatives may introduce additional unkowns which can not be known ahead of
//! time so the domain may grow -> `GrowableSparseBitMatrix`

use std::cell::{Cell, RefCell};

use bitset::matrix::GrowableSparseBitMatrix;
use bitset::{BitSet, HybridBitSet};
use cfg::{Callback, ControlFlowGraph, InstrDst, Local, Op, Operand, Place};
use data_flow::{direction, Analysis, AnalysisDomain};

use crate::unkowns::{FirstOrderUnkownInfo, Unkown, Unkowns};

#[cfg(test)]
mod tests;

pub struct LiveDerivativeAnalysis {
    pub unkowns: Unkowns,
    pub bottom: GrowableSparseBitMatrix<Place, Unkown>,
    pub live_local_derivatives: RefCell<GrowableSparseBitMatrix<Local, Unkown>>,
    tmp_dense: Cell<BitSet<Unkown>>,
}

impl LiveDerivativeAnalysis {
    pub fn new(
        cfg: &ControlFlowGraph,
        unkowns: impl IntoIterator<Item = (Callback, FirstOrderUnkownInfo)>,
        bottom: Option<GrowableSparseBitMatrix<Place, Unkown>>,
    ) -> LiveDerivativeAnalysis {
        let unkowns = Unkowns::new(unkowns);
        let bottom = bottom
            .unwrap_or_else(|| GrowableSparseBitMatrix::new(cfg.next_place.into(), unkowns.len()));
        LiveDerivativeAnalysis {
            tmp_dense: Cell::new(BitSet::new_empty(0)),
            live_local_derivatives: RefCell::new(GrowableSparseBitMatrix::new(
                cfg.next_local.into(),
                unkowns.len(),
            )),
            unkowns,
            bottom,
        }
    }
}

impl AnalysisDomain for LiveDerivativeAnalysis {
    type Domain = GrowableSparseBitMatrix<Place, Unkown>;
    type Direction = direction::Backward;

    /// A descriptive name for this analysis. Used only for debugging.
    const NAME: &'static str = "Live Derivatives";

    /// The initial value of the dataflow state upon entry to each basic block.
    fn bottom_value(&self, _cfg: &ControlFlowGraph) -> GrowableSparseBitMatrix<Place, Unkown> {
        self.bottom.clone()
    }

    fn initialize_start_block(&self, _cfg: &ControlFlowGraph, _state: &mut Self::Domain) {}
}

impl Analysis for LiveDerivativeAnalysis {
    fn apply_phi_effect(
        &self,
        _cfg: &ControlFlowGraph,
        _state: &mut Self::Domain,
        phi: &cfg::Phi,
        _bb: cfg::BasicBlock,
        _idx: cfg::PhiIdx,
    ) {
        if self.live_local_derivatives.borrow().matrix.row(phi.dst).is_some() {
            // Propagate require dependencies from dst to all sources
            for (_, local) in &phi.sources {
                self.live_local_derivatives.borrow_mut().union_rows(phi.dst, *local);
            }
        }
    }

    fn apply_instr_effect(
        &self,
        _cfg: &ControlFlowGraph,
        state: &mut Self::Domain,
        instr: &cfg::Instruction,
        _idx: cfg::InstIdx,
        _bb: cfg::BasicBlock,
    ) {
        let (propgated, propgated_domain_size) = match instr.dst {
            InstrDst::Local(local) => (
                match self.live_local_derivatives.borrow().matrix.row(local) {
                    Some(HybridBitSet::Sparse(sparse)) => {
                        Some(HybridBitSet::Sparse(sparse.clone()))
                    }
                    Some(HybridBitSet::Dense(dense)) => {
                        let mut dst = self.tmp_dense.take();
                        dst.clone_from(dense);
                        Some(HybridBitSet::Dense(dst))
                    }
                    None => None,
                },
                self.live_local_derivatives.borrow().matrix.num_columns(),
            ),
            InstrDst::Place(place) => (state.matrix.take_row(place), state.matrix.num_columns()),
            InstrDst::Ignore => (None, 0),
        };

        if let Op::Call(callback) = instr.op {
            if let Some(unkown) = self.unkowns.callback_unkown(callback) {
                // A derivative operator (ddx)!
                match instr.args[0] {
                    Operand::Local(local) => {
                        let mut live_local_derivatives = self.live_local_derivatives.borrow_mut();
                        live_local_derivatives.insert(local, unkown.into());
                        if let Some(propgated) = &propgated {
                            for previous_order in propgated.iter() {
                                let unkown = self.unkowns.raise_order(previous_order, unkown);
                                live_local_derivatives.insert(local, unkown);
                            }
                        }
                    }
                    Operand::Place(place) => {
                        state.insert(place, unkown.into());
                        if let Some(propgated) = &propgated {
                            for previous_order in propgated.iter() {
                                let unkown = self.unkowns.raise_order(previous_order, unkown);
                                state.insert(place, unkown);
                            }
                        }
                    }
                    _ => (), // technically not well optimized but doesn't matter because who write ddx(<const>)
                };

                // Do not return here
                // we still need to lower order derivatives to calculate the higher order derivatives
            }
        }

        if let Some(propgated) = propgated {
            for arg in &*instr.args {
                match *arg {
                    Operand::Local(local) => {
                        self.live_local_derivatives.borrow_mut().union_with(
                            &propgated,
                            propgated_domain_size,
                            local,
                        );
                    }
                    Operand::Place(place) => {
                        state.union_with(&propgated, propgated_domain_size, place);
                    }
                    _ => (),
                }
            }

            if let HybridBitSet::Dense(dense) = propgated {
                self.tmp_dense.set(dense)
            }
        }
    }
}
