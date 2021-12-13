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

use bitset::matrix::GrowableSparseBitMatrix;
use cfg::{ControlFlowGraph, InstrDst, Local, Op, Operand, Place};
use data_flow::lattice::JoinSemiLattice;
use data_flow::{direction, Analysis, AnalysisDomain};

use crate::unkowns::{Unkown, Unkowns};

#[cfg(test)]
mod tests;

pub struct LiveDerivativeAnalysis {
    pub unkowns: Unkowns,
    pub bottom: LiveDerivatives,
}

#[derive(Debug, PartialEq)]
pub struct LiveDerivatives {
    pub required_places: GrowableSparseBitMatrix<Place, Unkown>,
    pub required_locals: GrowableSparseBitMatrix<Local, Unkown>,
}

impl Clone for LiveDerivatives {
    fn clone(&self) -> Self {
        Self {
            required_places: self.required_places.clone(),
            required_locals: self.required_locals.clone(),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        self.required_places.clone_from(&source.required_places);
        self.required_locals.clone_from(&source.required_locals);
    }
}

impl JoinSemiLattice for LiveDerivatives {
    fn join(&mut self, other: &Self) -> bool {
        self.required_places.join(&other.required_places)
            | self.required_locals.join(&other.required_locals)
    }
}

impl AnalysisDomain for LiveDerivativeAnalysis {
    type Domain = LiveDerivatives;
    type Direction = direction::Backward;

    /// A descriptive name for this analysis. Used only for debugging.
    const NAME: &'static str = "Live Derivatives";

    /// The initial value of the dataflow state upon entry to each basic block.
    fn bottom_value(&self, _cfg: &ControlFlowGraph) -> LiveDerivatives {
        self.bottom.clone()
    }

    fn initialize_start_block(&self, _cfg: &ControlFlowGraph, _state: &mut Self::Domain) {}
}

impl Analysis for LiveDerivativeAnalysis {
    fn apply_phi_effect(
        &self,
        _cfg: &ControlFlowGraph,
        state: &mut Self::Domain,
        phi: &cfg::Phi,
        _bb: cfg::BasicBlock,
        _idx: cfg::PhiIdx,
    ) {
        let unkowns = state.required_locals.matrix.take_row(phi.dst);

        if let Some(unkowns) = unkowns {
            let domain_size = state.required_locals.matrix.num_columns();
            // Propagate require dependencies from dst to all sources
            for (_, local) in &phi.sources {
                state.required_locals.union_with(&unkowns, domain_size, *local);
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
                state.required_locals.matrix.take_row(local),
                state.required_locals.matrix.num_columns(),
            ),
            InstrDst::Place(place) => (
                state.required_places.matrix.take_row(place),
                state.required_places.matrix.num_columns(),
            ),
            InstrDst::Ignore => (None, 0),
        };

        if let Op::Call(callback) = instr.op {
            if let Some(unkown) = self.unkowns.callback_unkown(callback) {
                // A derivative operator (ddx)!
                // We need to be derive the argument by the main unkown...
                let mut unkowns = vec![unkown.into()];

                // and any higher order derivatives of already required derivatives
                if let Some(propgated) = &propgated {
                    self.unkowns.raise_order(propgated, &mut unkowns, unkown)
                }

                match instr.args[0] {
                    Operand::Local(local) => {
                        for unkown in unkowns {
                            state.required_locals.insert(local, unkown);
                        }
                    }
                    Operand::Place(place) => {
                        for unkown in unkowns {
                            state.required_places.insert(place, unkown);
                        }
                    }
                    _ => (), // technically not well optimized but doesn't matter because who write ddx(<const>)
                };

                // Do not return here
                // we still need to lower order derivatives to calculate the higher order derivatives
            }
        }

        if let Some(propgated) = propgated.as_ref() {
            for arg in &*instr.args {
                match *arg {
                    Operand::Local(local) => {
                        state.required_locals.union_with(propgated, propgated_domain_size, local);
                    }
                    Operand::Place(place) => {
                        state.required_places.union_with(propgated, propgated_domain_size, place);
                    }
                    _ => (),
                }
            }
        }
    }
}
