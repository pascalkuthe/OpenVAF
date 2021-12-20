mod live_derivatives;
mod transform;
mod unkowns;

use bitset::GrowableSparseBitMatrix;
use cfg::{Callback, ControlFlowGraph, Place};
use data_flow::Analysis;

pub use crate::live_derivatives::LiveDerivativeAnalysis;
pub use crate::transform::gen_derivatives;
use crate::transform::{LocalDerivatives, PlaceDerivatives};
use crate::unkowns::FirstOrderUnkownInfo;
pub use crate::unkowns::{FirstOrderUnkown, Unkown, Unkowns};

pub fn live_derivatives(
    cfg: &ControlFlowGraph,
    unkowns: impl IntoIterator<Item = (Callback, FirstOrderUnkownInfo)>,
    bottom: Option<GrowableSparseBitMatrix<Place, Unkown>>,
) -> data_flow::Results<LiveDerivativeAnalysis> {
    LiveDerivativeAnalysis::new(cfg, unkowns, bottom).into_engine(cfg).iterate_to_fixpoint()
}

pub fn auto_diff(
    cfg: &mut ControlFlowGraph,
    unkowns: impl IntoIterator<Item = (Callback, FirstOrderUnkownInfo)>,
    bottom: Option<GrowableSparseBitMatrix<Place, Unkown>>,
) -> (LocalDerivatives, PlaceDerivatives) {
    let live_derivatives = live_derivatives(cfg, unkowns, bottom);
    gen_derivatives(cfg, &live_derivatives)
}
