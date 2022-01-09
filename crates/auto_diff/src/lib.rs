mod live_derivatives;
mod transform;
mod unkowns;

pub use crate::transform::{gen_derivatives, LocalDerivatives, PlaceDerivatives};
use crate::unkowns::FirstOrderUnkownInfo;
pub use crate::unkowns::{FirstOrderUnkown, Unkown, Unkowns};
use cfg::{Callback, ControlFlowGraph, Place};
pub use live_derivatives::LiveDerivatives;
use program_dependence::{use_def, ProgramDependenGraph};

pub fn auto_diff(
    cfg: &mut ControlFlowGraph,
    pdg: &ProgramDependenGraph,
    dfs: &use_def::DepthFirstSearch,
    param_cnt: usize,
    unkowns: impl IntoIterator<Item = (Callback, FirstOrderUnkownInfo)>,
    extra_derivatives: impl IntoIterator<Item = (Place, Callback)>,
) -> (LocalDerivatives, PlaceDerivatives, Unkowns) {
    let mut unkowns = Unkowns::new(unkowns);
    let live_derivatives =
        LiveDerivatives::build(dfs, cfg, pdg, param_cnt, &mut unkowns, extra_derivatives);
    let (locals, places) = gen_derivatives(cfg, &live_derivatives, &unkowns, pdg.interner());
    (locals, places, unkowns)
}
