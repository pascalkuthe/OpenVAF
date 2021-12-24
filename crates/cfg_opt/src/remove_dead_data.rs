use cfg::{Callback, CfgParam, ControlFlowGraph, Local, Place};
use typed_indexmap::TiSet;

pub type ParamMap = TiSet<CfgParam, CfgParam>;
pub type PlaceMap = TiSet<Place, Place>;
pub type LocalMap = TiSet<Local, Local>;
pub type CallBackMap = TiSet<Callback, Callback>;

pub fn remove_dead_data(cfg: &mut ControlFlowGraph) -> (LocalMap, PlaceMap, ParamMap, CallBackMap) {
    let mut local_map = TiSet::with_capacity(cfg.next_local.into());
    let mut place_map = TiSet::with_capacity(cfg.next_place.into());
    let mut param_map = TiSet::default();
    let mut callback_map = TiSet::default();

    cfg.visit_data_mut(
        |local| *local = local_map.ensure(*local).0,
        |place| *place = place_map.ensure(*place).0,
        |param| *param = param_map.ensure(*param).0,
        |callback| *callback = callback_map.ensure(*callback).0,
    );

    (local_map, place_map, param_map, callback_map)
}
