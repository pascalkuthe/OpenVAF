use cfg::{CfgParam, ControlFlowGraph, Local, Place};
use typed_index_collections::TiVec;

fn map<K>(old_idx: &mut K, map: &mut TiVec<K, Option<K>>, next_index: &mut usize)
where
    K: From<usize> + Copy,
    usize: From<K>,
{
    match &mut map[*old_idx] {
        Some(new_idx) => *old_idx = *new_idx,
        dst @ None => {
            *old_idx = (*next_index).into();
            *dst = Some((*next_index).into());
            *next_index += 1
        }
    }
}

pub type ParamMap = TiVec<CfgParam, Option<CfgParam>>;
pub type PlaceMap = TiVec<Place, Option<Place>>;
pub type LocalMap = TiVec<Local, Option<Local>>;

pub fn remove_dead_data(
    cfg: &mut ControlFlowGraph,
    param_cnt: usize,
) -> (LocalMap, PlaceMap, ParamMap) {
    let mut local_map: LocalMap = vec![None; cfg.next_local.into()].into();
    let mut next_local = 0;

    let mut place_map: PlaceMap = vec![None; cfg.next_place.into()].into();
    let mut next_place = 0;

    let mut param_map: ParamMap = vec![None; param_cnt].into();
    let mut next_param = 0;

    cfg.visit_data_mut(
        |local| map(local, &mut local_map, &mut next_local),
        |place| map(place, &mut place_map, &mut next_place),
        |param| map(param, &mut param_map, &mut next_param),
    );

    cfg.next_local = next_local.into();
    cfg.next_place = next_place.into();

    (local_map, place_map, param_map)
}
