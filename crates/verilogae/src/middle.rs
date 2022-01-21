use cfg::ControlFlowGraph;
use cfg_opt::{remove_dead_data, simplify_branches, simplify_cfg};
use hir_lower::{HirInterner, PlaceKind};
use program_dependence::{use_def, ProgramDependenGraph};

use crate::compiler_db::Function;

pub(crate) fn create_slice(
    cfg: &ControlFlowGraph,
    pdg: &ProgramDependenGraph,
    intern: &HirInterner,
    fun: &Function,
    verify: bool,
) -> (ControlFlowGraph, HirInterner) {
    let place = intern.places.index(&PlaceKind::Var(fun.var)).unwrap();

    let mut cfg = cfg.clone();
    let mut intern = intern.clone();

    // eliminate dead code
    let mut live_code = use_def::DepthFirstSearch::new(pdg);
    for var in &*fun.dependency_breaking {
        if let Some(place) = intern.places.index(&PlaceKind::Var(*var)) {
            if let Some(assigments) = pdg.interner().place_assigments.row(place) {
                live_code.visited_assigments.union(assigments);
            }
        }
    }
    live_code.walk_place::<true>(place, pdg, &cfg);
    for var in &*fun.dependency_breaking {
        if let Some(place) = intern.places.index(&PlaceKind::Var(*var)) {
            if let Some(assigments) = pdg.interner().place_assigments.row(place) {
                live_code.visited_assigments.subtract(assigments);
            }
        }
    }
    live_code.remove_unvisited_from_cfg(&mut cfg, pdg.interner(), None);
    simplify_branches(&mut cfg);
    simplify_cfg(&mut cfg);

    if verify {
        cfg.assert_verified();
    }

    let (_, place_map, param_map, callback_map) = remove_dead_data(&mut cfg);
    intern.map(&place_map, &param_map, &callback_map);

    (cfg, intern)
}
