use ahash::AHashMap;
use bitset::{BitSet, HybridBitSet, SparseBitMatrix};
use cfg::{Callback, ControlFlowGraph, Local, Op, Operand, Place};
use program_dependence::def_use::Def;
use program_dependence::{use_def, Assignment, ProgramDependenGraph};

use crate::{FirstOrderUnkown, Unkown, Unkowns};

#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub struct LiveDerivatives {
    pub local_derivatives: SparseBitMatrix<Local, Unkown>,
    pub assign_derivatives: SparseBitMatrix<Assignment, Unkown>,
    pub assign_zero: BitSet<Assignment>,
    pub ddx: AHashMap<Def, (FirstOrderUnkown, HybridBitSet<Unkown>)>,
}

impl LiveDerivatives {
    pub fn build(
        dfs: &use_def::DepthFirstSearch,
        cfg: &ControlFlowGraph,
        pdg: &ProgramDependenGraph,
        param_cnt: usize,
        unkowns: &mut Unkowns,
        extra_derivatives: impl IntoIterator<Item = (Place, Callback)>,
    ) -> LiveDerivatives {
        let mut unkown_params = BitSet::new_empty(param_cnt);

        for (_, params) in unkowns.first_order_unkowns.raw.iter() {
            for (param, _) in params.iter() {
                unkown_params.insert(*param);
            }
        }

        let mut locals = BitSet::new_empty(cfg.next_local.into());
        let mut assigns = BitSet::new_empty(pdg.interner().assigment_locations.len());
        let mut ddx = AHashMap::new();

        dfs.visited_instructions(pdg, cfg, |def, instr| {
            if let Op::Call(call) = instr.op {
                if let Some(unkown) = unkowns.first_order_unkowns.index(&call) {
                    ddx.insert(def, unkown);
                }
            }

            instr.visit_operands(|op| {
                if matches!(op, Operand::CfgParam(param) if unkown_params.contains(*param)) {
                    match def {
                        Def::Local(local) => {
                            locals.insert(local);
                        }
                        Def::Assignment(assign) => {
                            assigns.insert(assign);
                        }
                    }
                }
            })
        });

        let entrys = locals.iter().map(Def::from).chain(assigns.iter().map(Def::from));

        let mut res = LiveDerivatives {
            local_derivatives: SparseBitMatrix::new(cfg.next_local.into(), unkowns.len()),
            assign_derivatives: SparseBitMatrix::new(
                pdg.interner().assigment_locations.len(),
                unkowns.len(),
            ),
            assign_zero: BitSet::new_empty(0),
            ddx: AHashMap::new(),
        };

        let mut cursor = pdg.reaching_definitions.as_results_cursor(cfg);
        cursor.seek_to_block_end(cfg.blocks.last_key().unwrap(), cfg);

        for (place, cb) in extra_derivatives {
            if let Some(assigns) = pdg.interner().place_assigments.row(place) {
                let unkown = unkowns.first_order_unkowns.index(&cb).unwrap();
                for assign in assigns.iter() {
                    if cursor.get().contains(assign) {
                        res.assign_derivatives.insert(assign, unkown.into());
                    }
                }
            }
        }

        let dug = pdg.def_use_graph();
        let mut postorder = dug.dependent_defs_rec_postorder(entrys);
        for def in &mut postorder {
            let mut dst = HybridBitSet::new_empty();
            for def in dug.dependent_defs(def) {
                match def {
                    Def::Local(local) => {
                        if let Some(row) = res.local_derivatives.row(local) {
                            dst.union(row, unkowns.len());
                        }
                    }
                    Def::Assignment(assign) => {
                        if let Some(row) = res.assign_derivatives.row(assign) {
                            dst.union(row, unkowns.len());
                        }
                    }
                }
            }

            if let Some(ddx) = ddx.get(&def).copied() {
                let old = dst.clone();
                for unkown in old.iter() {
                    let higher_order = unkowns.raise_order(unkown, ddx);
                    dst.insert_growable(higher_order, unkowns.len());
                }

                dst.insert(ddx.into(), unkowns.len());
                res.ddx.insert(def, (ddx, old));
            }

            if !dst.is_empty() {
                match def {
                    Def::Local(local) => *res.local_derivatives.ensure_row(local) = dst,
                    Def::Assignment(assign) => *res.assign_derivatives.ensure_row(assign) = dst,
                }
            }
        }

        // All assigns who have not been visited always have a zero derivatives
        // Because places are mutable we can't do a simple trick of replacing all uses with zero
        // Instead we must write zeros into the derivative place
        // 99% of these derivatives (and quickly removed by DCE)
        // But in edgcases this can be very important

        res.assign_zero = {
            postorder.visited_assigns.inverse();
            postorder.visited_assigns
        };

        for assign in res.assign_zero.iter() {
            let mut dst = HybridBitSet::new_empty();
            for def in dug.dependent_defs(assign.into()) {
                match def {
                    Def::Local(local) => {
                        if let Some(row) = res.local_derivatives.row(local) {
                            dst.union(row, unkowns.len());
                        }
                    }
                    Def::Assignment(assign) => {
                        if let Some(row) = res.assign_derivatives.row(assign) {
                            dst.union(row, unkowns.len());
                        }
                    }
                }
            }

            if !dst.is_empty() {
                *res.assign_derivatives.ensure_row(assign) = dst
            }
        }
        res
    }
}
