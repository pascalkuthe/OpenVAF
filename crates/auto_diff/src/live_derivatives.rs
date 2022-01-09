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

        eprintln!("{:?}", unkowns.first_order_unkowns);

        dfs.visited_instructions(pdg, cfg, |def, instr| {
            if let Op::Call(call) = instr.op {
                eprintln!("call {:?}", call);
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
        for def in dug.dependent_defs_rec_postorder(entrys) {
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

        res
    }
}
