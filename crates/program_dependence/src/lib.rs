use bitset::SparseBitMatrix;
use cfg::{BasicBlock, ControlFlowGraph, InstIdx, InstrDst, Local, Location, LocationKind, Place};

use data_flow::GenKillAnalysis;
use once_cell::sync::OnceCell;
use stdx::{impl_debug, impl_idx_from, impl_idx_math};
use typed_index_collections::{TiSlice, TiVec};
use typed_indexmap::TiSet;

pub use crate::control_dependence::{control_dependence, ControlDependenceGraph};
pub use crate::def_use::DefUseGraph;
pub use crate::post_dominators::{post_dominators, PostDominators};
pub use crate::reaching_definitions::{ReachingDefinitions, ReachingDefintionsAnalysis};
pub use crate::use_def::UseDefGraph;

mod control_dependence;
pub mod def_use;
mod post_dominators;
mod reaching_definitions;
pub mod use_def;

#[cfg(test)]
mod tests;

#[derive(PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct Assignment(u32);
impl_idx_from!(Assignment(u32));
impl_idx_math!(Assignment(u32));
impl_debug!(match Assignment{param => "asssign{}",param.0;});

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct AssigmentLoc {
    pub bb: BasicBlock,
    pub instr: InstIdx,
}

impl_debug! {
    match AssigmentLoc{
        AssigmentLoc{bb, instr} => "{:?} -> {:?}", bb,instr;
    }
}

impl From<AssigmentLoc> for Location {
    fn from(loc: AssigmentLoc) -> Self {
        Location { bb: loc.bb, kind: LocationKind::Instruction(loc.instr) }
    }
}

#[derive(Debug, Clone)]
pub struct AssigmentInterner {
    pub assigment_locations: TiSet<Assignment, AssigmentLoc>,
    pub local_defs: Box<TiSlice<Local, Location>>,
    pub place_assigments: SparseBitMatrix<Place, Assignment>,
}

impl AssigmentInterner {
    pub fn new(cfg: &ControlFlowGraph) -> AssigmentInterner {
        // for this purpose LocationKind::Terminator == UNDEF
        let mut local_defs =
            TiVec::from(vec![
                Location { bb: 0u32.into(), kind: LocationKind::Terminator };
                cfg.next_local.into()
            ])
            .into_boxed_slice();

        let mut assigment_locations = TiSet::with_capacity(cfg.next_place.into());
        for (bb, data) in cfg.blocks.iter_enumerated() {
            for (phi, data) in data.phis.iter_enumerated() {
                local_defs[data.dst] = Location { bb, kind: LocationKind::Phi(phi) };
            }

            assigment_locations.raw.extend(data.instructions.iter_enumerated().filter_map(
                |(id, instr)| match instr.dst {
                    InstrDst::Place(_) => Some(AssigmentLoc { bb, instr: id }),
                    InstrDst::Local(local) => {
                        local_defs[local] = Location { bb, kind: LocationKind::Instruction(id) };
                        None
                    }
                    InstrDst::Ignore => None,
                },
            ))
        }

        let mut place_assigments =
            SparseBitMatrix::new(cfg.next_place.into(), assigment_locations.len());

        for (assigment, loc) in assigment_locations.iter_enumerated() {
            if let InstrDst::Place(place) = cfg[loc.bb].instructions[loc.instr].dst {
                place_assigments.insert(place, assigment);
            }
        }

        AssigmentInterner { assigment_locations, local_defs, place_assigments }
    }
}

#[derive(Debug, Clone)]
pub struct ProgramDependenGraph<'a> {
    pub reaching_definitions: ReachingDefinitions<'a>,
    pub control_dependence: ControlDependenceGraph,
    pub inv_control_dependence: OnceCell<ControlDependenceGraph>,
    pub use_def_graph: UseDefGraph,
    pub def_use_graph: OnceCell<DefUseGraph>,
}

impl<'a> ProgramDependenGraph<'a> {
    pub fn build(assignments: &'a AssigmentInterner, cfg: &ControlFlowGraph) -> Self {
        let reaching_definitions = ReachingDefintionsAnalysis { intern: assignments }
            .into_engine(cfg)
            .iterate_to_fixpoint();
        let ipdom = post_dominators(cfg);
        let control_dependence = control_dependence(&ipdom, cfg);
        let use_def_graph = UseDefGraph::build(cfg, &reaching_definitions);

        ProgramDependenGraph {
            reaching_definitions,
            control_dependence,
            inv_control_dependence: OnceCell::new(),
            use_def_graph,
            def_use_graph: OnceCell::new(),
        }
    }

    pub fn interner(&self) -> &'a AssigmentInterner {
        self.reaching_definitions.analysis.0.intern
    }

    pub fn inv_control_dependence(&self) -> &ControlDependenceGraph {
        self.inv_control_dependence.get_or_init(|| self.control_dependence.inverse())
    }

    pub fn def_use_graph(&self) -> &DefUseGraph {
        self.def_use_graph.get_or_init(|| DefUseGraph::build(&self.use_def_graph))
    }
}
