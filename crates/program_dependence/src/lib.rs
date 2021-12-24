use bitset::SparseBitMatrix;
use cfg::{BasicBlock, ControlFlowGraph, InstIdx, InstrDst, Local, Location, LocationKind, Place};

use data_flow::GenKillAnalysis;
use once_cell::unsync::OnceCell;
use stdx::{impl_debug, impl_idx_from, impl_idx_math};
use typed_index_collections::{TiSlice, TiVec};
use typed_indexmap::TiSet;

pub use crate::control_dependence::{control_dependence, ControlDependenceGraph};
pub use crate::def_use::DefUseGraph;
pub use crate::post_dominators::{post_dominators, PostDominators};
pub use crate::reaching_definitions::{ReachingDefinitions, ReachingDefintionsAnalysis};
pub use crate::use_def::UseDefVisitor;

mod control_dependence;
mod dead_code_elemination;
mod def_use;
mod post_dominators;
mod reaching_definitions;
mod use_def;

#[derive(PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct Assigment(u32);
impl_idx_from!(Assigment(u32));
impl_idx_math!(Assigment(u32));
impl_debug!(match Assigment{param => "asssign{}",param.0;});

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct AssigmentLoc {
    bb: BasicBlock,
    instr: InstIdx,
}

impl From<AssigmentLoc> for Location {
    fn from(loc: AssigmentLoc) -> Self {
        Location { block: loc.bb, kind: LocationKind::Instruction(loc.instr) }
    }
}

#[derive(Debug, Clone)]
pub struct AssigmentInterner {
    pub assigment_locations: TiSet<Assigment, AssigmentLoc>,
    pub local_defs: Box<TiSlice<Local, Location>>,
    pub place_assigments: SparseBitMatrix<Place, Assigment>,
}

impl AssigmentInterner {
    pub fn new(cfg: &ControlFlowGraph) -> AssigmentInterner {
        // for this purpose LocationKind::Terminator == UNDEF
        let mut local_defs =
            TiVec::from(vec![
                Location { block: 0u32.into(), kind: LocationKind::Terminator };
                cfg.next_local.into()
            ])
            .into_boxed_slice();

        let mut assigment_locations = TiSet::with_capacity(cfg.next_place.into());
        for (bb, data) in cfg.blocks.iter_enumerated() {
            for (phi, data) in data.phis.iter_enumerated() {
                local_defs[data.dst] = Location { block: bb, kind: LocationKind::Phi(phi) };
            }

            assigment_locations.raw.extend(data.instructions.iter_enumerated().filter_map(
                |(id, instr)| match instr.dst {
                    InstrDst::Place(_) => Some(AssigmentLoc { bb, instr: id }),
                    InstrDst::Local(local) => {
                        local_defs[local] =
                            Location { block: bb, kind: LocationKind::Instruction(id) };
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
    pub def_use_graph: Option<DefUseGraph>,
}

impl<'a> ProgramDependenGraph<'a> {
    pub fn build(
        assignments: &'a AssigmentInterner,
        cfg: &ControlFlowGraph,
        ipdom: &PostDominators,
    ) -> Self {
        let reaching_definitions =
            ReachingDefintionsAnalysis { assignments }.into_engine(cfg).iterate_to_fixpoint();
        let control_dependence = control_dependence(ipdom, cfg);

        ProgramDependenGraph {
            reaching_definitions,
            control_dependence,
            inv_control_dependence: OnceCell::new(),
            def_use_graph: None,
        }
    }

    pub fn interner(&self) -> &AssigmentInterner {
        self.reaching_definitions.analysis.0.assignments
    }

    pub fn inv_control_dependence(&self) -> &ControlDependenceGraph {
        self.inv_control_dependence.get_or_init(|| self.control_dependence.inverse())
    }
}
