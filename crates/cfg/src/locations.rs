use std::fmt::{self, Formatter};
use std::iter::once;
use std::ops::Index;

use stdx::{impl_debug, impl_idx_from, impl_idx_math};
use typed_index_collections::TiVec;

use crate::{BasicBlock, ControlFlowGraph, InstIdx, PhiIdx};

/// IntLocation represent a unique location in the control flow graph
/// A location either refers to a terminator, a phi instruction or a (normal) instruction
#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct IntLocation(u32);
impl_idx_from!(IntLocation(u32));
impl_idx_math!(IntLocation(u32));
impl_debug!(match IntLocation{param => "loc{}",param.0;});

pub struct BlockLocations {
    pub phi_start: IntLocation,
    pub instr_start: IntLocation,
    pub terminator: IntLocation,
}

impl fmt::Debug for BlockLocations {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.phi_start != self.instr_start {
            write!(f, "Phis from {:?} to {:?};", self.phi_start, self.instr_start)?
        }

        if self.instr_start != self.terminator {
            write!(f, "Stmts from {:?} to {:?};", self.instr_start, self.terminator)?
        }

        write!(f, "Terminator at {:?}", self.terminator)
    }
}

pub struct InternedLocations {
    pub locations: TiVec<IntLocation, Location>,
    pub blocks: TiVec<BasicBlock, BlockLocations>,
}

impl_debug!(match InternedLocations{InternedLocations{blocks,..} => "{:?}",blocks;});

impl InternedLocations {
    pub fn len(&self) -> usize {
        self.locations.len()
    }

    pub fn is_empty(&self) -> bool {
        self.locations.is_empty()
    }
    pub fn next_location(&self) -> IntLocation {
        self.locations.next_key()
    }
}

impl Index<IntLocation> for InternedLocations {
    type Output = Location;

    fn index(&self, interned: IntLocation) -> &Self::Output {
        &self.locations[interned]
    }
}

impl Index<BasicBlock> for InternedLocations {
    type Output = BlockLocations;

    fn index(&self, index: BasicBlock) -> &Self::Output {
        &self.blocks[index]
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct Location {
    pub block: BasicBlock,
    pub kind: LocationKind,
}

impl_debug!(match Location{Location{block,kind} => "{:?} -> {:?}", block,kind;});

#[derive(PartialOrd, Ord, Clone, Copy, Eq, PartialEq, Hash)]
pub enum LocationKind {
    // DO NOT CHANGE THE ORDER OF THE ENUM DISCRIMINANTS. IT IS RELIED UPON BY THE PartialOrd/Ord derivce!
    Phi(PhiIdx),
    Instruction(InstIdx),
    Terminator,
}

impl_debug! {
    match LocationKind{
        LocationKind::Phi(phi) => "{:?}",phi;
        LocationKind::Instruction(inst) => "{:?}",inst;
        LocationKind::Terminator => "terminator";
    }
}

impl ControlFlowGraph {
    pub fn locations(&self) -> impl Iterator<Item = Location> + '_ {
        self.blocks.iter_enumerated().flat_map(|(id, block)| {
            let phi_locations = block
                .phis
                .keys()
                .map(move |phi| Location { block: id, kind: LocationKind::Phi(phi) });

            let stmnt_locations = block
                .instructions
                .keys()
                .map(move |instr| Location { block: id, kind: LocationKind::Instruction(instr) });

            phi_locations
                .chain(stmnt_locations)
                .chain(once(Location { block: id, kind: LocationKind::Terminator }))
        })
    }

    pub fn intern_locations(&mut self) -> InternedLocations {
        let locations: TiVec<_, _> = self.locations().collect();
        let mut blocks: TiVec<BasicBlock, BlockLocations> = TiVec::with_capacity(self.blocks.len());
        let mut start = IntLocation::from(0u32);
        for (id, location) in locations.iter_enumerated() {
            if location.block != blocks.next_key() {
                blocks.push(BlockLocations {
                    phi_start: start,
                    instr_start: start + self.blocks[blocks.next_key()].phis.len(),
                    terminator: id - 1u32,
                });
                start = id;
            }
        }
        InternedLocations { locations, blocks }
    }
}
