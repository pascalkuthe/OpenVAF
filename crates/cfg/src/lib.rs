//! The `ControlFlowGraph` represents the central struct of the OpenVAF middle end.
//! A control flow graph is a standard structure in compiler design
//! (see [here](https://en.wikipedia.org/wiki/Control-flow_graph) for an intruction).
//!
//! The implementation in this crate is completly independent from the frontend of the compiler.
//! Instead of associating elemts of the Cfg directly with language elements abstract ids are
//! used to represent info that does not strictly pretain to the control flow graph.
//! There are multiple reasons for this:
//!
//! * MUCH faster compile times (middleend and frontend can compile in parallel)
//!
//! * Decoupled compoenents allow refactoring the middle and frontend independently. A close
//!   coupling between middleend and frontend has caused problems in the past (essentially changing
//!   the AST requires in the entire compiler)
//!
//! * There are multiple applications built upon OpenVAF (primarly osdic and verilogae).
//!   These implementations have different requirements which past implementations that were
//!   coupled to the frontend have failed to adequatly meet.
//!
//! * osdic splits the primary cfg into multiple cfgs that correspond to different stages in the
//!   simulator. These control flow graphs all have different inputs, outputs and the same
//!   and some functions behave different (or are unavialble) in some of these cfgs.

use std::{
    iter::once,
    ops::{Index, IndexMut},
};

use ahash::AHashMap;
use arena::{Arena, Idx};
use arrayvec::ArrayVec;
use graph_cyclical_cache::GraphIsCyclicCache;

use stdx::{impl_debug, impl_idx_from};
use transversal::{
    Postorder, PostorderIter, PostorderIterMut, ReversePostorder, ReversePostorderIter,
    ReversePostorderIterMut,
};
use typed_index_collections::TiVec;

use predecessors::PredecessorCache;

pub use locations::{BlockLocations, IntLocation, InternedLocations, Location, LocationKind};
pub use op::Op;
pub use ty::{Const, Ty};

mod graph_cyclical_cache;
mod locations;
mod predecessors;
mod ty;

mod op;
mod parse;
mod pretty;
pub mod transversal;

#[cfg(test)]
mod tests;

/// Cfg Parameters represent immutable input to a Cfg
/// If the cfg compiled to a function these usually make up the parameters of that resulting
/// functions
#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct CfgParam(u32);
impl_idx_from!(CfgParam(u32));
impl_debug!(match CfgParam{param => "p{}",param.0;});

/// Callbacks allow the CFG to call external function (for example belonging to the circuit
/// simulator). Furthermore these callbacks may be used to abstractly represent functions
/// That are later resolved to actual instructions based upon how the CFG is further processed
#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct Callback(u32);
impl_idx_from!(Callback(u32));
impl_debug!(match Callback{input => "cb{}",input.0;});

/// A Place represents a region of memory that can be accessed by the CFG
/// Usually these correspond to the variables and (branches) defined by the user.
/// However more places are also added when deriavtives are calculated
#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct Place(u32);
impl_idx_from!(Place(u32));
impl_debug!(match Place{local => "place{}",local.0;});

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct Local(u32);
impl_idx_from!(Local(u32));
impl_debug!(match Local{local => "_{}",local.0;});

#[derive(PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct BasicBlock(u32);
impl_idx_from!(BasicBlock(u32));
impl_debug!(match BasicBlock{input => "bb{}",input.0;});

#[derive(Clone, Debug)]
pub struct BasicBlockData {
    pub phis: Arena<Phi>,
    pub instructions: Arena<Instruction>,
    pub terminator: Option<Terminator>,
}

impl BasicBlockData {
    #[must_use]
    #[inline]
    pub fn terminator(&self) -> &Terminator {
        self.terminator.as_ref().unwrap()
    }

    #[must_use]
    #[inline]
    pub fn terminator_mut(&mut self) -> &mut Terminator {
        self.terminator.as_mut().unwrap()
    }

    #[must_use]
    #[inline]
    pub fn successors(&self) -> Successors {
        self.terminator().successors()
    }

    #[must_use]
    #[inline]
    pub fn successors_mut(&mut self) -> SuccessorsMut {
        self.terminator_mut().successors_mut()
    }

    pub fn visit_operands(&self, mut f: impl FnMut(&Operand)) {
        for instruction in &*self.instructions {
            instruction.visit_operands(&mut f)
        }
        if let Some(term) = &self.terminator {
            term.visit_operands(f)
        }
    }

    pub fn visit_operands_mut(&mut self, mut f: impl FnMut(&mut Operand)) {
        for instruction in &mut *self.instructions {
            instruction.visit_operands_mut(&mut f)
        }

        if let Some(term) = &mut self.terminator {
            term.visit_operands_mut(f)
        }
    }

    pub fn visit_locals(&self, mut f: impl FnMut(Local)) {
        for phi in &*self.phis {
            phi.visit_locals(&mut f)
        }
        for instruction in &*self.instructions {
            instruction.visit_locals(&mut f)
        }
        if let Some(term) = &self.terminator {
            term.visit_operands(|op| {
                if let Operand::Copy(local) = op {
                    f(*local)
                }
            })
        }
    }

    pub fn visit_locals_mut(&mut self, mut f: impl FnMut(&mut Local)) {
        for phi in &mut *self.phis {
            phi.visit_locals_mut(&mut f)
        }
        for instruction in &mut *self.instructions {
            instruction.visit_locals_mut(&mut f)
        }
        if let Some(term) = &mut self.terminator {
            term.visit_operands_mut(|op| {
                if let Operand::Copy(local) = op {
                    f(local)
                }
            })
        }
    }
}

pub type PhiIdx = Idx<Phi>;

#[derive(Clone)]
pub struct Phi {
    pub dst: Local,
    pub sources: AHashMap<BasicBlock, Local>,
}

impl_debug!(match Phi{Phi{dst,sources} => "{:?} := {:?}",dst,{let mut src: Vec<_> = sources.iter().collect();src.sort_unstable_by_key(|(bb,_)|*bb);src};});

impl Phi {
    pub fn visit_locals(&self, mut f: impl FnMut(Local)) {
        f(self.dst);
        for src in self.sources.values() {
            f(*src)
        }
    }

    pub fn visit_locals_mut(&mut self, mut f: impl FnMut(&mut Local)) {
        f(&mut self.dst);
        for src in self.sources.values_mut() {
            f(src)
        }
    }
}

pub type InstIdx = Idx<Instruction>;

#[derive(Clone, Debug)]
pub struct Instruction {
    pub dst: Option<Local>,
    pub op: Op,
    pub args: Box<[Operand]>,
    pub src: u32,
}

impl Instruction {
    pub fn visit_operands(&self, mut f: impl FnMut(&Operand)) {
        for arg in &*self.args {
            f(arg)
        }
    }

    pub fn visit_operands_mut(&mut self, mut f: impl FnMut(&mut Operand)) {
        for arg in &mut *self.args {
            f(arg)
        }
    }

    pub fn visit_locals(&self, mut f: impl FnMut(Local)) {
        if let Some(dst) = self.dst {
            f(dst)
        }
        self.visit_operands(|op| {
            if let Operand::Copy(local) = op {
                f(*local)
            }
        })
    }

    pub fn visit_locals_mut(&mut self, mut f: impl FnMut(&mut Local)) {
        if let Some(dst) = &mut self.dst {
            f(dst)
        }
        self.visit_operands_mut(|op| {
            if let Operand::Copy(local) = op {
                f(local)
            }
        })
    }
}

#[derive(Clone, PartialEq)]
pub enum Operand {
    Const(Const),
    Copy(Local),
    Read(CfgParam),
}

impl_debug! {
    match Operand{
        Operand::Const(con) => "{:?}", con;
        Operand::Copy(local) => "{:?}", local;
        Operand::Read(param) => "{:?}", param;
    }
}

#[derive(Clone, PartialEq)]
pub enum Terminator {
    Goto(BasicBlock),
    Split { condition: Operand, true_block: BasicBlock, false_block: BasicBlock, loop_head: bool },
    End,
}

impl_debug! {
    match Terminator{
        Terminator::Goto(dst) => "goto {:?};", dst;
        Terminator::End => "end";
        Terminator::Split{condition,true_block,false_block,loop_head}
            => "if {:?} {{ {:?} }} else {{ {:?} }} {}", condition, true_block, false_block, if *loop_head { "(loop)" } else { "" };
    }
}

pub type Successors = ArrayVec<BasicBlock, 2>;
pub type SuccessorsMut<'a> = ArrayVec<&'a mut BasicBlock, 2>;

impl Terminator {
    #[must_use]
    #[inline]
    pub fn successors(&self) -> Successors {
        let mut res = ArrayVec::new();
        match *self {
            Self::End => (),
            Self::Goto(dst) => res.push(dst),
            Self::Split { true_block, false_block, .. } => {
                res.push(true_block);
                res.push(false_block)
            }
        }
        res
    }

    #[must_use]
    #[inline]
    pub fn successors_mut(&mut self) -> SuccessorsMut {
        let mut res = ArrayVec::new();
        match self {
            Self::End => (),
            Self::Goto(dst) => res.push(dst),
            Self::Split { true_block, false_block, .. } => {
                res.push(true_block);
                res.push(false_block)
            }
        }
        res
    }

    #[inline]
    pub fn visit_bb(&self, mut f: impl FnMut(BasicBlock)) {
        match self {
            Self::Goto(dst) => f(*dst),
            Self::Split { true_block, false_block, .. } => {
                f(*true_block);
                f(*false_block);
            }

            Self::End => (),
        }
    }

    #[inline]
    pub fn visit_bb_mut(&mut self, mut f: impl FnMut(&mut BasicBlock)) {
        match self {
            Self::Goto(dst) => f(dst),
            Self::Split { true_block, false_block, .. } => {
                f(true_block);
                f(false_block);
            }

            Self::End => (),
        }
    }

    #[inline]
    pub fn visit_operands(&self, mut f: impl FnMut(&Operand)) {
        if let Terminator::Split { condition, .. } = self {
            f(condition)
        }
    }

    #[inline]
    pub fn visit_operands_mut(&mut self, mut f: impl FnMut(&mut Operand)) {
        if let Terminator::Split { condition, .. } = self {
            f(condition)
        }
    }

    pub fn is_loop_head(&self) -> bool {
        matches!(self, Self::Split { loop_head: true, .. })
    }
}

#[derive(Clone, Debug)]
pub struct ControlFlowGraph {
    pub mut_locals: AHashMap<Local, Place>,
    pub places: TiVec<Place, Local>,
    next_local: Local,
    pub blocks: TiVec<BasicBlock, BasicBlockData>,
    pub predecessor_cache: PredecessorCache,
    pub is_cyclic: GraphIsCyclicCache,
}

impl Default for ControlFlowGraph {
    fn default() -> Self {
        Self {
            mut_locals: Default::default(),
            places: Default::default(),
            next_local: Local(0),
            blocks: Default::default(),
            predecessor_cache: Default::default(),
            is_cyclic: Default::default(),
        }
    }
}

impl ControlFlowGraph {
    #[inline(always)]
    #[must_use]
    pub const fn entry(&self) -> BasicBlock {
        BasicBlock(0)
    }

    #[inline(always)]
    #[must_use]
    pub fn exit(&self) -> BasicBlock {
        self.blocks.last_key().unwrap()
    }

    pub fn is_cyclic(&self) -> bool {
        self.is_cyclic.is_cyclic(self)
    }

    pub fn predecessors(&self, bb: BasicBlock) -> &[BasicBlock] {
        &self.predecessor_cache.compute(&self.blocks)[bb]
    }

    #[must_use]
    #[inline]
    pub fn successors(&self, bb: BasicBlock) -> Successors {
        self.blocks[bb].successors()
    }

    /// Provides mutable acccess to block successors
    /// Note that if any changes are performed to the successors then
    /// predecessor cache must be invalidated with `cfg.predecessor_cache.invalidate()`
    /// if the graph is simplified such that loops might have been removed also call `cfg.is_cyclic.invalidate()`
    #[must_use]
    #[inline]
    pub fn successors_mut(&mut self, bb: BasicBlock) -> SuccessorsMut {
        self.blocks[bb].successors_mut()
    }

    pub fn visit_operands(&self, mut f: impl FnMut(&Operand)) {
        for block in &self.blocks {
            block.visit_operands(&mut f)
        }
    }

    pub fn visit_operands_mut(&mut self, mut f: impl FnMut(&mut Operand)) {
        for block in &mut self.blocks {
            block.visit_operands_mut(&mut f)
        }
    }

    pub fn visit_locals(&self, mut f: impl FnMut(Local)) {
        for block in &self.blocks {
            block.visit_locals(&mut f)
        }
    }

    pub fn visit_locals_mut(&mut self, mut f: impl FnMut(&mut Local)) {
        for block in &mut self.blocks {
            block.visit_locals_mut(&mut f)
        }
    }

    pub fn new_temporary(&mut self) -> Local {
        let next = self.next_local;
        self.next_local.0 += 1u32;
        next
    }

    pub fn new_place(&mut self) -> (Place, Local) {
        let local = self.next_local;
        let place = self.places.push_and_get_key(local);
        self.mut_locals.insert(local, place);
        (place, local)
    }

    // transversals

    pub fn postorder(&self) -> Postorder {
        Postorder::new(self, self.entry())
    }

    pub fn postorder_iter(&self) -> PostorderIter {
        PostorderIter::new(self, self.entry())
    }

    pub fn postorder_itermut(&mut self) -> PostorderIterMut {
        PostorderIterMut::new(self, self.entry())
    }

    pub fn reverse_postorder(&self) -> ReversePostorder {
        ReversePostorder::new(self, self.entry())
    }

    pub fn reverse_postorder_iter(&self) -> ReversePostorderIter<'_> {
        ReversePostorderIter::new(self, self.entry())
    }

    pub fn reverse_postorder_itermut(&mut self) -> ReversePostorderIterMut<'_> {
        ReversePostorderIterMut::new(self, self.entry())
    }

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

impl Index<BasicBlock> for ControlFlowGraph {
    type Output = BasicBlockData;

    fn index(&self, bb: BasicBlock) -> &BasicBlockData {
        &self.blocks[bb]
    }
}

impl IndexMut<BasicBlock> for ControlFlowGraph {
    fn index_mut(&mut self, bb: BasicBlock) -> &mut BasicBlockData {
        &mut self.blocks[bb]
    }
}
