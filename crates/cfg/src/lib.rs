//! The `ControlFlowGraph` represents the central data structure of the OpenVAF middle end.
//! A control flow graph is a standard structure in compiler design
//! (see [here](https://en.wikipedia.org/wiki/Control-flow_graph) for an instruction).
//!
//! The implementation in this crate is completely independent from the front-end (Ast/HIR) of the compiler.
//! Instead of associating elements of the CFG directly with language elements abstract ids are
//! used to represent info that does not strictly pertain to the control flow graph.
//! There are multiple reasons for this:
//!
//! * MUCH faster compile times (middle-end and front-end can compile in parallel)
//!
//! * Decoupled components allow refactoring the middle and front-end independently. The tight coupling
//!   between middle-end and front-end has caused problems in the past (changing
//!   the AST required changes in the entire compiler)
//!
//! * There are multiple applications built upon OpenVAF (OSDIc and VerilogAE).
//!   These implementations have different requirements which past implementations that were
//!   coupled to the front-end have failed to adequately meet.
//!
//! * osdic splits the primary CFG into multiple CFGs that correspond to different stages in the
//!   simulator. These control flow graphs all have different inputs, outputs and the same
//!   and some functions behave different (or are unavailable) in some of these CFGs.

use std::hash::Hash;
use std::ops::{Index, IndexMut};

use ahash::AHashMap;
use arena::{Arena, Idx};
use arrayvec::ArrayVec;
pub use builder::CfgBuilder;
pub use lasso::Spur;
use stdx::{impl_debug, impl_from, impl_idx_from};
use typed_index_collections::TiVec;

use crate::graph_cyclical_cache::GraphIsCyclicCache;
pub use crate::locations::{
    BlockLocations, IntLocation, InternedLocations, Location, LocationKind,
};
pub use crate::op::Op;
use crate::predecessors::PredecessorCache;
use crate::transversal::{
    Postorder, PostorderIter, PostorderIterMut, ReversePostorder, ReversePostorderIter,
    ReversePostorderIterMut,
};
pub use crate::ty::{Complex64, Const};

#[cfg(test)]
mod tests;

mod builder;
mod graph_cyclical_cache;
mod locations;
mod op;
mod parse;
mod predecessors;
mod pretty;
pub mod transversal;
mod ty;

/// CFG Parameters represent immutable input to a CFG
/// If the CFG compiled to a function these usually make up the parameters of that resulting
/// functions
#[derive(PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct CfgParam(u32);
impl_idx_from!(CfgParam(u32));
impl_debug!(match CfgParam{param => "#{}",param.0;});

/// Callbacks allow the CFG to call external function (for example belonging to the circuit
/// simulator). Furthermore these callbacks may be used to abstractly represent functions
/// That are later resolved to actual instructions based upon how the CFG is further processed
#[derive(PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct Callback(pub u32);
impl_idx_from!(Callback(u32));
impl_debug!(match Callback{input => "cb{}",input.0;});

/// A Place represents a region of memory that can be accessed by the CFG
/// Usually these correspond to the variables and (branches) defined by the user.
/// However more places are also added when derivatives are calculated
#[derive(PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct Place(u32);
impl_idx_from!(Place(u32));
impl_debug!(match Place{local => "p{}",local.0;});

#[derive(PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct Local(u32);
impl_idx_from!(Local(u32));
impl_debug!(match Local{local => "_{}",local.0;});

#[derive(PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct BasicBlock(u32);
impl_idx_from!(BasicBlock(u32));
impl_debug!(match BasicBlock{input => "bb{}",input.0;});

#[derive(Clone, Debug, Default)]
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

    pub fn visit_data(
        &self,
        mut f_local: impl FnMut(Local),
        mut f_place: impl FnMut(Place),
        mut f_param: impl FnMut(CfgParam),
    ) {
        for phi in &*self.phis {
            phi.visit_locals(&mut f_local)
        }
        for instruction in &*self.instructions {
            instruction.visit_operands(|operand| match operand {
                Operand::Local(local) => f_local(*local),
                Operand::Place(place) => f_place(*place),
                Operand::CfgParam(param) => f_param(*param),
                Operand::Const(_) => (),
            });
            match instruction.dst {
                InstrDst::Local(local) => f_local(local),
                InstrDst::Place(place) => f_place(place),
                InstrDst::Ignore => (),
            }
        }
        if let Some(term) = &self.terminator {
            term.visit_operands(|operand| match operand {
                Operand::Local(local) => f_local(*local),
                Operand::Place(place) => f_place(*place),
                Operand::CfgParam(param) => f_param(*param),
                Operand::Const(_) => (),
            });
        }
    }

    pub fn visit_data_mut(
        &mut self,
        mut f_local: impl FnMut(&mut Local),
        mut f_place: impl FnMut(&mut Place),
        mut f_param: impl FnMut(&mut CfgParam),
    ) {
        for phi in &mut *self.phis {
            phi.visit_locals_mut(&mut f_local)
        }
        for instruction in &mut *self.instructions {
            instruction.visit_operands_mut(|operand| match operand {
                Operand::Local(local) => f_local(local),
                Operand::Place(place) => f_place(place),
                Operand::CfgParam(param) => f_param(param),
                Operand::Const(_) => (),
            });
            match &mut instruction.dst {
                InstrDst::Local(local) => f_local(local),
                InstrDst::Place(place) => f_place(place),
                InstrDst::Ignore => (),
            }
        }
        if let Some(term) = &mut self.terminator {
            term.visit_operands_mut(|operand| match operand {
                Operand::Local(local) => f_local(local),
                Operand::Place(place) => f_place(place),
                Operand::CfgParam(param) => f_param(param),
                Operand::Const(_) => (),
            });
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

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum InstrDst {
    Local(Local),
    Place(Place),
    Ignore,
}

impl_debug! {
    match InstrDst{
        InstrDst::Local(local) => "{:?}", local;
        InstrDst::Place(place) => "{:?}", place;
        InstrDst::Ignore => "_";
    }
}

impl TryFrom<InstrDst> for Operand {
    type Error = ();

    fn try_from(value: InstrDst) -> Result<Self, Self::Error> {
        match value {
            InstrDst::Local(local) => Ok(local.into()),
            InstrDst::Place(place) => Ok(place.into()),
            InstrDst::Ignore => Err(()),
        }
    }
}

impl_from!(Local,Place for InstrDst);

pub type InstIdx = Idx<Instruction>;

#[derive(Clone, Debug)]
pub struct Instruction {
    pub dst: InstrDst,
    pub op: Op,
    pub args: Box<[Operand]>,
    pub src: i32,
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
        if let InstrDst::Local(dst) = self.dst {
            f(dst)
        }
        self.visit_operands(|op| {
            if let Operand::Local(local) = op {
                f(*local)
            }
        })
    }

    pub fn visit_locals_mut(&mut self, mut f: impl FnMut(&mut Local)) {
        if let InstrDst::Local(dst) = &mut self.dst {
            f(dst)
        }
        self.visit_operands_mut(|op| {
            if let Operand::Local(local) = op {
                f(local)
            }
        })
    }
}

#[derive(Clone, PartialEq)]
pub enum Operand {
    Const(Const),
    Local(Local),
    Place(Place),
    CfgParam(CfgParam),
}

impl Operand {
    pub fn unwrap_cfg_param(self) -> CfgParam {
        match self {
            Operand::CfgParam(param) => param,
            _ => unreachable!("called unwrap_cfg_param on {:?}", self),
        }
    }

    pub fn unwrap_place(self) -> Place {
        match self {
            Operand::Place(place) => place,
            _ => unreachable!("called unwrap_place on {:?}", self),
        }
    }
}

impl_debug! {
    match Operand{
        Operand::Const(con) => "{:?}", con;
        Operand::Local(local) => "{:?}", local;
        Operand::CfgParam(param) => "{:?}", param;
        Operand::Place(place) => "{:?}",place;
    }
}

impl_from!(Const,Local,CfgParam,Place for Operand);

impl From<bool> for Operand {
    fn from(val: bool) -> Self {
        Operand::Const(val.into())
    }
}

impl From<f64> for Operand {
    fn from(val: f64) -> Self {
        Operand::Const(val.into())
    }
}

impl From<i32> for Operand {
    fn from(val: i32) -> Self {
        Operand::Const(val.into())
    }
}

impl From<Spur> for Operand {
    fn from(val: Spur) -> Self {
        Operand::Const(val.into())
    }
}

// impl From<Array<f64>> for Operand {
//     fn from(val: Array<f64>) -> Self {
//         Operand::Const(val.into())
//     }
// }

// impl From<Array<i32>> for Operand {
//     fn from(val: Array<i32>) -> Self {
//         Operand::Const(val.into())
//     }
// }

// impl From<Array<Spur>> for Operand {
//     fn from(val: Array<Spur>) -> Self {
//         Operand::Const(val.into())
//     }
// }

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
    pub next_local: Local,
    pub next_place: Place,
    pub blocks: TiVec<BasicBlock, BasicBlockData>,
    pub predecessor_cache: PredecessorCache,
    pub is_cyclic: GraphIsCyclicCache,
}

impl Default for ControlFlowGraph {
    fn default() -> Self {
        Self {
            next_local: Local(0),
            blocks: Default::default(),
            predecessor_cache: Default::default(),
            is_cyclic: Default::default(),
            next_place: Place(0),
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

    pub fn new_place(&mut self) -> Place {
        let res = self.next_place;
        self.next_place.0 += 1;
        res
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

    pub fn visit_data(
        &self,
        mut f_local: impl FnMut(Local),
        mut f_place: impl FnMut(Place),
        mut f_param: impl FnMut(CfgParam),
    ) {
        for block in &self.blocks {
            block.visit_data(&mut f_local, &mut f_place, &mut f_param)
        }
    }

    pub fn visit_data_mut(
        &mut self,
        mut f_local: impl FnMut(&mut Local),
        mut f_place: impl FnMut(&mut Place),
        mut f_param: impl FnMut(&mut CfgParam),
    ) {
        for block in &mut self.blocks {
            block.visit_data_mut(&mut f_local, &mut f_place, &mut f_param)
        }
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
            block.visit_data(&mut f, |_| (), |_| ())
        }
    }

    pub fn visit_locals_mut(&mut self, mut f: impl FnMut(&mut Local)) {
        for block in &mut self.blocks {
            block.visit_data_mut(&mut f, |_| (), |_| ())
        }
    }

    pub fn new_local(&mut self) -> Local {
        let next = self.next_local;
        self.next_local.0 += 1u32;
        next
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
