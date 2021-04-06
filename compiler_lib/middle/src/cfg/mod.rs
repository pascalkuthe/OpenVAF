//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use openvaf_data_structures::index_vec::IndexVec;
use openvaf_ir::ids::{StatementId, SyntaxCtx};
use openvaf_ir::{id_type, impl_id_type, Unknown};

use crate::cfg::predecessors::PredecessorCache;
use crate::cfg::transversal::ReversePostorderIterMut;
use crate::cfg::transversal::{
    Postorder, PostorderIter, PostorderIterMut, ReversePostorder, ReversePostorderIter,
};
use crate::{COperand, CallType, CallTypeConversion, CallTypeDerivative, Derivative,  InputKind, Local, LocalDeclaration, LocalKind, Mir,  OperandData, ParameterCallType, ParameterInput, RValue, Statement, StmntKind, VariableLocalKind, Spanned};
use openvaf_data_structures::HashMap;
use osdi_types::Type;

use crate::util::AtMostTwoIter;
use openvaf_ir::convert::Convert;
use osdi_types::ConstVal::Scalar;
use osdi_types::SimpleConstVal::Real;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::iter::once;
use std::ops::Index;
use tracing::Span;
use crate::cfg::builder::CfgBuilder;

pub mod predecessors;
pub mod transversal;

pub mod builder;
#[cfg(feature = "graphviz")]
mod graphviz;
mod print;

// Macros that call functions that produce a cfg with more or less blocks (and therefore a new tag)
id_type!(BasicBlock(u16));
id_type!(LocationId(u32));
id_type!(Phi(u16));

impl_id_type!(BasicBlock in ControlFlowGraph<C> => blocks as BasicBlockData<C> where <C: CallType>);

#[derive(Debug)]
pub struct BlockLocations {
    pub phi_start: LocationId,
    pub stmnt_start: LocationId,
    pub terminator: LocationId,
}

#[derive(Debug)]
pub struct InternedLocations {
    locations: IndexVec<LocationId, Location>,
    blocks: IndexVec<BasicBlock, BlockLocations>,
}

impl InternedLocations {
    pub fn len(&self) -> usize {
        self.locations.len()
    }
    pub fn len_idx(&self) -> LocationId {
        self.locations.len_idx()
    }
}

impl Index<LocationId> for InternedLocations {
    type Output = Location;

    fn index(&self, block: LocationId) -> &Self::Output {
        &self.locations[block]
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

impl Debug for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} -> {:?}", self.block, self.kind)
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub enum LocationKind {
    Phi(Phi),
    Statement(StatementId),
    Terminator,
}

impl Debug for LocationKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Phi(phi) => Debug::fmt(phi, f),
            Self::Statement(stmnt) => Debug::fmt(stmnt, f),
            Self::Terminator => f.write_str("terminator"),
        }
    }
}

pub type Successors = AtMostTwoIter<BasicBlock>;
pub type SuccessorsMut<'a> = AtMostTwoIter<&'a mut BasicBlock>;

#[derive(Clone, Debug, Default)]
pub struct ControlFlowGraph<C: CallType> {
    /// The external input that the ControlFlowGraph requires
    pub locals: IndexVec<Local, LocalDeclaration>,
    pub blocks: IndexVec<BasicBlock, BasicBlockData<C>>,
    pub derivatives: HashMap<Local, HashMap<Unknown, Local>>,
    pub predecessor_cache: PredecessorCache,
}

pub trait CfgPass<'a, C: CallType> {
    type Result: 'a;
    const NAME: &'static str;

    fn run(self, cfg: &'a mut ControlFlowGraph<C>) -> Self::Result;
    fn span(&self) -> Span;
}

#[macro_export]
macro_rules! impl_pass_span {
    ($name:literal) => {
        impl_pass_span!(self; $name,);
    };

    ($name:literal, $($field:tt)*) => {
        impl_pass_span!(self; $name, $($field:tt)*);
    };

    ($self:ident; $name:literal, $($field:tt)*) => {
        const NAME: &'static str = $name;
        fn span(&$self) -> tracing::Span{
            tracing::info_span!(target: "CfgPasses", $name, $($field)*)
        }
    }
}

pub const START_BLOCK: BasicBlock = BasicBlock::from_raw_unchecked(0);

// TODO branches?
impl<C> ControlFlowGraph<C>
where
    C: CallType + From<ParameterCallType>,
    C::I: From<ParameterInput>,
{
    pub fn insert_variable_declarations<A: CallType>(&mut self, mir: &Mir<A>) {
        let terminator = self.blocks[START_BLOCK].terminator.take().unwrap();

        let old_end = self.end();

        // clone is necessary since additional locals may be added (which we dont want to iterate anywau) which would require mutable aliasingg
        let old_locals = self.locals.clone();
        let mut builder = CfgBuilder::edit::<false, false>(self,START_BLOCK,0,0);

        for (local, decl) in old_locals.iter_enumerated() {
            if let LocalKind::Variable(var, kind) = decl.kind {
                let span = mir[mir.variables[var].sctx].span;
                let sctx = mir.variables[var].sctx;

                match kind {
                    VariableLocalKind::Derivative => {
                        builder.assign(local,RValue::Use(Spanned{span, contents: OperandData::Constant(Scalar(Real(0.0)))}), sctx);
                    }
                    VariableLocalKind::User => {
                        let default = mir.variables[var].default.borrow().clone();
                        let val = builder.insert_expr( sctx, default);
                        builder.assign(local,val, sctx);
                    }
                }
            }
        }

        let start_sctx = terminator.sctx;
        let mut terminator = terminator.kind;
        for bb in terminator.successors_mut(){
            if *bb == old_end{
                *bb = builder.cfg.end()
            }
        }

        builder.terminate(terminator, start_sctx);
        self.predecessor_cache.invalidate();
    }
}

impl<C: CallType> ControlFlowGraph<C> {
    pub fn empty() -> Self {
        Self {
            locals: IndexVec::new(),
            blocks: IndexVec::new(),
            derivatives: HashMap::new(),
            predecessor_cache: PredecessorCache::new(),
        }
    }
    pub fn new() -> Self {
        Self {
            locals: IndexVec::with_capacity(16),
            blocks: IndexVec::with_capacity(4),
            derivatives: HashMap::new(),
            predecessor_cache: PredecessorCache::new(),
        }
    }

    pub fn run_pass<'a, T>(&'a mut self, t: T) -> <T as CfgPass<'a, C>>::Result
    where
        T: CfgPass<'a, C>,
    {
        let span = t.span();
        let _enter = span.enter();
        let x = t.run(self);
        drop(_enter);
        x
    }

    // #[inline]
    // pub fn locals(&self) -> impl Iterator<Item = Local> + '_ {}

    #[inline]
    pub fn end(&self) -> BasicBlock {
        self.blocks.last_idx()
    }

    pub fn new_temporary(&mut self, ty: Type) -> Local {
        self.locals.push(LocalDeclaration {
            ty,
            kind: LocalKind::Temporary,
        })
    }

    // transversals

    pub fn postorder(&self) -> Postorder {
        Postorder::new(self, START_BLOCK)
    }

    pub fn postorder_iter(&self) -> PostorderIter<C> {
        PostorderIter::new(self, START_BLOCK)
    }

    pub fn postorder_itermut(&mut self) -> PostorderIterMut<C> {
        PostorderIterMut::new(self, START_BLOCK)
    }

    pub fn reverse_postorder(&self) -> ReversePostorder {
        ReversePostorder::new(self, START_BLOCK)
    }

    pub fn reverse_postorder_iter(&self) -> ReversePostorderIter<'_, C> {
        ReversePostorderIter::new(self, START_BLOCK)
    }

    pub fn reverse_postorder_itermut(&mut self) -> ReversePostorderIterMut<'_, C> {
        ReversePostorderIterMut::new(self, START_BLOCK)
    }

    pub fn locations(&self) -> impl Iterator<Item = Location> + '_ {
        self.blocks.iter_enumerated().flat_map(|(id, block)| {
            let phi_locations = block.phi_statements.indices().map(move |phi| Location {
                block: id,
                kind: LocationKind::Phi(phi),
            });

            let stmnt_locations = block.statements.indices().map(move |stmt| Location {
                block: id,
                kind: LocationKind::Statement(stmt),
            });

            phi_locations.chain(stmnt_locations).chain(once(Location {
                block: id,
                kind: LocationKind::Terminator,
            }))
        })
    }

    #[inline]
    pub fn for_locals(&self, mut f: impl FnMut(Local)) {
        for block in &self.blocks {
            block.for_locals(&mut f)
        }
    }

    #[inline]
    pub fn locals(&self) -> impl Iterator<Item = Local> {
        // This is mostly three adress code
        let mut buff = Vec::with_capacity(3);
        self.for_locals(|local| buff.push(local));
        buff.into_iter()
    }

    #[inline]
    pub fn for_locals_mut(&mut self, mut f: impl FnMut(&mut Local)) {
        for block in &mut self.blocks {
            block.for_locals_mut(&mut f)
        }
    }

    #[inline]
    pub fn locals_mut(&mut self) -> impl Iterator<Item = &mut Local> {
        // This is mostly three adress code
        let mut buff = Vec::with_capacity(3);
        // This is save since each local is an individual location in memory the interface of the for_locals funciton just doesn't capture that
        self.for_locals_mut(|local| buff.push(local as *mut Local));
        buff.into_iter().map(|local_ptr| unsafe { &mut *local_ptr })
    }

    pub fn map<X: CallType>(
        self,
        conversion: &mut impl CallTypeConversion<C, X>,
    ) -> ControlFlowGraph<X> {
        let blocks = self
            .blocks
            .into_iter()
            .map(|bb| {
                let statements = bb
                    .statements
                    .into_iter()
                    .map(|(kind, sctx)| {
                        let kind = match kind {
                            StmntKind::Assignment(dst, val) => {
                                StmntKind::Assignment(dst, val.map_operands(conversion))
                            }
                            StmntKind::Call(call, args, span) => {
                                conversion.map_call_stmnt(call, args, span)
                            }
                            StmntKind::NoOp => StmntKind::NoOp,
                            StmntKind::CollapseHint(hi, lo) => StmntKind::CollapseHint(hi, lo),
                        };
                        (kind, sctx)
                    })
                    .collect();

                let terminator = bb.terminator.map(|term| {
                    let kind = match term.kind {
                        TerminatorKind::Goto(dst) => TerminatorKind::Goto(dst),
                        TerminatorKind::Split {
                            condition,
                            true_block,
                            false_block,
                            loop_head,
                        } => TerminatorKind::Split {
                            condition: condition.map_operands(conversion),
                            true_block,
                            false_block,
                            loop_head,
                        },
                        TerminatorKind::End => TerminatorKind::End,
                    };
                    Terminator {
                        kind,
                        sctx: term.sctx,
                    }
                });

                BasicBlockData {
                    phi_statements: bb.phi_statements,
                    statements,
                    terminator,
                }
            })
            .collect();

        ControlFlowGraph {
            locals: self.locals,
            blocks,
            derivatives: self.derivatives,
            predecessor_cache: self.predecessor_cache,
        }
    }





    pub fn intern_locations(&mut self) -> InternedLocations {
        let locations: IndexVec<_, _> = self.locations().collect();
        let mut blocks: IndexVec<BasicBlock, BlockLocations> =
            IndexVec::with_capacity(self.blocks.len());
        let mut start = LocationId::new(0);
        for (id, location) in locations.iter_enumerated() {
            if location.block != blocks.len_idx() {
                blocks.push(BlockLocations {
                    phi_start: start,
                    stmnt_start: start + self.blocks[blocks.len_idx()].phi_statements.len(),
                    terminator: id - 1,
                });
                start = id;
            }

            debug_assert_eq!(blocks.len_idx(), location.block)
        }

        if let Some(last) = self.blocks.last() {
            blocks.push(BlockLocations {
                phi_start: start,
                stmnt_start: start + last.phi_statements.len(),
                terminator: locations.len_idx(),
            });
        }

        InternedLocations { locations, blocks }
    }

    // Relations

    pub fn predecessors(&self, bb: BasicBlock) -> &[BasicBlock] {
        &self.predecessor_cache.compute(&self.blocks)[bb]
    }

    pub fn successors(&self, bb: BasicBlock) -> Successors {
        self[bb].successors()
    }

    /// Demands the derivative of an [`Operand`](crate::Operand)
    ///
    /// # Panics
    /// * if `operand` contains a reference to a local that is not part of this CFG
    /// * if a branch local was supplied (these may not be read only written to)
    /// * if the derivative of a non numeric (string) local was demanded
    pub fn demand_operand_derivative_unchecked<MC: CallType>(
        &mut self,
        mir: &Mir<MC>,
        operand: &COperand<C>,
        unknown: Unknown,
    ) -> CallTypeDerivative<C> {
        match operand.contents {
            OperandData::Constant(_) => Derivative::Zero,

            OperandData::Copy(local) => {
                let derivative = self.demand_derivative_unchecked(local, unknown);
                Derivative::Operand(OperandData::Copy(derivative))
            }

            OperandData::Read(ref input) => input.derivative(unknown, mir),
        }
    }

    /// Demands the derivative of an [`Operand`](crate::Operand)
    ///
    /// # Panics
    /// * if `local` is not part of this CFG
    /// * if a branch local was supplied (these may not be read only written to)
    /// * if the derivative of a non numeric (string) local was demanded
    pub fn demand_derivative_unchecked(&mut self, local: Local, unknown: Unknown) -> Local {
        let locals = &mut self.locals;

        let derivative = *self
            .derivatives
            .entry(local)
            .or_insert_with(|| HashMap::with_capacity(4))
            .entry(unknown)
            .or_insert_with(|| {
                let kind = match locals[local].kind {
                    LocalKind::Temporary => LocalKind::Temporary,
                    LocalKind::Variable(var, _) => {
                        LocalKind::Variable(var, VariableLocalKind::Derivative)
                    }
                    LocalKind::Branch(_, _) => {
                        unreachable!("Branch locals may only appear in a write postion")
                    }
                };
                locals.push(LocalDeclaration {
                    kind,
                    ty: Type::REAL,
                })
            });

        derivative
    }
}

impl<A, B> Convert<ControlFlowGraph<B>> for ControlFlowGraph<A>
where
    B: CallType,
    A: Into<B> + CallType,
    A::I: Into<B::I>,
{
    fn convert(self) -> ControlFlowGraph<B> {
        ControlFlowGraph {
            locals: self.locals,
            blocks: self.blocks.into_iter().map(|bb| bb.convert()).collect(),
            derivatives: self.derivatives,
            predecessor_cache: self.predecessor_cache,
        }
    }
}

#[derive(Clone)]
pub struct PhiData {
    pub dst: Local,
    pub sources: HashMap<BasicBlock, Local>,
    pub sctx: SyntaxCtx,
}

impl Display for PhiData {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = phi {{ ", self.dst)?;
        for (bb, src) in self.sources.iter() {
            write!(f, "{:?} => {}, ", bb, src)?;
        }
        f.write_str("}")?;
        Ok(())
    }
}

impl Debug for PhiData {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

#[derive(Clone, Debug)]
pub struct BasicBlockData<C: CallType> {
    pub phi_statements: IndexVec<Phi, PhiData>,
    pub statements: IndexVec<StatementId, Statement<C>>,
    pub terminator: Option<Terminator<C>>,
}

impl<A, B> Convert<BasicBlockData<B>> for BasicBlockData<A>
where
    B: CallType,
    A: Into<B> + CallType,
    A::I: Into<B::I>,
{
    fn convert(self) -> BasicBlockData<B> {
        BasicBlockData {
            phi_statements: self.phi_statements,
            statements: self
                .statements
                .into_iter()
                .map(|(stmnt, sctx)| (stmnt.convert(), sctx))
                .collect(),
            terminator: self.terminator.convert(),
        }
    }
}

impl<C: CallType> BasicBlockData<C> {
    #[must_use]
    pub fn successors(&self) -> Successors {
        self.terminator().successors()
    }
    #[must_use]
    pub fn successors_mut(&mut self) -> SuccessorsMut {
        self.terminator_mut().successors_mut()
    }

    #[must_use]
    pub fn terminator(&self) -> &Terminator<C> {
        self.terminator.as_ref().unwrap()
    }

    #[must_use]
    pub fn terminator_mut(&mut self) -> &mut Terminator<C> {
        self.terminator.as_mut().unwrap()
    }

    pub fn for_locals(&self, mut f: impl FnMut(Local)) {
        for phi in &self.phi_statements {
            f(phi.dst);
            for src in phi.sources.values() {
                f(*src)
            }
        }

        for (stmnt, _) in &self.statements {
            stmnt.for_locals(&mut f)
        }

        if let Some(terminator) = &self.terminator {
            if let TerminatorKind::Split { condition, .. } = &terminator.kind {
                condition.for_locals(f)
            }
        }
    }

    pub fn locals(&self) -> impl Iterator<Item = Local> {
        // This is mostly three adress code
        let mut buff = Vec::with_capacity(3);
        self.for_locals(|local| buff.push(local));
        buff.into_iter()
    }

    pub fn for_locals_mut(&mut self, mut f: impl FnMut(&mut Local)) {
        for phi in &mut self.phi_statements {
            f(&mut phi.dst);
            for src in phi.sources.values_mut() {
                f(src)
            }
        }

        for (stmnt, _) in &mut self.statements {
            stmnt.for_locals_mut(&mut f)
        }

        if let Some(terminator) = &mut self.terminator {
            if let TerminatorKind::Split { condition, .. } = &mut terminator.kind {
                condition.for_locals_mut(f)
            }
        }
    }

    pub fn locals_mut(&mut self) -> impl Iterator<Item = &mut Local> {
        // This is mostly three adress code
        let mut buff = Vec::with_capacity(3);
        // This is save since each local is an individual location in memory the interface of the for_locals funciton just doesn't capture that
        self.for_locals_mut(|local| buff.push(local as *mut Local));
        buff.into_iter().map(|local_ptr| unsafe { &mut *local_ptr })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Terminator<C: CallType> {
    pub sctx: SyntaxCtx,
    pub kind: TerminatorKind<C>,
}

impl<C: CallType> Terminator<C> {
    #[must_use]
    pub fn successors(&self) -> Successors {
        self.kind.successors()
    }

    #[must_use]
    #[inline]
    pub fn successors_mut(&mut self) -> SuccessorsMut {
        self.kind.successors_mut()
    }

    #[inline]
    pub fn map(&mut self, f: impl Fn(BasicBlock) -> BasicBlock) {
        self.kind.map(f)
    }
}

impl<A, B> Convert<Terminator<B>> for Terminator<A>
where
    B: CallType,
    A: Into<B> + CallType,
    A::I: Into<B::I>,
{
    fn convert(self) -> Terminator<B> {
        Terminator {
            sctx: self.sctx,
            kind: self.kind.convert(),
        }
    }
}

impl<A, B> Convert<TerminatorKind<B>> for TerminatorKind<A>
where
    B: CallType,
    A: Into<B> + CallType,
    A::I: Into<B::I>,
{
    fn convert(self) -> TerminatorKind<B> {
        match self {
            Self::Goto(dst) => TerminatorKind::Goto(dst),
            Self::End => TerminatorKind::End,
            Self::Split {
                condition,
                true_block,
                false_block,
                loop_head,
            } => TerminatorKind::Split {
                condition: condition.convert(),
                true_block,
                false_block,
                loop_head,
            },
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum TerminatorKind<C: CallType> {
    Goto(BasicBlock),
    Split {
        condition: RValue<C>,
        true_block: BasicBlock,
        false_block: BasicBlock,
        loop_head: bool,
    },
    End,
}

impl<C: CallType> Display for TerminatorKind<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Goto(bb) => {
                write!(f, "goto {:?}", bb)
            }
            TerminatorKind::Split {
                condition,
                true_block,
                false_block,
                ..
            } => {
                write!(
                    f,
                    "if {} {{ goto {:?} }} else {{ goto {:?} }}",
                    condition, true_block, false_block
                )
            }
            TerminatorKind::End => write!(f, "terminate"),
        }
    }
}

impl<C: CallType> Debug for TerminatorKind<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl<C: CallType> TerminatorKind<C> {
    #[must_use]
    pub fn successors(&self) -> Successors {
        match self {
            Self::End => Successors::new_empty(),
            Self::Goto(dst) => Successors::new_single(*dst),
            Self::Split {
                true_block,
                false_block,
                ..
            } => Successors::new_double(*true_block, *false_block),
        }
    }

    #[must_use]
    #[inline]
    pub fn successors_mut(&mut self) -> SuccessorsMut {
        match self {
            Self::End => SuccessorsMut::new_empty(),
            Self::Goto(dst) => SuccessorsMut::new_single(dst),
            Self::Split {
                true_block,
                false_block,
                ..
            } => SuccessorsMut::new_double(true_block, false_block),
        }
    }

    #[inline]
    pub fn map(&mut self, f: impl Fn(BasicBlock) -> BasicBlock) {
        match self {
            Self::Goto(dst) => *dst = f(*dst),
            Self::Split {
                true_block,
                false_block,
                ..
            } => {
                *true_block = f(*true_block);
                *false_block = f(*false_block);
            }

            Self::End => (),
        }
    }

    pub fn is_loop_head(&self) -> bool {
        matches!(
            self,
            Self::Split {
                loop_head: true,
                ..
            }
        )
    }
}
