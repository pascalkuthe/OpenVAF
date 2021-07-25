#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use data_structures::index_vec::{define_index_type, IndexSlice,
                                         IndexVec};

pub use ir::ids::{AttributeId, BranchId, DisciplineId, IdRange,
                          ModuleId, NatureId, NetId, ParameterId, PortId,
                          StatementId, SyntaxCtx, VariableId};
pub use ir::{impl_id_type, Attributes, DisciplineAccess,
                     ParameterExcludeConstraint, ParameterRangeConstraint,
                     ParameterRangeConstraintBound, Port, Spanned,
                     UnaryOperator, Unknown};

use session::{sourcemap::{Span, StringLiteral}, symbols::sysfun};

use session::symbols::{kw, Ident};

use crate::cfg::ControlFlowGraph;
use derive_more::{Display, From};
use std::fmt::{Debug, Display, Formatter};

pub use crate::functions::CfgFunctions;
pub use fold::{fold_rvalue, RValueFold};
pub use osdi_types::{SimpleType, Type, TypeInfo};

use diagnostics::lints::{Lint, LintLevel};

pub mod cfg {





































    // Aligment





















































    // All stmnts excepot array construction and calls have 3 or less operands
    // Even calls have 3 or less arguments in practice in 90% of cases

    // This is mostly three adress code
    // This is save since each local is an individual location in memory the interface of the for_locals funciton just doesn't capture that



    // All stmnts excepot array construction and calls have 3 or less operands
    // Even calls have 3 or less arguments in practice in 90% of cases

    // This is mostly three adress code
    // This is save since each local is an individual location in memory the interface of the for_locals funciton just doesn't capture that





















    use data_structures::{index_vec::define_index_type,
                                  index_vec::IndexVec};
    use ir::ids::{StatementId, SyntaxCtx};
    use ir::{impl_id_type, Unknown};
    use crate::cfg::predecessors::PredecessorCache;
    use crate::cfg::transversal::ReversePostorderIterMut;
    use crate::cfg::transversal::{Postorder, PostorderIter, PostorderIterMut,
                                  ReversePostorder, ReversePostorderIter};
    use crate::{COperand, CallTypeDerivative, CfgConversion, CfgFunctions,
                CfgInputs, DefaultConversion, Derivative, Local,
                LocalDeclaration, LocalKind, Mir, OperandData,
                ParameterCallType, RValue, Spanned, Statement,
                VariableLocalKind};
    use data_structures::HashMap;
    use osdi_types::Type;
    use crate::cfg::builder::CfgBuilder;
    use crate::cfg::graph_cyclical_cache::GraphIsCyclicCache;
    use crate::functions::DefaultFunctions;
    use crate::inputs::ParameterInput;
    use crate::util::AtMostTwoIter;
    use data_structures::arrayvec::ArrayVec;
    use osdi_types::ConstVal::Scalar;
    use osdi_types::SimpleConstVal::Real;
    use std::fmt;
    use std::fmt::{Debug, Display, Formatter};
    use std::hash::Hash;
    use std::iter::once;
    use std::ops::Index;
    use tracing::Span;
    pub mod predecessors {
        //! Lazily compute the reverse control-flow graph for the MIR.
        use crate::cfg::{BasicBlock, BasicBlockData};
        use crate::CfgFunctions;
        use data_structures::index_vec::{index_vec, IndexVec};
        use data_structures::sync::OnceCell;
        pub type Predecessors = IndexVec<BasicBlock, Vec<BasicBlock>>;
        pub struct PredecessorCache {
            cache: OnceCell<Predecessors>,
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::clone::Clone for PredecessorCache {
            #[inline]
            fn clone(&self) -> PredecessorCache {
                match *self {
                    PredecessorCache { cache: ref __self_0_0 } =>
                    PredecessorCache{cache:
                                         ::core::clone::Clone::clone(&(*__self_0_0)),},
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::fmt::Debug for PredecessorCache {
            fn fmt(&self, f: &mut ::core::fmt::Formatter)
             -> ::core::fmt::Result {
                match *self {
                    PredecessorCache { cache: ref __self_0_0 } => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_struct(f,
                                                                      "PredecessorCache");
                        let _ =
                            ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                            "cache",
                                                            &&(*__self_0_0));
                        ::core::fmt::DebugStruct::finish(debug_trait_builder)
                    }
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::default::Default for PredecessorCache {
            #[inline]
            fn default() -> PredecessorCache {
                PredecessorCache{cache: ::core::default::Default::default(),}
            }
        }
        impl PredecessorCache {
            #[inline]
            pub fn new() -> Self { PredecessorCache{cache: OnceCell::new(),} }
            /// Invalidates the predecessor cache.
            #[inline]
            pub fn invalidate(&mut self) { self.cache = OnceCell::new(); }
            /// Returns the the predecessor graph for this CFG.
            #[inline]
            pub(super) fn compute<C: CfgFunctions>(&self,
                                                   basic_blocks:
                                                       &IndexVec<BasicBlock,
                                                                 BasicBlockData<C>>)
             -> &Predecessors {
                self.cache.get_or_init(||
                                           {
                                               let mut preds =
                                                   ::index_vec::IndexVec::from_vec(::alloc::vec::from_elem(Vec::with_capacity(2),
                                                                                                           basic_blocks.len()));
                                               for (id, bb) in
                                                   basic_blocks.iter_enumerated()
                                                   {
                                                   for succ in bb.successors()
                                                       {
                                                       preds[succ].push(id);
                                                   }
                                               }
                                               preds
                                           })
            }
        }
    }
    pub mod transversal {
        use crate::cfg::{BasicBlock, BasicBlockData, ControlFlowGraph,
                         Successors, START_BLOCK};
        use crate::CfgFunctions;
        use data_structures::bit_set::BitSet;
        use std::mem::transmute;
        /// Postorder traversal of a graph.
        ///
        /// Postorder traversal is when each node is visited after all of its
        /// successors, except when the successor is only reachable by a back-edge
        ///
        ///
        /// ```text
        ///
        ///         A
        ///        / \
        ///       /   \
        ///      B     C
        ///       \   /
        ///        \ /
        ///         D
        /// ```
        ///
        /// A Postorder traversal of this graph is `D B C A` or `D C B A`
        ///
        pub struct Postorder {
            visited: BitSet<BasicBlock>,
            visit_stack: Vec<(BasicBlock, Successors)>,
            root_is_start_block: bool,
        }
        impl <'lt> Postorder {
            pub fn new<C: CfgFunctions>(cfg: &ControlFlowGraph<C>,
                                        root: BasicBlock) -> Postorder {
                let mut po =
                    Postorder{visited: BitSet::new_empty(cfg.blocks.len()),
                              visit_stack: Vec::new(),
                              root_is_start_block: root == START_BLOCK,};
                po.visited.insert(root);
                po.visit_stack.push((root, cfg.successors(root)));
                po.traverse_successor(cfg);
                po
            }
            fn traverse_successor<C: CfgFunctions>(&mut self,
                                                   cfg:
                                                       &ControlFlowGraph<C>) {
                while let Some(bb) =
                          self.visit_stack.last_mut().and_then(|(_, iter)|
                                                                   iter.next())
                      {
                    if self.visited.insert(bb) {
                        self.visit_stack.push((bb, cfg.successors(bb)));
                    }
                }
            }
        }
        pub struct PostorderIter<'lt, C: CfgFunctions> {
            pub base: Postorder,
            pub cfg: &'lt ControlFlowGraph<C>,
        }
        impl <'lt, C: CfgFunctions> PostorderIter<'lt, C> {
            pub fn new(cfg: &'lt ControlFlowGraph<C>, root: BasicBlock)
             -> Self {
                Self{base: Postorder::new(cfg, root), cfg,}
            }
        }
        impl <'lt, C: CfgFunctions> Iterator for PostorderIter<'lt, C> {
            type Item = (BasicBlock, &'lt BasicBlockData<C>);
            fn next(&mut self)
             -> Option<(BasicBlock, &'lt BasicBlockData<C>)> {
                let next = self.base.visit_stack.pop();
                if next.is_some() { self.base.traverse_successor(self.cfg); }
                next.map(|(bb, _)| (bb, &self.cfg[bb]))
            }
            fn size_hint(&self) -> (usize, Option<usize>) {
                let upper = self.cfg.blocks.len() - self.base.visited.count();
                let lower =
                    if self.base.root_is_start_block {
                        upper
                    } else { self.base.visit_stack.len() };
                (lower, Some(upper))
            }
        }
        pub struct PostorderIterMut<'lt, C: CfgFunctions> {
            base: Postorder,
            cfg: &'lt mut ControlFlowGraph<C>,
        }
        impl <'lt, C: CfgFunctions> PostorderIterMut<'lt, C> {
            pub fn new(cfg: &'lt mut ControlFlowGraph<C>, root: BasicBlock)
             -> Self {
                Self{base: Postorder::new(cfg, root), cfg,}
            }
        }
        impl <'lt, C: CfgFunctions> Iterator for PostorderIterMut<'lt, C> {
            type Item = (BasicBlock, &'lt mut BasicBlockData<C>);
            fn next(&mut self)
             -> Option<(BasicBlock, &'lt mut BasicBlockData<C>)> {
                let next = self.base.visit_stack.pop();
                if next.is_some() { self.base.traverse_successor(self.cfg); }
                next.map(|(bb, _)|
                             (bb, unsafe { transmute(&mut self.cfg[bb]) }))
            }
            fn size_hint(&self) -> (usize, Option<usize>) {
                let upper = self.cfg.blocks.len() - self.base.visited.count();
                let lower =
                    if self.base.root_is_start_block {
                        upper
                    } else { self.base.visit_stack.len() };
                (lower, Some(upper))
            }
        }
        /// Reverse postorder traversal of a graph
        ///
        /// Reverse postorder is the reverse order of a postorder traversal.
        /// This is different to a preorder traversal and represents a natural
        /// linearization of control-flow.
        ///
        /// ```text
        ///
        ///         A
        ///        / \
        ///       /   \
        ///      B     C
        ///       \   /
        ///        \ /
        ///         D
        /// ```
        ///
        /// A reverse postorder traversal of this graph is either `A B C D` or `A C B D`
        /// Note that for a graph containing no loops (i.e., A DAG), this is equivalent to
        /// a topological sort.
        ///
        /// Construction of a `ReversePostorder` traversal requires doing a full
        /// postorder traversal of the graph, therefore this traversal should be
        /// constructed as few times as possible. Use the `reset` method to be able
        /// to re-use the traversal
        pub struct ReversePostorder {
            blocks: Vec<BasicBlock>,
            idx: usize,
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::clone::Clone for ReversePostorder {
            #[inline]
            fn clone(&self) -> ReversePostorder {
                match *self {
                    ReversePostorder {
                    blocks: ref __self_0_0, idx: ref __self_0_1 } =>
                    ReversePostorder{blocks:
                                         ::core::clone::Clone::clone(&(*__self_0_0)),
                                     idx:
                                         ::core::clone::Clone::clone(&(*__self_0_1)),},
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::fmt::Debug for ReversePostorder {
            fn fmt(&self, f: &mut ::core::fmt::Formatter)
             -> ::core::fmt::Result {
                match *self {
                    ReversePostorder {
                    blocks: ref __self_0_0, idx: ref __self_0_1 } => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_struct(f,
                                                                      "ReversePostorder");
                        let _ =
                            ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                            "blocks",
                                                            &&(*__self_0_0));
                        let _ =
                            ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                            "idx",
                                                            &&(*__self_0_1));
                        ::core::fmt::DebugStruct::finish(debug_trait_builder)
                    }
                }
            }
        }
        impl ReversePostorder {
            pub fn new<C: CfgFunctions>(cfg: &ControlFlowGraph<C>,
                                        root: BasicBlock) -> Self {
                let blocks: Vec<_> =
                    PostorderIter::new(cfg, root).map(|(bb, _)| bb).collect();
                let len = blocks.len();
                ReversePostorder{blocks, idx: len,}
            }
            pub fn reset(&mut self) { self.idx = self.blocks.len(); }
        }
        impl Iterator for ReversePostorder {
            type Item = BasicBlock;
            fn next(&mut self) -> Option<BasicBlock> {
                if self.idx == 0 { return None; }
                self.idx -= 1;
                self.blocks.get(self.idx).copied()
            }
            fn size_hint(&self) -> (usize, Option<usize>) {
                (self.idx, Some(self.idx))
            }
        }
        impl ExactSizeIterator for ReversePostorder {
            fn len(&self) -> usize { self.idx }
        }
        pub struct ReversePostorderIter<'lt, C: CfgFunctions> {
            cfg: &'lt ControlFlowGraph<C>,
            base: ReversePostorder,
        }
        impl <'lt, C: CfgFunctions> ReversePostorderIter<'lt, C> {
            pub fn new(cfg: &'lt ControlFlowGraph<C>, root: BasicBlock)
             -> Self {
                Self{base: ReversePostorder::new(cfg, root), cfg,}
            }
        }
        impl <'lt, C: CfgFunctions> ExactSizeIterator for
         ReversePostorderIter<'lt, C> {
            fn len(&self) -> usize { self.base.len() }
        }
        impl <'lt, C: CfgFunctions> Iterator for ReversePostorderIter<'lt, C>
         {
            type Item = (BasicBlock, &'lt BasicBlockData<C>);
            fn next(&mut self)
             -> Option<(BasicBlock, &'lt BasicBlockData<C>)> {
                self.base.next().map(|bb| (bb, &self.cfg[bb]))
            }
            fn size_hint(&self) -> (usize, Option<usize>) {
                self.base.size_hint()
            }
        }
        pub struct ReversePostorderIterMut<'lt, C: CfgFunctions> {
            cfg: &'lt mut ControlFlowGraph<C>,
            base: ReversePostorder,
        }
        impl <'lt, C: CfgFunctions> ReversePostorderIterMut<'lt, C> {
            pub fn new(cfg: &'lt mut ControlFlowGraph<C>, root: BasicBlock)
             -> Self {
                Self{base: ReversePostorder::new(cfg, root), cfg,}
            }
        }
        impl <'lt, C: CfgFunctions> Iterator for
         ReversePostorderIterMut<'lt, C> {
            type Item = (BasicBlock, &'lt mut BasicBlockData<C>);
            fn next(&mut self)
             -> Option<(BasicBlock, &'lt mut BasicBlockData<C>)> {
                self.base.next().map(|bb|
                                         (bb,
                                          unsafe {
                                              transmute(&mut self.cfg[bb])
                                          }))
            }
            fn size_hint(&self) -> (usize, Option<usize>) {
                self.base.size_hint()
            }
        }
        impl <'lt, C: CfgFunctions> ExactSizeIterator for
         ReversePostorderIterMut<'lt, C> {
            fn len(&self) -> usize { self.base.len() }
        }
    }
    pub mod builder {
        use crate::cfg::{BasicBlock, BasicBlockData, ControlFlowGraph,
                         PhiData, Terminator, TerminatorKind};
        use crate::{BranchId, COperand, CfgConversion, CfgFunctions,
                    DisciplineAccess, Expression, Local, LocalDeclaration,
                    LocalKind, Mir, OperandData, RValue, Spanned, StatementId,
                    StmntKind, SyntaxCtx, TyRValue, Type, VariableId,
                    VariableLocalKind};
        use data_structures::index_vec::{index_vec, IndexVec};
        use data_structures::HashMap;
        use session::sourcemap::Span;
        use std::array;
        use tracing::trace;
        pub trait CfgEdit {
            type CfgFunctions: CfgFunctions;
            fn borrow_mut(&mut self)
            -> &mut ControlFlowGraph<Self::CfgFunctions>;
        }
        impl <'a, C: CfgFunctions> CfgEdit for &'a mut ControlFlowGraph<C> {
            type CfgFunctions = C;
            fn borrow_mut(&mut self)
             -> &mut ControlFlowGraph<Self::CfgFunctions> {
                self
            }
        }
        impl <C: CfgFunctions> CfgEdit for ControlFlowGraph<C> {
            type CfgFunctions = C;
            fn borrow_mut(&mut self)
             -> &mut ControlFlowGraph<Self::CfgFunctions> {
                self
            }
        }
        pub struct CfgBuilder<C> {
            pub cfg: C,
            vars: IndexVec<VariableId, Option<Local>>,
            flows: IndexVec<BranchId, Option<Local>>,
            potentials: IndexVec<BranchId, Option<Local>>,
            pub current: BasicBlock,
        }
        impl <C: CfgFunctions> CfgBuilder<ControlFlowGraph<C>> {
            pub fn new_small() -> Self {
                let mut res =
                    Self{cfg: ControlFlowGraph::new(),
                         vars: IndexVec::new(),
                         flows: IndexVec::new(),
                         potentials: IndexVec::new(),
                         current: BasicBlock::from_raw_unchecked(0),};
                res.enter_new_block();
                res
            }
            pub fn new_fn(variable_cnt: usize, root_sctx: SyntaxCtx) -> Self {
                let mut res =
                    Self{cfg: ControlFlowGraph::new(),
                         vars:
                             ::index_vec::IndexVec::from_vec(::alloc::vec::from_elem(None,
                                                                                     variable_cnt)),
                         flows: IndexVec::new(),
                         potentials: IndexVec::new(),
                         current: BasicBlock::from_raw_unchecked(0),};
                let entry = res.create_block();
                res.enter_new_block();
                res.terminate_bb(entry, TerminatorKind::Goto(res.current),
                                 root_sctx);
                res
            }
            pub fn new_main(variable_cnt: usize, branch_cnt: usize,
                            root_sctx: SyntaxCtx) -> Self {
                let mut res =
                    Self{cfg: ControlFlowGraph::new(),
                         vars:
                             ::index_vec::IndexVec::from_vec(::alloc::vec::from_elem(None,
                                                                                     variable_cnt)),
                         flows:
                             ::index_vec::IndexVec::from_vec(::alloc::vec::from_elem(None,
                                                                                     branch_cnt)),
                         potentials:
                             ::index_vec::IndexVec::from_vec(::alloc::vec::from_elem(None,
                                                                                     branch_cnt)),
                         current: BasicBlock::from_raw_unchecked(0),};
                let entry = res.create_block();
                res.enter_new_block();
                res.terminate_bb(entry, TerminatorKind::Goto(res.current),
                                 root_sctx);
                res
            }
        }
        impl <'a, C: CfgFunctions> CfgBuilder<&'a mut ControlFlowGraph<C>> {
            pub fn edit<const INIT_BRANCHES : bool, const INIT_VARS :
                        bool>(cfg: &'a mut ControlFlowGraph<C>,
                              current_bb: BasicBlock, var_cnt: usize,
                              branch_cnt: usize) -> Self {
                let mut vars =
                    if INIT_VARS {
                        ::index_vec::IndexVec::from_vec(::alloc::vec::from_elem(None,
                                                                                var_cnt))
                    } else { IndexVec::new() };
                let (mut flows, mut potentials) =
                    if INIT_BRANCHES {
                        (::index_vec::IndexVec::from_vec(::alloc::vec::from_elem(None,
                                                                                 branch_cnt)),
                         ::index_vec::IndexVec::from_vec(::alloc::vec::from_elem(None,
                                                                                 branch_cnt)))
                    } else { (IndexVec::new(), IndexVec::new()) };
                if INIT_BRANCHES | INIT_VARS {
                    for (local, decl) in cfg.locals.iter_enumerated() {
                        match decl.kind {
                            LocalKind::Variable(var, VariableLocalKind::User)
                            if INIT_VARS => {
                                vars[var] = Some(local)
                            }
                            LocalKind::Branch(DisciplineAccess::Potential,
                                              branch, VariableLocalKind::User)
                            if INIT_BRANCHES =>
                            potentials[branch] = Some(local),
                            LocalKind::Branch(DisciplineAccess::Flow, branch,
                                              VariableLocalKind::User) if
                            INIT_BRANCHES => {
                                flows[branch] = Some(local)
                            }
                            _ => (),
                        }
                    }
                }
                Self{cfg,
                     vars: Default::default(),
                     flows: Default::default(),
                     potentials: Default::default(),
                     current: current_bb,}
            }
        }
        impl <C: CfgFunctions, CFG: CfgEdit<CfgFunctions = C>> CfgBuilder<CFG>
         {
            pub fn finish(mut self, sctx: SyntaxCtx) -> CFG {
                let end = self.cfg.borrow_mut().end();
                self.terminate_bb(end, TerminatorKind::End, sctx);
                self.cfg
            }
            /// # Note
            /// This funciton is optimized for joining multiple cfgs late in the compilation process.
            /// It therefore doesn't join derivative mappings If you need that do it manually (simply join the hashmaps with the second cfgs locals offsetet).
            /// Furthermore it doesn't invalidate the predecessor cache you need to do that after you are done with modifying the cfg
            pub fn insert_cfg<X: CfgFunctions,
                              Conversion: CfgConversion<X, C>, const
                              RETAIN_END : bool, const MAP_LOCALS :
                              bool>(&mut self, mut cfg: ControlFlowGraph<X>,
                                    conversion: &mut Conversion)
             -> BasicBlock {
                if true {
                    if !!cfg.blocks.is_empty() {
                        ::core::panicking::panic("assertion failed: !cfg.blocks.is_empty()")
                    };
                };
                let dst = self.cfg.borrow_mut();
                let block_offset = dst.blocks.last_idx();
                let local_offset = dst.locals.len();
                for block in cfg.blocks.iter_mut() {
                    for succ in block.successors_mut() {
                        *succ += block_offset
                    }
                    if MAP_LOCALS {
                        block.for_locals_mut(|local| *local += local_offset)
                    }
                }
                dst.locals.append(&mut cfg.locals);
                let new_end = dst.end() + cfg.blocks.len();
                let (inserted_cfg_start, inserted_cfg_end) =
                    if RETAIN_END {
                        let inserted_cfg_start = dst.end();
                        let old_end = dst.end();
                        for block in dst.blocks.iter_mut() {
                            if let Some(terminator) = &mut block.terminator {
                                for block in terminator.successors_mut() {
                                    if *block == old_end { *block = new_end }
                                }
                            }
                        }
                        dst.blocks.splice(dst.end()..dst.end(),
                                          cfg.blocks.into_iter().map(|block|
                                                                         block.map(conversion)));
                        (inserted_cfg_start, new_end - 1)
                    } else {
                        let inserted_cfg_start = dst.blocks.len_idx();
                        dst.blocks.extend(cfg.blocks.into_iter().map(|block|
                                                                         block.map(conversion)));
                        (inserted_cfg_start, new_end)
                    };
                dst[inserted_cfg_end].terminator = None;
                self.current = inserted_cfg_end;
                inserted_cfg_start
            }
            /// See `insert_cfg`
            pub fn insert_expr<X, Convert: CfgConversion<X, C>, const
                               RETAIN_END : bool, const MAP_LOCALS :
                               bool>(&mut self, sctx: SyntaxCtx,
                                     expr: Expression<X>,
                                     conversion: &mut Convert) -> RValue<C>
             where X: CfgFunctions + Into<C>, X::I: Into<C::I> {
                let mut res = expr.1;
                let cfg = expr.0;
                if !cfg.blocks.is_empty() {
                    if MAP_LOCALS {
                        if let OperandData::Copy(local) = &mut res.contents {
                            *local += self.cfg.borrow_mut().locals.len()
                        }
                    }
                    let pred = self.current;
                    let start =
                        self.insert_cfg::<X, _, RETAIN_END,
                                          MAP_LOCALS>(cfg, conversion);
                    if !(!RETAIN_END || self.cfg.borrow_mut().end() != pred) {
                        ::core::panicking::panic("assertion failed: !RETAIN_END || self.cfg.borrow_mut().end() != pred")
                    };
                    if pred != start {
                        self.terminate_bb(pred, TerminatorKind::Goto(start),
                                          sctx)
                    }
                }
                RValue::Use(conversion.map_operand(res))
            }
            pub fn new_temporary(&mut self, ty: Type) -> Local {
                self.cfg.borrow_mut().new_temporary(ty)
            }
            pub fn variable_local<A: CfgFunctions>(&mut self, var: VariableId,
                                                   mir: &Mir<A>)
             -> (Local, Type) {
                if let Some(res) = self.vars[var] {
                    (res, self.cfg.borrow_mut().locals[res].ty)
                } else {
                    let ty = mir.variables[var].ty;
                    let local =
                        self.cfg.borrow_mut().locals.push(LocalDeclaration{kind:
                                                                               LocalKind::Variable(var,
                                                                                                   VariableLocalKind::User),
                                                                           ty,});
                    self.vars[var] = Some(local);
                    (local, ty)
                }
            }
            pub fn branch_local(&mut self, branch: BranchId,
                                access: DisciplineAccess) -> Local {
                let local_store =
                    match access {
                        DisciplineAccess::Flow => &mut self.flows,
                        DisciplineAccess::Potential => &mut self.potentials,
                    };
                if let Some(local) = local_store[branch] {
                    local
                } else {
                    let local =
                        self.cfg.borrow_mut().locals.push(LocalDeclaration{kind:
                                                                               LocalKind::Branch(access,
                                                                                                 branch,
                                                                                                 VariableLocalKind::User),
                                                                           ty:
                                                                               Type::REAL,});
                    local_store[branch] = Some(local);
                    local
                }
            }
            pub fn rvalue_to_operand(&mut self, data: TyRValue<C>, span: Span,
                                     sctx: SyntaxCtx) -> COperand<C> {
                self.rvalue_to_operand_in_bb(self.current, data, span, sctx)
            }
            pub fn rvalue_to_operand_in_bb(&mut self, bb: BasicBlock,
                                           data: TyRValue<C>, span: Span,
                                           sctx: SyntaxCtx) -> COperand<C> {
                if let RValue::Use(op) = data.val {
                    op
                } else {
                    Spanned::new(OperandData::Copy(self.assign_temporary_in_bb(bb,
                                                                               data,
                                                                               sctx)),
                                 span)
                }
            }
            pub fn assign_temporary_phi_static_srces<const N :
                                                     usize>(&mut self,
                                                            sources:
                                                                [(BasicBlock,
                                                                  Local); N],
                                                            sctx: SyntaxCtx,
                                                            ty: Type)
             -> Local {
                self.assign_temporary_phi_static_srces_in_bb(self.current,
                                                             sources, sctx,
                                                             ty)
            }
            pub fn assign_temporary_phi_static_srces_in_bb<const N :
                                                           usize>(&mut self,
                                                                  bb:
                                                                      BasicBlock,
                                                                  sources:
                                                                      [(BasicBlock,
                                                                        Local); N],
                                                                  sctx:
                                                                      SyntaxCtx,
                                                                  ty: Type)
             -> Local {
                self.assign_temporary_phi_in_bb(bb,
                                                array::IntoIter::new(sources).collect(),
                                                sctx, ty)
            }
            pub fn assign_temporary_phi(&mut self,
                                        sources: HashMap<BasicBlock, Local>,
                                        sctx: SyntaxCtx, ty: Type) -> Local {
                self.assign_temporary_phi_in_bb(self.current, sources, sctx,
                                                ty)
            }
            pub fn assign_temporary_phi_in_bb(&mut self, bb: BasicBlock,
                                              sources:
                                                  HashMap<BasicBlock, Local>,
                                              sctx: SyntaxCtx, ty: Type)
             -> Local {
                let dst = self.new_temporary(ty);
                self.assign_phi_in_bb(dst, bb, sources, sctx)
            }
            pub fn assign_phi_static_srces<const N :
                                           usize>(&mut self, dst: Local,
                                                  sources:
                                                      [(BasicBlock,
                                                        Local); N],
                                                  sctx: SyntaxCtx) -> Local {
                self.assign_phi_static_srces_in_bb(dst, self.current, sources,
                                                   sctx)
            }
            pub fn assign_phi_static_srces_in_bb<const N :
                                                 usize>(&mut self, dst: Local,
                                                        bb: BasicBlock,
                                                        sources:
                                                            [(BasicBlock,
                                                              Local); N],
                                                        sctx: SyntaxCtx)
             -> Local {
                self.assign_phi_in_bb(dst, bb,
                                      array::IntoIter::new(sources).collect(),
                                      sctx)
            }
            pub fn assign_phi(&mut self, dst: Local,
                              sources: HashMap<BasicBlock, Local>,
                              sctx: SyntaxCtx) -> Local {
                self.assign_phi_in_bb(dst, self.current, sources, sctx)
            }
            pub fn assign_phi_in_bb(&mut self, dst: Local, bb: BasicBlock,
                                    sources: HashMap<BasicBlock, Local>,
                                    sctx: SyntaxCtx) -> Local {
                self.cfg.borrow_mut().blocks[bb].phi_statements.push(PhiData{dst,
                                                                             sources,
                                                                             sctx,});
                dst
            }
            pub fn assign_temporary(&mut self, data: TyRValue<C>,
                                    sctx: SyntaxCtx) -> Local {
                self.assign_temporary_in_bb(self.current, data, sctx)
            }
            pub fn assign_temporary_in_bb(&mut self, bb: BasicBlock,
                                          data: TyRValue<C>, sctx: SyntaxCtx)
             -> Local {
                let lhs = self.new_temporary(data.ty);
                self.assign_in_bb(bb, lhs, data.val, sctx);
                lhs
            }
            pub fn assign(&mut self, lhs: Local, rhs: RValue<C>,
                          sctx: SyntaxCtx) -> StatementId {
                self.cfg.borrow_mut().blocks[self.current].statements.push((StmntKind::Assignment(lhs,
                                                                                                  rhs),
                                                                            sctx))
            }
            pub fn assign_in_bb(&mut self, bb: BasicBlock, lhs: Local,
                                rhs: RValue<C>, sctx: SyntaxCtx)
             -> StatementId {
                self.cfg.borrow_mut().blocks[bb].statements.push((StmntKind::Assignment(lhs,
                                                                                        rhs),
                                                                  sctx))
            }
            pub fn terminate_bb(&mut self, block: BasicBlock,
                                kind: TerminatorKind<C>, sctx: SyntaxCtx) {
                {
                    ;
                    if ::tracing::Level::TRACE <=
                           ::tracing::level_filters::STATIC_MAX_LEVEL &&
                           ::tracing::Level::TRACE <=
                               ::tracing::level_filters::LevelFilter::current()
                       {
                        use ::tracing::__macro_support::*;
                        static CALLSITE:
                         ::tracing::__macro_support::MacroCallsite =
                            {
                                use ::tracing::__macro_support::MacroCallsite;
                                static META: ::tracing::Metadata<'static> =
                                    {
                                        ::tracing_core::metadata::Metadata::new("event middle/src/cfg/builder.rs:441",
                                                                                "middle::cfg::builder",
                                                                                ::tracing::Level::TRACE,
                                                                                Some("middle/src/cfg/builder.rs"),
                                                                                Some(441u32),
                                                                                Some("middle::cfg::builder"),
                                                                                ::tracing_core::field::FieldSet::new(&["message",
                                                                                                                       "block",
                                                                                                                       "kind"],
                                                                                                                     ::tracing_core::callsite::Identifier(&CALLSITE)),
                                                                                ::tracing::metadata::Kind::EVENT)
                                    };
                                MacroCallsite::new(&META)
                            };
                        let interest = CALLSITE.interest();
                        if !interest.is_never() &&
                               CALLSITE.is_enabled(interest) {
                            let meta = CALLSITE.metadata();
                            ::tracing::Event::dispatch(meta,
                                                       &{
                                                            #[allow(unused_imports)]
                                                            use ::tracing::field::{debug,
                                                                                   display,
                                                                                   Value};
                                                            let mut iter =
                                                                meta.fields().iter();
                                                            meta.fields().value_set(&[(&iter.next().expect("FieldSet corrupted (this is a bug)"),
                                                                                       Some(&::core::fmt::Arguments::new_v1(&["terminating"],
                                                                                                                            &match ()
                                                                                                                                 {
                                                                                                                                 ()
                                                                                                                                 =>
                                                                                                                                 [],
                                                                                                                             })
                                                                                                as
                                                                                                &Value)),
                                                                                      (&iter.next().expect("FieldSet corrupted (this is a bug)"),
                                                                                       Some(&block.index()
                                                                                                as
                                                                                                &Value)),
                                                                                      (&iter.next().expect("FieldSet corrupted (this is a bug)"),
                                                                                       Some(&debug(&kind)
                                                                                                as
                                                                                                &Value))])
                                                        });
                        }
                    }
                };
                if true {
                    if !self.cfg.borrow_mut()[block].terminator.is_none() {
                        {
                            ::std::rt::begin_panic_fmt(&::core::fmt::Arguments::new_v1(&["terminate: block ",
                                                                                         "=",
                                                                                         " already has a terminator set"],
                                                                                       &match (&block,
                                                                                               &self.cfg.borrow_mut()[block])
                                                                                            {
                                                                                            (arg0,
                                                                                             arg1)
                                                                                            =>
                                                                                            [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                          ::core::fmt::Debug::fmt),
                                                                                             ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                          ::core::fmt::Debug::fmt)],
                                                                                        }))
                        }
                    };
                };
                self.cfg.borrow_mut()[block].terminator =
                    Some(Terminator{sctx, kind,});
            }
            pub fn terminate(&mut self, kind: TerminatorKind<C>,
                             sctx: SyntaxCtx) {
                self.terminate_bb(self.current, kind, sctx)
            }
            pub fn create_block(&mut self) -> BasicBlock {
                self.cfg.borrow_mut().blocks.push(BasicBlockData{phi_statements:
                                                                     IndexVec::with_capacity(16),
                                                                 statements:
                                                                     IndexVec::with_capacity(128),
                                                                 terminator:
                                                                     None,})
            }
            pub fn enter_new_block(&mut self) {
                self.current =
                    self.cfg.borrow_mut().blocks.push(BasicBlockData{phi_statements:
                                                                         IndexVec::with_capacity(16),
                                                                     statements:
                                                                         IndexVec::with_capacity(128),
                                                                     terminator:
                                                                         None,})
            }
        }
    }
    mod graph_cyclical_cache {
        use crate::cfg::{BasicBlock, ControlFlowGraph, START_BLOCK};
        use crate::CfgFunctions;
        use data_structures::bit_set::BitSet;
        use data_structures::sync::OnceCell;
        /// Helper type to cache the result of `graph::is_cyclic`.
        pub struct GraphIsCyclicCache {
            cache: OnceCell<bool>,
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::clone::Clone for GraphIsCyclicCache {
            #[inline]
            fn clone(&self) -> GraphIsCyclicCache {
                match *self {
                    GraphIsCyclicCache { cache: ref __self_0_0 } =>
                    GraphIsCyclicCache{cache:
                                           ::core::clone::Clone::clone(&(*__self_0_0)),},
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::fmt::Debug for GraphIsCyclicCache {
            fn fmt(&self, f: &mut ::core::fmt::Formatter)
             -> ::core::fmt::Result {
                match *self {
                    GraphIsCyclicCache { cache: ref __self_0_0 } => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_struct(f,
                                                                      "GraphIsCyclicCache");
                        let _ =
                            ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                            "cache",
                                                            &&(*__self_0_0));
                        ::core::fmt::DebugStruct::finish(debug_trait_builder)
                    }
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::default::Default for GraphIsCyclicCache {
            #[inline]
            fn default() -> GraphIsCyclicCache {
                GraphIsCyclicCache{cache:
                                       ::core::default::Default::default(),}
            }
        }
        impl GraphIsCyclicCache {
            #[inline]
            pub(super) fn new() -> Self {
                GraphIsCyclicCache{cache: OnceCell::new(),}
            }
            pub(super) fn is_cyclic<C: CfgFunctions>(&self,
                                                     cfg:
                                                         &ControlFlowGraph<C>)
             -> bool {
                *self.cache.get_or_init(||
                                            {
                                                TriColorDepthFirstSearch::new(cfg).run_from_start(&mut CycleDetector).is_some()
                                            })
            }
            /// Invalidates the cache.
            #[inline]
            pub fn invalidate(&mut self) { self.cache = OnceCell::new(); }
        }
        pub enum ControlFlow<B> {

            /// Move on to the next phase of the operation as normal.
            Continue,

            /// Exit the operation without running subsequent phases.
            Break(B),
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl <B: ::core::fmt::Debug> ::core::fmt::Debug for ControlFlow<B> {
            fn fmt(&self, f: &mut ::core::fmt::Formatter)
             -> ::core::fmt::Result {
                match (&*self,) {
                    (&ControlFlow::Continue,) => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                     "Continue");
                        ::core::fmt::DebugTuple::finish(debug_trait_builder)
                    }
                    (&ControlFlow::Break(ref __self_0),) => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                     "Break");
                        let _ =
                            ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                           &&(*__self_0));
                        ::core::fmt::DebugTuple::finish(debug_trait_builder)
                    }
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl <B: ::core::clone::Clone> ::core::clone::Clone for ControlFlow<B>
         {
            #[inline]
            fn clone(&self) -> ControlFlow<B> {
                match (&*self,) {
                    (&ControlFlow::Continue,) => ControlFlow::Continue,
                    (&ControlFlow::Break(ref __self_0),) =>
                    ControlFlow::Break(::core::clone::Clone::clone(&(*__self_0))),
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl <B: ::core::marker::Copy> ::core::marker::Copy for ControlFlow<B>
         {
        }
        impl <B> ::core::marker::StructuralPartialEq for ControlFlow<B> { }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl <B: ::core::cmp::PartialEq> ::core::cmp::PartialEq for
         ControlFlow<B> {
            #[inline]
            fn eq(&self, other: &ControlFlow<B>) -> bool {
                {
                    let __self_vi =
                        ::core::intrinsics::discriminant_value(&*self);
                    let __arg_1_vi =
                        ::core::intrinsics::discriminant_value(&*other);
                    if true && __self_vi == __arg_1_vi {
                        match (&*self, &*other) {
                            (&ControlFlow::Break(ref __self_0),
                             &ControlFlow::Break(ref __arg_1_0)) =>
                            (*__self_0) == (*__arg_1_0),
                            _ => true,
                        }
                    } else { false }
                }
            }
            #[inline]
            fn ne(&self, other: &ControlFlow<B>) -> bool {
                {
                    let __self_vi =
                        ::core::intrinsics::discriminant_value(&*self);
                    let __arg_1_vi =
                        ::core::intrinsics::discriminant_value(&*other);
                    if true && __self_vi == __arg_1_vi {
                        match (&*self, &*other) {
                            (&ControlFlow::Break(ref __self_0),
                             &ControlFlow::Break(ref __arg_1_0)) =>
                            (*__self_0) != (*__arg_1_0),
                            _ => false,
                        }
                    } else { true }
                }
            }
        }
        /// The status of a node in the depth-first search.
        ///
        /// See the documentation of `TriColorDepthFirstSearch` to see how a node's status is updated
        /// during DFS.
        pub enum NodeStatus {

            /// This node has been examined by the depth-first search but is not yet `Settled`.
            ///
            /// Also referred to as "gray" or "discovered" nodes in [CLR].
            ///
            /// [CLR]: https://en.wikipedia.org/wiki/Introduction_to_Algorithms
            Visited,

            /// This node and all nodes reachable from it have been examined by the depth-first search.
            ///
            /// Also referred to as "black" or "finished" nodes in [CLR].
            ///
            /// [CLR]: https://en.wikipedia.org/wiki/Introduction_to_Algorithms
            Settled,
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::clone::Clone for NodeStatus {
            #[inline]
            fn clone(&self) -> NodeStatus { { *self } }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::marker::Copy for NodeStatus { }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::fmt::Debug for NodeStatus {
            fn fmt(&self, f: &mut ::core::fmt::Formatter)
             -> ::core::fmt::Result {
                match (&*self,) {
                    (&NodeStatus::Visited,) => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                     "Visited");
                        ::core::fmt::DebugTuple::finish(debug_trait_builder)
                    }
                    (&NodeStatus::Settled,) => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                     "Settled");
                        ::core::fmt::DebugTuple::finish(debug_trait_builder)
                    }
                }
            }
        }
        impl ::core::marker::StructuralPartialEq for NodeStatus { }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::cmp::PartialEq for NodeStatus {
            #[inline]
            fn eq(&self, other: &NodeStatus) -> bool {
                {
                    let __self_vi =
                        ::core::intrinsics::discriminant_value(&*self);
                    let __arg_1_vi =
                        ::core::intrinsics::discriminant_value(&*other);
                    if true && __self_vi == __arg_1_vi {
                        match (&*self, &*other) { _ => true, }
                    } else { false }
                }
            }
        }
        impl ::core::marker::StructuralEq for NodeStatus { }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::cmp::Eq for NodeStatus {
            #[inline]
            #[doc(hidden)]
            #[no_coverage]
            fn assert_receiver_is_total_eq(&self) -> () { { } }
        }
        struct Event<N> {
            node: N,
            becomes: NodeStatus,
        }
        /// A depth-first search that also tracks when all successors of a node have been examined.
        ///
        /// This is based on the DFS described in [Introduction to Algorithms (1st ed.)][CLR], hereby
        /// referred to as **CLR**. However, we use the terminology in [`NodeStatus`] above instead of
        /// "discovered"/"finished" or "white"/"grey"/"black". Each node begins the search with no status,
        /// becomes `Visited` when it is first examined by the DFS and is `Settled` when all nodes
        /// reachable from it have been examined. This allows us to differentiate between "tree", "back"
        /// and "forward" edges (see [`TriColorVisitor::node_examined`]).
        ///
        /// Unlike the pseudocode in [CLR], this implementation is iterative and does not use timestamps.
        /// We accomplish this by storing `Event`s on the stack that result in a (possible) state change
        /// for each node. A `Visited` event signifies that we should examine this node if it has not yet
        /// been `Visited` or `Settled`. When a node is examined for the first time, we mark it as
        /// `Visited` and push a `Settled` event for it on stack followed by `Visited` events for all of
        /// its predecessors, scheduling them for examination. Multiple `Visited` events for a single node
        /// may exist on the stack simultaneously if a node has multiple predecessors, but only one
        /// `Settled` event will ever be created for each node. After all `Visited` events for a node's
        /// successors have been popped off the stack (as well as any new events triggered by visiting
        /// those successors), we will pop off that node's `Settled` event.
        ///
        /// [CLR]: https://en.wikipedia.org/wiki/Introduction_to_Algorithms
        pub struct TriColorDepthFirstSearch<'graph, C: CfgFunctions> {
            graph: &'graph ControlFlowGraph<C>,
            stack: Vec<Event<BasicBlock>>,
            visited: BitSet<BasicBlock>,
            settled: BitSet<BasicBlock>,
        }
        impl <'a, C: CfgFunctions> TriColorDepthFirstSearch<'a, C> {
            pub fn new(graph: &'a ControlFlowGraph<C>) -> Self {
                TriColorDepthFirstSearch{graph,
                                         stack: ::alloc::vec::Vec::new(),
                                         visited:
                                             BitSet::new_empty(graph.blocks.len()),
                                         settled:
                                             BitSet::new_empty(graph.blocks.len()),}
            }
            /// Performs a depth-first search, starting from the given `root`.
            ///
            /// This won't visit nodes that are not reachable from `root`.
            pub fn run_from<V: TriColorVisitor>(mut self, root: BasicBlock,
                                                visitor: &mut V)
             -> Option<V::BreakVal> {
                use NodeStatus::{Settled, Visited};
                self.stack.push(Event{node: root, becomes: Visited,});
                loop {
                    match self.stack.pop()? {
                        Event { node, becomes: Settled } => {
                            let not_previously_settled =
                                self.settled.insert(node);
                            if true {
                                if !not_previously_settled {
                                    {
                                        ::std::rt::begin_panic("A node should be settled exactly once")
                                    }
                                };
                            };
                            if let ControlFlow::Break(val) =
                                   visitor.node_settled(node) {
                                return Some(val);
                            }
                        }
                        Event { node, becomes: Visited } => {
                            let not_previously_visited =
                                self.visited.insert(node);
                            let prior_status =
                                if not_previously_visited {
                                    None
                                } else if self.settled.contains(node) {
                                    Some(Settled)
                                } else { Some(Visited) };
                            if let ControlFlow::Break(val) =
                                   visitor.node_examined(node, prior_status) {
                                return Some(val);
                            }
                            if prior_status.is_some() { continue ; }
                            self.stack.push(Event{node, becomes: Settled,});
                            for succ in self.graph.successors(node) {
                                if !visitor.ignore_edge(node, succ) {
                                    self.stack.push(Event{node: succ,
                                                          becomes: Visited,});
                                }
                            }
                        }
                    }
                }
            }
        }
        impl <'a, C: CfgFunctions> TriColorDepthFirstSearch<'a, C> {
            /// Performs a depth-first search, starting from `G::start_node()`.
            ///
            /// This won't visit nodes that are not reachable from the start node.
            pub fn run_from_start<V: TriColorVisitor>(self, visitor: &mut V)
             -> Option<V::BreakVal> {
                self.run_from(START_BLOCK, visitor)
            }
        }
        /// What to do when a node is examined or becomes `Settled` during DFS.
        pub trait TriColorVisitor {
            /// The value returned by this search.
            type BreakVal;
            /// Called when a node is examined by the depth-first search.
            ///
            /// By checking the value of `prior_status`, this visitor can determine whether the edge
            /// leading to this node was a tree edge (`None`), forward edge (`Some(Settled)`) or back edge
            /// (`Some(Visited)`). For a full explanation of each edge type, see the "Depth-first Search"
            /// chapter in [CLR] or [wikipedia].
            ///
            /// If you want to know *both* nodes linked by each edge, you'll need to modify
            /// `TriColorDepthFirstSearch` to store a `source` node for each `Visited` event.
            ///
            /// [wikipedia]: https://en.wikipedia.org/wiki/Depth-first_search#Output_of_a_depth-first_search
            /// [CLR]: https://en.wikipedia.org/wiki/Introduction_to_Algorithms
            fn node_examined(&mut self, _node: BasicBlock,
                             _prior_status: Option<NodeStatus>)
             -> ControlFlow<Self::BreakVal> {
                ControlFlow::Continue
            }
            /// Called after all nodes reachable from this one have been examined.
            fn node_settled(&mut self, _node: BasicBlock)
             -> ControlFlow<Self::BreakVal> {
                ControlFlow::Continue
            }
            /// Behave as if no edges exist from `source` to `target`.
            fn ignore_edge(&mut self, _source: BasicBlock,
                           _target: BasicBlock) -> bool {
                false
            }
        }
        /// This `TriColorVisitor` looks for back edges in a graph, which indicate that a cycle exists.
        pub struct CycleDetector;
        impl TriColorVisitor for CycleDetector {
            type BreakVal = ();
            fn node_examined(&mut self, _node: BasicBlock,
                             prior_status: Option<NodeStatus>)
             -> ControlFlow<Self::BreakVal> {
                match prior_status {
                    Some(NodeStatus::Visited) => ControlFlow::Break(()),
                    _ => ControlFlow::Continue,
                }
            }
        }
    }
    mod print {
        use crate::cfg::{BasicBlockData, ControlFlowGraph};
        use crate::{CfgFunctions, LocalKind, Mir, ModuleId,
                    VariableLocalKind};
        use diagnostics::ListPrettyPrinter;
        use std::io;
        use std::path::Path;
        const INDENT: &str = "    ";
        /// Alignment for lining up comments following MIR statements
        const ALIGN: usize = 120;
        impl <C: CfgFunctions> Mir<C> {
            pub fn print_header<W: io::Write>(&self, mut w: W)
             -> io::Result<()> {
                w.write_fmt(::core::fmt::Arguments::new_v1(&["OpenVAF MIR print; Intended for human consumption only (no parsing)\n"],
                                                           &match () {
                                                                () => [],
                                                            }))?;
                w.write_fmt(::core::fmt::Arguments::new_v1(&["NETS\n"],
                                                           &match () {
                                                                () => [],
                                                            }))?;
                for (net, info) in self.nets.iter_enumerated() {
                    w.write_fmt(::core::fmt::Arguments::new_v1(&["", "",
                                                                 " = ", "\n"],
                                                               &match (&INDENT,
                                                                       &net,
                                                                       &info)
                                                                    {
                                                                    (arg0,
                                                                     arg1,
                                                                     arg2) =>
                                                                    [::core::fmt::ArgumentV1::new(arg0,
                                                                                                  ::core::fmt::Display::fmt),
                                                                     ::core::fmt::ArgumentV1::new(arg1,
                                                                                                  ::core::fmt::Debug::fmt),
                                                                     ::core::fmt::ArgumentV1::new(arg2,
                                                                                                  ::core::fmt::Debug::fmt)],
                                                                }))?;
                }
                w.write_fmt(::core::fmt::Arguments::new_v1(&["\n"],
                                                           &match () {
                                                                () => [],
                                                            }))?;
                w.write_fmt(::core::fmt::Arguments::new_v1(&["BRANCHES\n"],
                                                           &match () {
                                                                () => [],
                                                            }))?;
                for (branch, info) in self.branches.iter_enumerated() {
                    w.write_fmt(::core::fmt::Arguments::new_v1(&["", "",
                                                                 " = ", "\n"],
                                                               &match (&INDENT,
                                                                       &branch,
                                                                       &info)
                                                                    {
                                                                    (arg0,
                                                                     arg1,
                                                                     arg2) =>
                                                                    [::core::fmt::ArgumentV1::new(arg0,
                                                                                                  ::core::fmt::Display::fmt),
                                                                     ::core::fmt::ArgumentV1::new(arg1,
                                                                                                  ::core::fmt::Debug::fmt),
                                                                     ::core::fmt::ArgumentV1::new(arg2,
                                                                                                  ::core::fmt::Debug::fmt)],
                                                                }))?;
                }
                w.write_fmt(::core::fmt::Arguments::new_v1(&["\n"],
                                                           &match () {
                                                                () => [],
                                                            }))?;
                w.write_fmt(::core::fmt::Arguments::new_v1(&["PARMETERS\n"],
                                                           &match () {
                                                                () => [],
                                                            }))?;
                for (param, info) in self.parameters.iter_enumerated() {
                    info.ty.with_info(|ty|
                                          {
                                              w.write_fmt(::core::fmt::Arguments::new_v1(&["",
                                                                                           "param ",
                                                                                           ": ",
                                                                                           " = ",
                                                                                           ";\n"],
                                                                                         &match (&INDENT,
                                                                                                 &param,
                                                                                                 &ty,
                                                                                                 &info.ident)
                                                                                              {
                                                                                              (arg0,
                                                                                               arg1,
                                                                                               arg2,
                                                                                               arg3)
                                                                                              =>
                                                                                              [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                            ::core::fmt::Display::fmt),
                                                                                               ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                            ::core::fmt::Display::fmt),
                                                                                               ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                            ::core::fmt::Display::fmt),
                                                                                               ::core::fmt::ArgumentV1::new(arg3,
                                                                                                                            ::core::fmt::Display::fmt)],
                                                                                          }))
                                          })?;
                }
                Ok(())
            }
            pub fn print_modules<W: io::Write>(&self, mut w: W)
             -> io::Result<()> {
                for module in self.modules.iter() {
                    w.write_fmt(::core::fmt::Arguments::new_v1(&["Module ",
                                                                 " \n"],
                                                               &match (&module.ident,)
                                                                    {
                                                                    (arg0,) =>
                                                                    [::core::fmt::ArgumentV1::new(arg0,
                                                                                                  ::core::fmt::Display::fmt)],
                                                                }))?;
                    for port in module.ports.clone() {
                        w.write_fmt(::core::fmt::Arguments::new_v1(&["",
                                                                     "PORTS\n"],
                                                                   &match (&INDENT,)
                                                                        {
                                                                        (arg0,)
                                                                        =>
                                                                        [::core::fmt::ArgumentV1::new(arg0,
                                                                                                      ::core::fmt::Display::fmt)],
                                                                    }))?;
                        let directions =
                            match (self.ports[port].output,
                                   self.ports[port].input) {
                                (true, true) => "inout",
                                (true, false) => "out",
                                (false, true) => "input",
                                _ => "ILLEGAL",
                            };
                        w.write_fmt(::core::fmt::Arguments::new_v1_formatted(&["",
                                                                               "",
                                                                               "port ",
                                                                               ": ",
                                                                               " = ",
                                                                               ";\n"],
                                                                             &match (&INDENT,
                                                                                     &port,
                                                                                     &directions,
                                                                                     &self.ports[port].net)
                                                                                  {
                                                                                  (arg0,
                                                                                   arg1,
                                                                                   arg2,
                                                                                   arg3)
                                                                                  =>
                                                                                  [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                ::core::fmt::Display::fmt),
                                                                                   ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                ::core::fmt::Display::fmt),
                                                                                   ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                ::core::fmt::Display::fmt),
                                                                                   ::core::fmt::ArgumentV1::new(arg3,
                                                                                                                ::core::fmt::Debug::fmt)],
                                                                              },
                                                                             &[::core::fmt::rt::v1::Argument{position:
                                                                                                                 0usize,
                                                                                                             format:
                                                                                                                 ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                     ' ',
                                                                                                                                                 align:
                                                                                                                                                     ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                 flags:
                                                                                                                                                     0u32,
                                                                                                                                                 precision:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                 width:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,},},
                                                                               ::core::fmt::rt::v1::Argument{position:
                                                                                                                 0usize,
                                                                                                             format:
                                                                                                                 ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                     ' ',
                                                                                                                                                 align:
                                                                                                                                                     ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                 flags:
                                                                                                                                                     0u32,
                                                                                                                                                 precision:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                 width:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,},},
                                                                               ::core::fmt::rt::v1::Argument{position:
                                                                                                                 1usize,
                                                                                                             format:
                                                                                                                 ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                     ' ',
                                                                                                                                                 align:
                                                                                                                                                     ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                 flags:
                                                                                                                                                     0u32,
                                                                                                                                                 precision:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                 width:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,},},
                                                                               ::core::fmt::rt::v1::Argument{position:
                                                                                                                 2usize,
                                                                                                             format:
                                                                                                                 ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                     ' ',
                                                                                                                                                 align:
                                                                                                                                                     ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                 flags:
                                                                                                                                                     0u32,
                                                                                                                                                 precision:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                 width:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,},},
                                                                               ::core::fmt::rt::v1::Argument{position:
                                                                                                                 3usize,
                                                                                                             format:
                                                                                                                 ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                     ' ',
                                                                                                                                                 align:
                                                                                                                                                     ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                 flags:
                                                                                                                                                     0u32,
                                                                                                                                                 precision:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                 width:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,},}]))?;
                    }
                    w.write_fmt(::core::fmt::Arguments::new_v1(&["\n"],
                                                               &match () {
                                                                    () => [],
                                                                }))?;
                    module.analog_cfg.borrow().print(self, &mut w)?;
                }
                Ok(())
            }
            pub fn print_modules_with_shared<W: io::Write>(&self, mut w: W,
                                                           id: ModuleId,
                                                           cfg:
                                                               &ControlFlowGraph<C>)
             -> io::Result<()> {
                for (module, info) in self.modules.iter_enumerated() {
                    w.write_fmt(::core::fmt::Arguments::new_v1(&["Module ",
                                                                 " \n"],
                                                               &match (&info.ident,)
                                                                    {
                                                                    (arg0,) =>
                                                                    [::core::fmt::ArgumentV1::new(arg0,
                                                                                                  ::core::fmt::Display::fmt)],
                                                                }))?;
                    for port in info.ports.clone() {
                        w.write_fmt(::core::fmt::Arguments::new_v1(&["",
                                                                     "PORTS\n"],
                                                                   &match (&INDENT,)
                                                                        {
                                                                        (arg0,)
                                                                        =>
                                                                        [::core::fmt::ArgumentV1::new(arg0,
                                                                                                      ::core::fmt::Display::fmt)],
                                                                    }))?;
                        let directions =
                            match (self.ports[port].output,
                                   self.ports[port].input) {
                                (true, true) => "inout",
                                (true, false) => "out",
                                (false, true) => "input",
                                _ => "ILLEGAL",
                            };
                        w.write_fmt(::core::fmt::Arguments::new_v1_formatted(&["",
                                                                               "",
                                                                               "port ",
                                                                               ": ",
                                                                               " = ",
                                                                               ";\n"],
                                                                             &match (&INDENT,
                                                                                     &port,
                                                                                     &directions,
                                                                                     &self.ports[port].net)
                                                                                  {
                                                                                  (arg0,
                                                                                   arg1,
                                                                                   arg2,
                                                                                   arg3)
                                                                                  =>
                                                                                  [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                ::core::fmt::Display::fmt),
                                                                                   ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                ::core::fmt::Display::fmt),
                                                                                   ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                ::core::fmt::Display::fmt),
                                                                                   ::core::fmt::ArgumentV1::new(arg3,
                                                                                                                ::core::fmt::Debug::fmt)],
                                                                              },
                                                                             &[::core::fmt::rt::v1::Argument{position:
                                                                                                                 0usize,
                                                                                                             format:
                                                                                                                 ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                     ' ',
                                                                                                                                                 align:
                                                                                                                                                     ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                 flags:
                                                                                                                                                     0u32,
                                                                                                                                                 precision:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                 width:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,},},
                                                                               ::core::fmt::rt::v1::Argument{position:
                                                                                                                 0usize,
                                                                                                             format:
                                                                                                                 ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                     ' ',
                                                                                                                                                 align:
                                                                                                                                                     ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                 flags:
                                                                                                                                                     0u32,
                                                                                                                                                 precision:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                 width:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,},},
                                                                               ::core::fmt::rt::v1::Argument{position:
                                                                                                                 1usize,
                                                                                                             format:
                                                                                                                 ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                     ' ',
                                                                                                                                                 align:
                                                                                                                                                     ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                 flags:
                                                                                                                                                     0u32,
                                                                                                                                                 precision:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                 width:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,},},
                                                                               ::core::fmt::rt::v1::Argument{position:
                                                                                                                 2usize,
                                                                                                             format:
                                                                                                                 ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                     ' ',
                                                                                                                                                 align:
                                                                                                                                                     ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                 flags:
                                                                                                                                                     0u32,
                                                                                                                                                 precision:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                 width:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,},},
                                                                               ::core::fmt::rt::v1::Argument{position:
                                                                                                                 3usize,
                                                                                                             format:
                                                                                                                 ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                     ' ',
                                                                                                                                                 align:
                                                                                                                                                     ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                 flags:
                                                                                                                                                     0u32,
                                                                                                                                                 precision:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                 width:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,},}]))?;
                    }
                    w.write_fmt(::core::fmt::Arguments::new_v1(&["\n"],
                                                               &match () {
                                                                    () => [],
                                                                }))?;
                    if module == id {
                        cfg.print(self, &mut w)?;
                    } else { info.analog_cfg.borrow().print(self, &mut w)?; }
                }
                Ok(())
            }
            pub fn print_to_file(&self, path: impl AsRef<Path>)
             -> io::Result<()> {
                let file = std::fs::File::create(path.as_ref())?;
                self.print(file)
            }
            pub fn print_to_file_with_shared(&self, path: impl AsRef<Path>,
                                             id: ModuleId,
                                             cfg: &ControlFlowGraph<C>)
             -> io::Result<()> {
                let file = std::fs::File::create(path.as_ref())?;
                self.print_with_shared(file, id, cfg)
            }
            pub fn print(&self, mut dst: impl io::Write) -> io::Result<()> {
                self.print_header(&mut dst)?;
                dst.write_fmt(::core::fmt::Arguments::new_v1(&["\n"],
                                                             &match () {
                                                                  () => [],
                                                              }))?;
                self.print_modules(&mut dst)
            }
            pub fn print_with_shared(&self, mut dst: impl io::Write,
                                     id: ModuleId, cfg: &ControlFlowGraph<C>)
             -> io::Result<()> {
                self.print_header(&mut dst)?;
                dst.write_fmt(::core::fmt::Arguments::new_v1(&["\n"],
                                                             &match () {
                                                                  () => [],
                                                              }))?;
                self.print_modules_with_shared(&mut dst, id, cfg)
            }
        }
        impl <C: CfgFunctions> ControlFlowGraph<C> {
            pub fn print<A: CfgFunctions,
                         W: io::Write>(&self, mir: &Mir<A>, mut w: W)
             -> io::Result<()> {
                w.write_fmt(::core::fmt::Arguments::new_v1(&["LOCALS\n"],
                                                           &match () {
                                                                () => [],
                                                            }))?;
                for (local, decl) in self.locals.iter_enumerated() {
                    let (decl, comment) =
                        match decl.kind {
                            LocalKind::Temporary => {
                                decl.ty.with_info(|ty|
                                                      w.write_fmt(::core::fmt::Arguments::new_v1_formatted(&["",
                                                                                                             "",
                                                                                                             "let ",
                                                                                                             ": ",
                                                                                                             ";\n"],
                                                                                                           &match (&INDENT,
                                                                                                                   &local,
                                                                                                                   &ty)
                                                                                                                {
                                                                                                                (arg0,
                                                                                                                 arg1,
                                                                                                                 arg2)
                                                                                                                =>
                                                                                                                [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                              ::core::fmt::Display::fmt),
                                                                                                                 ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                              ::core::fmt::Display::fmt),
                                                                                                                 ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                                              ::core::fmt::Display::fmt)],
                                                                                                            },
                                                                                                           &[::core::fmt::rt::v1::Argument{position:
                                                                                                                                               0usize,
                                                                                                                                           format:
                                                                                                                                               ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                                   ' ',
                                                                                                                                                                               align:
                                                                                                                                                                                   ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                               flags:
                                                                                                                                                                                   0u32,
                                                                                                                                                                               precision:
                                                                                                                                                                                   ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                               width:
                                                                                                                                                                                   ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                             ::core::fmt::rt::v1::Argument{position:
                                                                                                                                               0usize,
                                                                                                                                           format:
                                                                                                                                               ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                                   ' ',
                                                                                                                                                                               align:
                                                                                                                                                                                   ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                               flags:
                                                                                                                                                                                   0u32,
                                                                                                                                                                               precision:
                                                                                                                                                                                   ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                               width:
                                                                                                                                                                                   ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                             ::core::fmt::rt::v1::Argument{position:
                                                                                                                                               1usize,
                                                                                                                                           format:
                                                                                                                                               ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                                   ' ',
                                                                                                                                                                               align:
                                                                                                                                                                                   ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                               flags:
                                                                                                                                                                                   0u32,
                                                                                                                                                                               precision:
                                                                                                                                                                                   ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                               width:
                                                                                                                                                                                   ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                             ::core::fmt::rt::v1::Argument{position:
                                                                                                                                               2usize,
                                                                                                                                           format:
                                                                                                                                               ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                                   ' ',
                                                                                                                                                                               align:
                                                                                                                                                                                   ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                               flags:
                                                                                                                                                                                   0u32,
                                                                                                                                                                               precision:
                                                                                                                                                                                   ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                               width:
                                                                                                                                                                                   ::core::fmt::rt::v1::Count::Implied,},}])))?;
                                continue ;
                            }
                            LocalKind::Variable(var, VariableLocalKind::User)
                            =>
                            (decl.ty.with_info(|ty|
                                                   {
                                                       let res =
                                                           ::alloc::fmt::format(::core::fmt::Arguments::new_v1_formatted(&["",
                                                                                                                           "",
                                                                                                                           "let mut ",
                                                                                                                           ": ",
                                                                                                                           ";"],
                                                                                                                         &match (&INDENT,
                                                                                                                                 &local,
                                                                                                                                 &ty)
                                                                                                                              {
                                                                                                                              (arg0,
                                                                                                                               arg1,
                                                                                                                               arg2)
                                                                                                                              =>
                                                                                                                              [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                                            ::core::fmt::Display::fmt),
                                                                                                                               ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                                            ::core::fmt::Display::fmt),
                                                                                                                               ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                                                            ::core::fmt::Display::fmt)],
                                                                                                                          },
                                                                                                                         &[::core::fmt::rt::v1::Argument{position:
                                                                                                                                                             0usize,
                                                                                                                                                         format:
                                                                                                                                                             ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                                                 ' ',
                                                                                                                                                                                             align:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                                             flags:
                                                                                                                                                                                                 0u32,
                                                                                                                                                                                             precision:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                                             width:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                                           ::core::fmt::rt::v1::Argument{position:
                                                                                                                                                             0usize,
                                                                                                                                                         format:
                                                                                                                                                             ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                                                 ' ',
                                                                                                                                                                                             align:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                                             flags:
                                                                                                                                                                                                 0u32,
                                                                                                                                                                                             precision:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                                             width:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                                           ::core::fmt::rt::v1::Argument{position:
                                                                                                                                                             1usize,
                                                                                                                                                         format:
                                                                                                                                                             ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                                                 ' ',
                                                                                                                                                                                             align:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                                             flags:
                                                                                                                                                                                                 0u32,
                                                                                                                                                                                             precision:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                                             width:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                                           ::core::fmt::rt::v1::Argument{position:
                                                                                                                                                             2usize,
                                                                                                                                                         format:
                                                                                                                                                             ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                                                 ' ',
                                                                                                                                                                                             align:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                                             flags:
                                                                                                                                                                                                 0u32,
                                                                                                                                                                                             precision:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                                             width:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,},}]));
                                                       res
                                                   }),
                             {
                                 let res =
                                     ::alloc::fmt::format(::core::fmt::Arguments::new_v1(&["Corresponds to ",
                                                                                           " (",
                                                                                           ")"],
                                                                                         &match (&mir.variables[var].ident,
                                                                                                 &var)
                                                                                              {
                                                                                              (arg0,
                                                                                               arg1)
                                                                                              =>
                                                                                              [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                            ::core::fmt::Display::fmt),
                                                                                               ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                            ::core::fmt::Debug::fmt)],
                                                                                          }));
                                 res
                             }),
                            LocalKind::Variable(var,
                                                VariableLocalKind::Derivative(ref unkowns))
                            => {
                                let unkowns =
                                    ListPrettyPrinter{list:
                                                          unkowns.as_slice(),
                                                      prefix: "d/d",
                                                      postfix: " ",};
                                (decl.ty.with_info(|ty|
                                                       {
                                                           let res =
                                                               ::alloc::fmt::format(::core::fmt::Arguments::new_v1_formatted(&["",
                                                                                                                               "",
                                                                                                                               "let mut ",
                                                                                                                               ": ",
                                                                                                                               ";"],
                                                                                                                             &match (&INDENT,
                                                                                                                                     &local,
                                                                                                                                     &ty)
                                                                                                                                  {
                                                                                                                                  (arg0,
                                                                                                                                   arg1,
                                                                                                                                   arg2)
                                                                                                                                  =>
                                                                                                                                  [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                                                ::core::fmt::Display::fmt),
                                                                                                                                   ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                                                ::core::fmt::Display::fmt),
                                                                                                                                   ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                                                                ::core::fmt::Display::fmt)],
                                                                                                                              },
                                                                                                                             &[::core::fmt::rt::v1::Argument{position:
                                                                                                                                                                 0usize,
                                                                                                                                                             format:
                                                                                                                                                                 ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                                                     ' ',
                                                                                                                                                                                                 align:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                                                 flags:
                                                                                                                                                                                                     0u32,
                                                                                                                                                                                                 precision:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                                                 width:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                                               ::core::fmt::rt::v1::Argument{position:
                                                                                                                                                                 0usize,
                                                                                                                                                             format:
                                                                                                                                                                 ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                                                     ' ',
                                                                                                                                                                                                 align:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                                                 flags:
                                                                                                                                                                                                     0u32,
                                                                                                                                                                                                 precision:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                                                 width:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                                               ::core::fmt::rt::v1::Argument{position:
                                                                                                                                                                 1usize,
                                                                                                                                                             format:
                                                                                                                                                                 ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                                                     ' ',
                                                                                                                                                                                                 align:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                                                 flags:
                                                                                                                                                                                                     0u32,
                                                                                                                                                                                                 precision:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                                                 width:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                                               ::core::fmt::rt::v1::Argument{position:
                                                                                                                                                                 2usize,
                                                                                                                                                             format:
                                                                                                                                                                 ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                                                     ' ',
                                                                                                                                                                                                 align:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                                                 flags:
                                                                                                                                                                                                     0u32,
                                                                                                                                                                                                 precision:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                                                 width:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,},}]));
                                                           res
                                                       }),
                                 {
                                     let res =
                                         ::alloc::fmt::format(::core::fmt::Arguments::new_v1(&["Corresponds to ",
                                                                                               " ",
                                                                                               " (",
                                                                                               ")"],
                                                                                             &match (&unkowns,
                                                                                                     &mir.variables[var].ident,
                                                                                                     &var)
                                                                                                  {
                                                                                                  (arg0,
                                                                                                   arg1,
                                                                                                   arg2)
                                                                                                  =>
                                                                                                  [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                ::core::fmt::Display::fmt),
                                                                                                   ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                ::core::fmt::Display::fmt),
                                                                                                   ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                                ::core::fmt::Debug::fmt)],
                                                                                              }));
                                     res
                                 })
                            }
                            LocalKind::Branch(access, branch,
                                              VariableLocalKind::User) =>
                            (decl.ty.with_info(|ty|
                                                   {
                                                       let res =
                                                           ::alloc::fmt::format(::core::fmt::Arguments::new_v1_formatted(&["",
                                                                                                                           "",
                                                                                                                           "let mut ",
                                                                                                                           ": ",
                                                                                                                           ";"],
                                                                                                                         &match (&INDENT,
                                                                                                                                 &local,
                                                                                                                                 &ty)
                                                                                                                              {
                                                                                                                              (arg0,
                                                                                                                               arg1,
                                                                                                                               arg2)
                                                                                                                              =>
                                                                                                                              [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                                            ::core::fmt::Display::fmt),
                                                                                                                               ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                                            ::core::fmt::Display::fmt),
                                                                                                                               ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                                                            ::core::fmt::Display::fmt)],
                                                                                                                          },
                                                                                                                         &[::core::fmt::rt::v1::Argument{position:
                                                                                                                                                             0usize,
                                                                                                                                                         format:
                                                                                                                                                             ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                                                 ' ',
                                                                                                                                                                                             align:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                                             flags:
                                                                                                                                                                                                 0u32,
                                                                                                                                                                                             precision:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                                             width:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                                           ::core::fmt::rt::v1::Argument{position:
                                                                                                                                                             0usize,
                                                                                                                                                         format:
                                                                                                                                                             ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                                                 ' ',
                                                                                                                                                                                             align:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                                             flags:
                                                                                                                                                                                                 0u32,
                                                                                                                                                                                             precision:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                                             width:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                                           ::core::fmt::rt::v1::Argument{position:
                                                                                                                                                             1usize,
                                                                                                                                                         format:
                                                                                                                                                             ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                                                 ' ',
                                                                                                                                                                                             align:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                                             flags:
                                                                                                                                                                                                 0u32,
                                                                                                                                                                                             precision:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                                             width:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                                           ::core::fmt::rt::v1::Argument{position:
                                                                                                                                                             2usize,
                                                                                                                                                         format:
                                                                                                                                                             ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                                                 ' ',
                                                                                                                                                                                             align:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                                             flags:
                                                                                                                                                                                                 0u32,
                                                                                                                                                                                             precision:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                                             width:
                                                                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,},}]));
                                                       res
                                                   }),
                             {
                                 let res =
                                     ::alloc::fmt::format(::core::fmt::Arguments::new_v1(&["Corresponds to ",
                                                                                           "(",
                                                                                           ") "],
                                                                                         &match (&access,
                                                                                                 &mir.branches[branch].ident,
                                                                                                 &branch)
                                                                                              {
                                                                                              (arg0,
                                                                                               arg1,
                                                                                               arg2)
                                                                                              =>
                                                                                              [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                            ::core::fmt::Display::fmt),
                                                                                               ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                            ::core::fmt::Display::fmt),
                                                                                               ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                            ::core::fmt::Debug::fmt)],
                                                                                          }));
                                 res
                             }),
                            LocalKind::Branch(access, branch,
                                              VariableLocalKind::Derivative(ref unkowns))
                            => {
                                let unkowns =
                                    ListPrettyPrinter{list:
                                                          unkowns.as_slice(),
                                                      prefix: "d/d",
                                                      postfix: " ",};
                                (decl.ty.with_info(|ty|
                                                       {
                                                           let res =
                                                               ::alloc::fmt::format(::core::fmt::Arguments::new_v1_formatted(&["",
                                                                                                                               "",
                                                                                                                               "let mut ",
                                                                                                                               ": ",
                                                                                                                               ";"],
                                                                                                                             &match (&INDENT,
                                                                                                                                     &local,
                                                                                                                                     &ty)
                                                                                                                                  {
                                                                                                                                  (arg0,
                                                                                                                                   arg1,
                                                                                                                                   arg2)
                                                                                                                                  =>
                                                                                                                                  [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                                                ::core::fmt::Display::fmt),
                                                                                                                                   ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                                                ::core::fmt::Display::fmt),
                                                                                                                                   ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                                                                ::core::fmt::Display::fmt)],
                                                                                                                              },
                                                                                                                             &[::core::fmt::rt::v1::Argument{position:
                                                                                                                                                                 0usize,
                                                                                                                                                             format:
                                                                                                                                                                 ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                                                     ' ',
                                                                                                                                                                                                 align:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                                                 flags:
                                                                                                                                                                                                     0u32,
                                                                                                                                                                                                 precision:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                                                 width:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                                               ::core::fmt::rt::v1::Argument{position:
                                                                                                                                                                 0usize,
                                                                                                                                                             format:
                                                                                                                                                                 ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                                                     ' ',
                                                                                                                                                                                                 align:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                                                 flags:
                                                                                                                                                                                                     0u32,
                                                                                                                                                                                                 precision:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                                                 width:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                                               ::core::fmt::rt::v1::Argument{position:
                                                                                                                                                                 1usize,
                                                                                                                                                             format:
                                                                                                                                                                 ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                                                     ' ',
                                                                                                                                                                                                 align:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                                                 flags:
                                                                                                                                                                                                     0u32,
                                                                                                                                                                                                 precision:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                                                 width:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                                               ::core::fmt::rt::v1::Argument{position:
                                                                                                                                                                 2usize,
                                                                                                                                                             format:
                                                                                                                                                                 ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                                                     ' ',
                                                                                                                                                                                                 align:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                                                 flags:
                                                                                                                                                                                                     0u32,
                                                                                                                                                                                                 precision:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                                                 width:
                                                                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,},}]));
                                                           res
                                                       }),
                                 {
                                     let res =
                                         ::alloc::fmt::format(::core::fmt::Arguments::new_v1(&["Corresponds to a ",
                                                                                               " ",
                                                                                               "(",
                                                                                               "), "],
                                                                                             &match (&unkowns,
                                                                                                     &access,
                                                                                                     &mir.branches[branch].ident,
                                                                                                     &branch)
                                                                                                  {
                                                                                                  (arg0,
                                                                                                   arg1,
                                                                                                   arg2,
                                                                                                   arg3)
                                                                                                  =>
                                                                                                  [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                ::core::fmt::Display::fmt),
                                                                                                   ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                ::core::fmt::Display::fmt),
                                                                                                   ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                                ::core::fmt::Display::fmt),
                                                                                                   ::core::fmt::ArgumentV1::new(arg3,
                                                                                                                                ::core::fmt::Debug::fmt)],
                                                                                              }));
                                     res
                                 })
                            }
                        };
                    w.write_fmt(::core::fmt::Arguments::new_v1_formatted(&["",
                                                                           " // ",
                                                                           "\n"],
                                                                         &match (&decl,
                                                                                 &comment,
                                                                                 &ALIGN)
                                                                              {
                                                                              (arg0,
                                                                               arg1,
                                                                               arg2)
                                                                              =>
                                                                              [::core::fmt::ArgumentV1::new(arg0,
                                                                                                            ::core::fmt::Display::fmt),
                                                                               ::core::fmt::ArgumentV1::new(arg1,
                                                                                                            ::core::fmt::Display::fmt),
                                                                               ::core::fmt::ArgumentV1::from_usize(arg2)],
                                                                          },
                                                                         &[::core::fmt::rt::v1::Argument{position:
                                                                                                             0usize,
                                                                                                         format:
                                                                                                             ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                 ' ',
                                                                                                                                             align:
                                                                                                                                                 ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                             flags:
                                                                                                                                                 0u32,
                                                                                                                                             precision:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                             width:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Param(2usize),},},
                                                                           ::core::fmt::rt::v1::Argument{position:
                                                                                                             1usize,
                                                                                                         format:
                                                                                                             ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                 ' ',
                                                                                                                                             align:
                                                                                                                                                 ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                             flags:
                                                                                                                                                 0u32,
                                                                                                                                             precision:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                             width:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,},}]))?;
                }
                w.write_fmt(::core::fmt::Arguments::new_v1(&["\n"],
                                                           &match () {
                                                                () => [],
                                                            }))?;
                for (bb, body) in self.blocks.iter_enumerated() {
                    w.write_fmt(::core::fmt::Arguments::new_v1(&["", "",
                                                                 ":\n"],
                                                               &match (&INDENT,
                                                                       &bb) {
                                                                    (arg0,
                                                                     arg1) =>
                                                                    [::core::fmt::ArgumentV1::new(arg0,
                                                                                                  ::core::fmt::Display::fmt),
                                                                     ::core::fmt::ArgumentV1::new(arg1,
                                                                                                  ::core::fmt::Debug::fmt)],
                                                                }))?;
                    body.print(&mut w)?;
                    w.write_fmt(::core::fmt::Arguments::new_v1(&["\n"],
                                                               &match () {
                                                                    () => [],
                                                                }))?;
                }
                Ok(())
            }
        }
        impl <C: CfgFunctions> BasicBlockData<C> {
            pub fn print<W: io::Write>(&self, mut w: W) -> io::Result<()> {
                for (phi, info) in self.phi_statements.iter_enumerated() {
                    let content =
                        {
                            let res =
                                ::alloc::fmt::format(::core::fmt::Arguments::new_v1_formatted(&["",
                                                                                                "",
                                                                                                "",
                                                                                                ";"],
                                                                                              &match (&INDENT,
                                                                                                      &info)
                                                                                                   {
                                                                                                   (arg0,
                                                                                                    arg1)
                                                                                                   =>
                                                                                                   [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                 ::core::fmt::Display::fmt),
                                                                                                    ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                 ::core::fmt::Display::fmt)],
                                                                                               },
                                                                                              &[::core::fmt::rt::v1::Argument{position:
                                                                                                                                  0usize,
                                                                                                                              format:
                                                                                                                                  ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                      ' ',
                                                                                                                                                                  align:
                                                                                                                                                                      ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                  flags:
                                                                                                                                                                      0u32,
                                                                                                                                                                  precision:
                                                                                                                                                                      ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                  width:
                                                                                                                                                                      ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                ::core::fmt::rt::v1::Argument{position:
                                                                                                                                  0usize,
                                                                                                                              format:
                                                                                                                                  ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                      ' ',
                                                                                                                                                                  align:
                                                                                                                                                                      ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                  flags:
                                                                                                                                                                      0u32,
                                                                                                                                                                  precision:
                                                                                                                                                                      ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                  width:
                                                                                                                                                                      ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                ::core::fmt::rt::v1::Argument{position:
                                                                                                                                  1usize,
                                                                                                                              format:
                                                                                                                                  ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                      ' ',
                                                                                                                                                                  align:
                                                                                                                                                                      ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                  flags:
                                                                                                                                                                      0u32,
                                                                                                                                                                  precision:
                                                                                                                                                                      ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                  width:
                                                                                                                                                                      ::core::fmt::rt::v1::Count::Implied,},}]));
                            res
                        };
                    w.write_fmt(::core::fmt::Arguments::new_v1_formatted(&["",
                                                                           " // ",
                                                                           "\n"],
                                                                         &match (&content,
                                                                                 &phi,
                                                                                 &ALIGN)
                                                                              {
                                                                              (arg0,
                                                                               arg1,
                                                                               arg2)
                                                                              =>
                                                                              [::core::fmt::ArgumentV1::new(arg0,
                                                                                                            ::core::fmt::Display::fmt),
                                                                               ::core::fmt::ArgumentV1::new(arg1,
                                                                                                            ::core::fmt::Debug::fmt),
                                                                               ::core::fmt::ArgumentV1::from_usize(arg2)],
                                                                          },
                                                                         &[::core::fmt::rt::v1::Argument{position:
                                                                                                             0usize,
                                                                                                         format:
                                                                                                             ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                 ' ',
                                                                                                                                             align:
                                                                                                                                                 ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                             flags:
                                                                                                                                                 0u32,
                                                                                                                                             precision:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                             width:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Param(2usize),},},
                                                                           ::core::fmt::rt::v1::Argument{position:
                                                                                                             1usize,
                                                                                                         format:
                                                                                                             ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                 ' ',
                                                                                                                                             align:
                                                                                                                                                 ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                             flags:
                                                                                                                                                 0u32,
                                                                                                                                             precision:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                             width:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,},}]))?;
                }
                for (stmnt, (info, _)) in self.statements.iter_enumerated() {
                    let content =
                        {
                            let res =
                                ::alloc::fmt::format(::core::fmt::Arguments::new_v1_formatted(&["",
                                                                                                "",
                                                                                                "",
                                                                                                ";"],
                                                                                              &match (&INDENT,
                                                                                                      &info)
                                                                                                   {
                                                                                                   (arg0,
                                                                                                    arg1)
                                                                                                   =>
                                                                                                   [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                 ::core::fmt::Display::fmt),
                                                                                                    ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                 ::core::fmt::Display::fmt)],
                                                                                               },
                                                                                              &[::core::fmt::rt::v1::Argument{position:
                                                                                                                                  0usize,
                                                                                                                              format:
                                                                                                                                  ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                      ' ',
                                                                                                                                                                  align:
                                                                                                                                                                      ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                  flags:
                                                                                                                                                                      0u32,
                                                                                                                                                                  precision:
                                                                                                                                                                      ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                  width:
                                                                                                                                                                      ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                ::core::fmt::rt::v1::Argument{position:
                                                                                                                                  0usize,
                                                                                                                              format:
                                                                                                                                  ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                      ' ',
                                                                                                                                                                  align:
                                                                                                                                                                      ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                  flags:
                                                                                                                                                                      0u32,
                                                                                                                                                                  precision:
                                                                                                                                                                      ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                  width:
                                                                                                                                                                      ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                ::core::fmt::rt::v1::Argument{position:
                                                                                                                                  1usize,
                                                                                                                              format:
                                                                                                                                  ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                      ' ',
                                                                                                                                                                  align:
                                                                                                                                                                      ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                  flags:
                                                                                                                                                                      0u32,
                                                                                                                                                                  precision:
                                                                                                                                                                      ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                  width:
                                                                                                                                                                      ::core::fmt::rt::v1::Count::Implied,},}]));
                            res
                        };
                    w.write_fmt(::core::fmt::Arguments::new_v1_formatted(&["",
                                                                           " // ",
                                                                           "\n"],
                                                                         &match (&content,
                                                                                 &stmnt,
                                                                                 &ALIGN)
                                                                              {
                                                                              (arg0,
                                                                               arg1,
                                                                               arg2)
                                                                              =>
                                                                              [::core::fmt::ArgumentV1::new(arg0,
                                                                                                            ::core::fmt::Display::fmt),
                                                                               ::core::fmt::ArgumentV1::new(arg1,
                                                                                                            ::core::fmt::Debug::fmt),
                                                                               ::core::fmt::ArgumentV1::from_usize(arg2)],
                                                                          },
                                                                         &[::core::fmt::rt::v1::Argument{position:
                                                                                                             0usize,
                                                                                                         format:
                                                                                                             ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                 ' ',
                                                                                                                                             align:
                                                                                                                                                 ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                             flags:
                                                                                                                                                 0u32,
                                                                                                                                             precision:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                             width:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Param(2usize),},},
                                                                           ::core::fmt::rt::v1::Argument{position:
                                                                                                             1usize,
                                                                                                         format:
                                                                                                             ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                 ' ',
                                                                                                                                             align:
                                                                                                                                                 ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                             flags:
                                                                                                                                                 0u32,
                                                                                                                                             precision:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                             width:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,},}]))?;
                }
                match &self.terminator {
                    Some(term) if term.kind.is_loop_head() => {
                        let content =
                            {
                                let res =
                                    ::alloc::fmt::format(::core::fmt::Arguments::new_v1_formatted(&["",
                                                                                                    "",
                                                                                                    "",
                                                                                                    ";"],
                                                                                                  &match (&INDENT,
                                                                                                          &term.kind)
                                                                                                       {
                                                                                                       (arg0,
                                                                                                        arg1)
                                                                                                       =>
                                                                                                       [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                     ::core::fmt::Display::fmt),
                                                                                                        ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                     ::core::fmt::Display::fmt)],
                                                                                                   },
                                                                                                  &[::core::fmt::rt::v1::Argument{position:
                                                                                                                                      0usize,
                                                                                                                                  format:
                                                                                                                                      ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                          ' ',
                                                                                                                                                                      align:
                                                                                                                                                                          ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                      flags:
                                                                                                                                                                          0u32,
                                                                                                                                                                      precision:
                                                                                                                                                                          ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                      width:
                                                                                                                                                                          ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                    ::core::fmt::rt::v1::Argument{position:
                                                                                                                                      0usize,
                                                                                                                                  format:
                                                                                                                                      ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                          ' ',
                                                                                                                                                                      align:
                                                                                                                                                                          ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                      flags:
                                                                                                                                                                          0u32,
                                                                                                                                                                      precision:
                                                                                                                                                                          ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                      width:
                                                                                                                                                                          ::core::fmt::rt::v1::Count::Implied,},},
                                                                                                    ::core::fmt::rt::v1::Argument{position:
                                                                                                                                      1usize,
                                                                                                                                  format:
                                                                                                                                      ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                                          ' ',
                                                                                                                                                                      align:
                                                                                                                                                                          ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                                      flags:
                                                                                                                                                                          0u32,
                                                                                                                                                                      precision:
                                                                                                                                                                          ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                                      width:
                                                                                                                                                                          ::core::fmt::rt::v1::Count::Implied,},}]));
                                res
                            };
                        w.write_fmt(::core::fmt::Arguments::new_v1_formatted(&["",
                                                                               " // loop condition\n"],
                                                                             &match (&content,
                                                                                     &ALIGN)
                                                                                  {
                                                                                  (arg0,
                                                                                   arg1)
                                                                                  =>
                                                                                  [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                ::core::fmt::Display::fmt),
                                                                                   ::core::fmt::ArgumentV1::from_usize(arg1)],
                                                                              },
                                                                             &[::core::fmt::rt::v1::Argument{position:
                                                                                                                 0usize,
                                                                                                             format:
                                                                                                                 ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                     ' ',
                                                                                                                                                 align:
                                                                                                                                                     ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                                 flags:
                                                                                                                                                     0u32,
                                                                                                                                                 precision:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                                 width:
                                                                                                                                                     ::core::fmt::rt::v1::Count::Param(1usize),},}]))
                    }
                    Some(term) =>
                    w.write_fmt(::core::fmt::Arguments::new_v1_formatted(&["",
                                                                           "",
                                                                           "",
                                                                           ";\n"],
                                                                         &match (&INDENT,
                                                                                 &term.kind)
                                                                              {
                                                                              (arg0,
                                                                               arg1)
                                                                              =>
                                                                              [::core::fmt::ArgumentV1::new(arg0,
                                                                                                            ::core::fmt::Display::fmt),
                                                                               ::core::fmt::ArgumentV1::new(arg1,
                                                                                                            ::core::fmt::Display::fmt)],
                                                                          },
                                                                         &[::core::fmt::rt::v1::Argument{position:
                                                                                                             0usize,
                                                                                                         format:
                                                                                                             ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                 ' ',
                                                                                                                                             align:
                                                                                                                                                 ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                             flags:
                                                                                                                                                 0u32,
                                                                                                                                             precision:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                             width:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,},},
                                                                           ::core::fmt::rt::v1::Argument{position:
                                                                                                             0usize,
                                                                                                         format:
                                                                                                             ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                 ' ',
                                                                                                                                             align:
                                                                                                                                                 ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                             flags:
                                                                                                                                                 0u32,
                                                                                                                                             precision:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                             width:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,},},
                                                                           ::core::fmt::rt::v1::Argument{position:
                                                                                                             1usize,
                                                                                                         format:
                                                                                                             ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                 ' ',
                                                                                                                                             align:
                                                                                                                                                 ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                             flags:
                                                                                                                                                 0u32,
                                                                                                                                             precision:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                             width:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,},}])),
                    None =>
                    w.write_fmt(::core::fmt::Arguments::new_v1_formatted(&["",
                                                                           "",
                                                                           " MISSING TERMINATOR\n"],
                                                                         &match (&INDENT,)
                                                                              {
                                                                              (arg0,)
                                                                              =>
                                                                              [::core::fmt::ArgumentV1::new(arg0,
                                                                                                            ::core::fmt::Display::fmt)],
                                                                          },
                                                                         &[::core::fmt::rt::v1::Argument{position:
                                                                                                             0usize,
                                                                                                         format:
                                                                                                             ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                 ' ',
                                                                                                                                             align:
                                                                                                                                                 ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                             flags:
                                                                                                                                                 0u32,
                                                                                                                                             precision:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                             width:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,},},
                                                                           ::core::fmt::rt::v1::Argument{position:
                                                                                                             0usize,
                                                                                                         format:
                                                                                                             ::core::fmt::rt::v1::FormatSpec{fill:
                                                                                                                                                 ' ',
                                                                                                                                             align:
                                                                                                                                                 ::core::fmt::rt::v1::Alignment::Unknown,
                                                                                                                                             flags:
                                                                                                                                                 0u32,
                                                                                                                                             precision:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,
                                                                                                                                             width:
                                                                                                                                                 ::core::fmt::rt::v1::Count::Implied,},}])),
                }
            }
        }
    }
    #[repr(transparent)]
    pub struct BasicBlock {
        _raw: u16,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for BasicBlock { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for BasicBlock {
        #[inline]
        fn clone(&self) -> BasicBlock {
            { let _: ::core::clone::AssertParamIsClone<u16>; *self }
        }
    }
    impl ::core::marker::StructuralPartialEq for BasicBlock { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for BasicBlock {
        #[inline]
        fn eq(&self, other: &BasicBlock) -> bool {
            match *other {
                BasicBlock { _raw: ref __self_1_0 } =>
                match *self {
                    BasicBlock { _raw: ref __self_0_0 } =>
                    (*__self_0_0) == (*__self_1_0),
                },
            }
        }
        #[inline]
        fn ne(&self, other: &BasicBlock) -> bool {
            match *other {
                BasicBlock { _raw: ref __self_1_0 } =>
                match *self {
                    BasicBlock { _raw: ref __self_0_0 } =>
                    (*__self_0_0) != (*__self_1_0),
                },
            }
        }
    }
    impl ::core::marker::StructuralEq for BasicBlock { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for BasicBlock {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            { let _: ::core::cmp::AssertParamIsEq<u16>; }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::hash::Hash for BasicBlock {
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
            match *self {
                BasicBlock { _raw: ref __self_0_0 } => {
                    ::core::hash::Hash::hash(&(*__self_0_0), state)
                }
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialOrd for BasicBlock {
        #[inline]
        fn partial_cmp(&self, other: &BasicBlock)
         -> ::core::option::Option<::core::cmp::Ordering> {
            match *other {
                BasicBlock { _raw: ref __self_1_0 } =>
                match *self {
                    BasicBlock { _raw: ref __self_0_0 } =>
                    match ::core::cmp::PartialOrd::partial_cmp(&(*__self_0_0),
                                                               &(*__self_1_0))
                        {
                        ::core::option::Option::Some(::core::cmp::Ordering::Equal)
                        =>
                        ::core::option::Option::Some(::core::cmp::Ordering::Equal),
                        cmp => cmp,
                    },
                },
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Ord for BasicBlock {
        #[inline]
        fn cmp(&self, other: &BasicBlock) -> ::core::cmp::Ordering {
            match *other {
                BasicBlock { _raw: ref __self_1_0 } =>
                match *self {
                    BasicBlock { _raw: ref __self_0_0 } =>
                    match ::core::cmp::Ord::cmp(&(*__self_0_0),
                                                &(*__self_1_0)) {
                        ::core::cmp::Ordering::Equal =>
                        ::core::cmp::Ordering::Equal,
                        cmp => cmp,
                    },
                },
            }
        }
    }
    impl BasicBlock {
        /// If `Self::CHECKS_MAX_INDEX` is true, we'll assert if trying to
        /// produce a value larger than this in any of the ctors that don't
        /// have `unchecked` in their name.
        pub const MAX_INDEX: usize = (<u16>::max_value() as usize);
        /// Does this index type assert if asked to construct an index
        /// larger than MAX_INDEX?
        pub const CHECKS_MAX_INDEX: bool = !false;
        /// Construct this index type from a usize. Alias for `from_usize`.
        #[inline(always)]
        pub fn new(value: usize) -> Self { Self::from_usize(value) }
        /// Construct this index type from the wrapped integer type.
        #[inline(always)]
        pub fn from_raw(value: u16) -> Self {
            Self::from_usize(value as usize)
        }
        /// Construct this index type from one in a different domain
        #[inline(always)]
        pub fn from_foreign<F: ::index_vec::Idx>(value: F) -> Self {
            Self::from_usize(value.index())
        }
        /// Construct from a usize without any checks.
        #[inline(always)]
        pub const fn from_usize_unchecked(value: usize) -> Self {
            Self{_raw: value as u16,}
        }
        /// Construct from the underlying type without any checks.
        #[inline(always)]
        pub const fn from_raw_unchecked(raw: u16) -> Self { Self{_raw: raw,} }
        /// Construct this index type from a usize.
        #[inline]
        pub fn from_usize(value: usize) -> Self {
            Self::check_index(value as usize);
            Self{_raw: value as u16,}
        }
        /// Get the wrapped index as a usize.
        #[inline(always)]
        pub fn index(self) -> usize { self._raw as usize }
        /// Get the wrapped index.
        #[inline(always)]
        pub fn raw(self) -> u16 { self._raw }
        /// Asserts `v <= Self::MAX_INDEX` unless Self::CHECKS_MAX_INDEX is false.
        #[inline]
        pub fn check_index(v: usize) {
            if Self::CHECKS_MAX_INDEX && (v > Self::MAX_INDEX) {
                ::index_vec::__max_check_fail(v, Self::MAX_INDEX);
            }
        }
        const _ENSURE_RAW_IS_UNSIGNED: [(); 0] =
            [(); <u16>::min_value() as usize];
    }
    impl core::fmt::Debug for BasicBlock {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            f.write_fmt(::core::fmt::Arguments::new_v1(&["bb"],
                                                       &match (&self.index(),)
                                                            {
                                                            (arg0,) =>
                                                            [::core::fmt::ArgumentV1::new(arg0,
                                                                                          ::core::fmt::Display::fmt)],
                                                        }))
        }
    }
    impl core::cmp::PartialOrd<usize> for BasicBlock {
        #[inline]
        fn partial_cmp(&self, other: &usize) -> Option<core::cmp::Ordering> {
            self.index().partial_cmp(other)
        }
    }
    impl core::cmp::PartialOrd<BasicBlock> for usize {
        #[inline]
        fn partial_cmp(&self, other: &BasicBlock)
         -> Option<core::cmp::Ordering> {
            self.partial_cmp(&other.index())
        }
    }
    impl PartialEq<usize> for BasicBlock {
        #[inline]
        fn eq(&self, other: &usize) -> bool { self.index() == *other }
    }
    impl PartialEq<BasicBlock> for usize {
        #[inline]
        fn eq(&self, other: &BasicBlock) -> bool { *self == other.index() }
    }
    impl core::ops::Add<usize> for BasicBlock {
        type Output = Self;
        #[inline]
        fn add(self, other: usize) -> Self {
            Self::new(self.index().wrapping_add(other))
        }
    }
    impl core::ops::Sub<usize> for BasicBlock {
        type Output = Self;
        #[inline]
        fn sub(self, other: usize) -> Self {
            Self::new(self.index().wrapping_sub(other))
        }
    }
    impl core::ops::AddAssign<usize> for BasicBlock {
        #[inline]
        fn add_assign(&mut self, other: usize) { *self = *self + other }
    }
    impl core::ops::SubAssign<usize> for BasicBlock {
        #[inline]
        fn sub_assign(&mut self, other: usize) { *self = *self - other; }
    }
    impl core::ops::Rem<usize> for BasicBlock {
        type Output = Self;
        #[inline]
        fn rem(self, other: usize) -> Self { Self::new(self.index() % other) }
    }
    impl core::ops::Add<BasicBlock> for usize {
        type Output = BasicBlock;
        #[inline]
        fn add(self, other: BasicBlock) -> BasicBlock { other + self }
    }
    impl core::ops::Sub<BasicBlock> for usize {
        type Output = BasicBlock;
        #[inline]
        fn sub(self, other: BasicBlock) -> BasicBlock {
            BasicBlock::new(self.wrapping_sub(other.index()))
        }
    }
    impl core::ops::Add for BasicBlock {
        type Output = BasicBlock;
        #[inline]
        fn add(self, other: BasicBlock) -> BasicBlock {
            BasicBlock::new(other.index() + self.index())
        }
    }
    impl core::ops::Sub for BasicBlock {
        type Output = BasicBlock;
        #[inline]
        fn sub(self, other: BasicBlock) -> BasicBlock {
            BasicBlock::new(self.index().wrapping_sub(other.index()))
        }
    }
    impl core::ops::AddAssign for BasicBlock {
        #[inline]
        fn add_assign(&mut self, other: BasicBlock) { *self = *self + other }
    }
    impl core::ops::SubAssign for BasicBlock {
        #[inline]
        fn sub_assign(&mut self, other: BasicBlock) { *self = *self - other; }
    }
    impl ::index_vec::Idx for BasicBlock {
        #[inline]
        fn from_usize(value: usize) -> Self { Self::from(value) }
        #[inline]
        fn index(self) -> usize { usize::from(self) }
    }
    impl From<BasicBlock> for usize {
        #[inline]
        fn from(v: BasicBlock) -> usize { v.index() }
    }
    impl From<usize> for BasicBlock {
        #[inline]
        fn from(value: usize) -> Self { BasicBlock::from_usize(value) }
    }
    const _: [(); 1] = [(); true as usize];
    impl From<BasicBlock> for u16 {
        #[inline]
        fn from(v: BasicBlock) -> u16 { v.raw() }
    }
    impl From<u16> for BasicBlock {
        #[inline]
        fn from(value: u16) -> Self { Self::from_raw(value) }
    }
    impl core::fmt::Display for BasicBlock {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            f.write_fmt(::core::fmt::Arguments::new_v1(&["bb"],
                                                       &match (&self.index(),)
                                                            {
                                                            (arg0,) =>
                                                            [::core::fmt::ArgumentV1::new(arg0,
                                                                                          ::core::fmt::Display::fmt)],
                                                        }))
        }
    }
    #[repr(transparent)]
    pub struct IntLocation {
        _raw: u32,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for IntLocation { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for IntLocation {
        #[inline]
        fn clone(&self) -> IntLocation {
            { let _: ::core::clone::AssertParamIsClone<u32>; *self }
        }
    }
    impl ::core::marker::StructuralPartialEq for IntLocation { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for IntLocation {
        #[inline]
        fn eq(&self, other: &IntLocation) -> bool {
            match *other {
                IntLocation { _raw: ref __self_1_0 } =>
                match *self {
                    IntLocation { _raw: ref __self_0_0 } =>
                    (*__self_0_0) == (*__self_1_0),
                },
            }
        }
        #[inline]
        fn ne(&self, other: &IntLocation) -> bool {
            match *other {
                IntLocation { _raw: ref __self_1_0 } =>
                match *self {
                    IntLocation { _raw: ref __self_0_0 } =>
                    (*__self_0_0) != (*__self_1_0),
                },
            }
        }
    }
    impl ::core::marker::StructuralEq for IntLocation { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for IntLocation {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            { let _: ::core::cmp::AssertParamIsEq<u32>; }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::hash::Hash for IntLocation {
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
            match *self {
                IntLocation { _raw: ref __self_0_0 } => {
                    ::core::hash::Hash::hash(&(*__self_0_0), state)
                }
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialOrd for IntLocation {
        #[inline]
        fn partial_cmp(&self, other: &IntLocation)
         -> ::core::option::Option<::core::cmp::Ordering> {
            match *other {
                IntLocation { _raw: ref __self_1_0 } =>
                match *self {
                    IntLocation { _raw: ref __self_0_0 } =>
                    match ::core::cmp::PartialOrd::partial_cmp(&(*__self_0_0),
                                                               &(*__self_1_0))
                        {
                        ::core::option::Option::Some(::core::cmp::Ordering::Equal)
                        =>
                        ::core::option::Option::Some(::core::cmp::Ordering::Equal),
                        cmp => cmp,
                    },
                },
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Ord for IntLocation {
        #[inline]
        fn cmp(&self, other: &IntLocation) -> ::core::cmp::Ordering {
            match *other {
                IntLocation { _raw: ref __self_1_0 } =>
                match *self {
                    IntLocation { _raw: ref __self_0_0 } =>
                    match ::core::cmp::Ord::cmp(&(*__self_0_0),
                                                &(*__self_1_0)) {
                        ::core::cmp::Ordering::Equal =>
                        ::core::cmp::Ordering::Equal,
                        cmp => cmp,
                    },
                },
            }
        }
    }
    impl IntLocation {
        /// If `Self::CHECKS_MAX_INDEX` is true, we'll assert if trying to
        /// produce a value larger than this in any of the ctors that don't
        /// have `unchecked` in their name.
        pub const MAX_INDEX: usize = (<u32>::max_value() as usize);
        /// Does this index type assert if asked to construct an index
        /// larger than MAX_INDEX?
        pub const CHECKS_MAX_INDEX: bool = !false;
        /// Construct this index type from a usize. Alias for `from_usize`.
        #[inline(always)]
        pub fn new(value: usize) -> Self { Self::from_usize(value) }
        /// Construct this index type from the wrapped integer type.
        #[inline(always)]
        pub fn from_raw(value: u32) -> Self {
            Self::from_usize(value as usize)
        }
        /// Construct this index type from one in a different domain
        #[inline(always)]
        pub fn from_foreign<F: ::index_vec::Idx>(value: F) -> Self {
            Self::from_usize(value.index())
        }
        /// Construct from a usize without any checks.
        #[inline(always)]
        pub const fn from_usize_unchecked(value: usize) -> Self {
            Self{_raw: value as u32,}
        }
        /// Construct from the underlying type without any checks.
        #[inline(always)]
        pub const fn from_raw_unchecked(raw: u32) -> Self { Self{_raw: raw,} }
        /// Construct this index type from a usize.
        #[inline]
        pub fn from_usize(value: usize) -> Self {
            Self::check_index(value as usize);
            Self{_raw: value as u32,}
        }
        /// Get the wrapped index as a usize.
        #[inline(always)]
        pub fn index(self) -> usize { self._raw as usize }
        /// Get the wrapped index.
        #[inline(always)]
        pub fn raw(self) -> u32 { self._raw }
        /// Asserts `v <= Self::MAX_INDEX` unless Self::CHECKS_MAX_INDEX is false.
        #[inline]
        pub fn check_index(v: usize) {
            if Self::CHECKS_MAX_INDEX && (v > Self::MAX_INDEX) {
                ::index_vec::__max_check_fail(v, Self::MAX_INDEX);
            }
        }
        const _ENSURE_RAW_IS_UNSIGNED: [(); 0] =
            [(); <u32>::min_value() as usize];
    }
    impl core::fmt::Debug for IntLocation {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            f.write_fmt(::core::fmt::Arguments::new_v1(&["loc"],
                                                       &match (&self.index(),)
                                                            {
                                                            (arg0,) =>
                                                            [::core::fmt::ArgumentV1::new(arg0,
                                                                                          ::core::fmt::Display::fmt)],
                                                        }))
        }
    }
    impl core::cmp::PartialOrd<usize> for IntLocation {
        #[inline]
        fn partial_cmp(&self, other: &usize) -> Option<core::cmp::Ordering> {
            self.index().partial_cmp(other)
        }
    }
    impl core::cmp::PartialOrd<IntLocation> for usize {
        #[inline]
        fn partial_cmp(&self, other: &IntLocation)
         -> Option<core::cmp::Ordering> {
            self.partial_cmp(&other.index())
        }
    }
    impl PartialEq<usize> for IntLocation {
        #[inline]
        fn eq(&self, other: &usize) -> bool { self.index() == *other }
    }
    impl PartialEq<IntLocation> for usize {
        #[inline]
        fn eq(&self, other: &IntLocation) -> bool { *self == other.index() }
    }
    impl core::ops::Add<usize> for IntLocation {
        type Output = Self;
        #[inline]
        fn add(self, other: usize) -> Self {
            Self::new(self.index().wrapping_add(other))
        }
    }
    impl core::ops::Sub<usize> for IntLocation {
        type Output = Self;
        #[inline]
        fn sub(self, other: usize) -> Self {
            Self::new(self.index().wrapping_sub(other))
        }
    }
    impl core::ops::AddAssign<usize> for IntLocation {
        #[inline]
        fn add_assign(&mut self, other: usize) { *self = *self + other }
    }
    impl core::ops::SubAssign<usize> for IntLocation {
        #[inline]
        fn sub_assign(&mut self, other: usize) { *self = *self - other; }
    }
    impl core::ops::Rem<usize> for IntLocation {
        type Output = Self;
        #[inline]
        fn rem(self, other: usize) -> Self { Self::new(self.index() % other) }
    }
    impl core::ops::Add<IntLocation> for usize {
        type Output = IntLocation;
        #[inline]
        fn add(self, other: IntLocation) -> IntLocation { other + self }
    }
    impl core::ops::Sub<IntLocation> for usize {
        type Output = IntLocation;
        #[inline]
        fn sub(self, other: IntLocation) -> IntLocation {
            IntLocation::new(self.wrapping_sub(other.index()))
        }
    }
    impl core::ops::Add for IntLocation {
        type Output = IntLocation;
        #[inline]
        fn add(self, other: IntLocation) -> IntLocation {
            IntLocation::new(other.index() + self.index())
        }
    }
    impl core::ops::Sub for IntLocation {
        type Output = IntLocation;
        #[inline]
        fn sub(self, other: IntLocation) -> IntLocation {
            IntLocation::new(self.index().wrapping_sub(other.index()))
        }
    }
    impl core::ops::AddAssign for IntLocation {
        #[inline]
        fn add_assign(&mut self, other: IntLocation) { *self = *self + other }
    }
    impl core::ops::SubAssign for IntLocation {
        #[inline]
        fn sub_assign(&mut self, other: IntLocation) {
            *self = *self - other;
        }
    }
    impl ::index_vec::Idx for IntLocation {
        #[inline]
        fn from_usize(value: usize) -> Self { Self::from(value) }
        #[inline]
        fn index(self) -> usize { usize::from(self) }
    }
    impl From<IntLocation> for usize {
        #[inline]
        fn from(v: IntLocation) -> usize { v.index() }
    }
    impl From<usize> for IntLocation {
        #[inline]
        fn from(value: usize) -> Self { IntLocation::from_usize(value) }
    }
    const _: [(); 1] = [(); true as usize];
    impl From<IntLocation> for u32 {
        #[inline]
        fn from(v: IntLocation) -> u32 { v.raw() }
    }
    impl From<u32> for IntLocation {
        #[inline]
        fn from(value: u32) -> Self { Self::from_raw(value) }
    }
    impl core::fmt::Display for IntLocation {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            f.write_fmt(::core::fmt::Arguments::new_v1(&["loc"],
                                                       &match (&self.index(),)
                                                            {
                                                            (arg0,) =>
                                                            [::core::fmt::ArgumentV1::new(arg0,
                                                                                          ::core::fmt::Display::fmt)],
                                                        }))
        }
    }
    #[repr(transparent)]
    pub struct Phi {
        _raw: u16,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for Phi { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for Phi {
        #[inline]
        fn clone(&self) -> Phi {
            { let _: ::core::clone::AssertParamIsClone<u16>; *self }
        }
    }
    impl ::core::marker::StructuralPartialEq for Phi { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for Phi {
        #[inline]
        fn eq(&self, other: &Phi) -> bool {
            match *other {
                Phi { _raw: ref __self_1_0 } =>
                match *self {
                    Phi { _raw: ref __self_0_0 } =>
                    (*__self_0_0) == (*__self_1_0),
                },
            }
        }
        #[inline]
        fn ne(&self, other: &Phi) -> bool {
            match *other {
                Phi { _raw: ref __self_1_0 } =>
                match *self {
                    Phi { _raw: ref __self_0_0 } =>
                    (*__self_0_0) != (*__self_1_0),
                },
            }
        }
    }
    impl ::core::marker::StructuralEq for Phi { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for Phi {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            { let _: ::core::cmp::AssertParamIsEq<u16>; }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::hash::Hash for Phi {
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
            match *self {
                Phi { _raw: ref __self_0_0 } => {
                    ::core::hash::Hash::hash(&(*__self_0_0), state)
                }
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialOrd for Phi {
        #[inline]
        fn partial_cmp(&self, other: &Phi)
         -> ::core::option::Option<::core::cmp::Ordering> {
            match *other {
                Phi { _raw: ref __self_1_0 } =>
                match *self {
                    Phi { _raw: ref __self_0_0 } =>
                    match ::core::cmp::PartialOrd::partial_cmp(&(*__self_0_0),
                                                               &(*__self_1_0))
                        {
                        ::core::option::Option::Some(::core::cmp::Ordering::Equal)
                        =>
                        ::core::option::Option::Some(::core::cmp::Ordering::Equal),
                        cmp => cmp,
                    },
                },
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Ord for Phi {
        #[inline]
        fn cmp(&self, other: &Phi) -> ::core::cmp::Ordering {
            match *other {
                Phi { _raw: ref __self_1_0 } =>
                match *self {
                    Phi { _raw: ref __self_0_0 } =>
                    match ::core::cmp::Ord::cmp(&(*__self_0_0),
                                                &(*__self_1_0)) {
                        ::core::cmp::Ordering::Equal =>
                        ::core::cmp::Ordering::Equal,
                        cmp => cmp,
                    },
                },
            }
        }
    }
    impl Phi {
        /// If `Self::CHECKS_MAX_INDEX` is true, we'll assert if trying to
        /// produce a value larger than this in any of the ctors that don't
        /// have `unchecked` in their name.
        pub const MAX_INDEX: usize = (<u16>::max_value() as usize);
        /// Does this index type assert if asked to construct an index
        /// larger than MAX_INDEX?
        pub const CHECKS_MAX_INDEX: bool = !false;
        /// Construct this index type from a usize. Alias for `from_usize`.
        #[inline(always)]
        pub fn new(value: usize) -> Self { Self::from_usize(value) }
        /// Construct this index type from the wrapped integer type.
        #[inline(always)]
        pub fn from_raw(value: u16) -> Self {
            Self::from_usize(value as usize)
        }
        /// Construct this index type from one in a different domain
        #[inline(always)]
        pub fn from_foreign<F: ::index_vec::Idx>(value: F) -> Self {
            Self::from_usize(value.index())
        }
        /// Construct from a usize without any checks.
        #[inline(always)]
        pub const fn from_usize_unchecked(value: usize) -> Self {
            Self{_raw: value as u16,}
        }
        /// Construct from the underlying type without any checks.
        #[inline(always)]
        pub const fn from_raw_unchecked(raw: u16) -> Self { Self{_raw: raw,} }
        /// Construct this index type from a usize.
        #[inline]
        pub fn from_usize(value: usize) -> Self {
            Self::check_index(value as usize);
            Self{_raw: value as u16,}
        }
        /// Get the wrapped index as a usize.
        #[inline(always)]
        pub fn index(self) -> usize { self._raw as usize }
        /// Get the wrapped index.
        #[inline(always)]
        pub fn raw(self) -> u16 { self._raw }
        /// Asserts `v <= Self::MAX_INDEX` unless Self::CHECKS_MAX_INDEX is false.
        #[inline]
        pub fn check_index(v: usize) {
            if Self::CHECKS_MAX_INDEX && (v > Self::MAX_INDEX) {
                ::index_vec::__max_check_fail(v, Self::MAX_INDEX);
            }
        }
        const _ENSURE_RAW_IS_UNSIGNED: [(); 0] =
            [(); <u16>::min_value() as usize];
    }
    impl core::fmt::Debug for Phi {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            f.write_fmt(::core::fmt::Arguments::new_v1(&["phi"],
                                                       &match (&self.index(),)
                                                            {
                                                            (arg0,) =>
                                                            [::core::fmt::ArgumentV1::new(arg0,
                                                                                          ::core::fmt::Display::fmt)],
                                                        }))
        }
    }
    impl core::cmp::PartialOrd<usize> for Phi {
        #[inline]
        fn partial_cmp(&self, other: &usize) -> Option<core::cmp::Ordering> {
            self.index().partial_cmp(other)
        }
    }
    impl core::cmp::PartialOrd<Phi> for usize {
        #[inline]
        fn partial_cmp(&self, other: &Phi) -> Option<core::cmp::Ordering> {
            self.partial_cmp(&other.index())
        }
    }
    impl PartialEq<usize> for Phi {
        #[inline]
        fn eq(&self, other: &usize) -> bool { self.index() == *other }
    }
    impl PartialEq<Phi> for usize {
        #[inline]
        fn eq(&self, other: &Phi) -> bool { *self == other.index() }
    }
    impl core::ops::Add<usize> for Phi {
        type Output = Self;
        #[inline]
        fn add(self, other: usize) -> Self {
            Self::new(self.index().wrapping_add(other))
        }
    }
    impl core::ops::Sub<usize> for Phi {
        type Output = Self;
        #[inline]
        fn sub(self, other: usize) -> Self {
            Self::new(self.index().wrapping_sub(other))
        }
    }
    impl core::ops::AddAssign<usize> for Phi {
        #[inline]
        fn add_assign(&mut self, other: usize) { *self = *self + other }
    }
    impl core::ops::SubAssign<usize> for Phi {
        #[inline]
        fn sub_assign(&mut self, other: usize) { *self = *self - other; }
    }
    impl core::ops::Rem<usize> for Phi {
        type Output = Self;
        #[inline]
        fn rem(self, other: usize) -> Self { Self::new(self.index() % other) }
    }
    impl core::ops::Add<Phi> for usize {
        type Output = Phi;
        #[inline]
        fn add(self, other: Phi) -> Phi { other + self }
    }
    impl core::ops::Sub<Phi> for usize {
        type Output = Phi;
        #[inline]
        fn sub(self, other: Phi) -> Phi {
            Phi::new(self.wrapping_sub(other.index()))
        }
    }
    impl core::ops::Add for Phi {
        type Output = Phi;
        #[inline]
        fn add(self, other: Phi) -> Phi {
            Phi::new(other.index() + self.index())
        }
    }
    impl core::ops::Sub for Phi {
        type Output = Phi;
        #[inline]
        fn sub(self, other: Phi) -> Phi {
            Phi::new(self.index().wrapping_sub(other.index()))
        }
    }
    impl core::ops::AddAssign for Phi {
        #[inline]
        fn add_assign(&mut self, other: Phi) { *self = *self + other }
    }
    impl core::ops::SubAssign for Phi {
        #[inline]
        fn sub_assign(&mut self, other: Phi) { *self = *self - other; }
    }
    impl ::index_vec::Idx for Phi {
        #[inline]
        fn from_usize(value: usize) -> Self { Self::from(value) }
        #[inline]
        fn index(self) -> usize { usize::from(self) }
    }
    impl From<Phi> for usize {
        #[inline]
        fn from(v: Phi) -> usize { v.index() }
    }
    impl From<usize> for Phi {
        #[inline]
        fn from(value: usize) -> Self { Phi::from_usize(value) }
    }
    const _: [(); 1] = [(); true as usize];
    impl From<Phi> for u16 {
        #[inline]
        fn from(v: Phi) -> u16 { v.raw() }
    }
    impl From<u16> for Phi {
        #[inline]
        fn from(value: u16) -> Self { Self::from_raw(value) }
    }
    impl core::fmt::Display for Phi {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            f.write_fmt(::core::fmt::Arguments::new_v1(&["phi"],
                                                       &match (&self.index(),)
                                                            {
                                                            (arg0,) =>
                                                            [::core::fmt::ArgumentV1::new(arg0,
                                                                                          ::core::fmt::Display::fmt)],
                                                        }))
        }
    }
    impl <C: CfgFunctions> ::std::ops::Index<BasicBlock> for
     ControlFlowGraph<C> {
        type Output = BasicBlockData<C>;
        fn index(&self, index: BasicBlock) -> &Self::Output {
            &self.blocks[index]
        }
    }
    impl <C: CfgFunctions> ::std::ops::IndexMut<BasicBlock> for
     ControlFlowGraph<C> {
        fn index_mut(&mut self, index: BasicBlock) -> &mut Self::Output {
            &mut self.blocks[index]
        }
    }
    impl <C: CfgFunctions> ::std::ops::Index<::core::ops::Range<BasicBlock>>
     for ControlFlowGraph<C> {
        type Output =
         data_structures::index_vec::IndexSlice<BasicBlock,
                                                        [BasicBlockData<C>]>;
        fn index(&self, range: ::core::ops::Range<BasicBlock>)
         -> &Self::Output {
            &self.blocks[range]
        }
    }
    impl <C: CfgFunctions>
     ::std::ops::IndexMut<::core::ops::Range<BasicBlock>> for
     ControlFlowGraph<C> {
        fn index_mut(&mut self, range: ::core::ops::Range<BasicBlock>)
         -> &mut Self::Output {
            &mut self.blocks[range]
        }
    }
    impl <C: CfgFunctions>
     ::std::ops::Index<::ir::ids::IdRange<BasicBlock>> for
     ControlFlowGraph<C> {
        type Output =
         ::data_structures::index_vec::IndexSlice<BasicBlock,
                                                          [BasicBlockData<C>]>;
        fn index(&self, range: ::ir::ids::IdRange<BasicBlock>)
         -> &Self::Output {
            &self.blocks[range.0]
        }
    }
    impl <C: CfgFunctions>
     ::std::ops::IndexMut<::ir::ids::IdRange<BasicBlock>> for
     ControlFlowGraph<C> {
        fn index_mut(&mut self, range: ::ir::ids::IdRange<BasicBlock>)
         -> &mut Self::Output {
            &mut self.blocks[range.0]
        }
    }
    pub struct BlockLocations {
        pub phi_start: IntLocation,
        pub stmnt_start: IntLocation,
        pub terminator: IntLocation,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for BlockLocations {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {
                BlockLocations {
                phi_start: ref __self_0_0,
                stmnt_start: ref __self_0_1,
                terminator: ref __self_0_2 } => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_struct(f,
                                                                  "BlockLocations");
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "phi_start",
                                                        &&(*__self_0_0));
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "stmnt_start",
                                                        &&(*__self_0_1));
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "terminator",
                                                        &&(*__self_0_2));
                    ::core::fmt::DebugStruct::finish(debug_trait_builder)
                }
            }
        }
    }
    impl Display for BlockLocations {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            if self.phi_start != self.stmnt_start {
                f.write_fmt(::core::fmt::Arguments::new_v1(&["Phis from ",
                                                             " to ", ";"],
                                                           &match (&self.phi_start,
                                                                   &self.stmnt_start)
                                                                {
                                                                (arg0, arg1)
                                                                =>
                                                                [::core::fmt::ArgumentV1::new(arg0,
                                                                                              ::core::fmt::Display::fmt),
                                                                 ::core::fmt::ArgumentV1::new(arg1,
                                                                                              ::core::fmt::Display::fmt)],
                                                            }))?
            }
            if self.stmnt_start != self.terminator {
                f.write_fmt(::core::fmt::Arguments::new_v1(&["Stmts from ",
                                                             " to ", ";"],
                                                           &match (&self.stmnt_start,
                                                                   &self.terminator)
                                                                {
                                                                (arg0, arg1)
                                                                =>
                                                                [::core::fmt::ArgumentV1::new(arg0,
                                                                                              ::core::fmt::Display::fmt),
                                                                 ::core::fmt::ArgumentV1::new(arg1,
                                                                                              ::core::fmt::Display::fmt)],
                                                            }))?
            }
            f.write_fmt(::core::fmt::Arguments::new_v1(&["Terminator at "],
                                                       &match (&self.terminator,)
                                                            {
                                                            (arg0,) =>
                                                            [::core::fmt::ArgumentV1::new(arg0,
                                                                                          ::core::fmt::Display::fmt)],
                                                        }))
        }
    }
    pub struct InternedLocations {
        pub locations: IndexVec<IntLocation, Location>,
        pub blocks: IndexVec<BasicBlock, BlockLocations>,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for InternedLocations {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {
                InternedLocations {
                locations: ref __self_0_0, blocks: ref __self_0_1 } => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_struct(f,
                                                                  "InternedLocations");
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "locations",
                                                        &&(*__self_0_0));
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "blocks",
                                                        &&(*__self_0_1));
                    ::core::fmt::DebugStruct::finish(debug_trait_builder)
                }
            }
        }
    }
    impl Display for InternedLocations {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            for (bb, locations) in self.blocks.iter_enumerated() {
                f.write_fmt(::core::fmt::Arguments::new_v1(&["BB ", ": ",
                                                             "\n"],
                                                           &match (&bb,
                                                                   &locations)
                                                                {
                                                                (arg0, arg1)
                                                                =>
                                                                [::core::fmt::ArgumentV1::new(arg0,
                                                                                              ::core::fmt::Display::fmt),
                                                                 ::core::fmt::ArgumentV1::new(arg1,
                                                                                              ::core::fmt::Display::fmt)],
                                                            }))?;
            }
            Ok(())
        }
    }
    impl InternedLocations {
        pub fn len(&self) -> usize { self.locations.len() }
        pub fn is_empty(&self) -> bool { self.locations.is_empty() }
        pub fn len_idx(&self) -> IntLocation { self.locations.len_idx() }
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
    pub struct Location {
        pub block: BasicBlock,
        pub kind: LocationKind,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for Location {
        #[inline]
        fn clone(&self) -> Location {
            {
                let _: ::core::clone::AssertParamIsClone<BasicBlock>;
                let _: ::core::clone::AssertParamIsClone<LocationKind>;
                *self
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for Location { }
    impl ::core::marker::StructuralEq for Location { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for Location {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            {
                let _: ::core::cmp::AssertParamIsEq<BasicBlock>;
                let _: ::core::cmp::AssertParamIsEq<LocationKind>;
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for Location { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for Location {
        #[inline]
        fn eq(&self, other: &Location) -> bool {
            match *other {
                Location { block: ref __self_1_0, kind: ref __self_1_1 } =>
                match *self {
                    Location { block: ref __self_0_0, kind: ref __self_0_1 }
                    =>
                    (*__self_0_0) == (*__self_1_0) &&
                        (*__self_0_1) == (*__self_1_1),
                },
            }
        }
        #[inline]
        fn ne(&self, other: &Location) -> bool {
            match *other {
                Location { block: ref __self_1_0, kind: ref __self_1_1 } =>
                match *self {
                    Location { block: ref __self_0_0, kind: ref __self_0_1 }
                    =>
                    (*__self_0_0) != (*__self_1_0) ||
                        (*__self_0_1) != (*__self_1_1),
                },
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::hash::Hash for Location {
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
            match *self {
                Location { block: ref __self_0_0, kind: ref __self_0_1 } => {
                    ::core::hash::Hash::hash(&(*__self_0_0), state);
                    ::core::hash::Hash::hash(&(*__self_0_1), state)
                }
            }
        }
    }
    impl Debug for Location {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            Display::fmt(self, f)
        }
    }
    impl Display for Location {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            f.write_fmt(::core::fmt::Arguments::new_v1(&["", " -> "],
                                                       &match (&self.block,
                                                               &self.kind) {
                                                            (arg0, arg1) =>
                                                            [::core::fmt::ArgumentV1::new(arg0,
                                                                                          ::core::fmt::Debug::fmt),
                                                             ::core::fmt::ArgumentV1::new(arg1,
                                                                                          ::core::fmt::Debug::fmt)],
                                                        }))
        }
    }
    pub enum LocationKind { Phi(Phi), Statement(StatementId), Terminator, }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialOrd for LocationKind {
        #[inline]
        fn partial_cmp(&self, other: &LocationKind)
         -> ::core::option::Option<::core::cmp::Ordering> {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        (&LocationKind::Phi(ref __self_0),
                         &LocationKind::Phi(ref __arg_1_0)) =>
                        match ::core::cmp::PartialOrd::partial_cmp(&(*__self_0),
                                                                   &(*__arg_1_0))
                            {
                            ::core::option::Option::Some(::core::cmp::Ordering::Equal)
                            =>
                            ::core::option::Option::Some(::core::cmp::Ordering::Equal),
                            cmp => cmp,
                        },
                        (&LocationKind::Statement(ref __self_0),
                         &LocationKind::Statement(ref __arg_1_0)) =>
                        match ::core::cmp::PartialOrd::partial_cmp(&(*__self_0),
                                                                   &(*__arg_1_0))
                            {
                            ::core::option::Option::Some(::core::cmp::Ordering::Equal)
                            =>
                            ::core::option::Option::Some(::core::cmp::Ordering::Equal),
                            cmp => cmp,
                        },
                        _ =>
                        ::core::option::Option::Some(::core::cmp::Ordering::Equal),
                    }
                } else {
                    ::core::cmp::PartialOrd::partial_cmp(&__self_vi,
                                                         &__arg_1_vi)
                }
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Ord for LocationKind {
        #[inline]
        fn cmp(&self, other: &LocationKind) -> ::core::cmp::Ordering {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        (&LocationKind::Phi(ref __self_0),
                         &LocationKind::Phi(ref __arg_1_0)) =>
                        match ::core::cmp::Ord::cmp(&(*__self_0),
                                                    &(*__arg_1_0)) {
                            ::core::cmp::Ordering::Equal =>
                            ::core::cmp::Ordering::Equal,
                            cmp => cmp,
                        },
                        (&LocationKind::Statement(ref __self_0),
                         &LocationKind::Statement(ref __arg_1_0)) =>
                        match ::core::cmp::Ord::cmp(&(*__self_0),
                                                    &(*__arg_1_0)) {
                            ::core::cmp::Ordering::Equal =>
                            ::core::cmp::Ordering::Equal,
                            cmp => cmp,
                        },
                        _ => ::core::cmp::Ordering::Equal,
                    }
                } else { ::core::cmp::Ord::cmp(&__self_vi, &__arg_1_vi) }
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for LocationKind {
        #[inline]
        fn clone(&self) -> LocationKind {
            {
                let _: ::core::clone::AssertParamIsClone<Phi>;
                let _: ::core::clone::AssertParamIsClone<StatementId>;
                *self
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for LocationKind { }
    impl ::core::marker::StructuralEq for LocationKind { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for LocationKind {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            {
                let _: ::core::cmp::AssertParamIsEq<Phi>;
                let _: ::core::cmp::AssertParamIsEq<StatementId>;
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for LocationKind { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for LocationKind {
        #[inline]
        fn eq(&self, other: &LocationKind) -> bool {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        (&LocationKind::Phi(ref __self_0),
                         &LocationKind::Phi(ref __arg_1_0)) =>
                        (*__self_0) == (*__arg_1_0),
                        (&LocationKind::Statement(ref __self_0),
                         &LocationKind::Statement(ref __arg_1_0)) =>
                        (*__self_0) == (*__arg_1_0),
                        _ => true,
                    }
                } else { false }
            }
        }
        #[inline]
        fn ne(&self, other: &LocationKind) -> bool {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        (&LocationKind::Phi(ref __self_0),
                         &LocationKind::Phi(ref __arg_1_0)) =>
                        (*__self_0) != (*__arg_1_0),
                        (&LocationKind::Statement(ref __self_0),
                         &LocationKind::Statement(ref __arg_1_0)) =>
                        (*__self_0) != (*__arg_1_0),
                        _ => false,
                    }
                } else { true }
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::hash::Hash for LocationKind {
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
            match (&*self,) {
                (&LocationKind::Phi(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self),
                                             state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&LocationKind::Statement(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self),
                                             state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                _ => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self),
                                             state)
                }
            }
        }
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
    pub struct ControlFlowGraph<C: CfgFunctions = DefaultFunctions> {
        /// The external input that the ControlFlowGraph requires
        pub locals: IndexVec<Local, LocalDeclaration>,
        pub blocks: IndexVec<BasicBlock, BasicBlockData<C>>,
        pub derivatives: HashMap<Local, HashMap<Unknown, Local>>,
        pub predecessor_cache: PredecessorCache,
        pub is_cyclic: GraphIsCyclicCache,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl <C: ::core::clone::Clone + CfgFunctions> ::core::clone::Clone for
     ControlFlowGraph<C> {
        #[inline]
        fn clone(&self) -> ControlFlowGraph<C> {
            match *self {
                ControlFlowGraph {
                locals: ref __self_0_0,
                blocks: ref __self_0_1,
                derivatives: ref __self_0_2,
                predecessor_cache: ref __self_0_3,
                is_cyclic: ref __self_0_4 } =>
                ControlFlowGraph{locals:
                                     ::core::clone::Clone::clone(&(*__self_0_0)),
                                 blocks:
                                     ::core::clone::Clone::clone(&(*__self_0_1)),
                                 derivatives:
                                     ::core::clone::Clone::clone(&(*__self_0_2)),
                                 predecessor_cache:
                                     ::core::clone::Clone::clone(&(*__self_0_3)),
                                 is_cyclic:
                                     ::core::clone::Clone::clone(&(*__self_0_4)),},
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl <C: ::core::fmt::Debug + CfgFunctions> ::core::fmt::Debug for
     ControlFlowGraph<C> {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {
                ControlFlowGraph {
                locals: ref __self_0_0,
                blocks: ref __self_0_1,
                derivatives: ref __self_0_2,
                predecessor_cache: ref __self_0_3,
                is_cyclic: ref __self_0_4 } => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_struct(f,
                                                                  "ControlFlowGraph");
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "locals",
                                                        &&(*__self_0_0));
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "blocks",
                                                        &&(*__self_0_1));
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "derivatives",
                                                        &&(*__self_0_2));
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "predecessor_cache",
                                                        &&(*__self_0_3));
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "is_cyclic",
                                                        &&(*__self_0_4));
                    ::core::fmt::DebugStruct::finish(debug_trait_builder)
                }
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl <C: ::core::default::Default + CfgFunctions> ::core::default::Default
     for ControlFlowGraph<C> {
        #[inline]
        fn default() -> ControlFlowGraph<C> {
            ControlFlowGraph{locals: ::core::default::Default::default(),
                             blocks: ::core::default::Default::default(),
                             derivatives: ::core::default::Default::default(),
                             predecessor_cache:
                                 ::core::default::Default::default(),
                             is_cyclic: ::core::default::Default::default(),}
        }
    }
    pub trait ModificationPass<'a, C: CfgFunctions> {
        type Result: 'a;
        const NAME: &'static str ;
        fn span(&self)
        -> Span;
        fn run(self, cfg: &'a mut ControlFlowGraph<C>)
        -> Self::Result;
    }
    pub trait AnalysisPass<'a, C: CfgFunctions> {
        type Result: 'a;
        const NAME: &'static str ;
        fn span(&self)
        -> Span;
        fn run(self, cfg: &'a ControlFlowGraph<C>)
        -> Self::Result;
    }
    #[macro_export]
    macro_rules! impl_pass_span {
        ($ name : literal) => { impl_pass_span! (self ; $ name,) ; } ;
        ($ name : literal, $ ($ field : tt) *) =>
        { impl_pass_span! (self ; $ name, $ ($ field : tt) *) ; } ;
        ($ self : ident ; $ name : literal, $ ($ field : tt) *) =>
        {
            const NAME : & 'static str = $ name ; fn span(& $ self) -> tracing
            :: Span
            {
                tracing :: info_span!
                (target : "CfgPasses", $ name, $ ($ field) *)
            }
        }
    }
    pub const START_BLOCK: BasicBlock = BasicBlock::from_raw_unchecked(0);
    impl <C> ControlFlowGraph<C> where C: CfgFunctions +
     From<ParameterCallType>, C::I: From<ParameterInput> {
        pub fn insert_variable_declarations<A: CfgFunctions>(&mut self,
                                                             mir: &Mir<A>) {
            let terminator =
                self.blocks[START_BLOCK].terminator.take().unwrap();
            let old_end = self.end();
            let old_locals = self.locals.clone();
            let mut builder =
                CfgBuilder::edit::<false, false>(self, START_BLOCK, 0, 0);
            for (local, decl) in old_locals.iter_enumerated() {
                if let LocalKind::Variable(var, ref kind) = decl.kind {
                    let span = mir[mir.variables[var].sctx].span;
                    let sctx = mir.variables[var].sctx;
                    match kind {
                        VariableLocalKind::Derivative(_) => {
                            builder.assign(local,
                                           RValue::Use(Spanned{span,
                                                               contents:
                                                                   OperandData::Constant(Scalar(Real(0.0))),}),
                                           sctx);
                        }
                        VariableLocalKind::User => {
                            let default =
                                mir.variables[var].default.borrow().clone();
                            let val =
                                builder.insert_expr::<_, _, true,
                                                      true>(sctx, default,
                                                            &mut DefaultConversion);
                            builder.assign(local, val, sctx);
                        }
                    }
                }
            }
            let start_sctx = terminator.sctx;
            let mut terminator = terminator.kind;
            for bb in terminator.successors_mut() {
                if *bb == old_end { *bb = builder.cfg.end() }
            }
            builder.terminate(terminator, start_sctx);
            self.predecessor_cache.invalidate();
            self.is_cyclic.invalidate();
        }
    }
    impl <C: CfgFunctions> ControlFlowGraph<C> {
        pub fn empty() -> Self {
            Self{locals: IndexVec::new(),
                 blocks: IndexVec::new(),
                 derivatives: HashMap::new(),
                 predecessor_cache: PredecessorCache::new(),
                 is_cyclic: GraphIsCyclicCache::new(),}
        }
        pub fn new() -> Self {
            Self{locals: IndexVec::with_capacity(16),
                 blocks: IndexVec::with_capacity(4),
                 derivatives: HashMap::new(),
                 predecessor_cache: PredecessorCache::new(),
                 is_cyclic: GraphIsCyclicCache::new(),}
        }
        pub fn modify<'a, T>(&'a mut self, t: T)
         -> <T as ModificationPass<'a, C>>::Result where
         T: ModificationPass<'a, C> {
            let span = t.span();
            let _enter = span.enter();
            let x = t.run(self);
            drop(_enter);
            x
        }
        pub fn analyse<'a, T>(&'a self, t: T)
         -> <T as AnalysisPass<'a, C>>::Result where T: AnalysisPass<'a, C> {
            let span = t.span();
            let _enter = span.enter();
            let x = t.run(self);
            drop(_enter);
            x
        }
        #[inline]
        pub fn end(&self) -> BasicBlock { self.blocks.last_idx() }
        pub fn new_temporary(&mut self, ty: Type) -> Local {
            self.locals.push(LocalDeclaration{ty,
                                              kind: LocalKind::Temporary,})
        }
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
        pub fn reverse_postorder_itermut(&mut self)
         -> ReversePostorderIterMut<'_, C> {
            ReversePostorderIterMut::new(self, START_BLOCK)
        }
        pub fn locations(&self) -> impl Iterator<Item = Location> + '_ {
            self.blocks.iter_enumerated().flat_map(|(id, block)|
                                                       {
                                                           let phi_locations =
                                                               block.phi_statements.indices().map(move
                                                                                                      |phi|
                                                                                                      Location{block:
                                                                                                                   id,
                                                                                                               kind:
                                                                                                                   LocationKind::Phi(phi),});
                                                           let stmnt_locations =
                                                               block.statements.indices().map(move
                                                                                                  |stmt|
                                                                                                  Location{block:
                                                                                                               id,
                                                                                                           kind:
                                                                                                               LocationKind::Statement(stmt),});
                                                           phi_locations.chain(stmnt_locations).chain(once(Location{block:
                                                                                                                        id,
                                                                                                                    kind:
                                                                                                                        LocationKind::Terminator,}))
                                                       })
        }
        #[inline]
        pub fn for_locals(&self, mut f: impl FnMut(Local)) {
            for block in &self.blocks { block.for_locals(&mut f) }
        }
        #[inline]
        pub fn locals(&self) -> impl Iterator<Item = Local> {
            let mut buff = Vec::with_capacity(3);
            self.for_locals(|local| buff.push(local));
            buff.into_iter()
        }
        #[inline]
        pub fn for_locals_mut(&mut self, mut f: impl FnMut(&mut Local)) {
            for block in &mut self.blocks { block.for_locals_mut(&mut f) }
        }
        #[inline]
        pub fn locals_mut(&mut self) -> impl Iterator<Item = &mut Local> {
            let mut buff = Vec::with_capacity(3);
            self.for_locals_mut(|local| buff.push(local as *mut Local));
            buff.into_iter().map(|local_ptr| unsafe { &mut *local_ptr })
        }
        pub fn map<X: CfgFunctions>(self,
                                    conversion: &mut impl CfgConversion<C, X>)
         -> ControlFlowGraph<X> {
            let blocks =
                self.blocks.into_iter().map(|bb|
                                                bb.map(conversion)).collect();
            ControlFlowGraph{locals: self.locals,
                             blocks,
                             derivatives: self.derivatives,
                             predecessor_cache: self.predecessor_cache,
                             is_cyclic: self.is_cyclic,}
        }
        pub fn intern_locations(&mut self) -> InternedLocations {
            let locations: IndexVec<_, _> = self.locations().collect();
            let mut blocks: IndexVec<BasicBlock, BlockLocations> =
                IndexVec::with_capacity(self.blocks.len());
            let mut start = IntLocation::new(0);
            for (id, location) in locations.iter_enumerated() {
                if location.block != blocks.len_idx() {
                    blocks.push(BlockLocations{phi_start: start,
                                               stmnt_start:
                                                   start +
                                                       self.blocks[blocks.len_idx()].phi_statements.len(),
                                               terminator: id - 1,});
                    start = id;
                }
                if true {
                    {
                        match (&blocks.len_idx(), &location.block) {
                            (left_val, right_val) => {
                                if !(*left_val == *right_val) {
                                    let kind =
                                        ::core::panicking::AssertKind::Eq;
                                    ::core::panicking::assert_failed(kind,
                                                                     &*left_val,
                                                                     &*right_val,
                                                                     ::core::option::Option::None);
                                }
                            }
                        }
                    };
                }
            }
            if let Some(last) = self.blocks.last() {
                blocks.push(BlockLocations{phi_start: start,
                                           stmnt_start:
                                               start +
                                                   last.phi_statements.len(),
                                           terminator: locations.len_idx(),});
            }
            InternedLocations{locations, blocks,}
        }
        pub fn is_cyclic(&self) -> bool { self.is_cyclic.is_cyclic(self) }
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
        pub fn demand_operand_derivative_unchecked<MC: CfgFunctions>(&mut self,
                                                                     mir:
                                                                         &Mir<MC>,
                                                                     operand:
                                                                         &COperand<C>,
                                                                     unknown:
                                                                         Unknown)
         -> CallTypeDerivative<C> {
            match operand.contents {
                OperandData::Constant(_) => Derivative::Zero,
                OperandData::Copy(local) => {
                    let derivative =
                        self.demand_derivative_unchecked(local, unknown);
                    Derivative::Operand(OperandData::Copy(derivative))
                }
                OperandData::Read(ref input) =>
                input.derivative(unknown, mir).into(),
            }
        }
        /// Demands the derivative of an [`Operand`](crate::Operand)
        ///
        /// # Panics
        /// * if `local` is not part of this CFG
        /// * if a branch local was supplied (these may not be read only written to)
        /// * if the derivative of a non numeric (string) local was demanded
        pub fn demand_derivative_unchecked(&mut self, local: Local,
                                           unknown: Unknown) -> Local {
            let locals = &mut self.locals;
            let derivative =
                *self.derivatives.entry(local).or_insert_with(||
                                                                  HashMap::with_capacity(4)).entry(unknown).or_insert_with(||
                                                                                                                               {
                                                                                                                                   let kind =
                                                                                                                                       match locals[local].kind
                                                                                                                                           {
                                                                                                                                           LocalKind::Temporary
                                                                                                                                           =>
                                                                                                                                           LocalKind::Temporary,
                                                                                                                                           LocalKind::Variable(var,
                                                                                                                                                               VariableLocalKind::User)
                                                                                                                                           =>
                                                                                                                                           {
                                                                                                                                               let mut unkowns =
                                                                                                                                                   ArrayVec::new();
                                                                                                                                               unkowns.push(unknown);
                                                                                                                                               LocalKind::Variable(var,
                                                                                                                                                                   VariableLocalKind::Derivative(unkowns))
                                                                                                                                           }
                                                                                                                                           LocalKind::Variable(var,
                                                                                                                                                               VariableLocalKind::Derivative(ref unkowns))
                                                                                                                                           =>
                                                                                                                                           {
                                                                                                                                               let mut unkowns =
                                                                                                                                                   unkowns.clone();
                                                                                                                                               unkowns.push(unknown);
                                                                                                                                               LocalKind::Variable(var,
                                                                                                                                                                   VariableLocalKind::Derivative(unkowns))
                                                                                                                                           }
                                                                                                                                           LocalKind::Branch(access,
                                                                                                                                                             branch,
                                                                                                                                                             VariableLocalKind::User)
                                                                                                                                           =>
                                                                                                                                           {
                                                                                                                                               let mut unkowns =
                                                                                                                                                   ArrayVec::new();
                                                                                                                                               unkowns.push(unknown);
                                                                                                                                               LocalKind::Branch(access,
                                                                                                                                                                 branch,
                                                                                                                                                                 VariableLocalKind::Derivative(unkowns))
                                                                                                                                           }
                                                                                                                                           LocalKind::Branch(access,
                                                                                                                                                             branch,
                                                                                                                                                             VariableLocalKind::Derivative(ref unkowns))
                                                                                                                                           =>
                                                                                                                                           {
                                                                                                                                               let mut unkowns =
                                                                                                                                                   unkowns.clone();
                                                                                                                                               unkowns.push(unknown);
                                                                                                                                               LocalKind::Branch(access,
                                                                                                                                                                 branch,
                                                                                                                                                                 VariableLocalKind::Derivative(unkowns))
                                                                                                                                           }
                                                                                                                                       };
                                                                                                                                   locals.push(LocalDeclaration{kind,
                                                                                                                                                                ty:
                                                                                                                                                                    Type::REAL,})
                                                                                                                               });
            derivative
        }
    }
    pub struct PhiData {
        pub dst: Local,
        pub sources: HashMap<BasicBlock, Local>,
        pub sctx: SyntaxCtx,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for PhiData {
        #[inline]
        fn clone(&self) -> PhiData {
            match *self {
                PhiData {
                dst: ref __self_0_0,
                sources: ref __self_0_1,
                sctx: ref __self_0_2 } =>
                PhiData{dst: ::core::clone::Clone::clone(&(*__self_0_0)),
                        sources: ::core::clone::Clone::clone(&(*__self_0_1)),
                        sctx: ::core::clone::Clone::clone(&(*__self_0_2)),},
            }
        }
    }
    impl Display for PhiData {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            f.write_fmt(::core::fmt::Arguments::new_v1(&["", " = phi { "],
                                                       &match (&self.dst,) {
                                                            (arg0,) =>
                                                            [::core::fmt::ArgumentV1::new(arg0,
                                                                                          ::core::fmt::Display::fmt)],
                                                        }))?;
            for (bb, src) in self.sources.iter() {
                f.write_fmt(::core::fmt::Arguments::new_v1(&["", " => ",
                                                             ", "],
                                                           &match (&bb, &src)
                                                                {
                                                                (arg0, arg1)
                                                                =>
                                                                [::core::fmt::ArgumentV1::new(arg0,
                                                                                              ::core::fmt::Debug::fmt),
                                                                 ::core::fmt::ArgumentV1::new(arg1,
                                                                                              ::core::fmt::Display::fmt)],
                                                            }))?;
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
    pub struct BasicBlockData<C: CfgFunctions> {
        pub phi_statements: IndexVec<Phi, PhiData>,
        pub statements: IndexVec<StatementId, Statement<C>>,
        pub terminator: Option<Terminator<C>>,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl <C: ::core::clone::Clone + CfgFunctions> ::core::clone::Clone for
     BasicBlockData<C> {
        #[inline]
        fn clone(&self) -> BasicBlockData<C> {
            match *self {
                BasicBlockData {
                phi_statements: ref __self_0_0,
                statements: ref __self_0_1,
                terminator: ref __self_0_2 } =>
                BasicBlockData{phi_statements:
                                   ::core::clone::Clone::clone(&(*__self_0_0)),
                               statements:
                                   ::core::clone::Clone::clone(&(*__self_0_1)),
                               terminator:
                                   ::core::clone::Clone::clone(&(*__self_0_2)),},
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl <C: ::core::fmt::Debug + CfgFunctions> ::core::fmt::Debug for
     BasicBlockData<C> {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {
                BasicBlockData {
                phi_statements: ref __self_0_0,
                statements: ref __self_0_1,
                terminator: ref __self_0_2 } => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_struct(f,
                                                                  "BasicBlockData");
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "phi_statements",
                                                        &&(*__self_0_0));
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "statements",
                                                        &&(*__self_0_1));
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "terminator",
                                                        &&(*__self_0_2));
                    ::core::fmt::DebugStruct::finish(debug_trait_builder)
                }
            }
        }
    }
    impl <C: CfgFunctions> BasicBlockData<C> {
        fn map<X: CfgFunctions>(self,
                                conversion: &mut impl CfgConversion<C, X>)
         -> BasicBlockData<X> {
            let statements =
                self.statements.into_iter().map(|(kind, sctx)|
                                                    (conversion.map_stmnt(kind),
                                                     sctx)).collect();
            let terminator =
                self.terminator.map(|term|
                                        {
                                            let kind =
                                                match term.kind {
                                                    TerminatorKind::Goto(dst)
                                                    =>
                                                    TerminatorKind::Goto(dst),
                                                    TerminatorKind::Split {
                                                    condition,
                                                    true_block,
                                                    false_block,
                                                    loop_head } =>
                                                    TerminatorKind::Split{condition:
                                                                              condition.map_operands(conversion),
                                                                          true_block,
                                                                          false_block,
                                                                          loop_head,},
                                                    TerminatorKind::End =>
                                                    TerminatorKind::End,
                                                };
                                            Terminator{kind, sctx: term.sctx,}
                                        });
            BasicBlockData{phi_statements: self.phi_statements,
                           statements,
                           terminator,}
        }
        #[must_use]
        pub fn successors(&self) -> Successors {
            self.terminator().successors()
        }
        #[must_use]
        pub fn successors_mut(&mut self) -> SuccessorsMut {
            self.terminator_mut().successors_mut()
        }
        #[must_use]
        #[inline(always)]
        pub fn terminator(&self) -> &Terminator<C> {
            self.terminator.as_ref().unwrap()
        }
        #[must_use]
        #[inline(always)]
        pub fn terminator_mut(&mut self) -> &mut Terminator<C> {
            self.terminator.as_mut().unwrap()
        }
        pub fn for_locals(&self, mut f: impl FnMut(Local)) {
            for phi in &self.phi_statements {
                f(phi.dst);
                for src in phi.sources.values() { f(*src) }
            }
            for (stmnt, _) in &self.statements { stmnt.for_locals(&mut f) }
            if let Some(terminator) = &self.terminator {
                if let TerminatorKind::Split { condition, .. } =
                       &terminator.kind {
                    condition.for_locals(f)
                }
            }
        }
        pub fn locals(&self) -> impl Iterator<Item = Local> {
            let mut buff = Vec::with_capacity(3);
            self.for_locals(|local| buff.push(local));
            buff.into_iter()
        }
        pub fn for_locals_mut(&mut self, mut f: impl FnMut(&mut Local)) {
            for phi in &mut self.phi_statements {
                f(&mut phi.dst);
                for src in phi.sources.values_mut() { f(src) }
            }
            for (stmnt, _) in &mut self.statements {
                stmnt.for_locals_mut(&mut f)
            }
            if let Some(terminator) = &mut self.terminator {
                if let TerminatorKind::Split { condition, .. } =
                       &mut terminator.kind {
                    condition.for_locals_mut(f)
                }
            }
        }
        pub fn locals_mut(&mut self) -> impl Iterator<Item = &mut Local> {
            let mut buff = Vec::with_capacity(3);
            self.for_locals_mut(|local| buff.push(local as *mut Local));
            buff.into_iter().map(|local_ptr| unsafe { &mut *local_ptr })
        }
    }
    pub struct Terminator<C: CfgFunctions> {
        pub sctx: SyntaxCtx,
        pub kind: TerminatorKind<C>,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl <C: ::core::clone::Clone + CfgFunctions> ::core::clone::Clone for
     Terminator<C> {
        #[inline]
        fn clone(&self) -> Terminator<C> {
            match *self {
                Terminator { sctx: ref __self_0_0, kind: ref __self_0_1 } =>
                Terminator{sctx: ::core::clone::Clone::clone(&(*__self_0_0)),
                           kind:
                               ::core::clone::Clone::clone(&(*__self_0_1)),},
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl <C: ::core::fmt::Debug + CfgFunctions> ::core::fmt::Debug for
     Terminator<C> {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {
                Terminator { sctx: ref __self_0_0, kind: ref __self_0_1 } => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_struct(f,
                                                                  "Terminator");
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "sctx",
                                                        &&(*__self_0_0));
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "kind",
                                                        &&(*__self_0_1));
                    ::core::fmt::DebugStruct::finish(debug_trait_builder)
                }
            }
        }
    }
    impl <C: CfgFunctions> ::core::marker::StructuralPartialEq for
     Terminator<C> {
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl <C: ::core::cmp::PartialEq + CfgFunctions> ::core::cmp::PartialEq for
     Terminator<C> {
        #[inline]
        fn eq(&self, other: &Terminator<C>) -> bool {
            match *other {
                Terminator { sctx: ref __self_1_0, kind: ref __self_1_1 } =>
                match *self {
                    Terminator { sctx: ref __self_0_0, kind: ref __self_0_1 }
                    =>
                    (*__self_0_0) == (*__self_1_0) &&
                        (*__self_0_1) == (*__self_1_1),
                },
            }
        }
        #[inline]
        fn ne(&self, other: &Terminator<C>) -> bool {
            match *other {
                Terminator { sctx: ref __self_1_0, kind: ref __self_1_1 } =>
                match *self {
                    Terminator { sctx: ref __self_0_0, kind: ref __self_0_1 }
                    =>
                    (*__self_0_0) != (*__self_1_0) ||
                        (*__self_0_1) != (*__self_1_1),
                },
            }
        }
    }
    impl <C: CfgFunctions> Terminator<C> {
        #[must_use]
        pub fn successors(&self) -> Successors { self.kind.successors() }
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
    pub enum TerminatorKind<C: CfgFunctions> {
        Goto(BasicBlock),
        Split {
            condition: RValue<C>,
            true_block: BasicBlock,
            false_block: BasicBlock,
            loop_head: bool,
        },
        End,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl <C: ::core::clone::Clone + CfgFunctions> ::core::clone::Clone for
     TerminatorKind<C> {
        #[inline]
        fn clone(&self) -> TerminatorKind<C> {
            match (&*self,) {
                (&TerminatorKind::Goto(ref __self_0),) =>
                TerminatorKind::Goto(::core::clone::Clone::clone(&(*__self_0))),
                (&TerminatorKind::Split {
                 condition: ref __self_0,
                 true_block: ref __self_1,
                 false_block: ref __self_2,
                 loop_head: ref __self_3 },) =>
                TerminatorKind::Split{condition:
                                          ::core::clone::Clone::clone(&(*__self_0)),
                                      true_block:
                                          ::core::clone::Clone::clone(&(*__self_1)),
                                      false_block:
                                          ::core::clone::Clone::clone(&(*__self_2)),
                                      loop_head:
                                          ::core::clone::Clone::clone(&(*__self_3)),},
                (&TerminatorKind::End,) => TerminatorKind::End,
            }
        }
    }
    impl <C: CfgFunctions> ::core::marker::StructuralPartialEq for
     TerminatorKind<C> {
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl <C: ::core::cmp::PartialEq + CfgFunctions> ::core::cmp::PartialEq for
     TerminatorKind<C> {
        #[inline]
        fn eq(&self, other: &TerminatorKind<C>) -> bool {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        (&TerminatorKind::Goto(ref __self_0),
                         &TerminatorKind::Goto(ref __arg_1_0)) =>
                        (*__self_0) == (*__arg_1_0),
                        (&TerminatorKind::Split {
                         condition: ref __self_0,
                         true_block: ref __self_1,
                         false_block: ref __self_2,
                         loop_head: ref __self_3 }, &TerminatorKind::Split {
                         condition: ref __arg_1_0,
                         true_block: ref __arg_1_1,
                         false_block: ref __arg_1_2,
                         loop_head: ref __arg_1_3 }) =>
                        (*__self_0) == (*__arg_1_0) &&
                            (*__self_1) == (*__arg_1_1) &&
                            (*__self_2) == (*__arg_1_2) &&
                            (*__self_3) == (*__arg_1_3),
                        _ => true,
                    }
                } else { false }
            }
        }
        #[inline]
        fn ne(&self, other: &TerminatorKind<C>) -> bool {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        (&TerminatorKind::Goto(ref __self_0),
                         &TerminatorKind::Goto(ref __arg_1_0)) =>
                        (*__self_0) != (*__arg_1_0),
                        (&TerminatorKind::Split {
                         condition: ref __self_0,
                         true_block: ref __self_1,
                         false_block: ref __self_2,
                         loop_head: ref __self_3 }, &TerminatorKind::Split {
                         condition: ref __arg_1_0,
                         true_block: ref __arg_1_1,
                         false_block: ref __arg_1_2,
                         loop_head: ref __arg_1_3 }) =>
                        (*__self_0) != (*__arg_1_0) ||
                            (*__self_1) != (*__arg_1_1) ||
                            (*__self_2) != (*__arg_1_2) ||
                            (*__self_3) != (*__arg_1_3),
                        _ => false,
                    }
                } else { true }
            }
        }
    }
    impl <C: CfgFunctions> Display for TerminatorKind<C> {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            match self {
                Self::Goto(bb) => {
                    f.write_fmt(::core::fmt::Arguments::new_v1(&["goto "],
                                                               &match (&bb,) {
                                                                    (arg0,) =>
                                                                    [::core::fmt::ArgumentV1::new(arg0,
                                                                                                  ::core::fmt::Debug::fmt)],
                                                                }))
                }
                TerminatorKind::Split { condition, true_block, false_block, ..
                } => {
                    f.write_fmt(::core::fmt::Arguments::new_v1(&["if ",
                                                                 " { goto ",
                                                                 " } else { goto ",
                                                                 " }"],
                                                               &match (&condition,
                                                                       &true_block,
                                                                       &false_block)
                                                                    {
                                                                    (arg0,
                                                                     arg1,
                                                                     arg2) =>
                                                                    [::core::fmt::ArgumentV1::new(arg0,
                                                                                                  ::core::fmt::Display::fmt),
                                                                     ::core::fmt::ArgumentV1::new(arg1,
                                                                                                  ::core::fmt::Debug::fmt),
                                                                     ::core::fmt::ArgumentV1::new(arg2,
                                                                                                  ::core::fmt::Debug::fmt)],
                                                                }))
                }
                TerminatorKind::End =>
                f.write_fmt(::core::fmt::Arguments::new_v1(&["terminate"],
                                                           &match () {
                                                                () => [],
                                                            })),
            }
        }
    }
    impl <C: CfgFunctions> Debug for TerminatorKind<C> {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            Display::fmt(self, f)
        }
    }
    impl <C: CfgFunctions> TerminatorKind<C> {
        #[must_use]
        pub fn successors(&self) -> Successors {
            match self {
                Self::End => Successors::new_empty(),
                Self::Goto(dst) => Successors::new_single(*dst),
                Self::Split { true_block, false_block, .. } =>
                Successors::new_double(*true_block, *false_block),
            }
        }
        #[must_use]
        #[inline]
        pub fn successors_mut(&mut self) -> SuccessorsMut {
            match self {
                Self::End => SuccessorsMut::new_empty(),
                Self::Goto(dst) => SuccessorsMut::new_single(dst),
                Self::Split { true_block, false_block, .. } =>
                SuccessorsMut::new_double(true_block, false_block),
            }
        }
        #[inline]
        pub fn map(&mut self, f: impl Fn(BasicBlock) -> BasicBlock) {
            match self {
                Self::Goto(dst) => *dst = f(*dst),
                Self::Split { true_block, false_block, .. } => {
                    *true_block = f(*true_block);
                    *false_block = f(*false_block);
                }
                Self::End => (),
            }
        }
        pub fn is_loop_head(&self) -> bool {
            match self {
                Self::Split { loop_head: true, .. } => true,
                _ => false,
            }
        }
    }
}
pub mod const_fold {
    #![allow(clippy :: float_cmp)]
    use crate::dfa::lattice::FlatSet::{self, Bottom, Elem, Top};
    use crate::{fold_rvalue, COperand, CallArg, CfgFunctions, ConstVal, Local,
                OperandData, RValue, RValueFold};
    use data_structures::index_vec::IndexSlice;
    use session::sourcemap::Span;
    use osdi_types::ConstVal::Scalar;
    use osdi_types::SimpleConstVal::{Bool, Integer, Real};
    use osdi_types::{Complex64, Type};
    use std::fmt::Formatter;
    use std::fmt::{Debug, Display};
    use std::marker::PhantomData;
    use std::ops::{Add, BitAnd, BitOr, BitXor, Neg, Rem, Shl, Shr, Sub};
    type CallResolverOperand<R> = COperand<<R as CallResolver>::C>;
    mod propagation {
        use crate::cfg::{BasicBlock, ControlFlowGraph, ModificationPass, Phi,
                         PhiData, Terminator, TerminatorKind};
        use crate::const_fold::{CallResolver, ConstantFold,
                                NoInputConstResolution};
        use crate::dfa::lattice::{FlatSet, JoinSemiLattice, SparseFlatSetMap};
        use crate::dfa::visitor::ResultsVisitorMut;
        use crate::dfa::{direciton, Analysis, AnalysisDomain, Results,
                         SplitEdgeEffects};
        use crate::osdi_types::SimpleConstVal::Bool;
        use crate::{dfa, CfgFunctions, ConstVal, Expression, CfgInputs, Local,
                    LocalKind, Operand, OperandData, RValue, StmntKind};
        use crate::{impl_pass_span, StatementId, SyntaxCtx};
        use data_structures::bit_set::HybridBitSet;
        use osdi_types::ConstVal::Scalar;
        use osdi_types::Type;
        use std::cell::UnsafeCell;
        use std::marker::PhantomData;
        use tracing::trace_span;
        pub struct BasicBlockConstants {
            pub reachable: bool,
            pub constants: SparseFlatSetMap<Local, ConstVal>,
            temporaries_changed: bool,
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::fmt::Debug for BasicBlockConstants {
            fn fmt(&self, f: &mut ::core::fmt::Formatter)
             -> ::core::fmt::Result {
                match *self {
                    BasicBlockConstants {
                    reachable: ref __self_0_0,
                    constants: ref __self_0_1,
                    temporaries_changed: ref __self_0_2 } => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_struct(f,
                                                                      "BasicBlockConstants");
                        let _ =
                            ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                            "reachable",
                                                            &&(*__self_0_0));
                        let _ =
                            ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                            "constants",
                                                            &&(*__self_0_1));
                        let _ =
                            ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                            "temporaries_changed",
                                                            &&(*__self_0_2));
                        ::core::fmt::DebugStruct::finish(debug_trait_builder)
                    }
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::clone::Clone for BasicBlockConstants {
            #[inline]
            fn clone(&self) -> BasicBlockConstants {
                match *self {
                    BasicBlockConstants {
                    reachable: ref __self_0_0,
                    constants: ref __self_0_1,
                    temporaries_changed: ref __self_0_2 } =>
                    BasicBlockConstants{reachable:
                                            ::core::clone::Clone::clone(&(*__self_0_0)),
                                        constants:
                                            ::core::clone::Clone::clone(&(*__self_0_1)),
                                        temporaries_changed:
                                            ::core::clone::Clone::clone(&(*__self_0_2)),},
                }
            }
        }
        impl ::core::marker::StructuralPartialEq for BasicBlockConstants { }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::cmp::PartialEq for BasicBlockConstants {
            #[inline]
            fn eq(&self, other: &BasicBlockConstants) -> bool {
                match *other {
                    BasicBlockConstants {
                    reachable: ref __self_1_0,
                    constants: ref __self_1_1,
                    temporaries_changed: ref __self_1_2 } =>
                    match *self {
                        BasicBlockConstants {
                        reachable: ref __self_0_0,
                        constants: ref __self_0_1,
                        temporaries_changed: ref __self_0_2 } =>
                        (*__self_0_0) == (*__self_1_0) &&
                            (*__self_0_1) == (*__self_1_1) &&
                            (*__self_0_2) == (*__self_1_2),
                    },
                }
            }
            #[inline]
            fn ne(&self, other: &BasicBlockConstants) -> bool {
                match *other {
                    BasicBlockConstants {
                    reachable: ref __self_1_0,
                    constants: ref __self_1_1,
                    temporaries_changed: ref __self_1_2 } =>
                    match *self {
                        BasicBlockConstants {
                        reachable: ref __self_0_0,
                        constants: ref __self_0_1,
                        temporaries_changed: ref __self_0_2 } =>
                        (*__self_0_0) != (*__self_1_0) ||
                            (*__self_0_1) != (*__self_1_1) ||
                            (*__self_0_2) != (*__self_1_2),
                    },
                }
            }
        }
        impl JoinSemiLattice for BasicBlockConstants {
            fn join(&mut self, other: &Self) -> bool {
                if true {
                    if !other.reachable {
                        ::core::panicking::panic("assertion failed: other.reachable")
                    };
                };
                self.temporaries_changed.join(&other.temporaries_changed) |
                    self.reachable.join(&true) |
                    self.constants.join(&other.constants)
            }
        }
        impl BasicBlockConstants {
            pub fn get_folded_val(&self, local: Local) -> Option<ConstVal> {
                self.constants.element_sets.get(&local).cloned()
            }
        }
        pub struct ConditionalConstPropagation<'lt,
                                               R: CallResolver>(ConditionalConstantPropagationImpl<'lt,
                                                                                                   R>);
        impl <'lt, R: CallResolver>
         From<ConditionalConstantPropagationImpl<'lt, R>> for
         ConditionalConstPropagation<'lt, R> {
            fn from(inner: ConditionalConstantPropagationImpl<'lt, R>)
             -> Self {
                Self(inner)
            }
        }
        use __sealed::ConditionalConstantPropagationImpl;
        mod __sealed {
            use super::*;
            pub struct ConditionalConstantPropagationImpl<'lt,
                                                          R: CallResolver> {
                pub resolver: &'lt R,
                pub global_vars: HybridBitSet<Local>,
                constant_temporaries: UnsafeCell<SparseFlatSetMap<Local,
                                                                  ConstVal>>,
            }
            impl <'lt, R: CallResolver>
             ConditionalConstantPropagationImpl<'lt, R> {
                pub fn new(resolver: &'lt R, global_vars: HybridBitSet<Local>,
                           local_cnt: usize) -> Self {
                    Self{resolver,
                         global_vars,
                         constant_temporaries:
                             UnsafeCell::new(SparseFlatSetMap::new_empty(local_cnt)),}
                }
                pub(super) fn get_temporary_constant(&self, local: Local)
                 -> FlatSet<ConstVal> {
                    unsafe {
                        &mut *self.constant_temporaries.get()
                    }.get_cloned_flat_set(local)
                }
                pub(super) fn get_folded_temporary(&self, local: Local)
                 -> Option<ConstVal> {
                    unsafe {
                        &mut *self.constant_temporaries.get()
                    }.element_sets.get(&local).cloned()
                }
                pub(super) fn extend_with_temporary_constants(&self,
                                                              dst:
                                                                  &mut SparseFlatSetMap<Local,
                                                                                        ConstVal>) {
                    dst.element_sets.extend(unsafe {
                                                &*self.constant_temporaries.get()
                                            }.element_sets.iter().map(|(k, v)|
                                                                          (*k,
                                                                           v.clone())));
                    dst.top_sets.union(&unsafe {
                                            &*self.constant_temporaries.get()
                                        }.top_sets);
                }
                pub(super) fn write_constant_to_local(&self,
                                                      cfg:
                                                          &ControlFlowGraph<R::C>,
                                                      state:
                                                          &mut BasicBlockConstants,
                                                      dst: Local,
                                                      val:
                                                          FlatSet<ConstVal>) {
                    if LocalKind::Temporary == cfg.locals[dst].kind {
                        state.temporaries_changed =
                            unsafe {
                                &mut *self.constant_temporaries.get()
                            }.set_flat_set(dst, val)
                    } else { state.constants.set_flat_set(dst, val); }
                }
            }
        }
        impl <'lt, R: CallResolver> AnalysisDomain<R::C> for
         ConditionalConstPropagation<'lt, R> {
            type Domain = BasicBlockConstants;
            type Direction = direciton::Forward;
            const NAME: &'static str = "Const Propagation";
            fn bottom_value(&self,
                            cfg: &ControlFlowGraph<<R as CallResolver>::C>)
             -> Self::Domain {
                self.0.bottom_value(cfg)
            }
            fn initialize_start_block(&self,
                                      cfg:
                                          &ControlFlowGraph<<R as
                                                            CallResolver>::C>,
                                      state: &mut Self::Domain) {
                self.0.initialize_start_block(cfg, state)
            }
        }
        impl <'lt, R: CallResolver> AnalysisDomain<R::C> for
         ConditionalConstantPropagationImpl<'lt, R> {
            type Domain = BasicBlockConstants;
            type Direction = direciton::Forward;
            const NAME: &'static str = "Const Propagation";
            fn bottom_value(&self,
                            cfg: &ControlFlowGraph<<R as CallResolver>::C>)
             -> Self::Domain {
                BasicBlockConstants{reachable: false,
                                    temporaries_changed: false,
                                    constants:
                                        SparseFlatSetMap::new_empty(cfg.locals.len()),}
            }
            fn initialize_start_block(&self,
                                      _cfg:
                                          &ControlFlowGraph<<R as
                                                            CallResolver>::C>,
                                      state: &mut Self::Domain) {
                state.reachable = true;
                state.constants.top_sets.union(&self.global_vars);
            }
        }
        impl <'lt, R: CallResolver> Analysis<R::C> for
         ConditionalConstPropagation<'lt, R> {
            fn init_block(&self, _cfg: &ControlFlowGraph<R::C>,
                          state: &mut Self::Domain) {
                state.temporaries_changed = false;
                self.0.extend_with_temporary_constants(&mut state.constants);
            }
            fn apply_phi_effect(&self, cfg: &ControlFlowGraph<R::C>,
                                state: &mut Self::Domain, phi: &PhiData,
                                bb: BasicBlock, idx: Phi) {
                self.0.apply_phi_effect(cfg, state, phi, bb, idx)
            }
            fn apply_statement_effect(&self, cfg: &ControlFlowGraph<R::C>,
                                      state: &mut Self::Domain,
                                      statement:
                                          &(StmntKind<<R as CallResolver>::C>,
                                            SyntaxCtx), idx: StatementId,
                                      bb: BasicBlock) {
                self.0.apply_statement_effect(cfg, state, statement, idx, bb)
            }
        }
        impl <'lt, R: CallResolver> Analysis<R::C> for
         ConditionalConstantPropagationImpl<'lt, R> {
            fn init_block(&self, _cfg: &ControlFlowGraph<R::C>,
                          state: &mut Self::Domain) {
                state.temporaries_changed = false;
            }
            fn apply_phi_effect(&self, cfg: &ControlFlowGraph<R::C>,
                                state: &mut Self::Domain, phi: &PhiData,
                                _bb: BasicBlock, _idx: Phi) {
                let flat_set =
                    phi.sources.iter().fold(FlatSet::Bottom,
                                            |mut dst, (_, local)|
                                                {
                                                    match self.get_temporary_constant(*local)
                                                        {
                                                        FlatSet::Bottom =>
                                                        state.constants.join_into(*local,
                                                                                  &mut dst),
                                                        res => dst.join(&res),
                                                    };
                                                    dst
                                                });
                self.write_constant_to_local(cfg, state, phi.dst, flat_set)
            }
            fn apply_statement_effect(&self, cfg: &ControlFlowGraph<R::C>,
                                      state: &mut Self::Domain,
                                      statement:
                                          &(StmntKind<<R as CallResolver>::C>,
                                            SyntaxCtx), _idx: StatementId,
                                      _bb: BasicBlock) {
                if let StmntKind::Assignment(dst, ref val) = statement.0 {
                    let val =
                        ConstantFold{locals: &state.constants,
                                     resolver: self.resolver,
                                     resolve_special_locals:
                                         |temporary|
                                             self.get_temporary_constant(temporary),}.resolve_rvalue(val,
                                                                                                     cfg.locals[dst].ty);
                    self.write_constant_to_local(cfg, state, dst, val)
                }
            }
            #[inline(always)]
            fn apply_edge_effects(&self, _cfg: &ControlFlowGraph<R::C>,
                                  _block: BasicBlock, state: &Self::Domain)
             -> bool {
                state.reachable
            }
            fn apply_split_edge_effects(&self, _cfg: &ControlFlowGraph<R::C>,
                                        _block: BasicBlock,
                                        discr: &RValue<R::C>,
                                        state: &Self::Domain,
                                        edge_effects:
                                            &mut impl SplitEdgeEffects<Self::Domain>) {
                if !state.reachable {
                    edge_effects.apply(|_, _, _| false)
                } else {
                    let mut fold =
                        ConstantFold{locals: &state.constants,
                                     resolver: self.resolver,
                                     resolve_special_locals:
                                         |temporary|
                                             self.get_temporary_constant(temporary),};
                    if let FlatSet::Elem(Scalar(Bool(const_discriminant))) =
                           fold.resolve_rvalue(discr, Type::BOOL) {
                        edge_effects.apply(|_, _, switch_edge|
                                               switch_edge ==
                                                   const_discriminant)
                    }
                }
            }
        }
        impl <C: CfgFunctions> ControlFlowGraph<C> {
            /// This function runs a conditional constant folding algorithm.
            /// The resulting data flow graph is returned without modifying the CFG itself.
            /// This function is rarely used on its own. Use [`propagate_constants`]/[`propagate_constants_with_inputs`] instead
            ///
            ///
            /// # Parameters
            ///
            /// * `input` -
            /// Generally inputs that are always known to be constants should just be emitted as such during HIR lowering.
            /// This parameter is mostly useful when creating multiple cfgs from the same code and constant folding different sets of inputs
            ///
            ///
            /// # Note
            ///
            /// Call very cautiously this is the single most expensive operation in the compiler. Preferably this should be done only after simpler more efficent optimizations have been performed.
            ///
            /// # Returns
            ///
            /// [`Results`](create::dfa::Results) of condtional const propagation: The constants at the beginning of each block and whether a block is rechable.
            /// See documentation of [`Results`](create::dfa::Results) on how to browse these results.
            ///
            /// However he most common use case is to pass the result to [`write_constants`] which writes the constants back into the CFG
            ///
            pub fn conditional_constant_propagation<'a,
                                                    R: CallResolver<C =
                                                                    C>>(&self,
                                                                        resolver:
                                                                            &'a R,
                                                                        global_vars:
                                                                            HybridBitSet<Local>)
             -> dfa::Results<C, ConditionalConstPropagation<'a, R>> {
                let span =
                    {
                        use ::tracing::__macro_support::Callsite as _;
                        static CALLSITE:
                         ::tracing::__macro_support::MacroCallsite =
                            {
                                use ::tracing::__macro_support::MacroCallsite;
                                static META: ::tracing::Metadata<'static> =
                                    {
                                        ::tracing_core::metadata::Metadata::new("fold_constants",
                                                                                "middle::const_fold::propagation",
                                                                                ::tracing::Level::TRACE,
                                                                                Some("middle/src/const_fold/propagation.rs"),
                                                                                Some(315u32),
                                                                                Some("middle::const_fold::propagation"),
                                                                                ::tracing_core::field::FieldSet::new(&[],
                                                                                                                     ::tracing_core::callsite::Identifier(&CALLSITE)),
                                                                                ::tracing::metadata::Kind::SPAN)
                                    };
                                MacroCallsite::new(&META)
                            };
                        let mut interest =
                            ::tracing::subscriber::Interest::never();
                        if ::tracing::Level::TRACE <=
                               ::tracing::level_filters::STATIC_MAX_LEVEL &&
                               ::tracing::Level::TRACE <=
                                   ::tracing::level_filters::LevelFilter::current()
                               &&
                               {
                                   interest = CALLSITE.interest();
                                   !interest.is_never()
                               } && CALLSITE.is_enabled(interest) {
                            let meta = CALLSITE.metadata();
                            ::tracing::Span::new(meta,
                                                 &{
                                                      meta.fields().value_set(&[])
                                                  })
                        } else {
                            let span = CALLSITE.disabled_span();
                            { };
                            span
                        }
                    };
                let _enter = span.enter();
                let res =
                    ConditionalConstantPropagationImpl::new(resolver,
                                                            global_vars,
                                                            self.locals.len()).into_engine(self).iterate_to_fixpoint();
                Results{analysis: res.analysis.into(),
                        entry_sets: res.entry_sets,}
            }
            pub fn write_constants<R: CallResolver<C =
                                                   C>>(&mut self,
                                                       const_prop_result:
                                                           &dfa::Results<C,
                                                                         ConditionalConstPropagation<R>>) {
                const_prop_result.visit_with_mut(self,
                                                 &mut ConstWriter(&const_prop_result.analysis.0))
            }
        }
        impl <C: CfgFunctions> Expression<C> {
            pub fn const_eval(&self) -> Option<ConstVal> {
                self.const_eval_with_inputs(&NoInputConstResolution(PhantomData),
                                            HybridBitSet::new_empty())
            }
            pub fn const_eval_with_inputs<R: CallResolver<C =
                                                          C>>(&self,
                                                              inputs: &R,
                                                              global_vars:
                                                                  HybridBitSet<Local>)
             -> Option<ConstVal> {
                match self.1.contents {
                    OperandData::Constant(ref val) => Some(val.clone()),
                    OperandData::Copy(local) => {
                        let mut res =
                            self.0.conditional_constant_propagation(inputs,
                                                                    global_vars).into_results_cursor(&self.0);
                        res.seek_to_exit_block_end(&self.0);
                        res.get().get_folded_val(local)
                    }
                    OperandData::Read(ref input) =>
                    inputs.resolve_input(input).into(),
                }
            }
        }
        pub struct ConstantPropagation<'a, R> {
            resolver: &'a R,
            global_vars: HybridBitSet<Local>,
        }
        impl <'a, C: CfgFunctions, R: CallResolver<C = C>>
         ModificationPass<'_, C> for ConstantPropagation<'a, R> {
            type Result = ();
            const NAME: &'static str = "constant_propagation";
            fn span(&self) -> tracing::Span {
                {
                    use ::tracing::__macro_support::Callsite as _;
                    static CALLSITE: ::tracing::__macro_support::MacroCallsite
                     =
                        {
                            use ::tracing::__macro_support::MacroCallsite;
                            static META: ::tracing::Metadata<'static> =
                                {
                                    ::tracing_core::metadata::Metadata::new("constant_propagation",
                                                                            "CfgPasses",
                                                                            ::tracing::Level::INFO,
                                                                            Some("middle/src/const_fold/propagation.rs"),
                                                                            Some(373u32),
                                                                            Some("middle::const_fold::propagation"),
                                                                            ::tracing_core::field::FieldSet::new(&["input_resolver"],
                                                                                                                 ::tracing_core::callsite::Identifier(&CALLSITE)),
                                                                            ::tracing::metadata::Kind::SPAN)
                                };
                            MacroCallsite::new(&META)
                        };
                    let mut interest =
                        ::tracing::subscriber::Interest::never();
                    if ::tracing::Level::INFO <=
                           ::tracing::level_filters::STATIC_MAX_LEVEL &&
                           ::tracing::Level::INFO <=
                               ::tracing::level_filters::LevelFilter::current()
                           &&
                           {
                               interest = CALLSITE.interest();
                               !interest.is_never()
                           } && CALLSITE.is_enabled(interest) {
                        let meta = CALLSITE.metadata();
                        ::tracing::Span::new(meta,
                                             &{
                                                  #[allow(unused_imports)]
                                                  use ::tracing::field::{debug,
                                                                         display,
                                                                         Value};
                                                  let mut iter =
                                                      meta.fields().iter();
                                                  meta.fields().value_set(&[(&iter.next().expect("FieldSet corrupted (this is a bug)"),
                                                                             Some(&debug(self.resolver)
                                                                                      as
                                                                                      &Value))])
                                              })
                    } else { let span = CALLSITE.disabled_span(); { }; span }
                }
            }
            fn run(self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
                let res =
                    cfg.conditional_constant_propagation(self.resolver,
                                                         self.global_vars);
                cfg.write_constants(&res)
            }
        }
        impl <C: CfgFunctions> Default for
         ConstantPropagation<'static, NoInputConstResolution<C>> {
            fn default() -> Self {
                Self{resolver: &NoInputConstResolution(PhantomData),
                     global_vars: HybridBitSet::new_empty(),}
            }
        }
        impl <'a, R: CallResolver> ConstantPropagation<'a, R> {
            pub fn with_resolver(resolver: &'a R) -> Self {
                Self{resolver, global_vars: HybridBitSet::new_empty(),}
            }
        }
        impl <C: CfgFunctions>
         ConstantPropagation<'static, NoInputConstResolution<C>> {
            pub fn with_initial_conditions(global_vars: HybridBitSet<Local>)
             -> Self {
                Self{resolver: &NoInputConstResolution(PhantomData),
                     global_vars,}
            }
        }
        struct ConstWriter<'a, 'b,
                           R: CallResolver>(&'a ConditionalConstantPropagationImpl<'b,
                                                                                   R>);
        impl <'a, 'b, R: CallResolver> ResultsVisitorMut<R::C> for
         ConstWriter<'a, 'b, R> {
            type FlowState = BasicBlockConstants;
            fn visit_statement_after_effect(&mut self,
                                            state: &Self::FlowState,
                                            stmnt:
                                                &mut (StmntKind<R::C>,
                                                      SyntaxCtx),
                                            _block: BasicBlock,
                                            _id: StatementId) {
                match stmnt.0 {
                    StmntKind::Assignment(dst, ref mut rval) => {
                        let val =
                            self.0.get_folded_temporary(dst).or_else(||
                                                                         state.get_folded_val(dst));
                        write_consts_to_rval(state, val, rval)
                    }
                    StmntKind::Call(_, ref mut args, _) =>
                    write_consts_to_operands(state, args),
                    StmntKind::NoOp => { }
                }
            }
            #[inline(always)]
            fn visit_terminator_before_effect(&self, state: &Self::FlowState,
                                              term: &mut Terminator<R::C>,
                                              _block: BasicBlock) {
                if let TerminatorKind::Split { ref mut condition, .. } =
                       term.kind {
                    let mut fold =
                        ConstantFold{locals: &state.constants,
                                     resolver: self.0.resolver,
                                     resolve_special_locals:
                                         |temporary|
                                             self.0.get_temporary_constant(temporary),};
                    let val =
                        fold.resolve_rvalue(condition,
                                            Type::BOOL).into_option();
                    write_consts_to_rval(state, val, condition);
                }
            }
        }
        fn write_consts_to_rval<C: CfgFunctions>(state: &BasicBlockConstants,
                                                 folded_val: Option<ConstVal>,
                                                 dst: &mut RValue<C>) {
            if let Some(folded_val) = folded_val {
                *dst =
                    RValue::Use(Operand{span: dst.span(),
                                        contents:
                                            OperandData::Constant(folded_val),});
            } else { write_consts_to_rval_operands(state, dst) }
        }
        fn write_consts_to_rval_operands<C: CfgFunctions>(state:
                                                              &BasicBlockConstants,
                                                          dst:
                                                              &mut RValue<C>) {
            write_consts_to_operands(state, dst.operands_mut())
        }
        fn write_consts_to_operands<'c, I: CfgInputs +
                                    'c>(state: &BasicBlockConstants,
                                        operands:
                                            impl IntoIterator<Item =
                                                              &'c mut Operand<I>>) {
            for operand in operands {
                if let OperandData::Copy(local) = operand.contents {
                    if let Some(folded_val) = state.get_folded_val(local) {
                        operand.contents =
                            OperandData::Constant(folded_val.clone())
                    }
                }
            }
        }
    }
    use crate::dfa::lattice::SparseFlatSetMap;
    use crate::osdi_types::SimpleConstVal::Cmplx;
    use itertools::Itertools;
    pub use propagation::ConstantPropagation;
    use std::mem::size_of_val;
    #[allow(clippy :: from_over_into)]
    impl <T: PartialEq> Into<Option<T>> for FlatSet<T> {
        fn into(self) -> Option<T> { self.into_option() }
    }
    macro_rules! undefined_operation {
        ($ op : expr, $ arg : expr) =>
        { unreachable! ("Operation {} not defined for {:?}", $ op, $ arg) } ;
        ($ op : expr, $ lhs : expr, $ rhs : expr) =>
        {
            unreachable!
            ("Operation {} not defined for {:?} and {:?}", $ op, $ lhs, $ rhs)
        } ;
    }
    /// This trait allows constant folding inputs such as port connected
    /// Note that in many instances it is better to emit constant values during HIR lowering instead
    /// This should only be used if you want multiple CFGs where an input is constant folded in one case
    /// but not the other
    pub trait CallResolver: Debug {
        type C: CfgFunctions;
        fn resolve_input(&self, input: &<Self::C as CfgFunctions>::I)
        -> FlatSet<ConstVal>;
    }
    pub struct NoInputConstResolution<C>(PhantomData<fn(&C)>);
    impl <C> NoInputConstResolution<C> {
        pub fn new() -> Self { Self(PhantomData) }
    }
    impl <C> Default for NoInputConstResolution<C> {
        fn default() -> Self { Self::new() }
    }
    impl <C> Debug for NoInputConstResolution<C> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            f.write_str("NoInputConstResolution")
        }
    }
    impl <C> Display for NoInputConstResolution<C> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            f.write_str("none")
        }
    }
    impl <C: CfgFunctions> CallResolver for NoInputConstResolution<C> {
        type C = C;
        fn resolve_input(&self, _input: &<C as CfgFunctions>::I)
         -> FlatSet<ConstVal> {
            FlatSet::Top
        }
    }
    pub type LocalConsts = SparseFlatSetMap<Local, ConstVal>;
    struct ConstantFold<'lt, R: CallResolver, F: Fn(Local)
                        -> FlatSet<ConstVal>> {
        locals: &'lt LocalConsts,
        resolver: &'lt R,
        resolve_special_locals: F,
    }
    impl <'lt, R: CallResolver, F: Fn(Local) -> FlatSet<ConstVal>>
     ConstantFold<'lt, R, F> {
        fn resolve_rvalue(&mut self, rvalue: &RValue<R::C>, ty: Type)
         -> FlatSet<ConstVal> {
            fold_rvalue(self, rvalue, ty)
        }
        fn resolve_operand(&self, op: &CallResolverOperand<R>)
         -> FlatSet<ConstVal> {
            match op.contents {
                OperandData::Constant(ref val) => FlatSet::Elem(val.clone()),
                OperandData::Copy(local) =>
                (self.resolve_special_locals)(local).map_bottom(||
                                                                    self.locals.get_cloned_flat_set(local)),
                OperandData::Read(ref input) =>
                self.resolver.resolve_input(input),
            }
        }
        fn eval_real(&self, arg: &CallResolverOperand<R>,
                     eval: impl FnOnce(f64) -> f64, op: &'static str)
         -> FlatSet<ConstVal> {
            self.resolve_operand(arg).map(|arg|
                                              {
                                                  if let Scalar(Real(val)) =
                                                         arg {
                                                      Scalar(Real(eval(val)))
                                                  } else {
                                                      {
                                                          ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: Operation ",
                                                                                                                        " not defined for "],
                                                                                                                      &match (&op,
                                                                                                                              &arg)
                                                                                                                           {
                                                                                                                           (arg0,
                                                                                                                            arg1)
                                                                                                                           =>
                                                                                                                           [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                                         ::core::fmt::Display::fmt),
                                                                                                                            ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                                         ::core::fmt::Debug::fmt)],
                                                                                                                       }))
                                                      }
                                                  }
                                              })
        }
        fn eval_cmplx(&self, arg: &CallResolverOperand<R>,
                      eval: impl FnOnce(Complex64) -> Complex64,
                      op: &'static str) -> FlatSet<ConstVal> {
            self.resolve_operand(arg).map(|arg|
                                              {
                                                  if let Scalar(Cmplx(val)) =
                                                         arg {
                                                      Scalar(Cmplx(eval(val)))
                                                  } else {
                                                      {
                                                          ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: Operation ",
                                                                                                                        " not defined for "],
                                                                                                                      &match (&op,
                                                                                                                              &arg)
                                                                                                                           {
                                                                                                                           (arg0,
                                                                                                                            arg1)
                                                                                                                           =>
                                                                                                                           [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                                         ::core::fmt::Display::fmt),
                                                                                                                            ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                                         ::core::fmt::Debug::fmt)],
                                                                                                                       }))
                                                      }
                                                  }
                                              })
        }
        fn eval_int(&self, arg: &CallResolverOperand<R>,
                    eval: impl FnOnce(i64) -> i64, op: &'static str)
         -> FlatSet<ConstVal> {
            self.resolve_operand(arg).map(|arg|
                                              {
                                                  if let Scalar(Integer(val))
                                                         = arg {
                                                      Scalar(Integer(eval(val)))
                                                  } else {
                                                      {
                                                          ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: Operation ",
                                                                                                                        " not defined for "],
                                                                                                                      &match (&op,
                                                                                                                              &arg)
                                                                                                                           {
                                                                                                                           (arg0,
                                                                                                                            arg1)
                                                                                                                           =>
                                                                                                                           [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                                         ::core::fmt::Display::fmt),
                                                                                                                            ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                                         ::core::fmt::Debug::fmt)],
                                                                                                                       }))
                                                      }
                                                  }
                                              })
        }
        fn eval_bool(&self, arg: &CallResolverOperand<R>,
                     eval: impl FnOnce(bool) -> bool, op: &'static str)
         -> FlatSet<ConstVal> {
            self.resolve_operand(arg).map(|arg|
                                              {
                                                  if let Scalar(Bool(val)) =
                                                         arg {
                                                      Scalar(Bool(eval(val)))
                                                  } else {
                                                      {
                                                          ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: Operation ",
                                                                                                                        " not defined for "],
                                                                                                                      &match (&op,
                                                                                                                              &arg)
                                                                                                                           {
                                                                                                                           (arg0,
                                                                                                                            arg1)
                                                                                                                           =>
                                                                                                                           [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                                         ::core::fmt::Display::fmt),
                                                                                                                            ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                                         ::core::fmt::Debug::fmt)],
                                                                                                                       }))
                                                      }
                                                  }
                                              })
        }
        fn eval_real_comparison(&self, lhs: &CallResolverOperand<R>,
                                rhs: &CallResolverOperand<R>,
                                eval: impl FnOnce(f64, f64) -> bool,
                                op: &'static str) -> FlatSet<ConstVal> {
            let lhs = self.resolve_operand(lhs);
            let rhs = self.resolve_operand(rhs);
            lhs.apply_binary_op(rhs,
                                |lhs, rhs|
                                    match (lhs, rhs) {
                                        (Scalar(Real(lhs)), Scalar(Real(rhs)))
                                        => Scalar(Bool(eval(lhs, rhs))),
                                        (lhs, rhs) => {
                                            ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: Operation ",
                                                                                                          " not defined for ",
                                                                                                          " and "],
                                                                                                        &match (&op,
                                                                                                                &lhs,
                                                                                                                &rhs)
                                                                                                             {
                                                                                                             (arg0,
                                                                                                              arg1,
                                                                                                              arg2)
                                                                                                             =>
                                                                                                             [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                           ::core::fmt::Display::fmt),
                                                                                                              ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                           ::core::fmt::Debug::fmt),
                                                                                                              ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                                           ::core::fmt::Debug::fmt)],
                                                                                                         }))
                                        }
                                    })
        }
        fn eval_int_comparison(&self, lhs: &CallResolverOperand<R>,
                               rhs: &CallResolverOperand<R>,
                               eval: impl FnOnce(i64, i64) -> bool,
                               op: &'static str) -> FlatSet<ConstVal> {
            let lhs = self.resolve_operand(lhs);
            let rhs = self.resolve_operand(rhs);
            lhs.apply_binary_op(rhs,
                                |lhs, rhs|
                                    match (lhs, rhs) {
                                        (Scalar(Integer(lhs)),
                                         Scalar(Integer(rhs))) =>
                                        Scalar(Bool(eval(lhs, rhs))),
                                        (lhs, rhs) => {
                                            ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: Operation ",
                                                                                                          " not defined for ",
                                                                                                          " and "],
                                                                                                        &match (&op,
                                                                                                                &lhs,
                                                                                                                &rhs)
                                                                                                             {
                                                                                                             (arg0,
                                                                                                              arg1,
                                                                                                              arg2)
                                                                                                             =>
                                                                                                             [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                           ::core::fmt::Display::fmt),
                                                                                                              ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                           ::core::fmt::Debug::fmt),
                                                                                                              ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                                           ::core::fmt::Debug::fmt)],
                                                                                                         }))
                                        }
                                    })
        }
        fn eval_bin_real(&self, lhs: &CallResolverOperand<R>,
                         rhs: &CallResolverOperand<R>,
                         eval: impl FnOnce(f64, f64) -> f64, op: &'static str)
         -> FlatSet<ConstVal> {
            let lhs = self.resolve_operand(lhs);
            let rhs = self.resolve_operand(rhs);
            lhs.apply_binary_op(rhs,
                                |lhs, rhs|
                                    match (lhs, rhs) {
                                        (Scalar(Real(lhs)), Scalar(Real(rhs)))
                                        => Scalar(Real(eval(lhs, rhs))),
                                        (lhs, rhs) => {
                                            ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: Operation ",
                                                                                                          " not defined for ",
                                                                                                          " and "],
                                                                                                        &match (&op,
                                                                                                                &lhs,
                                                                                                                &rhs)
                                                                                                             {
                                                                                                             (arg0,
                                                                                                              arg1,
                                                                                                              arg2)
                                                                                                             =>
                                                                                                             [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                           ::core::fmt::Display::fmt),
                                                                                                              ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                           ::core::fmt::Debug::fmt),
                                                                                                              ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                                           ::core::fmt::Debug::fmt)],
                                                                                                         }))
                                        }
                                    })
        }
        fn eval_bin_int(&self, lhs: &CallResolverOperand<R>,
                        rhs: &CallResolverOperand<R>,
                        eval: impl FnOnce(i64, i64) -> i64, op: &'static str)
         -> FlatSet<ConstVal> {
            let lhs = self.resolve_operand(lhs);
            let rhs = self.resolve_operand(rhs);
            lhs.apply_binary_op(rhs,
                                |lhs, rhs|
                                    match (lhs, rhs) {
                                        (Scalar(Integer(lhs)),
                                         Scalar(Integer(rhs))) =>
                                        Scalar(Integer(eval(lhs, rhs))),
                                        (lhs, rhs) => {
                                            ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: Operation ",
                                                                                                          " not defined for ",
                                                                                                          " and "],
                                                                                                        &match (&op,
                                                                                                                &lhs,
                                                                                                                &rhs)
                                                                                                             {
                                                                                                             (arg0,
                                                                                                              arg1,
                                                                                                              arg2)
                                                                                                             =>
                                                                                                             [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                           ::core::fmt::Display::fmt),
                                                                                                              ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                           ::core::fmt::Debug::fmt),
                                                                                                              ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                                           ::core::fmt::Debug::fmt)],
                                                                                                         }))
                                        }
                                    })
        }
        fn eval_bin_bool(&self, lhs: &CallResolverOperand<R>,
                         rhs: &CallResolverOperand<R>,
                         eval: impl FnOnce(bool, bool) -> bool,
                         op: &'static str) -> FlatSet<ConstVal> {
            let lhs = self.resolve_operand(lhs);
            let rhs = self.resolve_operand(rhs);
            lhs.apply_binary_op(rhs,
                                |lhs, rhs|
                                    match (lhs, rhs) {
                                        (Scalar(Bool(lhs)), Scalar(Bool(rhs)))
                                        => Scalar(Bool(eval(lhs, rhs))),
                                        (lhs, rhs) => {
                                            ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: Operation ",
                                                                                                          " not defined for ",
                                                                                                          " and "],
                                                                                                        &match (&op,
                                                                                                                &lhs,
                                                                                                                &rhs)
                                                                                                             {
                                                                                                             (arg0,
                                                                                                              arg1,
                                                                                                              arg2)
                                                                                                             =>
                                                                                                             [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                           ::core::fmt::Display::fmt),
                                                                                                              ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                           ::core::fmt::Debug::fmt),
                                                                                                              ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                                           ::core::fmt::Debug::fmt)],
                                                                                                         }))
                                        }
                                    })
        }
    }
    impl <'lt, R: CallResolver, F: Fn(Local) -> FlatSet<ConstVal>>
     RValueFold<R::C> for ConstantFold<'lt, R, F> {
        type T = FlatSet<ConstVal>;
        fn fold_cmplx_arith_negate(&mut self, _op: Span,
                                   arg: &CallResolverOperand<R>) -> Self::T {
            self.eval_cmplx(arg, Complex64::neg, "Complex Negate")
        }
        fn fold_real_arith_negate(&mut self, _op: Span,
                                  arg: &CallResolverOperand<R>) -> Self::T {
            self.eval_real(arg, f64::neg, "Real Negate")
        }
        fn fold_bit_negate(&mut self, _op: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_int(arg, i64::reverse_bits, "Bit Negate")
        }
        fn fold_int_arith_negate(&mut self, _op: Span,
                                 arg: &CallResolverOperand<R>) -> Self::T {
            self.eval_int(arg, i64::neg, "Integer Negate")
        }
        fn fold_logic_negate(&mut self, _op: Span,
                             arg: &CallResolverOperand<R>) -> Self::T {
            self.eval_bool(arg, |arg| !arg, "Integer Negate")
        }
        fn fold_cmplx_add(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                          rhs: &CallResolverOperand<R>) -> Self::T {
            let lhs = self.resolve_operand(lhs);
            let rhs = self.resolve_operand(rhs);
            lhs.apply_binary_op(rhs,
                                |lhs, rhs|
                                    match (lhs, rhs) {
                                        (Scalar(Cmplx(lhs)),
                                         Scalar(Cmplx(rhs))) =>
                                        Scalar(Cmplx(lhs + rhs)),
                                        (Scalar(Cmplx(lhs)),
                                         Scalar(Real(rhs))) =>
                                        Scalar(Cmplx(lhs + rhs)),
                                        (Scalar(Real(lhs)),
                                         Scalar(Cmplx(rhs))) =>
                                        Scalar(Cmplx(lhs + rhs)),
                                        (lhs, rhs) => {
                                            ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: Operation ",
                                                                                                          " not defined for ",
                                                                                                          " and "],
                                                                                                        &match (&"Complex Summation",
                                                                                                                &lhs,
                                                                                                                &rhs)
                                                                                                             {
                                                                                                             (arg0,
                                                                                                              arg1,
                                                                                                              arg2)
                                                                                                             =>
                                                                                                             [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                           ::core::fmt::Display::fmt),
                                                                                                              ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                           ::core::fmt::Debug::fmt),
                                                                                                              ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                                           ::core::fmt::Debug::fmt)],
                                                                                                         }))
                                        }
                                    })
        }
        fn fold_cmplx_sub(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                          rhs: &CallResolverOperand<R>) -> Self::T {
            let lhs = self.resolve_operand(lhs);
            let rhs = self.resolve_operand(rhs);
            lhs.apply_binary_op(rhs,
                                |lhs, rhs|
                                    match (lhs, rhs) {
                                        (Scalar(Cmplx(lhs)),
                                         Scalar(Cmplx(rhs))) =>
                                        Scalar(Cmplx(lhs - rhs)),
                                        (Scalar(Cmplx(lhs)),
                                         Scalar(Real(rhs))) =>
                                        Scalar(Cmplx(lhs - rhs)),
                                        (Scalar(Real(lhs)),
                                         Scalar(Cmplx(rhs))) =>
                                        Scalar(Cmplx(lhs - rhs)),
                                        (lhs, rhs) => {
                                            ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: Operation ",
                                                                                                          " not defined for ",
                                                                                                          " and "],
                                                                                                        &match (&"Complex Subtraction",
                                                                                                                &lhs,
                                                                                                                &rhs)
                                                                                                             {
                                                                                                             (arg0,
                                                                                                              arg1,
                                                                                                              arg2)
                                                                                                             =>
                                                                                                             [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                           ::core::fmt::Display::fmt),
                                                                                                              ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                           ::core::fmt::Debug::fmt),
                                                                                                              ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                                           ::core::fmt::Debug::fmt)],
                                                                                                         }))
                                        }
                                    })
        }
        fn fold_cmplx_mul(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                          rhs: &CallResolverOperand<R>) -> Self::T {
            let lhs = self.resolve_operand(lhs);
            let rhs = self.resolve_operand(rhs);
            match (lhs, rhs) {
                (Elem(Scalar(Cmplx(arg1))), Elem(Scalar(Cmplx(arg2)))) => {
                    Elem(Scalar(Cmplx(arg1 * arg2)))
                }
                (Elem(Scalar(Cmplx(arg1))), Elem(Scalar(Real(arg2)))) => {
                    Elem(Scalar(Cmplx(arg1 * arg2)))
                }
                (Elem(Scalar(Real(arg1))), Elem(Scalar(Cmplx(arg2)))) => {
                    Elem(Scalar(Cmplx(arg1 * arg2)))
                }
                (Elem(arg1), Elem(arg2)) => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: Operation ",
                                                                                      " not defined for ",
                                                                                      " and "],
                                                                                    &match (&"Complex Multiplication",
                                                                                            &arg1,
                                                                                            &arg2)
                                                                                         {
                                                                                         (arg0,
                                                                                          arg1,
                                                                                          arg2)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt),
                                                                                          ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                       ::core::fmt::Debug::fmt),
                                                                                          ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                       ::core::fmt::Debug::fmt)],
                                                                                     }))
                    }
                }
                (Elem(Scalar(Cmplx(arg))), _) | (_, Elem(Scalar(Cmplx(arg))))
                if arg == Complex64::new(0.0, 0.0) => {
                    Elem(Scalar(Cmplx(Complex64::new(0.0, 0.0))))
                }
                (Elem(Scalar(Real(arg))), _) | (_, Elem(Scalar(Real(arg)))) if
                arg == 0.0 => {
                    Elem(Scalar(Cmplx(Complex64::new(0.0, 0.0))))
                }
                (Top, _) | (_, Top) => Top,
                (_, _) => Bottom,
            }
        }
        fn fold_cmplx_div(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                          rhs: &CallResolverOperand<R>) -> Self::T {
            let lhs = self.resolve_operand(lhs);
            let rhs = self.resolve_operand(rhs);
            match (lhs, rhs) {
                (Elem(Scalar(Cmplx(arg1))), Elem(Scalar(Cmplx(arg2)))) => {
                    Elem(Scalar(Cmplx(arg1 / arg2)))
                }
                (Elem(Scalar(Real(arg1))), Elem(Scalar(Cmplx(arg2)))) => {
                    Elem(Scalar(Cmplx(arg1 / arg2)))
                }
                (Elem(Scalar(Cmplx(arg1))), Elem(Scalar(Real(arg2)))) => {
                    Elem(Scalar(Cmplx(arg1 / arg2)))
                }
                (Elem(arg1), Elem(arg2)) => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: Operation ",
                                                                                      " not defined for ",
                                                                                      " and "],
                                                                                    &match (&"Complex Division",
                                                                                            &arg1,
                                                                                            &arg2)
                                                                                         {
                                                                                         (arg0,
                                                                                          arg1,
                                                                                          arg2)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt),
                                                                                          ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                       ::core::fmt::Debug::fmt),
                                                                                          ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                       ::core::fmt::Debug::fmt)],
                                                                                     }))
                    }
                }
                (Elem(Scalar(Cmplx(arg))), _) if
                arg == Complex64::new(0.0, 0.0) => {
                    Elem(Scalar(Cmplx(Complex64::new(0.0, 0.0))))
                }
                (Elem(Scalar(Real(arg))), _) if arg == 0.0 => {
                    Elem(Scalar(Cmplx(Complex64::new(0.0, 0.0))))
                }
                (Top, _) | (_, Top) => Top,
                (_, _) => Bottom,
            }
        }
        fn fold_real_add(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                         rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_real(lhs, rhs, f64::add, "Real Summation")
        }
        fn fold_real_sub(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                         rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_real(lhs, rhs, f64::sub, "Real Subtraction")
        }
        fn fold_real_mul(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                         rhs: &CallResolverOperand<R>) -> Self::T {
            let lhs = self.resolve_operand(lhs);
            let rhs = self.resolve_operand(rhs);
            match (lhs, rhs) {
                (Elem(Scalar(Real(arg1))), Elem(Scalar(Real(arg2)))) =>
                Elem(Scalar(Real(arg1 * arg2))),
                (Elem(arg1), Elem(arg2)) => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: Operation ",
                                                                                      " not defined for ",
                                                                                      " and "],
                                                                                    &match (&"Real Multiplication",
                                                                                            &arg1,
                                                                                            &arg2)
                                                                                         {
                                                                                         (arg0,
                                                                                          arg1,
                                                                                          arg2)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt),
                                                                                          ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                       ::core::fmt::Debug::fmt),
                                                                                          ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                       ::core::fmt::Debug::fmt)],
                                                                                     }))
                    }
                }
                (Elem(Scalar(Real(arg))), _) | (_, Elem(Scalar(Real(arg)))) if
                arg == 0.0 => {
                    Elem(Scalar(Real(0.0)))
                }
                (Top, _) | (_, Top) => Top,
                _ => Bottom,
            }
        }
        fn fold_real_div(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                         rhs: &CallResolverOperand<R>) -> Self::T {
            let lhs = self.resolve_operand(lhs);
            let rhs = self.resolve_operand(rhs);
            match (lhs, rhs) {
                (Elem(Scalar(Real(arg1))), Elem(Scalar(Real(arg2)))) =>
                Elem(Scalar(Real(arg1 / arg2))),
                (Elem(arg1), Elem(arg2)) => {
                    ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: Operation ",
                                                                                  " not defined for ",
                                                                                  " and "],
                                                                                &match (&"Real Division",
                                                                                        &arg1,
                                                                                        &arg2)
                                                                                     {
                                                                                     (arg0,
                                                                                      arg1,
                                                                                      arg2)
                                                                                     =>
                                                                                     [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                   ::core::fmt::Display::fmt),
                                                                                      ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                   ::core::fmt::Debug::fmt),
                                                                                      ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                   ::core::fmt::Debug::fmt)],
                                                                                 }))
                }
                (Elem(Scalar(Real(arg))), _) if arg == 0.0 =>
                Elem(Scalar(Real(0.0))),
                (Top, _) | (_, Top) => Top,
                _ => Bottom,
            }
        }
        fn fold_real_rem(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                         rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_real(lhs, rhs, f64::rem, "Real Remainder")
        }
        fn fold_int_add(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                        rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_int(lhs, rhs, i64::add, "Integer Subtraction")
        }
        fn fold_int_sub(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                        rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_int(lhs, rhs, i64::sub, "Integer Subtraction")
        }
        fn fold_int_mul(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                        rhs: &CallResolverOperand<R>) -> Self::T {
            let lhs = self.resolve_operand(lhs);
            let rhs = self.resolve_operand(rhs);
            match (lhs, rhs) {
                (Elem(Scalar(Integer(arg1))), Elem(Scalar(Integer(arg2)))) =>
                {
                    Elem(Scalar(Integer(arg1 * arg2)))
                }
                (Elem(arg1), Elem(arg2)) => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: Operation ",
                                                                                      " not defined for ",
                                                                                      " and "],
                                                                                    &match (&"Integer Multiplication",
                                                                                            &arg1,
                                                                                            &arg2)
                                                                                         {
                                                                                         (arg0,
                                                                                          arg1,
                                                                                          arg2)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt),
                                                                                          ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                       ::core::fmt::Debug::fmt),
                                                                                          ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                       ::core::fmt::Debug::fmt)],
                                                                                     }))
                    }
                }
                (Elem(Scalar(Integer(0))), _) | (_, Elem(Scalar(Integer(0))))
                => {
                    Elem(Scalar(Integer(0)))
                }
                (Top, _) | (_, Top) => Top,
                _ => Bottom,
            }
        }
        fn fold_int_div(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                        rhs: &CallResolverOperand<R>) -> Self::T {
            let lhs = self.resolve_operand(lhs);
            let rhs = self.resolve_operand(rhs);
            match (lhs, rhs) {
                (Elem(Scalar(Integer(arg1))), Elem(Scalar(Integer(arg2)))) =>
                {
                    Elem(Scalar(Integer(arg1 / arg2)))
                }
                (Elem(arg1), Elem(arg2)) => {
                    ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: Operation ",
                                                                                  " not defined for ",
                                                                                  " and "],
                                                                                &match (&"Real Division",
                                                                                        &arg1,
                                                                                        &arg2)
                                                                                     {
                                                                                     (arg0,
                                                                                      arg1,
                                                                                      arg2)
                                                                                     =>
                                                                                     [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                   ::core::fmt::Display::fmt),
                                                                                      ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                   ::core::fmt::Debug::fmt),
                                                                                      ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                   ::core::fmt::Debug::fmt)],
                                                                                 }))
                }
                (Elem(Scalar(Integer(0))), _) => Elem(Scalar(Integer(0))),
                (Top, _) | (_, Top) => Top,
                _ => Bottom,
            }
        }
        fn fold_int_rem(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                        rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_int(lhs, rhs, i64::rem, "Integer Remainder")
        }
        fn fold_shiftl(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                       rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_int(lhs, rhs, i64::shl, "Integer Shift Left")
        }
        fn fold_shiftr(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                       rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_int(lhs, rhs, i64::shr, "Integer Shift Right")
        }
        fn fold_lt_real(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                        rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_real_comparison(lhs, rhs, |arg1, arg2| arg1 < arg2,
                                      "Real Less Than")
        }
        fn fold_le_real(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                        rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_real_comparison(lhs, rhs, |arg1, arg2| arg1 <= arg2,
                                      "Real Less Equal")
        }
        fn fold_gt_real(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                        rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_real_comparison(lhs, rhs, |arg1, arg2| arg1 > arg2,
                                      "Real Greater than")
        }
        fn fold_ge_real(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                        rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_real_comparison(lhs, rhs, |arg1, arg2| arg1 >= arg2,
                                      "Real Greater Equal")
        }
        fn fold_lt_int(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                       rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_int_comparison(lhs, rhs, |arg1, arg2| arg1 < arg2,
                                     "Integer Less Then")
        }
        fn fold_le_int(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                       rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_int_comparison(lhs, rhs, |arg1, arg2| arg1 <= arg2,
                                     "Integer Less Equal")
        }
        fn fold_gt_int(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                       rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_int_comparison(lhs, rhs, |arg1, arg2| arg1 > arg2,
                                     "Integer Greater Then")
        }
        fn fold_ge_int(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                       rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_int_comparison(lhs, rhs, |arg1, arg2| arg1 >= arg2,
                                     "Integer Greater Equal")
        }
        fn fold_eq(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                   rhs: &CallResolverOperand<R>, _ty: Type) -> Self::T {
            let lhs = self.resolve_operand(lhs);
            let rhs = self.resolve_operand(rhs);
            lhs.apply_binary_op(rhs, |x, y| Scalar(Bool(x == y)))
        }
        fn fold_ne(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                   rhs: &CallResolverOperand<R>, _ty: Type) -> Self::T {
            let lhs = self.resolve_operand(lhs);
            let rhs = self.resolve_operand(rhs);
            lhs.apply_binary_op(rhs, |x, y| Scalar(Bool(x != y)))
        }
        fn fold_xor(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                    rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_int(lhs, rhs, i64::bitxor, "Integer XOR")
        }
        fn fold_nxor(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                     rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_int(lhs, rhs, |lhs, rhs| !(lhs ^ rhs),
                              "Integer NXOR")
        }
        fn fold_and(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                    rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_int(lhs, rhs, i64::bitand, "Integer NXOR")
        }
        fn fold_or(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                   rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_int(lhs, rhs, i64::bitor, "Integer NXOR")
        }
        fn fold_bool_xor(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                         rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_bool(lhs, rhs, bool::bitxor, "Integer NXOR")
        }
        fn fold_bool_nxor(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                          rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_bool(lhs, rhs, |lhs, rhs| !(lhs ^ rhs),
                               "Integer NXOR")
        }
        fn fold_bool_and(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                         rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_bool(lhs, rhs, bool::bitand, "Integer NXOR")
        }
        fn fold_bool_or(&mut self, _op: Span, lhs: &CallResolverOperand<R>,
                        rhs: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_bool(lhs, rhs, bool::bitor, "Integer NXOR")
        }
        fn fold_exp(&mut self, _span: Span, arg: &CallResolverOperand<R>,
                    _limit: bool) -> Self::T {
            self.eval_real(arg, f64::exp, "EXP")
        }
        fn fold_ln(&mut self, _span: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_real(arg, f64::ln, "LN")
        }
        fn fold_log(&mut self, _span: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_real(arg, f64::log10, "LOG")
        }
        fn fold_clog2(&mut self, _span: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_int(arg,
                          |val|
                              8 * size_of_val(&val) as i64 -
                                  val.abs().leading_zeros() as i64, "clog2")
        }
        fn fold_sqrt(&mut self, _span: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_real(arg, f64::sqrt, "SQRT")
        }
        fn fold_cmplx_abs(&mut self, _span: Span,
                          arg: &CallResolverOperand<R>) -> Self::T {
            self.eval_cmplx(arg, |x| x.norm().into(), "CMPLX ABS")
        }
        fn fold_real_abs(&mut self, _span: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_real(arg, f64::abs, "Real ABS")
        }
        fn fold_int_abs(&mut self, _span: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_int(arg, i64::abs, "Integer ABS")
        }
        fn fold_ceil(&mut self, _span: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_real(arg, f64::ceil, "Real Ceil")
        }
        fn fold_floor(&mut self, _span: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_real(arg, f64::floor, "Real Flor")
        }
        fn fold_sin(&mut self, _span: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_real(arg, f64::sin, "Real Sin")
        }
        fn fold_cos(&mut self, _span: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_real(arg, f64::cos, "Real Cos")
        }
        fn fold_tan(&mut self, _span: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_real(arg, f64::tan, "Real Tan")
        }
        fn fold_sinh(&mut self, _span: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_real(arg, f64::sinh, "Real SinH")
        }
        fn fold_cosh(&mut self, _span: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_real(arg, f64::cosh, "Real CosH")
        }
        fn fold_tanh(&mut self, _span: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_real(arg, f64::tanh, "Real TanH")
        }
        fn fold_asin(&mut self, _span: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_real(arg, f64::asin, "Real ASIN")
        }
        fn fold_acos(&mut self, _span: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_real(arg, f64::acos, "Real ACOS")
        }
        fn fold_atan(&mut self, _span: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_real(arg, f64::atan, "Real ATAN")
        }
        fn fold_asinh(&mut self, _span: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_real(arg, f64::asin, "Real ASINH")
        }
        fn fold_acosh(&mut self, _span: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_real(arg, f64::acosh, "Real ACOSH")
        }
        fn fold_atanh(&mut self, _span: Span, arg: &CallResolverOperand<R>)
         -> Self::T {
            self.eval_real(arg, f64::atanh, "Real ATANH")
        }
        fn fold_pow(&mut self, _span: Span, arg1: &CallResolverOperand<R>,
                    arg2: &CallResolverOperand<R>) -> Self::T {
            let lhs = self.resolve_operand(arg1);
            let rhs = self.resolve_operand(arg2);
            match (lhs, rhs) {
                (Elem(Scalar(Real(arg1))), Elem(Scalar(Real(arg2)))) => {
                    Elem(Scalar(Real(arg1.powf(arg2))))
                }
                (Elem(arg1), Elem(arg2)) => {
                    ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: Operation ",
                                                                                  " not defined for ",
                                                                                  " and "],
                                                                                &match (&"POW",
                                                                                        &arg1,
                                                                                        &arg2)
                                                                                     {
                                                                                     (arg0,
                                                                                      arg1,
                                                                                      arg2)
                                                                                     =>
                                                                                     [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                   ::core::fmt::Display::fmt),
                                                                                      ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                   ::core::fmt::Debug::fmt),
                                                                                      ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                   ::core::fmt::Debug::fmt)],
                                                                                 }))
                }
                (Elem(Scalar(Real(arg))), _) if arg == 0.0 =>
                Elem(Scalar(Real(0.0))),
                (Elem(Scalar(Real(arg))), _) if arg == 1.0 =>
                Elem(Scalar(Real(1.0))),
                (_, Elem(Scalar(Real(arg)))) if arg == 0.0 =>
                Elem(Scalar(Real(1.0))),
                (Top, _) | (_, Top) => Top,
                _ => Bottom,
            }
        }
        fn fold_hypot(&mut self, _span: Span, arg1: &CallResolverOperand<R>,
                      arg2: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_real(arg1, arg2, f64::hypot, "HYPOT")
        }
        fn fold_real_min(&mut self, _span: Span,
                         arg1: &CallResolverOperand<R>,
                         arg2: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_real(arg1, arg2, f64::min, "Real Min")
        }
        fn fold_int_min(&mut self, _span: Span, arg1: &CallResolverOperand<R>,
                        arg2: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_int(arg1, arg2, i64::min, "Integer Min")
        }
        fn fold_real_max(&mut self, _span: Span,
                         arg1: &CallResolverOperand<R>,
                         arg2: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_real(arg1, arg2, f64::max, "Real Max")
        }
        fn fold_int_max(&mut self, _span: Span, arg1: &CallResolverOperand<R>,
                        arg2: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_int(arg1, arg2, i64::max, "Integer Max")
        }
        fn fold_atan2(&mut self, _span: Span, arg1: &CallResolverOperand<R>,
                      arg2: &CallResolverOperand<R>) -> Self::T {
            self.eval_bin_real(arg1, arg2, f64::atan2, "ATAN2")
        }
        fn fold_cast(&mut self, arg: &CallResolverOperand<R>, dst: Type)
         -> Self::T {
            let arg = self.resolve_operand(arg);
            arg.map(|arg|
                        {
                            match (dst, arg) {
                                (Type::REAL, Scalar(Integer(val))) =>
                                Scalar(Real(val as f64)),
                                (Type::REAL, Scalar(Bool(val))) =>
                                Scalar(Real(val as i64 as f64)),
                                (Type::BOOL, Scalar(Integer(val))) =>
                                Scalar(Bool(val != 0)),
                                (Type::BOOL, Scalar(Real(val))) =>
                                Scalar(Bool(val != 0.0)),
                                (Type::INT, Scalar(Real(val))) =>
                                Scalar(Integer(val.round() as i64)),
                                (Type::INT, Scalar(Bool(val))) =>
                                Scalar(Integer(val as i64)),
                                (Type::CMPLX, Scalar(Real(val))) =>
                                Scalar(Cmplx(Complex64::new(val, 0.0))),
                                (Type::CMPLX, Scalar(Integer(val))) => {
                                    Scalar(Cmplx(Complex64::new(val as f64,
                                                                0.0)))
                                }
                                _ => {
                                    {
                                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                                    &match (&"Malformed MIR",)
                                                                                                         {
                                                                                                         (arg0,)
                                                                                                         =>
                                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                       ::core::fmt::Display::fmt)],
                                                                                                     }))
                                    }
                                }
                            }
                        })
        }
        fn fold_use(&mut self, arg: &CallResolverOperand<R>) -> Self::T {
            self.resolve_operand(arg)
        }
        fn fold_select(&mut self, cond: &CallResolverOperand<R>,
                       true_val: &CallResolverOperand<R>,
                       false_val: &CallResolverOperand<R>) -> Self::T {
            let cond = self.resolve_operand(cond);
            cond.and_then(|cond|
                              match cond {
                                  Scalar(Bool(false)) =>
                                  self.resolve_operand(false_val),
                                  Scalar(Bool(true)) =>
                                  self.resolve_operand(true_val),
                                  cond => {
                                      ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: Operation ",
                                                                                                    " not defined for "],
                                                                                                  &match (&"SELECT",
                                                                                                          &cond)
                                                                                                       {
                                                                                                       (arg0,
                                                                                                        arg1)
                                                                                                       =>
                                                                                                       [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                                     ::core::fmt::Display::fmt),
                                                                                                        ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                                     ::core::fmt::Debug::fmt)],
                                                                                                   }))
                                  }
                              })
        }
        fn fold_call(&mut self, call: &R::C,
                     args: &IndexSlice<CallArg, [CallResolverOperand<R>]>,
                     _span: Span) -> Self::T {
            let args =
                args.iter().map(|op| self.resolve_operand(op)).collect_vec();
            call.const_fold(&args)
        }
        fn fold_array(&mut self, args: &[CallResolverOperand<R>], _span: Span,
                      ty: Type) -> Self::T {
            let mut unknown = false;
            let mut res = Vec::with_capacity(args.len());
            for arg in args {
                match self.resolve_operand(arg) {
                    FlatSet::Top => return Top,
                    FlatSet::Bottom => unknown = true,
                    FlatSet::Elem(_) if unknown => (),
                    FlatSet::Elem(val) => val.flatten(&mut res),
                }
            }
            if unknown {
                FlatSet::Bottom
            } else {
                FlatSet::Elem(ConstVal::Array(res.into_boxed_slice(), ty))
            }
        }
    }
}
pub mod derivatives {
    use crate::cfg::{ControlFlowGraph, PhiData};
    use crate::{COperand, CallTypeDerivative, CfgFunctions, Derivative, Local,
                Mir, Operand, OperandData, RValue, Statement, StmntKind,
                SyntaxCtx};
    use enum_map::EnumMap;
    use data_structures::index_vec::IndexVec;
    use diagnostics::MultiDiagnostic;
    use ir::ids::StatementId;
    use std::mem::replace;
    pub use error::Error;
    mod error {
        use std::fmt::{Display, Formatter};
        use thiserror::Error;
        use diagnostics::{AnnotationType, DiagnosticSlice,
                                  LibraryDiagnostic, Text};
        use session::sourcemap::Span;
        pub enum UndefinedDerivative {
            Modulus,
            Noise,
            BitWiseOp,
            LogicOp,
            Comparison,
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::marker::Copy for UndefinedDerivative { }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::clone::Clone for UndefinedDerivative {
            #[inline]
            fn clone(&self) -> UndefinedDerivative { { *self } }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::fmt::Debug for UndefinedDerivative {
            fn fmt(&self, f: &mut ::core::fmt::Formatter)
             -> ::core::fmt::Result {
                match (&*self,) {
                    (&UndefinedDerivative::Modulus,) => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                     "Modulus");
                        ::core::fmt::DebugTuple::finish(debug_trait_builder)
                    }
                    (&UndefinedDerivative::Noise,) => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                     "Noise");
                        ::core::fmt::DebugTuple::finish(debug_trait_builder)
                    }
                    (&UndefinedDerivative::BitWiseOp,) => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                     "BitWiseOp");
                        ::core::fmt::DebugTuple::finish(debug_trait_builder)
                    }
                    (&UndefinedDerivative::LogicOp,) => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                     "LogicOp");
                        ::core::fmt::DebugTuple::finish(debug_trait_builder)
                    }
                    (&UndefinedDerivative::Comparison,) => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                     "Comparison");
                        ::core::fmt::DebugTuple::finish(debug_trait_builder)
                    }
                }
            }
        }
        impl ::core::marker::StructuralEq for UndefinedDerivative { }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::cmp::Eq for UndefinedDerivative {
            #[inline]
            #[doc(hidden)]
            #[no_coverage]
            fn assert_receiver_is_total_eq(&self) -> () { { } }
        }
        impl ::core::marker::StructuralPartialEq for UndefinedDerivative { }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::cmp::PartialEq for UndefinedDerivative {
            #[inline]
            fn eq(&self, other: &UndefinedDerivative) -> bool {
                {
                    let __self_vi =
                        ::core::intrinsics::discriminant_value(&*self);
                    let __arg_1_vi =
                        ::core::intrinsics::discriminant_value(&*other);
                    if true && __self_vi == __arg_1_vi {
                        match (&*self, &*other) { _ => true, }
                    } else { false }
                }
            }
        }
        impl Display for UndefinedDerivative {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                match self {
                    Self::Modulus => f.write_str("modulus ('%')"),
                    Self::Noise => f.write_str("noise filters"),
                    Self::BitWiseOp =>
                    f.write_str("bit wise operations (such as |, &, ~)"),
                    Self::LogicOp =>
                    f.write_str("logical operations (such as ||, &&, ~)"),
                    Self::Comparison =>
                    f.write_str("comparison operations (such as ==, >, !=)"),
                }
            }
        }
        pub enum Error {

            #[error("the derivative of {0} is not defined")]
            DerivativeNotDefined(UndefinedDerivative, Span),

            #[error("derivatives can only be calculated for numeric variables")]
            OnlyNumericExpressionsCanBeDerived(Span),
        }
        #[allow(unused_qualifications)]
        impl std::error::Error for Error { }
        #[allow(unused_qualifications)]
        impl std::fmt::Display for Error {
            fn fmt(&self, __formatter: &mut std::fmt::Formatter)
             -> std::fmt::Result {
                #[allow(unused_imports)]
                use thiserror::private::{DisplayAsDisplay, PathAsDisplay};

                #[allow(unused_variables, deprecated, clippy ::
                        nonstandard_macro_braces, clippy ::
                        used_underscore_binding,)]
                match self {
                    Error::DerivativeNotDefined(_0, _1) =>
                    __formatter.write_fmt(::core::fmt::Arguments::new_v1(&["the derivative of ",
                                                                           " is not defined"],
                                                                         &match (&_0.as_display(),)
                                                                              {
                                                                              (arg0,)
                                                                              =>
                                                                              [::core::fmt::ArgumentV1::new(arg0,
                                                                                                            ::core::fmt::Display::fmt)],
                                                                          })),
                    Error::OnlyNumericExpressionsCanBeDerived(_0) =>
                    __formatter.write_fmt(::core::fmt::Arguments::new_v1(&["derivatives can only be calculated for numeric variables"],
                                                                         &match ()
                                                                              {
                                                                              ()
                                                                              =>
                                                                              [],
                                                                          })),
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::fmt::Debug for Error {
            fn fmt(&self, f: &mut ::core::fmt::Formatter)
             -> ::core::fmt::Result {
                match (&*self,) {
                    (&Error::DerivativeNotDefined(ref __self_0,
                                                  ref __self_1),) => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                     "DerivativeNotDefined");
                        let _ =
                            ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                           &&(*__self_0));
                        let _ =
                            ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                           &&(*__self_1));
                        ::core::fmt::DebugTuple::finish(debug_trait_builder)
                    }
                    (&Error::OnlyNumericExpressionsCanBeDerived(ref __self_0),)
                    => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                     "OnlyNumericExpressionsCanBeDerived");
                        let _ =
                            ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                           &&(*__self_0));
                        ::core::fmt::DebugTuple::finish(debug_trait_builder)
                    }
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::clone::Clone for Error {
            #[inline]
            fn clone(&self) -> Error {
                match (&*self,) {
                    (&Error::DerivativeNotDefined(ref __self_0,
                                                  ref __self_1),) =>
                    Error::DerivativeNotDefined(::core::clone::Clone::clone(&(*__self_0)),
                                                ::core::clone::Clone::clone(&(*__self_1))),
                    (&Error::OnlyNumericExpressionsCanBeDerived(ref __self_0),)
                    =>
                    Error::OnlyNumericExpressionsCanBeDerived(::core::clone::Clone::clone(&(*__self_0))),
                }
            }
        }
        impl LibraryDiagnostic for Error {
            #[inline(always)]
            fn annotation_type(&self) -> Option<AnnotationType> {
                Some(AnnotationType::Error)
            }
            fn slices(&self) -> Vec<DiagnosticSlice> {
                match self {
                    Self::DerivativeNotDefined(_, span) =>
                    <[_]>::into_vec(box
                                        [DiagnosticSlice{slice_span:
                                                             span.data(),
                                                         messages:
                                                             <[_]>::into_vec(box
                                                                                 [(AnnotationType::Error,
                                                                                   Text::const_str("Derivative of this expression is not defined"),
                                                                                   span.data())]),
                                                         fold: false,}]),
                    Self::OnlyNumericExpressionsCanBeDerived(span) =>
                    <[_]>::into_vec(box
                                        [DiagnosticSlice{slice_span:
                                                             span.data(),
                                                         messages:
                                                             <[_]>::into_vec(box
                                                                                 [(AnnotationType::Error,
                                                                                   Text::const_str("Strings can not be differentiated"),
                                                                                   span.data())]),
                                                         fold: false,}]),
                }
            }
        }
    }
    pub mod lints {
        use core::fmt::Formatter;
        use diagnostics::lints::{builtin, Lint, LintDiagnostic};
        use diagnostics::{AnnotationType, DiagnosticSlice, FooterItem,
                                  Text};
        use session::sourcemap::Span;
        use std::error::Error;
        use std::fmt::Display;
        pub struct RoundingDerivativeNotFullyDefined(pub Span);
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::clone::Clone for RoundingDerivativeNotFullyDefined {
            #[inline]
            fn clone(&self) -> RoundingDerivativeNotFullyDefined {
                match *self {
                    RoundingDerivativeNotFullyDefined(ref __self_0_0) =>
                    RoundingDerivativeNotFullyDefined(::core::clone::Clone::clone(&(*__self_0_0))),
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::fmt::Debug for RoundingDerivativeNotFullyDefined {
            fn fmt(&self, f: &mut ::core::fmt::Formatter)
             -> ::core::fmt::Result {
                match *self {
                    RoundingDerivativeNotFullyDefined(ref __self_0_0) => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                     "RoundingDerivativeNotFullyDefined");
                        let _ =
                            ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                           &&(*__self_0_0));
                        ::core::fmt::DebugTuple::finish(debug_trait_builder)
                    }
                }
            }
        }
        impl Error for RoundingDerivativeNotFullyDefined { }
        impl Display for RoundingDerivativeNotFullyDefined {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                f.write_str("Derivative of rounding required")
            }
        }
        impl LintDiagnostic for RoundingDerivativeNotFullyDefined {
            fn lint(&self) -> Lint { builtin::rounding_derivative }
            fn slices(&self, main_annotation_type: AnnotationType)
             -> Vec<DiagnosticSlice> {
                <[_]>::into_vec(box
                                    [DiagnosticSlice{slice_span:
                                                         self.0.data(),
                                                     messages:
                                                         <[_]>::into_vec(box
                                                                             [(main_annotation_type,
                                                                               Text::const_str("Required while deriving this"),
                                                                               self.0.data())]),
                                                     fold: false,}])
            }
            #[inline]
            fn footer(&self) -> Vec<FooterItem> {
                <[_]>::into_vec(box
                                    [FooterItem{id: None,
                                                label:
                                                    Text::const_str("Derivative is assumed to be always 0. However technically it is undefined for n/2 (n is a whole number)"),
                                                annotation_type:
                                                    AnnotationType::Note,}])
            }
        }
        pub struct NoiseDerivative(Span);
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::clone::Clone for NoiseDerivative {
            #[inline]
            fn clone(&self) -> NoiseDerivative {
                match *self {
                    NoiseDerivative(ref __self_0_0) =>
                    NoiseDerivative(::core::clone::Clone::clone(&(*__self_0_0))),
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::fmt::Debug for NoiseDerivative {
            fn fmt(&self, f: &mut ::core::fmt::Formatter)
             -> ::core::fmt::Result {
                match *self {
                    NoiseDerivative(ref __self_0_0) => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                     "NoiseDerivative");
                        let _ =
                            ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                           &&(*__self_0_0));
                        ::core::fmt::DebugTuple::finish(debug_trait_builder)
                    }
                }
            }
        }
        impl Error for NoiseDerivative { }
        impl Display for NoiseDerivative {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                f.write_str("Derivative of rounding required")
            }
        }
        impl LintDiagnostic for NoiseDerivative {
            fn lint(&self) -> Lint { builtin::noise_derivative }
            fn slices(&self, main_annotation_type: AnnotationType)
             -> Vec<DiagnosticSlice> {
                <[_]>::into_vec(box
                                    [DiagnosticSlice{slice_span:
                                                         self.0.data(),
                                                     messages:
                                                         <[_]>::into_vec(box
                                                                             [(main_annotation_type,
                                                                               Text::const_str("Required while deriving this"),
                                                                               self.0.data())]),
                                                     fold: false,}])
            }
            #[inline]
            fn footer(&self) -> Vec<FooterItem> {
                <[_]>::into_vec(box
                                    [FooterItem{id: None,
                                                label:
                                                    Text::const_str("Derivative is assumed to be always 0. However technically it is undefined for n/2 (n is a whole number)"),
                                                annotation_type:
                                                    AnnotationType::Note,}])
            }
        }
    }
    mod rvalue {
        use super::error::Error::DerivativeNotDefined;
        use super::error::UndefinedDerivative;
        use super::lints::RoundingDerivativeNotFullyDefined;
        use super::{operand_to_derivative, AutoDiff};
        use crate::osdi_types::ConstVal::Scalar;
        use crate::osdi_types::SimpleConstVal::Real;
        use crate::BinOp::{Divide, Minus, Multiply, Plus};
        use crate::ComparisonOp::Equal;
        use crate::OperandData::Constant;
        use crate::{fold_rvalue, BinOp, COperand, CallArg, CallTypeDerivative,
                    CfgFunctions, Derivative, Local, Operand, OperandData,
                    RValue, RValueFold, StmntKind, SyntaxCtx, Type};
        use enum_map::{Enum, EnumMap};
        use data_structures::index_vec::IndexSlice;
        use diagnostics::lints::Linter;
        use ir::Math1::{Cos, CosH, Ln, Sin, SinH, Sqrt};
        use ir::Math2::Pow;
        use ir::UnaryOperator::ArithmeticNegate;
        use ir::{Math1, Spanned, UnaryOperator, Unknown};
        use session::sourcemap::span::DUMMY_SP;
        use session::sourcemap::Span;
        pub enum OuterDerivativeCacheSlot { Lhs, Rhs, }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::fmt::Debug for OuterDerivativeCacheSlot {
            fn fmt(&self, f: &mut ::core::fmt::Formatter)
             -> ::core::fmt::Result {
                match (&*self,) {
                    (&OuterDerivativeCacheSlot::Lhs,) => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                     "Lhs");
                        ::core::fmt::DebugTuple::finish(debug_trait_builder)
                    }
                    (&OuterDerivativeCacheSlot::Rhs,) => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                     "Rhs");
                        ::core::fmt::DebugTuple::finish(debug_trait_builder)
                    }
                }
            }
        }
        impl ::core::marker::StructuralEq for OuterDerivativeCacheSlot { }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::cmp::Eq for OuterDerivativeCacheSlot {
            #[inline]
            #[doc(hidden)]
            #[no_coverage]
            fn assert_receiver_is_total_eq(&self) -> () { { } }
        }
        impl ::core::marker::StructuralPartialEq for OuterDerivativeCacheSlot
         {
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::cmp::PartialEq for OuterDerivativeCacheSlot {
            #[inline]
            fn eq(&self, other: &OuterDerivativeCacheSlot) -> bool {
                {
                    let __self_vi =
                        ::core::intrinsics::discriminant_value(&*self);
                    let __arg_1_vi =
                        ::core::intrinsics::discriminant_value(&*other);
                    if true && __self_vi == __arg_1_vi {
                        match (&*self, &*other) { _ => true, }
                    } else { false }
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::marker::Copy for OuterDerivativeCacheSlot { }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::clone::Clone for OuterDerivativeCacheSlot {
            #[inline]
            fn clone(&self) -> OuterDerivativeCacheSlot { { *self } }
        }
        #[automatically_derived]
        impl <V> ::enum_map::Enum<V> for OuterDerivativeCacheSlot {
            type Array = [V; 2usize];
            #[inline]
            fn from_usize(value: usize) -> Self {
                match value {
                    0usize => Self::Lhs,
                    1usize => Self::Rhs,
                    _ => {
                        ::core::panicking::panic("internal error: entered unreachable code")
                    }
                }
            }
            #[inline]
            fn into_usize(self) -> usize { self as usize }
        }
        impl OuterDerivativeCacheSlot {
            pub const SINGLE: Self = Self::Lhs;
        }
        impl <'lt, C: CfgFunctions, MC: CfgFunctions> AutoDiff<'lt, C, MC> {
            pub(crate) fn rvalue_derivative(&mut self, lhs: Local,
                                            rhs: &RValue<C>, unknown: Unknown,
                                            origin: SyntaxCtx,
                                            cache:
                                                &mut EnumMap<OuterDerivativeCacheSlot,
                                                             Option<COperand<C>>>)
             -> RValue<C> {
                let ty = self.cfg.locals[lhs].ty;
                let mut fold =
                    RValueAutoDiff{original_local: lhs,
                                   origin,
                                   ad: self,
                                   unknown,
                                   outer_derivative_cache: cache,};
                let res = fold_rvalue(&mut fold, rhs, ty);
                res.unwrap_or_else(||
                                       RValue::Use(Operand::new(Constant(0.0.into()),
                                                                DUMMY_SP)))
            }
        }
        enum OneAndSquareKind {

            /// 1 + f^2
            Positive,

            /// 1 - f^2
            OneMinusSquared,

            /// f^2 - 1
            SquaredMinusOne,
        }
        pub struct RValueAutoDiff<'lt, 'adlt, C: CfgFunctions,
                                  MC: CfgFunctions> {
            /// Local that the RValue this is being derived will be saved to
            pub original_local: Local,
            origin: SyntaxCtx,
            pub ad: &'lt mut AutoDiff<'adlt, C, MC>,
            pub unknown: Unknown,
            outer_derivative_cache: &'lt mut EnumMap<OuterDerivativeCacheSlot,
                                                     Option<COperand<C>>>,
        }
        impl <'lt, 'adlt, C: CfgFunctions, MC: CfgFunctions> RValueFold<C> for
         RValueAutoDiff<'lt, 'adlt, C, MC> {
            type T = Option<RValue<C>>;
            fn fold_cmplx_arith_negate(&mut self, _op: Span,
                                       _arg: &COperand<C>) -> Self::T {
                ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["not implemented: "],
                                                                            &match (&::core::fmt::Arguments::new_v1(&["Complex derivatives"],
                                                                                                                    &match ()
                                                                                                                         {
                                                                                                                         ()
                                                                                                                         =>
                                                                                                                         [],
                                                                                                                     }),)
                                                                                 {
                                                                                 (arg0,)
                                                                                 =>
                                                                                 [::core::fmt::ArgumentV1::new(arg0,
                                                                                                               ::core::fmt::Display::fmt)],
                                                                             }))
            }
            #[inline]
            fn fold_real_arith_negate(&mut self, op: Span, arg: &COperand<C>)
             -> Self::T {
                let arg_derivative = self.derivative(arg).into_option()?;
                let arg_derivative = Operand::new(arg_derivative, arg.span);
                Some(RValue::UnaryOperation(Spanned::new(UnaryOperator::ArithmeticNegate,
                                                         op), arg_derivative))
            }
            #[inline]
            fn fold_bit_negate(&mut self, op: Span, _arg: &COperand<C>)
             -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::BitWiseOp,
                                                        op));
                None
            }
            #[inline]
            fn fold_int_arith_negate(&mut self, op: Span, arg: &COperand<C>)
             -> Self::T {
                self.fold_real_arith_negate(op, &arg)
            }
            #[inline]
            fn fold_logic_negate(&mut self, op: Span, _arg: &COperand<C>)
             -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::LogicOp,
                                                        op));
                None
            }
            fn fold_cmplx_add(&mut self, _op: Span, _lhs: &COperand<C>,
                              _rhs: &COperand<C>) -> Self::T {
                ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["not implemented: "],
                                                                            &match (&::core::fmt::Arguments::new_v1(&["Complex derivatives"],
                                                                                                                    &match ()
                                                                                                                         {
                                                                                                                         ()
                                                                                                                         =>
                                                                                                                         [],
                                                                                                                     }),)
                                                                                 {
                                                                                 (arg0,)
                                                                                 =>
                                                                                 [::core::fmt::ArgumentV1::new(arg0,
                                                                                                               ::core::fmt::Display::fmt)],
                                                                             }))
            }
            fn fold_cmplx_sub(&mut self, _op: Span, _lhs: &COperand<C>,
                              _rhs: &COperand<C>) -> Self::T {
                ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["not implemented: "],
                                                                            &match (&::core::fmt::Arguments::new_v1(&["Complex derivatives"],
                                                                                                                    &match ()
                                                                                                                         {
                                                                                                                         ()
                                                                                                                         =>
                                                                                                                         [],
                                                                                                                     }),)
                                                                                 {
                                                                                 (arg0,)
                                                                                 =>
                                                                                 [::core::fmt::ArgumentV1::new(arg0,
                                                                                                               ::core::fmt::Display::fmt)],
                                                                             }))
            }
            fn fold_cmplx_mul(&mut self, _op: Span, _lhs: &COperand<C>,
                              _rhs: &COperand<C>) -> Self::T {
                ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["not implemented: "],
                                                                            &match (&::core::fmt::Arguments::new_v1(&["Complex derivatives"],
                                                                                                                    &match ()
                                                                                                                         {
                                                                                                                         ()
                                                                                                                         =>
                                                                                                                         [],
                                                                                                                     }),)
                                                                                 {
                                                                                 (arg0,)
                                                                                 =>
                                                                                 [::core::fmt::ArgumentV1::new(arg0,
                                                                                                               ::core::fmt::Display::fmt)],
                                                                             }))
            }
            fn fold_cmplx_div(&mut self, _op: Span, _lhs: &COperand<C>,
                              _rhs: &COperand<C>) -> Self::T {
                ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["not implemented: "],
                                                                            &match (&::core::fmt::Arguments::new_v1(&["Complex derivatives"],
                                                                                                                    &match ()
                                                                                                                         {
                                                                                                                         ()
                                                                                                                         =>
                                                                                                                         [],
                                                                                                                     }),)
                                                                                 {
                                                                                 (arg0,)
                                                                                 =>
                                                                                 [::core::fmt::ArgumentV1::new(arg0,
                                                                                                               ::core::fmt::Display::fmt)],
                                                                             }))
            }
            #[inline]
            fn fold_real_add(&mut self, op: Span, lhs: &COperand<C>,
                             rhs: &COperand<C>) -> Self::T {
                let lhs_derivative = self.derivative(lhs);
                let rhs_derivative = self.derivative(rhs);
                Self::derivative_sum(Spanned::new(lhs_derivative, lhs.span),
                                     Spanned::new(rhs_derivative, rhs.span),
                                     op)
            }
            #[inline]
            fn fold_real_sub(&mut self, op: Span, lhs: &COperand<C>,
                             rhs: &COperand<C>) -> Self::T {
                let lhs_derivative = self.derivative(lhs);
                let rhs_derivative = self.derivative(rhs);
                Self::derivative_sub(Spanned::new(lhs_derivative, lhs.span),
                                     Spanned::new(rhs_derivative, rhs.span),
                                     op)
            }
            #[inline]
            fn fold_real_mul(&mut self, op: Span, lhs: &COperand<C>,
                             rhs: &COperand<C>) -> Self::T {
                let lhs_derivative = self.derivative(lhs);
                let rhs_derivative = self.derivative(rhs);
                self.derivative_mul(lhs, rhs, lhs_derivative, rhs_derivative,
                                    op)
            }
            #[inline]
            fn fold_real_div(&mut self, op: Span, lhs: &COperand<C>,
                             rhs: &COperand<C>) -> Self::T {
                let lhs_derivative = self.derivative(lhs);
                let rhs_derivative = self.derivative(rhs);
                self.derivative_div(lhs, rhs, lhs_derivative, rhs_derivative,
                                    op)
            }
            #[inline]
            fn fold_real_rem(&mut self, op: Span, _lhs: &COperand<C>,
                             _rhs: &COperand<C>) -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::Modulus,
                                                        op));
                None
            }
            #[inline]
            fn fold_int_add(&mut self, op: Span, lhs: &COperand<C>,
                            rhs: &COperand<C>) -> Self::T {
                self.fold_real_add(op, &lhs, &rhs)
            }
            #[inline]
            fn fold_int_sub(&mut self, op: Span, lhs: &COperand<C>,
                            rhs: &COperand<C>) -> Self::T {
                self.fold_real_sub(op, &lhs, &rhs)
            }
            #[inline]
            fn fold_int_mul(&mut self, op: Span, lhs: &COperand<C>,
                            rhs: &COperand<C>) -> Self::T {
                let lhs_derivative = self.derivative(lhs);
                let rhs_derivative = self.derivative(rhs);
                let new_lhs;
                let new_rhs;
                let rhs =
                    if lhs_derivative != Derivative::Zero {
                        new_rhs =
                            self.gen_temporary(RValue::Cast(rhs.clone()), op);
                        &new_rhs
                    } else { rhs };
                let lhs =
                    if rhs_derivative != Derivative::Zero {
                        new_lhs =
                            self.gen_temporary(RValue::Cast(lhs.clone()), op);
                        &new_lhs
                    } else { lhs };
                self.derivative_mul(lhs, rhs, lhs_derivative, rhs_derivative,
                                    op)
            }
            #[inline]
            fn fold_int_div(&mut self, op: Span, lhs: &COperand<C>,
                            rhs: &COperand<C>) -> Self::T {
                let lhs_derivative = self.derivative(&lhs);
                let rhs_derivative = self.derivative(&rhs);
                let new_lhs;
                let new_rhs;
                let (lhs, rhs) =
                    if lhs_derivative != Derivative::Zero ||
                           rhs_derivative != Derivative::Zero {
                        new_rhs =
                            self.gen_temporary(RValue::Cast(rhs.clone()), op);
                        let lhs =
                            if rhs_derivative != Derivative::Zero {
                                new_lhs =
                                    self.gen_temporary(RValue::Cast(lhs.clone()),
                                                       op);
                                &new_lhs
                            } else { lhs };
                        (lhs, &new_rhs)
                    } else { (lhs, rhs) };
                self.derivative_div(lhs, rhs, lhs_derivative, rhs_derivative,
                                    op)
            }
            #[inline]
            fn fold_int_rem(&mut self, op: Span, _lhs: &COperand<C>,
                            _rhs: &COperand<C>) -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::Modulus,
                                                        op));
                None
            }
            #[inline]
            fn fold_shiftl(&mut self, op: Span, lhs: &COperand<C>,
                           rhs: &COperand<C>) -> Self::T {
                let dlhs =
                    self.chain_rule(&rhs,
                                    |fold|
                                        {
                                            let two =
                                                Self::gen_constant(2.0, op);
                                            let rhs =
                                                fold.gen_temporary(RValue::Cast(rhs.clone()),
                                                                   op);
                                            RValue::Math2(Spanned::new(Pow,
                                                                       op),
                                                          two, rhs)
                                        }, op, OuterDerivativeCacheSlot::Lhs);
                let drhs =
                    self.chain_rule(&rhs,
                                    |fold|
                                        {
                                            let lhs =
                                                fold.gen_temporary(RValue::Cast(lhs.clone()),
                                                                   op);
                                            let ln2 =
                                                Self::gen_constant(std::f64::consts::LN_2,
                                                                   op);
                                            let product =
                                                RValue::BinaryOperation(Spanned::new(Multiply,
                                                                                     op),
                                                                        lhs,
                                                                        ln2);
                                            let product =
                                                fold.gen_temporary(product,
                                                                   op);
                                            let original =
                                                fold.original_operand(op);
                                            let original =
                                                fold.gen_temporary(RValue::Cast(original),
                                                                   op);
                                            RValue::BinaryOperation(Spanned::new(Multiply,
                                                                                 op),
                                                                    product,
                                                                    original)
                                        }, op, OuterDerivativeCacheSlot::Rhs);
                match (dlhs, drhs) {
                    (None, None) => None,
                    (Some(val), None) | (None, Some(val)) => Some(val),
                    (Some(dlhs), Some(drhs)) => {
                        let dlhs = self.gen_temporary(dlhs, op);
                        let drhs = self.gen_temporary(drhs, op);
                        Some(RValue::BinaryOperation(Spanned::new(Multiply,
                                                                  op), dlhs,
                                                     drhs))
                    }
                }
            }
            #[inline]
            fn fold_shiftr(&mut self, op: Span, lhs: &COperand<C>,
                           rhs: &COperand<C>) -> Self::T {
                let dlhs =
                    self.chain_rule(&rhs,
                                    |fold|
                                        {
                                            let two =
                                                Self::gen_constant(2.0, op);
                                            let rhs =
                                                fold.gen_temporary(RValue::UnaryOperation(Spanned::new(ArithmeticNegate,
                                                                                                       op),
                                                                                          rhs.clone()),
                                                                   op);
                                            let rhs =
                                                fold.gen_temporary(RValue::Cast(rhs),
                                                                   op);
                                            RValue::Math2(Spanned::new(Pow,
                                                                       op),
                                                          two, rhs)
                                        }, op, OuterDerivativeCacheSlot::Lhs);
                let drhs =
                    self.chain_rule(&rhs,
                                    |fold|
                                        {
                                            let lhs =
                                                fold.gen_temporary(RValue::Cast(lhs.clone()),
                                                                   op);
                                            let ln2 =
                                                Self::gen_constant(-std::f64::consts::LN_2,
                                                                   op);
                                            let product =
                                                RValue::BinaryOperation(Spanned::new(Multiply,
                                                                                     op),
                                                                        lhs,
                                                                        ln2);
                                            let product =
                                                fold.gen_temporary(product,
                                                                   op);
                                            let original =
                                                fold.original_operand(op);
                                            let original =
                                                fold.gen_temporary(RValue::Cast(original),
                                                                   op);
                                            RValue::BinaryOperation(Spanned::new(Multiply,
                                                                                 op),
                                                                    product,
                                                                    original)
                                        }, op, OuterDerivativeCacheSlot::Rhs);
                match (dlhs, drhs) {
                    (None, None) => None,
                    (Some(val), None) | (None, Some(val)) => Some(val),
                    (Some(dlhs), Some(drhs)) => {
                        let dlhs = self.gen_temporary(dlhs, op);
                        let drhs = self.gen_temporary(drhs, op);
                        Some(RValue::BinaryOperation(Spanned::new(Multiply,
                                                                  op), dlhs,
                                                     drhs))
                    }
                }
            }
            #[inline]
            fn fold_lt_real(&mut self, op: Span, _lhs: &COperand<C>,
                            _rhs: &COperand<C>) -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::Comparison,
                                                        op));
                None
            }
            #[inline]
            fn fold_le_real(&mut self, op: Span, _lhs: &COperand<C>,
                            _rhs: &COperand<C>) -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::Comparison,
                                                        op));
                None
            }
            #[inline]
            fn fold_gt_real(&mut self, op: Span, _lhs: &COperand<C>,
                            _rhs: &COperand<C>) -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::Comparison,
                                                        op));
                None
            }
            #[inline]
            fn fold_ge_real(&mut self, op: Span, _lhs: &COperand<C>,
                            _rhs: &COperand<C>) -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::Comparison,
                                                        op));
                None
            }
            #[inline]
            fn fold_lt_int(&mut self, op: Span, _lhs: &COperand<C>,
                           _rhs: &COperand<C>) -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::Comparison,
                                                        op));
                None
            }
            #[inline]
            fn fold_le_int(&mut self, op: Span, _lhs: &COperand<C>,
                           _rhs: &COperand<C>) -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::Comparison,
                                                        op));
                None
            }
            #[inline]
            fn fold_gt_int(&mut self, op: Span, _lhs: &COperand<C>,
                           _rhs: &COperand<C>) -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::Comparison,
                                                        op));
                None
            }
            #[inline]
            fn fold_ge_int(&mut self, op: Span, _lhs: &COperand<C>,
                           _rhs: &COperand<C>) -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::Comparison,
                                                        op));
                None
            }
            #[inline]
            fn fold_eq(&mut self, op: Span, _lhs: &COperand<C>,
                       _rhs: &COperand<C>, _ty: Type) -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::Comparison,
                                                        op));
                None
            }
            #[inline]
            fn fold_ne(&mut self, op: Span, _lhs: &COperand<C>,
                       _rhs: &COperand<C>, _ty: Type) -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::Comparison,
                                                        op));
                None
            }
            #[inline]
            fn fold_xor(&mut self, op: Span, _lhs: &COperand<C>,
                        _rhs: &COperand<C>) -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::BitWiseOp,
                                                        op));
                None
            }
            #[inline]
            fn fold_nxor(&mut self, op: Span, _lhs: &COperand<C>,
                         _rhs: &COperand<C>) -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::BitWiseOp,
                                                        op));
                None
            }
            #[inline]
            fn fold_and(&mut self, op: Span, _lhs: &COperand<C>,
                        _rhs: &COperand<C>) -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::BitWiseOp,
                                                        op));
                None
            }
            #[inline]
            fn fold_or(&mut self, op: Span, _lhs: &COperand<C>,
                       _rhs: &COperand<C>) -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::BitWiseOp,
                                                        op));
                None
            }
            fn fold_bool_xor(&mut self, op: Span, _lhs: &COperand<C>,
                             _rhs: &COperand<C>) -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::BitWiseOp,
                                                        op));
                None
            }
            fn fold_bool_nxor(&mut self, op: Span, _lhs: &COperand<C>,
                              _rhs: &COperand<C>) -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::BitWiseOp,
                                                        op));
                None
            }
            fn fold_bool_and(&mut self, op: Span, _lhs: &COperand<C>,
                             _rhs: &COperand<C>) -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::BitWiseOp,
                                                        op));
                None
            }
            fn fold_bool_or(&mut self, op: Span, _lhs: &COperand<C>,
                            _rhs: &COperand<C>) -> Self::T {
                self.ad.errors.add(DerivativeNotDefined(UndefinedDerivative::BitWiseOp,
                                                        op));
                None
            }
            fn fold_exp(&mut self, span: Span, arg: &COperand<C>,
                        _limit: bool) -> Self::T {
                let arg_derivative = self.derivative(arg).into_option()?;
                let arg_derivative = Operand::new(arg_derivative, arg.span);
                let original = self.original_operand(span);
                Some(RValue::BinaryOperation(Spanned::new(Multiply, span),
                                             arg_derivative, original))
            }
            fn fold_ln(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
                let arg_derivative = self.derivative(arg).into_option()?;
                let arg_derivative = Operand::new(arg_derivative, arg.span);
                Some(RValue::BinaryOperation(Spanned::new(Divide, span),
                                             arg_derivative, arg.clone()))
            }
            fn fold_log(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
                self.chain_rule(arg,
                                |_|
                                    {
                                        let top =
                                            Self::gen_constant(std::f64::consts::LOG10_E,
                                                               span);
                                        RValue::BinaryOperation(Spanned::new(Divide,
                                                                             span),
                                                                top,
                                                                arg.clone())
                                    }, span, OuterDerivativeCacheSlot::SINGLE)
            }
            fn fold_clog2(&mut self, span: Span, _arg: &COperand<C>)
             -> Self::T {
                Linter::dispatch_late(Box::new(RoundingDerivativeNotFullyDefined(span)),
                                      self.origin);
                None
            }
            fn fold_sqrt(&mut self, span: Span, arg: &COperand<C>)
             -> Self::T {
                self.chain_rule(arg,
                                |fold|
                                    {
                                        let top =
                                            Self::gen_constant(0.5, span);
                                        let den = fold.original_operand(span);
                                        RValue::BinaryOperation(Spanned::new(Divide,
                                                                             span),
                                                                top, den)
                                    }, span, OuterDerivativeCacheSlot::SINGLE)
            }
            fn fold_cmplx_abs(&mut self, _span: Span, _arg: &COperand<C>)
             -> Self::T {
                ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["not implemented: "],
                                                                            &match (&::core::fmt::Arguments::new_v1(&["Complex derivatives"],
                                                                                                                    &match ()
                                                                                                                         {
                                                                                                                         ()
                                                                                                                         =>
                                                                                                                         [],
                                                                                                                     }),)
                                                                                 {
                                                                                 (arg0,)
                                                                                 =>
                                                                                 [::core::fmt::ArgumentV1::new(arg0,
                                                                                                               ::core::fmt::Display::fmt)],
                                                                             }))
            }
            fn fold_real_abs(&mut self, span: Span, arg: &COperand<C>)
             -> Self::T {
                self.derivative_abs(span, arg, Type::REAL)
            }
            fn fold_int_abs(&mut self, span: Span, arg: &COperand<C>)
             -> Self::T {
                self.derivative_abs(span, arg, Type::INT)
            }
            fn fold_ceil(&mut self, span: Span, _arg: &COperand<C>)
             -> Self::T {
                Linter::dispatch_late(Box::new(RoundingDerivativeNotFullyDefined(span)),
                                      self.origin);
                None
            }
            fn fold_floor(&mut self, span: Span, _arg: &COperand<C>)
             -> Self::T {
                Linter::dispatch_late(Box::new(RoundingDerivativeNotFullyDefined(span)),
                                      self.origin);
                None
            }
            fn fold_sin(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
                self.chain_rule(arg,
                                |_|
                                    RValue::Math1(Spanned::new(Cos, span),
                                                  arg.clone()), span,
                                OuterDerivativeCacheSlot::SINGLE)
            }
            fn fold_cos(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
                self.chain_rule(arg,
                                |fold|
                                    {
                                        let sin =
                                            fold.gen_single_arg_math(Sin,
                                                                     arg.clone(),
                                                                     span);
                                        RValue::UnaryOperation(Spanned::new(ArithmeticNegate,
                                                                            span),
                                                               sin)
                                    }, span, OuterDerivativeCacheSlot::SINGLE)
            }
            fn fold_tan(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
                self.chain_rule(arg,
                                |fold|
                                    {
                                        let original =
                                            fold.original_operand(span);
                                        fold.gen_one_and_square(OneAndSquareKind::Positive,
                                                                original,
                                                                span)
                                    }, span, OuterDerivativeCacheSlot::SINGLE)
            }
            fn fold_sinh(&mut self, span: Span, arg: &COperand<C>)
             -> Self::T {
                self.chain_rule(arg,
                                |_|
                                    RValue::Math1(Spanned::new(CosH, span),
                                                  arg.clone()), span,
                                OuterDerivativeCacheSlot::SINGLE)
            }
            fn fold_cosh(&mut self, span: Span, arg: &COperand<C>)
             -> Self::T {
                self.chain_rule(arg,
                                |_|
                                    RValue::Math1(Spanned::new(SinH, span),
                                                  arg.clone()), span,
                                OuterDerivativeCacheSlot::SINGLE)
            }
            fn fold_tanh(&mut self, span: Span, arg: &COperand<C>)
             -> Self::T {
                self.chain_rule(arg,
                                |fold|
                                    {
                                        let original =
                                            fold.original_operand(span);
                                        fold.gen_one_and_square(OneAndSquareKind::OneMinusSquared,
                                                                original,
                                                                span)
                                    }, span, OuterDerivativeCacheSlot::SINGLE)
            }
            fn fold_asin(&mut self, span: Span, arg: &COperand<C>)
             -> Self::T {
                let arg_derivative = self.derivative(arg).into_option()?;
                let arg_derivative = Operand::new(arg_derivative, arg.span);
                let sqrt_arg =
                    self.gen_one_and_square(OneAndSquareKind::OneMinusSquared,
                                            arg.clone(), span);
                let sqrt_arg = self.gen_temporary(sqrt_arg, span);
                let den = self.gen_single_arg_math(Sqrt, sqrt_arg, span);
                Some(RValue::BinaryOperation(Spanned::new(Divide, span),
                                             arg_derivative, den))
            }
            fn fold_acos(&mut self, span: Span, arg: &COperand<C>)
             -> Self::T {
                let dasin = self.fold_asin(span, arg)?;
                let dasin = self.gen_temporary(dasin, span);
                Some(RValue::UnaryOperation(Spanned::new(ArithmeticNegate,
                                                         span), dasin))
            }
            fn fold_atan(&mut self, span: Span, arg: &COperand<C>)
             -> Self::T {
                let arg_derivative = self.derivative(arg).into_option()?;
                let arg_derivative = Operand::new(arg_derivative, arg.span);
                let den =
                    self.gen_one_and_square(OneAndSquareKind::Positive,
                                            arg.clone(), span);
                let den = self.gen_temporary(den, span);
                Some(RValue::BinaryOperation(Spanned::new(Divide, span),
                                             arg_derivative, den))
            }
            fn fold_asinh(&mut self, span: Span, arg: &COperand<C>)
             -> Self::T {
                let arg_derivative = self.derivative(arg).into_option()?;
                let arg_derivative = Operand::new(arg_derivative, arg.span);
                let sqrt_arg =
                    self.gen_one_and_square(OneAndSquareKind::Positive,
                                            arg.clone(), span);
                let sqrt_arg = self.gen_temporary(sqrt_arg, span);
                let den = self.gen_single_arg_math(Sqrt, sqrt_arg, span);
                Some(RValue::BinaryOperation(Spanned::new(Divide, span),
                                             arg_derivative, den))
            }
            fn fold_acosh(&mut self, span: Span, arg: &COperand<C>)
             -> Self::T {
                let arg_derivative = self.derivative(arg).into_option()?;
                let arg_derivative = Operand::new(arg_derivative, arg.span);
                let sqrt_arg =
                    self.gen_one_and_square(OneAndSquareKind::SquaredMinusOne,
                                            arg.clone(), span);
                let sqrt_arg = self.gen_temporary(sqrt_arg, span);
                let den = self.gen_single_arg_math(Sqrt, sqrt_arg, span);
                Some(RValue::BinaryOperation(Spanned::new(Divide, span),
                                             arg_derivative, den))
            }
            fn fold_atanh(&mut self, span: Span, arg: &COperand<C>)
             -> Self::T {
                let arg_derivative = self.derivative(arg).into_option()?;
                let arg_derivative = Operand::new(arg_derivative, arg.span);
                let den =
                    self.gen_one_and_square(OneAndSquareKind::OneMinusSquared,
                                            arg.clone(), span);
                let den = self.gen_temporary(den, span);
                Some(RValue::BinaryOperation(Spanned::new(Divide, span),
                                             arg_derivative, den))
            }
            fn fold_pow(&mut self, span: Span, lhs: &COperand<C>,
                        rhs: &COperand<C>) -> Self::T {
                let sum1 =
                    self.chain_rule(&lhs,
                                    |_|
                                        RValue::BinaryOperation(Spanned::new(Divide,
                                                                             span),
                                                                rhs.clone(),
                                                                lhs.clone()),
                                    span, OuterDerivativeCacheSlot::Lhs);
                let sum2 =
                    self.chain_rule(&rhs,
                                    |_|
                                        RValue::Math1(Spanned::new(Ln, span),
                                                      lhs.clone()), span,
                                    OuterDerivativeCacheSlot::Rhs);
                let sum =
                    match (sum1, sum2) {
                        (None, None) => return None,
                        (Some(val), None) | (None, Some(val)) => val,
                        (Some(lhs), Some(rhs)) => {
                            let lhs = self.gen_temporary(lhs, span);
                            let rhs = self.gen_temporary(rhs, span);
                            RValue::BinaryOperation(Spanned::new(Plus, span),
                                                    lhs, rhs)
                        }
                    };
                let sum = self.gen_temporary(sum, span);
                let original = self.original_operand(span);
                Some(RValue::BinaryOperation(Spanned::new(Multiply, span),
                                             sum, original))
            }
            fn fold_hypot(&mut self, span: Span, arg1: &COperand<C>,
                          arg2: &COperand<C>) -> Self::T {
                let darg1 = self.derivative(arg1);
                let darg2 = self.derivative(arg2);
                let num =
                    self.derivative_mul(arg1, arg2, darg2, darg1, span)?;
                let num = self.gen_temporary(num, span);
                let original = self.original_operand(span);
                Some(RValue::BinaryOperation(Spanned::new(Divide, span), num,
                                             original))
            }
            #[inline]
            fn fold_real_min(&mut self, span: Span, arg1: &COperand<C>,
                             arg2: &COperand<C>) -> Self::T {
                self.minmax_derivative(span, arg1, arg2, Type::REAL)
            }
            #[inline]
            fn fold_int_min(&mut self, span: Span, arg1: &COperand<C>,
                            arg2: &COperand<C>) -> Self::T {
                self.minmax_derivative(span, arg1, arg2, Type::INT)
            }
            #[inline]
            fn fold_real_max(&mut self, span: Span, arg1: &COperand<C>,
                             arg2: &COperand<C>) -> Self::T {
                self.minmax_derivative(span, arg1, arg2, Type::REAL)
            }
            #[inline]
            fn fold_int_max(&mut self, span: Span, arg1: &COperand<C>,
                            arg2: &COperand<C>) -> Self::T {
                self.minmax_derivative(span, arg1, arg2, Type::INT)
            }
            fn fold_atan2(&mut self, span: Span, arg1: &COperand<C>,
                          arg2: &COperand<C>) -> Self::T {
                let darg1 = self.derivative(arg1);
                let darg2 = self.derivative(arg2);
                let darg2 =
                    match darg2 {
                        Derivative::Zero => Derivative::Zero,
                        Derivative::One =>
                        Derivative::Operand(Constant((-1.0).into())),
                        Derivative::Operand(op) => {
                            let operand =
                                self.gen_temporary(RValue::UnaryOperation(Spanned::new(ArithmeticNegate,
                                                                                       span),
                                                                          Spanned::new(op,
                                                                                       arg2.span)),
                                                   span);
                            Derivative::Operand(operand.contents)
                        }
                    };
                let num =
                    self.derivative_mul(arg1, arg1, darg1, darg2, span)?;
                let num = self.gen_temporary(num, span);
                let sum1 =
                    self.gen_binop(Multiply, arg1.clone(), arg1.clone(),
                                   span);
                let sum2 =
                    self.gen_binop(Multiply, arg2.clone(), arg2.clone(),
                                   span);
                let den = self.gen_binop(Plus, sum1, sum2, span);
                Some(RValue::BinaryOperation(Spanned::new(Divide, span), num,
                                             den))
            }
            fn fold_cast(&mut self, arg: &COperand<C>, ty: Type) -> Self::T {
                if ty == Type::INT {
                    Linter::dispatch_late(Box::new(RoundingDerivativeNotFullyDefined(arg.span)),
                                          self.origin);
                    None
                } else {
                    let arg_derivative = self.derivative(arg).into_option()?;
                    let arg_derivative =
                        Operand::new(arg_derivative, arg.span);
                    Some(RValue::Use(arg_derivative))
                }
            }
            fn fold_use(&mut self, arg: &COperand<C>) -> Self::T {
                let darg = self.derivative(arg).into_operand();
                let darg = Operand::new(darg, arg.span);
                Some(RValue::Use(darg))
            }
            fn fold_select(&mut self, cond: &COperand<C>,
                           true_op: &COperand<C>, false_op: &COperand<C>)
             -> Self::T {
                let dtrue_op = self.derivative(true_op);
                let dfalse_op = self.derivative(false_op);
                match (dtrue_op, dfalse_op) {
                    (Derivative::Zero, Derivative::Zero) => None,
                    (dtrue_op, dfalse_op) =>
                    Some(RValue::Select(cond.clone(),
                                        Operand::new(dtrue_op.into_operand(),
                                                     true_op.span),
                                        Operand::new(dfalse_op.into_operand(),
                                                     false_op.span))),
                }
            }
            fn fold_call(&mut self, call: &C,
                         args: &IndexSlice<CallArg, [COperand<C>]>,
                         span: Span) -> Self::T {
                call.derivative(args, self, span)
            }
            fn fold_array(&mut self, _args: &[COperand<C>], _span: Span,
                          _ty: Type) -> Self::T {
                {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                    &match (&"Array derivatives are not defined",)
                                                                                         {
                                                                                         (arg0,)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt)],
                                                                                     }))
                    }
                }
            }
        }
        impl <'lt, 'adlt, C: CfgFunctions, MC: CfgFunctions>
         RValueAutoDiff<'lt, 'adlt, C, MC> {
            fn minmax_derivative(&mut self, span: Span, arg1: &COperand<C>,
                                 arg2: &COperand<C>, ty: Type)
             -> Option<RValue<C>> {
                let darg1 = self.derivative(arg1);
                let darg2 = self.derivative(arg2);
                match (darg1, darg2) {
                    (Derivative::Zero, Derivative::Zero) => None,
                    (darg1, darg2) => {
                        let cond =
                            self.gen_logic_temporary(RValue::Comparison(Spanned::new(Equal,
                                                                                     span),
                                                                        self.original_operand(span),
                                                                        arg1.clone(),
                                                                        ty),
                                                     span);
                        Some(RValue::Select(cond,
                                            Operand::new(darg1.into_operand(),
                                                         arg1.span),
                                            Operand::new(darg2.into_operand(),
                                                         arg2.span)))
                    }
                }
            }
            fn derivative_abs(&mut self, span: Span, arg: &COperand<C>,
                              ty: Type) -> Option<RValue<C>> {
                let darg = self.derivative(arg).into_option()?;
                let darg = Operand::new(darg, span);
                let minus_darg =
                    self.gen_temporary(RValue::UnaryOperation(Spanned::new(ArithmeticNegate,
                                                                           span),
                                                              darg.clone()),
                                       span);
                let cond =
                    self.gen_logic_temporary(RValue::Comparison(Spanned::new(Equal,
                                                                             span),
                                                                self.original_operand(span),
                                                                arg.clone(),
                                                                ty), span);
                Some(RValue::Select(cond, darg, minus_darg))
            }
            fn derivative_sum(lhs_derivative: Spanned<CallTypeDerivative<C>>,
                              rhs_derivative: Spanned<CallTypeDerivative<C>>,
                              span: Span) -> Option<RValue<C>> {
                match (lhs_derivative.contents.into_option(),
                       rhs_derivative.contents.into_option()) {
                    (None, None) => None,
                    (None, Some(rhs)) => {
                        let rhs = Operand::new(rhs, rhs_derivative.span);
                        Some(RValue::Use(rhs))
                    }
                    (Some(lhs), None) => {
                        let lhs = Operand::new(lhs, lhs_derivative.span);
                        Some(RValue::Use(lhs))
                    }
                    (Some(lhs), Some(rhs)) => {
                        let lhs = Operand::new(lhs, lhs_derivative.span);
                        let rhs = Operand::new(rhs, rhs_derivative.span);
                        Some(RValue::BinaryOperation(Spanned::new(BinOp::Plus,
                                                                  span), lhs,
                                                     rhs))
                    }
                }
            }
            fn derivative_sub(lhs_derivative: Spanned<CallTypeDerivative<C>>,
                              rhs_derivative: Spanned<CallTypeDerivative<C>>,
                              span: Span) -> Option<RValue<C>> {
                match (lhs_derivative.contents.into_option(),
                       rhs_derivative.contents.into_option()) {
                    (None, None) => None,
                    (None, Some(rhs)) => {
                        let rhs = Operand::new(rhs, rhs_derivative.span);
                        Some(RValue::UnaryOperation(Spanned::new(UnaryOperator::ArithmeticNegate,
                                                                 span), rhs))
                    }
                    (Some(val), None) =>
                    Some(RValue::Use(Operand::new(val, span))),
                    (Some(lhs), Some(rhs)) => {
                        let lhs = Operand::new(lhs, lhs_derivative.span);
                        let rhs = Operand::new(rhs, rhs_derivative.span);
                        Some(RValue::BinaryOperation(Spanned::new(BinOp::Minus,
                                                                  span), lhs,
                                                     rhs))
                    }
                }
            }
            #[inline]
            fn derivative_mul(&mut self, lhs: &COperand<C>, rhs: &COperand<C>,
                              lhs_derivative: CallTypeDerivative<C>,
                              rhs_derivative: CallTypeDerivative<C>,
                              span: Span) -> Option<RValue<C>> {
                let sum1 =
                    match lhs_derivative {
                        Derivative::Zero => lhs_derivative,
                        Derivative::One =>
                        Derivative::Operand(rhs.contents.clone()),
                        Derivative::Operand(operand) => {
                            let operand =
                                self.gen_binop(BinOp::Multiply,
                                               Operand::new(operand,
                                                            lhs.span),
                                               rhs.clone(), span);
                            operand_to_derivative::<C>(operand)
                        }
                    };
                let sum2 =
                    match rhs_derivative {
                        Derivative::Zero => rhs_derivative,
                        Derivative::One =>
                        Derivative::Operand(lhs.contents.clone()),
                        Derivative::Operand(operand) => {
                            let operand =
                                self.gen_binop(BinOp::Multiply,
                                               Operand::new(operand,
                                                            rhs.span),
                                               lhs.clone(), span);
                            operand_to_derivative::<C>(operand)
                        }
                    };
                Self::derivative_sum(Spanned::new(sum1, lhs.span),
                                     Spanned::new(sum2, rhs.span), span)
            }
            fn derivative_div(&mut self, lhs: &COperand<C>, rhs: &COperand<C>,
                              lhs_derivative: CallTypeDerivative<C>,
                              rhs_derivative: CallTypeDerivative<C>,
                              span: Span) -> Option<RValue<C>> {
                let sum1 =
                    match lhs_derivative {
                        Derivative::Zero => Derivative::Zero,
                        Derivative::One => {
                            let invers =
                                if let Some(invers) =
                                       &self.outer_derivative_cache[OuterDerivativeCacheSlot::Lhs]
                                   {
                                    invers.clone()
                                } else {
                                    let invers =
                                        self.gen_binop(BinOp::Divide,
                                                       Operand::new(OperandData::Constant(Scalar(Real(1.0))),
                                                                    span),
                                                       rhs.clone(), span);
                                    self.outer_derivative_cache[OuterDerivativeCacheSlot::Lhs]
                                        = Some(invers.clone());
                                    invers
                                };
                            Derivative::Operand(invers.contents)
                        }
                        Derivative::Operand(ref operand) => {
                            let operand =
                                self.gen_binop(BinOp::Divide,
                                               Operand::new(operand.clone(),
                                                            lhs.span),
                                               rhs.clone(), span);
                            operand_to_derivative::<C>(operand)
                        }
                    };
                let sum2 =
                    if let Some(res) =
                           self.chain_rule_with_known_derivative(rhs_derivative,
                                                                 |fold|
                                                                     {
                                                                         let bottom =
                                                                             fold.gen_binop(BinOp::Multiply,
                                                                                            rhs.clone(),
                                                                                            rhs.clone(),
                                                                                            span);
                                                                         RValue::BinaryOperation(Spanned::new(BinOp::Divide,
                                                                                                              span),
                                                                                                 lhs.clone(),
                                                                                                 bottom)
                                                                     }, span,
                                                                 OuterDerivativeCacheSlot::Rhs)
                       {
                        Derivative::Operand(self.gen_temporary(res,
                                                               span).contents)
                    } else { Derivative::Zero };
                Self::derivative_sub(Spanned::new(sum1, lhs.span),
                                     Spanned::new(sum2, rhs.span), span)
            }
            fn gen_constant(val: f64, span: Span) -> COperand<C> {
                Operand::new(OperandData::Constant(val.into()), span)
            }
            fn gen_temporary(&mut self, rhs: RValue<C>, span: Span)
             -> COperand<C> {
                let local = self.ad.cfg.new_temporary(Type::REAL);
                let kind = StmntKind::Assignment(local, rhs);
                self.ad.forward_stmnts.push((kind, self.origin));
                Operand::new(OperandData::Copy(local), span)
            }
            fn gen_logic_temporary(&mut self, rhs: RValue<C>, span: Span)
             -> COperand<C> {
                let local = self.ad.cfg.new_temporary(Type::BOOL);
                let kind = StmntKind::Assignment(local, rhs);
                self.ad.forward_stmnts.push((kind, self.origin));
                Operand::new(OperandData::Copy(local), span)
            }
            fn gen_binop(&mut self, op: BinOp, lhs: COperand<C>,
                         rhs: COperand<C>, span: Span) -> COperand<C> {
                let rhs =
                    RValue::BinaryOperation(Spanned::new(op, span), lhs, rhs);
                self.gen_temporary(rhs, span)
            }
            fn gen_single_arg_math(&mut self, kind: Math1, arg: COperand<C>,
                                   span: Span) -> COperand<C> {
                let rhs = RValue::Math1(Spanned::new(kind, span), arg);
                self.gen_temporary(rhs, span)
            }
            pub fn chain_rule(&mut self, arg: &COperand<C>,
                              generate_outer:
                                  impl FnOnce(&mut Self) -> RValue<C>,
                              span: Span,
                              cache_slot: OuterDerivativeCacheSlot)
             -> Option<RValue<C>> {
                let derivative = self.derivative(arg);
                self.chain_rule_with_known_derivative(derivative,
                                                      generate_outer, span,
                                                      cache_slot)
            }
            fn chain_rule_with_known_derivative(&mut self,
                                                derivative:
                                                    CallTypeDerivative<C>,
                                                generate_outer:
                                                    impl FnOnce(&mut Self)
                                                    -> RValue<C>, span: Span,
                                                cache_slot:
                                                    OuterDerivativeCacheSlot)
             -> Option<RValue<C>> {
                match derivative {
                    Derivative::Zero => None,
                    Derivative::One => Some(generate_outer(self)),
                    Derivative::Operand(inner) => {
                        let outer =
                            if let Some(outer) =
                                   &self.outer_derivative_cache[cache_slot] {
                                outer.clone()
                            } else {
                                let outer = generate_outer(self);
                                let outer = self.gen_temporary(outer, span);
                                self.outer_derivative_cache[cache_slot] =
                                    Some(outer.clone());
                                outer
                            };
                        let inner = Operand::new(inner, span);
                        Some(RValue::BinaryOperation(Spanned::new(BinOp::Multiply,
                                                                  span),
                                                     inner, outer))
                    }
                }
            }
            /// Generate an expression whos form is determined by [`OnePlusSquareKind`]
            fn gen_one_and_square(&mut self, kind: OneAndSquareKind,
                                  arg: COperand<C>, op_span: Span)
             -> RValue<C> {
                let span = arg.span;
                let squared =
                    self.gen_binop(Multiply, arg.clone(), arg, span);
                match kind {
                    OneAndSquareKind::Positive =>
                    RValue::BinaryOperation(Spanned::new(Plus, op_span),
                                            Self::gen_constant(1.0, op_span),
                                            squared),
                    OneAndSquareKind::OneMinusSquared =>
                    RValue::BinaryOperation(Spanned::new(Minus, op_span),
                                            Self::gen_constant(1.0, op_span),
                                            squared),
                    OneAndSquareKind::SquaredMinusOne =>
                    RValue::BinaryOperation(Spanned::new(Minus, op_span),
                                            squared,
                                            Self::gen_constant(1.0, op_span)),
                }
            }
            /// Generates an operand that reads the local that the rvalue currently being folded writes to
            fn original_operand(&self, span: Span) -> COperand<C> {
                Operand::new(OperandData::Copy(self.original_local), span)
            }
            pub fn derivative(&mut self, operand: &COperand<C>)
             -> CallTypeDerivative<C> {
                self.ad.cfg.demand_operand_derivative_unchecked(self.ad.mir,
                                                                operand,
                                                                self.unknown)
            }
        }
    }
    pub use rvalue::RValueAutoDiff;
    impl <C: CfgFunctions> ControlFlowGraph<C> {
        pub fn generate_derivatives<MC: CfgFunctions>(&mut self,
                                                      mir: &Mir<MC>,
                                                      errors:
                                                          &mut MultiDiagnostic<Error>) {
            AutoDiff{cfg: self,
                     mir,
                     errors,
                     forward_stmnts: IndexVec::with_capacity(128),}.run()
        }
    }
    pub struct AutoDiff<'lt, C: CfgFunctions, MC: CfgFunctions> {
        cfg: &'lt mut ControlFlowGraph<C>,
        pub mir: &'lt Mir<MC>,
        errors: &'lt mut MultiDiagnostic<Error>,
        forward_stmnts: IndexVec<StatementId, Statement<C>>,
    }
    impl <'lt, C: CfgFunctions, MC: CfgFunctions> AutoDiff<'lt, C, MC> {
        fn append_assignment_and_derivatives(&mut self, lhs: Local,
                                             mut rhs: RValue<C>,
                                             sctx: SyntaxCtx) {
            let derivatives = self.cfg.derivatives.get(&lhs).cloned();
            if let Some(derivatives) = derivatives {
                if !derivatives.is_empty() {
                    let mut cache = EnumMap::default();
                    let new_value =
                        self.cfg.new_temporary(self.cfg.locals[lhs].ty);
                    let new_rhs =
                        RValue::Use(Operand::new(OperandData::Copy(new_value),
                                                 rhs.span()));
                    self.forward_stmnts.push((StmntKind::Assignment(new_value,
                                                                    rhs.clone()),
                                              sctx));
                    for (unkown, derivative_local) in derivatives {
                        let derivative_rhs =
                            self.rvalue_derivative(new_value, &rhs, unkown,
                                                   sctx, &mut cache);
                        self.append_assignment_and_derivatives(derivative_local,
                                                               derivative_rhs,
                                                               sctx)
                    }
                    rhs = new_rhs;
                }
            }
            self.forward_stmnts.push((StmntKind::Assignment(lhs, rhs), sctx));
        }
        pub fn run(mut self) {
            let postorder: Vec<_> =
                self.cfg.postorder_iter().map(|(id, _)| id).collect();
            for id in postorder {
                let new_stmts =
                    IndexVec::with_capacity(2 *
                                                self.cfg[id].statements.len());
                let old_stmts =
                    replace(&mut self.cfg[id].statements, new_stmts);
                for (stmnt, sctx) in old_stmts.into_iter().rev() {
                    match stmnt {
                        StmntKind::Assignment(lhs, rhs) => {
                            self.append_assignment_and_derivatives(lhs, rhs,
                                                                   sctx);
                            self.forward_stmnts.reverse();
                            self.cfg[id].statements.append(&mut self.forward_stmnts);
                        }
                        StmntKind::NoOp => (),
                        stmnt => {
                            self.cfg[id].statements.push((stmnt, sctx));
                        }
                    }
                }
                self.cfg[id].statements.reverse();
                let phis =
                    IndexVec::with_capacity(self.cfg[id].phi_statements.len());
                let mut phis =
                    replace(&mut self.cfg[id].phi_statements, phis);
                for phi in &phis {
                    for (unkown, dst) in
                        self.cfg.derivatives.get(&phi.dst).cloned().into_iter().flatten()
                        {
                        let sources =
                            phi.sources.iter().map(|(bb, local)|
                                                       {
                                                           (*bb,
                                                            self.cfg.demand_derivative_unchecked(*local,
                                                                                                 unkown))
                                                       }).collect();
                        self.cfg[id].phi_statements.push(PhiData{dst,
                                                                 sources,
                                                                 sctx:
                                                                     phi.sctx,});
                    }
                }
                self.cfg[id].phi_statements.append(&mut phis)
            }
        }
    }
    fn operand_to_derivative<C: CfgFunctions>(operand: COperand<C>)
     -> CallTypeDerivative<C> {
        Derivative::Operand(operand.contents)
    }
}
pub mod dfa {
    //! This module impliments a general data flow framework that allows to easily implement multiple data flow analysis
    use crate::cfg::{BasicBlock, ControlFlowGraph, Location, LocationKind,
                     Phi, PhiData, Terminator};
    use crate::dfa::direciton::Direction;
    use crate::dfa::engine::Engine;
    use crate::dfa::lattice::JoinSemiLattice;
    use crate::{CfgFunctions, RValue, Statement, StatementId};
    use data_structures::{bit_set::{BitSet, FullBitSetOperations,
                                            HybridBitSet}, index_vec::Idx};
    use std::borrow::BorrowMut;
    use std::cmp::Ordering;
    pub use cursors::{GenKillResultsCursor, GenKillResultsRefCursor,
                      ResultsCursor, ResultsRefCursor};
    pub use engine::{GenKillResults, Results};
    use std::fmt::Debug;
    pub use visitor::{ResultsVisitable, ResultsVisitor, ResultsVisitorMut};
    mod cursors {
        //! Random access inspection of the results of a dataflow analysis.
        use std::borrow::Borrow;
        use std::cmp::Ordering;
        use super::{Analysis, Direction, Effect, EffectIndex, Results};
        use crate::cfg::{BasicBlock, ControlFlowGraph, Location, LocationKind,
                         START_BLOCK};
        use crate::dfa::GenKillAnalysisImpl;
        use crate::CfgFunctions;
        /// A `ResultsCursor` that borrows the underlying `Results`.
        pub type ResultsRefCursor<'a, C, A> =
         ResultsCursor<C, A, &'a Results<C, A>>;
        pub type GenKillResultsCursor<'a, C, A> =
         ResultsCursor<C, GenKillAnalysisImpl<A>,
                       Results<C, GenKillAnalysisImpl<A>>>;
        pub type GenKillResultsRefCursor<'a, C, A> =
         ResultsCursor<C, GenKillAnalysisImpl<A>,
                       &'a Results<C, GenKillAnalysisImpl<A>>>;
        /// Allows random access inspection of the results of a dataflow analysis.
        ///
        /// This cursor only has linear performance within a basic block when its statements are visited in
        /// the same order as the `DIRECTION` of the analysis. In the worst case—when statements are
        /// visited in *reverse* order—performance will be quadratic in the number of statements in the
        /// block. The order in which basic blocks are inspected has no impact on performance.
        ///
        /// A `ResultsCursor` can either own (the default) or borrow the dataflow results it inspects. The
        /// type of ownership is determined by `R` (see `ResultsRefCursor` above).
        pub struct ResultsCursor<C: CfgFunctions, A, R = Results<C, A>> where
                   A: Analysis<C> {
            results: R,
            state: A::Domain,
            pos: CursorPosition,
            /// Indicates that `state` has been modified with a custom effect.
            ///
            /// When this flag is set, we need to reset to an entry set before doing a seek.
            state_needs_reset: bool,
        }
        impl <C: CfgFunctions, A, R> ResultsCursor<C, A, R> where
         A: Analysis<C>, R: Borrow<Results<C, A>> {
            /// Returns a new cursor that can inspect `results`.
            pub fn new(cfg: &ControlFlowGraph<C>, results: R) -> Self {
                let bottom_value =
                    results.borrow().analysis.bottom_value(cfg);
                ResultsCursor{results,
                              state_needs_reset: true,
                              state: bottom_value,
                              pos: CursorPosition::block_entry(START_BLOCK),}
            }
            /// Returns the underlying `Results`.
            pub fn results(&self) -> &Results<C, A> { &self.results.borrow() }
            /// Returns the `Analysis` used to generate the underlying `Results`.
            pub fn analysis(&self) -> &A { &self.results.borrow().analysis }
            /// Returns the dataflow state at the current location.
            pub fn get(&self) -> &A::Domain { &self.state }
            pub fn finish(self) -> A::Domain { self.state }
            /// Resets the cursor to hold the entry set for the given basic block.
            ///
            /// For forward dataflow analyses, this is the dataflow state prior to the first statement.
            ///
            /// For backward dataflow analyses, this is the dataflow state after the terminator.
            pub(super) fn seek_to_block_entry(&mut self, block: BasicBlock,
                                              cfg: &ControlFlowGraph<C>) {
                self.state.clone_from(&self.results.borrow().entry_set_for_block(block));
                self.results.borrow().analysis.init_block(cfg,
                                                          &mut self.state);
                self.pos = CursorPosition::block_entry(block);
                self.state_needs_reset = false;
            }
            /// Resets the cursor to hold the state prior to the first statement in a basic block.
            ///
            /// For forward analyses, this is the entry set for the given block.
            ///
            /// For backward analyses, this is the state that will be propagated to its
            /// predecessors (ignoring edge-specific effects).
            pub fn seek_to_block_start(&mut self, block: BasicBlock,
                                       cfg: &ControlFlowGraph<C>) {
                if A::Direction::IS_FORWARD {
                    self.seek_to_block_entry(block, cfg)
                } else { self.seek(Effect::After.at_index(0), block, cfg) }
            }
            /// Resets the cursor to hold the state after the terminator in a basic block.
            ///
            /// For backward analyses, this is the entry set for the given block.
            ///
            /// For forward analyses, this is the state that will be propagated to its
            /// successors (ignoring edge-specific effects).
            pub fn seek_to_block_end(&mut self, block: BasicBlock,
                                     cfg: &ControlFlowGraph<C>) {
                if A::Direction::IS_FORWARD {
                    self.seek_after_effect(Location{block,
                                                    kind:
                                                        LocationKind::Terminator,},
                                           cfg)
                } else { self.seek_to_block_entry(block, cfg) }
            }
            /// Resets the cursor to hold the state after the terminator at the exit block of the cfg.
            pub fn seek_to_exit_block_end(&mut self,
                                          cfg: &ControlFlowGraph<C>) {
                self.seek_to_block_end(cfg.end(), cfg)
            }
            /// Advances the cursor to hold the dataflow state at `target` after its effect is
            /// applied.
            pub fn seek_after_effect(&mut self, target: Location,
                                     cfg: &ControlFlowGraph<C>) {
                self.seek(Effect::After.at_location(target, cfg),
                          target.block, cfg)
            }
            /// Advances the cursor to hold the dataflow state at `target` after its effect is
            /// applied.
            pub fn seek_before_effect(&mut self, target: Location,
                                      cfg: &ControlFlowGraph<C>) {
                self.seek(Effect::Before.at_location(target, cfg),
                          target.block, cfg)
            }
            fn seek(&mut self, target: EffectIndex, block: BasicBlock,
                    cfg: &ControlFlowGraph<C>) {
                if self.state_needs_reset || self.pos.block != block {
                    self.seek_to_block_entry(block, cfg);
                } else if let Some(curr_effect) = self.pos.curr_effect_index {
                    let mut ord = curr_effect.idx.cmp(&target.idx);
                    if !A::Direction::IS_FORWARD { ord = ord.reverse() }
                    match ord.then_with(||
                                            curr_effect.effect.cmp(&target.effect))
                        {
                        Ordering::Equal => return,
                        Ordering::Greater =>
                        self.seek_to_block_entry(block, cfg),
                        Ordering::Less => { }
                    }
                }
                if true {
                    {
                        match (&block, &self.pos.block) {
                            (left_val, right_val) => {
                                if !(*left_val == *right_val) {
                                    let kind =
                                        ::core::panicking::AssertKind::Eq;
                                    ::core::panicking::assert_failed(kind,
                                                                     &*left_val,
                                                                     &*right_val,
                                                                     ::core::option::Option::None);
                                }
                            }
                        }
                    };
                };
                let block_data = &cfg.blocks[block];
                let next_effect =
                    if A::Direction::IS_FORWARD {
                        self.pos.curr_effect_index.map_or_else(||
                                                                   Effect::Before.at_index(0),
                                                               EffectIndex::next_in_forward_order)
                    } else {
                        self.pos.curr_effect_index.map_or_else(||
                                                                   Effect::Before.at_index(block_data.statements.len()),
                                                               EffectIndex::next_in_backward_order)
                    };
                let analysis = &self.results.borrow().analysis;
                A::Direction::apply_effects_in_range(analysis, cfg,
                                                     &mut self.state, block,
                                                     block_data,
                                                     next_effect..=target);
                self.pos =
                    CursorPosition{block, curr_effect_index: Some(target),};
            }
            /// Applies `f` to the cursor's internal state.
            ///
            /// This can be used, e.g., to apply the call return effect directly to the cursor without
            /// creating an extra copy of the dataflow state.
            pub fn apply_custom_effect(&mut self,
                                       f: impl FnOnce(&A, &mut A::Domain)) {
                f(&self.results.borrow().analysis, &mut self.state);
                self.state_needs_reset = true;
            }
        }
        struct CursorPosition {
            block: BasicBlock,
            curr_effect_index: Option<EffectIndex>,
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::clone::Clone for CursorPosition {
            #[inline]
            fn clone(&self) -> CursorPosition {
                {
                    let _: ::core::clone::AssertParamIsClone<BasicBlock>;
                    let _:
                            ::core::clone::AssertParamIsClone<Option<EffectIndex>>;
                    *self
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::marker::Copy for CursorPosition { }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::fmt::Debug for CursorPosition {
            fn fmt(&self, f: &mut ::core::fmt::Formatter)
             -> ::core::fmt::Result {
                match *self {
                    CursorPosition {
                    block: ref __self_0_0, curr_effect_index: ref __self_0_1 }
                    => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_struct(f,
                                                                      "CursorPosition");
                        let _ =
                            ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                            "block",
                                                            &&(*__self_0_0));
                        let _ =
                            ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                            "curr_effect_index",
                                                            &&(*__self_0_1));
                        ::core::fmt::DebugStruct::finish(debug_trait_builder)
                    }
                }
            }
        }
        impl CursorPosition {
            fn block_entry(block: BasicBlock) -> CursorPosition {
                CursorPosition{block, curr_effect_index: None,}
            }
        }
    }
    pub mod direciton {
        use super::Analysis;
        use crate::cfg::{BasicBlock, BasicBlockData, ControlFlowGraph, Phi,
                         TerminatorKind};
        use crate::dfa::visitor::{ResultsVisitable, ResultsVisitor,
                                  ResultsVisitorMut};
        use crate::dfa::{Effect, EffectIndex, GenKillAnalysis, GenKillSet};
        use crate::{CfgFunctions, StatementId};
        use std::cmp::max;
        use std::ops::RangeInclusive;
        pub trait Direction {
            const IS_FORWARD: bool ;
            /// Applies all effects between the given `EffectIndex`s.
            ///
            /// `effects.start()` must precede or equal `effects.end()` in this direction.
            fn apply_effects_in_range<A,
                                      C>(analysis: &A,
                                         cfg: &ControlFlowGraph<C>,
                                         state: &mut A::Domain,
                                         block: BasicBlock,
                                         block_data: &BasicBlockData<C>,
                                         effects: RangeInclusive<EffectIndex>)
            where
            C: CfgFunctions,
            A: Analysis<C>;
            fn apply_effects_in_block<A,
                                      C>(analysis: &mut A,
                                         cfg: &ControlFlowGraph<C>,
                                         state: &mut A::Domain,
                                         block: BasicBlock,
                                         block_data: &BasicBlockData<C>)
            where
            C: CfgFunctions,
            A: Analysis<C>;
            fn gen_kill_effects_in_block<A,
                                         C>(analysis: &mut A,
                                            cfg: &ControlFlowGraph<C>,
                                            state: &mut GenKillSet<A::Idx>,
                                            block: BasicBlock,
                                            block_data: &BasicBlockData<C>)
            where
            C: CfgFunctions,
            A: GenKillAnalysis<C>;
            fn visit_results_in_block<C, F,
                                      R>(state: &mut F,
                                         cfg: &ControlFlowGraph<C>,
                                         block: BasicBlock,
                                         block_data: &BasicBlockData<C>,
                                         results: &R,
                                         vis:
                                             &mut impl ResultsVisitor<C,
                                                                      FlowState
                                                                      = F>)
            where
            R: ResultsVisitable<C, FlowState = F>,
            C: CfgFunctions;
            fn visit_results_in_block_mut<C, F,
                                          R>(state: &mut F,
                                             cfg: &mut ControlFlowGraph<C>,
                                             block: BasicBlock, results: &R,
                                             vis:
                                                 &mut impl ResultsVisitorMut<C,
                                                                             FlowState
                                                                             =
                                                                             F>)
            where
            R: ResultsVisitable<C, FlowState = F>,
            C: CfgFunctions;
            fn join_state_into_successors_of<A,
                                             C>(analysis: &mut A,
                                                cfg: &ControlFlowGraph<C>,
                                                exit_state: &mut A::Domain,
                                                block:
                                                    (BasicBlock,
                                                     &BasicBlockData<C>),
                                                propagate:
                                                    impl FnMut(BasicBlock,
                                                               &A::Domain))
            where
            C: CfgFunctions,
            A: Analysis<C>;
        }
        /// Dataflow that runs from the exit of a block (the terminator), to its entry (the first statement).
        pub struct Backward;
        impl Direction for Backward {
            const IS_FORWARD: bool = false;
            fn apply_effects_in_range<A,
                                      C>(analysis: &A,
                                         cfg: &ControlFlowGraph<C>,
                                         state: &mut A::Domain,
                                         block: BasicBlock,
                                         block_data: &BasicBlockData<C>,
                                         effects: RangeInclusive<EffectIndex>)
             where C: CfgFunctions, A: Analysis<C> {
                let (from, to) = (*effects.start(), *effects.end());
                let terminator_index =
                    block_data.statements.len() +
                        block_data.phi_statements.len();
                if !(from.idx <= terminator_index) {
                    ::core::panicking::panic("assertion failed: from.idx <= terminator_index")
                };
                if !!to.precedes_in_backward_order(from) {
                    ::core::panicking::panic("assertion failed: !to.precedes_in_backward_order(from)")
                };
                let mut end =
                    if from.effect == Effect::Before {
                        if from.idx == terminator_index {
                            analysis.apply_terminator_effect(cfg, state,
                                                             block_data.terminator(),
                                                             block);
                            from.idx
                        } else { from.idx + 1 }
                    } else { from.idx };
                let start =
                    if to.effect == Effect::Before {
                        to.idx + 1
                    } else { to.idx };
                if end > block_data.phi_statements.len() {
                    let stmnt_start =
                        StatementId::new(start.saturating_sub(block_data.phi_statements.len()));
                    let stmnt_end =
                        StatementId::new(end -
                                             block_data.phi_statements.len());
                    for (idx, stmnt) in
                        block_data.statements[stmnt_start..stmnt_end].iter_enumerated().rev()
                        {
                        analysis.apply_statement_effect(cfg, state, stmnt,
                                                        idx, block);
                    }
                    end = block_data.phi_statements.len()
                }
                if start < block_data.phi_statements.len() {
                    let start = Phi::new(start);
                    let end = Phi::new(end);
                    for (phi, phi_data) in
                        block_data.phi_statements[start..end].iter_enumerated().rev()
                        {
                        analysis.apply_phi_effect(cfg, state, phi_data, block,
                                                  phi);
                    }
                }
            }
            fn apply_effects_in_block<A,
                                      C>(analysis: &mut A,
                                         cfg: &ControlFlowGraph<C>,
                                         state: &mut A::Domain,
                                         block: BasicBlock,
                                         block_data: &BasicBlockData<C>) where
             C: CfgFunctions, A: Analysis<C> {
                analysis.init_block(cfg, state);
                let terminator = block_data.terminator();
                analysis.apply_terminator_effect(cfg, state, terminator,
                                                 block);
                for (statement_index, stmnt) in
                    block_data.statements.iter_enumerated().rev() {
                    analysis.apply_statement_effect(cfg, state, stmnt,
                                                    statement_index, block);
                }
                for (statement_index, phi) in
                    block_data.phi_statements.iter_enumerated().rev() {
                    analysis.apply_phi_effect(cfg, state, phi, block,
                                              statement_index);
                }
            }
            fn gen_kill_effects_in_block<A,
                                         C>(analysis: &mut A,
                                            cfg: &ControlFlowGraph<C>,
                                            state: &mut GenKillSet<A::Idx>,
                                            block: BasicBlock,
                                            block_data: &BasicBlockData<C>)
             where C: CfgFunctions, A: GenKillAnalysis<C> {
                let terminator = block_data.terminator();
                analysis.terminator_effect(cfg, state, terminator, block);
                for (statement_index, stmnt) in
                    block_data.statements.iter_enumerated().rev() {
                    analysis.statement_effect(cfg, state, stmnt,
                                              statement_index, block);
                }
                for (statement_index, phi) in
                    block_data.phi_statements.iter_enumerated().rev() {
                    analysis.phi_effect(cfg, state, phi, block,
                                        statement_index);
                }
            }
            fn visit_results_in_block<C, F,
                                      R>(state: &mut F,
                                         cfg: &ControlFlowGraph<C>,
                                         block: BasicBlock,
                                         block_data: &BasicBlockData<C>,
                                         results: &R,
                                         vis:
                                             &mut impl ResultsVisitor<C,
                                                                      FlowState
                                                                      = F>)
             where R: ResultsVisitable<C, FlowState = F>, C: CfgFunctions {
                if !(block < cfg.blocks.len_idx()) {
                    ::core::panicking::panic("assertion failed: block < cfg.blocks.len_idx()")
                };
                results.reset_to_block_entry(cfg, state, block);
                vis.visit_block_end(state, block_data, block);
                vis.visit_terminator_before_effect(state,
                                                   block_data.terminator(),
                                                   block);
                results.reconstruct_terminator_effect(cfg, state,
                                                      block_data.terminator(),
                                                      block);
                vis.visit_terminator_after_effect(state,
                                                  block_data.terminator(),
                                                  block);
                for (phi, phi_data) in
                    cfg.blocks[block].phi_statements.iter_enumerated().rev() {
                    vis.visit_phi_before_effect(state, phi_data, block, phi);
                    results.reconstruct_phi_effect(cfg, state, phi_data,
                                                   block, phi);
                    vis.visit_phi_after_effect(state, phi_data, block, phi);
                }
                for (id, stmnt) in
                    cfg.blocks[block].statements.iter_enumerated().rev() {
                    vis.visit_statement_before_effect(state, stmnt, block,
                                                      id);
                    results.reconstruct_statement_effect(cfg, state, stmnt,
                                                         block, id);
                    vis.visit_statement_after_effect(state, stmnt, block, id);
                }
                vis.visit_block_start(state, block_data, block)
            }
            fn visit_results_in_block_mut<C, F,
                                          R>(state: &mut F,
                                             cfg: &mut ControlFlowGraph<C>,
                                             block: BasicBlock, results: &R,
                                             vis:
                                                 &mut impl ResultsVisitorMut<C,
                                                                             FlowState
                                                                             =
                                                                             F>)
             where R: ResultsVisitable<C, FlowState = F>, C: CfgFunctions {
                if !(block < cfg.blocks.len_idx()) {
                    ::core::panicking::panic("assertion failed: block < cfg.blocks.len_idx()")
                };
                results.reset_to_block_entry(cfg, state, block);
                vis.visit_block_end(state, &mut cfg.blocks[block], block);
                vis.visit_terminator_before_effect(state,
                                                   cfg.blocks[block].terminator_mut(),
                                                   block);
                results.reconstruct_terminator_effect(cfg, state,
                                                      cfg.blocks[block].terminator(),
                                                      block);
                vis.visit_terminator_after_effect(state,
                                                  cfg.blocks[block].terminator_mut(),
                                                  block);
                for phi in cfg.blocks[block].phi_statements.indices().rev() {
                    vis.visit_phi_before_effect(state,
                                                unsafe {
                                                    cfg.blocks[block].phi_statements.raw.get_unchecked_mut(phi.index())
                                                }, block, phi);
                    results.reconstruct_phi_effect(cfg, state,
                                                   unsafe {
                                                       cfg.blocks[block].phi_statements.raw.get_unchecked(phi.index())
                                                   }, block, phi);
                    vis.visit_phi_after_effect(state,
                                               unsafe {
                                                   cfg.blocks[block].phi_statements.raw.get_unchecked_mut(phi.index())
                                               }, block, phi);
                }
                for stmnt in cfg.blocks[block].statements.indices().rev() {
                    vis.visit_statement_before_effect(state,
                                                      unsafe {
                                                          cfg.blocks[block].statements.raw.get_unchecked_mut(stmnt.index())
                                                      }, block, stmnt);
                    results.reconstruct_statement_effect(cfg, state,
                                                         unsafe {
                                                             cfg.blocks[block].statements.raw.get_unchecked(stmnt.index())
                                                         }, block, stmnt);
                    vis.visit_statement_after_effect(state,
                                                     unsafe {
                                                         cfg.blocks[block].statements.raw.get_unchecked_mut(stmnt.index())
                                                     }, block, stmnt);
                }
                vis.visit_block_start(state, &mut cfg.blocks[block], block)
            }
            fn join_state_into_successors_of<A,
                                             C>(_analysis: &mut A,
                                                cfg: &ControlFlowGraph<C>,
                                                exit_state: &mut A::Domain,
                                                block:
                                                    (BasicBlock,
                                                     &BasicBlockData<C>),
                                                mut propagate:
                                                    impl FnMut(BasicBlock,
                                                               &A::Domain))
             where C: CfgFunctions, A: Analysis<C> {
                for pred in cfg.predecessors(block.0).iter().copied() {
                    propagate(pred, exit_state)
                }
            }
        }
        /// Dataflow that runs from the entry of a block (the first statement), to its exit (terminator).
        pub struct Forward;
        impl Direction for Forward {
            const IS_FORWARD: bool = true;
            fn apply_effects_in_range<A,
                                      C>(analysis: &A,
                                         cfg: &ControlFlowGraph<C>,
                                         state: &mut A::Domain,
                                         block: BasicBlock,
                                         block_data: &BasicBlockData<C>,
                                         effects: RangeInclusive<EffectIndex>)
             where C: CfgFunctions, A: Analysis<C> {
                let (from, to) = (*effects.start(), *effects.end());
                let terminator_index =
                    block_data.statements.len() +
                        block_data.phi_statements.len();
                if !(from.idx <= terminator_index) {
                    ::core::panicking::panic("assertion failed: from.idx <= terminator_index")
                };
                if !!to.precedes_in_forward_order(from) {
                    ::core::panicking::panic("assertion failed: !to.precedes_in_forward_order(from)")
                };
                let mut start =
                    if from.effect == Effect::After {
                        from.idx + 1
                    } else { from.idx };
                let end =
                    if to.effect == Effect::After {
                        if to.idx == terminator_index {
                            to.idx
                        } else { to.idx + 1 }
                    } else { to.idx };
                if start < block_data.phi_statements.len() {
                    let phi_start = Phi::new(start);
                    let phi_end =
                        max(Phi::new(end),
                            block_data.phi_statements.len_idx());
                    for (phi, phi_data) in
                        block_data.phi_statements[phi_start..phi_end].iter_enumerated()
                        {
                        analysis.apply_phi_effect(cfg, state, phi_data, block,
                                                  phi);
                    }
                    start = block_data.phi_statements.len()
                }
                if end > block_data.phi_statements.len() {
                    let start =
                        StatementId::new(start -
                                             block_data.phi_statements.len());
                    let end =
                        StatementId::new(end -
                                             block_data.phi_statements.len());
                    for (idx, stmnt) in
                        block_data.statements[start..end].iter_enumerated() {
                        analysis.apply_statement_effect(cfg, state, stmnt,
                                                        idx, block);
                    }
                }
                if to.idx == terminator_index && to.effect == Effect::After {
                    analysis.apply_terminator_effect(cfg, state,
                                                     block_data.terminator(),
                                                     block);
                }
            }
            fn apply_effects_in_block<A,
                                      C>(analysis: &mut A,
                                         cfg: &ControlFlowGraph<C>,
                                         state: &mut A::Domain,
                                         block: BasicBlock,
                                         block_data: &BasicBlockData<C>) where
             C: CfgFunctions, A: Analysis<C> {
                analysis.init_block(cfg, state);
                for (statement_index, phi) in
                    block_data.phi_statements.iter_enumerated() {
                    analysis.apply_phi_effect(cfg, state, phi, block,
                                              statement_index);
                }
                for (statement_index, stmnt) in
                    block_data.statements.iter_enumerated() {
                    analysis.apply_statement_effect(cfg, state, stmnt,
                                                    statement_index, block);
                }
                let terminator = block_data.terminator();
                analysis.apply_terminator_effect(cfg, state, terminator,
                                                 block);
            }
            fn gen_kill_effects_in_block<A,
                                         C>(analysis: &mut A,
                                            cfg: &ControlFlowGraph<C>,
                                            state: &mut GenKillSet<A::Idx>,
                                            block: BasicBlock,
                                            block_data: &BasicBlockData<C>)
             where C: CfgFunctions, A: GenKillAnalysis<C> {
                for (statement_index, phi) in
                    block_data.phi_statements.iter_enumerated() {
                    analysis.phi_effect(cfg, state, phi, block,
                                        statement_index);
                }
                for (statement_index, stmnt) in
                    block_data.statements.iter_enumerated() {
                    analysis.statement_effect(cfg, state, stmnt,
                                              statement_index, block);
                }
                let terminator = block_data.terminator();
                analysis.terminator_effect(cfg, state, terminator, block);
            }
            fn visit_results_in_block<C, F,
                                      R>(state: &mut F,
                                         cfg: &ControlFlowGraph<C>,
                                         block: BasicBlock,
                                         block_data: &BasicBlockData<C>,
                                         results: &R,
                                         vis:
                                             &mut impl ResultsVisitor<C,
                                                                      FlowState
                                                                      = F>)
             where R: ResultsVisitable<C, FlowState = F>, C: CfgFunctions {
                if !(block < cfg.blocks.len_idx()) {
                    ::core::panicking::panic("assertion failed: block < cfg.blocks.len_idx()")
                };
                results.reset_to_block_entry(cfg, state, block);
                vis.visit_block_start(state, block_data, block);
                for (phi, phi_data) in
                    cfg.blocks[block].phi_statements.iter_enumerated() {
                    vis.visit_phi_before_effect(state, phi_data, block, phi);
                    results.reconstruct_phi_effect(cfg, state, phi_data,
                                                   block, phi);
                    vis.visit_phi_after_effect(state, phi_data, block, phi);
                }
                for (id, stmnt) in
                    cfg.blocks[block].statements.iter_enumerated() {
                    vis.visit_statement_before_effect(state, stmnt, block,
                                                      id);
                    results.reconstruct_statement_effect(cfg, state, stmnt,
                                                         block, id);
                    vis.visit_statement_after_effect(state, stmnt, block, id);
                }
                vis.visit_terminator_before_effect(state,
                                                   block_data.terminator(),
                                                   block);
                results.reconstruct_terminator_effect(cfg, state,
                                                      block_data.terminator(),
                                                      block);
                vis.visit_terminator_after_effect(state,
                                                  block_data.terminator(),
                                                  block);
                vis.visit_block_end(state, block_data, block);
            }
            fn visit_results_in_block_mut<C, F,
                                          R>(state: &mut F,
                                             cfg: &mut ControlFlowGraph<C>,
                                             block: BasicBlock, results: &R,
                                             vis:
                                                 &mut impl ResultsVisitorMut<C,
                                                                             FlowState
                                                                             =
                                                                             F>)
             where R: ResultsVisitable<C, FlowState = F>, C: CfgFunctions {
                if !(block < cfg.blocks.len_idx()) {
                    ::core::panicking::panic("assertion failed: block < cfg.blocks.len_idx()")
                };
                results.reset_to_block_entry(cfg, state, block);
                vis.visit_block_start(state, &mut cfg.blocks[block], block);
                for phi in cfg.blocks[block].phi_statements.indices() {
                    vis.visit_phi_before_effect(state,
                                                unsafe {
                                                    cfg.blocks[block].phi_statements.raw.get_unchecked_mut(phi.index())
                                                }, block, phi);
                    results.reconstruct_phi_effect(cfg, state,
                                                   unsafe {
                                                       cfg.blocks[block].phi_statements.raw.get_unchecked(phi.index())
                                                   }, block, phi);
                    vis.visit_phi_after_effect(state,
                                               unsafe {
                                                   cfg.blocks[block].phi_statements.raw.get_unchecked_mut(phi.index())
                                               }, block, phi);
                }
                for stmnt in cfg.blocks[block].statements.indices() {
                    vis.visit_statement_before_effect(state,
                                                      unsafe {
                                                          cfg.blocks[block].statements.raw.get_unchecked_mut(stmnt.index())
                                                      }, block, stmnt);
                    results.reconstruct_statement_effect(cfg, state,
                                                         unsafe {
                                                             cfg.blocks[block].statements.raw.get_unchecked(stmnt.index())
                                                         }, block, stmnt);
                    vis.visit_statement_after_effect(state,
                                                     unsafe {
                                                         cfg.blocks[block].statements.raw.get_unchecked_mut(stmnt.index())
                                                     }, block, stmnt);
                }
                vis.visit_terminator_before_effect(state,
                                                   cfg.blocks[block].terminator_mut(),
                                                   block);
                results.reconstruct_terminator_effect(cfg, state,
                                                      cfg.blocks[block].terminator(),
                                                      block);
                vis.visit_terminator_after_effect(state,
                                                  cfg.blocks[block].terminator_mut(),
                                                  block);
                vis.visit_block_end(state, &mut cfg.blocks[block], block);
            }
            fn join_state_into_successors_of<A,
                                             C>(analysis: &mut A,
                                                cfg: &ControlFlowGraph<C>,
                                                exit_state: &mut A::Domain,
                                                block:
                                                    (BasicBlock,
                                                     &BasicBlockData<C>),
                                                mut propagate:
                                                    impl FnMut(BasicBlock,
                                                               &A::Domain))
             where C: CfgFunctions, A: Analysis<C> {
                match block.1.terminator().kind {
                    TerminatorKind::Goto(succ) => {
                        if analysis.apply_edge_effects(cfg, block.0,
                                                       exit_state) {
                            propagate(succ, exit_state)
                        }
                    }
                    TerminatorKind::Split {
                    ref condition, true_block, false_block, .. } => {
                        let mut applier =
                            SplitEdgeEffectApplier{exit_state,
                                                   true_block,
                                                   false_block,
                                                   propagate,
                                                   effects_applied: false,};
                        analysis.apply_split_edge_effects(cfg, block.0,
                                                          condition,
                                                          exit_state,
                                                          &mut applier);
                        let SplitEdgeEffectApplier {
                                exit_state, mut propagate, effects_applied, ..
                                } = applier;
                        if !effects_applied {
                            propagate(false_block, exit_state);
                            propagate(true_block, exit_state);
                        }
                    }
                    TerminatorKind::End => { }
                }
            }
        }
        struct SplitEdgeEffectApplier<'a, D, F> {
            exit_state: &'a D,
            true_block: BasicBlock,
            false_block: BasicBlock,
            propagate: F,
            effects_applied: bool,
        }
        impl <D, F> super::SplitEdgeEffects<D> for
         SplitEdgeEffectApplier<'_, D, F> where D: Clone,
         F: FnMut(BasicBlock, &D) {
            fn apply(&mut self,
                     mut apply_edge_effect:
                         impl FnMut(&mut D, BasicBlock, bool) -> bool) {
                if !!self.effects_applied {
                    ::core::panicking::panic("assertion failed: !self.effects_applied")
                };
                let mut tmp = self.exit_state.clone();
                if apply_edge_effect(&mut tmp, self.true_block, true) {
                    (self.propagate)(self.true_block, &tmp);
                }
                tmp.clone_from(&self.exit_state);
                if apply_edge_effect(&mut tmp, self.false_block, false) {
                    (self.propagate)(self.false_block, &tmp);
                }
                self.effects_applied = true;
            }
        }
    }
    mod engine {
        //! A solver for dataflow problems.
        use std::borrow::BorrowMut;
        use super::{Analysis, Direction, GenKillAnalysis, GenKillSet,
                    JoinSemiLattice};
        use crate::cfg::{BasicBlock, BasicBlockData, ControlFlowGraph,
                         START_BLOCK};
        use crate::dfa::cursors::{ResultsCursor, ResultsRefCursor};
        use crate::dfa::visitor::{visit_results, visit_results_mut,
                                  ResultsVisitable, ResultsVisitor,
                                  ResultsVisitorMut};
        use crate::dfa::GenKillAnalysisImpl;
        use crate::CfgFunctions;
        use data_structures::index_vec::{index_vec, Idx, IndexVec};
        use data_structures::{iter, WorkQueue};
        use std::fmt;
        use std::fmt::{Debug, Formatter};
        use tracing::debug;
        pub type GenKillResults<C, A> = Results<C, GenKillAnalysisImpl<A>>;
        /// A dataflow analysis that has converged to fixpoint.
        pub struct Results<C, A> where A: Analysis<C>, C: CfgFunctions {
            pub analysis: A,
            pub(crate) entry_sets: IndexVec<BasicBlock, A::Domain>,
        }
        impl <A, C> Results<C, A> where A: Analysis<C>, C: CfgFunctions {
            /// Creates a `ResultsCursor` that can inspect these `Results`.
            pub fn into_results_cursor(self, cfg: &ControlFlowGraph<C>)
             -> ResultsCursor<C, A> {
                ResultsCursor::new(cfg, self)
            }
            /// Creates a `ResultsCursor` that can inspect these `Results`.
            pub fn as_results_cursor(&self, cfg: &ControlFlowGraph<C>)
             -> ResultsRefCursor<C, A> {
                ResultsRefCursor::new(cfg, &self)
            }
            /// Gets the dataflow state for the given block.
            pub fn entry_set_for_block(&self, block: BasicBlock)
             -> &A::Domain {
                &self.entry_sets[block]
            }
            pub fn visit_in_blocks_with<'a>(&self,
                                            cfg: &'a ControlFlowGraph<C>,
                                            blocks:
                                                impl IntoIterator<Item =
                                                                  (BasicBlock,
                                                                   &'a BasicBlockData<C>)>,
                                            vis:
                                                &mut impl ResultsVisitor<C,
                                                                         FlowState
                                                                         =
                                                                         <Self
                                                                         as
                                                                         ResultsVisitable<C>>::FlowState>) {
                visit_results(cfg, blocks, self, vis)
            }
            pub fn visit_in_blocks_with_mut(&self,
                                            cfg: &mut ControlFlowGraph<C>,
                                            blocks:
                                                impl IntoIterator<Item =
                                                                  BasicBlock>,
                                            vis:
                                                &mut impl ResultsVisitorMut<C,
                                                                            FlowState
                                                                            =
                                                                            <Self
                                                                            as
                                                                            ResultsVisitable<C>>::FlowState>) {
                visit_results_mut(cfg, blocks, self, vis)
            }
            pub fn visit_with(&self, cfg: &ControlFlowGraph<C>,
                              vis:
                                  &mut impl ResultsVisitor<C, FlowState =
                                                           <Self as
                                                           ResultsVisitable<C>>::FlowState>) {
                self.visit_in_blocks_with(cfg, cfg.blocks.iter_enumerated(),
                                          vis)
            }
            pub fn visit_with_mut(&self, cfg: &mut ControlFlowGraph<C>,
                                  vis:
                                      &mut impl ResultsVisitorMut<C, FlowState
                                                                  =
                                                                  <Self as
                                                                  ResultsVisitable<C>>::FlowState>) {
                self.visit_in_blocks_with_mut(cfg, cfg.blocks.indices(), vis)
            }
        }
        impl <C, A> Debug for Results<C, A> where A: Analysis<C>,
         C: CfgFunctions, A::Domain: Debug {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                Debug::fmt(&self.entry_sets, f)
            }
        }
        /// A solver for dataflow problems.
        pub struct Engine<'a, C, A> where A: Analysis<C>, C: CfgFunctions {
            cfg: &'a ControlFlowGraph<C>,
            entry_sets: IndexVec<BasicBlock, A::Domain>,
            analysis: A,
            /// Cached, cumulative transfer functions for each block.
            #[allow(clippy :: type_complexity)]
            apply_trans_for_block: Option<Box<dyn Fn(BasicBlock,
                                                     &mut A::Domain)>>,
        }
        impl <'a, C, A, T> Engine<'a, C, GenKillAnalysisImpl<A>> where
         A: GenKillAnalysis<C, Idx = T>, C: CfgFunctions, T: Idx {
            /// Creates a new `Engine` to solve a gen-kill dataflow problem.
            pub fn new_gen_kill(cfg: &'a ControlFlowGraph<C>, mut analysis: A)
             -> Self {
                if !cfg.is_cyclic() {
                    {
                        ;
                        if ::tracing::Level::DEBUG <=
                               ::tracing::level_filters::STATIC_MAX_LEVEL &&
                               ::tracing::Level::DEBUG <=
                                   ::tracing::level_filters::LevelFilter::current()
                           {
                            use ::tracing::__macro_support::*;
                            static CALLSITE:
                             ::tracing::__macro_support::MacroCallsite =
                                {
                                    use ::tracing::__macro_support::MacroCallsite;
                                    static META: ::tracing::Metadata<'static>
                                     =
                                        {
                                            ::tracing_core::metadata::Metadata::new("event middle/src/dfa/engine.rs:140",
                                                                                    "middle::dfa::engine",
                                                                                    ::tracing::Level::DEBUG,
                                                                                    Some("middle/src/dfa/engine.rs"),
                                                                                    Some(140u32),
                                                                                    Some("middle::dfa::engine"),
                                                                                    ::tracing_core::field::FieldSet::new(&["message"],
                                                                                                                         ::tracing_core::callsite::Identifier(&CALLSITE)),
                                                                                    ::tracing::metadata::Kind::EVENT)
                                        };
                                    MacroCallsite::new(&META)
                                };
                            let interest = CALLSITE.interest();
                            if !interest.is_never() &&
                                   CALLSITE.is_enabled(interest) {
                                let meta = CALLSITE.metadata();
                                ::tracing::Event::dispatch(meta,
                                                           &{
                                                                #[allow(unused_imports)]
                                                                use ::tracing::field::{debug,
                                                                                       display,
                                                                                       Value};
                                                                let mut iter =
                                                                    meta.fields().iter();
                                                                meta.fields().value_set(&[(&iter.next().expect("FieldSet corrupted (this is a bug)"),
                                                                                           Some(&::core::fmt::Arguments::new_v1(&["Non Cyclical CFG! Gen Kill Transfer Functions are not cached"],
                                                                                                                                &match ()
                                                                                                                                     {
                                                                                                                                     ()
                                                                                                                                     =>
                                                                                                                                     [],
                                                                                                                                 })
                                                                                                    as
                                                                                                    &Value))])
                                                            });
                            }
                        }
                    };
                    return Self::new(cfg, GenKillAnalysisImpl(analysis),
                                     None);
                }
                {
                    ;
                    if ::tracing::Level::DEBUG <=
                           ::tracing::level_filters::STATIC_MAX_LEVEL &&
                           ::tracing::Level::DEBUG <=
                               ::tracing::level_filters::LevelFilter::current()
                       {
                        use ::tracing::__macro_support::*;
                        static CALLSITE:
                         ::tracing::__macro_support::MacroCallsite =
                            {
                                use ::tracing::__macro_support::MacroCallsite;
                                static META: ::tracing::Metadata<'static> =
                                    {
                                        ::tracing_core::metadata::Metadata::new("event middle/src/dfa/engine.rs:144",
                                                                                "middle::dfa::engine",
                                                                                ::tracing::Level::DEBUG,
                                                                                Some("middle/src/dfa/engine.rs"),
                                                                                Some(144u32),
                                                                                Some("middle::dfa::engine"),
                                                                                ::tracing_core::field::FieldSet::new(&["message"],
                                                                                                                     ::tracing_core::callsite::Identifier(&CALLSITE)),
                                                                                ::tracing::metadata::Kind::EVENT)
                                    };
                                MacroCallsite::new(&META)
                            };
                        let interest = CALLSITE.interest();
                        if !interest.is_never() &&
                               CALLSITE.is_enabled(interest) {
                            let meta = CALLSITE.metadata();
                            ::tracing::Event::dispatch(meta,
                                                       &{
                                                            #[allow(unused_imports)]
                                                            use ::tracing::field::{debug,
                                                                                   display,
                                                                                   Value};
                                                            let mut iter =
                                                                meta.fields().iter();
                                                            meta.fields().value_set(&[(&iter.next().expect("FieldSet corrupted (this is a bug)"),
                                                                                       Some(&::core::fmt::Arguments::new_v1(&["Cyclical CFG! Caching gen kill sets"],
                                                                                                                            &match ()
                                                                                                                                 {
                                                                                                                                 ()
                                                                                                                                 =>
                                                                                                                                 [],
                                                                                                                             })
                                                                                                as
                                                                                                &Value))])
                                                        });
                        }
                    }
                };
                let identity =
                    GenKillSet::identity(analysis.domain_size(cfg));
                let mut trans_for_block =
                    ::index_vec::IndexVec::from_vec(::alloc::vec::from_elem(identity,
                                                                            cfg.blocks.len()));
                for ((block, block_data), trans) in
                    iter::zip(cfg.blocks.iter_enumerated(),
                              &mut trans_for_block) {
                    A::Direction::gen_kill_effects_in_block(&mut analysis,
                                                            cfg, trans, block,
                                                            block_data);
                }
                Self::new(cfg, GenKillAnalysisImpl(analysis),
                          Some(Box::new(move |bb, dst|
                                            {
                                                trans_for_block[bb].apply(dst.borrow_mut())
                                            })))
            }
        }
        impl <'a, C, A, D> Engine<'a, C, A> where A: Analysis<C, Domain = D>,
         D: Clone + JoinSemiLattice + Debug, C: CfgFunctions {
            /// Creates a new `Engine` to solve a dataflow problem with an arbitrary transfer
            /// function.
            ///
            /// Gen-kill problems should use `new_gen_kill`, which will coalesce transfer functions for
            /// better performance.
            pub fn new_generic(cfg: &'a ControlFlowGraph<C>, analysis: A)
             -> Self {
                Self::new(cfg, analysis, None)
            }
            #[allow(clippy :: type_complexity)]
            fn new(cfg: &'a ControlFlowGraph<C>, analysis: A,
                   apply_trans_for_block:
                       Option<Box<dyn Fn(BasicBlock, &mut A::Domain)>>)
             -> Self {
                let bottom_value = analysis.bottom_value(cfg);
                let mut entry_sets =
                    ::index_vec::IndexVec::from_vec(::alloc::vec::from_elem(bottom_value.clone(),
                                                                            cfg.blocks.len()));
                analysis.initialize_start_block(cfg,
                                                &mut entry_sets[START_BLOCK]);
                if !A::Direction::IS_FORWARD &&
                       entry_sets[START_BLOCK] != bottom_value {
                    {
                        ::std::rt::begin_panic("`initialize_start_block` is not yet supported for backward dataflow analyses")
                    };
                }
                Engine{cfg, entry_sets, analysis, apply_trans_for_block,}
            }
            /// Computes the fixpoint for this dataflow problem and returns it.
            pub fn iterate_to_fixpoint(self) -> Results<C, A> {
                let Engine {
                        mut analysis,
                        cfg,
                        mut entry_sets,
                        apply_trans_for_block, .. } = self;
                let mut dirty_queue: WorkQueue<BasicBlock> =
                    WorkQueue::with_none(cfg.blocks.len());
                if A::Direction::IS_FORWARD {
                    dirty_queue.extend(cfg.reverse_postorder())
                } else {
                    dirty_queue.extend(cfg.postorder_iter().map(|(bb, _)| bb))
                }
                let mut state = analysis.bottom_value(cfg);
                while let Some(bb) = dirty_queue.pop() {
                    let bb_data = &cfg[bb];
                    state.clone_from(&entry_sets[bb]);
                    match &apply_trans_for_block {
                        Some(apply) => apply(bb, &mut state),
                        None =>
                        A::Direction::apply_effects_in_block(&mut analysis,
                                                             cfg, &mut state,
                                                             bb, bb_data),
                    }
                    A::Direction::join_state_into_successors_of(&mut analysis,
                                                                cfg,
                                                                &mut state,
                                                                (bb, bb_data),
                                                                |target:
                                                                     BasicBlock,
                                                                 state:
                                                                     &A::Domain|
                                                                    {
                                                                        let set_changed =
                                                                            entry_sets[target].join(state);
                                                                        if set_changed
                                                                           {
                                                                            dirty_queue.insert(target);
                                                                        }
                                                                    });
                }
                Results{analysis, entry_sets,}
            }
        }
    }
    pub mod lattice {
        //! Traits used to represent [lattices] for use as the domain of a dataflow analysis.
        //!
        //! # Overview
        //!
        //! The most common lattice is a powerset of some set `S`, ordered by [set inclusion]. The [Hasse
        //! diagram] for the powerset of a set with two elements (`X` and `Y`) is shown below. Note that
        //! distinct elements at the same height in a Hasse diagram (e.g. `{X}` and `{Y}`) are
        //! *incomparable*, not equal.
        //!
        //! ```text
        //!      {X, Y}    <- top
        //!       /  \
        //!    {X}    {Y}
        //!       \  /
        //!        {}      <- bottom
        //!
        //! ```
        //!
        //! The defining characteristic of a lattice—the one that differentiates it from a [partially
        //! ordered set][poset]—is the existence of a *unique* least upper and greatest lower bound for
        //! every pair of elements. The lattice join operator (`∨`) returns the least upper bound, and the
        //! lattice meet operator (`∧`) returns the greatest lower bound. Types that implement one operator
        //! but not the other are known as semilattices. Dataflow analysis only uses the join operator and
        //! will work with any join-semilattice, but both should be specified when possible.
        //!
        //! ## `PartialOrd`
        //!
        //! Given that they represent partially ordered sets, you may be surprised that [`JoinSemiLattice`]
        //! and [`MeetSemiLattice`] do not have [`PartialOrd`][std::cmp::PartialOrd] as a supertrait. This
        //! is because most standard library types use lexicographic ordering instead of set inclusion for
        //! their `PartialOrd` impl. Since we do not actually need to compare lattice elements to run a
        //! dataflow analysis, there's no need for a newtype wrapper with a custom `PartialOrd` impl. The
        //! only benefit would be the ability to check that the least upper (or greatest lower) bound
        //! returned by the lattice join (or meet) operator was in fact greater (or lower) than the inputs.
        //!
        //! [lattices]: https://en.wikipedia.org/wiki/Lattice_(order)
        //! [set inclusion]: https://en.wikipedia.org/wiki/Subset
        //! [Hasse diagram]: https://en.wikipedia.org/wiki/Hasse_diagram
        //! [poset]: https://en.wikipedia.org/wiki/Partially_ordered_set
        use data_structures::bit_set::BitSet;
        use data_structures::index_vec::{Idx, IndexVec};
        use data_structures::{iter, HashMap};
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::option::Option::Some;
        /// A [partially ordered set][poset] that has a [least upper bound][lub] for any pair of elements
        /// in the set.
        ///
        /// [lub]: https://en.wikipedia.org/wiki/Infimum_and_supremum
        /// [poset]: https://en.wikipedia.org/wiki/Partially_ordered_set
        pub trait JoinSemiLattice: PartialEq {
            /// Computes the least upper bound of two elements, storing the result in `self` and returning
            /// `true` if `self` has changed.
            ///
            /// The lattice join operator is abbreviated as `∨`.
            fn join(&mut self, other: &Self)
            -> bool;
        }
        /// A [partially ordered set][poset] that has a [greatest lower bound][glb] for any pair of
        /// elements in the set.
        ///
        /// Dataflow analyses only require that their domains implement [`JoinSemiLattice`], not
        /// `MeetSemiLattice`. However, types that will be used as dataflow domains should implement both
        /// so that they can be used with [`Dual`].
        ///
        /// [glb]: https://en.wikipedia.org/wiki/Infimum_and_supremum
        /// [poset]: https://en.wikipedia.org/wiki/Partially_ordered_set
        pub trait MeetSemiLattice: PartialEq {
            /// Computes the greatest lower bound of two elements, storing the result in `self` and
            /// returning `true` if `self` has changed.
            ///
            /// The lattice meet operator is abbreviated as `∧`.
            fn meet(&mut self, other: &Self)
            -> bool;
        }
        /// A `bool` is a "two-point" lattice with `true` as the top element and `false` as the bottom:
        ///
        /// ```text
        ///      true
        ///        |
        ///      false
        /// ```
        impl JoinSemiLattice for bool {
            fn join(&mut self, other: &Self) -> bool {
                if let (false, true) = (*self, *other) {
                    *self = true;
                    return true;
                }
                false
            }
        }
        impl MeetSemiLattice for bool {
            fn meet(&mut self, other: &Self) -> bool {
                if let (true, false) = (*self, *other) {
                    *self = false;
                    return true;
                }
                false
            }
        }
        /// A tuple (or list) of lattices is itself a lattice whose least upper bound is the concatenation
        /// of the least upper bounds of each element of the tuple (or list).
        ///
        /// In other words:
        ///     (A₀, A₁, ..., Aₙ) ∨ (B₀, B₁, ..., Bₙ) = (A₀∨B₀, A₁∨B₁, ..., Aₙ∨Bₙ)
        impl <I: Idx, T: JoinSemiLattice> JoinSemiLattice for IndexVec<I, T> {
            fn join(&mut self, other: &Self) -> bool {
                if true {
                    {
                        match (&self.len(), &other.len()) {
                            (left_val, right_val) => {
                                if !(*left_val == *right_val) {
                                    let kind =
                                        ::core::panicking::AssertKind::Eq;
                                    ::core::panicking::assert_failed(kind,
                                                                     &*left_val,
                                                                     &*right_val,
                                                                     ::core::option::Option::None);
                                }
                            }
                        }
                    };
                };
                let mut changed = false;
                for (a, b) in iter::zip(self, other) { changed |= a.join(b); }
                changed
            }
        }
        impl <I: Idx, T: MeetSemiLattice> MeetSemiLattice for IndexVec<I, T> {
            fn meet(&mut self, other: &Self) -> bool {
                if true {
                    {
                        match (&self.len(), &other.len()) {
                            (left_val, right_val) => {
                                if !(*left_val == *right_val) {
                                    let kind =
                                        ::core::panicking::AssertKind::Eq;
                                    ::core::panicking::assert_failed(kind,
                                                                     &*left_val,
                                                                     &*right_val,
                                                                     ::core::option::Option::None);
                                }
                            }
                        }
                    };
                };
                let mut changed = false;
                for (a, b) in iter::zip(self, other) { changed |= a.meet(b); }
                changed
            }
        }
        /// A `BitSet` represents the lattice formed by the powerset of all possible values of
        /// the index type `T` ordered by inclusion. Equivalently, it is a tuple of "two-point" lattices,
        /// one for each possible value of `T`.
        impl <T: Idx> JoinSemiLattice for BitSet<T> {
            fn join(&mut self, other: &Self) -> bool { self.union(other) }
        }
        impl <T: Idx> MeetSemiLattice for BitSet<T> {
            fn meet(&mut self, other: &Self) -> bool { self.intersect(other) }
        }
        pub struct SparseFlatSetMap<K: Hash + Eq + Idx, V: PartialEq +
                                    Clone> {
            pub element_sets: HashMap<K, V>,
            pub top_sets: BitSet<K>,
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl <K: ::core::clone::Clone + Hash + Eq + Idx,
              V: ::core::clone::Clone + PartialEq + Clone>
         ::core::clone::Clone for SparseFlatSetMap<K, V> {
            #[inline]
            fn clone(&self) -> SparseFlatSetMap<K, V> {
                match *self {
                    SparseFlatSetMap {
                    element_sets: ref __self_0_0, top_sets: ref __self_0_1 }
                    =>
                    SparseFlatSetMap{element_sets:
                                         ::core::clone::Clone::clone(&(*__self_0_0)),
                                     top_sets:
                                         ::core::clone::Clone::clone(&(*__self_0_1)),},
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl <K: ::core::fmt::Debug + Hash + Eq + Idx, V: ::core::fmt::Debug +
              PartialEq + Clone> ::core::fmt::Debug for SparseFlatSetMap<K, V>
         {
            fn fmt(&self, f: &mut ::core::fmt::Formatter)
             -> ::core::fmt::Result {
                match *self {
                    SparseFlatSetMap {
                    element_sets: ref __self_0_0, top_sets: ref __self_0_1 }
                    => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_struct(f,
                                                                      "SparseFlatSetMap");
                        let _ =
                            ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                            "element_sets",
                                                            &&(*__self_0_0));
                        let _ =
                            ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                            "top_sets",
                                                            &&(*__self_0_1));
                        ::core::fmt::DebugStruct::finish(debug_trait_builder)
                    }
                }
            }
        }
        impl <K: Hash + Eq + Idx, V: PartialEq + Clone>
         ::core::marker::StructuralPartialEq for SparseFlatSetMap<K, V> {
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl <K: ::core::cmp::PartialEq + Hash + Eq + Idx,
              V: ::core::cmp::PartialEq + PartialEq + Clone>
         ::core::cmp::PartialEq for SparseFlatSetMap<K, V> {
            #[inline]
            fn eq(&self, other: &SparseFlatSetMap<K, V>) -> bool {
                match *other {
                    SparseFlatSetMap {
                    element_sets: ref __self_1_0, top_sets: ref __self_1_1 }
                    =>
                    match *self {
                        SparseFlatSetMap {
                        element_sets: ref __self_0_0, top_sets: ref __self_0_1
                        } =>
                        (*__self_0_0) == (*__self_1_0) &&
                            (*__self_0_1) == (*__self_1_1),
                    },
                }
            }
            #[inline]
            fn ne(&self, other: &SparseFlatSetMap<K, V>) -> bool {
                match *other {
                    SparseFlatSetMap {
                    element_sets: ref __self_1_0, top_sets: ref __self_1_1 }
                    =>
                    match *self {
                        SparseFlatSetMap {
                        element_sets: ref __self_0_0, top_sets: ref __self_0_1
                        } =>
                        (*__self_0_0) != (*__self_1_0) ||
                            (*__self_0_1) != (*__self_1_1),
                    },
                }
            }
        }
        impl <K: Hash + Eq + Idx, V: PartialEq + Clone> SparseFlatSetMap<K, V>
         {
            pub fn new_empty(domain_size: usize) -> Self {
                Self{element_sets: HashMap::new(),
                     top_sets: BitSet::new_empty(domain_size),}
            }
            pub fn get_flat_set(&self, key: K) -> FlatSet<&V> {
                if self.top_sets.contains(key) {
                    FlatSet::Top
                } else if let Some(val) = self.element_sets.get(&key) {
                    FlatSet::Elem(val)
                } else { FlatSet::Bottom }
            }
            pub fn get_cloned_flat_set(&self, key: K) -> FlatSet<V> {
                if self.top_sets.contains(key) {
                    FlatSet::Top
                } else if let Some(val) = self.element_sets.get(&key) {
                    FlatSet::Elem(val.clone())
                } else { FlatSet::Bottom }
            }
            pub fn set_flat_set(&mut self, key: K, val: FlatSet<V>) -> bool {
                match val {
                    FlatSet::Top => {
                        let changed =
                            self.element_sets.remove(&key).is_some();
                        changed | self.top_sets.insert(key)
                    }
                    FlatSet::Elem(val) => {
                        let changed =
                            self.element_sets.insert(key, val).is_some();
                        changed | self.top_sets.remove(key)
                    }
                    FlatSet::Bottom => {
                        let changed =
                            self.element_sets.remove(&key).is_some();
                        changed | self.top_sets.remove(key)
                    }
                }
            }
            pub fn join_into(&self, key: K, dst: &mut FlatSet<V>) -> bool {
                match dst {
                    FlatSet::Bottom => {
                        if self.top_sets.contains(key) {
                            *dst = FlatSet::Top;
                            true
                        } else if let Some(val) = self.element_sets.get(&key)
                         {
                            *dst = FlatSet::Elem(val.clone());
                            true
                        } else { false }
                    }
                    FlatSet::Elem(x) => {
                        if self.top_sets.contains(key) {
                            *dst = FlatSet::Top;
                            true
                        } else {
                            match self.element_sets.get(&key) {
                                Some(val) if val != x => {
                                    *dst = FlatSet::Top;
                                    true
                                }
                                _ => false,
                            }
                        }
                    }
                    FlatSet::Top => false,
                }
            }
        }
        impl <K: Hash + Eq + Idx, V: PartialEq + Clone> JoinSemiLattice for
         SparseFlatSetMap<K, V> {
            fn join(&mut self, other: &Self) -> bool {
                let mut changed = self.top_sets.join(&other.top_sets);
                for (key, val) in &other.element_sets {
                    self.element_sets.entry(*key).or_insert_with(||
                                                                     {
                                                                         changed
                                                                             =
                                                                             true;
                                                                         val.clone()
                                                                     });
                }
                let top_sets = &mut self.top_sets;
                self.element_sets.retain(|key, val|
                                             match other.element_sets.get(key)
                                                 {
                                                 Some(other) if other != val
                                                 => {
                                                     top_sets.insert(*key);
                                                     changed = true;
                                                     false
                                                 }
                                                 Some(_) | None if
                                                 top_sets.contains(*key) =>
                                                 false,
                                                 Some(_) | None => true,
                                             });
                changed
            }
        }
        /// The counterpart of a given semilattice `T` using the [inverse order].
        ///
        /// The dual of a join-semilattice is a meet-semilattice and vice versa. For example, the dual of a
        /// powerset has the empty set as its top element and the full set as its bottom element and uses
        /// set *intersection* as its join operator.
        ///
        /// [inverse order]: https://en.wikipedia.org/wiki/Duality_(order_theory)
        pub struct Dual<T>(pub T);
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl <T: ::core::clone::Clone> ::core::clone::Clone for Dual<T> {
            #[inline]
            fn clone(&self) -> Dual<T> {
                match *self {
                    Dual(ref __self_0_0) =>
                    Dual(::core::clone::Clone::clone(&(*__self_0_0))),
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl <T: ::core::marker::Copy> ::core::marker::Copy for Dual<T> { }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl <T: ::core::fmt::Debug> ::core::fmt::Debug for Dual<T> {
            fn fmt(&self, f: &mut ::core::fmt::Formatter)
             -> ::core::fmt::Result {
                match *self {
                    Dual(ref __self_0_0) => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                     "Dual");
                        let _ =
                            ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                           &&(*__self_0_0));
                        ::core::fmt::DebugTuple::finish(debug_trait_builder)
                    }
                }
            }
        }
        impl <T> ::core::marker::StructuralPartialEq for Dual<T> { }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl <T: ::core::cmp::PartialEq> ::core::cmp::PartialEq for Dual<T> {
            #[inline]
            fn eq(&self, other: &Dual<T>) -> bool {
                match *other {
                    Dual(ref __self_1_0) =>
                    match *self {
                        Dual(ref __self_0_0) =>
                        (*__self_0_0) == (*__self_1_0),
                    },
                }
            }
            #[inline]
            fn ne(&self, other: &Dual<T>) -> bool {
                match *other {
                    Dual(ref __self_1_0) =>
                    match *self {
                        Dual(ref __self_0_0) =>
                        (*__self_0_0) != (*__self_1_0),
                    },
                }
            }
        }
        impl <T> ::core::marker::StructuralEq for Dual<T> { }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl <T: ::core::cmp::Eq> ::core::cmp::Eq for Dual<T> {
            #[inline]
            #[doc(hidden)]
            #[no_coverage]
            fn assert_receiver_is_total_eq(&self) -> () {
                { let _: ::core::cmp::AssertParamIsEq<T>; }
            }
        }
        impl <T> std::borrow::Borrow<T> for Dual<T> {
            fn borrow(&self) -> &T { &self.0 }
        }
        impl <T> std::borrow::BorrowMut<T> for Dual<T> {
            fn borrow_mut(&mut self) -> &mut T { &mut self.0 }
        }
        impl <T: MeetSemiLattice> JoinSemiLattice for Dual<T> {
            fn join(&mut self, other: &Self) -> bool { self.0.meet(&other.0) }
        }
        impl <T: JoinSemiLattice> MeetSemiLattice for Dual<T> {
            fn meet(&mut self, other: &Self) -> bool { self.0.join(&other.0) }
        }
        /// Extends a type `T` with top and bottom elements to make it a partially ordered set in which no
        /// value of `T` is comparable with any other. A flat set has the following [Hasse diagram]:
        ///
        /// ```text
        ///         top
        ///       / /  \ \
        /// all possible values of `T`
        ///       \ \  / /
        ///        bottom
        /// ```
        ///
        /// [Hasse diagram]: https://en.wikipedia.org/wiki/Hasse_diagram
        pub enum FlatSet<T: PartialEq> { Bottom, Elem(T), Top, }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl <T: ::core::clone::Clone + PartialEq> ::core::clone::Clone for
         FlatSet<T> {
            #[inline]
            fn clone(&self) -> FlatSet<T> {
                match (&*self,) {
                    (&FlatSet::Bottom,) => FlatSet::Bottom,
                    (&FlatSet::Elem(ref __self_0),) =>
                    FlatSet::Elem(::core::clone::Clone::clone(&(*__self_0))),
                    (&FlatSet::Top,) => FlatSet::Top,
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl <T: ::core::marker::Copy + PartialEq> ::core::marker::Copy for
         FlatSet<T> {
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl <T: ::core::fmt::Debug + PartialEq> ::core::fmt::Debug for
         FlatSet<T> {
            fn fmt(&self, f: &mut ::core::fmt::Formatter)
             -> ::core::fmt::Result {
                match (&*self,) {
                    (&FlatSet::Bottom,) => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                     "Bottom");
                        ::core::fmt::DebugTuple::finish(debug_trait_builder)
                    }
                    (&FlatSet::Elem(ref __self_0),) => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                     "Elem");
                        let _ =
                            ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                           &&(*__self_0));
                        ::core::fmt::DebugTuple::finish(debug_trait_builder)
                    }
                    (&FlatSet::Top,) => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                     "Top");
                        ::core::fmt::DebugTuple::finish(debug_trait_builder)
                    }
                }
            }
        }
        impl <T: PartialEq> ::core::marker::StructuralPartialEq for FlatSet<T>
         {
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl <T: ::core::cmp::PartialEq + PartialEq> ::core::cmp::PartialEq
         for FlatSet<T> {
            #[inline]
            fn eq(&self, other: &FlatSet<T>) -> bool {
                {
                    let __self_vi =
                        ::core::intrinsics::discriminant_value(&*self);
                    let __arg_1_vi =
                        ::core::intrinsics::discriminant_value(&*other);
                    if true && __self_vi == __arg_1_vi {
                        match (&*self, &*other) {
                            (&FlatSet::Elem(ref __self_0),
                             &FlatSet::Elem(ref __arg_1_0)) =>
                            (*__self_0) == (*__arg_1_0),
                            _ => true,
                        }
                    } else { false }
                }
            }
            #[inline]
            fn ne(&self, other: &FlatSet<T>) -> bool {
                {
                    let __self_vi =
                        ::core::intrinsics::discriminant_value(&*self);
                    let __arg_1_vi =
                        ::core::intrinsics::discriminant_value(&*other);
                    if true && __self_vi == __arg_1_vi {
                        match (&*self, &*other) {
                            (&FlatSet::Elem(ref __self_0),
                             &FlatSet::Elem(ref __arg_1_0)) =>
                            (*__self_0) != (*__arg_1_0),
                            _ => false,
                        }
                    } else { true }
                }
            }
        }
        impl <T: PartialEq> ::core::marker::StructuralEq for FlatSet<T> { }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl <T: ::core::cmp::Eq + PartialEq> ::core::cmp::Eq for FlatSet<T> {
            #[inline]
            #[doc(hidden)]
            #[no_coverage]
            fn assert_receiver_is_total_eq(&self) -> () {
                { let _: ::core::cmp::AssertParamIsEq<T>; }
            }
        }
        impl <T: Clone + PartialEq> JoinSemiLattice for FlatSet<T> {
            fn join(&mut self, other: &Self) -> bool {
                let result =
                    match (&*self, other) {
                        (Self::Top, _) | (_, Self::Bottom) => return false,
                        (Self::Elem(a), Self::Elem(b)) if a == b =>
                        return false,
                        (Self::Bottom, Self::Elem(x)) =>
                        Self::Elem(x.clone()),
                        _ => Self::Top,
                    };
                *self = result;
                true
            }
        }
        impl <T: Clone + Eq> MeetSemiLattice for FlatSet<T> {
            fn meet(&mut self, other: &Self) -> bool {
                let result =
                    match (&*self, other) {
                        (Self::Bottom, _) | (_, Self::Top) => return false,
                        (Self::Elem(ref a), Self::Elem(ref b)) if a == b =>
                        return false,
                        (Self::Top, Self::Elem(ref x)) =>
                        Self::Elem(x.clone()),
                        _ => Self::Bottom,
                    };
                *self = result;
                true
            }
        }
        impl <T: PartialEq> FlatSet<T> {
            pub fn to_option(&self) -> Option<&T> {
                if let Self::Elem(val) = self { Some(val) } else { None }
            }
            pub fn into_option(self) -> Option<T> {
                if let Self::Elem(val) = self { Some(val) } else { None }
            }
            pub fn map<Y: PartialEq>(self, f: impl FnOnce(T) -> Y)
             -> FlatSet<Y> {
                match self {
                    Self::Elem(x) => FlatSet::Elem(f(x)),
                    Self::Top => FlatSet::Top,
                    Self::Bottom => FlatSet::Bottom,
                }
            }
            pub fn and_then<Y: PartialEq>(self,
                                          f: impl FnOnce(T) -> FlatSet<Y>)
             -> FlatSet<Y> {
                match self {
                    Self::Elem(x) => f(x),
                    Self::Top => FlatSet::Top,
                    Self::Bottom => FlatSet::Bottom,
                }
            }
            pub fn apply_binary_op(self, other: Self,
                                   f: impl FnOnce(T, T) -> T) -> Self {
                match (self, other) {
                    (Self::Elem(arg1), Self::Elem(arg2)) =>
                    FlatSet::Elem(f(arg1, arg2)),
                    (Self::Top, _) | (_, Self::Top) => FlatSet::Top,
                    _ => FlatSet::Bottom,
                }
            }
            pub fn map_bottom(self, f: impl FnOnce() -> Self) -> Self {
                match self { Self::Bottom => f(), res => res, }
            }
        }
        impl <T: PartialEq + Clone> FlatSet<T> {
            pub fn join_elem(&mut self, val: &T) -> bool {
                match self {
                    Self::Elem(x) if &*x == val => false,
                    Self::Top => false,
                    Self::Bottom => { *self = Self::Elem(val.clone()); true }
                    dst => { *dst = Self::Top; true }
                }
            }
        }
        impl <T: Debug + PartialEq> FlatSet<T> {
            pub fn expect_elem(self, msg: &'static str) -> T {
                if let Self::Elem(val) = self {
                    val
                } else {
                    {
                        ::std::rt::begin_panic_fmt(&::core::fmt::Arguments::new_v1(&["Expected a value found ",
                                                                                     ": "],
                                                                                   &match (&self,
                                                                                           &msg)
                                                                                        {
                                                                                        (arg0,
                                                                                         arg1)
                                                                                        =>
                                                                                        [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                      ::core::fmt::Debug::fmt),
                                                                                         ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                      ::core::fmt::Display::fmt)],
                                                                                    }))
                    }
                }
            }
            pub fn unwrap(self) -> T {
                if let Self::Elem(val) = self {
                    val
                } else {
                    {
                        ::std::rt::begin_panic_fmt(&::core::fmt::Arguments::new_v1(&["Expected a constant value bound found ",
                                                                                     "!"],
                                                                                   &match (&self,)
                                                                                        {
                                                                                        (arg0,)
                                                                                        =>
                                                                                        [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                      ::core::fmt::Debug::fmt)],
                                                                                    }))
                    }
                }
            }
        }
        impl <T: Clone + PartialEq> FlatSet<&T> {
            pub fn cloned(&self) -> FlatSet<T> {
                match *self {
                    FlatSet::Bottom => FlatSet::Bottom,
                    FlatSet::Elem(x) => FlatSet::Elem(x.clone()),
                    FlatSet::Top => FlatSet::Top,
                }
            }
        }
    }
    pub mod visitor {
        use super::{Analysis, Direction, Results};
        use crate::cfg::{BasicBlock, BasicBlockData, ControlFlowGraph, Phi,
                         PhiData, Terminator};
        use crate::{CfgFunctions, Statement, StatementId};
        /// Calls the corresponding method in `ResultsVisitor` for every location in a ControlFlow Graph with the
        /// dataflow state at that location.
        pub fn visit_results<'a, C, F,
                             V>(cfg: &'a ControlFlowGraph<C>,
                                blocks:
                                    impl IntoIterator<Item =
                                                      (BasicBlock,
                                                       &'a BasicBlockData<C>)>,
                                results: &V,
                                vis:
                                    &mut impl ResultsVisitor<C, FlowState =
                                                             F>) where
         C: CfgFunctions, V: ResultsVisitable<C, FlowState = F> {
            let mut state = results.new_flow_state(cfg);
            for (block, block_data) in blocks {
                V::Direction::visit_results_in_block(&mut state, cfg, block,
                                                     block_data, results,
                                                     vis);
            }
        }
        /// Calls the corresponding method in `ResultsVisitor` for every location in a ControlFlow Graph with the
        /// dataflow state at that location.
        pub fn visit_results_mut<C, F,
                                 V>(cfg: &mut ControlFlowGraph<C>,
                                    blocks:
                                        impl IntoIterator<Item = BasicBlock>,
                                    results: &V,
                                    vis:
                                        &mut impl ResultsVisitorMut<C,
                                                                    FlowState
                                                                    = F>)
         where C: CfgFunctions, V: ResultsVisitable<C, FlowState = F> {
            let mut state = results.new_flow_state(cfg);
            for block in blocks {
                V::Direction::visit_results_in_block_mut(&mut state, cfg,
                                                         block, results, vis);
            }
        }
        pub trait ResultsVisitor<C: CfgFunctions> {
            type FlowState;
            #[inline(always)]
            fn visit_block_start(&mut self, _state: &Self::FlowState,
                                 _block_data: &BasicBlockData<C>,
                                 _block: BasicBlock) {
            }
            #[inline(always)]
            fn visit_phi_before_effect(&mut self, _state: &Self::FlowState,
                                       _stmnt: &PhiData, _block: BasicBlock,
                                       _id: Phi) {
            }
            #[inline(always)]
            fn visit_phi_after_effect(&mut self, _state: &Self::FlowState,
                                      _stmnt: &PhiData, _block: BasicBlock,
                                      _id: Phi) {
            }
            #[inline(always)]
            fn visit_statement_before_effect(&mut self,
                                             _state: &Self::FlowState,
                                             _stmnt: &Statement<C>,
                                             _block: BasicBlock,
                                             _id: StatementId) {
            }
            #[inline(always)]
            fn visit_statement_after_effect(&mut self,
                                            _state: &Self::FlowState,
                                            _stmnt: &Statement<C>,
                                            _block: BasicBlock,
                                            _id: StatementId) {
            }
            #[inline(always)]
            fn visit_terminator_after_effect(&mut self,
                                             _state: &Self::FlowState,
                                             _term: &Terminator<C>,
                                             _block: BasicBlock) {
            }
            #[inline(always)]
            fn visit_terminator_before_effect(&mut self,
                                              _state: &Self::FlowState,
                                              _term: &Terminator<C>,
                                              _block: BasicBlock) {
            }
            #[inline(always)]
            fn visit_block_end(&mut self, _state: &Self::FlowState,
                               _block_data: &BasicBlockData<C>,
                               _block: BasicBlock) {
            }
        }
        pub trait ResultsVisitorMut<C: CfgFunctions> {
            type FlowState;
            #[inline(always)]
            fn visit_block_start(&mut self, _state: &Self::FlowState,
                                 _block_data: &mut BasicBlockData<C>,
                                 _block: BasicBlock) {
            }
            #[inline(always)]
            fn visit_phi_before_effect(&mut self, _state: &Self::FlowState,
                                       _stmnt: &mut PhiData,
                                       _block: BasicBlock, _id: Phi) {
            }
            #[inline(always)]
            fn visit_phi_after_effect(&mut self, _state: &Self::FlowState,
                                      _stmnt: &mut PhiData,
                                      _block: BasicBlock, _id: Phi) {
            }
            #[inline(always)]
            fn visit_statement_before_effect(&mut self,
                                             _state: &Self::FlowState,
                                             _stmnt: &mut Statement<C>,
                                             _block: BasicBlock,
                                             _id: StatementId) {
            }
            #[inline(always)]
            fn visit_statement_after_effect(&mut self,
                                            _state: &Self::FlowState,
                                            _stmnt: &mut Statement<C>,
                                            _block: BasicBlock,
                                            _id: StatementId) {
            }
            #[inline(always)]
            fn visit_terminator_before_effect(&self, _state: &Self::FlowState,
                                              _term: &mut Terminator<C>,
                                              _block: BasicBlock) {
            }
            #[inline(always)]
            fn visit_terminator_after_effect(&self, _state: &Self::FlowState,
                                             _term: &mut Terminator<C>,
                                             _block: BasicBlock) {
            }
            #[inline(always)]
            fn visit_block_end(&mut self, _state: &Self::FlowState,
                               _block_data: &mut BasicBlockData<C>,
                               _block: BasicBlock) {
            }
        }
        /// Things that can be visited by a `ResultsVisitor`.
        ///
        /// This trait exists so that we can visit the results of multiple dataflow analyses simultaneously.
        /// DO NOT IMPLEMENT MANUALLY. Instead, use the `impl_visitable` macro below.
        pub trait ResultsVisitable<C: CfgFunctions> {
            type Direction: Direction;
            type FlowState;
            /// Creates an empty `FlowState` to hold the transient state for these dataflow results.
            ///
            /// The value of the newly created `FlowState` will be overwritten by `reset_to_block_entry`
            /// before it can be observed by a `ResultsVisitor`.
            fn new_flow_state(&self, cfg: &ControlFlowGraph<C>)
            -> Self::FlowState;
            fn reset_to_block_entry(&self, cfg: &ControlFlowGraph<C>,
                                    state: &mut Self::FlowState,
                                    block: BasicBlock);
            fn reconstruct_phi_effect(&self, cfg: &ControlFlowGraph<C>,
                                      state: &mut Self::FlowState,
                                      phi: &PhiData, bb: BasicBlock, id: Phi);
            fn reconstruct_statement_effect(&self, cfg: &ControlFlowGraph<C>,
                                            state: &mut Self::FlowState,
                                            statement: &Statement<C>,
                                            bb: BasicBlock, id: StatementId);
            fn reconstruct_terminator_effect(&self, cfg: &ControlFlowGraph<C>,
                                             state: &mut Self::FlowState,
                                             terminator: &Terminator<C>,
                                             bb: BasicBlock);
        }
        impl <C: CfgFunctions, A> ResultsVisitable<C> for Results<C, A> where
         A: Analysis<C> {
            type Direction = A::Direction;
            type FlowState = A::Domain;
            fn new_flow_state(&self, cfg: &ControlFlowGraph<C>)
             -> Self::FlowState {
                self.analysis.bottom_value(cfg)
            }
            fn reset_to_block_entry(&self, cfg: &ControlFlowGraph<C>,
                                    state: &mut Self::FlowState,
                                    block: BasicBlock) {
                state.clone_from(&self.entry_set_for_block(block));
                self.analysis.init_block(cfg, state)
            }
            fn reconstruct_phi_effect(&self, cfg: &ControlFlowGraph<C>,
                                      state: &mut Self::FlowState,
                                      phi: &PhiData, bb: BasicBlock,
                                      id: Phi) {
                self.analysis.apply_phi_effect(cfg, state, phi, bb, id);
            }
            fn reconstruct_statement_effect(&self, cfg: &ControlFlowGraph<C>,
                                            state: &mut Self::FlowState,
                                            statement: &Statement<C>,
                                            bb: BasicBlock, id: StatementId) {
                self.analysis.apply_statement_effect(cfg, state, statement,
                                                     id, bb);
            }
            fn reconstruct_terminator_effect(&self, cfg: &ControlFlowGraph<C>,
                                             state: &mut Self::FlowState,
                                             terminator: &Terminator<C>,
                                             bb: BasicBlock) {
                self.analysis.apply_terminator_effect(cfg, state, terminator,
                                                      bb);
            }
        }
    }
    /// Define the domain of a dataflow problem.
    ///
    /// This trait specifies the lattice on which this analysis operates (the domain) as well as its
    /// initial value at the entry point of each basic block.
    pub trait AnalysisDomain<C: CfgFunctions> {
        /// The type that holds the dataflow state at any given point in the program.
        type Domain: Clone + JoinSemiLattice + Debug;
        /// The direction of this analysis. Either `Forward` or `Backward`.
        type Direction: Direction;
        /// A descriptive name for this analysis. Used only for debugging.
        ///
        /// This name should be brief and contain no spaces, periods or other characters that are not
        /// suitable as part of a filename.
        const NAME: &'static str ;
        /// The initial value of the dataflow state upon entry to each basic block.
        fn bottom_value(&self, cfg: &ControlFlowGraph<C>)
        -> Self::Domain;
        /// Mutates the initial value of the dataflow state upon entry to the `START_BLOCK`.
        ///
        /// For backward analyses, initial state besides the bottom value is not yet supported. Trying
        /// to mutate the initial state will result in a panic.
        fn initialize_start_block(&self, cfg: &ControlFlowGraph<C>,
                                  state: &mut Self::Domain);
    }
    /// Define the domain of a dataflow problem.
    ///
    /// This trait specifies the lattice on which this analysis operates (the domain) as well as its
    /// initial value at the entry point of each basic block.
    pub trait GenKillAnalysisDomain<C: CfgFunctions> {
        /// The type that holds the dataflow state at any given point in the program.
        type Domain: Clone + JoinSemiLattice + GenKill<Self::Idx> +
         BorrowMut<BitSet<Self::Idx>> + Debug;
        type Idx: Idx;
        /// The direction of this analysis. Either `Forward` or `Backward`.
        type Direction: Direction;
        /// A descriptive name for this analysis. Used only for debugging.
        ///
        /// This name should be brief and contain no spaces, periods or other characters that are not
        /// suitable as part of a filename.
        const NAME: &'static str ;
        /// The initial value of the dataflow state upon entry to each basic block.
        fn bottom_value(&self, cfg: &ControlFlowGraph<C>)
        -> Self::Domain;
        /// Mutates the initial value of the dataflow state upon entry to the `START_BLOCK`.
        ///
        /// For backward analyses, initial state besides the bottom value is not yet supported. Trying
        /// to mutate the initial state will result in a panic.
        fn initialize_start_block(&self, cfg: &ControlFlowGraph<C>,
                                  state: &mut Self::Domain);
        fn domain_size(&self, cfg: &ControlFlowGraph<C>)
        -> usize;
    }
    /// A dataflow problem with an arbitrarily complex transfer function.
    ///
    /// # Convergence
    ///
    /// When implementing this trait directly (not via [`GenKillAnalysis`]), it's possible to choose a
    /// transfer function such that the analysis does not reach fixpoint. To guarantee convergence,
    /// your transfer functions must maintain the following invariant:
    ///
    /// > If the dataflow state **before** some point in the program changes to be greater
    /// than the prior state **before** that point, the dataflow state **after** that point must
    /// also change to be greater than the prior state **after** that point.
    ///
    /// This invariant guarantees that the dataflow state at a given point in the program increases
    /// monotonically until fixpoint is reached. Note that this monotonicity requirement only applies
    /// to the same point in the program at different points in time. The dataflow state at a given
    /// point in the program may or may not be greater than the state at any preceding point.
    pub trait Analysis<C: CfgFunctions>: AnalysisDomain<C> {
        /// Init the state of block before the other analysis functions are called.
        #[inline(always)]
        fn init_block(&self, _cfg: &ControlFlowGraph<C>,
                      _state: &mut Self::Domain) {
        }
        /// Updates the current dataflow state with the effect of evaluating a phi.
        #[inline(always)]
        fn apply_phi_effect(&self, _cfg: &ControlFlowGraph<C>,
                            _state: &mut Self::Domain, _phi: &PhiData,
                            _bb: BasicBlock, _idx: Phi) {
        }
        /// Updates the current dataflow state with the effect of evaluating a statement.
        #[inline(always)]
        fn apply_statement_effect(&self, _cfg: &ControlFlowGraph<C>,
                                  _state: &mut Self::Domain,
                                  _statement: &Statement<C>,
                                  _idx: StatementId, _bb: BasicBlock) {
        }
        /// Updates the current dataflow state with the effect of evaluating a terminator.
        #[inline(always)]
        fn apply_terminator_effect(&self, _cfg: &ControlFlowGraph<C>,
                                   _state: &mut Self::Domain,
                                   _terminator: &Terminator<C>,
                                   _bb: BasicBlock) {
        }
        /// Updates the current dataflow state with the effect of taking a particular branch in a
        /// `Split` terminator.
        ///
        /// Unlike the other edge-specific effects, which are allowed to mutate `Self::Domain`
        /// directly, overriders of this method should simply determine whether join should be performed along this edge
        ///
        /// FIXME: This class of effects is not supported for backward dataflow analyses.
        #[inline(always)]
        fn apply_edge_effects(&self, _cfg: &ControlFlowGraph<C>,
                              _block: BasicBlock, _state: &Self::Domain)
         -> bool {
            true
        }
        /// Updates the current dataflow state with the effect of taking a particular branch in a
        /// `Split` terminator.
        ///
        /// Unlike the other edge-specific effects, which are allowed to mutate `Self::Domain`
        /// directly, overriders of this method should simply determine whether join should be performed along this edge
        ///
        /// FIXME: This class of effects is not supported for backward dataflow analyses.
        #[inline(always)]
        fn apply_split_edge_effects(&self, _cfg: &ControlFlowGraph<C>,
                                    _block: BasicBlock, _discr: &RValue<C>,
                                    _state: &Self::Domain,
                                    _edge_effects:
                                        &mut impl SplitEdgeEffects<Self::Domain>) {
        }
        /// Creates an `Engine` to find the fixpoint for this dataflow problem.
        ///
        /// You shouldn't need to override this outside this module, since the combination of the
        /// default impl and the one for all `A: GenKillAnalysis` will do the right thing.
        /// Its purpose is to enable method chaining like so:
        ///
        /// ```ignore (cross-crate-imports)
        /// let results = MyAnalysis::new(tcx, body)
        ///     .into_engine(tcx, body, def_id)
        ///     .iterate_to_fixpoint()
        ///     .into_results_cursor(body);
        /// ```
        fn into_engine(self, cfg: &ControlFlowGraph<C>) -> Engine<C, Self>
         where Self: Sized {
            Engine::new_generic(cfg, self)
        }
    }
    /// A gen/kill dataflow problem.
    ///
    /// Each method in this trait has a corresponding one in `Analysis`. However, these methods only
    /// allow modification of the dataflow state via "gen" and "kill" operations. By defining transfer
    /// functions for each statement in this way, the transfer function for an entire basic block can
    /// be computed efficiently.
    ///
    /// `Analysis` is automatically implemented for all implementers of `GenKillAnalysis`.
    pub trait GenKillAnalysis<C: CfgFunctions>: GenKillAnalysisDomain<C> {
        /// Updates the current dataflow state with the effect of evaluating a phi.
        fn phi_effect(&self, _cfg: &ControlFlowGraph<C>,
                      _trans: &mut impl GenKill<Self::Idx>, _phi: &PhiData,
                      _bb: BasicBlock, _idx: Phi) {
        }
        /// Updates the current dataflow state with the effect of evaluating a statement.
        fn statement_effect(&self, _cfg: &ControlFlowGraph<C>,
                            _trans: &mut impl GenKill<Self::Idx>,
                            _statement: &Statement<C>, _idx: StatementId,
                            _bb: BasicBlock) {
        }
        /// Updates the current dataflow state with the effect of evaluating a terminator.
        fn terminator_effect(&self, _cfg: &ControlFlowGraph<C>,
                             _trans: &mut impl GenKill<Self::Idx>,
                             _terminator: &Terminator<C>, _bb: BasicBlock) {
        }
        fn into_engine(self, cfg: &ControlFlowGraph<C>)
         -> Engine<C, GenKillAnalysisImpl<Self>> where Self: Sized {
            Engine::new_gen_kill(cfg, self)
        }
    }
    pub struct GenKillAnalysisImpl<A>(A);
    impl <A, C> AnalysisDomain<C> for GenKillAnalysisImpl<A> where
     C: CfgFunctions, A: GenKillAnalysisDomain<C> {
        type Domain = A::Domain;
        type Direction = A::Direction;
        const NAME: &'static str = A::NAME;
        #[inline(always)]
        fn bottom_value(&self, cfg: &ControlFlowGraph<C>) -> Self::Domain {
            self.0.bottom_value(cfg)
        }
        #[inline(always)]
        fn initialize_start_block(&self, cfg: &ControlFlowGraph<C>,
                                  state: &mut Self::Domain) {
            self.0.initialize_start_block(cfg, state)
        }
    }
    impl <A, C> Analysis<C> for GenKillAnalysisImpl<A> where C: CfgFunctions,
     A: GenKillAnalysis<C> {
        #[inline(always)]
        /// Updates the current dataflow state with the effect of evaluating a phi.
        fn apply_phi_effect(&self, cfg: &ControlFlowGraph<C>,
                            state: &mut Self::Domain, phi: &PhiData,
                            bb: BasicBlock, idx: Phi) {
            self.0.phi_effect(cfg, state, phi, bb, idx)
        }
        #[inline(always)]
        /// Updates the current dataflow state with the effect of evaluating a statement.
        fn apply_statement_effect(&self, cfg: &ControlFlowGraph<C>,
                                  state: &mut Self::Domain,
                                  statement: &Statement<C>, idx: StatementId,
                                  bb: BasicBlock) {
            self.0.statement_effect(cfg, state, statement, idx, bb)
        }
        #[inline(always)]
        /// Updates the current dataflow state with the effect of evaluating a terminator.
        fn apply_terminator_effect(&self, cfg: &ControlFlowGraph<C>,
                                   state: &mut Self::Domain,
                                   terminator: &Terminator<C>,
                                   bb: BasicBlock) {
            self.0.terminator_effect(cfg, state, terminator, bb)
        }
        #[inline(always)]
        fn into_engine(self, cfg: &ControlFlowGraph<C>) -> Engine<C, Self>
         where Self: Sized {
            Engine::new_gen_kill(cfg, self.0)
        }
    }
    /// The legal operations for a transfer function in a gen/kill problem.
    ///
    /// This abstraction exists because there are two different contexts in which we call the methods in
    /// `GenKillAnalysis`. Sometimes we need to store a single transfer function that can be efficiently
    /// applied multiple times, such as when computing the cumulative transfer function for each block.
    /// These cases require a `GenKillSet`, which in turn requires two `BitSet`s of storage. Oftentimes,
    /// however, we only need to apply an effect once. In *these* cases, it is more efficient to pass the
    /// `BitSet` representing the state vector directly into the `*_effect` methods as opposed to
    /// building up a `GenKillSet` and then throwing it away.
    pub trait GenKill<T: Idx>: Debug {
        /// Inserts `elem` into the state vector.
        fn gen(&mut self, elem: T);
        /// Removes `elem` from the state vector.
        fn kill(&mut self, elem: T);
        /// Calls `gen` for each element in `elems`.
        fn gen_all(&mut self, elems: impl IntoIterator<Item = T>) {
            for elem in elems { self.gen(elem); }
        }
        /// Calls `kill` for each element in `elems`.
        fn kill_all(&mut self, elems: impl IntoIterator<Item = T>) {
            for elem in elems { self.kill(elem); }
        }
        /// Calls `gen` for each element in `elems`.
        fn gen_set(&mut self, elems: &impl FullBitSetOperations<T>);
        /// Calls `kill` for each element in `elems`.
        fn kill_set(&mut self, elems: &impl FullBitSetOperations<T>);
    }
    /// Stores a transfer function for a gen/kill problem.
    ///
    /// Calling `gen`/`kill` on a `GenKillSet` will "build up" a transfer function so that it can be
    /// applied multiple times efficiently. When there are multiple calls to `gen` and/or `kill` for
    /// the same element, the most recent one takes precedence.
    pub struct GenKillSet<T: Idx> {
        gen: HybridBitSet<T>,
        kill: HybridBitSet<T>,
        domain_size: usize,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl <T: ::core::clone::Clone + Idx> ::core::clone::Clone for
     GenKillSet<T> {
        #[inline]
        fn clone(&self) -> GenKillSet<T> {
            match *self {
                GenKillSet {
                gen: ref __self_0_0,
                kill: ref __self_0_1,
                domain_size: ref __self_0_2 } =>
                GenKillSet{gen: ::core::clone::Clone::clone(&(*__self_0_0)),
                           kill: ::core::clone::Clone::clone(&(*__self_0_1)),
                           domain_size:
                               ::core::clone::Clone::clone(&(*__self_0_2)),},
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl <T: ::core::fmt::Debug + Idx> ::core::fmt::Debug for GenKillSet<T> {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {
                GenKillSet {
                gen: ref __self_0_0,
                kill: ref __self_0_1,
                domain_size: ref __self_0_2 } => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_struct(f,
                                                                  "GenKillSet");
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "gen",
                                                        &&(*__self_0_0));
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "kill",
                                                        &&(*__self_0_1));
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "domain_size",
                                                        &&(*__self_0_2));
                    ::core::fmt::DebugStruct::finish(debug_trait_builder)
                }
            }
        }
    }
    impl <T: Idx> GenKillSet<T> {
        /// Creates a new transfer function that will leave the dataflow state unchanged.
        pub fn identity(domain_size: usize) -> Self {
            GenKillSet{gen: HybridBitSet::new_empty(),
                       kill: HybridBitSet::new_empty(),
                       domain_size,}
        }
        pub fn apply(&self, state: &mut BitSet<T>) {
            state.union(&self.gen);
            state.subtract(&self.kill);
        }
    }
    impl <T: Idx> GenKill<T> for GenKillSet<T> {
        fn gen(&mut self, elem: T) {
            self.gen.insert(elem, self.domain_size);
            self.kill.remove(elem);
        }
        fn kill(&mut self, elem: T) {
            self.kill.insert(elem, self.domain_size);
            self.gen.remove(elem);
        }
        fn gen_set(&mut self, elems: &impl FullBitSetOperations<T>) {
            self.kill.subtract(elems);
            self.gen.union(elems, self.domain_size);
        }
        fn kill_set(&mut self, elems: &impl FullBitSetOperations<T>) {
            self.kill.union(elems, self.domain_size);
            self.gen.subtract(elems);
        }
    }
    impl <T: Idx> GenKill<T> for BitSet<T> {
        fn gen(&mut self, elem: T) { self.insert(elem); }
        fn kill(&mut self, elem: T) { self.remove(elem); }
        fn gen_set(&mut self, elems: &impl FullBitSetOperations<T>) {
            self.union(elems);
        }
        fn kill_set(&mut self, elems: &impl FullBitSetOperations<T>) {
            self.subtract(elems);
        }
    }
    impl <T: Idx> GenKill<T> for lattice::Dual<BitSet<T>> {
        fn gen(&mut self, elem: T) { self.0.insert(elem); }
        fn kill(&mut self, elem: T) { self.0.remove(elem); }
        fn gen_set(&mut self, elems: &impl FullBitSetOperations<T>) {
            self.0.union(elems);
        }
        fn kill_set(&mut self, elems: &impl FullBitSetOperations<T>) {
            self.0.subtract(elems);
        }
    }
    pub enum Effect { Before, After, }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for Effect {
        #[inline]
        fn clone(&self) -> Effect { { *self } }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for Effect { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for Effect {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match (&*self,) {
                (&Effect::Before,) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "Before");
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&Effect::After,) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "After");
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for Effect { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for Effect {
        #[inline]
        fn eq(&self, other: &Effect) -> bool {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) { _ => true, }
                } else { false }
            }
        }
    }
    impl ::core::marker::StructuralEq for Effect { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for Effect {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () { { } }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialOrd for Effect {
        #[inline]
        fn partial_cmp(&self, other: &Effect)
         -> ::core::option::Option<::core::cmp::Ordering> {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        _ =>
                        ::core::option::Option::Some(::core::cmp::Ordering::Equal),
                    }
                } else {
                    ::core::cmp::PartialOrd::partial_cmp(&__self_vi,
                                                         &__arg_1_vi)
                }
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Ord for Effect {
        #[inline]
        fn cmp(&self, other: &Effect) -> ::core::cmp::Ordering {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        _ => ::core::cmp::Ordering::Equal,
                    }
                } else { ::core::cmp::Ord::cmp(&__self_vi, &__arg_1_vi) }
            }
        }
    }
    impl Effect {
        pub const fn at_index(self, idx: usize) -> EffectIndex {
            EffectIndex{effect: self, idx,}
        }
        pub fn at_location<C: CfgFunctions>(self, loc: Location,
                                            cfg: &ControlFlowGraph<C>)
         -> EffectIndex {
            let idx =
                match loc.kind {
                    LocationKind::Phi(x) => x.index(),
                    LocationKind::Statement(x) =>
                    x.index() + cfg.blocks[loc.block].phi_statements.len(),
                    LocationKind::Terminator => {
                        cfg.blocks[loc.block].phi_statements.len() +
                            cfg.blocks[loc.block].statements.len()
                    }
                };
            self.at_index(idx)
        }
    }
    pub struct EffectIndex {
        idx: usize,
        effect: Effect,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for EffectIndex {
        #[inline]
        fn clone(&self) -> EffectIndex {
            {
                let _: ::core::clone::AssertParamIsClone<usize>;
                let _: ::core::clone::AssertParamIsClone<Effect>;
                *self
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for EffectIndex { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for EffectIndex {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {
                EffectIndex { idx: ref __self_0_0, effect: ref __self_0_1 } =>
                {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_struct(f,
                                                                  "EffectIndex");
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "idx",
                                                        &&(*__self_0_0));
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "effect",
                                                        &&(*__self_0_1));
                    ::core::fmt::DebugStruct::finish(debug_trait_builder)
                }
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for EffectIndex { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for EffectIndex {
        #[inline]
        fn eq(&self, other: &EffectIndex) -> bool {
            match *other {
                EffectIndex { idx: ref __self_1_0, effect: ref __self_1_1 } =>
                match *self {
                    EffectIndex { idx: ref __self_0_0, effect: ref __self_0_1
                    } =>
                    (*__self_0_0) == (*__self_1_0) &&
                        (*__self_0_1) == (*__self_1_1),
                },
            }
        }
        #[inline]
        fn ne(&self, other: &EffectIndex) -> bool {
            match *other {
                EffectIndex { idx: ref __self_1_0, effect: ref __self_1_1 } =>
                match *self {
                    EffectIndex { idx: ref __self_0_0, effect: ref __self_0_1
                    } =>
                    (*__self_0_0) != (*__self_1_0) ||
                        (*__self_0_1) != (*__self_1_1),
                },
            }
        }
    }
    impl ::core::marker::StructuralEq for EffectIndex { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for EffectIndex {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            {
                let _: ::core::cmp::AssertParamIsEq<usize>;
                let _: ::core::cmp::AssertParamIsEq<Effect>;
            }
        }
    }
    impl EffectIndex {
        fn next_in_forward_order(self) -> Self {
            match self.effect {
                Effect::Before => Effect::After.at_index(self.idx),
                Effect::After => Effect::Before.at_index(self.idx + 1),
            }
        }
        fn next_in_backward_order(self) -> Self {
            match self.effect {
                Effect::Before => Effect::After.at_index(self.idx),
                Effect::After => Effect::Before.at_index(self.idx - 1),
            }
        }
        /// Returns `true` if the effect at `self` should be applied earlier than the effect at `other`
        /// in forward order.
        fn precedes_in_forward_order(self, other: Self) -> bool {
            let ord =
                self.idx.cmp(&other.idx).then_with(||
                                                       self.effect.cmp(&other.effect));
            ord == Ordering::Less
        }
        /// Returns `true` if the effect at `self` should be applied earlier than the effect at `other`
        /// in backward order.
        fn precedes_in_backward_order(self, other: Self) -> bool {
            let ord =
                other.idx.cmp(&self.idx).then_with(||
                                                       self.effect.cmp(&other.effect));
            ord == Ordering::Less
        }
    }
    /// A type that records the edge-specific effects for a `SwitchInt` terminator.
    pub trait SplitEdgeEffects<D> {
        /// Calls `apply_edge_effect` for each outgoing edge from a `SwitchInt` terminator and
        /// records the results.
        fn apply(&mut self,
                 apply_edge_effect:
                     impl FnMut(&mut D, BasicBlock, bool) -> bool);
    }
}
mod fold {
    use crate::{BinOp, COperand, CallArg, CfgFunctions, ComparisonOp, Math1,
                Math2, RValue, Type};
    use data_structures::index_vec::IndexSlice;
    use ir::UnaryOperator;
    use session::sourcemap::Span;
    #[inline]
    pub fn fold_rvalue<C: CfgFunctions,
                       F: RValueFold<C>>(fold: &mut F, rvalue: &RValue<C>,
                                         ty: Type) -> F::T {
        match *rvalue {
            RValue::UnaryOperation(op, ref arg) =>
            match op.contents {
                UnaryOperator::ArithmeticNegate if ty == Type::REAL => {
                    fold.fold_real_arith_negate(op.span, arg)
                }
                UnaryOperator::ArithmeticNegate if ty == Type::INT => {
                    fold.fold_int_arith_negate(op.span, arg)
                }
                UnaryOperator::ArithmeticNegate if ty == Type::CMPLX => {
                    fold.fold_cmplx_arith_negate(op.span, arg)
                }
                UnaryOperator::ArithmeticNegate => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                    &match (&"Misstyped MIR",)
                                                                                         {
                                                                                         (arg0,)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt)],
                                                                                     }))
                    }
                }
                UnaryOperator::BitNegate =>
                fold.fold_bit_negate(op.span, arg),
                UnaryOperator::LogicNegate =>
                fold.fold_logic_negate(op.span, arg),
            },
            RValue::BinaryOperation(op, ref lhs, ref rhs) =>
            match op.contents {
                BinOp::Plus if ty == Type::CMPLX =>
                fold.fold_cmplx_add(op.span, lhs, rhs),
                BinOp::Plus if ty == Type::REAL =>
                fold.fold_real_add(op.span, lhs, rhs),
                BinOp::Plus if ty == Type::INT =>
                fold.fold_int_add(op.span, lhs, rhs),
                BinOp::Plus => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                    &match (&"Misstyped MIR",)
                                                                                         {
                                                                                         (arg0,)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt)],
                                                                                     }))
                    }
                }
                BinOp::Minus if ty == Type::CMPLX =>
                fold.fold_cmplx_sub(op.span, lhs, rhs),
                BinOp::Minus if ty == Type::REAL =>
                fold.fold_real_sub(op.span, lhs, rhs),
                BinOp::Minus if ty == Type::INT =>
                fold.fold_int_sub(op.span, lhs, rhs),
                BinOp::Minus => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                    &match (&"Misstyped MIR",)
                                                                                         {
                                                                                         (arg0,)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt)],
                                                                                     }))
                    }
                }
                BinOp::Multiply if ty == Type::CMPLX =>
                fold.fold_cmplx_mul(op.span, lhs, rhs),
                BinOp::Multiply if ty == Type::REAL =>
                fold.fold_real_mul(op.span, lhs, rhs),
                BinOp::Multiply if ty == Type::INT =>
                fold.fold_int_mul(op.span, lhs, rhs),
                BinOp::Multiply => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                    &match (&"Misstyped MIR",)
                                                                                         {
                                                                                         (arg0,)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt)],
                                                                                     }))
                    }
                }
                BinOp::Divide if ty == Type::CMPLX =>
                fold.fold_cmplx_div(op.span, lhs, rhs),
                BinOp::Divide if ty == Type::REAL =>
                fold.fold_real_div(op.span, lhs, rhs),
                BinOp::Divide if ty == Type::INT =>
                fold.fold_int_div(op.span, lhs, rhs),
                BinOp::Divide => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                    &match (&"Misstyped MIR",)
                                                                                         {
                                                                                         (arg0,)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt)],
                                                                                     }))
                    }
                }
                BinOp::Modulus if ty == Type::REAL =>
                fold.fold_real_rem(op.span, lhs, rhs),
                BinOp::Modulus if ty == Type::INT =>
                fold.fold_int_rem(op.span, lhs, rhs),
                BinOp::Modulus => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                    &match (&"Misstyped MIR",)
                                                                                         {
                                                                                         (arg0,)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt)],
                                                                                     }))
                    }
                }
                BinOp::ShiftLeft => fold.fold_shiftl(op.span, lhs, rhs),
                BinOp::ShiftRight => fold.fold_shiftr(op.span, lhs, rhs),
                BinOp::Xor if ty == Type::INT =>
                fold.fold_xor(op.span, lhs, rhs),
                BinOp::NXor if ty == Type::INT =>
                fold.fold_nxor(op.span, lhs, rhs),
                BinOp::And if ty == Type::INT =>
                fold.fold_and(op.span, lhs, rhs),
                BinOp::Or if ty == Type::INT =>
                fold.fold_or(op.span, lhs, rhs),
                BinOp::Xor if ty == Type::BOOL =>
                fold.fold_bool_xor(op.span, lhs, rhs),
                BinOp::NXor if ty == Type::BOOL =>
                fold.fold_bool_nxor(op.span, lhs, rhs),
                BinOp::And if ty == Type::BOOL =>
                fold.fold_bool_and(op.span, lhs, rhs),
                BinOp::Or if ty == Type::BOOL =>
                fold.fold_bool_or(op.span, lhs, rhs),
                BinOp::Xor => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                    &match (&"Misstyped MIR",)
                                                                                         {
                                                                                         (arg0,)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt)],
                                                                                     }))
                    }
                }
                BinOp::NXor => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                    &match (&"Misstyped MIR",)
                                                                                         {
                                                                                         (arg0,)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt)],
                                                                                     }))
                    }
                }
                BinOp::And => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                    &match (&"Misstyped MIR",)
                                                                                         {
                                                                                         (arg0,)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt)],
                                                                                     }))
                    }
                }
                BinOp::Or => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                    &match (&"Misstyped MIR",)
                                                                                         {
                                                                                         (arg0,)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt)],
                                                                                     }))
                    }
                }
            },
            RValue::Math1(kind, ref arg) =>
            match kind.contents {
                Math1::Abs if ty == Type::CMPLX =>
                fold.fold_cmplx_abs(kind.span, arg),
                Math1::Abs if ty == Type::REAL =>
                fold.fold_real_abs(kind.span, arg),
                Math1::Abs if ty == Type::INT =>
                fold.fold_int_abs(kind.span, arg),
                Math1::Abs => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                    &match (&"Misstyped MIR",)
                                                                                         {
                                                                                         (arg0,)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt)],
                                                                                     }))
                    }
                }
                Math1::Sqrt => fold.fold_sqrt(kind.span, arg),
                Math1::Exp(lim) => fold.fold_exp(kind.span, arg, lim),
                Math1::Ln => fold.fold_ln(kind.span, arg),
                Math1::Log => fold.fold_log(kind.span, arg),
                Math1::Clog2 => fold.fold_clog2(kind.span, arg),
                Math1::Floor => fold.fold_floor(kind.span, arg),
                Math1::Ceil => fold.fold_ceil(kind.span, arg),
                Math1::Sin => fold.fold_sin(kind.span, arg),
                Math1::Cos => fold.fold_cos(kind.span, arg),
                Math1::Tan => fold.fold_tan(kind.span, arg),
                Math1::ArcSin => fold.fold_asin(kind.span, arg),
                Math1::ArcCos => fold.fold_acos(kind.span, arg),
                Math1::ArcTan => fold.fold_atan(kind.span, arg),
                Math1::SinH => fold.fold_sinh(kind.span, arg),
                Math1::CosH => fold.fold_cosh(kind.span, arg),
                Math1::TanH => fold.fold_tanh(kind.span, arg),
                Math1::ArcSinH => fold.fold_asinh(kind.span, arg),
                Math1::ArcCosH => fold.fold_acosh(kind.span, arg),
                Math1::ArcTanH => fold.fold_atanh(kind.span, arg),
            },
            RValue::Comparison(op, ref lhs, ref rhs, ty) =>
            match op.contents {
                ComparisonOp::LessThen if ty == Type::INT =>
                fold.fold_lt_int(op.span, lhs, rhs),
                ComparisonOp::LessThen if ty == Type::REAL =>
                fold.fold_lt_real(op.span, lhs, rhs),
                ComparisonOp::LessThen => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                    &match (&"Misstyped MIR",)
                                                                                         {
                                                                                         (arg0,)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt)],
                                                                                     }))
                    }
                }
                ComparisonOp::LessEqual if ty == Type::INT =>
                fold.fold_le_int(op.span, lhs, rhs),
                ComparisonOp::LessEqual if ty == Type::REAL =>
                fold.fold_le_real(op.span, lhs, rhs),
                ComparisonOp::LessEqual => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                    &match (&"Misstyped MIR",)
                                                                                         {
                                                                                         (arg0,)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt)],
                                                                                     }))
                    }
                }
                ComparisonOp::GreaterThen if ty == Type::INT =>
                fold.fold_gt_int(op.span, lhs, rhs),
                ComparisonOp::GreaterThen if ty == Type::REAL =>
                fold.fold_gt_real(op.span, lhs, rhs),
                ComparisonOp::GreaterThen => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                    &match (&"Misstyped MIR",)
                                                                                         {
                                                                                         (arg0,)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt)],
                                                                                     }))
                    }
                }
                ComparisonOp::GreaterEqual if ty == Type::INT =>
                fold.fold_ge_int(op.span, lhs, rhs),
                ComparisonOp::GreaterEqual if ty == Type::REAL =>
                fold.fold_ge_real(op.span, lhs, rhs),
                ComparisonOp::GreaterEqual => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                    &match (&"Misstyped MIR",)
                                                                                         {
                                                                                         (arg0,)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt)],
                                                                                     }))
                    }
                }
                ComparisonOp::Equal => fold.fold_eq(op.span, lhs, rhs, ty),
                ComparisonOp::NotEqual => fold.fold_ne(op.span, lhs, rhs, ty),
            },
            RValue::Math2(kind, ref arg1, ref arg2) =>
            match kind.contents {
                Math2::Min if ty == Type::REAL =>
                fold.fold_real_min(kind.span, arg1, arg2),
                Math2::Min if ty == Type::INT =>
                fold.fold_int_min(kind.span, arg1, arg2),
                Math2::Min => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                    &match (&"Misstyped MIR",)
                                                                                         {
                                                                                         (arg0,)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt)],
                                                                                     }))
                    }
                }
                Math2::Max if ty == Type::REAL =>
                fold.fold_real_max(kind.span, arg1, arg2),
                Math2::Max if ty == Type::INT =>
                fold.fold_int_max(kind.span, arg1, arg2),
                Math2::Max => {
                    {
                        ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                    &match (&"Misstyped MIR",)
                                                                                         {
                                                                                         (arg0,)
                                                                                         =>
                                                                                         [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                       ::core::fmt::Display::fmt)],
                                                                                     }))
                    }
                }
                Math2::Pow => fold.fold_pow(kind.span, arg1, arg2),
                Math2::Hypot => fold.fold_hypot(kind.span, arg1, arg2),
                Math2::ArcTan2 => fold.fold_atan2(kind.span, arg1, arg2),
            },
            RValue::Cast(ref arg) => fold.fold_cast(arg, ty),
            RValue::Use(ref arg) => fold.fold_use(arg),
            RValue::Select(ref cond, ref arg1, ref arg2) =>
            fold.fold_select(cond, arg1, arg2),
            RValue::Call(ref call, ref args, span) =>
            fold.fold_call(call, args, span),
            RValue::Array(ref args, span) => fold.fold_array(args, span, ty),
        }
    }
    pub trait RValueFold<C: CfgFunctions> {
        /// Result of the Fold
        type T;
        fn fold_cmplx_arith_negate(&mut self, op: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_real_arith_negate(&mut self, op: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_bit_negate(&mut self, op: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_int_arith_negate(&mut self, op: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_logic_negate(&mut self, op: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_cmplx_add(&mut self, op: Span, lhs: &COperand<C>,
                          rhs: &COperand<C>)
        -> Self::T;
        fn fold_cmplx_sub(&mut self, op: Span, lhs: &COperand<C>,
                          rhs: &COperand<C>)
        -> Self::T;
        fn fold_cmplx_mul(&mut self, op: Span, lhs: &COperand<C>,
                          rhs: &COperand<C>)
        -> Self::T;
        fn fold_cmplx_div(&mut self, op: Span, lhs: &COperand<C>,
                          rhs: &COperand<C>)
        -> Self::T;
        fn fold_real_add(&mut self, op: Span, lhs: &COperand<C>,
                         rhs: &COperand<C>)
        -> Self::T;
        fn fold_real_sub(&mut self, op: Span, lhs: &COperand<C>,
                         rhs: &COperand<C>)
        -> Self::T;
        fn fold_real_mul(&mut self, op: Span, lhs: &COperand<C>,
                         rhs: &COperand<C>)
        -> Self::T;
        fn fold_real_div(&mut self, op: Span, lhs: &COperand<C>,
                         rhs: &COperand<C>)
        -> Self::T;
        fn fold_real_rem(&mut self, op: Span, lhs: &COperand<C>,
                         rhs: &COperand<C>)
        -> Self::T;
        fn fold_int_add(&mut self, op: Span, lhs: &COperand<C>,
                        rhs: &COperand<C>)
        -> Self::T;
        fn fold_int_sub(&mut self, op: Span, lhs: &COperand<C>,
                        rhs: &COperand<C>)
        -> Self::T;
        fn fold_int_mul(&mut self, op: Span, lhs: &COperand<C>,
                        rhs: &COperand<C>)
        -> Self::T;
        fn fold_int_div(&mut self, op: Span, lhs: &COperand<C>,
                        rhs: &COperand<C>)
        -> Self::T;
        fn fold_int_rem(&mut self, op: Span, lhs: &COperand<C>,
                        rhs: &COperand<C>)
        -> Self::T;
        fn fold_shiftl(&mut self, op: Span, lhs: &COperand<C>,
                       rhs: &COperand<C>)
        -> Self::T;
        fn fold_shiftr(&mut self, op: Span, lhs: &COperand<C>,
                       rhs: &COperand<C>)
        -> Self::T;
        fn fold_lt_real(&mut self, op: Span, lhs: &COperand<C>,
                        rhs: &COperand<C>)
        -> Self::T;
        fn fold_le_real(&mut self, op: Span, lhs: &COperand<C>,
                        rhs: &COperand<C>)
        -> Self::T;
        fn fold_gt_real(&mut self, op: Span, lhs: &COperand<C>,
                        rhs: &COperand<C>)
        -> Self::T;
        fn fold_ge_real(&mut self, op: Span, lhs: &COperand<C>,
                        rhs: &COperand<C>)
        -> Self::T;
        fn fold_lt_int(&mut self, op: Span, lhs: &COperand<C>,
                       rhs: &COperand<C>)
        -> Self::T;
        fn fold_le_int(&mut self, op: Span, lhs: &COperand<C>,
                       rhs: &COperand<C>)
        -> Self::T;
        fn fold_gt_int(&mut self, op: Span, lhs: &COperand<C>,
                       rhs: &COperand<C>)
        -> Self::T;
        fn fold_ge_int(&mut self, op: Span, lhs: &COperand<C>,
                       rhs: &COperand<C>)
        -> Self::T;
        fn fold_eq(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>,
                   ty: Type)
        -> Self::T;
        fn fold_ne(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>,
                   ty: Type)
        -> Self::T;
        fn fold_xor(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>)
        -> Self::T;
        fn fold_nxor(&mut self, op: Span, lhs: &COperand<C>,
                     rhs: &COperand<C>)
        -> Self::T;
        fn fold_and(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>)
        -> Self::T;
        fn fold_or(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>)
        -> Self::T;
        fn fold_bool_xor(&mut self, op: Span, lhs: &COperand<C>,
                         rhs: &COperand<C>)
        -> Self::T;
        fn fold_bool_nxor(&mut self, op: Span, lhs: &COperand<C>,
                          rhs: &COperand<C>)
        -> Self::T;
        fn fold_bool_and(&mut self, op: Span, lhs: &COperand<C>,
                         rhs: &COperand<C>)
        -> Self::T;
        fn fold_bool_or(&mut self, op: Span, lhs: &COperand<C>,
                        rhs: &COperand<C>)
        -> Self::T;
        fn fold_exp(&mut self, span: Span, arg: &COperand<C>, limit: bool)
        -> Self::T;
        fn fold_ln(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_log(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_clog2(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_sqrt(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_cmplx_abs(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_real_abs(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_int_abs(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_ceil(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_floor(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_sin(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_cos(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_tan(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_sinh(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_cosh(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_tanh(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_asin(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_acos(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_atan(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_asinh(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_acosh(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_atanh(&mut self, span: Span, arg: &COperand<C>)
        -> Self::T;
        fn fold_pow(&mut self, span: Span, arg1: &COperand<C>,
                    arg2: &COperand<C>)
        -> Self::T;
        fn fold_hypot(&mut self, span: Span, arg1: &COperand<C>,
                      arg2: &COperand<C>)
        -> Self::T;
        fn fold_real_min(&mut self, span: Span, arg1: &COperand<C>,
                         arg2: &COperand<C>)
        -> Self::T;
        fn fold_int_min(&mut self, span: Span, arg1: &COperand<C>,
                        arg2: &COperand<C>)
        -> Self::T;
        fn fold_real_max(&mut self, span: Span, arg1: &COperand<C>,
                         arg2: &COperand<C>)
        -> Self::T;
        fn fold_int_max(&mut self, span: Span, arg1: &COperand<C>,
                        arg2: &COperand<C>)
        -> Self::T;
        fn fold_atan2(&mut self, span: Span, arg1: &COperand<C>,
                      arg2: &COperand<C>)
        -> Self::T;
        /// Folds a cast from int -> real
        fn fold_cast(&mut self, arg: &COperand<C>, dst: Type)
        -> Self::T;
        fn fold_use(&mut self, arg: &COperand<C>)
        -> Self::T;
        fn fold_select(&mut self, cond: &COperand<C>, arg1: &COperand<C>,
                       arg2: &COperand<C>)
        -> Self::T;
        fn fold_call(&mut self, call: &C,
                     args: &IndexSlice<CallArg, [COperand<C>]>, span: Span)
        -> Self::T;
        fn fold_array(&mut self, args: &[COperand<C>], span: Span, ty: Type)
        -> Self::T;
    }
}
pub mod functions {
    use crate::derivatives::RValueAutoDiff;
    use crate::dfa::lattice::FlatSet;
    use crate::inputs::{DefaultInputs, NoInput, Voltage};
    use crate::{COperand, CfgInputs, ConstVal, DisciplineAccess, NetId,
                RValue, SimpleConstVal, Spanned};
    use crate::{Derivative, OperandData};
    use derive_more::{Display, TryInto};
    use enum_dispatch::enum_dispatch;
    use data_structures::index_vec::{index_vec, IndexSlice};
    use ir::ids::CallArg;
    use ir::{Noise, Print, StopTask};
    use session::{sourcemap::{Span, StringLiteral},
                          symbols::{kw, sysfun}};
    use std::convert::TryInto;
    use std::fmt;
    use std::fmt::{Debug, Display, Formatter};
    pub trait CfgFunctions: Debug + Clone + PartialEq + Display {
        type I: CfgInputs;
        fn const_fold(&self, call: &[FlatSet<ConstVal>])
        -> FlatSet<ConstVal>;
        fn derivative<C: CfgFunctions>(&self,
                                       args:
                                           &IndexSlice<CallArg,
                                                       [COperand<Self>]>,
                                       ad: &mut RValueAutoDiff<Self, C>,
                                       span: Span)
        -> Option<RValue<Self>>;
    }
    pub trait CfgFunctionEnum: CfgFunction<Self> {
        type I: CfgInputs;
    }
    pub trait CfgFunction<F = Self>: Debug + Clone + PartialEq + Display where
     F: CfgFunctions + From<Self> + TryInto<Self> {
        fn const_fold(&self, call: &[FlatSet<ConstVal>])
        -> FlatSet<ConstVal>;
        fn derivative<X: CfgFunctions>(&self,
                                       args:
                                           &IndexSlice<CallArg,
                                                       [COperand<F>]>,
                                       ad: &mut RValueAutoDiff<F, X>,
                                       span: Span)
        -> Option<RValue<F>>;
    }
    impl <T> CfgFunctions for T where T: CfgFunctionEnum {
        type I = <Self as CfgFunctionEnum>::I;
        #[inline(always)]
        fn const_fold(&self, call: &[FlatSet<ConstVal>])
         -> FlatSet<ConstVal> {
            CfgFunction::const_fold(self, call)
        }
        #[inline(always)]
        fn derivative<C: CfgFunctions>(&self,
                                       args:
                                           &IndexSlice<CallArg,
                                                       [COperand<Self>]>,
                                       ad: &mut RValueAutoDiff<Self, C>,
                                       span: Span) -> Option<RValue<Self>> {
            CfgFunction::derivative(self, args, ad, span)
        }
    }
    #[display(fmt = "{}({})", "noise_type", "name")]
    pub struct NoiseCall {
        noise_type: Noise,
        name: StringLiteral,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for NoiseCall {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {
                NoiseCall { noise_type: ref __self_0_0, name: ref __self_0_1 }
                => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_struct(f,
                                                                  "NoiseCall");
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "noise_type",
                                                        &&(*__self_0_0));
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "name",
                                                        &&(*__self_0_1));
                    ::core::fmt::DebugStruct::finish(debug_trait_builder)
                }
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for NoiseCall { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for NoiseCall {
        #[inline]
        fn eq(&self, other: &NoiseCall) -> bool {
            match *other {
                NoiseCall { noise_type: ref __self_1_0, name: ref __self_1_1 }
                =>
                match *self {
                    NoiseCall {
                    noise_type: ref __self_0_0, name: ref __self_0_1 } =>
                    (*__self_0_0) == (*__self_1_0) &&
                        (*__self_0_1) == (*__self_1_1),
                },
            }
        }
        #[inline]
        fn ne(&self, other: &NoiseCall) -> bool {
            match *other {
                NoiseCall { noise_type: ref __self_1_0, name: ref __self_1_1 }
                =>
                match *self {
                    NoiseCall {
                    noise_type: ref __self_0_0, name: ref __self_0_1 } =>
                    (*__self_0_0) != (*__self_1_0) ||
                        (*__self_0_1) != (*__self_1_1),
                },
            }
        }
    }
    impl ::core::marker::StructuralEq for NoiseCall { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for NoiseCall {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            {
                let _: ::core::cmp::AssertParamIsEq<Noise>;
                let _: ::core::cmp::AssertParamIsEq<StringLiteral>;
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for NoiseCall {
        #[inline]
        fn clone(&self) -> NoiseCall {
            {
                let _: ::core::clone::AssertParamIsClone<Noise>;
                let _: ::core::clone::AssertParamIsClone<StringLiteral>;
                *self
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for NoiseCall { }
    impl ::core::fmt::Display for NoiseCall {
        #[allow(unused_variables)]
        #[inline]
        fn fmt(&self,
               _derive_more_display_formatter: &mut ::core::fmt::Formatter)
         -> ::core::fmt::Result {
            match self {
                NoiseCall { noise_type, name } =>
                _derive_more_display_formatter.write_fmt(::core::fmt::Arguments::new_v1(&["",
                                                                                          "(",
                                                                                          ")"],
                                                                                        &match (&noise_type,
                                                                                                &name)
                                                                                             {
                                                                                             (arg0,
                                                                                              arg1)
                                                                                             =>
                                                                                             [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                           ::core::fmt::Display::fmt),
                                                                                              ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                           ::core::fmt::Display::fmt)],
                                                                                         })),
                _ => Ok(()),
            }
        }
    }
    impl <F> CfgFunction<F> for NoiseCall where F: CfgFunctions + From<Self> +
     TryInto<Self> {
        fn const_fold(&self, _call: &[FlatSet<ConstVal>])
         -> FlatSet<ConstVal> {
            FlatSet::Top
        }
        #[inline(always)]
        fn derivative<X: CfgFunctions>(&self,
                                       _args:
                                           &IndexSlice<CallArg,
                                                       [COperand<F>]>,
                                       _ad: &mut RValueAutoDiff<F, X>,
                                       _span: Span) -> Option<RValue<F>> {
            None
        }
    }
    pub enum TimeDerivative {

        #[display(fmt = "ddt")]
        Base,

        #[display(fmt = "ddt_ddx")]
        PartialDerivative,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for TimeDerivative {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match (&*self,) {
                (&TimeDerivative::Base,) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "Base");
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&TimeDerivative::PartialDerivative,) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                 "PartialDerivative");
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for TimeDerivative { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for TimeDerivative {
        #[inline]
        fn eq(&self, other: &TimeDerivative) -> bool {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) { _ => true, }
                } else { false }
            }
        }
    }
    impl ::core::marker::StructuralEq for TimeDerivative { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for TimeDerivative {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () { { } }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for TimeDerivative {
        #[inline]
        fn clone(&self) -> TimeDerivative {
            match (&*self,) {
                (&TimeDerivative::Base,) => TimeDerivative::Base,
                (&TimeDerivative::PartialDerivative,) =>
                TimeDerivative::PartialDerivative,
            }
        }
    }
    impl ::core::fmt::Display for TimeDerivative {
        #[allow(unused_variables)]
        #[inline]
        fn fmt(&self,
               _derive_more_display_formatter: &mut ::core::fmt::Formatter)
         -> ::core::fmt::Result {
            match self {
                TimeDerivative::Base =>
                _derive_more_display_formatter.write_fmt(::core::fmt::Arguments::new_v1(&["ddt"],
                                                                                        &match ()
                                                                                             {
                                                                                             ()
                                                                                             =>
                                                                                             [],
                                                                                         })),
                TimeDerivative::PartialDerivative =>
                _derive_more_display_formatter.write_fmt(::core::fmt::Arguments::new_v1(&["ddt_ddx"],
                                                                                        &match ()
                                                                                             {
                                                                                             ()
                                                                                             =>
                                                                                             [],
                                                                                         })),
                _ => Ok(()),
            }
        }
    }
    impl <F> CfgFunction<F> for TimeDerivative where F: CfgFunctions +
     From<Self> + TryInto<Self> {
        #[inline(always)]
        fn const_fold(&self, _call: &[FlatSet<ConstVal>])
         -> FlatSet<ConstVal> {
            FlatSet::Top
        }
        #[inline]
        fn derivative<X: CfgFunctions>(&self,
                                       args:
                                           &IndexSlice<CallArg,
                                                       [COperand<F>]>,
                                       _ad: &mut RValueAutoDiff<F, X>,
                                       span: Span) -> Option<RValue<F>> {
            match self {
                Self::Base =>
                Some(RValue::Call(Self::PartialDerivative.into(),
                                  args.to_owned(), span)),
                Self::PartialDerivative => {
                    ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["not yet implemented: "],
                                                                                &match (&::core::fmt::Arguments::new_v1(&["Error"],
                                                                                                                        &match ()
                                                                                                                             {
                                                                                                                             ()
                                                                                                                             =>
                                                                                                                             [],
                                                                                                                         }),)
                                                                                     {
                                                                                     (arg0,)
                                                                                     =>
                                                                                     [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                   ::core::fmt::Display::fmt)],
                                                                                 }))
                }
            }
        }
    }
    #[display(fmt = "{}({}({},{}), {:?})", "sysfun::limit", "access", "hi",
              "lo", "fun")]
    pub struct Lim {
        access: DisciplineAccess,
        hi: NetId,
        lo: NetId,
        fun: LimFunction,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for Lim {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {
                Lim {
                access: ref __self_0_0,
                hi: ref __self_0_1,
                lo: ref __self_0_2,
                fun: ref __self_0_3 } => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_struct(f, "Lim");
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "access",
                                                        &&(*__self_0_0));
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "hi",
                                                        &&(*__self_0_1));
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "lo",
                                                        &&(*__self_0_2));
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "fun",
                                                        &&(*__self_0_3));
                    ::core::fmt::DebugStruct::finish(debug_trait_builder)
                }
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for Lim { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for Lim {
        #[inline]
        fn eq(&self, other: &Lim) -> bool {
            match *other {
                Lim {
                access: ref __self_1_0,
                hi: ref __self_1_1,
                lo: ref __self_1_2,
                fun: ref __self_1_3 } =>
                match *self {
                    Lim {
                    access: ref __self_0_0,
                    hi: ref __self_0_1,
                    lo: ref __self_0_2,
                    fun: ref __self_0_3 } =>
                    (*__self_0_0) == (*__self_1_0) &&
                        (*__self_0_1) == (*__self_1_1) &&
                        (*__self_0_2) == (*__self_1_2) &&
                        (*__self_0_3) == (*__self_1_3),
                },
            }
        }
        #[inline]
        fn ne(&self, other: &Lim) -> bool {
            match *other {
                Lim {
                access: ref __self_1_0,
                hi: ref __self_1_1,
                lo: ref __self_1_2,
                fun: ref __self_1_3 } =>
                match *self {
                    Lim {
                    access: ref __self_0_0,
                    hi: ref __self_0_1,
                    lo: ref __self_0_2,
                    fun: ref __self_0_3 } =>
                    (*__self_0_0) != (*__self_1_0) ||
                        (*__self_0_1) != (*__self_1_1) ||
                        (*__self_0_2) != (*__self_1_2) ||
                        (*__self_0_3) != (*__self_1_3),
                },
            }
        }
    }
    impl ::core::marker::StructuralEq for Lim { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for Lim {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            {
                let _: ::core::cmp::AssertParamIsEq<DisciplineAccess>;
                let _: ::core::cmp::AssertParamIsEq<NetId>;
                let _: ::core::cmp::AssertParamIsEq<NetId>;
                let _: ::core::cmp::AssertParamIsEq<LimFunction>;
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for Lim {
        #[inline]
        fn clone(&self) -> Lim {
            match *self {
                Lim {
                access: ref __self_0_0,
                hi: ref __self_0_1,
                lo: ref __self_0_2,
                fun: ref __self_0_3 } =>
                Lim{access: ::core::clone::Clone::clone(&(*__self_0_0)),
                    hi: ::core::clone::Clone::clone(&(*__self_0_1)),
                    lo: ::core::clone::Clone::clone(&(*__self_0_2)),
                    fun: ::core::clone::Clone::clone(&(*__self_0_3)),},
            }
        }
    }
    impl ::core::fmt::Display for Lim {
        #[allow(unused_variables)]
        #[inline]
        fn fmt(&self,
               _derive_more_display_formatter: &mut ::core::fmt::Formatter)
         -> ::core::fmt::Result {
            match self {
                Lim { access, hi, lo, fun } =>
                _derive_more_display_formatter.write_fmt(::core::fmt::Arguments::new_v1(&["",
                                                                                          "(",
                                                                                          "(",
                                                                                          ",",
                                                                                          "), ",
                                                                                          ")"],
                                                                                        &match (&sysfun::limit,
                                                                                                &access,
                                                                                                &hi,
                                                                                                &lo,
                                                                                                &fun)
                                                                                             {
                                                                                             (arg0,
                                                                                              arg1,
                                                                                              arg2,
                                                                                              arg3,
                                                                                              arg4)
                                                                                             =>
                                                                                             [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                           ::core::fmt::Display::fmt),
                                                                                              ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                           ::core::fmt::Display::fmt),
                                                                                              ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                           ::core::fmt::Display::fmt),
                                                                                              ::core::fmt::ArgumentV1::new(arg3,
                                                                                                                           ::core::fmt::Display::fmt),
                                                                                              ::core::fmt::ArgumentV1::new(arg4,
                                                                                                                           ::core::fmt::Debug::fmt)],
                                                                                         })),
                _ => Ok(()),
            }
        }
    }
    impl <F> CfgFunction<F> for Lim where F: CfgFunctions + From<Self> +
     TryInto<Self> {
        fn const_fold(&self, _call: &[FlatSet<ConstVal>])
         -> FlatSet<ConstVal> {
            FlatSet::Top
        }
        fn derivative<X: CfgFunctions>(&self,
                                       _args:
                                           &IndexSlice<CallArg,
                                                       [COperand<F>]>,
                                       ad: &mut RValueAutoDiff<F, X>,
                                       span: Span) -> Option<RValue<F>> {
            let derivative =
                Voltage{hi: self.hi,
                        lo:
                            self.lo,}.derivative(ad.unknown,
                                                 ad.ad.mir).into_option()?;
            Some(RValue::Use(Spanned{span,
                                     contents:
                                         OperandData::Constant(derivative),}))
        }
    }
    #[display(fmt = "$collapse({:?},{:?})", "0", "1")]
    pub struct NodeCollapse(NetId, NetId);
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for NodeCollapse {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {
                NodeCollapse(ref __self_0_0, ref __self_0_1) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                 "NodeCollapse");
                    let _ =
                        ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                       &&(*__self_0_0));
                    let _ =
                        ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                       &&(*__self_0_1));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for NodeCollapse { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for NodeCollapse {
        #[inline]
        fn eq(&self, other: &NodeCollapse) -> bool {
            match *other {
                NodeCollapse(ref __self_1_0, ref __self_1_1) =>
                match *self {
                    NodeCollapse(ref __self_0_0, ref __self_0_1) =>
                    (*__self_0_0) == (*__self_1_0) &&
                        (*__self_0_1) == (*__self_1_1),
                },
            }
        }
        #[inline]
        fn ne(&self, other: &NodeCollapse) -> bool {
            match *other {
                NodeCollapse(ref __self_1_0, ref __self_1_1) =>
                match *self {
                    NodeCollapse(ref __self_0_0, ref __self_0_1) =>
                    (*__self_0_0) != (*__self_1_0) ||
                        (*__self_0_1) != (*__self_1_1),
                },
            }
        }
    }
    impl ::core::marker::StructuralEq for NodeCollapse { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for NodeCollapse {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            {
                let _: ::core::cmp::AssertParamIsEq<NetId>;
                let _: ::core::cmp::AssertParamIsEq<NetId>;
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for NodeCollapse {
        #[inline]
        fn clone(&self) -> NodeCollapse {
            match *self {
                NodeCollapse(ref __self_0_0, ref __self_0_1) =>
                NodeCollapse(::core::clone::Clone::clone(&(*__self_0_0)),
                             ::core::clone::Clone::clone(&(*__self_0_1))),
            }
        }
    }
    impl ::core::fmt::Display for NodeCollapse {
        #[allow(unused_variables)]
        #[inline]
        fn fmt(&self,
               _derive_more_display_formatter: &mut ::core::fmt::Formatter)
         -> ::core::fmt::Result {
            match self {
                NodeCollapse(_0, _1) =>
                _derive_more_display_formatter.write_fmt(::core::fmt::Arguments::new_v1(&["$collapse(",
                                                                                          ",",
                                                                                          ")"],
                                                                                        &match (&0,
                                                                                                &1)
                                                                                             {
                                                                                             (arg0,
                                                                                              arg1)
                                                                                             =>
                                                                                             [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                           ::core::fmt::Debug::fmt),
                                                                                              ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                           ::core::fmt::Debug::fmt)],
                                                                                         })),
                _ => Ok(()),
            }
        }
    }
    impl <F> CfgFunction<F> for NodeCollapse where F: CfgFunctions +
     From<Self> + TryInto<Self> {
        fn const_fold(&self, _call: &[FlatSet<ConstVal>])
         -> FlatSet<ConstVal> {
            {
                {
                    ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                &match (&"node collapse functions do not return a value",)
                                                                                     {
                                                                                     (arg0,)
                                                                                     =>
                                                                                     [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                   ::core::fmt::Display::fmt)],
                                                                                 }))
                }
            }
        }
        fn derivative<X: CfgFunctions>(&self,
                                       _args:
                                           &IndexSlice<CallArg,
                                                       [COperand<F>]>,
                                       _ad: &mut RValueAutoDiff<F, X>,
                                       _span: Span) -> Option<RValue<F>> {
            {
                {
                    ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                &match (&"node collapse functions do not return a value",)
                                                                                     {
                                                                                     (arg0,)
                                                                                     =>
                                                                                     [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                   ::core::fmt::Display::fmt)],
                                                                                 }))
                }
            }
        }
    }
    impl <F> CfgFunction<F> for StopTask where F: CfgFunctions + From<Self> +
     TryInto<Self> {
        fn const_fold(&self, _call: &[FlatSet<ConstVal>])
         -> FlatSet<ConstVal> {
            {
                {
                    ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                &match (&"stop tasks do not return a value",)
                                                                                     {
                                                                                     (arg0,)
                                                                                     =>
                                                                                     [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                   ::core::fmt::Display::fmt)],
                                                                                 }))
                }
            }
        }
        fn derivative<X: CfgFunctions>(&self,
                                       _args:
                                           &IndexSlice<CallArg,
                                                       [COperand<F>]>,
                                       _ad: &mut RValueAutoDiff<F, X>,
                                       _span: Span) -> Option<RValue<F>> {
            {
                {
                    ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                &match (&"stop tasks do not return a value",)
                                                                                     {
                                                                                     (arg0,)
                                                                                     =>
                                                                                     [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                   ::core::fmt::Display::fmt)],
                                                                                 }))
                }
            }
        }
    }
    impl <F> CfgFunction<F> for Print where F: CfgFunctions + From<Self> +
     TryInto<Self> {
        fn const_fold(&self, _call: &[FlatSet<ConstVal>])
         -> FlatSet<ConstVal> {
            {
                {
                    ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                &match (&"print functions do not return a value",)
                                                                                     {
                                                                                     (arg0,)
                                                                                     =>
                                                                                     [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                   ::core::fmt::Display::fmt)],
                                                                                 }))
                }
            }
        }
        fn derivative<X: CfgFunctions>(&self,
                                       _args:
                                           &IndexSlice<CallArg,
                                                       [COperand<F>]>,
                                       _ad: &mut RValueAutoDiff<F, X>,
                                       _span: Span) -> Option<RValue<F>> {
            {
                {
                    ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                &match (&"print functions do not return a value",)
                                                                                     {
                                                                                     (arg0,)
                                                                                     =>
                                                                                     [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                   ::core::fmt::Display::fmt)],
                                                                                 }))
                }
            }
        }
    }
    pub enum LimFunction { Native(StringLiteral), VerilogA, }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for LimFunction {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match (&*self,) {
                (&LimFunction::Native(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "Native");
                    let _ =
                        ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                       &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&LimFunction::VerilogA,) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                 "VerilogA");
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for LimFunction { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for LimFunction {
        #[inline]
        fn eq(&self, other: &LimFunction) -> bool {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        (&LimFunction::Native(ref __self_0),
                         &LimFunction::Native(ref __arg_1_0)) =>
                        (*__self_0) == (*__arg_1_0),
                        _ => true,
                    }
                } else { false }
            }
        }
        #[inline]
        fn ne(&self, other: &LimFunction) -> bool {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        (&LimFunction::Native(ref __self_0),
                         &LimFunction::Native(ref __arg_1_0)) =>
                        (*__self_0) != (*__arg_1_0),
                        _ => false,
                    }
                } else { true }
            }
        }
    }
    impl ::core::marker::StructuralEq for LimFunction { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for LimFunction {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            { let _: ::core::cmp::AssertParamIsEq<StringLiteral>; }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for LimFunction {
        #[inline]
        fn clone(&self) -> LimFunction {
            match (&*self,) {
                (&LimFunction::Native(ref __self_0),) =>
                LimFunction::Native(::core::clone::Clone::clone(&(*__self_0))),
                (&LimFunction::VerilogA,) => LimFunction::VerilogA,
            }
        }
    }
    pub enum DefaultFunctions {
        Noise(NoiseCall),
        TimeDerivative(TimeDerivative),
        StopTask(StopTask),
        Print(Print),
        NodeCollapse(NodeCollapse),
        Lim(Lim),
    }
    impl ::core::marker::StructuralPartialEq for DefaultFunctions { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for DefaultFunctions {
        #[inline]
        fn eq(&self, other: &DefaultFunctions) -> bool {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        (&DefaultFunctions::Noise(ref __self_0),
                         &DefaultFunctions::Noise(ref __arg_1_0)) =>
                        (*__self_0) == (*__arg_1_0),
                        (&DefaultFunctions::TimeDerivative(ref __self_0),
                         &DefaultFunctions::TimeDerivative(ref __arg_1_0)) =>
                        (*__self_0) == (*__arg_1_0),
                        (&DefaultFunctions::StopTask(ref __self_0),
                         &DefaultFunctions::StopTask(ref __arg_1_0)) =>
                        (*__self_0) == (*__arg_1_0),
                        (&DefaultFunctions::Print(ref __self_0),
                         &DefaultFunctions::Print(ref __arg_1_0)) =>
                        (*__self_0) == (*__arg_1_0),
                        (&DefaultFunctions::NodeCollapse(ref __self_0),
                         &DefaultFunctions::NodeCollapse(ref __arg_1_0)) =>
                        (*__self_0) == (*__arg_1_0),
                        (&DefaultFunctions::Lim(ref __self_0),
                         &DefaultFunctions::Lim(ref __arg_1_0)) =>
                        (*__self_0) == (*__arg_1_0),
                        _ => unsafe { ::core::intrinsics::unreachable() }
                    }
                } else { false }
            }
        }
        #[inline]
        fn ne(&self, other: &DefaultFunctions) -> bool {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        (&DefaultFunctions::Noise(ref __self_0),
                         &DefaultFunctions::Noise(ref __arg_1_0)) =>
                        (*__self_0) != (*__arg_1_0),
                        (&DefaultFunctions::TimeDerivative(ref __self_0),
                         &DefaultFunctions::TimeDerivative(ref __arg_1_0)) =>
                        (*__self_0) != (*__arg_1_0),
                        (&DefaultFunctions::StopTask(ref __self_0),
                         &DefaultFunctions::StopTask(ref __arg_1_0)) =>
                        (*__self_0) != (*__arg_1_0),
                        (&DefaultFunctions::Print(ref __self_0),
                         &DefaultFunctions::Print(ref __arg_1_0)) =>
                        (*__self_0) != (*__arg_1_0),
                        (&DefaultFunctions::NodeCollapse(ref __self_0),
                         &DefaultFunctions::NodeCollapse(ref __arg_1_0)) =>
                        (*__self_0) != (*__arg_1_0),
                        (&DefaultFunctions::Lim(ref __self_0),
                         &DefaultFunctions::Lim(ref __arg_1_0)) =>
                        (*__self_0) != (*__arg_1_0),
                        _ => unsafe { ::core::intrinsics::unreachable() }
                    }
                } else { true }
            }
        }
    }
    impl ::core::marker::StructuralEq for DefaultFunctions { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for DefaultFunctions {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            {
                let _: ::core::cmp::AssertParamIsEq<NoiseCall>;
                let _: ::core::cmp::AssertParamIsEq<TimeDerivative>;
                let _: ::core::cmp::AssertParamIsEq<StopTask>;
                let _: ::core::cmp::AssertParamIsEq<Print>;
                let _: ::core::cmp::AssertParamIsEq<NodeCollapse>;
                let _: ::core::cmp::AssertParamIsEq<Lim>;
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for DefaultFunctions {
        #[inline]
        fn clone(&self) -> DefaultFunctions {
            match (&*self,) {
                (&DefaultFunctions::Noise(ref __self_0),) =>
                DefaultFunctions::Noise(::core::clone::Clone::clone(&(*__self_0))),
                (&DefaultFunctions::TimeDerivative(ref __self_0),) =>
                DefaultFunctions::TimeDerivative(::core::clone::Clone::clone(&(*__self_0))),
                (&DefaultFunctions::StopTask(ref __self_0),) =>
                DefaultFunctions::StopTask(::core::clone::Clone::clone(&(*__self_0))),
                (&DefaultFunctions::Print(ref __self_0),) =>
                DefaultFunctions::Print(::core::clone::Clone::clone(&(*__self_0))),
                (&DefaultFunctions::NodeCollapse(ref __self_0),) =>
                DefaultFunctions::NodeCollapse(::core::clone::Clone::clone(&(*__self_0))),
                (&DefaultFunctions::Lim(ref __self_0),) =>
                DefaultFunctions::Lim(::core::clone::Clone::clone(&(*__self_0))),
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for DefaultFunctions {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match (&*self,) {
                (&DefaultFunctions::Noise(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "Noise");
                    let _ =
                        ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                       &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&DefaultFunctions::TimeDerivative(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                 "TimeDerivative");
                    let _ =
                        ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                       &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&DefaultFunctions::StopTask(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                 "StopTask");
                    let _ =
                        ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                       &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&DefaultFunctions::Print(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "Print");
                    let _ =
                        ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                       &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&DefaultFunctions::NodeCollapse(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                 "NodeCollapse");
                    let _ =
                        ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                       &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&DefaultFunctions::Lim(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "Lim");
                    let _ =
                        ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                       &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
            }
        }
    }
    impl ::core::fmt::Display for DefaultFunctions {
        #[allow(unused_variables)]
        #[inline]
        fn fmt(&self,
               _derive_more_display_formatter: &mut ::core::fmt::Formatter)
         -> ::core::fmt::Result {
            match self {
                DefaultFunctions::Noise(_0) =>
                ::core::fmt::Display::fmt(_0, _derive_more_display_formatter),
                DefaultFunctions::TimeDerivative(_0) =>
                ::core::fmt::Display::fmt(_0, _derive_more_display_formatter),
                DefaultFunctions::StopTask(_0) =>
                ::core::fmt::Display::fmt(_0, _derive_more_display_formatter),
                DefaultFunctions::Print(_0) =>
                ::core::fmt::Display::fmt(_0, _derive_more_display_formatter),
                DefaultFunctions::NodeCollapse(_0) =>
                ::core::fmt::Display::fmt(_0, _derive_more_display_formatter),
                DefaultFunctions::Lim(_0) =>
                ::core::fmt::Display::fmt(_0, _derive_more_display_formatter),
                _ => Ok(()),
            }
        }
    }
    impl ::core::convert::From<NoiseCall> for DefaultFunctions {
        fn from(v: NoiseCall) -> DefaultFunctions {
            DefaultFunctions::Noise(v)
        }
    }
    impl ::core::convert::From<TimeDerivative> for DefaultFunctions {
        fn from(v: TimeDerivative) -> DefaultFunctions {
            DefaultFunctions::TimeDerivative(v)
        }
    }
    impl ::core::convert::From<StopTask> for DefaultFunctions {
        fn from(v: StopTask) -> DefaultFunctions {
            DefaultFunctions::StopTask(v)
        }
    }
    impl ::core::convert::From<Print> for DefaultFunctions {
        fn from(v: Print) -> DefaultFunctions { DefaultFunctions::Print(v) }
    }
    impl ::core::convert::From<NodeCollapse> for DefaultFunctions {
        fn from(v: NodeCollapse) -> DefaultFunctions {
            DefaultFunctions::NodeCollapse(v)
        }
    }
    impl ::core::convert::From<Lim> for DefaultFunctions {
        fn from(v: Lim) -> DefaultFunctions { DefaultFunctions::Lim(v) }
    }
    impl core::convert::TryInto<NoiseCall> for DefaultFunctions {
        type Error = &'static str;
        fn try_into(self)
         ->
             ::core::result::Result<NoiseCall,
                                    <Self as
                                    core::convert::TryInto<NoiseCall>>::Error> {
            match self {
                DefaultFunctions::Noise(v) => { Ok(v) }
                DefaultFunctions::TimeDerivative(v) => {
                    Err("Tried to convert variant TimeDerivative to Noise")
                }
                DefaultFunctions::StopTask(v) => {
                    Err("Tried to convert variant StopTask to Noise")
                }
                DefaultFunctions::Print(v) => {
                    Err("Tried to convert variant Print to Noise")
                }
                DefaultFunctions::NodeCollapse(v) => {
                    Err("Tried to convert variant NodeCollapse to Noise")
                }
                DefaultFunctions::Lim(v) => {
                    Err("Tried to convert variant Lim to Noise")
                }
            }
        }
    }
    impl core::convert::TryInto<TimeDerivative> for DefaultFunctions {
        type Error = &'static str;
        fn try_into(self)
         ->
             ::core::result::Result<TimeDerivative,
                                    <Self as
                                    core::convert::TryInto<TimeDerivative>>::Error> {
            match self {
                DefaultFunctions::TimeDerivative(v) => { Ok(v) }
                DefaultFunctions::Noise(v) => {
                    Err("Tried to convert variant Noise to TimeDerivative")
                }
                DefaultFunctions::StopTask(v) => {
                    Err("Tried to convert variant StopTask to TimeDerivative")
                }
                DefaultFunctions::Print(v) => {
                    Err("Tried to convert variant Print to TimeDerivative")
                }
                DefaultFunctions::NodeCollapse(v) => {
                    Err("Tried to convert variant NodeCollapse to TimeDerivative")
                }
                DefaultFunctions::Lim(v) => {
                    Err("Tried to convert variant Lim to TimeDerivative")
                }
            }
        }
    }
    impl core::convert::TryInto<StopTask> for DefaultFunctions {
        type Error = &'static str;
        fn try_into(self)
         ->
             ::core::result::Result<StopTask,
                                    <Self as
                                    core::convert::TryInto<StopTask>>::Error> {
            match self {
                DefaultFunctions::StopTask(v) => { Ok(v) }
                DefaultFunctions::Noise(v) => {
                    Err("Tried to convert variant Noise to StopTask")
                }
                DefaultFunctions::TimeDerivative(v) => {
                    Err("Tried to convert variant TimeDerivative to StopTask")
                }
                DefaultFunctions::Print(v) => {
                    Err("Tried to convert variant Print to StopTask")
                }
                DefaultFunctions::NodeCollapse(v) => {
                    Err("Tried to convert variant NodeCollapse to StopTask")
                }
                DefaultFunctions::Lim(v) => {
                    Err("Tried to convert variant Lim to StopTask")
                }
            }
        }
    }
    impl core::convert::TryInto<Print> for DefaultFunctions {
        type Error = &'static str;
        fn try_into(self)
         ->
             ::core::result::Result<Print,
                                    <Self as
                                    core::convert::TryInto<Print>>::Error> {
            match self {
                DefaultFunctions::Print(v) => { Ok(v) }
                DefaultFunctions::Noise(v) => {
                    Err("Tried to convert variant Noise to Print")
                }
                DefaultFunctions::TimeDerivative(v) => {
                    Err("Tried to convert variant TimeDerivative to Print")
                }
                DefaultFunctions::StopTask(v) => {
                    Err("Tried to convert variant StopTask to Print")
                }
                DefaultFunctions::NodeCollapse(v) => {
                    Err("Tried to convert variant NodeCollapse to Print")
                }
                DefaultFunctions::Lim(v) => {
                    Err("Tried to convert variant Lim to Print")
                }
            }
        }
    }
    impl core::convert::TryInto<NodeCollapse> for DefaultFunctions {
        type Error = &'static str;
        fn try_into(self)
         ->
             ::core::result::Result<NodeCollapse,
                                    <Self as
                                    core::convert::TryInto<NodeCollapse>>::Error> {
            match self {
                DefaultFunctions::NodeCollapse(v) => { Ok(v) }
                DefaultFunctions::Noise(v) => {
                    Err("Tried to convert variant Noise to NodeCollapse")
                }
                DefaultFunctions::TimeDerivative(v) => {
                    Err("Tried to convert variant TimeDerivative to NodeCollapse")
                }
                DefaultFunctions::StopTask(v) => {
                    Err("Tried to convert variant StopTask to NodeCollapse")
                }
                DefaultFunctions::Print(v) => {
                    Err("Tried to convert variant Print to NodeCollapse")
                }
                DefaultFunctions::Lim(v) => {
                    Err("Tried to convert variant Lim to NodeCollapse")
                }
            }
        }
    }
    impl core::convert::TryInto<Lim> for DefaultFunctions {
        type Error = &'static str;
        fn try_into(self)
         ->
             ::core::result::Result<Lim,
                                    <Self as
                                    core::convert::TryInto<Lim>>::Error> {
            match self {
                DefaultFunctions::Lim(v) => { Ok(v) }
                DefaultFunctions::Noise(v) => {
                    Err("Tried to convert variant Noise to Lim")
                }
                DefaultFunctions::TimeDerivative(v) => {
                    Err("Tried to convert variant TimeDerivative to Lim")
                }
                DefaultFunctions::StopTask(v) => {
                    Err("Tried to convert variant StopTask to Lim")
                }
                DefaultFunctions::Print(v) => {
                    Err("Tried to convert variant Print to Lim")
                }
                DefaultFunctions::NodeCollapse(v) => {
                    Err("Tried to convert variant NodeCollapse to Lim")
                }
            }
        }
    }
    impl CfgFunction<F> for DefaultFunctions {
        #[inline]
        fn const_fold(&self, __enum_dispatch_arg_0: &[FlatSet<ConstVal>])
         -> FlatSet<ConstVal> {
            match self {
                DefaultFunctions::Noise(inner) =>
                CfgFunction::<F>::const_fold(inner, __enum_dispatch_arg_0),
                DefaultFunctions::TimeDerivative(inner) =>
                CfgFunction::<F>::const_fold(inner, __enum_dispatch_arg_0),
                DefaultFunctions::StopTask(inner) =>
                CfgFunction::<F>::const_fold(inner, __enum_dispatch_arg_0),
                DefaultFunctions::Print(inner) =>
                CfgFunction::<F>::const_fold(inner, __enum_dispatch_arg_0),
                DefaultFunctions::NodeCollapse(inner) =>
                CfgFunction::<F>::const_fold(inner, __enum_dispatch_arg_0),
                DefaultFunctions::Lim(inner) =>
                CfgFunction::<F>::const_fold(inner, __enum_dispatch_arg_0),
            }
        }
        #[inline]
        fn derivative<X: CfgFunctions>(&self,
                                       __enum_dispatch_arg_0:
                                           &IndexSlice<CallArg,
                                                       [COperand<F>]>,
                                       __enum_dispatch_arg_1:
                                           &mut RValueAutoDiff<F, X>,
                                       __enum_dispatch_arg_2: Span)
         -> Option<RValue<F>> {
            match self {
                DefaultFunctions::Noise(inner) =>
                CfgFunction::<F>::derivative::<X>(inner,
                                                  __enum_dispatch_arg_0,
                                                  __enum_dispatch_arg_1,
                                                  __enum_dispatch_arg_2),
                DefaultFunctions::TimeDerivative(inner) =>
                CfgFunction::<F>::derivative::<X>(inner,
                                                  __enum_dispatch_arg_0,
                                                  __enum_dispatch_arg_1,
                                                  __enum_dispatch_arg_2),
                DefaultFunctions::StopTask(inner) =>
                CfgFunction::<F>::derivative::<X>(inner,
                                                  __enum_dispatch_arg_0,
                                                  __enum_dispatch_arg_1,
                                                  __enum_dispatch_arg_2),
                DefaultFunctions::Print(inner) =>
                CfgFunction::<F>::derivative::<X>(inner,
                                                  __enum_dispatch_arg_0,
                                                  __enum_dispatch_arg_1,
                                                  __enum_dispatch_arg_2),
                DefaultFunctions::NodeCollapse(inner) =>
                CfgFunction::<F>::derivative::<X>(inner,
                                                  __enum_dispatch_arg_0,
                                                  __enum_dispatch_arg_1,
                                                  __enum_dispatch_arg_2),
                DefaultFunctions::Lim(inner) =>
                CfgFunction::<F>::derivative::<X>(inner,
                                                  __enum_dispatch_arg_0,
                                                  __enum_dispatch_arg_1,
                                                  __enum_dispatch_arg_2),
            }
        }
    }
    impl CfgFunctionEnum for DefaultFunctions {
        type I = DefaultInputs;
    }
}
pub mod inputs {
    use crate::functions::NoiseCall;
    use crate::osdi_types::ConstVal::Scalar;
    use crate::osdi_types::SimpleConstVal::Real;
    use crate::{BranchId, CfgFunctions, ConstVal, Derivative, Mir,
                OperandData, ParameterId, PortId, Type};
    use derive_more::{Display, From, TryInto};
    use enum_dispatch::enum_dispatch;
    use ir::ids::NetId;
    use ir::Unknown;
    use session::{sourcemap::StringLiteral, symbols::{kw, sysfun}};
    use std::convert::TryInto;
    use std::fmt::{Debug, Display, Formatter};
    pub trait CfgInputs: Clone + Sized + Debug + PartialEq + Display {
        fn derivative<C: CfgFunctions>(&self, unknown: Unknown, mir: &Mir<C>)
        -> InputDerivative;
        fn ty<C: CfgFunctions>(&self, mir: &Mir<C>)
        -> Type;
    }
    pub enum InputDerivative { One, Zero, Const(ConstVal), }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for InputDerivative {
        #[inline]
        fn clone(&self) -> InputDerivative {
            match (&*self,) {
                (&InputDerivative::One,) => InputDerivative::One,
                (&InputDerivative::Zero,) => InputDerivative::Zero,
                (&InputDerivative::Const(ref __self_0),) =>
                InputDerivative::Const(::core::clone::Clone::clone(&(*__self_0))),
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for InputDerivative {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match (&*self,) {
                (&InputDerivative::One,) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "One");
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&InputDerivative::Zero,) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "Zero");
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&InputDerivative::Const(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "Const");
                    let _ =
                        ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                       &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for InputDerivative { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for InputDerivative {
        #[inline]
        fn eq(&self, other: &InputDerivative) -> bool {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        (&InputDerivative::Const(ref __self_0),
                         &InputDerivative::Const(ref __arg_1_0)) =>
                        (*__self_0) == (*__arg_1_0),
                        _ => true,
                    }
                } else { false }
            }
        }
        #[inline]
        fn ne(&self, other: &InputDerivative) -> bool {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        (&InputDerivative::Const(ref __self_0),
                         &InputDerivative::Const(ref __arg_1_0)) =>
                        (*__self_0) != (*__arg_1_0),
                        _ => false,
                    }
                } else { true }
            }
        }
    }
    impl InputDerivative {
        pub fn into_option(self) -> Option<ConstVal> {
            match self {
                InputDerivative::One => Some(Scalar(Real(1.0))),
                InputDerivative::Zero => None,
                InputDerivative::Const(val) => Some(val),
            }
        }
    }
    #[doc =
      " This struct is generated by HIR lowering and represents all input kinds which are explicitly found in the VerilogA code"]
    #[doc =
      " OpenVAF drivers can easily create their own variant of this enum by combining variants with `#[enum_dispatch(CfgInputs)]`"]
    pub enum DefaultInputs {
        Parameter(ParameterInput),
        PortConnected(PortConnected),
        SimParam(SimParam),
        Voltage(Voltage),
        CurrentProbe(CurrentProbe),
        Temperature(Temperature),
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for DefaultInputs {
        #[inline]
        fn clone(&self) -> DefaultInputs {
            match (&*self,) {
                (&DefaultInputs::Parameter(ref __self_0),) =>
                DefaultInputs::Parameter(::core::clone::Clone::clone(&(*__self_0))),
                (&DefaultInputs::PortConnected(ref __self_0),) =>
                DefaultInputs::PortConnected(::core::clone::Clone::clone(&(*__self_0))),
                (&DefaultInputs::SimParam(ref __self_0),) =>
                DefaultInputs::SimParam(::core::clone::Clone::clone(&(*__self_0))),
                (&DefaultInputs::Voltage(ref __self_0),) =>
                DefaultInputs::Voltage(::core::clone::Clone::clone(&(*__self_0))),
                (&DefaultInputs::CurrentProbe(ref __self_0),) =>
                DefaultInputs::CurrentProbe(::core::clone::Clone::clone(&(*__self_0))),
                (&DefaultInputs::Temperature(ref __self_0),) =>
                DefaultInputs::Temperature(::core::clone::Clone::clone(&(*__self_0))),
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for DefaultInputs {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match (&*self,) {
                (&DefaultInputs::Parameter(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                 "Parameter");
                    let _ =
                        ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                       &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&DefaultInputs::PortConnected(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                 "PortConnected");
                    let _ =
                        ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                       &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&DefaultInputs::SimParam(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                 "SimParam");
                    let _ =
                        ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                       &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&DefaultInputs::Voltage(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                 "Voltage");
                    let _ =
                        ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                       &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&DefaultInputs::CurrentProbe(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                 "CurrentProbe");
                    let _ =
                        ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                       &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&DefaultInputs::Temperature(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                 "Temperature");
                    let _ =
                        ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                       &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for DefaultInputs { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for DefaultInputs {
        #[inline]
        fn eq(&self, other: &DefaultInputs) -> bool {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        (&DefaultInputs::Parameter(ref __self_0),
                         &DefaultInputs::Parameter(ref __arg_1_0)) =>
                        (*__self_0) == (*__arg_1_0),
                        (&DefaultInputs::PortConnected(ref __self_0),
                         &DefaultInputs::PortConnected(ref __arg_1_0)) =>
                        (*__self_0) == (*__arg_1_0),
                        (&DefaultInputs::SimParam(ref __self_0),
                         &DefaultInputs::SimParam(ref __arg_1_0)) =>
                        (*__self_0) == (*__arg_1_0),
                        (&DefaultInputs::Voltage(ref __self_0),
                         &DefaultInputs::Voltage(ref __arg_1_0)) =>
                        (*__self_0) == (*__arg_1_0),
                        (&DefaultInputs::CurrentProbe(ref __self_0),
                         &DefaultInputs::CurrentProbe(ref __arg_1_0)) =>
                        (*__self_0) == (*__arg_1_0),
                        (&DefaultInputs::Temperature(ref __self_0),
                         &DefaultInputs::Temperature(ref __arg_1_0)) =>
                        (*__self_0) == (*__arg_1_0),
                        _ => unsafe { ::core::intrinsics::unreachable() }
                    }
                } else { false }
            }
        }
        #[inline]
        fn ne(&self, other: &DefaultInputs) -> bool {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        (&DefaultInputs::Parameter(ref __self_0),
                         &DefaultInputs::Parameter(ref __arg_1_0)) =>
                        (*__self_0) != (*__arg_1_0),
                        (&DefaultInputs::PortConnected(ref __self_0),
                         &DefaultInputs::PortConnected(ref __arg_1_0)) =>
                        (*__self_0) != (*__arg_1_0),
                        (&DefaultInputs::SimParam(ref __self_0),
                         &DefaultInputs::SimParam(ref __arg_1_0)) =>
                        (*__self_0) != (*__arg_1_0),
                        (&DefaultInputs::Voltage(ref __self_0),
                         &DefaultInputs::Voltage(ref __arg_1_0)) =>
                        (*__self_0) != (*__arg_1_0),
                        (&DefaultInputs::CurrentProbe(ref __self_0),
                         &DefaultInputs::CurrentProbe(ref __arg_1_0)) =>
                        (*__self_0) != (*__arg_1_0),
                        (&DefaultInputs::Temperature(ref __self_0),
                         &DefaultInputs::Temperature(ref __arg_1_0)) =>
                        (*__self_0) != (*__arg_1_0),
                        _ => unsafe { ::core::intrinsics::unreachable() }
                    }
                } else { true }
            }
        }
    }
    impl ::core::fmt::Display for DefaultInputs {
        #[allow(unused_variables)]
        #[inline]
        fn fmt(&self,
               _derive_more_display_formatter: &mut ::core::fmt::Formatter)
         -> ::core::fmt::Result {
            match self {
                DefaultInputs::Parameter(_0) =>
                ::core::fmt::Display::fmt(_0, _derive_more_display_formatter),
                DefaultInputs::PortConnected(_0) =>
                ::core::fmt::Display::fmt(_0, _derive_more_display_formatter),
                DefaultInputs::SimParam(_0) =>
                ::core::fmt::Display::fmt(_0, _derive_more_display_formatter),
                DefaultInputs::Voltage(_0) =>
                ::core::fmt::Display::fmt(_0, _derive_more_display_formatter),
                DefaultInputs::CurrentProbe(_0) =>
                ::core::fmt::Display::fmt(_0, _derive_more_display_formatter),
                DefaultInputs::Temperature(_0) =>
                ::core::fmt::Display::fmt(_0, _derive_more_display_formatter),
                _ => Ok(()),
            }
        }
    }
    impl ::core::convert::From<ParameterInput> for DefaultInputs {
        fn from(v: ParameterInput) -> DefaultInputs {
            DefaultInputs::Parameter(v)
        }
    }
    impl ::core::convert::From<PortConnected> for DefaultInputs {
        fn from(v: PortConnected) -> DefaultInputs {
            DefaultInputs::PortConnected(v)
        }
    }
    impl ::core::convert::From<SimParam> for DefaultInputs {
        fn from(v: SimParam) -> DefaultInputs { DefaultInputs::SimParam(v) }
    }
    impl ::core::convert::From<Voltage> for DefaultInputs {
        fn from(v: Voltage) -> DefaultInputs { DefaultInputs::Voltage(v) }
    }
    impl ::core::convert::From<CurrentProbe> for DefaultInputs {
        fn from(v: CurrentProbe) -> DefaultInputs {
            DefaultInputs::CurrentProbe(v)
        }
    }
    impl ::core::convert::From<Temperature> for DefaultInputs {
        fn from(v: Temperature) -> DefaultInputs {
            DefaultInputs::Temperature(v)
        }
    }
    impl core::convert::TryInto<ParameterInput> for DefaultInputs {
        type Error = &'static str;
        fn try_into(self)
         ->
             ::core::result::Result<ParameterInput,
                                    <Self as
                                    core::convert::TryInto<ParameterInput>>::Error> {
            match self {
                DefaultInputs::Parameter(v) => { Ok(v) }
                DefaultInputs::PortConnected(v) => {
                    Err("Tried to convert variant PortConnected to Parameter")
                }
                DefaultInputs::SimParam(v) => {
                    Err("Tried to convert variant SimParam to Parameter")
                }
                DefaultInputs::Voltage(v) => {
                    Err("Tried to convert variant Voltage to Parameter")
                }
                DefaultInputs::CurrentProbe(v) => {
                    Err("Tried to convert variant CurrentProbe to Parameter")
                }
                DefaultInputs::Temperature(v) => {
                    Err("Tried to convert variant Temperature to Parameter")
                }
            }
        }
    }
    impl core::convert::TryInto<PortConnected> for DefaultInputs {
        type Error = &'static str;
        fn try_into(self)
         ->
             ::core::result::Result<PortConnected,
                                    <Self as
                                    core::convert::TryInto<PortConnected>>::Error> {
            match self {
                DefaultInputs::PortConnected(v) => { Ok(v) }
                DefaultInputs::Parameter(v) => {
                    Err("Tried to convert variant Parameter to PortConnected")
                }
                DefaultInputs::SimParam(v) => {
                    Err("Tried to convert variant SimParam to PortConnected")
                }
                DefaultInputs::Voltage(v) => {
                    Err("Tried to convert variant Voltage to PortConnected")
                }
                DefaultInputs::CurrentProbe(v) => {
                    Err("Tried to convert variant CurrentProbe to PortConnected")
                }
                DefaultInputs::Temperature(v) => {
                    Err("Tried to convert variant Temperature to PortConnected")
                }
            }
        }
    }
    impl core::convert::TryInto<SimParam> for DefaultInputs {
        type Error = &'static str;
        fn try_into(self)
         ->
             ::core::result::Result<SimParam,
                                    <Self as
                                    core::convert::TryInto<SimParam>>::Error> {
            match self {
                DefaultInputs::SimParam(v) => { Ok(v) }
                DefaultInputs::Parameter(v) => {
                    Err("Tried to convert variant Parameter to SimParam")
                }
                DefaultInputs::PortConnected(v) => {
                    Err("Tried to convert variant PortConnected to SimParam")
                }
                DefaultInputs::Voltage(v) => {
                    Err("Tried to convert variant Voltage to SimParam")
                }
                DefaultInputs::CurrentProbe(v) => {
                    Err("Tried to convert variant CurrentProbe to SimParam")
                }
                DefaultInputs::Temperature(v) => {
                    Err("Tried to convert variant Temperature to SimParam")
                }
            }
        }
    }
    impl core::convert::TryInto<Voltage> for DefaultInputs {
        type Error = &'static str;
        fn try_into(self)
         ->
             ::core::result::Result<Voltage,
                                    <Self as
                                    core::convert::TryInto<Voltage>>::Error> {
            match self {
                DefaultInputs::Voltage(v) => { Ok(v) }
                DefaultInputs::Parameter(v) => {
                    Err("Tried to convert variant Parameter to Voltage")
                }
                DefaultInputs::PortConnected(v) => {
                    Err("Tried to convert variant PortConnected to Voltage")
                }
                DefaultInputs::SimParam(v) => {
                    Err("Tried to convert variant SimParam to Voltage")
                }
                DefaultInputs::CurrentProbe(v) => {
                    Err("Tried to convert variant CurrentProbe to Voltage")
                }
                DefaultInputs::Temperature(v) => {
                    Err("Tried to convert variant Temperature to Voltage")
                }
            }
        }
    }
    impl core::convert::TryInto<CurrentProbe> for DefaultInputs {
        type Error = &'static str;
        fn try_into(self)
         ->
             ::core::result::Result<CurrentProbe,
                                    <Self as
                                    core::convert::TryInto<CurrentProbe>>::Error> {
            match self {
                DefaultInputs::CurrentProbe(v) => { Ok(v) }
                DefaultInputs::Parameter(v) => {
                    Err("Tried to convert variant Parameter to CurrentProbe")
                }
                DefaultInputs::PortConnected(v) => {
                    Err("Tried to convert variant PortConnected to CurrentProbe")
                }
                DefaultInputs::SimParam(v) => {
                    Err("Tried to convert variant SimParam to CurrentProbe")
                }
                DefaultInputs::Voltage(v) => {
                    Err("Tried to convert variant Voltage to CurrentProbe")
                }
                DefaultInputs::Temperature(v) => {
                    Err("Tried to convert variant Temperature to CurrentProbe")
                }
            }
        }
    }
    impl core::convert::TryInto<Temperature> for DefaultInputs {
        type Error = &'static str;
        fn try_into(self)
         ->
             ::core::result::Result<Temperature,
                                    <Self as
                                    core::convert::TryInto<Temperature>>::Error> {
            match self {
                DefaultInputs::Temperature(v) => { Ok(v) }
                DefaultInputs::Parameter(v) => {
                    Err("Tried to convert variant Parameter to Temperature")
                }
                DefaultInputs::PortConnected(v) => {
                    Err("Tried to convert variant PortConnected to Temperature")
                }
                DefaultInputs::SimParam(v) => {
                    Err("Tried to convert variant SimParam to Temperature")
                }
                DefaultInputs::Voltage(v) => {
                    Err("Tried to convert variant Voltage to Temperature")
                }
                DefaultInputs::CurrentProbe(v) => {
                    Err("Tried to convert variant CurrentProbe to Temperature")
                }
            }
        }
    }
    impl CfgInputs for DefaultInputs {
        #[inline]
        fn derivative<C: CfgFunctions>(&self, __enum_dispatch_arg_0: Unknown,
                                       __enum_dispatch_arg_1: &Mir<C>)
         -> InputDerivative {
            match self {
                DefaultInputs::Parameter(inner) =>
                CfgInputs::derivative::<C>(inner, __enum_dispatch_arg_0,
                                           __enum_dispatch_arg_1),
                DefaultInputs::PortConnected(inner) =>
                CfgInputs::derivative::<C>(inner, __enum_dispatch_arg_0,
                                           __enum_dispatch_arg_1),
                DefaultInputs::SimParam(inner) =>
                CfgInputs::derivative::<C>(inner, __enum_dispatch_arg_0,
                                           __enum_dispatch_arg_1),
                DefaultInputs::Voltage(inner) =>
                CfgInputs::derivative::<C>(inner, __enum_dispatch_arg_0,
                                           __enum_dispatch_arg_1),
                DefaultInputs::CurrentProbe(inner) =>
                CfgInputs::derivative::<C>(inner, __enum_dispatch_arg_0,
                                           __enum_dispatch_arg_1),
                DefaultInputs::Temperature(inner) =>
                CfgInputs::derivative::<C>(inner, __enum_dispatch_arg_0,
                                           __enum_dispatch_arg_1),
            }
        }
        #[inline]
        fn ty<C: CfgFunctions>(&self, __enum_dispatch_arg_0: &Mir<C>)
         -> Type {
            match self {
                DefaultInputs::Parameter(inner) =>
                CfgInputs::ty::<C>(inner, __enum_dispatch_arg_0),
                DefaultInputs::PortConnected(inner) =>
                CfgInputs::ty::<C>(inner, __enum_dispatch_arg_0),
                DefaultInputs::SimParam(inner) =>
                CfgInputs::ty::<C>(inner, __enum_dispatch_arg_0),
                DefaultInputs::Voltage(inner) =>
                CfgInputs::ty::<C>(inner, __enum_dispatch_arg_0),
                DefaultInputs::CurrentProbe(inner) =>
                CfgInputs::ty::<C>(inner, __enum_dispatch_arg_0),
                DefaultInputs::Temperature(inner) =>
                CfgInputs::ty::<C>(inner, __enum_dispatch_arg_0),
            }
        }
    }
    pub enum NoInput { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for NoInput { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for NoInput {
        #[inline]
        fn clone(&self) -> NoInput { { *self } }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for NoInput {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            unsafe { ::core::intrinsics::unreachable() }
        }
    }
    impl ::core::marker::StructuralPartialEq for NoInput { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for NoInput {
        #[inline]
        fn eq(&self, other: &NoInput) -> bool {
            unsafe { ::core::intrinsics::unreachable() }
        }
    }
    impl ::core::fmt::Display for NoInput {
        #[allow(unused_variables)]
        #[inline]
        fn fmt(&self,
               _derive_more_display_formatter: &mut ::core::fmt::Formatter)
         -> ::core::fmt::Result {
            match self { _ => Ok(()), }
        }
    }
    impl CfgInputs for NoInput {
        fn derivative<C: CfgFunctions>(&self, _unknown: Unknown,
                                       _mir: &Mir<C>) -> InputDerivative {
            match *self { }
        }
        fn ty<C: CfgFunctions>(&self, _mir: &Mir<C>) -> Type {
            {
                {
                    ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(&["internal error: entered unreachable code: "],
                                                                                &match (&"This cfg has no input",)
                                                                                     {
                                                                                     (arg0,)
                                                                                     =>
                                                                                     [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                   ::core::fmt::Display::fmt)],
                                                                                 }))
                }
            }
        }
    }
    impl <I: CfgInputs> From<InputDerivative> for Derivative<I> {
        fn from(from: InputDerivative) -> Self {
            match from {
                InputDerivative::One => Self::One,
                InputDerivative::Zero => Self::Zero,
                InputDerivative::Const(val) =>
                Self::Operand(OperandData::Constant(val)),
            }
        }
    }
    pub enum SimParamKind {

        #[display(fmt = "get_val,t y=real,opt=false")]
        Real,

        #[display(fmt = "get_val, ty=real,opt=true")]
        RealOptional,

        #[display(fmt = "is_val_given")]
        RealOptionalGiven,

        #[display(fmt = "get_val, ty=string, opt=false")]
        String,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for SimParamKind { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for SimParamKind {
        #[inline]
        fn clone(&self) -> SimParamKind { { *self } }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for SimParamKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match (&*self,) {
                (&SimParamKind::Real,) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "Real");
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&SimParamKind::RealOptional,) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                 "RealOptional");
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&SimParamKind::RealOptionalGiven,) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                 "RealOptionalGiven");
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&SimParamKind::String,) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "String");
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for SimParamKind { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for SimParamKind {
        #[inline]
        fn eq(&self, other: &SimParamKind) -> bool {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) { _ => true, }
                } else { false }
            }
        }
    }
    impl ::core::fmt::Display for SimParamKind {
        #[allow(unused_variables)]
        #[inline]
        fn fmt(&self,
               _derive_more_display_formatter: &mut ::core::fmt::Formatter)
         -> ::core::fmt::Result {
            match self {
                SimParamKind::Real =>
                _derive_more_display_formatter.write_fmt(::core::fmt::Arguments::new_v1(&["get_val,t y=real,opt=false"],
                                                                                        &match ()
                                                                                             {
                                                                                             ()
                                                                                             =>
                                                                                             [],
                                                                                         })),
                SimParamKind::RealOptional =>
                _derive_more_display_formatter.write_fmt(::core::fmt::Arguments::new_v1(&["get_val, ty=real,opt=true"],
                                                                                        &match ()
                                                                                             {
                                                                                             ()
                                                                                             =>
                                                                                             [],
                                                                                         })),
                SimParamKind::RealOptionalGiven =>
                _derive_more_display_formatter.write_fmt(::core::fmt::Arguments::new_v1(&["is_val_given"],
                                                                                        &match ()
                                                                                             {
                                                                                             ()
                                                                                             =>
                                                                                             [],
                                                                                         })),
                SimParamKind::String =>
                _derive_more_display_formatter.write_fmt(::core::fmt::Arguments::new_v1(&["get_val, ty=string, opt=false"],
                                                                                        &match ()
                                                                                             {
                                                                                             ()
                                                                                             =>
                                                                                             [],
                                                                                         })),
                _ => Ok(()),
            }
        }
    }
    #[display(fmt = "{}({}, {})", "sysfun::simparam", "self.name",
              "self.kind")]
    pub struct SimParam {
        pub name: StringLiteral,
        pub kind: SimParamKind,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for SimParam { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for SimParam {
        #[inline]
        fn clone(&self) -> SimParam {
            {
                let _: ::core::clone::AssertParamIsClone<StringLiteral>;
                let _: ::core::clone::AssertParamIsClone<SimParamKind>;
                *self
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for SimParam {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {
                SimParam { name: ref __self_0_0, kind: ref __self_0_1 } => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_struct(f,
                                                                  "SimParam");
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "name",
                                                        &&(*__self_0_0));
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "kind",
                                                        &&(*__self_0_1));
                    ::core::fmt::DebugStruct::finish(debug_trait_builder)
                }
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for SimParam { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for SimParam {
        #[inline]
        fn eq(&self, other: &SimParam) -> bool {
            match *other {
                SimParam { name: ref __self_1_0, kind: ref __self_1_1 } =>
                match *self {
                    SimParam { name: ref __self_0_0, kind: ref __self_0_1 } =>
                    (*__self_0_0) == (*__self_1_0) &&
                        (*__self_0_1) == (*__self_1_1),
                },
            }
        }
        #[inline]
        fn ne(&self, other: &SimParam) -> bool {
            match *other {
                SimParam { name: ref __self_1_0, kind: ref __self_1_1 } =>
                match *self {
                    SimParam { name: ref __self_0_0, kind: ref __self_0_1 } =>
                    (*__self_0_0) != (*__self_1_0) ||
                        (*__self_0_1) != (*__self_1_1),
                },
            }
        }
    }
    impl ::core::fmt::Display for SimParam {
        #[allow(unused_variables)]
        #[inline]
        fn fmt(&self,
               _derive_more_display_formatter: &mut ::core::fmt::Formatter)
         -> ::core::fmt::Result {
            match self {
                SimParam { name, kind } =>
                _derive_more_display_formatter.write_fmt(::core::fmt::Arguments::new_v1(&["",
                                                                                          "(",
                                                                                          ", ",
                                                                                          ")"],
                                                                                        &match (&sysfun::simparam,
                                                                                                &self.name,
                                                                                                &self.kind)
                                                                                             {
                                                                                             (arg0,
                                                                                              arg1,
                                                                                              arg2)
                                                                                             =>
                                                                                             [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                           ::core::fmt::Display::fmt),
                                                                                              ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                           ::core::fmt::Display::fmt),
                                                                                              ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                           ::core::fmt::Display::fmt)],
                                                                                         })),
                _ => Ok(()),
            }
        }
    }
    impl CfgInputs for SimParam {
        #[inline(always)]
        fn derivative<C: CfgFunctions>(&self, _unknown: Unknown,
                                       _mir: &Mir<C>) -> InputDerivative {
            InputDerivative::Zero
        }
        fn ty<C: CfgFunctions>(&self, _mir: &Mir<C>) -> Type {
            match self.kind {
                SimParamKind::String => Type::STRING,
                SimParamKind::Real => Type::REAL,
                SimParamKind::RealOptional => Type::REAL,
                SimParamKind::RealOptionalGiven => Type::BOOL,
            }
        }
    }
    #[display(fmt = "{}", "sysfun::temperature")]
    pub struct Temperature;
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for Temperature { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for Temperature {
        #[inline]
        fn clone(&self) -> Temperature { { *self } }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for Temperature {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {
                Temperature => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                 "Temperature");
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for Temperature { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for Temperature {
        #[inline]
        fn eq(&self, other: &Temperature) -> bool {
            match *other {
                Temperature => match *self { Temperature => true, },
            }
        }
    }
    impl ::core::fmt::Display for Temperature {
        #[allow(unused_variables)]
        #[inline]
        fn fmt(&self,
               _derive_more_display_formatter: &mut ::core::fmt::Formatter)
         -> ::core::fmt::Result {
            match self {
                Temperature =>
                _derive_more_display_formatter.write_fmt(::core::fmt::Arguments::new_v1(&[""],
                                                                                        &match (&sysfun::temperature,)
                                                                                             {
                                                                                             (arg0,)
                                                                                             =>
                                                                                             [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                           ::core::fmt::Display::fmt)],
                                                                                         })),
                _ => Ok(()),
            }
        }
    }
    impl CfgInputs for Temperature {
        #[inline]
        fn derivative<C: CfgFunctions>(&self, unknown: Unknown, _mir: &Mir<C>)
         -> InputDerivative {
            if unknown == Unknown::Temperature {
                InputDerivative::One
            } else { InputDerivative::Zero }
        }
        #[inline(always)]
        fn ty<C: CfgFunctions>(&self, _mir: &Mir<C>) -> Type { Type::REAL }
    }
    #[display(fmt = "{}({})", "sysfun::port_connected", "0")]
    pub struct PortConnected(pub PortId);
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for PortConnected { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for PortConnected {
        #[inline]
        fn clone(&self) -> PortConnected {
            { let _: ::core::clone::AssertParamIsClone<PortId>; *self }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for PortConnected {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {
                PortConnected(ref __self_0_0) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                 "PortConnected");
                    let _ =
                        ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                       &&(*__self_0_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for PortConnected { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for PortConnected {
        #[inline]
        fn eq(&self, other: &PortConnected) -> bool {
            match *other {
                PortConnected(ref __self_1_0) =>
                match *self {
                    PortConnected(ref __self_0_0) =>
                    (*__self_0_0) == (*__self_1_0),
                },
            }
        }
        #[inline]
        fn ne(&self, other: &PortConnected) -> bool {
            match *other {
                PortConnected(ref __self_1_0) =>
                match *self {
                    PortConnected(ref __self_0_0) =>
                    (*__self_0_0) != (*__self_1_0),
                },
            }
        }
    }
    impl ::core::fmt::Display for PortConnected {
        #[allow(unused_variables)]
        #[inline]
        fn fmt(&self,
               _derive_more_display_formatter: &mut ::core::fmt::Formatter)
         -> ::core::fmt::Result {
            match self {
                PortConnected(_0) =>
                _derive_more_display_formatter.write_fmt(::core::fmt::Arguments::new_v1(&["",
                                                                                          "(",
                                                                                          ")"],
                                                                                        &match (&sysfun::port_connected,
                                                                                                &0)
                                                                                             {
                                                                                             (arg0,
                                                                                              arg1)
                                                                                             =>
                                                                                             [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                           ::core::fmt::Display::fmt),
                                                                                              ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                           ::core::fmt::Display::fmt)],
                                                                                         })),
                _ => Ok(()),
            }
        }
    }
    impl CfgInputs for PortConnected {
        #[inline]
        fn derivative<C: CfgFunctions>(&self, _unknown: Unknown,
                                       _mir: &Mir<C>) -> InputDerivative {
            {
                ::core::panicking::panic("internal error: entered unreachable code")
            }
        }
        #[inline(always)]
        fn ty<C: CfgFunctions>(&self, _mir: &Mir<C>) -> Type { Type::BOOL }
    }
    #[display(fmt = "{}({},{})", "kw::potential", "lo", "hi")]
    pub struct Voltage {
        pub hi: NetId,
        pub lo: NetId,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for Voltage { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for Voltage {
        #[inline]
        fn clone(&self) -> Voltage {
            {
                let _: ::core::clone::AssertParamIsClone<NetId>;
                let _: ::core::clone::AssertParamIsClone<NetId>;
                *self
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for Voltage {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {
                Voltage { hi: ref __self_0_0, lo: ref __self_0_1 } => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_struct(f,
                                                                  "Voltage");
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "hi",
                                                        &&(*__self_0_0));
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "lo",
                                                        &&(*__self_0_1));
                    ::core::fmt::DebugStruct::finish(debug_trait_builder)
                }
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for Voltage { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for Voltage {
        #[inline]
        fn eq(&self, other: &Voltage) -> bool {
            match *other {
                Voltage { hi: ref __self_1_0, lo: ref __self_1_1 } =>
                match *self {
                    Voltage { hi: ref __self_0_0, lo: ref __self_0_1 } =>
                    (*__self_0_0) == (*__self_1_0) &&
                        (*__self_0_1) == (*__self_1_1),
                },
            }
        }
        #[inline]
        fn ne(&self, other: &Voltage) -> bool {
            match *other {
                Voltage { hi: ref __self_1_0, lo: ref __self_1_1 } =>
                match *self {
                    Voltage { hi: ref __self_0_0, lo: ref __self_0_1 } =>
                    (*__self_0_0) != (*__self_1_0) ||
                        (*__self_0_1) != (*__self_1_1),
                },
            }
        }
    }
    impl ::core::fmt::Display for Voltage {
        #[allow(unused_variables)]
        #[inline]
        fn fmt(&self,
               _derive_more_display_formatter: &mut ::core::fmt::Formatter)
         -> ::core::fmt::Result {
            match self {
                Voltage { hi, lo } =>
                _derive_more_display_formatter.write_fmt(::core::fmt::Arguments::new_v1(&["",
                                                                                          "(",
                                                                                          ",",
                                                                                          ")"],
                                                                                        &match (&kw::potential,
                                                                                                &lo,
                                                                                                &hi)
                                                                                             {
                                                                                             (arg0,
                                                                                              arg1,
                                                                                              arg2)
                                                                                             =>
                                                                                             [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                           ::core::fmt::Display::fmt),
                                                                                              ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                           ::core::fmt::Display::fmt),
                                                                                              ::core::fmt::ArgumentV1::new(arg2,
                                                                                                                           ::core::fmt::Display::fmt)],
                                                                                         })),
                _ => Ok(()),
            }
        }
    }
    impl CfgInputs for Voltage {
        #[inline]
        fn derivative<C: CfgFunctions>(&self, unknown: Unknown, _mir: &Mir<C>)
         -> InputDerivative {
            match unknown {
                Unknown::NodePotential(node) if self.hi == node =>
                InputDerivative::One,
                Unknown::NodePotential(node) if self.lo == node => {
                    InputDerivative::Const(Scalar(Real(-1.0)))
                }
                Unknown::BranchPotential(hi_demanded, lo_demanded) if
                self.hi == hi_demanded && self.lo == lo_demanded => {
                    InputDerivative::One
                }
                Unknown::BranchPotential(hi_demanded, lo_demanded) if
                self.lo == hi_demanded && self.hi == lo_demanded => {
                    InputDerivative::Const(Scalar(Real(-1.0)))
                }
                _ => InputDerivative::Zero,
            }
        }
        #[inline(always)]
        fn ty<C: CfgFunctions>(&self, _mir: &Mir<C>) -> Type { Type::REAL }
    }
    #[display(fmt = "{}({})", "kw::flow", "0")]
    pub struct CurrentProbe(BranchId);
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for CurrentProbe { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for CurrentProbe {
        #[inline]
        fn clone(&self) -> CurrentProbe {
            { let _: ::core::clone::AssertParamIsClone<BranchId>; *self }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for CurrentProbe {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {
                CurrentProbe(ref __self_0_0) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f,
                                                                 "CurrentProbe");
                    let _ =
                        ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                       &&(*__self_0_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for CurrentProbe { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for CurrentProbe {
        #[inline]
        fn eq(&self, other: &CurrentProbe) -> bool {
            match *other {
                CurrentProbe(ref __self_1_0) =>
                match *self {
                    CurrentProbe(ref __self_0_0) =>
                    (*__self_0_0) == (*__self_1_0),
                },
            }
        }
        #[inline]
        fn ne(&self, other: &CurrentProbe) -> bool {
            match *other {
                CurrentProbe(ref __self_1_0) =>
                match *self {
                    CurrentProbe(ref __self_0_0) =>
                    (*__self_0_0) != (*__self_1_0),
                },
            }
        }
    }
    impl ::core::fmt::Display for CurrentProbe {
        #[allow(unused_variables)]
        #[inline]
        fn fmt(&self,
               _derive_more_display_formatter: &mut ::core::fmt::Formatter)
         -> ::core::fmt::Result {
            match self {
                CurrentProbe(_0) =>
                _derive_more_display_formatter.write_fmt(::core::fmt::Arguments::new_v1(&["",
                                                                                          "(",
                                                                                          ")"],
                                                                                        &match (&kw::flow,
                                                                                                &0)
                                                                                             {
                                                                                             (arg0,
                                                                                              arg1)
                                                                                             =>
                                                                                             [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                           ::core::fmt::Display::fmt),
                                                                                              ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                           ::core::fmt::Display::fmt)],
                                                                                         })),
                _ => Ok(()),
            }
        }
    }
    impl CfgInputs for CurrentProbe {
        #[inline]
        fn derivative<C: CfgFunctions>(&self, unknown: Unknown, _mir: &Mir<C>)
         -> InputDerivative {
            if match unknown {
                   Unknown::Flow(branch) if branch == self.0 => true,
                   _ => false,
               } {
                InputDerivative::One
            } else { InputDerivative::Zero }
        }
        #[inline(always)]
        fn ty<C: CfgFunctions>(&self, _mir: &Mir<C>) -> Type { Type::REAL }
    }
    pub enum ParameterInput {
        Value(ParameterId),

        #[display(fmt = "{}({})", "sysfun::param_given", "0")]
        Given(ParameterId),
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for ParameterInput { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for ParameterInput {
        #[inline]
        fn clone(&self) -> ParameterInput {
            {
                let _: ::core::clone::AssertParamIsClone<ParameterId>;
                let _: ::core::clone::AssertParamIsClone<ParameterId>;
                *self
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for ParameterInput {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match (&*self,) {
                (&ParameterInput::Value(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "Value");
                    let _ =
                        ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                       &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&ParameterInput::Given(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "Given");
                    let _ =
                        ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                       &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
            }
        }
    }
    impl ::core::marker::StructuralEq for ParameterInput { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for ParameterInput {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            {
                let _: ::core::cmp::AssertParamIsEq<ParameterId>;
                let _: ::core::cmp::AssertParamIsEq<ParameterId>;
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for ParameterInput { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for ParameterInput {
        #[inline]
        fn eq(&self, other: &ParameterInput) -> bool {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        (&ParameterInput::Value(ref __self_0),
                         &ParameterInput::Value(ref __arg_1_0)) =>
                        (*__self_0) == (*__arg_1_0),
                        (&ParameterInput::Given(ref __self_0),
                         &ParameterInput::Given(ref __arg_1_0)) =>
                        (*__self_0) == (*__arg_1_0),
                        _ => unsafe { ::core::intrinsics::unreachable() }
                    }
                } else { false }
            }
        }
        #[inline]
        fn ne(&self, other: &ParameterInput) -> bool {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        (&ParameterInput::Value(ref __self_0),
                         &ParameterInput::Value(ref __arg_1_0)) =>
                        (*__self_0) != (*__arg_1_0),
                        (&ParameterInput::Given(ref __self_0),
                         &ParameterInput::Given(ref __arg_1_0)) =>
                        (*__self_0) != (*__arg_1_0),
                        _ => unsafe { ::core::intrinsics::unreachable() }
                    }
                } else { true }
            }
        }
    }
    impl ::core::fmt::Display for ParameterInput {
        #[allow(unused_variables)]
        #[inline]
        fn fmt(&self,
               _derive_more_display_formatter: &mut ::core::fmt::Formatter)
         -> ::core::fmt::Result {
            match self {
                ParameterInput::Value(_0) =>
                ::core::fmt::Display::fmt(_0, _derive_more_display_formatter),
                ParameterInput::Given(_0) =>
                _derive_more_display_formatter.write_fmt(::core::fmt::Arguments::new_v1(&["",
                                                                                          "(",
                                                                                          ")"],
                                                                                        &match (&sysfun::param_given,
                                                                                                &0)
                                                                                             {
                                                                                             (arg0,
                                                                                              arg1)
                                                                                             =>
                                                                                             [::core::fmt::ArgumentV1::new(arg0,
                                                                                                                           ::core::fmt::Display::fmt),
                                                                                              ::core::fmt::ArgumentV1::new(arg1,
                                                                                                                           ::core::fmt::Display::fmt)],
                                                                                         })),
                _ => Ok(()),
            }
        }
    }
    impl CfgInputs for ParameterInput {
        fn derivative<C: CfgFunctions>(&self, unknown: Unknown, _mir: &Mir<C>)
         -> InputDerivative {
            if match (unknown, self) {
                   (Unknown::Parameter(x), Self::Value(y)) if &x == y => true,
                   _ => false,
               } {
                InputDerivative::One
            } else { InputDerivative::Zero }
        }
        fn ty<C: CfgFunctions>(&self, mir: &Mir<C>) -> Type {
            match self {
                Self::Value(param) => mir[*param].ty,
                Self::Given(_) => Type::BOOL,
            }
        }
    }
    impl From<NoInput> for DefaultInputs {
        fn from(src: NoInput) -> Self { match src { } }
    }
    impl TryInto<NoInput> for DefaultInputs {
        type Error = ();
        fn try_into(self) -> Result<NoInput, Self::Error> { Err(()) }
    }
}
mod util {
    use std::mem::MaybeUninit;
    #[repr(u8)]
    enum AtMostTwo { Zero = 0, One = 1, Two = 2, }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for AtMostTwo {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match (&*self,) {
                (&AtMostTwo::Zero,) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "Zero");
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&AtMostTwo::One,) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "One");
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&AtMostTwo::Two,) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "Two");
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for AtMostTwo { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for AtMostTwo {
        #[inline]
        fn clone(&self) -> AtMostTwo { { *self } }
    }
    impl ::core::marker::StructuralEq for AtMostTwo { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for AtMostTwo {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () { { } }
    }
    impl ::core::marker::StructuralPartialEq for AtMostTwo { }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for AtMostTwo {
        #[inline]
        fn eq(&self, other: &AtMostTwo) -> bool {
            {
                let __self_vi =
                    ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi =
                    ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) { _ => true, }
                } else { false }
            }
        }
    }
    impl AtMostTwo {
        fn decriment(self) -> Option<Self> {
            match self {
                Self::Zero => None,
                Self::One => Some(Self::Zero),
                Self::Two => Some(Self::One),
            }
        }
    }
    pub struct AtMostTwoIter<T> {
        data: [MaybeUninit<T>; 2],
        len: AtMostTwo,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl <T: ::core::fmt::Debug> ::core::fmt::Debug for AtMostTwoIter<T> {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {
                AtMostTwoIter { data: ref __self_0_0, len: ref __self_0_1 } =>
                {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_struct(f,
                                                                  "AtMostTwoIter");
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "data",
                                                        &&(*__self_0_0));
                    let _ =
                        ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                        "len",
                                                        &&(*__self_0_1));
                    ::core::fmt::DebugStruct::finish(debug_trait_builder)
                }
            }
        }
    }
    impl <T: Clone> Clone for AtMostTwoIter<T> {
        fn clone(&self) -> Self {
            unsafe {
                match self.len {
                    AtMostTwo::Zero => Self::new_empty(),
                    AtMostTwo::One =>
                    Self::new_single((&*self.data[0].as_ptr()).clone()),
                    AtMostTwo::Two =>
                    Self::new_double((&*self.data[0].as_ptr()).clone(),
                                     (&*self.data[1].as_ptr()).clone()),
                }
            }
        }
    }
    impl <T: Copy + Clone> Copy for AtMostTwoIter<T> { }
    impl <T> AtMostTwoIter<T> {
        #[inline]
        #[must_use]
        pub const fn new_empty() -> Self {
            Self{data: [MaybeUninit::uninit(), MaybeUninit::uninit()],
                 len: AtMostTwo::Zero,}
        }
        #[inline]
        #[must_use]
        pub const fn new_single(val: T) -> Self {
            Self{data: [MaybeUninit::new(val), MaybeUninit::uninit()],
                 len: AtMostTwo::One,}
        }
        #[inline]
        #[must_use]
        pub const fn new_double(val1: T, val2: T) -> Self {
            Self{data: [MaybeUninit::new(val1), MaybeUninit::new(val2)],
                 len: AtMostTwo::Two,}
        }
    }
    impl <T> Iterator for AtMostTwoIter<T> {
        type Item = T;
        fn next(&mut self) -> Option<Self::Item> {
            let len = self.len.decriment()?;
            self.len = len;
            Some(unsafe { std::ptr::read(self.data[len as usize].as_ptr()) })
        }
        fn size_hint(&self) -> (usize, Option<usize>) {
            (self.len as usize, Some(self.len as usize))
        }
    }
    impl <T: Clone + Copy> ExactSizeIterator for AtMostTwoIter<T> {
        fn len(&self) -> usize { self.len as usize }
    }
}
pub type ConstVal = osdi_types::ConstVal<StringLiteral>;
pub type SimpleConstVal = osdi_types::SimpleConstVal<StringLiteral>;
use crate::derivatives::RValueAutoDiff;
use crate::dfa::lattice::FlatSet;
use crate::functions::DefaultFunctions;
pub use crate::inputs::CfgInputs;
use crate::inputs::{NoInput, ParameterInput};
use crate::osdi_types::ConstVal::Scalar;
use data_structures::arrayvec::ArrayVec;
use data_structures::{index_vec::index_vec, iter::Itertools,
                              sync::RwLock, HashMap};
use diagnostics::ListFormatter;
use ir::ids::CallArg;
use ir::{Math1, Math2, Noise, Print, StopTask};
pub use osdi_types;
use osdi_types::SimpleConstVal::Real;
use std::fmt;
pub struct SyntaxContextData {
    pub span: Span,
    pub lint_levels: HashMap<Lint, LintLevel>,
    pub parent: Option<SyntaxCtx>,
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::fmt::Debug for SyntaxContextData {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match *self {
            SyntaxContextData {
            span: ref __self_0_0,
            lint_levels: ref __self_0_1,
            parent: ref __self_0_2 } => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_struct(f,
                                                              "SyntaxContextData");
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "span", &&(*__self_0_0));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "lint_levels",
                                                    &&(*__self_0_1));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "parent",
                                                    &&(*__self_0_2));
                ::core::fmt::DebugStruct::finish(debug_trait_builder)
            }
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::clone::Clone for SyntaxContextData {
    #[inline]
    fn clone(&self) -> SyntaxContextData {
        match *self {
            SyntaxContextData {
            span: ref __self_0_0,
            lint_levels: ref __self_0_1,
            parent: ref __self_0_2 } =>
            SyntaxContextData{span:
                                  ::core::clone::Clone::clone(&(*__self_0_0)),
                              lint_levels:
                                  ::core::clone::Clone::clone(&(*__self_0_1)),
                              parent:
                                  ::core::clone::Clone::clone(&(*__self_0_2)),},
        }
    }
}
pub type Statement<C> = (StmntKind<C>, SyntaxCtx);
impl <C: CfgFunctions> ::std::ops::Index<SyntaxCtx> for Mir<C> {
    type Output = SyntaxContextData;
    fn index(&self, index: SyntaxCtx) -> &Self::Output {
        &self.syntax_ctx[index]
    }
}
impl <C: CfgFunctions> ::std::ops::IndexMut<SyntaxCtx> for Mir<C> {
    fn index_mut(&mut self, index: SyntaxCtx) -> &mut Self::Output {
        &mut self.syntax_ctx[index]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<::core::ops::Range<SyntaxCtx>> for
 Mir<C> {
    type Output =
     data_structures::index_vec::IndexSlice<SyntaxCtx,
                                                    [SyntaxContextData]>;
    fn index(&self, range: ::core::ops::Range<SyntaxCtx>) -> &Self::Output {
        &self.syntax_ctx[range]
    }
}
impl <C: CfgFunctions> ::std::ops::IndexMut<::core::ops::Range<SyntaxCtx>> for
 Mir<C> {
    fn index_mut(&mut self, range: ::core::ops::Range<SyntaxCtx>)
     -> &mut Self::Output {
        &mut self.syntax_ctx[range]
    }
}
impl <C: CfgFunctions>
 ::std::ops::Index<::ir::ids::IdRange<SyntaxCtx>> for Mir<C> {
    type Output =
     ::data_structures::index_vec::IndexSlice<SyntaxCtx,
                                                      [SyntaxContextData]>;
    fn index(&self, range: ::ir::ids::IdRange<SyntaxCtx>)
     -> &Self::Output {
        &self.syntax_ctx[range.0]
    }
}
impl <C: CfgFunctions>
 ::std::ops::IndexMut<::ir::ids::IdRange<SyntaxCtx>> for Mir<C> {
    fn index_mut(&mut self, range: ::ir::ids::IdRange<SyntaxCtx>)
     -> &mut Self::Output {
        &mut self.syntax_ctx[range.0]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<BranchId> for Mir<C> {
    type Output = Branch;
    fn index(&self, index: BranchId) -> &Self::Output {
        &self.branches[index]
    }
}
impl <C: CfgFunctions> ::std::ops::IndexMut<BranchId> for Mir<C> {
    fn index_mut(&mut self, index: BranchId) -> &mut Self::Output {
        &mut self.branches[index]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<::core::ops::Range<BranchId>> for
 Mir<C> {
    type Output =
     data_structures::index_vec::IndexSlice<BranchId, [Branch]>;
    fn index(&self, range: ::core::ops::Range<BranchId>) -> &Self::Output {
        &self.branches[range]
    }
}
impl <C: CfgFunctions> ::std::ops::IndexMut<::core::ops::Range<BranchId>> for
 Mir<C> {
    fn index_mut(&mut self, range: ::core::ops::Range<BranchId>)
     -> &mut Self::Output {
        &mut self.branches[range]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<::ir::ids::IdRange<BranchId>>
 for Mir<C> {
    type Output =
     ::data_structures::index_vec::IndexSlice<BranchId, [Branch]>;
    fn index(&self, range: ::ir::ids::IdRange<BranchId>)
     -> &Self::Output {
        &self.branches[range.0]
    }
}
impl <C: CfgFunctions>
 ::std::ops::IndexMut<::ir::ids::IdRange<BranchId>> for Mir<C> {
    fn index_mut(&mut self, range: ::ir::ids::IdRange<BranchId>)
     -> &mut Self::Output {
        &mut self.branches[range.0]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<NetId> for Mir<C> {
    type Output = Net;
    fn index(&self, index: NetId) -> &Self::Output { &self.nets[index] }
}
impl <C: CfgFunctions> ::std::ops::IndexMut<NetId> for Mir<C> {
    fn index_mut(&mut self, index: NetId) -> &mut Self::Output {
        &mut self.nets[index]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<::core::ops::Range<NetId>> for Mir<C>
 {
    type Output =
     data_structures::index_vec::IndexSlice<NetId, [Net]>;
    fn index(&self, range: ::core::ops::Range<NetId>) -> &Self::Output {
        &self.nets[range]
    }
}
impl <C: CfgFunctions> ::std::ops::IndexMut<::core::ops::Range<NetId>> for
 Mir<C> {
    fn index_mut(&mut self, range: ::core::ops::Range<NetId>)
     -> &mut Self::Output {
        &mut self.nets[range]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<::ir::ids::IdRange<NetId>>
 for Mir<C> {
    type Output =
     ::data_structures::index_vec::IndexSlice<NetId, [Net]>;
    fn index(&self, range: ::ir::ids::IdRange<NetId>)
     -> &Self::Output {
        &self.nets[range.0]
    }
}
impl <C: CfgFunctions> ::std::ops::IndexMut<::ir::ids::IdRange<NetId>>
 for Mir<C> {
    fn index_mut(&mut self, range: ::ir::ids::IdRange<NetId>)
     -> &mut Self::Output {
        &mut self.nets[range.0]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<PortId> for Mir<C> {
    type Output = Port;
    fn index(&self, index: PortId) -> &Self::Output { &self.ports[index] }
}
impl <C: CfgFunctions> ::std::ops::IndexMut<PortId> for Mir<C> {
    fn index_mut(&mut self, index: PortId) -> &mut Self::Output {
        &mut self.ports[index]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<::core::ops::Range<PortId>> for
 Mir<C> {
    type Output =
     data_structures::index_vec::IndexSlice<PortId, [Port]>;
    fn index(&self, range: ::core::ops::Range<PortId>) -> &Self::Output {
        &self.ports[range]
    }
}
impl <C: CfgFunctions> ::std::ops::IndexMut<::core::ops::Range<PortId>> for
 Mir<C> {
    fn index_mut(&mut self, range: ::core::ops::Range<PortId>)
     -> &mut Self::Output {
        &mut self.ports[range]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<::ir::ids::IdRange<PortId>>
 for Mir<C> {
    type Output =
     ::data_structures::index_vec::IndexSlice<PortId, [Port]>;
    fn index(&self, range: ::ir::ids::IdRange<PortId>)
     -> &Self::Output {
        &self.ports[range.0]
    }
}
impl <C: CfgFunctions>
 ::std::ops::IndexMut<::ir::ids::IdRange<PortId>> for Mir<C> {
    fn index_mut(&mut self, range: ::ir::ids::IdRange<PortId>)
     -> &mut Self::Output {
        &mut self.ports[range.0]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<VariableId> for Mir<C> {
    type Output = Variable;
    fn index(&self, index: VariableId) -> &Self::Output {
        &self.variables[index]
    }
}
impl <C: CfgFunctions> ::std::ops::IndexMut<VariableId> for Mir<C> {
    fn index_mut(&mut self, index: VariableId) -> &mut Self::Output {
        &mut self.variables[index]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<::core::ops::Range<VariableId>> for
 Mir<C> {
    type Output =
     data_structures::index_vec::IndexSlice<VariableId, [Variable]>;
    fn index(&self, range: ::core::ops::Range<VariableId>) -> &Self::Output {
        &self.variables[range]
    }
}
impl <C: CfgFunctions> ::std::ops::IndexMut<::core::ops::Range<VariableId>>
 for Mir<C> {
    fn index_mut(&mut self, range: ::core::ops::Range<VariableId>)
     -> &mut Self::Output {
        &mut self.variables[range]
    }
}
impl <C: CfgFunctions>
 ::std::ops::Index<::ir::ids::IdRange<VariableId>> for Mir<C> {
    type Output =
     ::data_structures::index_vec::IndexSlice<VariableId, [Variable]>;
    fn index(&self, range: ::ir::ids::IdRange<VariableId>)
     -> &Self::Output {
        &self.variables[range.0]
    }
}
impl <C: CfgFunctions>
 ::std::ops::IndexMut<::ir::ids::IdRange<VariableId>> for Mir<C> {
    fn index_mut(&mut self, range: ::ir::ids::IdRange<VariableId>)
     -> &mut Self::Output {
        &mut self.variables[range.0]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<ModuleId> for Mir<C> {
    type Output = Module<C>;
    fn index(&self, index: ModuleId) -> &Self::Output { &self.modules[index] }
}
impl <C: CfgFunctions> ::std::ops::IndexMut<ModuleId> for Mir<C> {
    fn index_mut(&mut self, index: ModuleId) -> &mut Self::Output {
        &mut self.modules[index]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<::core::ops::Range<ModuleId>> for
 Mir<C> {
    type Output =
     data_structures::index_vec::IndexSlice<ModuleId, [Module<C>]>;
    fn index(&self, range: ::core::ops::Range<ModuleId>) -> &Self::Output {
        &self.modules[range]
    }
}
impl <C: CfgFunctions> ::std::ops::IndexMut<::core::ops::Range<ModuleId>> for
 Mir<C> {
    fn index_mut(&mut self, range: ::core::ops::Range<ModuleId>)
     -> &mut Self::Output {
        &mut self.modules[range]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<::ir::ids::IdRange<ModuleId>>
 for Mir<C> {
    type Output =
     ::data_structures::index_vec::IndexSlice<ModuleId, [Module<C>]>;
    fn index(&self, range: ::ir::ids::IdRange<ModuleId>)
     -> &Self::Output {
        &self.modules[range.0]
    }
}
impl <C: CfgFunctions>
 ::std::ops::IndexMut<::ir::ids::IdRange<ModuleId>> for Mir<C> {
    fn index_mut(&mut self, range: ::ir::ids::IdRange<ModuleId>)
     -> &mut Self::Output {
        &mut self.modules[range.0]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<DisciplineId> for Mir<C> {
    type Output = Discipline;
    fn index(&self, index: DisciplineId) -> &Self::Output {
        &self.disciplines[index]
    }
}
impl <C: CfgFunctions> ::std::ops::IndexMut<DisciplineId> for Mir<C> {
    fn index_mut(&mut self, index: DisciplineId) -> &mut Self::Output {
        &mut self.disciplines[index]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<::core::ops::Range<DisciplineId>> for
 Mir<C> {
    type Output =
     data_structures::index_vec::IndexSlice<DisciplineId,
                                                    [Discipline]>;
    fn index(&self, range: ::core::ops::Range<DisciplineId>)
     -> &Self::Output {
        &self.disciplines[range]
    }
}
impl <C: CfgFunctions> ::std::ops::IndexMut<::core::ops::Range<DisciplineId>>
 for Mir<C> {
    fn index_mut(&mut self, range: ::core::ops::Range<DisciplineId>)
     -> &mut Self::Output {
        &mut self.disciplines[range]
    }
}
impl <C: CfgFunctions>
 ::std::ops::Index<::ir::ids::IdRange<DisciplineId>> for Mir<C> {
    type Output =
     ::data_structures::index_vec::IndexSlice<DisciplineId,
                                                      [Discipline]>;
    fn index(&self, range: ::ir::ids::IdRange<DisciplineId>)
     -> &Self::Output {
        &self.disciplines[range.0]
    }
}
impl <C: CfgFunctions>
 ::std::ops::IndexMut<::ir::ids::IdRange<DisciplineId>> for Mir<C> {
    fn index_mut(&mut self, range: ::ir::ids::IdRange<DisciplineId>)
     -> &mut Self::Output {
        &mut self.disciplines[range.0]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<NatureId> for Mir<C> {
    type Output = Nature;
    fn index(&self, index: NatureId) -> &Self::Output { &self.natures[index] }
}
impl <C: CfgFunctions> ::std::ops::IndexMut<NatureId> for Mir<C> {
    fn index_mut(&mut self, index: NatureId) -> &mut Self::Output {
        &mut self.natures[index]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<::core::ops::Range<NatureId>> for
 Mir<C> {
    type Output =
     data_structures::index_vec::IndexSlice<NatureId, [Nature]>;
    fn index(&self, range: ::core::ops::Range<NatureId>) -> &Self::Output {
        &self.natures[range]
    }
}
impl <C: CfgFunctions> ::std::ops::IndexMut<::core::ops::Range<NatureId>> for
 Mir<C> {
    fn index_mut(&mut self, range: ::core::ops::Range<NatureId>)
     -> &mut Self::Output {
        &mut self.natures[range]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<::ir::ids::IdRange<NatureId>>
 for Mir<C> {
    type Output =
     ::data_structures::index_vec::IndexSlice<NatureId, [Nature]>;
    fn index(&self, range: ::ir::ids::IdRange<NatureId>)
     -> &Self::Output {
        &self.natures[range.0]
    }
}
impl <C: CfgFunctions>
 ::std::ops::IndexMut<::ir::ids::IdRange<NatureId>> for Mir<C> {
    fn index_mut(&mut self, range: ::ir::ids::IdRange<NatureId>)
     -> &mut Self::Output {
        &mut self.natures[range.0]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<ParameterId> for Mir<C> {
    type Output = Parameter;
    fn index(&self, index: ParameterId) -> &Self::Output {
        &self.parameters[index]
    }
}
impl <C: CfgFunctions> ::std::ops::IndexMut<ParameterId> for Mir<C> {
    fn index_mut(&mut self, index: ParameterId) -> &mut Self::Output {
        &mut self.parameters[index]
    }
}
impl <C: CfgFunctions> ::std::ops::Index<::core::ops::Range<ParameterId>> for
 Mir<C> {
    type Output =
     data_structures::index_vec::IndexSlice<ParameterId, [Parameter]>;
    fn index(&self, range: ::core::ops::Range<ParameterId>) -> &Self::Output {
        &self.parameters[range]
    }
}
impl <C: CfgFunctions> ::std::ops::IndexMut<::core::ops::Range<ParameterId>>
 for Mir<C> {
    fn index_mut(&mut self, range: ::core::ops::Range<ParameterId>)
     -> &mut Self::Output {
        &mut self.parameters[range]
    }
}
impl <C: CfgFunctions>
 ::std::ops::Index<::ir::ids::IdRange<ParameterId>> for Mir<C> {
    type Output =
     ::data_structures::index_vec::IndexSlice<ParameterId,
                                                      [Parameter]>;
    fn index(&self, range: ::ir::ids::IdRange<ParameterId>)
     -> &Self::Output {
        &self.parameters[range.0]
    }
}
impl <C: CfgFunctions>
 ::std::ops::IndexMut<::ir::ids::IdRange<ParameterId>> for Mir<C> {
    fn index_mut(&mut self, range: ::ir::ids::IdRange<ParameterId>)
     -> &mut Self::Output {
        &mut self.parameters[range.0]
    }
}
pub type CallTypeDerivative<C> = Derivative<<C as CfgFunctions>::I>;
pub enum Derivative<I: CfgInputs> { One, Zero, Operand(OperandData<I>), }
#[automatically_derived]
#[allow(unused_qualifications)]
impl <I: ::core::clone::Clone + CfgInputs> ::core::clone::Clone for
 Derivative<I> {
    #[inline]
    fn clone(&self) -> Derivative<I> {
        match (&*self,) {
            (&Derivative::One,) => Derivative::One,
            (&Derivative::Zero,) => Derivative::Zero,
            (&Derivative::Operand(ref __self_0),) =>
            Derivative::Operand(::core::clone::Clone::clone(&(*__self_0))),
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl <I: ::core::fmt::Debug + CfgInputs> ::core::fmt::Debug for Derivative<I>
 {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match (&*self,) {
            (&Derivative::One,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "One");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&Derivative::Zero,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "Zero");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&Derivative::Operand(ref __self_0),) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "Operand");
                let _ =
                    ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                   &&(*__self_0));
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
        }
    }
}
impl <I: CfgInputs> ::core::marker::StructuralPartialEq for Derivative<I> { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl <I: ::core::cmp::PartialEq + CfgInputs> ::core::cmp::PartialEq for
 Derivative<I> {
    #[inline]
    fn eq(&self, other: &Derivative<I>) -> bool {
        {
            let __self_vi = ::core::intrinsics::discriminant_value(&*self);
            let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
            if true && __self_vi == __arg_1_vi {
                match (&*self, &*other) {
                    (&Derivative::Operand(ref __self_0),
                     &Derivative::Operand(ref __arg_1_0)) =>
                    (*__self_0) == (*__arg_1_0),
                    _ => true,
                }
            } else { false }
        }
    }
    #[inline]
    fn ne(&self, other: &Derivative<I>) -> bool {
        {
            let __self_vi = ::core::intrinsics::discriminant_value(&*self);
            let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
            if true && __self_vi == __arg_1_vi {
                match (&*self, &*other) {
                    (&Derivative::Operand(ref __self_0),
                     &Derivative::Operand(ref __arg_1_0)) =>
                    (*__self_0) != (*__arg_1_0),
                    _ => false,
                }
            } else { true }
        }
    }
}
impl <I: CfgInputs> Derivative<I> {
    #[inline]
    pub fn map_input<X: CfgInputs>(self, map: impl FnOnce(I) -> X)
     -> Derivative<X> {
        match self {
            Derivative::One => Derivative::One,
            Derivative::Zero => Derivative::Zero,
            Derivative::Operand(op) => Derivative::Operand(op.map_input(map)),
        }
    }
    pub fn into_operand(self) -> OperandData<I> {
        match self {
            Self::One => OperandData::Constant(1.0.into()),
            Self::Zero => OperandData::Constant(0.0.into()),
            Self::Operand(operand) => operand,
        }
    }
    pub fn into_option(self) -> Option<OperandData<I>> {
        match self {
            Self::One => Some(OperandData::Constant(1.0.into())),
            Self::Zero => None,
            Self::Operand(operand) => Some(operand),
        }
    }
}
pub enum ParameterCallType { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::marker::Copy for ParameterCallType { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::clone::Clone for ParameterCallType {
    #[inline]
    fn clone(&self) -> ParameterCallType { { *self } }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::fmt::Debug for ParameterCallType {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        unsafe { ::core::intrinsics::unreachable() }
    }
}
impl ::core::marker::StructuralEq for ParameterCallType { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::cmp::Eq for ParameterCallType {
    #[inline]
    #[doc(hidden)]
    #[no_coverage]
    fn assert_receiver_is_total_eq(&self) -> () { { } }
}
impl ::core::marker::StructuralPartialEq for ParameterCallType { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::cmp::PartialEq for ParameterCallType {
    #[inline]
    fn eq(&self, other: &ParameterCallType) -> bool {
        unsafe { ::core::intrinsics::unreachable() }
    }
}
impl CfgFunctions for ParameterCallType {
    type I = ParameterInput;
    fn const_fold(&self, _args: &[FlatSet<ConstVal>]) -> FlatSet<ConstVal> {
        match *self { }
    }
    fn derivative<C: CfgFunctions>(&self,
                                   _args:
                                       &IndexSlice<CallArg, [COperand<Self>]>,
                                   _ad: &mut RValueAutoDiff<Self, C>,
                                   _span: Span) -> Option<RValue<Self>> {
        match *self { }
    }
}
impl Display for ParameterCallType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("ILLEGAL")
    }
}
pub enum RealConstCallType { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::marker::Copy for RealConstCallType { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::clone::Clone for RealConstCallType {
    #[inline]
    fn clone(&self) -> RealConstCallType { { *self } }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::fmt::Debug for RealConstCallType {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        unsafe { ::core::intrinsics::unreachable() }
    }
}
impl ::core::marker::StructuralEq for RealConstCallType { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::cmp::Eq for RealConstCallType {
    #[inline]
    #[doc(hidden)]
    #[no_coverage]
    fn assert_receiver_is_total_eq(&self) -> () { { } }
}
impl ::core::marker::StructuralPartialEq for RealConstCallType { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::cmp::PartialEq for RealConstCallType {
    #[inline]
    fn eq(&self, other: &RealConstCallType) -> bool {
        unsafe { ::core::intrinsics::unreachable() }
    }
}
impl ::core::fmt::Display for RealConstCallType {
    #[allow(unused_variables)]
    #[inline]
    fn fmt(&self, _derive_more_display_formatter: &mut ::core::fmt::Formatter)
     -> ::core::fmt::Result {
        match self { _ => Ok(()), }
    }
}
impl CfgFunctions for RealConstCallType {
    type I = NoInput;
    fn const_fold(&self, _: &[FlatSet<ConstVal>]) -> FlatSet<ConstVal> {
        match *self { }
    }
    fn derivative<C: CfgFunctions>(&self,
                                   _args:
                                       &IndexSlice<CallArg, [COperand<Self>]>,
                                   _ad: &mut RValueAutoDiff<Self, C>,
                                   _span: Span) -> Option<RValue<Self>> {
        match *self { }
    }
}
/// An Expression used for variable default values etc.
/// In MIR Expressions are replaced by RValues which can have at most two operands.
///
/// As such more complex (nested) expressions allowed in parameter/variable default values
/// which can not be constant folded (because they are allowed to depend on parameters)
/// have to be represented as a [`ControlFlowGraph`] that calculates the values.
///
/// Furthermroe the [`Local`] that the resulting value can be read from after executing the cfg is also required
pub struct Expression<C: CfgFunctions>(pub ControlFlowGraph<C>,
                                       pub COperand<C>);
#[automatically_derived]
#[allow(unused_qualifications)]
impl <C: ::core::clone::Clone + CfgFunctions> ::core::clone::Clone for
 Expression<C> {
    #[inline]
    fn clone(&self) -> Expression<C> {
        match *self {
            Expression(ref __self_0_0, ref __self_0_1) =>
            Expression(::core::clone::Clone::clone(&(*__self_0_0)),
                       ::core::clone::Clone::clone(&(*__self_0_1))),
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl <C: ::core::fmt::Debug + CfgFunctions> ::core::fmt::Debug for
 Expression<C> {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match *self {
            Expression(ref __self_0_0, ref __self_0_1) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "Expression");
                let _ =
                    ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                   &&(*__self_0_0));
                let _ =
                    ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                   &&(*__self_0_1));
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
        }
    }
}
impl <C: CfgFunctions> Expression<C> {
    pub fn new_const(val: ConstVal, span: Span) -> Self {
        Self(ControlFlowGraph::empty(),
             Operand::new(OperandData::Constant(val), span))
    }
}
impl <C: CfgFunctions> Expression<C> {
    pub fn map<X: CfgFunctions>(self,
                                conversion: &mut impl CfgConversion<C, X>)
     -> Expression<X> {
        Expression(self.0.map(conversion), conversion.map_operand(self.1))
    }
}
pub struct Mir<C: CfgFunctions> {
    /// All branches in this project
    /// Remain unchanged from the HIR
    pub branches: IndexVec<BranchId, Branch>,
    /// All nets in this project
    /// Remain unchanged from the HIR
    pub nets: IndexVec<NetId, Net>,
    /// All ports in this project
    /// Remain unchanged from the HIR
    pub ports: IndexVec<PortId, Port>,
    /// All disciplines in this project
    /// Remain unchanged from the HIR
    pub disciplines: IndexVec<DisciplineId, Discipline>,
    pub modules: IndexVec<ModuleId, Module<C>>,
    pub parameters: IndexVec<ParameterId, Parameter>,
    pub variables: IndexVec<VariableId, Variable>,
    pub natures: IndexVec<NatureId, Nature>,
    pub syntax_ctx: IndexVec<SyntaxCtx, SyntaxContextData>,
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl <C: ::core::fmt::Debug + CfgFunctions> ::core::fmt::Debug for Mir<C> {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match *self {
            Mir {
            branches: ref __self_0_0,
            nets: ref __self_0_1,
            ports: ref __self_0_2,
            disciplines: ref __self_0_3,
            modules: ref __self_0_4,
            parameters: ref __self_0_5,
            variables: ref __self_0_6,
            natures: ref __self_0_7,
            syntax_ctx: ref __self_0_8 } => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_struct(f, "Mir");
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "branches",
                                                    &&(*__self_0_0));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "nets", &&(*__self_0_1));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "ports", &&(*__self_0_2));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "disciplines",
                                                    &&(*__self_0_3));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "modules",
                                                    &&(*__self_0_4));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "parameters",
                                                    &&(*__self_0_5));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "variables",
                                                    &&(*__self_0_6));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "natures",
                                                    &&(*__self_0_7));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "syntax_ctx",
                                                    &&(*__self_0_8));
                ::core::fmt::DebugStruct::finish(debug_trait_builder)
            }
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl <C: ::core::clone::Clone + CfgFunctions> ::core::clone::Clone for Mir<C>
 {
    #[inline]
    fn clone(&self) -> Mir<C> {
        match *self {
            Mir {
            branches: ref __self_0_0,
            nets: ref __self_0_1,
            ports: ref __self_0_2,
            disciplines: ref __self_0_3,
            modules: ref __self_0_4,
            parameters: ref __self_0_5,
            variables: ref __self_0_6,
            natures: ref __self_0_7,
            syntax_ctx: ref __self_0_8 } =>
            Mir{branches: ::core::clone::Clone::clone(&(*__self_0_0)),
                nets: ::core::clone::Clone::clone(&(*__self_0_1)),
                ports: ::core::clone::Clone::clone(&(*__self_0_2)),
                disciplines: ::core::clone::Clone::clone(&(*__self_0_3)),
                modules: ::core::clone::Clone::clone(&(*__self_0_4)),
                parameters: ::core::clone::Clone::clone(&(*__self_0_5)),
                variables: ::core::clone::Clone::clone(&(*__self_0_6)),
                natures: ::core::clone::Clone::clone(&(*__self_0_7)),
                syntax_ctx: ::core::clone::Clone::clone(&(*__self_0_8)),},
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl <C: ::core::default::Default + CfgFunctions> ::core::default::Default for
 Mir<C> {
    #[inline]
    fn default() -> Mir<C> {
        Mir{branches: ::core::default::Default::default(),
            nets: ::core::default::Default::default(),
            ports: ::core::default::Default::default(),
            disciplines: ::core::default::Default::default(),
            modules: ::core::default::Default::default(),
            parameters: ::core::default::Default::default(),
            variables: ::core::default::Default::default(),
            natures: ::core::default::Default::default(),
            syntax_ctx: ::core::default::Default::default(),}
    }
}
pub enum BinOp {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulus,
    ShiftLeft,
    ShiftRight,
    Xor,
    NXor,
    And,
    Or,
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::clone::Clone for BinOp {
    #[inline]
    fn clone(&self) -> BinOp { { *self } }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::marker::Copy for BinOp { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::fmt::Debug for BinOp {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match (&*self,) {
            (&BinOp::Plus,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "Plus");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&BinOp::Minus,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "Minus");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&BinOp::Multiply,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "Multiply");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&BinOp::Divide,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "Divide");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&BinOp::Modulus,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "Modulus");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&BinOp::ShiftLeft,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "ShiftLeft");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&BinOp::ShiftRight,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "ShiftRight");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&BinOp::Xor,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "Xor");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&BinOp::NXor,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "NXor");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&BinOp::And,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "And");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&BinOp::Or,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "Or");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
        }
    }
}
impl ::core::marker::StructuralPartialEq for BinOp { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::cmp::PartialEq for BinOp {
    #[inline]
    fn eq(&self, other: &BinOp) -> bool {
        {
            let __self_vi = ::core::intrinsics::discriminant_value(&*self);
            let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
            if true && __self_vi == __arg_1_vi {
                match (&*self, &*other) { _ => true, }
            } else { false }
        }
    }
}
impl ::core::marker::StructuralEq for BinOp { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::cmp::Eq for BinOp {
    #[inline]
    #[doc(hidden)]
    #[no_coverage]
    fn assert_receiver_is_total_eq(&self) -> () { { } }
}
impl Display for BinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Plus => f.write_str("+"),
            BinOp::Minus => f.write_str("-"),
            BinOp::Multiply => f.write_str("*"),
            BinOp::Divide => f.write_str("/"),
            BinOp::Modulus => f.write_str("%"),
            BinOp::ShiftLeft => f.write_str("<<"),
            BinOp::ShiftRight => f.write_str(">>"),
            BinOp::Xor => f.write_str("XOR"),
            BinOp::NXor => f.write_str("EQ"),
            BinOp::And => f.write_str("&"),
            BinOp::Or => f.write_str("|"),
        }
    }
}
pub enum ComparisonOp {
    LessThen,
    LessEqual,
    GreaterThen,
    GreaterEqual,
    Equal,
    NotEqual,
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::clone::Clone for ComparisonOp {
    #[inline]
    fn clone(&self) -> ComparisonOp { { *self } }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::marker::Copy for ComparisonOp { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::fmt::Debug for ComparisonOp {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match (&*self,) {
            (&ComparisonOp::LessThen,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "LessThen");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&ComparisonOp::LessEqual,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "LessEqual");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&ComparisonOp::GreaterThen,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f,
                                                             "GreaterThen");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&ComparisonOp::GreaterEqual,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f,
                                                             "GreaterEqual");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&ComparisonOp::Equal,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "Equal");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&ComparisonOp::NotEqual,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "NotEqual");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
        }
    }
}
impl ::core::marker::StructuralPartialEq for ComparisonOp { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::cmp::PartialEq for ComparisonOp {
    #[inline]
    fn eq(&self, other: &ComparisonOp) -> bool {
        {
            let __self_vi = ::core::intrinsics::discriminant_value(&*self);
            let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
            if true && __self_vi == __arg_1_vi {
                match (&*self, &*other) { _ => true, }
            } else { false }
        }
    }
}
impl ::core::marker::StructuralEq for ComparisonOp { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::cmp::Eq for ComparisonOp {
    #[inline]
    #[doc(hidden)]
    #[no_coverage]
    fn assert_receiver_is_total_eq(&self) -> () { { } }
}
impl Display for ComparisonOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let raw =
            match self {
                ComparisonOp::LessThen => "<",
                ComparisonOp::LessEqual => "<=",
                ComparisonOp::GreaterThen => ">",
                ComparisonOp::GreaterEqual => ">=",
                ComparisonOp::Equal => "==",
                ComparisonOp::NotEqual => "!=",
            };
        f.write_str(raw)
    }
}
pub struct LocalDeclaration {
    pub kind: LocalKind,
    pub ty: Type,
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::clone::Clone for LocalDeclaration {
    #[inline]
    fn clone(&self) -> LocalDeclaration {
        match *self {
            LocalDeclaration { kind: ref __self_0_0, ty: ref __self_0_1 } =>
            LocalDeclaration{kind:
                                 ::core::clone::Clone::clone(&(*__self_0_0)),
                             ty:
                                 ::core::clone::Clone::clone(&(*__self_0_1)),},
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::fmt::Debug for LocalDeclaration {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match *self {
            LocalDeclaration { kind: ref __self_0_0, ty: ref __self_0_1 } => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_struct(f,
                                                              "LocalDeclaration");
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "kind", &&(*__self_0_0));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder, "ty",
                                                    &&(*__self_0_1));
                ::core::fmt::DebugStruct::finish(debug_trait_builder)
            }
        }
    }
}
impl ::core::marker::StructuralPartialEq for LocalDeclaration { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::cmp::PartialEq for LocalDeclaration {
    #[inline]
    fn eq(&self, other: &LocalDeclaration) -> bool {
        match *other {
            LocalDeclaration { kind: ref __self_1_0, ty: ref __self_1_1 } =>
            match *self {
                LocalDeclaration { kind: ref __self_0_0, ty: ref __self_0_1 }
                =>
                (*__self_0_0) == (*__self_1_0) &&
                    (*__self_0_1) == (*__self_1_1),
            },
        }
    }
    #[inline]
    fn ne(&self, other: &LocalDeclaration) -> bool {
        match *other {
            LocalDeclaration { kind: ref __self_1_0, ty: ref __self_1_1 } =>
            match *self {
                LocalDeclaration { kind: ref __self_0_0, ty: ref __self_0_1 }
                =>
                (*__self_0_0) != (*__self_1_0) ||
                    (*__self_0_1) != (*__self_1_1),
            },
        }
    }
}
impl ::core::marker::StructuralEq for LocalDeclaration { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::cmp::Eq for LocalDeclaration {
    #[inline]
    #[doc(hidden)]
    #[no_coverage]
    fn assert_receiver_is_total_eq(&self) -> () {
        {
            let _: ::core::cmp::AssertParamIsEq<LocalKind>;
            let _: ::core::cmp::AssertParamIsEq<Type>;
        }
    }
}
pub enum VariableLocalKind {
    User,

    #[cfg(not(feature = "arbitrary_order_derivative"))]
    Derivative(ArrayVec<Unknown, 7>),
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::clone::Clone for VariableLocalKind {
    #[inline]
    fn clone(&self) -> VariableLocalKind {
        match (&*self,) {
            (&VariableLocalKind::User,) => VariableLocalKind::User,
            (&VariableLocalKind::Derivative(ref __self_0),) =>
            VariableLocalKind::Derivative(::core::clone::Clone::clone(&(*__self_0))),
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::fmt::Debug for VariableLocalKind {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match (&*self,) {
            (&VariableLocalKind::User,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "User");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&VariableLocalKind::Derivative(ref __self_0),) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "Derivative");
                let _ =
                    ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                   &&(*__self_0));
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
        }
    }
}
impl ::core::marker::StructuralPartialEq for VariableLocalKind { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::cmp::PartialEq for VariableLocalKind {
    #[inline]
    fn eq(&self, other: &VariableLocalKind) -> bool {
        {
            let __self_vi = ::core::intrinsics::discriminant_value(&*self);
            let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
            if true && __self_vi == __arg_1_vi {
                match (&*self, &*other) {
                    (&VariableLocalKind::Derivative(ref __self_0),
                     &VariableLocalKind::Derivative(ref __arg_1_0)) =>
                    (*__self_0) == (*__arg_1_0),
                    _ => true,
                }
            } else { false }
        }
    }
    #[inline]
    fn ne(&self, other: &VariableLocalKind) -> bool {
        {
            let __self_vi = ::core::intrinsics::discriminant_value(&*self);
            let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
            if true && __self_vi == __arg_1_vi {
                match (&*self, &*other) {
                    (&VariableLocalKind::Derivative(ref __self_0),
                     &VariableLocalKind::Derivative(ref __arg_1_0)) =>
                    (*__self_0) != (*__arg_1_0),
                    _ => false,
                }
            } else { true }
        }
    }
}
impl ::core::marker::StructuralEq for VariableLocalKind { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::cmp::Eq for VariableLocalKind {
    #[inline]
    #[doc(hidden)]
    #[no_coverage]
    fn assert_receiver_is_total_eq(&self) -> () {
        { let _: ::core::cmp::AssertParamIsEq<ArrayVec<Unknown, 7>>; }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::hash::Hash for VariableLocalKind {
    fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
        match (&*self,) {
            (&VariableLocalKind::Derivative(ref __self_0),) => {
                ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self),
                                         state);
                ::core::hash::Hash::hash(&(*__self_0), state)
            }
            _ => {
                ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self),
                                         state)
            }
        }
    }
}
pub enum LocalKind {

    /// A local correspond to a variable
    /// These locals are not ssa and as such are mapped to alloca/pointers
    /// Note that multiple locals may exist for the same variable (eg  derivatives)
    /// The Local is always what uniquely identifies the memory location
    Variable(VariableId, VariableLocalKind),

    /// There shall only be one local for every combination of DisciplineAccess
    /// and BranchId. This is automatically enforced during HIR lowering.
    /// Adding a second local for the same branch/discipline combination will result in UB (in the output binary not the compiler itself
    Branch(DisciplineAccess, BranchId, VariableLocalKind),

    /// Temporary values introduced by OpenVAF
    /// These act like SSA as such it is UB (in the output binary not the compiler itself) to write to the same local twice
    /// (will probably cause a panic a some stage or a sigfault during codegen)
    Temporary,
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::clone::Clone for LocalKind {
    #[inline]
    fn clone(&self) -> LocalKind {
        match (&*self,) {
            (&LocalKind::Variable(ref __self_0, ref __self_1),) =>
            LocalKind::Variable(::core::clone::Clone::clone(&(*__self_0)),
                                ::core::clone::Clone::clone(&(*__self_1))),
            (&LocalKind::Branch(ref __self_0, ref __self_1, ref __self_2),) =>
            LocalKind::Branch(::core::clone::Clone::clone(&(*__self_0)),
                              ::core::clone::Clone::clone(&(*__self_1)),
                              ::core::clone::Clone::clone(&(*__self_2))),
            (&LocalKind::Temporary,) => LocalKind::Temporary,
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::fmt::Debug for LocalKind {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match (&*self,) {
            (&LocalKind::Variable(ref __self_0, ref __self_1),) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "Variable");
                let _ =
                    ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                   &&(*__self_0));
                let _ =
                    ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                   &&(*__self_1));
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&LocalKind::Branch(ref __self_0, ref __self_1, ref __self_2),) =>
            {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "Branch");
                let _ =
                    ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                   &&(*__self_0));
                let _ =
                    ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                   &&(*__self_1));
                let _ =
                    ::core::fmt::DebugTuple::field(debug_trait_builder,
                                                   &&(*__self_2));
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&LocalKind::Temporary,) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "Temporary");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
        }
    }
}
impl ::core::marker::StructuralPartialEq for LocalKind { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::cmp::PartialEq for LocalKind {
    #[inline]
    fn eq(&self, other: &LocalKind) -> bool {
        {
            let __self_vi = ::core::intrinsics::discriminant_value(&*self);
            let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
            if true && __self_vi == __arg_1_vi {
                match (&*self, &*other) {
                    (&LocalKind::Variable(ref __self_0, ref __self_1),
                     &LocalKind::Variable(ref __arg_1_0, ref __arg_1_1)) =>
                    (*__self_0) == (*__arg_1_0) &&
                        (*__self_1) == (*__arg_1_1),
                    (&LocalKind::Branch(ref __self_0, ref __self_1,
                                        ref __self_2),
                     &LocalKind::Branch(ref __arg_1_0, ref __arg_1_1,
                                        ref __arg_1_2)) =>
                    (*__self_0) == (*__arg_1_0) && (*__self_1) == (*__arg_1_1)
                        && (*__self_2) == (*__arg_1_2),
                    _ => true,
                }
            } else { false }
        }
    }
    #[inline]
    fn ne(&self, other: &LocalKind) -> bool {
        {
            let __self_vi = ::core::intrinsics::discriminant_value(&*self);
            let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
            if true && __self_vi == __arg_1_vi {
                match (&*self, &*other) {
                    (&LocalKind::Variable(ref __self_0, ref __self_1),
                     &LocalKind::Variable(ref __arg_1_0, ref __arg_1_1)) =>
                    (*__self_0) != (*__arg_1_0) ||
                        (*__self_1) != (*__arg_1_1),
                    (&LocalKind::Branch(ref __self_0, ref __self_1,
                                        ref __self_2),
                     &LocalKind::Branch(ref __arg_1_0, ref __arg_1_1,
                                        ref __arg_1_2)) =>
                    (*__self_0) != (*__arg_1_0) || (*__self_1) != (*__arg_1_1)
                        || (*__self_2) != (*__arg_1_2),
                    _ => false,
                }
            } else { true }
        }
    }
}
impl ::core::marker::StructuralEq for LocalKind { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::cmp::Eq for LocalKind {
    #[inline]
    #[doc(hidden)]
    #[no_coverage]
    fn assert_receiver_is_total_eq(&self) -> () {
        {
            let _: ::core::cmp::AssertParamIsEq<VariableId>;
            let _: ::core::cmp::AssertParamIsEq<VariableLocalKind>;
            let _: ::core::cmp::AssertParamIsEq<DisciplineAccess>;
            let _: ::core::cmp::AssertParamIsEq<BranchId>;
            let _: ::core::cmp::AssertParamIsEq<VariableLocalKind>;
        }
    }
}
pub type Operand<I> = Spanned<OperandData<I>>;
pub enum OperandData<I: CfgInputs> {
    Constant(ConstVal),
    Copy(Local),
    Read(I),
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl <I: ::core::clone::Clone + CfgInputs> ::core::clone::Clone for
 OperandData<I> {
    #[inline]
    fn clone(&self) -> OperandData<I> {
        match (&*self,) {
            (&OperandData::Constant(ref __self_0),) =>
            OperandData::Constant(::core::clone::Clone::clone(&(*__self_0))),
            (&OperandData::Copy(ref __self_0),) =>
            OperandData::Copy(::core::clone::Clone::clone(&(*__self_0))),
            (&OperandData::Read(ref __self_0),) =>
            OperandData::Read(::core::clone::Clone::clone(&(*__self_0))),
        }
    }
}
impl <I: CfgInputs> ::core::marker::StructuralPartialEq for OperandData<I> { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl <I: ::core::cmp::PartialEq + CfgInputs> ::core::cmp::PartialEq for
 OperandData<I> {
    #[inline]
    fn eq(&self, other: &OperandData<I>) -> bool {
        {
            let __self_vi = ::core::intrinsics::discriminant_value(&*self);
            let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
            if true && __self_vi == __arg_1_vi {
                match (&*self, &*other) {
                    (&OperandData::Constant(ref __self_0),
                     &OperandData::Constant(ref __arg_1_0)) =>
                    (*__self_0) == (*__arg_1_0),
                    (&OperandData::Copy(ref __self_0),
                     &OperandData::Copy(ref __arg_1_0)) =>
                    (*__self_0) == (*__arg_1_0),
                    (&OperandData::Read(ref __self_0),
                     &OperandData::Read(ref __arg_1_0)) =>
                    (*__self_0) == (*__arg_1_0),
                    _ => unsafe { ::core::intrinsics::unreachable() }
                }
            } else { false }
        }
    }
    #[inline]
    fn ne(&self, other: &OperandData<I>) -> bool {
        {
            let __self_vi = ::core::intrinsics::discriminant_value(&*self);
            let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
            if true && __self_vi == __arg_1_vi {
                match (&*self, &*other) {
                    (&OperandData::Constant(ref __self_0),
                     &OperandData::Constant(ref __arg_1_0)) =>
                    (*__self_0) != (*__arg_1_0),
                    (&OperandData::Copy(ref __self_0),
                     &OperandData::Copy(ref __arg_1_0)) =>
                    (*__self_0) != (*__arg_1_0),
                    (&OperandData::Read(ref __self_0),
                     &OperandData::Read(ref __arg_1_0)) =>
                    (*__self_0) != (*__arg_1_0),
                    _ => unsafe { ::core::intrinsics::unreachable() }
                }
            } else { true }
        }
    }
}
#[automatically_derived]
impl <I: CfgInputs> ::core::convert::From<(I)> for OperandData<I> {
    #[inline]
    fn from(original: (I)) -> OperandData<I> { OperandData::Read(original) }
}
#[automatically_derived]
impl <I: CfgInputs> ::core::convert::From<(ConstVal)> for OperandData<I> {
    #[inline]
    fn from(original: (ConstVal)) -> OperandData<I> {
        OperandData::Constant(original)
    }
}
#[automatically_derived]
impl <I: CfgInputs> ::core::convert::From<(Local)> for OperandData<I> {
    #[inline]
    fn from(original: (Local)) -> OperandData<I> {
        OperandData::Copy(original)
    }
}
impl <I: CfgInputs> OperandData<I> {
    pub fn ty<MC: CfgFunctions,
              C: CfgFunctions<I =
                              I>>(&self, mir: &Mir<MC>,
                                  cfg: &ControlFlowGraph<C>) -> Type {
        match self {
            Self::Constant(val) => val.ty(),
            Self::Copy(local) => cfg.locals[*local].ty,
            Self::Read(input) => input.ty(mir),
        }
    }
    #[inline]
    pub fn map_input<X: CfgInputs>(self, map: impl FnOnce(I) -> X)
     -> OperandData<X> {
        match self {
            Self::Constant(val) => OperandData::Constant(val),
            Self::Copy(local) => OperandData::Copy(local),
            Self::Read(input) => OperandData::Read(map(input)),
        }
    }
}
impl <I: CfgInputs> Display for OperandData<I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OperandData::Constant(val) =>
            f.write_fmt(::core::fmt::Arguments::new_v1(&[""],
                                                       &match (&val,) {
                                                            (arg0,) =>
                                                            [::core::fmt::ArgumentV1::new(arg0,
                                                                                          ::core::fmt::Debug::fmt)],
                                                        })),
            OperandData::Copy(local) => Display::fmt(local, f),
            OperandData::Read(input) => Display::fmt(input, f),
        }
    }
}
impl <I: CfgInputs> Debug for OperandData<I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}
#[repr(transparent)]
pub struct Local {
    _raw: u32,
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::marker::Copy for Local { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::clone::Clone for Local {
    #[inline]
    fn clone(&self) -> Local {
        { let _: ::core::clone::AssertParamIsClone<u32>; *self }
    }
}
impl ::core::marker::StructuralPartialEq for Local { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::cmp::PartialEq for Local {
    #[inline]
    fn eq(&self, other: &Local) -> bool {
        match *other {
            Local { _raw: ref __self_1_0 } =>
            match *self {
                Local { _raw: ref __self_0_0 } =>
                (*__self_0_0) == (*__self_1_0),
            },
        }
    }
    #[inline]
    fn ne(&self, other: &Local) -> bool {
        match *other {
            Local { _raw: ref __self_1_0 } =>
            match *self {
                Local { _raw: ref __self_0_0 } =>
                (*__self_0_0) != (*__self_1_0),
            },
        }
    }
}
impl ::core::marker::StructuralEq for Local { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::cmp::Eq for Local {
    #[inline]
    #[doc(hidden)]
    #[no_coverage]
    fn assert_receiver_is_total_eq(&self) -> () {
        { let _: ::core::cmp::AssertParamIsEq<u32>; }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::hash::Hash for Local {
    fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
        match *self {
            Local { _raw: ref __self_0_0 } => {
                ::core::hash::Hash::hash(&(*__self_0_0), state)
            }
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::cmp::PartialOrd for Local {
    #[inline]
    fn partial_cmp(&self, other: &Local)
     -> ::core::option::Option<::core::cmp::Ordering> {
        match *other {
            Local { _raw: ref __self_1_0 } =>
            match *self {
                Local { _raw: ref __self_0_0 } =>
                match ::core::cmp::PartialOrd::partial_cmp(&(*__self_0_0),
                                                           &(*__self_1_0)) {
                    ::core::option::Option::Some(::core::cmp::Ordering::Equal)
                    =>
                    ::core::option::Option::Some(::core::cmp::Ordering::Equal),
                    cmp => cmp,
                },
            },
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::cmp::Ord for Local {
    #[inline]
    fn cmp(&self, other: &Local) -> ::core::cmp::Ordering {
        match *other {
            Local { _raw: ref __self_1_0 } =>
            match *self {
                Local { _raw: ref __self_0_0 } =>
                match ::core::cmp::Ord::cmp(&(*__self_0_0), &(*__self_1_0)) {
                    ::core::cmp::Ordering::Equal =>
                    ::core::cmp::Ordering::Equal,
                    cmp => cmp,
                },
            },
        }
    }
}
impl Local {
    /// If `Self::CHECKS_MAX_INDEX` is true, we'll assert if trying to
    /// produce a value larger than this in any of the ctors that don't
    /// have `unchecked` in their name.
    pub const MAX_INDEX: usize = (<u32>::max_value() as usize);
    /// Does this index type assert if asked to construct an index
    /// larger than MAX_INDEX?
    pub const CHECKS_MAX_INDEX: bool = !false;
    /// Construct this index type from a usize. Alias for `from_usize`.
    #[inline(always)]
    pub fn new(value: usize) -> Self { Self::from_usize(value) }
    /// Construct this index type from the wrapped integer type.
    #[inline(always)]
    pub fn from_raw(value: u32) -> Self { Self::from_usize(value as usize) }
    /// Construct this index type from one in a different domain
    #[inline(always)]
    pub fn from_foreign<F: ::index_vec::Idx>(value: F) -> Self {
        Self::from_usize(value.index())
    }
    /// Construct from a usize without any checks.
    #[inline(always)]
    pub const fn from_usize_unchecked(value: usize) -> Self {
        Self{_raw: value as u32,}
    }
    /// Construct from the underlying type without any checks.
    #[inline(always)]
    pub const fn from_raw_unchecked(raw: u32) -> Self { Self{_raw: raw,} }
    /// Construct this index type from a usize.
    #[inline]
    pub fn from_usize(value: usize) -> Self {
        Self::check_index(value as usize);
        Self{_raw: value as u32,}
    }
    /// Get the wrapped index as a usize.
    #[inline(always)]
    pub fn index(self) -> usize { self._raw as usize }
    /// Get the wrapped index.
    #[inline(always)]
    pub fn raw(self) -> u32 { self._raw }
    /// Asserts `v <= Self::MAX_INDEX` unless Self::CHECKS_MAX_INDEX is false.
    #[inline]
    pub fn check_index(v: usize) {
        if Self::CHECKS_MAX_INDEX && (v > Self::MAX_INDEX) {
            ::index_vec::__max_check_fail(v, Self::MAX_INDEX);
        }
    }
    const _ENSURE_RAW_IS_UNSIGNED: [(); 0] =
        [(); <u32>::min_value() as usize];
}
impl core::fmt::Debug for Local {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_fmt(::core::fmt::Arguments::new_v1(&["_"],
                                                   &match (&self.index(),) {
                                                        (arg0,) =>
                                                        [::core::fmt::ArgumentV1::new(arg0,
                                                                                      ::core::fmt::Display::fmt)],
                                                    }))
    }
}
impl core::cmp::PartialOrd<usize> for Local {
    #[inline]
    fn partial_cmp(&self, other: &usize) -> Option<core::cmp::Ordering> {
        self.index().partial_cmp(other)
    }
}
impl core::cmp::PartialOrd<Local> for usize {
    #[inline]
    fn partial_cmp(&self, other: &Local) -> Option<core::cmp::Ordering> {
        self.partial_cmp(&other.index())
    }
}
impl PartialEq<usize> for Local {
    #[inline]
    fn eq(&self, other: &usize) -> bool { self.index() == *other }
}
impl PartialEq<Local> for usize {
    #[inline]
    fn eq(&self, other: &Local) -> bool { *self == other.index() }
}
impl core::ops::Add<usize> for Local {
    type Output = Self;
    #[inline]
    fn add(self, other: usize) -> Self {
        Self::new(self.index().wrapping_add(other))
    }
}
impl core::ops::Sub<usize> for Local {
    type Output = Self;
    #[inline]
    fn sub(self, other: usize) -> Self {
        Self::new(self.index().wrapping_sub(other))
    }
}
impl core::ops::AddAssign<usize> for Local {
    #[inline]
    fn add_assign(&mut self, other: usize) { *self = *self + other }
}
impl core::ops::SubAssign<usize> for Local {
    #[inline]
    fn sub_assign(&mut self, other: usize) { *self = *self - other; }
}
impl core::ops::Rem<usize> for Local {
    type Output = Self;
    #[inline]
    fn rem(self, other: usize) -> Self { Self::new(self.index() % other) }
}
impl core::ops::Add<Local> for usize {
    type Output = Local;
    #[inline]
    fn add(self, other: Local) -> Local { other + self }
}
impl core::ops::Sub<Local> for usize {
    type Output = Local;
    #[inline]
    fn sub(self, other: Local) -> Local {
        Local::new(self.wrapping_sub(other.index()))
    }
}
impl core::ops::Add for Local {
    type Output = Local;
    #[inline]
    fn add(self, other: Local) -> Local {
        Local::new(other.index() + self.index())
    }
}
impl core::ops::Sub for Local {
    type Output = Local;
    #[inline]
    fn sub(self, other: Local) -> Local {
        Local::new(self.index().wrapping_sub(other.index()))
    }
}
impl core::ops::AddAssign for Local {
    #[inline]
    fn add_assign(&mut self, other: Local) { *self = *self + other }
}
impl core::ops::SubAssign for Local {
    #[inline]
    fn sub_assign(&mut self, other: Local) { *self = *self - other; }
}
impl ::index_vec::Idx for Local {
    #[inline]
    fn from_usize(value: usize) -> Self { Self::from(value) }
    #[inline]
    fn index(self) -> usize { usize::from(self) }
}
impl From<Local> for usize {
    #[inline]
    fn from(v: Local) -> usize { v.index() }
}
impl From<usize> for Local {
    #[inline]
    fn from(value: usize) -> Self { Local::from_usize(value) }
}
const _: [(); 1] = [(); true as usize];
impl From<Local> for u32 {
    #[inline]
    fn from(v: Local) -> u32 { v.raw() }
}
impl From<u32> for Local {
    #[inline]
    fn from(value: u32) -> Self { Self::from_raw(value) }
}
impl core::fmt::Display for Local {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_fmt(::core::fmt::Arguments::new_v1(&["_"],
                                                   &match (&self.index(),) {
                                                        (arg0,) =>
                                                        [::core::fmt::ArgumentV1::new(arg0,
                                                                                      ::core::fmt::Display::fmt)],
                                                    }))
    }
}
pub type COperand<C> = Operand<<C as CfgFunctions>::I>;
pub type COperandData<C> = OperandData<<C as CfgFunctions>::I>;
pub struct TyRValue<C: CfgFunctions = DefaultFunctions> {
    pub val: RValue<C>,
    pub ty: Type,
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl <C: ::core::clone::Clone + CfgFunctions> ::core::clone::Clone for
 TyRValue<C> {
    #[inline]
    fn clone(&self) -> TyRValue<C> {
        match *self {
            TyRValue { val: ref __self_0_0, ty: ref __self_0_1 } =>
            TyRValue{val: ::core::clone::Clone::clone(&(*__self_0_0)),
                     ty: ::core::clone::Clone::clone(&(*__self_0_1)),},
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl <C: ::core::fmt::Debug + CfgFunctions> ::core::fmt::Debug for TyRValue<C>
 {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match *self {
            TyRValue { val: ref __self_0_0, ty: ref __self_0_1 } => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_struct(f, "TyRValue");
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "val", &&(*__self_0_0));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder, "ty",
                                                    &&(*__self_0_1));
                ::core::fmt::DebugStruct::finish(debug_trait_builder)
            }
        }
    }
}
impl <C: CfgFunctions> ::core::marker::StructuralPartialEq for TyRValue<C> { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl <C: ::core::cmp::PartialEq + CfgFunctions> ::core::cmp::PartialEq for
 TyRValue<C> {
    #[inline]
    fn eq(&self, other: &TyRValue<C>) -> bool {
        match *other {
            TyRValue { val: ref __self_1_0, ty: ref __self_1_1 } =>
            match *self {
                TyRValue { val: ref __self_0_0, ty: ref __self_0_1 } =>
                (*__self_0_0) == (*__self_1_0) &&
                    (*__self_0_1) == (*__self_1_1),
            },
        }
    }
    #[inline]
    fn ne(&self, other: &TyRValue<C>) -> bool {
        match *other {
            TyRValue { val: ref __self_1_0, ty: ref __self_1_1 } =>
            match *self {
                TyRValue { val: ref __self_0_0, ty: ref __self_0_1 } =>
                (*__self_0_0) != (*__self_1_0) ||
                    (*__self_0_1) != (*__self_1_1),
            },
        }
    }
}
#[allow(clippy :: from_over_into)]
impl <C: CfgFunctions> Into<Type> for TyRValue<C> {
    fn into(self) -> Type { self.ty }
}
impl <C: CfgFunctions> From<TyRValue<C>> for RValue<C> {
    fn from(typed: TyRValue<C>) -> Self { typed.val }
}
pub enum RValue<C: CfgFunctions> {
    UnaryOperation(Spanned<UnaryOperator>, COperand<C>),
    BinaryOperation(Spanned<BinOp>, COperand<C>, COperand<C>),
    Math1(Spanned<Math1>, COperand<C>),
    Math2(Spanned<Math2>, COperand<C>, COperand<C>),
    Comparison(Spanned<ComparisonOp>, COperand<C>, COperand<C>, Type),

    ///
    Select(COperand<C>, COperand<C>, COperand<C>),
    Cast(COperand<C>),
    Use(COperand<C>),
    Call(C, IndexVec<CallArg, COperand<C>>, Span),
    Array(Vec<COperand<C>>, Span),
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl <C: ::core::clone::Clone + CfgFunctions> ::core::clone::Clone for
 RValue<C> {
    #[inline]
    fn clone(&self) -> RValue<C> {
        match (&*self,) {
            (&RValue::UnaryOperation(ref __self_0, ref __self_1),) =>
            RValue::UnaryOperation(::core::clone::Clone::clone(&(*__self_0)),
                                   ::core::clone::Clone::clone(&(*__self_1))),
            (&RValue::BinaryOperation(ref __self_0, ref __self_1,
                                      ref __self_2),) =>
            RValue::BinaryOperation(::core::clone::Clone::clone(&(*__self_0)),
                                    ::core::clone::Clone::clone(&(*__self_1)),
                                    ::core::clone::Clone::clone(&(*__self_2))),
            (&RValue::Math1(ref __self_0, ref __self_1),) =>
            RValue::Math1(::core::clone::Clone::clone(&(*__self_0)),
                          ::core::clone::Clone::clone(&(*__self_1))),
            (&RValue::Math2(ref __self_0, ref __self_1, ref __self_2),) =>
            RValue::Math2(::core::clone::Clone::clone(&(*__self_0)),
                          ::core::clone::Clone::clone(&(*__self_1)),
                          ::core::clone::Clone::clone(&(*__self_2))),
            (&RValue::Comparison(ref __self_0, ref __self_1, ref __self_2,
                                 ref __self_3),) =>
            RValue::Comparison(::core::clone::Clone::clone(&(*__self_0)),
                               ::core::clone::Clone::clone(&(*__self_1)),
                               ::core::clone::Clone::clone(&(*__self_2)),
                               ::core::clone::Clone::clone(&(*__self_3))),
            (&RValue::Select(ref __self_0, ref __self_1, ref __self_2),) =>
            RValue::Select(::core::clone::Clone::clone(&(*__self_0)),
                           ::core::clone::Clone::clone(&(*__self_1)),
                           ::core::clone::Clone::clone(&(*__self_2))),
            (&RValue::Cast(ref __self_0),) =>
            RValue::Cast(::core::clone::Clone::clone(&(*__self_0))),
            (&RValue::Use(ref __self_0),) =>
            RValue::Use(::core::clone::Clone::clone(&(*__self_0))),
            (&RValue::Call(ref __self_0, ref __self_1, ref __self_2),) =>
            RValue::Call(::core::clone::Clone::clone(&(*__self_0)),
                         ::core::clone::Clone::clone(&(*__self_1)),
                         ::core::clone::Clone::clone(&(*__self_2))),
            (&RValue::Array(ref __self_0, ref __self_1),) =>
            RValue::Array(::core::clone::Clone::clone(&(*__self_0)),
                          ::core::clone::Clone::clone(&(*__self_1))),
        }
    }
}
impl <C: CfgFunctions> ::core::marker::StructuralPartialEq for RValue<C> { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl <C: ::core::cmp::PartialEq + CfgFunctions> ::core::cmp::PartialEq for
 RValue<C> {
    #[inline]
    fn eq(&self, other: &RValue<C>) -> bool {
        {
            let __self_vi = ::core::intrinsics::discriminant_value(&*self);
            let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
            if true && __self_vi == __arg_1_vi {
                match (&*self, &*other) {
                    (&RValue::UnaryOperation(ref __self_0, ref __self_1),
                     &RValue::UnaryOperation(ref __arg_1_0, ref __arg_1_1)) =>
                    (*__self_0) == (*__arg_1_0) &&
                        (*__self_1) == (*__arg_1_1),
                    (&RValue::BinaryOperation(ref __self_0, ref __self_1,
                                              ref __self_2),
                     &RValue::BinaryOperation(ref __arg_1_0, ref __arg_1_1,
                                              ref __arg_1_2)) =>
                    (*__self_0) == (*__arg_1_0) && (*__self_1) == (*__arg_1_1)
                        && (*__self_2) == (*__arg_1_2),
                    (&RValue::Math1(ref __self_0, ref __self_1),
                     &RValue::Math1(ref __arg_1_0, ref __arg_1_1)) =>
                    (*__self_0) == (*__arg_1_0) &&
                        (*__self_1) == (*__arg_1_1),
                    (&RValue::Math2(ref __self_0, ref __self_1, ref __self_2),
                     &RValue::Math2(ref __arg_1_0, ref __arg_1_1,
                                    ref __arg_1_2)) =>
                    (*__self_0) == (*__arg_1_0) && (*__self_1) == (*__arg_1_1)
                        && (*__self_2) == (*__arg_1_2),
                    (&RValue::Comparison(ref __self_0, ref __self_1,
                                         ref __self_2, ref __self_3),
                     &RValue::Comparison(ref __arg_1_0, ref __arg_1_1,
                                         ref __arg_1_2, ref __arg_1_3)) =>
                    (*__self_0) == (*__arg_1_0) && (*__self_1) == (*__arg_1_1)
                        && (*__self_2) == (*__arg_1_2) &&
                        (*__self_3) == (*__arg_1_3),
                    (&RValue::Select(ref __self_0, ref __self_1,
                                     ref __self_2),
                     &RValue::Select(ref __arg_1_0, ref __arg_1_1,
                                     ref __arg_1_2)) =>
                    (*__self_0) == (*__arg_1_0) && (*__self_1) == (*__arg_1_1)
                        && (*__self_2) == (*__arg_1_2),
                    (&RValue::Cast(ref __self_0),
                     &RValue::Cast(ref __arg_1_0)) =>
                    (*__self_0) == (*__arg_1_0),
                    (&RValue::Use(ref __self_0), &RValue::Use(ref __arg_1_0))
                    => (*__self_0) == (*__arg_1_0),
                    (&RValue::Call(ref __self_0, ref __self_1, ref __self_2),
                     &RValue::Call(ref __arg_1_0, ref __arg_1_1,
                                   ref __arg_1_2)) =>
                    (*__self_0) == (*__arg_1_0) && (*__self_1) == (*__arg_1_1)
                        && (*__self_2) == (*__arg_1_2),
                    (&RValue::Array(ref __self_0, ref __self_1),
                     &RValue::Array(ref __arg_1_0, ref __arg_1_1)) =>
                    (*__self_0) == (*__arg_1_0) &&
                        (*__self_1) == (*__arg_1_1),
                    _ => unsafe { ::core::intrinsics::unreachable() }
                }
            } else { false }
        }
    }
    #[inline]
    fn ne(&self, other: &RValue<C>) -> bool {
        {
            let __self_vi = ::core::intrinsics::discriminant_value(&*self);
            let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
            if true && __self_vi == __arg_1_vi {
                match (&*self, &*other) {
                    (&RValue::UnaryOperation(ref __self_0, ref __self_1),
                     &RValue::UnaryOperation(ref __arg_1_0, ref __arg_1_1)) =>
                    (*__self_0) != (*__arg_1_0) ||
                        (*__self_1) != (*__arg_1_1),
                    (&RValue::BinaryOperation(ref __self_0, ref __self_1,
                                              ref __self_2),
                     &RValue::BinaryOperation(ref __arg_1_0, ref __arg_1_1,
                                              ref __arg_1_2)) =>
                    (*__self_0) != (*__arg_1_0) || (*__self_1) != (*__arg_1_1)
                        || (*__self_2) != (*__arg_1_2),
                    (&RValue::Math1(ref __self_0, ref __self_1),
                     &RValue::Math1(ref __arg_1_0, ref __arg_1_1)) =>
                    (*__self_0) != (*__arg_1_0) ||
                        (*__self_1) != (*__arg_1_1),
                    (&RValue::Math2(ref __self_0, ref __self_1, ref __self_2),
                     &RValue::Math2(ref __arg_1_0, ref __arg_1_1,
                                    ref __arg_1_2)) =>
                    (*__self_0) != (*__arg_1_0) || (*__self_1) != (*__arg_1_1)
                        || (*__self_2) != (*__arg_1_2),
                    (&RValue::Comparison(ref __self_0, ref __self_1,
                                         ref __self_2, ref __self_3),
                     &RValue::Comparison(ref __arg_1_0, ref __arg_1_1,
                                         ref __arg_1_2, ref __arg_1_3)) =>
                    (*__self_0) != (*__arg_1_0) || (*__self_1) != (*__arg_1_1)
                        || (*__self_2) != (*__arg_1_2) ||
                        (*__self_3) != (*__arg_1_3),
                    (&RValue::Select(ref __self_0, ref __self_1,
                                     ref __self_2),
                     &RValue::Select(ref __arg_1_0, ref __arg_1_1,
                                     ref __arg_1_2)) =>
                    (*__self_0) != (*__arg_1_0) || (*__self_1) != (*__arg_1_1)
                        || (*__self_2) != (*__arg_1_2),
                    (&RValue::Cast(ref __self_0),
                     &RValue::Cast(ref __arg_1_0)) =>
                    (*__self_0) != (*__arg_1_0),
                    (&RValue::Use(ref __self_0), &RValue::Use(ref __arg_1_0))
                    => (*__self_0) != (*__arg_1_0),
                    (&RValue::Call(ref __self_0, ref __self_1, ref __self_2),
                     &RValue::Call(ref __arg_1_0, ref __arg_1_1,
                                   ref __arg_1_2)) =>
                    (*__self_0) != (*__arg_1_0) || (*__self_1) != (*__arg_1_1)
                        || (*__self_2) != (*__arg_1_2),
                    (&RValue::Array(ref __self_0, ref __self_1),
                     &RValue::Array(ref __arg_1_0, ref __arg_1_1)) =>
                    (*__self_0) != (*__arg_1_0) ||
                        (*__self_1) != (*__arg_1_1),
                    _ => unsafe { ::core::intrinsics::unreachable() }
                }
            } else { true }
        }
    }
}
impl <C: CfgFunctions> Display for RValue<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            RValue::UnaryOperation(operator, operand) => {
                f.write_fmt(::core::fmt::Arguments::new_v1(&["", ""],
                                                           &match (&operator,
                                                                   &operand) {
                                                                (arg0, arg1)
                                                                =>
                                                                [::core::fmt::ArgumentV1::new(arg0,
                                                                                              ::core::fmt::Display::fmt),
                                                                 ::core::fmt::ArgumentV1::new(arg1,
                                                                                              ::core::fmt::Display::fmt)],
                                                            }))
            }
            RValue::BinaryOperation(op, lhs, rhs) => {
                f.write_fmt(::core::fmt::Arguments::new_v1(&["", " ", " "],
                                                           &match (&lhs, &op,
                                                                   &rhs) {
                                                                (arg0, arg1,
                                                                 arg2) =>
                                                                [::core::fmt::ArgumentV1::new(arg0,
                                                                                              ::core::fmt::Display::fmt),
                                                                 ::core::fmt::ArgumentV1::new(arg1,
                                                                                              ::core::fmt::Display::fmt),
                                                                 ::core::fmt::ArgumentV1::new(arg2,
                                                                                              ::core::fmt::Display::fmt)],
                                                            }))
            }
            RValue::Math1(fun, op) => {
                f.write_fmt(::core::fmt::Arguments::new_v1(&["", "(", ")"],
                                                           &match (&fun, &op)
                                                                {
                                                                (arg0, arg1)
                                                                =>
                                                                [::core::fmt::ArgumentV1::new(arg0,
                                                                                              ::core::fmt::Display::fmt),
                                                                 ::core::fmt::ArgumentV1::new(arg1,
                                                                                              ::core::fmt::Display::fmt)],
                                                            }))
            }
            RValue::Math2(fun, op1, op2) => {
                f.write_fmt(::core::fmt::Arguments::new_v1(&["", "(", ", ",
                                                             ")"],
                                                           &match (&fun, &op1,
                                                                   &op2) {
                                                                (arg0, arg1,
                                                                 arg2) =>
                                                                [::core::fmt::ArgumentV1::new(arg0,
                                                                                              ::core::fmt::Display::fmt),
                                                                 ::core::fmt::ArgumentV1::new(arg1,
                                                                                              ::core::fmt::Display::fmt),
                                                                 ::core::fmt::ArgumentV1::new(arg2,
                                                                                              ::core::fmt::Display::fmt)],
                                                            }))
            }
            RValue::Comparison(op, lhs, rhs, _) => {
                f.write_fmt(::core::fmt::Arguments::new_v1(&["", " ", " "],
                                                           &match (&lhs, &op,
                                                                   &rhs) {
                                                                (arg0, arg1,
                                                                 arg2) =>
                                                                [::core::fmt::ArgumentV1::new(arg0,
                                                                                              ::core::fmt::Display::fmt),
                                                                 ::core::fmt::ArgumentV1::new(arg1,
                                                                                              ::core::fmt::Display::fmt),
                                                                 ::core::fmt::ArgumentV1::new(arg2,
                                                                                              ::core::fmt::Display::fmt)],
                                                            }))
            }
            RValue::Select(cond, true_val, false_val) => {
                f.write_fmt(::core::fmt::Arguments::new_v1(&["if ", " { ",
                                                             " } else { ",
                                                             " }"],
                                                           &match (&cond,
                                                                   &true_val,
                                                                   &false_val)
                                                                {
                                                                (arg0, arg1,
                                                                 arg2) =>
                                                                [::core::fmt::ArgumentV1::new(arg0,
                                                                                              ::core::fmt::Display::fmt),
                                                                 ::core::fmt::ArgumentV1::new(arg1,
                                                                                              ::core::fmt::Display::fmt),
                                                                 ::core::fmt::ArgumentV1::new(arg2,
                                                                                              ::core::fmt::Display::fmt)],
                                                            }))
            }
            RValue::Cast(op) => {
                f.write_fmt(::core::fmt::Arguments::new_v1(&["", " as _"],
                                                           &match (&op,) {
                                                                (arg0,) =>
                                                                [::core::fmt::ArgumentV1::new(arg0,
                                                                                              ::core::fmt::Display::fmt)],
                                                            }))
            }
            RValue::Use(op) => Display::fmt(op, f),
            RValue::Call(call, operands, _) => {
                f.write_fmt(::core::fmt::Arguments::new_v1(&["", "(", ")"],
                                                           &match (&call,
                                                                   &ListFormatter::with_final_seperator(operands.raw.as_slice(),
                                                                                                        ", "))
                                                                {
                                                                (arg0, arg1)
                                                                =>
                                                                [::core::fmt::ArgumentV1::new(arg0,
                                                                                              ::core::fmt::Display::fmt),
                                                                 ::core::fmt::ArgumentV1::new(arg1,
                                                                                              ::core::fmt::Display::fmt)],
                                                            }))
            }
            RValue::Array(vals, _) => {
                f.write_fmt(::core::fmt::Arguments::new_v1(&["[", "]"],
                                                           &match (&ListFormatter::with_final_seperator(vals.as_slice(),
                                                                                                        ", "),)
                                                                {
                                                                (arg0,) =>
                                                                [::core::fmt::ArgumentV1::new(arg0,
                                                                                              ::core::fmt::Display::fmt)],
                                                            }))
            }
        }
    }
}
impl <C: CfgFunctions> Debug for RValue<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}
impl <C: CfgFunctions> RValue<C> {
    pub fn operands(&self) -> impl Iterator<Item = &COperand<C>> {
        let res =
            match self {
                Self::UnaryOperation(_, op) | Self::Math1(_, op) |
                Self::Cast(op) | Self::Use(op) => {
                    <[_]>::into_vec(box [op])
                }
                Self::BinaryOperation(_, op1, op2) |
                Self::Comparison(_, op1, op2, _) | Self::Math2(_, op1, op2) =>
                <[_]>::into_vec(box [op1, op2]),
                Self::Select(op1, op2, op3) =>
                <[_]>::into_vec(box [op1, op2, op3]),
                Self::Call(_x, args, _) => args.iter().collect_vec(),
                Self::Array(args, _) => args.iter().into_iter().collect_vec(),
            };
        res.into_iter()
    }
    pub fn locals(&self) -> impl Iterator<Item = Local> + '_ {
        self.operands().filter_map(|operand|
                                       {
                                           if let OperandData::Copy(local) =
                                                  operand.contents {
                                               Some(local)
                                           } else { None }
                                       })
    }
    pub fn for_locals(&self, mut f: impl FnMut(Local)) {
        match self {
            Self::UnaryOperation(_, Operand {
                                 contents: OperandData::Copy(local), .. }) |
            Self::Math1(_, Operand { contents: OperandData::Copy(local), .. })
            | Self::Cast(Operand { contents: OperandData::Copy(local), .. }) |
            Self::Use(Operand { contents: OperandData::Copy(local), .. }) =>
            f(*local),
            Self::BinaryOperation(_, op1, op2) |
            Self::Comparison(_, op1, op2, _) | Self::Math2(_, op1, op2) => {
                if let OperandData::Copy(local) = op1.contents { f(local); }
                if let OperandData::Copy(local) = op2.contents { f(local); }
            }
            Self::Select(op1, op2, op3) => {
                if let OperandData::Copy(local) = op1.contents { f(local); }
                if let OperandData::Copy(local) = op2.contents { f(local); }
                if let OperandData::Copy(local) = op3.contents { f(local); }
            }
            Self::Call(_, args, _) => {
                for arg in args {
                    if let OperandData::Copy(local) = arg.contents {
                        f(local)
                    }
                }
            }
            Self::Array(args, _) => {
                for arg in args {
                    if let OperandData::Copy(local) = arg.contents {
                        f(local)
                    }
                }
            }
            _ => (),
        }
    }
    pub fn map_operands<X: CfgFunctions>(self,
                                         conversion:
                                             &mut impl CfgConversion<C, X>)
     -> RValue<X> {
        match self {
            RValue::UnaryOperation(op, arg) => {
                RValue::UnaryOperation(op, conversion.map_operand(arg))
            }
            RValue::BinaryOperation(op, arg1, arg2) =>
            RValue::BinaryOperation(op, conversion.map_operand(arg1),
                                    conversion.map_operand(arg2)),
            RValue::Math1(fun, arg) =>
            RValue::Math1(fun, conversion.map_operand(arg)),
            RValue::Math2(fun, arg1, arg2) =>
            RValue::Math2(fun, conversion.map_operand(arg1),
                          conversion.map_operand(arg2)),
            RValue::Comparison(op, arg1, arg2, ty) =>
            RValue::Comparison(op, conversion.map_operand(arg1),
                               conversion.map_operand(arg2), ty),
            RValue::Select(cond, true_val, false_val) =>
            RValue::Select(conversion.map_operand(cond),
                           conversion.map_operand(true_val),
                           conversion.map_operand(false_val)),
            RValue::Cast(op) => RValue::Cast(conversion.map_operand(op)),
            RValue::Use(op) => RValue::Use(conversion.map_operand(op)),
            RValue::Call(call, args, span) =>
            conversion.map_call_val(call, args, span),
            RValue::Array(vals, span) =>
            RValue::Array(vals.into_iter().map(|op|
                                                   conversion.map_operand(op)).collect(),
                          span),
        }
    }
    pub fn operands_mut(&mut self) -> impl Iterator<Item = &mut COperand<C>> {
        match self {
            Self::UnaryOperation(_, op) | Self::Math1(_, op) | Self::Cast(op)
            | Self::Use(op) => {
                <[_]>::into_vec(box [op])
            }
            Self::BinaryOperation(_, op1, op2) |
            Self::Comparison(_, op1, op2, _) | Self::Math2(_, op1, op2) =>
            <[_]>::into_vec(box [op1, op2]),
            Self::Select(op1, op2, op3) =>
            <[_]>::into_vec(box [op1, op2, op3]),
            Self::Call(_x, args, _) => args.iter_mut().collect_vec(),
            Self::Array(args, _) => args.iter_mut().collect_vec(),
        }.into_iter()
    }
    pub fn for_locals_mut(&mut self, mut f: impl FnMut(&mut Local)) {
        match self {
            Self::UnaryOperation(_, Operand {
                                 contents: OperandData::Copy(local), .. }) |
            Self::Math1(_, Operand { contents: OperandData::Copy(local), .. })
            | Self::Cast(Operand { contents: OperandData::Copy(local), .. }) |
            Self::Use(Operand { contents: OperandData::Copy(local), .. }) =>
            f(local),
            Self::BinaryOperation(_, op1, op2) |
            Self::Comparison(_, op1, op2, _) | Self::Math2(_, op1, op2) => {
                if let OperandData::Copy(local) = &mut op1.contents {
                    f(local);
                }
                if let OperandData::Copy(local) = &mut op2.contents {
                    f(local);
                }
            }
            Self::Select(op1, op2, op3) => {
                if let OperandData::Copy(local) = &mut op1.contents {
                    f(local);
                }
                if let OperandData::Copy(local) = &mut op2.contents {
                    f(local);
                }
                if let OperandData::Copy(local) = &mut op3.contents {
                    f(local);
                }
            }
            Self::Call(_, args, _) => {
                for arg in args {
                    if let OperandData::Copy(local) = &mut arg.contents {
                        f(local)
                    }
                }
            }
            Self::Array(args, _) => {
                for arg in args {
                    if let OperandData::Copy(local) = &mut arg.contents {
                        f(local)
                    }
                }
            }
            _ => (),
        }
    }
    pub fn locals_mut(&mut self) -> impl Iterator<Item = &mut Local> {
        self.operands_mut().filter_map(|operand|
                                           {
                                               if let OperandData::Copy(local)
                                                      = &mut operand.contents
                                                  {
                                                   Some(local)
                                               } else { None }
                                           })
    }
    pub fn span(&self) -> Span {
        match self {
            Self::UnaryOperation(_, op) | Self::Math1(_, op) | Self::Cast(op)
            | Self::Use(op) => {
                op.span
            }
            Self::BinaryOperation(_, op1, op2) |
            Self::Comparison(_, op1, op2, _) | Self::Math2(_, op1, op2) =>
            op1.span.extend(op2.span),
            Self::Select(op1, _op2, op3) => op1.span.extend(op3.span),
            Self::Call(_, _, span) => *span,
            Self::Array(_, span) => *span,
        }
    }
}
pub enum StmntKind<C: CfgFunctions> {
    Assignment(Local, RValue<C>),
    Call(C, IndexVec<CallArg, COperand<C>>, Span),

    /// No Operation (does nothing)
    /// Statements are often overwritten with NoOp instead of being deleted because its cheaper
    NoOp,
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl <C: ::core::clone::Clone + CfgFunctions> ::core::clone::Clone for
 StmntKind<C> {
    #[inline]
    fn clone(&self) -> StmntKind<C> {
        match (&*self,) {
            (&StmntKind::Assignment(ref __self_0, ref __self_1),) =>
            StmntKind::Assignment(::core::clone::Clone::clone(&(*__self_0)),
                                  ::core::clone::Clone::clone(&(*__self_1))),
            (&StmntKind::Call(ref __self_0, ref __self_1, ref __self_2),) =>
            StmntKind::Call(::core::clone::Clone::clone(&(*__self_0)),
                            ::core::clone::Clone::clone(&(*__self_1)),
                            ::core::clone::Clone::clone(&(*__self_2))),
            (&StmntKind::NoOp,) => StmntKind::NoOp,
        }
    }
}
impl <C: CfgFunctions> ::core::marker::StructuralPartialEq for StmntKind<C> {
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl <C: ::core::cmp::PartialEq + CfgFunctions> ::core::cmp::PartialEq for
 StmntKind<C> {
    #[inline]
    fn eq(&self, other: &StmntKind<C>) -> bool {
        {
            let __self_vi = ::core::intrinsics::discriminant_value(&*self);
            let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
            if true && __self_vi == __arg_1_vi {
                match (&*self, &*other) {
                    (&StmntKind::Assignment(ref __self_0, ref __self_1),
                     &StmntKind::Assignment(ref __arg_1_0, ref __arg_1_1)) =>
                    (*__self_0) == (*__arg_1_0) &&
                        (*__self_1) == (*__arg_1_1),
                    (&StmntKind::Call(ref __self_0, ref __self_1,
                                      ref __self_2),
                     &StmntKind::Call(ref __arg_1_0, ref __arg_1_1,
                                      ref __arg_1_2)) =>
                    (*__self_0) == (*__arg_1_0) && (*__self_1) == (*__arg_1_1)
                        && (*__self_2) == (*__arg_1_2),
                    _ => true,
                }
            } else { false }
        }
    }
    #[inline]
    fn ne(&self, other: &StmntKind<C>) -> bool {
        {
            let __self_vi = ::core::intrinsics::discriminant_value(&*self);
            let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
            if true && __self_vi == __arg_1_vi {
                match (&*self, &*other) {
                    (&StmntKind::Assignment(ref __self_0, ref __self_1),
                     &StmntKind::Assignment(ref __arg_1_0, ref __arg_1_1)) =>
                    (*__self_0) != (*__arg_1_0) ||
                        (*__self_1) != (*__arg_1_1),
                    (&StmntKind::Call(ref __self_0, ref __self_1,
                                      ref __self_2),
                     &StmntKind::Call(ref __arg_1_0, ref __arg_1_1,
                                      ref __arg_1_2)) =>
                    (*__self_0) != (*__arg_1_0) || (*__self_1) != (*__arg_1_1)
                        || (*__self_2) != (*__arg_1_2),
                    _ => false,
                }
            } else { true }
        }
    }
}
impl <C: CfgFunctions> Display for StmntKind<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            StmntKind::Assignment(dst, val) =>
            f.write_fmt(::core::fmt::Arguments::new_v1(&["", " = "],
                                                       &match (&dst, &val) {
                                                            (arg0, arg1) =>
                                                            [::core::fmt::ArgumentV1::new(arg0,
                                                                                          ::core::fmt::Display::fmt),
                                                             ::core::fmt::ArgumentV1::new(arg1,
                                                                                          ::core::fmt::Display::fmt)],
                                                        })),
            StmntKind::Call(call, args, _) =>
            f.write_fmt(::core::fmt::Arguments::new_v1(&["", "(", ")"],
                                                       &match (&call,
                                                               &ListFormatter::with_final_seperator(&args.as_slice().raw,
                                                                                                    ", "))
                                                            {
                                                            (arg0, arg1) =>
                                                            [::core::fmt::ArgumentV1::new(arg0,
                                                                                          ::core::fmt::Display::fmt),
                                                             ::core::fmt::ArgumentV1::new(arg1,
                                                                                          ::core::fmt::Display::fmt)],
                                                        })),
            StmntKind::NoOp => Ok(()),
        }
    }
}
impl <C: CfgFunctions> Debug for StmntKind<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}
impl <C: CfgFunctions> StmntKind<C> {
    pub fn read_locals(&self) -> impl Iterator<Item = Local> {
        let mut buff = Vec::with_capacity(3);
        self.for_read_locals(|local| buff.push(local));
        buff.into_iter()
    }
    pub fn read_locals_mut(&mut self) -> impl Iterator<Item = &mut Local> {
        let mut buff = Vec::with_capacity(3);
        self.for_read_locals_mut(|local| buff.push(local as *mut Local));
        buff.into_iter().map(|local_ptr| unsafe { &mut *local_ptr })
    }
    pub fn for_read_locals(&self, mut f: impl FnMut(Local)) {
        match *self {
            Self::Assignment(_, ref val) => val.for_locals(f),
            Self::Call(_, ref args, _) => {
                for arg in args {
                    if let OperandData::Copy(local) = arg.contents {
                        f(local)
                    }
                }
            }
            Self::NoOp => { }
        }
    }
    pub fn for_read_locals_mut(&mut self, mut f: impl FnMut(&mut Local)) {
        match self {
            Self::Assignment(_, val) => val.for_locals_mut(f),
            Self::Call(_, args, _) => {
                for arg in args {
                    if let OperandData::Copy(local) = &mut arg.contents {
                        f(local)
                    }
                }
            }
            Self::NoOp => { }
        }
    }
    pub fn locals(&self) -> impl Iterator<Item = Local> {
        let mut buff = Vec::with_capacity(3);
        self.for_locals(|local| buff.push(local));
        buff.into_iter()
    }
    pub fn locals_mut(&mut self) -> impl Iterator<Item = &mut Local> {
        let mut buff = Vec::with_capacity(3);
        self.for_locals_mut(|local| buff.push(local as *mut Local));
        buff.into_iter().map(|local_ptr| unsafe { &mut *local_ptr })
    }
    pub fn for_locals(&self, mut f: impl FnMut(Local)) {
        match *self {
            Self::Assignment(dst, ref val) => { f(dst); val.for_locals(f) }
            Self::Call(_, ref args, _) => {
                for arg in args {
                    if let OperandData::Copy(local) = arg.contents {
                        f(local)
                    }
                }
            }
            Self::NoOp => { }
        }
    }
    pub fn for_locals_mut(&mut self, mut f: impl FnMut(&mut Local)) {
        match self {
            Self::Assignment(dst, val) => { f(dst); val.for_locals_mut(f) }
            Self::Call(_, args, _) => {
                for arg in args {
                    if let OperandData::Copy(local) = &mut arg.contents {
                        f(local)
                    }
                }
            }
            Self::NoOp => { }
        }
    }
}
pub struct Variable {
    pub ident: Ident,
    pub variable_type: Type,
    pub default: RwLock<Expression<ParameterCallType>>,
    pub unit: Option<StringLiteral>,
    pub desc: Option<StringLiteral>,
    pub sctx: SyntaxCtx,
    pub ty: Type,
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::clone::Clone for Variable {
    #[inline]
    fn clone(&self) -> Variable {
        match *self {
            Variable {
            ident: ref __self_0_0,
            variable_type: ref __self_0_1,
            default: ref __self_0_2,
            unit: ref __self_0_3,
            desc: ref __self_0_4,
            sctx: ref __self_0_5,
            ty: ref __self_0_6 } =>
            Variable{ident: ::core::clone::Clone::clone(&(*__self_0_0)),
                     variable_type:
                         ::core::clone::Clone::clone(&(*__self_0_1)),
                     default: ::core::clone::Clone::clone(&(*__self_0_2)),
                     unit: ::core::clone::Clone::clone(&(*__self_0_3)),
                     desc: ::core::clone::Clone::clone(&(*__self_0_4)),
                     sctx: ::core::clone::Clone::clone(&(*__self_0_5)),
                     ty: ::core::clone::Clone::clone(&(*__self_0_6)),},
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::fmt::Debug for Variable {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match *self {
            Variable {
            ident: ref __self_0_0,
            variable_type: ref __self_0_1,
            default: ref __self_0_2,
            unit: ref __self_0_3,
            desc: ref __self_0_4,
            sctx: ref __self_0_5,
            ty: ref __self_0_6 } => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_struct(f, "Variable");
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "ident", &&(*__self_0_0));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "variable_type",
                                                    &&(*__self_0_1));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "default",
                                                    &&(*__self_0_2));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "unit", &&(*__self_0_3));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "desc", &&(*__self_0_4));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "sctx", &&(*__self_0_5));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder, "ty",
                                                    &&(*__self_0_6));
                ::core::fmt::DebugStruct::finish(debug_trait_builder)
            }
        }
    }
}
pub struct Nature {
    pub ident: Ident,
    pub abstol: f64,
    pub units: StringLiteral,
    pub access: Ident,
    pub idt_nature: NatureId,
    pub ddt_nature: NatureId,
    pub sctx: SyntaxCtx,
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::marker::Copy for Nature { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::clone::Clone for Nature {
    #[inline]
    fn clone(&self) -> Nature {
        {
            let _: ::core::clone::AssertParamIsClone<Ident>;
            let _: ::core::clone::AssertParamIsClone<f64>;
            let _: ::core::clone::AssertParamIsClone<StringLiteral>;
            let _: ::core::clone::AssertParamIsClone<Ident>;
            let _: ::core::clone::AssertParamIsClone<NatureId>;
            let _: ::core::clone::AssertParamIsClone<NatureId>;
            let _: ::core::clone::AssertParamIsClone<SyntaxCtx>;
            *self
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::fmt::Debug for Nature {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match *self {
            Nature {
            ident: ref __self_0_0,
            abstol: ref __self_0_1,
            units: ref __self_0_2,
            access: ref __self_0_3,
            idt_nature: ref __self_0_4,
            ddt_nature: ref __self_0_5,
            sctx: ref __self_0_6 } => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_struct(f, "Nature");
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "ident", &&(*__self_0_0));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "abstol",
                                                    &&(*__self_0_1));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "units", &&(*__self_0_2));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "access",
                                                    &&(*__self_0_3));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "idt_nature",
                                                    &&(*__self_0_4));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "ddt_nature",
                                                    &&(*__self_0_5));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "sctx", &&(*__self_0_6));
                ::core::fmt::DebugStruct::finish(debug_trait_builder)
            }
        }
    }
}
pub struct Module<I: CfgFunctions> {
    pub ident: Ident,
    pub ports: IdRange<PortId>,
    pub parameters: IdRange<ParameterId>,
    pub analog_cfg: RwLock<ControlFlowGraph<I>>,
    pub sctx: SyntaxCtx,
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl <I: ::core::fmt::Debug + CfgFunctions> ::core::fmt::Debug for Module<I> {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match *self {
            Module {
            ident: ref __self_0_0,
            ports: ref __self_0_1,
            parameters: ref __self_0_2,
            analog_cfg: ref __self_0_3,
            sctx: ref __self_0_4 } => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_struct(f, "Module");
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "ident", &&(*__self_0_0));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "ports", &&(*__self_0_1));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "parameters",
                                                    &&(*__self_0_2));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "analog_cfg",
                                                    &&(*__self_0_3));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "sctx", &&(*__self_0_4));
                ::core::fmt::DebugStruct::finish(debug_trait_builder)
            }
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl <I: ::core::clone::Clone + CfgFunctions> ::core::clone::Clone for
 Module<I> {
    #[inline]
    fn clone(&self) -> Module<I> {
        match *self {
            Module {
            ident: ref __self_0_0,
            ports: ref __self_0_1,
            parameters: ref __self_0_2,
            analog_cfg: ref __self_0_3,
            sctx: ref __self_0_4 } =>
            Module{ident: ::core::clone::Clone::clone(&(*__self_0_0)),
                   ports: ::core::clone::Clone::clone(&(*__self_0_1)),
                   parameters: ::core::clone::Clone::clone(&(*__self_0_2)),
                   analog_cfg: ::core::clone::Clone::clone(&(*__self_0_3)),
                   sctx: ::core::clone::Clone::clone(&(*__self_0_4)),},
        }
    }
}
pub struct Parameter {
    pub ident: Ident,
    pub ty: Type,
    pub default: RwLock<Expression<ParameterCallType>>,
    pub kind: RwLock<ParameterConstraint>,
    pub unit: Option<StringLiteral>,
    pub desc: Option<StringLiteral>,
    pub sctx: SyntaxCtx,
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::clone::Clone for Parameter {
    #[inline]
    fn clone(&self) -> Parameter {
        match *self {
            Parameter {
            ident: ref __self_0_0,
            ty: ref __self_0_1,
            default: ref __self_0_2,
            kind: ref __self_0_3,
            unit: ref __self_0_4,
            desc: ref __self_0_5,
            sctx: ref __self_0_6 } =>
            Parameter{ident: ::core::clone::Clone::clone(&(*__self_0_0)),
                      ty: ::core::clone::Clone::clone(&(*__self_0_1)),
                      default: ::core::clone::Clone::clone(&(*__self_0_2)),
                      kind: ::core::clone::Clone::clone(&(*__self_0_3)),
                      unit: ::core::clone::Clone::clone(&(*__self_0_4)),
                      desc: ::core::clone::Clone::clone(&(*__self_0_5)),
                      sctx: ::core::clone::Clone::clone(&(*__self_0_6)),},
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::fmt::Debug for Parameter {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match *self {
            Parameter {
            ident: ref __self_0_0,
            ty: ref __self_0_1,
            default: ref __self_0_2,
            kind: ref __self_0_3,
            unit: ref __self_0_4,
            desc: ref __self_0_5,
            sctx: ref __self_0_6 } => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_struct(f, "Parameter");
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "ident", &&(*__self_0_0));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder, "ty",
                                                    &&(*__self_0_1));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "default",
                                                    &&(*__self_0_2));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "kind", &&(*__self_0_3));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "unit", &&(*__self_0_4));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "desc", &&(*__self_0_5));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "sctx", &&(*__self_0_6));
                ::core::fmt::DebugStruct::finish(debug_trait_builder)
            }
        }
    }
}
/// A Parameter kind indicates what kind of constraints are placed on a parmeter
///
///
/// These differ between Ordered (float, integer) and unordered (string) values
/// Unordered
///
/// # Note
/// Don't confuse parameter kind with parameter type.
/// Each parameter kind actually correspond to multiple types
pub enum ParameterConstraint {
    Ordered {
        included: Vec<ParameterRangeConstraint<Expression<ParameterCallType>>>,
        excluded: Vec<ParameterExcludeConstraint<Expression<ParameterCallType>>>,
    },
    UnOrdered {
        included: Vec<Expression<ParameterCallType>>,
        excluded: Vec<Expression<ParameterCallType>>,
    },
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::fmt::Debug for ParameterConstraint {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match (&*self,) {
            (&ParameterConstraint::Ordered {
             included: ref __self_0, excluded: ref __self_1 },) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_struct(f, "Ordered");
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "included",
                                                    &&(*__self_0));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "excluded",
                                                    &&(*__self_1));
                ::core::fmt::DebugStruct::finish(debug_trait_builder)
            }
            (&ParameterConstraint::UnOrdered {
             included: ref __self_0, excluded: ref __self_1 },) => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_struct(f, "UnOrdered");
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "included",
                                                    &&(*__self_0));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "excluded",
                                                    &&(*__self_1));
                ::core::fmt::DebugStruct::finish(debug_trait_builder)
            }
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::clone::Clone for ParameterConstraint {
    #[inline]
    fn clone(&self) -> ParameterConstraint {
        match (&*self,) {
            (&ParameterConstraint::Ordered {
             included: ref __self_0, excluded: ref __self_1 },) =>
            ParameterConstraint::Ordered{included:
                                             ::core::clone::Clone::clone(&(*__self_0)),
                                         excluded:
                                             ::core::clone::Clone::clone(&(*__self_1)),},
            (&ParameterConstraint::UnOrdered {
             included: ref __self_0, excluded: ref __self_1 },) =>
            ParameterConstraint::UnOrdered{included:
                                               ::core::clone::Clone::clone(&(*__self_0)),
                                           excluded:
                                               ::core::clone::Clone::clone(&(*__self_1)),},
        }
    }
}
pub struct DefaultConversion;
impl <S, D> CfgConversion<S, D> for DefaultConversion where S: CfgFunctions +
 Into<D>, D: CfgFunctions, S::I: Into<D::I> {
    fn map_input(&mut self, src: S::I) -> COperandData<D> {
        OperandData::Read(src.into())
    }
    fn map_call_val(&mut self, call: S, args: IndexVec<CallArg, COperand<S>>,
                    span: Span) -> RValue<D> {
        RValue::Call(call.into(),
                     args.into_iter().map(|arg|
                                              CfgConversion::<S,
                                                              D>::map_operand(self,
                                                                              arg)).collect(),
                     span)
    }
    fn map_call_stmnt(&mut self, call: S,
                      args: IndexVec<CallArg, COperand<S>>, span: Span)
     -> StmntKind<D> {
        StmntKind::Call(call.into(),
                        args.into_iter().map(|arg|
                                                 CfgConversion::<S,
                                                                 D>::map_operand(self,
                                                                                 arg)).collect(),
                        span)
    }
}
pub trait CfgConversion<S: CfgFunctions, D: CfgFunctions>: Sized {
    fn map_operand(&mut self, op: COperand<S>) -> COperand<D> {
        let contents =
            match op.contents {
                OperandData::Read(input) => self.map_input(input),
                OperandData::Constant(val) => OperandData::Constant(val),
                OperandData::Copy(loc) => OperandData::Copy(loc),
            };
        Spanned{contents, span: op.span,}
    }
    fn map_input(&mut self, _src: S::I)
    -> COperandData<D>;
    fn map_call_val(&mut self, call: S, args: IndexVec<CallArg, COperand<S>>,
                    span: Span)
    -> RValue<D>;
    fn map_call_stmnt(&mut self, call: S,
                      args: IndexVec<CallArg, COperand<S>>, span: Span)
    -> StmntKind<D>;
    fn map_stmnt(&mut self, kind: StmntKind<S>) -> StmntKind<D> {
        match kind {
            StmntKind::Assignment(dst, val) =>
            StmntKind::Assignment(dst, val.map_operands(self)),
            StmntKind::Call(call, args, span) =>
            self.map_call_stmnt(call, args, span),
            StmntKind::NoOp => StmntKind::NoOp,
        }
    }
}
pub struct Net {
    pub ident: Ident,
    pub discipline: DisciplineId,
    pub sctx: SyntaxCtx,
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::clone::Clone for Net {
    #[inline]
    fn clone(&self) -> Net {
        {
            let _: ::core::clone::AssertParamIsClone<Ident>;
            let _: ::core::clone::AssertParamIsClone<DisciplineId>;
            let _: ::core::clone::AssertParamIsClone<SyntaxCtx>;
            *self
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::marker::Copy for Net { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::fmt::Debug for Net {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match *self {
            Net {
            ident: ref __self_0_0,
            discipline: ref __self_0_1,
            sctx: ref __self_0_2 } => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_struct(f, "Net");
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "ident", &&(*__self_0_0));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "discipline",
                                                    &&(*__self_0_1));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "sctx", &&(*__self_0_2));
                ::core::fmt::DebugStruct::finish(debug_trait_builder)
            }
        }
    }
}
pub struct Branch {
    pub ident: Ident,
    pub hi: NetId,
    pub lo: NetId,
    pub sctx: SyntaxCtx,
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::clone::Clone for Branch {
    #[inline]
    fn clone(&self) -> Branch {
        {
            let _: ::core::clone::AssertParamIsClone<Ident>;
            let _: ::core::clone::AssertParamIsClone<NetId>;
            let _: ::core::clone::AssertParamIsClone<NetId>;
            let _: ::core::clone::AssertParamIsClone<SyntaxCtx>;
            *self
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::marker::Copy for Branch { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::fmt::Debug for Branch {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match *self {
            Branch {
            ident: ref __self_0_0,
            hi: ref __self_0_1,
            lo: ref __self_0_2,
            sctx: ref __self_0_3 } => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_struct(f, "Branch");
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "ident", &&(*__self_0_0));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder, "hi",
                                                    &&(*__self_0_1));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder, "lo",
                                                    &&(*__self_0_2));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "sctx", &&(*__self_0_3));
                ::core::fmt::DebugStruct::finish(debug_trait_builder)
            }
        }
    }
}
pub struct Discipline {
    pub ident: Ident,
    pub flow_nature: Option<NatureId>,
    pub potential_nature: Option<NatureId>,
    pub continuous: bool,
    pub sctx: SyntaxCtx,
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::clone::Clone for Discipline {
    #[inline]
    fn clone(&self) -> Discipline {
        {
            let _: ::core::clone::AssertParamIsClone<Ident>;
            let _: ::core::clone::AssertParamIsClone<Option<NatureId>>;
            let _: ::core::clone::AssertParamIsClone<Option<NatureId>>;
            let _: ::core::clone::AssertParamIsClone<bool>;
            let _: ::core::clone::AssertParamIsClone<SyntaxCtx>;
            *self
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::marker::Copy for Discipline { }
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::fmt::Debug for Discipline {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match *self {
            Discipline {
            ident: ref __self_0_0,
            flow_nature: ref __self_0_1,
            potential_nature: ref __self_0_2,
            continuous: ref __self_0_3,
            sctx: ref __self_0_4 } => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_struct(f,
                                                              "Discipline");
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "ident", &&(*__self_0_0));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "flow_nature",
                                                    &&(*__self_0_1));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "potential_nature",
                                                    &&(*__self_0_2));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "continuous",
                                                    &&(*__self_0_3));
                let _ =
                    ::core::fmt::DebugStruct::field(debug_trait_builder,
                                                    "sctx", &&(*__self_0_4));
                ::core::fmt::DebugStruct::finish(debug_trait_builder)
            }
        }
    }
}
