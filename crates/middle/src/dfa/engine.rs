/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

//! A solver for dataflow problems.

use std::borrow::BorrowMut;

use super::{Analysis, Direction, GenKillAnalysis, GenKillSet, JoinSemiLattice};
use crate::cfg::{BasicBlock, BasicBlockData, ControlFlowGraph, START_BLOCK};
use crate::dfa::cursors::{ResultsCursor, ResultsRefCursor};
use crate::dfa::visitor::{
    visit_results, visit_results_mut, ResultsVisitable, ResultsVisitor, ResultsVisitorMut,
};
use crate::dfa::GenKillAnalysisImpl;
use crate::CfgFunctions;
use data_structures::index_vec::{index_vec, Idx, IndexVec};
use data_structures::{iter, WorkQueue};
use std::fmt;
use std::fmt::{Debug, Formatter};
use tracing::debug;

pub type GenKillResults<C, A> = Results<C, GenKillAnalysisImpl<A>>;

/// A dataflow analysis that has converged to fixpoint.
pub struct Results<C, A>
where
    A: Analysis<C>,
    C: CfgFunctions,
{
    pub analysis: A,
    pub(crate) entry_sets: IndexVec<BasicBlock, A::Domain>,
}

impl<A, C> Results<C, A>
where
    A: Analysis<C>,
    C: CfgFunctions,
{
    /// Creates a `ResultsCursor` that can inspect these `Results`.
    pub fn into_results_cursor(self, cfg: &ControlFlowGraph<C>) -> ResultsCursor<C, A> {
        ResultsCursor::new(cfg, self)
    }

    /// Creates a `ResultsCursor` that can inspect these `Results`.
    pub fn as_results_cursor(&self, cfg: &ControlFlowGraph<C>) -> ResultsRefCursor<C, A> {
        ResultsRefCursor::new(cfg, &self)
    }

    /// Gets the dataflow state for the given block.
    pub fn entry_set_for_block(&self, block: BasicBlock) -> &A::Domain {
        &self.entry_sets[block]
    }

    pub fn visit_in_blocks_with<'a>(
        &self,
        cfg: &'a ControlFlowGraph<C>,
        blocks: impl IntoIterator<Item = (BasicBlock, &'a BasicBlockData<C>)>,
        vis: &mut impl ResultsVisitor<C, FlowState = <Self as ResultsVisitable<C>>::FlowState>,
    ) {
        visit_results(cfg, blocks, self, vis)
    }

    pub fn visit_in_blocks_with_mut(
        &self,
        cfg: &mut ControlFlowGraph<C>,
        blocks: impl IntoIterator<Item = BasicBlock>,
        vis: &mut impl ResultsVisitorMut<C, FlowState = <Self as ResultsVisitable<C>>::FlowState>,
    ) {
        visit_results_mut(cfg, blocks, self, vis)
    }

    pub fn visit_with(
        &self,
        cfg: &ControlFlowGraph<C>,
        vis: &mut impl ResultsVisitor<C, FlowState = <Self as ResultsVisitable<C>>::FlowState>,
    ) {
        self.visit_in_blocks_with(cfg, cfg.blocks.iter_enumerated(), vis)
    }

    pub fn visit_with_mut(
        &self,
        cfg: &mut ControlFlowGraph<C>,
        vis: &mut impl ResultsVisitorMut<C, FlowState = <Self as ResultsVisitable<C>>::FlowState>,
    ) {
        self.visit_in_blocks_with_mut(cfg, cfg.blocks.indices(), vis)
    }
}

impl<C, A> Debug for Results<C, A>
where
    A: Analysis<C>,
    C: CfgFunctions,
    A::Domain: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.entry_sets, f)
    }
}

/// A solver for dataflow problems.
pub struct Engine<'a, C, A>
where
    A: Analysis<C>,
    C: CfgFunctions,
{
    cfg: &'a ControlFlowGraph<C>,
    entry_sets: IndexVec<BasicBlock, A::Domain>,
    analysis: A,

    /// Cached, cumulative transfer functions for each block.
    //
    // FIXME(ecstaticmorse): This boxed `Fn` trait object is invoked inside a tight loop for
    // gen/kill problems on cyclic CFGs. This is not ideal, but it doesn't seem to degrade
    // performance in practice. I've tried a few ways to avoid this, but they have downsides. See
    // the message for the commit that added this FIXME for more information.
    #[allow(clippy::type_complexity)]
    apply_trans_for_block: Option<Box<dyn Fn(BasicBlock, &mut A::Domain)>>,
}

impl<'a, C, A, T> Engine<'a, C, GenKillAnalysisImpl<A>>
where
    A: GenKillAnalysis<C, Idx = T>,
    C: CfgFunctions,
    T: Idx,
{
    /// Creates a new `Engine` to solve a gen-kill dataflow problem.
    pub fn new_gen_kill(cfg: &'a ControlFlowGraph<C>, mut analysis: A) -> Self {
        // If there are no back-edges in the control-flow graph, we only ever need to apply the
        // transfer function for each block exactly once (assuming that we process blocks in RPO).
        //
        // In this case, there's no need to compute the block transfer functions ahead of time.
        if !cfg.is_cyclic() {
            debug!("Non Cyclical CFG! Gen Kill Transfer Functions are not cached");
            return Self::new(cfg, GenKillAnalysisImpl(analysis), None);
        }

        debug!("Cyclical CFG! Caching gen kill sets");

        // Otherwise, compute and store the cumulative transfer function for each block.

        let identity = GenKillSet::identity(analysis.domain_size(cfg));
        let mut trans_for_block = index_vec![identity; cfg.blocks.len()];

        for ((block, block_data), trans) in
            iter::zip(cfg.blocks.iter_enumerated(), &mut trans_for_block)
        {
            A::Direction::gen_kill_effects_in_block(&mut analysis, cfg, trans, block, block_data);
        }

        Self::new(
            cfg,
            GenKillAnalysisImpl(analysis),
            Some(Box::new(move |bb, dst| trans_for_block[bb].apply(dst.borrow_mut()))),
        )
    }
}

impl<'a, C, A, D> Engine<'a, C, A>
where
    A: Analysis<C, Domain = D>,
    D: Clone + JoinSemiLattice + Debug,
    C: CfgFunctions,
{
    /// Creates a new `Engine` to solve a dataflow problem with an arbitrary transfer
    /// function.
    ///
    /// Gen-kill problems should use `new_gen_kill`, which will coalesce transfer functions for
    /// better performance.
    pub fn new_generic(cfg: &'a ControlFlowGraph<C>, analysis: A) -> Self {
        Self::new(cfg, analysis, None)
    }

    #[allow(clippy::type_complexity)]
    fn new(
        cfg: &'a ControlFlowGraph<C>,
        analysis: A,
        apply_trans_for_block: Option<Box<dyn Fn(BasicBlock, &mut A::Domain)>>,
    ) -> Self {
        let bottom_value = analysis.bottom_value(cfg);
        let mut entry_sets = index_vec![bottom_value.clone(); cfg.blocks.len()];
        analysis.initialize_start_block(cfg, &mut entry_sets[START_BLOCK]);

        if !A::Direction::IS_FORWARD && entry_sets[START_BLOCK] != bottom_value {
            panic!("`initialize_start_block` is not yet supported for backward dataflow analyses");
        }

        Engine { cfg, entry_sets, analysis, apply_trans_for_block }
    }

    /// Computes the fixpoint for this dataflow problem and returns it.
    pub fn iterate_to_fixpoint(self) -> Results<C, A>
//  where
  //      A::Domain: DebugWithContext<A>,
    {
        let Engine { mut analysis, cfg, mut entry_sets, apply_trans_for_block, .. } = self;

        let mut dirty_queue: WorkQueue<BasicBlock> = WorkQueue::with_none(cfg.blocks.len());

        // Iterating the whole graph garuntees nice iteration order.
        // However it also forces DFA to run over the whole graph even for analysis modes where it could be avoided (eg COndition Constants where unreachable sets may never be visited at all)
        // TODO determine if improving this might be worthwhile (I wouldd guess not but CC is very slow)
        if A::Direction::IS_FORWARD {
            dirty_queue.extend(cfg.reverse_postorder())
        } else {
            // Reverse post-order on the reverse CFG may generate a better iteration order for
            // backward dataflow analyses, but probably not enough to matter.
            dirty_queue.extend(cfg.postorder_iter().map(|(bb, _)| bb))
        }

        // `state` is not actually used between iterations;
        // this is just an optimization to avoid reallocating every iteration.
        let mut state = analysis.bottom_value(cfg);
        while let Some(bb) = dirty_queue.pop() {
            let bb_data = &cfg[bb];

            // Set the state to the entry state of the block.
            // This is equivalent to `state = entry_sets[bb].clone()`,
            // but it saves an allocation, thus improving compile times.
            state.clone_from(&entry_sets[bb]);

            // Apply the block transfer function, using the cached one if it exists.
            match &apply_trans_for_block {
                Some(apply) => apply(bb, &mut state),
                None => A::Direction::apply_effects_in_block(
                    &mut analysis,
                    cfg,
                    &mut state,
                    bb,
                    bb_data,
                ),
            }

            A::Direction::join_state_into_successors_of(
                &mut analysis,
                cfg,
                &mut state,
                (bb, bb_data),
                |target: BasicBlock, state: &A::Domain| {
                    let set_changed = entry_sets[target].join(state);
                    if set_changed {
                        dirty_queue.insert(target);
                    }
                },
            );
        }

        Results { analysis, entry_sets }

        // let res = write_graphviz_results(tcx, &body, &results, pass_name);
        // if let Err(e) = res {
        //     error!("Failed to write graphviz dataflow results: {}", e);
        // }
    }
}

// Graphviz

// /// Writes a DOT file containing the results of a dataflow analysis if the user requested it via
// /// `rustc_mir` attributes.
// fn write_graphviz_results<A>(
//     tcx: TyCtxt<'tcx>,
//     body: &mir::Body<'tcx>,
//     results: &Results<'tcx, A>,
//     pass_name: Option<&'static str>,
// ) -> std::io::Result<()>
// where
//     A: Analysis<'tcx>,
//     A::Domain: DebugWithContext<A>,
// {
//     use std::fs;
//     use std::io::{self, Write};
//
//     let def_id = body.source.def_id();
//     let attrs = match RustcMirAttrs::parse(tcx, def_id) {
//         Ok(attrs) => attrs,
//
//         // Invalid `rustc_mir` attrs are reported in `RustcMirAttrs::parse`
//         Err(()) => return Ok(()),
//     };
//
//     let mut file = match attrs.output_path(A::NAME) {
//         Some(path) => {
//             debug!(
//                 "printing dataflow results for {:?} to {}",
//                 def_id,
//                 path.display()
//             );
//             if let Some(parent) = path.parent() {
//                 fs::create_dir_all(parent)?;
//             }
//             io::BufWriter::new(fs::File::create(&path)?)
//         }
//
//         None if tcx.sess.opts.debugging_opts.dump_mir_dataflow
//             && dump_enabled(tcx, A::NAME, def_id) =>
//         {
//             create_dump_file(
//                 tcx,
//                 ".dot",
//                 None,
//                 A::NAME,
//                 &pass_name.unwrap_or("-----"),
//                 body.source,
//             )?
//         }
//
//         _ => return Ok(()),
//     };
//
//     let style = match attrs.formatter {
//         Some(sym::two_phase) => graphviz::OutputStyle::BeforeAndAfter,
//         _ => graphviz::OutputStyle::AfterOnly,
//     };
//
//     let mut buf = Vec::new();
//
//     let graphviz = graphviz::Formatter::new(body, results, style);
//     let mut render_opts = vec![dot::RenderOption::Fontname(
//         tcx.sess.opts.debugging_opts.graphviz_font.clone(),
//     )];
//     if tcx.sess.opts.debugging_opts.graphviz_dark_mode {
//         render_opts.push(dot::RenderOption::DarkTheme);
//     }
//     dot::render_opts(&graphviz, &mut buf, &render_opts)?;
//
//     file.write_all(&buf)?;
//
//     Ok(())
// }
//
// #[derive(Default)]
// struct RustcMirAttrs {
//     basename_and_suffix: Option<PathBuf>,
//     formatter: Option<Symbol>,
// }
//
// impl RustcMirAttrs {
//     fn parse(tcx: TyCtxt<'tcx>, def_id: DefId) -> Result<Self, ()> {
//         let attrs = tcx.get_attrs(def_id);
//
//         let mut result = Ok(());
//         let mut ret = RustcMirAttrs::default();
//
//         let rustc_mir_attrs = attrs
//             .iter()
//             .filter(|attr| tcx.sess.check_name(attr, sym::rustc_mir))
//             .flat_map(|attr| {
//                 attr.meta_item_list()
//                     .into_iter()
//                     .flat_map(|v| v.into_iter())
//             });
//
//         for attr in rustc_mir_attrs {
//             let attr_result = if attr.has_name(sym::borrowck_graphviz_postflow) {
//                 Self::set_field(&mut ret.basename_and_suffix, tcx, &attr, |s| {
//                     let path = PathBuf::from(s.to_string());
//                     match path.file_name() {
//                         Some(_) => Ok(path),
//                         None => {
//                             tcx.sess
//                                 .span_err(attr.span(), "path must end in a filename");
//                             Err(())
//                         }
//                     }
//                 })
//             } else if attr.has_name(sym::borrowck_graphviz_format) {
//                 Self::set_field(&mut ret.formatter, tcx, &attr, |s| match s {
//                     sym::gen_kill | sym::two_phase => Ok(s),
//                     _ => {
//                         tcx.sess.span_err(attr.span(), "unknown formatter");
//                         Err(())
//                     }
//                 })
//             } else {
//                 Ok(())
//             };
//
//             result = result.and(attr_result);
//         }
//
//         result.map(|()| ret)
//     }
//
//     fn set_field<T>(
//         field: &mut Option<T>,
//         tcx: TyCtxt<'tcx>,
//         attr: &ast::NestedMetaItem,
//         mapper: impl FnOnce(Symbol) -> Result<T, ()>,
//     ) -> Result<(), ()> {
//         if field.is_some() {
//             tcx.sess.span_err(
//                 attr.span(),
//                 &format!("duplicate values for `{}`", attr.name_or_empty()),
//             );
//
//             return Err(());
//         }
//
//         if let Some(s) = attr.value_str() {
//             *field = Some(mapper(s)?);
//             Ok(())
//         } else {
//             tcx.sess.span_err(
//                 attr.span(),
//                 &format!("`{}` requires an argument", attr.name_or_empty()),
//             );
//             Err(())
//         }
//     }
//
//     /// Returns the path where dataflow results should be written, or `None`
//     /// `borrowck_graphviz_postflow` was not specified.
//     ///
//     /// This performs the following transformation to the argument of `borrowck_graphviz_postflow`:
//     ///
//     /// "path/suffix.dot" -> "path/analysis_name_suffix.dot"
//     fn output_path(&self, analysis_name: &str) -> Option<PathBuf> {
//         let mut ret = self.basename_and_suffix.as_ref().cloned()?;
//         let suffix = ret.file_name().unwrap(); // Checked when parsing attrs
//
//         let mut file_name: OsString = analysis_name.into();
//         file_name.push("_");
//         file_name.push(suffix);
//         ret.set_file_name(file_name);
//
//         Some(ret)
//     }
// }
