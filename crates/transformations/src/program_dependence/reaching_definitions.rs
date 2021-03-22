//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************
#![allow(clippy::similar_names)]

use openvaf_data_structures::{BitSet, SparseBitSetMatrix};
use openvaf_middle::cfg::{
    BasicBlock, ControlFlowGraph, InternedLocations, LocationId, TerminatorKind,
};
use openvaf_middle::dfa::{DfGraph, Engine, Forward, GenKillAnalysis, GenKillEngine, GenKillSet};
use openvaf_middle::{CallType, Local, OperandData, StmntKind};
use std::mem::take;

#[derive(Debug, Clone)]
pub struct UseDefGraph {
    pub use_def_chains: SparseBitSetMatrix<LocationId, LocationId>,
    pub assignments: SparseBitSetMatrix<Local, LocationId>,
}
pub struct ReachingDefinitionsAnalysis<'a> {
    graph: UseDefGraph,
    locations: &'a InternedLocations,
}

impl<'a> ReachingDefinitionsAnalysis<'a> {
    pub fn new<C: CallType>(
        cfg: & ControlFlowGraph<C>,
        locations: &'a InternedLocations,
    ) -> Self {
        let mut graph = UseDefGraph {
            use_def_chains: SparseBitSetMatrix::new_empty(locations.len_idx(), locations.len_idx()),
            assignments: SparseBitSetMatrix::new_empty(cfg.locals.len_idx(), locations.len_idx()),
        };

        for (block, data) in cfg.blocks.iter_enumerated() {
            let block_location = &locations[block];

            for (phi, data) in data.phi_statements.iter_enumerated() {
                graph
                    .assignments
                    .insert(data.dst, block_location.phi_start + phi.index())
            }

            for (stmnt, (kind, _)) in data.statements.iter_enumerated() {
                if let StmntKind::Assignment(dst, _) = *kind {
                    graph
                        .assignments
                        .insert(dst, block_location.stmnt_start + stmnt.index())
                }
            }
        }

        Self { graph, locations }
    }

    fn write_use_def_chain(
        &mut self,
        location: LocationId,
        locals: impl IntoIterator<Item = Local>,
        tmp: &mut BitSet<LocationId>,
        reaching: &BitSet<LocationId>,
    ) {
        tmp.clear();
        for local in locals {
            tmp.union_with(&self.graph.assignments[local])
        }
        tmp.intersect_with(reaching);

        self.graph.use_def_chains[location] = tmp.to_hybrid();
    }

    pub fn solve<C: CallType>(&mut self, cfg: &ControlFlowGraph<C>) -> DfGraph<BitSet<LocationId>>{
        let mut genkill_engine = GenKillEngine::new(cfg, self);
        let engine = Engine::new(cfg, &mut genkill_engine);
        engine.iterate_to_fixpoint()
    }

    pub fn to_use_def<C: CallType>(mut self, cfg: &ControlFlowGraph<C>, mut dfg: DfGraph<BitSet<LocationId>>) -> UseDefGraph {


        for (block, data) in cfg.blocks.iter_enumerated() {
            // reusing the in set because we dont need it afterwards anyway
            let mut reaching_definitions = take(&mut dfg.in_sets[block]);
            // reusing the out set because we dont need it afterwards anyway
            let mut tmp = take(&mut dfg.out_sets[block]);

            let block_locations = &self.locations[block];

            for (phi, data) in data.phi_statements.iter_enumerated() {
                let location = block_locations.phi_start + phi.index();
                self.write_use_def_chain(
                    location,
                    data.sources.iter().map(|(_, local)| *local),
                    &mut tmp,
                    &reaching_definitions,
                );
                reaching_definitions.difference_with(&self.graph.assignments[data.dst]);
                reaching_definitions.insert(location)
            }

            for (stmnt, (kind, _)) in data.statements.iter_enumerated() {
                let location = block_locations.stmnt_start + stmnt.index();
                match kind {
                    StmntKind::Assignment(dst, value) => {
                        self.write_use_def_chain(
                            location,
                            value.locals(),
                            &mut tmp,
                            &reaching_definitions,
                        );
                        reaching_definitions.difference_with(&self.graph.assignments[*dst]);
                        reaching_definitions.insert(location)
                    }
                    StmntKind::Call(_, args, _) => {
                        let locals = args.iter().filter_map(|operand| {
                            if let OperandData::Copy(local) = operand.contents {
                                Some(local)
                            } else {
                                None
                            }
                        });
                        self.write_use_def_chain(
                            location + stmnt.index(),
                            locals,
                            &mut tmp,
                            &reaching_definitions,
                        )
                    }
                    StmntKind::NoOp => {}
                }
            }

            if let TerminatorKind::Split { condition, .. } = &data.terminator().kind {
                self.write_use_def_chain(
                    block_locations.terminator,
                    condition.locals(),
                    &mut tmp,
                    &reaching_definitions,
                );
            }
        }

        self.graph
    }
}

impl<'lt, C: CallType> GenKillAnalysis<C> for ReachingDefinitionsAnalysis<'lt> {
    type SetType = LocationId;
    type Direction = Forward;

    fn transfer_function(
        &mut self,
        gen_kill_set: &mut GenKillSet<LocationId>,
        basic_bock: BasicBlock,
        cfg: &ControlFlowGraph<C>,
    ) {
        let block_location = &self.locations[basic_bock];

        for (phi, data) in cfg[basic_bock].phi_statements.iter_enumerated() {
            gen_kill_set.kill_all(&self.graph.assignments[data.dst]);
            gen_kill_set.gen(block_location.phi_start + phi.index());
        }

        for (stmnt, (kind, _)) in cfg[basic_bock].statements.iter_enumerated() {
            if let StmntKind::Assignment(dst, _) = kind {
                gen_kill_set.kill_all(&self.graph.assignments[*dst]);
                gen_kill_set.gen(block_location.stmnt_start + stmnt.index());
            }
        }
    }

    fn max_idx(&self) -> Self::SetType {
        self.locations.len_idx()
    }

    fn setup_entry(&mut self, _block: BasicBlock, _graph: &mut DfGraph<BitSet<Self::SetType>>) {}
}

#[cfg(feature = "graph_debug")]
mod print {
    use super::*;
    use rustc_ap_graphviz as dot;
    use rustc_ap_graphviz::LabelText::{EscStr, LabelStr};
    use rustc_ap_graphviz::{Edges, Id, LabelText, Nodes};

    use crate::HashSet;
    use std::borrow::Cow;
    use std::io::Write;

    impl ControlFlowGraph {
        pub fn render_data_dependence_to<W: Write>(&self, write: &mut W, udg: &UseDefGraph) {
            dot::render(&DataDependenceGraph(self, udg), write).expect("Rendering failed")
        }
    }

    struct DataDependenceGraph<'lt>(&'lt ControlFlowGraph, &'lt UseDefGraph);

    #[derive(Copy, Clone, Eq, PartialEq)]
    enum FlowOrDependence {
        Flow,
        Dependence,
    }

    impl<'a> dot::Labeller<'a> for DataDependenceGraph<'a> {
        type Node = BasicBlock;
        type Edge = (FlowOrDependence, BasicBlock, BasicBlock);

        fn graph_id(&'a self) -> Id<'a> {
            dot::Id::new("ControlFlowGraph").unwrap()
        }

        fn node_id(&'a self, n: &Self::Node) -> Id<'a> {
            dot::Id::new(format!("BB_{}", n.index())).unwrap()
        }

        fn node_label(&'a self, &n: &Self::Node) -> LabelText<'a> {
            match self.0.blocks[n].terminator {
                Terminator::End => EscStr(Cow::Owned(format!(
                    "BB_{}: {} Statements\n END",
                    n.index(),
                    self.0.blocks[n].statements.len(),
                ))),
                Terminator::Goto(_) => LabelStr(Cow::Owned(format!(
                    "BB_{}: {} Statements",
                    n.index(),
                    self.0.blocks[n].statements.len(),
                ))),
                Terminator::Split {
                    condition, merge, ..
                } => LabelStr(Cow::Owned(format!(
                    "BB_{}: {} Statements\nSplit at {:?}\nMerge at BB_{}",
                    n.index(),
                    self.0.blocks[n].statements.len(),
                    condition,
                    merge.index(),
                ))),
            }
        }
        fn edge_label(
            &'a self,
            &(edge_type, start, dst): &(FlowOrDependence, BasicBlock, BasicBlock),
        ) -> LabelText<'a> {
            match edge_type {
                FlowOrDependence::Flow => match self.0[start].terminator {
                    Terminator::Goto(_) => LabelStr(Cow::Borrowed("GOTO")),
                    Terminator::End => LabelStr(Cow::Borrowed("ILLEGAL")),
                    Terminator::Split {
                        true_block,
                        false_block,
                        ..
                    } => {
                        let true_or_false = if true_block == dst {
                            "TRUE"
                        } else if false_block == dst {
                            "FALSE"
                        } else {
                            "ILLEGAL"
                        };
                        LabelStr(Cow::Borrowed(true_or_false))
                    }
                },

                FlowOrDependence::Dependence => LabelStr(Cow::Borrowed("Data dependence")),
            }
        }
    }

    impl<'a> dot::GraphWalk<'a> for DataDependenceGraph<'a> {
        type Node = BasicBlock;
        type Edge = (FlowOrDependence, BasicBlock, BasicBlock);

        fn nodes(&'a self) -> Nodes<'a, Self::Node> {
            Cow::Owned(self.0.blocks.indices().collect())
        }

        fn edges(&'a self) -> Edges<'a, Self::Edge> {
            let mut edges = Vec::new();
            for (id, bb) in self.0.blocks.iter_enumerated() {
                match bb.terminator {
                    Terminator::Goto(dst) => edges.push((FlowOrDependence::Flow, id, dst)),
                    Terminator::Split {
                        true_block,
                        false_block,
                        ..
                    } => {
                        edges.push((FlowOrDependence::Flow, id, false_block));
                        edges.push((FlowOrDependence::Flow, id, true_block));
                    }
                    Terminator::End => (),
                }
            }
            let mut processed_data_dependencies =
                HashSet::with_capacity(self.0.blocks.len() * self.0.blocks.len());

            for (src, data_dependencies) in self.1.stmt_use_def_chains.iter_enumerated() {
                let src_block = self.0.containing_block(src).unwrap();
                for dst in data_dependencies.ones() {
                    let dst_block = self.0.containing_block(dst).unwrap();
                    if processed_data_dependencies.insert((src_block, dst_block)) {
                        edges.push((FlowOrDependence::Dependence, src_block, dst_block));
                    }
                }
            }

            for (src, data_dependencies) in self.1.terminator_use_def_chains.iter_enumerated() {
                for dst in data_dependencies.ones() {
                    let dst_block = self.0.containing_block(dst).unwrap();
                    if processed_data_dependencies.insert((src, dst_block)) {
                        edges.push((FlowOrDependence::Dependence, src, dst_block));
                    }
                }
            }

            Cow::Owned(edges)
        }

        fn source(&'a self, edge: &Self::Edge) -> Self::Node {
            edge.1
        }

        fn target(&'a self, edge: &Self::Edge) -> Self::Node {
            edge.2
        }
    }
}
