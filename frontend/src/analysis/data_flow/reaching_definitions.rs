//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************
#![allow(clippy::similar_names)]

use crate::analysis::data_flow::framework::{
    Engine, Forward, GenKillAnalysis, GenKillEngine, GenKillSet,
};

use crate::cfg::ControlFlowGraph;
use crate::cfg::{BasicBlockId, Terminator};
use crate::data_structures::{BitSet, SparseBitSetMatrix};
use crate::ir::mir::visit::ExpressionVisit;
use crate::ir::{StatementId, VariableId};
use crate::mir::Mir;
use crate::mir::Statement;
use std::mem::take;

#[derive(Debug, Clone)]
pub struct UseDefGraph {
    pub stmt_use_def_chains: SparseBitSetMatrix<StatementId, StatementId>,
    pub terminator_use_def_chains: SparseBitSetMatrix<BasicBlockId, StatementId>,
    pub assignments: SparseBitSetMatrix<VariableId, StatementId>,
}

impl UseDefGraph {
    pub fn new(mir: &Mir, cfg: &ControlFlowGraph) -> Self {
        let statement_count = mir.statements.len_idx();
        let var_count = mir.variables.len_idx();

        Self {
            stmt_use_def_chains: SparseBitSetMatrix::new_empty(statement_count, statement_count),
            terminator_use_def_chains: SparseBitSetMatrix::new_empty(
                cfg.blocks.len_idx(),
                statement_count,
            ),
            assignments: SparseBitSetMatrix::new_empty(var_count, statement_count),
        }
    }

    #[must_use]
    pub fn stmt_len_idx(&self) -> StatementId {
        self.stmt_use_def_chains.x_len_idx()
    }
}

pub struct ReachingDefinitionsAnalysis<'lt> {
    graph: UseDefGraph,
    mir: &'lt Mir,
}

impl<'lt> ReachingDefinitionsAnalysis<'lt> {
    pub fn new(mir: &'lt Mir, cfg: &ControlFlowGraph) -> Self {
        let mut graph = UseDefGraph::new(mir, cfg);

        for (id, stmt) in mir.statements.iter_enumerated() {
            match stmt {
                Statement::Assignment(_, var, _) => graph.assignments.insert(*var, id),

                Statement::Contribute(_, _, _, _) | Statement::StopTask(_, _) => (),
            }
        }

        Self { graph, mir }
    }

    pub fn run(mut self, cfg: &ControlFlowGraph) -> UseDefGraph {
        let mut genkill_engine = GenKillEngine::new(cfg, &mut self);
        let engine = Engine::new(cfg, &mut genkill_engine);
        let mut dfg = engine.iterate_to_fixpoint();

        for (id, bb) in cfg.blocks.iter_enumerated() {
            // reusing the in set because we dont need it afterwards anyway
            let mut reachable = take(&mut dfg.in_sets[id]);

            // reusing the out set because we dont need it afterwards anyway
            let mut tmp = take(&mut dfg.out_sets[id]);

            for stmt in bb.statements.iter().copied() {
                match self.mir[stmt] {
                    Statement::Assignment(_, var, expr) => {
                        tmp.clear();

                        UseDefBuilder {
                            mir: self.mir,
                            graph: &mut self.graph,
                            dst: &mut tmp,
                        }
                        .visit_expr(expr);

                        tmp.intersect_with(&reachable);

                        self.graph.stmt_use_def_chains[stmt] = tmp.to_hybrid();

                        reachable.difference_with(&self.graph.assignments[var]);
                        reachable.insert(stmt);
                    }

                    Statement::Contribute(_, _, _, val) => {
                        tmp.clear();
                        UseDefBuilder {
                            mir: self.mir,
                            graph: &mut self.graph,
                            dst: &mut tmp,
                        }
                        .visit_real_expr(val);

                        tmp.intersect_with(&reachable);
                        self.graph.stmt_use_def_chains[stmt] = tmp.to_hybrid();

                        // branches are currently not tracked
                    }
                    Statement::StopTask(_, _) => {}
                }
            }

            if let Terminator::Split { condition, .. } = bb.terminator {
                tmp.clear();

                UseDefBuilder {
                    mir: self.mir,
                    graph: &mut self.graph,
                    dst: &mut tmp,
                }
                .visit_integer_expr(condition);

                tmp.intersect_with(&reachable);
                self.graph.terminator_use_def_chains[id] = tmp.to_hybrid()
            }
        }

        self.graph
    }
}

impl<'lt> GenKillAnalysis<'_> for ReachingDefinitionsAnalysis<'lt> {
    type SetType = StatementId;
    type Direction = Forward;

    fn transfer_function(
        &mut self,
        gen_kill_set: &mut GenKillSet<StatementId>,
        basic_bock: BasicBlockId,
        cfg: &ControlFlowGraph,
    ) {
        for stmt in cfg[basic_bock].statements.iter().copied() {
            match self.mir[stmt] {
                Statement::Assignment(_, var, _) => {
                    gen_kill_set.kill_all(&self.graph.assignments[var]);
                    gen_kill_set.gen(stmt);
                }

                // branches are currently not tracked
                Statement::Contribute(_, _, _, _) | Statement::StopTask(_, _) => {}
            }
        }
    }

    fn max_idx(&self) -> Self::SetType {
        self.graph.stmt_len_idx()
    }
}

struct UseDefBuilder<'lt> {
    mir: &'lt Mir,
    graph: &'lt UseDefGraph,
    dst: &'lt mut BitSet<StatementId>,
}

impl<'lt> ExpressionVisit for UseDefBuilder<'lt> {
    fn mir(&self) -> &Mir {
        self.mir
    }

    fn visit_variable_reference(&mut self, var: VariableId) {
        self.dst.union_with(&self.graph.assignments[var]);
    }
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
        type Node = BasicBlockId;
        type Edge = (FlowOrDependence, BasicBlockId, BasicBlockId);

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
            &(edge_type, start, dst): &(FlowOrDependence, BasicBlockId, BasicBlockId),
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
        type Node = BasicBlockId;
        type Edge = (FlowOrDependence, BasicBlockId, BasicBlockId);

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
