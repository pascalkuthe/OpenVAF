//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::post_dominators;
use crate::IPDOM;
use log::trace;
use openvaf_data_structures::{HybridBitSet, SparseBitSetMatrix};
use openvaf_mir::cfg::BasicBlockId;
use openvaf_mir::cfg::ControlFlowGraph;
use openvaf_mir::cfg::Terminator;
use std::ops::{Index, IndexMut};

pub struct ControlDependenceGraph(pub SparseBitSetMatrix<BasicBlockId, BasicBlockId>);
impl ControlDependenceGraph {
    /// Calculates the Control Dependence Graph of a CFG.
    /// A basic block is control dependent on a statement if this statements decides whether the block is executed (for example an if statement).
    /// Statements that cause control flow are not represented as statements but as basic block terminators in the cfg.
    /// As such the control dependence Graph simply maps basic blocks to all basics block whos terminator affats whether a statements is executed
    pub fn from_cfg(cfg: &ControlFlowGraph) -> Self {
        Self::from_ipdom(cfg, &post_dominators(cfg))
    }

    /// Calculates the control dependence graph when `ipdom(bb)` has already been calculated
    ///# Note
    /// This is only a seperate funciton to avoid recalculating the post dominators in some cases
    /// If you dont want to reuse `ipdom` you should use [`from_cfg`](crate::ControlDependenceGraph::from_cfg)
    pub fn from_ipdom(cfg: &ControlFlowGraph, ipdom: &IPDOM) -> Self {
        let mut cdg = Self(SparseBitSetMatrix::new_empty(
            cfg.blocks.len_idx(),
            cfg.blocks.len_idx(),
        ));
        for (id, bb) in cfg.blocks.iter_enumerated() {
            // we only care about control dependencies on branches since end and goto are unconditional jumps
            if let Terminator::Split {
                true_block,
                false_block,
                merge,
                ..
            } = bb.terminator
            {
                cdg.propagate_control_dependence(&ipdom, true_block, id);
                // loops can not return in verilog as such the code afterwards is not control dependent upon the loop
                if merge != id {
                    cdg.propagate_control_dependence(&ipdom, false_block, id);
                }
            }
        }

        cdg
    }

    fn propagate_control_dependence(
        &mut self,
        ipdom: &IPDOM,
        mut from: BasicBlockId,
        to: BasicBlockId,
    ) {
        loop {
            trace!(
                "propgating control dependency on {:?} to {:?} until {:?}",
                to,
                from,
                ipdom[to]
            );

            self.0.insert(from, to);
            from = ipdom[from];
            if from == ipdom[to] {
                break;
            }
        }
    }
}

impl Index<BasicBlockId> for ControlDependenceGraph {
    type Output = Option<HybridBitSet<BasicBlockId>>;

    fn index(&self, index: BasicBlockId) -> &Self::Output {
        &self.0[index]
    }
}

impl IndexMut<BasicBlockId> for ControlDependenceGraph {
    fn index_mut(&mut self, index: BasicBlockId) -> &mut Self::Output {
        &mut self.0[index]
    }
}

#[cfg(feature = "graph_debug")]
mod print {

    use super::*;
    use rustc_ap_graphviz as dot;
    use rustc_ap_graphviz::LabelText::{EscStr, LabelStr};
    use rustc_ap_graphviz::{Edges, Id, LabelText, Nodes};
    use std::borrow::Cow;
    use std::io::Write;

    impl ControlDependenceGraph {
        pub fn render_control_dependence_to<W: Write>(
            &self,
            write: &mut W,
            cfg: &ControlFlowGraph,
        ) {
            dot::render(&ControlDependenceCFG(cfg, self), write).expect("Rendering failed")
        }
    }

    struct ControlDependenceCFG<'lt>(&'lt ControlFlowGraph, &'lt ControlDependenceGraph);
    #[derive(Copy, Clone, Eq, PartialEq)]
    enum FlowOrDependence {
        Flow,
        Dependence,
    }

    impl<'a> dot::Labeller<'a> for ControlDependenceCFG<'a> {
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

                FlowOrDependence::Dependence => LabelStr(Cow::Borrowed("Control dependence")),
            }
        }
    }

    impl<'a> dot::GraphWalk<'a> for ControlDependenceCFG<'a> {
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
            for (src, control_dependencies) in self.1.iter_enumerated() {
                for dst in control_dependencies.ones() {
                    edges.push((FlowOrDependence::Dependence, src, dst));
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
