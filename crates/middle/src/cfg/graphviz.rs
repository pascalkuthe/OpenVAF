/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use super::*;
use data_structures::index_vec::Idx;
use rustc_ap_graphviz as dot;
use rustc_ap_graphviz::LabelText::{EscStr, LabelStr};
use rustc_ap_graphviz::{Edges, Id, LabelText, Nodes};
use std::borrow::Cow;
use std::io::Write;

impl<C: CfgFunctions> ControlFlowGraph<C> {
    pub fn render_as_graphviz<W: Write>(&self, write: &mut W) {
        dot::render(self, write).expect("Rendering failed")
    }
}

impl<'a, C: CfgFunctions> dot::Labeller<'a> for ControlFlowGraph<C> {
    type Node = BasicBlock;
    type Edge = (BasicBlock, BasicBlock);

    fn graph_id(&'a self) -> Id<'a> {
        dot::Id::new("ControlFlowGraph").unwrap()
    }

    fn node_id(&'a self, n: &Self::Node) -> Id<'a> {
        dot::Id::new(format!("BB_{}", n.index())).unwrap()
    }

    fn node_label(&'a self, &n: &Self::Node) -> LabelText<'a> {
        match self.blocks[n].terminator().kind {
            TerminatorKind::End => EscStr(Cow::Owned(format!(
                "BB_{}: {} Statements\n END",
                n.index(),
                self.blocks[n].statements.len(),
            ))),
            TerminatorKind::Goto(_) => LabelStr(Cow::Owned(format!(
                "BB_{}: {} Statements",
                n.index(),
                self.blocks[n].statements.len(),
            ))),
            TerminatorKind::Split { loop_head, .. } => LabelStr(Cow::Owned(format!(
                "BB_{}: {} Statements{}",
                n.index(),
                self.blocks[n].statements.len(),
                if loop_head { " (loop)" } else { "" },
            ))),
        }
    }
    fn edge_label(&'a self, &(start, dst): &(BasicBlock, BasicBlock)) -> LabelText<'a> {
        match self.blocks[start].terminator().kind {
            TerminatorKind::Goto(_) => LabelStr(Cow::Borrowed("GOTO")),
            TerminatorKind::End => LabelStr(Cow::Borrowed("ILLEGAL")),
            TerminatorKind::Split { true_block, false_block, .. } => {
                let true_or_false = if true_block == dst {
                    "TRUE"
                } else if false_block == dst {
                    "FALSE"
                } else {
                    "ILLEGAL"
                };
                LabelStr(Cow::Borrowed(true_or_false))
            }
        }
    }
}

impl<'a, C: CfgFunctions> dot::GraphWalk<'a> for ControlFlowGraph<C> {
    type Node = BasicBlock;
    type Edge = (BasicBlock, BasicBlock);

    fn nodes(&'a self) -> Nodes<'a, Self::Node> {
        Cow::Owned(self.blocks.indices().collect())
    }

    fn edges(&'a self) -> Edges<'a, Self::Edge> {
        let mut edges = Vec::new();
        for (id, bb) in self.blocks.iter_enumerated() {
            match bb.terminator().kind {
                TerminatorKind::Goto(dst) => edges.push((id, dst)),
                TerminatorKind::Split { true_block, false_block, .. } => {
                    edges.push((id, false_block));
                    edges.push((id, true_block));
                }
                TerminatorKind::End => (),
            }
        }
        Cow::Owned(edges)
    }

    fn source(&'a self, edge: &Self::Edge) -> Self::Node {
        edge.0
    }

    fn target(&'a self, edge: &Self::Edge) -> Self::Node {
        edge.1
    }
}
