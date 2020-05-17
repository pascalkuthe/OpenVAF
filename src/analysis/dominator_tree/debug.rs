//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use super::*;
use rustc_ap_graphviz as dot;
use rustc_ap_graphviz::LabelText::{EscStr, LabelStr};
use rustc_ap_graphviz::{Edges, Id, LabelText, Nodes};
use std::borrow::Cow;
use std::io::Write;

impl<'cfg> DominatorTree<'cfg> {
    pub fn render_to<W: Write>(&self, write: &mut W) {
        dot::render(self, write).expect("Rendering failed")
    }
}
impl<'a, 'cfg> dot::Labeller<'a> for DominatorTree<'cfg> {
    type Node = BasicBlockId<'cfg>;
    type Edge = (BasicBlockId<'cfg>, BasicBlockId<'cfg>);

    fn graph_id(&'a self) -> Id<'a> {
        dot::Id::new("DominatorTree").unwrap()
    }

    fn node_id(&'a self, n: &Self::Node) -> Id<'a> {
        dot::Id::new(format!("BB_{}", n.index())).unwrap()
    }

    fn edge_label(
        &'a self,
        &(start, dst): &(BasicBlockId<'cfg>, BasicBlockId<'cfg>),
    ) -> LabelText<'a> {
        match self[start] {
            DominatorTreeNode::Leaf(_) => LabelStr(Cow::Borrowed("ILLEGAL")),
            DominatorTreeNode::Root(branches) | DominatorTreeNode::Branch(_, branches) => {
                let true_or_false = if branches
                    .true_child()
                    .map_or(false, |(true_child, _)| true_child == dst)
                {
                    "TRUE"
                } else if branches
                    .false_child()
                    .map_or(false, |(false_child, _)| false_child == dst)
                {
                    "FALSE"
                } else if branches.main_child == dst {
                    "MERGE"
                } else {
                    "ILLEGAL"
                };
                LabelStr(Cow::Borrowed(true_or_false))
            }
        }
    }
}

impl<'a, 'cfg> dot::GraphWalk<'a> for DominatorTree<'cfg> {
    type Node = BasicBlockId<'cfg>;
    type Edge = (BasicBlockId<'cfg>, BasicBlockId<'cfg>);

    fn nodes(&'a self) -> Nodes<'a, Self::Node> {
        Cow::Owned(self.data.full_range().collect())
    }

    fn edges(&'a self) -> Edges<'a, Self::Edge> {
        let mut edges = Vec::new();
        for block in self.data.full_range() {
            match self[block] {
                DominatorTreeNode::Leaf(_) => (),
                DominatorTreeNode::Root(branches) | DominatorTreeNode::Branch(_, branches) => {
                    edges.push((block, branches.main_child));
                    if let Some((true_child, _)) = branches.true_child() {
                        edges.push((block, true_child));
                    }
                    if let Some((false_child, _)) = branches.false_child() {
                        edges.push((block, false_child));
                    }
                }
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
