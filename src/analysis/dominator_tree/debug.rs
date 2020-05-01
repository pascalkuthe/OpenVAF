//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use super::*;
use rustc_ap_graphviz as dot;
use rustc_ap_graphviz::LabelText::{EscStr, LabelStr};
use rustc_ap_graphviz::{Edges, Id, LabelText, Nodes};
use std::borrow::Cow;
use std::io::Write;

impl<'tag, 'cfg> DominatorTree<'tag, 'cfg> {
    pub fn render_to<W: Write>(&self, write: &mut W) {
        dot::render(self, write).expect("Rendering failed")
    }
}
impl<'a, 'tag, 'cfg> dot::Labeller<'a> for DominatorTree<'tag, 'cfg> {
    type Node = DominatorTreeId<'tag>;
    type Edge = (DominatorTreeId<'tag>, DominatorTreeId<'tag>);

    fn graph_id(&'a self) -> Id<'a> {
        dot::Id::new("DominatorTree").unwrap()
    }

    fn node_id(&'a self, n: &Self::Node) -> Id<'a> {
        dot::Id::new(format!("BB_{}", n.index())).unwrap()
    }

    fn edge_label(
        &'a self,
        &(start, dst): &(DominatorTreeId<'tag>, DominatorTreeId<'tag>),
    ) -> LabelText<'a> {
        match self[start].node_type {
            DominatorTreeNodeType::Leaf(_) => LabelStr(Cow::Borrowed("ILLEGAL")),
            DominatorTreeNodeType::Root(branches) | DominatorTreeNodeType::Branch(_, branches) => {
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

impl<'a, 'tag, 'cfg> dot::GraphWalk<'a> for DominatorTree<'tag, 'cfg> {
    type Node = DominatorTreeId<'tag>;
    type Edge = (DominatorTreeId<'tag>, DominatorTreeId<'tag>);

    fn nodes(&'a self) -> Nodes<'a, Self::Node> {
        Cow::Owned(self.data.full_range().collect())
    }

    fn edges(&'a self) -> Edges<'a, Self::Edge> {
        let mut edges = Vec::new();
        for block in self.data.full_range() {
            match self[block].node_type {
                DominatorTreeNodeType::Leaf(_) => (),
                DominatorTreeNodeType::Root(branches)
                | DominatorTreeNodeType::Branch(_, branches) => {
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
