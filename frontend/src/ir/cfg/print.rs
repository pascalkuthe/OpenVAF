use super::*;
use rustc_ap_graphviz as dot;
use rustc_ap_graphviz::LabelText::{EscStr, LabelStr};
use rustc_ap_graphviz::{Edges, Id, LabelText, Nodes};
use std::borrow::Cow;
use std::io::Write;

impl ControlFlowGraph {
    pub fn render_to<W: Write>(&self, write: &mut W) {
        dot::render(self, write).expect("Rendering failed")
    }
}

impl<'a> dot::Labeller<'a> for ControlFlowGraph {
    type Node = BasicBlockId;
    type Edge = (BasicBlockId, BasicBlockId);

    fn graph_id(&'a self) -> Id<'a> {
        dot::Id::new("ControlFlowGraph").unwrap()
    }

    fn node_id(&'a self, n: &Self::Node) -> Id<'a> {
        dot::Id::new(format!("BB_{}", n.index())).unwrap()
    }

    fn node_label(&'a self, &n: &Self::Node) -> LabelText<'a> {
        match self.blocks[n].terminator {
            Terminator::End => EscStr(Cow::Owned(format!(
                "BB_{}: {} Statements\n END",
                n.index(),
                self.blocks[n].statements.len(),
            ))),
            Terminator::Goto(_) => LabelStr(Cow::Owned(format!(
                "BB_{}: {} Statements",
                n.index(),
                self.blocks[n].statements.len(),
            ))),
            Terminator::Split {
                condition, merge, ..
            } => LabelStr(Cow::Owned(format!(
                "BB_{}: {} Statements\nSplit at {:?}\nMerge at BB_{}",
                n.index(),
                self.blocks[n].statements.len(),
                condition,
                merge.index(),
            ))),
        }
    }
    fn edge_label(&'a self, &(start, dst): &(BasicBlockId, BasicBlockId)) -> LabelText<'a> {
        match self.blocks[start].terminator {
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
        }
    }
}

impl<'a> dot::GraphWalk<'a> for ControlFlowGraph {
    type Node = BasicBlockId;
    type Edge = (BasicBlockId, BasicBlockId);

    fn nodes(&'a self) -> Nodes<'a, Self::Node> {
        Cow::Owned(self.blocks.indices().collect())
    }

    fn edges(&'a self) -> Edges<'a, Self::Edge> {
        let mut edges = Vec::new();
        for (id, bb) in self.blocks.iter_enumerated() {
            match bb.terminator {
                Terminator::Goto(dst) => edges.push((id, dst)),
                Terminator::Split {
                    true_block,
                    false_block,
                    ..
                } => {
                    edges.push((id, false_block));
                    edges.push((id, true_block));
                }
                Terminator::End => (),
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
