//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::analysis::dominator_tree::DominatorTreeNode;
use crate::analysis::DominatorTree;
use crate::ir::mir::control_flow_graph::BasicBlockId;

impl<'cfg> DominatorTree<'cfg> {
    pub fn postorder_cfg_visit(&self, mut f: impl FnMut(BasicBlockId<'cfg>)) {
        self.partial_postorder_cfg_visit(self.end, None, &mut f);
    }

    fn partial_postorder_cfg_visit(
        &self,
        start: BasicBlockId<'cfg>,
        end: Option<BasicBlockId<'cfg>>,
        f: &mut impl FnMut(BasicBlockId<'cfg>),
    ) {
        let mut current = start;
        loop {
            match self[current] {
                DominatorTreeNode::Leaf(parent) => (),
                DominatorTreeNode::Root(branch) | DominatorTreeNode::Branch(_, branch) => {
                    if let Some((start, end)) = branch.true_child() {
                        self.partial_postorder_cfg_visit(end, Some(current), f);
                    }
                    if let Some((start, end)) = branch.false_child() {
                        self.partial_postorder_cfg_visit(end, Some(current), f);
                    }
                }
            }

            f(current);

            current = match self[current].parent() {
                None => return,
                parent if parent == end => return,
                Some(parent) => parent,
            }
        }
    }
}
