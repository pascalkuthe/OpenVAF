//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

#[cfg(test)]
mod test;
#[macro_use]
mod util;
#[macro_use]
pub mod preprocessor;
pub mod syntax;
use crate::ast;
use crate::ast::{Node, RawAst};
use crate::error::SyntaxResult;
use crate::parsing::syntax::util::ParseTreeNode;
use crate::parsing::syntax::ParseTreeToRawAstFolder;

#[derive(Clone, PartialEq)]
pub struct Source {
    pub raw: String,
}

#[derive(Debug, Clone)]
pub struct ParseTree<'lt> {
    pub top_node: ParseTreeNode<'lt>,
}
impl<'lt> ParseTree<'lt> {
    /// Transforms the parse tree into a `RawAst`.
    /// *Note* The resulting `RawAst` is just a different representation of the original data
    /// The lifetime of this functions return value is therefor bounded to that of the `ParseTree` its called upon.
    pub fn fold_to_raw_ast(self) -> SyntaxResult<RawAst<'lt>> {
        let ast = {
            let mut arena = indextree::Arena::new();
            let top_node = arena.new_node(ast::RawNode {
                src: self.top_node.as_span(),
                node_info: Node::Top,
            });
            RawAst { arena, top_node }
        };
        let mut folder = ParseTreeToRawAstFolder {
            state_stack: Vec::new(),
            ast,
        };
        folder.fold(self)?;
        Ok(folder.collapse())
    }
}
