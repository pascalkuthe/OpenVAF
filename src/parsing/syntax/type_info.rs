//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the rust_adms project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/rust_adms/blob/master/LICENSE.
//  *  No part of rust_adms, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************


use super::*;

impl ParseTreeToRawAstFolder {
    pub(super) fn process_range(&mut self, matched_parse_tree_node: ParseTreeNode) -> SyntaxResult<NodeId> {
        let range_node = self.ast.arena.new_node(ast::Node::Range);
        let mut description = matched_parse_tree_node.into_inner();
        range_node.append(
            self.process_constant_expression(description.next().unwrap())?,
            &mut self.ast.arena);
        range_node.append(
            self.process_constant_expression(description.next().unwrap())?,
            &mut self.ast.arena);
        Ok(range_node)
    }

    //Todo remove adding to parent
    pub(super) fn process_single_range(&mut self, matched_parse_tree_node: ParseTreeNode, parent_ast_node: NodeId) -> SyntaxResult {
        trace!("Processing range from {}", matched_parse_tree_node);
        let range_node = self.ast.arena.new_node(ast::Node::Range);
        parent_ast_node.append(range_node, &mut self.ast.arena);
        let mut description = matched_parse_tree_node.into_inner();
        range_node.append(
            self.process_constant_expression(description.next().unwrap())?
            , &mut self.ast.arena);
        Ok(())
    }

    pub(super) fn process_type(matched_pair: Pair<Rule>) -> node_types::VerilogType {
        let actual_pair =
            if let Rule::NET_TYPE = matched_pair.as_rule() {
                matched_pair.into_inner().next().unwrap()
            } else {
                matched_pair
            };
        match actual_pair.as_rule() {
            Rule::TOK_WREAL => node_types::VerilogType::WREAL,
            Rule::TOK_SUPPLY0 => node_types::VerilogType::SUPPLY0,
            Rule::TOK_SUPPLY1 => node_types::VerilogType::SUPPLY1,
            Rule::TOK_TRI => node_types::VerilogType::TRI,
            Rule::TOK_TRIAND => node_types::VerilogType::TRIAND,
            Rule::TOK_TRIOR => node_types::VerilogType::TRIOR,
            Rule::TOK_TRI0 => node_types::VerilogType::TRI0,
            Rule::TOK_TRI1 => node_types::VerilogType::TRI1,
            Rule::TOK_WIRE => node_types::VerilogType::WIRE,
            Rule::TOK_UWIRE => node_types::VerilogType::UWIRE,
            Rule::TOK_WAND => node_types::VerilogType::WAND,
            Rule::TOK_WOR => node_types::VerilogType::WOR,
            Rule::TOK_REG => node_types::VerilogType::REG,
            Rule::TOK_INTEGER => node_types::VerilogType::INTEGER,
            Rule::TOK_TIME => node_types::VerilogType::TIME,
            Rule::TOK_REAL => node_types::VerilogType::REAL,
            _ => unexpected_rule!(actual_pair),
        }
    }
}