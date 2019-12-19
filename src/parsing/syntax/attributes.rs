//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the rust_adms project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/rust_adms/blob/master/LICENSE.
//  *  No part of rust_adms, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use std::collections::HashMap;

use super::*;

impl ParseTreeToRawAstFolder {
    pub(super) fn process_attributes(&mut self, parse_tree_nodes: &mut Pairs<Rule>) -> SyntaxResult<Vec<NodeId>> {
        let mut attributes: HashMap<String, Option<ParseTreeNode>> = HashMap::new();
        for_rules!(attribute_list in parse_tree_nodes where Rule::ATTRIBUTE => {
            for attribute in attribute_list.into_inner() {
                let mut description = attribute.clone().into_inner();
                let identifier = as_string!(description.next().unwrap());
                //Overwrite if attribute already declared
                if let Some (overwritten) = attributes.insert(identifier.clone(), description.next()) {
                    warn!("{}",error_message::<Rule>(&format!("Attribute {} is overwritten here. Old value: {:?}",identifier,overwritten),attribute.as_span()));
                }
            }
        });
        let res = Vec::new();
        for attribute in attributes {
            let value =
                if let Some(expr) = attribute.1 {
                    self.process_constant_expression(expr)?
                } else {
                    self.ast.arena.new_node(ast::Node::IntegerValue(1))
                };
            debug!("Processing Attribute {} with value {}", attribute.0, value);
            let node = self.ast.arena.new_node(ast::Node::Attribute(attribute.0));
            node.append(value, &mut self.ast.arena)
        }
        Ok(res)
    }

    pub(super) fn append_attributes(&mut self, parent: NodeId, attributes: &[NodeId]) {
        for attribute in attributes {
            parent.append(*attribute, &mut self.ast.arena);
        }
    }
}