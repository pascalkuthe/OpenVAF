//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the rust_adms project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/rust_adms/blob/master/LICENSE.
//  *  No part of rust_adms, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use std::collections::HashMap;

use super::*;
use crate::ast::Node;
use crate::error::*;
use pest::Span;

impl<'lt> ParseTreeToRawAstFolder<'lt> {
    pub(super) fn process_attributes(
        &mut self,
        parse_tree_nodes: &mut Pairs<'lt, Rule>,
    ) -> SyntaxResult<Vec<NodeId>> {
        let mut attributes: HashMap<String, (Option<ParseTreeNode>, Span)> = HashMap::new();
        for_rules!(attribute_list in parse_tree_nodes where Rule::ATTRIBUTE => {
            for attribute in attribute_list.into_inner() {
                let span = attribute.as_span();
                let mut description = attribute.into_inner();
                let identifier = as_string!(description.next().unwrap());
                //Overwrite if attribute already declared
                if let Some (overwritten) = attributes.insert(identifier.clone(),( description.next(),span.clone())) {
                    warn!("{}",error_message::<Rule>(&format!("Attribute {} is overwritten here. Old value: {:?}",identifier,overwritten),span));
                }
            }
        });
        let res = Vec::new();
        for (name, (node, span)) in attributes {
            let value = if let Some(expr) = node {
                self.process_constant_expression(expr)?
            } else {
                self.ast.arena.new_node(ast::RawNode {
                    src: span.clone(), //the span of the entire declaration is specifie here since if any errors occure here thats where they'd implicitly originate
                    node_info: Node::IntegerValue(1),
                })
            };
            debug!("Processing Attribute {} with value {}", name, value);
            let node = self.ast.arena.new_node(ast::RawNode {
                src: span,
                node_info: Node::Attribute(name),
            });
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
