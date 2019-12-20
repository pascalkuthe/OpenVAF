//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the rust_adms project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/rust_adms/blob/master/LICENSE.
//  *  No part of rust_adms, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use super::*;

impl<'lt> ParseTreeToRawAstFolder<'lt> {
    mk_fold_fn!(module (self,parse_tree_node,'lt,description) {
        let name = identifier_string(description.next().unwrap());
        debug!("Processing Module {} ",name);
        let ast_node = self.ast.arena.new_node(ast::RawNode{
            node_info: Node::Module(name),
            src:parse_tree_node.as_span()
        });
        if_rule!(let Some(parameter_list)=description.next() where Rule::PARAMETER_DECLARATION_LIST => {
            self.fold_list_items_with_attributes(parameter_list,ast_node)?
        });
        if_rule!(let Some(port_list)=description.next() where Rule::PORT_LIST|Rule::PORT_DECLARATION_LIST => {
            self.fold_list_items_with_attributes(port_list,ast_node)?
        });
        for module_item_or_port_declaration in description {
            self.fold_item_with_attributes(module_item_or_port_declaration, ast_node)?
        }
        Ok([ast_node])
    });

    mk_fold_fn!(analog(self,parse_tree_node,'lt,description){
        let node;
        self.state_stack.push(State::AnalogBehavior);
        let inital_or_behavior = description.next().unwrap();
        match inital_or_behavior.as_rule() {
            Rule::TOK_INITIAL => {
                node = self.ast.arena.new_node(ast::RawNode{
                    node_info : Node::Analog(true),
                    src:inital_or_behavior.as_span()
                });
                self.state_stack.push(State::INITIAL);
                debug!("Processing analog initial statement!");
                self.fold_item_with_attributes(description.next().unwrap(), node)?;
                self.state_stack.pop();
            },
            Rule::BEHAVIORAL_STMT => {
                node = self.ast.arena.new_node(ast::RawNode{
                    node_info : Node::Analog(false),
                    src:inital_or_behavior.as_span()
                });
                debug!("Processing analog statement");
                self.fold_item_with_attributes(inital_or_behavior, node)?;
            },
            _ => unexpected_rule!(inital_or_behavior),
        };
        self.state_stack.pop();
        Ok([node])
    });

    //TODO understand standard
    //TODO complety reimplement this
    pub(super) fn process_port(
        &mut self,
        parse_tree_node: ParseTreeNode<'lt>,
        parent_ast_node: NodeId,
        attributes: Vec<NodeId>,
    ) -> SyntaxResult {
        let span = parse_tree_node.as_span();
        let mut description = parse_tree_node.into_inner();
        if description.peek().is_none() {
            //EMPTY ports are allowed according to standard (they represent ports not connected to the interior)
            return Ok(());
        }
        if description.peek().unwrap().as_rule() == Rule::IDENTIFIER {
            unimplemented!("Weird port listing not yet supported");
        }

        description = description.next().unwrap().into_inner();
        let mut expression = description.next().unwrap().into_inner();
        if description.peek().is_some() {
            unimplemented!("Multiple references for single port not yet supported")
        }
        let identifer = identifier_string(expression.next().unwrap());
        if expression.peek().is_some() {
            unimplemented!("Port listing with range not yet supported")
        }
        let node = self.ast.arena.new_node(ast::RawNode {
            src: span,
            node_info: Node::ModPort(identifer),
        });
        parent_ast_node.append(node, &mut self.ast.arena);
        Ok(())
    }
}
