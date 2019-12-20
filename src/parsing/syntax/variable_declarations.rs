//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the rust_adms project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/rust_adms/blob/master/LICENSE.
//  *  No part of rust_adms, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::error::error;

use super::*;

impl<'lt> ParseTreeToRawAstFolder<'lt> {
    mk_fold_fn!(variable_declaration(self,parse_tree_node,'lt,description){
        //TODO drive and charge strength
        let (variable_info, optional_range) = self.process_variable_info(&mut description)?;
        debug!("Variable with Info {:?} and Range {:?} and name"
               ,variable_info,optional_range);
        let node_creation = |name|->ast::RawNode {
            ast::RawNode{
            src:parse_tree_node.as_span(),
            node_info: Node::VariableOrWire{name,type_info:variable_info}
        }};
        self.process_variable_identifier_list(description,node_creation,optional_range)
    });

    mk_fold_fn!(port_declaration(self,parse_tree_node,'lt,description){
        let (mut is_input,mut is_output) = (false,false);
        let inout_declaration = description.next().unwrap();
        match inout_declaration.as_rule() {
            Rule::TOK_IN => {
                is_input = true;
            },
            Rule::TOK_OUT => {
                is_output = true;
            }
            Rule::TOK_INOUT => {
                is_output = true;
                is_input = true;
            }
            _ => unexpected_rule!(inout_declaration),
        }
        let (base_type,optional_range) = self.process_variable_info(&mut description)?;
        //TODO enforce that some port types aren't legal
        let port_info = ast::node_types::PORT{is_input,is_output,base_type};
        debug!("Port with Info {:?} and Range {:?} and name"
               ,port_info,optional_range);
        let node_creation = |name|->ast::RawNode{
            ast::RawNode{
                src:parse_tree_node.as_span(),
                node_info: Node::ModPortDec(name,port_info)
            }
        };
        self.process_variable_identifier_list(description,node_creation,optional_range)
    });

    mk_fold_fn!(parameter_declaration(self,parse_tree_node,'lt,description){
        rule_eq!(let local = description.next().unwrap(); Rule::TOK_LOCALPARAM , Rule::TOK_PARAMETER);
        let mut parameter_type = ast::node_types::SIGNED { verilog_type: ast::node_types::VerilogType::UNDECLARED, signed: false };
        let mut optional_range : Option<NodeId> = None;
        for_rules!(type_info_node in description where !Rule::PARAMETER_ASSIGNMENT_LIST => {
            self.process_signed_type_info(type_info_node,&mut parameter_type,&mut optional_range)?
        });
        debug!("Parameters with Info {:?} and Range {:?} and name"
               ,parameter_type,optional_range);
        let node_creation = |name|->ast::RawNode{
            ast::RawNode{
                src:parse_tree_node.as_span(),
                node_info:Node::Parameter{name,type_info:parameter_type,local}
            }
        };
        let mut parameter_assigment_list = description.next().unwrap().into_inner();
        let mut res = Vec::new();
        while let Some(current_node) = self.process_declaration_identifier(& mut parameter_assigment_list,node_creation.clone(),optional_range)? {
            let value = parameter_assigment_list.next().unwrap();
            debug!("with default value: {}",value.as_str());
            current_node.append(self.process_expression(value)?, &mut self.ast.arena);
            //TODO VALUE RANGE
            for_rules!(value_range in parameter_assigment_list where Rule::VALUE_RANGE => {
            });
            res.push(current_node);
        }
        Ok(res)
    });

    mk_fold_fn!(branch_declaration(self,parse_tree_node,'lt,description){
        //Resolve what the branch is describing
        let port_or_nets = description.next().unwrap();
        rule_eq!(let port_branch = port_or_nets; Rule::BRANCH_PORT_REFERENCE,Rule::BRANCH_NET_REFERENCE);
        let mut reference_description = port_or_nets.into_inner();
        let first_reference = self.process_reference_with_optional_range(&mut reference_description)?;
        //Port branches only refer to their port: (<port>) while normal branches are between two nets (net1,net2)
        let references =
            if reference_description.peek().is_some() {
               (first_reference, Some(self.process_reference_with_optional_range(&mut reference_description)?))
            }else{
                (first_reference,None)
            };
        //TODO document that order of wires is switched to avoid sign error
        let node_creation = |name|->ast::RawNode {
            ast::RawNode{
                src:parse_tree_node.as_span(),
                node_info:Node::Branch{name,port_branch}
            }
        };
        let mut identifier_list = description.next().unwrap().into_inner();
        let mut res = Vec::new();
        while let Some(current_node) = self.process_declaration_identifier(& mut identifier_list,node_creation,references.1)? {
            current_node.append(references.0,&mut self.ast.arena);
            res.push(current_node);
        }
        Ok(res)

    });

    fn process_reference_with_optional_range(
        &mut self,
        wire_reference_description: &mut Pairs<'lt, Rule>,
    ) -> SyntaxResult<NodeId> {
        let reference_node =
            self.process_hierarchical_id(wire_reference_description.next().unwrap())?;
        if_rule!(let Some(range) =  wire_reference_description.next() where Rule::SINGEL_RANGE => {
             self.process_single_range(wire_reference_description.next().unwrap(), reference_node)?;
        });
        Ok(reference_node)
    }

    fn process_variable_identifier_list<F>(
        &mut self,
        mut description: Pairs<'lt, Rule>,
        node_creation: F,
        optional_range: Option<NodeId>,
    ) -> SyntaxResult<Vec<NodeId>>
    where
        F: FnOnce(String) -> ast::RawNode<'lt> + Clone,
    {
        let mut identifier_list = description.next().unwrap().into_inner();
        let mut res = Vec::new();
        while let Some(current_node) = self.process_declaration_identifier(
            &mut identifier_list,
            node_creation.clone(),
            optional_range,
        )? {
            //TODO debug value
            if let Some(node) = self.process_optional_constant_expression(&mut identifier_list)? {
                current_node.append(node, &mut self.ast.arena);
            }
            res.push(current_node);
        }
        Ok(res)
    }

    fn process_declaration_identifier<F>(
        &mut self,
        description: &mut Pairs<Rule>,
        node_creation: F,
        optional_range: Option<NodeId>,
    ) -> SyntaxResult<Option<NodeId>>
    where
        F: FnOnce(String) -> ast::RawNode<'lt>,
    {
        if let Some(identifier_node) = description.next() {
            let name = identifier_string(identifier_node);
            debug!("{} declared", name);
            let node = self.ast.arena.new_node(node_creation(name));
            if let Some(range) = optional_range {
                node.append(range, &mut self.ast.arena);
            }
            Ok(Some(node))
        } else {
            Ok(None)
        }
    }
    fn process_variable_info(
        &mut self,
        description: &mut Pairs<'lt, Rule>,
    ) -> SyntaxResult<(ast::node_types::VARIABLE, Option<NodeId>)> {
        let mut variable_type = ast::node_types::VARIABLE::new();
        let mut optional_range: Option<NodeId> = None;
        for_rules!(type_info_node in description where !Rule::VARIABLE_IDENTIFIER_LIST|Rule::IDENTIFIER_LIST => {
           if type_info_node.as_rule() == Rule::IDENTIFIER {
               variable_type.discipline = identifier_string(type_info_node);
           }else{
               self.process_signed_type_info(type_info_node,&mut variable_type.basic_type,&mut optional_range)?;
           }
        });
        Ok((variable_type, optional_range))
    }

    fn process_signed_type_info(
        &mut self,
        type_info_node: ParseTreeNode<'lt>,
        type_info: &mut ast::node_types::SIGNED,
        optional_range: &mut Option<NodeId>,
    ) -> SyntaxResult {
        match type_info_node.as_rule() {
            Rule::TOK_SIGNED => type_info.signed = true,
            Rule::RANGE => {
                if optional_range.is_some() {
                    return error("Range may only be declared once", type_info_node.as_span());
                }
                *optional_range = Some(self.process_range(type_info_node)?)
            }
            _ => type_info.verilog_type = Self::process_type(type_info_node),
        }
        Ok(())
    }
}
