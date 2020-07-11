/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */
use crate::ast::{Branch, BranchAccess, BranchDeclaration};

use crate::ir::{AttributeNode, Attributes, Node};
use crate::parser::error::Result;
use crate::parser::Error::UnexpectedToken;
use crate::parser::Parser;
use crate::parser::Token;
use crate::symbol_table::SymbolDeclaration;
use crate::util::format_list;

impl<'lt> Parser<'lt> {
    pub fn parse_branch_declaration(&mut self, attributes: Attributes) -> Result {
        let start = self.previous_span(1);
        self.expect(Token::ParenOpen)?;
        let branch = self.parse_branch()?;
        self.expect(Token::ParenClose)?;
        self.parse_list(
            |parser| {
                let name = parser.parse_identifier()?;
                let source = parser.span_to_current_end(start);
                let branch_decl = parser.ast.branches.push(AttributeNode {
                    attributes,
                    contents: BranchDeclaration {
                        name,
                        branch: branch.clone(),
                    },
                    span: source,
                });
                parser.insert_symbol(
                    parser.ast[branch_decl].contents.name,
                    SymbolDeclaration::Branch(branch_decl),
                );
                Ok(())
            },
            Token::Semicolon,
            true,
        )?;
        Ok(())
    }

    pub fn parse_branch(&mut self) -> Result<Branch> {
        if self.look_ahead(0)?.0 == Token::OpLess {
            self.consume(1);

            let res = Branch::Port(self.parse_hierarchical_identifier()?);

            self.expect(Token::OpGreater)?;
            Ok(res)
        } else {
            let first_net_name = self.parse_hierarchical_identifier()?;

            let (token, span) = self.look_ahead(0)?;
            match token {
                Token::Comma => {
                    self.consume(1);

                    let second_net_name = self.parse_hierarchical_identifier()?;

                    Ok(Branch::Nets(first_net_name, second_net_name))
                }
                Token::ParenClose => Ok(Branch::NetToGround(first_net_name)),
                _ => Err(UnexpectedToken {
                    expected: format_list(vec![Token::Comma]),
                    span,
                }),
            }
        }
    }

    pub fn parse_branch_access(&mut self) -> Result<Node<BranchAccess>> {
        let span = self.look_ahead(0)?.1;
        self.expect(Token::ParenOpen)?;

        let res = if self.look_ahead(0)?.0 == Token::OpLess {
            self.consume(1);

            let res = Branch::Port(self.parse_hierarchical_identifier()?);

            self.expect(Token::OpGreater)?;

            BranchAccess::Implicit(res)
        } else {
            let first_net_name_or_identifer = self.parse_hierarchical_identifier()?;

            if self.look_ahead(0)?.0 == Token::Comma {
                self.consume(1);
                let second_net_name = self.parse_hierarchical_identifier()?;
                BranchAccess::Implicit(Branch::Nets(first_net_name_or_identifer, second_net_name))
            } else {
                BranchAccess::BranchOrNodePotential(first_net_name_or_identifer)
            }
        };

        self.expect(Token::ParenClose)?;
        Ok(Node::new(res, self.span_to_current_end(span)))
    }
}
