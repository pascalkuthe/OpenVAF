/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */
use crate::ast::{AttributeNode, Attributes, Branch, BranchAccess, BranchDeclaration, Node};
use crate::parser::error::Result;
use crate::parser::lexer::Token;
use crate::parser::Parser;
use crate::symbol_table::SymbolDeclaration;
use crate::util::Push;

impl<'lt, 'ast, 'astref, 'source_map> Parser<'lt, 'ast, 'astref, 'source_map> {
    pub fn parse_branch_declaration(&mut self, attributes: Attributes<'ast>) -> Result {
        let start = self.preprocessor.current_start();
        self.expect(Token::ParenOpen)?;
        let branch = self.parse_branch()?;
        self.expect(Token::ParenClose)?;
        self.parse_list(
            |sel: &mut Self| {
                let name = sel.parse_identifier(false)?;
                let source = sel.span_to_current_end(start);
                let branch_decl = sel.ast.push(AttributeNode {
                    attributes,
                    contents: BranchDeclaration {
                        name,
                        branch: branch.clone(),
                    },
                    source,
                });
                sel.insert_symbol(
                    sel.ast[branch_decl].contents.name,
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
        if self.look_ahead()?.0 == Token::OpLess {
            self.lookahead.take();
            let res = Branch::Port(self.parse_hierarchical_identifier(false)?);
            self.expect(Token::OpGreater)?;
            Ok(res)
        } else {
            let first_net_name = self.parse_hierarchical_identifier(false)?;
            self.expect(Token::Comma)?;
            let second_net_name = self.parse_hierarchical_identifier(false)?;
            Ok(Branch::Nets(first_net_name, second_net_name))
        }
    }
    pub fn parse_branch_access(&mut self) -> Result<Node<BranchAccess>> {
        self.expect(Token::ParenOpen)?;
        let start = self.preprocessor.current_start();
        let res = if self.look_ahead()?.0 == Token::OpLess {
            self.lookahead.take();
            let res = Branch::Port(self.parse_hierarchical_identifier(false)?);
            self.expect(Token::OpGreater)?;
            BranchAccess::Implicit(res)
        } else {
            let first_net_name_or_identifer = self.parse_hierarchical_identifier(false)?;
            if self.look_ahead()?.0 == Token::Comma {
                let second_net_name = self.parse_hierarchical_identifier(false)?;
                BranchAccess::Implicit(Branch::Nets(first_net_name_or_identifer, second_net_name))
            } else {
                BranchAccess::Explicit(first_net_name_or_identifer)
            }
        };

        self.expect(Token::ParenClose)?;
        Ok(Node::new(res, self.span_to_current_end(start)))
    }
}
