/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */
use crate::ast::{Branch, BranchAccess, BranchDeclaration, NatureAccess, Reference};
use crate::parser::error::Result;
use crate::parser::lexer::Token;
use crate::parser::Parser;

impl Parser {
    pub fn parse_branch_declaration(&mut self) -> Result<Vec<BranchDeclaration>> {
        self.expect(Token::ParenOpen)?;
        let branch = self.parse_branch()?;
        self.expect(Token::ParenClose)?;
        let mut res = vec![BranchDeclaration {
            name: self.parse_identifier(false)?,
            branch,
        }];
        self.parse_list(
            |sel: &mut Self| {
                res.push(BranchDeclaration {
                    name: sel.parse_identifier(false)?,
                    branch,
                });
                Ok(())
            },
            Token::Semicolon,
            true,
        )?;
        Ok(res)
    }
    pub fn parse_branch(&mut self) -> Result<Branch> {
        if self.look_ahead()?.0 == Token::OpLess {
            self.lookahead.take();
            let res = Branch::Port(Reference::new(self.parse_hieraichal_identifier(false)?));
            self.expect(Token::OpGreater)?;
            Ok(res)
        } else {
            let first_net_name = self.parse_hieraichal_identifier(false)?;
            self.expect(Token::Comma)?;
            let second_net_name = self.parse_hieraichal_identifier(false)?;
            Ok(Branch::Nets(
                Reference::new(first_net_name),
                Reference::new(second_net_name),
            ))
        }
    }
    pub fn parse_branch_access(&mut self) -> Result<BranchAccess> {
        self.expect(Token::ParenOpen)?;

        let res = if self.look_ahead()?.0 == Token::OpLess {
            self.lookahead.take();
            let res = Branch::Port(Reference::new(self.parse_hieraichal_identifier(false)?));
            self.expect(Token::OpGreater)?;
            BranchAccess::Implicit(res)
        } else {
            let first_net_name_or_identifer = self.parse_hieraichal_identifier(false)?;
            if self.look_ahead().0 == Token::Comma {
                let second_net_name = self.parse_hieraichal_identifier(false)?;
                BranchAccess::Implicit(Branch::Nets(
                    Reference::new(first_net_name),
                    Reference::new(second_net_name),
                ))
            } else {
                BranchAccess::Explicit(Reference::new(first_net_name_or_identifer))
            }
        };

        self.expect(Token::ParenClose)?;
        Ok(res)
    }
}
