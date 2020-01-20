/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::{Expression, Node, Variable, VariableType};
use crate::parser::lexer::Token;
use crate::parser::Parser;
use crate::parser::Result;
use crate::symbol::Ident;

impl<'source_map, 'ast> Parser<'source_map, 'ast> {
    pub fn parse_variable_declaration(
        &mut self,
        variable_type: VariableType,
    ) -> Result<Vec<Variable<'ast>>> {
        let (name, default_value) = self.parse_single_declaration_with_opt_default_value()?;

        let mut res = vec![Variable {
            name,
            default_value,
            variable_type,
        }];

        self.parse_list(
            |sel| {
                let (name, default_value) =
                    sel.parse_single_declaration_with_opt_default_value()?;
                res.push(Variable {
                    name,
                    default_value,
                    variable_type,
                });
                Ok(())
            },
            Token::Semicolon,
            true,
        )?;
        Ok(res)
    }

    fn parse_single_declaration_with_opt_default_value(
        &mut self,
    ) -> Result<(Ident, Option<&'ast Node<Expression<'ast>>>)> {
        let name = self.parse_identifier(false)?;
        let default_value = if self.look_ahead()?.0 == Token::Assign {
            self.lookahead.take();
            Some(&*self.ast_allocator.alloc(self.parse_expression()?))
        } else {
            None
        };
        Ok((name, default_value))
    }
}
