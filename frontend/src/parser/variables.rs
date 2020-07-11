/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::{Variable, VariableType};
use crate::ir::ExpressionId;

use crate::ir::{AttributeNode, Attributes};
use crate::parser::Parser;
use crate::parser::Result;
use crate::parser::Token;
use crate::symbol::Ident;
use crate::symbol_table::SymbolDeclaration;

impl<'lt> Parser<'lt> {
    pub fn parse_variable_declaration(
        &mut self,
        variable_type: VariableType,
        attributes: Attributes,
    ) -> Result {
        let start = self.previous_span(1);
        self.parse_list(
            |parser| {
                let (name, default_value) =
                    parser.parse_single_declaration_with_opt_default_value()?;
                let variable = parser.ast.variables.push(AttributeNode {
                    attributes,
                    span: parser.span_to_current_end(start),
                    contents: Variable {
                        name,
                        variable_type,
                        default_value,
                    },
                });
                parser.insert_symbol(name, SymbolDeclaration::Variable(variable));
                Ok(())
            },
            Token::Semicolon,
            true,
        )?;
        Ok(())
    }

    fn parse_single_declaration_with_opt_default_value(
        &mut self,
    ) -> Result<(Ident, Option<ExpressionId>)> {
        let name = self.parse_identifier()?;
        let default_value = if self.look_ahead(0)?.0 == Token::Assign {
            self.consume(1);
            Some(self.parse_expression()?)
        } else {
            None
        };
        Ok((name, default_value))
    }
}
