/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::{AttributeNode, Attributes, ExpressionId, Push, Variable, VariableType};
use crate::parser::lexer::Token;
use crate::parser::Parser;
use crate::parser::Result;
use crate::symbol::Ident;
use crate::symbol_table::SymbolDeclaration;

impl<'lt, 'ast, 'astref, 'source_map> Parser<'lt, 'ast, 'astref, 'source_map> {
    pub fn parse_variable_declaration(
        &mut self,
        variable_type: VariableType,
        attributes: Attributes<'ast>,
    ) -> Result {
        let start = self.preprocessor.current_start();
        let (name, default_value) = self.parse_single_declaration_with_opt_default_value()?;
        let variable = self.ast.push(AttributeNode {
            attributes,
            source: self.span_to_current_end(start),
            contents: Variable {
                name,
                variable_type,
                default_value,
            },
        });
        self.insert_symbol(name, SymbolDeclaration::Variable(variable));

        self.parse_list(
            |sel| {
                let (name, default_value) =
                    sel.parse_single_declaration_with_opt_default_value()?;
                let variable = sel.ast.push(AttributeNode {
                    attributes,
                    source: sel.span_to_current_end(start),
                    contents: Variable {
                        name,
                        variable_type,
                        default_value,
                    },
                });
                sel.insert_symbol(name, SymbolDeclaration::Variable(variable));
                Ok(())
            },
            Token::Semicolon,
            true,
        )?;
        Ok(())
    }

    fn parse_single_declaration_with_opt_default_value(
        &mut self,
    ) -> Result<(Ident, Option<ExpressionId<'ast>>)> {
        let name = self.parse_identifier(false)?;
        let default_value = if self.look_ahead()?.0 == Token::Assign {
            self.lookahead.take();
            Some(self.parse_expression_id()?)
        } else {
            None
        };
        Ok((name, default_value))
    }
}
