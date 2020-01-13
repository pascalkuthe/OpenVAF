use std::env::var;

use sr_alloc::StrId;

use crate::ast::{Discipline, Expression, Net, NetType, Reference, Variable, VariableType};
use crate::error::Error;
use crate::parser::error::Type::{UnexpectedToken, UnexpectedTokens};
use crate::parser::lexer::Token;
use crate::parser::Parser;
use crate::parser::Result;

impl Parser {
    pub fn parse_variable_declaration(
        &mut self,
        variable_type: VariableType,
    ) -> Result<Vec<Variable>> {
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
    ) -> Result<(StrId, Option<Expression>)> {
        let name = self.parse_identifier(false)?;
        let default_value = if self.look_ahead()?.0 == Token::Assign {
            self.lookahead.take();
            Some(self.parse_expression()?)
        } else {
            None
        };
        Ok((name, default_value))
    }
}
