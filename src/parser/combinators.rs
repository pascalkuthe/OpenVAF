/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::error::Error;
use crate::parser::error::Result;
use crate::parser::error::Type::{UnexpectedEof, UnexpectedToken};
use crate::parser::lexer::Token;
use crate::parser::Parser;

impl Parser {
    /// Combinator that parses a list delimited by a comma and terminated by [end].
    /// This function does not parse the first entry as this requires extra logic in some cases
    /// # Example
    /// ,x,y,z;
    pub fn parse_list<F>(&mut self, mut parse_list_item: F, end: Token, consume_end: bool) -> Result
    where
        F: FnMut(&mut Self) -> Result,
    {
        let start = self.preprocessor.current_start();
        loop {
            let (token, span) = if consume_end {
                self.next()?
            } else {
                self.look_ahead()?
            };
            match token {
                Token::EOF => {
                    return Err(Error {
                        error_type: UnexpectedEof {
                            expected: vec![end],
                        },
                        source: self.span_to_current_end(start),
                    })
                }
                Token::Comma => {
                    if !consume_end {
                        self.lookahead.take();
                    }
                    parse_list_item(self)?
                }
                token if end == token => return Ok(()),
                _ => {
                    return Err(Error {
                        source: span,
                        error_type: UnexpectedToken {
                            expected: vec![Token::Comma, end],
                        },
                    })
                }
            }
        }
    }
}
