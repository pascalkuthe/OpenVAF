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

impl<'lt, 'ast, 'source_map> Parser<'lt, 'ast, 'source_map> {
    /// Combinator that parses a list delimited by a comma and terminated by `end`.
    /// This function does not parse the first entry as this requires extra logic in some cases
    /// # Examples
    /// ,x,y,z;

    #[inline]
    pub fn parse_list_tail<F>(
        &mut self,
        mut parse_list_item: F,
        end: Token,
        consume_end: bool,
    ) -> Result
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

    /// Combinator that parses a list delimited by a comma and terminated by `end`.
    /// # Example
    /// x,y,z;
    #[inline]
    pub fn parse_list<F>(&mut self, mut parse_list_item: F, end: Token, consume_end: bool) -> Result
    where
        F: FnMut(&mut Self) -> Result,
    {
        parse_list_item(self)?;
        self.parse_list_tail(parse_list_item, end, consume_end)
    }

    #[inline]
    pub fn parse_and_recover_on_tokens<F>(
        &mut self,
        recover: Token,
        end: Token,
        mut parse: F,
    ) -> Result
    where
        F: FnMut(&mut Self, Token) -> Result,
    {
        'mainloop: loop {
            let error = match self.look_ahead() {
                Ok((token, _)) if token == end => {
                    self.lookahead.take();
                    break 'mainloop;
                }

                Ok((Token::EOF, source)) => {
                    self.lookahead.take();
                    return Err(Error {
                        error_type: UnexpectedEof {
                            expected: vec![end],
                        },
                        source,
                    });
                }

                Ok((token, _)) => {
                    if let Err(error) = parse(self, token) {
                        error
                    } else {
                        continue;
                    }
                }

                Err(error) => error,
            };
            self.non_critical_errors.push(error);
            loop {
                match self.look_ahead() {
                    Ok((token, _)) if token == end => {
                        self.lookahead.take();
                        break 'mainloop;
                    }

                    Ok((token, _)) if token == recover => {
                        self.lookahead.take();
                        break;
                    }

                    Ok((Token::EOF, source)) => {
                        self.lookahead.take();
                        return Err(Error {
                            error_type: UnexpectedEof {
                                expected: vec![end],
                            },
                            source,
                        });
                    }

                    Ok(_) => {
                        self.lookahead.take();
                    }

                    Err(error) => {
                        self.lookahead.take();
                        self.non_critical_errors.push(error);
                    }
                }
            }
        }
        Ok(())
    }
}

macro_rules! synchronize {
    ($parser:expr; $($production:stmt;),* sync $gen_token:expr => {$end_token:path => end , $($token:path => $parse:expr,),+}) => {
        'mainloop: loop {
            let mut try_parse = || {
                $($production),*
                let (token,source) = $gen_token?;
                match token {
                    $end_token => return Ok(true),
                    $($token => $parse?,),+
                    _ => return Err(Error {
                            error_type: error::Type::UnexpectedToken {
                                expected: vec![$end_token $(,$token),+],
                            },
                            source,
                        }),
                }
                Ok(false)
            };
            let error = match try_parse() {
                Ok(true) => break 'mainloop,
                Ok(false) => continue,
                Err(error) => error,
            };
            $parser.non_critical_errors.push(error);
            loop {
                match $parser.look_ahead() {
                    $(Ok(($token,_)) => break,),+
                    Ok(($end_token,_)) => break 'mainloop,
                    Ok(_) => {
                        $parser.lookahead.take();
                    }
                    Err(error) => {
                        $parser.lookahead.take();
                        $parser.non_critical_errors.push(error);
                    }
                }
            }
        }
    };
}
