/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::error::Error;
use crate::parser::error::Result;
use crate::parser::error::Type::{UnexpectedEof, UnexpectedToken, Unrecoverable};
use crate::parser::lexer::Token;
use crate::parser::{error, Parser};
use crate::Span;

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
        self.parse_and_recover_on_tokens(
            Token::Comma,
            end,
            consume_end,
            false,
            |parser, token| match token {
                Token::Comma => {
                    parser.lookahead.take();
                    parse_list_item(parser)
                }
                _ => {
                    parser.lookahead.take();
                    Err(Error {
                        source: parser.preprocessor.span(),
                        error_type: UnexpectedToken {
                            expected: vec![Token::Comma, end],
                        },
                    })
                }
            },
        )?;
        Ok(())
    }

    /// Combinator that parses a list delimited by a comma and terminated by `end`.
    /// # Example
    /// x,y,z;
    #[inline]
    pub fn parse_list<F>(&mut self, mut parse_list_item: F, end: Token, consume_end: bool) -> Result
    where
        F: FnMut(&mut Self) -> Result,
    {
        if let Err(error) = parse_list_item(self) {
            self.non_critical_errors.push(error);
            if self.recover_on(Token::Comma, end, consume_end, false)? {
                return Ok(());
            }
        }
        self.parse_list_tail(parse_list_item, end, consume_end)?;
        Ok(())
    }

    #[inline]
    pub fn parse_and_recover_on_tokens<F>(
        &mut self,
        recover: Token,
        end: Token,
        consume_end: bool,
        consume_recover: bool,

        mut parse: F,
    ) -> Result
    where
        F: FnMut(&mut Self, Token) -> Result,
    {
        loop {
            let error = match self.look_ahead() {
                Ok((token, _)) if token == end => {
                    if consume_end {
                        self.consume_lookahead();
                    }
                    break;
                }

                Ok((Token::EOF, source)) => {
                    self.unrecoverable = true;
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

            if self.recover_on(end, recover, consume_end, consume_recover)? {
                break;
            }
        }

        Ok(())
    }

    #[inline]
    pub fn recover_on(
        &mut self,
        end: Token,
        recover: Token,
        consume_end: bool,
        consume_recover: bool,
    ) -> Result<bool> {
        let start = self.preprocessor.current_start();
        if self.unrecoverable {
            return Err(Error {
                error_type: Unrecoverable,
                source: Span::new_short_empty_span(0),
            });
        }
        loop {
            match self.look_ahead() {
                Ok((token, _)) if token == end => {
                    if consume_end {
                        self.consume_lookahead();
                    }
                    return Ok(true);
                }

                Ok((token, _)) if token == recover => {
                    if consume_recover {
                        self.consume_lookahead();
                    }
                    return Ok(false);
                }

                Ok((Token::EOF, source)) => {
                    self.consume_lookahead();
                    self.unrecoverable = true;
                    return Err(Error {
                        error_type: UnexpectedEof {
                            expected: vec![end],
                        },
                        source: Span::new(start - 1, self.preprocessor.current_start() - 1),
                    });
                }

                Ok(_) => {
                    self.consume_lookahead();
                }

                Err(error) => {
                    self.consume_lookahead();
                    self.non_critical_errors.push(error);
                }
            }
        }
    }
    #[inline]
    pub fn unrecoverable_error(&mut self, spanned_error: error::Type) -> error::Error {
        let start = self.preprocessor.current_start();
        self.unrecoverable = true;
        loop {
            if let Ok((Token::EOF, source)) = self.next() {
                return Error {
                    error_type: spanned_error,
                    source: Span::new(start - 1, self.preprocessor.current_start() - 1),
                };
            }
        }
    }
}

macro_rules! synchronize {
    ($parser:expr; $($production:stmt;),* sync $gen_token:expr => {$end_token:path => end , $($token:path => $parse:expr,)+}) => {
        'mainloop: loop {
            let mut try_parse = || {
                $($production),*
                let (token,source) = $gen_token?;
                match token {
                    $end_token => return Ok(true),
                    $($token => $parse?,)+
                    _ => return Err(Error {
                            error_type: error::Type::UnexpectedToken {
                                expected: vec![$end_token $(,$token)+],
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
                    $(Ok(($token,_)) => break,)+
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
