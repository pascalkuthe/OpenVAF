/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::parser::error::Error::UnexpectedEofExpecting;
use crate::parser::error::Result;
use crate::parser::Error::{MissingOrUnexpectedToken, Unrecoverable};
use crate::parser::{Parser, Token};
use crate::sourcemap::Span;
use crate::util::format_list;

impl<'lt> Parser<'lt> {
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
        self.parse_and_recover_on_tokens(
            |token| token == Token::Comma,
            end,
            consume_end,
            false,
            |parser| match parser.next()? {
                (Token::Comma, _) => parse_list_item(parser).map(|_| true),
                (_, span) => Err(MissingOrUnexpectedToken {
                    expected: format_list(vec![Token::Comma, end]),
                    expected_at: parser.previous_span(2),
                    span,
                }),
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
            self.non_critical_errors.add(error);
            if self.recover_on(end, |token| token == Token::Comma, consume_end, false)? {
                return Ok(());
            }
        }
        self.parse_list_tail(parse_list_item, end, consume_end)?;
        Ok(())
    }

    #[inline]
    pub fn parse_and_recover_on_tokens<F, R>(
        &mut self,
        mut recover: R,
        end: Token,
        consume_end: bool,
        consume_recover: bool,
        mut parse: F,
    ) -> Result
    where
        F: FnMut(&mut Self) -> Result<bool>,
        R: FnMut(Token) -> bool,
    {
        let mut recovered = true;
        let start = self.previous_span(1);

        loop {
            let error = match self.optional_look_ahead(0) {
                Some((token, _)) if token == end => {
                    if consume_end {
                        self.consume(1);
                    }
                    break;
                }

                None => {
                    self.unrecoverable = true;
                    return Err(UnexpectedEofExpecting {
                        expected: end,
                        span: start.extend(self.last_span()),
                    });
                }

                Some(_) => match parse(self) {
                    Ok(false) => break,
                    Ok(true) => {
                        recovered = true;
                        continue;
                    }
                    Err(error) => error,
                },
            };

            if recovered {
                self.non_critical_errors.add(error);
                recovered = false;
            }

            if self.recover_on(end, &mut recover, consume_end, consume_recover)? {
                break;
            }
        }

        Ok(())
    }

    #[inline]
    pub fn recover_on(
        &mut self,
        end: Token,
        mut recover: impl FnMut(Token) -> bool,
        consume_end: bool,
        consume_recover: bool,
    ) -> Result<bool> {
        let start = self.previous_span(1);

        if self.unrecoverable {
            return Err(Unrecoverable);
        }

        loop {
            match self.optional_look_ahead(0) {
                Some((token, _)) if token == end => {
                    if consume_end {
                        self.consume(1);
                    }
                    return Ok(true);
                }

                Some((token, _)) if recover(token) => {
                    if consume_recover {
                        self.consume(1);
                    }
                    return Ok(false);
                }

                None => {
                    self.consume(1);
                    self.unrecoverable = true;
                    return Err(UnexpectedEofExpecting {
                        expected: end,
                        span: start.extend(self.last_span()),
                    });
                }

                Some(_) => {
                    self.consume(1);
                }
            }
        }
    }
    #[inline]
    pub fn panic_to_end(&mut self) -> Span {
        let start = self.previous_span(1);
        self.unrecoverable = true;
        self.position = self.src.len();
        self.span_to_current_end(start)
    }
}
macro_rules! synchronize {
    ($parser:expr; $($production:stmt;),* sync $gen_token:expr => {$($token:path => $parse:expr,)+}) => {
        'mainloop: loop {
            if $parser.src.len() <= $parser.position{
                break
            }
            let mut try_parse = || {
                $($production),*
                let (token,span) = $gen_token?;
                match token {
                    $($token => $parse?,)+
                    _ => return Err(UnexpectedToken {
                                expected: format_list(vec![$($token),+]),
                                span
                            }),
                }
                Ok(false)
            };
            let error = match try_parse() {
                Ok(true) => break 'mainloop,
                Ok(false) => continue,
                Err(error) => error,
            };
            $parser.non_critical_errors.add(error);
            loop {
                match $parser.optional_look_ahead(0) {
                    $(Some(($token,_)) => break,)+
                    Some(_) => {
                        $parser.consume(1);
                    }
                    None => {
                        $parser.consume(1);
                        break 'mainloop
                    }
                }
            }
        }
    };
}
