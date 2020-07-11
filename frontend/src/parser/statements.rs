/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::VariableType::{INTEGER, REAL};
use crate::ast::{BlockScope, Condition, SeqBlock, Statement};
use crate::ir::ast::{CaseItem, Cases, ForLoop, WhileLoop};
use crate::ir::{AttributeNode, Attributes, BlockId, PrintOnFinish, StatementId, StopTaskKind};
use crate::ir::{DisplayTaskKind, Node};
use crate::parser::error::Error::{IllegalFinishNumber, MultipleDefaultDeclarations};
use crate::parser::error::{Expected, Result};
use crate::parser::Error::UnexpectedTokens;
use crate::parser::Parser;
use crate::parser::Token;
use crate::symbol::keywords;
use crate::symbol::Ident;
use crate::symbol_table::{SymbolDeclaration, SymbolTable};
use crate::util::format_list;
use std::convert::TryInto;
pub fn stmt_recover() -> impl FnMut(Token) -> bool {
    let mut depth = 0;
    move |token: Token| {
        match token {
            Token::Semicolon | Token::End if depth == 0 => return true,
            Token::Begin => depth += 1,
            Token::End => depth -= 1,
            _ => (),
        }
        false
    }
}

impl<'lt> Parser<'lt> {
    #[allow(clippy::too_many_lines)]
    pub fn parse_statement(&mut self, attributes: Attributes) -> Result<StatementId> {
        let (token, span) = self.look_ahead(0)?;
        let res = match token {
            Token::If => {
                self.consume(1);
                Statement::Condition(self.parse_condition(attributes)?)
            }

            Token::Case => {
                self.consume(1);
                match self.parse_bracketed_expression() {
                    Ok(expr) => {
                        let mut cases = Vec::new();
                        let mut default = None;
                        self.parse_and_recover_on_tokens(
                            &mut stmt_recover(),
                            Token::EndCase,
                            true,
                            true,
                            |parser| {
                                let (token, span) = parser.look_ahead(0)?;
                                if token == Token::Default {
                                    parser.consume(1);
                                    parser.try_expect(Token::Colon);

                                    let attributes = parser.parse_attributes()?;
                                    let stmt = if parser.look_ahead(0)?.0 == Token::Semicolon {
                                        None
                                    } else {
                                        Some(parser.parse_statement(attributes)?)
                                    };

                                    if let Some((_, old)) = default {
                                        parser
                                            .non_critical_errors
                                            .add(MultipleDefaultDeclarations { old, new: span })
                                    } else {
                                        default = Some((stmt, span));
                                    }
                                } else {
                                    cases.push(parser.parse_case_item()?);
                                }

                                Ok(true)
                            },
                        )?;

                        let span = self.span_to_current_end(span);
                        Statement::Case(AttributeNode {
                            attributes,
                            span,
                            contents: Cases {
                                expr,
                                cases,
                                default: default.and_then(|(default, _)| default),
                            },
                        })
                    }
                    Err(error) => {
                        self.non_critical_errors.add(error);
                        self.recover_on(Token::EndCase, |_| false, true, true)?;
                        Statement::Block(BlockId::from_raw_unchecked(0)) //placeholder
                    }
                }
            }

            Token::Flow => {
                self.consume(1);
                let res =
                    self.parse_contribute_statement(attributes, Ident::new(keywords::flow, span))?;
                self.try_expect(Token::Semicolon);
                res
            }

            Token::Potential => {
                self.consume(1);
                let res = self.parse_contribute_statement(
                    attributes,
                    Ident::new(keywords::potential, span),
                )?;
                self.try_expect(Token::Semicolon);
                res
            }

            Token::Begin => {
                self.consume(1);
                Statement::Block(self.parse_block(attributes)?)
            }

            Token::While => {
                self.consume(1);
                Statement::While(AttributeNode {
                    contents: self.parse_while_loop()?,
                    attributes,
                    span: self.span_to_current_end(span),
                })
            }

            Token::For => {
                self.consume(1);
                match self.parse_for_loop() {
                    Ok(contents) => Statement::For(AttributeNode {
                        contents,
                        span: self.span_to_current_end(span),
                        attributes,
                    }),

                    Err(error) => {
                        self.non_critical_errors.add(error);
                        self.recover_on(Token::ParenClose, |_| false, true, true)?;
                        Statement::Block(BlockId::from_raw_unchecked(0))
                    }
                }
            }

            //TODO parse info warn and error into distinct ast nodes
            Token::Strobe | Token::Display | Token::Info | Token::Warn | Token::Error => {
                self.consume(1);
                self.expect(Token::ParenOpen)?;
                let mut args = Vec::with_capacity(4);
                self.parse_list(
                    |parser| {
                        args.push(parser.parse_expression()?);
                        Ok(())
                    },
                    Token::ParenClose,
                    true,
                )?;
                self.try_expect(Token::Semicolon);
                Statement::DisplayTask(
                    AttributeNode {
                        contents: DisplayTaskKind::Convergence(true),
                        span,
                        attributes,
                    },
                    args,
                )
            }

            Token::Write => {
                self.consume(1);
                self.expect(Token::ParenOpen)?;
                let mut args = Vec::with_capacity(4);
                self.parse_list(
                    |parser| {
                        args.push(parser.parse_expression()?);
                        Ok(())
                    },
                    Token::ParenClose,
                    true,
                )?;
                self.try_expect(Token::Semicolon);
                Statement::DisplayTask(
                    AttributeNode {
                        contents: DisplayTaskKind::Convergence(false),
                        span,
                        attributes,
                    },
                    args,
                )
            }

            Token::Debug => {
                self.consume(1);
                self.expect(Token::ParenOpen)?;
                let mut args = Vec::with_capacity(4);
                self.parse_list(
                    |parser| {
                        args.push(parser.parse_expression()?);
                        Ok(())
                    },
                    Token::ParenClose,
                    true,
                )?;
                self.try_expect(Token::Semicolon);
                Statement::DisplayTask(
                    AttributeNode {
                        contents: DisplayTaskKind::Debug,
                        span,
                        attributes,
                    },
                    args,
                )
            }

            Token::Finish => {
                self.consume(1);
                let kind = if self.look_ahead(0)?.0 == Token::ParenOpen {
                    self.consume(1);
                    let arg = self.expect_int()?;
                    let kind: PrintOnFinish = match arg.try_into() {
                        Ok(kind) => kind,
                        Err(()) => {
                            self.non_critical_errors
                                .add(IllegalFinishNumber(arg, self.previous_span(1)));
                            PrintOnFinish::Location
                        }
                    };
                    self.try_expect(Token::ParenClose);
                    kind
                } else {
                    PrintOnFinish::Location
                };

                self.try_expect(Token::Semicolon);

                Statement::StopTask(
                    AttributeNode {
                        contents: StopTaskKind::Finish,
                        span,
                        attributes,
                    },
                    kind,
                )
            }

            Token::Stop => {
                self.consume(1);
                let kind = if self.look_ahead(0)?.0 == Token::ParenOpen {
                    self.consume(1);
                    let arg = self.expect_int()?;
                    let kind: PrintOnFinish = match arg.try_into() {
                        Ok(kind) => kind,
                        Err(()) => {
                            self.non_critical_errors
                                .add(IllegalFinishNumber(arg, self.previous_span(1)));
                            PrintOnFinish::Location
                        }
                    };
                    self.try_expect(Token::ParenClose);
                    kind
                } else {
                    PrintOnFinish::Location
                };

                Statement::StopTask(
                    AttributeNode {
                        contents: StopTaskKind::Stop,
                        span,
                        attributes,
                    },
                    kind,
                )
            }

            Token::Ident(name) => {
                let ident = Ident::new(name, span);
                self.consume(1);
                let res = match self.look_ahead(0)?.0 {
                    Token::ParenOpen => self.parse_contribute_statement(attributes, ident)?,

                    _ => {
                        let identifier = self.parse_hierarchical_identifier_with_start(ident)?;
                        self.expect(Token::Assign)?;
                        Statement::Assign(attributes, identifier, self.parse_expression()?)
                    }
                };
                self.try_expect(Token::Semicolon);
                res
            }

            _ => {
                self.consume(1);
                return Err(UnexpectedTokens {
                    expected: format_list(vec![Expected::Statement]),
                    span,
                });
            }
        };
        Ok(self.ast.statements.push(res))
    }

    pub const BLOCK_DEFAULT_STATEMENT_CAPACITY: usize = 64;
    pub const BLOCK_DEFAULT_SYMTABLE_SIZE: usize = 8;
    pub fn parse_block(&mut self, attributes: Attributes) -> Result<BlockId> {
        let start = self.previous_span(1);

        let scope = if self.look_ahead(0)?.0 == Token::Colon {
            self.consume(1);
            let name = self.parse_identifier()?;

            self.scope_stack.push(SymbolTable::with_capacity(
                Self::BLOCK_DEFAULT_SYMTABLE_SIZE,
            ));

            loop {
                let attributes = self.parse_attributes()?;
                let (token, _) = self.look_ahead(0)?;
                //TODO recovery
                match token {
                    Token::Integer | Token::Time => {
                        self.consume(1);
                        self.parse_variable_declaration(INTEGER, attributes)?;
                    }

                    Token::Real | Token::Realtime => {
                        self.consume(1);
                        self.parse_variable_declaration(REAL, attributes)?;
                    }

                    Token::Parameter => {
                        self.consume(1);
                        self.parse_parameter_decl(attributes)?;
                    }
                    _ => break,
                };
            }
            Some(name)
        } else {
            None
        };

        let mut statements = Vec::with_capacity(Self::BLOCK_DEFAULT_STATEMENT_CAPACITY);

        self.parse_and_recover_on_tokens(stmt_recover(), Token::End, true, true, |parser| {
            let attributes = parser.parse_attributes()?;
            statements.push(parser.parse_statement(attributes)?);
            Ok(true)
        })?;

        let scope = scope.map(|name| BlockScope {
            name,
            symbols: self.scope_stack.pop().unwrap(),
        });

        let res = self.ast.blocks.push(AttributeNode {
            attributes,
            span: self.span_to_current_end(start),
            contents: SeqBlock { scope, statements },
        });

        if let Some(BlockScope { name, .. }) = self.ast[res].contents.scope {
            self.insert_symbol(name, SymbolDeclaration::Block(res));
        }
        Ok(res)
    }

    pub fn parse_contribute_statement(
        &mut self,
        attributes: Attributes,
        nature_access: Ident,
    ) -> Result<Statement> {
        let branch = self.parse_branch_access()?;
        self.expect(Token::Contribute)?;
        let expr = self.parse_expression()?;
        Ok(Statement::Contribute(
            attributes,
            nature_access,
            branch,
            expr,
        ))
    }

    pub fn parse_case_item(&mut self) -> Result<Node<CaseItem>> {
        let first_value = self.parse_expression()?;
        let mut values = vec![first_value];
        self.parse_list_tail(
            |parser| {
                values.push(parser.parse_expression()?);
                Ok(())
            },
            Token::Colon,
            true,
        )?;

        let attributes = self.parse_attributes()?;
        let stmt = if self.look_ahead(0)?.0 == Token::Semicolon {
            self.consume(1);
            None
        } else {
            Some(self.parse_statement(attributes)?)
        };
        let span = self.ast[first_value].span.extend(self.previous_span(1));
        Ok(Node {
            span,
            contents: CaseItem { values, stmt },
        })
    }

    pub fn parse_condition(&mut self, attributes: Attributes) -> Result<AttributeNode<Condition>> {
        let start = self.previous_span(1);
        let condition = self.parse_bracketed_expression()?;
        let statement_attributes = self.parse_attributes()?;
        let if_statement = self.parse_statement(statement_attributes)?;

        let else_statement = if self.look_ahead(0)?.0 == Token::Else {
            self.consume(1);
            let attributes = self.parse_attributes()?;
            Some(self.parse_statement(attributes)?)
        } else {
            None
        };

        Ok(AttributeNode {
            attributes,
            span: self.span_to_current_end(start),
            contents: Condition {
                condition,
                if_statement,
                else_statement,
            },
        })
    }

    pub fn parse_while_loop(&mut self) -> Result<WhileLoop> {
        let condition = self.parse_bracketed_expression()?;
        let attributes = self.parse_attributes()?;
        let body = self.parse_statement(attributes)?;
        Ok(WhileLoop { condition, body })
    }

    pub fn parse_for_loop(&mut self) -> Result<ForLoop> {
        self.try_expect(Token::ParenOpen);
        let inital_var = self.parse_hierarchical_identifier()?;
        self.expect(Token::Assign)?;
        let inital_val = self.parse_expression()?;
        self.try_expect(Token::Semicolon);
        let condition = self.parse_expression()?;
        self.try_expect(Token::Semicolon);
        let increment_var = self.parse_hierarchical_identifier()?;
        self.expect(Token::Assign)?;
        let increment_val = self.recover_expression_on_bracket()?;
        let attributes = self.parse_attributes()?;
        let body = self.parse_statement(attributes)?;

        Ok(ForLoop {
            condition,
            initial_var: inital_var,
            initial_val: inital_val,
            increment_var,
            increment_val,
            body,
        })
    }
}
