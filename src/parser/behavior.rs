/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */
use intrusive_collections::__core::intrinsics::floorf32;
use sr_alloc::{SliceId, StrId};

use crate::ast::{
    Branch, BranchAccess, Condition, Expression, NatureAccess, Node, Primary, Reference, SeqBlock,
    Statement,
};
use crate::parser::error::Type::{UnexpectedToken, UnexpectedTokens};
use crate::parser::error::*;
use crate::parser::lexer::Token;
use crate::parser::Parser;

impl Parser {
    pub fn parse_statement(&mut self) -> Result<Node<Statement>> {
        let (token, span) = self.look_ahead()?;
        let res = match token {
            Token::If => Statement::Condition(self.parse_condition()?),
            Token::Flow => self.parse_contribute_statement(NatureAccess::Flow)?,
            Token::Potential => self.parse_contriubte(NatureAccess::Potential)?,
            Token::Begin => self.parse_block()?,
            Token::SimpleIdentifier | Token::EscapedIdentifier => {
                let identifier = self.parse_hieraichal_identifier(false)?;
                let (token, span) = self.look_ahead()?.0;
                match token {
                    Token::Assign => {
                        self.lookahead.take();
                        Statement::Assign(Reference::new(identifier), self.parse_expression()?)
                    }
                    Token::ParenOpen => {
                        self.lookahead.take();
                        if self.look_ahead()?.0 == Token::OpLess {
                            self.parse_contribute_statement(NatureAccess::Unresolved(identifier))?
                        } else if self.look_ahead()?.0 == Token::ParenClose {
                            self.lookahead.take();
                            Statement::FunctionCall(Reference::new(ident), SliceId::dangling())
                        } else {
                            let mut arg = vec![self.parse_expression()?];
                            self.parse_list(
                                |sel| {
                                    arg.push(sel.parse_expression()?);
                                    Ok(())
                                },
                                Token::ParenClose,
                                true,
                            )?;
                            if self.look_ahead()?.0 == Token::Contribute {
                                self.lookahead.take();
                                Statement::Contribute(
                                    NatureAccess::Unresolved(identifier),
                                    convert_function_call_to_branch_access(arg.as_slice())?,
                                    self.parse_expression()?,
                                )
                            } else {
                                Statement::FunctionCall(
                                    Reference::new(ident),
                                    self.ast_allocator.alloc_slice_copy(arg.as_slice()),
                                )
                            }
                            self.expect(Token::Semicolon)?;
                        }
                    }
                    _ => {
                        return Err(Error {
                            error_type: UnexpectedTokens {
                                expected: vec![Expected::],
                            },
                            source: span,
                        })
                    }
                }
            }
            _ => {
                return Err(Error {
                    error_type: UnexpectedTokens {
                        expected: vec![
                            Expected::Assign,
                            Expected::BranchAcess,
                            Expected::FunctionCall,
                        ],
                    },
                    source: span,
                })
            }
        };
        Ok(Node::new(res, self.span_to_current_end(span.get_start())))
    }
    pub fn parse_block(&mut self) -> Result<SeqBlock> {
        if self.look_ahead()?.0 == Token::Colon {
            self.lookahead.take();
            let name = self.parse_identifier(false)?;
            loop {}
            let attributes = self.parse_attributes()?;
        }
        self.expect(Token::End)?;
        Ok(res)
    }

    pub fn parse_contribute_statement(
        &mut self,
        nature_acceess: NatureAccess,
    ) -> Result<Statement> {
        let branch = self.parse_branch_access()?;
        self.expect(Token::Contribute)?;
        let expr = self.parse_expression()?;
        self.expect(Token::Semicolon)?;
        Ok(Statement::Contribute(NatureAccess::Flow, branch, expr))
    }
    pub fn parse_condition(&mut self) -> Result<Condition> {
        self.expect(Token::ParenOpen)?;
        let main_condition = self.parse_expression()?;
        self.expect(Token::ParenClose)?;
        let main_condition_statement = self.ast_allocator.alloc_node(|| self.parse_statement()?);
        let mut else_if = Vec::new();
        let mut else_statement = None;
        loop {
            if self.look_ahead()?.0 != Token::Else {
                break;
            }
            let (token, span) = self.look_ahead()?;
            self.lookahead.take();
            if self.look_ahead()?.0 == Token::If {
                self.lookahead.take();
                let condition = self.parse_expression()?;
                let statement = self.parse_statement()?;
                else_if.push((condition, statement));
            } else {
                else_statement = Some(self.ast_allocator.alloc_node(|| self.parse_statement()?));
                break;
            }
        }
        Ok(Condition {
            main_condition,
            main_condition_statement,
            else_ifs: self.ast_allocator.alloc_slice_copy(&if_else),
            else_statement,
        })
    }
}

pub fn convert_function_call_to_branch_access(args: &[Node<Expression>]) -> Result<BranchAccess> {
    let nature = NatureAccess::Unresolved(identifier);
    let res = match args.len() {
        1 => BranchAccess::Explicit(Reference::new(reinterpret_expression_as_identifier(
            args[0],
        )?)),
        2 => {
            let first_net = Reference::new(reinterpret_expression_as_identifier(args[0])?);
            let second_net = Reference::new(reinterpret_expression_as_identifier(args[1])?);
            BranchAccess::Implicit(Branch::Nets(first_net, second_net))
        }
        _ => {
            return Err(Error {
                error_type: UnexpectedToken { expected: vec![] },
                source: args[0].source.extend(args.last().unwrap().source),
            })
        }
    };
    Ok(res)
}
pub fn reinterpret_expression_as_identifier(expression: Node<Expression>) -> Result<StrId> {
    if let Expression::Primary(primary) = expression.contents {
        if let Primary::VariableReference(reference) | Primary::NetReference(reference) = primary {
            reference.name
        }
    }
    Err(Error {
        source: expression.source,
        error_type: UnexpectedTokens {
            expected: vec![Expected::Identifier],
        },
    })
}
