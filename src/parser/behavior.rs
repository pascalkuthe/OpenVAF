/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::VariableType::{INTEGER, REAL};
use crate::ast::{
    BlockScope, Branch, BranchAccess, Condition, Expression, HierarchicalId, Primary, SeqBlock,
    Statement,
};
use crate::ir::ast::WhileLoop;
use crate::ir::DisplayTaskKind;
use crate::ir::{AttributeNode, Attributes, BlockId, Node, StatementId};
use crate::parser::error::Type::{
    HierarchicalIdNotAllowedAsNature, TooManyBranchArgs, UnexpectedTokens,
};
use crate::parser::error::*;
use crate::parser::lexer::Token;
use crate::parser::Parser;
use crate::symbol::keywords;
use crate::symbol::Ident;
use crate::symbol_table::{SymbolDeclaration, SymbolTable};

impl<'lt, 'source_map> Parser<'lt, 'source_map> {
    pub fn parse_statement(&mut self, attributes: Attributes) -> Result<StatementId> {
        let (token, span) = self.look_ahead_with_span()?;
        let res = match token {
            Token::If => {
                self.consume_lookahead();
                Statement::Condition(self.parse_condition(attributes)?)
            }

            Token::Flow => {
                self.consume_lookahead();
                self.parse_contribute_statement(attributes, Ident::new(keywords::FLOW, span))?
            }

            Token::Potential => {
                self.consume_lookahead();
                self.parse_contribute_statement(attributes, Ident::new(keywords::POTENTIAL, span))?
            }

            Token::Begin => {
                self.consume_lookahead();
                Statement::Block(self.parse_block(attributes)?)
            }

            Token::While => {
                self.consume_lookahead();
                let start = self.preprocessor.current_start();
                Statement::While(AttributeNode {
                    contents: self.parse_while_loop()?,
                    attributes,
                    source: self.span_to_current_end(start),
                })
            }

            Token::Strobe | Token::Display => {
                self.consume_lookahead();
                self.expect(Token::ParenOpen)?;
                let mut args = Vec::with_capacity(4);
                self.parse_list(
                    |parser| {
                        args.push(parser.parse_expression_id()?);
                        Ok(())
                    },
                    Token::ParenClose,
                    true,
                )?;
                self.try_expect(Token::Semicolon);
                Statement::DisplayTask(DisplayTaskKind::Convergence(true), args)
            }

            Token::Write => {
                self.consume_lookahead();
                self.expect(Token::ParenOpen)?;
                let mut args = Vec::with_capacity(4);
                self.parse_list(
                    |parser| {
                        args.push(parser.parse_expression_id()?);
                        Ok(())
                    },
                    Token::ParenClose,
                    true,
                )?;
                self.try_expect(Token::Semicolon);
                Statement::DisplayTask(DisplayTaskKind::Convergence(false), args)
            }

            Token::Debug => {
                self.consume_lookahead();
                self.expect(Token::ParenOpen)?;
                let mut args = Vec::with_capacity(4);
                self.parse_list(
                    |parser| {
                        args.push(parser.parse_expression_id()?);
                        Ok(())
                    },
                    Token::ParenClose,
                    true,
                )?;
                self.try_expect(Token::Semicolon);
                Statement::DisplayTask(DisplayTaskKind::Debug, args)
            }

            Token::SimpleIdentifier(_) | Token::EscapedIdentifier => {
                let identifier = self.parse_hierarchical_identifier()?;
                let (token, span) = self.look_ahead_with_span()?;
                let res = match token {
                    Token::Assign => {
                        self.consume_lookahead();
                        Statement::Assign(attributes, identifier, self.parse_expression_id()?)
                    }
                    Token::ParenOpen => {
                        self.consume_lookahead();
                        let (token, _) = self.look_ahead_with_span()?;
                        match token {
                            Token::OpLess => self.parse_contribute_statement(
                                attributes,
                                Self::convert_to_nature_identifier(identifier.names)?,
                            )?,
                            Token::ParenClose => {
                                self.consume_lookahead();
                                Statement::FunctionCall(attributes, identifier, Vec::new())
                            }
                            _ => {
                                let mut arg = vec![self.parse_expression()?];
                                self.parse_list_tail(
                                    |sel| {
                                        arg.push(sel.parse_expression()?);
                                        Ok(())
                                    },
                                    Token::ParenClose,
                                    true,
                                )?;
                                if self.look_ahead()? == Token::Contribute {
                                    self.consume_lookahead();
                                    Statement::Contribute(
                                        attributes,
                                        Self::convert_to_nature_identifier(identifier.names)?,
                                        self.convert_function_call_to_branch_access(arg),
                                        self.parse_expression_id()?,
                                    )
                                } else {
                                    Statement::FunctionCall(
                                        attributes,
                                        identifier,
                                        arg.into_iter()
                                            .map(|expr| self.ast.expressions.push(expr))
                                            .collect(),
                                    )
                                }
                            }
                        }
                    }

                    _ => {
                        self.consume_lookahead();
                        return Err(Error {
                            error_type: UnexpectedTokens {
                                expected: vec![Expected::Statement],
                            },
                            source: span,
                        });
                    }
                };
                self.try_expect(Token::Semicolon);
                res
            }

            _ => {
                self.consume_lookahead();
                return Err(Error {
                    error_type: UnexpectedTokens {
                        expected: vec![Expected::Statement],
                    },
                    source: span,
                });
            }
        };
        Ok(self.ast.statements.push(res))
    }

    /// Helper function that assert that a parsed hierarchical identifier is valid for acessing natures This is usued when it is not clear whether a nature or some other (hieraichal id) is parsed
    /// For example in the case of V(a) <+ a; V(a) the parser doesnt know yet whether its parsing a function call (in which M.V(a) would be valid) or a Branch acess (for which M.V would not be valid)
    /// When we hit the <+ statement this method is called to convert the types and assert that this is not an hierarchical id
    pub(crate) fn convert_to_nature_identifier(mut hierarchical_id: Vec<Ident>) -> Result<Ident> {
        if hierarchical_id.len() == 1 {
            Ok(hierarchical_id.pop().unwrap())
        } else {
            Err(Error {
                error_type: HierarchicalIdNotAllowedAsNature {
                    hierarchical_id: hierarchical_id.to_vec(),
                },
                source: hierarchical_id
                    .first()
                    .unwrap()
                    .span
                    .extend(hierarchical_id.last().unwrap().span),
            })
        }
    }

    pub const BLOCK_DEFAULT_STATEMENT_CAPACITY: usize = 64;
    pub const BLOCK_DEFAULT_SYMTABLE_SIZE: usize = 8;
    pub fn parse_block(&mut self, attributes: Attributes) -> Result<BlockId> {
        let start = self.preprocessor.current_start();

        let scope = if self.look_ahead()? == Token::Colon {
            self.consume_lookahead();
            let name = self.parse_identifier(false)?;
            self.scope_stack.push(SymbolTable::with_capacity_and_hasher(
                Self::BLOCK_DEFAULT_SYMTABLE_SIZE,
                Default::default(),
            ));
            loop {
                let attributes = self.parse_attributes()?;
                let token = self.look_ahead()?;
                //TODO recovery
                match token {
                    Token::Integer | Token::Time => {
                        self.lookahead.take();
                        self.parse_variable_declaration(INTEGER, attributes)?;
                    }

                    Token::Real | Token::Realtime => {
                        self.lookahead.take();
                        self.parse_variable_declaration(REAL, attributes)?;
                    }

                    Token::Parameter => {
                        self.lookahead.take();
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

        self.parse_and_recover_on_tokens(Token::Semicolon, Token::End, true, true, |parser| {
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
            source: self.span_to_current_end(start),
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
        nature_acceess: Ident,
    ) -> Result<Statement> {
        let branch = self.parse_branch_access()?;
        self.expect(Token::Contribute)?;
        let expr = self.parse_expression_id()?;
        self.try_expect(Token::Semicolon);
        Ok(Statement::Contribute(
            attributes,
            nature_acceess,
            branch,
            expr,
        ))
    }

    pub fn parse_condition(&mut self, attributes: Attributes) -> Result<AttributeNode<Condition>> {
        let start = self.preprocessor.current_start();
        let condition = self.parse_bracketed_expression()?;
        let condition = self.ast.expressions.push(condition);
        let statement_attributes = self.parse_attributes()?;
        let if_statement = self.parse_statement(statement_attributes)?;

        let else_statement = if self.look_ahead()? == Token::Else {
            self.lookahead.take();
            let attributes = self.parse_attributes()?;
            Some(self.parse_statement(attributes)?)
        } else {
            None
        };

        Ok(AttributeNode {
            attributes,
            source: self.span_to_current_end(start),
            contents: Condition {
                condition,
                if_statement,
                else_statement,
            },
        })
    }

    pub fn parse_while_loop(&mut self) -> Result<WhileLoop> {
        let condition = self.parse_bracketed_expression()?;
        let condition = self.ast.expressions.push(condition);
        let attributes = self.parse_attributes()?;
        let body = self.parse_statement(attributes)?;
        Ok(WhileLoop { condition, body })
    }

    pub fn convert_function_call_to_branch_access(
        &mut self,
        mut args: Vec<Node<Expression>>,
    ) -> Node<BranchAccess> {
        let span = args[0].source.extend(args.last().unwrap().source);
        // Really ugly at the moment hoping that i can move out of a vec at some point (probably with const generics)
        let res = match args.len() {
            0 => {
                self.non_critical_errors.push(Error {
                    error_type: UnexpectedTokens {
                        expected: vec![Expected::Statement],
                    },
                    source: self.preprocessor.span(),
                });
                BranchAccess::BranchOrNodePotential(HierarchicalId { names: vec![] })
            }
            1 => BranchAccess::BranchOrNodePotential(
                self.reinterpret_expression_as_identifier(args.pop().unwrap()),
            ),
            2 => {
                let second_net = self.reinterpret_expression_as_identifier(args.pop().unwrap());
                let first_net = self.reinterpret_expression_as_identifier(args.pop().unwrap());
                BranchAccess::Implicit(Branch::Nets(first_net, second_net))
            }
            3 => {
                self.non_critical_errors.push(Error {
                    error_type: TooManyBranchArgs(3),
                    source: args.pop().unwrap().source,
                });
                let second_net = self.reinterpret_expression_as_identifier(args.pop().unwrap());
                let first_net = self.reinterpret_expression_as_identifier(args.pop().unwrap());
                BranchAccess::Implicit(Branch::Nets(first_net, second_net))
            }
            len => {
                args.drain(5..);
                self.non_critical_errors.push(Error {
                    error_type: TooManyBranchArgs(len),
                    source: args
                        .pop()
                        .unwrap()
                        .source
                        .extend(args.pop().unwrap().source),
                });
                let second_net = self.reinterpret_expression_as_identifier(args.pop().unwrap());
                let first_net = self.reinterpret_expression_as_identifier(args.pop().unwrap());
                BranchAccess::Implicit(Branch::Nets(first_net, second_net))
            }
        };
        Node::new(res, span)
    }

    pub fn reinterpret_expression_as_identifier(
        &mut self,
        expression: Node<Expression>,
    ) -> HierarchicalId {
        if let Expression::Primary(Primary::VariableOrNetReference(name)) = expression.contents {
            name
        } else {
            self.non_critical_errors.push(Error {
                source: expression.source,
                error_type: UnexpectedTokens {
                    expected: vec![Expected::Identifier],
                },
            });
            HierarchicalId { names: vec![] }
        }
    }
}
