/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::{
    BlockScope, Branch, BranchAccess, Condition, Expression, HierarchicalId, Primary, SeqBlock,
    Statement, VariableType,
};
use crate::ir::ast::WhileLoop;
use crate::ir::Push;
use crate::ir::{AttributeNode, Attributes, BlockId, Node, StatementId};
use crate::parser::error::Type::{
    HierarchicalIdNotAllowedAsNature, UnexpectedToken, UnexpectedTokens,
};
use crate::parser::error::*;
use crate::parser::lexer::Token;
use crate::parser::Parser;
use crate::symbol::keywords;
use crate::symbol::Ident;
use crate::symbol_table::{SymbolDeclaration, SymbolTable};

impl<'lt, 'ast, 'source_map> Parser<'lt, 'ast, 'source_map> {
    pub fn parse_statement(&mut self, attributes: Attributes<'ast>) -> Result<StatementId<'ast>> {
        let (token, span) = self.look_ahead()?;
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

            Token::SimpleIdentifier(_) | Token::EscapedIdentifier => {
                let identifier = self.parse_hierarchical_identifier()?;
                let (token, span) = self.look_ahead()?;
                let res = match token {
                    Token::Assign => {
                        self.consume_lookahead();
                        Statement::Assign(attributes, identifier, self.parse_expression_id()?)
                    }
                    Token::ParenOpen => {
                        self.consume_lookahead();
                        let (token, _) = self.look_ahead()?;
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
                                if self.look_ahead()?.0 == Token::Contribute {
                                    self.consume_lookahead();
                                    Statement::Contribute(
                                        attributes,
                                        Self::convert_to_nature_identifier(identifier.names)?,
                                        convert_function_call_to_branch_access(arg)?,
                                        self.parse_expression_id()?,
                                    )
                                } else {
                                    Statement::FunctionCall(
                                        attributes,
                                        identifier,
                                        arg.into_iter().map(|expr| self.ast.push(expr)).collect(),
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
                self.expect(Token::Semicolon)?;
                res
            }

            _ => {
                self.consume_lookahead();
                return Err(Error {
                    error_type: UnexpectedTokens {
                        expected: vec![
                            Expected::Assign,
                            Expected::BranchAcess,
                            Expected::FunctionCall,
                        ],
                    },
                    source: span,
                });
            }
        };
        Ok(self.ast.push(res))
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
    pub fn parse_block(&mut self, attributes: Attributes<'ast>) -> Result<BlockId<'ast>> {
        let start = self.preprocessor.current_start();

        let scope = if self.look_ahead()?.0 == Token::Colon {
            self.consume_lookahead();
            let name = self.parse_identifier(false)?;
            self.scope_stack.push(SymbolTable::with_capacity_and_hasher(
                Self::BLOCK_DEFAULT_SYMTABLE_SIZE,
                Default::default(),
            ));
            loop {
                let attributes = self.parse_attributes()?;
                let token = self.look_ahead()?.0;
                match token {
                    Token::Integer => {
                        self.consume_lookahead();
                        self.parse_variable_declaration(VariableType::INTEGER, attributes)?
                    }
                    Token::Real => {
                        self.consume_lookahead();
                        self.parse_variable_declaration(VariableType::REAL, attributes)?
                    }
                    //TODO parameteres
                    _ => break,
                };
            }
            Some(name)
        } else {
            None
        };

        let mut statements = Vec::with_capacity(Self::BLOCK_DEFAULT_STATEMENT_CAPACITY);

        self.parse_and_recover_on_tokens(
            Token::Semicolon,
            Token::End,
            true,
            true,
            |parser, token| {
                let attributes = parser.parse_attributes()?;
                statements.push(parser.parse_statement(attributes)?);
                Ok(())
            },
        )?;
        let scope = scope.map(|name| BlockScope {
            name,
            symbols: self.scope_stack.pop().unwrap(),
        });
        let res = self.ast.push(AttributeNode {
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
        attributes: Attributes<'ast>,
        nature_acceess: Ident,
    ) -> Result<Statement<'ast>> {
        let branch = self.parse_branch_access()?;
        self.expect(Token::Contribute)?;
        let expr = self.parse_expression_id()?;
        self.expect(Token::Semicolon)?;
        Ok(Statement::Contribute(
            attributes,
            nature_acceess,
            branch,
            expr,
        ))
    }

    pub fn parse_condition(
        &mut self,
        attributes: Attributes<'ast>,
    ) -> Result<AttributeNode<'ast, Condition<'ast>>> {
        let start = self.preprocessor.current_start();
        self.expect(Token::ParenOpen)?;
        let main_condition = self.parse_expression_id()?;
        self.expect(Token::ParenClose)?;
        let statement_attributes = self.parse_attributes()?;
        let if_statement = self.parse_statement(statement_attributes)?;

        let else_statement = if self.look_ahead()?.0 == Token::Else {
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
                condition: main_condition,
                if_statement,
                else_statement,
            },
        })
    }

    pub fn parse_while_loop(&mut self) -> Result<WhileLoop<'ast>> {
        self.expect(Token::ParenOpen)?;
        let condition = self.parse_expression_id()?;
        self.expect(Token::ParenClose)?;
        let attributes = self.parse_attributes()?;
        let body = self.parse_statement(attributes)?;
        Ok(WhileLoop { condition, body })
    }
}

pub fn convert_function_call_to_branch_access(
    mut args: Vec<Node<Expression>>,
) -> Result<Node<BranchAccess>> {
    let span = args[0].source.extend(args.last().unwrap().source);
    let res = match args.len() {
        1 => BranchAccess::BranchOrNodePotential(reinterpret_expression_as_identifier(
            args.pop().unwrap(),
        )?),
        2 => {
            let second_net = reinterpret_expression_as_identifier(args.pop().unwrap())?;
            let first_net = reinterpret_expression_as_identifier(args.pop().unwrap())?;
            BranchAccess::Implicit(Branch::Nets(first_net, second_net))
        }
        _ => {
            return Err(Error {
                error_type: UnexpectedToken { expected: vec![] },
                source: args[0].source.extend(args.last().unwrap().source),
            })
        }
    };
    Ok(Node::new(res, span))
}
pub fn reinterpret_expression_as_identifier(
    expression: Node<Expression>,
) -> Result<HierarchicalId> {
    if let Expression::Primary(Primary::VariableOrNetReference(name)) = expression.contents {
        Ok(name)
    } else {
        Err(Error {
            source: expression.source,
            error_type: UnexpectedTokens {
                expected: vec![Expected::Identifier],
            },
        })
    }
}
