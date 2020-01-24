/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::{
    AttributeNode, BlockScope, Branch, BranchAccess, Condition, Expression, HierarchicalId, Node,
    Primary, SeqBlock, Statement, Variable, VariableType,
};
use crate::parser::error::Type::{
    HierarchicalIdNotAllowedAsNature, UnexpectedToken, UnexpectedTokens,
};
use crate::parser::error::*;
use crate::parser::lexer::Token;
use crate::parser::Parser;
use crate::symbol::keywords;
use crate::symbol::Ident;
use crate::symbol_table::{SymbolDeclaration, SymbolTable};

impl<'lt, 'source_map> Parser<'lt, 'source_map> {
    pub fn parse_statement(&mut self) -> Result<Node<Statement>> {
        let (token, span) = self.look_ahead()?;
        let res = match token {
            Token::If => {
                self.lookahead.take();
                Statement::Condition(self.parse_condition()?)
            }
            Token::Flow => {
                self.lookahead.take();
                self.parse_contribute_statement(Ident::new(keywords::FLOW, span))?
            }
            Token::Potential => {
                self.lookahead.take();
                self.parse_contribute_statement(Ident::new(keywords::POTENTIAL, span))?
            }
            Token::Begin => {
                let start = self.preprocessor.current_start();
                self.lookahead.take();
                let (block, block_symbol_table) = self.parse_block()?;
                let block_symbol_table = if let Some(block_symbol_table) = block_symbol_table {
                    Some((block.scope.unwrap().name, block_symbol_table))
                } else {
                    None
                };
                self.lookahead.take();
                let res = self.ast_allocator.alloc(block);
                if let Some(symbol_table) = block_symbol_table {
                    self.insert_symbol(
                        symbol_table.0,
                        SymbolDeclaration::Block(
                            res,
                            symbol_table.1,
                            self.span_to_current_end(start),
                        ),
                    )?;
                }
                Statement::Block(res)
            }
            Token::SimpleIdentifier | Token::EscapedIdentifier => {
                let identifier = self.parse_hierarchical_identifier_internal(false)?;
                let (token, span) = self.look_ahead()?;
                let res = match token {
                    Token::Assign => {
                        self.lookahead.take();
                        Statement::Assign(
                            HierarchicalId {
                                names: identifier.into_bump_slice(),
                            },
                            self.parse_expression()?,
                        )
                    }
                    Token::ParenOpen => {
                        self.lookahead.take();
                        let (token, _) = self.look_ahead()?;
                        match token {
                            Token::OpLess => self.parse_contribute_statement(
                                Self::convert_to_nature_identifier(identifier)?,
                            )?,
                            Token::ParenClose => {
                                self.lookahead.take();
                                Statement::FunctionCall(identifier.into(), &[])
                            }
                            _ => {
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
                                        Self::convert_to_nature_identifier(identifier)?,
                                        convert_function_call_to_branch_access(arg.as_slice())?,
                                        self.parse_expression()?,
                                    )
                                } else {
                                    Statement::FunctionCall(
                                        identifier.into(),
                                        self.ast_allocator.alloc_slice_copy(arg.as_slice()),
                                    )
                                }
                            }
                        }
                    }
                    _ => {
                        return Err(Error {
                            error_type: UnexpectedTokens {
                                expected: vec![Expected::Statement],
                            },
                            source: span,
                        })
                    }
                };
                self.expect(Token::Semicolon)?;
                res
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

    /// Helper function that assert that a parsed hierarchical identifier is valid for acessing natures This is usued when it is not clear whether a nature or some other (hieraichal id) is parsed
    /// For example in the case of V(a) <+ a; V(a) the parser doesnt know yet whether its parsing a function call (in which M.V(a) would be valid) or a Branch acess (for which M.V would not be valid)
    /// When we hit the <+ statement this method is called to convert the types and assert that this is not an hierarchical id
    pub(crate) fn convert_to_nature_identifier(
        mut hierarchical_id: bumpalo::collections::Vec<Ident>,
    ) -> Result<Ident> {
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

    pub fn parse_block(&mut self) -> Result<(SeqBlock, Option<SymbolTable>)> {
        let (scope, symbol_table) = if self.look_ahead()?.0 == Token::Colon {
            self.lookahead.take();
            let name = self.parse_identifier(false)?;
            self.scope_stack.push(SymbolTable::new());
            let attributes = self.parse_attributes()?;
            let mut variables = Vec::new();
            //TODO parameteres
            loop {
                let token = self.look_ahead()?.0;
                let start = self.preprocessor.current_start();
                let mut res: Vec<AttributeNode<Variable>> = match token {
                    Token::Integer => {
                        self.lookahead.take();
                        self.parse_variable_declaration(VariableType::INTEGER)?
                    }
                    Token::Real => {
                        self.lookahead.take();
                        self.parse_variable_declaration(VariableType::REAL)?
                    }
                    _ => break,
                }
                .into_iter()
                .map(|decl| AttributeNode {
                    source: self.span_to_current_end(start),
                    attributes,
                    contents: decl,
                })
                .collect();
                variables.append(&mut res);
            }
            (
                Some(BlockScope {
                    name,
                    variables: self.ast_allocator.alloc_slice_copy(variables.as_slice()),
                }),
                Some(self.scope_stack.pop().unwrap()),
            )
        } else {
            (None, None)
        };
        let mut statements = Vec::new();
        while self.look_ahead()?.0 != Token::End {
            statements.push(self.parse_statement()?);
        }
        self.lookahead.take();
        let statements = self.ast_allocator.alloc_slice_copy(statements.as_slice());
        let res = SeqBlock { scope, statements };
        Ok((res, symbol_table))
    }

    pub fn parse_contribute_statement(&mut self, nature_acceess: Ident) -> Result<Statement> {
        let branch = self.parse_branch_access()?;
        self.expect(Token::Contribute)?;
        let expr = self.parse_expression()?;
        self.expect(Token::Semicolon)?;
        Ok(Statement::Contribute(nature_acceess, branch, expr))
    }
    pub fn parse_condition(&mut self) -> Result<Condition> {
        self.expect(Token::ParenOpen)?;
        let main_condition = self.parse_expression()?;
        self.expect(Token::ParenClose)?;
        let main_condition_statement = self.parse_statement()?;
        let main_condition_statement = self.ast_allocator.alloc_with(|| main_condition_statement);
        let mut else_if = Vec::new();
        let mut else_statement = None;
        loop {
            if self.look_ahead()?.0 != Token::Else {
                break;
            }
            if self.look_ahead()?.0 == Token::If {
                self.lookahead.take();
                let condition = self.parse_expression()?;
                let statement = self.parse_statement()?;
                else_if.push((condition, statement));
            } else {
                let statement = self.parse_statement()?;
                else_statement = Some(&*self.ast_allocator.alloc_with(|| statement));
                break;
            }
        }
        Ok(Condition {
            main_condition,
            main_condition_statement,
            else_ifs: self.ast_allocator.alloc_slice_copy(&else_if),
            else_statement,
        })
    }
}

pub fn convert_function_call_to_branch_access(args: &[Node<Expression>]) -> Result<BranchAccess> {
    let res = match args.len() {
        1 => BranchAccess::Explicit(reinterpret_expression_as_identifier(args[0])?),
        2 => {
            let first_net = reinterpret_expression_as_identifier(args[0])?;
            let second_net = reinterpret_expression_as_identifier(args[1])?;
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
