/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use intrusive_collections::__core::cell::RefCell;

use crate::ast::{
    AttributeNode, Attributes, BlockScope, Branch, BranchAccess, Condition, Expression,
    HierarchicalId, Node, Primary, SeqBlock, Statement, VariableType,
};
use crate::ir::{BlockId, StatementId};
use crate::parser::error::Type::{
    HierarchicalIdNotAllowedAsNature, UnexpectedToken, UnexpectedTokens,
};
use crate::parser::error::*;
use crate::parser::lexer::Token;
use crate::parser::Parser;
use crate::symbol::keywords;
use crate::symbol::Ident;
use crate::symbol_table::{SymbolDeclaration, SymbolTable};
use crate::util::Push;

impl<'lt, 'ast, 'astref, 'source_map> Parser<'lt, 'ast, 'astref, 'source_map> {
    pub fn parse_statement(&mut self, attributes: Attributes<'ast>) -> Result<StatementId<'ast>> {
        let (token, span) = self.look_ahead()?;
        let res = match token {
            Token::If => {
                self.lookahead.take();
                Statement::Condition(self.parse_condition(attributes)?)
            }
            Token::Flow => {
                self.lookahead.take();
                self.parse_contribute_statement(attributes, Ident::new(keywords::FLOW, span))?
            }
            Token::Potential => {
                self.lookahead.take();
                self.parse_contribute_statement(attributes, Ident::new(keywords::POTENTIAL, span))?
            }
            Token::Begin => {
                self.lookahead.take();
                Statement::Block(self.parse_block(attributes)?)
            }
            Token::SimpleIdentifier | Token::EscapedIdentifier => {
                let identifier = self.parse_hierarchical_identifier_internal(false)?;
                let (token, span) = self.look_ahead()?;
                let res = match token {
                    Token::Assign => {
                        self.lookahead.take();
                        Statement::Assign(
                            attributes,
                            identifier.into(),
                            self.parse_expression_id()?,
                        )
                    }
                    Token::ParenOpen => {
                        self.lookahead.take();
                        let (token, _) = self.look_ahead()?;
                        match token {
                            Token::OpLess => self.parse_contribute_statement(
                                attributes,
                                Self::convert_to_nature_identifier(identifier)?,
                            )?,
                            Token::ParenClose => {
                                self.lookahead.take();
                                Statement::FunctionCall(
                                    attributes,
                                    identifier.into(),
                                    RefCell::default(),
                                )
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
                                    self.lookahead.take();
                                    Statement::Contribute(
                                        attributes,
                                        Self::convert_to_nature_identifier(identifier)?,
                                        convert_function_call_to_branch_access(arg)?,
                                        self.parse_expression_id()?,
                                    )
                                } else {
                                    Statement::FunctionCall(
                                        attributes,
                                        identifier.into(),
                                        RefCell::new(
                                            arg.into_iter()
                                                .map(|expr| self.ast.push(expr))
                                                .collect(),
                                        ),
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
            self.lookahead.take();
            let name = self.parse_identifier(false)?;
            self.scope_stack.push(SymbolTable::with_capacity(
                Self::BLOCK_DEFAULT_SYMTABLE_SIZE,
            ));
            loop {
                let attributes = self.parse_attributes()?;
                let token = self.look_ahead()?.0;
                match token {
                    Token::Integer => {
                        self.lookahead.take();
                        self.parse_variable_declaration(VariableType::INTEGER, attributes)?
                    }
                    Token::Real => {
                        self.lookahead.take();
                        self.parse_variable_declaration(VariableType::REAL, attributes)?
                    }
                    //TODO parameteres
                    _ => break,
                };
            }
            Some(BlockScope {
                name,
                symbols: self.scope_stack.pop().unwrap(),
            })
        } else {
            None
        };
        let mut statements = Vec::with_capacity(Self::BLOCK_DEFAULT_STATEMENT_CAPACITY);

        while self.look_ahead()?.0 != Token::End {
            let attributes = self.parse_attributes()?;
            statements.push(self.parse_statement(attributes)?);
        }
        self.lookahead.take();

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
        let main_condition_statement = self.parse_statement(statement_attributes)?;
        let mut else_ifs = Vec::new();
        let mut else_statement = None;

        loop {
            if self.look_ahead()?.0 != Token::Else {
                break;
            }
            self.lookahead.take();
            if self.look_ahead()?.0 == Token::If {
                self.lookahead.take();
                self.expect(Token::ParenOpen)?;
                let condition = self.parse_expression_id()?;
                self.expect(Token::ParenClose)?;
                let attributes = self.parse_attributes()?;
                else_ifs.push((condition, self.parse_statement(attributes)?));
            } else {
                let attributes = self.parse_attributes()?;
                else_statement = Some(self.parse_statement(attributes)?);
                break;
            }
        }

        Ok(AttributeNode {
            attributes,
            source: self.span_to_current_end(start),
            contents: Condition {
                main_condition,
                main_condition_statement,
                else_ifs,
                else_statement,
            },
        })
    }
}

pub fn convert_function_call_to_branch_access(
    mut args: Vec<Node<Expression>>,
) -> Result<BranchAccess> {
    let res = match args.len() {
        1 => BranchAccess::Explicit(reinterpret_expression_as_identifier(args.pop().unwrap())?),
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
