use crate::ast::ModuleItem::BranchDecl;
use crate::ast::{Branch, BranchDeclaration, Reference};
use crate::parser::error::Result;
use crate::parser::lexer::Token;
use crate::parser::Parser;

impl Parser {
    pub fn parse_branch_declaration(&mut self) -> Result<BranchDeclaration> {
        self.expect(Token::ParenOpen);
        let branch = self.parse_branch()?;
        self.expect(Token::ParenClose)?;
        let name = self.parse_identifier(false)?;
        self.expect(Token::Semicolon);
        Ok(BranchDeclaration { name, branch })
    }
    pub fn parse_branch(&mut self) -> Result<Branch> {
        if self.look_ahead()?.0 == Token::OpLess {
            self.lookahead.take();
            let res = Branch::Port(Reference::new(self.parse_hieraichal_identifier(false)?));
            self.expect(Token::OpGreater);
            Ok(res)
        } else {
            let first_net_name = self.parse_hieraichal_identifier(false)?;
            self.expect(Token::Comma)?;
            let second_net_name = self.parse_hieraichal_identifier(false)?;
            Ok(Branch::Nets(
                Reference::new(first_net_name),
                Reference::new(second_net_name),
            ))
        }
    }
}
