use crate::ast::{Expression, Primary};
use crate::parser::error::*;
use crate::parser::lexer::Token;
use crate::parser::Parser;

fn operator_precedance() {}
impl Parser {
    pub fn parse_expression(&mut self) -> Result<Expression> {
        unimplemented!("Expression")
    }
    fn precedance_climb_expression(&mut self, min_prec: u8) -> Result<Expression> {
        unimplemented!("Expression")
    }

    /*
    parse_expression_1(lhs, min_precedence)
    lookahead := peek next token
    while lookahead is a binary operator whose precedence is >= min_precedence
        op := lookahead
        advance to next token
        rhs := parse_primary ()
        lookahead := peek next token
        while lookahead is a binary operator whose precedence is greater
                 than op's, or a right-associative operator
                 whose precedence is equal to op's
            rhs := parse_expression_1 (rhs, lookahead's precedence)
            lookahead := peek next token
        lhs := the result of applying op with operands lhs and rhs
    return lhs
    */
    fn parse_primary(&mut self)->Result<Primary>{
            let res = match self.look_ahead()?.0{
                Token::LiteralRealNumber =>,
                Token::LiteralUnsignedNumber => ,
                Token::String =>,
                Token::SimpleIdentifier|Token::EscapedIdentifier => {
                    let ident = self.parse_hieraichal_identifier(false)?;
                    if self.look_ahead()?.0 == Token::ParenOpen{
                        self.lookahead.take();
                        if self.look_ahead()?.0 == Token::OpLess{
                            return Token::Branch(self.parse_branch()?);
                        }
                        self.expect(Token::ParenClose)
    //                    self.parse_list(|sel| arg.push(sel.parse_expression()),Token::ParenClose,true); Function call
                    }

                }
            }
            Ok(res)
        }
}
