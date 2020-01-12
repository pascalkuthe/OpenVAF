use crate::ast::VerilogType;
use crate::parser::lexer::Token;
use crate::parser::Parser;
use crate::parser::Result;

impl Parser {
    pub fn parse_type(&mut self, token: Token) -> std::result::Result<VerilogType, ()> {
        let vtype = match token {
            Token::Wreal => VerilogType::WREAL,
            Token::Supply0 => VerilogType::SUPPLY0,
            Token::Supply1 => VerilogType::SUPPLY1,
            Token::Tri => VerilogType::TRI,
            Token::TriAnd => VerilogType::TRIAND,
            Token::TriOr => VerilogType::TRIOR,
            Token::Tri0 => VerilogType::TRI0,
            Token::Tri1 => VerilogType::TRI1,
            Token::Wire => VerilogType::WIRE,
            Token::Uwire => VerilogType::UWIRE,
            Token::Wand => VerilogType::WAND,
            Token::Wor => VerilogType::WOR,
            Token::Reg => VerilogType::REG,
            Token::Integer => VerilogType::INTEGER,
            Token::Time => VerilogType::TIME,
            Token::Real => VerilogType::REAL,
            _ => return Err(()),
        };
        Ok(vtype)
    }
}
