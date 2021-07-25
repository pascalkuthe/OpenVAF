/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::{lexer::RawToken, sourcemap::CtxSpan};
use data_structures::{
    index_vec::{define_index_type, IndexVec},
    text_size::{TextRange, TextSize},
};
use std::convert::{TryFrom, TryInto};

pub(crate) type ParsedTokenStream<'s> = Vec<(ParsedToken<'s>, TextRange)>;

pub(crate) type MacroArgs<'s> = IndexVec<MacroArg, (ParsedTokenStream<'s>, TextRange)>;

define_index_type! {
            pub struct MacroArg = u8;

            DISPLAY_FORMAT = "macro_arg{}";

            DEBUG_FORMAT = "macro_arg{}";

            IMPL_RAW_CONVERSIONS = true;

}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum ParsedToken<'s> {
    ResolvedToken(TokenKind),
    ArgumentReference(MacroArg),
    MacroCall(MacroCall<'s>),
}

impl TryFrom<RawToken> for ParsedToken<'_> {
    type Error = ();

    fn try_from(value: RawToken) -> Result<Self, Self::Error> {
        Ok(Self::ResolvedToken(value.try_into()?))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Macro<'s> {
    pub head: TextSize,
    pub span: CtxSpan,
    pub body: Vec<(ParsedToken<'s>, TextRange)>,
    pub arg_cnt: usize,
}

impl Macro<'_> {
    pub fn head_span(&self) -> CtxSpan {
        self.span.with_len(self.head)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct MacroCall<'s> {
    pub name: &'s str,
    pub arg_bindings: MacroArgs<'s>,
}

pub type TokenStream = Vec<Token>;

#[derive(Clone, Debug, PartialEq, Copy, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: CtxSpan,
}

#[derive(Clone, Debug, PartialEq, Copy, Eq, Hash)]
pub enum TokenKind {
    Comment,
    WhiteSpace,
    EOF,

    //Identifiers
    Identifier,
    SystemCall,

    //Literals
    LitString,
    LitInteger,
    LitSiRealNumber,
    LitStdRealNumber,

    //Symbols
    Dot,
    Semicolon,

    Colon,
    Comma,

    ParenOpen,
    ParenClose,

    AttributeStart,
    AttributeEnd,

    SquareBracketOpen,
    SquareBracketClose,

    Contribute,

    Assign,
    Hash,

    //Arithmatic Operators
    Mul,
    Div,

    Modulus,
    Plus,

    Minus,
    Pow,

    ShiftLeft,
    ShiftRight,

    ArithmeticShiftLeft,
    ArithmeticShiftRight,

    //Unaryerators
    LogicNot,
    BitNot,

    //Relational
    Less,
    LessEqual,

    Greater,
    GreaterEqual,

    Equal,
    NotEqual,

    //Logic
    LogicAnd,
    LogicalOr,

    //Bit
    BitXor,
    LNXor,
    RNXor,

    And,
    Pipe,

    //Other
    QuestionMark,

    LBrace,
    RBrace,

    ArrStart,
    EventStart,

    //Keywords
    If,
    Else,
    While,
    For,

    Case,

    EndCase,
    Default,

    Begin,
    End,

    Module,
    EndModule,

    Discipline,
    EndDiscipline,

    Nature,
    EndNature,

    Branch,
    Parameter,

    Analog,
    Function,
    EndFunction,

    Input,
    Inout,
    Output,

    Root,

    //Types
    String,
    Integer,
    Real,

    NetType,

    From,
    Exclude,
    Infinity,

    Error,
}

impl TryFrom<RawToken> for TokenKind {
    type Error = ();

    fn try_from(raw: RawToken) -> Result<Self, ()> {
        let res = match raw {
            RawToken::MacroDefNewLine | RawToken::WhiteSpace | RawToken::Newline => {
                Self::WhiteSpace
            }
            RawToken::Comment | RawToken::BlockComment => Self::Comment,
            RawToken::SimpleIdentifier | RawToken::EscapedIdentifier => Self::Identifier,
            RawToken::SystemCall => Self::SystemCall,
            RawToken::LitString => Self::LitString,
            RawToken::LitInteger => Self::LitInteger,
            RawToken::LitSiRealNumber => Self::LitSiRealNumber,
            RawToken::LitStdRealNumber => Self::LitStdRealNumber,
            RawToken::Dot => Self::Dot,
            RawToken::Semicolon => Self::Semicolon,
            RawToken::Colon => Self::Colon,
            RawToken::Comma => Self::Comma,
            RawToken::ParenOpen => Self::ParenOpen,
            RawToken::ParenClose => Self::ParenClose,
            RawToken::AttributeStart => Self::AttributeStart,
            RawToken::AttributeEnd => Self::AttributeEnd,
            RawToken::SquareBracketOpen => Self::SquareBracketOpen,
            RawToken::SquareBracketClose => Self::SquareBracketClose,
            RawToken::Contribute => Self::Contribute,
            RawToken::Assign => Self::Assign,
            RawToken::Hash => Self::Hash,
            RawToken::Mul => Self::Mul,
            RawToken::Div => Self::Div,
            RawToken::Modulus => Self::Modulus,
            RawToken::Plus => Self::Plus,
            RawToken::Minus => Self::Minus,
            RawToken::Pow => Self::Pow,
            RawToken::ShiftLeft => Self::ShiftLeft,
            RawToken::ShiftRight => Self::ShiftRight,
            RawToken::ArithmeticShiftLeft => Self::ArithmeticShiftLeft,
            RawToken::ArithmeticShiftRight => Self::ArithmeticShiftRight,
            RawToken::LogicNot => Self::LogicNot,
            RawToken::BitNot => Self::BitNot,
            RawToken::Less => Self::Less,
            RawToken::LessEqual => Self::LessEqual,
            RawToken::Greater => Self::Greater,
            RawToken::GreaterEqual => Self::GreaterEqual,
            RawToken::Equal => Self::Equal,
            RawToken::NotEqual => Self::NotEqual,
            RawToken::LogicAnd => Self::LogicAnd,
            RawToken::LogicalOr => Self::LogicalOr,
            RawToken::And => Self::And,
            RawToken::BitXor => Self::BitXor,
            RawToken::LNXor => Self::LNXor,
            RawToken::RNXor => Self::RNXor,
            RawToken::Pipe => Self::Pipe,
            RawToken::QuestionMark => Self::QuestionMark,
            RawToken::LBrace => Self::LBrace,
            RawToken::RBrace => Self::RBrace,
            RawToken::ArrStart => Self::ArrStart,
            RawToken::EventStart => Self::EventStart,
            RawToken::If => Self::If,
            RawToken::Else => Self::Else,
            RawToken::While => Self::While,
            RawToken::For => Self::For,
            RawToken::Case => Self::Case,
            RawToken::EndCase => Self::EndCase,
            RawToken::Default => Self::Default,
            RawToken::Begin => Self::Begin,
            RawToken::End => Self::End,
            RawToken::Module => Self::Module,
            RawToken::EndModule => Self::EndModule,
            RawToken::Discipline => Self::Discipline,
            RawToken::EndDiscipline => Self::EndDiscipline,
            RawToken::Nature => Self::Nature,
            RawToken::EndNature => Self::EndNature,
            RawToken::Branch => Self::Branch,
            RawToken::Parameter => Self::Parameter,
            RawToken::Analog => Self::Analog,
            RawToken::Function => Self::Function,
            RawToken::EndFunction => Self::EndFunction,
            RawToken::Input => Self::Input,
            RawToken::Inout => Self::Inout,
            RawToken::Output => Self::Output,
            RawToken::String => Self::String,
            RawToken::Integer => Self::Integer,
            RawToken::Real => Self::Real,
            RawToken::From => Self::From,
            RawToken::Exclude => Self::Exclude,
            RawToken::Infinity => Self::Infinity,
            RawToken::EOF => Self::EOF,
            RawToken::NetType => Self::NetType,
            RawToken::Root => Self::Root,
            RawToken::Unexpected => Self::Error,
            _ => return Err(()),
        };

        Ok(res)
    }
}
