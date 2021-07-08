/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use core::fmt::Formatter;
use openvaf_session::sourcemap::Span;
use openvaf_session::sourcemap::StringLiteral;
use openvaf_session::symbols::Symbol;

use std::fmt::Display;

pub type SpannedToken = (Token, Span);

pub type TokenStream = Vec<SpannedToken>;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Token {
    Ident(Symbol),
    SystemFunctionIdent(Symbol),
    LiteralString(StringLiteral),
    RealLiteral(f64),
    IntLiteral(u32),

    Accessor,
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

    OpMul,
    OpDiv,
    OpModulus,
    Plus,
    Minus,
    OpExp,
    OpLogicNot,
    OpBitNot,

    OpArithmeticShiftLeft,
    OpArithmeticShiftRight,

    OpLess,
    OpLessEqual,
    OpGreater,
    OpGreaterEqual,
    OpEqual,
    OpNotEqual,
    OpLogicAnd,
    OpLogicalOr,

    OpBitAnd,
    OpBitXor,
    OpBitNXor,
    OpBitOr,

    OpCondition,

    //Keywords
    If,
    Else,
    Case,
    EndCase,
    Default,

    While,
    For,

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
    DefineParameter,
    LocalParameter,

    Analog,
    Function,
    EndFunction,
    AnalogInitial,

    Input,
    Inout,
    Output,

    Signed,
    Vectored,
    Scalared,

    //Types
    String,
    Time,
    Realtime,
    Integer,
    Real,
    NetType(Symbol),

    Potential,
    Flow,
    Domain,
    Discrete,
    Continuous,

    From,
    Exclude,
    Infinity,
    MinusInfinity,

    ConcatStart,
    ConcatEnd,
    ArrStart,

    EventStart,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let name = match self {
            Self::Ident(_) => "identifier",
            Self::SystemFunctionIdent(_) => "system function identifier",
            Self::LiteralString(_) => "string literal",
            Self::RealLiteral(_) => "real number",
            Self::IntLiteral(_) => "integer number",
            Self::Accessor => ".",
            Self::Semicolon => ";",
            Self::Colon => ":",
            Self::Comma => ",",
            Self::ParenOpen => "(",
            Self::ParenClose => ")",
            Self::AttributeStart => "(*",
            Self::AttributeEnd => "*)",
            Self::SquareBracketOpen => "[",
            Self::SquareBracketClose => "]",
            Self::Contribute => "<+",
            Self::Assign => "=",
            Self::Hash => "#",
            Self::OpMul => "*",
            Self::OpDiv => "/",
            Self::OpModulus => "%",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::OpExp => "**",
            Self::OpLogicNot => "!",
            Self::OpBitNot => "~",
            Self::OpArithmeticShiftLeft => "<<",
            Self::OpArithmeticShiftRight => ">>",
            Self::OpLess => "<",
            Self::OpLessEqual => "<=",
            Self::OpGreater => ">",
            Self::OpGreaterEqual => ">=",
            Self::OpEqual => "==",
            Self::OpNotEqual => "!=",
            Self::OpLogicAnd => "&&",
            Self::OpLogicalOr => "||",
            Self::OpBitAnd => "&",
            Self::OpBitXor => "^",
            Self::OpBitNXor => "^~/~^",
            Self::OpBitOr => "|",
            Self::OpCondition => "?",
            Self::If => "if",
            Self::Else => "else",
            Self::Case => "case",
            Self::EndCase => "endcase",
            Self::While => "while",
            Self::For => "for",
            Self::Begin => "begin",
            Self::End => "end",
            Self::Module => "module",
            Self::EndModule => "endmodule",
            Self::Discipline => "discipline",
            Self::EndDiscipline => "enddiscipline",
            Self::Nature => "nature",
            Self::EndNature => "endnature",
            Self::Branch => "branch",
            Self::Parameter => "parameter",
            Self::DefineParameter => "defparam",
            Self::LocalParameter => "localparam",
            Self::Analog => "analog",
            Self::AnalogInitial => "inital",
            Self::Input => "input",
            Self::Inout => "inout",
            Self::Output => "output",
            Self::Signed => "signed",
            Self::Vectored => "vectored",
            Self::Scalared => "scalared",
            Self::String => "string",
            Self::Time => "time",
            Self::Realtime => "realtime",
            Self::Integer => "integer",
            Self::Real => "real",
            Self::NetType(name) => return write!(f, "{}", name),
            Self::Potential => "potential",
            Self::Flow => "flow",
            Self::Domain => "domain",
            Self::Discrete => "discrete",
            Self::Continuous => "continuous",
            Self::From => "from",
            Self::Exclude => "exclude",
            Self::Infinity => "inf",
            Self::MinusInfinity => "-inf",
            Self::Function => "function",
            Self::EndFunction => "endfunction",
            Self::Default => "default",
            Self::ConcatStart => "{",
            Self::ConcatEnd => "}",
            Self::ArrStart => "'",
            Self::EventStart => "@",
        };

        write!(f, "{}", name)
    }
}
