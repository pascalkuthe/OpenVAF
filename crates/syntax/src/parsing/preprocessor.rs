use preprocessor::sourcemap::CtxSpan;
use rowan::TextSize;

use crate::{
    SyntaxKind::{self, *},
    T,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct Token {
    pub kind: SyntaxKind,
    pub span: CtxSpan,
}

impl Token {
    pub fn is_trivia(&self) -> bool {
        self.kind.is_trivia()
    }

    pub fn len(&self) -> TextSize {
        self.span.range.len()
    }
}

impl From<preprocessor::Token> for Token {
    fn from(t: preprocessor::Token) -> Self {
        Token { kind: convert_token_to_syntax_kind(t.kind), span: t.span }
    }
}

pub(super) fn convert_token_to_syntax_kind(t: preprocessor::TokenKind) -> SyntaxKind {
    use preprocessor::TokenKind;
    match t {
        TokenKind::Comment => COMMENT,
        TokenKind::WhiteSpace => WHITESPACE,
        TokenKind::EOF => EOF,
        TokenKind::Identifier => IDENT,
        TokenKind::SystemCall => SYSFUN,
        TokenKind::LitString => STRING,
        TokenKind::LitInteger => INT_NUMBER,
        TokenKind::LitSiRealNumber => SI_REAL_NUMBER,
        TokenKind::LitStdRealNumber => STD_REAL_NUMBER,
        TokenKind::Dot => T![.],
        TokenKind::Semicolon => T![;],
        TokenKind::Colon => T![:],
        TokenKind::Comma => T![,],
        TokenKind::ParenOpen => T!['('],
        TokenKind::ParenClose => T![')'],
        TokenKind::AttributeStart => T!["(*"],
        TokenKind::AttributeEnd => T!["*)"],
        TokenKind::SquareBracketOpen => T!['['],
        TokenKind::SquareBracketClose => T![']'],
        TokenKind::Contribute => T![<+],
        TokenKind::Assign => T![=],
        TokenKind::Hash => T![#],
        TokenKind::Mul => T![*],
        TokenKind::Div => T![/],
        TokenKind::Modulus => T![%],
        TokenKind::Plus => T![+],
        TokenKind::Minus => T![-],
        TokenKind::Pow => T![**],
        TokenKind::ShiftLeft => T![<<],
        TokenKind::ShiftRight => T![>>],
        TokenKind::LogicNot => T![!],
        TokenKind::BitNot => T![~],
        TokenKind::Less => T![<],
        TokenKind::LessEqual => T![<=],
        TokenKind::Greater => T![>],
        TokenKind::GreaterEqual => T![>=],
        TokenKind::Equal => T![==],
        TokenKind::NotEqual => T![!=],
        TokenKind::LogicAnd => T![&&],
        TokenKind::LogicalOr => T![||],
        TokenKind::BitXor => T![^],
        TokenKind::RNXor => T![^~],
        TokenKind::LNXor => T![~^],
        TokenKind::And => T![&],
        TokenKind::Pipe => T![|],
        TokenKind::QuestionMark => T![?],
        TokenKind::LBrace => T!['{'],
        TokenKind::RBrace => T!['}'],
        TokenKind::ArrStart => T!["'{"],
        TokenKind::EventStart => T![@],
        TokenKind::If => IF_KW,
        TokenKind::Else => ELSE_KW,
        TokenKind::While => WHILE_KW,
        TokenKind::For => FOR_KW,
        TokenKind::Case => CASE_KW,
        TokenKind::EndCase => ENDCASE_KW,
        TokenKind::Default => DEFAULT_KW,
        TokenKind::Begin => BEGIN_KW,
        TokenKind::End => END_KW,
        TokenKind::Module => MODULE_KW,
        TokenKind::EndModule => ENDMODULE_KW,
        TokenKind::Discipline => DISCIPLINE_KW,
        TokenKind::EndDiscipline => ENDDISCIPLINE_KW,
        TokenKind::Nature => NATURE_KW,
        TokenKind::EndNature => ENDNATURE_KW,
        TokenKind::Branch => BRANCH_KW,
        TokenKind::Parameter => PARAMETER_KW,
        TokenKind::Analog => ANALOG_KW,
        TokenKind::Function => FUNCTION_KW,
        TokenKind::EndFunction => ENDFUNCTION_KW,
        TokenKind::Input => INPUT_KW,
        TokenKind::Inout => INOUT_KW,
        TokenKind::Output => OUTPUT_KW,
        TokenKind::String => STRING_KW,
        TokenKind::Integer => INTEGER_KW,
        TokenKind::Real => REAL_KW,
        TokenKind::NetType => NET_TYPE,
        TokenKind::From => FROM_KW,
        TokenKind::Exclude => EXCLUDE_KW,
        TokenKind::Infinity => INF_KW,
        TokenKind::Error => ERROR,
        TokenKind::ArithmeticShiftLeft => T![<<<],
        TokenKind::ArithmeticShiftRight => T![>>>],
        TokenKind::Root => ROOT_KW,
    }
}
