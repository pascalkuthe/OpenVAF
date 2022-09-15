use text_size::TextSize;

/// Parsed token.
/// It doesn't contain information about data that has been parsed,
/// only the type of the token and its size.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub len: TextSize,
}

/// Enum representing common lexeme types.
// perf note: Changing all `usize` to `u32` doesn't change performance
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    // Multi-char tokens:
    /// "// comment"
    LineComment,
    /// `/* block comment */`
    BlockComment {
        terminated: bool,
    },
    /// Any whitespace characters sequence.
    Whitespace,

    /// a normal identifier
    SimpleIdent,

    /// an escaped identifer starts with \ and ends with a whitespace
    EscapedIdent,

    /// a system call Identifier
    SystemCallIdent,

    Literal {
        kind: LiteralKind,
    },

    /// a compiler directive
    CompilerDirective,

    /// Because macro definitions (`define) are whitespace aware,
    /// the lexer this includes all tokens to the next whitespace
    Define {
        end: usize,
    },

    IllegalDefine,

    // One-char tokens:
    /// ";"
    Semi,
    /// ","
    Comma,
    /// "."
    Dot,
    /// "("
    OpenParen,
    /// ")"
    CloseParen,
    /// "{"
    OpenBrace,
    /// "}"
    CloseBrace,
    /// "["
    OpenBracket,
    /// "]"
    CloseBracket,
    /// "@"
    At,
    /// "#"
    Pound,
    /// "~"
    Tilde,
    /// "?"
    Question,
    /// ":"
    Colon,
    /// "$"
    Dollar,
    /// "="
    Eq,
    /// "!"
    Not,
    /// "<"
    Lt,
    /// ">"
    Gt,
    /// "-"
    Minus,
    /// "&"
    And,
    /// "|"
    Or,
    /// "+"
    Plus,
    /// "*"
    Star,
    /// "/"
    Slash,
    /// "^"
    Caret,
    /// "%"
    Percent,

    /// (*
    AttrOpenParen,
    /// *)
    AttrCloseParen,
    /// '{
    ArrStart,

    /// ==
    Eq2,
    /// !=
    Neq,
    /// <=
    Leq,
    /// >=
    Geq,

    /// ||
    Pipe2,
    /// &&
    Amp2,

    /// <<
    Shl,
    /// >>
    Shr,

    /// <<<
    ShlA,
    /// >>>
    ShrA,

    /// <+
    Contribute,

    /// **
    Pow,

    /// ~^
    NXorL,
    /// ^~
    NXorR,

    /// Unknown token, not expected by the lexer, e.g. "№"
    Unknown,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralKind {
    Int,
    Float { has_scale_char: bool },
    Str { terminated: bool },
}
