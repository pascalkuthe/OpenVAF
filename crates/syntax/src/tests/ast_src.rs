//! Defines input for code generation process.

use sourcegen::to_upper_snake_case;

pub(crate) struct KindsSrc<'a> {
    pub(crate) punct: &'a [(&'a str, &'a str)],
    pub(crate) keywords: &'a [&'a str],
    pub(crate) literals: &'a [&'a str],
    pub(crate) tokens: &'a [&'a str],
    pub(crate) nodes: &'a [&'a str],
}

pub(crate) const KINDS_SRC: KindsSrc = KindsSrc {
    punct: &[
        (";", "SEMICOLON"),
        (",", "COMMA"),
        ("(", "L_PAREN"),
        (")", "R_PAREN"),
        ("{", "L_CURLY"),
        ("}", "R_CURLY"),
        ("[", "L_BRACK"),
        ("]", "R_BRACK"),
        ("<", "L_ANGLE"),
        (">", "R_ANGLE"),
        ("@", "AT"),
        ("#", "POUND"),
        ("~", "TILDE"),
        ("?", "QUESTION"),
        ("$", "DOLLAR"),
        ("&", "AMP"),
        ("|", "PIPE"),
        ("+", "PLUS"),
        ("*", "STAR"),
        ("/", "SLASH"),
        ("^", "CARET"),
        ("%", "PERCENT"),
        ("_", "UNDERSCORE"),
        (".", "DOT"),
        (":", "COLON"),
        ("=", "EQ"),
        ("==", "EQ2"),
        ("!", "BANG"),
        ("!=", "NEQ"),
        ("-", "MINUS"),
        ("<=", "LTEQ"),
        (">=", "GTEQ"),
        ("&&", "AMP2"),
        ("||", "PIPE2"),
        ("<<<", "ASHL"),
        (">>>", "ASHR"),
        ("<<", "SHL"),
        (">>", "SHR"),
        ("(*", "L_ATTR_PAREN"),
        ("*)", "R_ATTR_PAREN"),
        ("'{", "ARR_START"),
        ("<+", "CONTR"),
        ("**", "POW"),
        ("~^", "L_NXOR"),
        ("^~", "R_NXOR"),
    ],
    keywords: &[
        "analog",
        "begin",
        "branch",
        "case",
        "default",
        "disable",
        "discipline",
        "else",
        "end",
        "endcase",
        "enddiscipline",
        "endfunction",
        "endmodule",
        "endnature",
        "exclude",
        "for",
        "from",
        "function",
        "if",
        "inf",
        "inout",
        "input",
        "integer",
        "module",
        "nature",
        "output",
        "parameter",
        "real",
        "string",
        "while",
        "root",
        "initial_step",
        "final_step",
    ],
    literals: &["INT_NUMBER", "STD_REAL_NUMBER", "SI_REAL_NUMBER", "STR_LIT"],
    tokens: &[
        "ERROR",
        "IDENT",
        "SYSFUN",
        "NET_TYPE",
        "WHITESPACE",
        "COMMENT",
    ],
    nodes: &[
        "ANALOG_BEHAVIOUR",
        "ARG",
        "ARG_LIST",
        "ARRAY_EXPR",
        "ASSIGN",
        "ASSIGN_STMT",
        "ASSIGN_OR_EXPR",
        "ATTR",
        "ATTR_LIST",
        "BIN_EXPR",
        "BLOCK_SCOPE",
        "BLOCK_STMT",
        "BRANCH_DECL",
        "CALL",
        "CASE",
        "CASE_STMT",
        "CONSTRAINT",
        "DIRECTION",
        "DISCIPLINE_DECL",
        "DISCIPLINE_ATTR",
        "EVENT_STMT",
        "FOR_STMT",
        "FUNCTION",
        "FUNCTION_ARG",
        "IF_STMT",
        "LITERAL",
        "MODULE_DECL",
        "MODULE_PORT",
        "NAME",
        "NAME_REF",
        "SYS_FUN",
        "BODY_PORT_DECL",
        "NATURE_DECL",
        "NATURE_ATTR",
        "NET_DECL",
        "NETS",
        "PARAM",
        "PARAM_DECL",
        "PAREN_EXPR",
        "PATH",
        "PATH_EXPR",
        "PORT_DECL",
        "PORTS",
        "PREFIX_EXPR",
        "RANGE",
        "SELECT_EXPR",
        "TYPE",
        "VAR",
        "VAR_DECL",
        "WHILE_STMT",
        "EMPTY_STMT",
        "EXPR_STMT",
        "PORT_FLOW",
        "SOURCE_FILE",
        "FUNCTION_REF",
    ],
};

#[derive(Default, Debug)]
pub(crate) struct AstSrc {
    pub(crate) tokens: Vec<String>,
    pub(crate) nodes: Vec<AstNodeSrc>,
    pub(crate) enums: Vec<AstEnumSrc>,
}

#[derive(Debug)]
pub(crate) struct AstNodeSrc {
    pub(crate) doc: Vec<String>,
    pub(crate) name: String,
    pub(crate) traits: Vec<String>,
    pub(crate) fields: Vec<Field>,
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum Field {
    Token(String),
    Node { name: String, ty: String, cardinality: Cardinality },
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum Cardinality {
    Optional,
    Many,
}

#[derive(Debug)]
pub(crate) struct AstEnumSrc {
    pub(crate) doc: Vec<String>,
    pub(crate) name: String,
    pub(crate) traits: Vec<String>,
    pub(crate) variants: Vec<AstEnumVariant>,
    pub(crate) nested_variant: Option<String>,
}

#[derive(Debug)]
pub(crate) enum AstEnumVariant {
    Node(String),
    Token(String),
}

impl AstEnumVariant {
    pub(crate) fn syntax_kind(&self) -> String {
        match self {
            AstEnumVariant::Token(ref name) => format!("{}_KW", to_upper_snake_case(name)),
            AstEnumVariant::Node(ref name) => to_upper_snake_case(name),
        }
    }

    pub(crate) fn name(&self) -> &str {
        match self {
            AstEnumVariant::Node(ref name) | AstEnumVariant::Token(ref name) => name,
        }
    }
}
