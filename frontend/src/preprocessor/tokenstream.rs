use crate::parser::tokenstream::Token as ParserToken;
use crate::symbol::Symbol;
use crate::Span;
use index_vec::{define_index_type, IndexVec};

pub type TokenStream = Vec<SpannedToken>;

pub type SpannedToken = (Token, Span);

pub type MacroArgs = IndexVec<MacroArg, TokenStream>;

define_index_type! {
            pub struct MacroArg = u8;

            DISPLAY_FORMAT = "{}";

            DEBUG_FORMAT = "<MacroArgumentIndex {}>";

            IMPL_RAW_CONVERSIONS = true;

}

#[derive(Clone, Debug)]
pub enum Token {
    ResolvedToken(ParserToken),
    MacroDefinition(Symbol, Macro),
    ArgumentReference(MacroArg),
    MacroCall(MacroCall),
    Condition(UnresolvedCondition),
    FileInclude(String),
}

#[derive(Debug, Clone)]
pub struct Macro {
    pub head: Span,
    pub body: Vec<SpannedToken>,
    pub arg_len_idx: MacroArg,
}

#[derive(Debug, Clone)]
pub struct UnresolvedCondition {
    pub if_def: Symbol,
    pub inverted: bool,
    pub true_tokens: Vec<SpannedToken>,
    pub else_ifs: Vec<(Symbol, Vec<SpannedToken>)>,
    pub else_tokens: Vec<SpannedToken>,
}

#[derive(Debug, Clone)]
pub struct MacroCall {
    pub name: Symbol,
    pub arg_bindings: MacroArgs,
}
