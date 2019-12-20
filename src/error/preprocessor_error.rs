use crate::parsing::preprocessor::Rule;
/// The preprocessor uses a different grammar and therefor has different Rules.
/// Since this crate relies on `pest::error::Error` which is generic over a rule this was important
pub type PreprocessorError = pest::error::Error<Rule>;

impl From<PreprocessorError> for super::Error {
    fn from(raw_error: PreprocessorError) -> Self {
        Self::Preprocessor(raw_error.renamed_rules(format_rule))
    }
}
//TODO const when format becomes const
fn format_rule(rule: &Rule) -> String {
    match rule {
        Rule::TOK_IFDEF => "ifdef",
        Rule::TOK_IFNDEF => "ifndef",
        Rule::TOK_ELSIF => "elsif",
        Rule::TOK_DEFINE => "define",
        Rule::COMPILER_DIRECTIVE => "Compiler directive",
        Rule::TOK_COMPILER_DIRECTIVE_START => "`",
        Rule::IDENTIFIER => "Identifier",
        Rule::SIMPLE_IDENTIFIER => "Identifier",

        other => return format!("{:?}", other),
    }
    .to_string()
}
