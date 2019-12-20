use crate::parsing::syntax::Rule;

pub type SyntaxError = pest::error::Error<Rule>;

impl From<SyntaxError> for super::Error {
    fn from(raw_error: SyntaxError) -> Self {
        /*
        if let pest::error::ErrorVariant::ParsingError{mut ref positives,ref negatives} = raw_error{

         }
         //TODO map rule names, simplify instead of showing long list of rules
         Use renamed_rules() as guide (not good enough for enduse since I want to go Expected Expression and not list all operators, primaries etc.
         */
        Self::Syntax(raw_error)
    }
}
