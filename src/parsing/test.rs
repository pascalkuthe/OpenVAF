use crate::error::UserError;
use crate::parsing::Source;
use std::convert::TryFrom;
use std::fs;

#[test]
pub fn generate_ast_test() -> Result<(), UserError> {
    //TODO write parse.va
    let preprocessed_source = Source::try_from(fs::read_to_string("tests/parse.va")?)?;
    let raw_ast = preprocessed_source.parse()?.fold_to_raw_ast()?;
    raw_ast.pprint(); //this is just here since I just use this test to manually ensure the result is correct
                      //TODO verify that this is correct
    Ok(())
}
