use crate::error::Error;
use crate::parsing::Source;
use log::info;
use std::convert::TryFrom;
use std::fs;

#[test]
pub fn generate_ast_test() -> Result<(), Error> {
    //TODO write parse.va
    let preprocessed_source = Source::try_from(fs::read_to_string("tests/parse.va")?)?;
    let raw_ast = preprocessed_source.parse()?.fold_to_raw_ast()?;
    info!("{}", raw_ast); //this is just here since I just use this test to manually ensure the result is correct
                          //TODO verify that this is correct
    Ok(())
}
