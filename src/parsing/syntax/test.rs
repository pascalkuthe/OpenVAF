use crate::error::Error;
use crate::parsing::Source;
use crate::test::setup_logger;
use std::convert::TryFrom;
use std::fs;

//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

/* TODO write verilog syntax test
#[test]
pub fn parse_test()->Result<(),UserError>{
    setup_logger();
    let preprocessed_source = Source::try_from(fs::read_to_string("tests/syntax.va")?)?;
    let parse_tree = preprocessed_source.parse()?;
    //TODO comare to hand verified result
    Ok(())
}
#[test]
pub fn ast_generation_test()->Result<(),UserError>{
    let preprocessed_source = Source::try_from(fs::read_to_string("tests/syntax.va")?)?;
    let raw_ast = preprocessed_source.parse()?.fold_to_raw_ast()?;
    //TODO compare to hand verified result
    Ok(())
}*/
