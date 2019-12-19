use pest::error::{Error, ErrorVariant};
use pest::RuleType;
use std::fs;

use crate::parsing::syntax::{create_parse_tree, ParseTree};
use crate::setup_logger;

use super::*;

#[test]
pub fn macro_test() {
    setup_logger();
    let preprocessed_source = match process_file("tests/macros.va") {
        Ok(res) => res,
        Err(e) => panic!("{}", e)
    };
    //Trimming is done here since amount of newlines may change and mostly doesnt make any statement about correctness (as long as necessary ones arent removed)
    assert_eq!(preprocessed_source.0.replace("\r", "").replace("\n", "").replace(" ", ""), "OK1,OK2,__SMS__OK3OK3L,OK4")
}

#[test]
pub fn code_invariance() {
    setup_logger();
    let preprocessed_source = match process_file("tests/invariance.va") {
        Err(io_err) => panic!(format!("{}", io_err)),
        Ok(source) => { source }
    };
    let parse_tree = match create_parse_tree(&preprocessed_source) {
        Ok(result) => result,
        Err(e) => panic!("{}", e)
    };
    let control_source = PreprocessedSource::skip_preprocessor(
        fs::read_to_string("tests/invariance_control.va").expect("File not found!"));
    let control_parse_tree = match create_parse_tree(&control_source) {
        Ok(result) => result,
        Err(e) => panic!()
    };
    if let Err(e) = assert_eq_parse_tree(parse_tree.0, control_parse_tree.0) {
        panic!(e)
    }
}

fn assert_eq_parse_tree(top_node1: crate::parsing::syntax::util::ParseTreeNode, top_node2: crate::parsing::syntax::util::ParseTreeNode) -> std::result::Result<(), String> {
    if top_node1.as_rule() != top_node2.as_rule() {
        return Err(format!("Rules mismatch for {:?},{:?}", top_node1, top_node2));
    }
    let mut children_tree1 = top_node1.clone().into_inner();
    let mut children_tree2 = top_node2.clone().into_inner();
    while let (Some(child1), Some(child2)) = (children_tree1.next(), children_tree2.next()) {
        assert_eq_parse_tree(child1, child2)?
    }
    if children_tree1.next().is_none() && children_tree2.next().is_none() {
        Ok(())
    } else {
        return Err(format!("Children mismatch for {:?},{:?}", top_node1, top_node2));
    }
}
