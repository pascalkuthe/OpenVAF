use super::*;
use crate::error::UserError;
use crate::test::setup_logger;
use std::fs;

#[test]
pub fn macro_test() -> std::result::Result<(), UserError> {
    setup_logger();
    let preprocessed_source = Source::try_from(fs::read_to_string("tests/macros.va")?)?;
    //Trimming is done here since amount of newlines may change and mostly doesnt make any statement about correctness (as long as necessary ones arent removed)
    assert_eq!(
        preprocessed_source
            .raw
            .replace("\r", "")
            .replace("\n", "")
            .replace(" ", ""),
        "OK1,OK2,__SMS__OK3OK3L,OK4"
    );
    Ok(())
}

#[test]
pub fn code_invariance() -> std::result::Result<(), UserError> {
    setup_logger();
    let preprocessed_source = Source::try_from(fs::read_to_string("tests/invariance.va")?)?;
    let control_source = Source::skip_preprocessing(
        fs::read_to_string("tests/invariance_control.va").expect("File not found!"),
    );
    assert_eq_parse_tree(
        preprocessed_source.parse()?.top_node,
        control_source.parse()?.top_node,
    )?;
    Ok(())
}

fn assert_eq_parse_tree(
    top_node1: crate::parsing::syntax::util::ParseTreeNode,
    top_node2: crate::parsing::syntax::util::ParseTreeNode,
) -> std::result::Result<(), String> {
    if top_node1.as_rule() != top_node2.as_rule() {
        return Err(format!(
            "Rules mismatch for {:?},{:?}",
            top_node1, top_node2
        ));
    }
    let mut children_tree1 = top_node1.clone().into_inner();
    let mut children_tree2 = top_node2.clone().into_inner();
    while let (Some(child1), Some(child2)) = (children_tree1.next(), children_tree2.next()) {
        assert_eq_parse_tree(child1, child2)?
    }
    if children_tree1.next().is_none() && children_tree2.next().is_none() {
        Ok(())
    } else {
        return Err(format!(
            "Children mismatch for {:?},{:?}",
            top_node1, top_node2
        ));
    }
}
