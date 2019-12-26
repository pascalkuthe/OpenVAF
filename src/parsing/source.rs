
use std::path::Path;
use nom::lib::std::collections::HashMap;
use hesr_alloc::{SliceId, Allocator, StrId};
use intrusive_collections::{RBTree, RBTreeLink};
use intrusive_collections::{KeyAdapter,intrusive_adapter};
use nom::character::complete::multispace0;
use nom::combinator::iterator;

//TODO map transformed files for pretty errors (start => (stop,((original_start,original_stop,file)|(newlines,end)))
// (sorted by start automatically since we linearay transverse the file) get closest start,
// To get actual position check whether inside anything go backwards until not inside anymore for recursive trace
// when not inside anything (anymore) sum all preceding blocks lines
pub enum SourceBlock{
    ExternalInsert {org_start:usize,org_stop:usize,file:StrId},
    InternalInsert {org_start:usize,org_stop:usize},
    Whitespace {newlines:usize,end_colum:usize},
}
pub struct SourceBlockRBNode{
    start:usize,
    block:SourceBlock,
    link:RBTreeLink
}
intrusive_adapter!(pub SourceBlockAdapter = Box<SourceBlockRBNode>: SourceBlockRBNode{ link: RBTreeLink });
impl<'a> KeyAdapter<'a> for SourceBlockAdapter {
    type Key = usize;
    fn get_key(&self, s: &'a SourceBlockRBNode) -> usize { s.start }
}
struct Source{
    files: SliceId<StrId>,
    alloc: Allocator,
    lookup: RBTree<SourceBlockAdapter>,
}
impl Source{

    pub (super)fn add_file(&mut self , file:&Path)->&str{
        let inital_contents = std::fs::read_to_string(file);
        unimplemented!()
    }


}