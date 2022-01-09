use stdx::{impl_debug, impl_from_typed};

use crate::{BasicBlock, InstIdx, PhiIdx};

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct Location {
    pub bb: BasicBlock,
    pub kind: LocationKind,
}

impl_debug!(match Location{Location{bb,kind} => "{:?} -> {:?}", bb,kind;});

#[derive(PartialOrd, Ord, Clone, Copy, Eq, PartialEq, Hash)]
pub enum LocationKind {
    // DO NOT CHANGE THE ORDER OF THE ENUM DISCRIMINANTS. IT IS RELIED UPON BY THE PartialOrd/Ord derivce!
    Phi(PhiIdx),
    Instruction(InstIdx),
    Terminator,
}

impl_from_typed! {
    Phi(PhiIdx),
    Instruction(InstIdx) for LocationKind
}

impl_debug! {
    match LocationKind{
        LocationKind::Phi(phi) => "{:?}",phi;
        LocationKind::Instruction(inst) => "{:?}",inst;
        LocationKind::Terminator => "terminator";
    }
}
