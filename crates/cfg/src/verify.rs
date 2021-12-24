use bitset::BitSet;
use lasso::Rodeo;
use stdx::impl_debug;

use crate::{
    BasicBlock, ControlFlowGraph, InstrDst, Local, LocationKind, Operand, PhiIdx, Terminator,
};

pub enum VerifyError {
    InvalidPhiSource { bb: BasicBlock, phi: PhiIdx, src: BasicBlock },
    InvalidPhi { bb: BasicBlock, phi: PhiIdx, missing_sources: Box<[BasicBlock]> },
    InvalidRead { bb: BasicBlock, loc: LocationKind, local: Local },
}

impl_debug! {
    match VerifyError{
        VerifyError::InvalidPhiSource{ bb, phi, src} => "{:?} is not a predecessor of {:?} (but used in phi {:?})", src, bb, phi;
        VerifyError::InvalidPhi{ bb, phi, missing_sources} => "phi {:?}::{:?} is missing sources {:?}", bb, phi, missing_sources;
        VerifyError::InvalidRead{bb, loc, local} => "{:?}->{:?}: {:?} was read before write!", bb,loc,local;
    }
}

impl ControlFlowGraph {
    pub fn verify(&self) -> Box<[VerifyError]> {
        let mut res = Vec::new();
        let mut set_locals = BitSet::new_empty(self.next_local.into());

        for (bb, data) in self.reverse_postorder_iter() {
            for (phi, data) in data.phis.iter_enumerated() {
                for src in data.sources.values() {
                    if !set_locals.contains(*src) {
                        res.push(VerifyError::InvalidRead { bb, loc: phi.into(), local: *src })
                    }
                }
                set_locals.insert(data.dst);

                let mut predecessors = self.predecessors(bb).to_owned();
                for predecessor in data.sources.keys() {
                    let pos = predecessors.iter().position(|it| it == predecessor);
                    match pos {
                        Some(pos) => {
                            predecessors.swap_remove(pos);
                        }
                        None => {
                            res.push(VerifyError::InvalidPhiSource { bb, phi, src: *predecessor });
                        }
                    }
                }
                if !predecessors.is_empty() {
                    res.push(VerifyError::InvalidPhi {
                        bb,
                        phi,
                        missing_sources: predecessors.into_boxed_slice(),
                    });
                }
            }

            for (instr, data) in data.instructions.iter_enumerated() {
                data.visit_operands(|op| {
                    if let Operand::Local(local) = op {
                        if !set_locals.contains(*local) {
                            res.push(VerifyError::InvalidRead {
                                bb,
                                loc: instr.into(),
                                local: *local,
                            })
                        }
                    }
                });

                if let InstrDst::Local(local) = data.dst {
                    set_locals.insert(local);
                }
            }

            if let Some(Terminator::Split { condition: Operand::Local(local), .. }) =
                data.terminator
            {
                if !set_locals.contains(local) {
                    res.push(VerifyError::InvalidRead { bb, loc: LocationKind::Terminator, local })
                }
            }
        }

        res.into_boxed_slice()
    }

    pub fn assert_verified(&self, literals: &Rodeo) {
        let errs = self.verify();
        if !errs.is_empty() {
            let cfg = self.dump(Some(literals));
            eprintln!(
                "\n\x1b[31;1merror\x1b[0m: CFG violates invariants\n\n{}\n\n{:#?}",
                cfg, errs
            );
            panic!("Invalid CFG")
        }
    }
}
