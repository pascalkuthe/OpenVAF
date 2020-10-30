/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use openvaf_data_structures::BitSet;
use openvaf_ir::{DoubleArgMath, SingleArgMath, UnaryOperator};
use openvaf_middle::cfg::{
    BasicBlock, BasicBlockData, CfgPass, ControlFlowGraph, Location, LocationKind, TerminatorKind,
};
use openvaf_middle::{
    impl_pass_span, BinOp, CallType, ComparisonOp, Local, LocalKind, Mir, RValue, StmntKind, Type,
};

#[cfg_attr(feature = "serde_dump", derive(serde::Serialize))]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Malformation {
    MissingTerminator,
    DstTypeMissmatch,
    OperandTypeMissmatch,
    UndefinedCast,
    DoubleWriteToTemporary,
    PhiTakingNonPredecessor(BasicBlock),
    PhiMissingPredecessor(BasicBlock),
    UnknownLocal { local: usize, last_local: usize },
}

#[cfg_attr(feature = "serde_dump", derive(serde::Serialize))]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MalformedCfg {
    location: Location,
    error: Malformation,
}

pub struct Verify<'a, A: CallType>(pub &'a Mir<A>);

impl<'a, A: CallType, C: CallType> CfgPass<'_, C> for Verify<'a, A> {
    type Result = Vec<MalformedCfg>;

    fn run(self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
        let mut verify = VerifyImpl {
            cfg,
            mir: self.0,
            errors: Vec::new(),
            written_locals: BitSet::new_empty(cfg.locals.len_idx()),
            terminators_valid: true,
        };
        for (block, block_data) in cfg.blocks.iter_enumerated() {
            verify.verify_block(block, block_data);
        }
        if verify.terminators_valid {
            for (block, block_data) in cfg.blocks.iter_enumerated() {
                verify.verify_phis(block, block_data);
            }
        }
        verify.errors
    }

    impl_pass_span!("verify");
}

struct VerifyImpl<'a, C: CallType, A: CallType> {
    cfg: &'a ControlFlowGraph<C>,
    mir: &'a Mir<A>,
    errors: Vec<MalformedCfg>,
    written_locals: BitSet<Local>,
    terminators_valid: bool,
}

impl<'a, C: CallType, A: CallType> VerifyImpl<'a, C, A> {
    fn verify_phis(&mut self, block: BasicBlock, block_data: &BasicBlockData<C>) {
        let predecessor = self.cfg.predecessors(block);

        for (phi_id, phi) in block_data.phi_statements.iter_enumerated() {
            let ty = self.cfg.locals[phi.dst].ty;

            let location = Location {
                block,
                kind: LocationKind::Phi(phi_id),
            };

            for (block, local) in &phi.sources {
                if !predecessor.contains(&block) {
                    self.errors.push(MalformedCfg {
                        location,
                        error: Malformation::PhiTakingNonPredecessor(*block),
                    });
                }

                if self.cfg.locals[*local].ty != ty {
                    self.errors.push(MalformedCfg {
                        location,
                        error: Malformation::OperandTypeMissmatch,
                    });
                }
            }

            for predecessor in predecessor {
                if !phi.sources.contains_key(predecessor) {
                    self.errors.push(MalformedCfg {
                        location,
                        error: Malformation::PhiMissingPredecessor(block),
                    });
                }
            }

            self.verify_local_write(phi.dst, location)
        }
    }

    fn verify_block(&mut self, block: BasicBlock, block_data: &BasicBlockData<C>) {
        for (stmnt, (kind, _)) in block_data.statements.iter_enumerated() {
            match kind {
                StmntKind::Assignment(lhs, val) => {
                    let location = Location {
                        block,
                        kind: LocationKind::Statement(stmnt),
                    };

                    self.verify_local_write(*lhs, location);
                    self.verify_rvalue(val, self.cfg.locals[*lhs].ty, location)
                }
                StmntKind::Call(_, _, _) => {
                    // Calls can not be verified
                    // Should this be added to the trait (probably not thats small enough of a debug to be not my problem)
                }
                StmntKind::NoOp => {}
            }
        }

        let terminator_loc = Location {
            block,
            kind: LocationKind::Terminator,
        };

        if let Some(terminator) = &block_data.terminator {
            if let TerminatorKind::Split { condition, .. } = &terminator.kind {
                self.verify_rvalue(condition, Type::BOOL, terminator_loc);
            }
        } else {
            self.errors.push(MalformedCfg {
                location: terminator_loc,
                error: Malformation::MissingTerminator,
            });
            self.terminators_valid = false
        }
    }

    pub fn verify_local_write(&mut self, local: Local, location: Location) {
        if self.written_locals.put(local)
            && matches!(self.cfg.locals[local].kind, LocalKind::Temporary)
        {
            self.errors.push(MalformedCfg {
                location,
                error: Malformation::DoubleWriteToTemporary,
            })
        }
        if local > self.cfg.locals.last_idx() {
            self.errors.push(MalformedCfg {
                location,
                error: Malformation::UnknownLocal {
                    local: local.index(),
                    last_local: self.cfg.locals.last_idx().index(),
                },
            })
        }
    }

    pub fn verify_rvalue(&mut self, val: &RValue<C>, dst_ty: Type, location: Location) {
        let ty = match val {
            RValue::UnaryOperation(op, arg) => {
                match (op.contents, arg.contents.ty(self.mir, self.cfg)) {
                    (UnaryOperator::BitNegate, Type::INT)
                    | (UnaryOperator::ArithmeticNegate, Type::INT) => Type::INT,

                    (UnaryOperator::LogicNegate, Type::BOOL) => Type::BOOL,

                    (UnaryOperator::ArithmeticNegate, Type::REAL) => Type::REAL,
                    (_, _) => {
                        self.errors.push(MalformedCfg {
                            location,
                            error: Malformation::OperandTypeMissmatch,
                        });
                        return;
                    }
                }
            }
            RValue::BinaryOperation(op, lhs, rhs) => {
                let lhs_ty = lhs.contents.ty(self.mir, self.cfg);
                let rhs_ty = rhs.contents.ty(self.mir, self.cfg);
                if lhs_ty != rhs_ty {
                    self.errors.push(MalformedCfg {
                        location,
                        error: Malformation::OperandTypeMissmatch,
                    });
                    return;
                }

                match (op.contents, lhs_ty) {
                    (_, Type::INT) => Type::INT,
                    (BinOp::Plus, Type::REAL)
                    | (BinOp::Minus, Type::REAL)
                    | (BinOp::Multiply, Type::REAL)
                    | (BinOp::Divide, Type::REAL)
                    | (BinOp::Modulus, Type::REAL) => Type::REAL,

                    (BinOp::Xor, Type::BOOL)
                    | (BinOp::NXor, Type::BOOL)
                    | (BinOp::Or, Type::BOOL)
                    | (BinOp::And, Type::BOOL) => Type::BOOL,

                    _ => {
                        self.errors.push(MalformedCfg {
                            location,
                            error: Malformation::OperandTypeMissmatch,
                        });
                        return;
                    }
                }
            }

            RValue::SingleArgMath(op, arg) => {
                let ty = arg.contents.ty(self.mir, self.cfg);
                match (op.contents, ty) {
                    (SingleArgMath::Abs, Type::INT) => Type::INT,
                    (_, Type::REAL) => Type::REAL,
                    (_, _) => {
                        self.errors.push(MalformedCfg {
                            location,
                            error: Malformation::OperandTypeMissmatch,
                        });
                        return;
                    }
                }
            }
            RValue::DoubleArgMath(op, arg1, arg2) => {
                let arg1_ty = arg1.contents.ty(self.mir, self.cfg);
                let arg2_ty = arg2.contents.ty(self.mir, self.cfg);
                if arg1_ty != arg2_ty {
                    self.errors.push(MalformedCfg {
                        location,
                        error: Malformation::OperandTypeMissmatch,
                    });
                    return;
                }
                match (op.contents, arg1_ty) {
                    (DoubleArgMath::Min, Type::INT) | (DoubleArgMath::Max, Type::INT) => Type::INT,
                    (_, Type::REAL) => Type::REAL,
                    (_, _) => {
                        self.errors.push(MalformedCfg {
                            location,
                            error: Malformation::OperandTypeMissmatch,
                        });
                        return;
                    }
                }
            }

            RValue::Comparison(op, lhs, rhs, ty) => {
                let lhs_ty = lhs.contents.ty(self.mir, self.cfg);
                let rhs_ty = rhs.contents.ty(self.mir, self.cfg);
                if lhs_ty != rhs_ty || *ty != lhs_ty {
                    self.errors.push(MalformedCfg {
                        location,
                        error: Malformation::OperandTypeMissmatch,
                    });
                    return;
                }

                match (op.contents, lhs_ty) {
                    (ComparisonOp::LessEqual, Type::INT)
                    | (ComparisonOp::LessThen, Type::INT)
                    | (ComparisonOp::GreaterEqual, Type::INT)
                    | (ComparisonOp::GreaterThen, Type::INT)
                    | (ComparisonOp::LessEqual, Type::REAL)
                    | (ComparisonOp::LessThen, Type::REAL)
                    | (ComparisonOp::GreaterEqual, Type::REAL)
                    | (ComparisonOp::GreaterThen, Type::REAL) => Type::BOOL,

                    (ComparisonOp::Equal, _) | (ComparisonOp::NotEqual, _) => Type::BOOL,

                    (_, _) => {
                        self.errors.push(MalformedCfg {
                            location,
                            error: Malformation::OperandTypeMissmatch,
                        });
                        return;
                    }
                }
            }

            RValue::Select(cond, true_val, false_val) => {
                if cond.contents.ty(self.mir, self.cfg) != Type::BOOL {
                    self.errors.push(MalformedCfg {
                        location,
                        error: Malformation::OperandTypeMissmatch,
                    })
                }

                let true_val_ty = true_val.contents.ty(self.mir, self.cfg);
                let false_val_ty = false_val.contents.ty(self.mir, self.cfg);
                if true_val_ty != false_val_ty {
                    self.errors.push(MalformedCfg {
                        location,
                        error: Malformation::OperandTypeMissmatch,
                    });
                    return;
                }

                true_val_ty
            }

            RValue::Cast(arg) => {
                let arg_ty = arg.contents.ty(self.mir, self.cfg);
                if !matches!(
                    (arg_ty, dst_ty),
                    (Type::REAL, Type::INT)
                        | (Type::INT, Type::REAL)
                        | (Type::BOOL, Type::INT)
                        | (Type::BOOL, Type::REAL)
                ) {
                    self.errors.push(MalformedCfg {
                        location,
                        error: Malformation::UndefinedCast,
                    });
                }
                return;
            }

            RValue::Use(op) => op.contents.ty(self.mir, self.cfg),

            RValue::Call(_, _, _) => {
                // assume calls are always correct?
                return;
            }

            RValue::Array(arr, _) => {
                let mut iter = arr.iter();
                let ty = iter
                    .next()
                    .expect("Empty arrys are not supported")
                    .contents
                    .ty(self.mir, self.cfg);
                if iter.any(|x| x.contents.ty(self.mir, self.cfg) != ty) {
                    self.errors.push(MalformedCfg {
                        location,
                        error: Malformation::OperandTypeMissmatch,
                    })
                }
                ty
            }
        };

        if dst_ty != ty {
            self.errors.push(MalformedCfg {
                location,
                error: Malformation::DstTypeMissmatch,
            })
        }
    }
}
