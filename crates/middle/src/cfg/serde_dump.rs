/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::cfg::{BasicBlock, ControlFlowGraph, Phi, PhiData, TerminatorKind};
use crate::{
    BinOp, COperand, CallType, ComparisonOp, Local, LocalKind, Mir, OperandData, RValue,
    StatementId, StmntKind, VariableLocalKind,
};
use openvaf_ir::UnaryOperator;
use serde::ser::{SerializeSeq, SerializeStruct};
use serde::{Serialize, Serializer};

pub struct CfgDump<'a, C: CallType, A: CallType> {
    pub cfg: &'a ControlFlowGraph<C>,
    pub mir: &'a Mir<A>,
    pub blocks_in_resverse_postorder: bool,
}

impl<'a, C: CallType, A: CallType> Serialize for CfgDump<'a, C, A> {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        if self.blocks_in_resverse_postorder {
            self.serialize_transveral(serializer, self.cfg.reverse_postorder())
        } else {
            self.serialize_transveral(serializer, self.cfg.blocks.indices())
        }
    }
}

impl<'a, C: CallType, A: CallType> CfgDump<'a, C, A> {
    fn serialize_transveral<S>(
        &self,
        serializer: S,
        transversal: impl Iterator<Item = BasicBlock> + ExactSizeIterator,
    ) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        let mut serializer = serializer.serialize_seq(Some(transversal.len()))?;

        for bb in transversal {
            serializer.serialize_element(&BbDump {
                bb,
                cfg: self.cfg,
                mir: self.mir,
            })?
        }

        serializer.end()
    }
}

struct BbDump<'a, C: CallType, A: CallType> {
    bb: BasicBlock,
    cfg: &'a ControlFlowGraph<C>,
    mir: &'a Mir<A>,
}

impl<'a, C: CallType, A: CallType> Serialize for BbDump<'a, C, A> {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        let mut serializer = serializer.serialize_struct("BasicBlock", 3)?;
        serializer.serialize_field("block", &self.bb.index())?;
        serializer.serialize_field(
            "body",
            &BbStatementDump {
                bb: self.bb,
                cfg: self.cfg,
                mir: self.mir,
            },
        )?;
        serializer.serialize_field(
            "terminator",
            &TerminatorDump {
                bb: self.bb,
                cfg: self.cfg,
                mir: self.mir,
            },
        )?;

        serializer.end()
    }
}

struct TerminatorDump<'a, C: CallType, A: CallType> {
    bb: BasicBlock,
    cfg: &'a ControlFlowGraph<C>,
    mir: &'a Mir<A>,
}
impl<'a, C: CallType, A: CallType> Serialize for TerminatorDump<'a, C, A> {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        match self.cfg[self.bb].terminator.as_ref().map(|x| &x.kind) {
            Some(TerminatorKind::Goto(dst)) => {
                let mut serializer = serializer.serialize_struct("GOTO", 1)?;
                serializer.serialize_field("goto", &dst)?;
                serializer.end()
            }
            Some(TerminatorKind::Split {
                condition,
                true_block,
                false_block,
                loop_head,
            }) => {
                let mut serializer = serializer.serialize_struct("SPLIT", 3)?;
                serializer.serialize_field(
                    "cond",
                    &RValDump {
                        rval: condition,
                        cfg: self.cfg,
                        mir: self.mir,
                    },
                )?;

                serializer.serialize_field("true_block", &true_block)?;
                serializer.serialize_field("false_block", &false_block)?;
                serializer.serialize_field("loop", loop_head)?;

                serializer.end()
            }
            Some(TerminatorKind::End) => serializer.serialize_str("End"),
            None => serializer.serialize_str("MISSING"),
        }
    }
}

struct BbStatementDump<'a, C: CallType, A: CallType> {
    bb: BasicBlock,
    cfg: &'a ControlFlowGraph<C>,
    mir: &'a Mir<A>,
}

impl<'a, C: CallType, A: CallType> Serialize for BbStatementDump<'a, C, A> {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        let phi_statements = &self.cfg.blocks[self.bb].phi_statements;
        let statements = &self.cfg.blocks[self.bb].statements;

        let mut serializer =
            serializer.serialize_seq(Some(statements.len() + phi_statements.len()))?;

        for (id, phi) in phi_statements.iter_enumerated() {
            serializer.serialize_element(&PhiDump {
                phi,
                id,
                cfg: self.cfg,
                mir: self.mir,
            })?;
        }

        for (id, (stmnt, _)) in statements.iter_enumerated() {
            serializer.serialize_element(&StmntDump {
                stmnt,
                id,
                cfg: self.cfg,
                mir: self.mir,
            })?;
        }

        serializer.end()
    }
}

struct PhiDump<'a, C: CallType, A: CallType> {
    phi: &'a PhiData,
    id: Phi,
    cfg: &'a ControlFlowGraph<C>,
    mir: &'a Mir<A>,
}

impl<'a, C: CallType, A: CallType> Serialize for PhiDump<'a, C, A> {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        let mut serializer = serializer.serialize_struct("phi", 3)?;
        serializer.serialize_field("phi", &self.id.index())?;
        serializer.serialize_field(
            "dst",
            &LocalDump {
                local: self.phi.dst,
                mir: &self.mir,
                cfg: &self.cfg,
            },
        )?;

        serializer.serialize_field(
            "sources",
            &PhiSourcesDump {
                phi: self.phi,
                mir: &self.mir,
                cfg: &self.cfg,
            },
        )?;

        serializer.end()
    }
}

struct PhiSourcesDump<'a, C: CallType, A: CallType> {
    phi: &'a PhiData,
    cfg: &'a ControlFlowGraph<C>,
    mir: &'a Mir<A>,
}

impl<'a, C: CallType, A: CallType> Serialize for PhiSourcesDump<'a, C, A> {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        let mut serializer = serializer.serialize_seq(Some(self.phi.sources.len()))?;

        for (&block, &local) in self.phi.sources.iter() {
            serializer.serialize_element(&PhiSourceDump {
                block,
                local,
                cfg: self.cfg,
                mir: self.mir,
            })?;
        }

        serializer.end()
    }
}

struct PhiSourceDump<'a, C: CallType, A: CallType> {
    block: BasicBlock,
    local: Local,
    cfg: &'a ControlFlowGraph<C>,
    mir: &'a Mir<A>,
}

impl<'a, C: CallType, A: CallType> Serialize for PhiSourceDump<'a, C, A> {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        let mut serializer = serializer.serialize_struct("PhiSource", 2)?;

        serializer.serialize_field(
            "local",
            &LocalDump {
                local: self.local,
                cfg: self.cfg,
                mir: self.mir,
            },
        )?;

        serializer.serialize_field("block", &self.block)?;

        serializer.end()
    }
}

struct StmntDump<'a, C: CallType, A: CallType> {
    stmnt: &'a StmntKind<C>,
    id: StatementId,
    cfg: &'a ControlFlowGraph<C>,
    mir: &'a Mir<A>,
}

impl<'a, C: CallType, A: CallType> Serialize for StmntDump<'a, C, A> {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        match *self.stmnt {
            StmntKind::NoOp => serializer.serialize_str("noop"),
            StmntKind::Assignment(dst, ref val) => {
                let mut serializer = serializer.serialize_struct("assign", 3)?;
                serializer.serialize_field("stmnt", &self.id.index())?;
                serializer.serialize_field(
                    "dst",
                    &LocalDump {
                        local: dst,
                        mir: &self.mir,
                        cfg: &self.cfg,
                    },
                )?;

                serializer.serialize_field(
                    "val",
                    &RValDump {
                        rval: val,
                        mir: &self.mir,
                        cfg: &self.cfg,
                    },
                )?;

                serializer.end()
            }
            StmntKind::Call(ref call, ref args, _) => {
                let mut serializer = serializer.serialize_struct("call", 2)?;
                serializer.serialize_field("info", &format!("{:?}", call))?;
                serializer.serialize_field(
                    "args",
                    &OperandArrayDump {
                        arr: &args.raw,
                        cfg: self.cfg,
                        mir: self.mir,
                    },
                )?;
                serializer.end()
            }
        }
    }
}

struct RValDump<'a, C: CallType, A: CallType> {
    rval: &'a RValue<C>,
    cfg: &'a ControlFlowGraph<C>,
    mir: &'a Mir<A>,
}

impl<'a, C: CallType, A: CallType> Serialize for RValDump<'a, C, A> {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        match self.rval {
            RValue::UnaryOperation(op, arg) => {
                let name = match op.contents {
                    UnaryOperator::BitNegate => "bitnot",
                    UnaryOperator::LogicNegate => "not",
                    UnaryOperator::ArithmeticNegate => "minus",
                };
                let mut serializer = serializer.serialize_struct("UnaryOp", 2)?;

                serializer.serialize_field("operation", name)?;

                serializer.serialize_field(
                    "arg",
                    &OperandDump {
                        op: &arg.contents,
                        cfg: self.cfg,
                        mir: self.mir,
                    },
                )?;

                serializer.end()
            }

            RValue::Comparison(op, lhs, rhs, ty) => {
                let name = match op.contents {
                    ComparisonOp::LessThen => "lt",
                    ComparisonOp::LessEqual => "le",
                    ComparisonOp::GreaterThen => "gt",
                    ComparisonOp::GreaterEqual => "ge",
                    ComparisonOp::Equal => "eq",
                    ComparisonOp::NotEqual => "ne",
                };
                let mut serializer = serializer.serialize_struct("Comparison", 3)?;

                serializer.serialize_field("operation", &name)?;

                serializer.serialize_field(
                    "lhs",
                    &OperandDump {
                        op: &lhs.contents,
                        cfg: self.cfg,
                        mir: self.mir,
                    },
                )?;

                serializer.serialize_field(
                    "rhs",
                    &OperandDump {
                        op: &rhs.contents,
                        cfg: self.cfg,
                        mir: self.mir,
                    },
                )?;

                serializer.serialize_field("ty", ty)?;

                serializer.end()
            }

            RValue::BinaryOperation(op, lhs, rhs) => {
                let name = match op.contents {
                    BinOp::Plus => "plus",
                    BinOp::Minus => "minus",
                    BinOp::Multiply => "mul",
                    BinOp::Divide => "div",
                    BinOp::Modulus => "mod",
                    BinOp::ShiftLeft => "shiftl",
                    BinOp::ShiftRight => "shiftr",
                    BinOp::Xor => "xor",
                    BinOp::NXor => "nxor",
                    BinOp::And => "and",
                    BinOp::Or => "or",
                };

                let mut serializer = serializer.serialize_struct("BinOp", 3)?;

                serializer.serialize_field("operation", &name)?;

                serializer.serialize_field(
                    "lhs",
                    &OperandDump {
                        op: &lhs.contents,
                        cfg: self.cfg,
                        mir: self.mir,
                    },
                )?;

                serializer.serialize_field(
                    "rhs",
                    &OperandDump {
                        op: &rhs.contents,
                        cfg: self.cfg,
                        mir: self.mir,
                    },
                )?;

                serializer.end()
            }

            RValue::SingleArgMath(func, arg) => {
                let mut serializer = serializer.serialize_struct("SingleArgMath", 3)?;

                serializer.serialize_field("operation", func.contents.name())?;

                serializer.serialize_field(
                    "arg",
                    &OperandDump {
                        op: &arg.contents,
                        cfg: self.cfg,
                        mir: self.mir,
                    },
                )?;

                serializer.end()
            }

            RValue::DoubleArgMath(func, arg1, arg2) => {
                let mut serializer = serializer.serialize_struct("DoubleArgMath", 3)?;
                serializer.serialize_field("operation", &func.contents.name())?;

                serializer.serialize_field(
                    "arg1",
                    &OperandDump {
                        op: &arg1.contents,
                        cfg: self.cfg,
                        mir: self.mir,
                    },
                )?;

                serializer.serialize_field(
                    "arg2",
                    &OperandDump {
                        op: &arg2.contents,
                        cfg: self.cfg,
                        mir: self.mir,
                    },
                )?;

                serializer.end()
            }

            RValue::Select(cond, true_val, false_val) => {
                let mut serializer = serializer.serialize_struct("Select", 4)?;
                serializer.serialize_field("operation", "select")?;

                serializer.serialize_field(
                    "cond",
                    &OperandDump {
                        op: &cond.contents,
                        cfg: self.cfg,
                        mir: self.mir,
                    },
                )?;

                serializer.serialize_field(
                    "true",
                    &OperandDump {
                        op: &true_val.contents,
                        cfg: self.cfg,
                        mir: self.mir,
                    },
                )?;

                serializer.serialize_field(
                    "false",
                    &OperandDump {
                        op: &false_val.contents,
                        cfg: self.cfg,
                        mir: self.mir,
                    },
                )?;

                serializer.end()
            }
            RValue::Cast(arg) => {
                let mut serializer = serializer.serialize_struct("UnaryOp", 2)?;

                serializer.serialize_field("operation", "cast")?;

                serializer.serialize_field(
                    "arg",
                    &OperandDump {
                        op: &arg.contents,
                        cfg: self.cfg,
                        mir: self.mir,
                    },
                )?;

                serializer.end()
            }

            RValue::Use(op) => OperandDump {
                op: &op.contents,
                cfg: self.cfg,
                mir: self.mir,
            }
            .serialize(serializer),

            RValue::Call(call, args, _) => {
                let mut serializer = serializer.serialize_struct("call", 2)?;
                serializer.serialize_field("call", &format!("{:?}", call))?;
                serializer.serialize_field(
                    "args",
                    &OperandArrayDump {
                        arr: &args.raw,
                        cfg: self.cfg,
                        mir: self.mir,
                    },
                )?;
                serializer.end()
            }

            RValue::Array(arr, _) => OperandArrayDump {
                arr: &arr,
                cfg: self.cfg,
                mir: self.mir,
            }
            .serialize(serializer),
        }
    }
}

struct OperandArrayDump<'a, C: CallType, A: CallType> {
    arr: &'a [COperand<C>],
    cfg: &'a ControlFlowGraph<C>,
    mir: &'a Mir<A>,
}

impl<'a, C: CallType, A: CallType> Serialize for OperandArrayDump<'a, C, A> {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        let mut serializer = serializer.serialize_seq(Some(self.arr.len()))?;
        for op in self.arr {
            serializer.serialize_element(&OperandDump {
                op: &op.contents,
                cfg: self.cfg,
                mir: self.mir,
            })?;
        }
        serializer.end()
    }
}

struct OperandDump<'a, C: CallType, A: CallType> {
    op: &'a OperandData<<C as CallType>::I>,
    cfg: &'a ControlFlowGraph<C>,
    mir: &'a Mir<A>,
}

impl<'a, C: CallType, A: CallType> Serialize for OperandDump<'a, C, A> {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        match self.op {
            OperandData::Constant(val) => val.serialize(serializer),
            OperandData::Copy(local) => LocalDump {
                local: *local,
                cfg: self.cfg,
                mir: self.mir,
            }
            .serialize(serializer),

            OperandData::Read(input) => serializer.serialize_str(&format!("{:?}", input)),
        }
    }
}
struct LocalDump<'a, C: CallType, A: CallType> {
    local: Local,
    cfg: &'a ControlFlowGraph<C>,
    mir: &'a Mir<A>,
}

impl<'a, C: CallType, X: CallType> Serialize for LocalDump<'a, C, X> {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        match self.cfg.locals[self.local].kind {
            LocalKind::Variable(var, VariableLocalKind::Derivative) => {
                serializer.serialize_str(&format!("__DERIVATIVE__{}", self.mir[var].ident))
            }
            LocalKind::Variable(var, VariableLocalKind::User) => serializer
                .serialize_str(&format!("{} (local: {})", self.mir[var].ident, self.local)),
            LocalKind::Branch(access, branch) => serializer.serialize_str(&format!(
                "{}({}) (local: {})",
                access, self.mir[branch].ident, self.local
            )),
            LocalKind::Temporary => {
                let mut serializer = serializer.serialize_struct("temporary", 2)?;
                serializer.serialize_field("temporary", &self.local.index())?;
                serializer.serialize_field("ty", &self.cfg.locals[self.local].ty)?;
                serializer.end()
            }
        }
    }
}
