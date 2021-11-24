use cfg::{Const, ControlFlowGraph, Local, Op, Operand};
use data_flow::lattice::FlatSet;

use crate::{ssa_constants::SsaConstants, BlockConsts, GlobalConsts};

pub struct EvalCtx<'a> {
    cfg: &'a ControlFlowGraph,
    block_consts: &'a BlockConsts,
    ssa_consts: &'a SsaConstants,
}
impl EvalCtx {
    pub fn eval_op(&self, op: Op, args: &[Operand]) -> FlatSet<Const> {
        let res = match op {
            Op::Mov => args[0],
            Op::IntBitNegate => todo!(),
            Op::BoolBitNegate => todo!(),
            Op::RealArtihNeg => todo!(),
            Op::IntArithNeg => todo!(),
            Op::RealToInt => todo!(),
            Op::IntToReal => todo!(),
            Op::BoolToInt => todo!(),
            Op::IntToBool => todo!(),
            Op::RealToComplex => todo!(),
            Op::RealComponent => todo!(),
            Op::ImagComponent => todo!(),
            Op::IntPlus => todo!(),
            Op::IntMinus => todo!(),
            Op::IntMul => todo!(),
            Op::IntDiv => todo!(),
            Op::IntRem => todo!(),
            Op::IntShl => todo!(),
            Op::IntShr => todo!(),
            Op::IntXor => todo!(),
            Op::IntNXor => todo!(),
            Op::IntAnd => todo!(),
            Op::IntOr => todo!(),
            Op::RealPlus => todo!(),
            Op::RealMinus => todo!(),
            Op::RealMul => todo!(),
            Op::RealDiv => todo!(),
            Op::CmplxPlus => todo!(),
            Op::CmplxMinus => todo!(),
            Op::CmplxMul => todo!(),
            Op::CmplxDiv => todo!(),
            Op::Select => todo!(),
            Op::CreateRealArray => todo!(),
            Op::CreateIntArray => todo!(),
            Op::CreateStringArray => todo!(),
            Op::CreateComplexArray => todo!(),
            Op::Sqrt => todo!(),
            Op::Exp => todo!(),
            Op::LimExp => todo!(),
            Op::Ln => todo!(),
            Op::Log => todo!(),
            Op::Floor => todo!(),
            Op::Ceil => todo!(),
            Op::RealAbs => todo!(),
            Op::RealPow => todo!(),
            Op::Sin => todo!(),
            Op::Cos => todo!(),
            Op::Tan => todo!(),
            Op::Hypot => todo!(),
            Op::ArcSin => todo!(),
            Op::ArcCos => todo!(),
            Op::ArcTan => todo!(),
            Op::ArcTan2 => todo!(),
            Op::SinH => todo!(),
            Op::CosH => todo!(),
            Op::TanH => todo!(),
            Op::ArcSinH => todo!(),
            Op::ArcCosH => todo!(),
            Op::ArcTanH => todo!(),
            Op::RealMin => todo!(),
            Op::RealMax => todo!(),
            Op::IntAbs => todo!(),
            Op::IntPow => todo!(),
            Op::IntMin => todo!(),
            Op::IntMax => todo!(),
            Op::Call(_) => todo!(),
        };
    }

    fn get_operand(op: &Operand) -> FlatSet<Const> {
        match op {
            Operand::Const(c) => FlatSet::Elem(c),
            Operand::Copy(local) => self.get_local(local),
            Operand::Read(_) => FlatSet::Top,
        }
    }

    fn get_local(&self, local: Local) -> FlatSet<Const> {
        match self.cfg.locals[local].place {
            Some(place) => self.block_consts.get_cloned_flat_set(place),
            None => match self.ssa_consts.get(local) {
                Some(val) => FlatSet::Elem(val),
                None => FlatSet::Bottom,
            },
        }
    }
}
