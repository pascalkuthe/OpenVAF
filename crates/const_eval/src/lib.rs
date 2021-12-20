#![allow(clippy::float_cmp)]

use std::fmt::Debug;
use std::mem::size_of_val;
use std::ops::{Add, Div, Mul, Neg, Not, RangeInclusive, Sub};

use ahash::AHashMap;
use cfg::{CfgParam, Complex64, Const, ControlFlowGraph, Op, Operand, Place, Spur};
use data_flow::lattice::{FlatSet, SparseFlatSetMap};
use data_flow::{Analysis, Results};

use crate::propagation::{ConditionalConstPropagation, WriteBackConsts};
use crate::ssa_constants::SsaConstants;

pub mod propagation;
pub mod ssa_constants;

#[cfg(test)]
mod tests;

pub type ConstPlaces = SparseFlatSetMap<Place, Const>;

pub struct EvalCtx<'a> {
    const_places: &'a ConstPlaces,
    ssa_consts: &'a SsaConstants,
    known_params: &'a AHashMap<CfgParam, Const>,
}

fn real_to_int_checked(val: f64) -> Option<i32> {
    const VALID_RANGE: RangeInclusive<f64> = (i32::MIN as f64)..=(i32::MAX as f64);
    let val = val.round();
    if VALID_RANGE.contains(&val) {
        Some(val as i32)
    } else {
        None
    }
}

impl EvalCtx<'_> {
    pub fn eval_op(&mut self, op: Op, args: &[Operand], src: i32) -> FlatSet<Const> {
        match op {
            Op::Copy => self.get_operand(&args[0]),
            Op::IntBitNegate => self.op_1(i32::not, args),
            Op::BoolBitNegate => self.op_1(bool::not, args),
            Op::RealArtihNeg => self.op_1(f64::neg, args),
            Op::IntArithNeg => self.op_1_checked(i32::checked_neg, args),
            Op::RealToInt => self.op_1_checked(real_to_int_checked, args),
            Op::IntToReal => self.op_1(|val: i32| val as f64, args),
            Op::BoolToReal => self.op_1(|val: bool| val as i32 as f64, args),
            Op::BoolToInt => self.op_1(|val: bool| val as i32, args),
            Op::IntToBool => self.op_1(|val: i32| val != 0, args),
            Op::RealToComplex => self.op_1(|val| Complex64 { real: val, imag: 0.0 }, args),
            Op::RealComponent => self.op_1(|val: Complex64| val.real, args),
            Op::ImagComponent => self.op_1(|val: Complex64| val.imag, args),
            Op::IntAdd => self.op_2_checked(i32::checked_add, args),
            Op::IntSub if args[0] == args[1] => FlatSet::Elem(0i32.into()),
            Op::IntSub => self.op_2_checked(i32::checked_sub, args),
            Op::IntMul => self.op_2_checked(i32::checked_mul, args),
            Op::IntDiv if args[0] == args[1] => FlatSet::Elem(1i32.into()),
            Op::IntDiv => self.op_2_checked(i32::checked_div, args),
            Op::IntRem => self.op_2_checked(i32::checked_rem, args),
            Op::IntShl => self.op_2_checked(|lhs: i32, rhs: i32| lhs.checked_shl(rhs as u32), args),
            Op::IntShr => self.op_2_checked(|lhs: i32, rhs: i32| lhs.checked_shl(rhs as u32), args),
            Op::IntXor => self.op_2(|lhs: i32, rhs: i32| lhs ^ rhs, args),
            Op::IntNXor => self.op_2(|lhs: i32, rhs: i32| !(lhs ^ rhs), args),
            Op::IntAnd => self.op_2(|lhs: i32, rhs: i32| lhs & rhs, args),
            Op::IntOr => self.op_2(|lhs: i32, rhs: i32| lhs | rhs, args),
            Op::RealAdd => self.op_2(|lhs: f64, rhs: f64| lhs + rhs, args),
            Op::RealSub if args[0] == args[1] && src < 0 => FlatSet::Elem(0f64.into()),
            Op::RealSub => self.op_2(|lhs: f64, rhs: f64| lhs - rhs, args),
            Op::RealMul => self.f64_mul_short_cricuit(args, src),
            Op::RealDiv if args[0] == args[1] && src < 0 => FlatSet::Elem(1f64.into()),
            Op::RealDiv => self.f64_div_short_cricuit(args, src),
            Op::RealRem => self.op_2(|lhs: f64, rhs: f64| lhs % rhs, args),
            Op::CmplxPlus => self.op_2(Complex64::add, args),
            Op::CmplxMinus => self.op_2(Complex64::sub, args),
            Op::CmplxMul => self.op_2(Complex64::mul, args),
            Op::CmplxDiv => self.op_2(Complex64::div, args),
            Op::IntLessThen => self.op_2(|lhs: i32, rhs: i32| lhs < rhs, args),
            Op::IntGreaterThen => self.op_2(|lhs: i32, rhs: i32| lhs > rhs, args),
            Op::RealLessThen => self.op_2(|lhs: f64, rhs: f64| lhs < rhs, args),
            Op::RealGreaterThen => self.op_2(|lhs: f64, rhs: f64| lhs > rhs, args),
            Op::IntLessEqual => self.op_2(|lhs: i32, rhs: i32| lhs <= rhs, args),
            Op::IntGreaterEqual => self.op_2(|lhs: i32, rhs: i32| lhs >= rhs, args),
            Op::RealLessEqual => self.op_2(|lhs: f64, rhs: f64| lhs <= rhs, args),
            Op::RealGreaterEqual => self.op_2(|lhs: f64, rhs: f64| lhs >= rhs, args),
            Op::IntEq => self.op_2(|lhs: i32, rhs: i32| lhs == rhs, args),
            Op::RealEq => self.op_2(|lhs: f64, rhs: f64| lhs == rhs, args),
            Op::StringEq => self.op_2(|lhs: Spur, rhs: Spur| lhs == rhs, args),
            Op::BoolEq => self.op_2(|lhs: bool, rhs: bool| lhs == rhs, args),
            Op::IntNeq => self.op_2(|lhs: i32, rhs: i32| lhs != rhs, args),
            Op::RealNeq => self.op_2(|lhs: f64, rhs: f64| lhs != rhs, args),
            Op::StringNeq => self.op_2(|lhs: Spur, rhs: Spur| lhs != rhs, args),
            Op::BoolNeq => self.op_2(|lhs: bool, rhs: bool| lhs != rhs, args),
            Op::Sqrt => self.op_1(f64::sqrt, args),
            Op::Exp => self.op_1(f64::exp, args),
            Op::Ln => self.op_1(f64::ln, args),
            Op::Log => self.op_1(f64::log10, args),
            Op::Clog2 => self
                .op_1(|val: i32| 8 * size_of_val(&val) as i32 - val.leading_zeros() as i32, args),
            Op::Floor => self.op_1(f64::floor, args),
            Op::Ceil => self.op_1(f64::ceil, args),
            Op::Sin => self.op_1(f64::sin, args),
            Op::Cos => self.op_1(f64::cos, args),
            Op::Tan => self.op_1(f64::tan, args),
            Op::Hypot => self.op_2(f64::hypot, args),
            Op::ArcSin => self.op_1(f64::asin, args),
            Op::ArcCos => self.op_1(f64::acos, args),
            Op::ArcTan => self.op_1(f64::atan, args),
            Op::ArcTan2 => self.op_2(f64::atan2, args),
            Op::SinH => self.op_1(f64::acos, args),
            Op::CosH => self.op_1(f64::cosh, args),
            Op::TanH => self.op_1(f64::tanh, args),
            Op::ArcSinH => self.op_1(f64::asinh, args),
            Op::ArcCosH => self.op_1(f64::acosh, args),
            Op::ArcTanH => self.op_1(f64::atanh, args),
            Op::RealPow => self.op_2(f64::powf, args),
            // TODO const eval call?
            Op::Call(_) => FlatSet::Top,
            Op::NoOp => FlatSet::Top,
        }
    }

    fn get_operand(&self, op: &Operand) -> FlatSet<Const> {
        match *op {
            Operand::Const(ref c) => FlatSet::Elem(c.clone()),
            // TOOD avoid clones?
            Operand::Local(local) => self.ssa_consts.get(local),
            Operand::Place(place) => self.const_places.get_cloned_flat_set(place),
            Operand::CfgParam(param) => {
                self.known_params.get(&param).cloned().map_or(FlatSet::Top, FlatSet::Elem)
            }
        }
    }

    fn op_1_checked<Arg, Ret>(
        &mut self,
        f: impl FnOnce(Arg) -> Option<Ret>,
        args: &[Operand],
    ) -> FlatSet<Const>
    where
        Arg: TryFrom<Const> + Debug,
        Arg::Error: Debug,
        Ret: Into<Const>,
    {
        self.get_operand(&args[0]).and_then(|c| match f(c.try_into().unwrap()) {
            Some(c) => FlatSet::Elem(c.into()),
            None => {
                // self.overflows.push((src, vec![c]));
                // TODO report this
                FlatSet::Top
            }
        })
    }

    fn op_1<Arg, Ret>(&self, f: impl FnOnce(Arg) -> Ret, args: &[Operand]) -> FlatSet<Const>
    where
        Arg: TryFrom<Const> + Debug,
        Arg::Error: Debug,
        Ret: Into<Const>,
    {
        self.get_operand(&args[0]).map(|c| f(c.try_into().unwrap()).into())
    }

    fn op_2_checked<Arg1, Arg2, Ret>(
        &mut self,
        f: impl FnOnce(Arg1, Arg2) -> Option<Ret>,
        args: &[Operand],
    ) -> FlatSet<Const>
    where
        Arg1: TryFrom<Const> + Debug,
        Arg1::Error: Debug,
        Arg2: TryFrom<Const> + Debug,
        Arg2::Error: Debug,
        Ret: Into<Const>,
    {
        match (self.get_operand(&args[0]), self.get_operand(&args[1])) {
            (FlatSet::Elem(arg1), FlatSet::Elem(arg2)) => {
                match f(arg1.try_into().unwrap(), arg2.try_into().unwrap()) {
                    Some(c) => FlatSet::Elem(c.into()),
                    None => {
                        // TODO repor this somewhere
                        // self.overflows.push((src, vec![arg1, arg2]));
                        FlatSet::Top
                    }
                }
            }

            (FlatSet::Top, _) | (_, FlatSet::Top) => FlatSet::Top,
            _ => FlatSet::Bottom,
        }
    }

    fn op_2<Arg1, Arg2, Ret>(
        &self,
        f: impl FnOnce(Arg1, Arg2) -> Ret,
        args: &[Operand],
    ) -> FlatSet<Const>
    where
        Arg1: TryFrom<Const> + Debug,
        Arg1::Error: Debug,
        Arg2: TryFrom<Const> + Debug,
        Arg2::Error: Debug,
        Ret: Into<Const>,
    {
        match (self.get_operand(&args[0]), self.get_operand(&args[1])) {
            (FlatSet::Elem(arg1), FlatSet::Elem(arg2)) => {
                let res = f(arg1.try_into().unwrap(), arg2.try_into().unwrap());
                FlatSet::Elem(res.into())
            }

            (FlatSet::Top, _) | (_, FlatSet::Top) => FlatSet::Top,
            _ => FlatSet::Bottom,
        }
    }

    // These are two optimizations that ignore NANs and always propagate 0.0
    // While this is usually not a good idea for auto diff this is an essential optimizations
    // Since that code is not handwritten anywau for good rounding behaviour/nan handling we
    // propagate the zeros in that case

    fn f64_mul_short_cricuit(&self, args: &[Operand], src: i32) -> FlatSet<Const> {
        match (self.get_operand(&args[0]), self.get_operand(&args[1])) {
            (FlatSet::Elem(val), _) | (_, FlatSet::Elem(val))
                if src < 0 && val.clone().unwrap_real() == 0f64 =>
            {
                FlatSet::Elem(0f64.into())
            }

            (FlatSet::Elem(lhs), FlatSet::Elem(rhs)) => {
                let lhs: f64 = lhs.try_into().unwrap();
                let rhs: f64 = rhs.try_into().unwrap();
                FlatSet::Elem((lhs * rhs).into())
            }

            (FlatSet::Top, _) | (_, FlatSet::Top) => FlatSet::Top,
            _ => FlatSet::Bottom,
        }
    }

    fn f64_div_short_cricuit(&self, args: &[Operand], src: i32) -> FlatSet<Const> {
        match (self.get_operand(&args[0]), self.get_operand(&args[1])) {
            (FlatSet::Elem(val), _) if src < 0 && val.clone().unwrap_real() == 0f64 => {
                FlatSet::Elem(0f64.into())
            }

            (FlatSet::Elem(lhs), FlatSet::Elem(rhs)) => {
                let lhs: f64 = lhs.try_into().unwrap();
                let rhs: f64 = rhs.try_into().unwrap();
                FlatSet::Elem((lhs / rhs).into())
            }

            (FlatSet::Top, _) | (_, FlatSet::Top) => FlatSet::Top,
            _ => FlatSet::Bottom,
        }
    }
}

pub fn conditional_const_propagation<'a>(
    cfg: &mut ControlFlowGraph,
    known_params: &'a AHashMap<CfgParam, Const>,
) -> Results<ConditionalConstPropagation<'a>> {
    let res = ConditionalConstPropagation {
        ssa_consts: SsaConstants::new(cfg.next_local.into()),
        known_params,
    }
    .into_engine(cfg)
    .iterate_to_fixpoint();
    res.visit_with_mut(cfg, &mut WriteBackConsts(&res.analysis));
    res
}
