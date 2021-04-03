/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

#![allow(clippy::float_cmp)]

use crate::{
    fold_rvalue, COperand, CallArg, CallType, ConstVal, Local, OperandData, RValue, RValueFold,
};
use openvaf_data_structures::index_vec::IndexSlice;
use openvaf_session::sourcemap::Span;
use osdi_types::ConstVal::Scalar;
use osdi_types::SimpleConstVal::{Bool, Integer, Real};
use osdi_types::Type;
use std::borrow::{Borrow, BorrowMut};
use std::fmt::Formatter;
use std::fmt::{Debug, Display};
use std::marker::PhantomData;
use std::ops::{Add, BitAnd, BitOr, BitXor, Neg, Rem, Shl, Shr, Sub};
use DiamondLattice::{NotAConstant, Unknown, Val};

type CallResolverOperand<R> = COperand<<R as CallResolver>::C>;

mod propagation;
use crate::const_fold::propagation::BasicBlockConstants;
pub use propagation::ConstantPropagation;

#[derive(Debug, Clone, PartialEq)]
pub enum DiamondLattice {
    Unknown,
    Val(ConstVal),
    NotAConstant,
}

impl DiamondLattice {
    pub fn apply_binary_op(
        self,
        other: Self,
        f: impl FnOnce(ConstVal, ConstVal) -> ConstVal,
    ) -> Self {
        match (self, other) {
            (Val(arg1), Val(arg2)) => Val(f(arg1, arg2)),
            (NotAConstant, _) | (_, NotAConstant) => NotAConstant,
            _ => Unknown,
        }
    }

    pub fn map(self, f: impl FnOnce(ConstVal) -> ConstVal) -> DiamondLattice {
        match self {
            Val(x) => Val(f(x)),
            NotAConstant => NotAConstant,
            Unknown => Unknown,
        }
    }

    pub fn and_then(self, f: impl FnOnce(ConstVal) -> DiamondLattice) -> DiamondLattice {
        match self {
            Val(x) => f(x),
            NotAConstant => NotAConstant,
            Unknown => Unknown,
        }
    }

    pub fn meet(&mut self, other: &Self) {
        match (self, other) {
            (x, y) if x == y => (),
            (_, Self::Unknown) => (),
            (dst, res) if *dst == Self::Unknown => *dst = res.clone(),
            (dst, _) => *dst = Self::NotAConstant,
        }
    }

    pub fn meet_constant(&mut self, val: &ConstVal) {
        match self {
            Self::Val(x) if x == val => (),
            Self::Unknown => *self = Self::Val(val.clone()),
            dst => *dst = Self::NotAConstant,
        }
    }
}

impl DiamondLattice {
    pub fn expect(self, msg: &'static str) -> ConstVal {
        if let Val(val) = self {
            val
        } else {
            panic!("Expected a constant value bound found {:?}: {}", self, msg)
        }
    }

    pub fn unwrap(self) -> ConstVal {
        if let Val(val) = self {
            val
        } else {
            panic!("Expected a constant value bound found {:?}!", self)
        }
    }
}

impl Into<Option<ConstVal>> for DiamondLattice {
    fn into(self) -> Option<ConstVal> {
        if let Self::Val(val) = self {
            Some(val)
        } else {
            None
        }
    }
}

macro_rules! undefined_operation {
    ($span: expr; $op: expr, $arg: expr) => {
        unreachable!("Operation {} not defined for {:?}", $op, $arg)
    };
    ($span: expr; $op: expr, $lhs: expr, $rhs: expr) => {
        unreachable!(
            "Operation {} not defined for {:?} and {:?}",
            $op, $lhs, $rhs
        )
    };
}

/// This trait allows constant folding inputs such as port connected
/// Note that in many instances it is better to emit constant values during HIR lowering instead
/// This should only be used if you want multiple CFGs where an input is constant folded in one case
/// but not the other
pub trait CallResolver: Debug {
    type C: CallType;
    fn resolve_call(
        &self,
        input: &Self::C,
        args: &IndexSlice<CallArg, [COperand<Self::C>]>,
    ) -> DiamondLattice;
    fn resolve_input(&self, input: &<Self::C as CallType>::I) -> DiamondLattice;
}

pub struct NoInputConstResolution<C>(PhantomData<fn(&C)>);

impl<C> Debug for NoInputConstResolution<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("NoInputConstResolution")
    }
}

impl<C> Display for NoInputConstResolution<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("none")
    }
}

impl<C: CallType> CallResolver for NoInputConstResolution<C> {
    type C = C;
    fn resolve_call(
        &self,
        _call: &C,
        _args: &IndexSlice<CallArg, [COperand<C>]>,
    ) -> DiamondLattice {
        DiamondLattice::NotAConstant
    }

    fn resolve_input(&self, _input: &<C as CallType>::I) -> DiamondLattice {
        DiamondLattice::NotAConstant
    }
}

struct ConstantFold<'lt, R: CallResolver, P: Borrow<BasicBlockConstants>> {
    locals: P,
    resolver: &'lt R,
}

impl<'lt, R: CallResolver, P: BorrowMut<BasicBlockConstants>> ConstantFold<'lt, R, P> {
    fn resolve_statement(&mut self, dst: Local, rvalue: &RValue<R::C>, ty: Type) -> DiamondLattice {
        let res = fold_rvalue(self, rvalue, ty);
        self.locals.borrow_mut().write_lattice(dst, res.clone());
        res
    }
}

impl<'lt, R: CallResolver, P: Borrow<BasicBlockConstants>> ConstantFold<'lt, R, P> {
    fn resolve_operand(&self, op: &CallResolverOperand<R>) -> DiamondLattice {
        match op.contents {
            OperandData::Constant(ref val) => DiamondLattice::Val(val.clone()),
            OperandData::Copy(local) => match self.locals.borrow().constants.get(&local) {
                Some(val) => DiamondLattice::Val(val.clone()),
                None if self.locals.borrow().not_a_constant.contains(local) => {
                    DiamondLattice::NotAConstant
                }
                None => DiamondLattice::Unknown,
            },
            OperandData::Read(ref input) => self.resolver.resolve_input(input),
        }
    }

    fn eval_real(
        &self,
        arg: &CallResolverOperand<R>,
        eval: impl FnOnce(f64) -> f64,
        op: &'static str,
        span: Span,
    ) -> DiamondLattice {
        self.resolve_operand(arg).map(|arg| {
            if let Scalar(Real(val)) = arg {
                Scalar(Real(eval(val)))
            } else {
                undefined_operation!(span; op,arg)
            }
        })
    }

    fn eval_int(
        &self,
        arg: &CallResolverOperand<R>,
        eval: impl FnOnce(i64) -> i64,
        op: &'static str,
        span: Span,
    ) -> DiamondLattice {
        self.resolve_operand(arg).map(|arg| {
            if let Scalar(Integer(val)) = arg {
                Scalar(Integer(eval(val)))
            } else {
                undefined_operation!(span; op,arg)
            }
        })
    }

    fn eval_bool(
        &self,
        arg: &CallResolverOperand<R>,
        eval: impl FnOnce(bool) -> bool,
        op: &'static str,
        span: Span,
    ) -> DiamondLattice {
        self.resolve_operand(arg).map(|arg| {
            if let Scalar(Bool(val)) = arg {
                Scalar(Bool(eval(val)))
            } else {
                undefined_operation!(span; op,arg)
            }
        })
    }

    fn eval_real_comparison(
        &self,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
        eval: impl FnOnce(f64, f64) -> bool,
        op: &'static str,
        span: Span,
    ) -> DiamondLattice {
        let lhs = self.resolve_operand(lhs);
        let rhs = self.resolve_operand(rhs);
        lhs.apply_binary_op(rhs, |lhs, rhs| match (lhs, rhs) {
            (Scalar(Real(lhs)), Scalar(Real(rhs))) => Scalar(Bool(eval(lhs, rhs))),
            (lhs, rhs) => undefined_operation!(span; op, lhs, rhs),
        })
    }

    fn eval_int_comparison(
        &self,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
        eval: impl FnOnce(i64, i64) -> bool,
        op: &'static str,
        span: Span,
    ) -> DiamondLattice {
        let lhs = self.resolve_operand(lhs);
        let rhs = self.resolve_operand(rhs);
        lhs.apply_binary_op(rhs, |lhs, rhs| match (lhs, rhs) {
            (Scalar(Integer(lhs)), Scalar(Integer(rhs))) => Scalar(Bool(eval(lhs, rhs))),
            (lhs, rhs) => undefined_operation!(span; op, lhs, rhs),
        })
    }

    fn eval_bin_real(
        &self,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
        eval: impl FnOnce(f64, f64) -> f64,
        op: &'static str,
        span: Span,
    ) -> DiamondLattice {
        let lhs = self.resolve_operand(lhs);
        let rhs = self.resolve_operand(rhs);
        lhs.apply_binary_op(rhs, |lhs, rhs| match (lhs, rhs) {
            (Scalar(Real(lhs)), Scalar(Real(rhs))) => Scalar(Real(eval(lhs, rhs))),
            (lhs, rhs) => undefined_operation!(span; op, lhs, rhs),
        })
    }

    fn eval_bin_int(
        &self,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
        eval: impl FnOnce(i64, i64) -> i64,
        op: &'static str,
        span: Span,
    ) -> DiamondLattice {
        let lhs = self.resolve_operand(lhs);
        let rhs = self.resolve_operand(rhs);
        lhs.apply_binary_op(rhs, |lhs, rhs| match (lhs, rhs) {
            (Scalar(Integer(lhs)), Scalar(Integer(rhs))) => Scalar(Integer(eval(lhs, rhs))),
            (lhs, rhs) => undefined_operation!(span; op, lhs, rhs),
        })
    }

    fn eval_bin_bool(
        &self,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
        eval: impl FnOnce(bool, bool) -> bool,
        op: &'static str,
        span: Span,
    ) -> DiamondLattice {
        let lhs = self.resolve_operand(lhs);
        let rhs = self.resolve_operand(rhs);
        lhs.apply_binary_op(rhs, |lhs, rhs| match (lhs, rhs) {
            (Scalar(Bool(lhs)), Scalar(Bool(rhs))) => Scalar(Bool(eval(lhs, rhs))),
            (lhs, rhs) => undefined_operation!(span; op, lhs, rhs),
        })
    }
}

impl<'lt, P: Borrow<BasicBlockConstants>, R: CallResolver> RValueFold<R::C>
    for ConstantFold<'lt, R, P>
{
    type T = DiamondLattice;

    fn fold_real_arith_negate(&mut self, op: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_real(arg, f64::neg, "Real Negate", op)
    }

    fn fold_bit_negate(&mut self, op: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_int(arg, i64::reverse_bits, "Bit Negate", op)
    }

    fn fold_int_arith_negate(&mut self, op: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_int(arg, i64::neg, "Integer Negate", op)
    }

    fn fold_logic_negate(&mut self, op: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_bool(arg, |arg| !arg, "Integer Negate", op)
    }

    fn fold_real_add(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_real(lhs, rhs, f64::add, "Real Summation", op)
    }

    fn fold_real_sub(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_real(lhs, rhs, f64::sub, "Real Subtraction", op)
    }

    fn fold_real_mul(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        let lhs = self.resolve_operand(lhs);
        let rhs = self.resolve_operand(rhs);
        match (lhs, rhs) {
            (Val(Scalar(Real(arg1))), Val(Scalar(Real(arg2)))) => Val(Scalar(Real(arg1 * arg2))),

            (Val(arg1), Val(arg2)) => undefined_operation!(op; "Real Multiplication", arg1, arg2),

            (Val(Scalar(Real(arg))), _) | (_, Val(Scalar(Real(arg)))) if arg == 0.0 => {
                Val(Scalar(Real(0.0)))
            }

            (NotAConstant, _) | (_, NotAConstant) => NotAConstant,

            _ => Unknown,
        }
    }

    fn fold_real_div(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        let lhs = self.resolve_operand(lhs);
        let rhs = self.resolve_operand(rhs);
        match (lhs, rhs) {
            (Val(Scalar(Real(arg1))), Val(Scalar(Real(arg2)))) => Val(Scalar(Real(arg1 / arg2))),

            (Val(arg1), Val(arg2)) => undefined_operation!(op; "Real Division", arg1, arg2),

            (Val(Scalar(Real(arg))), _) if arg == 0.0 => Val(Scalar(Real(0.0))),

            (NotAConstant, _) | (_, NotAConstant) => NotAConstant,

            _ => Unknown,
        }
    }

    fn fold_real_rem(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_real(lhs, rhs, f64::rem, "Real Remainder", op)
    }

    fn fold_int_add(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_int(lhs, rhs, i64::add, "Integer Subtraction", op)
    }

    fn fold_int_sub(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_int(lhs, rhs, i64::sub, "Integer Subtraction", op)
    }

    fn fold_int_mul(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        let lhs = self.resolve_operand(lhs);
        let rhs = self.resolve_operand(rhs);
        match (lhs, rhs) {
            (Val(Scalar(Integer(arg1))), Val(Scalar(Integer(arg2)))) => {
                Val(Scalar(Integer(arg1 * arg2)))
            }

            (Val(arg1), Val(arg2)) => {
                undefined_operation!(op; "Integer Multiplication", arg1, arg2)
            }

            (Val(Scalar(Integer(0))), _) | (_, Val(Scalar(Integer(0)))) => Val(Scalar(Integer(0))),

            (NotAConstant, _) | (_, NotAConstant) => NotAConstant,

            _ => Unknown,
        }
    }

    fn fold_int_div(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        let lhs = self.resolve_operand(lhs);
        let rhs = self.resolve_operand(rhs);
        match (lhs, rhs) {
            (Val(Scalar(Integer(arg1))), Val(Scalar(Integer(arg2)))) => {
                Val(Scalar(Integer(arg1 / arg2)))
            }

            (Val(arg1), Val(arg2)) => undefined_operation!(op; "Real Division", arg1, arg2),

            (Val(Scalar(Integer(0))), _) => Val(Scalar(Integer(0))),

            (NotAConstant, _) | (_, NotAConstant) => NotAConstant,

            _ => Unknown,
        }
    }

    fn fold_int_rem(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_int(lhs, rhs, i64::rem, "Integer Remainder", op)
    }

    fn fold_shiftl(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_int(lhs, rhs, i64::shl, "Integer Shift Left", op)
    }

    fn fold_shiftr(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_int(lhs, rhs, i64::shr, "Integer Shift Right", op)
    }

    fn fold_lt_real(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_real_comparison(lhs, rhs, |arg1, arg2| arg1 < arg2, "Real Less Than", op)
    }

    fn fold_le_real(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_real_comparison(lhs, rhs, |arg1, arg2| arg1 <= arg2, "Real Less Equal", op)
    }

    fn fold_gt_real(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_real_comparison(lhs, rhs, |arg1, arg2| arg1 > arg2, "Real Greater than", op)
    }

    fn fold_ge_real(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_real_comparison(
            lhs,
            rhs,
            |arg1, arg2| arg1 >= arg2,
            "Real Greater Equal",
            op,
        )
    }

    fn fold_lt_int(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_int_comparison(lhs, rhs, |arg1, arg2| arg1 < arg2, "Integer Less Then", op)
    }

    fn fold_le_int(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_int_comparison(
            lhs,
            rhs,
            |arg1, arg2| arg1 <= arg2,
            "Integer Less Equal",
            op,
        )
    }

    fn fold_gt_int(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_int_comparison(
            lhs,
            rhs,
            |arg1, arg2| arg1 > arg2,
            "Integer Greater Then",
            op,
        )
    }

    fn fold_ge_int(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_int_comparison(
            lhs,
            rhs,
            |arg1, arg2| arg1 >= arg2,
            "Integer Greater Equal",
            op,
        )
    }

    fn fold_eq(
        &mut self,
        _op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
        _ty: Type,
    ) -> Self::T {
        let lhs = self.resolve_operand(lhs);
        let rhs = self.resolve_operand(rhs);
        lhs.apply_binary_op(rhs, |x, y| Scalar(Bool(x == y)))
    }

    fn fold_ne(
        &mut self,
        _op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
        _ty: Type,
    ) -> Self::T {
        let lhs = self.resolve_operand(lhs);
        let rhs = self.resolve_operand(rhs);
        lhs.apply_binary_op(rhs, |x, y| Scalar(Bool(x != y)))
    }

    fn fold_xor(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_int(lhs, rhs, i64::bitxor, "Integer XOR", op)
    }

    fn fold_nxor(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_int(lhs, rhs, |lhs, rhs| !(lhs ^ rhs), "Integer NXOR", op)
    }

    fn fold_and(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_int(lhs, rhs, i64::bitand, "Integer NXOR", op)
    }

    fn fold_or(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_int(lhs, rhs, i64::bitor, "Integer NXOR", op)
    }

    fn fold_bool_xor(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_bool(lhs, rhs, bool::bitxor, "Integer NXOR", op)
    }

    fn fold_bool_nxor(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_bool(lhs, rhs, |lhs, rhs| !(lhs ^ rhs), "Integer NXOR", op)
    }

    fn fold_bool_and(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_bool(lhs, rhs, bool::bitand, "Integer NXOR", op)
    }

    fn fold_bool_or(
        &mut self,
        op: Span,
        lhs: &CallResolverOperand<R>,
        rhs: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_bool(lhs, rhs, bool::bitor, "Integer NXOR", op)
    }

    fn fold_exp(&mut self, span: Span, arg: &CallResolverOperand<R>, _limit: bool) -> Self::T {
        self.eval_real(arg, f64::exp, "EXP", span)
    }

    fn fold_ln(&mut self, span: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_real(arg, f64::ln, "LN", span)
    }

    fn fold_log(&mut self, span: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_real(arg, f64::log10, "LOG", span)
    }

    fn fold_sqrt(&mut self, span: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_real(arg, f64::sqrt, "SQRT", span)
    }

    fn fold_real_abs(&mut self, span: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_real(arg, f64::abs, "Real ABS", span)
    }

    fn fold_int_abs(&mut self, span: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_int(arg, i64::abs, "Integer ABS", span)
    }

    fn fold_ceil(&mut self, span: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_real(arg, f64::ceil, "Real Ceil", span)
    }

    fn fold_floor(&mut self, span: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_real(arg, f64::floor, "Real Flor", span)
    }

    fn fold_sin(&mut self, span: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_real(arg, f64::sin, "Real Sin", span)
    }

    fn fold_cos(&mut self, span: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_real(arg, f64::cos, "Real Cos", span)
    }

    fn fold_tan(&mut self, span: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_real(arg, f64::tan, "Real Tan", span)
    }

    fn fold_sinh(&mut self, span: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_real(arg, f64::sinh, "Real SinH", span)
    }

    fn fold_cosh(&mut self, span: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_real(arg, f64::cosh, "Real CosH", span)
    }

    fn fold_tanh(&mut self, span: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_real(arg, f64::tanh, "Real TanH", span)
    }

    fn fold_asin(&mut self, span: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_real(arg, f64::asin, "Real ASIN", span)
    }

    fn fold_acos(&mut self, span: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_real(arg, f64::acos, "Real ACOS", span)
    }

    fn fold_atan(&mut self, span: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_real(arg, f64::atan, "Real ATAN", span)
    }

    fn fold_asinh(&mut self, span: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_real(arg, f64::asin, "Real ASINH", span)
    }

    fn fold_acosh(&mut self, span: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_real(arg, f64::acosh, "Real ACOSH", span)
    }

    fn fold_atanh(&mut self, span: Span, arg: &CallResolverOperand<R>) -> Self::T {
        self.eval_real(arg, f64::atanh, "Real ATANH", span)
    }

    fn fold_pow(
        &mut self,
        span: Span,
        arg1: &CallResolverOperand<R>,
        arg2: &CallResolverOperand<R>,
    ) -> Self::T {
        let lhs = self.resolve_operand(arg1);
        let rhs = self.resolve_operand(arg2);
        match (lhs, rhs) {
            (Val(Scalar(Real(arg1))), Val(Scalar(Real(arg2)))) => {
                Val(Scalar(Real(arg1.powf(arg2))))
            }

            (Val(arg1), Val(arg2)) => undefined_operation!(op; "POW", arg1, arg2),

            (Val(Scalar(Real(arg))), _) if arg == 0.0 => Val(Scalar(Real(0.0))),
            (Val(Scalar(Real(arg))), _) if arg == 1.0 => Val(Scalar(Real(1.0))),

            (_, Val(Scalar(Real(arg)))) if arg == 0.0 => Val(Scalar(Real(1.0))),

            (NotAConstant, _) | (_, NotAConstant) => NotAConstant,

            _ => Unknown,
        }
    }

    fn fold_hypot(
        &mut self,
        span: Span,
        arg1: &CallResolverOperand<R>,
        arg2: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_real(arg1, arg2, f64::hypot, "HYPOT", span)
    }

    fn fold_real_min(
        &mut self,
        span: Span,
        arg1: &CallResolverOperand<R>,
        arg2: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_real(arg1, arg2, f64::min, "Real Min", span)
    }

    fn fold_int_min(
        &mut self,
        span: Span,
        arg1: &CallResolverOperand<R>,
        arg2: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_int(arg1, arg2, i64::min, "Integer Min", span)
    }

    fn fold_real_max(
        &mut self,
        span: Span,
        arg1: &CallResolverOperand<R>,
        arg2: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_real(arg1, arg2, f64::max, "Real Max", span)
    }

    fn fold_int_max(
        &mut self,
        span: Span,
        arg1: &CallResolverOperand<R>,
        arg2: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_int(arg1, arg2, i64::max, "Integer Max", span)
    }

    fn fold_atan2(
        &mut self,
        span: Span,
        arg1: &CallResolverOperand<R>,
        arg2: &CallResolverOperand<R>,
    ) -> Self::T {
        self.eval_bin_real(arg1, arg2, f64::atan2, "ATAN2", span)
    }

    fn fold_cast(&mut self, arg: &CallResolverOperand<R>, dst: Type) -> Self::T {
        let arg = self.resolve_operand(arg);
        arg.map(|arg| {
            match (dst, arg) {
                (Type::REAL, Scalar(Integer(val))) => Scalar(Real(val as f64)),
                (Type::REAL, Scalar(Bool(val))) => Scalar(Real(val as i64 as f64)),
                (Type::BOOL, Scalar(Integer(val))) => Scalar(Bool(val != 0)),
                (Type::BOOL, Scalar(Real(val))) => Scalar(Bool(val != 0.0)),
                (Type::INT, Scalar(Real(val))) => Scalar(Integer(val.round() as i64)),
                (Type::INT, Scalar(Bool(val))) => Scalar(Integer(val as i64)),
                // TODO array casts?
                _ => unreachable!("Malformed MIR"),
            }
        })
    }

    fn fold_use(&mut self, arg: &CallResolverOperand<R>) -> Self::T {
        self.resolve_operand(arg)
    }

    fn fold_select(
        &mut self,
        cond: &CallResolverOperand<R>,
        true_val: &CallResolverOperand<R>,
        false_val: &CallResolverOperand<R>,
    ) -> Self::T {
        let cond = self.resolve_operand(cond);
        cond.and_then(|cond| match cond {
            Scalar(Bool(false)) => self.resolve_operand(false_val),
            Scalar(Bool(true)) => self.resolve_operand(true_val),
            cond => undefined_operation!(cond.span; "SELECT",cond),
        })
    }

    fn fold_call(
        &mut self,
        call: &R::C,
        args: &IndexSlice<CallArg, [CallResolverOperand<R>]>,
        span: Span,
    ) -> Self::T {
        self.resolver.resolve_call(call, args)
    }

    fn fold_array(&mut self, args: &[CallResolverOperand<R>], _span: Span, ty: Type) -> Self::T {
        let mut unknown = false;
        let mut res = Vec::with_capacity(args.len());
        for arg in args {
            match self.resolve_operand(arg) {
                DiamondLattice::NotAConstant => return NotAConstant,
                DiamondLattice::Unknown => unknown = true,
                DiamondLattice::Val(_) if unknown => (),
                DiamondLattice::Val(val) => val.flatten(&mut res),
            }
        }

        if unknown {
            DiamondLattice::Unknown
        } else {
            DiamondLattice::Val(ConstVal::Array(res.into_boxed_slice(), ty))
        }
    }
}
