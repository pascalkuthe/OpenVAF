/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::intrinsics::Intrinsic;
use crate::{CallTypeCodeGen, CfgCodegen};
use inkwell::values::{BasicValue, BasicValueEnum, InstructionOpcode};
use inkwell::{FloatPredicate, IntPredicate};
use openvaf_data_structures::index_vec::{IndexSlice, IndexVec};
use openvaf_middle::{COperand, CallArg, CallType, RValueFold, Type};
use openvaf_session::sourcemap::Span;

impl<'lt, 'a, 'c, A: CallType, C: CallTypeCodeGen<'lt, 'c>> RValueFold<C>
    for CfgCodegen<'lt, 'a, 'c, C::CodeGenData, A, C>
{
    type T = BasicValueEnum<'c>;

    fn fold_real_arith_negate(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx
            .builder
            .build_float_neg(arg.into_float_value(), "real_neg")
            .as_basic_value_enum()
    }

    fn fold_bit_negate(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx
            .builder
            .build_not(arg.into_int_value(), "inverse")
            .as_basic_value_enum()
    }

    fn fold_int_arith_negate(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx
            .builder
            .build_int_neg(arg.into_int_value(), "int_neg")
            .as_basic_value_enum()
    }

    fn fold_logic_negate(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx
            .builder
            .build_not(arg.into_int_value(), "inverse")
            .as_basic_value_enum()
    }

    fn fold_real_add(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);
        self.ctx
            .builder
            .build_float_add(lhs.into_float_value(), rhs.into_float_value(), "float_add")
            .as_basic_value_enum()
    }

    fn fold_real_sub(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);
        self.ctx
            .builder
            .build_float_sub(lhs.into_float_value(), rhs.into_float_value(), "float_sub")
            .as_basic_value_enum()
    }

    fn fold_real_mul(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);
        self.ctx
            .builder
            .build_float_mul(lhs.into_float_value(), rhs.into_float_value(), "float_mul")
            .as_basic_value_enum()
    }

    fn fold_real_div(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);
        self.ctx
            .builder
            .build_float_div(lhs.into_float_value(), rhs.into_float_value(), "float_add")
            .as_basic_value_enum()
    }

    fn fold_real_rem(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);
        self.ctx
            .builder
            .build_float_rem(lhs.into_float_value(), rhs.into_float_value(), "float_rem")
            .as_basic_value_enum()
    }

    fn fold_int_add(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);
        self.ctx
            .builder
            .build_int_add(lhs.into_int_value(), rhs.into_int_value(), "int_add")
            .as_basic_value_enum()
    }

    fn fold_int_sub(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);
        self.ctx
            .builder
            .build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "int_sub")
            .as_basic_value_enum()
    }

    fn fold_int_mul(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);
        self.ctx
            .builder
            .build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "int_mul")
            .as_basic_value_enum()
    }

    fn fold_int_div(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);
        self.ctx
            .builder
            .build_int_signed_div(lhs.into_int_value(), rhs.into_int_value(), "int_div")
            .as_basic_value_enum()
    }

    fn fold_int_rem(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);
        self.ctx
            .builder
            .build_int_signed_rem(lhs.into_int_value(), rhs.into_int_value(), "int_rem")
            .as_basic_value_enum()
    }

    fn fold_shiftl(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);
        self.ctx
            .builder
            .build_left_shift(lhs.into_int_value(), rhs.into_int_value(), "shiftl")
            .as_basic_value_enum()
    }

    fn fold_shiftr(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);
        self.ctx
            .builder
            .build_right_shift(lhs.into_int_value(), rhs.into_int_value(), true, "shiftr")
            .as_basic_value_enum()
    }

    fn fold_lt_real(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);
        self.ctx
            .builder
            .build_float_compare(
                FloatPredicate::OLT,
                lhs.into_float_value(),
                rhs.into_float_value(),
                "float_cmp",
            )
            .as_basic_value_enum()
    }

    fn fold_le_real(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);
        self.ctx
            .builder
            .build_float_compare(
                FloatPredicate::OLE,
                lhs.into_float_value(),
                rhs.into_float_value(),
                "float_cmp",
            )
            .as_basic_value_enum()
    }

    fn fold_gt_real(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);
        self.ctx
            .builder
            .build_float_compare(
                FloatPredicate::OGT,
                lhs.into_float_value(),
                rhs.into_float_value(),
                "float_cmp",
            )
            .as_basic_value_enum()
    }

    fn fold_ge_real(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);
        self.ctx
            .builder
            .build_float_compare(
                FloatPredicate::OGE,
                lhs.into_float_value(),
                rhs.into_float_value(),
                "float_cmp",
            )
            .as_basic_value_enum()
    }

    fn fold_lt_int(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);
        self.ctx
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                lhs.into_int_value(),
                rhs.into_int_value(),
                "int_cmp",
            )
            .as_basic_value_enum()
    }

    fn fold_le_int(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);
        self.ctx
            .builder
            .build_int_compare(
                IntPredicate::SLE,
                lhs.into_int_value(),
                rhs.into_int_value(),
                "int_cmp",
            )
            .as_basic_value_enum()
    }

    fn fold_gt_int(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);
        self.ctx
            .builder
            .build_int_compare(
                IntPredicate::SGT,
                lhs.into_int_value(),
                rhs.into_int_value(),
                "int_cmp",
            )
            .as_basic_value_enum()
    }

    fn fold_ge_int(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);
        self.ctx
            .builder
            .build_int_compare(
                IntPredicate::SGE,
                lhs.into_int_value(),
                rhs.into_int_value(),
                "int_cmp",
            )
            .as_basic_value_enum()
    }

    fn fold_eq(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>, ty: Type) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);

        match ty {
            Type::INT => self
                .ctx
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "int_cmp",
                )
                .as_basic_value_enum(),
            Type::REAL => self
                .ctx
                .builder
                .build_float_compare(
                    FloatPredicate::OEQ,
                    lhs.into_float_value(),
                    rhs.into_float_value(),
                    "float_cmp",
                )
                .as_basic_value_enum(),
            Type::STRING => self.string_comparison(false, lhs, rhs),

            _ => todo!("Array comparison"),
        }
    }

    fn fold_ne(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>, ty: Type) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);

        match ty {
            Type::INT => self
                .ctx
                .builder
                .build_int_compare(
                    IntPredicate::NE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "int_cmp",
                )
                .as_basic_value_enum(),
            Type::REAL => self
                .ctx
                .builder
                .build_float_compare(
                    FloatPredicate::ONE,
                    lhs.into_float_value(),
                    rhs.into_float_value(),
                    "float_cmp",
                )
                .as_basic_value_enum(),
            Type::STRING => self.string_comparison(true, lhs, rhs),

            _ => todo!("Array comparison"),
        }
    }

    fn fold_xor(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);

        self.ctx
            .builder
            .build_xor(lhs.into_int_value(), rhs.into_int_value(), "xor")
            .as_basic_value_enum()
    }

    fn fold_nxor(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);

        let xor = self
            .ctx
            .builder
            .build_xor(lhs.into_int_value(), rhs.into_int_value(), "nxor");
        self.ctx
            .builder
            .build_not(xor, "nxor")
            .as_basic_value_enum()
    }

    fn fold_and(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);

        self.ctx
            .builder
            .build_and(lhs.into_int_value(), rhs.into_int_value(), "and")
            .as_basic_value_enum()
    }

    fn fold_or(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);

        self.ctx
            .builder
            .build_or(lhs.into_int_value(), rhs.into_int_value(), "or")
            .as_basic_value_enum()
    }

    fn fold_bool_xor(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);

        self.ctx
            .builder
            .build_xor(lhs.into_int_value(), rhs.into_int_value(), "xor")
            .as_basic_value_enum()
    }

    fn fold_bool_nxor(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);

        let xor = self
            .ctx
            .builder
            .build_xor(lhs.into_int_value(), rhs.into_int_value(), "nxor");
        self.ctx
            .builder
            .build_not(xor, "nxor")
            .as_basic_value_enum()
    }

    fn fold_bool_and(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);

        self.ctx
            .builder
            .build_and(lhs.into_int_value(), rhs.into_int_value(), "and")
            .as_basic_value_enum()
    }

    fn fold_bool_or(&mut self, _: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs = self.operand(lhs);
        let rhs = self.operand(rhs);

        self.ctx
            .builder
            .build_or(lhs.into_int_value(), rhs.into_int_value(), "or")
            .as_basic_value_enum()
    }

    fn fold_exp(&mut self, _: Span, arg: &COperand<C>, limit: bool) -> Self::T {
        let arg = self.operand(arg);
        if limit {
            C::gen_limexp(self, arg)
        } else {
            self.ctx.build_intrinsic_call(Intrinsic::Exp, &[arg])
        }
    }

    fn fold_ln(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx.build_intrinsic_call(Intrinsic::Ln, &[arg])
    }

    fn fold_log(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx.build_intrinsic_call(Intrinsic::Log, &[arg])
    }

    fn fold_sqrt(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx.build_intrinsic_call(Intrinsic::Sqrt, &[arg])
    }

    fn fold_real_abs(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx.build_intrinsic_call(Intrinsic::FloatAbs, &[arg])
    }

    fn fold_int_abs(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx.build_intrinsic_call(Intrinsic::IntAbs, &[arg])
    }

    fn fold_ceil(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx.build_intrinsic_call(Intrinsic::Ceil, &[arg])
    }

    fn fold_floor(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx.build_intrinsic_call(Intrinsic::Floor, &[arg])
    }

    fn fold_sin(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx.build_intrinsic_call(Intrinsic::Sin, &[arg])
    }

    fn fold_cos(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx.build_intrinsic_call(Intrinsic::Cos, &[arg])
    }

    fn fold_tan(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx.build_intrinsic_call(Intrinsic::Tan, &[arg])
    }

    fn fold_sinh(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx.build_intrinsic_call(Intrinsic::SinH, &[arg])
    }

    fn fold_cosh(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx.build_intrinsic_call(Intrinsic::CosH, &[arg])
    }

    fn fold_tanh(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx.build_intrinsic_call(Intrinsic::TanH, &[arg])
    }

    fn fold_asin(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx.build_intrinsic_call(Intrinsic::ASin, &[arg])
    }

    fn fold_acos(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx.build_intrinsic_call(Intrinsic::ACos, &[arg])
    }

    fn fold_atan(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx.build_intrinsic_call(Intrinsic::ATan, &[arg])
    }

    fn fold_asinh(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx.build_intrinsic_call(Intrinsic::ASinH, &[arg])
    }

    fn fold_acosh(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx.build_intrinsic_call(Intrinsic::ACosH, &[arg])
    }

    fn fold_atanh(&mut self, _: Span, arg: &COperand<C>) -> Self::T {
        let arg = self.operand(arg);
        self.ctx.build_intrinsic_call(Intrinsic::ATanH, &[arg])
    }

    fn fold_pow(&mut self, _: Span, arg1: &COperand<C>, arg2: &COperand<C>) -> Self::T {
        let arg1 = self.operand(arg1);
        let arg2 = self.operand(arg2);
        self.ctx.build_intrinsic_call(Intrinsic::Pow, &[arg1, arg2])
    }

    fn fold_hypot(&mut self, _: Span, arg1: &COperand<C>, arg2: &COperand<C>) -> Self::T {
        let arg1 = self.operand(arg1);
        let arg2 = self.operand(arg2);
        self.ctx
            .build_intrinsic_call(Intrinsic::Hypot, &[arg1, arg2])
    }

    fn fold_real_min(&mut self, _: Span, arg1: &COperand<C>, arg2: &COperand<C>) -> Self::T {
        let arg1 = self.operand(arg1);
        let arg2 = self.operand(arg2);
        self.ctx
            .build_intrinsic_call(Intrinsic::FloatMin, &[arg1, arg2])
    }

    fn fold_int_min(&mut self, _: Span, arg1: &COperand<C>, arg2: &COperand<C>) -> Self::T {
        let arg1 = self.operand(arg1);
        let arg2 = self.operand(arg2);
        self.ctx
            .build_intrinsic_call(Intrinsic::IntMax, &[arg1, arg2])
    }

    fn fold_real_max(&mut self, _: Span, arg1: &COperand<C>, arg2: &COperand<C>) -> Self::T {
        let arg1 = self.operand(arg1);
        let arg2 = self.operand(arg2);
        self.ctx
            .build_intrinsic_call(Intrinsic::FloatMax, &[arg1, arg2])
    }

    fn fold_int_max(&mut self, _: Span, arg1: &COperand<C>, arg2: &COperand<C>) -> Self::T {
        let arg1 = self.operand(arg1);
        let arg2 = self.operand(arg2);
        self.ctx
            .build_intrinsic_call(Intrinsic::IntMax, &[arg1, arg2])
    }

    fn fold_atan2(&mut self, _: Span, arg1: &COperand<C>, arg2: &COperand<C>) -> Self::T {
        let arg1 = self.operand(arg1);
        let arg2 = self.operand(arg2);
        self.ctx
            .build_intrinsic_call(Intrinsic::ATan2, &[arg1, arg2])
    }

    fn fold_cast(&mut self, arg: &COperand<C>, dst_ty: Type) -> Self::T {
        let src_ty = arg.contents.ty(self.ctx.mir, self.cfg);
        let src = self.operand(arg);

        match (dst_ty, src_ty) {
            (Type::INT, Type::REAL) => self
                .ctx
                .build_intrinsic_call(Intrinsic::IntToFloatConversion, &[src]),

            (Type::INT, Type::BOOL) => self
                .ctx
                .builder
                .build_int_cast(
                    src.into_int_value(),
                    self.ctx.integer_ty(),
                    "implict bool to int",
                )
                .as_basic_value_enum(),

            (Type::REAL, Type::INT) => self.ctx.builder.build_cast(
                InstructionOpcode::SIToFP,
                src,
                self.ctx.real_ty(),
                "implicit int to real",
            ),

            (Type::REAL, Type::BOOL) => self.ctx.builder.build_cast(
                InstructionOpcode::UIToFP,
                src,
                self.ctx.real_ty(),
                "implicit bool to real",
            ),

            (Type::BOOL, Type::INT) => self
                .ctx
                .builder
                .build_int_compare(
                    IntPredicate::NE,
                    src.into_int_value(),
                    self.ctx.integer_ty().const_int(0, true),
                    "implicit int -> bool",
                )
                .as_basic_value_enum(),

            (Type::BOOL, Type::REAL) => self
                .ctx
                .builder
                .build_float_compare(
                    FloatPredicate::ONE,
                    src.into_float_value(),
                    self.ctx.real_ty().const_float(0.0),
                    "implict real -> bool",
                )
                .as_basic_value_enum(),
            _ => unreachable!("Malformed MIR"),
        }
    }

    fn fold_use(&mut self, arg: &COperand<C>) -> Self::T {
        self.operand(arg)
    }

    fn fold_select(
        &mut self,
        cond: &COperand<C>,
        true_val: &COperand<C>,
        false_val: &COperand<C>,
    ) -> Self::T {
        let cond = self.operand(cond);
        let true_val = self.operand(true_val);
        let false_val = self.operand(false_val);
        self.ctx
            .builder
            .build_select(cond.into_int_value(), true_val, false_val, "select")
    }

    fn fold_call(
        &mut self,
        call: &C,
        args: &IndexSlice<CallArg, [COperand<C>]>,
        _: Span,
    ) -> Self::T {
        let args: IndexVec<CallArg, _> = args.iter().map(|op| self.operand(op)).collect();
        C::gen_call_rvalue(call, self, args.as_slice())
    }

    fn fold_array(&mut self, _: &[COperand<C>], _: Span, _: Type) -> Self::T {
        unimplemented!()
    }
}
