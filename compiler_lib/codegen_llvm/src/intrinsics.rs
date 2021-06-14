/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::LlvmCodegen;
use enum_map::{enum_map, Enum, EnumMap};
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::FunctionType;
use inkwell::values::{BasicValueEnum, FunctionValue};
use inkwell::AddressSpace;
use openvaf_middle::CallType;

pub type IntrinsicPrototypes<'ctx> = EnumMap<Intrinsic, FunctionValue<'ctx>>;

pub fn intrinsic_prototypes<'c>(
    module: &Module<'c>,
    ctx: &'c Context,
    msvc: bool,
) -> IntrinsicPrototypes<'c> {
    enum_map! {
        intrinsic => module.add_function(
            intrinsic.name(msvc),
            intrinsic.fn_type(ctx),
            Some(Linkage::External),
        )
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Enum)]
pub enum Intrinsic {
    IntToFloatConversion,
    Cos,
    Sin,
    Tan,
    ACos,
    ASin,
    ATan,
    ATan2,
    CosH,
    SinH,
    TanH,
    ACosH,
    ASinH,
    ATanH,
    Floor,
    Ceil,
    Hypot,
    Sqrt,
    Pow,
    Log,
    Ln,
    Exp,
    FloatAbs,
    IntAbs,
    FloatMin,
    IntMin,
    FloatMax,
    IntMax,
    StringCmp,
}

impl Intrinsic {
    pub fn name(self, msvc: bool) -> &'static str {
        match self {
            // Real llvm intrinsics
            Self::IntToFloatConversion => "llvm.llround.i64.f64",

            Self::Cos => "llvm.cos.f64",
            Self::Sin => "llvm.sin.f64",
            Self::Floor => "llvm.floor.f64",
            Self::Ceil => "llvm.ceil.f64",
            Self::Sqrt => "llvm.sqrt.f64",
            Self::Pow => "llvm.pow.f64",

            Self::Log => "llvm.log10.f64",
            Self::Ln => "llvm.log.f64",
            Self::Exp => "llvm.exp.f64",

            Self::FloatAbs => "llvm.fabs.f64",
            Self::IntAbs => "llvm.abs.i64",
            Self::FloatMin => "llvm.fmin.f64",
            Self::IntMin => "llvm.smin.i64",
            Self::FloatMax => "llvm.maximum.f64",
            Self::IntMax => "llvm.minimum.i64",

            //system libc calls
            Self::Tan => "tan",
            Self::ACos => "acos",
            Self::ASin => "asin",
            Self::ATan => "atan",
            Self::ATan2 => "atan2",

            Self::CosH => "cosh",
            Self::SinH => "sinh",
            Self::TanH => "tanh",
            Self::ACosH => "acosh",
            Self::ASinH => "asinh",
            Self::ATanH => "atanh",

            Self::Hypot if msvc => "_hypot",
            Self::Hypot => "hypot",
            Self::StringCmp => "strcmp",
        }
    }

    pub fn fn_type(self, ctx: &Context) -> FunctionType<'_> {
        match self {
            Self::IntToFloatConversion => ctx.i64_type().fn_type(&[ctx.f64_type().into()], false),

            Self::Cos
            | Self::Sin
            | Self::Tan
            | Self::ACos
            | Self::ASin
            | Self::ATan
            | Self::CosH
            | Self::SinH
            | Self::TanH
            | Self::ACosH
            | Self::ASinH
            | Self::ATanH
            | Self::Floor
            | Self::Ceil
            | Self::Sqrt
            | Self::Log
            | Self::Ln
            | Self::Exp
            | Self::FloatAbs => ctx.f64_type().fn_type(&[ctx.f64_type().into()], false),

            Self::ATan2 | Self::Hypot | Self::Pow | Self::FloatMin | Self::FloatMax => ctx
                .f64_type()
                .fn_type(&[ctx.f64_type().into(), ctx.f64_type().into()], false),

            Self::IntAbs => ctx.i64_type().fn_type(&[ctx.i64_type().into()], false),

            Self::IntMin | Self::IntMax => ctx
                .i64_type()
                .fn_type(&[ctx.i64_type().into(), ctx.i64_type().into()], false),
            Self::StringCmp => {
                let char_ptr = ctx.i8_type().ptr_type(AddressSpace::Generic).into();
                ctx.i32_type().fn_type(&[char_ptr, char_ptr], false)
            }
        }
    }
}

impl<'a, 'ctx, A: CallType> LlvmCodegen<'a, 'ctx, A> {
    pub fn build_intrinsic_call(
        &mut self,
        intrinisc: Intrinsic,
        args: &[BasicValueEnum<'ctx>],
    ) -> BasicValueEnum<'ctx> {
        let prototype = self.intrinsics[intrinisc];
        self.builder
            .build_call(prototype, args, intrinisc.name(false))
            .try_as_basic_value()
            .unwrap_left()
    }
}
