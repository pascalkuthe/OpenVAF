/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::framework::session::middle::TestFunctions;
use data_structures::index_vec::IndexSlice;
use tracing::field::debug;

impl<'lt, 'c> CallTypeCodeGen<'lt, 'c> for TestFunctions {
    type CodeGenData = ();

    fn read_input<'a, A: CfgFunctions>(
        cg: &mut CfgCodegen<'lt, 'a, 'c, (), A, Self>,
        input: &Self::I,
    ) -> BasicValueEnum<'c> {
        match input {
            DefaultInputs::Parameter(ParameterInput::Value(id)) => {
                match cg.ctx.mir.parameters[*id].ty {
                    Type::REAL => cg.ctx.real_ty().const_float(0.0).as_basic_value_enum(),
                    Type::INT => cg.ctx.integer_ty().const_int(0, true).as_basic_value_enum(),
                    Type::STRING => cg.ctx.str_literal(StringLiteral::DUMMY).as_basic_value_enum(),
                    _ => todo!("Arrays"),
                }
            }
            DefaultInputs::Parameter(ParameterInput::Given(_)) => {
                cg.ctx.bool_ty().const_int(1, false).as_basic_value_enum()
            }
            DefaultInputs::PortConnected(_) => {
                cg.ctx.bool_ty().const_int(1, false).as_basic_value_enum()
            }
            DefaultInputs::SimParam(param) => match param.kind {
                SimParamKind::Real => cg.ctx.real_ty().const_float(0.0).as_basic_value_enum(),
                SimParamKind::RealOptional => {
                    cg.ctx.real_ty().const_float(0.0).as_basic_value_enum()
                }
                SimParamKind::RealOptionalGiven => {
                    cg.ctx.bool_ty().const_zero().as_basic_value_enum()
                }
                SimParamKind::String => {
                    cg.ctx.str_literal(StringLiteral::DUMMY).as_basic_value_enum()
                }
            },

            DefaultInputs::Voltage(_)
            | DefaultInputs::CurrentProbe(_)
            | DefaultInputs::PartialTimeDerivative(_) => {
                cg.ctx.real_ty().const_float(0.0).as_basic_value_enum()
            }
            DefaultInputs::Temperature(_) => {
                cg.ctx.real_ty().const_float(300.0).as_basic_value_enum()
            }
        }
    }

    fn gen_call_rvalue<'a, A: CfgFunctions>(
        &self,
        cg: &mut CfgCodegen<'lt, 'a, 'c, (), A, Self>,
        _args: &IndexSlice<CallArg, [BasicValueEnum<'c>]>,
    ) -> BasicValueEnum<'c> {
        if matches!(self, Self::Analysis(_)) {
            cg.ctx.bool_ty().const_zero().as_basic_value_enum()
        } else {
            cg.ctx.real_ty().const_float(0.0).as_basic_value_enum()
        }
    }

    fn gen_call<'a, A: CfgFunctions>(
        &self,
        _cg: &mut CfgCodegen<'lt, 'a, 'c, (), A, Self>,
        _args: &IndexSlice<CallArg, [BasicValueEnum<'c>]>,
    ) {
        debug!("ignoring stop call")
    }

    fn gen_limexp<'a, A: CfgFunctions>(
        cg: &mut CfgCodegen<'lt, 'a, 'c, (), A, Self>,
        arg: BasicValueEnum<'c>,
    ) -> BasicValueEnum<'c> {
        cg.ctx.build_intrinsic_call(Intrinsic::Exp, &[arg])
    }
}
