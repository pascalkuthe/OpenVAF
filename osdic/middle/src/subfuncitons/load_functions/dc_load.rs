/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::frontend::{GeneralOsdiCall, GeneralOsdiInput};
use crate::lim::LimFunction;
use openvaf_data_structures::index_vec::{IndexSlice, IndexVec};
use openvaf_ir::ids::NetId;
use openvaf_ir::{PrintOnFinish, StopTaskKind};
use openvaf_middle::const_fold::FlatSet;
use openvaf_middle::derivatives::RValueAutoDiff;
use openvaf_middle::{
    COperand, COperandData, CallArg, CfgConversion, CfgFunctions, ConstVal, Operand, OperandData,
    RValue, SimpleConstVal, StmntKind,
};
use openvaf_session::sourcemap::Span;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

#[derive(PartialEq, Eq, Clone)]
pub enum DcLoadFunctionCall {
    StopTask(StopTaskKind, PrintOnFinish),
    Lim {
        hi: NetId,
        lo: NetId,
        fun: LimFunction,
    },
}

impl CfgFunctions for DcLoadFunctionCall {
    type I = GeneralOsdiInput;

    fn const_fold(&self, _call: &[FlatSet]) -> FlatSet {
        FlatSet::Top
    }

    fn derivative<C: CfgFunctions>(
        &self,
        _args: &IndexSlice<CallArg, [COperand<Self>]>,
        _ad: &mut RValueAutoDiff<Self, C>,
        _span: Span,
    ) -> Option<RValue<Self>> {
        unreachable!()
    }
}

impl Display for DcLoadFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::StopTask(StopTaskKind::Stop, finish) => {
                write!(f, "$stop({:?})", finish)
            }
            Self::StopTask(StopTaskKind::Finish, finish) => {
                write!(f, "$finish({:?})", finish)
            }
            Self::Lim { hi, lo, fun } => write!(f, "$limit(pot({:?}, {:?}), {} )", hi, lo, fun,),
        }
    }
}

impl Debug for DcLoadFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

pub struct GeneralToDcLoad;

impl CfgConversion<GeneralOsdiCall, DcLoadFunctionCall> for GeneralToDcLoad {
    fn map_input(
        &mut self,
        src: <GeneralOsdiCall as CfgFunctions>::I,
    ) -> COperandData<DcLoadFunctionCall> {
        OperandData::Read(src)
    }

    fn map_call_val(
        &mut self,
        call: GeneralOsdiCall,
        args: IndexVec<CallArg, COperand<GeneralOsdiCall>>,
        span: Span,
    ) -> RValue<DcLoadFunctionCall> {
        match call {
            GeneralOsdiCall::Noise
            | GeneralOsdiCall::SymbolicDerivativeOfTimeDerivative
            | GeneralOsdiCall::TimeDerivative => RValue::Use(Operand {
                span,
                contents: OperandData::Constant(ConstVal::Scalar(SimpleConstVal::Real(0.0))),
            }),
            GeneralOsdiCall::StopTask(_, _) | GeneralOsdiCall::NodeCollapse(_, _) => unreachable!(),
            GeneralOsdiCall::Lim { hi, lo, fun } => {
                let args = args.into_iter().map(|arg| self.map_operand(arg)).collect();
                RValue::Call(DcLoadFunctionCall::Lim { hi, lo, fun }, args, span)
            }
        }
    }

    fn map_call_stmnt(
        &mut self,
        call: GeneralOsdiCall,
        args: IndexVec<CallArg, COperand<DcLoadFunctionCall>>,
        span: Span,
    ) -> StmntKind<DcLoadFunctionCall> {
        match call {
            GeneralOsdiCall::Noise => unreachable!(),
            GeneralOsdiCall::TimeDerivative
            | GeneralOsdiCall::SymbolicDerivativeOfTimeDerivative => unreachable!(),
            GeneralOsdiCall::StopTask(kind, print) => {
                StmntKind::Call(DcLoadFunctionCall::StopTask(kind, print), args, span)
            }
            GeneralOsdiCall::NodeCollapse(_, _) => StmntKind::NoOp,
            GeneralOsdiCall::Lim { .. } => unreachable!(),
        }
    }
}
