/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::cfg::ControlFlowGraph;
use crate::derivatives::RValueAutoDiff;
use crate::dfa::lattice::FlatSet;
use crate::inputs::{
    CurrentProbe, DefaultInputs, LimFunctionInput, NoInput, ParameterInput, PartialTimeDerivative,
    Voltage,
};
use crate::{fmt, BinOp, Operand, OperandData};
use crate::{COperand, CfgInputs, ConstVal, RValue, Spanned};
use derive_more::{Display, From, TryInto};
use enum_dispatch::enum_dispatch;
use data_structures::index_vec::{index_vec, IndexSlice};
use ir::ids::{CallArg, NodeId};
use ir::{Print, StopTask};
use macros::CfgFunctions;
use session::symbols::Symbol;
use session::{
    sourcemap::{Span, StringLiteral},
    symbols::{kw, sysfun},
};
use osdi_types::ConstVal::Scalar;
use osdi_types::SimpleConstVal::Real;
use std::convert::TryFrom;
use std::fmt::{Debug, Display, Formatter};

pub trait CfgFunctions: Debug + Clone + Display {
    type I: CfgInputs;

    fn const_fold(&self, call: &[FlatSet<ConstVal>]) -> FlatSet<ConstVal>;
    fn derivative<C: CfgFunctions>(
        &self,
        args: &IndexSlice<CallArg, [COperand<Self>]>,
        ad: &mut RValueAutoDiff<Self, C>,
        span: Span,
    ) -> Option<RValue<Self>>;
}

pub trait CfgFunctionEnum: CfgFunction<Self> {
    type I: CfgInputs;
}

#[enum_dispatch]
pub trait CfgFunction<F: CfgFunctions = Self>: Debug + Clone + Display {
    fn fconst_fold(&self, call: &[FlatSet<ConstVal>]) -> FlatSet<ConstVal>;
    fn fderivative<X: CfgFunctions>(
        &self,
        args: &IndexSlice<CallArg, [COperand<F>]>,
        ad: &mut RValueAutoDiff<F, X>,
        span: Span,
    ) -> Option<RValue<F>>;
}

impl<T: CfgFunctionEnum> CfgFunctions for T {
    type I = <Self as CfgFunctionEnum>::I;

    #[inline(always)]
    fn const_fold(&self, call: &[FlatSet<ConstVal>]) -> FlatSet<ConstVal> {
        CfgFunction::fconst_fold(self, call)
    }

    #[inline(always)]
    fn derivative<C: CfgFunctions>(
        &self,
        args: &IndexSlice<CallArg, [COperand<Self>]>,
        ad: &mut RValueAutoDiff<Self, C>,
        span: Span,
    ) -> Option<RValue<Self>> {
        CfgFunction::fderivative(self, args, ad, span)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Noise {
    White,
    Flicker,
    Table,
    TableLog,
    FileTable,
    FileTableLog,
}

impl Noise {
    pub const fn symbol(&self) -> Symbol {
        match self {
            Self::White => kw::white_noise,
            Self::Flicker => kw::flicker_noise,
            Self::Table | Self::FileTable => kw::noise_table,
            Self::TableLog | Self::FileTableLog => kw::noise_table_log,
        }
    }
}

impl Display for Noise {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.symbol())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Display)]
#[display(fmt = "{}", "noise_type")]
pub struct NoiseCall {
    pub noise_type: Noise,
    pub name: Option<StringLiteral>,
}

impl<F: CfgFunctions> CfgFunction<F> for NoiseCall {
    fn fconst_fold(&self, _call: &[FlatSet<ConstVal>]) -> FlatSet<ConstVal> {
        FlatSet::Top
    }

    #[inline(always)]
    fn fderivative<X: CfgFunctions>(
        &self,
        _args: &IndexSlice<CallArg, [COperand<F>]>,
        _ad: &mut RValueAutoDiff<F, X>,
        _span: Span,
    ) -> Option<RValue<F>> {
        None
    }
}

#[derive(Debug, PartialEq, Clone, Display)]
#[display(fmt = "{}", kw::ddt)]
pub struct TimeDerivative(pub Option<f64>);

impl<F: CfgFunctions> CfgFunction<F> for TimeDerivative
where
    F: CfgFunctions,
    <F as CfgFunctions>::I: From<PartialTimeDerivative>,
{
    #[inline(always)]
    fn fconst_fold(&self, _call: &[FlatSet<ConstVal>]) -> FlatSet<ConstVal> {
        FlatSet::Top
    }

    #[inline]
    fn fderivative<X: CfgFunctions>(
        &self,
        args: &IndexSlice<CallArg, [COperand<F>]>,
        ad: &mut RValueAutoDiff<F, X>,
        span: Span,
    ) -> Option<RValue<F>> {
        let inner_derivative = ad.derivative(&args[0]).into_option()?;
        let outer_derivative = OperandData::Read(PartialTimeDerivative.into());
        // No point in caching a constant so we dont use ad.chain_rule
        RValue::BinaryOperation(
            Spanned { contents: BinOp::Multiply, span },
            Operand { span, contents: outer_derivative },
            Operand { contents: inner_derivative, span },
        )
        .into()
    }
}

#[derive(Debug, Clone, Display)]
#[display(fmt = "{}({})", "sysfun::discontinuity", "0")]
pub struct Discontinuity(pub i64);

impl<F> CfgFunction<F> for Discontinuity
where
    F: CfgFunctions,
{
    fn fconst_fold(&self, _call: &[FlatSet<ConstVal>]) -> FlatSet<ConstVal> {
        unreachable!()
    }

    fn fderivative<X: CfgFunctions>(
        &self,
        _args: &IndexSlice<CallArg, [COperand<F>]>,
        _ad: &mut RValueAutoDiff<F, X>,
        _span: Span,
    ) -> Option<RValue<F>> {
        unreachable!()
    }
}

#[derive(Debug, Clone, Display)]
#[display(fmt = "{}{:?}", "kw::analysis", "0")]
pub struct Analysis(pub Box<[StringLiteral]>);

impl<F> CfgFunction<F> for Analysis
where
    F: CfgFunctions,
{
    fn fconst_fold(&self, _call: &[FlatSet<ConstVal>]) -> FlatSet<ConstVal> {
        FlatSet::Top
    }

    fn fderivative<X: CfgFunctions>(
        &self,
        _args: &IndexSlice<CallArg, [COperand<F>]>,
        _ad: &mut RValueAutoDiff<F, X>,
        _span: Span,
    ) -> Option<RValue<F>> {
        None
    }
}

#[derive(Debug, Clone, Display)]
#[display(fmt = "{}({})", "kw::ac_stim", "0")]
pub struct AcStimulus(pub Symbol);

impl<F> CfgFunction<F> for AcStimulus
where
    F: CfgFunctions + From<Self>,
{
    fn fconst_fold(&self, _call: &[FlatSet<ConstVal>]) -> FlatSet<ConstVal> {
        FlatSet::Top // can only be a constant folded in specific analysis modes (complex number in SS, 0 in DC)
    }

    fn fderivative<X: CfgFunctions>(
        &self,
        args: &IndexSlice<CallArg, [COperand<F>]>,
        ad: &mut RValueAutoDiff<F, X>,
        span: Span,
    ) -> Option<RValue<F>> {
        // d [A*exp(jp)] / dx = dA/dx * exp(jp) + (A*dp/dx) * exp[j(p+ pi/2)]

        // dA/dx * exp(jp)
        let dmag = ad.derivative(&args[1]).into_option();
        let lhs = dmag.map(|dmag| {
            let span = args[0].span;
            let dmag = Operand::new(dmag, span);
            RValue::Call(AcStimulus(self.0).into(), index_vec![dmag, args[1].clone()], span)
        });

        // (A*dp/dx) * exp[j(p+ pi/2)]
        let dphase = ad.derivative(&args[2]).into_option();
        let rhs = dphase.map(|dphase| {
            let span = args[1].span;
            let dphase = Operand::new(dphase, span);
            let mag = ad.gen_binop(BinOp::Multiply, args[0].clone(), dphase, span);

            let phase_offset = Operand::new(Scalar(Real(std::f64::consts::PI / 2.0)).into(), span);
            let phase = ad.gen_binop(BinOp::Plus, args[1].clone(), phase_offset, span);

            RValue::Call(AcStimulus(self.0).into(), index_vec![mag, phase], span)
        });

        match (lhs, rhs) {
            (None, None) => None,
            (None, Some(rhs)) => rhs.into(),
            (Some(lhs), None) => lhs.into(),
            (Some(lhs), Some(rhs)) => {
                let lhs = ad.gen_temporary(lhs, args[0].span);
                let rhs = ad.gen_temporary(rhs, args[1].span);
                RValue::BinaryOperation(Spanned::new(BinOp::Plus, span), lhs, rhs).into()
            }
        }
    }
}

#[derive(Debug, Clone, Display)]
#[display(fmt = "{}({}, {:?})", "sysfun::limit", "voltage", "fun")]
pub struct VoltageLim {
    pub voltage: Voltage,
    pub fun: LimFunction,
}

impl<F> CfgFunction<F> for VoltageLim
where
    F: CfgFunctions,
{
    fn fconst_fold(&self, _call: &[FlatSet<ConstVal>]) -> FlatSet<ConstVal> {
        FlatSet::Top
    }

    fn fderivative<X: CfgFunctions>(
        &self,
        _args: &IndexSlice<CallArg, [COperand<F>]>,
        ad: &mut RValueAutoDiff<F, X>,
        span: Span,
    ) -> Option<RValue<F>> {
        let derivative = self.voltage.derivative(ad.unknown, ad.ad.mir).into_option()?;

        Some(RValue::Use(Spanned { span, contents: OperandData::Constant(derivative) }))
    }
}

#[derive(Debug, Clone, Display)]
#[display(fmt = "{}({}, {:?})", "sysfun::limit", "current", "fun")]
pub struct CurrentLim {
    pub current: CurrentProbe,
    pub fun: LimFunction,
}

impl<F> CfgFunction<F> for CurrentLim
where
    F: CfgFunctions,
{
    fn fconst_fold(&self, _call: &[FlatSet<ConstVal>]) -> FlatSet<ConstVal> {
        FlatSet::Top
    }

    fn fderivative<X: CfgFunctions>(
        &self,
        _args: &IndexSlice<CallArg, [COperand<F>]>,
        ad: &mut RValueAutoDiff<F, X>,
        span: Span,
    ) -> Option<RValue<F>> {
        let derivative = self.current.derivative(ad.unknown, ad.ad.mir).into_option()?;

        Some(RValue::Use(Spanned { span, contents: OperandData::Constant(derivative) }))
    }
}

//TODO Currrent lim

#[derive(Debug, PartialEq, Eq, Clone, Display)]
#[display(fmt = "$collapse({:?},{:?})", "0", "1")]
pub struct NodeCollapse(pub NodeId, pub NodeId);

impl<F> CfgFunction<F> for NodeCollapse
where
    F: CfgFunctions,
{
    fn fconst_fold(&self, _call: &[FlatSet<ConstVal>]) -> FlatSet<ConstVal> {
        unreachable!("node collapse functions do not return a value")
    }

    fn fderivative<X: CfgFunctions>(
        &self,
        _args: &IndexSlice<CallArg, [COperand<F>]>,
        _ad: &mut RValueAutoDiff<F, X>,
        _span: Span,
    ) -> Option<RValue<F>> {
        unreachable!("node collapse functions do not return a value")
    }
}

impl<F> CfgFunction<F> for StopTask
where
    F: CfgFunctions,
{
    fn fconst_fold(&self, _call: &[FlatSet<ConstVal>]) -> FlatSet<ConstVal> {
        unreachable!("stop tasks do not return a value")
    }

    fn fderivative<X: CfgFunctions>(
        &self,
        _args: &IndexSlice<CallArg, [COperand<F>]>,
        _ad: &mut RValueAutoDiff<F, X>,
        _span: Span,
    ) -> Option<RValue<F>> {
        unreachable!("stop tasks do not return a value")
    }
}

impl<F> CfgFunction<F> for Print
where
    F: CfgFunctions,
{
    fn fconst_fold(&self, _call: &[FlatSet<ConstVal>]) -> FlatSet<ConstVal> {
        unreachable!("print functions do not return a value")
    }

    fn fderivative<X: CfgFunctions>(
        &self,
        _args: &IndexSlice<CallArg, [COperand<F>]>,
        _ad: &mut RValueAutoDiff<F, X>,
        _span: Span,
    ) -> Option<RValue<F>> {
        unreachable!("print functions do not return a value")
    }
}

#[derive(Debug, Clone, Display)]
pub enum LimFunction<C: CfgFunctions = LimFunctionDefaultFunctions> {
    #[display(fmt = "{}", "0")]
    Native(StringLiteral),
    #[display(fmt = "va_function")]
    VerilogA(ControlFlowGraph<C>), // TODO user limit functions
}

#[derive(Clone, Debug, Display, From, TryInto, CfgFunctions)]
#[cfg_inputs(DefaultInputs)]
pub enum DefaultFunctions {
    Noise(NoiseCall),
    TimeDerivative(TimeDerivative),
    StopTask(StopTask),
    Print(Print),
    NodeCollapse(NodeCollapse),
    VoltageLim(VoltageLim),
    CurrentLim(CurrentLim),
    AcStimulus(AcStimulus),
    Analysis(Analysis),
    Discontinuity(Discontinuity),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Display, CfgFunctions)]
#[cfg_inputs(ParameterInput)]
pub enum ParameterCallType {}

impl TryFrom<DefaultFunctions> for ParameterCallType {
    type Error = ();

    fn try_from(_: DefaultFunctions) -> Result<Self, Self::Error> {
        Err(())
    }
}

impl From<ParameterCallType> for DefaultFunctions {
    fn from(src: ParameterCallType) -> DefaultFunctions {
        match src {}
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Display, CfgFunctions)]
#[cfg_inputs(NoInput)]
pub enum EmptyCallType {}

impl TryFrom<DefaultFunctions> for EmptyCallType {
    type Error = ();

    fn try_from(_: DefaultFunctions) -> Result<Self, Self::Error> {
        Err(())
    }
}

#[derive(Clone, Debug, Display, From, TryInto, CfgFunctions)]
#[cfg_inputs(LimFunctionInput)]
pub enum LimFunctionDefaultFunctions {
    StopTask(StopTask),
    Print(Print),
    Analysis(Analysis),
    Discontinuity(Discontinuity),
}

impl From<ParameterCallType> for LimFunctionDefaultFunctions {
    fn from(src: ParameterCallType) -> LimFunctionDefaultFunctions {
        match src {}
    }
}

// #[derive(Clone, Debug, Copy, Eq, PartialEq)]
// pub enum PrintOnFinish {
//     Nothing,
//     Location,
//     LocationAndResourceUsage,
// }
//
// impl TryFrom<i64> for PrintOnFinish {
//     type Error = ();
//
//     fn try_from(finish_number: i64) -> Result<Self, Self::Error> {
//         Ok(match finish_number {
//             0 => Self::Nothing,
//             1 => Self::Location,
//             2 => Self::LocationAndResourceUsage,
//             _ => return Err(()),
//         })
//     }
// }
