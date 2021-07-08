/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::lowering::{HirFold, HirLowering, LocalCtx, TyPrinter};
use crate::{
    AllowedOperation, AllowedOperations, BranchKind, ConstVal, Expression, ExpressionId,
    FunctionId, Hir, Spanned, Type, Unknown,
};
use enumset::enum_set;
use openvaf_constants::Constants;
use openvaf_data_structures::index_vec::{index_vec, IndexSlice, IndexVec};
use openvaf_ir::{ids::CallArg, DisciplineAccess, Math1, Math2, Print, SimpleType, StopTask};
use openvaf_middle::osdi_types::SimpleConstVal::Real;
use openvaf_middle::{
    functions, BinOp, CfgConversion, CfgInputs, Operand, OperandData, RValue, StmntKind, TyRValue,
};
use openvaf_session::{
    sourcemap::Span,
    symbols::{kw, sym, sysfun, Symbol},
};
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::option::Option::{None, Some};

use crate::lowering::errors::Error::{
    DerivativeNotAllowed, ExpectedRealFunction, ExpectedStringOrArray, FunctionExpectedReference,
    UserLimFunctionWithOutputArgs,
};
use crate::lowering::errors::{ArgumentCountMismatch, Error};
use derive_more::Display;
use enum_dispatch::enum_dispatch;
use openvaf_data_structures::iter::zip;
use openvaf_middle::functions::{
    CurrentLim, DefaultFunctions, LimFunctionDefaultFunctions, NoiseCall, TimeDerivative,
    VoltageLim,
};
use openvaf_middle::inputs::{
    CurrentProbe, DefaultInputs, LimFunctionInput, ParameterInput, PortConnected, SimParam,
    SimParamKind, Temperature, Voltage,
};
use openvaf_middle::osdi_types::ConstVal::Scalar;
use openvaf_session::sourcemap::StringLiteral;

#[enum_dispatch(HirFunction)]
#[derive(Display, Clone, Copy, Debug, PartialEq)]
pub enum Function {
    User(FunctionId),
    ParameterizationRead,
    Math1,
    Math2,
    Print,
    Noise,
    StopTask,
    Derivative,
    LimFunction,
    Discontinuity,
    AcStimulus,
    Analysis,
}

impl Function {
    pub fn check_arg_length<L: HirLowering>(
        &self,
        args: &IndexSlice<CallArg, [ExpressionId]>,
        fold: &mut HirFold<L>,
        span: Span,
    ) {
        let arg_min = self.min_arg_cnt(fold.hir);
        let arg_max = self.max_arg_cnt(fold.hir);
        let arg_cnt = args.len() as u8;
        if arg_cnt < arg_min || arg_max.map_or(false, |arg_max| arg_cnt > arg_max) {
            fold.errors.add(Error::ArgCountMissMatch {
                missmatch: ArgumentCountMismatch {
                    at_most: arg_max,
                    at_least: arg_min,
                    found: arg_cnt,
                },
                span,
                name: self.symbol(fold.hir),
                decl: self.decl(fold.hir),
            })
        }
    }
}

pub enum FunctionLoweringReturn {
    Val(TyRValue),
    Void(DefaultFunctions, IndexVec<CallArg, Operand>, Span),
}

impl From<TyRValue> for FunctionLoweringReturn {
    fn from(val: TyRValue) -> Self {
        Self::Val(val)
    }
}

#[enum_dispatch]
pub trait HirFunction: Clone + Debug + Display {
    fn min_arg_cnt(&self, hir: &Hir) -> u8;
    fn max_arg_cnt(&self, hir: &Hir) -> Option<u8> {
        Some(self.min_arg_cnt(hir))
    }
    fn symbol(&self, hir: &Hir) -> Symbol;
    fn has_side_effects(&self, hir: &Hir) -> bool;
    fn returns_value(&self, hir: &Hir) -> bool;
    fn lower<L: HirLowering>(
        &self,
        ctx: &mut LocalCtx<L>,
        args: &IndexSlice<CallArg, [ExpressionId]>,
        span: Span,
    ) -> Option<FunctionLoweringReturn>;

    #[inline(always)]
    fn allowed_arg_operation(&self, _arg: CallArg) -> Option<AllowedOperations> {
        None
    }

    fn decl(&self, _hir: &Hir) -> Option<Span> {
        None
    }
}

impl HirFunction for StopTask {
    fn min_arg_cnt(&self, _hir: &Hir) -> u8 {
        0
    }

    fn max_arg_cnt(&self, _hir: &Hir) -> Option<u8> {
        Some(1)
    }

    fn symbol(&self, _hir: &Hir) -> Symbol {
        self.symbol()
    }

    fn has_side_effects(&self, _hir: &Hir) -> bool {
        true
    }

    fn returns_value(&self, _hir: &Hir) -> bool {
        false
    }

    fn lower<L: HirLowering>(
        &self,
        _ctx: &mut LocalCtx<L>,
        _args: &IndexSlice<CallArg, [ExpressionId]>,
        span: Span,
    ) -> Option<FunctionLoweringReturn> {
        // TODO warn that the finish number is ignored
        Some(FunctionLoweringReturn::Void(
            (*self).into(),
            IndexVec::new(),
            span,
        ))
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug, Display)]
#[display(fmt = "{}", "sysfun::discontinuity")]
pub struct Discontinuity;

impl HirFunction for Discontinuity {
    fn min_arg_cnt(&self, _hir: &Hir) -> u8 {
        1
    }

    fn symbol(&self, _hir: &Hir) -> Symbol {
        sysfun::discontinuity
    }

    fn has_side_effects(&self, _hir: &Hir) -> bool {
        true
    }

    fn returns_value(&self, _hir: &Hir) -> bool {
        false
    }

    fn lower<L: HirLowering>(
        &self,
        ctx: &mut LocalCtx<L>,
        args: &IndexSlice<CallArg, [ExpressionId]>,
        span: Span,
    ) -> Option<FunctionLoweringReturn> {
        let val = ctx.fold.eval_int_constant(args[0])?;
        FunctionLoweringReturn::Void(
            openvaf_middle::functions::Discontinuity(val).into(),
            IndexVec::new(),
            span,
        )
        .into()
    }

    fn allowed_arg_operation(&self, _arg: CallArg) -> Option<AllowedOperations> {
        Some(AllowedOperations::empty())
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug, Display)]
#[display(fmt = "{}", "kw::ac_stim")]
pub struct AcStimulus;

impl HirFunction for AcStimulus {
    fn min_arg_cnt(&self, _hir: &Hir) -> u8 {
        0
    }

    fn max_arg_cnt(&self, _hir: &Hir) -> Option<u8> {
        Some(3)
    }

    fn symbol(&self, _hir: &Hir) -> Symbol {
        kw::ac_stim
    }

    fn has_side_effects(&self, _hir: &Hir) -> bool {
        false
    }

    fn returns_value(&self, _hir: &Hir) -> bool {
        true
    }

    fn lower<L: HirLowering>(
        &self,
        ctx: &mut LocalCtx<L>,
        args: &IndexSlice<CallArg, [ExpressionId]>,
        span: Span,
    ) -> Option<FunctionLoweringReturn> {
        let name = args
            .get(0)
            .and_then(|name| {
                let name = ctx.fold.eval_string_constant(*name)?;
                Symbol::intern(&name.unescaped_contents()).into()
            })
            .unwrap_or(sym::ac);

        let mag = args
            .get(1)
            .and_then(|mag| ctx.fold_real(*mag))
            .unwrap_or(Operand {
                contents: Scalar(Real(1.0)).into(),
                span,
            });

        let phase = args
            .get(1)
            .and_then(|phase| ctx.fold_real(*phase))
            .unwrap_or(Operand {
                contents: Scalar(Real(1.0)).into(),
                span,
            });

        FunctionLoweringReturn::Val(TyRValue {
            val: RValue::Call(
                openvaf_middle::functions::AcStimulus(name).into(),
                index_vec![mag, phase],
                span,
            ),
            ty: Type::REAL,
        })
        .into()
    }

    fn allowed_arg_operation(&self, arg: CallArg) -> Option<AllowedOperations> {
        if arg.raw() == 0 {
            Some(AllowedOperations::empty())
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug, Display)]
#[display(fmt = "{}", "kw::analysis")]
pub struct Analysis;

impl HirFunction for Analysis {
    fn min_arg_cnt(&self, _hir: &Hir) -> u8 {
        1
    }

    fn max_arg_cnt(&self, _hir: &Hir) -> Option<u8> {
        None
    }

    fn symbol(&self, _hir: &Hir) -> Symbol {
        kw::analysis
    }

    fn has_side_effects(&self, _hir: &Hir) -> bool {
        false
    }

    fn returns_value(&self, _hir: &Hir) -> bool {
        true
    }

    fn lower<L: HirLowering>(
        &self,
        ctx: &mut LocalCtx<L>,
        args: &IndexSlice<CallArg, [ExpressionId]>,
        span: Span,
    ) -> Option<FunctionLoweringReturn> {
        let names = args
            .iter()
            .filter_map(|name| ctx.fold.eval_string_constant(*name))
            .collect();

        FunctionLoweringReturn::Val(TyRValue {
            val: RValue::Call(
                openvaf_middle::functions::Analysis(names).into(),
                IndexVec::new(),
                span,
            ),
            ty: Type::BOOL,
        })
        .into()
    }

    fn allowed_arg_operation(&self, _arg: CallArg) -> Option<AllowedOperations> {
        Some(AllowedOperations::empty())
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum Derivative {
    Time,
    Partial,
}

impl Derivative {
    pub fn symbol(&self) -> Symbol {
        match self {
            Self::Time => kw::ddt,
            Self::Partial => kw::ddx,
        }
    }
}

impl Display for Derivative {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.symbol())
    }
}

impl HirFunction for Derivative {
    fn min_arg_cnt(&self, _hir: &Hir) -> u8 {
        match self {
            Derivative::Time => 1,
            Derivative::Partial => 2,
        }
    }

    fn max_arg_cnt(&self, _hir: &Hir) -> Option<u8> {
        Some(2)
    }

    fn symbol(&self, _hir: &Hir) -> Symbol {
        self.symbol()
    }

    fn has_side_effects(&self, _hir: &Hir) -> bool {
        false
    }

    fn returns_value(&self, _hir: &Hir) -> bool {
        true
    }

    #[inline(always)]
    fn allowed_arg_operation(&self, arg: CallArg) -> Option<AllowedOperations> {
        if arg == 1 {
            match self {
                Derivative::Time => Some(AllowedOperations::empty()),
                Derivative::Partial => {
                    Some(AllowedOperations::all()) // Allow everything here. We create a better error during hir lowering
                }
            }
        } else {
            None
        }
    }

    fn lower<L: HirLowering>(
        &self,
        ctx: &mut LocalCtx<L>,
        args: &IndexSlice<CallArg, [ExpressionId]>,
        span: Span,
    ) -> Option<FunctionLoweringReturn> {
        let val = ctx.fold_real(args[0]);

        let val = match self {
            Self::Time => {
                let abstol = args.get(1).and_then(|abstol| {
                    if let Expression::NatureReference(nature) = ctx.fold.hir[*abstol].contents {
                        Some(ctx.fold.mir[nature].abstol)
                    } else {
                        ctx.fold.eval_real_constant(*abstol)
                    }
                });

                RValue::Call(TimeDerivative(abstol).into(), index_vec![val?], span)
            }

            Self::Partial => {
                let unknown = match ctx.fold.hir[args[1]].contents {
                    Expression::FunctionCall(
                        Function::ParameterizationRead(ParameterizationRead::Temperature),
                        _,
                        _,
                    ) => Unknown::Temperature,
                    Expression::BranchAccess(DisciplineAccess::Flow, branch) => {
                        Unknown::Flow(branch)
                    }
                    Expression::BranchAccess(DisciplineAccess::Potential, branch)
                        if ctx.fold.hir.branches[branch].kind == BranchKind::UnnamedToGnd =>
                    {
                        Unknown::NodePotential(ctx.fold.hir.branches[branch].hi)
                    }
                    Expression::BranchAccess(DisciplineAccess::Potential, branch)
                        if ctx.fold.hir.branches[branch].kind == BranchKind::Unnamed =>
                    {
                        let branch = &ctx.fold.hir.branches[branch];
                        Unknown::BranchPotential(branch.hi, branch.lo)
                    }
                    _ => {
                        ctx.fold
                            .errors
                            .add(DerivativeNotAllowed(ctx.fold.hir[args[1]].span));
                        return None;
                    }
                };

                let derivative = ctx
                    .cfg_builder
                    .cfg
                    .demand_operand_derivative_unchecked(&ctx.fold.mir, &val?, unknown)
                    .into_operand();
                Operand::new(derivative, span).into()
            }
        };

        FunctionLoweringReturn::Val(TyRValue {
            val,
            ty: Type::REAL,
        })
        .into()
    }
}

#[derive(Debug, Clone, Display, Copy, PartialEq)]
pub enum LimFunction {
    #[display(fmt = "{}({})", "sysfun::limit", "0")]
    Native(StringLiteral),
    #[display(fmt = "{}({})", "sysfun::limit", "0")]
    VerilogA(FunctionId),
}

impl LimFunction {
    pub fn symbol(&self) -> Symbol {
        sysfun::limit
    }
}

impl HirFunction for LimFunction {
    fn min_arg_cnt(&self, hir: &Hir) -> u8 {
        match *self {
            Self::Native(_) => 2,
            Self::VerilogA(fun) => hir[fun].args.len() as u8 + 2,
        }
    }

    fn max_arg_cnt(&self, hir: &Hir) -> Option<u8> {
        match *self {
            Self::Native(_) => None,
            Self::VerilogA(fun) => Some(hir[fun].args.len() as u8 + 2),
        }
    }

    fn symbol(&self, _hir: &Hir) -> Symbol {
        self.symbol()
    }

    fn has_side_effects(&self, _hir: &Hir) -> bool {
        true
    }

    fn returns_value(&self, _hir: &Hir) -> bool {
        true
    }

    #[inline(always)]
    fn allowed_arg_operation(&self, arg: CallArg) -> Option<AllowedOperations> {
        match arg.raw() {
            0 => Some(AllowedOperations::only(AllowedOperation::BranchAccess)),
            1 => Some(AllowedOperations::only(
                AllowedOperation::UserFunctionReference,
            )),
            _ => None,
        }
    }

    fn lower<L: HirLowering>(
        &self,
        ctx: &mut LocalCtx<L>,
        args: &IndexSlice<CallArg, [ExpressionId]>,
        span: Span,
    ) -> Option<FunctionLoweringReturn> {
        let (fun, fun_args): (_, IndexVec<_, _>) = match self {
            LimFunction::Native(name) => {
                let args = args.as_raw_slice()[2..]
                    .iter()
                    .filter_map(|x| {
                        ctx.lower_expr(*x)
                            .map(|val| ctx.rvalue_to_operand(val, span))
                    })
                    .collect();
                (openvaf_middle::functions::LimFunction::Native(*name), args)
            }
            LimFunction::VerilogA(fun) => {
                let fun = &ctx.fold.hir.functions[*fun];
                let ty = ctx.fold.hir[fun.return_variable].ty;
                if ty != Type::REAL {
                    ctx.fold.errors.add(ExpectedRealFunction {
                        span,
                        function: fun.ident,
                        ty,
                    })
                }

                let args = zip(&fun.args, &args.as_raw_slice()[2..])
                    .filter_map(|(decl, val)| {
                        let var = ctx.fold.hir[decl.local_var];
                        if decl.output {
                            ctx.fold.errors.add(UserLimFunctionWithOutputArgs {
                                arg: var.ident,
                                arg_decl: ctx.fold.hir[var.sctx].span,
                                function: fun.ident,
                                call: span,
                            })
                        }
                        ctx.lower_assignment_expr(*val, var.ty)
                            .map(|val| ctx.rvalue_to_operand(val, span))
                    })
                    .collect();

                let mut lctx = LocalCtx::new_fn(ctx.fold);
                lctx.lower_block(&fun.body);
                let cfg = lctx.cfg_builder.finish(fun.sctx);
                let cfg = cfg.map(&mut DefaultToLimFun);

                (openvaf_middle::functions::LimFunction::VerilogA(cfg), args)
            }
        };

        let fun: openvaf_middle::functions::LimFunction = fun;

        let call = match ctx.fold.hir[args[0]].contents {
            Expression::BranchAccess(DisciplineAccess::Potential, branch) => {
                let branch = &ctx.fold.hir[branch];
                VoltageLim {
                    voltage: Voltage {
                        lo: branch.lo,
                        hi: branch.hi,
                    },
                    fun,
                }
                .into()
            }
            Expression::BranchAccess(DisciplineAccess::Flow, branch) => CurrentLim {
                current: CurrentProbe(branch),
                fun,
            }
            .into(),
            _ => {
                todo!("Error");
            }
        };

        FunctionLoweringReturn::Val(TyRValue {
            val: RValue::Call(call, fun_args, span),
            ty: Type::REAL,
        })
        .into()
    }
}

pub struct DefaultToLimFun;

impl CfgConversion<DefaultFunctions, LimFunctionDefaultFunctions> for DefaultToLimFun {
    fn map_input(&mut self, src: DefaultInputs) -> OperandData<LimFunctionInput> {
        let input = match src {
            DefaultInputs::Parameter(param) => LimFunctionInput::Parameter(param),
            DefaultInputs::PortConnected(port_connected) => {
                LimFunctionInput::PortConnected(port_connected)
            }
            DefaultInputs::SimParam(param) => LimFunctionInput::SimParam(param),
            DefaultInputs::Voltage(_)
            | DefaultInputs::CurrentProbe(_)
            | DefaultInputs::PartialTimeDerivative(_) => unreachable!(),
            DefaultInputs::Temperature(temp) => LimFunctionInput::Temperature(temp),
        };
        OperandData::Read(input)
    }

    fn map_call_val(
        &mut self,
        call: DefaultFunctions,
        args: IndexVec<CallArg, Operand<DefaultInputs>>,
        span: Span,
    ) -> RValue<LimFunctionDefaultFunctions> {
        let call = match call {
            DefaultFunctions::TimeDerivative(_)
            | DefaultFunctions::NodeCollapse(_)
            | DefaultFunctions::VoltageLim(_)
            | DefaultFunctions::CurrentLim(_) => unreachable!(),
            DefaultFunctions::StopTask(task) => task.into(),
            DefaultFunctions::Print(print) => print.into(),
            DefaultFunctions::Discontinuity(discontinuity) => discontinuity.into(),
            DefaultFunctions::Analysis(_)
            | DefaultFunctions::AcStimulus(_)
            | DefaultFunctions::Noise(_) => todo!("Error"),
        };

        RValue::Call(
            call,
            args.into_iter()
                .map(|arg| Self::map_operand(self, arg))
                .collect(),
            span,
        )
    }

    fn map_call_stmnt(
        &mut self,
        call: DefaultFunctions,
        args: IndexVec<CallArg, Operand<DefaultInputs>>,
        span: Span,
    ) -> StmntKind<LimFunctionDefaultFunctions> {
        let call = match call {
            DefaultFunctions::TimeDerivative(_)
            | DefaultFunctions::NodeCollapse(_)
            | DefaultFunctions::VoltageLim(_)
            | DefaultFunctions::CurrentLim(_) => unreachable!(),
            DefaultFunctions::StopTask(task) => task.into(),
            DefaultFunctions::Print(print) => print.into(),
            DefaultFunctions::Discontinuity(discontinuity) => discontinuity.into(),
            DefaultFunctions::Analysis(_)
            | DefaultFunctions::AcStimulus(_)
            | DefaultFunctions::Noise(_) => todo!("Error"),
        };
        StmntKind::Call(
            call,
            args.into_iter()
                .map(|arg| Self::map_operand(self, arg))
                .collect(),
            span,
        )
    }
}

impl HirFunction for Print {
    fn min_arg_cnt(&self, _hir: &Hir) -> u8 {
        1
    }

    fn max_arg_cnt(&self, _hir: &Hir) -> Option<u8> {
        None
    }

    fn symbol(&self, _hir: &Hir) -> Symbol {
        self.symbol()
    }

    fn has_side_effects(&self, _hir: &Hir) -> bool {
        true
    }

    fn returns_value(&self, _hir: &Hir) -> bool {
        false
    }

    fn lower<L: HirLowering>(
        &self,
        ctx: &mut LocalCtx<L>,
        args: &IndexSlice<CallArg, [ExpressionId]>,
        span: Span,
    ) -> Option<FunctionLoweringReturn> {
        let args = args
            .iter()
            .filter_map(|&arg| {
                let span = ctx.fold.hir[arg].span;
                let arg = ctx.lower_expr(arg)?;
                Some(ctx.rvalue_to_operand(arg, span))
            })
            .collect();
        Some(FunctionLoweringReturn::Void((*self).into(), args, span))
    }
}

impl HirFunction for Math1 {
    fn min_arg_cnt(&self, _hir: &Hir) -> u8 {
        1
    }

    fn symbol(&self, _hir: &Hir) -> Symbol {
        self.symbol()
    }

    fn has_side_effects(&self, _hir: &Hir) -> bool {
        false
    }

    fn returns_value(&self, _hir: &Hir) -> bool {
        true
    }

    fn lower<L: HirLowering>(
        &self,
        ctx: &mut LocalCtx<L>,
        args: &IndexSlice<CallArg, [ExpressionId]>,
        span: Span,
    ) -> Option<FunctionLoweringReturn> {
        let arg = args[0];
        let (arg, ty) = match self {
            Self::Abs => ctx.fold_numeric(arg)?,
            Self::Clog2 => (ctx.fold_int(arg)?, Type::INT),
            _ => (ctx.fold_real(arg)?, Type::REAL),
        };

        FunctionLoweringReturn::Val(TyRValue {
            val: RValue::Math1(Spanned::new(*self, span), arg),
            ty,
        })
        .into()
    }
}

impl HirFunction for Math2 {
    fn min_arg_cnt(&self, _hir: &Hir) -> u8 {
        2
    }

    fn symbol(&self, _hir: &Hir) -> Symbol {
        self.symbol()
    }

    fn has_side_effects(&self, _hir: &Hir) -> bool {
        false
    }

    fn returns_value(&self, _hir: &Hir) -> bool {
        true
    }

    fn lower<L: HirLowering>(
        &self,
        ctx: &mut LocalCtx<L>,
        args: &IndexSlice<CallArg, [ExpressionId]>,
        span: Span,
    ) -> Option<FunctionLoweringReturn> {
        let (arg1, arg2, ty) = match self {
            Math2::Pow | Math2::ArcTan2 | Math2::Hypot => {
                let (arg1, arg2) = ctx.fold_real_binop(args[0], args[1])?;
                (arg1, arg2, Type::REAL)
            }
            Math2::Min | Math2::Max => ctx.fold_numeric_binop(args[0], args[1])?,
        };

        FunctionLoweringReturn::Val(TyRValue {
            val: RValue::Math2(Spanned::new(*self, span), arg1, arg2),
            ty,
        })
        .into()
    }
}

impl HirFunction for FunctionId {
    #[inline]
    fn min_arg_cnt(&self, hir: &Hir) -> u8 {
        hir.functions[*self].args.len() as u8
    }

    #[inline]
    fn symbol(&self, hir: &Hir) -> Symbol {
        hir[*self].ident.name
    }

    fn has_side_effects(&self, hir: &Hir) -> bool {
        hir[*self].args.iter().any(|x| x.output)
    }

    fn returns_value(&self, _hir: &Hir) -> bool {
        true
    }

    fn lower<L: HirLowering>(
        &self,
        ctx: &mut LocalCtx<L>,
        args: &IndexSlice<CallArg, [ExpressionId]>,
        span: Span,
    ) -> Option<FunctionLoweringReturn> {
        FunctionLoweringReturn::Val(ctx.lower_user_function_call(*self, args, span)?).into()
    }
    fn decl(&self, hir: &Hir) -> Option<Span> {
        Some(hir[*self].ident.span)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Noise {
    White,
    Flicker,
    Table,
    TableLog,
}

impl Noise {
    pub const fn symbol(&self) -> Symbol {
        match self {
            Self::Table => kw::noise_table,
            Self::White => kw::white_noise,
            Self::Flicker => kw::flicker_noise,
            Self::TableLog => kw::noise_table_log,
        }
    }
}

impl Display for Noise {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.symbol())
    }
}

impl HirFunction for Noise {
    fn min_arg_cnt(&self, _hir: &Hir) -> u8 {
        match self {
            Self::Flicker => 2,
            _ => 1,
        }
    }

    fn max_arg_cnt(&self, hir: &Hir) -> Option<u8> {
        Some(self.min_arg_cnt(hir) + 1)
    }

    fn symbol(&self, _hir: &Hir) -> Symbol {
        self.symbol()
    }

    fn has_side_effects(&self, _hir: &Hir) -> bool {
        false
    }

    fn returns_value(&self, _hir: &Hir) -> bool {
        true
    }

    fn lower<L: HirLowering>(
        &self,
        ctx: &mut LocalCtx<L>,
        args: &IndexSlice<CallArg, [ExpressionId]>,
        span: Span,
    ) -> Option<FunctionLoweringReturn> {
        let (noise_type, args, name) = match (self, args.as_raw_slice()) {
            (Self::Flicker, [pwr, exp, name @ ..]) => {
                let name = name
                    .get(0)
                    .and_then(|name| ctx.fold.eval_string_constant(*name));
                (
                    functions::Noise::Flicker,
                    index_vec![ctx.fold_real(*pwr)?, ctx.fold_real(*exp)?],
                    name,
                )
            }
            (Self::White, [pwr, name @ ..]) => {
                let name = name
                    .get(0)
                    .and_then(|name| ctx.fold.eval_string_constant(*name));
                (
                    functions::Noise::White,
                    index_vec![ctx.fold_real(*pwr)?],
                    name,
                )
            }
            (Self::Table | Self::TableLog, [input_expr, name @ ..]) => {
                let name = name
                    .get(0)
                    .and_then(|name| ctx.fold.eval_string_constant(*name));
                let input = ctx.lower_expr(*input_expr)?;
                let input_span = ctx.fold.hir[*input_expr].span;
                let is_file = input.ty == Type::STRING;
                if !is_file {
                    input.ty.with_info(|type_info| {
                        if type_info.dimensions.len() != 1 || type_info.element != SimpleType::Real
                        {
                            ctx.fold.errors.add(ExpectedStringOrArray {
                                span: input_span,
                                found: TyPrinter(input.ty),
                            })
                        }
                    });
                }
                let noise_type = match (*self == Self::TableLog, is_file) {
                    (true, true) => functions::Noise::FileTableLog,
                    (true, false) => functions::Noise::TableLog,
                    (false, true) => functions::Noise::FileTable,
                    (false, false) => functions::Noise::Table,
                };

                (
                    noise_type,
                    index_vec![ctx.rvalue_to_operand(input, input_span)],
                    name,
                )
            }
            _ => unreachable!(),
        };

        FunctionLoweringReturn::Val(TyRValue {
            val: RValue::Call(NoiseCall { noise_type, name }.into(), args, span),
            ty: Type::REAL,
        })
        .into()
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Copy)]
pub enum ParameterizationRead {
    Temperature,
    Vt,
    Simparam,
    SimparamStr,
    PortConnected,
    ParameterGiven,
}

impl ParameterizationRead {
    #[inline]
    fn symbol(&self) -> Symbol {
        match self {
            Self::Temperature => sysfun::temperature,
            Self::Vt => sysfun::vt,
            Self::Simparam => sysfun::simparam,
            Self::SimparamStr => sysfun::simparam_str,
            Self::PortConnected => sysfun::port_connected,
            Self::ParameterGiven => sysfun::param_given,
        }
    }

    fn expected_reference(&self, span: Span, refkind: &'static str) -> Error {
        FunctionExpectedReference {
            span,
            name: self.symbol(),
            refkind,
        }
    }
}

impl HirFunction for ParameterizationRead {
    fn min_arg_cnt(&self, _hir: &Hir) -> u8 {
        match self {
            Self::Temperature | Self::Vt => 0,

            Self::Simparam | Self::PortConnected | Self::ParameterGiven => 1,
            Self::SimparamStr => 2,
        }
    }

    fn max_arg_cnt(&self, _hir: &Hir) -> Option<u8> {
        let res = match self {
            Self::Temperature => 0,
            Self::Vt => 1,
            Self::PortConnected | Self::ParameterGiven => 1,
            Self::Simparam | Self::SimparamStr => 2,
        };

        Some(res)
    }

    #[inline]
    fn symbol(&self, _hir: &Hir) -> Symbol {
        self.symbol()
    }

    fn has_side_effects(&self, _hir: &Hir) -> bool {
        false
    }

    fn returns_value(&self, _hir: &Hir) -> bool {
        true
    }

    fn lower<L: HirLowering>(
        &self,
        ctx: &mut LocalCtx<L>,
        args: &IndexSlice<CallArg, [ExpressionId]>,
        span: Span,
    ) -> Option<FunctionLoweringReturn> {
        let val: DefaultInputs = match self {
            Self::Vt => {
                let temp = match args.get(0) {
                    Some(temp) => ctx.fold_real(*temp)?,
                    None => Operand::new(OperandData::Read(Temperature.into()), span),
                };

                return FunctionLoweringReturn::Val(TyRValue {
                    val: RValue::BinaryOperation(
                        Spanned::new(BinOp::Multiply, span),
                        Operand::new(
                            OperandData::Constant(ConstVal::Scalar(Real(
                                Constants::kb(span) / Constants::q(span),
                            ))),
                            span,
                        ),
                        temp,
                    ),
                    ty: Type::REAL,
                })
                .into();
            }

            Self::Temperature => Temperature.into(),

            Self::Simparam => {
                let name = ctx.fold.eval_string_constant(args[0])?;
                match args.get(1) {
                    Some(&default) => {
                        let val = ctx.lower_optional_simparam(name, default, span)?;
                        return FunctionLoweringReturn::Val(TyRValue {
                            val,
                            ty: Type::REAL,
                        })
                        .into();
                    }
                    None => SimParam {
                        name,
                        kind: SimParamKind::Real,
                    }
                    .into(),
                }
            }
            Self::SimparamStr => {
                let name = ctx.fold.eval_string_constant(args[0])?;
                SimParam {
                    name,
                    kind: SimParamKind::String,
                }
                .into()
            }

            Self::PortConnected => {
                if let Expression::PortReference(port) = ctx.fold.hir[args[0]].contents {
                    PortConnected(port).into()
                } else {
                    ctx.fold.errors.add(self.expected_reference(span, "port"));
                    return None;
                }
            }

            Self::ParameterGiven => {
                if let Expression::ParameterReference(param) = ctx.fold.hir[args[0]].contents {
                    ParameterInput::Given(param).into()
                } else {
                    ctx.fold
                        .errors
                        .add(self.expected_reference(span, "parameter"));
                    return None;
                }
            }
        };
        let ty = val.ty(&ctx.fold.mir);
        FunctionLoweringReturn::Val(TyRValue {
            val: RValue::Use(Spanned {
                contents: OperandData::Read(val),
                span,
            }),
            ty,
        })
        .into()
    }

    fn allowed_arg_operation(&self, arg: CallArg) -> Option<AllowedOperations> {
        match self {
            Self::Simparam | Self::SimparamStr if arg.raw() == 0 => {
                Some(AllowedOperations::empty())
            }
            Self::PortConnected => Some(enum_set!(AllowedOperation::PortReferences)),
            Self::ParameterGiven => Some(enum_set!(AllowedOperation::ParameterReferences)),
            _ => None,
        }
    }
}

impl Display for ParameterizationRead {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.symbol())
    }
}
