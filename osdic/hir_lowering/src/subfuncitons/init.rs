use crate::frontend::GeneralOsdiCall;
use openvaf_data_structures::index_vec::{index_box, IndexBox, IndexSlice, IndexVec};
use openvaf_ir::ids::{ParameterId, PortId};
use openvaf_ir::{ParameterRangeConstraintBound, Spanned, Type};
use openvaf_middle::cfg::builder::{CfgBuilder, CfgEdit};
use openvaf_middle::cfg::{ControlFlowGraph, PhiData, TerminatorKind};
use openvaf_middle::const_fold::DiamondLattice;
use openvaf_middle::{
    BinOp, COperand, COperandData, CallArg, CallType, CallTypeConversion, ComparisonOp, Derivative,
    Expression, InputKind, Local, LocalDeclaration, LocalKind, Mir, OperandData, Parameter,
    ParameterCallType, ParameterConstraint, ParameterExcludeConstraint, ParameterInput,
    ParameterRangeConstraint, RValue, StmntKind, TyRValue,
};
use openvaf_session::sourcemap::Span;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Deref, Range};
use openvaf_hir::SyntaxCtx;

#[derive(PartialEq, Eq, Clone)]
pub enum InitFunctionCallType {
    ParamOutOfBounds(ParameterId),
}

impl CallType for InitFunctionCallType {
    type I = ParameterInput;

    fn const_fold(&self, _: &[DiamondLattice]) -> DiamondLattice {
        match self {
            Self::ParamOutOfBounds(_) => unreachable!(),
        }
    }

    fn derivative<C: CallType>(
        &self,
        _original: Local,
        _mir: &Mir<C>,
        _arg_derivative: impl FnMut(CallArg) -> Derivative<Self::I>,
    ) -> Derivative<Self::I> {
        match self {
            Self::ParamOutOfBounds(_) => unreachable!(),
        }
    }
}

impl Display for InitFunctionCallType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            InitFunctionCallType::ParamOutOfBounds(param) => {
                write!(f, "err_param_out_of_bounds( {:?} )", param)
            }
        }
    }
}

impl Debug for InitFunctionCallType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

pub fn generate_param_init<A: CallType>(
    mir: &Mir<A>,
) -> (
    ControlFlowGraph<InitFunctionCallType>,
    IndexBox<ParameterId, [Local]>,
) {
    let mut cfg = CfgBuilder::new_small();

    // Create locals for the parameters
    cfg.cfg.locals = mir
        .parameters
        .iter()
        .map(|info| LocalDeclaration {
            kind: LocalKind::Temporary,
            ty: info.ty,
        })
        .collect();
    let param_locals: IndexBox<_, _> = mir
        .parameters
        .indices()
        .map(|param| Local::from_raw_unchecked(param.raw()))
        .collect();

    let mut conditions = Vec::with_capacity(8);

    for (param, info) in mir.parameters.iter_enumerated() {
        let Parameter {
            ty,
            ref default,
            ref kind,
            sctx,
            ..
        } = *info;
        let span = mir[sctx].span;

        let prev = cfg.current;

        cfg.enter_new_block();
        let val = RValue::Use(Spanned {
            contents: OperandData::Read(ParameterInput::Value(param)),
            span,
        });
        let read_val = cfg.assign_temporary(TyRValue { val, ty: ty }, sctx);
        let true_block = cfg.current;

        cfg.enter_new_block();

        let false_block_head = cfg.current;

        let default = default.read().clone();
        let default = default.map(&mut ParamInitializationMapper(&param_locals, cfg.cfg.locals.len()));

        let val = cfg.insert_expr::<_, false, false>(sctx, default);
        let default_val = cfg.assign_temporary(TyRValue { val, ty: ty }, sctx);
        let false_block_tail = cfg.current;

        cfg.enter_new_block();

        cfg.terminate_bb(false_block_tail, TerminatorKind::Goto(cfg.current), sctx);
        cfg.terminate_bb(true_block, TerminatorKind::Goto(cfg.current), sctx);

        cfg.terminate_bb(
            prev,
            TerminatorKind::Split {
                condition: RValue::Use(Spanned {
                    span,
                    contents: OperandData::Read(ParameterInput::Given(param)),
                }),
                true_block,
                false_block: false_block_head,
                loop_head: false,
            },
            sctx,
        );

        let val = cfg.assign_phi_static_srces(
            param_locals[param],
            [(true_block, read_val), (false_block_tail, default_val)],
            sctx,
        );


        let mut create_comparison = |cfg: &mut CfgBuilder<ControlFlowGraph<_>>, expr: &Expression<_>, comparison| {
            let expr = expr.clone().map( &mut ParamInitializationMapper(&param_locals, cfg.cfg.locals.len()));
            let expr = cfg.insert_expr::<_, false, false>(sctx, expr);
            let expr = cfg.rvalue_to_operand(TyRValue { val: expr, ty }, span, sctx);
            let val = Spanned {
                span,
                contents: OperandData::Copy(val),
            };

            RValue::Comparison(
                Spanned {
                    contents: comparison,
                    span,
                },
                expr,
                val,
                ty,
            )
        };

        let param_kind = kind.read();
        match param_kind.deref() {
            ParameterConstraint::Ordered { included, excluded } => {
                for included_range in included.iter() {
                    let op = if included_range.start.inclusive {
                        ComparisonOp::GreaterThen // should: start <= val ==> err: start > val
                    } else {
                        ComparisonOp::GreaterEqual // should: start < val ==> err: start >= val
                    };
                    let hi_comparison =
                        create_comparison(&mut cfg, &included_range.start.bound, op);
                    let hi_comparison = cfg.rvalue_to_operand(
                        TyRValue {
                            val: hi_comparison,
                            ty: Type::BOOL,
                        },
                        span,
                        sctx,
                    );

                    let op = if included_range.end.inclusive {
                        ComparisonOp::LessThen // should: end >= val ==> err: end < val
                    } else {
                        ComparisonOp::LessEqual // should: end > val ==> err: end <= val
                    };

                    let lo_comparison =
                        create_comparison(&mut cfg, &included_range.end.bound, op);
                    let lo_comparison = cfg.rvalue_to_operand(
                        TyRValue {
                            val: lo_comparison,
                            ty: Type::BOOL,
                        },
                        span,
                        sctx,
                    );
                    let err_cond = RValue::BinaryOperation(
                        Spanned {
                            contents: BinOp::Or,
                            span,
                        },
                        lo_comparison,
                        hi_comparison,
                    );
                    conditions.push(err_cond);
                }
                for excluded_val in excluded.iter() {
                    match excluded_val {
                        ParameterExcludeConstraint::Value(excluded_val) => {
                            let err_cond =
                                create_comparison(&mut cfg, excluded_val, ComparisonOp::Equal);
                            conditions.push(err_cond)
                        }

                        ParameterExcludeConstraint::Range(excluded_range) => {
                            let op = if excluded_range.start.inclusive {
                                ComparisonOp::LessEqual // err: start <= val
                            } else {
                                ComparisonOp::LessThen // err: start < val
                            };
                            let hi_comparison =
                                create_comparison(&mut cfg, &excluded_range.start.bound, op);
                            let hi_comparison = cfg.rvalue_to_operand(
                                TyRValue {
                                    val: hi_comparison,
                                    ty: Type::BOOL,
                                },
                                span,
                                sctx,
                            );

                            let op = if excluded_range.end.inclusive {
                                ComparisonOp::GreaterEqual // err: end >= val
                            } else {
                                ComparisonOp::GreaterThen // err: end > val
                            };

                            let lo_comparison =
                                create_comparison(&mut cfg, &excluded_range.end.bound, op);
                            let lo_comparison = cfg.rvalue_to_operand(
                                TyRValue {
                                    val: lo_comparison,
                                    ty: Type::BOOL,
                                },
                                span,
                                sctx,
                            );
                            let err_cond = RValue::BinaryOperation(
                                Spanned {
                                    contents: BinOp::And,
                                    span,
                                },
                                lo_comparison,
                                hi_comparison,
                            );
                            conditions.push(err_cond);
                        }
                    }
                }
            }
            ParameterConstraint::UnOrdered {
                ref included,
                excluded,
            } => {
                for included_val in included.iter() {
                    let err_cond =
                        create_comparison(&mut cfg, included_val, ComparisonOp::NotEqual);
                    conditions.push(err_cond)
                }

                for excluded_val in excluded.iter() {
                    let err_cond = create_comparison(&mut cfg, excluded_val, ComparisonOp::Equal);
                    conditions.push(err_cond)
                }
            }
        }

        if let Some(mut res) = conditions.pop() {
            for cond in conditions.drain(..) {
                let cond = cfg.rvalue_to_operand(
                    TyRValue {
                        val: cond,
                        ty: Type::BOOL,
                    },
                    span,
                    sctx,
                );
                let old_res = cfg.rvalue_to_operand(
                    TyRValue {
                        val: res,
                        ty: Type::BOOL,
                    },
                    span,
                    sctx,
                );
                res = RValue::BinaryOperation(
                    Spanned {
                        contents: BinOp::Or,
                        span,
                    },
                    old_res,
                    cond,
                );
            }

            let err_block = cfg.create_block();
            cfg.cfg.blocks[err_block].statements.push((
                StmntKind::Call(
                    InitFunctionCallType::ParamOutOfBounds(param),
                    IndexVec::new(),
                    span,
                ),
                sctx,
            ));

            let old = cfg.current;
            cfg.enter_new_block();
            cfg.terminate_bb(
                old,
                TerminatorKind::Split {
                    condition: res,
                    true_block: err_block,
                    false_block: cfg.current,
                    loop_head: false,
                },
                sctx,
            );

            cfg.terminate_bb(err_block, TerminatorKind::Goto(cfg.current), sctx);
            conditions.clear();
        }
    }
    (cfg.finish(SyntaxCtx::ROOT), param_locals)
}

struct ParamInitializationMapper<'a>(&'a IndexSlice<ParameterId, [Local]>, usize);

impl<'a> CallTypeConversion<ParameterCallType, InitFunctionCallType>
    for ParamInitializationMapper<'a>
{
    fn map_operand(&mut self, op: COperand<ParameterCallType>) -> COperand<InitFunctionCallType> {
        let contents = match op.contents {
            OperandData::Read(input) => self.map_input(input),
            OperandData::Constant(val) => OperandData::Constant(val),
            OperandData::Copy(loc) => OperandData::Copy(loc+self.1),
        };
        Spanned {
            contents,
            span: op.span,
        }
    }

    fn map_input(&mut self, src: ParameterInput) -> COperandData<InitFunctionCallType> {
        match src {
            ParameterInput::Value(param) => OperandData::Copy(self.0[param]),
            src => OperandData::Read(src),
        }
    }

    fn map_call_val(
        &mut self,
        call: ParameterCallType,
        _args: IndexVec<CallArg, COperand<ParameterCallType>>,
        _span: Span,
    ) -> RValue<InitFunctionCallType> {
        match call {}
    }

    fn map_call_stmnt(
        &mut self,
        call: ParameterCallType,
        _args: IndexVec<CallArg, COperand<ParameterCallType>>,
        _span: Span,
    ) -> StmntKind<InitFunctionCallType> {
        match call {}
    }

    fn map_stmnt(&mut self, kind: StmntKind<ParameterCallType>) ->StmntKind<InitFunctionCallType>{
        match kind {
            StmntKind::Assignment(dst, val) => {
                StmntKind::Assignment(dst + self.1, val.map_operands(self))
            }
            StmntKind::Call(call, args, span) => {
                self.map_call_stmnt(call, args, span)
            }
            StmntKind::NoOp => StmntKind::NoOp,
            StmntKind::CollapseHint(hi, lo) => StmntKind::CollapseHint(hi, lo),
        }
    }
}
