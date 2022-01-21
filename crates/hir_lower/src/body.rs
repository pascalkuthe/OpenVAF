use cfg::{
    smallvec, Callback, CfgBuilder, CfgParam, Const, ControlFlowGraph, Local, Op, Operand, Place,
    Terminator,
};
use hir_def::body::{Body, ConstraintKind, ConstraintValue};
use hir_def::expr::CaseCond;
use hir_def::{
    BuiltIn, Case, DefWithBodyId, Expr, ExprId, FunctionId, Literal, LocalFunctionArgId, NodeId,
    ParamSysFun, Stmt, StmtId, Type,
};
use hir_ty::builtin::{
    ABS_INT, ABS_REAL, DDX_POT, IDT_NO_IC, MAX_INT, MAX_REAL, NATURE_ACCESS_BRANCH,
    NATURE_ACCESS_NODES, NATURE_ACCESS_NODE_GND, NATURE_ACCESS_PORT_FLOW, SIMPARAM_DEFAULT,
    SIMPARAM_NO_DEFAULT,
};
use hir_ty::db::HirTyDB;
use hir_ty::inference::{AssignDst, BranchWrite, InferenceResult, ResolvedFun};
use hir_ty::types::{Ty as HirTy, BOOL_EQ, INT_EQ, INT_OP, REAL_EQ, REAL_OP, STR_EQ};
use lasso::Rodeo;
use stdx::iter::zip;

use crate::{CallBackKind, CurrentKind, HirInterner, ParamInfoKind, ParamKind, PlaceKind};
use syntax::ast::{AssignOp, BinaryOp, UnaryOp};
use typed_index_collections::{TiSlice, TiVec};

impl HirInterner {
    pub fn lower_body(
        db: &dyn HirTyDB,
        def: DefWithBodyId,
    ) -> (ControlFlowGraph, HirInterner, Rodeo) {
        let mut cfg = ControlFlowGraph::default();
        let body = db.body(def);
        let infere = db.inference_result(def);

        let mut res = HirInterner::default();
        let mut literals = Rodeo::new();
        let mut ctx = LoweringCtx {
            db,
            cfg: &mut CfgBuilder::new(&mut cfg),
            literals: &mut literals,
            data: &mut res,
            body: &body,
            infere: &infere,
            args: TiSlice::from_ref(&[]),
        };

        ctx.lower_entry_stmts();
        ctx.cfg.terminate(Terminator::Ret);
        cfg.next_place = res.places.len().into();

        (cfg, res, literals)
    }

    pub fn insert_var_init(
        &mut self,
        db: &dyn HirTyDB,
        literals: &mut Rodeo,
        dst: &mut ControlFlowGraph,
    ) {
        let mut cfg = CfgBuilder::new(dst);
        let entry = cfg.current;
        for (place, kind) in self.places.clone().iter_enumerated() {
            let val = match *kind {
                PlaceKind::Var(var) => self.lower_expr_body(db, literals, var.into(), 0, &mut cfg),
                PlaceKind::BranchVoltage(_)
                | PlaceKind::BranchCurrent(_)
                | PlaceKind::ImplicitBranchVoltage { .. }
                | PlaceKind::ImplicitBranchCurrent { .. }
                | PlaceKind::Derivative { .. } => 0f64.into(),
                PlaceKind::Param(_)
                | PlaceKind::FunctionReturn { .. }
                | PlaceKind::ParamMin(_)
                | PlaceKind::ParamMax(_) => continue,
            };
            cfg.build_assign(place.into(), Op::Copy, smallvec![val], 1)
        }
        cfg.prepend_entry(entry)
    }

    pub fn insert_param_init(
        &mut self,
        db: &dyn HirTyDB,
        literals: &mut Rodeo,
        dst: &mut ControlFlowGraph,
    ) {
        let mut cfg = CfgBuilder::new(dst);
        let entry = cfg.current;
        for kind in self.params.raw.clone() {
            if let ParamKind::Param(param) = kind {
                let (given, _) = self.params.ensure(ParamKind::ParamGiven { param });
                let (place, _) = self.places.ensure(PlaceKind::Param(param));

                let body = db.body(param.into());
                let infere = db.inference_result(param.into());
                let info = db.param_exprs(param);
                let data = db.param_data(param).clone();
                let ty = &data.ty;

                cfg.build_cond(given.into(), |cfg, given| {
                    if !given {
                        let def_val = self.lower_expr_body(db, literals, param.into(), 0, cfg);
                        cfg.build_assign(place.into(), Op::Copy, smallvec![def_val], 0);
                    }
                });

                let mut ctx = LoweringCtx {
                    db,
                    data: self,
                    literals,
                    cfg: &mut cfg,
                    body: &body,
                    infere: &infere,
                    args: TiSlice::from_ref(&[]),
                };

                let call = ctx
                    .data
                    .callbacks
                    .ensure(CallBackKind::ParamInfo(ParamInfoKind::Invalid, param))
                    .0;

                let min_inclusive = ctx
                    .data
                    .callbacks
                    .ensure(CallBackKind::ParamInfo(ParamInfoKind::MinInclusive, param))
                    .0;

                let max_inclusive = ctx
                    .data
                    .callbacks
                    .ensure(CallBackKind::ParamInfo(ParamInfoKind::MaxInclusive, param))
                    .0;

                let min_exclusive = ctx
                    .data
                    .callbacks
                    .ensure(CallBackKind::ParamInfo(ParamInfoKind::MinExclusive, param))
                    .0;

                let max_exclusive = ctx
                    .data
                    .callbacks
                    .ensure(CallBackKind::ParamInfo(ParamInfoKind::MaxExclusive, param))
                    .0;
                let exclude_start = ctx.cfg.create_block();

                let mut exclude_pos = exclude_start;
                let mut from_pos = ctx.cfg.current;

                let default_bounds: Option<(Operand, Operand)> = match ty {
                    Type::Real => Some((f64::NEG_INFINITY.into(), f64::INFINITY.into())),
                    Type::Integer => Some((i32::MIN.into(), i32::MAX.into())),
                    _ => None,
                };

                let bounds_info = default_bounds.map(|(default_min, default_max)| {
                    let min = ctx.data.places.ensure(PlaceKind::ParamMin(param)).0;
                    let max = ctx.data.places.ensure(PlaceKind::ParamMax(param)).0;
                    (min, max, default_min, default_max)
                });

                let mut has_bounds = false;

                for bound in info.bounds.iter() {
                    let exclude = match bound.kind {
                        ConstraintKind::Exclude => {
                            ctx.cfg.current = exclude_pos;
                            true
                        }
                        ConstraintKind::From => {
                            ctx.cfg.current = from_pos;
                            false
                        }
                    };

                    match bound.val {
                        ConstraintValue::Value(val) => {
                            let val = ctx.lower_expr(val);
                            let op = match (ty, exclude) {
                                (Type::Real, false) => Op::RealEq,
                                (Type::Integer, false) => Op::IntEq,
                                (Type::String, false) => Op::StringEq,
                                (Type::Real, true) => Op::RealNeq,
                                (Type::Integer, true) => Op::IntNeq,
                                (Type::String, true) => Op::StringNeq,
                                _ => unreachable!(),
                            };

                            let mut err = ctx.cfg.build_val(op, smallvec![val, place.into()], 0);
                            if exclude {
                                err =
                                    ctx.cfg.build_val(Op::BoolBitNegate, smallvec![err.into()], 0);
                            }
                            ctx.cfg.build_cond(err.into(), |cfg, is_err| {
                                if is_err {
                                    cfg.build_stmt(Op::Call(call), smallvec![], 0);
                                }
                            });
                        }
                        ConstraintValue::Range(range) => {
                            let op = match (ty, range.start_inclusive) {
                                (Type::Real, true) => Op::RealLessThen,
                                (Type::Real, false) => Op::RealLessEqual,
                                (Type::Integer, true) => Op::IntLessThen,
                                (Type::Integer, false) => Op::IntLessEqual,
                                _ => unreachable!(),
                            };

                            let (min, max, _, _) = bounds_info.unwrap();

                            let start = ctx.lower_expr(range.start);
                            let mut err1 = ctx.cfg.build_val(op, smallvec![place.into(), start], 0);
                            if exclude {
                                err1 =
                                    ctx.cfg.build_val(Op::BoolBitNegate, smallvec![err1.into()], 0);
                            } else if has_bounds {
                                let is_min = ctx.cfg.build_val(op, smallvec![start, min.into()], 0);
                                ctx.cfg.build_cond(is_min.into(), |cfg, is_min| {
                                    if is_min {
                                        cfg.build_assign(min.into(), Op::Copy, smallvec![start], 0);

                                        let call = if range.start_inclusive {
                                            min_inclusive
                                        } else {
                                            min_exclusive
                                        };

                                        cfg.build_stmt(Op::Call(call), smallvec![], 0);
                                    }
                                });
                            } else {
                                ctx.cfg.build_assign(min.into(), Op::Copy, smallvec![start], 0);

                                let call = if range.start_inclusive {
                                    min_inclusive
                                } else {
                                    min_exclusive
                                };

                                ctx.cfg.build_stmt(Op::Call(call), smallvec![], 0)
                            }

                            ctx.cfg.build_cond(err1.into(), |cfg, is_err| {
                                if is_err {
                                    cfg.build_stmt(Op::Call(call), smallvec![], 0);
                                }
                            });

                            let op = match (ty, range.end_inclusive) {
                                (Type::Real, true) => Op::RealGreaterThen,
                                (Type::Real, false) => Op::RealGreaterEqual,
                                (Type::Integer, true) => Op::IntGreaterThen,
                                (Type::Integer, false) => Op::IntGreaterEqual,
                                _ => unreachable!(),
                            };
                            let end = ctx.lower_expr(range.end);
                            let mut err2 = ctx.cfg.build_val(op, smallvec![place.into(), end], 0);
                            if exclude {
                                err2 =
                                    ctx.cfg.build_val(Op::BoolBitNegate, smallvec![err2.into()], 0);
                            } else if has_bounds {
                                let is_max = ctx.cfg.build_val(op, smallvec![end, max.into()], 0);
                                ctx.cfg.build_cond(is_max.into(), |cfg, is_max| {
                                    if is_max {
                                        cfg.build_assign(max.into(), Op::Copy, smallvec![end], 0);
                                        let call = if range.end_inclusive {
                                            max_inclusive
                                        } else {
                                            max_exclusive
                                        };

                                        cfg.build_stmt(Op::Call(call), smallvec![], 0)
                                    }
                                });
                            } else {
                                ctx.cfg.build_assign(max.into(), Op::Copy, smallvec![end], 0);
                                has_bounds = true;

                                let call =
                                    if range.end_inclusive { max_inclusive } else { max_exclusive };

                                ctx.cfg.build_stmt(Op::Call(call), smallvec![], 0)
                            }

                            ctx.cfg.build_cond(err2.into(), |cfg, is_err| {
                                if is_err {
                                    cfg.build_stmt(Op::Call(call), smallvec![], 0);
                                }
                            });
                        }
                    }

                    match bound.kind {
                        ConstraintKind::Exclude => {
                            exclude_pos = ctx.cfg.current;
                        }
                        ConstraintKind::From => {
                            from_pos = ctx.cfg.current;
                        }
                    };
                }

                cfg.enter_new_block();
                cfg.terminate_bb(from_pos, Terminator::Goto(exclude_start));
                cfg.terminate_bb(exclude_pos, Terminator::Goto(cfg.current));
                if !has_bounds {
                    let (min, max, min_val, max_val) = bounds_info.unwrap();
                    cfg.build_assign(max.into(), Op::Copy, smallvec![max_val], 0);
                    cfg.build_assign(min.into(), Op::Copy, smallvec![min_val], 0);
                    cfg.build_stmt(Op::Call(min_inclusive), smallvec![], 0);
                    cfg.build_stmt(Op::Call(max_inclusive), smallvec![], 0);
                }
            }
        }
        cfg.prepend_entry(entry);
        cfg.cfg.next_place = self.places.len().into();
    }

    /// Lowers a body
    fn lower_expr_body(
        &mut self,
        db: &dyn HirTyDB,
        literals: &mut Rodeo,
        def: DefWithBodyId,
        i: usize,
        cfg: &mut CfgBuilder,
    ) -> Operand {
        let body = db.body(def);
        let infere = db.inference_result(def);

        let mut ctx = LoweringCtx {
            db,
            data: self,
            literals,
            cfg,
            body: &body,
            infere: &infere,
            args: TiSlice::from_ref(&[]),
        };

        let expr = body.stmts[body.entry_stmts[i]].unwrap_expr();
        ctx.lower_expr(expr)
    }
}

macro_rules! match_signature {
    ($signature:ident: $($case:ident $(| $extra_case:ident)* => $res:expr),*) => {
        match *$signature.unwrap(){
            $($case $(|$extra_case)* => $res,)*
            signature => unreachable!("invalid signature {:?}",signature)
        }

    };
}

pub struct LoweringCtx<'a, 'c> {
    pub db: &'a dyn HirTyDB,
    pub data: &'a mut HirInterner,
    pub literals: &'a mut Rodeo,
    pub cfg: &'a mut CfgBuilder<'c>,
    pub body: &'a Body,
    pub infere: &'a InferenceResult,
    pub args: &'a TiSlice<LocalFunctionArgId, Operand>,
}

impl LoweringCtx<'_, '_> {
    pub fn lower_entry_stmts(&mut self) {
        for stmt in &*self.body.entry_stmts {
            self.lower_stmt(*stmt)
        }
    }

    pub fn lower_stmt(&mut self, stmt: StmtId) {
        match self.body.stmts[stmt] {
            Stmt::Empty | Stmt::Missing => (),
            Stmt::Expr(expr) => {
                self.lower_expr(expr);
            }
            Stmt::EventControl { .. } => {
                // TODO handle porperly
            }
            Stmt::Assigment { val, assignment_kind, .. } => {
                let mut val_ = self.lower_expr(val);
                if matches!(self.infere.assigment_destination[&stmt], AssignDst::Flow(_)) {
                    let mfactor = self.param(ParamKind::ParamSysFun(ParamSysFun::mfactor)).into();
                    val_ = self
                        .cfg
                        .build_val(Op::RealMul, smallvec![val_, mfactor], u32::from(val) as i32)
                        .into();
                }

                let mut negate = false;

                let place = match self.infere.assigment_destination[&stmt] {
                    AssignDst::Var(var) => self.place(PlaceKind::Var(var)),
                    AssignDst::FunVar { fun, arg: None } => {
                        self.place(PlaceKind::FunctionReturn { fun })
                    }
                    AssignDst::FunVar { arg: Some(arg), .. } => self.args[arg].unwrap_place(),
                    AssignDst::Flow(BranchWrite::Explicit(branch)) => {
                        self.place(PlaceKind::BranchCurrent(branch))
                    }
                    AssignDst::Potential(BranchWrite::Explicit(branch)) => {
                        self.place(PlaceKind::BranchVoltage(branch))
                    }
                    // TODO verify no contributes between GND nodes
                    AssignDst::Flow(BranchWrite::Implict { hi, lo }) => self
                        .lower_contribute_implict_branch(
                            &mut negate,
                            hi,
                            lo,
                            |hi, lo| PlaceKind::ImplicitBranchCurrent { hi, lo },
                            |hi, lo| ParamKind::Current(CurrentKind::ImplictBranch { hi, lo }),
                        ),
                    AssignDst::Potential(BranchWrite::Implict { hi, lo }) => self
                        .lower_contribute_implict_branch(
                            &mut negate,
                            hi,
                            lo,
                            |hi, lo| PlaceKind::ImplicitBranchVoltage { hi, lo },
                            |hi, lo| ParamKind::Voltage { hi, lo },
                        ),
                };
                if assignment_kind == AssignOp::Contribute {
                    let op = if negate { Op::RealSub } else { Op::RealAdd };
                    self.cfg.build_assign(
                        place.into(),
                        op,
                        smallvec![place.into(), val_],
                        u32::from(val) as i32,
                    );
                } else {
                    self.cfg.build_assign(
                        place.into(),
                        Op::Copy,
                        smallvec![val_],
                        u32::from(val) as i32,
                    );
                }
            }
            Stmt::Block { ref body } => {
                for stmt in body {
                    self.lower_stmt(*stmt)
                }
            }
            Stmt::If { cond, then_branch, else_branch } => {
                let cond = self.lower_expr(cond);
                self.cfg.build_cond(cond, |cfg, branch| {
                    let stmt = if branch { then_branch } else { else_branch };
                    LoweringCtx {
                        db: self.db,
                        data: self.data,
                        literals: self.literals,
                        cfg,
                        body: self.body,
                        infere: self.infere,
                        args: self.args,
                    }
                    .lower_stmt(stmt)
                });
            }
            Stmt::ForLoop { init, cond, incr, body } => {
                self.lower_stmt(init);
                self.lower_loop(cond, |s| {
                    s.lower_stmt(body);
                    s.lower_stmt(incr);
                });
            }
            Stmt::WhileLoop { cond, body } => self.lower_loop(cond, |s| s.lower_stmt(body)),
            Stmt::Case { discr, ref case_arms } => self.lower_case(discr, case_arms),
        }
    }

    fn lower_case(&mut self, discr: ExprId, case_arms: &[Case]) {
        let discr_op = match self.infere.expr_types[discr].to_value().unwrap() {
            Type::Real => Op::RealEq,
            Type::Integer => Op::IntEq,
            Type::Bool => Op::BoolEq,
            Type::String => Op::StringEq,
            Type::Array { .. } => todo!(),
            ty => unreachable!("Invalid type {}", ty),
        };
        let discr = self.lower_expr(discr);

        let end = self.cfg.create_block();

        for Case { cond, body } in case_arms {
            // TODO does default mean that further cases are ignored?
            // standard seems to suggest that no matter where the default case is placed that all
            // other conditions are tested prior
            let vals = match cond {
                CaseCond::Vals(vals) => vals,
                CaseCond::Default => continue,
            };

            // save position of the condition for later
            let cond_head = self.cfg.current;

            // First lower the body
            let body_head = self.cfg.enter_new_block();

            self.lower_stmt(*body);
            // At the end of the body skip to the end of the case
            self.cfg.terminate(Terminator::Goto(end));

            // now do the conditions...
            // Go back to that block
            self.cfg.current = cond_head;

            for val in vals {
                // Lower the condition (val == discriminant)
                let val_ = self.lower_expr(*val);
                let cond =
                    self.cfg.build_val(discr_op, smallvec![val_, discr], u32::from(*val) as i32);

                // Create the next block (either the next condition or another arm)

                // Save the current block (still needs to be terminated)
                let cond_block = self.cfg.current;
                let next_block = self.cfg.enter_new_block();

                // If the condition is true go to the body otherwise try the next block
                self.cfg.terminate_bb(
                    cond_block,
                    Terminator::Split {
                        condition: cond.into(),
                        true_block: body_head,
                        false_block: next_block,
                        loop_head: false,
                    },
                );
            }
        }

        if let Some(default_case) =
            case_arms.iter().find(|arm| matches!(arm.cond, CaseCond::Default))
        {
            self.lower_stmt(default_case.body);
        }

        self.cfg.terminate(Terminator::Goto(end));

        self.cfg.current = end;
    }

    fn lower_loop(&mut self, cond: ExprId, lower_body: impl FnOnce(&mut Self)) {
        let start = self.cfg.current;

        let loop_cond_head = self.cfg.enter_new_block();
        let condition = self.lower_expr(cond);
        let loop_cond_tail = self.cfg.current;

        let loop_body_head = self.cfg.enter_new_block();
        lower_body(self);
        let loop_body_tail = self.cfg.current;

        let loop_end = self.cfg.enter_new_block();

        self.cfg.terminate_bb(
            loop_cond_tail,
            Terminator::Split {
                condition,
                true_block: loop_body_head,
                false_block: loop_end,
                loop_head: true,
            },
        );
        self.cfg.terminate_bb(start, Terminator::Goto(loop_cond_head));
        self.cfg.terminate_bb(loop_body_tail, Terminator::Goto(loop_cond_head));
    }

    fn place(&mut self, kind: PlaceKind) -> Place {
        self.data.places.ensure(kind).0
    }

    fn param(&mut self, kind: ParamKind) -> CfgParam {
        self.data.params.ensure(kind).0
    }

    fn callback(&mut self, kind: CallBackKind) -> Callback {
        self.data.callbacks.ensure(kind).0
    }

    pub fn lower_expr(&mut self, expr: ExprId) -> Operand {
        let res: Operand = match self.body.exprs[expr] {
            hir_def::Expr::Path { port: false, .. } => match self.infere.expr_types[expr] {
                HirTy::Var(_, var) => self.place(PlaceKind::Var(var)).into(),
                HirTy::FuntionVar { fun, arg: None, .. } => {
                    self.place(PlaceKind::FunctionReturn { fun }).into()
                }
                HirTy::FuntionVar { arg: Some(arg), .. } => self.args[arg],
                HirTy::Param(_, param) => self.param(ParamKind::Param(param)).into(),
                HirTy::NatureAttr(_, attr) => {
                    self.data.lower_expr_body(self.db, self.literals, attr.into(), 0, self.cfg)
                }
                _ => unreachable!(),
            },
            hir_def::Expr::BinaryOp { lhs, rhs, op: Some(op) } => {
                self.lower_bin_op(expr, lhs, rhs, op).into()
            }
            hir_def::Expr::UnaryOp { expr: arg, op } => self.lower_unary_op(expr, arg, op),
            hir_def::Expr::Select { cond, then_val, else_val } => self
                .lower_select(expr, cond, |s| s.lower_expr(then_val), |s| s.lower_expr(else_val))
                .into(),
            hir_def::Expr::Call { ref args, .. } => match self.infere.resolved_calls[&expr] {
                ResolvedFun::User(fun) => self.lower_user_fun(expr, fun, args),
                ResolvedFun::BuiltIn(builtin) => self.lower_builtin(expr, builtin, args),
                ResolvedFun::Param(param) => self.param(ParamKind::ParamSysFun(param)).into(), // TODO
            },
            hir_def::Expr::Array(ref vals) => self.lower_array(expr, vals),
            hir_def::Expr::Literal(ref lit) => match *lit {
                hir_def::Literal::String(ref str) => {
                    Const::String(self.literals.get_or_intern(str)).into()
                }
                hir_def::Literal::Int(val) => val.into(),
                hir_def::Literal::Float(val) => f64::from(val).into(),
                hir_def::Literal::Inf => match self.infere.expr_types[expr].to_value().unwrap() {
                    Type::Real => return f64::INFINITY.into(),
                    Type::Integer => return i32::MAX.into(),
                    _ => unreachable!(),
                },
            },
            ref expr => unreachable!(
                "encounted invalid expr {:?}: this should have been caught in the frontend",
                expr
            ),
        };

        if let Some(dst) = self.infere.casts.get(&expr) {
            self.insert_cast(expr, res, dst)
        } else {
            res
        }
    }

    fn insert_cast(&mut self, expr: ExprId, val: Operand, dst: &Type) -> Operand {
        let src = self.infere.expr_types[expr].to_value().unwrap();
        let op = match (dst, src) {
            (Type::Real, Type::Integer) => Op::IntToReal,
            (Type::Integer, Type::Real) => Op::RealToInt,
            (Type::Bool, Type::Real) => Op::RealToBool,
            (Type::Real, Type::Bool) => Op::BoolToReal,
            (Type::Integer, Type::Bool) => Op::BoolToInt,
            (Type::Bool, Type::Integer) => Op::IntToBool,
            (Type::Array { .. }, Type::EmptyArray) | (Type::EmptyArray, Type::Array { .. }) => {
                return val
            }
            // TODO Arrays
            // (dst @ Type::Array { .. }, src @ Type::Array { .. }) => {
            //     match (dst.base_type(), src.base_type()) {
            //         (Type::Real, Type::Integer) => Op::ArrIntToReal,
            //         (Type::Integer, Type::Real) => Op::ArrRealToInt,
            //         (Type::Real, Type::Bool) => Op::ArrBoolToReal,
            //         (Type::Integer, Type::Bool) => Op::ArrBoolToInt,
            //         (Type::Bool, Type::Integer) => Op::ArrIntToBool,
            //         _ => unreachable!(),
            //     }
            // }
            (dst, src) => unreachable!(
                "unkown cast found for {:?} ({:?}): {:?} as {:?} -> {:?}",
                expr, self.body.exprs[expr], self.infere.expr_types[expr], src, dst
            ),
        };
        self.cfg.build_val(op, smallvec![val], u32::from(expr) as i32).into()
    }

    fn lower_unary_op(&mut self, expr: ExprId, arg: ExprId, op: UnaryOp) -> Operand {
        let op = match op {
            UnaryOp::BitNegate => Op::IntBitNegate,
            UnaryOp::Not => Op::BoolBitNegate,
            UnaryOp::Neg => {
                // Special case INFINITY
                if Expr::Literal(Literal::Inf) == self.body.exprs[arg] {
                    match self.infere.expr_types[arg].to_value().unwrap() {
                        Type::Real => return f64::NEG_INFINITY.into(),
                        Type::Integer => return i32::MIN.into(),
                        _ => unreachable!(),
                    }
                }
                match self.infere.resolved_signatures[&expr] {
                    REAL_OP => Op::RealArtihNeg,
                    INT_OP => Op::IntArithNeg,
                    _ => unreachable!(),
                }
            }
            UnaryOp::Identity => Op::Copy,
        };

        let arg = self.lower_expr(arg);
        self.cfg.build_val(op, smallvec![arg], u32::from(expr) as i32).into()
    }

    fn lower_array(&mut self, _expr: ExprId, _args: &[ExprId]) -> Operand {
        todo!("arrays")
    }

    fn node(&self, node: NodeId) -> Option<NodeId> {
        if self.db.node_data(node).is_gnd {
            None
        } else {
            Some(node)
        }
    }

    fn nodes_from_args(
        &mut self,
        expr: ExprId,
        args: &[ExprId],
        kind: impl Fn(NodeId, Option<NodeId>) -> ParamKind,
    ) -> Operand {
        let hi = self.infere.expr_types[args[0]].unwrap_node();
        let lo = args.get(1).map(|arg| self.infere.expr_types[*arg].unwrap_node());
        self.nodes(expr, hi, lo, kind)
    }

    fn nodes(
        &mut self,
        expr: ExprId,
        hi: NodeId,
        lo: Option<NodeId>,
        kind: impl Fn(NodeId, Option<NodeId>) -> ParamKind,
    ) -> Operand {
        let hi = self.node(hi);
        let lo = lo.and_then(|lo| self.node(lo));
        match (hi, lo) {
            (Some(hi), None) => self.param(kind(hi, None)).into(),
            (None, Some(lo)) => {
                let lo = self.param(kind(lo, None));
                self.cfg
                    .build_val(Op::RealArtihNeg, smallvec![lo.into()], u32::from(expr) as i32)
                    .into()
            }
            // TODO refactor to nice if let binding when stable
            (Some(hi), Some(lo)) => {
                if let Some(inverted) = self.data.params.index(&kind(lo, Some(hi))) {
                    self.cfg
                        .build_val(
                            Op::RealArtihNeg,
                            smallvec![inverted.into()],
                            u32::from(expr) as i32,
                        )
                        .into()
                } else {
                    self.param(kind(hi, Some(lo))).into()
                }
            }
            (None, None) => 0.0.into(),
        }
    }

    fn lower_contribute_implict_branch(
        &mut self,
        negate: &mut bool,
        hi: NodeId,
        lo: Option<NodeId>,
        kind: impl Fn(NodeId, Option<NodeId>) -> PlaceKind,
        param_kind: impl Fn(NodeId, Option<NodeId>) -> ParamKind,
    ) -> Place {
        let hi = self.node(hi);
        let lo = lo.and_then(|lo| self.node(lo));
        match (hi, lo) {
            (Some(hi), None) => self.place(kind(hi, None)),
            (None, Some(lo)) => {
                *negate = true;
                self.place(kind(lo, None))
            }
            (Some(hi), Some(lo)) => {
                if self.data.params.contains(&param_kind(lo, Some(hi))) {
                    *negate = true;
                    self.place(kind(lo, Some(hi)))
                } else {
                    self.data.params.ensure(param_kind(hi, Some(lo)));
                    self.place(kind(hi, Some(lo)))
                }
            }
            (None, None) => unreachable!(),
        }
    }

    fn lower_bin_op(&mut self, expr: ExprId, lhs: ExprId, rhs: ExprId, op: BinaryOp) -> Local {
        let signature = self.infere.resolved_signatures.get(&expr);
        let op = match op {
            BinaryOp::BooleanOr => {
                // lhs || rhs if lhs { true } else { rhs }
                return self.lower_select(expr, lhs, |_| true.into(), |s| s.lower_expr(rhs));
            }

            BinaryOp::BooleanAnd => {
                // lhs && rhs if lhs { rhs } else { false }
                return self.lower_select(expr, lhs, |s| s.lower_expr(rhs), |_| false.into());
            }

            BinaryOp::EqualityTest => match_signature! {
                signature:
                    BOOL_EQ => Op::BoolEq,
                    INT_EQ => Op::IntEq,
                    REAL_EQ => Op::RealEq,
                    STR_EQ => Op::StringEq
            },
            BinaryOp::NegatedEqualityTest => match_signature! {
                signature:
                    BOOL_EQ => Op::BoolNeq,
                    INT_EQ => Op::IntNeq,
                    REAL_EQ => Op::RealNeq,
                    STR_EQ => Op::StringNeq
            },
            BinaryOp::GreaterEqualTest => {
                match_signature!(signature: INT_OP => Op::IntGreaterEqual, REAL_OP => Op::RealGreaterEqual)
            }
            BinaryOp::GreaterTest => {
                match_signature!(signature: INT_OP => Op::IntGreaterThen, REAL_OP => Op::RealGreaterThen)
            }
            BinaryOp::LesserEqualTest => {
                match_signature!(signature: INT_OP => Op::IntLessEqual, REAL_OP => Op::RealLessEqual)
            }
            BinaryOp::LesserTest => {
                match_signature!(signature: INT_OP => Op::IntLessThen, REAL_OP => Op::RealLessThen)
            }
            BinaryOp::Addition => {
                match_signature!(signature: INT_OP => Op::IntAdd, REAL_OP => Op::RealAdd)
            }
            BinaryOp::Subtraction => {
                match_signature!(signature: INT_OP => Op::IntSub, REAL_OP => Op::RealSub)
            }
            BinaryOp::Multiplication => {
                match_signature!(signature: INT_OP => Op::IntMul, REAL_OP => Op::RealMul)
            }
            BinaryOp::Division => {
                match_signature!(signature: INT_OP => Op::IntDiv, REAL_OP => Op::RealDiv)
            }
            BinaryOp::Remainder => {
                match_signature!(signature: INT_OP => Op::IntRem, REAL_OP => Op::RealRem)
            }
            BinaryOp::Power => Op::RealPow,

            BinaryOp::LeftShift => Op::IntShl,
            BinaryOp::RightShift => Op::IntShr,

            BinaryOp::BitwiseXor => Op::IntXor,
            BinaryOp::BitwiseEq => Op::IntNXor,
            BinaryOp::BitwiseOr => Op::IntOr,
            BinaryOp::BitwiseAnd => Op::IntAdd,
        };

        let lhs = self.lower_expr(lhs);
        let rhs = self.lower_expr(rhs);
        self.cfg.build_val(op, smallvec![lhs, rhs], u32::from(expr) as i32)
    }

    fn lower_to_local(&mut self, expr: ExprId, f: impl FnOnce(&mut Self) -> Operand) -> Local {
        let val = f(self);
        self.cfg.to_local(val, u32::from(expr) as i32)
    }

    fn lower_select(
        &mut self,
        expr: ExprId,
        cond: ExprId,
        lower_then_val: impl FnMut(&mut LoweringCtx) -> Operand,
        lower_else_val: impl FnMut(&mut LoweringCtx) -> Operand,
    ) -> Local {
        let cond = self.lower_expr(cond);
        self.lower_select_with(expr, cond, lower_then_val, lower_else_val)
    }

    fn lower_select_with(
        &mut self,
        expr: ExprId,
        cond: Operand,
        mut lower_then_val: impl FnMut(&mut LoweringCtx) -> Operand,
        mut lower_else_val: impl FnMut(&mut LoweringCtx) -> Operand,
    ) -> Local {
        self.cfg.build_select(cond, |cfg, branch| {
            let mut ctx = LoweringCtx {
                db: self.db,
                data: self.data,
                literals: self.literals,
                cfg,
                body: self.body,
                infere: self.infere,
                args: self.args,
            };
            if branch {
                ctx.lower_to_local(expr, &mut lower_then_val)
            } else {
                ctx.lower_to_local(expr, &mut lower_else_val)
            }
        })
    }

    fn lower_user_fun(&mut self, expr: ExprId, fun: FunctionId, args: &[ExprId]) -> Operand {
        let info = self.db.function_data(fun);
        let return_var = self.place(PlaceKind::FunctionReturn { fun });

        let args: TiVec<_, _> = args.iter().map(|arg| self.lower_expr(*arg)).collect();
        let body = self.db.body(fun.into());
        let infere = self.db.inference_result(fun.into());

        // TODO do not inline functions!
        for (arg, operand) in zip(&*info.args, &args) {
            if arg.is_output && !arg.is_input {
                let init = match &arg.ty {
                    Type::Real => 0.0.into(),
                    Type::Integer => 0.into(),
                    ty => unreachable!("invalid function arg type {:?}", ty),
                };
                self.cfg.build_assign(
                    (*operand).unwrap_place().into(),
                    Op::Copy,
                    smallvec![init],
                    u32::from(expr) as i32,
                )
            }
        }

        let init = match &info.return_ty {
            Type::Real => 0.0.into(),
            Type::Integer => 0.into(),
            ty => unreachable!("invalid function return type {:?}", ty),
        };
        self.cfg.build_assign(return_var.into(), Op::Copy, smallvec![init], u32::from(expr) as i32);

        let mut ctx = LoweringCtx {
            db: self.db,
            data: self.data,
            literals: self.literals,
            cfg: self.cfg,
            body: &body,
            infere: &infere,
            args: &args,
        };

        ctx.lower_entry_stmts();

        Operand::Place(return_var)
    }

    fn lower_builtin(&mut self, expr: ExprId, builtin: BuiltIn, args: &[ExprId]) -> Operand {
        let src = u32::from(expr) as i32;
        let signature = self.infere.resolved_signatures.get(&expr);
        let op = match builtin {
            BuiltIn::abs => {
                let (negate, comparison, zero) = match_signature!(signature:
                    ABS_REAL => (Op::RealArtihNeg,Op::RealLessThen,0f64.into()),
                    ABS_INT => (Op::IntArithNeg,Op::IntLessThen,0.into())
                );
                let val = self.lower_expr(args[0]);
                let cond = self.cfg.build_val(comparison, smallvec![val, zero], src);
                // can't use build select here without producing an unneccary local/bb that our
                // optimizer currently can not remove
                return self
                    .lower_select_with(
                        expr,
                        cond.into(),
                        |sel| -> Operand { sel.cfg.build_val(negate, smallvec![val], src).into() },
                        |_| val,
                    )
                    .into();
            }
            BuiltIn::acos => Op::ArcCos,
            BuiltIn::acosh => Op::ArcCosH,
            BuiltIn::asin => Op::ArcSin,
            BuiltIn::asinh => Op::ArcSinH,
            BuiltIn::atan => Op::ArcTan,
            BuiltIn::atan2 => Op::ArcTan2,
            BuiltIn::atanh => Op::ArcTanH,
            BuiltIn::cos => Op::Cos,
            BuiltIn::cosh => Op::CosH,
            // TODO implement properly
            BuiltIn::limexp | BuiltIn::exp => Op::Exp,
            BuiltIn::floor => Op::Floor,
            BuiltIn::hypot => Op::Hypot,
            BuiltIn::ln => Op::Ln,

            BuiltIn::sin => Op::Sin,
            BuiltIn::sinh => Op::SinH,
            BuiltIn::sqrt => Op::Sqrt,
            BuiltIn::tan => Op::Tan,
            BuiltIn::tanh => Op::TanH,
            BuiltIn::clog2 => Op::Clog2,
            BuiltIn::log10 | BuiltIn::log => Op::Log,
            BuiltIn::ceil => Op::Ceil,

            BuiltIn::max => {
                let comparison = match_signature!(signature: MAX_REAL => Op::RealGreaterThen, MAX_INT => Op::IntGreaterThen);
                let lhs = self.lower_expr(args[0]);
                let rhs = self.lower_expr(args[1]);
                let cond = self.cfg.build_val(comparison, smallvec![lhs, rhs], src);
                return self.lower_select_with(expr, cond.into(), |_| lhs, |_| rhs).into();
            }
            BuiltIn::min => {
                let comparison = match_signature!(signature: MAX_REAL => Op::RealLessThen, MAX_INT => Op::IntLessThen);
                let lhs = self.lower_expr(args[0]);
                let rhs = self.lower_expr(args[1]);
                let cond = self.cfg.build_val(comparison, smallvec![lhs, rhs], src);
                return self.lower_select_with(expr, cond.into(), |_| lhs, |_| rhs).into();
            }
            BuiltIn::pow => Op::RealPow,

            // TODO implement properly
            BuiltIn::display
            | BuiltIn::strobe
            | BuiltIn::write
            | BuiltIn::monitor
            | BuiltIn::debug
            | BuiltIn::stop
            | BuiltIn::warning
            | BuiltIn::error
            | BuiltIn::info
            | BuiltIn::finish => return Operand::Const(Const::Zst),
            BuiltIn::fatal => {
                //TODO terminate
                self.cfg.terminate(Terminator::Ret);
                // Create dead block. This will be removed by CFG simplify
                self.cfg.enter_new_block();
                return Operand::Const(Const::Zst);
            }
            BuiltIn::analysis => return false.into(),
            BuiltIn::ac_stim
            | BuiltIn::noise_table
            | BuiltIn::noise_table_log
            | BuiltIn::white_noise
            | BuiltIn::flicker_noise
            | BuiltIn::abstime
            | BuiltIn::ddt => return 0.0.into(),

            BuiltIn::idt | BuiltIn::idtmod => {
                let dc_val = match *signature.unwrap() {
                    IDT_NO_IC => 0.0.into(),
                    _ => self.lower_expr(args[1]),
                };
                return dc_val;
            }

            BuiltIn::slew | BuiltIn::transition | BuiltIn::absdelay => {
                return self.lower_expr(args[0])
            }
            BuiltIn::flow => {
                let res = match_signature! {
                    signature:
                        NATURE_ACCESS_NODES|NATURE_ACCESS_NODE_GND => self.nodes_from_args(
                            expr,
                            args,
                            |hi, lo| ParamKind::Current(CurrentKind::ImplictBranch{hi,lo})
                        ),
                        NATURE_ACCESS_BRANCH => self.param(ParamKind::Current(
                            CurrentKind::ExplicitBranch(self.infere.expr_types[args[0]].unwrap_branch())
                        )).into(),
                        NATURE_ACCESS_PORT_FLOW => self.param(ParamKind::Current(
                            CurrentKind::Port(self.infere.expr_types[args[0]].unwrap_port_flow())
                        )).into()
                };
                let mfactor = self.param(ParamKind::ParamSysFun(ParamSysFun::mfactor)).into();
                return self.cfg.build_val(Op::RealDiv, smallvec![res, mfactor], src).into();
            }
            BuiltIn::potential => {
                let res = match_signature! {
                    signature:
                        NATURE_ACCESS_NODES|NATURE_ACCESS_NODE_GND => self.nodes_from_args(expr, args, |hi,lo|ParamKind::Voltage{hi,lo}),
                        NATURE_ACCESS_BRANCH => {
                            let branch = self.infere.expr_types[args[0]].unwrap_branch();
                            let info =self.db.branch_info(branch).unwrap().kind;
                            self.nodes(expr, info.unwrap_hi_node(), info.lo_node(), |hi,lo|ParamKind::Voltage{hi,lo})
                        }
                };
                return res;
            }
            BuiltIn::vt => {
                // TODO make this a database input
                const KB: f64 = 1.3806488e-23;
                const Q: f64 = 1.602176565e-19;
                let temp = match args.get(0) {
                    Some(temp) => self.lower_expr(*temp),
                    None => self.param(ParamKind::Temperature).into(),
                };

                return self
                    .cfg
                    .build_val(Op::RealMul, smallvec![(KB / Q).into(), temp], src)
                    .into();
            }

            BuiltIn::ddx => {
                let val = self.lower_expr(args[0]);
                let cfg_param = self.lower_expr(args[1]).unwrap_cfg_param();
                let call = if *signature.unwrap() == DDX_POT {
                    // TODO how to handle gnd nodes?
                    let node = self.data.params[cfg_param].unwrap_pot_node();
                    CallBackKind::NodeDerivative(node)
                } else {
                    CallBackKind::Derivative(cfg_param)
                };
                let call = self.callback(call);
                let dst = self.cfg.build_val(Op::Call(call), smallvec![val], src);
                // self.data.derivatives.insert(dst, unkown);
                return dst.into();
            }
            BuiltIn::temperature => return self.param(ParamKind::Temperature).into(),
            BuiltIn::simparam => {
                let call = match_signature!(signature: SIMPARAM_NO_DEFAULT => CallBackKind::SimParam,SIMPARAM_DEFAULT => CallBackKind::SimParamOpt);
                Op::Call(self.callback(call))
            }
            BuiltIn::simparam_str => Op::Call(self.callback(CallBackKind::SimParamStr)),
            BuiltIn::param_given => {
                return self
                    .param(ParamKind::ParamGiven {
                        param: self.infere.expr_types[args[0]].unwrap_param(),
                    })
                    .into()
            }
            BuiltIn::port_connected => {
                return self
                    .param(ParamKind::PortConnected {
                        port: self.infere.expr_types[args[0]].unwrap_node(),
                    })
                    .into()
            }
            _ => todo!(),
            // BuiltIn::fclose => todo!(),
            // BuiltIn::fopen => todo!(),
            // BuiltIn::fdisplay => todo!(),
            // BuiltIn::fwrite => todo!(),
            // BuiltIn::fstrobe => todo!(),
            // BuiltIn::fmonitor => todo!(),
            // BuiltIn::fgets => todo!(),
            // BuiltIn::fscanf => todo!(),
            // BuiltIn::swrite => todo!(),
            // BuiltIn::sformat => todo!(),
            // BuiltIn::sscanf => todo!(),
            // BuiltIn::rewind => todo!(),
            // BuiltIn::fseek => todo!(),
            // BuiltIn::ftell => todo!(),
            // BuiltIn::fflush => todo!(),
            // BuiltIn::ferror => todo!(),
            // BuiltIn::feof => todo!(),
            // BuiltIn::fdebug => todo!(),

            // // TODO random
            // BuiltIn::dist_chi_square => todo!(),
            // BuiltIn::dist_exponential => todo!(),
            // BuiltIn::dist_poisson => todo!(),
            // BuiltIn::dist_uniform => todo!(),
            // BuiltIn::dist_erlang => todo!(),
            // BuiltIn::dist_normal => todo!(),
            // BuiltIn::dist_t => todo!(),
            // BuiltIn::random => todo!(),
            // BuiltIn::arandom => todo!(),
            // BuiltIn::rdist_chi_square => todo!(),
            // BuiltIn::rdist_exponential => todo!(),
            // BuiltIn::rdist_poisson => todo!(),
            // BuiltIn::rdist_uniform => todo!(),
            // BuiltIn::rdist_erlang => todo!(),
            // BuiltIn::rdist_normal => todo!(),
            // BuiltIn::rdist_t => todo!(),

            // BuiltIn::simprobe => todo!(),
            // BuiltIn::discontinuity => todo!(),
            // BuiltIn::analog_node_alias => todo!(),
            // BuiltIn::analog_port_alias => todo!(),
            // BuiltIn::test_plusargs => todo!(),
            // BuiltIn::value_plusargs => todo!(),
            // BuiltIn::limit => todo!(),
            // BuiltIn::bound_step => todo!(),

            // // TODO transforms
            // // TODO what is the DC value?
            // BuiltIn::zi_nd => todo!(),
            // BuiltIn::zi_np => todo!(),
            // BuiltIn::zi_zd => todo!(),
            // BuiltIn::zi_zp => todo!(),
            // BuiltIn::laplace_nd => todo!(),
            // BuiltIn::laplace_np => todo!(),
            // BuiltIn::laplace_zd => todo!(),
            // BuiltIn::laplace_zp => todo!(),

            // // TODO is this correct DC behaviour_
            // BuiltIn::last_crossing => return 0f64.into(),
        };

        let operands = args.iter().map(|arg| self.lower_expr(*arg)).collect();
        self.cfg.build_val(op, operands, src).into()
    }
}
