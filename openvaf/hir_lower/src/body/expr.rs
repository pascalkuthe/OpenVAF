use std::mem::replace;

use hir::signatures::{
    ABSDELAY_MAX, ABS_INT, ABS_REAL, BOOL_EQ, DDX_POT, IDTMOD_IC, IDTMOD_IC_MODULUS,
    IDTMOD_IC_MODULUS_OFFSET, IDTMOD_IC_MODULUS_OFFSET_NATURE, IDTMOD_IC_MODULUS_OFFSET_TOL,
    IDTMOD_NO_IC, IDT_IC, IDT_IC_ASSERT, IDT_IC_ASSERT_NATURE, IDT_IC_ASSERT_TOL, IDT_NO_IC,
    INT_EQ, INT_OP, LIMIT_BUILTIN_FUNCTION, MAX_INT, MAX_REAL, NATURE_ACCESS_BRANCH,
    NATURE_ACCESS_NODES, NATURE_ACCESS_NODE_GND, NATURE_ACCESS_PORT_FLOW, REAL_EQ, REAL_OP,
    SIMPARAM_DEFAULT, SIMPARAM_NO_DEFAULT, STR_EQ,
};
use hir::{BuiltIn, Expr, ExprId, Literal, ParamSysFun, Ref, ResolvedFun, Type};
use mir::builder::InstBuilder;
use mir::{Opcode, Value, FALSE, F_THREE, F_ZERO, GRAVESTONE, TRUE, ZERO};
use mir_build::RetBuilder;
use stdx::iter::zip;
use syntax::ast::{BinaryOp, UnaryOp};

use crate::body::LoweringCtx;
use crate::{
    CallBackKind, CurrentKind, DisplayKind, FmtArg, FmtArgKind, IdtKind, ImplicitEquationKind,
    ParamKind, PlaceKind, REACTIVE_DIM,
};

impl LoweringCtx<'_, '_> {
    pub(super) fn lower_expr_as_contrib(&mut self, expr: ExprId) -> Value {
        let old = replace(&mut self.contribute_rhs, true);
        let val = self.lower_expr_(expr);
        self.contribute_rhs = old;
        val
    }

    pub(super) fn lower_expr_maybe_linear(&mut self, expr: ExprId, is_linear: bool) -> Value {
        let new = self.contribute_rhs & is_linear;
        let old = replace(&mut self.contribute_rhs, new);
        let val = self.lower_expr_(expr);
        self.contribute_rhs = old;
        val
    }

    pub fn lower_expr(&mut self, expr: ExprId) -> Value {
        let old = replace(&mut self.contribute_rhs, false);
        let val = self.lower_expr_(expr);
        self.contribute_rhs = old;
        val
    }

    pub(super) fn lower_expr_(&mut self, expr: ExprId) -> Value {
        let old_loc = self.func.get_srcloc();
        self.func.set_srcloc(mir::SourceLoc::new(u32::from(expr) as i32 + 1));

        let mut res = match self.body.get_expr(expr) {
            Expr::Read(Ref::Variable(var)) => {
                let place = self.place(PlaceKind::Var(var));
                let mut val = self.func.use_var(place);
                if self.tagged_vars.contains(&var) {
                    val = self.func.ins().optbarrier(val);
                    self.data.tagged_reads.insert(val, var);
                }
                val
            }
            Expr::Read(Ref::ParamSysFun(param)) => self.param(ParamKind::ParamSysFun(param)),
            Expr::Read(Ref::Parameter(param)) => self.param(ParamKind::Param(param)),
            Expr::Read(Ref::FunctionReturn(fun)) => {
                let place = self.place(PlaceKind::FunctionReturn(fun));
                self.func.use_var(place)
            }
            Expr::Read(Ref::FunctionArg(fun)) => {
                let place = self.place(PlaceKind::FunctionArg(fun));
                self.func.use_var(place)
            }
            Expr::Read(Ref::NatureAttr(attr)) => {
                self.data.lower_expr_body(self.db, attr.value(self.db), 0, self.func)
            }
            Expr::BinaryOp { lhs, rhs, op } => self.lower_bin_op(expr, lhs, rhs, op),
            Expr::UnaryOp { expr: arg, op } => self.lower_unary_op(expr, arg, op),
            Expr::Select { cond, then_val, else_val } => {
                let cond = self.lower_expr(cond);
                let (mut then_src, mut else_src) = self.func.make_cond(cond, |func, then| {
                    let mut ctx = LoweringCtx {
                        db: self.db,
                        data: self.data,
                        func,
                        body: self.body.clone(),
                        tagged_vars: self.tagged_vars,
                        places: self.places,
                        extra_dims: self.extra_dims.as_deref_mut(),
                        path: self.path,
                        contribute_rhs: self.contribute_rhs,
                        inside_lim: false,
                    };
                    let expr = if then { then_val } else { else_val };
                    ctx.lower_expr(expr)
                });

                if self.contribute_rhs {
                    if let Some(extra_dims) = self.extra_dims.as_deref_mut() {
                        for vals in extra_dims {
                            let then_val = vals.get(&then_val).copied();
                            let else_val = vals.get(&else_val).copied();
                            if then_val.is_none() && else_val.is_none() {
                                continue;
                            }
                            then_src.1 = then_val.unwrap_or(F_ZERO);
                            else_src.1 = else_val.unwrap_or(F_ZERO);
                            self.func.ins().phi(&[then_src, else_src]);
                        }
                    }
                }
                self.func.ins().phi(&[then_src, else_src])
            }
            Expr::Call { args, fun } => match fun {
                ResolvedFun::User { func, limit } => self.lower_user_fun(func, limit, args),
                ResolvedFun::BuiltIn(builtin) => self.lower_builtin(expr, builtin, args),
            },
            Expr::Array(vals) => self.lower_array(expr, vals),
            Expr::Literal(lit) => match *lit {
                Literal::String(ref str) => self.func.sconst(str),
                Literal::Int(val) => self.func.iconst(val),
                Literal::Float(val) => self.func.fconst(val.into()),
                Literal::Inf => {
                    self.func.set_srcloc(old_loc);
                    match self.body.expr_type(expr) {
                        Type::Real => return self.func.fconst(f64::INFINITY),
                        Type::Integer => return self.func.iconst(i32::MAX),
                        _ => unreachable!(),
                    }
                }
            },
        };

        if let Some((src, dst)) = self.body.needs_cast(expr) {
            res = self.insert_cast(res, &src, dst)
        };
        self.func.set_srcloc(old_loc);
        res
    }

    fn lower_unary_op(&mut self, expr: ExprId, arg: ExprId, op: UnaryOp) -> Value {
        let is_inf = self.body.as_literal(arg) == Some(&Literal::Inf);
        let is_linear =
            (op == UnaryOp::Neg && !is_inf && self.body.get_call_signature(expr) == REAL_OP)
                || op == UnaryOp::Identity;
        let arg_ = self.lower_expr_maybe_linear(arg, is_linear);
        match op {
            UnaryOp::BitNegate => self.func.ins().ineg(arg_),
            UnaryOp::Not => self.func.ins().bnot(arg_),
            UnaryOp::Neg => {
                // Special case INFINITY
                if is_inf {
                    match self.body.expr_type(arg) {
                        Type::Real => return self.func.fconst(f64::NEG_INFINITY),
                        Type::Integer => return self.func.iconst(i32::MIN),
                        ty => unreachable!("{ty:?}"),
                    }
                }
                match self.body.get_call_signature(expr) {
                    REAL_OP => {
                        if self.contribute_rhs {
                            if let Some(extra_dims) = self.extra_dims.as_deref_mut() {
                                for vals in extra_dims {
                                    if let Some(val) = vals.get(&arg) {
                                        let val = self.func.ins().fneg(*val);
                                        vals.insert(expr, val);
                                    }
                                }
                            }
                        }
                        self.func.ins().fneg(arg_)
                    }
                    INT_OP => self.func.ins().ineg(arg_),
                    _ => unreachable!(),
                }
            }
            UnaryOp::Identity => {
                if self.contribute_rhs {
                    if let Some(extra_dims) = self.extra_dims.as_deref_mut() {
                        for vals in extra_dims {
                            if let Some(val) = vals.get(&arg).copied() {
                                vals.insert(expr, val);
                            }
                        }
                    }
                }
                arg_
            }
        }
    }

    fn lower_array(&mut self, _expr: ExprId, _args: &[ExprId]) -> Value {
        todo!("arrays")
    }
    fn lower_bin_op(&mut self, expr: ExprId, lhs: ExprId, rhs: ExprId, op: BinaryOp) -> Value {
        let signature = self.body.get_call_signature(expr);
        let op = match op {
            BinaryOp::BooleanOr => {
                // lhs || rhs if lhs { true } else { rhs }
                return self.lower_select(lhs, |_| TRUE, |s| s.lower_expr(rhs));
            }

            BinaryOp::BooleanAnd => {
                // lhs && rhs if lhs { rhs } else { false }
                return self.lower_select(lhs, |s| s.lower_expr(rhs), |_| FALSE);
            }

            BinaryOp::EqualityTest => match_signature! {
                signature:
                    BOOL_EQ => Opcode::Beq,
                    INT_EQ  => Opcode::Ieq,
                    REAL_EQ => Opcode::Feq,
                    STR_EQ  => Opcode::Seq
            },
            BinaryOp::NegatedEqualityTest => match_signature! {
                signature:
                    BOOL_EQ => Opcode::Bne,
                    INT_EQ  => Opcode::Ine,
                    REAL_EQ => Opcode::Fne,
                    STR_EQ  => Opcode::Sne
            },
            BinaryOp::GreaterEqualTest => {
                match_signature!(signature: INT_OP => Opcode::Ige, REAL_OP => Opcode::Fge)
            }
            BinaryOp::GreaterTest => {
                match_signature!(signature: INT_OP => Opcode::Igt, REAL_OP => Opcode::Fgt)
            }
            BinaryOp::LesserEqualTest => {
                match_signature!(signature: INT_OP => Opcode::Ile, REAL_OP => Opcode::Fle)
            }
            BinaryOp::LesserTest => {
                match_signature!(signature: INT_OP => Opcode::Ilt, REAL_OP => Opcode::Flt)
            }
            BinaryOp::Addition => {
                match_signature!(signature: INT_OP => Opcode::Iadd, REAL_OP => Opcode::Fadd)
            }
            BinaryOp::Subtraction => {
                match_signature!(signature: INT_OP => Opcode::Isub, REAL_OP => Opcode::Fsub)
            }
            BinaryOp::Multiplication => {
                match_signature!(signature: INT_OP => Opcode::Imul, REAL_OP => Opcode::Fmul)
            }
            BinaryOp::Division => {
                match_signature!(signature: INT_OP => Opcode::Idiv, REAL_OP => Opcode::Fdiv)
            }
            BinaryOp::Remainder => {
                match_signature!(signature: INT_OP => Opcode::Irem, REAL_OP => Opcode::Frem)
            }
            BinaryOp::Power => Opcode::Pow,

            BinaryOp::LeftShift => Opcode::Ishl,
            BinaryOp::RightShift => Opcode::Ishr,

            BinaryOp::BitwiseXor => Opcode::Ixor,
            BinaryOp::BitwiseEq => {
                let lhs = self.lower_expr(lhs);
                let rhs = self.lower_expr(rhs);
                let res = self.func.ins().ixor(lhs, rhs);
                return self.func.ins().inot(res);
            }
            BinaryOp::BitwiseOr => Opcode::Ior,
            BinaryOp::BitwiseAnd => Opcode::Iand,
        };

        if self.contribute_rhs {
            match op {
                Opcode::Fadd => {
                    let lhs_ = self.lower_expr_(lhs);
                    let rhs_ = self.lower_expr_(rhs);
                    let res = self.func.ins().fadd(lhs_, rhs_);
                    for dim in self.extra_dims.as_deref_mut().unwrap() {
                        let lhs = dim.get(&lhs).copied();
                        let rhs = dim.get(&rhs).copied();
                        match (lhs, rhs) {
                            (None, None) => (),
                            (Some(val), None) | (None, Some(val)) => {
                                dim.insert(expr, val);
                            }

                            (Some(lhs), Some(rhs)) => {
                                let val = self.func.ins().fadd(lhs, rhs);
                                dim.insert(expr, val);
                            }
                        }
                    }

                    return res;
                }
                Opcode::Fsub => {
                    let lhs_ = self.lower_expr_(lhs);
                    let rhs_ = self.lower_expr_(rhs);
                    let res = self.func.ins().fsub(lhs_, rhs_);
                    for dim in self.extra_dims.as_deref_mut().unwrap() {
                        let lhs = dim.get(&lhs).copied();
                        let rhs = dim.get(&rhs).copied();
                        match (lhs, rhs) {
                            (None, None) => (),
                            (Some(val), None) => {
                                dim.insert(expr, val);
                            }

                            (None, Some(val)) => {
                                let val = self.func.ins().fneg(val);
                                dim.insert(expr, val);
                            }

                            (Some(lhs), Some(rhs)) => {
                                let val = self.func.ins().fsub(lhs, rhs);
                                dim.insert(expr, val);
                            }
                        }
                    }

                    return res;
                }
                Opcode::Fmul | Opcode::Fdiv => {
                    let lhs_ = self.lower_expr_(lhs);
                    let has_extra_dims = self
                        .extra_dims
                        .as_deref_mut()
                        .unwrap()
                        .iter()
                        .any(|vals| vals.contains_key(&lhs));
                    if has_extra_dims {
                        let rhs_ = self.lower_expr(rhs);
                        for dim_vals in self.extra_dims.as_deref_mut().unwrap() {
                            if let Some(val) = dim_vals.get(&lhs).copied() {
                                let val = self.func.ins().binary1(op, val, rhs_);
                                dim_vals.insert(expr, val);
                            }
                        }

                        return self.func.ins().binary1(op, lhs_, rhs_);
                    } else if op == Opcode::Fmul {
                        let rhs_ = self.lower_expr_(rhs);
                        for dim_vals in self.extra_dims.as_deref_mut().unwrap() {
                            if let Some(val) = dim_vals.get(&rhs).copied() {
                                let val = self.func.ins().binary1(op, lhs_, val);
                                dim_vals.insert(expr, val);
                            }
                        }
                        return self.func.ins().fmul(lhs_, rhs_);
                    } else {
                        let rhs_ = self.lower_expr(rhs);
                        return self.func.ins().fdiv(lhs_, rhs_);
                    }
                }
                _ => (),
            }
        }

        let lhs_ = self.lower_expr(lhs);
        let rhs_ = self.lower_expr(rhs);
        self.func.ins().binary1(op, lhs_, rhs_)
    }

    fn lower_user_fun(&mut self, fun: hir::Function, lim: bool, args: &[ExprId]) -> Value {
        if lim {
            if self.extra_dims.is_none() {
                return self.lower_expr(args[0]);
            }
            let (new_val, state) = self.limit_state(args[0]);
            let new_val_place = self.place(PlaceKind::FunctionArg(fun.arg(0, self.db)));
            let old_val = self.param(ParamKind::PrevState(state));
            let old_val_place = self.place(PlaceKind::FunctionArg(fun.arg(1, self.db)));

            let enable_lim = self.param(ParamKind::EnableLim);
            let res = self.lower_select_with(
                enable_lim,
                |cx| {
                    cx.func.def_var(new_val_place, new_val);
                    cx.func.def_var(old_val_place, old_val);
                    cx.lower_user_fun_impl(fun, args, true)
                },
                |_| new_val,
            );

            self.insert_limit(state, res)
        } else {
            self.lower_user_fun_impl(fun, args, false)
        }
    }

    fn lower_user_fun_impl(
        &mut self,
        fun: hir::Function,
        args: &[ExprId],
        inside_lim: bool,
    ) -> Value {
        // FIXME proper path for functions
        let mut path = self.path.to_owned();
        path.push_str(&fun.name(self.db));

        let mut args = zip(fun.args(self.db), args);
        // skip the first two arguments
        if inside_lim {
            args.next();
            args.next();
        }
        for (arg, expr) in args.clone() {
            let place = self.place(PlaceKind::FunctionArg(arg));
            let init = if arg.is_input(self.db) {
                self.lower_expr(*expr)
            } else {
                match &arg.ty(self.db) {
                    Type::Real => F_ZERO,
                    Type::Integer => ZERO,
                    ty => unreachable!("invalid function arg type {:?}", ty),
                }
            };

            self.func.def_var(place, init);
        }

        let init = match &fun.return_ty(self.db) {
            Type::Real => F_ZERO,
            Type::Integer => ZERO,
            ty => unreachable!("invalid function return type {:?}", ty),
        };
        let ret_place = self.place(PlaceKind::FunctionReturn(fun));
        self.func.def_var(ret_place, init);
        let body = fun.body(self.db);

        let mut ctx = LoweringCtx {
            db: self.db,
            data: self.data,
            func: self.func,
            body: body.borrow(),
            path: self.path,
            tagged_vars: self.tagged_vars,
            places: self.places,
            extra_dims: None,
            // can not contain contribute so doesn't matter
            contribute_rhs: false,
            inside_lim,
        };

        ctx.lower_entry_stmts();

        // write outputs back to original (including possibly required cast)
        for (arg, &expr) in args {
            if arg.is_output(self.db) {
                let src_place = self.place(PlaceKind::FunctionArg(arg));
                let mut val = self.func.use_var(src_place);
                // casting in reverse here since we write back
                if let Some((dst, src)) = self.body.needs_cast(expr) {
                    val = self.insert_cast(val, src, &dst)
                }
                let dst = self.body.get_expr(expr).as_assignment_lhs();
                let dst_place = self.place(dst.into());
                self.func.def_var(dst_place, val);
            }
        }

        self.func.use_var(ret_place)
    }

    fn lower_builtin(&mut self, expr: ExprId, builtin: BuiltIn, args: &[ExprId]) -> Value {
        let signature = self.body.get_call_signature(expr);
        match builtin {
            BuiltIn::abs => {
                let (negate, comparison, zero) = match_signature!(signature:
                    ABS_REAL => (Opcode::Fneg, Opcode::Flt,  F_ZERO),
                    ABS_INT => (Opcode::Ineg, Opcode::Ilt, ZERO)
                );
                let val = self.lower_expr(args[0]);
                let (inst, dfg) = self.func.ins().binary(comparison, val, zero);
                let cond = dfg.first_result(inst);

                self.lower_select_with(
                    cond,
                    |sel| {
                        let (inst, dfg) = sel.func.ins().unary(negate, val);
                        dfg.first_result(inst)
                    },
                    |_| val,
                )
            }
            BuiltIn::acos => {
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().acos(arg0)
            }
            BuiltIn::acosh => {
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().acosh(arg0)
            }
            BuiltIn::asin => {
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().asin(arg0)
            }
            BuiltIn::asinh => {
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().asinh(arg0)
            }
            BuiltIn::atan => {
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().atan(arg0)
            }
            BuiltIn::atan2 => {
                let arg0 = self.lower_expr(args[0]);
                let arg1 = self.lower_expr(args[1]);
                self.func.ins().atan2(arg0, arg1)
            }
            BuiltIn::atanh => {
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().atanh(arg0)
            }
            BuiltIn::cos => {
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().cos(arg0)
            }
            BuiltIn::cosh => {
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().cosh(arg0)
            }
            // TODO implement limexp properly
            BuiltIn::exp => {
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().exp(arg0)
            }

            BuiltIn::limexp => {
                let arg0 = self.lower_expr(args[0]);
                // let (state, store) = self.stateful_callback(CallBackKind::StoreState);
                let cut_off = self.func.fconst(1e30f64.ln());
                let off = self.func.fconst(1e30f64);

                // let change = self.func.ins().fsub(arg0, state);
                let linearize = self.func.ins().fgt(arg0, cut_off);
                self.func.make_select(linearize, |func, linearize| {
                    if linearize {
                        let delta = func.ins().fsub(arg0, cut_off);
                        let lin = func.ins().fmul(off, delta);
                        func.ins().fadd(off, lin)
                    } else {
                        func.ins().exp(arg0)
                    }
                })
            }
            BuiltIn::floor => {
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().floor(arg0)
            }
            BuiltIn::hypot => {
                let arg0 = self.lower_expr(args[0]);
                let arg1 = self.lower_expr(args[1]);
                self.func.ins().hypot(arg0, arg1)
            }
            BuiltIn::ln => {
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().ln(arg0)
            }
            BuiltIn::sin => {
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().sin(arg0)
            }
            BuiltIn::sinh => {
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().sinh(arg0)
            }
            BuiltIn::sqrt => {
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().sqrt(arg0)
            }
            BuiltIn::tan => {
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().tan(arg0)
            }
            BuiltIn::tanh => {
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().tanh(arg0)
            }
            BuiltIn::clog2 => {
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().clog2(arg0)
            }
            BuiltIn::log10 | BuiltIn::log => {
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().log(arg0)
            }
            BuiltIn::ceil => {
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().ceil(arg0)
            }

            BuiltIn::max => {
                let comparison = match_signature!(signature: MAX_REAL => InstBuilder::fgt, MAX_INT => InstBuilder::igt);
                let arg0 = self.lower_expr(args[0]);
                let arg1 = self.lower_expr(args[1]);
                let cond = comparison(self.func.ins(), arg0, arg1);
                self.lower_select_with(cond, |_| arg0, |_| arg1)
            }
            BuiltIn::min => {
                let comparison = match_signature!(signature: MAX_REAL => InstBuilder::flt, MAX_INT => InstBuilder::ilt);
                let arg0 = self.lower_expr(args[0]);
                let arg1 = self.lower_expr(args[1]);
                let cond = comparison(self.func.ins(), arg0, arg1);
                self.lower_select_with(cond, |_| arg0, |_| arg1)
            }
            BuiltIn::pow => {
                let arg0 = self.lower_expr(args[0]);
                let arg1 = self.lower_expr(args[1]);
                self.func.ins().pow(arg0, arg1)
            }

            BuiltIn::write => {
                self.ins_display(DisplayKind::Display, false, args);
                GRAVESTONE
            }
            BuiltIn::display | BuiltIn::strobe | BuiltIn::monitor => {
                self.ins_display(DisplayKind::Display, true, args);
                GRAVESTONE
            }
            BuiltIn::debug => {
                self.ins_display(DisplayKind::Debug, true, args);
                GRAVESTONE
            }

            BuiltIn::warning => {
                self.ins_display(DisplayKind::Warn, true, args);
                GRAVESTONE
            }
            BuiltIn::error => {
                self.ins_display(DisplayKind::Error, true, args);
                GRAVESTONE
            }
            BuiltIn::info => {
                self.ins_display(DisplayKind::Info, true, args);
                GRAVESTONE
            }

            BuiltIn::fatal => {
                self.ins_display(DisplayKind::Fatal, true, args);
                self.func.ins().ret();

                let unreachable_bb = self.func.create_block();
                self.func.switch_to_block(unreachable_bb);
                self.func.seal_block(unreachable_bb);
                GRAVESTONE
            }
            BuiltIn::analysis => {
                let cb = self.callback(CallBackKind::Analysis);
                let arg = self.lower_expr(args[0]);
                self.func.ins().call1(cb, &[arg])
            }
            BuiltIn::ac_stim
            | BuiltIn::noise_table
            | BuiltIn::noise_table_log
            | BuiltIn::white_noise
            | BuiltIn::flicker_noise => F_ZERO,

            BuiltIn::abstime => self.param(ParamKind::Abstime),

            BuiltIn::ddt => {
                if self.extra_dims.is_none() {
                    return F_ZERO;
                }
                let arg0 = self.lower_expr(args[0]);
                let equation_kind = if self.contribute_rhs {
                    // optbarrier to allow removing this if this is not a linear contribute
                    let arg0 = self.func.ins().optbarrier(arg0);
                    self.extra_dims.as_mut().unwrap()[REACTIVE_DIM].insert(expr, arg0);
                    ImplicitEquationKind::UnresolvedDdt(arg0)
                } else {
                    ImplicitEquationKind::Ddt
                };

                let (equation, val) = self.implicit_eqation(equation_kind);
                self.define_resist_residual(val, equation);
                let react_residual = self.func.ins().fneg(arg0);
                self.define_react_residual(react_residual, equation);
                val
            }

            BuiltIn::idt | BuiltIn::idtmod if self.extra_dims.is_none() => {
                match signature {
                    IDT_NO_IC => F_ZERO, // fair enough approximation
                    _ => self.lower_expr_(args[1]),
                }
            }

            BuiltIn::idt => {
                let kind = match_signature! {
                    signature:
                        IDT_NO_IC => IdtKind::Basic,
                        IDT_IC => IdtKind::Ic,
                        // we currently do not support tolerance
                        IDT_IC_ASSERT | IDT_IC_ASSERT_TOL | IDT_IC_ASSERT_NATURE => IdtKind::Assert
                };

                self.lower_integral(kind, args)
            }

            BuiltIn::idtmod => {
                let kind = match_signature! {
                    signature:
                        IDTMOD_NO_IC => IdtKind::Basic,
                        IDTMOD_IC => IdtKind::Ic,
                        IDTMOD_IC_MODULUS => IdtKind::Modulus,
                        // we currently do not support tolerance
                        IDTMOD_IC_MODULUS_OFFSET
                        | IDTMOD_IC_MODULUS_OFFSET_TOL
                        | IDTMOD_IC_MODULUS_OFFSET_NATURE => IdtKind::ModulusOffset
                };

                self.lower_integral(kind, args)
            }

            BuiltIn::flow => {
                let res = match_signature! {
                    signature:
                        NATURE_ACCESS_NODES|NATURE_ACCESS_NODE_GND => self.nodes_from_args(
                            args,
                            |hi, lo| ParamKind::Current(CurrentKind::Unnamed{hi,lo})
                        ),
                        NATURE_ACCESS_BRANCH => self.param(ParamKind::Current(
                        CurrentKind::Branch(self.body.into_branch(args[0]))
                    )),
                        NATURE_ACCESS_PORT_FLOW => self.param(ParamKind::Current(
                            CurrentKind::Port(self.body.into_port_flow(args[0]))
                        ))
                };
                let mfactor = self.param(ParamKind::ParamSysFun(ParamSysFun::mfactor));
                return self.func.ins().fdiv(res, mfactor);
            }
            BuiltIn::potential => {
                match_signature! {
                    signature:
                        NATURE_ACCESS_NODES|NATURE_ACCESS_NODE_GND => self.nodes_from_args( args, |hi,lo|ParamKind::Voltage{hi,lo}),
                        NATURE_ACCESS_BRANCH => {
                            let branch = self.body.into_branch(args[0]).kind(self.db);
                            self.nodes(branch.unwrap_hi_node(), branch.lo_node(), |hi, lo| ParamKind::Voltage{ hi, lo })
                        }
                }
            }
            BuiltIn::vt => {
                // TODO make this a database input
                const KB: f64 = 1.3806488e-23;
                const Q: f64 = 1.602176565e-19;

                let fac = self.func.fconst(KB / Q);
                let temp = match args.get(0) {
                    Some(temp) => self.lower_expr(*temp),
                    None => self.param(ParamKind::Temperature),
                };

                self.func.ins().fmul(fac, temp)
            }

            BuiltIn::ddx => {
                let val = self.lower_expr(args[0]);
                let param = self.lower_expr(args[1]);
                let param = self.func.func.dfg.value_def(param).unwrap_param();
                let call = if signature == DDX_POT {
                    // TODO how to handle gnd nodes?
                    let node = self.data.params.get_index(param).unwrap().0.unwrap_pot_node();
                    CallBackKind::NodeDerivative(node)
                } else {
                    CallBackKind::Derivative(param)
                };
                let func_ref = self.callback(call);
                self.func.ins().call1(func_ref, &[val])
            }
            BuiltIn::temperature => self.param(ParamKind::Temperature),
            BuiltIn::simparam => {
                let call = match_signature!(signature: SIMPARAM_NO_DEFAULT => CallBackKind::SimParam,SIMPARAM_DEFAULT => CallBackKind::SimParamOpt);

                let is_opt = call == CallBackKind::SimParamOpt;
                let func_ref = self.callback(call);
                let arg0 = self.lower_expr(args[0]);
                if is_opt {
                    let arg1 = self.lower_expr(args[1]);
                    self.func.ins().call1(func_ref, &[arg0, arg1])
                } else {
                    self.func.ins().call1(func_ref, &[arg0])
                }
            }
            BuiltIn::simparam_str => {
                let func_ref = self.callback(CallBackKind::SimParamStr);
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().call1(func_ref, &[arg0])
            }
            BuiltIn::param_given => {
                self.param(ParamKind::ParamGiven { param: self.body.into_parameter(args[0]) })
            }
            BuiltIn::port_connected => {
                self.param(ParamKind::PortConnected { port: self.body.into_node(args[0]) })
            }
            BuiltIn::bound_step => {
                let cb = self.callback(CallBackKind::BoundStep);
                let step_size = self.lower_expr(args[0]);
                self.func.ins().call(cb, &[step_size]);
                GRAVESTONE
            }

            BuiltIn::limit if signature == LIMIT_BUILTIN_FUNCTION && self.extra_dims.is_some() => {
                let (new_val, state) = self.limit_state(args[0]);

                let prev_val = self.param(ParamKind::PrevState(state));
                let name = self.body.as_literal(args[1]).unwrap().unwrap_str();
                let name = self.func.interner.get_or_intern(name);
                let func_ref =
                    self.callback(CallBackKind::BuiltinLimit { name, num_args: args.len() as u32 });
                let mut call_args = vec![new_val, prev_val];
                call_args.extend(args[2..].iter().map(|arg| self.lower_expr(*arg)));

                let enable_lim = self.param(ParamKind::EnableLim);
                let res = self.func.make_select(enable_lim, |func, lim| {
                    if lim {
                        func.ins().call1(func_ref, &call_args)
                    } else {
                        new_val
                    }
                });

                self.insert_limit(state, res)
            }
            BuiltIn::discontinuity => {
                if self.inside_lim && Some(&Literal::Int(-1)) == self.body.as_literal(args[0]) {
                    self.callback(CallBackKind::LimDiscontinuity);
                } else {
                    // TODO implement support for discontinuity?
                }
                GRAVESTONE
            }
            BuiltIn::finish | BuiltIn::stop => GRAVESTONE,

            BuiltIn::absdelay if self.extra_dims.is_some() => {
                let arg = self.lower_expr_as_contrib(args[0]);
                let mut delay = self.lower_expr(args[1]);
                let (eq1, res) = self.implicit_eqation(ImplicitEquationKind::Absdelay);
                let (eq2, intermediate) = self.implicit_eqation(ImplicitEquationKind::Absdelay);
                if signature == ABSDELAY_MAX {
                    let max_delay = self.lower_expr(args[2]);
                    let use_delay = self.func.ins().fle(delay, max_delay);
                    delay = self.lower_select_with(use_delay, |_| delay, |_| max_delay);
                } else {
                    let func = self.callback(CallBackKind::StoreDelayTime(eq1));
                    delay = self.func.ins().call1(func, &[delay]);
                }

                let mut resist_val = self.func.ins().fsub(res, arg);
                resist_val = self.func.ins().fdiv(resist_val, delay);
                self.define_resist_residual(resist_val, eq1);
                self.define_react_residual(intermediate, eq1);

                let mut resist_val = self.func.ins().fsub(res, intermediate);
                resist_val = self.func.ins().fdiv(resist_val, delay);
                self.define_resist_residual(resist_val, eq2);
                let react_val = self.func.ins().fdiv(res, F_THREE);
                self.define_react_residual(react_val, eq2);

                res
            }

            BuiltIn::slew | BuiltIn::transition | BuiltIn::limit | BuiltIn::absdelay => {
                self.lower_expr(args[0])
            }

            _ => unreachable!(),
        }
    }

    fn lower_integral(&mut self, kind: IdtKind, args: &[ExprId]) -> Value {
        let (equation, val) = self.implicit_eqation(ImplicitEquationKind::Idt(kind));

        let mut enable_integral = self.param(ParamKind::EnableIntegration);
        let residual = if kind.has_ic() {
            if kind.has_assert() {
                enable_integral = self.lower_select_with(
                    enable_integral,
                    |s| {
                        let assert = s.lower_expr_(args[2]);
                        s.func.ins().feq(assert, F_ZERO)
                    },
                    |_| FALSE,
                )
            }

            self.lower_multi_select(enable_integral, |ctx, branch| {
                if branch {
                    if kind.has_modulus() {
                        let modulus = ctx.lower_expr(args[2]);
                        let (min, max) = if kind.has_offset() {
                            let offset = ctx.lower_expr(args[2]);
                            (offset, ctx.func.ins().fadd(offset, modulus))
                        } else {
                            (F_ZERO, modulus)
                        };
                        let too_large = ctx.func.ins().fgt(val, max);
                        ctx.lower_multi_select(too_large, |ctx, too_large| {
                            if too_large {
                                [ctx.func.ins().fsub(val, min), F_ZERO]
                            } else {
                                let too_small = ctx.func.ins().flt(val, min);
                                ctx.lower_multi_select(too_small, |ctx, too_small| {
                                    if too_small {
                                        [ctx.func.ins().fsub(val, min), F_ZERO]
                                    } else {
                                        let arg = ctx.lower_expr(args[0]);
                                        [ctx.func.ins().fneg(arg), val]
                                    }
                                })
                            }
                        })
                    } else {
                        let arg = ctx.lower_expr(args[0]);
                        [ctx.func.ins().fneg(arg), val]
                    }
                } else {
                    let ic = ctx.lower_expr_(args[1]);
                    [ctx.func.ins().fsub(val, ic), F_ZERO]
                }
            })
        } else {
            let arg = self.lower_expr(args[0]);
            [self.func.ins().fneg(arg), val]
        };

        self.define_resist_residual(residual[0], equation);
        self.define_react_residual(residual[1], equation);

        val
    }

    fn ins_display(&mut self, kind: DisplayKind, newline: bool, args: &[ExprId]) {
        let mut fmt_lit = String::new();
        let mut call_args = vec![GRAVESTONE];
        let mut arg_tys = Vec::new();

        let mut i = 0;

        while let Some(&expr) = args.get(i) {
            i += 1;
            if let Some(Literal::String(ref lit)) = self.body.as_literal(expr) {
                fmt_lit.reserve(lit.len());
                let mut chars = lit.chars();
                while let Some(mut c) = chars.next() {
                    if c == '%' {
                        c = chars.next().unwrap();
                        let ty = match c {
                            '%' => {
                                fmt_lit.push_str("%%");
                                continue;
                            }
                            'm' | 'M' => {
                                fmt_lit.push_str(self.path);
                                continue;
                            }
                            'l' | 'L' => {
                                // TODO support properly
                                fmt_lit.push_str("__.__");
                                continue;
                            }
                            'h' => {
                                fmt_lit.push_str("%x");
                                Type::Integer.into()
                            }
                            'H' => {
                                fmt_lit.push_str("%X");
                                Type::Integer.into()
                            }
                            'b' | 'B' => {
                                fmt_lit.push_str("%s");
                                FmtArg { ty: Type::Integer, kind: FmtArgKind::Binary }
                            }
                            'd' | 'D' => {
                                fmt_lit.push_str("%d");
                                Type::Integer.into()
                            }
                            'o' | 'O' => {
                                fmt_lit.push_str("%o");
                                Type::Integer.into()
                            }
                            'c' | 'C' => {
                                fmt_lit.push_str("%c");
                                Type::Integer.into()
                            }
                            's' | 'S' => {
                                fmt_lit.push_str("%s");
                                Type::String.into()
                            }
                            _ => {
                                fmt_lit.push('%');
                                // real fmt specifiers may contain cmplx prefixes
                                // we validatet these
                                while !matches!(c, 'e'..='g'|'E'..='G'|'r'|'R') {
                                    if c == '*' {
                                        arg_tys.push(Type::Integer.into());
                                        call_args.push(self.lower_expr(args[i]));
                                        i += 1;
                                    }
                                    fmt_lit.push(c);
                                    c = chars.next().unwrap()
                                }
                                let kind = if matches!(c, 'r' | 'R') {
                                    fmt_lit.push_str("f%c");
                                    FmtArgKind::EngineerReal
                                } else {
                                    fmt_lit.push(c);
                                    FmtArgKind::Other
                                };

                                FmtArg { ty: Type::Real, kind }
                            }
                        };

                        arg_tys.push(ty);
                        call_args.push(self.lower_expr(args[i]));
                        i += 1;
                    } else {
                        fmt_lit.push(c)
                    }
                }
            } else {
                let ty = self.resolved_ty(expr);
                let has_whitespace = fmt_lit.chars().last().map_or(false, |c| c.is_whitespace());
                if !has_whitespace {
                    fmt_lit.push(' ')
                }
                match ty {
                    Type::Real => fmt_lit.push_str("%g"),
                    Type::Integer => fmt_lit.push_str("%d"),
                    Type::String => fmt_lit.push_str("%s"),
                    Type::Void => {
                        fmt_lit.push(' ');
                        continue;
                    }
                    _ => unreachable!(),
                }

                arg_tys.push(ty.into());
                call_args.push(self.lower_expr(expr));
            }
        }
        if newline {
            fmt_lit.push('\n');
        }

        call_args[0] = self.func.sconst(&fmt_lit);
        let callback = CallBackKind::Print { kind, arg_tys: arg_tys.into_boxed_slice() };
        let func_ref = self.callback(callback);
        self.func.ins().call(func_ref, &call_args);
    }

    fn resolved_ty(&self, expr: ExprId) -> Type {
        self.body
            .needs_cast(expr)
            .map(|(_, dst)| dst.to_owned())
            .unwrap_or_else(|| self.body.expr_type(expr))
    }
}
