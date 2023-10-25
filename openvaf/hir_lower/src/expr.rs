use hir::builtin::{
    FLICKER_NOISE_NAME, NOISE_TABLE_FILE_NAME, NOISE_TABLE_INLINE_NAME, WHITE_NOISE_NAME,
};
use hir::signatures::{
    ABS_INT, ABS_REAL, BOOL_EQ, DDX_POT, IDTMOD_IC, IDTMOD_IC_MODULUS, IDTMOD_IC_MODULUS_OFFSET,
    IDTMOD_IC_MODULUS_OFFSET_NATURE, IDTMOD_IC_MODULUS_OFFSET_TOL, IDTMOD_NO_IC, IDT_IC,
    IDT_IC_ASSERT, IDT_IC_ASSERT_NATURE, IDT_IC_ASSERT_TOL, IDT_NO_IC, INT_EQ, INT_OP,
    LIMIT_BUILTIN_FUNCTION, MAX_INT, MAX_REAL, NATURE_ACCESS_BRANCH, NATURE_ACCESS_NODES,
    NATURE_ACCESS_NODE_GND, NATURE_ACCESS_PORT_FLOW, REAL_EQ, REAL_OP, SIMPARAM_DEFAULT,
    SIMPARAM_NO_DEFAULT, STR_EQ,
};
use hir::{Body, BuiltIn, Expr, ExprId, Literal, ParamSysFun, Ref, ResolvedFun, Type};
use mir::builder::InstBuilder;
use mir::{Opcode, Value, FALSE, F_ZERO, GRAVESTONE, INFINITY, TRUE, ZERO};
use mir_build::RetBuilder;
use stdx::iter::zip;
use syntax::ast::{BinaryOp, UnaryOp};

use crate::body::BodyLoweringCtx;
use crate::fmt::DisplayKind;
use crate::{
    CallBackKind, CurrentKind, IdtKind, ImplicitEquationKind, NoiseTable, ParamKind, PlaceKind,
};

impl BodyLoweringCtx<'_, '_, '_> {
    pub fn lower_expr(&mut self, expr: ExprId) -> Value {
        let old_loc = self.ctx.get_srcloc();
        self.ctx.set_srcloc(mir::SourceLoc::new(u32::from(expr) as i32 + 1));

        let mut res = match self.body.get_expr(expr) {
            Expr::Read(Ref::Variable(var)) => self.ctx.read_variable(var),
            Expr::Read(Ref::ParamSysFun(param)) => {
                self.ctx.use_param(ParamKind::ParamSysFun(param))
            }
            Expr::Read(Ref::Parameter(param)) => self.ctx.use_param(ParamKind::Param(param)),
            Expr::Read(Ref::FunctionReturn(fun)) => {
                self.ctx.use_place(PlaceKind::FunctionReturn(fun))
            }
            Expr::Read(Ref::FunctionArg(fun)) => self.ctx.use_place(PlaceKind::FunctionArg(fun)),
            Expr::Read(Ref::NatureAttr(attr)) => self.lower_body(attr.value(self.ctx.db), 0),
            Expr::BinaryOp { lhs, rhs, op } => self.lower_bin_op(expr, lhs, rhs, op),
            Expr::UnaryOp { expr: arg, op } => self.lower_unary_op(expr, arg, op),
            Expr::Select { cond, then_val, else_val } => {
                let cond = self.lower_expr(cond);
                let (then_src, else_src) = self.lower_cond_with(cond, |mut ctx, then| {
                    let expr = if then { then_val } else { else_val };
                    ctx.lower_expr(expr)
                });

                self.ctx.ins().phi(&[then_src, else_src])
            }
            Expr::Call { args, fun } => match fun {
                ResolvedFun::User { func, limit } => self.lower_user_fun(func, limit, args),
                ResolvedFun::BuiltIn(builtin) => self.lower_builtin(expr, builtin, args),
            },
            Expr::Array(vals) => self.lower_array(expr, vals),
            Expr::Literal(lit) => match *lit {
                Literal::String(ref str) => self.ctx.sconst(str),
                Literal::Int(val) => self.ctx.iconst(val),
                Literal::Float(val) => self.ctx.fconst(val.into()),
                Literal::Inf => {
                    self.ctx.set_srcloc(old_loc);
                    match self.body.expr_type(expr) {
                        Type::Real => return INFINITY,
                        Type::Integer => return self.ctx.iconst(i32::MAX),
                        _ => unreachable!(),
                    }
                }
            },
        };

        if let Some((src, dst)) = self.body.needs_cast(expr) {
            res = self.ctx.insert_cast(res, &src, dst)
        };
        self.ctx.set_srcloc(old_loc);
        res
    }

    fn lower_unary_op(&mut self, expr: ExprId, arg: ExprId, op: UnaryOp) -> Value {
        let is_inf = self.body.as_literal(arg) == Some(&Literal::Inf);
        let arg_ = self.lower_expr(arg);
        match op {
            UnaryOp::BitNegate => self.ctx.ins().ineg(arg_),
            UnaryOp::Not => self.ctx.ins().bnot(arg_),
            UnaryOp::Neg => {
                // Special case INFINITY
                if is_inf {
                    match self.body.expr_type(arg) {
                        Type::Real => return self.ctx.fconst(f64::NEG_INFINITY),
                        Type::Integer => return self.ctx.iconst(i32::MIN),
                        ty => unreachable!("{ty:?}"),
                    }
                }
                match self.body.get_call_signature(expr) {
                    REAL_OP => self.ctx.ins().fneg(arg_),
                    INT_OP => self.ctx.ins().ineg(arg_),
                    _ => unreachable!(),
                }
            }
            UnaryOp::Identity => arg_,
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
                return self.lower_select(lhs, |_| TRUE, |mut s| s.lower_expr(rhs));
            }

            BinaryOp::BooleanAnd => {
                // lhs && rhs if lhs { rhs } else { false }
                return self.lower_select(lhs, |mut s| s.lower_expr(rhs), |_| FALSE);
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
                let res = self.ctx.ins().ixor(lhs, rhs);
                return self.ctx.ins().inot(res);
            }
            BinaryOp::BitwiseOr => Opcode::Ior,
            BinaryOp::BitwiseAnd => Opcode::Iand,
        };

        let lhs_ = self.lower_expr(lhs);
        let rhs_ = self.lower_expr(rhs);
        self.ctx.ins().binary1(op, lhs_, rhs_)
    }

    fn lower_user_fun(&mut self, fun: hir::Function, lim: bool, args: &[ExprId]) -> Value {
        if lim {
            if self.ctx.no_equations {
                return self.lower_expr(args[0]);
            }
            let new_val = self.lower_expr(args[0]);
            let state = self.ctx.start_limit(new_val);
            let old_val = self.ctx.use_param(ParamKind::PrevState(state));
            let enable_lim = self.ctx.use_param(ParamKind::EnableLim);
            let res = self.lower_select_with(
                enable_lim,
                |mut cx| {
                    cx.ctx.def_place(PlaceKind::FunctionArg(fun.arg(0, self.ctx.db)), new_val);
                    cx.ctx.def_place(PlaceKind::FunctionArg(fun.arg(1, self.ctx.db)), old_val);
                    cx.lower_user_fun_impl(fun, args, true)
                },
                |_| new_val,
            );

            self.ctx.finish_limit(state, res)
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
        path.push_str(&fun.name(self.ctx.db));

        let mut args = zip(fun.args(self.ctx.db), args);
        // skip the first two arguments
        if inside_lim {
            args.next();
            args.next();
        }
        for (arg, expr) in args.clone() {
            let init = if arg.is_input(self.ctx.db) {
                self.lower_expr(*expr)
            } else {
                match &arg.ty(self.ctx.db) {
                    Type::Real => F_ZERO,
                    Type::Integer => ZERO,
                    ty => unreachable!("invalid function arg type {:?}", ty),
                }
            };

            self.ctx.def_place(PlaceKind::FunctionArg(arg), init);
        }

        let init = match &fun.return_ty(self.ctx.db) {
            Type::Real => F_ZERO,
            Type::Integer => ZERO,
            ty => unreachable!("invalid function return type {:?}", ty),
        };
        self.ctx.def_place(PlaceKind::FunctionReturn(fun), init);

        let body = fun.body(self.ctx.db);
        BodyLoweringCtx { body: body.borrow(), path: self.path, ctx: self.ctx }.lower_entry_stmts();

        // write outputs back to original (including possibly required cast)
        for (arg, &expr) in args {
            if arg.is_output(self.ctx.db) {
                let mut val = self.ctx.use_place(PlaceKind::FunctionArg(arg));
                // casting in reverse here since we write back
                if let Some((dst, src)) = self.body.needs_cast(expr) {
                    val = self.ctx.insert_cast(val, src, &dst)
                }
                let dst = self.body.get_expr(expr).as_assignment_lhs();
                self.ctx.def_place(dst.into(), val);
            }
        }

        self.ctx.use_place(PlaceKind::FunctionReturn(fun))
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
                let (inst, dfg) = self.ctx.ins().binary(comparison, val, zero);
                let cond = dfg.first_result(inst);

                self.lower_select_with(
                    cond,
                    |sel| {
                        let (inst, dfg) = sel.ctx.ins().unary(negate, val);
                        dfg.first_result(inst)
                    },
                    |_| val,
                )
            }
            BuiltIn::acos => {
                let arg0 = self.lower_expr(args[0]);
                self.ctx.ins().acos(arg0)
            }
            BuiltIn::acosh => {
                let arg0 = self.lower_expr(args[0]);
                self.ctx.ins().acosh(arg0)
            }
            BuiltIn::asin => {
                let arg0 = self.lower_expr(args[0]);
                self.ctx.ins().asin(arg0)
            }
            BuiltIn::asinh => {
                let arg0 = self.lower_expr(args[0]);
                self.ctx.ins().asinh(arg0)
            }
            BuiltIn::atan => {
                let arg0 = self.lower_expr(args[0]);
                self.ctx.ins().atan(arg0)
            }
            BuiltIn::atan2 => {
                let arg0 = self.lower_expr(args[0]);
                let arg1 = self.lower_expr(args[1]);
                self.ctx.ins().atan2(arg0, arg1)
            }
            BuiltIn::atanh => {
                let arg0 = self.lower_expr(args[0]);
                self.ctx.ins().atanh(arg0)
            }
            BuiltIn::cos => {
                let arg0 = self.lower_expr(args[0]);
                self.ctx.ins().cos(arg0)
            }
            BuiltIn::cosh => {
                let arg0 = self.lower_expr(args[0]);
                self.ctx.ins().cosh(arg0)
            }
            // TODO implement limexp properly
            BuiltIn::exp => {
                let arg0 = self.lower_expr(args[0]);
                self.ctx.ins().exp(arg0)
            }

            BuiltIn::limexp => {
                let arg0 = self.lower_expr(args[0]);
                // let (state, store) = self.stateful_callback(CallBackKind::StoreState);
                let cut_off = self.ctx.fconst(1e30f64.ln());
                let off = self.ctx.fconst(1e30f64);

                // let change = self.ctx.ins().fsub(arg0, state);
                let linearize = self.ctx.ins().fgt(arg0, cut_off);
                self.ctx.make_select(linearize, |func, linearize| {
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
                self.ctx.ins().floor(arg0)
            }
            BuiltIn::hypot => {
                let arg0 = self.lower_expr(args[0]);
                let arg1 = self.lower_expr(args[1]);
                self.ctx.ins().hypot(arg0, arg1)
            }
            BuiltIn::ln => {
                let arg0 = self.lower_expr(args[0]);
                self.ctx.ins().ln(arg0)
            }
            BuiltIn::sin => {
                let arg0 = self.lower_expr(args[0]);
                self.ctx.ins().sin(arg0)
            }
            BuiltIn::sinh => {
                let arg0 = self.lower_expr(args[0]);
                self.ctx.ins().sinh(arg0)
            }
            BuiltIn::sqrt => {
                let arg0 = self.lower_expr(args[0]);
                self.ctx.ins().sqrt(arg0)
            }
            BuiltIn::tan => {
                let arg0 = self.lower_expr(args[0]);
                self.ctx.ins().tan(arg0)
            }
            BuiltIn::tanh => {
                let arg0 = self.lower_expr(args[0]);
                self.ctx.ins().tanh(arg0)
            }
            BuiltIn::clog2 => {
                let arg0 = self.lower_expr(args[0]);
                self.ctx.ins().clog2(arg0)
            }
            BuiltIn::log10 | BuiltIn::log => {
                let arg0 = self.lower_expr(args[0]);
                self.ctx.ins().log(arg0)
            }
            BuiltIn::ceil => {
                let arg0 = self.lower_expr(args[0]);
                self.ctx.ins().ceil(arg0)
            }

            BuiltIn::max => {
                let comparison = match_signature!(signature: MAX_REAL => InstBuilder::fgt, MAX_INT => InstBuilder::igt);
                let arg0 = self.lower_expr(args[0]);
                let arg1 = self.lower_expr(args[1]);
                let cond = comparison(self.ctx.ins(), arg0, arg1);
                self.lower_select_with(cond, |_| arg0, |_| arg1)
            }
            BuiltIn::min => {
                let comparison = match_signature!(signature: MAX_REAL => InstBuilder::flt, MAX_INT => InstBuilder::ilt);
                let arg0 = self.lower_expr(args[0]);
                let arg1 = self.lower_expr(args[1]);
                let cond = comparison(self.ctx.ins(), arg0, arg1);
                self.lower_select_with(cond, |_| arg0, |_| arg1)
            }
            BuiltIn::pow => {
                let arg0 = self.lower_expr(args[0]);
                let arg1 = self.lower_expr(args[1]);
                self.ctx.ins().pow(arg0, arg1)
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
                self.ctx.ins().ret();

                let unreachable_bb = self.ctx.create_block();
                self.ctx.switch_to_block(unreachable_bb);
                self.ctx.seal_block(unreachable_bb);
                GRAVESTONE
            }
            BuiltIn::analysis => {
                let arg = self.lower_expr(args[0]);
                self.ctx.call1(CallBackKind::Analysis, &[arg])
            }

            BuiltIn::noise_table
            | BuiltIn::noise_table_log
            | BuiltIn::white_noise
            | BuiltIn::ac_stim
            | BuiltIn::flicker_noise
                if self.ctx.no_equations =>
            {
                F_ZERO
            }

            BuiltIn::white_noise => {
                // we create a dedicated callback for each noise source
                // by giving every source a unique index. Kind of ineffcient
                // but necessary to avoid accidental correlation/opimization
                // (for example white_noise(x) - white_noise(x) is not zero)
                let idx = self.ctx.num_noise_sources;
                self.ctx.num_noise_sources += 1;
                let name = if signature == WHITE_NOISE_NAME {
                    let name = self.body.as_literal(args[1]).unwrap().unwrap_str();
                    self.ctx.func.interner.get_or_intern(name)
                } else {
                    let name = format!("unnamed{idx}");
                    self.ctx.func.interner.get_or_intern(name)
                };
                let pwr = self.lower_expr(args[0]);
                self.ctx.call1(CallBackKind::WhiteNoise { name, idx }, &[pwr])
            }
            BuiltIn::flicker_noise => {
                // see above
                let idx = self.ctx.num_noise_sources;
                self.ctx.num_noise_sources += 1;
                let name = if signature == FLICKER_NOISE_NAME {
                    let name = self.body.as_literal(args[2]).unwrap().unwrap_str();
                    self.ctx.func.interner.get_or_intern(name)
                } else {
                    let name = format!("unnamed{idx}");
                    self.ctx.func.interner.get_or_intern(name)
                };
                let pwr = self.lower_expr(args[0]);
                let exp = self.lower_expr(args[1]);
                self.ctx.call1(CallBackKind::FlickerNoise { name, idx }, &[pwr, exp])
            }
            BuiltIn::noise_table | BuiltIn::noise_table_log => {
                // see above
                let idx = self.ctx.num_noise_sources;
                self.ctx.num_noise_sources += 1;
                let name = if matches!(signature, NOISE_TABLE_INLINE_NAME | NOISE_TABLE_FILE_NAME) {
                    let name = self.body.as_literal(args[1]).unwrap().unwrap_str();
                    self.ctx.func.interner.get_or_intern(name)
                } else {
                    let name = format!("unnamed{idx}");
                    self.ctx.func.interner.get_or_intern(name)
                };
                let log = builtin == BuiltIn::noise_table_log;
                let noise_table = NoiseTable::new([(0.0, 0.0)], log, name, idx);
                self.ctx.call1(CallBackKind::NoiseTable(Box::new(noise_table)), &[])
            }

            BuiltIn::abstime => self.ctx.use_param(ParamKind::Abstime),

            BuiltIn::ddt => {
                if self.ctx.no_equations {
                    return F_ZERO;
                }
                let arg = self.lower_expr(args[0]);
                self.ctx.call1(CallBackKind::TimeDerivative, &[arg])
            }

            BuiltIn::idt | BuiltIn::idtmod if self.ctx.no_equations => {
                match signature {
                    IDT_NO_IC => F_ZERO, // fair enough approximation
                    _ => self.lower_expr(args[1]),
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
                        NATURE_ACCESS_BRANCH => self.ctx.use_param(ParamKind::Current(
                            CurrentKind::Branch(self.body.into_branch(args[0]))
                        )),
                        NATURE_ACCESS_PORT_FLOW => self.ctx.use_param(ParamKind::Current(
                            CurrentKind::Port(self.body.into_port_flow(args[0]))
                        ))
                };
                let mfactor = self.ctx.use_param(ParamKind::ParamSysFun(ParamSysFun::mfactor));
                return self.ctx.ins().fdiv(res, mfactor);
            }
            BuiltIn::potential => {
                match_signature! {
                    signature:
                        NATURE_ACCESS_NODES|NATURE_ACCESS_NODE_GND => self.nodes_from_args( args, |hi,lo|ParamKind::Voltage{hi,lo}),
                        NATURE_ACCESS_BRANCH => {
                            let branch = self.body.into_branch(args[0]).kind(self.ctx.db);
                            self.ctx.nodes(branch.unwrap_hi_node(), branch.lo_node(), |hi, lo| ParamKind::Voltage{ hi, lo })
                        }
                }
            }
            BuiltIn::vt => {
                // TODO make this a database input
                const KB: f64 = 1.3806488e-23;
                const Q: f64 = 1.602176565e-19;

                let fac = self.ctx.fconst(KB / Q);
                let temp = match args.get(0) {
                    Some(temp) => self.lower_expr(*temp),
                    None => self.ctx.use_param(ParamKind::Temperature),
                };

                self.ctx.ins().fmul(fac, temp)
            }

            BuiltIn::ddx => {
                let val = self.lower_expr(args[0]);
                let unknown = self.lower_expr(args[1]);
                let call = if signature == DDX_POT {
                    // TODO how to handle gnd nodes?
                    let node = self.ctx.unwrap_node(unknown);
                    CallBackKind::NodeDerivative(node)
                } else {
                    CallBackKind::Derivative(self.ctx.dfg().value_def(unknown).unwrap_param())
                };
                self.ctx.call1(call, &[val])
            }
            BuiltIn::temperature => self.ctx.use_param(ParamKind::Temperature),
            BuiltIn::simparam => {
                let arg0 = self.lower_expr(args[0]);
                match_signature! {signature:
                    SIMPARAM_NO_DEFAULT => self.ctx.call1(CallBackKind::SimParam, &[arg0]),
                    SIMPARAM_DEFAULT => {
                        let arg1 = self.lower_expr(args[1]);
                        self.ctx.call1(CallBackKind::SimParamOpt, &[arg0, arg1])
                    }
                }
            }
            BuiltIn::simparam_str => {
                let arg0 = self.lower_expr(args[0]);
                self.ctx.call1(CallBackKind::SimParamStr, &[arg0])
            }
            BuiltIn::param_given => self
                .ctx
                .use_param(ParamKind::ParamGiven { param: self.body.into_parameter(args[0]) }),
            BuiltIn::port_connected => {
                self.ctx.use_param(ParamKind::PortConnected { port: self.body.into_node(args[0]) })
            }
            BuiltIn::bound_step => {
                let step_size = self.lower_expr(args[0]);
                self.ctx.def_place(PlaceKind::BoundStep, step_size);
                GRAVESTONE
            }

            BuiltIn::limit if signature == LIMIT_BUILTIN_FUNCTION && !self.ctx.no_equations => {
                let new_val = self.lower_expr(args[0]);
                let state = self.ctx.start_limit(new_val);
                let prev_val = self.ctx.use_param(ParamKind::PrevState(state));
                let name = self.body.as_literal(args[1]).unwrap().unwrap_str();
                let name = self.ctx.func.interner.get_or_intern(name);
                let mut call_args = vec![new_val, prev_val];
                call_args.extend(args[2..].iter().map(|arg| self.lower_expr(*arg)));

                let enable_lim = self.ctx.use_param(ParamKind::EnableLim);
                let res = self.ctx.make_select(enable_lim, |func, lim| {
                    if lim {
                        func.call1(
                            CallBackKind::BuiltinLimit { name, num_args: args.len() as u32 },
                            &call_args,
                        )
                    } else {
                        new_val
                    }
                });

                self.ctx.finish_limit(state, res)
            }
            BuiltIn::discontinuity => {
                if self.ctx.inside_lim && Some(&Literal::Int(-1)) == self.body.as_literal(args[0]) {
                    self.ctx.call(CallBackKind::LimDiscontinuity, &[]);
                } else {
                    // TODO implement support for discontinuity?
                }
                GRAVESTONE
            }
            BuiltIn::finish | BuiltIn::stop => GRAVESTONE,

            /* TODO: absdealy
            BuiltIn::absdelay => {
                let arg = self.lower_expr(args[0]);
                let mut delay = self.lower_expr(args[1]);
                let (eq1, res) = self.ctx.implicit_eqation(ImplicitEquationKind::Absdelay);
                let (eq2, intermediate) = self.ctx.implicit_eqation(ImplicitEquationKind::Absdelay);
                if signature == ABSDELAY_MAX {
                    let max_delay = self.lower_expr(args[2]);
                    let use_delay = self.ctx.ins().fle(delay, max_delay);
                    delay = self.lower_select_with(use_delay, |_| delay, |_| max_delay);
                } else {
                    delay = self.ctx.call1(CallBackKind::StoreDelayTime(eq1), &[delay]);
                }

                let mut resist_val = self.ctx.ins().fsub(res, arg);
                resist_val = self.ctx.ins().fdiv(resist_val, delay);
                self.ctx.def_resist_residual(resist_val, eq1);
                self.ctx.def_react_residual(intermediate, eq1);

                let mut resist_val = self.ctx.ins().fsub(res, intermediate);
                resist_val = self.ctx.ins().fdiv(resist_val, delay);
                self.ctx.def_resist_residual(resist_val, eq2);
                let react_val = self.ctx.ins().fdiv(res, F_THREE);
                self.ctx.def_react_residual(react_val, eq2);

                res
            }*/
            BuiltIn::slew | BuiltIn::transition | BuiltIn::limit | BuiltIn::absdelay => {
                self.lower_expr(args[0])
            }

            _ => unreachable!(),
        }
    }

    fn lower_integral(&mut self, kind: IdtKind, args: &[ExprId]) -> Value {
        let (equation, val) = self.ctx.implicit_eqation(ImplicitEquationKind::Idt(kind));

        let mut enable_integral = self.ctx.use_param(ParamKind::EnableIntegration);
        let residual = if kind.has_ic() {
            if kind.has_assert() {
                enable_integral = self.lower_select_with(
                    enable_integral,
                    |mut s| {
                        let assert = s.lower_expr(args[2]);
                        s.ctx.ins().feq(assert, F_ZERO)
                    },
                    |_| FALSE,
                )
            }

            self.lower_multi_select(enable_integral, |mut ctx, branch| {
                if branch {
                    if kind.has_modulus() {
                        let modulus = ctx.lower_expr(args[2]);
                        let (min, max) = if kind.has_offset() {
                            let offset = ctx.lower_expr(args[2]);
                            (offset, ctx.ctx.ins().fadd(offset, modulus))
                        } else {
                            (F_ZERO, modulus)
                        };
                        let too_large = ctx.ctx.ins().fgt(val, max);
                        ctx.lower_multi_select(too_large, |mut ctx, too_large| {
                            if too_large {
                                [ctx.ctx.ins().fsub(val, min), F_ZERO]
                            } else {
                                let too_small = ctx.ctx.ins().flt(val, min);
                                ctx.lower_multi_select(too_small, |mut ctx, too_small| {
                                    if too_small {
                                        [ctx.ctx.ins().fsub(val, min), F_ZERO]
                                    } else {
                                        let arg = ctx.lower_expr(args[0]);
                                        [ctx.ctx.ins().fneg(arg), val]
                                    }
                                })
                            }
                        })
                    } else {
                        let arg = ctx.lower_expr(args[0]);
                        [ctx.ctx.ins().fneg(arg), val]
                    }
                } else {
                    let ic = ctx.lower_expr(args[1]);
                    [ctx.ctx.ins().fsub(val, ic), F_ZERO]
                }
            })
        } else {
            let arg = self.lower_expr(args[0]);
            [self.ctx.ins().fneg(arg), val]
        };

        self.ctx.def_resist_residual(residual[0], equation);
        self.ctx.def_react_residual(residual[1], equation);

        val
    }

    pub fn resolved_ty(&self, expr: ExprId) -> Type {
        self.body
            .needs_cast(expr)
            .map(|(_, dst)| dst.to_owned())
            .unwrap_or_else(|| self.body.expr_type(expr))
    }

    pub fn lower_body(&mut self, body: Body, i: usize) -> Value {
        let expr = body.borrow().get_entry_expr(i);
        BodyLoweringCtx { ctx: self.ctx, body: body.borrow(), path: self.path }.lower_expr(expr)
    }
}
