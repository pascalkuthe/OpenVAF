#![allow(clippy::needless_option_as_deref)]

use std::f64::{INFINITY, NEG_INFINITY};
use std::mem::replace;

use ahash::{AHashMap, AHashSet};
use hir_def::body::{Body, ConstraintKind, ConstraintValue, ParamConstraint};
use hir_def::expr::{CaseCond, NZERO, PZERO};
use hir_def::{
    BuiltIn, Case, DefWithBodyId, Expr, ExprId, FunctionId, Literal, NodeId, ParamId, ParamSysFun,
    Stmt, StmtId, Type, VarId,
};
use hir_ty::builtin::{
    ABS_INT, ABS_REAL, DDX_POT, IDTMOD_IC, IDTMOD_IC_MODULUS, IDTMOD_IC_MODULUS_OFFSET,
    IDTMOD_IC_MODULUS_OFFSET_NATURE, IDTMOD_IC_MODULUS_OFFSET_TOL, IDTMOD_NO_IC, IDT_IC,
    IDT_IC_ASSERT, IDT_IC_ASSERT_NATURE, IDT_IC_ASSERT_TOL, IDT_NO_IC, MAX_INT, MAX_REAL,
    NATURE_ACCESS_BRANCH, NATURE_ACCESS_NODES, NATURE_ACCESS_NODE_GND, NATURE_ACCESS_PORT_FLOW,
    SIMPARAM_DEFAULT, SIMPARAM_NO_DEFAULT,
};
use hir_ty::db::HirTyDB;
use hir_ty::inference::{AssignDst, BranchWrite, InferenceResult, ResolvedFun};
use hir_ty::types::{Ty as HirTy, BOOL_EQ, INT_EQ, INT_OP, REAL_EQ, REAL_OP, STR_EQ};
use lasso::Rodeo;
use mir::builder::InstBuilder;
use mir::{Block, FuncRef, Function, Opcode, Value, FALSE, F_ZERO, GRAVESTONE, TRUE, ZERO};
use mir_build::{FunctionBuilder, FunctionBuilderContext, Place, RetBuilder};
use stdx::iter::zip;
use stdx::packed_option::ReservedValue;
use typed_index_collections::TiVec;
use typed_indexmap::TiSet;

use crate::{
    CallBackKind, CurrentKind, Dim, DimKind, DisplayKind, HirInterner, IdtKind, ImplicitEquation,
    ImplicitEquationKind, ParamInfoKind, ParamKind, PlaceKind, REACTIVE_DIM, RESISTIVE_DIM,
};
use syntax::ast::{BinaryOp, UnaryOp};

pub struct MirBuilder<'a> {
    db: &'a dyn HirTyDB,
    def: DefWithBodyId,
    is_output: &'a dyn Fn(PlaceKind) -> bool,
    required_vars: &'a mut dyn Iterator<Item = VarId>,
    tagged_reads: AHashSet<VarId>,
    tag_writes: bool,
    ctx: Option<&'a mut FunctionBuilderContext>,
    split_contribute: bool,
}

impl<'a> MirBuilder<'a> {
    pub fn new(
        db: &'a dyn HirTyDB,
        def: DefWithBodyId,
        is_output: &'a dyn Fn(PlaceKind) -> bool,
        required_vars: &'a mut dyn Iterator<Item = VarId>,
    ) -> MirBuilder<'a> {
        MirBuilder {
            db,
            def,
            tagged_reads: AHashSet::new(),
            is_output,
            required_vars,
            ctx: None,
            split_contribute: false,
            tag_writes: false,
        }
    }

    pub fn tag_reads(&mut self, var: VarId) -> bool {
        self.tagged_reads.insert(var)
    }

    pub fn with_tagged_reads(mut self, taged_vars: AHashSet<VarId>) -> Self {
        self.tagged_reads = taged_vars;
        self
    }

    pub fn tag_writes(&mut self) {
        self.tag_writes = true;
    }

    pub fn with_tagged_writes(mut self) -> Self {
        self.tag_writes = true;
        self
    }

    pub fn split_contributions(&mut self) {
        self.split_contribute = true;
    }

    pub fn with_split_contributions(mut self) -> Self {
        self.split_contribute = true;
        self
    }

    pub fn with_ctx(mut self, ctx: &'a mut FunctionBuilderContext) -> Self {
        self.ctx = Some(ctx);
        self
    }

    pub fn with_builder_ctx(mut self, ctx: &'a mut FunctionBuilderContext) -> Self {
        self.ctx = Some(ctx);
        self
    }

    pub fn build(self, literals: &mut Rodeo) -> (Function, HirInterner) {
        let mut func = Function::default();
        let mut interner = HirInterner::default();

        let mut ctx_;
        let ctx = if let Some(ctx) = self.ctx {
            ctx
        } else {
            ctx_ = FunctionBuilderContext::new();
            &mut ctx_
        };

        let mut builder = FunctionBuilder::new(&mut func, literals, ctx, self.tag_writes);

        let body = self.db.body(self.def);
        let path = if let DefWithBodyId::ModuleId(module) = self.def {
            self.db.module_data(module).name.to_string()
        } else {
            String::new()
        };
        let infere = self.db.inference_result(self.def);
        let mut places = TiSet::default();
        let mut extra_dims = TiVec::new();
        if self.split_contribute {
            interner.dims.push(DimKind::Resistive);
            interner.dims.push(DimKind::Reactive);
            extra_dims.push(AHashMap::default());
            extra_dims.push(AHashMap::default());
        }

        let mut ctx = LoweringCtx {
            db: self.db,
            func: &mut builder,
            data: &mut interner,
            body: &body,
            infere: &infere,
            tagged_vars: &self.tagged_reads,
            places: &mut places,
            extra_dims: self.split_contribute.then(|| &mut extra_dims),
            path: &path,
            contribute_rhs: false,
        };

        ctx.lower_entry_stmts();

        for var in self.required_vars {
            ctx.place(PlaceKind::Var(var));
        }

        let is_output = self.is_output;

        interner.outputs = places
            .iter_enumerated()
            .map(|(place, kind)| {
                if is_output(*kind) {
                    let mut val = builder.use_var(place);
                    if builder.func.dfg.values.def_allow_alias(val).inst().is_some() {
                        val = builder.ins().optbarrier(val);
                    }
                    (*kind, val.into())
                } else {
                    (*kind, None.into())
                }
            })
            .collect();

        builder.ins().ret();

        // println!("{}", builder.func.to_debug_string());
        builder.finalize();

        (func, interner)
    }
}

#[derive(Clone, Copy, Debug)]
struct CmpOps {
    lt: Option<Opcode>,
    le: Option<Opcode>,
    eq: Opcode,
}

impl CmpOps {
    fn from_ty(ty: &Type) -> Self {
        match ty {
            Type::Real => CmpOps { lt: Some(Opcode::Flt), le: Some(Opcode::Fle), eq: Opcode::Feq },
            Type::Integer => {
                CmpOps { lt: Some(Opcode::Ilt), le: Some(Opcode::Ile), eq: Opcode::Ieq }
            }
            Type::String => CmpOps { lt: None, le: None, eq: Opcode::Seq },
            Type::Array { ty, .. } => Self::from_ty(ty),
            Type::EmptyArray => CmpOps { lt: None, le: None, eq: Opcode::Ieq },
            _ => unreachable!(),
        }
    }

    fn in_bound(self, inclusive: bool) -> Opcode {
        if inclusive {
            self.le.unwrap()
        } else {
            self.lt.unwrap()
        }
    }
}

impl HirInterner {
    pub fn insert_var_init(&mut self, db: &dyn HirTyDB, func: &mut Function, literals: &mut Rodeo) {
        let mut ctx = FunctionBuilderContext::default();
        let (mut builder, term) = FunctionBuilder::edit(func, literals, &mut ctx, false);
        for (kind, param) in self.params.clone().raw.iter() {
            if let ParamKind::HiddenState(var) = *kind {
                if builder.func.dfg.value_dead(*param) {
                    continue;
                }
                let val = self.lower_expr_body(db, var.into(), 0, &mut builder);
                builder.func.dfg.replace_uses(*param, val);
            }
        }

        builder.ensured_sealed();
        builder.func.layout.append_inst_to_bb(term, builder.current_block())
    }

    pub fn insert_param_init(
        &mut self,
        db: &dyn HirTyDB,
        func: &mut Function,
        literals: &mut Rodeo,
        build_min_max: bool,
        build_stores: bool,
        params: &[ParamId],
    ) {
        let mut default_vals = if build_stores { vec![GRAVESTONE; params.len()] } else { vec![] };

        let f_neg_inf = func.dfg.fconst(NEG_INFINITY.into());
        let f_inf = func.dfg.fconst(INFINITY.into());
        let i_inf = func.dfg.iconst(i32::MAX);
        let i_neg_inf = func.dfg.iconst(i32::MIN);

        let mut ctx = FunctionBuilderContext::default();
        let (mut builder, term) = FunctionBuilder::edit(func, literals, &mut ctx, false);

        for (i, param) in params.iter().copied().enumerate() {
            let mut param_val = self.ensure_param(builder.func, ParamKind::Param(param));
            let param_given = self.ensure_param(builder.func, ParamKind::ParamGiven { param });

            let body = db.body(param.into());
            let infere = db.inference_result(param.into());
            let info = db.param_exprs(param);
            let data = db.param_data(param).clone();
            let ty = &data.ty;

            // create a temporary to hold onto the uses
            let new_val = builder.make_param(0u32.into());
            builder.func.dfg.replace_uses(param_val, new_val);

            let ops = CmpOps::from_ty(ty);

            let (then_src, else_src) = builder.make_cond(param_given, |builder, param_given| {
                if param_given {
                    if build_stores {
                        let mut ctx = LoweringCtx {
                            db,
                            data: self,
                            func: builder,
                            places: &mut TiSet::default(),
                            body: &body,
                            infere: &infere,
                            tagged_vars: &AHashSet::default(),
                            extra_dims: None,
                            path: "",
                            contribute_rhs: false,
                        };

                        let invalid =
                            ctx.callback(CallBackKind::ParamInfo(ParamInfoKind::Invalid, param));
                        let exit = ctx.func.create_block();

                        ctx.check_param(
                            param_val,
                            &info.bounds,
                            &[],
                            ConstraintKind::From,
                            ops,
                            invalid,
                            exit,
                        );

                        ctx.check_param(
                            param_val,
                            &info.bounds,
                            &[],
                            ConstraintKind::Exclude,
                            ops,
                            invalid,
                            exit,
                        );

                        ctx.func.switch_to_block(exit);
                    }
                    param_val
                } else {
                    let default_val = self.lower_expr_body(db, param.into(), 0, builder);
                    if build_stores {
                        let mut ctx = LoweringCtx {
                            db,
                            data: self,
                            func: builder,
                            places: &mut TiSet::default(),
                            body: &body,
                            infere: &infere,
                            tagged_vars: &AHashSet::default(),
                            extra_dims: None,
                            path: "",
                            contribute_rhs: false,
                        };

                        let invalid =
                            ctx.callback(CallBackKind::ParamInfo(ParamInfoKind::Invalid, param));
                        let exit = ctx.func.create_block();

                        ctx.check_param(
                            default_val,
                            &info.bounds,
                            &[],
                            ConstraintKind::From,
                            ops,
                            invalid,
                            exit,
                        );

                        ctx.check_param(
                            default_val,
                            &info.bounds,
                            &[],
                            ConstraintKind::Exclude,
                            ops,
                            invalid,
                            exit,
                        );

                        ctx.func.switch_to_block(exit);

                        default_vals[i] = builder.ins().optbarrier(default_val);
                    }
                    default_val
                }
            });

            // let last_inst = builder.func.layout.last_inst(else_src.0).unwrap();
            builder.ins().with_result(new_val).phi(&[then_src, else_src]);

            // we purposfull insert these reversed here (new val into params and old val into
            // outputs). This ensures that the code generated for other parameters uses the
            // correct value. After code generation is complete we swap these two again
            self.params.insert(ParamKind::Param(param), new_val);
            self.outputs.insert(PlaceKind::Param(param), param_val.into());
            param_val = new_val;

            if !build_min_max {
                continue;
            }

            let mut ctx = LoweringCtx {
                db,
                data: self,
                func: &mut builder,
                places: &mut TiSet::default(),
                body: &body,
                path: "",
                infere: &infere,
                tagged_vars: &AHashSet::default(),
                extra_dims: None,
                contribute_rhs: false,
            };

            let invalid = ctx.callback(CallBackKind::ParamInfo(ParamInfoKind::Invalid, param));

            let precomuted_vals = if build_min_max {
                let min_inclusive =
                    ctx.callback(CallBackKind::ParamInfo(ParamInfoKind::MinInclusive, param));

                let max_inclusive =
                    ctx.callback(CallBackKind::ParamInfo(ParamInfoKind::MaxInclusive, param));

                let min_exclusive =
                    ctx.callback(CallBackKind::ParamInfo(ParamInfoKind::MinExclusive, param));

                let max_exclusive =
                    ctx.callback(CallBackKind::ParamInfo(ParamInfoKind::MaxExclusive, param));

                let mut bounds = None;

                let precomputed_vals = info
                    .bounds
                    .iter()
                    .filter_map(|bound| {
                        if matches!(bound.kind, ConstraintKind::Exclude) {
                            return None;
                        }
                        let (val0, val1) = match bound.val {
                            ConstraintValue::Value(val) => {
                                let val = ctx.lower_expr(val);

                                if let Some((min, max)) = bounds {
                                    let is_min = ctx.func.ins().binary1(ops.le.unwrap(), val, min);
                                    let min = ctx.func.make_select(is_min, |builder, is_min| {
                                        if is_min {
                                            builder.ins().call(min_inclusive, &[]);
                                            val
                                        } else {
                                            min
                                        }
                                    });

                                    let is_max = ctx.func.ins().binary1(ops.le.unwrap(), max, val);
                                    let max = ctx.func.make_select(is_max, |builder, is_max| {
                                        if is_max {
                                            builder.ins().call(max_inclusive, &[]);
                                            val
                                        } else {
                                            min
                                        }
                                    });

                                    bounds = Some((min, max));
                                } else if ops.le.is_some() {
                                    bounds = Some((val, val));
                                    ctx.func.ins().call(min_inclusive, &[]);
                                    ctx.func.ins().call(max_inclusive, &[]);
                                }
                                (val, Value::reserved_value())
                            }
                            ConstraintValue::Range(range) => {
                                let start = ctx.lower_expr(range.start);
                                let end = ctx.lower_expr(range.end);

                                if let Some((min, max)) = bounds {
                                    let (op, call) = if range.start_inclusive {
                                        (ops.le.unwrap(), min_inclusive)
                                    } else {
                                        (ops.lt.unwrap(), min_exclusive)
                                    };

                                    let is_min = ctx.func.ins().binary1(op, start, min);
                                    let min = ctx.func.make_select(is_min, |builder, is_min| {
                                        if is_min {
                                            builder.ins().call(call, &[]);
                                            start
                                        } else {
                                            min
                                        }
                                    });

                                    let (op, call) = if range.end_inclusive {
                                        (ops.le.unwrap(), max_inclusive)
                                    } else {
                                        (ops.lt.unwrap(), max_exclusive)
                                    };

                                    let is_max = ctx.func.ins().binary1(op, max, end);
                                    let max = ctx.func.make_select(is_max, |builder, is_max| {
                                        if is_max {
                                            builder.ins().call(call, &[]);
                                            start
                                        } else {
                                            min
                                        }
                                    });

                                    bounds = Some((min, max));
                                } else {
                                    if range.start_inclusive {
                                        ctx.func.ins().call(min_inclusive, &[]);
                                    } else {
                                        ctx.func.ins().call(min_exclusive, &[]);
                                    }

                                    if range.start_inclusive {
                                        ctx.func.ins().call(max_inclusive, &[]);
                                    } else {
                                        ctx.func.ins().call(max_exclusive, &[]);
                                    }

                                    bounds = Some((start, end));
                                }

                                (start, end)
                            }
                        };

                        Some((val0, val1))
                    })
                    .collect();

                let (min, max) = bounds.unwrap_or_else(|| match ty {
                    Type::Real => (f_neg_inf, f_inf),
                    Type::Integer => (i_neg_inf, i_inf),
                    _ => unreachable!(),
                });

                ctx.data.outputs.insert(PlaceKind::ParamMin(param), min.into());

                ctx.data.outputs.insert(PlaceKind::ParamMax(param), max.into());
                precomputed_vals
            } else {
                vec![]
            };

            // first from bounds (here we also get min/max from)

            let exit = ctx.func.create_block();

            ctx.check_param(
                param_val,
                &info.bounds,
                &precomuted_vals,
                ConstraintKind::From,
                ops,
                invalid,
                exit,
            );

            ctx.check_param(
                param_val,
                &info.bounds,
                &precomuted_vals,
                ConstraintKind::Exclude,
                ops,
                invalid,
                exit,
            );

            ctx.func.switch_to_block(exit);
        }

        builder.ensured_sealed();
        builder.func.layout.append_inst_to_bb(term, builder.current_block());

        for (i, param) in params.iter().copied().enumerate() {
            let val = &mut self.params.raw[&ParamKind::Param(param)];
            let output_val = if build_stores { default_vals[i] } else { *val };
            *val = replace(&mut self.outputs[&PlaceKind::Param(param)], Some(output_val).into())
                .unwrap_unchecked();
        }
    }

    /// Lowers a body
    fn lower_expr_body(
        &mut self,
        db: &dyn HirTyDB,
        def: DefWithBodyId,
        i: usize,
        func: &mut FunctionBuilder,
    ) -> Value {
        let body = db.body(def);
        let infere = db.inference_result(def);

        let mut ctx = LoweringCtx {
            db,
            data: self,
            func,
            path: "",
            body: &body,
            infere: &infere,
            tagged_vars: &AHashSet::new(),
            places: &mut TiSet::default(),
            extra_dims: None,
            contribute_rhs: false,
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
    pub places: &'a mut TiSet<Place, PlaceKind>,
    pub func: &'a mut FunctionBuilder<'c>,
    pub body: &'a Body,
    pub path: &'a str,
    pub infere: &'a InferenceResult,
    pub tagged_vars: &'a AHashSet<VarId>,
    pub extra_dims: Option<&'a mut TiVec<Dim, AHashMap<ExprId, Value>>>,
    pub contribute_rhs: bool,
}

impl LoweringCtx<'_, '_> {
    #[allow(clippy::too_many_arguments)]
    fn check_param(
        &mut self,
        param_val: Value,
        bounds: &[ParamConstraint],
        precomputed_vals: &[(Value, Value)],
        kind: ConstraintKind,
        ops: CmpOps,
        invalid: FuncRef,
        global_exit: Block,
    ) {
        let mut exit = None;

        for (i, bound) in bounds.iter().enumerate() {
            if bound.kind != kind {
                continue;
            }

            let exit = match exit {
                Some(exit) => exit,
                None => {
                    let bb = self.func.create_block();
                    exit = Some(bb);
                    bb
                }
            };

            match bound.val {
                ConstraintValue::Value(val) => {
                    let val = precomputed_vals
                        .get(i)
                        .map_or_else(|| self.lower_expr(val), |(val, _)| *val);

                    let is_ok = self.func.ins().binary1(ops.eq, val, param_val);
                    let next_bb = self.func.create_block();
                    self.func.ins().br(is_ok, exit, next_bb);
                    self.func.switch_to_block(next_bb);
                }
                ConstraintValue::Range(range) => {
                    let (start, end) = precomputed_vals.get(i).map_or_else(
                        || (self.lower_expr(range.start), self.lower_expr(range.end)),
                        |(start, end)| (*start, *end),
                    );

                    let op = ops.in_bound(range.start_inclusive);
                    let is_lo_ok = self.func.ins().binary1(op, start, param_val);

                    let is_ok = self.func.make_select(is_lo_ok, |builder, is_ok| {
                        if is_ok {
                            let op = ops.in_bound(range.end_inclusive);
                            builder.ins().binary1(op, param_val, end)
                        } else {
                            FALSE
                        }
                    });

                    let next_bb = self.func.create_block();
                    self.func.ins().br(is_ok, exit, next_bb);
                    self.func.switch_to_block(next_bb);
                }
            }
        }

        match kind {
            ConstraintKind::From => {
                if let Some(exit) = exit {
                    // error on falltrough
                    self.func.ins().call(invalid, &[]);
                    self.func.ins().jump(global_exit);

                    self.func.switch_to_block(exit);
                }
            }

            ConstraintKind::Exclude => {
                self.func.ins().jump(global_exit);

                if let Some(exit) = exit {
                    // error on falltrough
                    self.func.switch_to_block(exit);
                    self.func.ins().call(invalid, &[]);
                    self.func.ins().jump(global_exit);
                }
            }
        }
    }

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
            Stmt::EventControl { body, .. } => {
                // TODO handle porperly
                self.lower_stmt(body);
            }
            Stmt::Assigment { val, .. } => {
                let place = match self.infere.assigment_destination[&stmt] {
                    AssignDst::Var(var) => PlaceKind::Var(var),
                    AssignDst::FunVar { fun, arg: None } => PlaceKind::FunctionReturn(fun),
                    AssignDst::FunVar { arg: Some(arg), fun } => {
                        PlaceKind::FunctionArg { fun, arg }
                    }
                    // TODO verify no contributes between GND nodes
                    AssignDst::Flow(kind) => return self.contribute(false, kind, val),
                    AssignDst::Potential(kind) => return self.contribute(true, kind, val),
                };

                let val_ = self.lower_expr(val);
                let place = self.place(place);
                self.func.def_var(place, val_)
            }

            Stmt::Block { ref body } => {
                for stmt in body {
                    self.lower_stmt(*stmt)
                }
            }
            Stmt::If { cond, then_branch, else_branch } => {
                let cond_ = self.lower_expr(cond);

                // let op_dependent_execution =
                //     self.op_dependent_execution || self.func.is_op_dependent(cond_);

                self.func.make_cond(cond_, |func, branch| {
                    let stmt = if branch { then_branch } else { else_branch };
                    LoweringCtx {
                        db: self.db,
                        data: self.data,
                        func,
                        body: self.body,
                        infere: self.infere,
                        tagged_vars: self.tagged_vars,
                        places: self.places,
                        extra_dims: self.extra_dims.as_deref_mut(),
                        path: "",
                        contribute_rhs: false,
                    }
                    .lower_stmt(stmt);
                });

                // if self.func.is_op_dependent(cond_) && !self.op_dependent_execution {
                //     println!("hmm exit {cond_}");
                // }
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
            Type::Real => Opcode::Feq,
            Type::Integer => Opcode::Ieq,
            Type::Bool => Opcode::Beq,
            Type::String => Opcode::Seq,
            Type::Array { .. } => todo!(),
            ty => unreachable!("Invalid type {}", ty),
        };
        let discr = self.lower_expr(discr);
        let end = self.func.create_block();

        for Case { cond, body } in case_arms {
            // TODO does default mean that further cases are ignored?
            // standard seems to suggest that no matter where the default case is placed that all
            // other conditions are tested prior
            let vals = match cond {
                CaseCond::Vals(vals) => vals,
                CaseCond::Default => continue,
            };

            // Create the body block
            let body_head = self.func.create_block();

            for val in vals {
                self.func.ensured_sealed();

                // Lower the condition (val == discriminant)
                let val_ = self.lower_expr(*val);

                let old_loc = self.func.get_srcloc();
                self.func.set_srcloc(mir::SourceLoc::new(u32::from(*val) as i32 + 1));
                let cond = self.func.ins().binary1(discr_op, val_, discr);
                self.func.set_srcloc(old_loc);

                // Create the next block
                let next_block = self.func.create_block();
                self.func.ins().branch(cond, body_head, next_block, false);

                self.func.switch_to_block(next_block);
            }

            self.func.seal_block(body_head);

            // lower the body
            let next = self.func.current_block();
            self.func.switch_to_block(body_head);
            self.lower_stmt(*body);
            self.func.ins().jump(end);
            self.func.switch_to_block(next);
        }

        if let Some(default_case) =
            case_arms.iter().find(|arm| matches!(arm.cond, CaseCond::Default))
        {
            self.lower_stmt(default_case.body);
        }

        self.func.ensured_sealed();
        self.func.ins().jump(end);

        self.func.seal_block(end);
        self.func.switch_to_block(end);
    }

    fn lower_loop(&mut self, cond: ExprId, lower_body: impl FnOnce(&mut Self)) {
        let loop_cond_head = self.func.create_block();
        let loop_body_head = self.func.create_block();
        let loop_end = self.func.create_block();

        self.func.ins().jump(loop_cond_head);
        self.func.switch_to_block(loop_cond_head);

        let cond = self.lower_expr(cond);
        self.func.ins().br_loop(cond, loop_body_head, loop_end);
        self.func.seal_block(loop_body_head);
        self.func.seal_block(loop_end);

        self.func.switch_to_block(loop_body_head);
        lower_body(self);
        self.func.ins().jump(loop_cond_head);

        self.func.seal_block(loop_cond_head);

        self.func.switch_to_block(loop_end);
    }

    fn place(&mut self, kind: PlaceKind) -> Place {
        Self::place_(self.func, self.data, self.places, kind)
    }

    fn contribute(&mut self, voltage_src: bool, mut kind: BranchWrite, val: ExprId) {
        let mut negate = false;
        if let BranchWrite::Unnamed { hi, lo } = &mut kind {
            self.lower_contribute_unnamed_branch(&mut negate, hi, lo, voltage_src)
        }
        let place = self.place(PlaceKind::IsVoltageSrc(kind));
        self.func.def_var(place, voltage_src.into());

        let (hi, lo) = kind.nodes(self.db);
        let is_zero = matches!(
            self.body.exprs[val],
            Expr::Literal(Literal::Int(0) | Literal::Float(PZERO | NZERO)),
        );
        if voltage_src && is_zero {
            let collapsed = self.callback(CallBackKind::CollapseHint(hi, lo));
            self.func.ins().call(collapsed, &[]);
        }

        self.contribute_rhs = self.extra_dims.is_some();
        let val_ = self.lower_expr_(val);
        self.contribute_rhs = false;

        let mut add_contribute = |mut val, dim| {
            if val == F_ZERO {
                return;
            }

            let compl_place = PlaceKind::Contribute { dst: kind, dim, voltage_src: !voltage_src };
            if let Some(place) = self.places.index(&compl_place) {
                self.func.def_var(place, F_ZERO);
            }

            if is_zero {
                return;
            }

            let place = PlaceKind::Contribute { dst: kind, dim, voltage_src };
            let place = Self::place_(self.func, self.data, self.places, place);
            let old = self.func.use_var(place);
            val = if negate {
                self.func.ins().fsub(old, val)
            } else if old == F_ZERO {
                val
            } else {
                self.func.ins().fadd(old, val)
            };
            self.func.def_var(place, val);
        };

        add_contribute(val_, RESISTIVE_DIM);
        if let Some(extra_dims) = self.extra_dims.as_deref_mut() {
            for (dim, vals) in extra_dims.iter_enumerated() {
                if let Some(val) = vals.get(&val) {
                    add_contribute(*val, dim);
                }
            }
        }
    }

    fn place_(
        func: &mut FunctionBuilder,
        intern: &mut HirInterner,
        places: &mut TiSet<Place, PlaceKind>,
        kind: PlaceKind,
    ) -> Place {
        let (place, inserted) = places.ensure(kind);
        if inserted {
            match kind {
                PlaceKind::Var(var) => {
                    let hidden_state = intern.ensure_param(func.func, ParamKind::HiddenState(var));
                    let entry = func.func.layout.entry_block().unwrap();
                    func.def_var_at(place, hidden_state, entry);
                }
                // always initalized
                PlaceKind::FunctionReturn { .. }
                | PlaceKind::FunctionArg { .. }
                | PlaceKind::Param(_)
                | PlaceKind::ParamMin(_)
                | PlaceKind::ParamMax(_) => (),

                // always zero initalized
                PlaceKind::ImplicitResidual { .. } | PlaceKind::Contribute { .. } => {
                    let entry = func.func.layout.entry_block().unwrap();
                    func.def_var_at(place, F_ZERO, entry);
                }
                PlaceKind::CollapseImplicitEquation(_) => {
                    let entry = func.func.layout.entry_block().unwrap();
                    func.def_var_at(place, TRUE, entry);
                }
                PlaceKind::IsVoltageSrc(_) => {
                    let entry = func.func.layout.entry_block().unwrap();
                    func.def_var_at(place, FALSE, entry);
                } // PlaceKind::CollapseNodes(_, _) => {
                  //     let entry = func.func.layout.entry_block().unwrap();
                  //     func.def_var_at(place, FALSE, entry);
                  // }
            }
        }
        place
    }

    fn param(&mut self, kind: ParamKind) -> Value {
        let (val, changed) =
            HirInterner::ensure_param_(&mut self.data.params, self.func.func, kind);
        if changed {
            self.func.op_dependent_vals.ensure(self.func.func.dfg.num_values());

            if matches!(
                kind,
                ParamKind::Voltage { .. }
                    | ParamKind::Current(_)
                    | ParamKind::ImplicitUnkown(_)
                    | ParamKind::Abstime
            ) {
                self.func.op_dependent_vals.insert(val);
            }
        }
        val
    }

    fn callback(&mut self, kind: CallBackKind) -> FuncRef {
        callback_(self.data, self.func, kind)
    }

    fn lower_expr_as_lhs(&mut self, expr: ExprId) -> Place {
        match self.body.exprs[expr] {
            hir_def::Expr::Path { port: false, .. } => match self.infere.expr_types[expr] {
                HirTy::Var(_, var) => self.place(PlaceKind::Var(var)),
                HirTy::FuntionVar { fun, arg: None, .. } => {
                    self.place(PlaceKind::FunctionReturn(fun))
                }
                HirTy::FuntionVar { arg: Some(arg), fun, .. } => {
                    self.place(PlaceKind::FunctionArg { fun, arg })
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    pub fn lower_expr_maybe_linear(&mut self, expr: ExprId, is_linear: bool) -> Value {
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
    pub fn lower_expr_(&mut self, expr: ExprId) -> Value {
        let old_loc = self.func.get_srcloc();
        self.func.set_srcloc(mir::SourceLoc::new(u32::from(expr) as i32 + 1));

        let mut res = match self.body.exprs[expr] {
            hir_def::Expr::Path { port: false, .. } => match self.infere.expr_types[expr] {
                HirTy::Var(_, var) => {
                    let place = self.place(PlaceKind::Var(var));
                    let mut val = self.func.use_var(place);
                    if self.tagged_vars.contains(&var) {
                        val = self.func.ins().optbarrier(val);
                        self.data.tagged_reads.insert(val, u32::from(var).into());
                    }
                    val
                }
                HirTy::FuntionVar { fun, arg: None, .. } => {
                    let place = self.place(PlaceKind::FunctionReturn(fun));
                    self.func.use_var(place)
                }
                HirTy::FuntionVar { arg: Some(arg), fun, .. } => {
                    let place = self.place(PlaceKind::FunctionArg { fun, arg });
                    self.func.use_var(place)
                }
                HirTy::Param(_, param) => self.param(ParamKind::Param(param)),
                HirTy::NatureAttr(_, attr) => {
                    self.data.lower_expr_body(self.db, attr.into(), 0, self.func)
                }
                ref expr => unreachable!("{:?}", expr),
            },
            hir_def::Expr::BinaryOp { lhs, rhs, op: Some(op) } => {
                self.lower_bin_op(expr, lhs, rhs, op)
            }
            hir_def::Expr::UnaryOp { expr: arg, op } => self.lower_unary_op(expr, arg, op),
            hir_def::Expr::Select { cond, then_val, else_val } => {
                let cond = self.lower_expr(cond);
                let (mut then_src, mut else_src) = self.func.make_cond(cond, |func, then| {
                    #[allow(clippy::needless_option_as_deref)]
                    let mut ctx = LoweringCtx {
                        db: self.db,
                        data: self.data,
                        func,
                        body: self.body,
                        infere: self.infere,
                        tagged_vars: self.tagged_vars,
                        places: self.places,
                        extra_dims: self.extra_dims.as_deref_mut(),
                        path: self.path,
                        contribute_rhs: self.contribute_rhs,
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
            hir_def::Expr::Call { ref args, .. } => match self.infere.resolved_calls[&expr] {
                ResolvedFun::User(fun) => self.lower_user_fun(fun, args),
                ResolvedFun::BuiltIn(builtin) => self.lower_builtin(expr, builtin, args),
                ResolvedFun::Param(param) => self.param(ParamKind::ParamSysFun(param)),
            },
            hir_def::Expr::Array(ref vals) => self.lower_array(expr, vals),
            hir_def::Expr::Literal(ref lit) => match *lit {
                hir_def::Literal::String(ref str) => self.func.sconst(str),
                hir_def::Literal::Int(val) => self.func.iconst(val),
                hir_def::Literal::Float(val) => self.func.fconst(val.into()),
                hir_def::Literal::Inf => {
                    self.func.set_srcloc(old_loc);
                    match self.infere.expr_types[expr].to_value().unwrap() {
                        Type::Real => return self.func.fconst(f64::INFINITY),
                        Type::Integer => return self.func.iconst(i32::MAX),
                        _ => unreachable!(),
                    }
                }
            },
            ref expr => unreachable!(
                "encounted invalid expr {:?}: this should have been caught in the frontend",
                expr
            ),
        };

        if let Some(dst) = self.infere.casts.get(&expr) {
            let src = self.infere.expr_types[expr].to_value().unwrap();
            res = self.insert_cast(res, &src, dst)
        };

        self.func.set_srcloc(old_loc);

        res
    }

    fn insert_cast(&mut self, val: Value, src: &Type, dst: &Type) -> Value {
        let op = match (dst, src) {
            (Type::Real, Type::Integer) => Opcode::IFcast,
            (Type::Integer, Type::Real) => Opcode::FIcast,
            (Type::Bool, Type::Real) => Opcode::FBcast,
            (Type::Real, Type::Bool) => Opcode::BFcast,
            (Type::Integer, Type::Bool) => Opcode::BIcast,
            (Type::Bool, Type::Integer) => Opcode::IBcast,
            (Type::Array { .. }, Type::EmptyArray) | (Type::EmptyArray, Type::Array { .. }) => {
                return val
            }
            _ => unreachable!("unkown cast found  {:?} -> {:?}", src, dst),
        };
        let inst = self.func.ins().unary(op, val).0;
        self.func.func.dfg.first_result(inst)
    }

    fn lower_unary_op(&mut self, expr: ExprId, arg: ExprId, op: UnaryOp) -> Value {
        let is_linear = (op == UnaryOp::Neg
            && Expr::Literal(Literal::Inf) != self.body.exprs[arg]
            && self.infere.resolved_signatures[&expr] == REAL_OP)
            || op == UnaryOp::Identity;
        let arg_ = self.lower_expr_maybe_linear(arg, is_linear);
        match op {
            UnaryOp::BitNegate => self.func.ins().ineg(arg_),
            UnaryOp::Not => self.func.ins().bnot(arg_),
            UnaryOp::Neg => {
                // Special case INFINITY
                if Expr::Literal(Literal::Inf) == self.body.exprs[arg] {
                    match self.infere.expr_types[arg].to_value().unwrap() {
                        Type::Real => return self.func.fconst(f64::NEG_INFINITY),
                        Type::Integer => return self.func.iconst(i32::MIN),
                        _ => unreachable!(),
                    }
                }
                match self.infere.resolved_signatures[&expr] {
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

    fn node(&self, node: NodeId) -> Option<NodeId> {
        if self.db.node_data(node).is_gnd {
            None
        } else {
            Some(node)
        }
    }

    fn nodes_from_args(
        &mut self,
        args: &[ExprId],
        kind: impl Fn(NodeId, Option<NodeId>) -> ParamKind,
    ) -> Value {
        let hi = self.infere.expr_types[args[0]].unwrap_node();
        let lo = args.get(1).map(|arg| self.infere.expr_types[*arg].unwrap_node());
        self.nodes(hi, lo, kind)
    }

    fn nodes(
        &mut self,
        hi: NodeId,
        lo: Option<NodeId>,
        kind: impl Fn(NodeId, Option<NodeId>) -> ParamKind,
    ) -> Value {
        let hi = self.node(hi);
        let lo = lo.and_then(|lo| self.node(lo));
        match (hi, lo) {
            (Some(hi), None) => self.param(kind(hi, None)),
            (None, Some(lo)) => {
                let lo = self.param(kind(lo, None));
                self.func.ins().fneg(lo)
            }
            // TODO refactor to nice if let binding when stable
            (Some(hi), Some(lo)) => {
                if let Some(inverted) = self.data.params.raw.get(&kind(lo, Some(hi))) {
                    self.func.ins().fneg(*inverted)
                } else {
                    self.param(kind(hi, Some(lo)))
                }
            }
            (None, None) => F_ZERO,
        }
    }

    fn lower_contribute_unnamed_branch(
        &mut self,
        negate: &mut bool,
        hi: &mut NodeId,
        lo: &mut Option<NodeId>,
        voltage_src: bool,
    ) {
        let hi_ = self.node(*hi);
        let lo_ = lo.and_then(|lo| self.node(lo));
        (*hi, *lo) = match (hi_, lo_) {
            (Some(hi), None) => (hi, None),
            (None, Some(lo)) => {
                *negate = true;
                (lo, None)
            }
            (Some(hi), Some(lo)) => {
                let existis = if let Some(dims) = self.extra_dims.as_deref() {
                    dims.keys().any(|dim| {
                        self.places.contains(&PlaceKind::Contribute {
                            dst: BranchWrite::Unnamed { hi: lo, lo: Some(hi) },
                            dim,
                            voltage_src,
                        })
                    })
                } else {
                    self.places.contains(&PlaceKind::Contribute {
                        dst: BranchWrite::Unnamed { hi: lo, lo: Some(hi) },
                        dim: RESISTIVE_DIM,
                        voltage_src,
                    })
                };
                if existis {
                    *negate = true;
                    (lo, Some(hi))
                } else {
                    let param_kind = if voltage_src {
                        ParamKind::Voltage { hi, lo: Some(lo) }
                    } else {
                        ParamKind::Current(CurrentKind::Unnamed { hi, lo: Some(lo) })
                    };
                    self.param(param_kind);
                    (hi, Some(lo))
                }
            }
            (None, None) => unreachable!(),
        };
    }

    fn lower_bin_op(&mut self, expr: ExprId, lhs: ExprId, rhs: ExprId, op: BinaryOp) -> Value {
        let signature = self.infere.resolved_signatures.get(&expr);
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
                    let mut lhs_ = self.lower_expr_(lhs);
                    let has_extra_dims = self
                        .extra_dims
                        .as_deref_mut()
                        .unwrap()
                        .iter()
                        .any(|vals| vals.contains_key(&lhs));
                    let is_op_dependent = has_extra_dims | self.func.is_op_dependent(lhs_);
                    if is_op_dependent {
                        let rhs_ = self.lower_expr(rhs);
                        if self.func.is_op_dependent(rhs_) {
                            lhs_ = self.lower_expr(lhs);
                        } else if has_extra_dims {
                            for dim_vals in self.extra_dims.as_deref_mut().unwrap() {
                                if let Some(val) = dim_vals.get(&lhs).copied() {
                                    let val = self.func.ins().binary1(op, val, rhs_);
                                    dim_vals.insert(expr, val);
                                }
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

    fn lower_select(
        &mut self,
        cond: ExprId,
        lower_then_val: impl FnMut(&mut LoweringCtx) -> Value,
        lower_else_val: impl FnMut(&mut LoweringCtx) -> Value,
    ) -> Value {
        let cond = self.lower_expr(cond);
        self.lower_select_with(cond, lower_then_val, lower_else_val)
    }

    fn lower_select_with(
        &mut self,
        cond: Value,
        mut lower_then_val: impl FnMut(&mut LoweringCtx) -> Value,
        mut lower_else_val: impl FnMut(&mut LoweringCtx) -> Value,
    ) -> Value {
        self.func.make_select(cond, |func, branch| {
            let mut ctx = LoweringCtx {
                db: self.db,
                data: self.data,
                func,
                body: self.body,
                infere: self.infere,
                tagged_vars: self.tagged_vars,
                places: self.places,
                extra_dims: self.extra_dims.as_deref_mut(),
                path: self.path,
                contribute_rhs: self.contribute_rhs,
            };
            if branch {
                lower_then_val(&mut ctx)
            } else {
                lower_else_val(&mut ctx)
            }
        })
    }

    fn lower_cond_with<T>(
        &mut self,
        cond: Value,
        mut lower_body: impl FnMut(&mut LoweringCtx, bool) -> T,
    ) -> ((Block, T), (Block, T)) {
        self.func.make_cond(cond, |func, branch| {
            #[allow(clippy::needless_option_as_deref)]
            let mut ctx = LoweringCtx {
                db: self.db,
                data: self.data,
                func,
                body: self.body,
                infere: self.infere,
                tagged_vars: self.tagged_vars,
                places: self.places,
                extra_dims: self.extra_dims.as_deref_mut(),
                path: self.path,
                contribute_rhs: self.contribute_rhs,
            };
            lower_body(&mut ctx, branch)
        })
    }

    fn lower_multi_select<const N: usize>(
        &mut self,
        cond: Value,
        lower_body: impl FnMut(&mut LoweringCtx, bool) -> [Value; N],
    ) -> [Value; N] {
        let ((then_bb, mut then_vals), (else_bb, else_vals)) =
            self.lower_cond_with(cond, lower_body);
        for (then_val, else_val) in zip(&mut then_vals, else_vals) {
            *then_val = self.func.ins().phi(&[(then_bb, *then_val), (else_bb, else_val)]);
        }
        then_vals
    }

    fn lower_user_fun(&mut self, fun: FunctionId, args: &[ExprId]) -> Value {
        let info = self.db.function_data(fun);
        let mut path = self.path.to_owned();
        path.push_str(&*info.name);

        let body = self.db.body(fun.into());
        let infere = self.db.inference_result(fun.into());

        // TODO do not inline functions?
        for ((arg, info), expr) in zip(info.args.iter_enumerated(), args) {
            let place = self.place(PlaceKind::FunctionArg { fun, arg });
            let init = if info.is_input {
                self.lower_expr(*expr)
            } else {
                match &info.ty {
                    Type::Real => F_ZERO,
                    Type::Integer => ZERO,
                    ty => unreachable!("invalid function arg type {:?}", ty),
                }
            };

            self.func.def_var(place, init);
        }

        let init = match &info.return_ty {
            Type::Real => F_ZERO,
            Type::Integer => ZERO,
            ty => unreachable!("invalid function return type {:?}", ty),
        };
        let ret_place = self.place(PlaceKind::FunctionReturn(fun));
        self.func.def_var(ret_place, init);

        let mut ctx = LoweringCtx {
            db: self.db,
            data: self.data,
            func: self.func,
            body: &body,
            path: self.path,
            infere: &infere,
            tagged_vars: self.tagged_vars,
            places: self.places,
            extra_dims: None,
            // can not contain contribute so doesn't matter
            contribute_rhs: false,
        };

        ctx.lower_entry_stmts();

        // write outputs back to original (including possibly required cast)
        for ((arg, info), expr) in zip(info.args.iter_enumerated(), args) {
            if info.is_output {
                let src_place = self.place(PlaceKind::FunctionArg { fun, arg });
                let dst_place = self.lower_expr_as_lhs(*expr);

                let mut val = self.func.use_var(src_place);

                if let Some(src) = self.infere.casts.get(expr) {
                    let dst = self.infere.expr_types[*expr].to_value().unwrap();
                    val = self.insert_cast(val, src, &dst)
                }

                self.func.def_var(dst_place, val);
            }
        }

        self.func.use_var(ret_place)
    }

    fn implicit_eqation(&mut self, kind: ImplicitEquationKind) -> (ImplicitEquation, Value) {
        let equation = self.data.implicit_equations.push_and_get_key(kind);
        let place = self.place(PlaceKind::CollapseImplicitEquation(equation));
        self.func.def_var(place, FALSE);
        let val = self.param(ParamKind::ImplicitUnkown(equation));
        (equation, val)
    }

    fn define_resist_residual(&mut self, residual_val: Value, equation: ImplicitEquation) {
        let place = PlaceKind::ImplicitResidual { equation, dim: RESISTIVE_DIM };
        let place = self.place(place);
        self.func.def_var(place, residual_val);
    }

    fn define_react_residual(&mut self, residual_val: Value, equation: ImplicitEquation) {
        let place = PlaceKind::ImplicitResidual { equation, dim: REACTIVE_DIM };
        let place = self.place(place);
        self.func.def_var(place, residual_val);
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
        self.define_resist_residual(residual[1], equation);

        val
    }

    fn lower_builtin(&mut self, expr: ExprId, builtin: BuiltIn, args: &[ExprId]) -> Value {
        // let arg = |sel, i| sel.lower_expr(args[i]);
        let signature = self.infere.resolved_signatures.get(&expr);
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
            BuiltIn::finish | BuiltIn::stop => GRAVESTONE,

            BuiltIn::fatal => {
                self.ins_display(DisplayKind::Fatal, true, args);
                self.func.ins().ret();

                let unreachable_bb = self.func.create_block();
                self.func.switch_to_block(unreachable_bb);
                self.func.seal_block(unreachable_bb);
                GRAVESTONE
            }
            BuiltIn::analysis => TRUE,
            BuiltIn::ac_stim
            | BuiltIn::noise_table
            | BuiltIn::noise_table_log
            | BuiltIn::white_noise
            | BuiltIn::flicker_noise => F_ZERO,

            BuiltIn::abstime => self.param(ParamKind::Abstime),

            BuiltIn::ddt if self.contribute_rhs => {
                self.contribute_rhs = false;
                let arg0 = self.lower_expr(args[0]);
                self.contribute_rhs = true;
                if self.func.is_op_dependent(arg0) {
                    if let Some(extra_dims) = &mut self.extra_dims {
                        extra_dims[REACTIVE_DIM].insert(expr, arg0);
                    }
                }
                F_ZERO
            }

            BuiltIn::ddt => {
                if self.extra_dims.is_some() {
                    let arg0 = self.lower_expr(args[0]);
                    if self.func.is_op_dependent(arg0) {
                        let (equation, val) = self.implicit_eqation(ImplicitEquationKind::Ddt);
                        self.define_resist_residual(val, equation);
                        let react_residual = self.func.ins().fneg(val);
                        self.define_react_residual(react_residual, equation);
                        return val;
                    }
                }
                F_ZERO
            }
            BuiltIn::idt | BuiltIn::idtmod if self.extra_dims.is_none() => {
                match *signature.unwrap() {
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

            BuiltIn::slew | BuiltIn::transition | BuiltIn::absdelay => self.lower_expr(args[0]),
            BuiltIn::flow => {
                let res = match_signature! {
                    signature:
                        NATURE_ACCESS_NODES|NATURE_ACCESS_NODE_GND => self.nodes_from_args(
                            args,
                            |hi, lo| ParamKind::Current(CurrentKind::Unnamed{hi,lo})
                        ),
                        NATURE_ACCESS_BRANCH => self.param(ParamKind::Current(
                        CurrentKind::Branch(self.infere.expr_types[args[0]].unwrap_branch())
                    )),
                        NATURE_ACCESS_PORT_FLOW => self.param(ParamKind::Current(
                            CurrentKind::Port(self.infere.expr_types[args[0]].unwrap_port_flow())
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
                            let branch = self.infere.expr_types[args[0]].unwrap_branch();
                            let info =self.db.branch_info(branch).unwrap().kind;
                            self.nodes( info.unwrap_hi_node(), info.lo_node(), |hi,lo|ParamKind::Voltage{hi,lo})
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
                let call = if *signature.unwrap() == DDX_POT {
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
            BuiltIn::param_given => self.param(ParamKind::ParamGiven {
                param: self.infere.expr_types[args[0]].unwrap_param(),
            }),
            BuiltIn::port_connected => self.param(ParamKind::PortConnected {
                port: self.infere.expr_types[args[0]].unwrap_node(),
            }),
            BuiltIn::bound_step => {
                let cb = self.callback(CallBackKind::BoundStep);
                let step_size = self.lower_expr(args[0]);
                self.func.ins().call(cb, &[step_size]);
                GRAVESTONE
            }
            _ => todo!(),
            // TODO files
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

            // // TODO impelement?
            // // TODO what is the DC value?
            // BuiltIn::zi_nd => todo!(),
            // BuiltIn::zi_np => todo!(),
            // BuiltIn::zi_zd => todo!(),
            // BuiltIn::zi_zp => todo!(),
            // BuiltIn::laplace_nd => todo!(),
            // BuiltIn::laplace_np => todo!(),
            // BuiltIn::laplace_zd => todo!(),
            // BuiltIn::laplace_zp => todo!(),
            // BuiltIn::last_crossing => return 0f64.into(),
        }
    }

    fn resolved_ty(&self, expr: ExprId) -> Type {
        self.infere
            .casts
            .get(&expr)
            .cloned()
            .unwrap_or_else(|| self.infere.expr_types[expr].to_value().unwrap())
    }

    fn ins_display(&mut self, kind: DisplayKind, newline: bool, args: &[ExprId]) {
        let mut fmt_lit = String::new();
        let mut call_args = vec![GRAVESTONE];
        let mut arg_tys = Vec::new();

        let mut i = 0;

        while let Some(expr) = args.get(i) {
            i += 1;
            if let Expr::Literal(Literal::String(ref lit)) = self.body.exprs[*expr] {
                fmt_lit.reserve(lit.len());
                let mut last_percent = false;
                for c in lit.chars() {
                    if last_percent {
                        match c {
                            '%' => fmt_lit.push_str("%%"),
                            'm' | 'M' => {
                                fmt_lit.push_str(self.path);
                            }
                            'l' | 'L' => {
                                // TODO support properly
                                fmt_lit.push_str("__.__");
                            }
                            c => {
                                fmt_lit.push('%');
                                fmt_lit.push(c);
                                let ty = self.resolved_ty(args[i]);
                                arg_tys.push(ty);
                                call_args.push(self.lower_expr(args[i]));
                                i += 1;
                            }
                        }
                        last_percent = false
                    } else if c == '%' {
                        last_percent = true;
                    } else {
                        fmt_lit.push(c)
                    }
                }
            } else {
                let ty = self.resolved_ty(*expr);
                match ty {
                    Type::Real => fmt_lit.push_str("%f"),
                    Type::Integer => fmt_lit.push_str("%d"),
                    Type::String => {
                        // if i == 0 {
                        //     TODO warn
                        // }
                        fmt_lit.push_str("%s")
                    }
                    Type::Void => {
                        fmt_lit.push(' ');
                        continue;
                    }
                    _ => unreachable!(),
                }

                arg_tys.push(ty);
                call_args.push(self.lower_expr(*expr));
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
}

/* TODO display typecheck
while let Some(c) = chars.next(){
         if c == '%'{
             match chars.next(){
                 '%' => ()
                 'h'|'d'|

             }
         }

         lit.push(ch)
     }*/

fn callback_(intern: &mut HirInterner, func: &mut FunctionBuilder, kind: CallBackKind) -> FuncRef {
    let data = kind.signature();
    let (func_ref, changed) = intern.callbacks.ensure(kind);
    if changed {
        let sig = func.func.import_function(data);
        debug_assert_eq!(func_ref, sig);
    }
    func_ref
}
