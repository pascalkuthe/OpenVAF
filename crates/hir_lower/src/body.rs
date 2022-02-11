use std::f64::{INFINITY, NEG_INFINITY};

use ahash::AHashSet;
use hir_def::body::{Body, ConstraintKind, ConstraintValue, ParamConstraint};
use hir_def::expr::{CaseCond, NZERO, PZERO};
use hir_def::{
    BuiltIn, Case, DefWithBodyId, Expr, ExprId, FunctionId, Literal, NodeId, ParamSysFun, Stmt,
    StmtId, Type, VarId,
};
use hir_ty::builtin::{
    ABS_INT, ABS_REAL, DDX_POT, IDT_NO_IC, MAX_INT, MAX_REAL, NATURE_ACCESS_BRANCH,
    NATURE_ACCESS_NODES, NATURE_ACCESS_NODE_GND, NATURE_ACCESS_PORT_FLOW, SIMPARAM_DEFAULT,
    SIMPARAM_NO_DEFAULT,
};
use hir_ty::db::HirTyDB;
use hir_ty::inference::{AssignDst, BranchWrite, InferenceResult, ResolvedFun};
use hir_ty::lower::BranchKind;
use hir_ty::types::{Ty as HirTy, BOOL_EQ, INT_EQ, INT_OP, REAL_EQ, REAL_OP, STR_EQ};
use lasso::Rodeo;
use mir::builder::InstBuilder;
use mir::{FuncRef, Function, Opcode, Value, FALSE, F_ZERO, GRAVESTONE, TRUE, ZERO};
use mir_build::{FunctionBuilder, FunctionBuilderContext, Place};
use stdx::iter::zip;
use stdx::packed_option::ReservedValue;
use typed_indexmap::TiSet;

use crate::{CallBackKind, CurrentKind, HirInterner, ParamInfoKind, ParamKind, PlaceKind};
use syntax::ast::{AssignOp, BinaryOp, UnaryOp};

pub struct MirBuilder<'a> {
    db: &'a dyn HirTyDB,
    def: DefWithBodyId,
    is_output: &'a dyn Fn(PlaceKind) -> bool,
    tagged_reads: AHashSet<VarId>,
    ctx: Option<&'a mut FunctionBuilderContext>,
}

impl<'a> MirBuilder<'a> {
    pub fn new(
        db: &'a dyn HirTyDB,
        def: DefWithBodyId,
        is_output: &'a dyn Fn(PlaceKind) -> bool,
    ) -> MirBuilder<'a> {
        MirBuilder { db, def, tagged_reads: AHashSet::new(), is_output, ctx: None }
    }

    pub fn tag_reads(&mut self, var: VarId) -> bool {
        self.tagged_reads.insert(var)
    }

    pub fn with_tagged_reads(mut self, taged_vars: AHashSet<VarId>) -> Self {
        self.tagged_reads = taged_vars;
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

    pub fn build(self) -> (Function, HirInterner, Rodeo) {
        let mut func = Function::default();
        let mut interner = HirInterner::default();
        let mut literals = Rodeo::new();

        let mut ctx_;
        let ctx = if let Some(ctx) = self.ctx {
            ctx
        } else {
            ctx_ = FunctionBuilderContext::new();
            &mut ctx_
        };

        let mut builder = FunctionBuilder::new(&mut func, &mut literals, ctx);

        let body = self.db.body(self.def);
        let infere = self.db.inference_result(self.def);
        let mut places = TiSet::default();

        let mut ctx = LoweringCtx {
            db: self.db,
            func: &mut builder,
            data: &mut interner,
            body: &body,
            infere: &infere,
            tagged_vars: &self.tagged_reads,
            places: &mut places,
        };

        ctx.lower_entry_stmts();
        let is_output = self.is_output;

        interner.outputs = places
            .iter_enumerated()
            .filter_map(|(place, kind)| {
                if is_output(*kind) {
                    let val = builder.use_var(place);
                    let val = builder.ins().optbarrier(val);
                    Some((*kind, val))
                } else {
                    None
                }
            })
            .collect();

        builder.ins().ret();
        builder.finalize();

        (func, interner, literals)
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
        let mut term = func.layout.entry_block().and_then(|bb| func.layout.last_inst(bb));
        if let Some(last_inst) = term {
            if func.dfg.insts[last_inst].is_terminator() {
                func.layout.remove_inst(last_inst);
            } else {
                term = None;
            }
        }
        let mut ctx = FunctionBuilderContext::default();
        let mut builder = FunctionBuilder::new(func, literals, &mut ctx);
        for (kind, param) in self.params.raw.iter() {
            if let ParamKind::HiddenState(var) = *kind {
                if builder.func.dfg.value_dead(*param) {
                    continue;
                }
                let val = self.lower_expr_body(db, var.into(), 0, &mut builder);
                builder.func.dfg.replace_uses(*param, val);
            }
        }

        if let Some(term) = term {
            builder.func.layout.append_inst_to_bb(term, builder.current_block())
        }
    }

    pub fn ensure_param(&mut self, func: &mut Function, kind: ParamKind) -> Value {
        let len = self.params.len();
        *self.params.raw.entry(kind).or_insert_with(|| func.dfg.make_param(len.into()))
    }

    pub fn insert_param_init(
        &mut self,
        db: &dyn HirTyDB,
        func: &mut Function,
        literals: &mut Rodeo,
        build_min_max: bool,
    ) {
        let mut term = func.layout.entry_block().and_then(|bb| func.layout.last_inst(bb));
        if let Some(last_inst) = term {
            if func.dfg.insts[last_inst].is_terminator() {
                func.layout.remove_inst(last_inst);
            } else {
                term = None;
            }
        }

        let f_neg_inf = func.dfg.fconst(NEG_INFINITY.into());
        let f_inf = func.dfg.fconst(INFINITY.into());
        let i_inf = func.dfg.iconst(i32::MAX);
        let i_neg_inf = func.dfg.iconst(i32::MIN);

        let mut ctx = FunctionBuilderContext::default();
        let mut builder = FunctionBuilder::new(func, literals, &mut ctx);

        for (kind, param_val) in self.params.raw.clone() {
            if let ParamKind::Param(param) = kind {
                let param_given = self.ensure_param(builder.func, ParamKind::ParamGiven { param });

                let body = db.body(param.into());
                let infere = db.inference_result(param.into());
                let info = db.param_exprs(param);
                let data = db.param_data(param).clone();
                let ty = &data.ty;

                let param_val = builder.make_select(param_given, |builder, param_given| {
                    if param_given {
                        param_val
                    } else {
                        self.lower_expr_body(db, param.into(), 0, builder)
                    }
                });

                self.outputs.insert(PlaceKind::Param(param), param_val);

                let mut ctx = LoweringCtx {
                    db,
                    data: self,
                    func: &mut builder,
                    places: &mut TiSet::default(),
                    body: &body,
                    infere: &infere,
                    tagged_vars: &AHashSet::default(),
                };

                let invalid = ctx.callback(CallBackKind::ParamInfo(ParamInfoKind::Invalid, param));

                let ops = CmpOps::from_ty(ty);

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
                                        let is_min =
                                            ctx.func.ins().binary1(ops.le.unwrap(), val, min);
                                        let min =
                                            ctx.func.make_select(is_min, |builder, is_min| {
                                                if is_min {
                                                    builder.ins().call(min_inclusive, &[]);
                                                    val
                                                } else {
                                                    min
                                                }
                                            });

                                        let is_max =
                                            ctx.func.ins().binary1(ops.le.unwrap(), max, val);
                                        let max =
                                            ctx.func.make_select(is_max, |builder, is_max| {
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
                                        let min =
                                            ctx.func.make_select(is_min, |builder, is_min| {
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
                                        let max =
                                            ctx.func.make_select(is_max, |builder, is_max| {
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

                    ctx.data.outputs.insert(PlaceKind::ParamMin(param), min);
                    ctx.data.outputs.insert(PlaceKind::ParamMax(param), max);
                    precomputed_vals
                } else {
                    vec![]
                };

                // first from bounds (here we also get min/max from)

                ctx.check_param(
                    param_val,
                    &info.bounds,
                    &precomuted_vals,
                    ConstraintKind::From,
                    ops,
                    invalid,
                );

                ctx.check_param(
                    param_val,
                    &info.bounds,
                    &precomuted_vals,
                    ConstraintKind::Exclude,
                    ops,
                    invalid,
                );
            }
        }

        let exit_bb = builder.func.layout.last_block().unwrap();
        builder.ins().jump(exit_bb);

        if let Some(term) = term {
            builder.func.layout.append_inst_to_bb(term, builder.current_block())
        }
    }

    /// Lowers a body
    fn lower_expr_body(
        &self,
        db: &dyn HirTyDB,
        def: DefWithBodyId,
        i: usize,
        func: &mut FunctionBuilder,
    ) -> Value {
        let body = db.body(def);
        let infere = db.inference_result(def);

        let mut ctx = LoweringCtx {
            db,
            data: &mut Self::default(),
            func,
            body: &body,
            infere: &infere,
            tagged_vars: &AHashSet::new(),
            places: &mut TiSet::default(),
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
    pub infere: &'a InferenceResult,
    pub tagged_vars: &'a AHashSet<VarId>,
}

impl LoweringCtx<'_, '_> {
    fn check_param(
        &mut self,
        param_val: Value,
        bounds: &[ParamConstraint],
        precomputed_vals: &[(Value, Value)],
        kind: ConstraintKind,
        ops: CmpOps,
        invalid: FuncRef,
    ) {
        let mut found_bound = false;
        let exit = self.func.create_block();

        for (i, bound) in bounds.iter().enumerate() {
            if bound.kind != kind {
                continue;
            }

            found_bound = true;

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

        if !found_bound {
            self.func.ins().jump(exit);
            self.func.switch_to_block(exit);
            return;
        }

        match kind {
            ConstraintKind::From => {
                // error on falltrough
                self.func.ins().call(invalid, &[]);

                self.func.ins().jump(exit);
                self.func.switch_to_block(exit);
            }

            ConstraintKind::Exclude => {
                let final_exit = self.func.create_block();
                self.func.ins().jump(final_exit);
                self.func.switch_to_block(exit);
                self.func.ins().call(invalid, &[]);
                self.func.ins().jump(final_exit);
                self.func.switch_to_block(final_exit);
            }
        }
    }

    pub fn lower_entry_stmts(&mut self) {
        for stmt in &*self.body.entry_stmts {
            self.lower_stmt(*stmt)
        }
    }

    fn collapse_hint(&mut self, hi: NodeId, lo: Option<NodeId>) {
        let hi = self.node(hi);
        let lo = lo.and_then(|lo| self.node(lo));
        let (hi, lo) = match (hi, lo) {
            (Some(node), None) | (None, Some(node)) => (node, None),
            (Some(hi), Some(lo)) => (hi, Some(lo)),
            (None, None) => unreachable!(),
        };
        let cb = self.callback(CallBackKind::CollapseHint(hi, lo));
        self.func.ins().call(cb, &[]);
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
                let mut val_ = if assignment_kind == AssignOp::Contribute
                    && matches!(
                        self.body.exprs[val],
                        Expr::Literal(Literal::Int(0) | Literal::Float(PZERO | NZERO)),
                    ) {
                    F_ZERO
                } else {
                    self.lower_expr(val)
                };
                if matches!(self.infere.assigment_destination[&stmt], AssignDst::Flow(_)) {
                    let mfactor = self.param(ParamKind::ParamSysFun(ParamSysFun::mfactor));
                    val_ = self.func.ins().fmul(val_, mfactor)
                }

                let mut negate = false;

                let place = match self.infere.assigment_destination[&stmt] {
                    AssignDst::Var(var) => self.place(PlaceKind::Var(var)),
                    AssignDst::FunVar { fun, arg: None } => {
                        self.place(PlaceKind::FunctionReturn { fun })
                    }
                    AssignDst::FunVar { arg: Some(arg), fun } => {
                        self.place(PlaceKind::FunctionArg { fun, arg })
                    }
                    AssignDst::Potential(BranchWrite::Explicit(branch)) if val_ == F_ZERO => {
                        let (hi, lo) = match self.db.branch_info(branch).unwrap().kind {
                            // TODO check in frontend
                            BranchKind::PortFlow(_) => unreachable!(),
                            BranchKind::NodeGnd(node) => (node, None),
                            BranchKind::Nodes(hi, lo) => (hi, Some(lo)),
                        };
                        return self.collapse_hint(hi, lo);
                    }
                    AssignDst::Potential(BranchWrite::Implict { hi, lo }) if val_ == F_ZERO => {
                        return self.collapse_hint(hi, lo)
                    }

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
                    let old = self.func.use_var(place);
                    val_ = if negate {
                        self.func.ins().fsub(old, val_)
                    } else {
                        self.func.ins().fadd(old, val_)
                    };
                };
                self.func.def_var(place, val_)
            }
            Stmt::Block { ref body } => {
                for stmt in body {
                    self.lower_stmt(*stmt)
                }
            }
            Stmt::If { cond, then_branch, else_branch } => {
                let cond = self.lower_expr(cond);
                self.func.make_cond(cond, |func, branch| {
                    let stmt = if branch { then_branch } else { else_branch };
                    LoweringCtx {
                        db: self.db,
                        data: self.data,
                        func,
                        body: self.body,
                        infere: self.infere,
                        tagged_vars: self.tagged_vars,
                        places: self.places,
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
        self.func.switch_to_block(end)
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
        let (place, inserted) = self.places.ensure(kind);
        if inserted {
            match kind {
                PlaceKind::Var(var) => {
                    let hidden_state = self.param(ParamKind::HiddenState(var));
                    let entry = self.func.func.layout.entry_block().unwrap();
                    self.func.def_var_at(place, hidden_state, entry);
                }
                // always initalized
                PlaceKind::FunctionReturn { .. }
                | PlaceKind::FunctionArg { .. }
                | PlaceKind::Param(_)
                | PlaceKind::ParamMin(_)
                | PlaceKind::ParamMax(_) => (),

                // always zero initalized
                PlaceKind::BranchVoltage(_)
                | PlaceKind::BranchCurrent(_)
                | PlaceKind::ImplicitBranchVoltage { .. }
                | PlaceKind::ImplicitBranchCurrent { .. } => {
                    let entry = self.func.func.layout.entry_block().unwrap();
                    self.func.def_var_at(place, F_ZERO, entry);
                }
            }
        }
        place
    }

    fn param(&mut self, kind: ParamKind) -> Value {
        self.data.ensure_param(self.func.func, kind)
    }

    fn callback(&mut self, kind: CallBackKind) -> FuncRef {
        let (func_ref, changed) = self.data.callbacks.ensure(kind);
        if changed {
            let data = kind.signature();
            self.func.import_function(data);
        }
        func_ref
    }

    fn lower_expr_as_lhs(&mut self, expr: ExprId, mut val: Value) {
        let place = match self.body.exprs[expr] {
            hir_def::Expr::Path { port: false, .. } => match self.infere.expr_types[expr] {
                HirTy::Var(_, var) => self.place(PlaceKind::Var(var)),
                HirTy::FuntionVar { fun, arg: None, .. } => {
                    self.place(PlaceKind::FunctionReturn { fun })
                }
                HirTy::FuntionVar { arg: Some(arg), fun, .. } => {
                    self.place(PlaceKind::FunctionArg { fun, arg })
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };

        if let Some(src) = self.infere.casts.get(&expr) {
            let dst = self.infere.expr_types[expr].to_value().unwrap();
            val = self.insert_cast(val, src, &dst)
        };

        self.func.def_var(place, val)
    }

    pub fn lower_expr(&mut self, expr: ExprId) -> Value {
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
                    let place = self.place(PlaceKind::FunctionReturn { fun });
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
                self.lower_select(cond, |s| s.lower_expr(then_val), |s| s.lower_expr(else_val))
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
        let arg_ = self.lower_expr(arg);
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
                    REAL_OP => self.func.ins().fneg(arg_),
                    INT_OP => self.func.ins().ineg(arg_),
                    _ => unreachable!(),
                }
            }
            UnaryOp::Identity => arg_,
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
                if self.data.params.contains_key(&param_kind(lo, Some(hi))) {
                    *negate = true;
                    self.place(kind(lo, Some(hi)))
                } else {
                    self.param(param_kind(hi, Some(lo)));
                    self.place(kind(hi, Some(lo)))
                }
            }
            (None, None) => unreachable!(),
        }
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

        let lhs = self.lower_expr(lhs);
        let rhs = self.lower_expr(rhs);
        let (inst, dfg) = self.func.ins().binary(op, lhs, rhs);
        dfg.first_result(inst)
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
            };
            if branch {
                lower_then_val(&mut ctx)
            } else {
                lower_else_val(&mut ctx)
            }
        })
    }

    fn lower_user_fun(&mut self, fun: FunctionId, args: &[ExprId]) -> Value {
        let info = self.db.function_data(fun);

        let body = self.db.body(fun.into());
        let infere = self.db.inference_result(fun.into());

        // TODO do not inline functions!
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
            self.func.def_var(place, init)
        }

        let init = match &info.return_ty {
            Type::Real => F_ZERO,
            Type::Integer => ZERO,
            ty => unreachable!("invalid function return type {:?}", ty),
        };
        let ret_place = self.place(PlaceKind::FunctionReturn { fun });
        self.func.def_var(ret_place, init);

        let mut ctx = LoweringCtx {
            db: self.db,
            data: self.data,
            func: self.func,
            body: &body,
            infere: &infere,
            tagged_vars: self.tagged_vars,
            places: self.places,
        };

        ctx.lower_entry_stmts();

        // write outputs back to original (including possibly required cast)
        for ((arg, info), expr) in zip(info.args.iter_enumerated(), args) {
            if info.is_output {
                let place = self.place(PlaceKind::FunctionArg { fun, arg });
                let val = self.func.use_var(place);
                self.lower_expr_as_lhs(*expr, val);
            }
        }

        self.func.use_var(ret_place)
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
            BuiltIn::limexp | BuiltIn::exp => {
                let arg0 = self.lower_expr(args[0]);
                self.func.ins().exp(arg0)
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
            | BuiltIn::finish => GRAVESTONE,

            BuiltIn::fatal => {
                //TODO terminate
                self.func.ins().ret();

                let unreachable_bb = self.func.create_block();
                self.func.switch_to_block(unreachable_bb);
                self.func.seal_block(unreachable_bb);
                GRAVESTONE
            }
            BuiltIn::analysis => FALSE,
            BuiltIn::ac_stim
            | BuiltIn::noise_table
            | BuiltIn::noise_table_log
            | BuiltIn::white_noise
            | BuiltIn::flicker_noise
            | BuiltIn::abstime
            | BuiltIn::ddt => F_ZERO,

            BuiltIn::idt | BuiltIn::idtmod => match *signature.unwrap() {
                IDT_NO_IC => F_ZERO,
                _ => self.lower_expr(args[1]),
            },

            // For benchmark
            // BuiltIn::idt | BuiltIn::idtmod | BuiltIn::ddt => match *signature.unwrap() {
            //     IDT_NO_IC => F_ZERO,
            //     _ => self.lower_expr(args[0]),
            // },
            BuiltIn::slew | BuiltIn::transition | BuiltIn::absdelay => self.lower_expr(args[0]),
            BuiltIn::flow => {
                let res = match_signature! {
                    signature:
                        NATURE_ACCESS_NODES|NATURE_ACCESS_NODE_GND => self.nodes_from_args(
                            args,
                            |hi, lo| ParamKind::Current(CurrentKind::ImplictBranch{hi,lo})
                        ),
                        NATURE_ACCESS_BRANCH => self.param(ParamKind::Current(
                        CurrentKind::ExplicitBranch(self.infere.expr_types[args[0]].unwrap_branch())
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

                let vt = self.func.fconst(KB / Q);
                let temp = match args.get(0) {
                    Some(temp) => self.lower_expr(*temp),
                    None => self.param(ParamKind::Temperature),
                };

                self.func.ins().fmul(vt, temp)
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
                let func_ref = self.callback(call);
                let arg0 = self.lower_expr(args[0]);
                if call == CallBackKind::SimParamOpt {
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
        }
    }
}
