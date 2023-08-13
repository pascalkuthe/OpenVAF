use std::f64::{INFINITY, NEG_INFINITY};
use std::mem::replace;

use ahash::AHashSet;
use hir::{CompilationDB, ConstraintValue, ParamConstraint, Parameter, Type};
use lasso::Rodeo;
use mir::builder::InstBuilder;
use mir::{Block, FuncRef, Function, Opcode, Value, FALSE, GRAVESTONE};
use mir_build::{FunctionBuilder, FunctionBuilderContext};
use stdx::packed_option::ReservedValue;
use syntax::ast::ConstraintKind;
use typed_indexmap::TiSet;

use crate::body::LoweringCtx;
use crate::{CallBackKind, HirInterner, ParamInfoKind, ParamKind, PlaceKind};

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
    pub fn insert_param_init(
        &mut self,
        db: &CompilationDB,
        func: &mut Function,
        literals: &mut Rodeo,
        build_min_max: bool,
        build_stores: bool,
        params: &[Parameter],
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

            let body = param.init(db);
            let ty = param.ty(db);
            let bounds = param.bounds(db);

            // create a temporary to hold onto the uses
            let new_val = builder.make_param(0u32.into());
            builder.func.dfg.replace_uses(param_val, new_val);

            let ops = CmpOps::from_ty(&ty);

            let (then_src, else_src) = builder.make_cond(param_given, |builder, param_given| {
                if param_given {
                    if build_stores {
                        let mut ctx = LoweringCtx {
                            db,
                            data: self,
                            func: builder,
                            places: &mut TiSet::default(),
                            body: body.borrow(),
                            tagged_vars: &AHashSet::default(),
                            extra_dims: None,
                            path: "",
                            contribute_rhs: false,
                            inside_lim: false,
                        };

                        let invalid =
                            ctx.callback(CallBackKind::ParamInfo(ParamInfoKind::Invalid, param));
                        let exit = ctx.func.create_block();

                        ctx.check_param(
                            param_val,
                            &bounds,
                            &[],
                            ConstraintKind::From,
                            ops,
                            invalid,
                            exit,
                        );

                        ctx.check_param(
                            param_val,
                            &bounds,
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
                    let default_val = self.lower_expr_body(db, body.clone(), 0, builder);
                    if build_stores {
                        let mut ctx = LoweringCtx {
                            db,
                            data: self,
                            func: builder,
                            places: &mut TiSet::default(),
                            body: body.borrow(),
                            tagged_vars: &AHashSet::default(),
                            extra_dims: None,
                            path: "",
                            contribute_rhs: false,
                            inside_lim: false,
                        };

                        let invalid =
                            ctx.callback(CallBackKind::ParamInfo(ParamInfoKind::Invalid, param));
                        let exit = ctx.func.create_block();

                        ctx.check_param(
                            default_val,
                            &bounds,
                            &[],
                            ConstraintKind::From,
                            ops,
                            invalid,
                            exit,
                        );

                        ctx.check_param(
                            default_val,
                            &bounds,
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
                body: body.borrow(),
                path: "",
                tagged_vars: &AHashSet::default(),
                extra_dims: None,
                contribute_rhs: false,
                inside_lim: false,
            };

            let invalid = ctx.callback(CallBackKind::ParamInfo(ParamInfoKind::Invalid, param));

            let precomputed_vals = if build_min_max {
                let min_inclusive =
                    ctx.callback(CallBackKind::ParamInfo(ParamInfoKind::MinInclusive, param));
                let max_inclusive =
                    ctx.callback(CallBackKind::ParamInfo(ParamInfoKind::MaxInclusive, param));
                let min_exclusive =
                    ctx.callback(CallBackKind::ParamInfo(ParamInfoKind::MinExclusive, param));
                let max_exclusive =
                    ctx.callback(CallBackKind::ParamInfo(ParamInfoKind::MaxExclusive, param));

                let mut lowered_bounds = None;
                let precomputed_vals = bounds
                    .iter()
                    .filter_map(|bound| {
                        if matches!(bound.kind, ConstraintKind::Exclude) {
                            return None;
                        }
                        let (val0, val1) = match bound.val {
                            ConstraintValue::Value(val) => {
                                let val = ctx.lower_expr(val);

                                if let Some((min, max)) = lowered_bounds {
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

                                    lowered_bounds = Some((min, max));
                                } else if ops.le.is_some() {
                                    lowered_bounds = Some((val, val));
                                    ctx.func.ins().call(min_inclusive, &[]);
                                    ctx.func.ins().call(max_inclusive, &[]);
                                }
                                (val, Value::reserved_value())
                            }
                            ConstraintValue::Range(range) => {
                                let start = ctx.lower_expr(range.start);
                                let end = ctx.lower_expr(range.end);

                                if let Some((min, max)) = lowered_bounds {
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

                                    lowered_bounds = Some((min, max));
                                } else {
                                    if range.start_inclusive {
                                        ctx.func.ins().call(min_inclusive, &[]);
                                    } else {
                                        ctx.func.ins().call(min_exclusive, &[]);
                                    }

                                    if range.end_inclusive {
                                        ctx.func.ins().call(max_inclusive, &[]);
                                    } else {
                                        ctx.func.ins().call(max_exclusive, &[]);
                                    }

                                    lowered_bounds = Some((start, end));
                                }

                                (start, end)
                            }
                        };

                        Some((val0, val1))
                    })
                    .collect();

                let (min, max) = lowered_bounds.unwrap_or_else(|| match ty {
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
                &bounds,
                &precomputed_vals,
                ConstraintKind::From,
                ops,
                invalid,
                exit,
            );

            ctx.check_param(
                param_val,
                &bounds,
                &precomputed_vals,
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
                    // error on fallthrough
                    self.func.ins().call(invalid, &[]);
                    self.func.ins().jump(global_exit);

                    self.func.switch_to_block(exit);
                }
            }

            ConstraintKind::Exclude => {
                self.func.ins().jump(global_exit);

                if let Some(exit) = exit {
                    // error on fallthrough
                    self.func.switch_to_block(exit);
                    self.func.ins().call(invalid, &[]);
                    self.func.ins().jump(global_exit);
                }
            }
        }
    }
}
