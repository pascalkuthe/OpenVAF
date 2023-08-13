use hir::{BranchWrite, Case, CaseCond, ContributeKind, ExprId, Node, Stmt, StmtId, Type};
use mir::builder::InstBuilder;
use mir::{Opcode, F_ZERO};

use crate::body::LoweringCtx;
use crate::{CallBackKind, CurrentKind, ParamKind, PlaceKind, RESISTIVE_DIM};

impl LoweringCtx<'_, '_> {
    pub(super) fn lower_stmt(&mut self, stmnt: StmtId) {
        // TODO(msrv): let .. else
        let stmnt = if let Some(stmnt) = self.body.get_stmt(stmnt) {
            stmnt
        } else {
            return;
        };
        match stmnt {
            Stmt::Expr(expr) => {
                self.lower_expr(expr);
            }
            Stmt::EventControl { body, .. } => {
                // TODO handle porperly
                self.lower_stmt(body);
            }
            Stmt::Assignment { lhs, rhs } => {
                let val_ = self.lower_expr(rhs);
                let place = self.place(lhs.into());
                self.func.def_var(place, val_)
            }
            Stmt::Contribute { kind, branch, rhs } => {
                self.contribute(kind == ContributeKind::Potential, branch, rhs)
            }

            Stmt::Block { body } => {
                for stmt in body {
                    self.lower_stmt(*stmt)
                }
            }
            Stmt::If { cond, then_branch, else_branch } => {
                let cond_ = self.lower_expr(cond);

                self.func.make_cond(cond_, |func, branch| {
                    let stmt = if branch { then_branch } else { else_branch };
                    LoweringCtx {
                        db: self.db,
                        data: self.data,
                        func,
                        body: self.body.clone(),
                        tagged_vars: self.tagged_vars,
                        places: self.places,
                        extra_dims: self.extra_dims.as_deref_mut(),
                        path: "",
                        contribute_rhs: false,
                        inside_lim: false,
                    }
                    .lower_stmt(stmt);
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
            Stmt::Case { discr, case_arms } => self.lower_case(discr, case_arms),
        }
    }

    fn lower_case(&mut self, discr: ExprId, case_arms: &[Case]) {
        let discr_op = match self.body.expr_type(discr) {
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

    fn contribute(&mut self, voltage_src: bool, mut write: BranchWrite, rhs: ExprId) {
        let mut negate = false;
        if let BranchWrite::Unnamed { hi, lo } = &mut write {
            self.lower_contribute_unnamed_branch(&mut negate, hi, lo, voltage_src)
        }
        let place = self.place(PlaceKind::IsVoltageSrc(write));
        self.func.def_var(place, voltage_src.into());

        let (hi, lo) = write.nodes(self.db);
        let is_zero = self.body.get_expr(rhs).is_zero();
        if voltage_src && is_zero {
            let collapsed = self.callback(CallBackKind::CollapseHint(hi, lo));
            self.func.ins().call(collapsed, &[]);
        }

        self.contribute_rhs = self.extra_dims.is_some();
        let val_ = self.lower_expr_(rhs);
        self.contribute_rhs = false;

        let mut add_contribute = |mut val, dim| {
            if val == F_ZERO {
                return;
            }

            let compl_place = PlaceKind::Contribute { dst: write, dim, voltage_src: !voltage_src };
            if let Some(place) = self.places.index(&compl_place) {
                self.func.def_var(place, F_ZERO);
            }

            if is_zero {
                return;
            }

            let place = PlaceKind::Contribute { dst: write, dim, voltage_src };
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
                if let Some(val) = vals.get(&rhs) {
                    add_contribute(*val, dim);
                }
            }
        }
    }

    fn lower_contribute_unnamed_branch(
        &mut self,
        negate: &mut bool,
        hi: &mut Node,
        lo: &mut Option<Node>,
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
}
