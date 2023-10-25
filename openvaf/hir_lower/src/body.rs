use hir::Node;
use hir::{BodyRef, ExprId};
use mir::builder::InstBuilder;
use mir::{Block, Value};
use stdx::iter::zip;

use crate::ctx::LoweringCtx;
use crate::ParamKind;

pub struct BodyLoweringCtx<'a, 'c1, 'c2> {
    pub ctx: &'a mut LoweringCtx<'c1, 'c2>,
    pub body: BodyRef<'a>,
    pub path: &'a str,
}

impl<'c1, 'c2> BodyLoweringCtx<'_, 'c1, 'c2> {
    pub fn lower_entry_stmts(&mut self) {
        for &stmnt in self.body.entry() {
            self.lower_stmt(stmnt)
        }
    }

    pub fn nodes_from_args(
        &mut self,
        args: &[ExprId],
        kind: impl Fn(Node, Option<Node>) -> ParamKind,
    ) -> Value {
        let hi = self.body.into_node(args[0]);
        let lo = args.get(1).map(|&arg| self.body.into_node(arg));
        self.ctx.nodes(hi, lo, kind)
    }

    pub fn lower_select(
        &mut self,
        cond: ExprId,
        lower_then_val: impl FnMut(BodyLoweringCtx<'_, 'c1, 'c2>) -> Value,
        lower_else_val: impl FnMut(BodyLoweringCtx<'_, 'c1, 'c2>) -> Value,
    ) -> Value {
        let cond = self.lower_expr(cond);
        self.lower_select_with(cond, lower_then_val, lower_else_val)
    }

    pub fn lower_select_with(
        &mut self,
        cond: Value,
        mut lower_then_val: impl FnMut(BodyLoweringCtx<'_, 'c1, 'c2>) -> Value,
        mut lower_else_val: impl FnMut(BodyLoweringCtx<'_, 'c1, 'c2>) -> Value,
    ) -> Value {
        self.ctx.make_select(cond, |ctx, branch| {
            let ctx = BodyLoweringCtx { ctx, body: self.body, path: self.path };
            if branch {
                lower_then_val(ctx)
            } else {
                lower_else_val(ctx)
            }
        })
    }

    pub fn lower_cond_with<T>(
        &mut self,
        cond: Value,
        mut lower_body: impl FnMut(BodyLoweringCtx<'_, 'c1, 'c2>, bool) -> T,
    ) -> ((Block, T), (Block, T)) {
        self.ctx.make_cond(cond, |ctx, branch| {
            let ctx = BodyLoweringCtx { ctx, body: self.body, path: self.path };
            lower_body(ctx, branch)
        })
    }

    pub fn lower_multi_select<const N: usize>(
        &mut self,
        cond: Value,
        lower_body: impl FnMut(BodyLoweringCtx<'_, 'c1, 'c2>, bool) -> [Value; N],
    ) -> [Value; N] {
        let ((then_bb, mut then_vals), (else_bb, else_vals)) =
            self.lower_cond_with(cond, lower_body);
        for (then_val, else_val) in zip(&mut then_vals, else_vals) {
            *then_val = self.ctx.ins().phi(&[(then_bb, *then_val), (else_bb, else_val)]);
        }
        then_vals
    }
}

impl LoweringCtx<'_, '_> {
    /// Lowers a body
    pub fn lower_expr_body(&mut self, body: BodyRef, i: usize) -> Value {
        BodyLoweringCtx { ctx: self, body, path: "" }.lower_expr(body.get_entry_expr(i))
    }
}
