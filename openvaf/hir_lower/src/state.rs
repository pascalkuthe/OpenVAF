use hir::CompilationDB;
use lasso::Rodeo;
use mir::Function;
use mir_build::{FunctionBuilder, FunctionBuilderContext};

use crate::ctx::LoweringCtx;
use crate::{HirInterner, ParamKind};

impl HirInterner {
    pub fn insert_var_init(
        &mut self,
        db: &CompilationDB,
        func: &mut Function,
        literals: &mut Rodeo,
    ) {
        let mut ctx = FunctionBuilderContext::default();
        let (builder, term) = FunctionBuilder::edit(func, literals, &mut ctx, false);
        let mut ctx = LoweringCtx::new(db, builder, true, self);
        for (kind, param) in ctx.intern.params.clone().iter() {
            if let ParamKind::HiddenState(var) = *kind {
                if ctx.dfg().value_dead(*param) {
                    continue;
                }
                let val = ctx.lower_expr_body(var.init(db).borrow(), 0);
                ctx.dfg_mut().replace_uses(*param, val);
            }
        }

        ctx.ensured_sealed();
        ctx.func.func.layout.append_inst_to_bb(term, ctx.current_block())
    }
}
