use hir::CompilationDB;
use lasso::Rodeo;
use mir::Function;
use mir_build::{FunctionBuilder, FunctionBuilderContext};

use crate::{HirInterner, ParamKind};

impl HirInterner {
    pub fn insert_var_init(
        &mut self,
        db: &CompilationDB,
        func: &mut Function,
        literals: &mut Rodeo,
    ) {
        let mut ctx = FunctionBuilderContext::default();
        let (mut builder, term) = FunctionBuilder::edit(func, literals, &mut ctx, false);
        for (kind, param) in self.params.clone().iter() {
            if let ParamKind::HiddenState(var) = *kind {
                if builder.func.dfg.value_dead(*param) {
                    continue;
                }
                let val = self.lower_expr_body(db, var.init(db), 0, &mut builder);
                builder.func.dfg.replace_uses(*param, val);
            }
        }

        builder.ensured_sealed();
        builder.func.layout.append_inst_to_bb(term, builder.current_block())
    }
}
