use bitset::BitSet;
use hir_def::ModuleId;
use hir_lower::{CallBackKind, HirInterner, MirBuilder, ParamKind, PlaceKind};
use lasso::Rodeo;
use mir::cursor::{Cursor, FuncCursor};
use mir::{ControlFlowGraph, Function};
use mir_autodiff::auto_diff;
use mir_opt::{
    dead_code_elimination, inst_combine, simplify_cfg, sparse_conditional_constant_propagation,
};

use crate::compilation_db::CompilationDB;
use crate::matrix::JacobianMatrix;
use crate::residual::Residual;

pub struct AnalogBlockMir {
    pub func: Function,
    pub intern: HirInterner,
    pub cfg: ControlFlowGraph,
    pub matrix: JacobianMatrix,
    pub residual: Residual,
}

impl AnalogBlockMir {
    pub fn new(db: &CompilationDB, module: ModuleId) -> (AnalogBlockMir, Rodeo) {
        let (mut func, mut intern, mut literals) = MirBuilder::new(db, module.into(), &|kind| {
            matches!(
                kind,
                // PlaceKind::Var(_)
                PlaceKind::BranchVoltage { .. }
                    | PlaceKind::ImplicitBranchVoltage { .. }
                    | PlaceKind::BranchCurrent { .. }
                    | PlaceKind::ImplicitBranchCurrent { .. }
            )
        })
        .with_split_contributions()
        .build();

        // TODO enable hidden state or warn
        intern.insert_var_init(db, &mut func, &mut literals);

        let mut cfg = ControlFlowGraph::new();
        cfg.compute(&func);

        simplify_cfg(&mut func, &mut cfg);

        for (param, (kind, _)) in intern.params.iter_enumerated() {
            if matches!(kind, ParamKind::Voltage { .. } | ParamKind::Current(_)) {
                let changed = intern.callbacks.ensure(CallBackKind::Derivative(param)).1;
                if changed {
                    let signature = CallBackKind::Derivative(param).signature();
                    func.import_function(signature);
                }
            }
        }

        let mut output_values = BitSet::new_empty(func.dfg.num_values());
        output_values.extend(intern.outputs.values().copied());
        let callbacks = &intern.callbacks;

        // TODO be smart about this?
        let extra_derivatives: Vec<_> = intern
            .outputs
            .iter()
            .filter_map(|(kind, val)| {
                if matches!(
                    kind,
                    PlaceKind::BranchVoltage { .. }
                        | PlaceKind::ImplicitBranchVoltage { .. }
                        | PlaceKind::BranchCurrent { .. }
                        | PlaceKind::ImplicitBranchCurrent { .. }
                ) {
                    Some(*val)
                } else {
                    None
                }
            })
            .flat_map(|val| {
                intern.params.iter_enumerated().filter_map(move |(param, (kind, _))| {
                    if matches!(kind, ParamKind::Voltage { .. } | ParamKind::Current(_)) {
                        Some((val, callbacks.unwrap_index(&CallBackKind::Derivative(param))))
                    } else {
                        None
                    }
                })
            })
            .collect();

        dead_code_elimination(&mut func, &output_values);

        let ad = auto_diff(&mut func, &cfg, intern.unkowns(), extra_derivatives);

        let mut matrix = JacobianMatrix::default();
        let mut residual = Residual::default();

        let output_block = {
            let val = intern.outputs[0];
            let inst = func.dfg.value_def(val).unwrap_inst();
            func.layout.inst_block(inst).unwrap()
        };

        let mut cursor = FuncCursor::new(&mut func).at_bottom(output_block);

        matrix.populate(db, &mut cursor, &intern, &ad);
        residual.populate(db, &mut cursor, &intern);
        output_values.ensure(cursor.func.dfg.num_values() + 1);
        matrix.insert_opt_barries(&mut cursor, &mut output_values);
        residual.insert_opt_barries(&mut cursor, &mut output_values);

        inst_combine(&mut func);
        sparse_conditional_constant_propagation(&mut func, &cfg);
        dead_code_elimination(&mut func, &output_values);
        simplify_cfg(&mut func, &mut cfg);

        inst_combine(&mut func);

        matrix.strip_opt_barries(&mut func, &mut output_values);
        residual.strip_opt_barries(&mut func, &mut output_values);
        matrix.sparsify();

        (AnalogBlockMir { func, intern, matrix, cfg, residual }, literals)
    }
}
