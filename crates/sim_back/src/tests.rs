use std::ffi::c_void;
use std::path::Path;

use ahash::AHashMap;
use hir_def::db::HirDefDB;
use hir_lower::ParamKind;
use lasso::{Rodeo, Spur};
use mir::{FuncRef, Function, Param, Value};
use mir_interpret::{Data, Func, Interpreter, InterpreterState};
use paths::AbsPathBuf;
use typed_index_collections::{TiSlice, TiVec};

use crate::compilation_db::CompilationDB;
use crate::matrix::JacobianMatrix;
use crate::middle::EvalMir;
use crate::SimUnkown;

mod stamps;

fn compile_to_mir(path: &Path) -> (CompilationDB, EvalMir, Rodeo) {
    let path = AbsPathBuf::assert(path.canonicalize().unwrap());
    let db = CompilationDB::new(path, &[], &[], &[]).unwrap();
    let modules = db.collect_modules().unwrap();
    let mut literals = Rodeo::new();
    let mir = EvalMir::new(&db, &modules[0], &mut literals);
    (db, mir, literals)
}

impl JacobianMatrix {
    fn resistive_stamps(&self, db: &dyn HirDefDB) -> AHashMap<(String, String), Value> {
        self.resistive
            .raw
            .iter()
            .filter_map(|(entry, val)| {
                if let (SimUnkown::KirchoffLaw(row), SimUnkown::KirchoffLaw(col)) =
                    (entry.row, entry.col)
                {
                    let row = db.node_data(row).name.to_string();
                    let col = db.node_data(col).name.to_string();
                    Some(((row, col), *val))
                } else {
                    None
                }
            })
            .collect()
    }

    fn reactive_stamps(&self, db: &dyn HirDefDB) -> AHashMap<(String, String), Value> {
        self.reactive
            .raw
            .iter()
            .filter_map(|(entry, val)| {
                if let (SimUnkown::KirchoffLaw(row), SimUnkown::KirchoffLaw(col)) =
                    (entry.row, entry.col)
                {
                    let row = db.node_data(row).name.to_string();
                    let col = db.node_data(col).name.to_string();
                    Some(((row, col), *val))
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn print_with_nums(&self, db: &dyn HirDefDB, vals: &InterpreterState) {
        for (entry, val) in &self.resistive.raw {
            let num: f64 = vals.read(*val);

            if let (SimUnkown::KirchoffLaw(row), SimUnkown::KirchoffLaw(col)) =
                (entry.row, entry.col)
            {
                println!(
                    "({}, {}) = {} = {}",
                    db.node_data(row).name,
                    db.node_data(col).name,
                    val,
                    num
                )
            }
        }
    }
}

impl EvalMir {
    pub fn interpret(
        &self,
        db: &CompilationDB,
        literals: &mut Rodeo,
        params: &AHashMap<&str, Data>,
        node_voltages: &AHashMap<&str, f64>,
        temp: f64,
    ) -> InterpreterState {
        let empty_str = literals.get_or_intern_static("");
        let val = Box::leak(Box::new(empty_str));

        let dummy_calls: TiVec<_, _> = self
            .eval_intern
            .callbacks
            .raw
            .iter()
            .map(|func| {
                let mut ptr = std::ptr::null_mut();
                let res = match func {
                    hir_lower::CallBackKind::Derivative(_)
                    | hir_lower::CallBackKind::NodeDerivative(_)
                    | hir_lower::CallBackKind::SimParam => {
                        |state: &mut InterpreterState,
                         _args: &[Value],
                         rets: &[Value],
                         _ptr: *mut c_void| {
                            state.write(rets[0], 0f64);
                        }
                    }
                    hir_lower::CallBackKind::SimParamOpt => {
                        |state: &mut InterpreterState,
                         args: &[Value],
                         rets: &[Value],
                         _ptr: *mut c_void| {
                            let val: Data = state.read(args[1]);
                            state.write(rets[0], val);
                        }
                    }
                    hir_lower::CallBackKind::SimParamStr => {
                        ptr = val as *mut Spur as *mut c_void;
                        |state: &mut InterpreterState,
                         _args: &[Value],
                         rets: &[Value],
                         ptr: *mut c_void| {
                            let empty_str = unsafe { *(ptr as *mut Spur) };
                            state.write(rets[0], empty_str);
                        }
                    }

                    hir_lower::CallBackKind::ParamInfo(_, _)
                    | hir_lower::CallBackKind::CollapseHint(_, _) => {
                        |_state: &mut InterpreterState,
                         _args: &[Value],
                         _rets: &[Value],
                         _ptr: *mut c_void| {}
                    }
                    hir_lower::CallBackKind::Print { .. } => {
                        |_state: &mut InterpreterState,
                         _args: &[Value],
                         _rets: &[Value],
                         _ptr: *mut c_void| {}
                    }
                    hir_lower::CallBackKind::BoundStep => {
                        |_state: &mut InterpreterState,
                         _args: &[Value],
                         _rets: &[Value],
                         _ptr: *mut c_void| {}
                    }
                };

                (res, ptr)
            })
            .collect();

        let mut params: TiVec<_, _> = self
            .eval_intern
            .params
            .raw
            .iter()
            .map(|(kind, _)| match kind {
                ParamKind::Param(param) => params[&*db.param_data(*param).name],
                ParamKind::Voltage { hi, lo } => (node_voltages[&*db.node_data(*hi).name]
                    - lo.map_or(0f64, |lo| node_voltages[&*db.node_data(lo).name]))
                .into(),
                ParamKind::Temperature => temp.into(),
                ParamKind::ParamGiven { .. } | ParamKind::PortConnected { .. } => true.into(),
                ParamKind::ParamSysFun(param) => param.default_value().into(),
                ParamKind::Current(_) | ParamKind::HiddenState(_) => Data::UNDEF,
                ParamKind::Abstime | ParamKind::ImplicitUnkown(_) => 0f64.into(),
                ParamKind::EnableIntegration => false.into(),
            })
            .collect();

        let instance_init = self.interpret_func(&self.init_inst_func, params.clone(), &dummy_calls);

        let off = params.len();
        params.extend((0..self.init_inst_cache_slots.len()).map(|_| Data::UNDEF));
        for (&val, &pos) in &self.init_inst_cache_vals {
            if !instance_init.read::<Data>(val).is_undef() {
                params.raw[usize::from(pos) + off] = instance_init.read(val);
            }
        }

        self.interpret_func(&self.eval_func, params, &dummy_calls)
    }

    fn interpret_func(
        &self,
        func: &Function,
        params: TiVec<Param, Data>,
        calls: &TiSlice<FuncRef, (Func, *mut c_void)>,
    ) -> InterpreterState {
        let mut interpreter = Interpreter::new(func, calls, &params);
        interpreter.run();
        interpreter.state
    }
}
