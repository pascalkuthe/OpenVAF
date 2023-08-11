use std::ffi::c_void;
use std::ops::Index;
use std::path::Path;

use ahash::AHashMap;
use hir::diagnostics::ConsoleSink;
use hir::CompilationDB;
use hir_lower::{CallBackKind, ParamKind, PlaceKind};
use lasso::{Rodeo, Spur};
use mir::{FuncRef, Function, Param, Value};
use mir_interpret::{Data, Func, Interpreter, InterpreterState};
use paths::AbsPathBuf;
use typed_index_collections::{TiSlice, TiVec};
use typed_indexmap::TiMap;

use crate::matrix::{JacobianMatrix, MatrixEntry, MatrixEntryId};
use crate::middle::EvalMir;
use crate::{collect_modules, ModuleInfo, SimUnknown};

mod stamps;

impl EvalMir {
    fn opvars(
        &self,
        db: &CompilationDB,
        module: &ModuleInfo,
        state: &InterpreterState,
    ) -> AHashMap<String, f64> {
        module
            .op_vars
            .keys()
            .map(|var| {
                let name = var.name(db).to_string();
                let val = self.eval_intern.outputs[&PlaceKind::Var(*var)].unwrap();
                (name, state.read(val))
            })
            .collect()
    }
}

fn compile_to_mir(path: &Path) -> (CompilationDB, ModuleInfo, EvalMir, Rodeo) {
    let path = AbsPathBuf::assert(path.canonicalize().unwrap());
    let db = CompilationDB::new_fs(path, &[], &[], &[]).unwrap();
    let mut modules = collect_modules(&db, false, &mut ConsoleSink::new(&db)).unwrap();
    let module = modules.pop().unwrap();
    let mut literals = Rodeo::new();
    let mir = EvalMir::new(&db, &module, &mut literals);
    (db, module, mir, literals)
}

#[derive(Default)]
struct MatrixEntries(AHashMap<String, AHashMap<String, f64>>);

impl MatrixEntries {
    pub fn new(
        src: &TiMap<MatrixEntryId, MatrixEntry, Value>,
        db: &CompilationDB,
        vals: &InterpreterState,
    ) -> MatrixEntries {
        let mut res = MatrixEntries::default();
        for (entry, val) in src.iter() {
            if let (SimUnknown::KirchoffLaw(row), SimUnknown::KirchoffLaw(col)) =
                (entry.row, entry.col)
            {
                let row = row.name(db).to_string();
                let col = col.name(db).to_string();
                res.0.entry(row).or_default().insert(col, vals.read(*val));
            }
        }

        res
    }
}

impl<'a> Index<(&'a str, &'a str)> for MatrixEntries {
    type Output = f64;

    fn index(&self, index: (&'a str, &'a str)) -> &Self::Output {
        &self.0[index.0][index.1]
    }
}

impl JacobianMatrix {
    fn resistive_stamps(&self, db: &CompilationDB, vals: &InterpreterState) -> MatrixEntries {
        MatrixEntries::new(&self.resistive, db, vals)
    }

    //     fn reactive_stamps(&self, db: &CompilationDB, vals: &InterpreterState) -> MatrixEntries {
    //         MatrixEntries::new(&self.reactive, db, vals)
    //     }

    pub fn print_with_nums(&self, db: &CompilationDB, vals: &InterpreterState) {
        for (entry, val) in &self.resistive.raw {
            let num: f64 = vals.read(*val);

            if let (SimUnknown::KirchoffLaw(row), SimUnknown::KirchoffLaw(col)) =
                (entry.row, entry.col)
            {
                println!("({}, {}) = {} = {}", row.name(db), col.name(db), val, num)
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
                    CallBackKind::Derivative(_)
                    | CallBackKind::NodeDerivative(_)
                    | CallBackKind::SimParam => {
                        |state: &mut InterpreterState,
                         _args: &[Value],
                         rets: &[Value],
                         _ptr: *mut c_void| {
                            state.write(rets[0], 0f64);
                        }
                    }
                    CallBackKind::SimParamOpt => {
                        |state: &mut InterpreterState,
                         args: &[Value],
                         rets: &[Value],
                         _ptr: *mut c_void| {
                            let val: Data = state.read(args[1]);
                            state.write(rets[0], val);
                        }
                    }
                    CallBackKind::SimParamStr => {
                        ptr = val as *mut Spur as *mut c_void;
                        |state: &mut InterpreterState,
                         _args: &[Value],
                         rets: &[Value],
                         ptr: *mut c_void| {
                            let empty_str = unsafe { *(ptr as *mut Spur) };
                            state.write(rets[0], empty_str);
                        }
                    }

                    CallBackKind::ParamInfo(_, _)
                    | CallBackKind::CollapseHint(_, _)
                    | CallBackKind::Print { .. }
                    | CallBackKind::BoundStep
                    | CallBackKind::LimDiscontinuity => {
                        |_state: &mut InterpreterState,
                         _args: &[Value],
                         _rets: &[Value],
                         _ptr: *mut c_void| {}
                    }
                    CallBackKind::BuiltinLimit { .. } => {
                        |state: &mut InterpreterState,
                         args: &[Value],
                         rets: &[Value],
                         _ptr: *mut c_void| {
                            let val: Data = state.read(args[0]);
                            state.write(rets[0], val)
                        }
                    }
                    CallBackKind::StoreLimit(_) | CallBackKind::StoreDelayTime(_) => {
                        |state: &mut InterpreterState,
                         args: &[Value],
                         rets: &[Value],
                         _ptr: *mut c_void| {
                            let val: Data = state.read(args[0]);
                            state.write(rets[0], val)
                        }
                    }
                    CallBackKind::Analysis => {
                        |state: &mut InterpreterState,
                         _args: &[Value],
                         rets: &[Value],
                         _ptr: *mut c_void| { state.write(rets[0], true) }
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
                ParamKind::Param(param) => params[&*param.name(db)],
                ParamKind::Voltage { hi, lo } => (node_voltages[&*hi.name(db)]
                    - lo.map_or(0f64, |lo| node_voltages[&*lo.name(db)]))
                .into(),
                ParamKind::Temperature => temp.into(),
                ParamKind::ParamGiven { .. } | ParamKind::PortConnected { .. } => true.into(),
                ParamKind::ParamSysFun(param) => param.default_value().into(),
                ParamKind::Current(_) | ParamKind::HiddenState(_) => Data::UNDEF,
                ParamKind::Abstime
                | ParamKind::ImplicitUnknown(_)
                | ParamKind::PrevState(_)
                | ParamKind::NewState(_) => 0f64.into(),
                ParamKind::EnableLim | ParamKind::EnableIntegration => false.into(),
            })
            .collect();

        let instance_init = self.interpret_func(&self.init_inst_func, params.clone(), &dummy_calls);

        let off = params.len();
        params.extend((0..self.cache_slots.len()).map(|_| Data::UNDEF));
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
