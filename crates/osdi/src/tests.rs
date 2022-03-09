use std::ffi::c_void;
use std::path::Path;

use hir_def::db::HirDefDB;
use hir_lower::ParamKind;
use lasso::{Rodeo, Spur};
use mir::Value;
use mir_interpret::{Data, Interpreter, InterpreterState};
use mir_llvm::LLVMBackend;
use quote::{format_ident, quote};
use sourcegen::{
    add_preamble, collect_integration_tests, ensure_file_contents, project_root, reformat,
};
use stdx::format_to;
use stdx::iter::zip;
use target::spec::Target;
use typed_index_collections::TiVec;

use crate::compilation_db::CompilationDB;
use crate::matrix::JacobianMatrix;
use crate::middle::AnalogBlockMir;

mod integration;
mod stamps;

#[test]
pub fn generate_integration_tests() {
    let tests = collect_integration_tests();
    let file = project_root().join("crates/osdi/src/tests/integration.rs");
    let test_impl = tests.into_iter().filter_map(|(test_name, _)| {
        // skip this test until we implement switch branches
        // TODO switch branches
        if test_name == "ASMHEMT"{
            return None
        }

        let test_case = format_ident!("{}", test_name.to_lowercase());
        let root_file_name = format!("{}.va", test_name.to_lowercase());

        let res = quote! {
            #[test]
            fn #test_case(){
                if skip_slow_tests(){
                    return
                }

                let root_file = project_root().join("integration_tests").join(#test_name).join(#root_file_name);
                super::full_compile(&root_file);
            }
        };

        Some(res)
    });

    let header = "
        use sourcegen::{skip_slow_tests, project_root};
    ";

    let file_string = quote!(
        #(#test_impl)*
    );
    let file_string = format!("{}\n{}", header, file_string);
    let file_string = add_preamble("generate_integration_tests", reformat(file_string));
    ensure_file_contents(&file, &file_string);
}

fn full_compile(path: &Path) {
    let db = CompilationDB::new(Path::new(path)).unwrap();
    let module = db.find_module(db.root_file);
    let (mir, literals) = AnalogBlockMir::new(&db, module);
    let target = Target::host_target().unwrap();
    let back = LLVMBackend::new(&[], &target, llvm::OptLevel::None);
    mir.to_bin(&db, &path.to_string_lossy(), &literals, &back);
}

fn compile_to_mir(path: &Path) -> (CompilationDB, AnalogBlockMir, Rodeo) {
    let db = CompilationDB::new(Path::new(path)).unwrap();
    let module = db.find_module(db.root_file);
    let (mir, literals) = AnalogBlockMir::new(&db, module);
    (db, mir, literals)
}

impl JacobianMatrix {
    fn print(&self, db: &dyn HirDefDB) -> String {
        let mut res = String::new();
        for (entry, val) in zip(&self.entries.raw, &self.values) {
            format_to!(
                res,
                "({}, {}) = {}\n",
                db.node_data(entry.row).name,
                db.node_data(entry.col).name,
                val
            )
        }

        res
    }

    fn print_with_nums(&self, db: &dyn HirDefDB, vals: &InterpreterState) {
        for (entry, val) in zip(&self.entries.raw, &self.values) {
            let num: f64 = vals.read(*val);
            println!(
                "({}, {}) = {} = {}",
                db.node_data(entry.row).name,
                db.node_data(entry.col).name,
                val,
                num
            )
        }
    }
}

impl AnalogBlockMir {
    pub fn interpret(
        &self,
        db: &CompilationDB,
        literals: &mut Rodeo,
        params: &ahash::AHashMap<&str, Data>,
        node_voltages: &ahash::AHashMap<&str, f64>,
        temp: f64,
    ) -> InterpreterState {
        let empty_str = literals.get_or_intern_static("");
        let val = Box::leak(Box::new(empty_str));

        let dummy_call: TiVec<_, _> = self
            .intern
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
                };

                (res, ptr)
            })
            .collect();
        let params: TiVec<_, _> = self
            .intern
            .params
            .raw
            .keys()
            .map(|kind| match kind {
                ParamKind::Param(param) => params[&*db.param_data(*param).name],
                ParamKind::Voltage { hi, lo } => (node_voltages[&*db.node_data(*hi).name]
                    - lo.map_or(0f64, |lo| node_voltages[&*db.node_data(lo).name]))
                .into(),
                ParamKind::Temperature => temp.into(),
                ParamKind::ParamGiven { .. } | ParamKind::PortConnected { .. } => true.into(),
                ParamKind::ParamSysFun(param) => param.default_value().into(),
                ParamKind::Current(_) | ParamKind::HiddenState(_) => Data::UNDEF,
            })
            .collect();
        let mut interpreter = Interpreter::new(&self.func, dummy_call.as_slice(), &params);
        interpreter.run();
        interpreter.state
    }
}
