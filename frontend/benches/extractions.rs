use criterion::{black_box, Criterion};
use criterion::{criterion_group, criterion_main};
use open_vaf::analysis::constant_fold::{ConstantFoldState, IntermediateWritingConstantFold};
use open_vaf::analysis::data_flow::reaching_definitions::ReachingDefinitionsAnalysis;
use open_vaf::analysis::ProgramDependenceGraph;
use open_vaf::diagnostic::UserResult;
use open_vaf::parser::tokenstream::TokenStream;
use open_vaf::preprocessor::{preprocess_user_facing, std_path};
use open_vaf::sourcemap::FileId;
use open_vaf::{Ast, ControlFlowGraph, SourceMap};
use std::fmt::{Debug, Display, Formatter};
use std::sync::Arc;

pub const TEST_EXPANSION_HINT: &'static str =
    "This error occured inside a compiler directive. If you have trouble understanding why/where this error occurs use the 'ExpansionPrinter'";

pub struct PrettyError(Box<dyn Display>);

impl<I: Display + 'static> From<I> for PrettyError {
    fn from(val: I) -> Self {
        Self(Box::new(val))
    }
}

impl Debug for PrettyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub fn preprocess_test(
    sm: SourceMap,
    main_file: FileId,
) -> UserResult<(TokenStream, Arc<SourceMap>)> {
    preprocess_user_facing(
        sm,
        TEST_EXPANSION_HINT,
        main_file,
        std_path(
            "tests/std/constants.vams".into(),
            "tests/std/disciplines.vams".into(),
        ),
    )
}

fn bsimsoi_criterion_benchmark(c: &mut Criterion) {
    let (sm, main_file) = SourceMap::new_with_mainfile("tests/integration/BSIMSOI/bsimsoi.va")
        .expect("Failed to open file");
    c.bench_function("bsim", |b| {
        b.iter(|| bsimsoi(black_box(sm.clone()), main_file))
    });
}

fn hl2_criterion_benchmark(c: &mut Criterion) {
    let (sm, main_file) = SourceMap::new_with_mainfile("tests/integration/HICUML2/hicuml2.va")
        .expect("Failed to open file");
    c.bench_function("hl2", |b| b.iter(|| hl2(black_box(sm.clone()), main_file)));
}

criterion_group!(
    benches,
    bsimsoi_criterion_benchmark,
    hl2_criterion_benchmark
);
criterion_main!(benches);

#[inline]
pub fn bsimsoi(sm: SourceMap, main_file: FileId) -> Result<(), PrettyError> {
    fern::Dispatch::new()
        .format(|out, message, _record| out.finish(*message))
        .level(log::LevelFilter::Warn)
        .chain(std::io::stderr())
        .apply();

    let (ts, sm) = preprocess_test(sm, main_file)?;
    let hir = Ast::parse_from_token_stream_user_facing(ts, &sm, TEST_EXPANSION_HINT)?
        .lower_user_facing(&sm, TEST_EXPANSION_HINT)?;
    let mut mir = hir.lower_user_facing(&sm, TEST_EXPANSION_HINT)?;

    for module in mir.modules.indices() {
        let mut cfg: ControlFlowGraph = mir[module].contents.analog_cfg.clone();

        cfg.calculate_all_registered_derivatives(&mut mir);

        let reaching_analysis = ReachingDefinitionsAnalysis::new(&mir, &cfg);

        let mut udg = reaching_analysis.run(&mut cfg);
        #[cfg(feature = "graph_debug")]
        {
            let mut file = File::create("cfg.dot").unwrap();
            cfg.render_to(&mut file);
            let mut file = File::create("data_dependencies.dot").unwrap();
            cfg.render_to(&mut file);
        }

        let ipdom = cfg.post_dominators();

        #[cfg(feature = "graph_debug")]
        {
            let mut file = File::create("ipdom.dot").unwrap();
            cfg.render_post_dominators(&mut file, &ipdom);
        }

        let control_dependencies = cfg.control_dependence_graph_from_ipdom(&ipdom);

        #[cfg(feature = "graph_debug")]
        {
            let mut file = File::create("control_dependence.dot").unwrap();
            cfg.render_control_dependence_to(&mut file, &control_dependencies);
        }

        cfg.constant_fold(
            &mut IntermediateWritingConstantFold(&mut mir),
            &mut udg,
            &mut ConstantFoldState::default(),
        );

        #[cfg(feature = "graph_debug")]
        {
            let mut file = File::create("cfg_constant_fold.dot").unwrap();
            cfg.render_to(&mut file);
        }

        let pdg = ProgramDependenceGraph {
            data_dependencies: udg,
            control_dependencies: control_dependencies,
        };

        // forward transfer current as a pretty complex slice
        let mut itf_id = None;
        for (id, var) in mir.variables.iter_enumerated() {
            if var.contents.ident.name.as_str() == "IC" {
                itf_id = Some(id)
            }
        }

        cfg.backward_variable_slice(itf_id.unwrap(), &pdg);

        cfg.simplify();

        let test = black_box(cfg);

        #[cfg(feature = "graph_debug")]
        {
            let mut file = File::create("cfg_sliced.dot").unwrap();
            cfg.render_to(&mut file);
        }
    }

    Ok(())
}

#[inline]
pub fn hl2(sm: SourceMap, main_file: FileId) -> Result<(), PrettyError> {
    fern::Dispatch::new()
        .format(|out, message, _record| out.finish(*message))
        .level(log::LevelFilter::Warn)
        .chain(std::io::stderr())
        .apply();

    let (ts, sm) = preprocess_test(sm, main_file)?;
    let hir = Ast::parse_from_token_stream_user_facing(ts, &sm, TEST_EXPANSION_HINT)?
        .lower_user_facing(&sm, TEST_EXPANSION_HINT)?;
    let mut mir = hir.lower_user_facing(&sm, TEST_EXPANSION_HINT)?;

    for module in mir.modules.indices() {
        let mut cfg: ControlFlowGraph = mir[module].contents.analog_cfg.clone();

        cfg.calculate_all_registered_derivatives(&mut mir);

        let reaching_analysis = ReachingDefinitionsAnalysis::new(&mir, &cfg);

        let mut udg = reaching_analysis.run(&mut cfg);
        #[cfg(feature = "graph_debug")]
        {
            let mut file = File::create("cfg.dot").unwrap();
            cfg.render_to(&mut file);
            let mut file = File::create("data_dependencies.dot").unwrap();
            cfg.render_to(&mut file);
        }

        let ipdom = cfg.post_dominators();

        #[cfg(feature = "graph_debug")]
        {
            let mut file = File::create("ipdom.dot").unwrap();
            cfg.render_post_dominators(&mut file, &ipdom);
        }

        let control_dependencies = cfg.control_dependence_graph_from_ipdom(&ipdom);

        #[cfg(feature = "graph_debug")]
        {
            let mut file = File::create("control_dependence.dot").unwrap();
            cfg.render_control_dependence_to(&mut file, &control_dependencies);
        }

        cfg.constant_fold(
            &mut IntermediateWritingConstantFold(&mut mir),
            &mut udg,
            &mut ConstantFoldState::default(),
        );

        #[cfg(feature = "graph_debug")]
        {
            let mut file = File::create("cfg_constant_fold.dot").unwrap();
            cfg.render_to(&mut file);
        }

        let pdg = ProgramDependenceGraph {
            data_dependencies: udg,
            control_dependencies: control_dependencies,
        };

        // forward transfer current as a pretty complex slice
        let mut itf_id = None;
        for (id, var) in mir.variables.iter_enumerated() {
            if var.contents.ident.name.as_str() == "itf" {
                itf_id = Some(id)
            }
        }

        cfg.backward_variable_slice(itf_id.unwrap(), &pdg);

        cfg.simplify();

        let test = black_box(cfg);

        #[cfg(feature = "graph_debug")]
        {
            let mut file = File::create("cfg_sliced.dot").unwrap();
            cfg.render_to(&mut file);
        }
    }

    Ok(())
}
