use std::path::Path;

use bumpalo::Bump;
use criterion::{criterion_group, criterion_main, Criterion};

use VARF::ast_lowering::fold_ast_to_hir_and_print_errors;
use VARF::parser;
use VARF::parser::insert_electrical_natures_and_disciplines;
use VARF::{fold_hir_to_mir, mk_ast};

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("linear", |b| b.iter_with_large_drop(|| linear()));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
pub fn linear() -> Result<(), ()> {
    let source_map_allocator = Bump::new();
    mk_ast!(ast);
    let source_map = parser::parse_and_print_errors(
        Path::new("tests/diode.va"),
        &source_map_allocator,
        &mut ast,
        true,
    )?;
    insert_electrical_natures_and_disciplines(&mut ast);
    let hir = fold_ast_to_hir_and_print_errors(ast, source_map, true)?;
    fold_hir_to_mir(hir, source_map, true)?;
    Ok(())
}
