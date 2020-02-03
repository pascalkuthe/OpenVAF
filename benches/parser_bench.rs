use std::path::Path;

use bumpalo::Bump;
use criterion::{criterion_group, criterion_main, Criterion};

use VARF::ast_lowering::resolve_and_print;
use VARF::mk_ast;
use VARF::parser;
use VARF::parser::insert_electrical_natures_and_disciplines;

/*fn linear() {
    let source_map_allocator = Bump::new();
    mk_ast!(ast);
    parser::parse_and_print_errors(
        Path::new("tests/linear.va"),
        &source_map_allocator,
        &mut ast,
    );
}*/

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("linear", |b| b.iter_with_large_drop(|| linear()));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
pub fn linear() -> Result<(), ()> {
    let source_map_allocator = Bump::new();
    mk_ast!(ast);
    let (source_map, res) = parser::parse_and_print_errors(
        Path::new("tests/linear.va"),
        &source_map_allocator,
        &mut ast,
    );
    res?;
    insert_electrical_natures_and_disciplines(&mut ast);
    resolve_and_print(ast, source_map)?;
    Ok(())
}
