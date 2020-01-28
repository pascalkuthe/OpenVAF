use std::path::Path;

use bumpalo::Bump;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

use VARF::mk_ast;
use VARF::parser;

fn linear() {
    let source_map_allocator = Bump::new();
    mk_ast!(ast);
    parser::parse_and_print_errors(
        Path::new("tests/linear.va"),
        &source_map_allocator,
        &mut ast,
    );
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("linear", |b| b.iter_with_large_drop(|| linear()));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
