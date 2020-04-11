use std::path::Path;

use bumpalo::Bump;
use criterion::{criterion_group, criterion_main, Criterion};

use VARF::mk_ast;

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("hicum", |b| b.iter_with_large_drop(|| hicum()));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
pub fn hicum() -> Result<(), ()> {
    let source_map_allocator = Bump::new();
    mk_ast!(ast);
    let source_map = ast
        .parse_from_and_print_errors(Path::new("tests/hl2.va"), &source_map_allocator, true)
        .ok_or(())?;
    let mir = ast
        .lower_and_print_errors(source_map, true)
        .ok_or(())?
        .lower_and_print_errors(source_map, true)
        .ok_or(())?;
    Ok(())
}
