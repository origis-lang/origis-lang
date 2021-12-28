use criterion::{
    black_box, criterion_group, criterion_main, Criterion,
};

fn parse_fn_def() {
    const SOURCE: &str = r#"fn sum(a, b: int) int { a + b }"#;
    let ast = origis_lang::parser::parse_module(SOURCE);
    let _ = black_box(ast);
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("parse_fn_def", |b| b.iter(parse_fn_def));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
