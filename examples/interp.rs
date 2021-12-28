use std::rc::Rc;

use criterion::{
    black_box, criterion_group, criterion_main, BatchSize, Criterion,
};
#[cfg(feature = "pprof")]
use pprof::criterion::{Output, PProfProfiler};

use origis_lang::interpreter::Interpreter;

fn init() -> Interpreter {
    let ast = origis_lang::parser::parse_module(
        r#"
    fn test_sum(a, b: int) int { a + b }
    fn test_recursion_fib(n: int) int { if n <= 1 { 1 } else { test_recursion_fib(n - 1) + test_recursion_fib(n - 2) } }
    "#,
    );
    let mut interp = origis_lang::interpreter::Interpreter::new();
    interp.builtin_std().unwrap();
    interp.add_module("main", ast).unwrap();
    interp
}

fn sum(interp: Rc<Interpreter>) {
    let ret = interp
        .call_fn(
            "main",
            "test_sum",
            origis_lang::fn_args! {
                "a" => 1,
                "b" => 2
            },
        )
        .unwrap();
    assert_eq!(black_box(ret.expect_int().unwrap()), 3);
}

fn recursion(interp: Rc<Interpreter>) {
    let ret = interp
        .call_fn(
            "main",
            "test_recursion_fib",
            origis_lang::fn_args! {
                "n" => 20
            },
        )
        .unwrap();
    //assert_eq!(black_box(ret.borrow().expect_int().unwrap()), 40_320);
}

fn criterion_benchmark(c: &mut Criterion) {
    let interp = Rc::new(init());
    c.bench_function("sum", |b| {
        b.iter_batched(|| interp.clone(), sum, BatchSize::SmallInput)
    });
    c.bench_function("recursion", |b| {
        b.iter_batched(
            || interp.clone(),
            recursion,
            BatchSize::SmallInput,
        )
    });
}

criterion_group!(
    name = benches;
    config = if cfg!(feature = "prof") {
        Criterion::default()
            .with_profiler(PProfProfiler::new(100, Output::Flamegraph(Some(pprof::flamegraph::Options::default()))))
    } else {
        Criterion::default()
    };
    targets = criterion_benchmark
);
criterion_main!(benches);
