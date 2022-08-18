use criterion::{black_box, criterion_group, criterion_main, Criterion};
use raclette::span::{SpanningExt, UNKNOWN_SPAN};

mod perf;

fn range_sum(c: &mut Criterion) {
    let input = "[:100000] |> %+";
    let parser = raclette::raclette::ExprParser::new();
    let parsed = parser
        .parse(raclette::ast::lexer(input))
        .unwrap()
        .spanned(UNKNOWN_SPAN);

    let mut interpreter = raclette::interpreter::Interpreter::new();
    c.bench_function(input, |b| {
        b.iter(|| interpreter.run_expr(black_box(parsed.clone())))
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().with_profiler(perf::FlamegraphFrofiler::new(100));
    targets = range_sum
}
criterion_main!(benches);
