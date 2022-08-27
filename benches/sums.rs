use criterion::{black_box, criterion_group, criterion_main, Criterion};
use raclette::{
    raclette::ExprParser,
    span::{SpannedValue, SpanningExt, UNKNOWN_SPAN},
};

mod perf;

fn ast(input: &str, parser: &ExprParser) -> SpannedValue<raclette::ast::Expr> {
    parser
        .parse(&input.into(), raclette::ast::lexer(input))
        .unwrap()
        .spanned(&*UNKNOWN_SPAN)
}

fn sums(c: &mut Criterion) {
    let parser = ExprParser::new();

    let input = "[:100000] |> %+";
    let parsed = ast(input, &parser);

    let mut interpreter = raclette::interpreter::Interpreter::new();
    c.bench_function(input, |b| {
        b.iter(|| interpreter.run_expr(black_box(parsed.clone())))
    });

    let input = "[:100000] |> list |> %+";
    let parsed = ast(input, &parser);

    let mut interpreter = raclette::interpreter::Interpreter::new();
    c.bench_function(input, |b| {
        b.iter(|| interpreter.run_expr(black_box(parsed.clone())))
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().with_profiler(perf::FlamegraphFrofiler::new(100));
    targets = sums
}
criterion_main!(benches);
