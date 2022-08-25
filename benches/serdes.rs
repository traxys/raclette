use criterion::{black_box, criterion_group, criterion_main, Criterion};
use raclette::{
    raclette::ExprParser,
    span::{SpanningExt, UNKNOWN_SPAN},
};

mod perf;

pub static CANADA_JSON: &str = include_str!("data/canada.json");
pub static CANADA_YAML: &str = include_str!("data/canada.yaml");
pub static CITM_JSON: &str = include_str!("data/citm_catalog.json");
pub static CITM_YAML: &str = include_str!("data/citm_catalog.yaml");
pub static CONFIG_TOML: &str = include_str!("data/canada.yaml");

fn des_ast(
    input: &str,
    format: &str,
    parser: &ExprParser,
) -> raclette::span::SpannedValue<raclette::ast::Expr> {
    let des = format!("|> des{{fmt = \"{}\"}} |> zero", format);

    parser
        .parse(
            std::iter::once(Ok((0, raclette::ast::Token::String(input.to_string()), 0)))
                .chain(raclette::ast::lexer(&des)),
        )
        .unwrap()
        .spanned(UNKNOWN_SPAN)
}

fn des(c: &mut Criterion) {
    let parser = ExprParser::new();

    {
        let parsed_json = des_ast(CANADA_JSON, "json", &parser);
        let mut interpreter = raclette::interpreter::Interpreter::new();
        c.bench_function("canada_json", |b| {
            b.iter(|| interpreter.run_expr(black_box(parsed_json.clone())))
        });
    }

    {
        let parsed_yaml = des_ast(CANADA_YAML, "yaml", &parser);
        let mut interpreter = raclette::interpreter::Interpreter::new();
        c.bench_function("canada_yaml", |b| {
            b.iter(|| interpreter.run_expr(black_box(parsed_yaml.clone())))
        });
    }

    {
        let parsed_json = des_ast(CITM_JSON, "json", &parser);
        let mut interpreter = raclette::interpreter::Interpreter::new();
        c.bench_function("citm_json", |b| {
            b.iter(|| interpreter.run_expr(black_box(parsed_json.clone())))
        });
    }

    {
        let parsed_yaml = des_ast(CITM_YAML, "yaml", &parser);
        let mut interpreter = raclette::interpreter::Interpreter::new();
        c.bench_function("citm_yaml", |b| {
            b.iter(|| interpreter.run_expr(black_box(parsed_yaml.clone())))
        });
    }

    {
        let parsed_toml = des_ast(CONFIG_TOML, "toml", &parser);
        let mut interpreter = raclette::interpreter::Interpreter::new();
        c.bench_function("toml", |b| {
            b.iter(|| interpreter.run_expr(black_box(parsed_toml.clone())))
        });
    }
}

criterion_group! {
    name = benches;
    config = Criterion::default().with_profiler(perf::FlamegraphFrofiler::new(100));
    targets = des
}
criterion_main!(benches);
