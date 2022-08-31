use raclette::{
    ast::lexer,
    interpreter::{value::HashableValue, Interpreter},
    raclette::ExprParser,
    span::{Span, SpanningExt},
    Value,
};

fn assert_toml_eq(a: &str, b: &str) {
    let a: toml::Value = toml::from_str(a).unwrap();
    let b: toml::Value = toml::from_str(b).unwrap();

    assert_eq!(a, b);
}

fn assert_ron_eq(a: &str, b: &str) {
    let a: ron::Value = ron::from_str(a).unwrap();
    let b: ron::Value = ron::from_str(b).unwrap();

    assert_eq!(a, b);
}

fn assert_json_eq(a: &str, b: &str) {
    let a: serde_json::Value = serde_json::from_str(a).unwrap();
    let b: serde_json::Value = serde_json::from_str(b).unwrap();

    assert_eq!(a, b);
}

fn assert_yaml_eq(a: &str, b: &str) {
    let a: serde_yaml::Value = serde_yaml::from_str(a).unwrap();
    let b: serde_yaml::Value = serde_yaml::from_str(b).unwrap();

    assert_eq!(a, b);
}

fn ser(format: &str, literal: &str, output: &str) {
    let parser = ExprParser::new();
    let source = format!(r#"{} |> ser{{fmt = "{}"}}"#, literal, format);
    let source = source.as_str();
    let expr = parser
        .parse(&source.into(), lexer(source))
        .expect("could not parse ser");
    let mut interpreter = Interpreter::new();
    let expr = interpreter
        .run_expr(expr.spanned(&Span {
            source: source.into(),
            start: 0,
            end: source.len(),
            value: (),
        }))
        .expect("interpreter");

    let expr = expr.borrow();
    let expr: &str = match &**expr {
        Value::Hashable(HashableValue::Str(s)) => &*s.0,
        _ => panic!("unexpected type, expected str"),
    };

    match format {
        "json" => assert_json_eq(expr, output),
        "toml" => assert_toml_eq(expr, output),
        "yaml" => assert_yaml_eq(expr, output),
        "ron" => assert_ron_eq(expr, output),
        _ => panic!("format not supported"),
    }
}

macro_rules! ser {
    ($({$name:ident, $format:literal => $input:literal = $output:literal})*) => {
        $(
        #[test]
        fn $name() {
            ser($format, $input, $output)
        }
        )*
    };
}

ser! {
    {json_empty_map, "json" => "{}" = "{}"}
    {json_map,       "json" => "~{a = 1, b = 2}" = r#"{"a": 1, "b": 2}"#}

    {ron_empty_map, "ron" => "{}" = "{}"}
    {ron_map,       "ron" => "~{a = 1, b = 2}" = r#"{"a": 1, "b": 2}"#}

    {yaml_empty_map, "yaml" => "{}" = "{}"}
    {yaml_map,       "yaml" => "~{a = 1, b = 2}" = r#"
        a: 1
        b: 2
    "#}

    {toml_empty_map, "toml" => "{}" = ""}
    {toml_map,       "toml" => "~{a = 1, b = 2}" = r#"
        a = 1
        b = 2
    "#}
}
