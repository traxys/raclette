use raclette::{
    ast::lexer,
    interpreter::{value::HashableValue, Interpreter},
    raclette::ExprParser,
    span::{Span, SpanningExt},
    Value,
};

fn assert_num(value: f64, value_def: &str) {
    let parser = ExprParser::new();

    let expr = parser
        .parse(&value_def.into(), lexer(value_def))
        .expect("parser");

    let mut interpreter = Interpreter::new();
    let result = interpreter
        .run_expr(expr.spanned(&Span {
            source: value_def.into(),
            start: 0,
            end: value_def.len(),
            value: (),
        }))
        .expect("interpreter");

    let res = result.borrow();

    match **res {
        Value::Hashable(HashableValue::Number(n)) => {
            assert_eq!(n as f64, value);
        }
        Value::Float(f) => {
            assert_eq!(f, value);
        }
        _ => panic!("Invalid value type"),
    }
}

macro_rules! num_expr_test {
    ($({$name:ident; $($tt:tt)*}),* $(,)?) => {
        $(
        #[test]
        fn $name() {
            let val = $($tt)*;
            let s = stringify!($($tt)*);

            assert_num(val, s);
        }
        )*
    };
}

num_expr_test! {
    {div; 4. / 3. / 2.},
    {mul; 2. * 3. * 4.},
    {add_mul; 1. + 1. / 2.},
}
