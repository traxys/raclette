#![no_main]
use libfuzzer_sys::fuzz_target;
use raclette::{
    ast::Expr,
    interpreter::Interpreter,
    span::{SpanningExt, UNKNOWN_SPAN},
};

fuzz_target!(|data: Expr| {
    let mut interpreter = Interpreter::new();
    let _ = interpreter.run_expr(data.spanned(UNKNOWN_SPAN));
});
