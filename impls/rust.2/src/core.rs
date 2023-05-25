use std::rc::Rc;

use crate::{
    eval::EvalError,
    types::{MalFn, MalType},
};

// returns the base environment
pub fn ns() -> Vec<(&'static str, MalFn)> {
    vec![
        ("+", bin_op(|a, b| a + b)),
        ("-", bin_op(|a, b| a - b)),
        ("*", bin_op(|a, b| a * b)),
        ("/", bin_op(|a, b| a / b)),
    ]
}

fn bin_op(f: impl Fn(i64, i64) -> i64 + 'static) -> MalFn {
    Rc::new(move |args| {
        let &[MalType::Number(a), MalType::Number(b)] = args else {
            return Err(EvalError::WrongArgs);
        };

        Ok(MalType::Number(f(a, b)))
    })
}
