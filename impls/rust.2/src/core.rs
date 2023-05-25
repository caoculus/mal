use std::rc::Rc;

use itertools::Itertools;

use crate::{types::{MalFn, MalType, MalError}, printer::{pr_str, PrintMode}};

// returns the base environment
pub fn ns() -> Vec<(&'static str, MalFn)> {
    vec![
        ("+", bin_op(|a, b| a + b, MalType::Number)),
        ("-", bin_op(|a, b| a - b, MalType::Number)),
        ("*", bin_op(|a, b| a * b, MalType::Number)),
        ("/", bin_op(|a, b| a / b, MalType::Number)),
        ("list", list()),
        ("list?", is_list()),
        ("empty?", empty()),
        ("count", count()),
        ("=", equal()),
        ("<", bin_op(|a, b| a < b, MalType::Bool)),
        ("<=", bin_op(|a, b| a <= b, MalType::Bool)),
        (">", bin_op(|a, b| a > b, MalType::Bool)),
        (">=", bin_op(|a, b| a >= b, MalType::Bool)),
        ("pr-str", print_pr_str()),
        ("str", print_str()),
        ("prn", print_prn()),
        ("println", print_println()),
    ]
}

fn bin_op<T>(f: impl Fn(i64, i64) -> T + 'static, wrap: impl Fn(T) -> MalType + 'static) -> MalFn {
    Rc::new(move |args| {
        let &[MalType::Number(a), MalType::Number(b)] = args else {
            return Err(MalError::WrongArgs);
        };

        Ok(wrap(f(a, b)))
    })
}

fn list() -> MalFn {
    Rc::new(move |args| Ok(MalType::List(Rc::new(args.to_vec()))))
}

fn is_list() -> MalFn {
    Rc::new(move |args| {
        let [first] = args else {
            return Err(MalError::WrongArgs);
        };

        Ok(MalType::Bool(matches!(first, MalType::List(..))))
    })
}

fn empty() -> MalFn {
    Rc::new(move |args| {
        let [MalType::List(list)] = args else {
            return Err(MalError::WrongArgs);
        };

        Ok(MalType::Bool(list.is_empty()))
    })
}

fn count() -> MalFn {
    Rc::new(move |args| {
        let [head] = args else {
            return Err(MalError::WrongArgs);
        };

        let res = match head {
            MalType::Nil => 0,
            MalType::List(list) => list.len(),
            // MalType::Vector(_) => todo!(),
            // MalType::Hashmap(_) => todo!(),
            _ => return Err(MalError::WrongArgs),
        };

        Ok(MalType::Number(res as i64))
    })
}

fn equal() -> MalFn {
    Rc::new(move |args| {
        let [first, second] = args else { 
            return Err(MalError::WrongArgs); 
        };

        Ok(MalType::Bool(first == second))
    })
}

fn print_pr_str() -> MalFn {
    Rc::new(move |args| {
        Ok(MalType::String(args.iter().map(|t| pr_str(t, PrintMode::Readable)).join(" ").into()))
    })
}

fn print_str() -> MalFn {
    Rc::new(move |args| {
        Ok(MalType::String(args.iter().map(|t| pr_str(t, PrintMode::Verbatim)).collect::<String>().into()))
    })
}

fn print_prn() -> MalFn {
    Rc::new(move |args| {
        let s = args.iter().map(|t| pr_str(t, PrintMode::Readable)).join(" ");
        println!("{}", s);
        Ok(MalType::Nil)
    })
}

fn print_println() -> MalFn {
    Rc::new(move |args| {
        let s = args.iter().map(|t| pr_str(t, PrintMode::Readable)).join(" ");
        println!("{}", s);
        Ok(MalType::Nil)
    })
}
