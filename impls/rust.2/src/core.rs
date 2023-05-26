use crate::args;
use std::{cell::RefCell, iter, rc::Rc};

use itertools::Itertools;

use crate::{
    env::Env,
    printer::{pr_str, PrintMode},
    reader::read_str,
    types::{MalClosure, MalError, MalFn, MalType},
};

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
        ("prn", prn()),
        ("println", print_ln()),
        ("read-string", read_string()),
        ("slurp", slurp()),
        ("atom", atom()),
        ("atom?", is_atom()),
        ("deref", deref()),
        ("reset!", reset()),
        ("swap!", swap()),
    ]
}

fn bin_op<T>(f: impl Fn(i64, i64) -> T + 'static, wrap: impl Fn(T) -> MalType + 'static) -> MalFn {
    Rc::new(move |args| {
        args!(&[MalType::Number(a), MalType::Number(b)] = args);

        Ok(wrap(f(a, b)))
    })
}

fn list() -> MalFn {
    Rc::new(move |args| Ok(MalType::List(Rc::new(args.to_vec()))))
}

fn is_list() -> MalFn {
    Rc::new(move |args| {
        args!([first] = args);

        Ok(MalType::Bool(matches!(first, MalType::List(..))))
    })
}

fn empty() -> MalFn {
    Rc::new(move |args| {
        args!([head] = args);

        let res = match head {
            MalType::List(list) | MalType::Vector(list) => list.is_empty(),
            MalType::Hashmap(map) => map.is_empty(),
            _ => return Err(MalError::WrongArgs),
        };

        Ok(MalType::Bool(res))
    })
}

fn count() -> MalFn {
    Rc::new(move |args| {
        args!([head] = args);

        let res = match head {
            MalType::Nil => 0,
            MalType::List(list) | MalType::Vector(list) => list.len(),
            MalType::Hashmap(map) => map.len(),
            _ => return Err(MalError::WrongArgs),
        };

        Ok(MalType::Number(res as i64))
    })
}

fn equal() -> MalFn {
    Rc::new(move |args| {
        args!([first, second] = args);

        Ok(MalType::Bool(first == second))
    })
}

fn print_pr_str() -> MalFn {
    Rc::new(move |args| {
        Ok(MalType::String(
            args.iter()
                .map(|t| pr_str(t, PrintMode::Readable))
                .join(" ")
                .into(),
        ))
    })
}

fn print_str() -> MalFn {
    Rc::new(move |args| {
        Ok(MalType::String(
            args.iter()
                .map(|t| pr_str(t, PrintMode::Verbatim))
                .collect::<String>()
                .into(),
        ))
    })
}

fn prn() -> MalFn {
    Rc::new(move |args| {
        let s = args
            .iter()
            .map(|t| pr_str(t, PrintMode::Readable))
            .join(" ");
        println!("{}", s);
        Ok(MalType::Nil)
    })
}

fn print_ln() -> MalFn {
    Rc::new(move |args| {
        let s = args
            .iter()
            .map(|t| pr_str(t, PrintMode::Verbatim))
            .join(" ");
        println!("{}", s);
        Ok(MalType::Nil)
    })
}

fn read_string() -> MalFn {
    Rc::new(move |args| {
        args!([MalType::String(s)] = args);

        read_str(s)
    })
}

fn slurp() -> MalFn {
    Rc::new(move |args| {
        args!([MalType::String(path)] = args);

        std::fs::read_to_string(path.as_ref())
            .map(|s| MalType::String(s.into()))
            .map_err(MalError::IOError)
    })
}

fn atom() -> MalFn {
    Rc::new(move |args| {
        args!([item] = args);

        Ok(MalType::Atom(Rc::new(RefCell::new(item.clone()))))
    })
}

fn is_atom() -> MalFn {
    Rc::new(move |args| {
        args!([item] = args);

        Ok(MalType::Bool(matches!(item, MalType::Atom(..))))
    })
}

fn deref() -> MalFn {
    Rc::new(move |args| {
        args!([MalType::Atom(item)] = args);

        Ok(item.borrow().clone())
    })
}

fn reset() -> MalFn {
    Rc::new(move |args| {
        args!([MalType::Atom(old), new] = args);

        *old.borrow_mut() = new.clone();

        Ok(new.clone())
    })
}

fn swap() -> MalFn {
    Rc::new(move |args| {
        args!([MalType::Atom(atom), f @ (MalType::Fn(..) | MalType::Closure(..)), tail @ ..] = args);

        // rebind args
        let args: Vec<_> = iter::once(atom.borrow().clone())
            .chain(tail.iter().cloned())
            .collect();

        let res = match f {
            MalType::Fn(f) => f(&args)?,
            MalType::Closure(closure) => {
                let MalClosure {
                    eval,
                    params,
                    outer,
                    body,
                } = closure.as_ref();

                let binds = params.bind(&args)?;
                let env = Env::new(Some((*outer).clone()), binds);

                eval(body.clone(), &env)?
            }
            _ => unreachable!(),
        };

        *atom.borrow_mut() = res.clone();
        Ok(res)
    })
}
