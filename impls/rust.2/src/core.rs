use crate::{args, types::MalResult};
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
    // TODO: if easier, change everything to pure functions?
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
        ("cons", cons()),
        ("concat", concat()),
        ("quasiquote", Rc::new(quasiquote)),
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

fn cons() -> MalFn {
    Rc::new(move |args| {
        args!([elt, MalType::List(list) | MalType::Vector(list)] = args);

        Ok(MalType::List(
            iter::once(elt.clone())
                .chain(list.iter().cloned())
                .collect_vec()
                .into(),
        ))
    })
}

fn concat() -> MalFn {
    Rc::new(move |args| {
        let elts = args
            .iter()
            .map(|elt| match elt {
                MalType::List(list) | MalType::Vector(list) => Ok(list),
                _ => Err(MalError::WrongArgs),
            })
            .collect::<MalResult<Vec<_>>>()?;

        Ok(MalType::List(
            elts.iter()
                .flat_map(|list| list.iter().cloned())
                .collect_vec()
                .into(),
        ))
    })
}

pub fn quasiquote(args: &[MalType]) -> MalResult<MalType> {
    fn go_list(list: &[MalType]) -> MalResult<MalType> {
        list.iter()
            .rev()
            .try_fold(MalType::List(vec![].into()), |acc, elt| {
                if let MalType::List(list) = elt {
                    match &list[..] {
                        [MalType::Symbol(s), tail @ ..] if &s[..] == "splice-unquote" => {
                            args!([snd] = tail);
                            return Ok(MalType::List(
                                vec![MalType::Symbol("concat".into()), snd.clone(), acc].into(),
                            ));
                        }
                        _ => {}
                    }
                }

                Ok(MalType::List(
                    vec![
                        MalType::Symbol("cons".into()),
                        quasiquote(&[elt.clone()])?,
                        acc,
                    ]
                    .into(),
                ))
            })
    }

    args!([ast] = args);

    match ast {
        MalType::List(list) => match &list[..] {
            [MalType::Symbol(s), tail @ ..] if &s[..] == "unquote" => {
                args!([snd] = tail);
                Ok(snd.clone())
            }
            _ => go_list(list),
        },
        MalType::Hashmap(..) | MalType::Symbol(..) => Ok(MalType::List(
            vec![MalType::Symbol("quote".into()), ast.clone()].into(),
        )),
        MalType::Vector(list) => Ok(MalType::List(
            vec![MalType::Symbol("vec".into()), go_list(list)?].into(),
        )),
        _ => Ok(ast.clone()),
    }
}
