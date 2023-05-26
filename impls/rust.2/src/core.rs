use crate::{args, types::MalResult};
use std::{cell::RefCell, iter, rc::Rc};

use itertools::Itertools;

use crate::{
    env::Env,
    printer::{pr_str, PrintMode},
    reader::read_str,
    types::{MalClosure, MalError, MalType},
};

type Args<'a> = &'a [MalType];
type MalRet = MalResult<MalType>;
type MalFnPtr = fn(Args) -> MalRet;

macro_rules! bin_op {
    ($op:tt, $ty:expr) => {
        move |args: &[MalType]| {
            args!(&[MalType::Number(a), MalType::Number(b)] = args);

            Ok($ty(a $op b))
        }
    };
}

macro_rules! is {
    ($pat:pat) => {
        move |args: &[MalType]| {
            args!([first] = args);

            Ok(MalType::Bool(matches!(first, $pat)))
        }
    };
}

// returns the base environment
pub const fn ns() -> &'static [(&'static str, MalFnPtr)] {
    &[
        ("+", bin_op!(+, MalType::Number)),
        ("-", bin_op!(-, MalType::Number)),
        ("*", bin_op!(*, MalType::Number)),
        ("/", bin_op!(/, MalType::Number)),
        ("list", list),
        ("list?", is!(MalType::List(..))),
        ("empty?", empty),
        ("count", count),
        ("=", equal),
        ("<", bin_op!(<, MalType::Bool)),
        ("<=", bin_op!(<=, MalType::Bool)),
        (">", bin_op!(>, MalType::Bool)),
        (">=", bin_op!(>=, MalType::Bool)),
        ("pr-str", print_pr_str),
        ("str", print_str),
        ("prn", prn),
        ("println", print_ln),
        ("read-string", read_string),
        ("slurp", slurp),
        ("atom", atom),
        ("atom?", is!(MalType::Atom(..))),
        ("deref", deref),
        ("reset!", reset),
        ("swap!", swap),
        ("cons", cons),
        ("concat", concat),
        ("quasiquote", quasiquote),
    ]
}

fn list(args: Args) -> MalRet {
    Ok(MalType::List(Rc::new(args.to_vec())))
}

fn empty(args: Args) -> MalRet {
    args!([head] = args);

    let res = match head {
        MalType::List(list) | MalType::Vector(list) => list.is_empty(),
        MalType::Hashmap(map) => map.is_empty(),
        _ => return Err(MalError::WrongArgs),
    };

    Ok(MalType::Bool(res))
}

fn count(args: Args) -> MalRet {
    args!([head] = args);

    let res = match head {
        MalType::Nil => 0,
        MalType::List(list) | MalType::Vector(list) => list.len(),
        MalType::Hashmap(map) => map.len(),
        _ => return Err(MalError::WrongArgs),
    };

    Ok(MalType::Number(res as i64))
}

fn equal(args: Args) -> MalRet {
    args!([first, second] = args);

    Ok(MalType::Bool(first == second))
}

fn print_pr_str(args: Args) -> MalRet {
    Ok(MalType::String(
        args.iter()
            .map(|t| pr_str(t, PrintMode::Readable))
            .join(" ")
            .into(),
    ))
}

fn print_str(args: Args) -> MalRet {
    Ok(MalType::String(
        args.iter()
            .map(|t| pr_str(t, PrintMode::Verbatim))
            .collect::<String>()
            .into(),
    ))
}

fn prn(args: Args) -> MalRet {
    let s = args
        .iter()
        .map(|t| pr_str(t, PrintMode::Readable))
        .join(" ");
    println!("{}", s);
    Ok(MalType::Nil)
}

fn print_ln(args: Args) -> MalRet {
    let s = args
        .iter()
        .map(|t| pr_str(t, PrintMode::Verbatim))
        .join(" ");
    println!("{}", s);
    Ok(MalType::Nil)
}

fn read_string(args: Args) -> MalRet {
    args!([MalType::String(s)] = args);

    read_str(s)
}

fn slurp(args: Args) -> MalRet {
    args!([MalType::String(path)] = args);

    std::fs::read_to_string(path.as_ref())
        .map(|s| MalType::String(s.into()))
        .map_err(MalError::IOError)
}

fn atom(args: Args) -> MalRet {
    args!([item] = args);

    Ok(MalType::Atom(Rc::new(RefCell::new(item.clone()))))
}

fn deref(args: Args) -> MalRet {
    args!([MalType::Atom(item)] = args);

    Ok(item.borrow().clone())
}

fn reset(args: Args) -> MalRet {
    args!([MalType::Atom(old), new] = args);

    *old.borrow_mut() = new.clone();

    Ok(new.clone())
}

fn swap(args: Args) -> MalRet {
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
}

fn cons(args: Args) -> MalRet {
    args!([elt, MalType::List(list) | MalType::Vector(list)] = args);

    Ok(MalType::List(
        iter::once(elt.clone())
            .chain(list.iter().cloned())
            .collect_vec()
            .into(),
    ))
}

fn concat(args: Args) -> MalRet {
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
