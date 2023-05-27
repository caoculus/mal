use crate::{error, try_let, types::MalResult};
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
        (stringify!($op),
            move |args: &[MalType]| {
                $crate::try_let!(&[MalType::Number(a), MalType::Number(b)] = args, "{} expects two numbers", stringify!($op));

                Ok($ty(a $op b))
            }
        )
    };
}

macro_rules! is {
    ($name:literal, $pat:pat) => {
        ($name, move |args: &[MalType]| {
            $crate::try_let!([first] = args, "{} expects 1 argument", $name);

            Ok(MalType::Bool(matches!(first, $pat)))
        })
    };
}

// returns the base environment
pub const fn ns() -> &'static [(&'static str, MalFnPtr)] {
    &[
        bin_op!(+, MalType::Number),
        bin_op!(-, MalType::Number),
        bin_op!(*, MalType::Number),
        bin_op!(/, MalType::Number),
        bin_op!(<, MalType::Bool),
        bin_op!(<=, MalType::Bool),
        bin_op!(>, MalType::Bool),
        bin_op!(>=, MalType::Bool),
        is!("list?", MalType::List(..)),
        is!("nil?", MalType::Nil),
        is!("true?", MalType::Bool(true)),
        is!("false?", MalType::Bool(false)),
        is!("symbol?", MalType::Symbol(..)),
        is!("atom?", MalType::Atom(..)),
        ("list", list),
        ("empty?", empty),
        ("count", count),
        ("=", equal),
        ("pr-str", print_pr_str),
        ("str", print_str),
        ("prn", prn),
        ("println", print_ln),
        ("read-string", read_string),
        ("slurp", slurp),
        ("atom", atom),
        ("deref", deref),
        ("reset!", reset),
        ("swap!", swap),
        ("cons", cons),
        ("concat", concat),
        ("quasiquote", quasiquote),
        ("vec", vec),
        ("nth", nth),
        ("first", first),
        ("rest", rest),
        ("throw", throw),
        ("apply", apply),
        ("map", map),
    ]
}

fn list(args: Args) -> MalRet {
    Ok(MalType::List(Rc::new(args.to_vec())))
}

fn empty(args: Args) -> MalRet {
    try_let!([head] = args, "empty expects 1 argument");

    let res = match head {
        MalType::List(list) | MalType::Vector(list) => list.is_empty(),
        MalType::Hashmap(map) => map.is_empty(),
        _ => return Err(MalError::WrongArgs),
    };

    Ok(MalType::Bool(res))
}

fn count(args: Args) -> MalRet {
    try_let!([head] = args, "count expects 1 argument");

    let res = match head {
        MalType::Nil => 0,
        MalType::List(list) | MalType::Vector(list) => list.len(),
        MalType::Hashmap(map) => map.len(),
        _ => error!("{head} is not nil, a list, a vector, or a hashmap"),
    };

    Ok(MalType::Number(res as i64))
}

fn equal(args: Args) -> MalRet {
    try_let!([first, second] = args, "equal expects 2 arguments");

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
    try_let!([MalType::String(s)] = args, "read_string expects a string");

    read_str(s)
}

fn slurp(args: Args) -> MalRet {
    try_let!([MalType::String(path)] = args, "slurp expects a string");

    std::fs::read_to_string(path.as_ref())
        .map(|s| MalType::String(s.into()))
        .map_err(MalError::IOError)
}

fn atom(args: Args) -> MalRet {
    try_let!([item] = args, "atom expects 1 argument");

    Ok(MalType::Atom(Rc::new(RefCell::new(item.clone()))))
}

fn deref(args: Args) -> MalRet {
    try_let!([MalType::Atom(item)] = args, "deref expects an atom");

    Ok(item.borrow().clone())
}

fn reset(args: Args) -> MalRet {
    try_let!(
        [MalType::Atom(old), new] = args,
        "reset expects an atom and a new value"
    );

    *old.borrow_mut() = new.clone();

    Ok(new.clone())
}

fn swap(args: Args) -> MalRet {
    try_let!([MalType::Atom(atom), f @ (MalType::Fn(..) | MalType::Closure(..)), tail @ ..] = args, "swap expects an atom, a function, and arguments");

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
                ..
            } = closure.as_ref();

            let binds = params.bind(&args)?;
            let env = Env::new(Some((*outer).clone()), binds);

            eval(body, &env)?
        }
        _ => unreachable!(),
    };

    *atom.borrow_mut() = res.clone();
    Ok(res)
}

fn cons(args: Args) -> MalRet {
    try_let!(
        [elt, MalType::List(list) | MalType::Vector(list)] = args,
        "cons expects an element and a list or vector"
    );

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

pub fn quasiquote(args: Args) -> MalResult<MalType> {
    fn go(ast: &MalType) -> MalResult<MalType> {
        match ast {
            MalType::List(list) => match &list[..] {
                [MalType::Symbol(s), tail @ ..] if &s[..] == "unquote" => {
                    try_let!([snd] = tail, "unquote expects 1 argument");
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

    fn go_list(list: &[MalType]) -> MalResult<MalType> {
        list.iter()
            .rev()
            .try_fold(MalType::List(vec![].into()), |acc, elt| {
                if let MalType::List(list) = elt {
                    match &list[..] {
                        [MalType::Symbol(s), tail @ ..] if &s[..] == "splice-unquote" => {
                            try_let!([snd] = tail, "splice-unquote expects 1 argument");
                            return Ok(MalType::List(
                                vec![MalType::Symbol("concat".into()), snd.clone(), acc].into(),
                            ));
                        }
                        _ => {}
                    }
                }

                Ok(MalType::List(
                    vec![MalType::Symbol("cons".into()), go(elt)?, acc].into(),
                ))
            })
    }

    try_let!([ast] = args, "quasiquote expects 1 argument");

    go(ast)
}

fn vec(args: Args) -> MalRet {
    try_let!(
        [MalType::List(list) | MalType::Vector(list)] = args,
        "vec expects a list or vector"
    );

    Ok(MalType::Vector(list.to_vec().into()))
}

fn nth(args: Args) -> MalRet {
    try_let!(
        [
            MalType::List(list) | MalType::Vector(list),
            MalType::Number(idx)
        ] = args,
        "nth expects a list or vector and an index"
    );

    let i: usize = usize::try_from(*idx).map_err(|_| MalError::OutOfBounds {
        max: list.len(),
        found: *idx,
    })?;
    list.get(i).cloned().ok_or(MalError::OutOfBounds {
        max: list.len(),
        found: *idx,
    })
}

fn first(args: Args) -> MalRet {
    try_let!([head @ (MalType::Nil | MalType::List(..) | MalType::Vector(..))] = args, "first expects nil, a list, or a vector");

    let res = match head {
        MalType::Nil => MalType::Nil,
        MalType::List(list) | MalType::Vector(list) => {
            list.first().cloned().unwrap_or(MalType::Nil)
        }
        _ => unreachable!(),
    };

    Ok(res)
}

fn rest(args: Args) -> MalRet {
    try_let!([head @ (MalType::Nil | MalType::List(..) | MalType::Vector(..))] = args, "rest expects nil, a list, or a vector");

    let res = match head {
        MalType::Nil => vec![],
        MalType::List(list) | MalType::Vector(list) => list.iter().skip(1).cloned().collect(),
        _ => unreachable!(),
    };

    Ok(MalType::List(res.into()))
}

fn throw(args: Args) -> MalRet {
    try_let!([e] = args, "throw expects 1 argument");
    Err(MalError::Exception(e.clone()))
}

fn apply(args: Args) -> MalRet {
    try_let!([f @ (MalType::Fn(..) | MalType::Closure(..)), mid @ .., MalType::List(list) | MalType::Vector(list)] = args, "apply expects a function, 0 or more arguments, and a list or vector");

    let args = mid
        .iter()
        .cloned()
        .chain(list.iter().cloned())
        .collect_vec();

    match f {
        MalType::Fn(f) => f(&args),
        MalType::Closure(closure) => {
            let (ast, env) = closure.apply(&args)?;
            (closure.eval)(&ast, &env)
        }
        _ => unreachable!(),
    }
}

fn map(args: Args) -> MalRet {
    try_let!([f @ (MalType::Fn(..) | MalType::Closure(..)), MalType::List(list) | MalType::Vector(list)] = args, "map expects a function and a list or vector");

    let new_list = match f {
        MalType::Fn(f) => list
            .iter()
            .map(|t| f(&[t.clone()]))
            .collect::<MalResult<Vec<MalType>>>()?,
        MalType::Closure(closure) => list
            .iter()
            .map(|t| {
                let (ast, env) = closure.apply(&[t.clone()])?;
                (closure.eval)(&ast, &env)
            })
            .collect::<MalResult<Vec<MalType>>>()?,
        _ => unreachable!(),
    };

    Ok(MalType::List(new_list.into()))
}
