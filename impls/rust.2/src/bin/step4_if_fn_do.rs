use itertools::Itertools;
use rustyline::{error::ReadlineError, DefaultEditor};
use std::{collections::HashMap, rc::Rc};

use mal::{
    env::Env,
    printer::{pr_str, PrintMode},
    reader::read_str,
    types::{MalError, MalResult, MalType},
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let repl_env = base_env();

    // defining the not function
    rep("(def! not (fn* (a) (if a false true)))", &repl_env)
        .expect("Failed to define `not` function");

    let mut rl = DefaultEditor::new()?;

    loop {
        match rl.readline("user> ") {
            Ok(line) => {
                rl.add_history_entry(&line)?;
                match rep(&line, &repl_env) {
                    Ok(res) => println!("{}", res),
                    Err(e) => println!("{}", e),
                };
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    Ok(())
}

fn rep(line: &str, repl_env: &Env) -> MalResult<String> {
    let ast = read_str(line)?;
    let res = eval(ast, repl_env)?;
    Ok(pr_str(&res, PrintMode::Readable))
}

fn base_env() -> Env {
    Env::new(
        None,
        mal::core::ns()
            .iter()
            .map(|(k, v)| ((*k).into(), MalType::Fn(Rc::new(v))))
            .collect(),
    )
}

fn eval(ast: MalType, repl_env: &Env) -> MalResult<MalType> {
    match ast {
        MalType::List(ref list) => {
            // empty list case
            let Some(head) = list.first() else {
                return Ok(ast);
            };

            match head {
                MalType::Symbol(s) if s.as_ref() == "def!" => eval_def(list, repl_env),
                MalType::Symbol(s) if s.as_ref() == "let*" => eval_let(list, repl_env),
                MalType::Symbol(s) if s.as_ref() == "do" => eval_do(list, repl_env),
                MalType::Symbol(s) if s.as_ref() == "if" => eval_if(list, repl_env),
                MalType::Symbol(s) if s.as_ref() == "fn*" => eval_fn(list, repl_env),
                _ => eval_list(list, repl_env),
            }
        }
        ast => eval_ast(ast, repl_env),
    }
}

fn eval_def(list: &[MalType], repl_env: &Env) -> MalResult<MalType> {
    let [_, MalType::Symbol(name), expr] = list else {
        return Err(MalError::WrongArgs);
    };

    let value = eval(expr.clone(), repl_env)?;
    repl_env.set(name.clone(), value.clone());

    Ok(value)
}

fn eval_let(list: &[MalType], repl_env: &Env) -> MalResult<MalType> {
    let [_, MalType::List(bindings) | MalType::Vector(bindings), expr] = list else {
        return Err(MalError::WrongArgs);
    };

    if bindings.len() % 2 != 0 {
        return Err(MalError::WrongArgs);
    }

    let new_env = Env::new(Some(repl_env.clone()), HashMap::new());

    for (name, expr) in bindings.iter().tuples() {
        let MalType::Symbol(name) = name else {
            return Err(MalError::WrongArgs);
        };

        let value = eval(expr.clone(), &new_env)?;

        new_env.set(name.clone(), value);
    }

    eval(expr.clone(), &new_env)
}

fn eval_if(list: &[MalType], repl_env: &Env) -> MalResult<MalType> {
    // if branch is allowed to skip its false branch
    // in which case, the false branch is simply nil
    let (cond, t_branch, f_branch) = match list {
        [_, cond, t_branch] => (cond, t_branch, &MalType::Nil),
        [_, cond, t_branch, f_branch] => (cond, t_branch, f_branch),
        _ => return Err(MalError::WrongArgs),
    };

    let cond_res = eval(cond.clone(), repl_env)?;

    if cond_res.into() {
        eval(t_branch.clone(), repl_env)
    } else {
        eval(f_branch.clone(), repl_env)
    }
}

fn eval_do(list: &[MalType], repl_env: &Env) -> MalResult<MalType> {
    let mut res = None;

    for val in list.iter().skip(1).cloned() {
        res = Some(eval(val, repl_env)?);
    }

    // tail must have at least one element
    res.ok_or(MalError::WrongArgs)
}

fn eval_fn(list: &[MalType], repl_env: &Env) -> MalResult<MalType> {
    let [_, MalType::List(binds) | MalType::Vector(binds), body] = list else {
        return Err(MalError::WrongArgs);
    };

    let Bindings { names, variadic } = parse_binds(binds)?;
    let body = body.clone();
    let outer = repl_env.clone();

    // a lot of cloning happening, because the function can get called
    // multiple times
    Ok(MalType::Fn(Rc::new(move |args| {
        let binds = match &variadic {
            Some(variadic) => {
                if args.len() < names.len() {
                    return Err(MalError::WrongArgs);
                }

                let var_list = MalType::List(Rc::new(args[names.len()..].to_vec()));
                names
                    .iter()
                    .cloned()
                    .zip(args.iter().cloned())
                    .chain(std::iter::once((variadic.clone(), var_list)))
                    .collect()
            }
            None => {
                if args.len() != names.len() {
                    return Err(MalError::WrongArgs);
                }

                names.iter().cloned().zip(args.iter().cloned()).collect()
            }
        };

        let env = Env::new(Some(outer.clone()), binds);
        eval(body.clone(), &env)
    })))
}

fn parse_binds(binds: &[MalType]) -> MalResult<Bindings> {
    let mut names = vec![];
    let mut variadic = None;

    #[derive(Clone, Copy, PartialEq, Eq)]
    enum State {
        Named,
        VarStart,
        Done,
    }

    let mut state = State::Named;

    for t in binds {
        let MalType::Symbol(name) = t else {
            return Err(MalError::WrongArgs);
        };

        match (state, name.as_ref()) {
            (State::Named, "&") => state = State::VarStart,
            (State::Named, _) => names.push(name.clone()),
            (State::VarStart, n) if n != "&" => {
                variadic = Some(name.clone());
                state = State::Done;
            }
            _ => return Err(MalError::WrongArgs),
        }
    }

    (state != State::VarStart)
        .then_some(Bindings { names, variadic })
        .ok_or(MalError::WrongArgs)
}

struct Bindings {
    names: Vec<Rc<str>>,
    variadic: Option<Rc<str>>,
}

fn eval_list(list: &Rc<Vec<MalType>>, repl_env: &Env) -> MalResult<MalType> {
    let MalType::List(list) = eval_ast(MalType::List(list.clone()), repl_env)? else {
        unreachable!("eval_ast should return a list")
    };

    let head = list.first().expect("list should not be empty");

    match head {
        MalType::Fn(f) => f(&list[1..]),
        _ => Err(MalError::InvalidHead),
    }
}

fn eval_ast(ast: MalType, repl_env: &Env) -> MalResult<MalType> {
    match ast {
        MalType::Symbol(s) => repl_env
            .get(&s)
            .ok_or_else(|| MalError::NotFound(s.clone())),
        MalType::List(l) => Ok(MalType::List(Rc::new(
            l.iter()
                .cloned()
                .map(|t| eval(t, repl_env))
                .collect::<MalResult<Vec<_>>>()?,
        ))),
        MalType::Vector(v) => Ok(MalType::Vector(Rc::new(
            v.iter()
                .cloned()
                .map(|t| eval(t, repl_env))
                .collect::<MalResult<Vec<_>>>()?,
        ))),
        MalType::Hashmap(h) => Ok(MalType::Hashmap(Rc::new(
            h.iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .map(|(k, v)| eval(v, repl_env).map(|v| (k, v)))
                .collect::<MalResult<HashMap<_, _>>>()?,
        ))),
        val => Ok(val),
    }
}

#[test]
fn test() {
    rep("( (fn* (& more) (count more)) 1 2 3)", &base_env()).unwrap();
}
