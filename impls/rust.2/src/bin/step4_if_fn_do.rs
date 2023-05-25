use itertools::Itertools;
use mal::{eval::EvalResult, printer::PrintMode};
use rustyline::{error::ReadlineError, DefaultEditor};
use std::{collections::HashMap, rc::Rc};

use mal::{env::Env, eval::EvalError, printer::pr_str, reader::read_str, types::MalType};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let repl_env = base_env();

    let mut rl = DefaultEditor::new()?;

    loop {
        match rl.readline("user> ") {
            Ok(line) => {
                rl.add_history_entry(&line)?;
                let ast = match read_str(&line) {
                    Ok(ast) => {
                        println!("{}", pr_str(&ast, PrintMode::Readable));
                        ast
                    }
                    Err(e) => {
                        println!("{}", e);
                        continue;
                    }
                };

                match eval(ast, &repl_env) {
                    Ok(res) => println!("{}", pr_str(&res, PrintMode::Readable)),
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

fn base_env() -> Env {
    let env = Env::new();

    env.set("+".into(), to_mal_fn(|a, b| a + b));
    env.set("-".into(), to_mal_fn(|a, b| a - b));
    env.set("*".into(), to_mal_fn(|a, b| a * b));
    env.set("/".into(), to_mal_fn(|a, b| a / b));

    env
}

fn to_mal_fn(f: impl Fn(i64, i64) -> i64 + 'static) -> MalType {
    MalType::Fn(Rc::new(move |args| {
        let &[MalType::Number(a), MalType::Number(b)] = args else {
            return Err(EvalError::WrongArgs);
        };

        Ok(MalType::Number(f(a, b)))
    }))
}

fn eval(ast: MalType, repl_env: &Env) -> EvalResult<MalType> {
    match ast {
        MalType::List(list) => {
            // empty list case
            let Some(head) = list.first() else {
                return Ok(MalType::List(list));
            };

            match head {
                MalType::Symbol(s) if s.as_ref() == "def!" => {
                    let [_, MalType::Symbol(name), expr] = list.as_slice() else {
                        return Err(EvalError::WrongArgs);
                    };

                    let value = eval(expr.clone(), repl_env)?;
                    repl_env.set(name.clone(), value.clone());

                    Ok(value)
                }
                MalType::Symbol(s) if s.as_ref() == "let*" => {
                    let [_, MalType::List(bindings) | MalType::Vector(bindings), expr] = list.as_slice() else {
                        return Err(EvalError::WrongArgs);
                    };

                    if bindings.len() % 2 != 0 {
                        return Err(EvalError::WrongArgs);
                    }

                    let mut prev = repl_env.clone();

                    for (name, expr) in bindings.iter().tuples() {
                        let MalType::Symbol(name) = name else {
                            return Err(EvalError::WrongArgs);
                        };

                        let value = eval(expr.clone(), &prev)?;
                        let new_env = Env::with_outer(prev);

                        new_env.set(name.clone(), value);

                        prev = new_env;
                    }

                    eval(expr.clone(), &prev)
                }
                MalType::Symbol(s) if s.as_ref() == "do" => {
                    let mut res = None;

                    for val in list.iter().skip(1).cloned() {
                        res = Some(eval(val, repl_env)?);
                    }

                    // tail must have at least one element
                    res.ok_or(EvalError::WrongArgs)
                }
                MalType::Symbol(s) if s.as_ref() == "if" => {
                    let [_, cond, t_branch, f_branch] = list.as_slice() else {
                                return Err(EvalError::WrongArgs);
                            };

                    let cond_res = eval(cond.clone(), repl_env)?;

                    if cond_res.into() {
                        eval(t_branch.clone(), repl_env)
                    } else {
                        eval(f_branch.clone(), repl_env)
                    }
                }
                MalType::Symbol(s) if s.as_ref() == "fn*" => {
                    let [_, MalType::List(binds), body] = list.as_slice() else {
                                return Err(EvalError::WrongArgs);
                            };

                    let binds = binds
                        .iter()
                        .map(|v| match v {
                            MalType::Symbol(s) => Ok(s.clone()),
                            _ => Err(EvalError::WrongArgs),
                        })
                        .collect::<EvalResult<Vec<Rc<str>>>>()?;
                    let body = body.clone();
                    let outer = repl_env.clone();

                    // a lot of cloning happening, because the function can get called
                    // multiple times
                    Ok(MalType::Fn(Rc::new(move |args| {
                        if binds.len() != args.len() {
                            return Err(EvalError::WrongArgs);
                        }

                        let env = Env::with_outer_and_binds(
                            outer.clone(),
                            binds.iter().cloned().zip(args.iter().cloned()),
                        );

                        eval(body.clone(), &env)
                    })))
                }
                _ => {
                    // re-evaluate
                    let MalType::List(list) = eval_ast(MalType::List(list.clone()), repl_env)? else {
                        unreachable!("eval_ast should return a list")
                    };

                    let head = list.first().expect("list should not be empty");

                    match head {
                        MalType::Fn(f) => f(&list[1..]),
                        _ => Err(EvalError::InvalidHead),
                    }
                }
            }
        }
        ast => eval_ast(ast, repl_env),
    }
}

fn eval_ast(ast: MalType, repl_env: &Env) -> EvalResult<MalType> {
    match ast {
        MalType::Symbol(s) => repl_env
            .get(&s)
            .ok_or_else(|| EvalError::NotFound(s.clone())),
        MalType::List(l) => Ok(MalType::List(Rc::new(
            l.iter()
                .cloned()
                .map(|t| eval(t, repl_env))
                .collect::<EvalResult<Vec<_>>>()?,
        ))),
        MalType::Vector(v) => Ok(MalType::Vector(Rc::new(
            v.iter()
                .cloned()
                .map(|t| eval(t, repl_env))
                .collect::<EvalResult<Vec<_>>>()?,
        ))),
        MalType::Hashmap(h) => Ok(MalType::Hashmap(Rc::new(
            h.iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .map(|(k, v)| eval(v, repl_env).map(|v| (k, v)))
                .collect::<EvalResult<HashMap<_, _>>>()?,
        ))),
        val => Ok(val),
    }
}
