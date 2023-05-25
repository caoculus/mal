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
                _ => {
                    // re-evaluate
                    let MalType::List(list) = eval_ast(MalType::List(list.clone()), repl_env)? else {
                        unreachable!("eval_ast should return a list")
                    };

                    let MalType::Fn(f) = list.first().expect("list should not be empty") else {
                        return Err(EvalError::InvalidHead);
                    };

                    f(&list[1..])
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
