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

    let mut rl = DefaultEditor::new()?;

    loop {
        match rl.readline("user> ") {
            Ok(line) => {
                rl.add_history_entry(&line)?;
                match rep(&line, &repl_env) {
                    Ok(res) => println!("{}", res),
                    Err(MalError::Comment) => {}
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
    let env = Env::default();

    env.set("+".into(), to_mal_fn(|a, b| a + b));
    env.set("-".into(), to_mal_fn(|a, b| a - b));
    env.set("*".into(), to_mal_fn(|a, b| a * b));
    env.set("/".into(), to_mal_fn(|a, b| a / b));

    env
}

fn to_mal_fn(f: impl Fn(i64, i64) -> i64 + 'static) -> MalType {
    MalType::func(Rc::new(move |args| {
        let &[MalType::Number(a), MalType::Number(b)] = args else {
            return Err(MalError::WrongArgs);
        };

        Ok(MalType::Number(f(a, b)))
    }))
}

fn eval(ast: MalType, repl_env: &Env) -> MalResult<MalType> {
    match ast {
        MalType::List(ref list, ..) => {
            // empty list case
            let Some(head) = list.first() else {
                return Ok(ast);
            };

            match head {
                MalType::Symbol(s) if s.as_ref() == "def!" => {
                    let [_, MalType::Symbol(name), expr] = list.as_slice() else {
                        return Err(MalError::WrongArgs);
                    };

                    let value = eval(expr.clone(), repl_env)?;
                    repl_env.set(name.clone(), value.clone());

                    Ok(value)
                }
                MalType::Symbol(s) if s.as_ref() == "let*" => {
                    let [_, MalType::List(bindings, ..) | MalType::Vector(bindings, ..), expr] = list.as_slice() else {
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
                _ => {
                    // re-evaluate
                    let MalType::List(list, ..) = eval_ast(MalType::list(list.clone()), repl_env)? else {
                        unreachable!("eval_ast should return a list")
                    };

                    let MalType::Fn(f, ..) = list.first().expect("list should not be empty") else {
                        return Err(MalError::InvalidHead);
                    };

                    f(&list[1..])
                }
            }
        }
        ast => eval_ast(ast, repl_env),
    }
}

fn eval_ast(ast: MalType, repl_env: &Env) -> MalResult<MalType> {
    match ast {
        MalType::Symbol(s) => repl_env
            .get(&s)
            .ok_or_else(|| MalError::NotFound(s.clone())),
        MalType::List(l, ..) => Ok(MalType::list(
            l.iter()
                .cloned()
                .map(|t| eval(t, repl_env))
                .collect::<MalResult<Vec<_>>>()?,
        )),
        MalType::Vector(v, ..) => Ok(MalType::vector(
            v.iter()
                .cloned()
                .map(|t| eval(t, repl_env))
                .collect::<MalResult<Vec<_>>>()?,
        )),
        MalType::Hashmap(h, ..) => Ok(MalType::hashmap(
            h.iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .map(|(k, v)| eval(v, repl_env).map(|v| (k, v)))
                .collect::<MalResult<HashMap<_, _>>>()?,
        )),
        val => Ok(val),
    }
}
