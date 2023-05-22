use std::{collections::HashMap, rc::Rc};

use itertools::Itertools;
use thiserror::Error;

use crate::{env::Env, types::MalType};

pub type EvalResult<T> = Result<T, EvalError>;
pub type MalFn = Box<dyn Fn(Vec<MalType>) -> EvalResult<MalType> + 'static>;

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("invalid arguments")]
    WrongArgs,
    #[error("first element of list is not a function")]
    NotAFunction,
    #[error("'{0}' not found.")]
    NotFound(Rc<str>),
}

pub fn eval(ast: MalType, repl_env: &Env) -> EvalResult<MalType> {
    match ast {
        MalType::List(list) => {
            let head = list.first().expect("list should never be empty");

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
                        let new_env = Env::new(Some(prev));

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

                    let MalType::Fn(f) = list.first().expect("list should never be empty") else {
                        return Err(EvalError::NotAFunction);
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
