use std::{collections::HashMap, rc::Rc};

use thiserror::Error;

use crate::types::MalType;

pub type EvalResult<T> = Result<T, EvalError>;
pub type MalFn = Box<dyn Fn(Vec<MalType>) -> EvalResult<MalType> + 'static>;

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("wrong argument count")]
    WrongArgCount,
    #[error("wrong argument type")]
    WrongArgType,
    #[error("first element of list is not a function")]
    NotAFunction,
    #[error("'{0}' not found.")]
    NotFound(Rc<str>),
}

pub fn eval(ast: MalType, repl_env: &HashMap<Rc<str>, MalType>) -> EvalResult<MalType> {
    match ast {
        MalType::List(_) => {
            // re-evaluate
            let MalType::List(list) = eval_ast(ast, repl_env)? else {
                unreachable!("eval_ast should return a list")
            };

            let MalType::Fn(f) = list.first().expect("list should never be empty") else {
                return Err(EvalError::NotAFunction);
            };

            f(&list[1..])
        }
        ast => eval_ast(ast, repl_env),
    }
    // match ast {
    //     MalType::List(list) => {
    //         // re-evaluate each element
    //         let list = list
    //             .iter()
    //             .cloned()
    //             .map(|t| eval(t, repl_env))
    //             .collect::<EvalResult<Vec<_>>>()?;

    //         // we check for nils elsewhere, so this should never be an empty list, right?
    //         let MalType::Symbol(symbol) = list.first().expect("List should never be empty") else {
    //             return Err(EvalError::NotASymbol);
    //         };
    //         let Some(f) = repl_env.get(symbol) else {
    //             return Err(EvalError::NotFound(symbol.clone()));
    //         };

    //         f(list)
    //     }
    //     MalType::Vector(v) => Ok(MalType::Vector(Rc::new(
    //         v.iter()
    //             .cloned()
    //             .map(|t| eval(t, repl_env))
    //             .collect::<EvalResult<Vec<_>>>()?,
    //     ))),
    //     MalType::Hashmap(h) => Ok(MalType::Hashmap(Rc::new(
    //         h.iter()
    //             .map(|(k, v)| (k.clone(), v.clone()))
    //             .map(|(k, v)| eval(v, repl_env).map(|v| (k, v)))
    //             .collect::<EvalResult<HashMap<_, _>>>()?,
    //     ))),
    //     ast => Ok(ast),
    // }
}

fn eval_ast(ast: MalType, repl_env: &HashMap<Rc<str>, MalType>) -> EvalResult<MalType> {
    match ast {
        MalType::Symbol(s) => repl_env
            .get(&s)
            .cloned()
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
