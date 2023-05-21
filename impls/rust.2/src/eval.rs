use std::collections::HashMap;

use thiserror::Error;

use crate::reader::MalType;

type EvalResult<T> = Result<T, EvalError>;
pub type MalFn = Box<dyn Fn(Vec<MalType>) -> EvalResult<MalType> + 'static>;

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("wrong argument count")]
    WrongArgCount,
    #[error("wrong argument type")]
    WrongArgType,
    #[error("first element of list is not a symbol")]
    NotASymbol,
    #[error("function not found in env")]
    FunctionNotFound,
}

pub fn eval(ast: MalType, repl_env: &HashMap<String, MalFn>) -> EvalResult<MalType> {
    match ast {
        MalType::List(list) => {
            // re-evaluate each element
            let list = list
                .into_iter()
                .map(|t| eval(t, repl_env))
                .collect::<EvalResult<Vec<_>>>()?;

            // we check for nils elsewhere, so this should never be an empty list, right?
            let MalType::Symbol(symbol) = list.first().expect("List should never be empty") else {
                return Err(EvalError::NotASymbol);
            };
            let Some(f) = repl_env.get(symbol) else {
                return Err(EvalError::FunctionNotFound);
            };

            f(list)
        }
        MalType::Vector(v) => Ok(MalType::Vector(
            v.into_iter()
                .map(|t| eval(t, repl_env))
                .collect::<EvalResult<Vec<_>>>()?,
        )),
        MalType::Hashmap(h) => Ok(MalType::Hashmap(
            h.into_iter()
                .map(|(k, v)| eval(v, repl_env).map(|v| (k, v)))
                .collect::<EvalResult<HashMap<_, _>>>()?,
        )),
        ast => Ok(ast),
    }
}

// fn eval_ast(ast: MalType, repl_env: &HashMap<String, MalFn>) -> EvalResult<MalType> {

//     todo!()
// }
