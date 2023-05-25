use std::rc::Rc;

use thiserror::Error;

pub type EvalResult<T> = Result<T, EvalError>;

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("invalid arguments")]
    WrongArgs,
    #[error("head of list cannot be evaluated (not a function or special keyword)")]
    InvalidHead,
    #[error("'{0}' not found.")]
    NotFound(Rc<str>),
}
