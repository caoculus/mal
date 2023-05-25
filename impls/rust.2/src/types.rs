use std::{collections::HashMap, rc::Rc};

use derivative::Derivative;
use thiserror::Error;

// strings are assumed to be lightweight enough
// other values are Rc-wrapped to make them better suited for cloning

pub type MalResult<T> = Result<T, MalError>;
pub type MalFn = Rc<dyn Fn(&[MalType]) -> MalResult<MalType> + 'static>;

pub const KEYWORD_PREFIX: char = '\u{029e}';

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub enum MalType {
    Nil,
    Bool(bool),
    Number(i64),
    String(Rc<str>),
    Symbol(Rc<str>),
    List(Rc<Vec<MalType>>),
    Vector(Rc<Vec<MalType>>),
    Hashmap(Rc<HashMap<Rc<str>, MalType>>),
    // TODO: consider splitting this based on closures vs non-closures?
    Fn(#[derivative(Debug = "ignore")] MalFn),
}

#[derive(Debug, Error, Clone)]
pub enum MalError {
    #[error("unbalanced")]
    UnbalancedString,
    #[error("EOF")]
    Eof,
    #[error("invalid hashmap")]
    InvalidHashmap,
    #[error("invalid arguments")]
    WrongArgs,
    #[error("head of list cannot be evaluated (not a function or special keyword)")]
    InvalidHead,
    #[error("'{0}' not found.")]
    NotFound(Rc<str>),
}

impl From<MalType> for bool {
    fn from(value: MalType) -> Self {
        !matches!(value, MalType::Nil | MalType::Bool(false))
    }
}

impl PartialEq for MalType {
    fn eq(&self, other: &Self) -> bool {
        // tests ask that lists and vectors be equal

        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Symbol(l0), Self::Symbol(r0)) => l0 == r0,
            (Self::List(l0) | Self::Vector(l0), Self::List(r0) | Self::Vector(r0)) => l0 == r0,
            (Self::Hashmap(l0), Self::Hashmap(r0)) => l0 == r0,
            _ => false,
        }
    }
}
