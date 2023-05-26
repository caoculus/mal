use std::{cell::RefCell, collections::HashMap, rc::Rc};

use derivative::Derivative;
use thiserror::Error;

use crate::env::Env;

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
    Fn(#[derivative(Debug = "ignore")] MalFn),
    Closure(#[derivative(Debug = "ignore")] Rc<MalClosure>),
    Atom(Rc<RefCell<MalType>>),
}

pub struct MalClosure {
    // NOTE: this is here because core needs to call eval, which is different for each step
    pub eval: fn(MalType, &Env) -> MalResult<MalType>,
    pub params: MalParams,
    pub outer: Env,
    pub body: MalType,
}

pub struct MalParams {
    pub names: Vec<Rc<str>>,
    pub variadic: Option<Rc<str>>,
}

impl MalParams {
    pub fn new(binds: &[MalType]) -> MalResult<Self> {
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
            .then_some(MalParams { names, variadic })
            .ok_or(MalError::WrongArgs)
    }

    pub fn bind(&self, args: &[MalType]) -> MalResult<HashMap<Rc<str>, MalType>> {
        let Self { names, variadic } = self;

        match &variadic {
            Some(variadic) => {
                if args.len() < names.len() {
                    return Err(MalError::WrongArgs);
                }

                let var_list = MalType::List(Rc::new(args[names.len()..].to_vec()));
                Ok(self
                    .names
                    .iter()
                    .cloned()
                    .zip(args.iter().cloned())
                    .chain(std::iter::once((variadic.clone(), var_list)))
                    .collect())
            }
            None => {
                if args.len() != names.len() {
                    return Err(MalError::WrongArgs);
                }

                Ok(self
                    .names
                    .iter()
                    .cloned()
                    .zip(args.iter().cloned())
                    .collect())
            }
        }
    }
}

#[macro_export]
macro_rules! args {
    ($pat:pat = $args:ident) => {
        let $pat = $args else { return Err(MalError::WrongArgs) };
    };
}

#[derive(Debug, Error)]
pub enum MalError {
    #[error("unbalanced")]
    UnbalancedString,
    #[error("EOF")]
    Eof,
    #[error("invalid hashmap")]
    InvalidHashmap,
    #[error("")]
    Comment,
    // TODO: change this to take a string
    #[error("invalid arguments")]
    WrongArgs,
    #[error("head of list cannot be evaluated (not a function or special keyword)")]
    InvalidHead,
    #[error("'{0}' not found.")]
    NotFound(Rc<str>),
    #[error(transparent)]
    IOError(std::io::Error),
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
