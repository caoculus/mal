use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use derivative::Derivative;
use itertools::{Either, Itertools};
use thiserror::Error;

use crate::{
    env::Env,
    printer::{pr_str, PrintMode},
};

pub type MalResult<T> = Result<T, MalError>;
pub type MalFn = Rc<dyn Fn(&[MalType]) -> MalResult<MalType> + 'static>;

pub const KEYWORD_PREFIX: char = '\u{029e}';

#[derive(Derivative, Clone, Default)]
#[derivative(Debug)]
pub enum MalType {
    #[default]
    Nil,
    Bool(bool),
    Number(i64),
    String(Rc<str>),
    Symbol(Rc<str>),
    List(Rc<Vec<MalType>>, Rc<MalType>),
    Vector(Rc<Vec<MalType>>, Rc<MalType>),
    Hashmap(Rc<HashMap<Rc<str>, MalType>>, Rc<MalType>),
    // NOTE: hard to change to a fn pointer, `eval` function needs this to be a closure still
    Fn(#[derivative(Debug = "ignore")] MalFn),
    Closure(#[derivative(Debug = "ignore")] Rc<MalClosure>),
    Atom(Rc<RefCell<MalType>>),
}

impl MalType {
    pub fn list(list: impl Into<Rc<Vec<MalType>>>) -> Self {
        Self::List(list.into(), Rc::new(Self::Nil))
    }

    pub fn vector(list: impl Into<Rc<Vec<MalType>>>) -> Self {
        Self::Vector(list.into(), Rc::new(Self::Nil))
    }

    pub fn hashmap(map: impl Into<Rc<HashMap<Rc<str>, MalType>>>) -> Self {
        Self::Hashmap(map.into(), Rc::new(Self::Nil))
    }

    pub fn string(s: &str) -> Self {
        Self::String(Rc::from(s))
    }

    pub fn symbol(s: &str) -> Self {
        Self::Symbol(Rc::from(s))
    }
}

#[derive(Debug, Error)]
pub enum MalError {
    #[error("unbalanced")]
    UnbalancedString,
    #[error("EOF")]
    Eof,
    #[error("trailing characters")]
    Trailing,
    #[error("invalid hashmap")]
    InvalidHashmap,
    #[error("")]
    Comment,
    // NOTE: this stays as a default for the earlier steps, too lazy to change all the places where
    // this is used
    #[error("invalid arguments")]
    WrongArgs,
    #[error("{0}")]
    String(String),
    #[error("head of list cannot be evaluated (not a function or special keyword)")]
    InvalidHead,
    // NOTE: to be phased out
    #[error("'{0}' not found")]
    NotFound(Rc<str>),
    #[error(transparent)]
    IOError(std::io::Error),
    #[error("index {found} is out of bounds for length {max}")]
    OutOfBounds { max: usize, found: i64 },
    #[error("Exception: {0}")]
    Exception(MalType),
}

#[macro_export]
macro_rules! error {
    () => { return Err(MalError::WrongArgs) };
    ($($arg:tt)*) => { return Err(MalError::String(format!($($arg)*))) };
}

#[macro_export]
macro_rules! try_let {
    ($pat:pat = $args:expr) => {
        let $pat = $args else { $crate::error!() };
    };
    ($pat:pat = $args:expr, $($arg:tt)*) => {
        let $pat = $args else { $crate::error!($($arg)*) };
    };
}

pub(crate) fn hashmap_pairs(
    iter: impl ExactSizeIterator<Item = MalType>,
) -> impl Iterator<Item = MalResult<(Rc<str>, MalType)>> {
    if iter.len() % 2 != 0 {
        return Either::Left(std::iter::once_with(|| {
            error!("hashmap bindings have odd length")
        }));
    }

    Either::Right(iter.tuples().map(|(k, v)| {
        let MalType::String(k) = k else {
                error!("expected string key for hashmap, found {k}")
            };

        Ok((k, v))
    }))
}

impl Display for MalType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pr_str(self, PrintMode::Readable))
    }
}

#[derive(Derivative)]
#[derivative(Debug, Clone)]
pub struct MalClosure {
    // NOTE: this is here because core needs to call eval, which is different for each step
    #[derivative(Debug = "ignore")]
    pub eval: fn(&MalType, &Env) -> MalResult<MalType>,
    pub params: MalParams,
    pub outer: Env,
    pub body: MalType,
    pub is_macro: bool,
    pub meta: Rc<MalType>,
}

impl MalClosure {
    pub fn apply(&self, args: &[MalType]) -> MalResult<(MalType, Env)> {
        let MalClosure {
            params,
            outer,
            body,
            ..
        } = self;

        let binds = params.bind(args)?;

        Ok((body.clone(), Env::new(Some(outer.clone()), binds)))
    }
}

impl Default for MalClosure {
    fn default() -> Self {
        Self {
            eval: |_, _| panic!("Forgot to set eval in closure!"),
            params: Default::default(),
            outer: Default::default(),
            body: Default::default(),
            is_macro: Default::default(),
            meta: Default::default(),
        }
    }
}

#[derive(Debug, Default, Clone)]
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

                let var_list = MalType::list(args[names.len()..].to_vec());
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
            (
                Self::List(l0, ..) | Self::Vector(l0, ..),
                Self::List(r0, ..) | Self::Vector(r0, ..),
            ) => l0 == r0,
            (Self::Hashmap(l0, ..), Self::Hashmap(r0, ..)) => l0 == r0,
            _ => false,
        }
    }
}
