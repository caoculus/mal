use std::{collections::HashMap, rc::Rc};

use derivative::Derivative;

use crate::eval::EvalResult;

// strings are assumed to be lightweight enough
// other values are Rc-wrapped to make them better suited for cloning

pub type MalFn = Rc<dyn Fn(&[MalType]) -> EvalResult<MalType> + 'static>;

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
}

impl From<MalType> for bool {
    fn from(value: MalType) -> Self {
        !matches!(value, MalType::Nil | MalType::Bool(false))
    }
}
