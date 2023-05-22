use std::{collections::HashMap, rc::Rc};

use derivative::Derivative;

use crate::eval::EvalResult;

// strings are assumed to be lightweight enough
// other values are Rc-wrapped to make them better suited for cloning

type MalFn = Rc<dyn Fn(&[MalType]) -> EvalResult<MalType> + 'static>;

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub enum MalType {
    Nil,
    Number(i64),
    String(Rc<str>),
    Symbol(Rc<str>),
    List(Rc<Vec<MalType>>),
    Vector(Rc<Vec<MalType>>),
    Hashmap(Rc<HashMap<Rc<str>, MalType>>),
    Fn(#[derivative(Debug = "ignore")] MalFn),
}
