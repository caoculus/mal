use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::types::MalType;

#[derive(Debug, Clone, Default)]
pub struct Env(Rc<EnvInner>);

#[derive(Debug, Default)]
struct EnvInner {
    bindings: RefCell<HashMap<Rc<str>, MalType>>,
    outer: Option<Env>,
}

impl Env {
    pub fn new(outer: Option<Env>, bindings: HashMap<Rc<str>, MalType>) -> Self {
        Self(Rc::new(EnvInner {
            bindings: RefCell::new(bindings),
            outer,
        }))
    }

    pub fn set(&self, key: Rc<str>, value: MalType) {
        self.0.bindings.borrow_mut().insert(key, value);
    }

    // TODO: do we need `find`?

    pub fn get(&self, key: &str) -> Option<MalType> {
        self.0
            .bindings
            .borrow()
            .get(key)
            .cloned()
            .or_else(|| self.0.outer.as_ref().and_then(|o| o.get(key)))
    }
}
