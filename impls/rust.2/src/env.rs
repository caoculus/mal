use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::types::MalType;

#[derive(Clone)]
pub struct Env(Rc<EnvInner>);

struct EnvInner {
    bindings: RefCell<HashMap<Rc<str>, MalType>>,
    outer: Option<Env>,
}

impl Env {
    // TODO: move these constructors to builder pattern
    pub fn new() -> Self {
        Self(Rc::new(EnvInner {
            bindings: RefCell::new(HashMap::new()),
            outer: None,
        }))
    }

    pub fn with_outer(outer: Env) -> Self {
        Self(Rc::new(EnvInner {
            bindings: RefCell::new(HashMap::new()),
            outer: Some(outer),
        }))
    }

    pub fn with_binds(binds: impl IntoIterator<Item = (Rc<str>, MalType)>) -> Self {
        Self(Rc::new(EnvInner {
            bindings: RefCell::new(HashMap::from_iter(binds)),
            outer: None,
        }))
    }

    pub fn with_outer_and_binds(
        outer: Env,
        binds: impl IntoIterator<Item = (Rc<str>, MalType)>,
    ) -> Self {
        Self(Rc::new(EnvInner {
            bindings: RefCell::new(HashMap::from_iter(binds)),
            outer: Some(outer),
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

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}
