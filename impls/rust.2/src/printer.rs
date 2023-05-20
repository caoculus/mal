use std::{borrow::Cow, iter};

use itertools::Itertools;

use crate::reader::{MalType, Pair};

pub fn pr_str(data: MalType<'_>) -> String {
    data.to_string()
}

impl ToString for MalType<'_> {
    fn to_string(&self) -> String {
        match self {
            MalType::Nil => "nil".into(),
            MalType::Number(n) => n.to_string(),
            MalType::Str(s) => s.to_string(),
            MalType::Symbol(s) => s.to_string(),
            MalType::List { pair, list } => iter::once(pair.left().into())
                .chain(Itertools::intersperse(
                    list.iter().map(|token| Cow::Owned(token.to_string())),
                    " ".into(),
                ))
                .chain(iter::once(pair.right().into()))
                .collect(),
        }
    }
}

impl Pair {
    fn left(self) -> &'static str {
        match self {
            Pair::Bracket => "[",
            Pair::Brace => "{",
            Pair::Parenthesis => "(",
        }
    }

    fn right(self) -> &'static str {
        match self {
            Pair::Bracket => "]",
            Pair::Brace => "}",
            Pair::Parenthesis => ")",
        }
    }
}
