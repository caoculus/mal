use std::{borrow::Cow, iter};

use itertools::Itertools;

use crate::reader::{MalAtom, MalType, Pair, Symbol};

pub fn pr_str(data: MalType<'_>) -> String {
    data.to_string()
}

impl ToString for MalType<'_> {
    fn to_string(&self) -> String {
        match self {
            MalType::Atom(a) => a.to_string(),
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

impl ToString for MalAtom<'_> {
    fn to_string(&self) -> String {
        match self {
            MalAtom::Number(n) => n.to_string(),
            MalAtom::Symbol(s) => s.to_string(),
        }
    }
}

impl ToString for Symbol<'_> {
    fn to_string(&self) -> String {
        match self {
            Symbol::TildeAt => "~@".into(),
            Symbol::Tick => "'".into(),
            Symbol::Backtick => "`".into(),
            Symbol::Tilde => "~".into(),
            Symbol::Caret => "^".into(),
            Symbol::At => "@".into(),
            Symbol::Str(s) => s.to_string(),
            Symbol::Comment(s) => s.to_string(),
            Symbol::Other(s) => s.to_string(),
        }
    }
}
