use std::borrow::Cow;

use itertools::Itertools;

use crate::types::{MalType, KEYWORD_PREFIX};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrintMode {
    Verbatim,
    Readable,
}

pub fn pr_str(data: &MalType, readably: PrintMode) -> String {
    match data {
        MalType::Nil => "nil".into(),
        MalType::Bool(b) => b.to_string(),
        MalType::Number(n) => n.to_string(),
        MalType::String(s) => print_mal_string(s, readably),
        MalType::Symbol(s) => s.to_string(),
        MalType::List(l) => list_to_string(l.iter().map(|t| pr_str(t, readably).into()), "(", ")"),
        MalType::Vector(v) => {
            list_to_string(v.iter().map(|t| pr_str(t, readably).into()), "[", "]")
        }
        MalType::Hashmap(h) => list_to_string(
            h.iter().flat_map(|(k, v)| {
                [
                    print_mal_string(k, readably).into(),
                    pr_str(v, readably).into(),
                ]
            }),
            "{",
            "}",
        ),
        MalType::Fn(_) => "#<function>".into(),
    }
}

fn print_mal_string(s: &str, readably: PrintMode) -> String {
    fn to_keyword_str(s: &str) -> String {
        std::iter::once(':').chain(s.chars().skip(1)).collect()
    }

    fn to_readable_str(s: &str) -> String {
        let mut res = String::with_capacity(s.len() + 2);

        res.push('"');

        for c in s.chars() {
            match c {
                '\\' => res.push_str("\\\\"),
                '\n' => res.push_str("\\n"),
                '"' => res.push_str("\\\""),
                _ => res.push(c),
            }
        }

        res.push('"');

        res
    }

    if s.starts_with(KEYWORD_PREFIX) {
        to_keyword_str(s)
    } else {
        match readably {
            PrintMode::Verbatim => s.to_string(),
            PrintMode::Readable => to_readable_str(s),
        }
    }
}

fn list_to_string<'a>(
    iter: impl Iterator<Item = Cow<'a, str>>,
    left: &'static str,
    right: &'static str,
) -> String {
    std::iter::once(left.into())
        .chain(Itertools::intersperse(iter, " ".into()))
        .chain(std::iter::once(right.into()))
        .collect()
}
