use std::borrow::Cow;

use itertools::Itertools;

use crate::reader::MalType;

pub fn pr_str(data: &MalType) -> String {
    data.to_string()
}

impl ToString for MalType {
    fn to_string(&self) -> String {
        match self {
            MalType::Nil => "()".into(),
            MalType::Number(n) => n.to_string(),
            MalType::String(s) => s.to_string(),
            MalType::Symbol(s) => s.to_string(),
            MalType::List(l) => list_to_string(l.iter().map(|t| t.to_string().into()), "(", ")"),
            MalType::Vector(v) => list_to_string(v.iter().map(|t| t.to_string().into()), "[", "]"),
            MalType::Hashmap(h) => list_to_string(
                h.iter().flat_map(|(k, v)| [k.into(), v.to_string().into()]),
                "{",
                "}",
            ),
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

// impl Pair {
//     fn left(self) -> &'static str {
//         match self {
//             Pair::Bracket => "[",
//             Pair::Brace => "{",
//             Pair::Parenthesis => "(",
//         }
//     }

//     fn right(self) -> &'static str {
//         match self {
//             Pair::Bracket => "]",
//             Pair::Brace => "}",
//             Pair::Parenthesis => ")",
//         }
//     }
// }
