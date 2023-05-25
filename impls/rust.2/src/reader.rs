use std::{collections::HashMap, iter::Peekable, rc::Rc};

use itertools::Itertools;
use once_cell::sync::OnceCell;
use regex::Regex;
use thiserror::Error;

use crate::types::{MalType, KEYWORD_PREFIX};

type ReadResult<T> = Result<T, ReadError>;

pub fn read_str(text: &str) -> ReadResult<MalType> {
    let mut reader = Reader::new(tokenize(text));
    reader.read_form()
}

fn tokenize(text: &str) -> impl Iterator<Item = ReadResult<Token<'_>>> {
    static REGEX: OnceCell<Regex> = OnceCell::new();
    let regex = REGEX.get_or_init(|| {
        Regex::new(r#"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"#)
            .unwrap()
    });

    regex
        .captures_iter(text)
        .flat_map(|m| to_token(m.get(1).unwrap().as_str()))
}

fn to_token(s: &str) -> Option<ReadResult<Token<'_>>> {
    use Pair::*;
    use Token::*;

    let token = match s {
        "[" => Left(Bracket),
        "]" => Right(Bracket),
        "{" => Left(Brace),
        "}" => Right(Brace),
        "(" => Left(Parenthesis),
        ")" => Right(Parenthesis),
        s if s.starts_with('"') => {
            return Some(
                parse_string(s)
                    .map(String)
                    .ok_or(ReadError::UnbalancedString),
            )
        }
        s if s.starts_with(':') => String(to_keyword(s)),
        s if s.starts_with(';') => return None,
        _ => Other(s),
    };

    Some(Ok(token))
}

// OK to always create an Rc, since we need one anyway
fn parse_string(s: &str) -> Option<Rc<str>> {
    // obvious failures
    if s.len() < 2 || !s.ends_with('"') {
        return None;
    }

    let mut res = String::with_capacity(s.len()); // possibly longer, but good start
    let mut escape = false;
    let mut done = false;

    // skip the leading quote
    for c in s.chars().skip(1) {
        match escape {
            true => {
                match c {
                    c @ ('"' | '\\') => res.push(c),
                    'n' => res.push('\n'),
                    _ => {}
                }
                escape = false;
            }
            false => match c {
                '\\' => escape = true,
                '"' => {
                    done = true;
                    break;
                }
                _ => res.push(c),
            },
        }
    }

    done.then(|| Rc::from(res))
}

fn to_keyword(s: &str) -> Rc<str> {
    Rc::from(
        std::iter::once(KEYWORD_PREFIX)
            .chain(s.chars().skip(1))
            .collect::<String>(),
    )
}

#[derive(Debug)]
enum Token<'a> {
    Left(Pair),
    Right(Pair),
    String(Rc<str>),
    Other(&'a str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Pair {
    Bracket,
    Brace,
    Parenthesis,
}

#[derive(Debug, Error, Clone, Copy)]
pub enum ReadError {
    #[error("unbalanced")]
    UnbalancedString,
    #[error("EOF")]
    Eof,
    #[error("invalid hashmap")]
    InvalidHashmap,
}

struct Reader<I>
where
    I: Iterator,
{
    tokens: Peekable<I>,
}

impl<'a, I> Reader<I>
where
    I: Iterator<Item = ReadResult<Token<'a>>>,
{
    fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    fn read_form(&mut self) -> ReadResult<MalType> {
        let Some(token) = self.tokens.peek() else { return Ok(MalType::Nil) };

        match token {
            Ok(Token::Right(_)) => Err(ReadError::Eof),
            &Ok(Token::Left(pair)) => {
                self.tokens.next();
                let list = self.read_list(pair)?;

                Self::parse_list(list, pair)
            }
            Ok(token) => {
                if let Token::Other(other) = token {
                    if let Some(symbol) = Self::parse_macro(other) {
                        self.tokens.next();

                        let tail = self.read_form()?;
                        return Ok(MalType::List(Rc::new(vec![
                            MalType::Symbol(symbol.into()),
                            tail,
                        ])));
                    }
                }

                let res = Self::read_atom(token);
                self.tokens.next();
                Ok(res)
            }
            &Err(e) => Err(e),
        }
    }

    fn parse_list(list: Vec<MalType>, pair: Pair) -> ReadResult<MalType> {
        match pair {
            Pair::Brace => {
                if list.len() % 2 != 0 {
                    return Err(ReadError::InvalidHashmap);
                }

                list.into_iter()
                    .tuples()
                    .map(|(k, v)| {
                        let MalType::String(k) = k else {
                            return Err(ReadError::InvalidHashmap);
                        };

                        Ok((k, v))
                    })
                    .collect::<ReadResult<HashMap<_, _>>>()
                    .map(|h| MalType::Hashmap(Rc::new(h)))
            }
            Pair::Bracket => Ok(MalType::Vector(Rc::new(list))),
            Pair::Parenthesis => Ok(MalType::List(Rc::new(list))),
        }
    }

    fn parse_macro(text: &'a str) -> Option<&'a str> {
        let res = match text {
            "'" => "quote",
            "`" => "quasiquote",
            "~" => "unquote",
            "~@" => "splice-unquote",
            "@" => "deref",
            _ => return None,
        };

        Some(res)
    }

    fn read_list(&mut self, pair: Pair) -> ReadResult<Vec<MalType>> {
        use Token::*;

        let mut list = vec![];

        while let Some(token) = self.tokens.peek() {
            match token {
                &Ok(Right(other)) => {
                    if pair != other {
                        return Err(ReadError::Eof);
                    }

                    self.tokens.next();
                    return Ok(list);
                }
                Ok(_) => list.push(self.read_form()?),
                Err(e) => return Err(*e),
            }
        }

        Err(ReadError::Eof)
    }

    fn read_atom(token: &Token<'a>) -> MalType {
        match token {
            Token::String(s) => MalType::String(s.clone()),
            Token::Other("nil") => MalType::Nil,
            Token::Other("true") => MalType::Bool(true),
            Token::Other("false") => MalType::Bool(false),
            Token::Other(s) => s
                .parse()
                .map(MalType::Number)
                .unwrap_or(MalType::Symbol(Rc::from(*s))),
            _ => unreachable!(),
        }
    }
}
