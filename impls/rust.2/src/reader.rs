use std::{collections::HashMap, iter::Peekable};

use itertools::Itertools;
use once_cell::sync::OnceCell;
use regex::Regex;
use thiserror::Error;

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
        s if s.starts_with(':') => String(s),
        s if s.starts_with(';') => return None,
        _ => Other(s),
    };

    Some(Ok(token))
}

// TODO: this needs more verification later
fn parse_string(s: &str) -> Option<&str> {
    // obviously too short
    if s.len() < 2 {
        return None;
    }

    // check from the back
    let mut chars = s.chars().rev();
    let Some('"') = chars.next() else { return None };

    let bs_count = chars.take_while(|&c| c == '\\').count();

    (bs_count % 2 == 0).then_some(s)
}

// TODO: make this no longer public
#[derive(Debug)]
pub enum Token<'a> {
    Left(Pair),
    Right(Pair),
    String(&'a str),
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
                        return Ok(MalType::List(vec![MalType::Symbol(symbol.into()), tail]));
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
                    .map(MalType::Hashmap)
            }
            Pair::Bracket => Ok(MalType::Vector(list)),
            Pair::Parenthesis => Ok(if list.is_empty() {
                MalType::Nil
            } else {
                MalType::List(list)
            }),
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
            Token::String(s) => MalType::String(s.to_string()),
            Token::Other(s) => s
                .parse()
                .map(MalType::Number)
                .unwrap_or(MalType::Symbol(s.to_string())),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum MalType {
    Nil,
    Number(i64),
    String(String),
    Symbol(String),
    List(Vec<MalType>),
    Vector(Vec<MalType>),
    Hashmap(HashMap<String, MalType>),
}
