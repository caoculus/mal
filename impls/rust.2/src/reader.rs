use std::{collections::HashMap, iter::Peekable, rc::Rc};

use once_cell::sync::OnceCell;
use regex::Regex;

use crate::types::{hashmap_pairs, MalError, MalResult, MalType, KEYWORD_PREFIX};

pub fn read_str(text: &str) -> MalResult<MalType> {
    let mut reader = Reader::new(tokenize(text));
    reader.read_form_init()
}

fn tokenize(text: &str) -> impl Iterator<Item = MalResult<Token<'_>>> {
    static REGEX: OnceCell<Regex> = OnceCell::new();
    let regex = REGEX.get_or_init(|| {
        Regex::new(r#"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"#)
            .unwrap()
    });

    regex
        .captures_iter(text)
        .flat_map(|m| to_token(m.get(1).unwrap().as_str()))
}

fn to_token(s: &str) -> Option<MalResult<Token<'_>>> {
    use Pair::*;
    use Token::*;

    let token = match s {
        "" => return None,
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
                    .ok_or(MalError::UnbalancedString),
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

struct Reader<I>
where
    I: Iterator,
{
    tokens: Peekable<I>,
}

impl<'a, I> Reader<I>
where
    I: Iterator<Item = MalResult<Token<'a>>>,
{
    fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    // hacky workaround?
    fn read_form_init(&mut self) -> MalResult<MalType> {
        if self.tokens.peek().is_none() {
            return Err(MalError::Comment);
        }

        let res = self.read_form()?;

        if let Some(token) = self.tokens.peek() {
            println!("Trailing: {token:?}");
            return Err(MalError::Trailing);
        }

        Ok(res)
    }

    fn read_form(&mut self) -> MalResult<MalType> {
        let token = self.tokens.peek().ok_or(MalError::Eof)?;

        match token {
            Ok(Token::Right(_)) => Err(MalError::Eof),
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
                        return Ok(MalType::list(vec![MalType::symbol(symbol), tail]));
                    } else if *other == "^" {
                        self.tokens.next();

                        let first = self.read_form()?;
                        let second = self.read_form()?;

                        return Ok(MalType::list(vec![
                            MalType::Symbol("with-meta".into()),
                            second,
                            first,
                        ]));
                    }
                }

                let res = Self::read_atom(token);
                self.tokens.next();
                Ok(res)
            }
            Err(_) => Err(self.tokens.next().unwrap().unwrap_err()),
        }
    }

    fn parse_list(list: Vec<MalType>, pair: Pair) -> MalResult<MalType> {
        match pair {
            Pair::Brace => hashmap_pairs(list.into_iter())
                .collect::<MalResult<HashMap<_, _>>>()
                .map(MalType::hashmap),
            Pair::Bracket => Ok(MalType::vector(list)),
            Pair::Parenthesis => Ok(MalType::list(list)),
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

    fn read_list(&mut self, pair: Pair) -> MalResult<Vec<MalType>> {
        use Token::*;

        let mut list = vec![];

        while let Some(token) = self.tokens.peek() {
            match token {
                &Ok(Right(other)) => {
                    if pair != other {
                        return Err(MalError::Eof);
                    }

                    self.tokens.next();
                    return Ok(list);
                }
                Ok(_) => list.push(self.read_form()?),
                Err(_) => return Err(self.tokens.next().unwrap().unwrap_err()),
            }
        }

        Err(MalError::Eof)
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
