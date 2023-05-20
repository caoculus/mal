use std::iter::Peekable;

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
        .map(|m| to_token(m.get(1).unwrap().as_str()))
}

fn to_token(s: &str) -> ReadResult<Token<'_>> {
    use Pair::*;
    use Token::*;

    let token = match s {
        "~@" => TildeAt,
        "[" => Left(Bracket),
        "]" => Right(Bracket),
        "{" => Left(Brace),
        "}" => Right(Brace),
        "(" => Left(Parenthesis),
        ")" => Right(Parenthesis),
        "'" => Tick,
        "`" => Backtick,
        "~" => Tilde,
        "^" => Caret,
        "@" => At,
        s if s.starts_with('"') => parse_string(s)
            .map(Str)
            .ok_or(ReadError::UnbalancedString)?,
        // TODO: consider removing the semicolon?
        s if s.starts_with(';') => Comment(s),
        _ => Other(s),
    };

    Ok(token)
}

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
    TildeAt,
    Left(Pair),
    Right(Pair),
    Tick,
    Backtick,
    Tilde,
    Caret,
    At,
    Str(&'a str),
    Comment(&'a str),
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

    fn read_form<'b>(&'b mut self) -> ReadResult<MalType<'a>> {
        use Token::*;

        match self.tokens.peek() {
            Some(Ok(Right(_))) => Err(ReadError::Eof),
            Some(&Ok(Left(pair))) => {
                self.tokens.next();
                self.read_list(pair)
                    .map(|list| MalType::List { pair, list })
            }
            _ => self.read_atom().map(MalType::Atom),
        }
    }

    fn read_list<'b>(&'b mut self, pair: Pair) -> ReadResult<Vec<MalType<'a>>> {
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

    fn read_atom<'b>(&'b mut self) -> ReadResult<MalAtom<'a>> {
        let token = self.tokens.next().ok_or(ReadError::Eof)??;

        let atom = match token {
            Token::TildeAt => MalAtom::Symbol(Symbol::TildeAt),
            Token::Tick => MalAtom::Symbol(Symbol::Tick),
            Token::Backtick => MalAtom::Symbol(Symbol::Backtick),
            Token::Tilde => MalAtom::Symbol(Symbol::Tilde),
            Token::Caret => MalAtom::Symbol(Symbol::Caret),
            Token::At => MalAtom::Symbol(Symbol::At),
            Token::Str(s) => MalAtom::Symbol(Symbol::Str(s)),
            Token::Comment(s) => MalAtom::Symbol(Symbol::Comment(s)),
            Token::Other(s) => s
                .parse()
                .map(MalAtom::Number)
                .unwrap_or(MalAtom::Symbol(Symbol::Other(s))),
            _ => unreachable!(),
        };

        Ok(atom)
    }
}

#[derive(Debug)]
pub enum MalType<'a> {
    Atom(MalAtom<'a>),
    List { pair: Pair, list: Vec<MalType<'a>> },
}

// TODO: divide this further
#[derive(Debug)]
pub enum MalAtom<'a> {
    Number(i64),
    Symbol(Symbol<'a>),
}

#[derive(Debug)]
pub enum Symbol<'a> {
    TildeAt,
    Tick,
    Backtick,
    Tilde,
    Caret,
    At,
    Str(&'a str),
    Comment(&'a str),
    Other(&'a str),
}
