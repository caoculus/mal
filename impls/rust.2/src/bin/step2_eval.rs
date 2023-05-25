use std::{collections::HashMap, rc::Rc};

use mal::{
    printer::{pr_str, PrintMode},
    reader::read_str,
    types::{MalError, MalResult, MalType},
};
use rustyline::{error::ReadlineError, DefaultEditor};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let repl_env: HashMap<Rc<str>, MalType> = HashMap::from([
        ("+".into(), to_mal_fn(|a, b| a + b)),
        ("-".into(), to_mal_fn(|a, b| a - b)),
        ("*".into(), to_mal_fn(|a, b| a * b)),
        ("/".into(), to_mal_fn(|a, b| a / b)),
    ]);

    let mut rl = DefaultEditor::new()?;

    loop {
        match rl.readline("user> ") {
            Ok(line) => {
                rl.add_history_entry(&line)?;
                match rep(&line, &repl_env) {
                    Ok(res) => println!("{}", res),
                    Err(e) => println!("{}", e),
                };
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    Ok(())
}

fn to_mal_fn(f: impl Fn(i64, i64) -> i64 + 'static) -> MalType {
    MalType::Fn(Rc::new(move |args| {
        let &[MalType::Number(a), MalType::Number(b)] = args else {
            return Err(MalError::WrongArgs);
        };

        Ok(MalType::Number(f(a, b)))
    }))
}

fn rep(line: &str, repl_env: &HashMap<Rc<str>, MalType>) -> MalResult<String> {
    let ast = read_str(line)?;
    let res = eval(ast, repl_env)?;
    Ok(pr_str(&res, PrintMode::Readable))
}

// this is kept separate because eval is different later
fn eval(ast: MalType, repl_env: &HashMap<Rc<str>, MalType>) -> MalResult<MalType> {
    match ast {
        MalType::List(ref list) => {
            // empty list is ignored
            if list.is_empty() {
                return Ok(ast);
            }

            // re-evaluate
            let MalType::List(list) = eval_ast(ast, repl_env)? else {
                unreachable!("eval_ast should return a list")
            };

            let Some(MalType::Fn(f)) = list.first() else {
                return Err(MalError::InvalidHead);
            };

            f(&list[1..])
        }
        ast => eval_ast(ast, repl_env),
    }
}

fn eval_ast(ast: MalType, repl_env: &HashMap<Rc<str>, MalType>) -> MalResult<MalType> {
    match ast {
        MalType::Symbol(s) => repl_env
            .get(&s)
            .cloned()
            .ok_or_else(|| MalError::NotFound(s.clone())),
        MalType::List(l) => Ok(MalType::List(Rc::new(
            l.iter()
                .cloned()
                .map(|t| eval(t, repl_env))
                .collect::<MalResult<Vec<_>>>()?,
        ))),
        MalType::Vector(v) => Ok(MalType::Vector(Rc::new(
            v.iter()
                .cloned()
                .map(|t| eval(t, repl_env))
                .collect::<MalResult<Vec<_>>>()?,
        ))),
        MalType::Hashmap(h) => Ok(MalType::Hashmap(Rc::new(
            h.iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .map(|(k, v)| eval(v, repl_env).map(|v| (k, v)))
                .collect::<MalResult<HashMap<_, _>>>()?,
        ))),
        val => Ok(val),
    }
}
