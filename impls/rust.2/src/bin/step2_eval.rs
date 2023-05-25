use std::{collections::HashMap, rc::Rc};

use mal::{
    eval::{EvalError, EvalResult},
    printer::{pr_str, PrintMode},
    reader::read_str,
    types::MalType,
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
                let ast = match read_str(&line) {
                    Ok(ast) => {
                        println!("{}", pr_str(&ast, PrintMode::Readable));
                        ast
                    }
                    Err(e) => {
                        println!("{}", e);
                        continue;
                    }
                };

                match eval(ast, &repl_env) {
                    Ok(res) => println!("{}", pr_str(&res, PrintMode::Readable)),
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
            return Err(EvalError::WrongArgs);
        };

        Ok(MalType::Number(f(a, b)))
    }))
}

// this is kept separate because eval is different later
fn eval(ast: MalType, repl_env: &HashMap<Rc<str>, MalType>) -> EvalResult<MalType> {
    match ast {
        MalType::List(_) => {
            // re-evaluate
            let MalType::List(list) = eval_ast(ast, repl_env)? else {
                unreachable!("eval_ast should return a list")
            };

            let Some(MalType::Fn(f)) = list.first() else {
                return Err(EvalError::InvalidHead);
            };

            f(&list[1..])
        }
        ast => eval_ast(ast, repl_env),
    }
}

fn eval_ast(ast: MalType, repl_env: &HashMap<Rc<str>, MalType>) -> EvalResult<MalType> {
    match ast {
        MalType::Symbol(s) => repl_env
            .get(&s)
            .cloned()
            .ok_or_else(|| EvalError::NotFound(s.clone())),
        MalType::List(l) => Ok(MalType::List(Rc::new(
            l.iter()
                .cloned()
                .map(|t| eval(t, repl_env))
                .collect::<EvalResult<Vec<_>>>()?,
        ))),
        MalType::Vector(v) => Ok(MalType::Vector(Rc::new(
            v.iter()
                .cloned()
                .map(|t| eval(t, repl_env))
                .collect::<EvalResult<Vec<_>>>()?,
        ))),
        MalType::Hashmap(h) => Ok(MalType::Hashmap(Rc::new(
            h.iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .map(|(k, v)| eval(v, repl_env).map(|v| (k, v)))
                .collect::<EvalResult<HashMap<_, _>>>()?,
        ))),
        val => Ok(val),
    }
}
