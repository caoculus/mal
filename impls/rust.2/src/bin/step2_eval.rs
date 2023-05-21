use std::{
    collections::HashMap,
    io::{stdin, stdout, Write},
};

use mal::{
    eval::{eval, EvalError, MalFn},
    printer::pr_str,
    reader::{read_str, MalType},
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut lines = stdin().lines();

    let repl_env: HashMap<String, MalFn> = HashMap::from([
        ("+".into(), to_mal_fn(|a, b| a + b)),
        ("-".into(), to_mal_fn(|a, b| a - b)),
        ("*".into(), to_mal_fn(|a, b| a * b)),
        ("/".into(), to_mal_fn(|a, b| a / b)),
    ]);

    loop {
        print!("user> ");
        stdout().flush()?;

        let Some(Ok(line)) = lines.next() else { break };

        let ast = match read_str(&line) {
            Ok(ast) => {
                println!("{}", pr_str(&ast));
                ast
            }
            Err(e) => {
                println!("{}", e);
                continue;
            }
        };

        match eval(ast, &repl_env) {
            Ok(res) => println!("{}", pr_str(&res)),
            Err(e) => println!("{}", e),
        };
    }

    Ok(())
}

fn to_mal_fn(f: impl Fn(i64, i64) -> i64 + 'static) -> MalFn {
    Box::new(move |args| {
        if args.len() != 3 {
            return Err(EvalError::WrongArgCount);
        }
        let (MalType::Number(a), MalType::Number(b)) = ({
            let mut iter = args.into_iter().skip(1);
            (iter.next().unwrap(), iter.next().unwrap())
        }) else {
            return Err(EvalError::WrongArgType);
        };

        Ok(MalType::Number(f(a, b)))
    })
}
