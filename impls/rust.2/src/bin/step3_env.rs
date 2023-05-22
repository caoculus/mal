use std::{
    io::{stdin, stdout, Write},
    rc::Rc,
};

use mal::{
    env::Env,
    eval::{eval, EvalError},
    printer::pr_str,
    reader::read_str,
    types::MalType,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut lines = stdin().lines();

    let repl_env = base_env();

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

fn base_env() -> Env {
    let env = Env::new(None);

    env.set("+".into(), to_mal_fn(|a, b| a + b));
    env.set("-".into(), to_mal_fn(|a, b| a - b));
    env.set("*".into(), to_mal_fn(|a, b| a * b));
    env.set("/".into(), to_mal_fn(|a, b| a / b));

    env
}

fn to_mal_fn(f: impl Fn(i64, i64) -> i64 + 'static) -> MalType {
    MalType::Fn(Rc::new(move |args| {
        let &[MalType::Number(a), MalType::Number(b)] = args else {
            return Err(EvalError::WrongArgs);
        };

        Ok(MalType::Number(f(a, b)))
    }))
}
