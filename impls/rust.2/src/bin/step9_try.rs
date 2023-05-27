use itertools::Itertools;
use rustyline::{error::ReadlineError, DefaultEditor};
use std::{collections::HashMap, ops::ControlFlow, rc::Rc};

use mal::{
    env::Env,
    error,
    printer::{pr_str, PrintMode},
    reader::read_str,
    try_let,
    types::{MalClosure, MalError, MalFn, MalParams, MalResult, MalType},
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let repl_env = base_env();

    define_additional(&repl_env);

    if check_cmd_args(&repl_env)?.is_break() {
        return Ok(());
    }

    let mut rl = DefaultEditor::new()?;

    loop {
        match rl.readline("user> ") {
            Ok(line) => {
                rl.add_history_entry(&line)?;
                match rep(&line, &repl_env) {
                    Ok(res) => println!("{}", res),
                    Err(MalError::Comment) => {}
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

fn check_cmd_args(repl_env: &Env) -> MalResult<ControlFlow<()>> {
    let args: Vec<_> = std::env::args().collect();

    if let [_, name, argv @ ..] = args.as_slice() {
        repl_env.set(
            "*ARGV*".into(),
            MalType::List(Rc::new(
                argv.iter()
                    .map(|a| MalType::String(a.as_str().into()))
                    .collect(),
            )),
        );

        rep(&format!("(load-file \"{name}\")"), repl_env)?;
        return Ok(ControlFlow::Break(()));
    }

    repl_env.set("*ARGV*".into(), MalType::List(vec![].into()));
    Ok(ControlFlow::Continue(()))
}

fn rep(line: &str, repl_env: &Env) -> MalResult<String> {
    let ast = read_str(line)?;
    let res = eval(&ast, repl_env)?;
    Ok(pr_str(&res, PrintMode::Readable))
}

fn base_env() -> Env {
    Env::new(
        None,
        mal::core::ns()
            .iter()
            .map(|(k, v)| ((*k).into(), MalType::Fn(Rc::new(v))))
            .collect(),
    )
}

fn define_additional(repl_env: &Env) {
    // eval function
    repl_env.set("eval".into(), MalType::Fn(eval_fn(repl_env.clone())));
    // not function
    rep("(def! not (fn* (a) (if a false true)))", repl_env).unwrap();
    // load-file function
    rep(
        r#"(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))"#,
        repl_env,
    )
    .unwrap();
    rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))", repl_env).unwrap();
}

fn eval_fn(repl_env: Env) -> MalFn {
    Rc::new(move |args| {
        let [ast] = args else { return Err(MalError::WrongArgs); };

        eval(ast, &repl_env)
    })
}

fn eval(ast: &MalType, repl_env: &Env) -> MalResult<MalType> {
    let mut repl_env = repl_env.clone();
    let mut ast = ast.clone();

    'outer: loop {
        ast = macro_expand(&ast, &repl_env)?;

        'list: {
            let (MalType::List(ref list) | MalType::Vector(ref list)) = ast else { break 'list; };
            let [head, tail @ ..] = &list[..] else { return Ok(ast); };

            'quote: {
                let MalType::Symbol(sym) = head else { break 'quote; };

                match &sym[..] {
                    "quote" => {
                        try_let!([val] = tail, "quote expects 1 argument",);

                        return Ok(val.clone());
                    }
                    "quasiquoteexpand" => {
                        try_let!(val @ &[_] = tail, "quasiquoteexpand expected 1 argument");

                        return mal::core::quasiquote(val);
                    }
                    "quasiquote" => {
                        try_let!(val @ &[_] = tail, "quasiquote expected 1 argument");

                        ast = mal::core::quasiquote(val)?;
                    }
                    _ => break 'quote,
                }

                continue 'outer;
            }

            let MalType::List(..) = ast else { break 'list; };

            'sym: {
                let MalType::Symbol(sym) = head else { break 'sym; };

                match &sym[..] {
                    "def!" => {
                        try_let!(
                            [MalType::Symbol(name), expr] = tail,
                            "def! expects a symbol and an expression"
                        );

                        let value = eval(expr, &repl_env)?;
                        repl_env.set(name.clone(), value.clone());

                        return Ok(value);
                    }
                    "defmacro!" => {
                        try_let!(
                            [MalType::Symbol(name), expr] = tail,
                            "defmacro! expects a symbol and an expression"
                        );

                        let value = eval(expr, &repl_env)?;
                        try_let!(MalType::Closure(ref closure) = value, "defmacro! expected {name} to evaluate to a closure, found {value}");

                        let macro_closure = MalType::Closure(
                            MalClosure {
                                is_macro: true,
                                ..(**closure).clone()
                            }
                            .into(),
                        );
                        repl_env.set(name.clone(), macro_closure);

                        return Ok(MalType::Closure(
                            MalClosure {
                                is_macro: true,
                                ..(**closure).clone()
                            }
                            .into(),
                        ));
                    }
                    "let*" => {
                        try_let!(
                            [MalType::List(bindings) | MalType::Vector(bindings), expr] = tail,
                            "let* expects list of bindings and expression"
                        );

                        if bindings.len() % 2 != 0 {
                            error!("let* binding list must have even length")
                        }

                        let new_env = Env::new(Some(repl_env), HashMap::new());

                        for (name, expr) in bindings.iter().tuples() {
                            try_let!(MalType::Symbol(name) = name);

                            let value = eval(expr, &new_env)?;

                            new_env.set(name.clone(), value);
                        }

                        (ast, repl_env) = (expr.clone(), new_env);
                    }
                    "do" => {
                        try_let!([mid @ .., last] = tail, "do expects non-empty arguments list");

                        for val in mid.iter() {
                            eval(val, &repl_env)?;
                        }

                        ast = last.clone();
                    }
                    "if" => {
                        // if branch is allowed to skip its false branch
                        // in which case, the false branch is simply nil
                        let (cond, t_branch, f_branch) = match tail {
                            [cond, t_branch] => (cond, t_branch, &MalType::Nil),
                            [cond, t_branch, f_branch] => (cond, t_branch, f_branch),
                            _ => error!(
                                "if expects condition, true branch, and optional false branch"
                            ),
                        };

                        let cond_res = eval(cond, &repl_env)?;

                        ast = if cond_res.into() {
                            t_branch.clone()
                        } else {
                            f_branch.clone()
                        };
                    }
                    "fn*" => {
                        try_let!(
                            [MalType::List(binds) | MalType::Vector(binds), body] = tail,
                            "fn* expects binding list and body"
                        );

                        let params = MalParams::new(binds)?;
                        return Ok(MalType::Closure(
                            MalClosure {
                                params,
                                outer: repl_env,
                                body: body.clone(),
                                eval,
                                ..Default::default()
                            }
                            .into(),
                        ));
                    }
                    "try*" => {
                        try_let!(
                            [body, MalType::List(catch)] = tail,
                            "try* expects body and catch body"
                        );
                        try_let!(
                            [MalType::Symbol(s), MalType::Symbol(bind), catch_body] = &catch[..],
                            "catch body should contain catch*, symbol, and body"
                        );
                        if &s[..] != "catch*" {
                            error!("catch body does not start with catch*")
                        }

                        let res = eval(body, &repl_env);
                        let e = match res {
                            Ok(res) => return Ok(res),
                            Err(MalError::Exception(e)) => e,
                            Err(e) => MalType::String(e.to_string().into()),
                        };

                        let new_env = Env::new(Some(repl_env), [(bind.clone(), e)].into());
                        (ast, repl_env) = (catch_body.clone(), new_env);
                    }
                    "macroexpand" => {
                        try_let!([ast] = tail, "macroexpand expects 1 argument");

                        return macro_expand(ast, &repl_env);
                    }
                    _ => break 'sym,
                }

                continue 'outer;
            }

            let MalType::List(list) = eval_ast(MalType::List(list.clone()), &repl_env)? else {
                unreachable!("eval_ast should return a list")
            };

            let [head, tail @ ..] = &list[..] else {
                unreachable!("list should not be empty")
            };

            match head {
                MalType::Fn(f) => return f(tail),
                MalType::Closure(closure) => {
                    (ast, repl_env) = closure.apply(tail)?;
                }
                _ => error!("{head} is not a function"),
            };

            continue 'outer;
        }

        return eval_ast(ast, &repl_env);
    }
}

fn eval_ast(ast: MalType, repl_env: &Env) -> MalResult<MalType> {
    match ast {
        MalType::Symbol(s) => repl_env
            .get(&s)
            .ok_or_else(|| MalError::NotFound(s.clone())),
        MalType::List(l) => Ok(MalType::List(
            l.iter()
                .map(|t| eval(t, repl_env))
                .collect::<MalResult<Vec<_>>>()?
                .into(),
        )),
        MalType::Vector(v) => Ok(MalType::Vector(
            v.iter()
                .map(|t| eval(t, repl_env))
                .collect::<MalResult<Vec<_>>>()?
                .into(),
        )),
        MalType::Hashmap(h) => Ok(MalType::Hashmap(
            h.iter()
                .map(|(k, v)| eval(v, repl_env).map(|v| (k.clone(), v)))
                .collect::<MalResult<HashMap<_, _>>>()?
                .into(),
        )),
        val => Ok(val),
    }
}

fn macro_expand(ast: &MalType, env: &Env) -> MalResult<MalType> {
    let mut ast = ast.clone();

    loop {
        let MalType::List(list) = &ast else { break; };
        let [MalType::Symbol(s), tail @ ..] = &list[..] else { break; };
        let Some(MalType::Closure(closure)) = env.get(s) else { break; };

        if !closure.is_macro {
            break;
        }

        let (cl_ast, cl_env) = closure.apply(tail)?;
        ast = eval(&cl_ast, &cl_env)?;
    }

    Ok(ast)
}

#[test]
fn test() {
    rep("((fn* (q) (quasiquote ((unquote q) (quote (unquote q))))) (quote (fn* (q) (quasiquote ((unquote q) (quote (unquote q)))))))", &base_env()).unwrap();
}
