use itertools::Itertools;
use rustyline::{error::ReadlineError, DefaultEditor};
use std::{borrow::Cow, collections::HashMap, ops::ControlFlow, rc::Rc};

use mal::{
    args,
    env::Env,
    printer::{pr_str, PrintMode},
    reader::read_str,
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
    let res = eval(ast, repl_env)?;
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
}

fn eval_fn(repl_env: Env) -> MalFn {
    Rc::new(move |args| {
        let [ast] = args else { return Err(MalError::WrongArgs); };

        eval(ast.clone(), &repl_env)
    })
}

fn eval(mut ast: MalType, repl_env: &Env) -> MalResult<MalType> {
    let mut repl_env = Cow::Borrowed(repl_env);

    loop {
        match ast {
            MalType::List(ref list) => {
                let [head, tail @ ..] = &list[..] else {
                    // empty list
                    return Ok(ast);
                };

                // deal with symbol cases first
                if let MalType::Symbol(s) = head {
                    match &s[..] {
                        "def!" => {
                            args!([MalType::Symbol(name), expr] = tail);

                            let value = eval(expr.clone(), &repl_env)?;
                            repl_env.set(name.clone(), value.clone());

                            return Ok(value);
                        }
                        "let*" => {
                            args!(
                                [MalType::List(bindings) | MalType::Vector(bindings), expr] = tail
                            );

                            if bindings.len() % 2 != 0 {
                                return Err(MalError::WrongArgs);
                            }

                            let new_env = Env::new(Some((*repl_env).clone()), HashMap::new());

                            for (name, expr) in bindings.iter().tuples() {
                                args!(MalType::Symbol(name) = name);

                                let value = eval(expr.clone(), &new_env)?;

                                new_env.set(name.clone(), value);
                            }

                            (ast, repl_env) = (expr.clone(), Cow::Owned(new_env));
                            continue;
                        }
                        "do" => {
                            args!([mid @ .., last] = tail);

                            for val in mid.iter().cloned() {
                                eval(val, &repl_env)?;
                            }

                            ast = last.clone();
                            continue;
                        }
                        "if" => {
                            // if branch is allowed to skip its false branch
                            // in which case, the false branch is simply nil
                            let (cond, t_branch, f_branch) = match tail {
                                [cond, t_branch] => (cond, t_branch, &MalType::Nil),
                                [cond, t_branch, f_branch] => (cond, t_branch, f_branch),
                                _ => return Err(MalError::WrongArgs),
                            };

                            let cond_res = eval(cond.clone(), &repl_env)?;

                            ast = if cond_res.into() {
                                t_branch.clone()
                            } else {
                                f_branch.clone()
                            };
                            continue;
                        }
                        "fn*" => {
                            args!([MalType::List(binds) | MalType::Vector(binds), body] = tail);

                            let params = MalParams::new(binds)?;
                            return Ok(MalType::Closure(
                                MalClosure {
                                    params,
                                    outer: (*repl_env).clone(),
                                    body: body.clone(),
                                    eval,
                                }
                                .into(),
                            ));
                        }
                        "quote" => {
                            args!([val] = tail);

                            return Ok(val.clone());
                        }
                        "quasiquoteexpand" => {
                            args!(val @ &[_] = tail);

                            return mal::core::quasiquote(val);
                        }
                        "quasiquote" => {
                            args!(val @ &[_] = tail);

                            ast = mal::core::quasiquote(val)?;
                            continue;
                        }
                        _ => {}
                    }
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
                        let MalClosure {
                            params,
                            outer,
                            body,
                            ..
                        } = closure.as_ref();
                        let binds = params.bind(tail)?;

                        (ast, repl_env) = (
                            body.clone(),
                            Cow::Owned(Env::new(Some(outer.clone()), binds)),
                        );
                        continue;
                    }
                    _ => return Err(MalError::InvalidHead),
                };
            }
            MalType::Vector(ref list) => {
                let [head, tail @ ..] = &list[..] else {
                    // empty list
                    return Ok(ast);
                };

                if let MalType::Symbol(s) = head {
                    match &s[..] {
                        "quote" => {
                            args!([val] = tail);

                            return Ok(val.clone());
                        }
                        "quasiquoteexpand" => {
                            args!(val @ &[_] = tail);
                            assert!(!val.is_empty());

                            return mal::core::quasiquote(val);
                        }
                        "quasiquote" => {
                            args!(val @ &[_] = tail);
                            assert!(!val.is_empty());

                            ast = mal::core::quasiquote(val)?;
                            continue;
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
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
                .cloned()
                .map(|t| eval(t, repl_env))
                .collect::<MalResult<Vec<_>>>()?
                .into(),
        )),
        MalType::Vector(v) => Ok(MalType::Vector(
            v.iter()
                .cloned()
                .map(|t| eval(t, repl_env))
                .collect::<MalResult<Vec<_>>>()?
                .into(),
        )),
        MalType::Hashmap(h) => Ok(MalType::Hashmap(
            h.iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .map(|(k, v)| eval(v, repl_env).map(|v| (k, v)))
                .collect::<MalResult<HashMap<_, _>>>()?
                .into(),
        )),
        val => Ok(val),
    }
}

#[test]
fn test() {
    rep("((fn* (q) (quasiquote ((unquote q) (quote (unquote q))))) (quote (fn* (q) (quasiquote ((unquote q) (quote (unquote q)))))))", &base_env()).unwrap();
}
