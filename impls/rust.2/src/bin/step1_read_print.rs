use std::{
    error::Error,
    io::{stdin, stdout, Write},
};

use mal::{printer::pr_str, reader::read_str};

fn main() -> Result<(), Box<dyn Error>> {
    let mut lines = stdin().lines();

    loop {
        print!("user> ");
        stdout().flush()?;

        let Some(Ok(line)) = lines.next() else { break };

        let res = read_str(&line).map(pr_str);
        match res {
            Ok(o) => println!("{}", o),
            Err(e) => println!("{}", e),
        }
    }

    Ok(())
}
