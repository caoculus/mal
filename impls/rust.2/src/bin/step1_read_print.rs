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

        match read_str(&line) {
            Ok(res) => println!("{}", pr_str(&res)),
            Err(e) => println!("{}", e),
        }
    }

    Ok(())
}
