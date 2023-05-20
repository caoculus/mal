use std::{
    error::Error,
    io::{stdin, stdout, Write},
};

fn main() -> Result<(), Box<dyn Error>> {
    let mut lines = stdin().lines();

    loop {
        print!("user> ");
        stdout().flush()?;

        let Some(Ok(line)) = lines.next() else { break };
        println!("{}", line);
    }

    Ok(())
}
