use std::error::Error;

use mal::{
    printer::{pr_str, PrintMode},
    reader::read_str,
};
use rustyline::{error::ReadlineError, DefaultEditor};

fn main() -> Result<(), Box<dyn Error>> {
    let mut rl = DefaultEditor::new()?;

    loop {
        match rl.readline("user> ") {
            Ok(line) => {
                rl.add_history_entry(&line)?;
                match read_str(&line) {
                    Ok(res) => println!("{}", pr_str(&res, PrintMode::Readable)),
                    Err(e) => println!("{}", e),
                }
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
