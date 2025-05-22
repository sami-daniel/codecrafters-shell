#[allow(unused_imports)]
use std::io::{self, Write};
use std::process::ExitCode;

fn main() -> ExitCode {
    loop {
        print!("$ ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        let command: Vec<&str> = input.trim().split(' ').collect();
        if let Some(command) = command.get(0) {
            match *command {
                "exit" => {
                    let parts = input.chars();
                    let exit_code = parts
                        .last()
                        .unwrap_or('0')
                        .to_string()
                        .parse::<u8>()
                        .unwrap_or(0);
                    return ExitCode::from(exit_code);
                }
                _ => {
                    println!("{}: command not found", &input.trim())
                }
            }
        } else {
            return ExitCode::from(0);
        }
    }
}
