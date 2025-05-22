#[allow(unused_imports)]
use std::io::{self, Write};
use std::process::ExitCode;

fn main() -> ExitCode {
    loop {
        print!("$ ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        let parts: Vec<&str> = input.trim().split(' ').collect();
        if let Some(command) = parts.get(0) {
            match *command {
                "exit" => {
                    let exit_code = parts
                        .last()
                        .unwrap_or(&"0")
                        .to_string()
                        .parse::<u8>()
                        .unwrap_or(0);
                    return ExitCode::from(exit_code);
                }
                "echo" => {
                    let payload = parts[1..].iter().enumerate();
                    let len = payload.len();
                    for (i, part) in payload {
                        print!("{part}");
                        if i < len - 1 {
                            print!(" ")
                        }
                    }
                    println!("")
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
