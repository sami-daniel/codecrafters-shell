#[allow(unused_imports)]
use std::io::{self, Write};
use std::{io::stdout, process::ExitCode};

fn main() -> ExitCode {
    let mut output_buf: Vec<u8> = vec![];
    loop {
        output_buf.clear();
        print!("$ ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        let mut parts = input.trim().split(' ').into_iter();
        let available_commands= vec!["echo", "exit", "type"];
        let mut output = stdout().lock();
        output.flush().unwrap();

        if let Some(command) = parts.next() {
            match command {
                "exit" => {
                    let exit_code = parts
                        .next()
                        .unwrap_or(&"0")
                        .to_string()
                        .parse::<u8>()
                        .unwrap_or(0);
                    return ExitCode::from(exit_code);
                }
                "echo" => {
                    while let Some(payload) = parts.next() {
                        output_buf.write_all(payload.as_bytes()).unwrap();
                        output_buf.push(b' ');
                    }
                    output_buf.remove(output_buf.len() - 1);
                    output_buf.push(b'\n');
                }
                "type" => {
                    if let Some(payload) = parts.next() {
                        if available_commands.contains(&payload) {
                            output_buf.write_all(format!("{payload} is a shell builtin\n").as_bytes()).unwrap();
                        } else {
                            output_buf.write_all(format!("{payload}: not found\n").as_bytes()).unwrap();
                        }
                    }
                }
                _ => {
                    report_not_recognized_command(&mut output_buf, command.trim());
                }
            }
        }

        output.write_all(&output_buf[..]).unwrap();
    }
}

#[inline(always)]
fn report_not_recognized_command(output: &mut Vec<u8>, command: &str) {
    output.write_all(format!("{}: command not found\n", &command.trim()).as_bytes()).unwrap();
}