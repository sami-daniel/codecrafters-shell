use std::env;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::{io::stdout, process::ExitCode};

enum Supported {
    Echo,
    Exit,
    Type,
    Partial(String),
    Unknown,
}

impl Supported {
    fn from_str(command: &str) -> Self {
        match command {
            "echo" => Self::Echo,
            "exit" => Self::Exit,
            "type" => Self::Type,
            _ => Self::Unknown,
        }
    }

    fn is_shell_builtin(command: &str) -> bool {
        matches!(
            Self::from_str(command),
            Supported::Echo | Supported::Exit | Supported::Type
        )
    }
}

struct Command {
    name: String,
    args: Vec<String>,
    path: Option<PathBuf>,
}

impl Command {
    fn parse(input: &str) -> Option<Self> {
        let mut parts = input.trim().split_whitespace();
        if let Some(name) = parts.next() {
            let args: Vec<String> = parts.map(|s| s.to_string()).collect();
            Some(Command {
                name: name.to_string(),
                args,
                path: None,
            })
        } else {
            None
        }
    }

    fn load_extern(command: &String) -> Option<Self> {
        let path_dirs = env::var("PATH").ok()?;
        for dir in path_dirs.split(':') {
            let full_path = Path::new(dir).join(command);
            if full_path.exists() && full_path.is_file() {
                return Some(Command {
                    name: command.to_string(),
                    args: vec![],
                    path: Some(full_path),
                });
            }
        }
        None
    }

    fn exists(path: &String) -> bool {
        Self::load_extern(path).is_some()
    }

    fn execute(&self, output_buf: &mut Vec<u8>) -> Option<ExitCode> {
        match Supported::from_str(&self.name) {
            Supported::Echo => {
                for arg in &self.args {
                    output_buf.write_all(arg.as_bytes()).unwrap();
                    output_buf.push(b' ');
                }

                output_buf.pop();
                output_buf.push(b'\n');

                None
            }
            Supported::Exit => {
                let code = self
                    .args
                    .get(0)
                    .and_then(|v| v.parse::<u8>().ok())
                    .unwrap_or(0);
                Some(ExitCode::from(code))
            }
            Supported::Type => {
                if let Some(cmd) = self.args.get(0) {
                    if Supported::is_shell_builtin(cmd) {
                        writeln!(output_buf, "{} is a shell builtin", cmd).unwrap();
                    } else {
                        if let Some(command) = Command::load_extern(cmd) {
                            writeln!(
                                output_buf,
                                "{} is {}",
                                cmd,
                                command.path.unwrap().as_path().display()
                            )
                            .unwrap();
                        } else {
                            report_not_found(output_buf, &cmd);
                        }
                    }
                }
                None
            }
            Supported::Unknown => {
                report_not_found(output_buf, &self.name);
                None
            }
            _ => None,
        }
    }
}

#[inline(always)]
fn report_not_found(output: &mut Vec<u8>, cmd: &String) {
    writeln!(output, "{}: command not found", cmd).unwrap();
}

fn main() -> ExitCode {
    let mut output_buf: Vec<u8> = vec![];
    let mut output = stdout().lock();

    loop {
        output_buf.clear();
        print!("$ ");
        output.flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        if let Some(command) = Command::parse(&input) {
            if let Some(code) = command.execute(&mut output_buf) {
                return code;
            }
        }

        output.write_all(&output_buf).unwrap();
    }
}
