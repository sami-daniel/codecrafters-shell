use std::env::{self, set_current_dir};
use std::ffi::{CStr, CString};
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::ptr::null_mut;
use std::{io::stdout, process::ExitCode};

use nix::libc::{fork, waitpid};

enum Supported {
    Echo,
    Exit,
    Type,
    Partial,
    Unknown,
    PrintWorkingDirectory,
    ChangeDirectory
}

impl Supported {
    fn from_str(command: &str) -> Self {
        match command {
            "echo" => Self::Echo,
            "exit" => Self::Exit,
            "type" => Self::Type,
            "pwd" => Self::PrintWorkingDirectory,
            "cd" => Self::ChangeDirectory,
            _ => {
                let s_command = String::from(command);
                if Command::exists(&s_command) {
                    return Self::Partial;
                }

                Self::Unknown
            }
        }
    }

    fn is_shell_builtin(command: &str) -> bool {
        matches!(
            Self::from_str(command),
            Supported::Echo | Supported::Exit | Supported::Type | Supported::PrintWorkingDirectory
        )
    }
}

struct Command {
    name: String,
    args: Vec<String>,
    path: Option<PathBuf>,
    kind: Supported,
}

impl Command {
    fn parse(input: &str) -> Option<Self> {
        let mut parts = input.trim().split_whitespace();
        if let Some(name) = parts.next() {
            let args: Vec<String> = parts.map(|s| s.to_string()).collect();
            let kind = Supported::from_str(name);

            let path = if let Supported::Partial = kind {
                Command::load_extern_path(&name)
            } else {
                None
            };

            Some(Command {
                name: name.to_string(),
                args,
                path,
                kind,
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
                    kind: Supported::Partial,
                });
            }
        }
        None
    }

    fn load_extern_path(command: &str) -> Option<PathBuf> {
        let path_dirs = env::var("PATH").ok()?;
        for dir in path_dirs.split(':') {
            let full_path = Path::new(dir).join(command);
            if full_path.exists() && full_path.is_file() {
                return Some(full_path);
            }
        }
        None
    }

    fn exists(path: &String) -> bool {
        Self::load_extern_path(path).is_some()
    }

    fn execute(&self, output_buf: &mut Vec<u8>) -> Option<ExitCode> {
        match self.kind {
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
                    } else if let Some(command) = Command::load_extern(cmd) {
                        writeln!(
                            output_buf,
                            "{} is {}",
                            cmd,
                            command.path.unwrap().as_path().display()
                        )
                        .unwrap();
                    } else {
                        writeln!(output_buf, "{}: not found", cmd).unwrap();
                    }
                }
                None
            }
            Supported::Unknown => {
                writeln!(output_buf, "{}: command not found", self.name).unwrap();
                None
            }
            Supported::Partial => {
                let pid = unsafe { fork() };

                match pid {
                    0 => {
                        self.exec_from_execve();

                        None // if we came here, means that execve failed
                    }
                    _ => {
                        unsafe {
                            waitpid(0, null_mut(), 0);
                        }
                        None // Means that i'm parent or the fork process failed
                    }
                }
            }
            Supported::PrintWorkingDirectory => {
                writeln!(
                    output_buf,
                    "{}",
                    Self::get_cwd()
                )
                .unwrap();

                None
            },
            Supported::ChangeDirectory => {
                set_current_dir(Path::new(Self::get_cwd().as_str()).join(Path::new(self.args.first().unwrap()))).unwrap();

                None
            }
        }
    }

    fn exec_from_execve(&self) {
        let c_command =
            CString::new(self.path.clone().unwrap().as_path().to_str().unwrap()).unwrap();
        let mut full_args = vec![CString::new(self.name.clone()).unwrap()];
        full_args.extend(
            self.args
                .iter()
                .map(|s| CString::new(s.as_str()).expect("Arg has \\0")),
        );
        let c_args: Vec<&CStr> = full_args.iter().map(|s| s.as_c_str()).collect();
        let c_env: Vec<&CStr> = vec![];
        nix::unistd::execve(&c_command, &c_args, &c_env[..]).unwrap();
    }

    #[inline(always)]
    fn get_cwd() -> String {
        env::current_dir().unwrap().to_str().unwrap().to_string()
    }
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
