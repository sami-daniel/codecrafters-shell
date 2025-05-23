use anyhow::{Context, Result};
use nix::libc::{fork, waitpid};
use std::{
    env::{self, set_current_dir},
    ffi::{CStr, CString},
    io::{self, Write},
    path::{Path, PathBuf},
    process::ExitCode,
    ptr::null_mut,
};

fn main() -> ExitCode {
    match run() {
        Ok(code) => code,
        Err(err) => {
            eprintln!("Error: {}", err);
            ExitCode::FAILURE
        }
    }
}

fn run() -> Result<ExitCode> {
    let mut output_buf = Vec::new();
    let mut output = io::stdout().lock();

    loop {
        output_buf.clear();
        print!("$ ");
        output.flush().context("Failed to flush stdout")?;

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .context("Failed to read input")?;

        if let Some(command) = &mut Command::parse(&input)? {
            if let Some(code) = command.execute(&mut output_buf)? {
                return Ok(code);
            }
        }

        output
            .write_all(&output_buf)
            .context("Failed to write output")?;
    }
}

enum Supported {
    Echo,
    Exit,
    Type,
    Partial,
    Unknown,
    PrintWorkingDirectory,
    ChangeDirectory,
}

struct Command {
    name: String,
    args: Vec<String>,
    path: Option<PathBuf>,
    kind: Supported,
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
                if Command::exists(command) {
                    Self::Partial
                } else {
                    Self::Unknown
                }
            }
        }
    }

    fn is_shell_builtin(command: &str) -> bool {
        matches!(
            Self::from_str(command),
            Self::Echo | Self::Exit | Self::Type | Self::PrintWorkingDirectory
        )
    }
}

impl Command {
    fn parse(input: &str) -> Result<Option<Self>> {
        let parts = input.splitn(2, ' ').collect::<Vec<&str>>();
        let chars = parts.last().unwrap().trim().chars();
        let mut args: Vec<String> = vec![];
        let mut in_single_quotes = false;
        let mut in_double_quotes = false;
        let mut word_buf = String::new();
    
        for ch in chars {
            match ch {
                '"' => {
                    if !in_single_quotes {
                        in_double_quotes = !in_double_quotes;
                        continue;
                    } else {
                        word_buf.push(ch);
                    }
                }
                '\'' => {
                    if !in_double_quotes {
                        in_single_quotes = !in_single_quotes;
                        continue;
                    } else {
                        word_buf.push(ch);
                    }
                }
                ' ' if !in_single_quotes && !in_double_quotes => {
                    if !word_buf.is_empty() {
                        args.push(word_buf.clone());
                        word_buf.clear();
                    }
                }
                _ => word_buf.push(ch),
            }
        }
    
        if !word_buf.is_empty() {
            args.push(word_buf);
        }
    
        dbg!(&args);
    
        if let Some(name) = parts.first().cloned() {
            let kind = Supported::from_str(&name);
            let path = if let Supported::Partial = kind {
                Self::load_extern_path(&name)
            } else {
                None
            };
    
            Ok(Some(Self {
                name: name.to_string(),
                args,
                path,
                kind,
            }))
        } else {
            Ok(None)
        }
    }

    fn load_extern(command: &String) -> Option<Self> {
        let path_dirs = env::var("PATH").ok()?;
        for dir in path_dirs.split(':') {
            let full_path = Path::new(dir).join(command);
            if full_path.exists() && full_path.is_file() {
                return Some(Self {
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

    fn exists(path: &str) -> bool {
        Self::load_extern_path(path).is_some()
    }

    fn execute(&mut self, output_buf: &mut Vec<u8>) -> Result<Option<ExitCode>> {
        for i in 0..self.args.len() {
            if self.args[i].contains("~") {
                let new = self.args[i].replace("~", Self::get_home()?.as_str());
                self.args[i] = new;
            }
        }
        match self.kind {
            Supported::Echo => {
                for arg in &self.args {
                    output_buf.write_all(arg.as_bytes())?;
                    output_buf.push(b' ');
                }
                output_buf.push(b'\n');
                Ok(None)
            }
            Supported::Exit => {
                let code = self
                    .args
                    .first()
                    .and_then(|v| v.parse::<u8>().ok())
                    .unwrap_or(0);
                Ok(Some(ExitCode::from(code)))
            }
            Supported::Type => {
                if let Some(cmd) = self.args.first() {
                    if Supported::is_shell_builtin(cmd) {
                        writeln!(output_buf, "{} is a shell builtin", cmd)?;
                    } else if let Some(command) = Self::load_extern(cmd) {
                        writeln!(output_buf, "{} is {}", cmd, command.path.unwrap().display())?;
                    } else {
                        writeln!(output_buf, "{}: not found", cmd)?;
                    }
                }
                Ok(None)
            }
            Supported::Unknown => {
                writeln!(output_buf, "{}: command not found", self.name)?;
                Ok(None)
            }
            Supported::Partial => {
                let pid = unsafe { fork() };
                match pid {
                    0 => {
                        self.exec_from_execve()?;
                        Ok(None)
                    }
                    _ => {
                        unsafe {
                            waitpid(0, null_mut(), 0);
                        }
                        Ok(None)
                    }
                }
            }
            Supported::PrintWorkingDirectory => {
                writeln!(output_buf, "{}", Self::get_cwd()?)?;
                Ok(None)
            }
            Supported::ChangeDirectory => {
                let path = match self.args.first() {
                    Some(p) if p.starts_with("~") => {
                        PathBuf::from(Self::get_home()?).join(Path::new(&p[1..]))
                    }
                    Some(p) => Path::new(&Self::get_cwd()?).join(p),
                    None => PathBuf::from(Self::get_home()?),
                };

                if set_current_dir(&path).is_err() {
                    writeln!(
                        output_buf,
                        "cd: {}: No such file or directory",
                        path.display()
                    )?;
                }

                Ok(None)
            }
        }
    }

    fn exec_from_execve(&self) -> Result<()> {
        let c_command = CString::new(
            self.path
                .as_ref()
                .context("Missing executable path")?
                .to_str()
                .context("Path contains invalid UTF-8")?,
        )?;
        let mut full_args = vec![CString::new(self.name.clone())?];
        full_args.extend(
            self.args
                .iter()
                .map(|s| CString::new(s.as_str()))
                .collect::<Result<Vec<_>, _>>()?,
        );
        let c_args: Vec<&CStr> = full_args.iter().map(|s| s.as_c_str()).collect();
        let c_env: Vec<&CStr> = vec![];

        nix::unistd::execve(&c_command, &c_args, &c_env)?;
        Ok(())
    }

    fn get_cwd() -> Result<String> {
        Ok(env::current_dir()?
            .to_str()
            .context("Invalid current dir")?
            .to_string())
    }

    fn get_home() -> Result<String> {
        env::var("HOME").context("Missing HOME variable")
    }
}
