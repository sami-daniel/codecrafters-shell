use anyhow::{Context, Ok, Result};
use nix::libc::{close, dup, dup2, fork, open, waitpid, O_APPEND, O_CREAT, O_RDWR, O_TRUNC};
use nix::unistd::ForkResult;
use rustyline::error::ReadlineError;
use rustyline::{
    self, completion::Completer, CompletionType, Config, Editor, Helper, Highlighter, Hinter,
    Validator,
};
use std::os::fd::{AsFd, AsRawFd, FromRawFd, OwnedFd};
use std::{
    env::{self, set_current_dir},
    ffi::{CStr, CString},
    io::{stdout, Write},
    path::{Path, PathBuf},
    process::ExitCode,
    ptr::null_mut,
    str::FromStr,
};

const BUILTIN_A: &[&str] = &["echo", "pwd", "cd", "type", "exit"];
const UNEXPECTED_BEHAVIOR_MSG: &str = "Unexpected behavior has happened";

fn main() -> ExitCode {
    match run() {
        Result::Ok(code) => code,
        Err(err) => {
            eprintln!("Error: {}", err);
            ExitCode::FAILURE
        }
    }
}

fn run() -> Result<ExitCode> {
    let config = Config::builder()
        .auto_add_history(true)
        .completion_type(CompletionType::List)
        .build();
    let mut editor = Editor::with_config(config).context("Failed to create editor")?;
    editor.set_helper(Some(AwesomeHelper {
        completer: AwesomeCompleter,
    }));

    loop {
        let readline = editor.readline("$ ");

        match readline {
            Result::Ok(line) => {
                let commands = parse_pipeline(&line)?;
                if let Some(code) = execute_pipeline(commands, &mut vec![])? {
                    return Ok(code);
                }
            }
            Err(ReadlineError::Interrupted) => continue,
            Err(ReadlineError::Eof) => return Ok(ExitCode::SUCCESS),
            Err(err) => return Err(err).context("Readline error"),
        }
    }
}

fn execute_pipeline(
    mut commands: Vec<Command>,
    output_buf: &mut Vec<u8>,
) -> Result<Option<ExitCode>> {
    let num_commands = commands.len();
    if num_commands == 0 {
        return Ok(None);
    }
    if num_commands == 1 {
        return commands[0].execute(output_buf, false);
    }

    let mut pipes = Vec::new();
    for _ in 0..num_commands - 1 {
        pipes.push(nix::unistd::pipe()?);
    }

    let mut children = Vec::new();

    for (index, mut command) in commands.into_iter().enumerate() {
        let stdin_pipe = index.checked_sub(1).map(|i| pipes[i].0.as_fd());
        let stdout_pipe = pipes.get(index).map(|p| p.1.as_fd());

        let pid = unsafe { nix::unistd::fork() };
        
        match pid {
            Result::Ok(ForkResult::Child) => { 
                if let Some(pipe_in) = stdin_pipe {
                    if !has_redirect(&command, 0) {
                        unsafe {
                            dup2(pipe_in.as_raw_fd(), 0);
                        }
                    }
                    // nix::unistd::close(pipe_in.as_raw_fd())?;
                }

                if let Some(pipe_out) = stdout_pipe {
                    if !has_redirect(&command, 1) {
                        unsafe {
                            dup2(pipe_out.as_raw_fd(), 1);
                        }
                    }
                    // nix::unistd::close(pipe_out.as_raw_fd())?;
                }

                for (r, w) in &pipes {
                    let r_fd = r.as_raw_fd();
                    let w_fd = w.as_raw_fd();
                    if r_fd != 0 && r_fd != 1 {
                        nix::unistd::close(r_fd)?;
                    }
                    if w_fd != 0 && w_fd != 1 {
                        nix::unistd::close(w_fd)?;
                    }
                }
                
                let _ = command.execute(output_buf, true)?;
                std::process::exit(0);
            }
            Result::Ok(ForkResult::Parent { child, .. }) => children.push(child),
            Err(e) => return Err(e).context("Fork failed"),
        }

        // if let Some(pipe_in) = stdin_pipe {
        //     nix::unistd::close(pipe_in.as_raw_fd())?;
        // }
        // if let Some(pipe_out) = stdout_pipe {
        //     nix::unistd::close(pipe_out.as_raw_fd())?;
        // }
    }

    for (r, w) in pipes {
        nix::unistd::close(r)?;
        nix::unistd::close(w)?;
    }

    for pid in children {
        nix::sys::wait::waitpid(pid, None)?;
    }

    Ok(None)
}

fn has_redirect(cmd: &Command, fd: i32) -> bool {
    cmd.redirects.iter().any(|r| r.fd == fd)
}

fn parse_pipeline(input: &str) -> Result<Vec<Command>> {
    let mut commands = Vec::new();
    let mut current = input.trim();

    while let Some(pos) = find_pipe_pos(current) {
        let (cmd, rest) = current.split_at(pos);
        let command = Command::parse(cmd)?.unwrap();
        commands.push(command);
        current = rest[1..].trim_start();
    }

    if !current.is_empty() {
        let command = Command::parse(current)?.unwrap();
        commands.push(command);
    }

    Ok(commands)
}

fn find_pipe_pos(s: &str) -> Option<usize> {
    let mut in_single = false;
    let mut in_double = false;
    let mut escape = false;

    for (i, c) in s.chars().enumerate() {
        if escape {
            escape = false;
            continue;
        }
        match c {
            '\\' => escape = true,
            '\'' if !in_double => in_single = !in_single,
            '"' if !in_single => in_double = !in_double,
            '|' if !in_single && !in_double => return Some(i),
            _ => {}
        }
    }
    None
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

#[derive(Debug)]
enum Target {
    File(PathBuf), // maybe we can do fd redirect
}

#[derive(Debug, Clone, Copy)]
enum Mode {
    Write,
    Append,
}

struct Command {
    name: String,
    args: Vec<String>,
    path: Option<PathBuf>,
    kind: Supported,
    redirects: Vec<Redirect>,
}

struct AwesomeCompleter;

#[derive(Highlighter, Hinter, Validator)]
struct AwesomeHelper {
    completer: AwesomeCompleter,
}

impl Helper for AwesomeHelper {}

impl Completer for AwesomeHelper {
    type Candidate = String;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        ctx: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        self.completer.complete(line, pos, ctx)
    }
}

fn extract_current_word(line: &str, pos: usize) -> (usize, &str) {
    let line_up_to_cursor = &line[..pos];
    if let Some(last_space) = line_up_to_cursor.rfind(' ') {
        (last_space + 1, &line_up_to_cursor[last_space + 1..])
    } else {
        (0, line_up_to_cursor)
    }
}

impl Completer for AwesomeCompleter {
    type Candidate = String;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        let (start, word) = extract_current_word(line, pos);
        let mut candidates = Vec::new();

        for builtin in BUILTIN_A {
            if builtin.starts_with(word) {
                candidates.push(builtin.to_string());
            }
        }

        if let Result::Ok(path_dirs) = env::var("PATH") {
            for dir in path_dirs.split(':') {
                if let Result::Ok(entries) = std::fs::read_dir(dir) {
                    for entry in entries.filter_map(Result::ok) {
                        let entry_name = entry.file_name();
                        let file_name = entry_name.to_string_lossy();
                        if file_name.starts_with(word) {
                            candidates.push(file_name.into_owned());
                        }
                    }
                }
            }
        }

        candidates.sort();
        candidates.dedup();

        let candidates = candidates
            .iter()
            .map(|c| format!("{c} "))
            .collect::<Vec<_>>();

        Result::Ok((start, candidates))
    }
}

#[derive(Debug)]
struct Redirect {
    fd: i32,
    target: Target,
    mode: Mode,
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
            Self::Echo
                | Self::Exit
                | Self::Type
                | Self::PrintWorkingDirectory
                | Self::ChangeDirectory
        )
    }
}

impl Command {
    fn parse(input: &str) -> Result<Option<Self>> {
        let mut chars = input.trim().chars().peekable();
        let mut args: Vec<String> = vec![];
        let mut redirects = vec![];
        let mut in_single_quotes = false;
        let mut in_double_quotes = false;
        let mut word_buf = String::new();
        let mut current_redirect: Option<(i32, Mode)> = None;

        while let Some(c) = chars.next() {
            match c {
                '>' if !in_double_quotes && !in_single_quotes => {
                    let mode = if chars.peek() == Some(&'>') {
                        chars.next();
                        Mode::Append
                    } else {
                        Mode::Write
                    };

                    if let Result::Ok(num) = word_buf.parse::<i32>() {
                        current_redirect = Some((num, mode));
                        word_buf.clear();
                    } else {
                        current_redirect = Some((1, mode));
                    }
                }
                '"' => {
                    if !in_single_quotes {
                        in_double_quotes = !in_double_quotes;
                        continue;
                    } else {
                        word_buf.push(c);
                    }
                }
                '\'' => {
                    if !in_double_quotes {
                        in_single_quotes = !in_single_quotes;
                        continue;
                    } else {
                        word_buf.push(c);
                    }
                }
                ' ' if !in_single_quotes && !in_double_quotes => {
                    if !word_buf.is_empty() {
                        args.push(word_buf.clone());
                        word_buf.clear();
                    }
                }
                '\\' if in_double_quotes && !in_single_quotes => {
                    // only a few chars are escapable in double quotes, in single
                    // quotes, everything is literal

                    // we assume that will never be equals to none. This would happen
                    // if and only if the " was not closed, then it will escape the \n
                    // and open a new secondary prompt in a line down
                    let next = chars.next().expect(UNEXPECTED_BEHAVIOR_MSG);

                    match next {
                        '\\' => word_buf.push(next),
                        '$' => word_buf.push(next),
                        '\"' => word_buf.push(next),
                        _ => word_buf.extend(vec![c, next]),
                    }
                }
                '\\' if !in_double_quotes && !in_single_quotes => {
                    // we assume that will never be equals to none. Then it will
                    // escape the \n (hidden) and open a new secondary prompt
                    // in a line down

                    let next = chars.next().expect(UNEXPECTED_BEHAVIOR_MSG);

                    match next {
                        ' ' => word_buf.push(next),
                        '\'' => word_buf.push(next),
                        '\"' => word_buf.push(next),
                        _ => word_buf.push(next),
                    }
                }
                _ => word_buf.push(c),
            }
        }

        if !word_buf.is_empty() {
            args.push(word_buf);
        }

        if let Some((fd, mode)) = current_redirect {
            let redirect_path = args.pop().expect(UNEXPECTED_BEHAVIOR_MSG);

            redirects.push(Redirect {
                fd,
                mode,
                target: Target::File(PathBuf::from(redirect_path)),
            })
        }

        ////dbg!(&args);

        if let Some(name) = args.first() {
            let name = name.trim();
            let kind = Supported::from_str(name);
            let path = if let Supported::Partial = kind {
                Self::load_extern_path(name)
            } else {
                None
            };

            Ok(Some(Self {
                name: name.to_string(),
                args: args.into_iter().skip(1).collect(),
                path,
                kind,
                redirects,
            }))
        } else {
            Ok(None)
        }
    }

    fn with_redirects<F>(&self, exec: F) -> Result<()>
    where
        F: FnOnce() -> Result<()>,
    {
        let mut saved_fds = vec![];

        for redirect in self.redirects.iter() {
            //dbg!(&&redirect);

            let original_fd = unsafe { dup(redirect.fd) };
            saved_fds.push((redirect.fd, original_fd));

            match &redirect.target {
                Target::File(path_buf) => {
                    let flags = match redirect.mode {
                        Mode::Write => O_CREAT | O_RDWR | O_TRUNC,
                        Mode::Append => O_CREAT | O_RDWR | O_APPEND,
                    };

                    unsafe {
                        let file = open(
                            CString::from_str(
                                path_buf
                                    .as_os_str()
                                    .to_str()
                                    .expect(UNEXPECTED_BEHAVIOR_MSG),
                            )?
                            .as_ptr(),
                            flags,
                            0o644,
                        );
                        dup2(file, redirect.fd);
                        close(file)
                    };
                }
            }
        }

        let result = exec();

        for (fd, saved) in saved_fds {
            unsafe {
                dup2(saved, fd);
                close(saved)
            };
        }

        result
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
                    redirects: vec![],
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

    fn execute(&mut self, output_buf: &mut Vec<u8>, in_pipeline: bool) -> Result<Option<ExitCode>> {
        for i in 0..self.args.len() {
            if self.args[i].contains("~") {
                let new = self.args[i].replace("~", Self::get_home()?.as_str());
                self.args[i] = new;
            }
        }
        match self.kind {
            Supported::Echo => {
                self.with_redirects(|| {
                    let mut stdout = stdout().lock();

                    for arg in &self.args {
                        stdout.write_all(arg.as_bytes())?;
                        stdout.write_all(b" ")?;
                    }
                    stdout.write_all(b"\n")?;

                    Ok(())
                })?;

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
                if in_pipeline {
                    self.exec_from_execve()?;
                    Ok(None)
                } else {
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
                    Self::with_redirects(self, || {
                        writeln!(
                            output_buf,
                            "cd: {}: No such file or directory",
                            path.display()
                        )?;

                        Ok(())
                    })?
                }

                Ok(None)
            }
        }
    }

    fn exec_from_execve(&self) -> Result<()> {
        let c_command = CString::new(
            self.path
                .as_ref()
                .expect(UNEXPECTED_BEHAVIOR_MSG)
                .to_str()
                .context(UNEXPECTED_BEHAVIOR_MSG)?,
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

        //dbg!(&c_command);
        //dbg!(&c_args);
        ////dbg!(&c_env);

        self.with_redirects(|| {
            nix::unistd::execve(&c_command, &c_args, &c_env).unwrap();
            unreachable!()
        })?;

        Ok(())
    }

    fn get_cwd() -> Result<String> {
        Ok(env::current_dir()?
            .to_str()
            .context(UNEXPECTED_BEHAVIOR_MSG)?
            .to_string())
    }

    fn get_home() -> Result<String> {
        env::var("HOME").context(UNEXPECTED_BEHAVIOR_MSG)
    }
}
