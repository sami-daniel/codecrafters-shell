#[allow(unused_imports)]
use std::io::{self, Write};

fn main() {
    loop {
        print!("$ ");
        io::stdout().flush().unwrap();
        
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        match input {
            _ => {    
                println!("{}: command not found", &input.trim())
            } 
        }
    }
}
