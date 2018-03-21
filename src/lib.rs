#[macro_use]
extern crate failure;
#[macro_use]
extern crate lazy_static;
extern crate regex;

mod scanner;

use std::env;
use failure::Error;
use std::fs::File;
use std::iter::Iterator;
use std::io;
use std::io::prelude::*;

pub fn run() -> Result<(), Error> {
    let args: Vec<_> = env::args().collect();
    match args.len() {
        0 | 1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => Err(format_err!("Invalid usage\nCorrect usage: rox [script]")),
    }
}

pub fn run_file(path: &str) -> Result<(), Error> {
    let mut file = File::open(path)?;
    let mut code = String::new();
    file.read_to_string(&mut code)?;
    run_str(&code)
}

pub fn run_prompt() -> Result<(), Error> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut line = String::new();
    loop {
        print!("> ");
        stdout.flush()?;
        stdin.read_line(&mut line)?;
        match run_str(&line) {
            Ok(_) => (),
            Err(e) => println!("{}", e),
        }
        line.clear();
    }
}

pub fn run_str(code: &str) -> Result<(), Error> {
    let mut scanner = scanner::Scanner::new(code);
    for token in scanner {
        match token {
            Ok(t) => println!("{}", t),
            Err(e) => println!("{}", e),
        }
    }
    return Ok(());
}

pub fn error(line: usize, msg: &str) -> Error {
    format_err!("[Line {}]: Error: {}", line, msg)
}