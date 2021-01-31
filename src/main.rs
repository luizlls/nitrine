use nitrine::{Source, parser::parse};

use std::path::PathBuf;
use std::fs;
use std::io::{stdin, stdout, Write};

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    if args.len() <= 1 {
        repl();
    } else {
        file(args.get(1).unwrap().to_string());
    }
}

fn file(file: String) {
    let path = PathBuf::from(file);
    let content = fs::read_to_string(&path).expect("Couldn't open the file");
    let source  = Source::new(&content, path);
    exec(source);
}

fn repl() {
    println!("nitrine v{}", env!("CARGO_PKG_VERSION"));

    let mut input = String::new();

    while let Ok(line) = read_line() {
        if !line.is_empty() {
            input.push_str(&line);
            input.push('\n');
        } else {
            exec(Source::source(&input));
            input.clear();
        }
    }
}

fn read_line() -> Result<String, ()> {
    let mut line = String::new();
    print!(">>>>");
    stdout().flush().unwrap();
    match stdin().read_line(&mut line) {
        Ok(_) => {
            Ok(line.trim_end().to_string())
        }
        _ => Err(()),
    }
}

fn exec(source: Source) {
    match parse(&source) {
        Ok(ast) => {
            println!("{:#?}", ast);
        }
        Err(error) => {
            eprintln!("{}", error);
        }
    }
}
