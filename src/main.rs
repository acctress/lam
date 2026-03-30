use std::io::{self, Write, BufRead, stdin};
use crate::parser::Parser;
use crate::runtime::{Env, Runtime};

mod parser;
mod runtime;
mod intrinsics;

const RESET: &str = "\x1b[0m";
const RED: &str = "\x1b[38;5;211m";
const CYAN: &str = "\x1b[38;5;159m";
const GREEN: &str = "\x1b[38;5;158m";
const YELLOW: &str = "\x1b[38;5;228m";
const MAGENTA: &str = "\x1b[38;5;219m";

fn main() {
    let mut env = Env::new();
    let rt = Runtime::new();

    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 {
        let src = std::fs::read_to_string(&args[1]).expect("failed to read file");
        let stripped: String = src.lines()
            .map(|l| l.trim())
            .filter(|l| !l.is_empty() && !l.starts_with("#"))
            .collect::<Vec<_>>()
            .join(" ");

        let mut parser = Parser::new(&stripped);
        while parser.got_tokens() {
            let node = parser.parse_top_level();
            rt.exec(node, &mut env);
        }
    } else {
        std::panic::set_hook(Box::new(|_| {}));

        loop {
            print!("{MAGENTA}λ{RESET} ");
            io::stdout().flush().unwrap();

            let mut l = String::new();
            if stdin().lock().read_line(&mut l).unwrap() == 0 {
                break;
            }

            let input = l.trim();
            if input.is_empty() { continue; }

            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                let mut parser = Parser::new(input);
                let node = parser.parse();
                rt.exec(node, &mut env)
            }));

            match result {
                Ok(value) => match &value {
                    runtime::Value::Num(_) => println!("{CYAN}→ {value}{RESET}"),
                    runtime::Value::Str(_) => println!("{GREEN}→ {value}{RESET}"),
                    runtime::Value::List(_) => println!("{YELLOW}→ {value}{RESET}"),
                    runtime::Value::Func(_) | runtime::Value::Nil => {},
                },
                Err(e) => {
                    let msg = if let Some(s) = e.downcast_ref::<String>() {
                        s.as_str()
                    } else if let Some(s) = e.downcast_ref::<&str>() {
                        s
                    } else {
                        "unknown error"
                    };

                    println!("{RED}error: {msg}\x1b[0m");
                }
            }
        }
    }
}