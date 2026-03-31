use std::cell::RefCell;
use std::io::{self, Write, BufRead, stdin};
use std::rc::Rc;
use ariadne::{Label, Report, ReportKind, Source};
use crate::error::LamError;
use crate::parser::Parser;
use crate::runtime::{Env, Runtime, Value};

mod parser;
mod runtime;
mod intrinsics;
mod error;

const RESET: &str = "\x1b[0m";
const RED: &str = "\x1b[38;5;211m";
const CYAN: &str = "\x1b[38;5;159m";
const GREEN: &str = "\x1b[38;5;158m";
const YELLOW: &str = "\x1b[38;5;228m";
const MAGENTA: &str = "\x1b[38;5;219m";

fn main() {
    let env = Rc::new(RefCell::new(Env::new()));
    let rt = Runtime::new();

    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 {
        let src = std::fs::read_to_string(&args[1]).expect("failed to read file");
        let stripped: String = src.lines()
            .map(str::trim)
            .filter(|l| !l.is_empty() && !l.starts_with('#'))
            .collect::<Vec<_>>()
            .join(" ");

        let mut parser = Parser::new(&stripped);
        while parser.got_tokens() {
            match parser.parse_top_level().and_then(|node| rt.exec(node, &env)) {
                Ok(_) => {},
                Err(e) => {
                    print_lam_error(&e, src.as_str());
                    std::process::exit(1);
                }
            }
        }
    } else {
        loop {
            print!("{MAGENTA}λ{RESET} ");
            io::stdout().flush().unwrap();

            let mut l = String::new();
            if stdin().lock().read_line(&mut l).unwrap() == 0 {
                break;
            }

            let input = l.trim();
            if input.is_empty() { continue; }

            let mut parser = Parser::new(input);
            match parser.parse().and_then(|node| rt.exec(node, &env)) {
                Ok(value) => match &value {
                    Value::Num(_) => println!("{CYAN}→ {value}{RESET}"),
                    Value::Str(_) => println!("{GREEN}→ {value}{RESET}"),
                    Value::List(_) => println!("{YELLOW}→ {value}{RESET}"),
                    Value::Func(_) | Value::Nil => {},
                },
                Err(e) => print_lam_error(&e, input),
            }
        }
    }
}

fn print_lam_error(err: &LamError, input: &str) {
    match err.span {
        Some((start, end)) => {
            Report::build(ReportKind::Error, ("input", start..end))
                .with_message(&err.msg)
                .with_label(
                    Label::new(("input", start..end))
                        .with_message(&err.msg)
                )
                .finish()
                .eprint(("input", Source::from(input)))
                .unwrap();
        }
        None => {
            eprintln!("{RED}Error: {}{RESET}", err.msg);
        }
    }
}