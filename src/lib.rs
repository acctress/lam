use wasm_bindgen::prelude::*;
use std::rc::Rc;
use std::cell::RefCell;

mod parser;
mod runtime;
mod intrinsics;
mod error;

use parser::Parser;
use runtime::{Env, Runtime};

#[wasm_bindgen]
pub fn eval(input: &str) -> String {
    let rt = Runtime::new();
    let env = Rc::new(RefCell::new(Env::new()));

    let stripped: String = input.lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .collect::<Vec<_>>()
        .join(" ");

    let mut parser = Parser::new(&stripped);
    let mut last_value = String::new();

    while parser.got_tokens() {
        match parser.parse_top_level() {
            Ok(v) => match rt.exec(&v, &env) {
                Ok(value) => last_value = value.to_string(),
                Err(e) => return format!("Error: {}", e.msg),
            },
            Err(e) => return format!("Error: {}", e.msg),
        }
    }

    last_value
}