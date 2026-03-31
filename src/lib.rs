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
        match parser.parse_top_level().and_then(|node| rt.exec(node, &env)) {
            Ok(v) => last_value = v.to_string(),
            Err(e) => return format!("Error: {}", e.msg),
        }
    }

    last_value
}