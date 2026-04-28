mod runtime;
mod intrinsics;
mod error;

use wasm_bindgen::prelude::*;
use std::rc::Rc;
use std::cell::RefCell;
use lam_parser::parse_source;
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

    let mut last_value = String::new();
    let nodes = parse_source(&stripped);
    for node in nodes {
        match rt.exec(&node, &env) {
            Ok(value) => last_value = value.to_string(),
            Err(e) => return format!("Error: {}", e.msg),
        }
    }

    last_value
}