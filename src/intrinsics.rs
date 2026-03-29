use crate::runtime::{Intrinsic, Value};

inventory::submit!(Intrinsic { name: "map", arity: 2, func: map });
fn map(args: Vec<Value>) -> Value {
    Value::Num(0.0)
}