use crate::runtime::{Runtime, Intrinsic, Value};

inventory::submit!(Intrinsic { name: "map", arity: 2, func: map });
fn map(rt: &Runtime, mut args: Vec<Value>) -> Value {
    let list = args.pop().unwrap();
    let func = args.pop().unwrap();

    match list {
        Value::List(items) => Value::List(
            items.into_iter().map(|i| rt.apply(func.clone(), i)).collect()
        ),
        _ => panic!("map expects a list as second argument"),
    }
}

inventory::submit!(Intrinsic { name: "putln", arity: 1, func: putln });
fn putln(rt: &Runtime, mut args: Vec<Value>) -> Value {
    let str = args.pop().unwrap();
    println!("{}", str);

    Value::Nil
}