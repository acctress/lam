use crate::runtime::{Runtime, Intrinsic, Value, LamFunc};

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

inventory::submit!(Intrinsic { name: "compose", arity: 2, func: compose });
fn compose(rt: &Runtime, mut args: Vec<Value>) -> Value {
    let inner = args.pop().unwrap();
    let outer = args.pop().unwrap();

    /* g(f(x)) */
    match (outer, inner) {
        (Value::Func(f), Value::Func(g)) => Value::Func(Box::new(LamFunc::Composition {
            outer: f,
            inner: g,
        })),
        _ => panic!("compose expects two functions!")
    }
}

inventory::submit!(Intrinsic { name: "list", arity: 2, func: list });
fn list(rt: &Runtime, mut args: Vec<Value>) -> Value {
    let count = args.pop().unwrap();
    let value = args.pop().unwrap();

    match count {
        Value::Num(n) => Value::List(vec![value; n as usize]),
        _ => panic!("list expects count to be a number"),
    }
}

inventory::submit!(Intrinsic { name: "zip", arity: 2, func: zip });
fn zip(rt: &Runtime, mut args: Vec<Value>) -> Value {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();

    let a_list = match a {
        Value::List(items) => items,
        _ => panic!("zip expects two lists")
    };

    let b_list = match b {
        Value::List(items) => items,
        _ => panic!("zip expects two lists")
    };

    Value::List(a_list.into_iter().zip(b_list).map(|(a, b)| Value::List(vec![a, b])).collect())
}

inventory::submit!(Intrinsic { name: "fold", arity: 3, func: fold });
fn fold(rt: &Runtime, mut args: Vec<Value>) -> Value {
    let list = args.pop().unwrap();
    let func = args.pop().unwrap();
    let init = args.pop().unwrap();

    let list_value = match list {
        Value::List(items) => items,
        _ => panic!("fold expects a list")
    };

    list_value.into_iter().fold(init, |acc, x| rt.apply(rt.apply(func.clone(), acc), x))
}