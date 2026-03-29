#![allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]

use crate::runtime::{Runtime, Intrinsic, Value, LamFunc};

inventory::submit!(Intrinsic { name: "+", arity: 2, func: add });
fn add(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a + b),
        _ => panic!("+ expects two numbers")
    }
}

inventory::submit!(Intrinsic { name: "-", arity: 2, func: sub });
fn sub(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a - b),
        _ => panic!("- expects two numbers")
    }
}

inventory::submit!(Intrinsic { name: "*", arity: 2, func: mul });
fn mul(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a * b),
        _ => panic!("* expects two numbers")
    }
}

inventory::submit!(Intrinsic { name: "/", arity: 2, func: div });
fn div(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a / b),
        _ => panic!("/ expects two numbers")
    }
}

inventory::submit!(Intrinsic { name: "==", arity: 2, func: eq });
fn eq(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(f64::from(a == b)),
        _ => panic!("== expects two numbers")
    }
}

inventory::submit!(Intrinsic { name: ">", arity: 2, func: gt });
fn gt(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(f64::from(a > b)),
        _ => panic!("> expects two numbers")
    }
}

inventory::submit!(Intrinsic { name: "<", arity: 2, func: lt });
fn lt(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(f64::from(a < b)),
        _ => panic!("< expects two numbers")
    }
}

inventory::submit!(Intrinsic { name: ">=", arity: 2, func: gte });
fn gte(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(f64::from(a >= b)),
        _ => panic!(">= expects two numbers")
    }
}

inventory::submit!(Intrinsic { name: "<=", arity: 2, func: lte });
fn lte(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(f64::from(a <= b)),
        _ => panic!("<= expects two numbers")
    }
}

inventory::submit!(Intrinsic { name: "!=", arity: 2, func: ne });
fn ne(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(f64::from(a != b)),
        _ => panic!("!= expects two numbers")
    }
}

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
fn putln(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let str = args.pop().unwrap();
    println!("{str}");

    Value::Nil
}

inventory::submit!(Intrinsic { name: "compose", arity: 2, func: compose });
fn compose(_rt: &Runtime, mut args: Vec<Value>) -> Value {
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
fn list(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let count = args.pop().unwrap();
    let value = args.pop().unwrap();

    match count {
        Value::Num(n) => Value::List(vec![value; n as usize]),
        _ => panic!("list expects count to be a number"),
    }
}

inventory::submit!(Intrinsic { name: "zip", arity: 2, func: zip });
fn zip(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();

    let Value::List(a_list) = a else { panic!("zip expects two lists") };
    let Value::List(b_list) = b else { panic!("zip expects two lists") };

    Value::List(a_list.into_iter().zip(b_list).map(|(a, b)| Value::List(vec![a, b])).collect())
}

inventory::submit!(Intrinsic { name: "fold", arity: 3, func: fold });
fn fold(rt: &Runtime, mut args: Vec<Value>) -> Value {
    let list = args.pop().unwrap();
    let func = args.pop().unwrap();
    let init = args.pop().unwrap();

    let Value::List(list_value) = list else { panic!("fold expects a list") };

    list_value.into_iter().fold(init, |acc, x| rt.apply(rt.apply(func.clone(), acc), x))
}

inventory::submit!(Intrinsic { name: "concat", arity: 2, func: concat });
fn concat(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let Value::Str(a) = args.pop().unwrap() else { panic!("concat expects two strings") };
    let Value::Str(b) = args.pop().unwrap() else { panic!("concat expects two strings") };
    Value::Str(format!("{b}{a}"))
}

inventory::submit!(Intrinsic { name: "append", arity: 2, func: append });
fn append(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let Value::List(b) = args.pop().unwrap() else { panic!("append expects two lists") };
    let Value::List(mut a) = args.pop().unwrap() else { panic!("append expects two lists") };
    a.extend(b);
    Value::List(a)
}

inventory::submit!(Intrinsic { name: "len", arity: 1, func: len });
fn len(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    match args.pop().unwrap() {
        Value::Str(s) => Value::Num(s.len() as f64),
        Value::List(l) => Value::Num(l.len() as f64),
        _ => panic!("len expects a string or a list")
    }
}

inventory::submit!(Intrinsic { name: "chars", arity: 1, func: chars });
fn chars(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let Value::Str(s) = args.pop().unwrap() else { panic!("chars expects one string") };
    Value::List(s.chars().map(|c| Value::Num(f64::from(c as u32))).collect())
}

inventory::submit!(Intrinsic { name: "str", arity: 1, func: str });
fn str(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let Value::List(nums) = args.pop().unwrap() else { panic!("str expects one list") };
    Value::Str(nums.into_iter().map(|c| {
        let Value::Num(n) = c else { panic!("str expects a list of numbers") };
        char::from(n as u8)
    }).collect())
}

inventory::submit!(Intrinsic { name: "split", arity: 2, func: split });
fn split(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let Value::Str(string) = args.pop().unwrap() else { panic!("split expects two strings") };
    let Value::Str(delim) = args.pop().unwrap() else { panic!("split expects two strings") };
    Value::List(string.split(&delim).map(|i| { Value::Str(i.to_string()) }).collect())
}

inventory::submit!(Intrinsic { name: "head", arity: 1, func: head });
fn head(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let Value::List(list) = args.pop().unwrap() else { panic!("head expects one list") };
    if list.is_empty() { return Value::Nil }
    list.first().unwrap().clone()
}

inventory::submit!(Intrinsic { name: "tail", arity: 1, func: tail });
fn tail(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let Value::List(list) = args.pop().unwrap() else { panic!("tail expects one list") };
    if list.is_empty() { return Value::Nil }
    Value::List(list[1..].to_vec())
}

inventory::submit!(Intrinsic { name: "reverse", arity: 1, func: reverse });
fn reverse(_rt: &Runtime, mut args: Vec<Value>) -> Value {
    let Value::List(mut list) = args.pop().unwrap() else { panic!("tail expects one list") };
    if list.is_empty() { return Value::Nil }
    list.reverse();
    Value::List(list)
}

inventory::submit!(Intrinsic { name: "filter", arity: 2, func: filter });
fn filter(rt: &Runtime, mut args: Vec<Value>) -> Value {
    let Value::List(list) = args.pop().unwrap() else { panic!("filter needs a list") };
    let pred = args.pop().unwrap();
    Value::List(list.into_iter().filter(|item| {
        matches!(rt.apply(pred.clone(), item.clone()), Value::Num(n) if n != 0f64)
    }).collect())
}