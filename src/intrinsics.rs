use crate::error::{LamError, LamResult};
use crate::runtime::{Runtime, Intrinsic, Value, LamFunc};

inventory::submit!(Intrinsic { name: "+", arity: 2, func: add });
fn add(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a + b)),
        _ => Err(LamError::new("+ expects two numbers")),
    }
}

inventory::submit!(Intrinsic { name: "-", arity: 2, func: sub });
fn sub(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a - b)),
        _ => Err(LamError::new("- expects two numbers")),
    }
}

inventory::submit!(Intrinsic { name: "*", arity: 2, func: mul });
fn mul(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a * b)),
        _ => Err(LamError::new("* expects two numbers")),
    }
}

inventory::submit!(Intrinsic { name: "/", arity: 2, func: div });
fn div(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a / b)),
        _ => Err(LamError::new("/ expects two numbers")),
    }
}

inventory::submit!(Intrinsic { name: "==", arity: 2, func: eq });
fn eq(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Ok(Value::Num(f64::from(a == b))),
        (Value::Str(a), Value::Str(b)) => Ok(Value::Num(f64::from(a == b))),
        _ => Err(LamError::new("== expects two values of the same type")),
    }
}

inventory::submit!(Intrinsic { name: ">", arity: 2, func: gt });
fn gt(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Ok(Value::Num(f64::from(a > b))),
        _ => Err(LamError::new("> expects two numbers")),
    }
}

inventory::submit!(Intrinsic { name: "<", arity: 2, func: lt });
fn lt(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Ok(Value::Num(f64::from(a < b))),
        _ => Err(LamError::new("< expects two numbers")),
    }
}

inventory::submit!(Intrinsic { name: ">=", arity: 2, func: gte });
fn gte(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Ok(Value::Num(f64::from(a >= b))),
        _ => Err(LamError::new(">= expects two numbers")),
    }
}

inventory::submit!(Intrinsic { name: "<=", arity: 2, func: lte });
fn lte(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Ok(Value::Num(f64::from(a <= b))),
        _ => Err(LamError::new("<= expects two numbers")),
    }
}

inventory::submit!(Intrinsic { name: "!=", arity: 2, func: ne });
fn ne(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Ok(Value::Num(f64::from(a != b))),
        (Value::Str(a), Value::Str(b)) => Ok(Value::Num(f64::from(a != b))),
        _ => Err(LamError::new("!= expects two values of the same type")),
    }
}

inventory::submit!(Intrinsic { name: "map", arity: 2, func: map });
fn map(rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let list = args.pop().unwrap();
    let func = args.pop().unwrap();
    match list {
        Value::List(items) => {
            let results: LamResult<Vec<Value>> = items.into_iter()
                .map(|i| rt.apply(func.clone(), i))
                .collect();
            Ok(Value::List(results?))
        }
        _ => Err(LamError::new("map expects a list as second argument")),
    }
}

inventory::submit!(Intrinsic { name: "putln", arity: 1, func: putln });
fn putln(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let s = args.pop().unwrap();
    println!("{s}");
    Ok(Value::Nil)
}

inventory::submit!(Intrinsic { name: "compose", arity: 2, func: compose });
fn compose(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let inner = args.pop().unwrap();
    let outer = args.pop().unwrap();
    match (outer, inner) {
        (Value::Func(f), Value::Func(g)) => Ok(Value::Func(Box::new(LamFunc::Composition {
            outer: f,
            inner: g,
        }))),
        _ => Err(LamError::new("compose expects two functions")),
    }
}

inventory::submit!(Intrinsic { name: "list", arity: 2, func: list });
fn list(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let count = args.pop().unwrap();
    let value = args.pop().unwrap();
    match count {
        Value::Num(n) => Ok(Value::List(vec![value; n as usize])),
        _ => Err(LamError::new("list expects count to be a number")),
    }
}

inventory::submit!(Intrinsic { name: "zip", arity: 2, func: zip });
fn zip(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    let Value::List(a_list) = a else { return Err(LamError::new("zip expects two lists")) };
    let Value::List(b_list) = b else { return Err(LamError::new("zip expects two lists")) };
    Ok(Value::List(a_list.into_iter().zip(b_list).map(|(a, b)| Value::List(vec![a, b])).collect()))
}

inventory::submit!(Intrinsic { name: "fold", arity: 3, func: fold });
fn fold(rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let list = args.pop().unwrap();
    let func = args.pop().unwrap();
    let init = args.pop().unwrap();
    let Value::List(items) = list else { return Err(LamError::new("fold expects a list")) };
    items.into_iter().try_fold(init, |acc, x| {
        let partial = rt.apply(func.clone(), acc)?;
        rt.apply(partial, x)
    })
}

inventory::submit!(Intrinsic { name: "concat", arity: 2, func: concat });
fn concat(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let Value::Str(a) = args.pop().unwrap() else { return Err(LamError::new("concat expects two strings")) };
    let Value::Str(b) = args.pop().unwrap() else { return Err(LamError::new("concat expects two strings")) };
    Ok(Value::Str(format!("{b}{a}")))
}

inventory::submit!(Intrinsic { name: "append", arity: 2, func: append });
fn append(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let Value::List(b) = args.pop().unwrap() else { return Err(LamError::new("append expects two lists")) };
    let Value::List(mut a) = args.pop().unwrap() else { return Err(LamError::new("append expects two lists")) };
    a.extend(b);
    Ok(Value::List(a))
}

inventory::submit!(Intrinsic { name: "len", arity: 1, func: len });
fn len(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    match args.pop().unwrap() {
        Value::Str(s) => Ok(Value::Num(s.len() as f64)),
        Value::List(l) => Ok(Value::Num(l.len() as f64)),
        _ => Err(LamError::new("len expects a string or a list")),
    }
}

inventory::submit!(Intrinsic { name: "chars", arity: 1, func: chars });
fn chars(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let Value::Str(s) = args.pop().unwrap() else { return Err(LamError::new("chars expects one string")) };
    Ok(Value::List(s.chars().map(|c| Value::Num(f64::from(c as u32))).collect()))
}

inventory::submit!(Intrinsic { name: "str", arity: 1, func: str });
fn str(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let Value::List(nums) = args.pop().unwrap() else { return Err(LamError::new("str expects one list")) };
    let mut result = String::new();
    for c in nums {
        let Value::Num(n) = c else { return Err(LamError::new("str expects a list of numbers")) };
        result.push(char::from(n as u8));
    }
    Ok(Value::Str(result))
}

inventory::submit!(Intrinsic { name: "split", arity: 2, func: split });
fn split(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let Value::Str(string) = args.pop().unwrap() else { return Err(LamError::new("split expects two strings")) };
    let Value::Str(delim) = args.pop().unwrap() else { return Err(LamError::new("split expects two strings")) };
    Ok(Value::List(string.split(&delim).map(|i| Value::Str(i.to_string())).collect()))
}

inventory::submit!(Intrinsic { name: "head", arity: 1, func: head });
fn head(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let Value::List(list) = args.pop().unwrap() else { return Err(LamError::new("head expects one list")) };
    if list.is_empty() { return Ok(Value::List(vec![])) }
    Ok(list.first().unwrap().clone())
}

inventory::submit!(Intrinsic { name: "tail", arity: 1, func: tail });
fn tail(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let Value::List(list) = args.pop().unwrap() else { return Err(LamError::new("tail expects one list")) };
    if list.is_empty() { return Ok(Value::List(vec![])) }
    Ok(Value::List(list[1..].to_vec()))
}

inventory::submit!(Intrinsic { name: "reverse", arity: 1, func: reverse });
fn reverse(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let Value::List(mut list) = args.pop().unwrap() else { return Err(LamError::new("reverse expects one list")) };
    if list.is_empty() { return Ok(Value::List(vec![])) }
    list.reverse();
    Ok(Value::List(list))
}

inventory::submit!(Intrinsic { name: "filter", arity: 2, func: filter });
fn filter(rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let Value::List(list) = args.pop().unwrap() else { return Err(LamError::new("filter needs a list")) };
    let pred = args.pop().unwrap();
    let mut result = vec![];
    for item in list {
        let v = rt.apply(pred.clone(), item.clone())?;
        if matches!(v, Value::Num(n) if n != 0f64) {
            result.push(item);
        }
    }
    Ok(Value::List(result))
}

inventory::submit!(Intrinsic { name: "drop", arity: 2, func: drop_list });
fn drop_list(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let Value::List(list) = args.pop().unwrap() else { return Err(LamError::new("drop expects a list")) };
    let Value::Num(n) = args.pop().unwrap() else { return Err(LamError::new("drop expects a number")) };
    let n = n as usize;
    if n >= list.len() { return Ok(Value::List(vec![])) }
    Ok(Value::List(list[n..].to_vec()))
}

inventory::submit!(Intrinsic { name: "take", arity: 2, func: take });
fn take(_rt: &Runtime, mut args: Vec<Value>) -> LamResult<Value> {
    let Value::List(list) = args.pop().unwrap() else { return Err(LamError::new("take expects a list")) };
    let Value::Num(n) = args.pop().unwrap() else { return Err(LamError::new("take expects a number")) };
    let n = (n as usize).min(list.len());
    Ok(Value::List(list[..n].to_vec()))
}