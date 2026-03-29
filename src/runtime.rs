use std::collections::HashMap;
use crate::parser::Node;

pub type IntrinsicFn = fn(&Runtime, Vec<Value>) -> Value;

pub struct Intrinsic {
    pub name: &'static str,
    pub arity: usize,
    pub func: IntrinsicFn,
}

inventory::collect!(Intrinsic);

pub struct Runtime {
    intrinsics: HashMap<String, (IntrinsicFn, usize)>,
}

#[derive(Clone)]
pub enum Value {
    Num(f64),
    Str(String),
    Char(char),
    List(Vec<Value>),
    Func(Box<LamFunc>),
}

#[derive(Clone)]
pub enum LamFunc {
    Partial { op: String, arg: Value },
    Intrinsic { name: String, args: Vec<Value>, arity: usize },
    Composition { outer: Box<LamFunc>, inner: Box<LamFunc> },
}

impl Runtime {
    pub fn new() -> Self {
        let mut rt = Runtime { intrinsics: HashMap::new() };
        for i in inventory::iter::<Intrinsic> {
            rt.intrinsics.insert(i.name.to_string(), (i.func, i.arity));
        }

        rt
    }

    pub fn exec(&self, node: Node) -> Value {
        self.eval(node)
    }

    pub fn eval(&self, node: Node) -> Value {
        match node {
            Node::Literal(n) => Value::Num(n),
            Node::Atom(s) => self.lookup_intrinsic(&s),
            Node::List(items) => Value::List(items.into_iter().map(|n| self.eval(n)).collect()),
            Node::Partial { op, arg } => Value::Func(Box::new(LamFunc::Partial { op, arg: self.eval(*arg) })),
            Node::Application { func, arg } => self.apply(self.eval(*func), self.eval(*arg)),
        }
    }

    pub(crate) fn apply(&self, func: Value, arg: Value) -> Value {
        match func {
            Value::Func(f) => match *f {
                LamFunc::Partial { op, arg: left } => {
                    self.apply_op(&op, left, arg)
                }

                LamFunc::Intrinsic { name, mut args, arity } => {
                    args.push(arg);

                    if args.len() == arity {
                        self.call_intrinsic(&name, args)
                    } else {
                        /* auto curry ! */
                        Value::Func(Box::new(LamFunc::Intrinsic { name, args, arity }))
                    }
                }

                LamFunc::Composition { outer, inner } => {
                    let m = self.apply(Value::Func(Box::from(*inner)), arg);
                    self.apply(Value::Func(Box::from(*outer)), m)
                }
            }

            _ => panic!("tried to apply a non function!")
        }
    }

    fn apply_op(&self, op: &str, left: Value, right: Value) -> Value {
        match (op, left, right) {
            ("+", Value::Num(a), Value::Num(b)) => Value::Num(a + b),
            ("-", Value::Num(a), Value::Num(b)) => Value::Num(a - b),
            ("*", Value::Num(a), Value::Num(b)) => Value::Num(a * b),
            ("/", Value::Num(a), Value::Num(b)) => Value::Num(a / b),
            _ => panic!("type mismatch when applying operator")
        }
    }

    fn call_intrinsic(&self, name: &str, args: Vec<Value>) -> Value {
        match self.intrinsics.get(name) {
            Some((func, _)) => func(self, args),
            None => panic!("unknown intrinsic '{}'", name)
        }
    }

    fn lookup_intrinsic(&self, name: &str) -> Value {
        match self.intrinsics.get(name) {
            Some((_, arity)) => Value::Func(Box::new(LamFunc::Intrinsic {
                name: name.to_string(),
                args: vec![],
                arity: *arity
            })),
            None => panic!("unknown symbol '{}'", name)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;
    use crate::runtime::Runtime;
    use super::*;

    #[test]
    fn eval_partial_application() {
        let mut p = Parser::new("(+ 5) 3");
        let node = p.parse();
        let rt = Runtime::new();
        let result = rt.exec(node);

        match result {
            Value::Num(n) => assert_eq!(8.0, n),
            _ => panic!("expected number"),
        }
    }

    #[test]
    fn eval_map() {
        let mut p = Parser::new("map (+ 1) [1, 2, 3]");
        let node = p.parse();
        let rt = Runtime::new();
        let result = rt.exec(node);

        match result {
            Value::List(items) => {
                assert_eq!(3, items.len());
                match (&items[0], &items[1], &items[2]) {
                    (Value::Num(a), Value::Num(b), Value::Num(c)) => {
                        assert_eq!(2.0, *a);
                        assert_eq!(3.0, *b);
                        assert_eq!(4.0, *c);
                    }
                    _ => panic!("expected numbers"),
                }
            }
            _ => panic!("expected list"),
        }
    }
}