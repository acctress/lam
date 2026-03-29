use std::collections::HashMap;
use std::fmt::Formatter;
use crate::parser::Node;

pub type IntrinsicFn = fn(&Runtime, Vec<Value>) -> Value;

pub struct Intrinsic {
    pub name: &'static str,
    pub arity: usize,
    pub func: IntrinsicFn,
}

inventory::collect!(Intrinsic);

#[derive(Clone, Debug)]
pub struct Env {
    bindings: HashMap<String, Value>,
    parent: Option<Box<Env>>,
}

pub struct Runtime {
    intrinsics: HashMap<String, (IntrinsicFn, usize)>,
}

#[derive(Clone, Debug)]
pub enum Value {
    Nil,
    Num(f64),
    Str(String),
    Char(char),
    List(Vec<Value>),
    Func(Box<LamFunc>),
}

#[derive(Clone, Debug)]
pub enum LamFunc {
    Intrinsic { name: String, args: Vec<Value>, arity: usize },
    Composition { outer: Box<LamFunc>, inner: Box<LamFunc> },
    UDef { name: String, params: Vec<String>, body: Node, env: Env },     /* user defined */
}

impl Runtime {
    pub fn new() -> Self {
        let mut rt = Runtime { intrinsics: HashMap::new() };
        for i in inventory::iter::<Intrinsic> {
            rt.intrinsics.insert(i.name.to_string(), (i.func, i.arity));
        }

        rt
    }

    pub fn exec(&self, node: Node, env: &mut Env) -> Value {
        self.eval(node, env)
    }

    pub fn eval(&self, node: Node, env: &mut Env) -> Value {
        match node {
            Node::Literal(n) => Value::Num(n),
            Node::Atom(s) => {
                if let Some(v) = env.get(&s) {
                    v.clone()
                } else if self.intrinsics.contains_key(&s) {
                    self.lookup_intrinsic(&s)
                } else if s.len() == 1 {
                    Value::Char(s.chars().next().unwrap())
                } else {
                    Value::Str(s)
                }
            },
            Node::List(items) => Value::List(items.into_iter().map(|n| self.eval(n, env)).collect()),
            Node::Partial { op, arg } => {
                let fun = self.eval(Node::Atom(op), env);
                self.apply(fun, self.eval(*arg, env))
            },
            Node::Application { func, arg } => self.apply(self.eval(*func, env), self.eval(*arg, env)),
            Node::Let { name, value, body } => {
                let v = self.eval(*value, env);
                /* each let binding gets it's own scope */
                let mut child = env.child();
                child.set(name, v);
                self.eval(*body, &mut child)
            },
            Node::If { cond, then, els } => {
                match self.eval(*cond, env) {
                    Value::Num(n) if n != 0f64 => self.eval(*then, env),
                    Value::Num(_) => self.eval(*els, env),
                    _ => panic!("if condition must evaluate to a number")
                }
            },
            Node::FnDef { name, params, body } => {
                let f = Value::Func(Box::new(LamFunc::UDef {
                    name: name.clone(), /* forgot about thiz */
                    params,
                    body: *body,
                    env: env.clone(),
                }));
                env.set(name, f);
                Value::Nil
            },
            Node::LambdaDef { params, body } => {
                Value::Func(Box::new(LamFunc::UDef {
                    name: String::new(),
                    params,
                    body: *body,
                    env: env.clone()
                }))
            }
        }
    }

    pub(crate) fn apply(&self, func: Value, arg: Value) -> Value {
        match func {
            Value::Func(f) => match *f {
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
                },

                LamFunc::UDef { name, params, body, env: closed } => {
                    let mut child = closed.child();
                    child.set(params[0].clone(), arg);
                    child.set(name.clone(), Value::Func(Box::new(LamFunc::UDef {
                        name: name.clone(),
                        params: params.clone(),
                        body: body.clone(),
                        env: closed.clone()
                    })));

                    if params.len() == 1 {
                        self.eval(body, &mut child)
                    } else {
                        /* auto curry! */
                        Value::Func(Box::new(LamFunc::UDef {
                            name,
                            params: params[1..].to_vec(),
                            body,
                            env: child
                        }))
                    }
                }
            }

            _ => panic!("tried to apply a non function!")
        }
    }

    fn call_intrinsic(&self, name: &str, args: Vec<Value>) -> Value {
        match self.intrinsics.get(name) {
            Some((func, _)) => func(self, args),
            None => panic!("unknown intrinsic '{name}'")
        }
    }

    fn lookup_intrinsic(&self, name: &str) -> Value {
        match self.intrinsics.get(name) {
            Some((_, arity)) => Value::Func(Box::new(LamFunc::Intrinsic {
                name: name.to_string(),
                args: vec![],
                arity: *arity
            })),
            None => panic!("unknown symbol '{name}'")
        }
    }
}

impl Env {
    pub fn new() -> Self {
        Env { bindings: HashMap::new(), parent: None }
    }

    pub fn child(&self) -> Self {
        Env { bindings: HashMap::new(), parent: Some(Box::new(self.clone())) }
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.bindings.get(name).or_else(|| self.parent.as_ref()?.get(name))
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Num(n) => write!(f, "{n}"),
            Value::Str(s) => write!(f, "\"{s}\""),
            Value::Char(c) => write!(f, "{c}"),
            Value::List(items) => {
                write!(f, "[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{item}")?;
                }
                write!(f, "]")
            }
            Value::Func(_) => write!(f, "<fn>"),
            Value::Nil => write!(f, "")
        }
    }
}