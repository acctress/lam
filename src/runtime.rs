use std::collections::HashMap;
use std::fmt::Formatter;
use crate::parser::{Node, Parser, Pattern};

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
    List(Vec<Value>),
    Func(Box<LamFunc>),
}

#[derive(Clone, Debug)]
pub enum LamFunc {
    Intrinsic { name: String, args: Vec<Value>, arity: usize },
    Composition { outer: Box<LamFunc>, inner: Box<LamFunc> },
    UDef { name: String, o_params: Vec<String>, params: Vec<String>, body: Node, env: Env },     /* user defined */
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
                    o_params: params.clone(),
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
                    o_params: params.clone(),
                    params,
                    body: *body,
                    env: env.clone()
                }))
            },
            Node::Match { expr, arms } => {
                let val = self.eval(*expr, env);
                for arm in arms {
                    if let Some(bindings) = self.match_pattern(&arm.pattern, &val) {
                        let mut c = env.child();
                        for (k, v) in bindings {
                            c.set(k, v);
                        }

                        if let Some(g) = arm.guard {
                            match self.eval(*g, &mut c) {
                                Value::Num(n) if n != 0f64 => return self.eval(*arm.body, &mut c),
                                _ => continue,
                            }
                        }

                        return self.eval(*arm.body, &mut c);
                    }
                }

                panic!("non exhaustive match")
            },
            Node::UseModule { path } => {
                let p = if path.ends_with(".lam") {
                    path.clone()
                } else {
                    let mut s = path.clone();
                    s.push_str(".lam");
                    s
                };

                let src = std::fs::read_to_string(p).expect("failed to read file");
                let stripped: String = src.lines()
                    .map(|l| l.trim())
                    .filter(|l| !l.is_empty() && !l.starts_with("#"))
                    .collect::<Vec<_>>()
                    .join(" ");

                let mut parser = Parser::new(&stripped);
                while parser.got_tokens() {
                    let node = parser.parse_top_level();
                    self.exec(node, env);
                }

                Value::Nil
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

                LamFunc::UDef { name, o_params, params, body, env: closed } => {
                    let mut child = closed.child();
                    child.set(params[0].clone(), arg);
                    child.set(name.clone(), Value::Func(Box::new(LamFunc::UDef {
                        name: name.clone(),
                        o_params: o_params.clone(),
                        params: o_params.clone(),
                        body: body.clone(),
                        env: closed.clone()
                    })));

                    if params.len() == 1 {
                        self.eval(body, &mut child)
                    } else {
                        /* auto curry! */
                        Value::Func(Box::new(LamFunc::UDef {
                            name,
                            o_params,
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

    fn match_pattern(&self, pattern: &Pattern, value: &Value) -> Option<HashMap<String, Value>> {
        let mut bindings = HashMap::new();
        match (pattern, value) {
            (Pattern::Wildcard, _) => Some(bindings),
            (Pattern::Literal(n), Value::Num(v)) if n == v => Some(bindings),
            (Pattern::StringLit(l), Value::Str(s)) if l == s => Some(bindings),
            (Pattern::Var(n), _) => {
                bindings.insert(n.clone(), value.clone());
                Some(bindings)
            },
            (Pattern::List(patterns, rest), Value::List(items)) => {
                if items.len() < patterns.len() { return None; }
                for (p, v) in patterns.iter().zip(items.iter()) {
                    let s = self.match_pattern(p, v)?;
                    bindings.extend(s);
                }
                if let Some(r) = rest {
                    bindings.insert(r.clone(), Value::List(items[patterns.len()..].to_vec()));
                } else if items.len() != patterns.len() {
                    return None;
                }
                Some(bindings)
            }
            _ => None,
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