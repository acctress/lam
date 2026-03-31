use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Formatter;
use std::rc::Rc;
use include_dir::{include_dir, Dir};
use crate::error::{LamError, LamResult};
use crate::parser::{Node, Parser, Pattern};

static STD_DIR: Dir = include_dir!("$CARGO_MANIFEST_DIR/std");

pub type IntrinsicFn = fn(&Runtime, Vec<Value>) -> LamResult<Value>;

pub struct Intrinsic {
    pub name: &'static str,
    pub arity: usize,
    pub func: IntrinsicFn,
}

inventory::collect!(Intrinsic);

#[derive(Clone, Debug)]
pub struct Env {
    bindings: HashMap<String, Value>,
    parent: Option<Rc<RefCell<Env>>>,
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
    UDef { name: String, o_params: Vec<String>, params: Vec<String>, body: Node, env: Rc<RefCell<Env>> },     /* user defined */
}

impl Runtime {
    pub fn new() -> Self {
        let mut rt = Runtime { intrinsics: HashMap::new() };
        for i in inventory::iter::<Intrinsic> {
            rt.intrinsics.insert(i.name.to_string(), (i.func, i.arity));
        }

        rt
    }

    pub fn exec(&self, node: Node, env: &Rc<RefCell<Env>>) -> LamResult<Value> {
        self.eval(node, env)
    }

    pub fn eval(&self, node: Node, env: &Rc<RefCell<Env>>) -> LamResult<Value> {
        match node {
            Node::Literal(n) => Ok(Value::Num(n)),
            Node::Atom(s) => {
                let x = if let Some(v) = env.borrow().get(&s) {
                    v.clone()
                } else if self.intrinsics.contains_key(&s) {
                    self.lookup_intrinsic(&s)?
                } else {
                    Value::Str(s)
                };

                Ok(x)
            },
            Node::List(items) => {
                let v: LamResult<Vec<Value>> = items.into_iter().map(|n| self.eval(n, env)).collect();
                Ok(Value::List(v?))
            },
            Node::Partial { op, arg } => {
                let fun = self.eval(Node::Atom(op), env)?;
                self.apply(fun, self.eval(*arg, env)?)
            },
            Node::Application { func, arg } => self.apply(self.eval(*func, env)?, self.eval(*arg, env)?),
            Node::Let { name, value, body } => {
                let v = self.eval(*value, env)?;
                let mut child = Env::child(env);
                child.borrow_mut().set(name, v);
                self.eval(*body, &child)
            },
            Node::If { cond, then, els } => {
                match self.eval(*cond, env) {
                    Ok(Value::Num(n)) if n != 0f64 => self.eval(*then, env),
                    Ok(Value::Num(_)) => self.eval(*els, env),
                    _ => Err(LamError::new("If condition must evaluate to a number"))
                }
            },
            Node::FnDef { name, params, body } => {
                let f = Value::Func(Box::new(LamFunc::UDef {
                    name: name.clone(), /* forgot about thiz */
                    o_params: params.clone(),
                    params,
                    body: *body,
                    env: Rc::clone(env),
                }));
                env.borrow_mut().set(name, f);
               Ok( Value::Nil)
            },
            Node::LambdaDef { params, body } => {
                Ok(Value::Func(Box::new(LamFunc::UDef {
                    name: String::new(),
                    o_params: params.clone(),
                    params,
                    body: *body,
                    env: Rc::clone(env),
                })))
            },
            Node::Match { expr, arms } => {
                let val = self.eval(*expr, env)?;
                for arm in arms {
                    if let Some(bindings) = self.match_pattern(&arm.pattern, &val) {
                        let c = Env::child(env);
                        for (k, v) in bindings {
                            c.borrow_mut().set(k, v);
                        }

                        if let Some(g) = arm.guard {
                            match self.eval(*g, &c) {
                                Ok(Value::Num(n)) if n != 0f64 => return self.eval(*arm.body, &c),
                                _ => continue,
                            }
                        }

                        return self.eval(*arm.body, &c);
                    }
                }

                Err(LamError::new("non-exhaustive match"))
            },
            Node::UseModule { path } => {
                let p = if is_lam_file(&path) {
                    path.clone()
                } else {
                    let mut s = path.clone();
                    s.push_str(".lam");
                    s
                };

                let src = if let Some(em_path) = p.strip_prefix("std/") {
                    if let Some(f) = STD_DIR.get_file(em_path) {
                        f.contents_utf8().expect("invalid utf8 in std file").to_string()
                    } else {
                        panic!("unknown std module '{em_path}'")
                    }
                } else {
                    std::fs::read_to_string(&p)
                        .map_err(|e| LamError::new(format!("Failed to real module file: {e}")))?
                };

                let stripped: String = src.lines()
                    .map(str::trim)
                    .filter(|l| !l.is_empty() && !l.starts_with('#'))
                    .collect::<Vec<_>>()
                    .join(" ");

                let mut parser = Parser::new(&stripped);
                while parser.got_tokens() {
                    let node = parser.parse_top_level()?;
                    self.exec(node, env)?;
                }

                Ok(Value::Nil)
            }
        }
    }

    pub(crate) fn apply(&self, func: Value, arg: Value) -> LamResult<Value> {
        match func {
            Value::Func(f) => match *f {
                LamFunc::Intrinsic { name, mut args, arity } => {
                    args.push(arg);

                    if args.len() == arity {
                        self.call_intrinsic(&name, args)
                    } else {
                        /* auto curry ! */
                       Ok( Value::Func(Box::new(LamFunc::Intrinsic { name, args, arity })))
                    }
                }

                LamFunc::Composition { outer, inner } => {
                    let m = self.apply(Value::Func(Box::from(*inner)), arg)?;
                    self.apply(Value::Func(Box::from(*outer)), m)
                },

                LamFunc::UDef { name, o_params, params, body, env: closed } => {
                    let child = Env::child(&closed);
                    child.borrow_mut().set(params[0].clone(), arg);
                    child.borrow_mut().set(name.clone(), Value::Func(Box::new(LamFunc::UDef {
                        name: name.clone(),
                        o_params: o_params.clone(),
                        params: o_params.clone(),
                        body: body.clone(),
                        env: Rc::clone(&closed)
                    })));

                    if params.len() == 1 {
                        self.eval(body, &child)
                    } else {
                        /* auto curry! */
                        Ok(Value::Func(Box::new(LamFunc::UDef {
                            name,
                            o_params,
                            params: params[1..].to_vec(),
                            body,
                            env: child
                        })))
                    }
                }
            }

            _ => Err(LamError::new("Cannot apply non function value"))
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

    fn call_intrinsic(&self, name: &str, args: Vec<Value>) -> LamResult<Value> {
        match self.intrinsics.get(name) {
            Some((func, _)) => func(self, args),
            None => Err(LamError::new(format!("Unknown intrinsic '{name}'"))),
        }
    }

    fn lookup_intrinsic(&self, name: &str) -> LamResult<Value> {
        match self.intrinsics.get(name) {
            Some((_, arity)) => Ok(Value::Func(Box::new(LamFunc::Intrinsic {
                name: name.to_string(),
                args: vec![],
                arity: *arity
            }))),

            None => Err(LamError::new(format!("Unknown symbol '{name}'")))
        }
    }
}

impl Env {
    pub fn new() -> Self {
        Env { bindings: HashMap::new(), parent: None }
    }

    pub fn child(thiz: &Rc<RefCell<Env>>) -> Rc<RefCell<Env>> {
        Rc::new(RefCell::new(Env { bindings: HashMap::new(), parent: Some(Rc::clone(thiz)) }))
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.bindings.get(name).cloned().or_else(|| {
            self.parent.as_ref()?.borrow().get(name)
        })
    }
}

fn is_lam_file(filename: &str) -> bool {
    let filename = std::path::Path::new(filename);
    filename.extension()
        .is_some_and(|e| e.eq_ignore_ascii_case("lam"))
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