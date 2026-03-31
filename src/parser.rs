use std::cmp::PartialEq;
use crate::error::{LamError, LamResult};

#[derive(Debug, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    LBracket,
    RBracket,
    Comma,
    Semi,
    Range,
    Spread,
    Lambda,
    Number(f64),
    String(String),
    Char(char),
    Symbol(String), /* identifier but with a fancy name */
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Box<Node>>,
    pub body: Box<Node>,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Literal(f64),
    StringLit(String),
    Wildcard,
    Var(String),
    List(Vec<Pattern>, Option<String>),
}

#[derive(Debug, Clone)]
pub enum Node {
    Literal(f64),
    Atom(String),
    Partial { op: String, arg: Box<Node> },
    Application { func: Box<Node>, arg: Box<Node> },
    List(Vec<Node>),
    Let { name: String, value: Box<Node>, body: Box<Node> },
    If { cond: Box<Node>, then: Box<Node>, els: Box<Node> },
    FnDef { name: String, params: Vec<String>, body: Box<Node> },
    LambdaDef { params: Vec<String>, body: Box<Node> },
    Match { expr: Box<Node>, arms: Vec<MatchArm> },
    UseModule { path: String },
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(input: &'_ str) -> Parser {
        let tokens = tokenize(input);

        Parser {
            tokens,
            pos: 0
        }
    }

    pub fn parse(&mut self) -> LamResult<Node> {
        self.parse_expr()
    }

    pub fn parse_top_level(&mut self) -> LamResult<Node> {  self.parse_primary()  }

    pub(crate) fn got_tokens(&self) -> bool {
        self.pos < self.tokens.len()
    }

    fn parse_expr(&mut self) -> LamResult<Node> {
        let mut node = self.parse_primary()?;

        while self.do_primary() {
            let arg = self.parse_primary()?;
            node = Node::Application { func: Box::from(node), arg: Box::new(arg) };
        }

        /* x |> y */
        while matches!(self.peek(), Some(Token::Symbol(s)) if s == "|>") {
            self.consume();

            let mut f = self.parse_primary()?;

            /* get partial application args e.g. `x |> filter (> 5)` = `filter (> 5) x` */
            while self.do_primary() && !matches!(self.peek(), Some(Token::Symbol(s)) if s == "|>") {
                let a = self.parse_primary()?;
                f = Node::Application { func: Box::new(f), arg: Box::new(a) };
            }

            node = Node::Application { func: Box::new(f), arg: Box::new(node) };
        }

        Ok(node)
    }

    fn parse_primary(&mut self) -> LamResult<Node> {
        match self.consume() {
            Some(token) => match token {
                Token::LParen => {
                    match self.peek() {
                        Some(Token::Symbol(s)) if s == "let" => self.parse_let(),
                        Some(Token::Symbol(s)) if s == "fn" => self.parse_fn(),
                        Some(Token::Symbol(s)) if s == "if" => self.parse_if(),
                        Some(Token::Symbol(s)) if s == "match" => self.parse_match(),
                        Some(Token::Symbol(s)) if s == "use" => self.parse_use(),
                        Some(Token::Symbol(s)) if !s.chars().next().unwrap().is_ascii_alphabetic() => self.parse_partial(),
                        _ => {
                            let i = self.parse_expr()?;
                            match self.consume() {
                                Some(Token::RParen) => {},
                                t => return Err(LamError::new(format!("Expected ')' but got '{t:?}'"))),
                            }
                            Ok(i)
                        }
                    }
                },
                Token::Lambda => self.parse_lambda(),
                Token::LBracket => self.parse_list(),
                Token::Number(n) => Ok(Node::Literal(*n)),
                Token::Char(c) => Ok(Node::Literal(f64::from(*c as u32))),
                Token::Symbol(s) | Token::String(s) => Ok(Node::Atom(s.clone())),
                _ => Err(LamError::new(format!("Unexpected token '{token:?}'"))),
            }
            _ => Err(LamError::new("Unexpected end of input")),
        }
    }

    fn parse_partial(&mut self) -> LamResult<Node> {
        let op = match self.consume() {
            Some(Token::Symbol(s)) => s.clone(),
            _ => panic!("expected operator in partial")
        };

        let mut args = vec![];
        while !matches!(self.peek(), Some(Token::RParen)) {
            args.push(self.parse_primary()?);
        }

        self.consume();

        match args.len() {
            0 => Ok(Node::Atom(op)),
            1 => Ok(Node::Partial { op, arg: Box::new(args.pop().unwrap())}),
            2 => {
                let r = args.pop().unwrap();
                let l = args.pop().unwrap();
                Ok(Node::Application {
                    func: Box::new(Node::Partial { op, arg: Box::new(l) }),
                    arg: Box::new(r)
                })
            }
            _ => Err(LamError::new("Operation section takes one to two arguments"))
        }
    }

    fn parse_list(&mut self) -> LamResult<Node> {
        let mut list: Vec<Node> = vec![];

        if matches!(self.peek(), Some(Token::RBracket)) {
            self.consume();
            return Ok(Node::List(list));
        }

        let first_elem = self.parse_primary()?;

        /* [x..y] or [x..y;z] */
        if matches!(self.peek(), Some(Token::Range)) {
            self.consume();

            let to = self.parse_primary()?;
            let step = if matches!(self.peek(), Some(Token::Semi)) {
                self.consume();
                self.parse_primary()?
            } else {
                Node::Literal(1f64)
            };

            match self.consume() {
                Some(Token::RBracket) => {},
                _ => return Err(LamError::new("Expected ']' to close range")),
            }

            match (&first_elem, &to, &step) {
                (Node::Literal(f), Node::Literal(t), Node::Literal(s)) => {
                    let mut value = *f;
                    while value < *t {
                        list.push(Node::Literal(value));
                        value += s;
                    }
                    return Ok(Node::List(list));
                }
                _ => return Err(LamError::new("Range bounds must be numeric literals"))
            }
        }

        list.push(first_elem);

        loop {
            match self.consume() {
                Some(Token::RBracket) => return Ok(Node::List(list)),
                Some(Token::Comma) => list.push(self.parse_primary()?),
                _ => return Err(LamError::new("Expected ',' or ']' in list")),
            }
        }
    }

    fn parse_lambda(&mut self) -> LamResult<Node> {
        /* parse e.g. (\x -> (+ x 1)) */
        let mut params = vec![];
        while !matches!(self.peek(), Some(Token::Symbol(s)) if s == "->") {
            match self.consume() {
                Some(Token::Symbol(s)) => params.push(s.clone()),
                _ => return Err(LamError::new("Expected parameter name in lambda")),
            }
        }

        self.consume();

        let body = self.parse_primary()?;

        Ok(Node::LambdaDef { params, body: Box::new(body) })
    }

    fn parse_let(&mut self) -> LamResult<Node> {
        /* parse e.g. (let (x 5) (+ x 1)) */
        /* ( -> let -> (name value) -> body -> ) */
        self.consume();

        match self.consume() {
            Some(Token::LParen) => {},
            _ => return Err(LamError::new("Expected '(' after 'let'"))
        }

        let name = match self.consume() {
            Some(Token::Symbol(s)) => s.clone(),
            _ => return Err(LamError::new("Expected variable name in let"))
        };

        let value = self.parse_expr()?;

        match self.consume() {
            Some(Token::RParen) => {},
            _ => return Err(LamError::new("Expected ')' to close let binding"))
        }

        let body = self.parse_expr()?;

        match self.consume() {
            Some(Token::RParen) => {},
            _ => return Err(LamError::new("Expected ')' to close let")),
        }

        Ok(Node::Let { name, value: Box::new(value), body: Box::new(body) })
    }

    fn parse_fn(&mut self) -> LamResult<Node> {
        /* parse e.g. (fn (double x) ((* 2) x)) */
        /* ( -> fn -> (name params) -> body -> ) */
        self.consume();

        match self.consume() {
            Some(Token::LParen) => {},
            _ => return Err(LamError::new("Expected '(' after 'fn'")),
        }

        let name = match self.consume() {
            Some(Token::Symbol(s)) => s.clone(),
            _ => return Err(LamError::new("Expected function name")),
        };

        let mut params = vec![];
        while !matches!(self.peek(), Some(Token::RParen)) {
            match self.consume() {
                Some(Token::Symbol(s)) => params.push(s.clone()),
                _ => return Err(LamError::new("Expected parameter name")),
            }
        }

        self.consume();

        let body = self.parse_expr()?;

        match self.consume() {
            Some(Token::RParen) => {},
            _ => return Err(LamError::new("Expected ')' to close fn")),
        }

        Ok(Node::FnDef { name, params, body: Box::new(body) })
    }

    fn parse_if(&mut self) -> LamResult<Node> {
        self.consume();

        let cond = self.parse_primary()?;
        let then = self.parse_primary()?;
        let els = self.parse_primary()?;

        match self.consume() {
            Some(Token::RParen) => {},
            _ => return Err(LamError::new("Expected ')' to close if")),
        }

        Ok(Node::If { cond: Box::new(cond), then: Box::new(then), els: Box::new(els) })
    }

    fn parse_match(&mut self) -> LamResult<Node> {
        self.consume();

        let scrutinee = self.parse_primary()?;
        let mut arms = vec![];

        while matches!(self.peek(), Some(Token::LParen)) {
            self.consume();
            let pattern = self.parse_pattern()?;
            let guard = if matches!(self.peek(), Some(Token::Symbol(s)) if s == "if") {
                self.consume();
                Some(Box::new(self.parse_primary()?))
            } else {
                None
            };

            let body = self.parse_primary()?;
            match self.consume() {
                Some(Token::RParen) => {},
                _ => return Err(LamError::new("Expected ')' to close match arm")),
            }

            arms.push(MatchArm { pattern, guard, body: Box::new(body) });
        }

        match self.consume() {
            Some(Token::RParen) => {},
            _ => return Err(LamError::new("Expected ')' to close match")),
        }

        Ok(Node::Match { expr: Box::new(scrutinee), arms })
    }

    fn parse_use(&mut self) -> LamResult<Node> {
        self.consume();
        let path = match self.consume() {
            Some(Token::String(s)) => s.clone(),
            _ => return Err(LamError::new("Expected a string in 'use'")),
        };
        match self.consume() {
            Some(Token::RParen) => {},
            _ => return Err(LamError::new("Expected ')' to close use")),
        }

        Ok(Node::UseModule { path })
    }

    fn parse_pattern(&mut self) -> LamResult<Pattern> {
        match self.consume() {
            Some(Token::Number(n)) => Ok(Pattern::Literal(*n)),
            Some(Token::String(s)) => Ok(Pattern::StringLit(s.clone())),
            Some(Token::Symbol(s)) if s == "_" => Ok(Pattern::Wildcard),
            Some(Token::Symbol(s)) => Ok(Pattern::Var(s.clone())),
            Some(Token::LBracket) => self.parse_list_pattern(),
            _ => Err(LamError::new("Unexpected token in pattern.")),
        }
    }

    fn parse_list_pattern(&mut self) -> LamResult<Pattern> {
        // println!("list pattern tokens: {&self.tokens[self.pos..]:?}");
        let mut patterns = vec![];
        let mut rest = None;

        if matches!(self.peek(), Some(Token::RBracket)) {
            self.consume();
            return Ok(Pattern::List(patterns, rest));
        }

        loop {
            if matches!(self.peek(), Some(Token::Spread)) {
                self.consume();
                match self.consume() {
                    Some(Token::Symbol(s)) => rest = Some(s.clone()),
                    _ => return Err(LamError::new("Expected name after '...' (spread)")),
                }
            } else {
                patterns.push(self.parse_pattern()?);
            }

            match self.consume() {
                Some(Token::RBracket) => return Ok(Pattern::List(patterns, rest)),
                Some(Token::Comma) => {},
                _ => return Err(LamError::new("Expected ',' or ']' in list pattern.")),
            }
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn consume(&mut self) -> Option<&Token> {
        let t = self.tokens.get(self.pos);
        self.pos += 1;
        t
    }

    fn do_primary(&self) -> bool {
        match self.peek() {
            Some(Token::RParen | Token::RBracket | Token::Comma) => false,
            Some(Token::Symbol(s)) if s == "|>" => false,
            Some(Token::LParen) => {
                !matches!(self.tokens.get(self.pos + 1),
                    Some(Token::Symbol(s)) if matches!(s.as_str(), "fn" | "let" | "if"))
            }
            Some(_) => true,
            None => false,
        }
    }
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = vec![];
    let mut chars = input.char_indices().peekable();

    while let Some((pos, current)) = chars.next() {
        match current {
            c if c.is_ascii_whitespace() => {},
            '(' => tokens.push(Token::LParen),
            ')' => tokens.push(Token::RParen),
            '[' => tokens.push(Token::LBracket),
            ']' => tokens.push(Token::RBracket),
            ',' => tokens.push(Token::Comma),
            ';' => tokens.push(Token::Semi),
            '.' if chars.peek().is_some_and(|(_, c)| *c == '.') => {
                chars.next();
                if chars.peek().is_some_and(|(_, c)| *c == '.') {
                    chars.next();
                    tokens.push(Token::Spread);
                } else {
                    tokens.push(Token::Range);
                }
            }
            '\\' => tokens.push(Token::Lambda),
            n if n.is_ascii_digit() => {
                let start = pos;
                let mut is_flt = false;

                while chars.peek().is_some_and(|(p, c)| {
                    if c.is_ascii_digit() {
                        true
                    } else if *c == '.' && !is_flt {
                        /* had the issue of ".." being a float, so uh set is_flt ONLY if peeked next char is a number */
                        let nx = input.as_bytes().get(p + 1);
                        if nx.is_some_and(u8::is_ascii_digit) {
                            is_flt = true;
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }) {
                    chars.next();
                }

                let end = chars.peek().map_or(input.len(), |(i, _)| *i);
                let value = &input[start..end];

                tokens.push(Token::Number(value.parse::<f64>().unwrap_or_else(|_| panic!("failed to parse '{value}' as number"))));
            },
            q if q == '"' => {
                let start = pos + 1;

                while chars.peek().is_some_and(|(_, c)| *c != q) {
                    chars.next();
                }

                let end = chars.peek().map_or(input.len(), |(i, _)| *i);
                chars.next();

                tokens.push(Token::String(input[start..end].to_string()));
            },
            '\'' => {
                if let Some((_, chr)) = chars.next() {
                    if chars.next().is_some_and(|(_, nxt)| nxt == '\'') {
                        tokens.push(Token::Char(chr));
                    } else if chars.next().is_some_and(|(_, nxt)| nxt.is_ascii_alphanumeric()) {
                        panic!("char literals can only contain one character");
                    } else {
                        panic!("unterminated char literal");
                    }
                }
            },
            _ => {
                let start = pos;

                while chars.peek().is_some_and(|(_, c)| !is_delimiter(*c)) {
                    chars.next();
                }

                let end = chars.peek().map_or(input.len(), |(i, _)| *i);
                let value = &input[start..end];

                tokens.push(Token::Symbol(value.to_string()));
            },
        }
    }

    tokens
}

fn is_delimiter(c: char) -> bool {
    c.is_ascii_whitespace() || matches!(c, '(' | ')' | '[' | ']' | '\'' | '"' | ';' | '.' | ',')
}