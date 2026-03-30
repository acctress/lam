use std::cmp::PartialEq;

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

    pub fn parse(&mut self) -> Node {
        self.parse_expr()
    }

    pub fn parse_top_level(&mut self) -> Node {  self.parse_primary()  }

    pub(crate) fn got_tokens(&self) -> bool {
        self.pos < self.tokens.len()
    }

    fn parse_expr(&mut self) -> Node {
        let mut node = self.parse_primary();

        while self.do_primary() {
            let arg = self.parse_primary();
            node = Node::Application { func: Box::from(node), arg: Box::new(arg) };
        }

        /* x |> y */
        while matches!(self.peek(), Some(Token::Symbol(s)) if s == "|>") {
            self.consume();

            let mut f = self.parse_primary();

            /* get partial application args e.g. `x |> filter (> 5)` = `filter (> 5) x` */
            while self.do_primary() && !matches!(self.peek(), Some(Token::Symbol(s)) if s == "|>") {
                let a = self.parse_primary();
                f = Node::Application { func: Box::new(f), arg: Box::new(a) };
            }

            node = Node::Application { func: Box::new(f), arg: Box::new(node) };
        }

        node
    }

    fn parse_primary(&mut self) -> Node {
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
                            let i = self.parse_expr();
                            let pos = self.pos;
                            match self.consume() {
                                Some(Token::RParen) => {},
                                t => panic!("expected ')' but got '{t:?}' at pos {pos}"),
                            }
                            i
                        }
                    }
                },
                Token::Lambda => self.parse_lambda(),
                Token::LBracket => self.parse_list(),
                Token::Number(n) => Node::Literal(*n),
                Token::Char(c) => Node::Literal(f64::from(*c as u32)),
                Token::Symbol(s) | Token::String(s) => Node::Atom(s.clone()),
                _ => panic!("unexpected token '{token:?}'"),
            }
            _ => panic!("unexpected end of input")
        }
    }

    fn parse_partial(&mut self) -> Node {
        let op = match self.consume() {
            Some(Token::Symbol(s)) => s.clone(),
            _ => panic!("expected operator in partial")
        };

        let mut args = vec![];
        while !matches!(self.peek(), Some(Token::RParen)) {
            args.push(self.parse_primary());
        }

        self.consume();

        match args.len() {
            0 => Node::Atom(op),
            1 => Node::Partial { op, arg: Box::new(args.pop().unwrap())},
            2 => {
                let r = args.pop().unwrap();
                let l = args.pop().unwrap();
                Node::Application {
                    func: Box::new(Node::Partial { op, arg: Box::new(l) }),
                    arg: Box::new(r)
                }
            }
            _ => panic!("operation section takes one to two arguments")
        }
    }

    fn parse_list(&mut self) -> Node {
        let mut list: Vec<Node> = vec![];

        if matches!(self.peek(), Some(Token::RBracket)) {
            self.consume();
            return Node::List(list);
        }

        let first_elem = self.parse_primary();

        /* [x..y] or [x..y;z] */
        if matches!(self.peek(), Some(Token::Range)) {
            self.consume();

            let to = self.parse_primary();
            let step = if matches!(self.peek(), Some(Token::Semi)) {
                self.consume();
                self.parse_primary()
            } else {
                Node::Literal(1f64)
            };

            match self.consume() {
                Some(Token::RBracket) => {},
                _ => panic!("expected ']' to close range"),
            }

            match (&first_elem, &to, &step) {
                (Node::Literal(f), Node::Literal(t), Node::Literal(s)) => {
                    let mut value = *f;
                    while value < *t {
                        list.push(Node::Literal(value));
                        value += s;
                    }
                    return Node::List(list);
                }
                _ => panic!("range bounds must be numeric literals")
            }
        }

        list.push(first_elem);

        loop {
            match self.consume() {
                Some(Token::RBracket) => return Node::List(list),
                Some(Token::Comma) => list.push(self.parse_primary()),
                _ => panic!("expected ',' or ']' in list"),
            }
        }
    }

    fn parse_lambda(&mut self) -> Node {
        /* parse e.g. (\x -> (+ x 1)) */
        let mut params = vec![];
        while !matches!(self.peek(), Some(Token::Symbol(s)) if s == "->") {
            match self.consume() {
                Some(Token::Symbol(s)) => params.push(s.clone()),
                _ => panic!("expected parameter name in lambda"),
            }
        }

        self.consume();

        let body = self.parse_primary();

        Node::LambdaDef { params, body: Box::new(body) }
    }

    fn parse_let(&mut self) -> Node {
        /* parse e.g. (let (x 5) (+ x 1)) */
        /* ( -> let -> (name value) -> body -> ) */
        self.consume();

        match self.consume() {
            Some(Token::LParen) => {},
            _ => panic!("expected '(' after let")
        }

        let name = match self.consume() {
            Some(Token::Symbol(s)) => s.clone(),
            _ => panic!("expected variable name in let")
        };

        let value = self.parse_expr();

        match self.consume() {
            Some(Token::RParen) => {},
            _ => panic!("expected ')' after let binding")
        }

        let body = self.parse_expr();

        match self.consume() {
            Some(Token::RParen) => {},
            _ => panic!("expected ')' to close let"),
        }

        Node::Let { name, value: Box::new(value), body: Box::new(body) }
    }

    fn parse_fn(&mut self) -> Node {
        /* parse e.g. (fn (double x) ((* 2) x)) */
        /* ( -> fn -> (name params) -> body -> ) */
        self.consume();

        match self.consume() {
            Some(Token::LParen) => {},
            _ => panic!("expected '(' after fn"),
        }

        let name = match self.consume() {
            Some(Token::Symbol(s)) => s.clone(),
            _ => panic!("expected function name"),
        };

        let mut params = vec![];
        while !matches!(self.peek(), Some(Token::RParen)) {
            match self.consume() {
                Some(Token::Symbol(s)) => params.push(s.clone()),
                _ => panic!("expected parameter name"),
            }
        }

        self.consume();

        let body = self.parse_expr();

        match self.consume() {
            Some(Token::RParen) => {},
            _ => panic!("expected ')' to close fn"),
        }

        Node::FnDef { name, params, body: Box::new(body) }
    }

    fn parse_if(&mut self) -> Node {
        self.consume();

        let cond = self.parse_primary();
        let then = self.parse_primary();
        let els = self.parse_primary();

        match self.consume() {
            Some(Token::RParen) => {},
            _ => panic!("expected ')' to close if"),
        }

        Node::If { cond: Box::new(cond), then: Box::new(then), els: Box::new(els) }
    }

    fn parse_match(&mut self) -> Node {
        self.consume();

        let scrutinee = self.parse_primary();
        let mut arms = vec![];

        while matches!(self.peek(), Some(Token::LParen)) {
            self.consume();
            let pattern = self.parse_pattern();
            let guard = if matches!(self.peek(), Some(Token::Symbol(s)) if s == "if") {
                self.consume();
                Some(Box::new(self.parse_primary()))
            } else {
                None
            };

            let body = self.parse_primary();
            match self.consume() {
                Some(Token::RParen) => {},
                _ => panic!("expected ')' to close match arm")
            }

            arms.push(MatchArm { pattern, guard, body: Box::new(body) });
        }

        match self.consume() {
            Some(Token::RParen) => {},
            _ => panic!("expected ')' to close match")
        }

        Node::Match { expr: Box::new(scrutinee), arms }
    }

    fn parse_use(&mut self) -> Node {
        self.consume();
        let path = match self.consume() {
            Some(Token::String(s)) => s.clone(),
            _ => panic!("use expects a file path string"),
        };
        match self.consume() {
            Some(Token::RParen) => {},
            _ => panic!("expected ')' to close use")
        }

        Node::UseModule { path }
    }

    fn parse_pattern(&mut self) -> Pattern {
        match self.consume() {
            Some(Token::Number(n)) => Pattern::Literal(*n),
            Some(Token::String(s)) => Pattern::StringLit(s.clone()),
            Some(Token::Symbol(s)) if s == "_" => Pattern::Wildcard,
            Some(Token::Symbol(s)) => Pattern::Var(s.clone()),
            Some(Token::LBracket) => self.parse_list_pattern(),
            _ => panic!("unexpected token in pattern")
        }
    }

    fn parse_list_pattern(&mut self) -> Pattern {
        // println!("list pattern tokens: {&self.tokens[self.pos..]:?}");
        let mut patterns = vec![];
        let mut rest = None;

        if matches!(self.peek(), Some(Token::RBracket)) {
            self.consume();
            return Pattern::List(patterns, rest);
        }

        loop {
            if matches!(self.peek(), Some(Token::Spread)) {
                self.consume();
                match self.consume() {
                    Some(Token::Symbol(s)) => rest = Some(s.clone()),
                    _ => panic!("expected name after ...")
                }
            } else {
                patterns.push(self.parse_pattern());
            }

            match self.consume() {
                Some(Token::RBracket) => return Pattern::List(patterns, rest),
                Some(Token::Comma) => continue,
                _ => panic!("expected ',' or ']' in list pattern")
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