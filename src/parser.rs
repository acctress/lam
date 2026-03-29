use std::cmp::PartialEq;
use crate::parser::Node::Application;

#[derive(Debug, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    LBracket,
    RBracket,
    Comma,
    Number(f64),
    String(String),
    Char(char),
    Symbol(String), /* identifier but with a fancy name */
}

#[derive(Debug)]
pub enum Node {
    Literal(f64),
    Atom(String),
    Partial { op: String, arg: Box<Node> },
    Application { func: Box<Node>, arg: Box<Node> },
    List(Vec<Node>),
}

pub struct Parser<'a> {
    input: &'a str,
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser<'_> {
    pub fn new(input: &'_ str) -> Parser<'_> {
        let tokens = tokenize(input);

        Parser {
            input,
            tokens,
            pos: 0
        }
    }

    pub fn tokens(&self) -> &Vec<Token> {
        &self.tokens
    }

    pub fn parse(&mut self) -> Node {
        self.parse_expr()
    }

    fn parse_expr(&mut self) -> Node {
        let mut node = self.parse_primary();

        while self.do_primary() {
            let arg = self.parse_primary();
            node = Application { func: Box::from(node), arg: Box::new(arg) };
        }

        node
    }

    fn parse_primary(&mut self) -> Node {
        match self.consume() {
            Some(token) => match token {
                Token::LParen => {
                    if matches!(self.peek(), Some(Token::Symbol(s)) if !s.chars().next().unwrap().is_ascii_alphabetic()) {
                        self.parse_partial()
                    } else {
                        let i = self.parse_expr(); /* inner expr */
                        match self.consume() {
                            Some(Token::RParen) => {},
                            _ => panic!("expected ')'"),
                        }
                        i
                    }
                },
                Token::LBracket => self.parse_list(),
                Token::Number(n) => Node::Literal(*n),
                Token::String(s) => Node::Atom(s.clone()),
                Token::Char(c) => Node::Literal(*c as u32 as f64),
                Token::Symbol(s) => Node::Atom(s.clone()),
                _ => panic!("unexpected token '{:?}'", token),
            }
            _ => panic!("unexpected end of input")
        }
    }

    fn parse_partial(&mut self) -> Node {
        let op = match self.consume() {
            Some(Token::Symbol(s)) => s.clone(),
            _ => panic!("expected operator in partial")
        };

        let arg = self.parse_expr();

        match self.consume() {
            Some(Token::RParen) => {},
            _ => panic!("expected ')' to close partial")
        }

        Node::Partial { op, arg: Box::new(arg) }
    }

    fn parse_list(&mut self) -> Node {
        let mut list: Vec<Node> = vec![];

        if matches!(self.peek(), Some(Token::RBracket)) {
            self.consume();
            return Node::List(list);
        }

        loop {
            list.push(self.parse_expr());

            match self.consume() {
                Some(Token::RBracket) => return Node::List(list),
                Some(Token::Comma) => continue,
                _ => panic!("expected ',' or ']' in list"),
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
        matches!(self.peek(), Some(t) if !matches!(t, Token::RParen | Token::RBracket | Token::Comma))
    }
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = vec![];
    let mut chars = input.char_indices().peekable();

    while let Some((pos, current)) = chars.next() {
        match current {
            c if c.is_ascii_whitespace() => continue,
            '(' => tokens.push(Token::LParen),
            ')' => tokens.push(Token::RParen),
            '[' => tokens.push(Token::LBracket),
            ']' => tokens.push(Token::RBracket),
            ',' => tokens.push(Token::Comma),
            n if n.is_ascii_digit() => {
                let start = pos;
                let mut is_flt = false;

                while chars.peek().is_some_and(|(_, c)| c.is_ascii_digit() || (*c == '.' && !is_flt && { is_flt = true; true })) {
                    chars.next();
                }

                let end = chars.peek().map_or(input.len(), |(i, _)| *i);
                let value = &input[start..end];

                tokens.push(Token::Number(value.parse::<f64>().expect(&format!("failed to parse '{}' as number", value))));
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
            q if q == '\'' => {
                if let Some((_, chr)) = chars.next() {
                    if chars.next().map_or(false, |(_, nxt)| nxt == '\'') {
                        tokens.push(Token::Char(chr));
                    } else {
                        panic!("unterminated char literal");
                    }
                }
            },
            c => {
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
    c.is_ascii_whitespace() || matches!(c, '(' | ')' | '[' | ']' | '\'' | '"')
}

#[cfg(test)]
mod tests {
    use std::ops::Deref;
    use super::*;

    #[test]
    fn test_basic_tokenization() {
        let p = Parser::new("(23.5373)thirty");

        /* expect 4 tokens */
        assert_eq!(4, p.tokens().len());

        /* expect token types */
        assert_eq!(Token::LParen, *p.tokens().get(0).unwrap());
        assert_eq!(Token::Number(23.5373), *p.tokens().get(1).unwrap());
        assert_eq!(Token::RParen, *p.tokens().get(2).unwrap());
        assert_eq!(Token::Symbol("thirty".to_string()), *p.tokens().get(3).unwrap());
    }

    #[test]
    fn test_string_tokenization() {
        let p = Parser::new(r#""hello world""#);

        for token in p.tokens() {
            println!("{:?}", token);
        }

        assert_eq!(1, p.tokens().len());
        assert_eq!(Token::String("hello world".to_string()), *p.tokens().get(0).unwrap());
    }

    #[test]
    fn test_char_tokenization() {
        let p = Parser::new(r#"'c'"#);

        assert_eq!(1, p.tokens().len());
        assert_eq!(Token::Char('c'), *p.tokens().get(0).unwrap());
    }

    #[test]
    fn parse_partial() {
        let mut p = Parser::new("(+ 5)");
        let node = p.parse();

        match node {
            Node::Partial { op, arg } => {
                assert_eq!("+".to_string(), op );
                match *arg {
                    Node::Literal(n) => assert_eq!(5.0, n),
                    _ => panic!("expected 5 as arg")
                }
            }

            _ => panic!("expected partial")
        }
    }

    #[test]
    fn parse_literal() {
        let mut p = Parser::new("42");
        let node = p.parse();

        match node {
            Node::Literal(n) => assert_eq!(42.0, n),
            _ => panic!("expected literal"),
        }
    }

    #[test]
    fn parse_symbol() {
        let mut p = Parser::new("map");
        let node = p.parse();

        match node {
            Node::Atom(s) => assert_eq!("map", s),
            _ => panic!("expected atom"),
        }
    }

    #[test]
    fn parse_char_as_literal() {
        let mut p = Parser::new("'a'");
        let node = p.parse();

        match node {
            Node::Literal(n) => assert_eq!(97.0, n),
            _ => panic!("expected literal from char"),
        }
    }

    #[test]
    fn parse_empty_list() {
        let mut p = Parser::new("[]");
        let node = p.parse();

        match node {
            Node::List(items) => assert_eq!(0, items.len()),
            _ => panic!("expected empty list"),
        }
    }

    #[test]
    fn parse_list() {
        let mut p = Parser::new("[1, 2, 3]");
        let node = p.parse();

        match node {
            Node::List(items) => {
                assert_eq!(3, items.len());
                match &items[0] {
                    Node::Literal(n) => assert_eq!(1.0, *n),
                    _ => panic!("expected literal"),
                }
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn parse_nested_partial() {
        let mut p = Parser::new("(+ (+ 1))");
        let node = p.parse();

        match node {
            Node::Partial { op, arg } => {
                assert_eq!("+", op);
                match *arg {
                    Node::Partial { op: inner_op, arg: inner_arg } => {
                        assert_eq!("+", inner_op);
                        match *inner_arg {
                            Node::Literal(n) => assert_eq!(1.0, n),
                            _ => panic!("expected literal"),
                        }
                    }
                    _ => panic!("expected inner partial"),
                }
            }
            _ => panic!("expected partial"),
        }
    }

    #[test]
    fn parse_application() {
        let mut p = Parser::new("map (+ 1) [1, 2, 3]");
        let node = p.parse();

        match node {
            Node::Application { func, arg } => {
                match *arg {
                    Node::List(items) => assert_eq!(3, items.len()),
                    _ => panic!("expected list as outer arg"),
                }
                match *func {
                    Node::Application { func: inner_func, arg: inner_arg } => {
                        match *inner_func {
                            Node::Atom(s) => assert_eq!("map", s),
                            _ => panic!("expected map"),
                        }
                        match *inner_arg {
                            Node::Partial { op, arg } => {
                                assert_eq!("+", op);
                                match *arg {
                                    Node::Literal(n) => assert_eq!(1.0, n),
                                    _ => panic!("expected 1"),
                                }
                            }
                            _ => panic!("expected partial"),
                        }
                    }
                    _ => panic!("expected inner application"),
                }
            }
            _ => panic!("expected application"),
        }
    }
}