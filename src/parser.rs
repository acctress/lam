#[derive(Debug, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    Number(f64),
    String(String),
    Symbol(String), /* identifier but with a fancy name */
}

pub enum Node {
    Literal(i64),
    Partial { op: String, arg: Box<Node> },
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

    pub fn dbg(&self) {
        println!("{:?}", self.tokens);
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
            c if c.is_ascii_alphabetic() => {
                let start = pos;

                while chars.peek().is_some_and(|(_, c)| c.is_ascii_alphabetic()) {
                    chars.next();
                }

                let end = chars.peek().map_or(input.len(), |(i, _)| *i);
                let value = &input[start..end];

                tokens.push(Token::Symbol(value.to_string()));
            }
            _ => continue,
        }
    }

    tokens
}

#[cfg(test)]
mod tests {
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
}