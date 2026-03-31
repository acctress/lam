#[derive(Debug, Clone)]
pub struct LamError {
    pub msg: String,
    pub span: Option<(usize, usize)>
}

pub type LamResult <T> = Result<T, LamError>;

impl LamError {
    pub fn new(msg: impl Into<String>) -> Self {
        LamError { msg: msg.into(), span: None }
    }

    pub fn with_span(msg: impl Into<String>, start: usize, end: usize) -> Self {
        LamError { msg: msg.into(), span: Some((start, end)) }
    }
}