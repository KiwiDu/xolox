#[derive(Debug, Clone)]
pub enum Error {
    RuntimeError(String),
    SyntaxError(String),
}
