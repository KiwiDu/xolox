#[derive(Debug, Clone)]
pub enum Error {
    RuntimeError(String),
    SyntaxError(String),
}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::RuntimeError(msg) => write!(f, "Runtime Error: {}", msg),
            Error::SyntaxError(msg) => write!(f, "Syntax Error: {}", msg),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }

    fn description(&self) -> &str {
        "description() is deprecated; use Display"
    }

    fn cause(&self) -> Option<&dyn std::error::Error> {
        self.source()
    }

    //fn provide<'a>(&'a self, demand: &mut std::any::Demand<'a>) {}
}
