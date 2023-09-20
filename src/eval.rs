enum Value {
    Str(String),
    Num(f64),
}
struct Eval {
    state: HashMap<Token, Value>,
}
fn eval(sexpr: S) {}
