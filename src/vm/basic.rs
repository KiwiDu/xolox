use super::ftbit::{StackType, StackVal, Unpack};
use super::Val;

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    //Basics
    RETURN,
    PUSH,
    POP,
    JUMP,
    JUMPIF,
    CALL,
    //Var and Fun
    GSET,
    GGET,
    LSET,
    LGET,

    //Native value
    TRUE,
    FALSE,
    NIL,
    //Frequent value
    ZERO,

    // Native func
    TIME,
    PRINT,

    // Ops
    ADD,
    SUB,
    MUL,
    DIV,
    NOT,

    GT,
    LT,
    EQ,

    //Sential
    SENTIAL,
}

impl Default for OpCode {
    fn default() -> Self {
        OpCode::RETURN
    }
}

pub struct Mem {
    pub stack: Vec<StackVal>,
    pub heap: Vec<Val>,
}

impl Mem {
    pub fn new() -> Self {
        Self {
            heap: Vec::new(),
            stack: Vec::with_capacity(32),
        }
    }

    pub fn push_val(&mut self, v: Val) {
        let ptr = self.alloc(v);
        self.push(ptr)
    }

    pub fn alloc(&mut self, v: Val) -> StackVal {
        match v {
            Val::Num(f) => StackVal::from_f64(f),
            Val::Bool(true) => StackVal::TRUE,
            Val::Bool(false) => StackVal::FALSE,
            Val::Nil => StackVal::NIL,
            _ => {
                let addr = self.alloc_obj(v) as u64;
                StackVal::from_parts(StackType::HeapPtr, addr)
            }
        }
    }

    fn alloc_obj(&mut self, v: Val) -> usize {
        for (i, h) in self.heap.iter().enumerate() {
            if h == &v {
                return i;
            }
        }
        self.heap.push(v);
        self.heap.len() - 1
    }

    pub(super) fn pop(&mut self) -> Option<StackVal> {
        self.stack.pop()
    }

    pub(super) fn peek(&mut self, d: usize) -> Option<StackVal> {
        self.stack.iter().nth_back(d).copied()
    }

    pub(super) fn push(&mut self, ptr: StackVal) {
        self.stack.push(ptr)
    }

    pub(super) fn push_bool(&mut self, b: bool) {
        match b {
            true => self.stack.push(StackVal::TRUE),
            false => self.stack.push(StackVal::FALSE),
        }
    }

    pub(super) fn pop_pri(&mut self) -> Option<Val> {
        match self.pop()?.unpack() {
            Unpack::Float(f) => Some(Val::Num(f)),
            Unpack::NaNBox(StackType::Nil, _) => Some(Val::Nil),
            Unpack::NaNBox(StackType::True, _) => Some(Val::Bool(true)),
            Unpack::NaNBox(StackType::False, _) => Some(Val::Bool(false)),
            _ => None,
        }
    }

    pub(super) fn pop_obj(&mut self) -> Option<&Val> {
        //println!("{:?}", self.peek(0)?.unpack());
        match self.pop()?.unpack() {
            Unpack::NaNBox(StackType::HeapPtr, ptr) => self.heap.get(ptr as usize),
            _ => None,
        }
    }

    pub(super) fn pop_val(&mut self) -> Option<Val> {
        match self.pop()?.unpack() {
            Unpack::Float(f) => Some(Val::Num(f)),
            Unpack::NaNBox(StackType::Nil, _) => Some(Val::Nil),
            Unpack::NaNBox(StackType::True, _) => Some(Val::Bool(true)),
            Unpack::NaNBox(StackType::False, _) => Some(Val::Bool(false)),
            Unpack::NaNBox(StackType::HeapPtr, ptr) => self.heap.get(ptr as usize).cloned(),
            _ => None,
        }
    }
}
