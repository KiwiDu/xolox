use std::{collections::HashMap, mem::transmute, rc::Rc, time::Instant};

use crate::vmval::VmVal as Val;

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    //Basics
    RETURN,
    PUSH,
    POP,
    JUMP,
    JUMPIF,
    //Var and Fun
    GDEF,
    GRES,
    LOCAL,
    ASSIGN,

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

    //Sential
    SENTIAL,
}

#[derive(Debug)]
pub struct Metal {
    pub chunk: Vec<u8>,
    pub heap: Vec<Val>,
}

impl Metal {
    pub fn new() -> Self {
        Self {
            chunk: Vec::with_capacity(32),
            heap: Vec::with_capacity(32),
        }
    }
}

pub struct VM {
    pub chunk: Vec<u8>, //
    pub stack: Vec<u64>,
    pub heap: Vec<Val>, //

    pub globals: HashMap<Rc<str>, u64>,
    pc: usize,
    timer: Instant,
}

macro_rules! BIN {
    ($self:ident, $opc:tt) => {{
        let b = &$self.heap[$self.stack.pop()? as usize];
        let a = &$self.heap[$self.stack.pop()? as usize];
        //println!("a:{}, b:{}",a,b);
        $self.push_value(a $opc b);
    }};
}

macro_rules! UNO {
    ($self:ident, $opc:tt) => {{
        let ptr = $self.stack.pop()?;
        let a = &$self.heap[ptr as usize];
        $self.push_value(($opc a).into());
    }};
}

impl VM {
    pub fn new() -> Self {
        Self {
            chunk: Vec::new(),
            heap: Vec::new(),
            stack: Vec::with_capacity(32),
            globals: HashMap::new(),
            pc: 0,
            timer: Instant::now(),
        }
    }

    fn next_op(&mut self) -> OpCode {
        let v = self.next();
        if v >= OpCode::SENTIAL as u8 {
            panic!("Corrupted VM");
        }
        unsafe { transmute(v) }
    }

    fn next(&mut self) -> u8 {
        if self.pc >= self.chunk.len() {
            return OpCode::RETURN as u8;
        }
        let v = self.chunk[self.pc];
        self.pc += 1;
        v
    }

    fn fwd(&mut self, step: usize) -> &[u8] {
        let beg = self.pc;
        self.pc += step;
        &self.chunk[beg..self.pc]
    }

    fn next16(&mut self) -> u16 {
        u16::from_be_bytes(self.fwd(2).try_into().unwrap())
    }

    fn next32(&mut self) -> u32 {
        u32::from_be_bytes(self.fwd(4).try_into().unwrap())
    }

    fn next64(&mut self) -> u64 {
        u64::from_be_bytes(self.fwd(8).try_into().unwrap())
    }

    pub fn push_value(&mut self, v: Val) {
        let ptr = self.malloc(v);
        self.push(ptr)
    }

    pub fn malloc(&mut self, v: Val) -> usize {
        self.heap.push(v);
        self.heap.len() - 1
    }

    fn pop(&mut self) -> Option<u64> {
        self.stack.pop()
    }

    fn push(&mut self, ptr: usize) {
        self.stack.push(ptr as u64)
    }

    pub fn pop_value(&mut self) -> Option<&Val> {
        let ptr = self.pop()?;
        self.heap.get(ptr as usize)
    }

    pub fn peek_value(&mut self, d: usize) -> Option<&Val> {
        let ptr = self.stack.iter().nth_back(d)?;
        self.heap.get(*ptr as usize)
    }

    pub fn run(&mut self) -> Option<&Val> {
        loop {
            let op = self.next_op();
            print!("{:?}\t", op);
            match op {
                OpCode::RETURN => return self.pop_value(),
                OpCode::PUSH => {
                    let no = self.next64();
                    self.stack.push(no);
                }
                OpCode::POP => {
                    self.stack.pop();
                }
                OpCode::JUMP => {
                    self.pc = self.next16() as usize;
                }
                OpCode::JUMPIF => {
                    let goto = self.next16() as usize;
                    if self.pop_value() == Some(&Val::Bool(true)) {
                        self.pc = goto;
                    }
                }
                OpCode::ADD => BIN!(self, +),
                OpCode::SUB => BIN!(self, -),
                OpCode::MUL => BIN!(self, *),
                OpCode::DIV => BIN!(self, /),
                OpCode::NOT => UNO!(self, !),
                OpCode::TIME => self.push_value(Val::Num(self.timer.elapsed().as_secs_f64())),
                OpCode::PRINT => println!("{}", self.pop_value().unwrap_or(&Val::Nil)),
                OpCode::TRUE => self.push_value(Val::Bool(true)),
                OpCode::FALSE => self.push_value(Val::Bool(false)),
                OpCode::NIL => self.push_value(Val::Nil),
                OpCode::ZERO => self.push_value(Val::Num(0.0)),
                OpCode::GDEF => {
                    let rhs = self.pop()? as usize;
                    let lhs = self.pop()? as usize;
                    if let Val::Var(lhs) = self.heap.get(lhs)? {
                        self.globals.insert(Rc::clone(lhs), rhs as u64);
                    } else {
                        panic!("Unassignable!")
                    }
                }
                OpCode::GRES => {
                    let ptr = self.pop()? as usize;
                    let g_var = self.heap.get(ptr)?; // Not using `pop_value` to bypass borrow checker
                    if let Val::Var(v) = g_var {
                        let val = self.globals.get(v).copied()? as usize;
                        print!("Got #{}# {}! \t", val, self.heap.get(val)?);
                        self.push(val);
                    } else {
                        panic!("Unaccessible!")
                    }
                }
                OpCode::LOCAL => todo!(),
                OpCode::ASSIGN => {
                    let rhs = self.pop()? as usize;
                    let lhs = self.pop()? as usize;
                    match self.heap.get(lhs)? {
                        Val::Var(n) => self.globals.insert(Rc::clone(n), rhs as u64),
                        a => None,
                    };
                }
                OpCode::SENTIAL => panic!("Corrupted!"),
            }
            println!("- {:?}", self.stack);
        }
    }
}
