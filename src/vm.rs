use std::{collections::HashMap, mem::transmute, rc::Rc, time::Instant};

use crate::{compile::Compiler, vmval::VmVal as Val};

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

    GT,
    LT,
    EQ,

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
    frames: Vec<Frame>,
    //pub chunk: Vec<u8>, //
    pub stack: Vec<u64>,
    pub heap: Vec<Val>, //

    pub globals: HashMap<Rc<str>, u64>,
    //pc: usize,
    timer: Instant,
}
struct Frame {
    chunk: *const Vec<u8>,
    pc: usize,
    arity: usize,
}

impl Frame {
    fn fun(chunk: &Vec<u8>, arity: usize) -> Self {
        Self {
            chunk,
            pc: 0,
            arity,
        }
    }
    fn next(&mut self) -> u8 {
        let chunk = unsafe { self.chunk.as_ref() }.unwrap();
        if self.pc >= chunk.len() {
            return OpCode::RETURN as u8;
        }
        let v = chunk[self.pc];
        self.pc += 1;
        v
    }

    fn next_op(&mut self) -> OpCode {
        let v = self.next();
        if v >= OpCode::SENTIAL as u8 {
            panic!("Corrupted VM");
        }
        unsafe { transmute(v) }
    }

    fn fwd(&mut self, step: usize) -> &[u8] {
        let beg = self.pc;
        self.pc += step;
        let chunk = unsafe { self.chunk.as_ref() }.unwrap();
        &chunk[beg..self.pc]
    }

    fn next16(&mut self) -> u16 {
        u16::from_be_bytes(self.fwd(2).try_into().unwrap())
    }

    /* fn next32(&mut self) -> u32 {
        u32::from_be_bytes(self.fwd(4).try_into().unwrap())
    } */

    fn next64(&mut self) -> u64 {
        u64::from_be_bytes(self.fwd(8).try_into().unwrap())
    }

    fn jmp(&mut self) {
        self.pc = self.next16() as usize;
    }
}

macro_rules! BIN {
    ($self:ident, $opc:tt) => {{
        let b = &$self.heap[$self.stack.pop()? as usize];
        let a = &$self.heap[$self.stack.pop()? as usize];
        //println!("a:{}, b:{}",a,b);
        $self.push_value(a $opc b);
    }};
}

macro_rules! CMP {
    ($self:ident, $opc:tt) => {{
        let b = &$self.heap[$self.stack.pop()? as usize];
        let a = &$self.heap[$self.stack.pop()? as usize];
        //println!("a:{}, b:{}",a,b);
        $self.push_value(Val::Bool(a $opc b));
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
            frames: Vec::new(),
            heap: Vec::new(),
            stack: Vec::with_capacity(32),
            globals: HashMap::new(),
            timer: Instant::now(),
        }
    }

    pub fn reg(&mut self, c: &Compiler) {
        self.frames.push(Frame {
            chunk: &c.chunk,
            pc: 0,
            arity: 0,
        })
    }

    fn top(&mut self) -> Option<&mut Frame> {
        self.frames.last_mut()
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

    fn peek(&mut self, d: usize) -> Option<u64> {
        self.stack.iter().nth_back(d).copied()
    }

    fn push(&mut self, ptr: usize) {
        self.stack.push(ptr as u64)
    }

    pub fn pop_value(&mut self) -> Option<&Val> {
        let ptr = self.pop()?;
        self.heap.get(ptr as usize)
    }

    pub fn peek_value(&mut self, d: usize) -> Option<&Val> {
        let ptr = self.peek(d)?;
        self.heap.get(ptr as usize)
    }

    pub fn run(&mut self, verbose: bool) -> Option<&Val> {
        loop {
            let top = self.top()?;
            let i = top.pc;
            let op = top.next_op();
            if verbose {
                print!("{:2} {:?}\t", i, op);
            }
            match op {
                OpCode::RETURN => {
                    if self.frames.len() == 1 {
                        return self.pop_value();
                    }
                    let top = self.frames.pop()?.arity;
                    if self.stack.is_empty() {
                        if top != 0 {
                            return None;
                        }
                    } else {
                        let end = self.stack.len() - 1;
                        self.stack.swap(end, end - top);
                        for _ in 0..top {
                            self.pop();
                        }
                    }
                }
                OpCode::PUSH => {
                    let no = self.frames.last_mut()?.next64();
                    self.stack.push(no);
                }
                OpCode::POP => {
                    self.stack.pop();
                }
                OpCode::JUMP => {
                    self.top()?.jmp();
                }
                OpCode::JUMPIF => {
                    if self.pop_value() == Some(&Val::Bool(true)) {
                        self.top()?.jmp();
                        //print!("JUMPPED!")
                    } else {
                        self.top()?.next16(); //Consume the unused addr.
                                              //print!("NO JUMP!")
                    }
                    //println!("{}", self.top()?.pc)
                }
                OpCode::ADD => BIN!(self, +),
                OpCode::SUB => BIN!(self, -),
                OpCode::MUL => BIN!(self, *),
                OpCode::DIV => BIN!(self, /),
                OpCode::GT => CMP!(self, >),
                OpCode::LT => CMP!(self, <),
                OpCode::EQ => CMP!(self, ==),
                OpCode::NOT => UNO!(self, !),
                OpCode::TIME => self.push_value(Val::Num(self.timer.elapsed().as_secs_f64())),
                OpCode::PRINT => println!(">> {}", self.pop_value().unwrap_or(&Val::Nil)),
                OpCode::TRUE => self.push_value(Val::Bool(true)),
                OpCode::FALSE => self.push_value(Val::Bool(false)),
                OpCode::NIL => self.push_value(Val::Nil),
                OpCode::ZERO => self.push_value(Val::Num(0.0)),
                OpCode::GSET => {
                    let rhs = self.pop()? as usize;
                    let lhs = self.pop()? as usize;
                    if let Val::Var(lhs) = self.heap.get(lhs)? {
                        self.globals.insert(Rc::clone(lhs), rhs as u64);
                    } else {
                        panic!("Unassignable!")
                    }
                }
                OpCode::GGET => {
                    let ptr = self.pop()? as usize;
                    let g_var = self.heap.get(ptr)?; // Not using `pop_value` to bypass borrow checker
                    match g_var {
                        Val::Var(v) => {
                            let val = self.globals.get(v).copied()? as usize;
                            if verbose {
                                print!("Got #{}: {} \t", val, self.heap.get(val)?);
                            }
                            self.push(val);
                        }
                        /* Val::Fun(n, a, b) => {
                            let sub = Frame::fun(b, *a as usize);
                            self.frames.push(sub);
                        } */
                        _ => panic!("Unaccessible!"),
                    }
                }
                OpCode::CALL => {
                    let ptr = self.pop()? as usize;
                    let fun = self.heap.get(ptr)?;
                    match fun {
                        Val::Fun(n, a, b) => {
                            let sub = Frame::fun(b, *a as usize);
                            self.frames.push(sub);
                        }
                        _ => panic!("Unaccessible!"),
                    }
                }
                OpCode::LSET => {
                    let local = self.top()?.next16() as usize;
                    let rhs = self.peek(0)?;
                    let lhs: &mut u64 = self.stack.iter_mut().nth_back(local)?;
                    *lhs = rhs
                }
                OpCode::LGET => {
                    let local = self.top()?.next16() as usize;
                    let ptr = self.peek(local)?;
                    self.push(ptr as usize);
                }
                OpCode::ASSIGN => {
                    let rhs = self.pop()? as usize;
                    let lhs = self.pop()? as usize;
                    match self.heap.get(lhs)? {
                        Val::Var(n) => self.globals.insert(Rc::clone(n), rhs as u64),
                        _ => None,
                    };
                }
                OpCode::SENTIAL => panic!("Corrupted!"),
            }
            if verbose {
                //println!("- {:?} {:?}", self.stack, self.heap);

                println!(
                    "- [{}] \n\t  [{}]",
                    self.stack
                        .iter()
                        .map(|v| v.to_string())
                        .reduce(|s, v| format!("{}, {}", s, v))
                        .unwrap_or("".to_string()),
                    self.heap
                        .iter()
                        .map(|v| v.to_string())
                        .reduce(|s, v| format!("{}, {}", s, v))
                        .unwrap_or("".to_string()),
                );
            }
        }
    }
}
