pub mod basic;
pub mod frame;
pub mod ftbit;
pub mod local;
pub mod rcstr;
pub mod vmval;

use std::{collections::HashMap, rc::Rc, time::Instant};

use crate::compile::Compiler;
use basic::OpCode;
use frame::Frame;
use ftbit::{StackType, StackVal, Unpack};
use vmval::VmVal;

use self::basic::Mem;
//type Str = RcStr<DefaultHasher>;
type Str = Rc<str>;
type Val = VmVal<Str>;

pub struct VM {
    pub mem: Mem,
    pub globals: HashMap<Str, StackVal>,
    frames: Vec<Frame>,
    timer: Instant,
}

macro_rules! BIN {
    ($self:ident, $opc:tt) => {{
        let b = $self.mem.pop()?;
        let a = $self.mem.pop()?;
        match (a.unpack(), b.unpack()){
            (Unpack::Float(fa),Unpack::Float(fb)) => $self.mem.push(StackVal::from_f64(fa $opc fb)),
            (Unpack::NaNBox(StackType::HeapPtr, pa),Unpack::NaNBox(StackType::HeapPtr, pb)) => {
                let a = $self.mem.heap.get(pa as usize)?;
                let b = $self.mem.heap.get(pb as usize)?;
                $self.mem.push_val(a $opc b);
            }
            _ => panic!("Invalid op! {}",stringify!(opc)),
        }
    }};
}

macro_rules! CMP {
    ($self:ident, $opc:tt) => {{
        let b = $self.mem.pop()?;
        let a = $self.mem.pop()?;
        match (a.unpack(), b.unpack()){
            (Unpack::Float(fa),Unpack::Float(fb)) => {
                $self.mem.push_bool(fa $opc fb)
            },
            (Unpack::NaNBox(StackType::HeapPtr, pa),Unpack::NaNBox(StackType::HeapPtr, pb)) => {
                let a = $self.mem.heap.get(pa as usize)?;
                let b = $self.mem.heap.get(pb as usize)?;
                $self.mem.push_bool(a $opc b);
            }
            _ => panic!("Invalid op! {}",stringify!(opc)),
        }
    }};
}

macro_rules! UNO {
    ($self:ident, $opc:tt) => {{
        let a = $self.mem.pop()?;
        match a.unpack(){
            Unpack::NaNBox(StackType::HeapPtr, pa)=>{
                let a = $self.mem.heap.get(pa as usize)?;
                $self.mem.push_val(($opc a).into());
            }
            _ => panic!("Invalid op! {}",stringify!(opc)),
        }

    }};
}

impl VM {
    pub fn new() -> Self {
        Self {
            frames: Vec::new(),
            mem: Mem::new(),
            globals: HashMap::new(),
            timer: Instant::now(),
        }
    }

    pub fn reg(&mut self, c: &Compiler) {
        self.frames.push(Frame {
            chunk: c.chunk.as_ptr(),
            pc: 0,
            offset: 0,
        })
    }

    fn top(&mut self) -> Option<&mut Frame> {
        self.frames.last_mut()
    }

    pub fn run(&mut self, verbose: bool) -> Option<Val> {
        loop {
            let top = self.top()?;
            let i = top.pc;
            let op = top.next_op();
            if verbose {
                print!(
                    "{} {:2} {:6}",
                    "\t".repeat(self.frames.len() - 1),
                    i,
                    format!("{:?}", op)
                );
            }
            match op {
                OpCode::RETURN => {
                    if self.frames.len() == 1 {
                        return self.mem.pop_val();
                    }
                    let top = self.frames.pop()?;
                    let offset = top.offset;
                    let ret = self.mem.pop()?;
                    //let _locals: Vec<_> = self.mem.stack.drain(offset..).collect();
                    self.mem.stack.truncate(offset);
                    self.mem.push(ret);
                    /* println!(
                        "[{}] -> {}",
                        locals
                            .iter()
                            .map(|&x| self.heap.get(x as usize).unwrap())
                            .fold("".to_string(), |a, x| format!("{} {}", a, x)),
                        self.heap.get(ret as usize)?
                    ); */
                }
                OpCode::PUSH => {
                    let no = self.frames.last_mut()?.next64();
                    self.mem.push(StackVal::from_raw(no));
                }
                OpCode::POP => {
                    self.mem.pop();
                }
                OpCode::JUMP => {
                    self.top()?.jmp();
                }
                OpCode::JUMPIF => {
                    if let Some(Val::Bool(true)) = self.mem.pop_pri() {
                        self.top()?.jmp();
                    } else {
                        self.top()?.next16(); //Consume the unused addr.
                    }
                }
                OpCode::ADD => BIN!(self, +),
                OpCode::SUB => BIN!(self, -),
                OpCode::MUL => BIN!(self, *),
                OpCode::DIV => BIN!(self, /),
                OpCode::GT => CMP!(self, >),
                OpCode::LT => CMP!(self, <),
                OpCode::EQ => CMP!(self, ==),
                OpCode::NOT => UNO!(self, !),
                OpCode::TIME => self
                    .mem
                    .push_val(Val::Num(self.timer.elapsed().as_secs_f64())),
                OpCode::PRINT => {
                    print!(">> ");
                    let v = self.mem.pop_pri().unwrap_or(Val::Nil);
                    if verbose {
                        print!("{}\t", v)
                    } else {
                        println!("{}", v)
                    }
                }
                OpCode::TRUE => self.mem.push_val(Val::Bool(true)),
                OpCode::FALSE => self.mem.push_val(Val::Bool(false)),
                OpCode::NIL => self.mem.push_val(Val::Nil),
                OpCode::ZERO => self.mem.push_val(Val::Num(0.0)),
                OpCode::GSET => {
                    let rhs = self.mem.pop()?;
                    let lhs = self.mem.pop_obj()?;
                    if let Val::Var(lhs) = lhs {
                        self.globals.insert(Rc::clone(lhs), rhs);
                    } else {
                        panic!("Unaccessible {} !", lhs);
                    }
                }
                OpCode::GGET => {
                    let g_var = self.mem.pop_obj()?;
                    match g_var {
                        Val::Var(v) => {
                            let val = self.globals.get(v).copied()?;
                            self.mem.push(val);
                        }
                        _ => panic!("Unaccessible {} !", g_var),
                    }
                }
                OpCode::CALL => {
                    let len = self.mem.stack.len() - 1; //To compensate for the following pop
                    let fun = self.mem.pop_obj()?;
                    match fun {
                        Val::Fun(_, a, b) => {
                            let a: usize = (*a).try_into().unwrap();
                            let sub = Frame::fun(&b, len - a);
                            self.frames.push(sub);
                        }
                        _ => panic!("Unaccessible!"),
                    }
                }
                OpCode::LSET => {
                    let local = self.top()?.next16().try_into().unwrap();
                    let offset = self.top()?.offset;
                    let rhs = self.mem.peek(0)?;
                    let lhs = self.mem.stack[offset..].iter_mut().nth(local)?;
                    println!("\n{local}:{:?} -> {:?}", lhs.unpack(), rhs.unpack());
                    *lhs = rhs
                }
                OpCode::LGET => {
                    let local = self.top()?.next16().try_into().unwrap();
                    let offset = self.top()?.offset;
                    //println!("local:{} offset:{}", local, offset);
                    let ptr = self.mem.stack[offset..].iter().nth(local).copied()?; //self.peek(local)?;
                    self.mem.push(ptr);
                }
                OpCode::SENTIAL => panic!("Corrupted!"),
            }
            if verbose {
                println!(
                    "- [{}] ",
                    self.mem
                        .stack
                        .iter()
                        .map(|v| { format!("{:?}", v.unpack()) })
                        .reduce(|s, v| format!("{}, {}", s, v))
                        .unwrap_or("".to_string()),
                );

                if let OpCode::GSET = op {
                    println!("{:?}", self.globals);
                }
                println!("\tH:{:?} \n", self.mem.heap,);
            }
        }
    }
}
