use std::{
    cell::{Cell, OnceCell},
    collections::{HashSet, VecDeque},
    mem::transmute,
    rc::Rc,
};

use crate::{
    sexpr::S,
    token::{Keywords, Token, TokenType},
    vm::OpCode,
    //value::Val,
    vmval::VmVal as Val,
};

#[derive(Debug)]
pub struct Compiler {
    pub chunk: Vec<u8>,
    depth: u32,
    pub heap: Vec<Val>,
    pub strings: HashSet<Rc<str>>,
    locals: Vec<(u32, Rc<str>)>,
    heap_offset: usize,
}
impl Compiler {
    pub fn emit_value(&mut self, v: Val) {
        let ptr = self.malloc(v) as u64;
        self.emit(ptr);
    }

    fn emit_var(&mut self, name: &str) {
        let name = self.intern(name);
        self.emit_value(Val::Var(name))
    }

    fn emit_str(&mut self, s: &str) {
        let s = self.intern(s);
        self.emit_value(Val::Str(s))
    }

    fn decl_fun(&mut self, name: &str, arity: u8, chunk: Vec<u8>) {
        let name = self.intern(name);
        self.malloc(Val::Fun(name, arity, chunk));
    }

    pub fn emit(&mut self, ptr: u64) {
        self.chunk.reserve(1 + 8);
        self.chunk.push(OpCode::PUSH as u8);
        let bytes = u64::to_be_bytes(ptr);
        self.chunk.extend_from_slice(&bytes)
    }

    pub fn malloc(&mut self, v: Val) -> usize {
        if let Some(s) = v.extract_str() {
            for (i, h) in self.heap.iter().enumerate() {
                if let Some(hs) = h.extract_str() {
                    if hs == s {
                        return i + self.heap_offset;
                    }
                }
            }
        }
        self.heap.push(v);
        self.heap.len() - 1 + self.heap_offset
    }
}
impl Compiler {
    pub fn new() -> Self {
        Self {
            depth: 0,
            chunk: Vec::with_capacity(32),
            heap: Vec::with_capacity(32),
            strings: HashSet::new(),
            locals: Vec::with_capacity(32),
            heap_offset: 0,
        }
    }

    pub fn compile(&mut self, tree: &S<Token>, set: bool) {
        match tree {
            S::Atom(a) => self.atom(a, set),
            S::Unary(op, a) => self.unary(op, a.as_ref(), set),
            S::Bin(op, l, r) => self.binary(op, l.as_ref(), r.as_ref(), set),
            S::Cons(op, t) => self.cons(op, t, set),
        }
    }

    fn intern(&mut self, s: &str) -> Rc<str> {
        if let Some(a) = self.strings.get(s) {
            Rc::clone(a)
        } else {
            self.strings.insert(s.into());
            self.intern(s)
        }
    }

    fn op(&mut self, c: OpCode) {
        self.chunk.push(c as u8);
    }

    fn name(t: &S<Token>) -> Rc<str> {
        match t {
            S::Atom(Token::Idt(s)) => &s[..],
            S::Atom(Token::Str(s)) => &s[..],
            _ => panic!("Invalid Atom!"),
        }
        .into()
    }

    fn atom(&mut self, t: &Token, set: bool) {
        if set {
            match t {
                Token::Idt(s) => self.emit_var(s),
                _ => panic!("Cannot assign to non-variables!"),
            }
        } else {
            match t {
                Token::Idt(s) => {
                    for (d, l) in self.locals.iter().rev() {
                        if s.as_str() == l.as_ref() {
                            return;
                        }
                    }
                    //Only globals need to be resolved
                    self.emit_var(s);
                    self.op(OpCode::GRES);
                }
                Token::Str(s) => self.emit_str(s),
                Token::Kwd(Keywords::True) => self.op(OpCode::TRUE),
                Token::Kwd(Keywords::False) => self.op(OpCode::FALSE),
                Token::Kwd(Keywords::Nil) => self.op(OpCode::NIL),
                Token::Num(f) => self.emit_value(Val::Num(*f)),
                _ => panic!("Invalid Atom!"),
            }
        }
    }

    fn unary(&mut self, h: &Token, t: &S<Token>, set: bool) {
        use TokenType::*;
        if set {
            panic!("Cannot be used as a left value.");
        }
        if let Token::Op(tt) = h {
            match tt {
                Plus => self.compile(t, set),
                Minus => {
                    self.op(OpCode::ZERO);
                    self.compile(t, set);
                    self.op(OpCode::SUB);
                }
                Bang => {
                    self.compile(t, set);
                    self.op(OpCode::NOT);
                }
                Semicolon => {
                    self.compile(t, set);
                    self.op(OpCode::POP);
                    self.op(OpCode::NIL);
                }
                _ => panic!("Unexpected unary operator: {}!", tt),
            }
        } else if let Token::Kwd(Keywords::Print) = h {
            self.compile(t, set);
            self.op(OpCode::PRINT);
        }
    }

    fn binary(&mut self, h: &Token, left: &S<Token>, right: &S<Token>, set: bool) {
        use TokenType::*;
        fn bin_op(c: &mut Compiler, op: &TokenType) {
            match op {
                Plus => c.op(OpCode::ADD),
                Minus => c.op(OpCode::SUB),
                Star => c.op(OpCode::MUL),
                Slash => c.op(OpCode::DIV),
                EqualEqual => c.op(OpCode::ADD),
                BangEqual => c.op(OpCode::ADD),
                Less => c.op(OpCode::ADD),
                LessEqual => c.op(OpCode::ADD),
                Greater => c.op(OpCode::ADD),
                GreaterEqual => c.op(OpCode::ADD),
                _ => panic!("Unexpected binary operator: {}!", op),
            }
        }
        fn global_assign(c: &mut Compiler, left: &S<Token>, right: &S<Token>) {
            c.compile(left, true);
            c.compile(right, false);
            c.op(OpCode::GDEF);
        }
        fn local_assign(c: &mut Compiler, d: u32, left: &S<Token>, right: &S<Token>) {
            c.compile(right, false);
            c.locals.push((d, Compiler::name(left)));
        }

        if set {
            panic!("Cannot be used as a left value.");
        }
        match h {
            Token::Kwd(Keywords::Var) => match self.depth {
                0 => global_assign(self, left, right),
                d => local_assign(self, d, left, right),
            },
            Token::Op(TokenType::Equal) => {
                self.compile(left, true);
                self.compile(right, false);
                self.op(OpCode::ASSIGN);
            }
            Token::Op(tt) => {
                self.compile(left, false);
                self.compile(right, false);
                bin_op(self, tt);
            }
            _ => panic!("Unsupported binary {}", h),
        }
    }

    fn cons(&mut self, h: &Token, t: &Vec<S<Token>>, set: bool) {
        match h {
            Token::Op(TokenType::LeftBrace) => {
                self.depth += 1;
                for a in t {
                    self.compile(a, set);
                }
                self.depth -= 1;
            }
            fun @ Token::Idt(_) => {
                for arg in t.iter().rev() {
                    self.compile(arg, false);
                }
                self.atom(fun, set);
            }
            Token::Kwd(Keywords::Fun) => {
                if let [S::Atom(Token::Idt(name)), args @ .., body] = t.as_slice() {
                    let mut fun_compiler = Compiler::new();
                    fun_compiler.heap_offset = self.heap.len() + 1;
                    fun_compiler.compile(body, false);
                    let mut fun_chunk = fun_compiler.chunk;
                    fun_chunk.shrink_to_fit();
                    let arity = args.len() as u8;
                    self.decl_fun(name, arity, fun_chunk);
                    self.heap.extend(fun_compiler.heap);
                }
            }
            Token::Kwd(_) => todo!(),
            _ => panic!("Unsupported cons `{}`", h),
        }
    }

    pub fn dasm(&mut self) {
        let mut mode: VecDeque<u8> = VecDeque::new();
        let mut bytes = self.chunk.iter();
        loop {
            match mode.pop_front() {
                Some(n) => {
                    let bytes: String = bytes
                        .by_ref()
                        .take(n as usize)
                        .map(|f| format!("{}", *f))
                        .collect();
                    {
                        println!(" {}", bytes.parse::<u64>().ok().unwrap());
                    }
                }
                None => {
                    if let Some(op) = bytes.next().copied() {
                        let op = unsafe { transmute::<u8, OpCode>(op) };
                        print!("{:?} ", op);
                        mode.extend(match op {
                            OpCode::PUSH => vec![8],
                            OpCode::POP => vec![8],
                            OpCode::JUMP => vec![2],
                            OpCode::JUMPIF => vec![2],
                            OpCode::ASSIGN => vec![8],
                            _ => {
                                println!();
                                vec![]
                            }
                        })
                    } else {
                        break;
                    }
                }
            }
        }
        /* loop {
            let op = self.next_op();
            print!("{:?}\t", op);
            match op {}
        } */
    }
}
