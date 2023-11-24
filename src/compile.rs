use std::{
    collections::{HashSet, VecDeque},
    mem::{swap, transmute},
    rc::Rc,
};

use crate::vm::{
    ftbit::{StackType, StackVal},
    local::Local,
    vmval::VmVal,
};
use crate::{
    sexpr::S,
    token::{Keywords, Token, TokenType},
    vm::basic::OpCode,
    //value::Val,
};
type Str = Rc<str>; //RcStr<DefaultHasher>;
type Val = VmVal<Str>;

#[derive(Debug)]
pub struct Compiler {
    pub chunk: Vec<u8>,
    depth: u32,
    pub heap: Vec<Val>,
    pub strings: HashSet<Str>,
    locals: Vec<Local<Str>>,
    heap_offset: usize,
}
impl Compiler {
    pub fn op_return(&mut self) {
        self.op(OpCode::RETURN)
    }
    pub fn emit_value(&mut self, v: Val) {
        let ptr = self.alloc(v).to_raw();
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

    /* fn decl_fun(&mut self, name: &str, arity: u8, chunk: Vec<u8>) {
        let name = self.intern(name);
        self.alloc(Val::Fun(name, arity, chunk));
    } */
    fn emit_fun(&mut self, name: &str, arity: u8, chunk: Vec<u8>) {
        let name = self.intern(name);
        self.emit_value(Val::Fun(name, arity, chunk));
    }

    pub fn emit(&mut self, ptr: u64) {
        self.chunk.reserve(1 + 8);
        self.chunk.push(OpCode::PUSH as u8);
        let bytes = u64::to_be_bytes(ptr);
        self.chunk.extend(bytes)
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
        self.heap.len() - 1 + self.heap_offset
    }
}
impl Compiler {
    pub fn new(heap_size: usize) -> Self {
        Self {
            depth: 0,
            chunk: Vec::with_capacity(32),
            heap: Vec::with_capacity(heap_size),
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

    fn intern(&mut self, s: &str) -> Str {
        if let Some(a) = self.strings.get(s) {
            a.clone()
        } else {
            self.strings.insert(s.into());
            self.intern(s)
        }
    }

    pub fn op(&mut self, c: OpCode) {
        self.chunk.push(c as u8);
    }

    pub fn ops<const N: usize>(&mut self, cs: [OpCode; N]) {
        self.chunk.extend(cs.iter().map(|&c| c as u8));
    }

    fn push16(&mut self, d: u16) {
        self.chunk.extend(u16::to_be_bytes(d))
    }

    fn slot(&mut self, size: usize) -> (usize, usize) {
        let l = self.chunk.len();
        self.chunk.extend(vec![0; size]);
        (l, l + size)
    }

    fn fill(&mut self, slot: (usize, usize), data: usize) {
        let (b, e) = slot;
        let data = usize::to_be_bytes(data);
        let n = data.len();
        let data = &data[n - (e - b)..n];
        self.chunk[b..e].clone_from_slice(data);
    }

    fn name(t: &S<Token>) -> &str {
        match t {
            S::Atom(Token::Idt(s)) => &s[..],
            S::Atom(Token::Str(s)) => &s[..],
            _ => panic!("Invalid Atom!"),
        }
    }

    fn resolve_local(&self, local: &str) -> Option<usize> {
        for (i, l) in self.locals.iter().enumerate() {
            if l.name.as_ref() == local {
                println!("\n\t{local}:{i}");
                return Some(i);
            }
        }
        None
    }
    fn atom(&mut self, t: &Token, set: bool) {
        if set {
            match t {
                Token::Idt(s) => {
                    if let Some(i) = self.resolve_local(s) {
                        self.op(OpCode::LSET);
                        self.push16(i as u16);
                    } else {
                        self.emit_var(s)
                    }
                }
                _ => panic!("Cannot assign to non-variables!"),
            }
        } else {
            match t {
                Token::Idt(s) => {
                    if let Some(i) = self.resolve_local(s) {
                        self.op(OpCode::LGET);
                        self.push16(i as u16);
                    } else {
                        //Only globals need to be resolved at runtime
                        self.emit_var(s);
                        self.op(OpCode::GGET);
                    }
                }
                Token::Str(s) => self.emit_str(s),
                Token::Kwd(Keywords::True) => self.op(OpCode::TRUE),
                Token::Kwd(Keywords::False) => self.op(OpCode::FALSE),
                Token::Kwd(Keywords::Nil) => self.op(OpCode::NIL),
                Token::Num(f) => self.emit_value(Val::Num(*f)),
                Token::NoOp => (), //No Operation
                _ => panic!("Invalid Atom! `{:#?}`", t),
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
                }
                _ => panic!("Unexpected unary operator: {}!", tt),
            }
        } else if let Token::Kwd(Keywords::Print) = h {
            self.compile(t, set);
            self.op(OpCode::PRINT);
        } else if let Token::Kwd(Keywords::Return) = h {
            self.compile(t, set);
            self.op(OpCode::RETURN);
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
                EqualEqual => c.op(OpCode::EQ),
                BangEqual => c.ops([OpCode::EQ, OpCode::NOT]),
                Less => c.op(OpCode::LT),
                LessEqual => c.ops([OpCode::GT, OpCode::NOT]),
                Greater => c.op(OpCode::GT),
                GreaterEqual => c.ops([OpCode::LT, OpCode::NOT]),
                _ => panic!("Unexpected binary operator: {}!", op),
            }
        }
        fn global_define(c: &mut Compiler, left: &S<Token>, right: &S<Token>) {
            c.compile(left, true);
            c.compile(right, false);
            c.op(OpCode::GSET);
        }
        fn global_assign(c: &mut Compiler, left: &S<Token>, right: &S<Token>) {
            c.compile(left, true);
            c.compile(right, false);
            c.op(OpCode::GSET);
        }
        fn local_define(c: &mut Compiler, d: u32, left: &S<Token>, right: &S<Token>) {
            c.compile(right, false);
            let name = c.intern(Compiler::name(left));
            c.locals.push(Local::new(name, d));
        }
        fn local_assign(c: &mut Compiler, _d: u32, left: &S<Token>, right: &S<Token>) {
            match c.resolve_local(Compiler::name(left)) {
                Some(i) => {
                    c.compile(right, false);
                    c.op(OpCode::LSET);
                    c.push16(i as u16);
                }
                None => global_assign(c, left, right),
            }
        }

        if set {
            panic!("Cannot be used as a left value.");
        }
        match h {
            Token::Kwd(Keywords::Var) => match self.depth {
                0 => global_define(self, left, right),
                d => local_define(self, d, left, right),
            },
            Token::Op(TokenType::Equal) => match self.depth {
                0 => {
                    self.compile(left, true);
                    self.compile(right, false);
                    self.op(OpCode::GSET);
                }
                d => local_assign(self, d, left, right),
            },
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
                let num_local = self.locals.iter().filter(|l| l.depth == self.depth).count();
                for _ in 0..num_local {
                    self.op(OpCode::POP);
                }
                self.depth -= 1;
            }
            fun @ Token::Idt(name) => match (name.as_str(), t.len()) {
                ("clock", 0) => {
                    self.op(OpCode::TIME);
                }
                _ => {
                    for arg in t.iter().rev() {
                        self.compile(arg, false);
                    }
                    self.atom(fun, set);
                    self.op(OpCode::CALL);
                }
            },
            Token::Kwd(Keywords::Fun) => {
                if let [S::Atom(fun @ Token::Idt(name)), args @ .., body] = t.as_slice() {
                    println!("\t@{}", name);

                    let mut forked = Compiler::new(32);
                    //Forking
                    swap(&mut self.heap, &mut forked.heap);
                    //fun_compiler.heap_offset = self.heap.len() + 1;
                    forked.depth = self.depth + 1;
                    forked.locals.extend(self.locals.iter().cloned());
                    forked.locals.extend(args.iter().enumerate().map(|(i, s)| {
                        if let S::Atom(Token::Idt(n)) = s {
                            Local::new(self.intern(n), i as u32)
                        } else {
                            panic!("Invalid args list")
                        }
                    }));

                    forked.compile(body, false);
                    forked.ops([OpCode::NIL, OpCode::RETURN]);

                    forked.dasm();

                    let mut fun_chunk = forked.chunk;
                    fun_chunk.shrink_to_fit();
                    let arity = args.len() as u8;

                    swap(&mut self.heap, &mut forked.heap);
                    self.atom(fun, true);
                    self.emit_fun(name, arity, fun_chunk);
                    self.op(OpCode::GSET);
                } else {
                    panic!("Invalid fnction!");
                }
            }
            Token::Kwd(Keywords::If) => {
                if let [cond, thendo, elsedo] = t.as_slice() {
                    //The condition
                    self.compile(cond, false);

                    //Jump to THEN if the condition is true
                    self.op(OpCode::JUMPIF);
                    let skip_else = self.slot(2);
                    //The ELSE branch begins.
                    self.compile(elsedo, set);
                    self.op(OpCode::JUMP); // If in ELSE, always skip THEN
                    let skip_then = self.slot(2);
                    //The ELSE branch ends
                    self.fill(skip_else, self.chunk.len());
                    self.compile(thendo, set); //The THEN branch
                    self.fill(skip_then, self.chunk.len());
                } else {
                    panic!("Invalid `if`!");
                }
            }
            Token::Kwd(_) => todo!(),
            _ => panic!("Unsupported cons `{}`", h),
        }
    }

    pub fn dasm(&mut self) {
        let mut mode: VecDeque<u8> = VecDeque::new();
        let mut bytes = self.chunk.iter().enumerate();
        loop {
            match mode.pop_front() {
                Some(n) => {
                    let bytes: String = bytes
                        .by_ref()
                        .take(n as usize)
                        .map(|f| format!("{}", *(f.1)))
                        .collect();
                    {
                        println!(" {}", bytes.parse::<u64>().ok().unwrap());
                    }
                }
                None => {
                    if let Some((i, &op)) = bytes.next() {
                        let op = unsafe { transmute::<u8, OpCode>(op) };
                        print!("{:2} {:?} ", i, op);
                        mode.extend(match op {
                            OpCode::PUSH => vec![8],
                            //OpCode::POP => vec![8],
                            OpCode::JUMP => vec![2],
                            OpCode::JUMPIF => vec![2],
                            OpCode::LGET => vec![2],
                            OpCode::LSET => vec![2],
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
