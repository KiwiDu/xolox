use std::{fmt::format, mem::transmute};

use crate::value::Val;

//Float NaN: 0111 1111 1111 | 4? 4? 4? 4? 4? 4? 4? | ???1
// 64 - 12 - 1 = 51b
const fn ones(c: u8) -> u64 {
    0x_ffff_ffff_ffff_ffff >> (64 - c)
}

const MASK: u64 = ones(51);

pub const fn pack(data: u64) -> f64 {
    let head: u64 = ones(11) << 52 | 1;
    let data = (data & MASK) << 1;
    //f64::from_bits(data | head)
    unsafe { transmute(data | head) } // unsafe for const
}

pub const fn unpack(p: f64) -> u64 {
    //let bytes = f64::to_be_bytes(p);
    //let data = u64::from_be_bytes(bytes);
    let data: u64 = unsafe { transmute(p) }; // unsafe for const
    (data >> 1) & MASK
}
const TRUE_NAN: f64 = pack(0);

// 7 variants, takes 3bits
pub enum StackType {
    //Float64 if not NaN, so not needed.
    NaN, //Reserved for the true NaN
    Nil,
    True,
    False,
    HeapPtr,     //Points to somewhere on the heap
    StackPtr,    //Points to somewhere on the stack
    InternedStr, //Points to the string pool
}

pub struct StackVal {
    p: f64,
}

impl StackVal {
    pub fn from_f64(f: f64) -> Self {
        if f.is_nan() {
            Self { p: TRUE_NAN }
        } else {
            Self { p: f }
        }
    }
    pub fn to_parts(&self) -> (StackType, u64) {
        let data = unpack(self.p);
        let type_id = data & 0b_0111; // Takes 3 bits
        let data = data >> 3;
        return (unsafe { transmute(type_id as u8) }, data);
    }
    pub fn to_val(&self) -> Val {
        if self.p.is_nan() {
            let (t, data) = self.to_parts();
            return match t {
                StackType::NaN => Val::Num(TRUE_NAN),
                StackType::Nil => Val::Nil,
                StackType::True => Val::Bool(true),
                StackType::False => Val::Bool(false),
                StackType::HeapPtr => Val::Var(format!("h{}", data)),
                StackType::StackPtr => Val::Var(format!("s{}", data)),
                StackType::InternedStr => Val::Str(format!("s{}", data)),
            };
        } else {
            return Val::Num(self.p);
        }
    }
}
