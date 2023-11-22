use std::mem::transmute;

//Float NaN: 0111 1111 1111 | 4? 4? 4? 4? 4? 4? 4? | ???1
// 64 - 12 - 1 = 51b
const fn ones(c: u8) -> u64 {
    0x_ffff_ffff_ffff_ffff >> (64 - c)
}

const MASK: u64 = ones(51);

pub const fn pack(data: u64) -> u64 {
    let head: u64 = ones(11) << 52 | 1;
    let data = (data & MASK) << 1;
    //f64::from_bits(data | head)
    data | head
    //unsafe { transmute(data | head) } // unsafe for const
}

pub const fn unpack(p: u64) -> u64 {
    //let bytes = f64::to_be_bytes(p);
    //let data = u64::from_be_bytes(bytes);
    //let data: u64 = unsafe { transmute(p) }; // unsafe for const
    (p >> 1) & MASK
}
const TRUE_NAN: u64 = pack(0);

// 7 variants, takes 3bits
#[derive(Debug)]
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

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StackVal {
    p: u64,
}

#[derive(Debug)]
pub enum Unpack {
    Float(f64),
    NaNBox(StackType, u64),
}

impl StackVal {
    pub const TRUE: Self = Self::from_parts(StackType::True, 0);
    pub const FALSE: Self = Self::from_parts(StackType::False, 0);
    pub const NIL: Self = Self::from_parts(StackType::Nil, 0);
    pub fn unpack(&self) -> Unpack {
        let f = self.to_f64();
        if f.is_nan() {
            let (t, d) = self.to_parts();
            Unpack::NaNBox(t, d)
        } else {
            Unpack::Float(f)
        }
    }
    pub fn from_raw(p: u64) -> Self {
        Self { p }
    }
    pub fn to_raw(&self) -> u64 {
        self.p
    }
    pub fn from_f64(f: f64) -> Self {
        Self {
            p: if f.is_nan() {
                TRUE_NAN
            } else {
                unsafe { transmute(f) }
            },
        }
    }
    pub fn to_f64(&self) -> f64 {
        unsafe { transmute(self.p) }
    }
    pub const fn to_parts(&self) -> (StackType, u64) {
        let data = unpack(self.p);
        let type_id = data & 0b_0111; // Takes 3 bits
        let data = data >> 3;
        return (unsafe { transmute(type_id as u8) }, data);
    }

    pub const fn from_parts(s: StackType, d: u64) -> Self {
        let type_id = s as u64;
        let data = type_id | (d << 3);
        Self { p: pack(data) }
    }
}
