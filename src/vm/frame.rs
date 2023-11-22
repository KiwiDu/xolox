use core::slice;
use std::mem::transmute;

use super::basic::OpCode;

pub struct Frame {
    pub chunk: *const u8,
    pub pc: usize,
    pub offset: usize,
}

impl Frame {
    pub(super) fn fun(chunk: &Vec<u8>, offset: usize) -> Self {
        Self {
            chunk: chunk.as_ptr(),
            pc: 0,
            offset,
        }
    }
    pub(super) fn next(&mut self) -> u8 {
        let v = unsafe { *self.chunk.add(self.pc) };
        self.pc += 1;
        v
    }

    pub(super) fn next_op(&mut self) -> OpCode {
        let v = self.next();
        if v >= OpCode::SENTIAL as u8 {
            panic!("Corrupted VM");
        }
        unsafe { transmute(v) }
    }

    fn fwd(&mut self, step: usize) -> &[u8] {
        let beg = self.pc;
        self.pc += step;
        let chunk = unsafe { slice::from_raw_parts(self.chunk.add(beg), step) };
        //&chunk[beg..self.pc]
        chunk
    }

    pub(super) fn next16(&mut self) -> u16 {
        u16::from_be_bytes(self.fwd(2).try_into().unwrap())
    }

    pub(super) fn next64(&mut self) -> u64 {
        u64::from_be_bytes(self.fwd(8).try_into().unwrap())
    }

    pub(super) fn jmp(&mut self) {
        self.pc = self.next16().try_into().unwrap();
    }
}
