#![no_std]

extern crate alloc;

use core::mem;

use alloc::vec::Vec;

pub struct Asm {
    buf: Vec<u16>,
}

#[derive(Debug, Clone, Copy)]
pub struct Register(u8);

pub const R0: Register = Register(0);
pub const R1: Register = Register(1);
pub const R2: Register = Register(2);
pub const R3: Register = Register(3);
pub const R4: Register = Register(4);
pub const R5: Register = Register(5);
pub const R6: Register = Register(6);
pub const R7: Register = Register(7);
pub const R8: Register = Register(8);
pub const R9: Register = Register(9);
pub const R10: Register = Register(10);
pub const R11: Register = Register(11);
pub const R12: Register = Register(12);
pub const R13: Register = Register(13);
pub const R14: Register = Register(14);
pub const R15: Register = Register(15);

impl Register {
    pub fn new(n: u8) -> Register {
        Register(n)
    }

    fn at(self, offset: u16) -> u16 {
        (self.0 as u16) << offset
    }
}

impl Asm {
    pub fn new() -> Asm {
        Asm { buf: Vec::new() }
    }

    pub unsafe fn exec(&self, a1: u32, a2: u32) -> u32 {
        let f: extern "C" fn(u32, u32) -> u32 = mem::transmute(self.buf.as_ptr().byte_add(1));
        core::arch::asm! {
            "DSB",
            "ISB"
        }
        f(a1, a2)
    }

    pub fn mov(&mut self, dst: Register, src: Register) {
        let base: u16 = 0b0100011000000000;
        self.buf
            .push(base | src.at(3) | dst.at(0) & 0b111 | dst.at(4) & 0b10000000);
    }

    pub fn ldr_imm(&mut self, dst: Register, ptr: Register, imm: u16) {
        let base: u16 = 0b0110100000000000;
        self.buf
            .push(base | (imm << 6) as u16 | dst.at(0) | ptr.at(3));
    }

    pub fn add(&mut self, dst: Register, left: Register, right: Register) {
        let base: u16 = 0b0001100000000000;
        self.buf.push(base | left.at(6) | right.at(3) | dst.at(0));
    }

    pub fn iter(&self) -> impl Iterator<Item = u16> + '_ {
        self.buf.iter().copied()
    }
}
