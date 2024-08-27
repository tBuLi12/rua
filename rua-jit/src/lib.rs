// #![no_std]

use std::{collections::HashMap, os, process::id};

use ast::{Add, Assign, Call, Expr, Ident, Index, Number, Statement, Variable};
use iced_x86::code_asm::{
    asm_traits::{CodeAsmPop, CodeAsmPush},
    dword_ptr, qword_ptr, r8, r9, rax, rbp, rcx, rdx, rsp, AsmMemoryOperand, CodeAssembler,
};
use iced_x86::{Decoder, DecoderOptions, Formatter, Instruction, NasmFormatter};
use value::{alloc, set_heap, Tagged, Value};
use windows::Win32::System::Memory;

pub mod value;

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Function {
    code_buffer: *mut u8,
}

// impl Drop for Function {
//     fn drop(&mut self) {
//         unsafe {
//             Memory::VirtualFree(self.code_buffer as *mut _, 0, Memory::MEM_RELEASE);
//         }
//     }
// }

impl Function {
    pub fn exec(
        &self,
        arg_count: usize,
        args_ptr: *mut Value,
        frame_base: *mut FrameBase,
    ) -> Value {
        unsafe {
            let func: extern "C" fn(
                arg_count: usize,
                args_ptr: *mut Value,
                frame_base: *mut FrameBase,
            ) -> Value = std::mem::transmute(self.code_buffer);
            func(arg_count, args_ptr, frame_base)
        }
    }
}

pub fn jit(chunk: &[Statement]) -> Function {
    let mut jit = Jit::new();
    jit.statements(chunk);
    jit.get_function()
}

pub struct Jit {
    asm: CodeAssembler,
    frame_size: usize,
    globals: HashMap<String, *mut Value>,
}

#[repr(C)]
pub struct FrameBase {
    prev: *mut FrameBase,
    size: usize,
}

extern "C" fn rua_call(
    arg_count: usize,
    args_ptr: *mut Value,
    frame_base: *mut FrameBase,
    fun: Value,
) -> Value {
    match fun.unpack() {
        Tagged::Function(fun) => unsafe { *fun }.exec(arg_count, args_ptr, frame_base),
        _ => {
            eprintln!("Call to non-function");
            std::process::exit(1)
        }
    }
}

extern "C" fn rua_add(lhs: Value, rhs: Value) -> Value {
    match (lhs.unpack(), rhs.unpack()) {
        (Tagged::Float(l), Tagged::Float(r)) => Tagged::Float(l + r).pack(),
        _ => {
            eprintln!("Invalid addition operands");
            std::process::exit(1)
        }
    }
}

extern "C" fn rua_print(
    arg_count: usize,
    args_ptr: *mut Value,
    frame_base: *mut FrameBase,
) -> Value {
    eprintln!("Rua print");
    std::process::exit(1)
}

impl Jit {
    pub unsafe fn heap_init() {
        let heap = Memory::VirtualAlloc(
            None,
            1 << 20,
            Memory::MEM_COMMIT | Memory::MEM_RESERVE,
            Memory::PAGE_READWRITE,
        );

        if heap.is_null() {
            panic!("VirtualAlloc failed");
        }

        set_heap(heap as *mut u8, 1 << 20)
    }

    fn new() -> Jit {
        Jit {
            asm: CodeAssembler::new(64).unwrap(),
            frame_size: 0,
            globals: [(
                "print".to_string(),
                Box::into_raw(Box::new(
                    Tagged::Function(unsafe {
                        let ptr = alloc() as *mut Function;
                        ptr.write(Function {
                            code_buffer: rua_print as *mut u8,
                        });
                        ptr
                    })
                    .pack(),
                )),
            )]
            .into_iter()
            .collect(),
        }
    }

    fn push<T>(&mut self, value: T) -> usize
    where
        CodeAssembler: CodeAsmPush<T>,
    {
        self.asm
            .add(qword_ptr(rsp - self.frame_size as i32), 1)
            .unwrap();
        self.frame_size += 1;
        self.asm.push(value).unwrap();

        self.frame_size
    }

    fn pop_slots(&mut self, n: i32) {
        self.asm.sub(qword_ptr(rsp - self.frame_size), n).unwrap();
        self.frame_size -= n as usize;
    }

    fn pop<T>(&mut self, value: T)
    where
        CodeAssembler: CodeAsmPop<T>,
    {
        self.pop_slots(1);
        self.asm.pop(value).unwrap();
    }

    fn pop_void(&mut self) {
        self.pop_slots(1);
        self.asm.add(rsp, 8).unwrap();
    }

    fn pop_many(&mut self, count: usize) {
        self.pop_slots(count as i32);
        self.asm.add(rsp, 8 * count as i32).unwrap();
    }

    fn next_slot_offset(&self) -> usize {
        self.frame_size + 1
    }

    fn slot(&self, offset: usize) -> AsmMemoryOperand {
        qword_ptr(rsp - self.frame_size + offset)
    }

    fn frame_base(&self) -> AsmMemoryOperand {
        qword_ptr(rsp - self.frame_size - 1)
    }

    fn statements(&mut self, statements: &[Statement]) {
        for statement in statements {
            match statement {
                // Statement::Assign(Assign { lhs, rhs }) => {
                //     let values: Vec<_> = rhs.iter().map(|expr| self.expr(expr)).collect();
                // }
                Statement::Call(Call { lhs, args }) => {
                    self.expr(&lhs);
                    let fun = self.push(rax);
                    let args_base = self.next_slot_offset();
                    for arg in args {
                        self.expr(&arg);
                        self.push(rax);
                    }
                    self.asm.sub(rsp, 32).unwrap();
                    self.asm.mov(rcx, args.len() as u64).unwrap();
                    self.asm.lea(rdx, self.slot(args_base)).unwrap();
                    self.asm.lea(r8, self.frame_base()).unwrap();
                    self.asm.mov(r9, self.slot(fun)).unwrap();
                    self.asm.call(rua_call as u64).unwrap();
                    self.asm.add(rsp, 32).unwrap();
                    self.pop_many(args.len());
                }
                _ => unimplemented!(),
            }
        }
    }

    fn resolve(&self, variable: Variable) -> usize {
        // match variable {
        //     Variable::Index(Index { idx, lhs }) => {}
        //     Variable::Variable(Ident { value, .. }) => {}
        // }
        unimplemented!()
    }

    fn expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Variable(Variable::Variable(ident)) => {
                if let Some(&value) = self.globals.get(&ident.value) {
                    self.asm.mov(rax, qword_ptr(value as usize)).unwrap();
                } else {
                    eprintln!("Undefined variable {}", ident.value);
                    std::process::exit(1);
                }
            }
            Expr::Add(Add { lhs, rhs }) => {
                self.expr(&lhs);
                let lhs = self.push(rax);
                self.expr(&rhs);
                self.asm.mov(rcx, self.slot(lhs)).unwrap();
                self.asm.mov(rdx, rax).unwrap();
                self.asm.call(rua_add as u64).unwrap();
                self.pop_void();
            }
            Expr::Number(Number { value, .. }) => {
                let value = *value as f32;
                self.asm
                    .mov(rax, Tagged::Float(value).pack().bits() as u64)
                    .unwrap()
            }
            _ => unimplemented!(),
        }
    }

    fn get_function(&mut self) -> Function {
        let bytes = self.asm.assemble(0).unwrap();

        let mut decoder = Decoder::with_ip(64, &bytes, 0, DecoderOptions::NONE);
        let mut formatter = NasmFormatter::new();
        let mut instruction = Instruction::default();
        let mut output = String::new();
        while decoder.can_decode() {
            // There's also a decode() method that returns an instruction but that also
            // means it copies an instruction (40 bytes):
            //     instruction = decoder.decode();
            decoder.decode_out(&mut instruction);

            // Format the instruction ("disassemble" it)
            output.clear();
            formatter.format(&instruction, &mut output);

            // Eg. "00007FFAC46ACDB2 488DAC2400FFFFFF     lea       rbp,[rsp-100h]"
            print!("{:016X} ", instruction.ip());
            let start_index = (instruction.ip() - 0) as usize;
            let instr_bytes = &bytes[start_index..start_index + instruction.len()];
            for b in instr_bytes.iter() {
                print!("{:02X}", b);
            }
            if instr_bytes.len() < 10 {
                for _ in 0..10 - instr_bytes.len() {
                    print!("  ");
                }
            }
            println!(" {}", output);
        }

        self.asm.reset();
        let code_buffer = unsafe {
            Memory::VirtualAlloc(
                None,
                bytes.len(),
                Memory::MEM_COMMIT | Memory::MEM_RESERVE,
                Memory::PAGE_EXECUTE_READWRITE,
            )
        };

        if code_buffer.is_null() {
            panic!("VirtualAlloc failed");
        }

        unsafe {
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), code_buffer as *mut u8, bytes.len());

            // Memory::VirtualProtect(
            //     None,
            //     bytes.len(),
            //     Memory::MEM_COMMIT | Memory::MEM_RESERVE,
            //     Memory::PAGE_EXECUTE_READWRITE,
            // )
        }

        Function {
            code_buffer: code_buffer as *mut u8,
        }
    }

    pub fn test(a: usize, b: usize) -> usize {
        let mut asm = CodeAssembler::new(64).unwrap();
        asm.add(rcx, rdx).unwrap();
        asm.mov(rax, rcx).unwrap();
        asm.ret().unwrap();
        let bytes = asm.assemble(0).unwrap();

        unsafe {
            let ptr = Memory::VirtualAlloc(
                None,
                bytes.len(),
                Memory::MEM_COMMIT | Memory::MEM_RESERVE,
                Memory::PAGE_EXECUTE_READWRITE,
            );

            if ptr.is_null() {
                panic!("VirtualAlloc failed");
            }

            std::ptr::copy_nonoverlapping(bytes.as_ptr(), ptr as *mut u8, bytes.len());

            let func: extern "C" fn(usize, usize) -> usize = std::mem::transmute(ptr);
            func(a, b)
        }
    }
}
