use std::{
    ops::Deref,
    ptr::{self, copy_nonoverlapping},
};

use crate::{
    heap::{self, as_ptr, as_tagged_index, ObjectHeader},
    Function,
};

#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct Value(u32);

#[derive(Debug)]
pub enum Tagged {
    // Table,
    Function(*mut Function),
    // Thread,
    // UserData,
    String(String),
    Number(f32),
    Bool,
    Nil,
}

// nans
// 1111 1111 110x
// 1111 1111 101x

// values
// 1111 1111 1110

// pointers
// 1111 1111 1111

fn oom() -> ! {
    eprintln!("Out of memory");
    std::process::exit(1)
}

fn string_too_long() -> ! {
    eprintln!("String too long");
    std::process::exit(1)
}

impl Value {
    pub fn unpack(self) -> Tagged {
        if self.0 & 0xfff00000 == 0xfff00000 {
            let ptr = as_ptr(self.0);
            if ptr >= unsafe { heap::TOP } {
                Tagged::String(String { ptr: ptr as *mut _ })
            } else {
                Tagged::Function(ptr as *mut _)
            }
        } else {
            Tagged::Number(f32::from_bits(self.0))
        }
    }

    pub fn bits(self) -> u32 {
        self.0
    }

    pub fn nil() -> Value {
        Value(0xfff00000)
    }
}

impl Tagged {
    pub fn pack(self) -> Value {
        match self {
            Self::Number(value) => {
                let mut bits = value.to_bits();
                if value.is_nan() {
                    bits &= 0x000fffff;
                    bits |= 0xffe00000;
                }

                Value(bits)
            }
            Self::Function(fun) => Value(as_tagged_index(fun as *mut _)),
            Self::String(string) => Value(as_tagged_index(string.ptr as *mut u8)),
        }
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub struct String {
    ptr: *mut u16,
}

impl String {
    pub fn new(value: &str) -> String {
        if value.len() > u16::MAX as usize {
            string_too_long();
        }

        unsafe {
            let ptr = heap::alloc_top(value.len() + 2);
            copy_nonoverlapping(value.as_ptr(), ptr, value.len());
            let size_ptr = ptr.add(value.len()) as *mut u16;
            *size_ptr = value.len() as u16;
            String { ptr: size_ptr }
        }
    }
}

impl Deref for String {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        unsafe {
            let size = *self.ptr;
            let ptr = self.ptr as *mut u8;
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                ptr.sub(size as usize),
                size as usize,
            ))
        }
    }
}

struct Object {
    header: *mut ObjectHeader,
}

impl Object {
    pub fn new() -> Object {
        Object {
            header: unsafe { heap::alloc_object_header() },
        }
    }

    pub fn set(&self, key: Value, value: Value) {
        unimplemented!()
    }

    pub fn get(&self, key: Value) -> Value {
        let header = unsafe { &*self.header };
        if header.body().is_null() {
            return Value::nil();
        }

        let body = unsafe { &*header.body() };
        let len = body.len() as usize;
        let values = unsafe { std::slice::from_raw_parts(header.body().add(1) as *mut Value, len) };

        for [current_key, value] in values.chunks_exact(2) {
            if key == *current_key {
                return *value;
            }
        }

        Value::nil()
    }
}
