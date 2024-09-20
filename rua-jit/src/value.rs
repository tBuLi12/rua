use std::{
    ops::Deref,
    ptr::{self, copy_nonoverlapping},
};

use crate::Function;

#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct Value(usize);

#[derive(Debug)]
pub enum Tagged {
    Number(f32),
    Function(*mut Function),
    String(String),
    // Nil,
    // Bool,
    // UserData,
    // Thread,
    // Table,
}

// pointers
// 1111 1111 1111

// nans
// 1111 1111 1110

static mut PTR_BASE_VALUE: *mut u8 = ptr::null_mut();
static mut BOTTOM: *mut u8 = ptr::null_mut();
static mut TOP: *mut u8 = ptr::null_mut();

fn as_ptr(mut tagged_index: usize) -> *mut u8 {
    tagged_index &= 0x000fffffffffffff;
    if tagged_index >= 1 << 20 {
        panic!("invalid tagged index")
    }

    unsafe { PTR_BASE_VALUE.add(tagged_index) }
}

fn as_tagged_index(ptr: *mut u8) -> usize {
    let offset = unsafe { ptr.byte_offset_from(PTR_BASE_VALUE) };
    if offset < 0 || offset >= 1 << 20 {
        panic!("invalid pointer")
    }
    (offset as usize) | 0xfff0000000000000
}

pub unsafe fn set_heap(base: *mut u8, size: usize) {
    unsafe {
        PTR_BASE_VALUE = base;
        BOTTOM = base;
        TOP = base.add(size);
    }
}

pub unsafe fn alloc() -> *mut usize {
    unsafe {
        let ptr = BOTTOM as *mut usize;
        BOTTOM = BOTTOM.byte_add(8);

        if BOTTOM >= TOP {
            oom()
        }

        ptr
    }
}

pub unsafe fn alloc_top(size: usize) -> *mut u8 {
    TOP = TOP.sub(size);

    if TOP < BOTTOM {
        oom()
    }

    TOP
}

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
        if self.0 & 0xfff0000000000000 == 0xfff0000000000000 {
            let ptr = as_ptr(self.0);
            if ptr >= unsafe { TOP } {
                Tagged::String(String { ptr: ptr as *mut _ })
            } else {
                Tagged::Function(ptr as *mut _)
            }
        } else {
            Tagged::Number(f32::from_bits((self.0 >> 32) as u32))
        }
    }

    pub fn bits(self) -> usize {
        self.0
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

                Value((bits as usize) << 32)
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
            let ptr = alloc_top(value.len() + 2);
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

struct Object {}

impl Object {
    pub fn new() -> Object {
        Object {}
    }
}
