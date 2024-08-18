use std::ptr;

use crate::Function;

#[repr(transparent)]
pub struct Value(usize);

#[derive(Debug)]
pub enum Tagged {
    Float(f32),
    Function(*mut Function),
}

// pointers
// 1111 1111 1111

// nans
// 1111 1111 1110

static mut PTR_BASE_VALUE: *mut u8 = ptr::null_mut();
static mut NEXT_FREE: *mut u8 = ptr::null_mut();

fn as_ptr(tagged_index: usize) -> *mut u8 {
    if tagged_index >= 1 << 20 {
        panic!("invalid tagged index")
    }

    unsafe { PTR_BASE_VALUE.add(tagged_index & 0x000fffffffffffff) }
}

fn as_tagged_index(ptr: *mut u8) -> usize {
    let offset = unsafe { ptr.byte_offset_from(PTR_BASE_VALUE) };
    if offset < 0 || offset >= 1 << 20 {
        panic!("invalid pointer")
    }
    (offset as usize) | 0xfff0000000000000
}

pub unsafe fn set_ptr_base(base: *mut u8) {
    unsafe {
        PTR_BASE_VALUE = base;
        NEXT_FREE = base;
    }
}

pub unsafe fn alloc() -> *mut usize {
    unsafe {
        let ptr = NEXT_FREE as *mut usize;
        NEXT_FREE = NEXT_FREE.byte_add(8);
        ptr
    }
}

impl Value {
    pub fn unpack(self) -> Tagged {
        if self.0 & 0xfff0000000000000 == 0xfff0000000000000 {
            Tagged::Function(as_ptr(self.0) as *mut _)
        } else {
            Tagged::Float(f32::from_bits((self.0 >> 32) as u32))
        }
    }

    pub fn bits(self) -> usize {
        self.0
    }
}

impl Tagged {
    pub fn pack(self) -> Value {
        match self {
            Self::Float(value) => {
                let mut bits = value.to_bits();
                if value.is_nan() {
                    bits &= 0x000fffff;
                    bits |= 0xffe00000;
                }

                Value((bits as usize) << 32)
            }
            Self::Function(fun) => Value(as_tagged_index(fun as *mut _)),
        }
    }
}
