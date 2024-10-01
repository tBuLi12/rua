use core::ptr;

pub static mut PTR_BASE_VALUE: *mut u8 = ptr::null_mut();
pub static mut BOTTOM: *mut u8 = ptr::null_mut();
pub static mut TOP: *mut u8 = ptr::null_mut();

pub static mut OBJECT_BODIES: *mut ObjectBodyDescriptor = ptr::null_mut();
pub static mut NEXT_FREE_HEADER: *mut ObjectHeader = ptr::null_mut();

unsafe fn set_heap(base: *mut u8, size: usize) {
    unsafe {
        PTR_BASE_VALUE = base;
        BOTTOM = base;
        OBJECT_BODIES = base as *mut _;
        TOP = base.add(size);
    }
}

#[cfg(all(target_arch = "x86_64", target_os = "windows"))]
mod init {
    use super::*;
    use windows::Win32::System::Memory;

    pub fn heap_init() {
        unsafe {
            let heap = Memory::VirtualAlloc(
                None,
                1 << 20,
                Memory::MEM_COMMIT | Memory::MEM_RESERVE,
                Memory::PAGE_EXECUTE_READWRITE,
            );

            if heap.is_null() {
                panic!("VirtualAlloc failed");
            }

            set_heap(heap as *mut u8, 1 << 20)
        }
    }
}

fn ensure_heap_init() {
    if unsafe { PTR_BASE_VALUE.is_null() } {
        init::heap_init();
    }
}

pub fn alloc_bottom(size: usize) -> *mut u32 {
    ensure_heap_init();
    unsafe {
        let ptr = BOTTOM as *mut u32;
        BOTTOM = BOTTOM.byte_add(4 * size);

        if BOTTOM >= TOP {
            oom()
        }

        ptr
    }
}

fn oom() -> ! {
    eprintln!("Out of memory");
    std::process::exit(1)
}

pub fn as_ptr(mut tagged_index: u32) -> *mut u8 {
    tagged_index &= 0x000fffff;
    if tagged_index >= 1 << 20 {
        panic!("invalid tagged index")
    }

    unsafe { PTR_BASE_VALUE.add(tagged_index as usize) }
}

pub fn as_tagged_index(ptr: *mut u8) -> u32 {
    let offset = unsafe { ptr.byte_offset_from(PTR_BASE_VALUE) };
    if offset < 0 || offset >= 1 << 20 {
        panic!("invalid pointer")
    }
    (offset as u32) | 0xfff00000
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct ObjectHeader {
    body_idx: u32,
}

impl ObjectHeader {
    pub fn body(self) -> *mut ObjectBodyDescriptor {
        as_ptr(self.body_idx) as *mut _
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
struct HeapPtr(u32);

impl HeapPtr {
    fn extra(self) -> u32 {
        self.0 >> (32 - 12)
    }

    fn main(self) -> u32 {
        self.0 & 0x000fffff
    }

    fn with_extra(self, extra: u32) -> HeapPtr {
        Self(extra << (32 - 12) | self.main())
    }

    fn with_main(self, main: u32) -> HeapPtr {
        Self(self.0 & 0xfff00000 | main)
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
struct ObjectBodyDescriptor(HeapPtr);

impl ObjectBodyDescriptor {
    pub fn len(self) -> u32 {
        self.0.extra()
    }

    pub fn header(self) -> *mut ObjectHeader {
        as_ptr(self.0.main()) as *mut _
    }

    fn with_header(self, header: *mut ObjectHeader) -> Self {
        Self(self.0.with_main(as_tagged_index(header as *mut u8)))
    }

    fn is_hole(self) -> bool {
        self.header().is_null()
    }

    fn with_len_minus(self, len: u32) -> Self {
        Self(self.0.with_extra(self.len() - len))
    }

    fn shrink_by(&mut self, size: u32) {
        if size > self.len() {
            return;
        }

        unsafe {
            (self as *mut Self)
                .add(size as usize)
                .write(self.with_len_minus(size));
        }
    }
}

pub unsafe fn alloc_object_header() -> *mut ObjectHeader {
    ensure_heap_init();

    if NEXT_FREE_HEADER.is_null() {
        return alloc_new_object_header();
    }

    let header = NEXT_FREE_HEADER;
    let next_free_idx = (*header).body_idx as usize;

    if next_free_idx != 0 {
        NEXT_FREE_HEADER = PTR_BASE_VALUE.byte_add(next_free_idx - 1) as *mut _;
    } else {
        NEXT_FREE_HEADER = ptr::null_mut();
    }

    header
}

unsafe fn alloc_new_object_header() -> *mut ObjectHeader {
    if ptr::addr_eq(OBJECT_BODIES, BOTTOM) {
        let header = alloc_bottom(1);
        OBJECT_BODIES = header as *mut _;
        return header as *mut _;
    }

    let first = OBJECT_BODIES;

    if !(*first).is_hole() {
        create_leading_hole();
    }

    (*first).shrink_by(1);
    OBJECT_BODIES = OBJECT_BODIES.add(1);
    return first as *mut _;
}

unsafe fn create_leading_hole() {
    let mut descriptor = OBJECT_BODIES;
    while !ptr::addr_eq(descriptor, BOTTOM) {
        if (*descriptor).is_hole() && (*descriptor).len() >= (*OBJECT_BODIES).len() {
            (*descriptor).shrink_by((*OBJECT_BODIES).len() + 1);
            ptr::copy_nonoverlapping(OBJECT_BODIES, descriptor, (*descriptor).len() as usize + 1);
            // move body
        }

        descriptor = descriptor.add((*descriptor).len() as usize + 1);

        *OBJECT_BODIES = (*OBJECT_BODIES).with_header(ptr::null_mut());
    }

    // compact heap

    // alloc more
    let next = alloc_bottom(((*OBJECT_BODIES).len() + 1) as usize);
    ptr::copy_nonoverlapping(
        OBJECT_BODIES,
        next as *mut _,
        (*OBJECT_BODIES).len() as usize + 1,
    );
}

pub fn alloc_top(size: usize) -> *mut u8 {
    ensure_heap_init();
    unsafe {
        TOP = TOP.sub(size);

        if TOP < BOTTOM {
            oom()
        }

        TOP
    }
}
