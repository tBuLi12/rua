use core::ptr;

static mut PTR_BASE_VALUE: *mut u8 = ptr::null_mut();
static mut BOTTOM: *mut u8 = ptr::null_mut();
static mut TOP: *mut u8 = ptr::null_mut();

unsafe fn set_heap(base: *mut u8, size: usize) {
    unsafe {
        PTR_BASE_VALUE = base;
        BOTTOM = base;
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
                Memory::PAGE_READWRITE,
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

fn oom() -> ! {
    eprintln!("Out of memory");
    std::process::exit(1)
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
