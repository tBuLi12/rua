use std::ptr;

use rua_jit::{jit, Jit};
use rua_parser::parse;

fn main() {
    rua_main()
}

fn rua_main() {
    let statements = parse("print(10)").unwrap();

    //     println!("{:#?}", statements);
    unsafe { Jit::heap_init() };

    // let fun = jit(&statements);
    // println!(
    //     "{:?}",
    //     unsafe { fun.exec(0, ptr::null_mut(), ptr::null_mut()) }.unpack()
    // );

    let str = rua_jit::value::String::new("Hello, world!");

    println!("{}", &*str);
}
