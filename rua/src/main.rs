use ast::{RuaError, RuaResult};
// use rua_bc_gen::to_bytecode;
use rua_parser::parse;

fn main() {
    rua_main()
}

fn rua_main() {
    let statements = parse(
        "
a = 3;
b = a + 4;
function some()
    d = a;
end
some();
        ",
    )
    .unwrap();

    println!("{:#?}", statements);
}
