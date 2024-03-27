use ast::{RuaError, RuaResult};
use rua_bc_gen::to_bytecode;
use rua_parser::parse;

fn main() {
    match rua_main() {
        Ok(()) => {}
        Err(RuaError { message, span }) => {
            eprintln!(
                "error: {} at line {} column {}",
                message, span.left.line, span.left.column
            );
        }
    }
}

fn rua_main() -> RuaResult<()> {
    let statements = parse(
        "
a = 3;
b = a + 4;
function some()
    d = a;
end
some();
        ",
    )?;
    println!("{:?}", &statements);
    let (funcs, code) = to_bytecode(&statements);

    for inst in code {
        println!("{:?}", inst);
    }

    for (i, func) in funcs.iter().enumerate() {
        println!("\n function {}", i);
        for inst in func {
            println!("{:?}", inst);
        }
    }

    Ok(())
}
