// use std::{collections::HashMap, mem};

// use rua_bc::{FuncId, Instruction, ValueId};

// pub fn interpret(bytecode: &[Instruction], funcs: Vec<Vec<Instruction>>) {
//     VM { funcs }.interpret(bytecode)
// }

// #[derive(Clone, Copy, PartialEq, PartialOrd, Ord, Eq)]
// struct GcRef<T: ?Sized>(*mut T);

// impl<T: ?Sized> std::hash::Hash for GcRef<T> {
//     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
//         state.write_usize(self.0 as *mut u8 as usize);
//     }
// }

// #[derive(Clone, Copy, PartialEq, PartialOrd)]
// struct F64(f64);

// impl std::hash::Hash for F64 {
//     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
//         state.write_u64(self.0.to_bits());
//     }
// }

// impl std::cmp::Eq for F64 {}

// #[derive(Hash, PartialEq, Eq)]
// enum Value {
//     Bool(bool),
//     Number(F64),
//     String(GcRef<str>),
//     Function { func: FuncId, captures: Vec<Value> },
//     Table(GcRef<HashMap<Value, Value>>),
// }

// impl Value {
//     fn get_function(self) -> Option<(Vec<Value>, FuncId)> {
//         match self {
//             Value::Function { func, captures } => Some((captures, func)),
//             _ => None,
//         }
//     }

//     fn get_table(self) -> Option<GcRef<HashMap<Value, Value>>> {
//         match self {
//             Value::Table(table) => Some(table),
//             _ => None,
//         }
//     }
// }

// struct VM {
//     funcs: Vec<Vec<Instruction>>,
//     locals: Vec<Value>,
//     captures: Vec<Value>,
//     prepared_args: Vec<Value>,
//     prepared_captures: Vec<Value>,
//     prepared_fields: Vec<(Value, Value)>,
//     frame: usize,
// }

// impl VM {
//     fn interpret(&mut self, bytecode: &[Instruction]) {
//         for inst in bytecode {
//             self.process(inst);
//         }
//     }

//     fn gc_manage<T: ?Sized>(&mut self, value: Box<T>) -> GcRef<T> {}

//     fn process(&mut self, instruction: &Instruction) -> Result<(), &'static str> {
//         use Instruction::*;

//         match instruction {
//             Nil => {}
//             Bool(value) => self.locals.push(Value::Bool(*value)),
//             Number(value) => self.locals.push(Value::Number(F64(*value))),
//             String(value) => self.locals.push(Value::String(
//                 self.gc_manage(value.clone().into_boxed_str()),
//             )),
//             Vararg => {
//                 unimplemented!()
//             }
//             CallPrepare(value) => {
//                 let val = self.resolve(*value);
//                 self.prepared_args.push(val);
//             }
//             Call(func) => {
//                 let val = self.resolve(*func);
//                 let (captures, func) = val.get_function().ok_or("expected function")?;
//                 let args = mem::take(&mut self.prepared_args);
//                 let ret_value = self.call(func, captures, args);
//                 self.locals.push(ret_value);
//             }
//             Add(ValueId, ValueId) => {}
//             Sub(ValueId, ValueId) => {}
//             Mul(ValueId, ValueId) => {}
//             Div(ValueId, ValueId) => {}
//             Pow(ValueId, ValueId) => {}
//             Neg(ValueId) => {}
//             Eq(ValueId, ValueId) => {}
//             NotEq(ValueId, ValueId) => {}
//             Greater(ValueId, ValueId) => {}
//             Less(ValueId, ValueId) => {}
//             GreaterEq(ValueId, ValueId) => {}
//             LessEq(ValueId, ValueId) => {}
//             And(ValueId, ValueId) => {}
//             Or(ValueId, ValueId) => {}
//             Not(ValueId) => {}
//             Concat(ValueId, ValueId) => {}
//             Len(ValueId) => {}
//             TablePrepare(key, value) => {
//                 let key = self.resolve(*key);
//                 let val = self.resolve(*value);
//                 self.prepared_fields.push((key, val));
//             }
//             Table => {
//                 let table = self.gc_manage(Box::new(
//                     mem::take(&mut self.prepared_fields).into_iter().collect(),
//                 ));
//                 self.locals.push(Value::Table(table));
//             }
//             PrepareCapture(value) => {
//                 let capture = self.resolve(*value);
//                 self.prepared_captures.push(capture);
//             }
//             Function(func) => self.locals.push(Value::Function {
//                 func: *func,
//                 captures: mem::take(&mut self.prepared_captures),
//             }),
//             SetField { table, field } => {
//                 let table = self.resolve(*table);
//             }
//             GetField {
//                 table: ValueId,
//                 field: ValueId,
//             } => {}
//             Jump(Label) => {}
//             Branch(ValueId, Label) => {}
//             PrepareAssign(ValueId) => {}
//             Assign(ValueId) => {}
//             Local => {}
//             Drop(usize) => {}
//             PrepareReturn(ValueId) => {}
//             Return => {}
//         }

//         Ok(())
//     }

//     fn call(&mut self, func: FuncId, captures: Vec<Value>, args: Vec<Value>) -> Value {
//         self.frame = self.locals.len();
//         self.locals.extend(args);
//         self.captures = captures;

//         let func = &self.funcs[func.0];
//         self.interpret(func);
//     }

//     fn resolve(&self, value_id: ValueId) -> Value {
//         match value_id {
//             ValueId::Local(id) => self.locals[self.frame + id],
//         }
//     }
// }
