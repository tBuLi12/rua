use rua_bc::{Instruction, ValueId};

pub fn interpret(bytecode: &[Instruction], funcs: Vec<Vec<Instruction>>) {
    VM { funcs }.interpret(bytecode)
}

enum Value {
    Bool(bool),
    Number(f64),
    String(String),
}

struct VM {
    funcs: Vec<Vec<Instruction>>,
    locals: Vec<Value>,
    prepared_args: Vec<Value>,
    prepared_captures: Vec<Value>,
    prepared_fields: Vec<(Value, Value)>,
}

impl VM {
    fn interpret(&mut self, bytecode: &[Instruction]) {
        for inst in bytecode {
            self.process(inst);
        }
    }

    fn process(&mut self, instruction: &Instruction) {
        use Instruction::*;

        match instruction {
            Nil => {}
            Bool(value) => self.locals.push(Value::Bool(*value)),
            Number(value) => self.locals.push(Value::Number(*value)),
            String(value) => self.locals.push(Value::String(value.clone())),
            Vararg => {
                unimplemented!()
            }
            CallPrepare(value) => {
                let val = self.resolve(*value);
                self.prepared_args.push(val);
            }
            Call(func) => {
                let val = self.resolve(*func);
            }
            Add(ValueId, ValueId) => {}
            Sub(ValueId, ValueId) => {}
            Mul(ValueId, ValueId) => {}
            Div(ValueId, ValueId) => {}
            Pow(ValueId, ValueId) => {}
            Neg(ValueId) => {}
            Eq(ValueId, ValueId) => {}
            NotEq(ValueId, ValueId) => {}
            Greater(ValueId, ValueId) => {}
            Less(ValueId, ValueId) => {}
            GreaterEq(ValueId, ValueId) => {}
            LessEq(ValueId, ValueId) => {}
            And(ValueId, ValueId) => {}
            Or(ValueId, ValueId) => {}
            Not(ValueId) => {}
            Concat(ValueId, ValueId) => {}
            Len(ValueId) => {}
            TablePrepare(ValueId, ValueId) => {}
            Table => {}
            PrepareCapture(ValueId) => {}
            Function(FuncId) => {}
            SetField {
                table: ValueId,
                field: ValueId,
            } => {}
            GetField {
                table: ValueId,
                field: ValueId,
            } => {}
            Jump(Label) => {}
            Branch(ValueId, Label) => {}
            PrepareAssign(ValueId) => {}
            Assign(ValueId) => {}
            Local => {}
            Drop(usize) => {}
            PrepareReturn(ValueId) => {}
            Return => {}
        }
    }

    fn resolve(&self, value_id: ValueId) -> Value {
        match value_id {
            ValueId::Local(id) => self.locals[id],
        }
    }
}
