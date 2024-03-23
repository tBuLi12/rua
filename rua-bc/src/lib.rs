#[derive(Clone, Copy)]
pub enum ValueId {
    Local(usize),
    Global(usize),
    Capture(usize),
}

pub struct FuncId(pub usize);

#[derive(Clone, Copy)]
pub struct Label(pub usize);

pub enum Instruction {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Vararg,
    CallPrepare(ValueId),
    Call(ValueId),
    Add(ValueId, ValueId),
    Sub(ValueId, ValueId),
    Mul(ValueId, ValueId),
    Div(ValueId, ValueId),
    Pow(ValueId, ValueId),
    Neg(ValueId),
    Eq(ValueId, ValueId),
    NotEq(ValueId, ValueId),
    Greater(ValueId, ValueId),
    Less(ValueId, ValueId),
    GreaterEq(ValueId, ValueId),
    LessEq(ValueId, ValueId),
    And(ValueId, ValueId),
    Or(ValueId, ValueId),
    Not(ValueId),
    Concat(ValueId, ValueId),
    Len(ValueId),
    TablePrepare(ValueId, ValueId),
    Table,
    PrepareCapture(ValueId),
    Function(FuncId),
    SetField {
        table: ValueId,
        field: ValueId,
        value: ValueId,
    },
    GetField {
        table: ValueId,
        field: ValueId,
    },
    Jump(Label),
    Branch(ValueId, Label),
    Assign(ValueId, ValueId),
    Drop(usize),
}
