#[derive(Clone, Copy, Debug)]
pub enum ValueId {
    Local(usize),
    Global(usize),
    Capture(usize),
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct FuncId(pub usize);

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Label(pub usize);

#[derive(Debug)]
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
    SetField { table: ValueId, field: ValueId },
    GetField { table: ValueId, field: ValueId },
    Jump(Label),
    Branch(ValueId, Label),
    PrepareAssign(ValueId),
    Assign(ValueId),
    Local,
    Drop(usize),
    PrepareReturn(ValueId),
    Return,
}
