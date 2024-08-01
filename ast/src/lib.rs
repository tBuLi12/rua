#![no_std]

extern crate alloc;
use alloc::{boxed::Box, string::String, vec::Vec};

#[derive(Clone, Copy, Debug)]
pub struct Position {
    pub line: u32,
    pub column: u32,
}

impl Position {
    pub fn extend_back(self, count: u32) -> Span {
        Span {
            left: Position {
                line: self.line,
                column: self.column - count,
            },
            right: self,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Span {
    pub left: Position,
    pub right: Position,
}

#[derive(Debug)]
pub struct RuaError {
    pub message: String,
    pub span: Span,
}

pub type RuaResult<T> = Result<T, RuaError>;

#[derive(Debug)]
pub struct Ident {
    pub value: String,
    pub span: Span,
}

#[derive(Debug)]
pub struct Index {
    pub lhs: Box<Expr>,
    pub idx: Box<Expr>,
}

#[derive(Debug)]
pub enum Variable {
    Variable(Ident),
    Index(Index),
}

#[derive(Debug)]
pub struct Assign {
    pub lhs: Vec<Variable>,
    pub rhs: Vec<Expr>,
}

#[derive(Debug)]
pub struct While {
    pub condition: Expr,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct Repeat {
    pub body: Vec<Statement>,
    pub until: Expr,
}

#[derive(Debug)]
pub struct Branch {
    pub condition: Expr,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct If {
    pub branches: Vec<Branch>,
    pub else_body: Option<Vec<Statement>>,
}

#[derive(Debug)]
pub struct NumericalFor {
    pub iter_var: Ident,
    pub initial: Box<Expr>,
    pub limit: Box<Expr>,
    pub step: Option<Box<Expr>>,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct For {
    pub vars: Vec<Ident>,
    pub exprs: Vec<Expr>,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct Return {
    pub values: Vec<Expr>,
}

#[derive(Debug)]
pub struct LocalName {
    pub name: Ident,
    pub attr: Option<Ident>,
}

#[derive(Debug)]
pub struct LocalAssign {
    pub vars: Vec<LocalName>,
    pub exprs: Vec<Expr>,
}

#[derive(Debug)]
pub struct Call {
    pub lhs: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug)]
pub struct MethodCall {
    pub lhs: Box<Expr>,
    pub name: Ident,
    pub args: Vec<Expr>,
}

#[derive(Debug)]
pub struct Bool {
    pub value: bool,
    pub span: Span,
}

#[derive(Debug)]
pub struct Number {
    pub value: f64,
    pub span: Span,
}

#[derive(Debug)]
pub struct StringLit {
    pub value: String,
    pub span: Span,
}

#[derive(Debug)]
pub struct Add {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Sub {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Mul {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Div {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct IDiv {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Mod {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Pow {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Neg {
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Eq {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct NotEq {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Greater {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Less {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct GreaterEq {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct LessEq {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct And {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Or {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct BitAnd {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct BitOr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct BitXor {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct ShiftLeft {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct ShiftRight {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct BitNot {
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Not {
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Concat {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Len {
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Field {
    pub name: Option<Expr>,
    pub value: Expr,
}

#[derive(Debug)]
pub struct Table {
    pub fields: Vec<Field>,
}

#[derive(Debug)]
pub enum Expr {
    Nil(Span),
    Bool(Bool),
    Number(Number),
    StringLit(StringLit),
    Vararg(Span),
    Variable(Variable),
    Call(Call),
    MethodCall(MethodCall),
    Add(Add),
    Sub(Sub),
    Mul(Mul),
    Div(Div),
    IDiv(IDiv),
    Mod(Mod),
    Pow(Pow),
    Neg(Neg),
    Eq(Eq),
    NotEq(NotEq),
    Greater(Greater),
    Less(Less),
    GreaterEq(GreaterEq),
    LessEq(LessEq),
    And(And),
    Or(Or),
    Not(Not),
    BitAnd(BitAnd),
    BitOr(BitOr),
    BitXor(BitXor),
    BitNot(BitNot),
    ShiftLeft(ShiftLeft),
    ShiftRight(ShiftRight),
    Concat(Concat),
    Len(Len),
    Table(Table),
    Function(Function),
}

#[derive(Debug)]
pub enum Statement {
    Block(Vec<Statement>),
    Assign(Assign),
    LocalAssign(LocalAssign),
    While(While),
    If(If),
    Repeat(Repeat),
    For(For),
    NumericalFor(NumericalFor),
    Label(Ident),
    Goto(Ident),
    Call(Call),
    MethodCall(MethodCall),
    Break,
    Return(Return),
}

#[derive(Debug)]
pub struct Function {
    pub params: Vec<Ident>,
    pub vararg: bool,
    pub body: Vec<Statement>,
}

impl Expr {
    pub fn expr_field(self, field: Expr) -> Variable {
        Variable::Index(Index {
            lhs: Box::new(self),
            idx: Box::new(field),
        })
    }

    pub fn named_field(self, name: Ident) -> Variable {
        self.expr_field(Expr::StringLit(StringLit {
            value: name.value,
            span: name.span,
        }))
    }
}

impl Variable {
    pub fn expr_field(self, field: Expr) -> Self {
        Expr::Variable(self).expr_field(field)
    }

    pub fn named_field(self, name: Ident) -> Self {
        Expr::Variable(self).named_field(name)
    }
}
