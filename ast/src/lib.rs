pub struct Position {
    pub line: u32,
    pub column: u32,
}

pub struct Span {
    pub left: Position,
    pub right: Position,
}

pub struct Ident {
    pub value: String,
    pub span: Span,
}

pub struct Index {
    pub lhs: Box<Expr>,
    pub idx: Box<Expr>,
}

pub enum Variable {
    Variable(Ident),
    Index(Index),
}

pub struct Assign {
    pub lhs: Vec<Variable>,
    pub rhs: Vec<Expr>,
}

pub struct While {
    pub condition: Expr,
    pub body: Box<Statement>,
}

pub struct Repeat {
    pub body: Box<Statement>,
    pub until: Expr,
}

pub struct Branch {
    pub condition: Expr,
    pub body: Statement,
}

pub struct If {
    pub branches: Vec<Branch>,
    pub else_body: Option<Box<Expr>>,
}

pub struct NumericalFor {
    pub iter_var: Ident,
    pub initial: Box<Expr>,
    pub limit: Box<Expr>,
    pub step: Option<Box<Expr>>,
    pub body: Vec<Statement>,
}

pub struct For {
    pub vars: Vec<Variable>,
    pub exprs: Vec<Expr>,
    pub body: Vec<Statement>,
}

pub struct Return {
    pub values: Vec<Expr>,
}

pub struct LocalName {
    pub name: Ident,
    pub attr: Option<Ident>,
}

pub struct LocalAssign {
    pub vars: Vec<LocalName>,
    pub exprs: Vec<Expr>,
}

pub struct Call {
    pub lhs: Box<Expr>,
    pub args: Vec<Expr>,
}

pub struct MethodCall {
    pub lhs: Box<Expr>,
    pub name: Ident,
    pub args: Vec<Expr>,
}

pub struct Bool {
    pub value: bool,
    pub span: Span,
}

pub struct Number {
    pub value: f64,
    pub span: Span,
}

pub struct StringLit {
    pub value: String,
    pub span: Span,
}

pub struct Add {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub struct Sub {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub struct Mul {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub struct Div {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub struct Pow {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub struct Neg {
    pub rhs: Box<Expr>,
}

pub struct Eq {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub struct NotEq {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub struct Greater {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub struct Less {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub struct GreaterEq {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub struct LessEq {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub struct And {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub struct Or {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub struct Not {
    pub rhs: Box<Expr>,
}

pub struct Concat {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub struct Len {
    pub rhs: Box<Expr>,
}

pub struct Field {
    pub name: Box<Expr>,
    pub value: Box<Expr>,
}

pub struct Table {
    pub fields: Vec<Field>,
}

pub enum Expr {
    Nil(Span),
    Bool(Bool),
    Number(Number),
    StringLit(StringLit),
    Vararg(Span),
    Variable(Variable),
    Call(Call),
    Add(Add),
    Sub(Sub),
    Mul(Mul),
    Div(Div),
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
    Concat(Concat),
    Len(Len),
    Table(Table),
    Function(Function),
}

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

pub struct Function {
    pub args: Vec<Ident>,
    pub vararg: bool,
    pub body: Vec<Statement>,
}
