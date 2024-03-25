use ast::*;
use lexer::{Keyword, Punctuation, Source, Token};

use crate::lexer::Lexer;

mod combinators;
mod lexer;

pub struct StrSource {
    text: Vec<String>,
    line: u32,
    column: u32,
}

impl StrSource {
    pub fn new(text: &str) -> Self {
        StrSource {
            text: text.split_inclusive('\n').map(str::to_string).collect(),
            line: 0,
            column: 0,
        }
    }
}

impl lexer::Source for StrSource {
    fn next(&mut self) -> RuaResult<Option<char>> {
        let next = self.text[self.line as usize][self.column as usize..]
            .chars()
            .next();

        match next {
            Some(character) => {
                self.column += character.len_utf8() as u32;
                Ok(Some(character))
            }
            None => {
                if self.line + 1 == self.text.len() as u32 {
                    Ok(None)
                } else {
                    self.line += 1;
                    self.column = 0;
                    self.next()
                }
            }
        }
    }
}

pub fn parse(chunk: &str) -> RuaResult<Vec<Statement>> {
    let mut lexer = Lexer::new(StrSource::new(chunk))?;
    let mut tokens = Parser {
        current: lexer.next()?,
        lexer,
    };

    Ok(rule::statements.parse(&mut tokens)?.unwrap_or(vec![]))
}

struct Parser<S> {
    lexer: Lexer<S>,
    current: Token,
}

trait Tokens {
    fn next(&mut self) -> RuaResult<Token>;
    fn current(&self) -> &Token;
}

impl<S: Source> Tokens for Parser<S> {
    fn next(&mut self) -> RuaResult<Token> {
        Ok(std::mem::replace(&mut self.current, self.lexer.next()?))
    }

    fn current(&self) -> &Token {
        &self.current
    }
}

trait Rule: Copy {
    type Output;

    fn parse(self, tokens: &mut impl Tokens) -> RuaResult<Option<Self::Output>>;

    fn must_parse(self, tokens: &mut impl Tokens) -> RuaResult<Self::Output> {
        self.parse(tokens)?.ok_or_else(|| ast::RuaError {
            message: "syntax error".to_string(),
            span: tokens.current().span(),
        })
    }

    fn fold<R: Rule>(
        self,
        rest: R,
        fun: impl Fn(Self::Output, R::Output) -> Self::Output + Copy,
    ) -> impl Rule<Output = Self::Output> {
        combinators::Fold {
            first: self,
            rest,
            fold: fun,
        }
    }

    fn and<R: Rule>(self, right: R) -> impl Rule<Output = (Self::Output, R::Output)> {
        combinators::And { left: self, right }
    }

    fn and_discard<R: Rule>(self, right: R) -> impl Rule<Output = Self::Output> {
        self.and(right).map(|(left, _)| left)
    }

    fn discard_and<R: Rule>(self, right: R) -> impl Rule<Output = R::Output> {
        self.and(right).map(|(_, right)| right)
    }

    fn or<R: Rule>(self, right: R) -> combinators::Or<Self, R> {
        combinators::Or { left: self, right }
    }

    fn map<Out>(self, fun: impl FnOnce(Self::Output) -> Out + Copy) -> impl Rule<Output = Out> {
        combinators::Map { rule: self, fun }
    }

    fn try_map<Out>(
        self,
        fun: impl FnOnce(Self::Output) -> RuaResult<Out> + Copy,
    ) -> impl Rule<Output = Out> {
        combinators::TryMap { rule: self, fun }
    }

    fn optional(self) -> impl Rule<Output = Option<Self::Output>> {
        combinators::Optional { rule: self }
    }

    fn repeated(self) -> impl Rule<Output = Vec<Self::Output>> {
        self.map(|first| vec![first])
            .optional()
            .map(|first| first.unwrap_or(vec![]))
            .fold(self, |mut all, next| {
                all.push(next);
                all
            })
    }

    fn separated_with<R: Rule>(self, sep: R) -> impl Rule<Output = Vec<Self::Output>> {
        self.map(|first| vec![first])
            .fold(sep.discard_and(self), |mut all, next| {
                all.push(next);
                all
            })
            .optional()
            .map(|items| items.unwrap_or(vec![]))
    }

    fn switch<L, R, LO, RO>(
        self,
        left: L,
        right: R,
    ) -> impl Rule<Output = Either<(LO, L::Output), (RO, R::Output)>>
    where
        L: Rule,
        R: Rule,
        Self: Rule<Output = Either<LO, RO>>,
    {
        combinators::Switch {
            first: self,
            left,
            right,
        }
    }
}

enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<R, F> Rule for F
where
    F: FnOnce() -> R + Copy,
    R: Rule,
{
    type Output = R::Output;

    fn parse(self, tokens: &mut impl Tokens) -> RuaResult<Option<Self::Output>> {
        self().parse(tokens)
    }
}

impl Rule for Punctuation {
    type Output = Span;

    fn parse(self, tokens: &mut impl Tokens) -> RuaResult<Option<Self::Output>> {
        Ok(match tokens.current() {
            Token::Punctuation(punct, span) if *punct == self => {
                tokens.next();
                Some(*span)
            }
            _ => None,
        })
    }
}

impl Rule for Keyword {
    type Output = Span;

    fn parse(self, tokens: &mut impl Tokens) -> RuaResult<Option<Self::Output>> {
        Ok(match tokens.current() {
            Token::Keyword(kw, span) if *kw == self => {
                tokens.next();
                Some(*span)
            }
            _ => None,
        })
    }
}

#[derive(Clone, Copy)]
struct IdentRule;

impl Rule for IdentRule {
    type Output = Ident;

    fn parse(self, tokens: &mut impl Tokens) -> RuaResult<Option<Self::Output>> {
        Ok(match tokens.current() {
            Token::Ident(_) => {
                let Token::Ident(ident) = tokens.next()? else {
                    unreachable!()
                };
                Some(ident)
            }
            _ => None,
        })
    }
}

impl Rule for () {
    type Output = ();

    fn parse(self, _: &mut impl Tokens) -> RuaResult<Option<Self::Output>> {
        Ok(Some(()))
    }
}

mod rule {
    use crate::*;

    pub fn statements() -> impl Rule<Output = Vec<Statement>> {
        statement
            .map(|first| {
                if let Some(first) = first {
                    vec![first]
                } else {
                    vec![]
                }
            })
            .optional()
            .map(|first| first.unwrap_or(vec![]))
            .fold(statement, |mut all, next| {
                if let Some(next) = next {
                    all.push(next);
                }
                all
            })
    }

    fn statement() -> impl Rule<Output = Option<Statement>> {
        Punctuation::Semicolon.map(|_| None).or(block
            .map(Statement::Block)
            .or(Keyword::While
                .discard_and(expr)
                .and(block)
                .map(|(condition, body)| Statement::While(While { condition, body })))
            .or(Keyword::Repeat
                .discard_and(statements)
                .and_discard(Keyword::Until)
                .and(expr)
                .map(|(body, until)| Statement::Repeat(Repeat { body, until })))
            .or(Keyword::If
                .discard_and(branch)
                .map(|branch| vec![branch])
                .fold(
                    Keyword::Elseif.discard_and(branch),
                    |mut branches, branch| {
                        branches.push(branch);
                        branches
                    },
                )
                .and(Keyword::Else.discard_and(statements).optional())
                .map(|(branches, else_body)| {
                    Statement::If(If {
                        branches,
                        else_body,
                    })
                })
                .and_discard(Keyword::End))
            .or(prefix_expr
                .try_map(|expr| match expr {
                    Expr::Variable(var) => Ok(Either::Left(var)),
                    Expr::Call(call) => Ok(Either::Right(Statement::Call(call))),
                    Expr::MethodCall(call) => Ok(Either::Right(Statement::MethodCall(call))),
                    _ => Err(unimplemented!()),
                })
                .switch(Punctuation::Comma.discard_and(variable).repeated(), ())
                .map(|assign_or_call| match assign_or_call {
                    Either::Left((first, mut vars)) => {
                        vars.insert(0, first);
                        Statement::Assign(Assign {
                            lhs: vars,
                            rhs: vec![],
                        })
                    }
                    Either::Right((statement, ())) => statement,
                }))
            .or(Keyword::Function
                .discard_and(ident)
                .map(Variable::Variable)
                .fold(
                    Punctuation::Period.discard_and(ident),
                    Variable::named_field,
                )
                .and(Punctuation::Colon.discard_and(ident).optional())
                .and(func_body)
                .map(|((mut name, field), mut func)| {
                    if let Some(field) = field {
                        let span = field.span;
                        name = name.named_field(field);
                        func.params.insert(
                            0,
                            Ident {
                                value: "self".to_string(),
                                span,
                            },
                        )
                    }
                    Statement::Assign(Assign {
                        lhs: vec![name],
                        rhs: vec![Expr::Function(func)],
                    })
                }))
            .map(Some))
    }

    fn func_body() -> impl Rule<Output = Function> {
        Punctuation::LParen
            .discard_and(ident.separated_with(Punctuation::Comma))
            .and_discard(Punctuation::RParen)
            .and(statements)
            .and_discard(Keyword::End)
            .map(|(params, body)| Function {
                params,
                vararg: false,
                body,
            })
    }

    fn args() -> impl Rule<Output = Vec<Expr>> {
        Punctuation::LParen
            .discard_and(expr.separated_with(Punctuation::Comma))
            .and_discard(Punctuation::RParen)
    }

    fn method_args() -> impl Rule<Output = (Ident, Vec<Expr>)> {
        Punctuation::Colon.discard_and(ident).and(args)
    }

    fn branch() -> impl Rule<Output = Branch> {
        expr.and_discard(Keyword::Then)
            .and(statements)
            .map(|(condition, body)| Branch { condition, body })
    }

    fn block() -> impl Rule<Output = Vec<Statement>> {
        Keyword::Do
            .discard_and(statements)
            .and_discard(Keyword::End)
    }

    fn expr() -> impl Rule<Output = Expr> {}

    fn concat_expr() -> impl Rule<Output = Expr> {
        additive_expr
            .and(
                Punctuation::DoublePeriod
                    .discard_and(concat_expr)
                    .optional(),
            )
            .map(|(lhs, rhs)| {
                if let Some(rhs) = rhs {
                    Expr::Concat(Concat {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    })
                } else {
                    lhs
                }
            })
    }

    fn additive_expr() -> impl Rule<Output = Expr> {
        multiplicative_expr.fold(
            Punctuation::Plus
                .discard_and(multiplicative_expr)
                .or(Punctuation::Minus.discard_and(multiplicative_expr))
                .either(),
            |lhs, rhs| {
                use Either::*;
                match rhs {
                    Left(rhs) => Expr::Add(Add {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }),
                    Right(rhs) => Expr::Sub(Sub {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }),
                }
            },
        )
    }

    fn multiplicative_expr() -> impl Rule<Output = Expr> {
        unary_expr.fold(
            Punctuation::Asterisk
                .discard_and(unary_expr)
                .or(Punctuation::Slash.discard_and(unary_expr))
                .either()
                .or(Punctuation::DoubleSlash.discard_and(unary_expr))
                .either()
                .or(Punctuation::Percent.discard_and(unary_expr))
                .either(),
            |lhs, rhs| {
                use Either::*;
                match rhs {
                    Left(Left(Left(rhs))) => Expr::Mul(Mul {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }),
                    Left(Left(Right(rhs))) => Expr::Div(Div {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }),
                    Left(Right(rhs)) => Expr::Div(Div {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }),
                    Right(rhs) => Expr::Div(Div {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }),
                }
            },
        )
    }

    fn unary_expr() -> impl Rule<Output = Expr> {
        Keyword::Not
            .discard_and(unary_expr)
            .map(|rhs| Expr::Not(Not { rhs: Box::new(rhs) }))
            .or(Punctuation::Hash
                .discard_and(unary_expr)
                .map(|rhs| Expr::Len(Len { rhs: Box::new(rhs) })))
            .or(Punctuation::Minus
                .discard_and(unary_expr)
                .map(|rhs| Expr::Neg(Neg { rhs: Box::new(rhs) })))
            // .or(Punctuation::Wave
            //     .discard_and(exponential_expr)
            //     .map(|rhs| Expr::named_field(named_field { rhs: Box::new(rhs) })))
            .or(exponential_expr)
    }

    fn exponential_expr() -> impl Rule<Output = Expr> {
        primary_expr
            .and(Punctuation::Caret.discard_and(exponential_expr).optional())
            .map(|(lhs, rhs)| {
                if let Some(rhs) = rhs {
                    Expr::Pow(Pow {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    })
                } else {
                    lhs
                }
            })
    }

    fn primary_expr() -> impl Rule<Output = Expr> {
        prefix_expr
            .or(Keyword::Nil.map(Expr::Nil))
            .or(Keyword::True.map(|span| Expr::Bool(Bool { value: true, span })))
            .or(Keyword::False.map(|span| Expr::Bool(Bool { value: false, span })))
            .or(Punctuation::Ellipsis.map(Expr::Vararg))
            .or(Keyword::Function.discard_and(func_body).map(Expr::Function))
            .or(table.map(Expr::Table))
    }

    fn table() -> impl Rule<Output = Table> {
        Punctuation::LBrace
            .discard_and(field.separated_with(Punctuation::Comma.or(Punctuation::Semicolon)))
            .and_discard(Punctuation::RBrace)
            .map(|fields| Table { fields })
    }

    fn field() -> impl Rule<Output = Field> {
        expr.map(|expr| match expr {
            Expr::Variable(Variable::Variable(name)) => Either::Left(name),
            expr => Either::Right(expr),
        })
        .switch(Punctuation::Equals.discard_and(expr), ())
        .map(|field_or_expr| match field_or_expr {
            Either::Left((name, value)) => Field {
                name: Some(Box::new(Expr::Variable(Variable::Variable(name)))),
                value: Box::new(value),
            },
            Either::Right((value, ())) => Field {
                name: None,
                value: Box::new(value),
            },
        })
        .or(Punctuation::LBracket
            .discard_and(expr)
            .and_discard(Punctuation::RBracket)
            .and_discard(Punctuation::Equals)
            .and(expr)
            .map(|(name, value)| Field {
                name: Some(Box::new(name)),
                value: Box::new(value),
            }))
    }

    fn prefix_expr() -> impl Rule<Output = Expr> {
        ident
            .map(|name| Expr::Variable(Variable::Variable(name)))
            .or(Punctuation::LParen
                .discard_and(expr)
                .and_discard(Punctuation::RParen))
            .fold(
                field_access.or(args).either().or(method_args).either(),
                |expr, rhs| {
                    use Either::*;

                    match rhs {
                        Left(Left(field)) => Expr::Variable(expr.expr_field(field)),
                        Left(Right(args)) => Expr::Call(Call {
                            lhs: Box::new(expr),
                            args,
                        }),
                        Right((name, args)) => Expr::MethodCall(MethodCall {
                            lhs: Box::new(expr),
                            name,
                            args,
                        }),
                    }
                },
            )
    }

    fn field_access() -> impl Rule<Output = Expr> {
        Punctuation::LBracket
            .discard_and(expr)
            .and_discard(Punctuation::RBracket)
            .or(Punctuation::Period.discard_and(ident).map(|name| {
                Expr::StringLit(StringLit {
                    value: name.value,
                    span: name.span,
                })
            }))
    }

    fn variable() -> impl Rule<Output = Variable> {
        prefix_expr
            .map(|expr| match expr {
                Expr::Variable(var) => Either::Left(var),
                expr => Either::Right(expr),
            })
            .switch((), field_access)
            .map(|var| match var {
                Either::Left((var, _)) => var,
                Either::Right((expr, field)) => expr.expr_field(field),
            })
    }

    fn ident() -> impl Rule<Output = Ident> {
        IdentRule
    }
}
