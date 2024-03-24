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
                }))
            .or(ident
                .map(|name| -> Either<Variable, Either<Call, MethodCall>> {
                    Either::Left(Variable::Variable(name))
                })
                .fold(
                    Punctuation::LBracket
                        .discard_and(expr)
                        .and_discard(Punctuation::RBracket)
                        .or(Punctuation::Period.discard_and(ident))
                        .either()
                        .or(args)
                        .either()
                        .or(method_args)
                        .either(),
                    |expr, rhs| {
                        use Either::*;

                        let expr = match expr {
                            Left(var) => Expr::Variable(var),
                            Right(Left(call)) => Expr::Call(call),
                            Right(Right(call)) => Expr::MethodCall(call),
                        };

                        match rhs {
                            Left(Left(Left(field))) => Either::Left(Variable::Index(Index {
                                lhs: Box::new(expr),
                                idx: Box::new(field),
                            })),
                            Left(Left(Right(field))) => Either::Left(Variable::Index(Index {
                                lhs: Box::new(expr),
                                idx: Box::new(Expr::Variable(Variable::Variable(field))),
                            })),
                            Left(Right(args)) => Right(Left(Call {
                                lhs: Box::new(expr),
                                args,
                            })),
                            Right((name, args)) => Right(Right(MethodCall {
                                lhs: Box::new(expr),
                                name,
                                args,
                            })),
                        }
                    },
                )
                .switch(Punctuation::Comma.discard_and(variable).repeated(), ())
                .map(|assign_or_call| match assign_or_call {
                    Either::Left((first, mut vars)) => {
                        vars.insert(0, first);
                        Statement::Assign(Assign {
                            lhs: vars,
                            rhs: vec![],
                        })
                    }
                    Either::Right((Either::Left(call), _)) => Statement::Call(call),
                    Either::Right((Either::Right(call), _)) => Statement::MethodCall(call),
                }))
            .map(Some))
    }

    fn args() -> impl Rule<Output = Vec<Expr>> {
        Punctuation::LParen
            .discard_and(
                expr.map(|expr| vec![expr])
                    .fold(Punctuation::Comma.discard_and(expr), |mut all, next| {
                        all.push(next);
                        all
                    })
                    .optional()
                    .map(|args| args.unwrap_or(vec![])),
            )
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

    fn variable() -> impl Rule<Output = Variable> {}

    fn ident() -> impl Rule<Output = Ident> {
        IdentRule
    }
}
