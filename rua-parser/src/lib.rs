// #![no_std]

extern crate alloc;
use core::fmt::{self, Display, Formatter};

use alloc::{
    boxed::Box,
    string::{String, ToString},
    vec,
    vec::Vec,
};

use ast::*;
use lexer::{Keyword, LexerError, Punctuation, Source, Token, TokenKind};

use crate::lexer::Lexer;

mod lexer;

#[derive(Debug)]
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

pub fn parse(string: &str) -> Result<Vec<Statement>, UnexpectedToken> {
    let source = StrSource::new(string);
    let mut parser = Parser::new(source).unwrap();
    match parser.parse_chunk() {
        Ok(ast) => Ok(ast),
        Err(ParserError::Lexer(_)) => unreachable!(),
        Err(ParserError::UnexpectedToken(e)) => Err(e),
    }
}

#[derive(Debug)]
pub enum Infallible {}

impl Display for Infallible {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "")
    }
}

impl lexer::Source for StrSource {
    type Error = Infallible;

    fn next(&mut self) -> Result<Option<char>, Infallible> {
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

// pub fn parse_str(chunk: &str) -> Result<Vec<Statement>> {
//     let mut lexer = Lexer::new(StrSource::new(chunk))?;
//     let mut tokens = Parser {
//         current: lexer.next()?,
//         lexer,
//     };

//     let statements = rule::statements.parse(&mut tokens)?.unwrap_or(vec![]);

//     if !matches!(tokens.current(), Token::Eof(_)) {
//         return Err(RuaError {
//             message: "expected end of file".to_string(),
//             span: tokens.current().span(),
//         });
//     }

//     Ok(statements)
// }

struct Parser<S> {
    lexer: Lexer<S>,
    current: Token,
}

#[derive(Debug)]
pub struct UnexpectedToken {
    pub span: Span,
    pub expected: &'static str,
}

#[derive(Debug)]
enum ParserError<S: Source> {
    Lexer(LexerError<S>),
    UnexpectedToken(UnexpectedToken),
}

impl<S: Source> Parser<S> {
    fn new(source: S) -> Result<Self, LexerError<S>> {
        let mut lexer = Lexer::new(source)?;
        Ok(Self {
            current: lexer.next()?,
            lexer,
        })
    }

    fn next(&mut self) -> Result<Token, ParserError<S>> {
        Ok(core::mem::replace(
            &mut self.current,
            self.lexer.next().map_err(ParserError::Lexer)?,
        ))
    }

    fn expect<T>(
        &mut self,
        what: &'static str,
        result: Result<Option<T>, ParserError<S>>,
    ) -> Result<T, ParserError<S>> {
        result?.ok_or_else(|| self.expected(what))
    }

    fn expected(&self, what: &'static str) -> ParserError<S> {
        ParserError::UnexpectedToken(UnexpectedToken {
            span: self.current.span(),
            expected: what,
        })
    }

    fn parse_chunk(&mut self) -> Result<Vec<Statement>, ParserError<S>> {
        let mut statements = vec![];

        while let Some(statement) = self.parse_statement()? {
            statements.push(statement);
            self.punctuation(Punctuation::Semicolon)?;
        }

        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Option<Statement>, ParserError<S>> {
        if let Some(first) = self.parse_prefix_expr()? {
            match first {
                Expr::Variable(first_var) => {
                    let mut vars = vec![];
                    while let Some(comma) = self.punctuation(Punctuation::Comma)? {
                        let var = self.parse_prefix_expr();
                        let var = self.expect("a variable", var)?;
                        let Expr::Variable(var) = var else {
                            return Err(self.expected("field access"));
                        };
                        vars.push(var);
                    }

                    self.expect_punctuation("=", Punctuation::Equals)?;
                    let exprlist = self.parse_exprlist();
                    let exprlist = self.expect("an expression", exprlist)?;
                    vars.insert(0, first_var);
                    Ok(Some(Statement::Assign(Assign {
                        lhs: vars,
                        rhs: exprlist,
                    })))
                }
                Expr::Call(call) => Ok(Some(Statement::Call(call))),
                Expr::MethodCall(call) => Ok(Some(Statement::MethodCall(call))),
                _ => Err(self.expected("call arguments")),
            }
        } else if let Some(_) = self.punctuation(Punctuation::DoubleColon)? {
            let name = self.expect_ident()?;
            let _ = self.expect_punctuation("::", Punctuation::DoubleColon)?;
            Ok(Some(Statement::Label(name)))
        } else if let Some(_) = self.keyword(Keyword::Break)? {
            Ok(Some(Statement::Break))
        } else if let Some(_) = self.keyword(Keyword::Goto)? {
            let name = self.expect_ident()?;
            Ok(Some(Statement::Goto(name)))
        } else if let Some(_) = self.keyword(Keyword::Do)? {
            let block = self.parse_chunk()?;
            Ok(Some(Statement::Block(block)))
        } else if let Some(_) = self.keyword(Keyword::While)? {
            let condition = self.expect_expr()?;
            let body = self.expect_block()?;
            Ok(Some(Statement::While(While { condition, body })))
        } else if let Some(_) = self.keyword(Keyword::Repeat)? {
            let body = self.expect_block()?;
            self.expect_keyword("until", Keyword::Until)?;
            let cond = self.expect_expr()?;
            Ok(Some(Statement::Repeat(Repeat { body, until: cond })))
        } else if let Some(_) = self.keyword(Keyword::If)? {
            let condition = self.expect_expr()?;
            self.expect_keyword("then", Keyword::Then)?;
            let body = self.parse_chunk()?;

            let mut branches = vec![Branch { body, condition }];

            while let Some(_) = self.keyword(Keyword::Elseif)? {
                let condition = self.expect_expr()?;
                self.expect_keyword("then", Keyword::Then)?;
                let body = self.parse_chunk()?;
                branches.push(Branch { condition, body });
            }

            let else_body = if let Some(_) = self.keyword(Keyword::Else)? {
                let body = self.parse_chunk()?;
                Some(body)
            } else {
                None
            };

            self.expect_keyword("end", Keyword::End)?;
            Ok(Some(Statement::If(If {
                branches,
                else_body,
            })))
        } else if let Some(_) = self.keyword(Keyword::For)? {
            let first = self.expect_ident()?;
            if let Some(_) = self.punctuation(Punctuation::Equals)? {
                let initial = self.expect_expr()?;
                self.expect_punctuation(",", Punctuation::Comma)?;
                let limit = self.expect_expr()?;
                let step = if let Some(_) = self.punctuation(Punctuation::Comma)? {
                    Some(self.expect_expr()?)
                } else {
                    None
                };

                let body = self.expect_block()?;

                Ok(Some(Statement::NumericalFor(NumericalFor {
                    iter_var: first,
                    initial: Box::new(initial),
                    limit: Box::new(limit),
                    step: step.map(Box::new),
                    body,
                })))
            } else {
                let mut vars = vec![first];
                while let Some(_) = self.punctuation(Punctuation::Comma)? {
                    let name = self.expect_ident()?;
                    vars.push(name);
                }

                self.expect_keyword("in", Keyword::In)?;
                let exprs = self.parse_exprlist();
                let exprs = self.expect("an expression", exprs)?;
                let body = self.expect_block()?;
                Ok(Some(Statement::For(For { vars, exprs, body })))
            }
        } else if let Some(_) = self.keyword(Keyword::Function)? {
            let fist = self.expect_ident()?;
            let mut var = Variable::Variable(fist);
            while let Some(_) = self.punctuation(Punctuation::Period)? {
                let name = self.expect_ident()?;
                var = Variable::Index(Index {
                    lhs: Box::new(Expr::Variable(var)),
                    idx: Box::new(Expr::Variable(Variable::Variable(name))),
                });
            }

            let colon = if let Some(colon) = self.punctuation(Punctuation::Colon)? {
                let name = self.expect_ident()?;
                var = Variable::Index(Index {
                    lhs: Box::new(Expr::Variable(var)),
                    idx: Box::new(Expr::Variable(Variable::Variable(name))),
                });
                Some(colon)
            } else {
                None
            };

            let body = self.parse_function_body();
            let mut body = self.expect("function body", body)?;

            if let Some(colon) = colon {
                body.params.insert(
                    0,
                    Ident {
                        value: "self".to_string(),
                        span: colon,
                    },
                )
            }

            Ok(Some(Statement::Assign(Assign {
                lhs: vec![var],
                rhs: vec![Expr::Function(body)],
            })))
        } else if let Some(_) = self.keyword(Keyword::Local)? {
            if let Some(_) = self.keyword(Keyword::Function)? {
                let name = self.expect_ident()?;
                let body = self.parse_function_body();
                let body = self.expect("function body", body)?;
                Ok(Some(Statement::LocalAssign(LocalAssign {
                    vars: vec![LocalName { name, attr: None }],
                    exprs: vec![Expr::Function(body)],
                })))
            } else if let Some(first) = self.parse_name_with_attr()? {
                let mut names = vec![first];
                while let Some(next) = self.parse_name_with_attr()? {
                    names.push(next);
                }

                let exprs = if let Some(_) = self.punctuation(Punctuation::Equals)? {
                    let exprlist = self.parse_exprlist();
                    let exprlist = self.expect("an expression", exprlist)?;
                    exprlist
                } else {
                    vec![]
                };

                Ok(Some(Statement::LocalAssign(LocalAssign {
                    vars: names,
                    exprs,
                })))
            } else {
                Err(self.expected("a local declaration"))
            }
        } else {
            Ok(None)
        }
    }

    fn punctuation(&mut self, punctuation: Punctuation) -> Result<Option<Span>, ParserError<S>> {
        if self.current.kind() == TokenKind::Punctuation(punctuation) {
            let span = self.current.span();
            self.next()?;
            Ok(Some(span))
        } else {
            Ok(None)
        }
    }

    fn keyword(&mut self, keyword: Keyword) -> Result<Option<Span>, ParserError<S>> {
        if self.current.kind() == TokenKind::Keyword(keyword) {
            let span = self.current.span();
            self.next()?;
            Ok(Some(span))
        } else {
            Ok(None)
        }
    }

    fn string_lit(&mut self) -> Result<Option<StringLit>, ParserError<S>> {
        if self.current.kind() == TokenKind::String {
            Ok(Some(match self.next()? {
                Token::String(string) => string,
                _ => unreachable!(),
            }))
        } else {
            Ok(None)
        }
    }

    fn ident(&mut self) -> Result<Option<Ident>, ParserError<S>> {
        if self.current.kind() == TokenKind::Ident {
            Ok(Some(match self.next()? {
                Token::Ident(ident) => ident,
                _ => unreachable!(),
            }))
        } else {
            Ok(None)
        }
    }

    fn number(&mut self) -> Result<Option<Number>, ParserError<S>> {
        if self.current.kind() == TokenKind::Number {
            Ok(Some(match self.next()? {
                Token::Number(num) => num,
                _ => unreachable!(),
            }))
        } else {
            Ok(None)
        }
    }

    fn expect_ident(&mut self) -> Result<Ident, ParserError<S>> {
        let ident = self.ident();
        self.expect("an identifier", ident)
    }

    fn expect_punctuation(
        &mut self,
        name: &'static str,
        punctuation: Punctuation,
    ) -> Result<Span, ParserError<S>> {
        let punct = self.punctuation(punctuation);
        self.expect(name, punct)
    }

    fn expect_keyword(
        &mut self,
        name: &'static str,
        keyword: Keyword,
    ) -> Result<Span, ParserError<S>> {
        let kw = self.keyword(keyword);
        self.expect(name, kw)
    }

    fn expect_block(&mut self) -> Result<Vec<Statement>, ParserError<S>> {
        let do_ = self.expect_keyword("do", Keyword::Do)?;
        let statements = self.parse_chunk()?;
        let end = self.expect_keyword("end", Keyword::End)?;
        Ok(statements)
    }

    fn parse_name_with_attr(&mut self) -> Result<Option<LocalName>, ParserError<S>> {
        let Some(name) = self.ident()? else {
            return Ok(None);
        };

        let attr = if let Some(_) = self.punctuation(Punctuation::Less)? {
            let name = self.expect_ident()?;
            self.expect_punctuation(">", Punctuation::Greater)?;
            Some(name)
        } else {
            None
        };

        Ok(Some(LocalName { name, attr }))
    }

    fn parse_expr(&mut self) -> Result<Option<Expr>, ParserError<S>> {
        let Some(expr) = self.parse_prefix_op_expr()? else {
            return Ok(None);
        };

        Ok(Some(self.parse_or_rhs(expr)?))
    }

    fn expect_expr(&mut self) -> Result<Expr, ParserError<S>> {
        let expr = self.parse_expr();
        self.expect("an expression", expr)
    }

    fn parse_primary_expr(&mut self) -> Result<Option<Expr>, ParserError<S>> {
        if let Some(prefix) = self.parse_prefix_expr()? {
            Ok(Some(prefix))
        } else if let Some(keyword) = self.keyword(Keyword::True)? {
            Ok(Some(Expr::Bool(Bool {
                value: true,
                span: keyword,
            })))
        } else if let Some(keyword) = self.keyword(Keyword::False)? {
            Ok(Some(Expr::Bool(Bool {
                value: false,
                span: keyword,
            })))
        } else if let Some(number) = self.number()? {
            Ok(Some(Expr::Number(number)))
        } else if let Some(string) = self.string_lit()? {
            Ok(Some(Expr::StringLit(string)))
        } else if let Some(nil) = self.keyword(Keyword::Nil)? {
            Ok(Some(Expr::Nil(nil)))
        } else if let Some(vararg) = self.punctuation(Punctuation::Ellipsis)? {
            Ok(Some(Expr::Vararg(vararg)))
        } else if let Some(table) = self.parse_table()? {
            Ok(Some(Expr::Table(table)))
        } else if let Some(fun) = self.parse_function_def()? {
            Ok(Some(Expr::Function(fun)))
        } else {
            Ok(None)
        }
    }

    fn parse_or_rhs(&mut self, mut expr: Expr) -> Result<Expr, ParserError<S>> {
        let mut expr = self.parse_and_rhs(expr)?;

        while let Some(or) = self.keyword(Keyword::Or)? {
            let rhs = self.expect_prefix_op_expr()?;
            let rhs = self.parse_and_rhs(rhs)?;
            expr = Expr::Or(Or {
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            });
        }

        Ok(expr)
    }

    fn parse_and_rhs(&mut self, mut expr: Expr) -> Result<Expr, ParserError<S>> {
        let mut expr = self.parse_bit_or_rhs(expr)?;

        while let Some(or) = self.keyword(Keyword::And)? {
            let rhs = self.expect_prefix_op_expr()?;
            let rhs = self.parse_bit_or_rhs(rhs)?;
            expr = Expr::And(And {
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            });
        }
        Ok(expr)
    }

    fn parse_bit_or_rhs(&mut self, mut expr: Expr) -> Result<Expr, ParserError<S>> {
        let mut expr = self.parse_bit_xor_rhs(expr)?;

        while let Some(or) = self.punctuation(Punctuation::Pipe)? {
            let rhs = self.expect_prefix_op_expr()?;
            let rhs = self.parse_bit_xor_rhs(rhs)?;
            expr = Expr::BitOr(BitOr {
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            });
        }

        Ok(expr)
    }

    fn parse_bit_xor_rhs(&mut self, mut expr: Expr) -> Result<Expr, ParserError<S>> {
        let mut expr = self.parse_bit_and_rhs(expr)?;

        while let Some(wave) = self.punctuation(Punctuation::Wave)? {
            let rhs = self.expect_prefix_op_expr()?;
            let rhs = self.parse_bit_and_rhs(rhs)?;
            expr = Expr::BitXor(BitXor {
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            });
        }

        Ok(expr)
    }

    fn parse_bit_and_rhs(&mut self, mut expr: Expr) -> Result<Expr, ParserError<S>> {
        let mut expr = self.parse_shift_rhs(expr)?;

        while let Some(amp) = self.punctuation(Punctuation::Ampersand)? {
            let rhs = self.expect_prefix_op_expr()?;
            let rhs = self.parse_shift_rhs(rhs)?;
            expr = Expr::BitAnd(BitAnd {
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            });
        }

        Ok(expr)
    }

    fn parse_shift_rhs(&mut self, mut expr: Expr) -> Result<Expr, ParserError<S>> {
        let mut expr = self.parse_concat_rhs(expr)?;

        loop {
            if let Some(dbl_less) = self.punctuation(Punctuation::DoubleLess)? {
                let rhs = self.expect_prefix_op_expr()?;
                let rhs = self.parse_concat_rhs(rhs)?;
                expr = Expr::ShiftLeft(ShiftLeft {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                });
                continue;
            }

            if let Some(dbl_greater) = self.punctuation(Punctuation::DoubleGreater)? {
                let rhs = self.parse_primary_expr();
                let rhs = self.expect("an expression", rhs)?;
                let rhs = self.parse_concat_rhs(rhs)?;
                expr = Expr::ShiftRight(ShiftRight {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                });
                continue;
            }

            break;
        }
        Ok(expr)
    }

    fn parse_concat_rhs(&mut self, mut expr: Expr) -> Result<Expr, ParserError<S>> {
        let mut expr = self.parse_add_rhs(expr)?;

        if let Some(dbl_period) = self.punctuation(Punctuation::DoublePeriod)? {
            let rhs = self.expect_prefix_op_expr()?;
            let rhs = self.parse_concat_rhs(rhs)?;
            expr = Expr::Concat(Concat {
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            });
        }

        Ok(expr)
    }

    fn parse_add_rhs(&mut self, mut expr: Expr) -> Result<Expr, ParserError<S>> {
        let mut expr = self.parse_mul_rhs(expr)?;

        loop {
            if let Some(plus) = self.punctuation(Punctuation::Plus)? {
                let rhs = self.expect_prefix_op_expr()?;
                let rhs = self.parse_mul_rhs(rhs)?;
                expr = Expr::Add(Add {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                });
                continue;
            }

            if let Some(minus) = self.punctuation(Punctuation::Minus)? {
                let rhs = self.expect_prefix_op_expr()?;
                let rhs = self.parse_mul_rhs(rhs)?;
                expr = Expr::Sub(Sub {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                });
                continue;
            }

            break;
        }
        Ok(expr)
    }

    fn parse_mul_rhs(&mut self, mut expr: Expr) -> Result<Expr, ParserError<S>> {
        loop {
            if let Some(or) = self.punctuation(Punctuation::Asterisk)? {
                let rhs = self.expect_prefix_op_expr()?;
                let rhs = self.parse_mul_rhs(rhs)?;
                expr = Expr::Mul(Mul {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                });
                continue;
            }

            if let Some(slash) = self.punctuation(Punctuation::Slash)? {
                let rhs = self.expect_prefix_op_expr()?;
                let rhs = self.parse_mul_rhs(rhs)?;
                expr = Expr::Div(Div {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                });
                continue;
            }

            if let Some(or) = self.punctuation(Punctuation::DoubleSlash)? {
                let rhs = self.expect_prefix_op_expr()?;
                let rhs = self.parse_mul_rhs(rhs)?;
                expr = Expr::IDiv(IDiv {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                });
                continue;
            }

            if let Some(or) = self.punctuation(Punctuation::Percent)? {
                let rhs = self.expect_prefix_op_expr()?;
                let rhs = self.parse_mul_rhs(rhs)?;
                expr = Expr::Mod(Mod {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                });
                continue;
            }

            break;
        }
        Ok(expr)
    }

    fn parse_prefix_op_expr(&mut self) -> Result<Option<Expr>, ParserError<S>> {
        if let Some(minus) = self.punctuation(Punctuation::Minus)? {
            return Ok(Some(Expr::Neg(Neg {
                rhs: Box::new(self.expect_prefix_op_expr()?),
            })));
        }

        if let Some(hash) = self.punctuation(Punctuation::Hash)? {
            return Ok(Some(Expr::Len(Len {
                rhs: Box::new(self.expect_prefix_op_expr()?),
            })));
        }

        if let Some(not) = self.keyword(Keyword::Not)? {
            return Ok(Some(Expr::Not(Not {
                rhs: Box::new(self.expect_prefix_op_expr()?),
            })));
        }

        if let Some(bit_not) = self.punctuation(Punctuation::Wave)? {
            return Ok(Some(Expr::BitNot(BitNot {
                rhs: Box::new(self.expect_prefix_op_expr()?),
            })));
        }

        if let Some(expr) = self.parse_primary_expr()? {
            let expr = self.parse_pow_rhs(expr)?;
            return Ok(Some(expr));
        }

        Ok(None)
    }

    fn expect_prefix_op_expr(&mut self) -> Result<Expr, ParserError<S>> {
        let expr = self.parse_prefix_op_expr();
        self.expect("an expression", expr)
    }

    fn parse_pow_rhs(&mut self, mut expr: Expr) -> Result<Expr, ParserError<S>> {
        while let Some(caret) = self.punctuation(Punctuation::Caret)? {
            let rhs = self.parse_primary_expr();
            let rhs = self.expect("an expression", rhs)?;
            expr = Expr::Pow(Pow {
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            });
        }
        Ok(expr)
    }

    fn parse_prefix_expr(&mut self) -> Result<Option<Expr>, ParserError<S>> {
        let first = if let Some(ident) = self.ident()? {
            Expr::Variable(Variable::Variable(ident))
        } else if let Some(l_paren) = self.punctuation(Punctuation::LParen)? {
            let inner = self.expect_expr()?;
            self.expect_punctuation(")", Punctuation::RParen)?;
            inner
        } else {
            return Ok(None);
        };

        let expr = self.parse_prefix_rhs(first)?;
        Ok(Some(expr))
    }

    fn parse_prefix_rhs(&mut self, mut expr: Expr) -> Result<Expr, ParserError<S>> {
        loop {
            if let Some(l_bracket) = self.punctuation(Punctuation::RBracket)? {
                let index = self.expect_expr()?;
                let r_bracket = self.expect_punctuation("]", Punctuation::RBracket)?;
                expr = Expr::Variable(Variable::Index(Index {
                    lhs: Box::new(expr),
                    idx: Box::new(index),
                }));
            } else if let Some(dot) = self.punctuation(Punctuation::Period)? {
                let name = self.expect_ident()?;
                expr = Expr::Variable(Variable::Index(Index {
                    lhs: Box::new(expr),
                    idx: Box::new(Expr::StringLit(StringLit {
                        value: name.value,
                        span: name.span,
                    })),
                }));
            } else if let Some(args) = self.parse_args()? {
                expr = Expr::Call(Call {
                    lhs: Box::new(expr),
                    args,
                });
            } else if let Some(colon) = self.punctuation(Punctuation::Colon)? {
                let name = self.expect_ident()?;
                let args = self.parse_args();
                let args = self.expect("an argument list", args)?;
                expr = Expr::MethodCall(MethodCall {
                    lhs: Box::new(expr),
                    name,
                    args,
                });
            } else {
                return Ok(expr);
            }
        }
    }

    fn parse_args(&mut self) -> Result<Option<Vec<Expr>>, ParserError<S>> {
        Ok(if let Some(string_lit) = self.string_lit()? {
            Some(vec![Expr::StringLit(string_lit)])
        } else if let Some(l_paren) = self.punctuation(Punctuation::LParen)? {
            let args = self.parse_optional_exprlist()?;
            self.expect_punctuation(")", Punctuation::RParen)?;
            Some(args)
        } else if let Some(table) = self.parse_table()? {
            Some(vec![Expr::Table(table)])
        } else {
            None
        })
    }

    fn parse_exprlist(&mut self) -> Result<Option<Vec<Expr>>, ParserError<S>> {
        let Some(first) = self.parse_expr()? else {
            return Ok(None);
        };

        let mut exprs = vec![first];

        while let Some(comma) = self.punctuation(Punctuation::Comma)? {
            exprs.push(self.expect_expr()?);
        }

        Ok(Some(exprs))
    }

    fn parse_optional_exprlist(&mut self) -> Result<Vec<Expr>, ParserError<S>> {
        let exprlist = self.parse_exprlist()?;
        Ok(exprlist.unwrap_or_default())
    }

    fn parse_table(&mut self) -> Result<Option<Table>, ParserError<S>> {
        let Some(l_brace) = self.punctuation(Punctuation::LBrace)? else {
            return Ok(None);
        };

        let mut fields = vec![];

        loop {
            let Some(field) = self.parse_field()? else {
                break;
            };

            fields.push(field);

            if self.punctuation(Punctuation::Comma)?.is_some() {
                continue;
            }

            if self.punctuation(Punctuation::Semicolon)?.is_some() {
                continue;
            }

            break;
        }

        Ok(Some(Table { fields }))
    }

    fn parse_field(&mut self) -> Result<Option<Field>, ParserError<S>> {
        Ok(
            if let Some(l_bracket) = self.punctuation(Punctuation::LBracket)? {
                let name = self.expect_expr()?;
                let r_bracket = self.expect_punctuation("]", Punctuation::RBracket)?;
                let value = self.expect_expr()?;
                Some(Field {
                    name: Some(name),
                    value,
                })
            } else if let Some(name) = self.ident()? {
                Some(if let Some(eq) = self.punctuation(Punctuation::Equals)? {
                    let expr = self.expect_expr()?;
                    Field {
                        name: Some(Expr::StringLit(StringLit {
                            value: name.value,
                            span: name.span,
                        })),
                        value: expr,
                    }
                } else {
                    Field {
                        name: None,
                        value: self.parse_or_rhs(Expr::Variable(Variable::Variable(name)))?,
                    }
                })
            } else if let Some(expr) = self.parse_expr()? {
                Some(Field {
                    name: None,
                    value: expr,
                })
            } else {
                None
            },
        )
    }

    fn parse_function_def(&mut self) -> Result<Option<Function>, ParserError<S>> {
        let Some(function) = self.keyword(Keyword::Function)? else {
            return Ok(None);
        };

        let fun = self.parse_function_body();
        let fun = self.expect("a function body", fun)?;
        Ok(Some(fun))
    }

    fn parse_function_body(&mut self) -> Result<Option<Function>, ParserError<S>> {
        let Some(l_paren) = self.punctuation(Punctuation::LParen)? else {
            return Ok(None);
        };

        let (params, vararg) = self.parse_function_parlist()?;
        self.expect_punctuation(")", Punctuation::RParen)?;
        let body = self.parse_chunk()?;
        self.expect_keyword("end", Keyword::End)?;

        Ok(Some(Function {
            params,
            vararg,
            body,
        }))
    }

    fn parse_function_parlist(&mut self) -> Result<(Vec<Ident>, bool), ParserError<S>> {
        let mut names = vec![];

        loop {
            let Some(name) = self.ident()? else {
                let vararg = self.punctuation(Punctuation::Ellipsis)?.is_some();
                return Ok((names, vararg));
            };

            names.push(name);

            let Some(comma) = self.punctuation(Punctuation::Comma)? else {
                return Ok((names, false));
            };
        }
    }
}
