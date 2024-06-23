#![no_std]

extern crate alloc;
use core::fmt::{self, Display, Formatter};

use alloc::{
    string::{String, ToString},
    vec,
    vec::Vec,
};

use ast::*;
use lexer::{Keyword, LexerError, Punctuation, Source, Token, TokenKind};

use crate::lexer::Lexer;

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

enum ParserError<S: Source> {
    Lexer(LexerError<S>),
    UnexpectedToken { span: Span, expected: TokenKind },
}

impl<S: Source> Parser<S> {
    fn next(&mut self) -> Result<Token, ParserError<S>> {
        Ok(core::mem::replace(
            &mut self.current,
            self.lexer.next().map_err(ParserError::Lexer)?,
        ))
    }

    fn expect<T>(
        &mut self,
        token_kind: TokenKind,
        result: Result<Option<T>, ParserError<S>>,
    ) -> Result<T, ParserError<S>> {
        result?.ok_or_else(|| ParserError::UnexpectedToken {
            span: self.current.span(),
            expected: token_kind,
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

    fn ident(&mut self) -> Result<Option<Ident>, ParserError<S>> {
        if self.current.kind() == TokenKind::Ident {
            let Token::Ident(ident) = self.next()? else {
                unreachable!()
            };
            Ok(Some(ident))
        } else {
            Ok(None)
        }
    }

    fn parse_block(&mut self) -> Result<Option<Statement>, ParserError<S>> {
        let Some(do_) = self.keyword(Keyword::Do)? else {
            return Ok(None);
        };

        let statements = self.parse_chunk()?;

        let end = self.keyword(Keyword::End);
        let end = self.expect(TokenKind::Keyword(Keyword::End), end)?;

        Ok(Some(Statement::Block(statements)))
    }

    fn parse_primary_expr(&mut self) -> Result<Option<Expr>, ParserError<S>> {
        if let Some(ident) = self.ident()? {
            Ok(Some(Expr::Variable(Variable::Variable(ident))))
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
            Ok(Some(Expr::Number(Number::Number(number))))
        } else if let Some(string) = self.string()? {
            Ok(Some(Expr::StringLit(StringLit::StringLit(string))))
        } else if let Some(keyword) = self.keyword(Keyword::Nil)? {
            Ok(Some(Expr::Nil(Span::default())))
        } else {
            Ok(None)
        }
    }

    fn parse_prefix_expr(&mut self) -> Result<Option<Expr>, ParserError<S>> {
        let Some(primary) = self.parse_primary_expr()? else {
            return Ok(None);
        };
    }
}
