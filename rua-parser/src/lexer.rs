use std::char;

use ast::{Ident, Number, Position, RuaError, RuaResult, Span, StringLit};

pub struct Lexer<S> {
    source: S,
    current: Option<char>,
    column: u32,
    line: u32,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Keyword {
    And,
    Break,
    Do,
    Else,
    Elseif,
    End,
    False,
    For,
    Function,
    Goto,
    If,
    In,
    Local,
    Nil,
    Not,
    Or,
    Repeat,
    Return,
    Then,
    True,
    Until,
    While,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Punctuation {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    Caret,
    Hash,
    Ampersand,
    Wave,
    Pipe,
    DoubleLess,
    DoubleGreater,
    DoubleSlash,
    DoubleEq,
    WaveEq,
    LessEq,
    GreaterEq,
    Less,
    Greater,
    Equals,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    DoubleColon,
    Semicolon,
    Colon,
    Comma,
    Period,
    DoublePeriod,
    Ellipsis,
}

#[derive(Debug, Clone)]
pub enum Token {
    Ident(Ident),
    Number(Number),
    String(StringLit),
    Keyword(Keyword, Span),
    Punctuation(Punctuation, Span),
    Eof(Span),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Ident,
    Number,
    String,
    Keyword(Keyword),
    Punctuation(Punctuation),
    Eof,
}

impl Token {
    pub fn span(&self) -> Span {
        match self {
            Self::Ident(v) => v.span,
            Self::Number(v) => v.span,
            Self::String(v) => v.span,
            Self::Keyword(_, s) => *s,
            Self::Punctuation(_, s) => *s,
            Self::Eof(s) => *s,
        }
    }
}

pub trait Source {
    fn next(&mut self) -> RuaResult<Option<char>>;
}

impl<S: Source> Lexer<S> {
    pub fn new(mut source: S) -> RuaResult<Self> {
        let first = source.next()?;

        Ok(Lexer {
            source,
            current: first,
            column: 0,
            line: 0,
        })
    }

    pub fn next(&mut self) -> RuaResult<Token> {
        dbg!(self._next())
    }

    fn _next(&mut self) -> RuaResult<Token> {
        loop {
            match self.current {
                Some(c) if c.is_whitespace() => {
                    self.read_char()?;
                }
                _ => break,
            }
        }

        if self.current.is_none() {
            return Ok(Token::Eof(self.current_position().extend_back(1)));
        }

        self.read_ident_or_keyword()
            .transpose()
            .or_else(|| self.read_string_literal().transpose())
            .or_else(|| self.read_punctuation().transpose())
            .or_else(|| self.read_numeric_literal().transpose())
            .ok_or_else(|| RuaError {
                message: "unexpected character".to_string(),
                span: self.current_position().extend_back(1),
            })?
    }

    fn current_position(&self) -> Position {
        Position {
            line: self.line,
            column: self.column,
        }
    }

    fn read_ident_or_keyword(&mut self) -> RuaResult<Option<Token>> {
        match self.current {
            Some(current) if current.is_alphabetic() || current == '_' => {}
            _ => return Ok(None),
        }

        let mut ident = String::from(self.read_char()?.unwrap());

        while let Some(char) = self.current {
            if !char.is_alphanumeric() {
                break;
            }
            ident.push(char);
            self.read_char()?;
        }

        let span = self.current_position().extend_back(ident.len() as u32);

        Ok(Some(match &ident[..] {
            "and" => Token::Keyword(Keyword::And, span),
            "break" => Token::Keyword(Keyword::Break, span),
            "do" => Token::Keyword(Keyword::Do, span),
            "else" => Token::Keyword(Keyword::Else, span),
            "elseif" => Token::Keyword(Keyword::Elseif, span),
            "end" => Token::Keyword(Keyword::End, span),
            "false" => Token::Keyword(Keyword::False, span),
            "for" => Token::Keyword(Keyword::For, span),
            "function" => Token::Keyword(Keyword::Function, span),
            "goto" => Token::Keyword(Keyword::Goto, span),
            "if" => Token::Keyword(Keyword::If, span),
            "in" => Token::Keyword(Keyword::In, span),
            "local" => Token::Keyword(Keyword::Local, span),
            "nil" => Token::Keyword(Keyword::Nil, span),
            "not" => Token::Keyword(Keyword::Not, span),
            "or" => Token::Keyword(Keyword::Or, span),
            "repeat" => Token::Keyword(Keyword::Repeat, span),
            "return" => Token::Keyword(Keyword::Return, span),
            "then" => Token::Keyword(Keyword::Then, span),
            "true" => Token::Keyword(Keyword::True, span),
            "until" => Token::Keyword(Keyword::Until, span),
            "while" => Token::Keyword(Keyword::While, span),
            _ => Token::Ident(Ident { span, value: ident }),
        }))
    }

    fn read_punctuation(&mut self) -> RuaResult<Option<Token>> {
        match self.current {
            Some(current) if current.is_ascii_punctuation() => {}
            _ => return Ok(None),
        }

        macro_rules! punct {
            ($name:ident, $len:expr) => {
                Some(Token::Punctuation(
                    Punctuation::$name,
                    self.current_position().extend_back($len),
                ))
            };
        }

        macro_rules! punct_impl {
            ([$char:literal => $name:ident , $($rest:tt)*] [$($out:tt)*] $none:expr, $depth:expr) => {
                punct_impl!([$($rest)*] [$($out)*
                    Some($char) => {
                        self.read_char()?;
                        punct!($name, $depth)
                    },
                ] $none, $depth)
            };

            ([$char:literal => $name:ident { $($nested:tt)* } , $($rest:tt)*] [$($out:tt)*] $none:expr, $depth:expr) => {
                punct_impl!([$($rest)*] [$($out)*
                    Some($char) => {
                        self.read_char()?;
                        punct_impl!([$($nested)*] [] Some(Token::Punctuation(
                            Punctuation::$name,
                            self.current_position().extend_back($depth),
                        )), $depth + 1)
                    },
                ] $none, $depth)
            };

            ([] [$($out:tt)*] $none:expr, $depth:expr) => {
                match self.current {
                    $($out)*
                    _ => $none,
                }
            };
        }

        macro_rules! puncts {
            ($($rest:tt)*) => {
                punct_impl!([$($rest)*] [] None, 1)
            };
        }

        let punct = puncts! {
            '+' => Plus,
            '*' => Asterisk,
            '(' => LParen,
            ')' => RParen,
            '{' => LBrace,
            '}' => RBrace,
            '[' => LBracket,
            ']' => RBracket,
            ':' => Colon {
                ':' => DoubleColon,
            },
            ';' => Semicolon,
            ',' => Comma,
            '.' => Period {
                '.' => DoublePeriod {
                    '.' => Ellipsis,
                },
            },
            '|' => Pipe,
            '&' => Ampersand,
            '-' => Minus,
            '%' => Percent,
            '^' => Caret,
            '#' => Hash,
            '/' => Slash {
                '/' => DoubleSlash,
            },
            '=' => Equals {
                '=' => DoubleEq,
            },
            '~' => Wave {
                '=' => WaveEq,
            },
            '<' => Less {
                '=' => LessEq,
                '<' => DoubleLess,
            },
            '>' => Greater {
                '=' => GreaterEq,
                '>' => DoubleGreater,
            },
        };

        punct.map(Some).ok_or_else(|| RuaError {
            message: "unknown punctuation".to_string(),
            span: self.current_position().extend_back(1),
        })
    }

    fn read_numeric_literal(&mut self) -> RuaResult<Option<Token>> {
        match self.current {
            Some(current) if current.is_digit(10) => {}
            _ => return Ok(None),
        }

        let mut value = 0;
        let mut len = 0;
        while let Some(Some(digit)) = self.current.map(|c| c.to_digit(10)) {
            self.read_char()?;
            len += 1;
            value *= 10;
            value += digit;
        }

        Ok(Some(Token::Number(Number {
            span: self.current_position().extend_back(len),
            value: value as f64,
        })))
    }

    fn read_string_literal(&mut self) -> RuaResult<Option<Token>> {
        if self.current != Some('"') {
            return Ok(None);
        }

        self.read_char()?;
        let mut value = String::new();

        loop {
            match self.read_char()? {
                Some('"') | None => {
                    return Ok(Some(Token::String(StringLit {
                        span: self.current_position().extend_back(value.len() as u32),
                        value,
                    })));
                }
                Some(character) => value.push(character),
            }
        }
    }

    fn read_char(&mut self) -> RuaResult<Option<char>> {
        let old = self.current;

        if let Some(char) = old {
            if char == '\n' {
                self.column = 0;
                self.line += 1;
            } else {
                self.column += 1;
            }
        }

        self.current = self.source.next()?;

        Ok(old)
    }
}
