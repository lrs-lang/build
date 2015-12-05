// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

use std::fmt::{Debug, Write};

use interner::{Interned};
use span::{Spanned};

pub type SToken = Spanned<Token>;

#[derive(Copy, Eq)]
pub enum Token {
    StringPart(Interned),
    String(Interned),

    Integer(i64),
    Ident(Interned),
    True,
    False,
    Null,
    Rec,
    Let,
    In,
    Inherit,
    If,
    Then,
    Else,
    Or,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    Semicolon,
    Dot,
    DotDot,
    Assign,
    Colon,
    Comma,
    Questionmark,
    At,
    Equals,
    Unequal,
    Lt,
    Le,
    Gt,
    Ge,
    Concat,
    Plus,
    Minus,
    Times,
    Div,
    Not,
    Mod,
    Overlay,
    AndAnd,
    OrOr,
    Implies,
}

impl Token {
    pub fn starts_expr(self) -> bool {
        match self {
            Token::StringPart(..) => true,
            Token::String(..)     => true,
            Token::Integer(..)    => true,
            Token::Ident(..)      => true,
            Token::True           => true,
            Token::False          => true,
            Token::Null           => true,
            Token::Rec            => true,
            Token::Let            => true,
            Token::If             => true,
            Token::LeftBracket    => true,
            Token::LeftBrace      => true,
            Token::LeftParen      => true,

            // Unary operators
            Token::Minus => true,
            Token::Not   => true,

            Token::In           => false,
            Token::Inherit      => false,
            Token::Then         => false,
            Token::Else         => false,
            Token::Or           => false,
            Token::RightBracket => false,
            Token::RightBrace   => false,
            Token::RightParen   => false,
            Token::Semicolon    => false,
            Token::Dot          => false,
            Token::DotDot       => false,
            Token::Assign       => false,
            Token::Colon        => false,
            Token::Comma        => false,
            Token::Questionmark => false,
            Token::At           => false,
            Token::Equals       => false,
            Token::Unequal      => false,
            Token::Lt           => false,
            Token::Le           => false,
            Token::Gt           => false,
            Token::Ge           => false,
            Token::Concat       => false,
            Token::Plus         => false,
            Token::Times        => false,
            Token::Div          => false,
            Token::Mod          => false,
            Token::Overlay      => false,
            Token::AndAnd       => false,
            Token::OrOr         => false,
            Token::Implies      => false,
        }
    }
}

impl Debug for Token {
    fn fmt<W: Write>(&self, mut w: &mut W) -> Result {
        let s = match *self {
            Token::StringPart(d)  => return write!(w, "StringPart({:?})", d),
            Token::String(d)      => return write!(w, "String({:?})", d),
            Token::Integer(d)     => return write!(w, "Integer({})", d),
            Token::Ident(d)       => return write!(w, "Ident({:?})", d),

            Token::True    => "true",
            Token::False   => "false",
            Token::Null    => "null",
            Token::Rec     => "rec",
            Token::Let     => "let",
            Token::In      => "in",
            Token::Inherit => "inherit",
            Token::If      => "if",
            Token::Then    => "then",
            Token::Else    => "else",
            Token::Or      => "or",

            Token::LeftBracket  => "[",
            Token::RightBracket => "]",
            Token::LeftBrace    => "{",
            Token::RightBrace   => "}",
            Token::LeftParen    => "(",
            Token::RightParen   => ")",
            Token::Semicolon    => ";",
            Token::Dot          => ".",
            Token::DotDot       => "..",
            Token::Assign       => "=",
            Token::Colon        => ":",
            Token::Comma        => ",",
            Token::Questionmark => "?",
            Token::At           => "@",
            Token::Equals       => "==",
            Token::Unequal      => "!=",
            Token::Lt           => "<",
            Token::Le           => "<=",
            Token::Gt           => ">",
            Token::Ge           => ">=",
            Token::Concat       => "++",
            Token::Plus         => "+",
            Token::Minus        => "-",
            Token::Times        => "*",
            Token::Div          => "/",
            Token::Mod          => "%",
            Token::Not          => "!",
            Token::Overlay      => "\\\\",
            Token::AndAnd       => "&&",
            Token::OrOr         => "||",
            Token::Implies      => "->",
        };
        
        w.write_all(s.as_bytes()).ignore_ok()
    }
}
