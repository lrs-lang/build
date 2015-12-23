// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

use std::marker::{Leak};

use span::{Span};
use interner::{Interned};
use tree::{SExpr};
use token::{SToken};

/// Trait used to print diagnostic messages.
pub trait Diagnostic: Leak {
    /// Writes a message in a way that indicates an error.
    fn error(&self, span: Span, error: Error);
    /// Writes a message in a way that indicates an notice.
    fn notice(&self, span: Span, notice: Notice);
}

pub enum Notice {
    Via,
}

pub enum Error {
    Unbound(Interned),
    FnPatMismatch,
    ArgMissingField(Interned),
    DivideByZero,
    FoundExpr(&'static str, SExpr),
    FoundToken(&'static str, SToken),
    FoundString(&'static str, &'static str),
    FoundChar(&'static str, char),
    MissingSetField(Interned),
    MissingListField(i64),
    CannotStringify(SExpr),
    InfiniteRecursion,
    OutOfBounds,
    LexerEof,
    OverflowingLiteral,
    InvalidCodepoint,
    UnknownEscapeSequence(u8),
    ExpectedEof,

    SetHasNoField(Interned),
    ListHasNoField(usize),
    FnPattern(Interned),
    FnMissingField(Interned),
    AssertionFailed,
    InteriorNull,

    DupSetField(Span),
    AbsolutePath,
    CannotOpen(Interned),
}
