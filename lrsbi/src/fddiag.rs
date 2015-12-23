// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

use std::rc::{Rc};
use std::share::{RefCell};
use std::fd::{FdIo};
use std::{tty};

use lrsb_types::token::{Token};
use lrsb_types::codemap::{Codemap};
use lrsb_types::span::{Span};
use lrsb_types::interner::{Interner};
use lrsb_types::diagnostic::{Notice, Error, Diagnostic};
use lrsb_parser::{Parser};
use lrsb_lexer::{Lexer};
use lrsb_eval::{Eval};

use term::{self, Color, CharAttr};

pub struct FdDiag {
    codemap: Rc<RefCell<Codemap>>,
    interner: Rc<Interner>,
    out: FdIo,
    is_term: bool,
}

impl FdDiag {
    pub fn new(codemap: Rc<RefCell<Codemap>>, interner: Rc<Interner>,
               fd: FdIo) -> FdDiag {
        FdDiag {
            codemap: codemap,
            interner: interner,
            out: fd,
            is_term: tty::is_a_tty(&fd),
        }
    }

    fn set_fg_color(&self, c: Color) {
        if self.is_term { term::set_fg_color(c); }
    }

    fn set_char_attr(&self, c: CharAttr, b: bool) {
        if self.is_term { term::set_char_attr(c, b); }
    }

    fn common<F>(&self, span: Span, color: Color, prefix: &str, f: F)
        where F: FnOnce(&mut Write) -> Result,
    {
        if span == Span::built_in() {
            write!(&self.out, "<built in> ");
            self.set_fg_color(color);
            self.set_char_attr(CharAttr::Bold, true);
            write!(&self.out, "{}", prefix);
            self.set_fg_color(Color::White);
            f(&mut &self.out);
            self.set_fg_color(Color::Default);
            self.set_char_attr(CharAttr::Bold, false);
            writeln!(&self.out, "");
            return;
        }

        let codemap = self.codemap.borrow();
        let file = codemap.file(span);
        let mut lines = file.lines(span);
        write!(&self.out, "{}:{}:{}: {}:{} ", file.file(), lines.start(),
               lines.start_idx(), lines.last(), lines.last_idx());
        self.set_fg_color(color);
        self.set_char_attr(CharAttr::Bold, true);
        write!(&self.out, "{}", prefix);
        self.set_fg_color(Color::White);
        f(&mut &self.out);
        self.set_fg_color(Color::Default);
        self.set_char_attr(CharAttr::Bold, false);
        writeln!(&self.out, "");
        let len = lines.len();
        for (_, src) in &mut lines {
            self.set_fg_color(Color::Black);
            self.set_char_attr(CharAttr::Bold, true);
            write!(&self.out, ">>> ");
            self.set_fg_color(Color::Default);
            self.set_char_attr(CharAttr::Bold, false);
            write!(&self.out, "{}", src);
        }
        if len == 1 {
            self.set_fg_color(Color::Black);
            self.set_char_attr(CharAttr::Bold, true);
            write!(&self.out, ">>> ");
            self.set_fg_color(Color::Default);
            self.set_char_attr(CharAttr::Bold, false);
            for _ in 0..lines.start_idx() {
                write!(&self.out, " ");
            }
            self.set_fg_color(color);
            self.set_char_attr(CharAttr::Bold, true);
            write!(&self.out, "^");
            for _ in lines.start_idx()..lines.last_idx() {
                write!(&self.out, "~");
            }
            self.set_fg_color(Color::Default);
            self.set_char_attr(CharAttr::Bold, false);
            writeln!(&self.out, "");
        }
    }
}

impl Diagnostic for FdDiag {
    fn error(&self, span: Span, error: Error) {
        macro_rules! err {
            ($f:expr) => { self.common(span, Color::Red, "error: ", $f) }
        }
        match error {
            Error::Unbound(id) => err!(move |mut w| {
                write!(w, "unbound name: `{}`", self.interner.get(id))
            }),
            Error::FnPatMismatch => err!(move |mut w| {
                write!(w, "argument does not match function pattern")
            }),
            Error::ArgMissingField(id) => err!(move |mut w| {
                write!(w, "argument misses field `{}`", self.interner.get(id))
            }),
            Error::DivideByZero => err!(move |mut w| {
                write!(w, "attempted to divide by zero")
            }),
            Error::FoundExpr(ex, found) => err!(move |mut w| {
                write!(w, "expected {}, found {:?}", ex, found.val)
            }),
            Error::FoundToken(ex, found) => err!(move |mut w| {
                write!(w, "expected {}, found {:?}", ex, found.val)
            }),
            Error::FoundString(ex, found) => err!(move |mut w| {
                write!(w, "expected {}, found {}", ex, found)
            }),
            Error::FoundChar(ex, found) => err!(move |mut w| {
                write!(w, "expected {}, found {:?}", ex, found)
            }),
            Error::MissingSetField(id) => err!(move |mut w| {
                write!(w, "set has no field `{}`", self.interner.get(id))
            }),
            Error::MissingListField(id) => err!(move |mut w| {
                write!(w, "list has no field `{}`", id)
            }),
            Error::CannotStringify(ex) => err!(move |mut w| {
                write!(w, "cannot stringify this expression: {:?}", ex.val)
            }),
            Error::InfiniteRecursion => err!(move |mut w| {
                write!(w, "infinite recursion")
            }),
            Error::OutOfBounds => err!(move |mut w| {
                write!(w, "out of bounds")
            }),
            Error::LexerEof => err!(move |mut w| {
                write!(w, "expected token, found end-of-file")
            }),
            Error::OverflowingLiteral => err!(move |mut w| {
                write!(w, "overflowing literal")
            }),
            Error::InvalidCodepoint => err!(move |mut w| {
                write!(w, "invalid codepoint")
            }),
            Error::UnknownEscapeSequence(c) => err!(move |mut w| {
                write!(w, "unknown escape sequence: {:?}", c)
            }),
            Error::ExpectedEof => err!(move |mut w| {
                write!(w, "expected end-of-file")
            }),
            Error::SetHasNoField(id) => err!(move |mut w| {
                write!(w, "set has no field `{}`", self.interner.get(id))
            }),
            Error::ListHasNoField(id) => err!(move |mut w| {
                write!(w, "list has no field `{}`", id)
            }),
            Error::FnPattern(_) => err!(move |mut w| {
                write!(w, "argument does not match function pattern")
            }),
            Error::FnMissingField(id) => err!(move |mut w| {
                write!(w, "missing field: `{}`", self.interner.get(id))
            }),
            Error::DupSetField(span) => {
                err!(move |mut w| {
                    write!(w, "duplicate set field")
                });
                self.common(span, Color::Cyan, "note: ", move |mut w| {
                    write!(w, "previous declaration")
                });
            },
            Error::AssertionFailed => err!(move |mut w| {
                write!(w, "assertion failed")
            }),
            Error::InteriorNull => err!(move |mut w| {
                write!(w, "string has interior null")
            }),
            Error::AbsolutePath => err!(move |mut w| {
                write!(w, "absolute path required")
            }),
            Error::CannotOpen(path) => err!(move |mut w| {
                write!(w, "cannot open file: {:?}", self.interner.get(path))
            }),
        }
    }

    fn notice(&self, span: Span, notice: Notice) {
        self.common(span, Color::Cyan, "note: ", |mut w| {
            match notice {
                Notice::Via => {
                    write!(w, "via")
                },
            }
        })
    }
}
