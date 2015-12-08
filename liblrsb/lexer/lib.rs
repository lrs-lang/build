// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

#![crate_name = "lrsb_lexer"]
#![crate_type = "lib"]

extern crate lrsb_types as types;

use std::rc::{Rc};
use std::string::{NoNullString};
use std::ringbuf::{DynRingBuf};
use std::{error};
use std::parse::{Parsable, HexU32};

use types::diagnostic::{Diagnostic, Error};
use types::token::{SToken, Token};
use types::span::{Span, Spanned};
use types::interner::{Interner, Interned};

pub struct CharStream {
    src: Rc<Vec<u8>>,
    peek: DynRingBuf<(u32, u8)>,
    pos: usize,
}

impl CharStream {
    fn next(&mut self) -> Option<(u32, u8)> {
        if self.peek.len() > 0 {
            return self.peek.pop_left();
        }
        self.real_next()
    }

    fn real_next(&mut self) -> Option<(u32, u8)> {
        let head = &self.src[self.pos..];
        if head.len() > 0 {
            self.pos += 1;
            Some(((self.pos - 1) as u32, head[0]))
        } else {
            None
        }
    }

    fn peek(&mut self, n: usize) -> Option<(u32, u8)> {
        if self.peek.len() <= n {
            for _ in 0..(n + 1 - self.peek.len()) {
                match self.real_next() {
                    Some(c) => self.peek.push_right(c),
                    _ => return None,
                }
            }
        }
        Some(self.peek[n])
    }

    fn peek_char(&mut self, n: usize) -> Option<u8> {
        self.peek(n).map(|v| v.1)
    }

    fn pos(&self) -> u32 {
        if self.peek.len() > 0 {
            self.peek[0].0
        } else {
            self.pos as u32
        }
    }

    fn text(&self) -> &[u8] {
        &self.src[self.pos() as usize..]
    }
}

pub struct Lexer<D: Diagnostic> {
    diagnostic: Rc<D>,
    interner: Rc<Interner>,
    peek: DynRingBuf<SToken>,
    lo: u32,
    pub chars: CharStream,
    dir: NoNullString,
}

macro_rules! next_t {
    ($slf:expr, $pat:pat, $name:expr) => {
        match try!($slf.next()) {
            t @ Spanned { val: $pat, .. } => Ok(t),
            t => {
                $slf.diagnostic.error(t.span, Error::FoundToken($name, t));
                Err(error::InvalidSequence)
            },
        }
    }
}

impl<D: Diagnostic> Lexer<D> {
    pub fn new(lo: u32, src: Rc<Vec<u8>>, diag: Rc<D>, interner: Rc<Interner>,
               dir: NoNullString) -> Lexer<D> {
        Lexer {
            diagnostic: diag,
            interner: interner,
            peek: DynRingBuf::new(),
            lo: lo,
            chars: CharStream {
                src: src,
                peek: DynRingBuf::new(),
                pos: 0,
            },
            dir: dir,
        }
    }

    pub fn pos(&self) -> u32 {
        self.lo + self.chars.pos()
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.chars.peek_char(0) {
                Some(b' ') | Some(b'\t') | Some(b'\n')  => { self.chars.next(); },
                _ => break,
            }
        }
    }

    fn skip_line(&mut self) {
        loop {
            match self.chars.next() {
                Some((_, b'\n')) | None => break,
                _ => { },
            }
        }
    }

    pub fn eof(&mut self) -> Result<bool> {
        Ok(try!(self.try_peek(0)).is_none())
    }

    fn unexpected_eof<T>(&self) -> Result<T> {
        self.diagnostic.error(Span { lo: self.pos(), hi: self.pos() },
                              Error::FoundString("token", "end-of-file"));
        Err(error::InvalidSequence)
    }

    pub fn peek(&mut self, n: usize) -> Result<SToken> {
        match try!(self.try_peek(n)) {
            Some(t) => Ok(t),
            _ => self.unexpected_eof(),
        }
    }

    pub fn try_peek(&mut self, n: usize) -> Result<Option<SToken>> {
        if self.peek.len() <= n {
            for _ in 0..(n + 1 - self.peek.len()) {
                match try!(self.real_next()) {
                    Some(c) => self.peek.push_right(c),
                    _ => return Ok(None),
                }
            }
        }
        Ok(Some(self.peek[n]))
    }

    pub fn next_ident(&mut self) -> Result<(SToken, Interned)> {
        let t = try!(self.next()); 
        match t.val {
            Token::Ident(id) => Ok((t, id)),
            _ => {
                self.diagnostic.error(t.span, Error::FoundToken("ident", t));
                Err(error::InvalidSequence)
            },
        }
    }

    pub fn next_assign(&mut self) -> Result<SToken> {
        next_t!(self, Token::Assign, "`=`")
    }

    pub fn next_then(&mut self) -> Result<SToken> {
        next_t!(self, Token::Then, "`then`")
    }

    pub fn next_else(&mut self) -> Result<SToken> {
        next_t!(self, Token::Else, "`else`")
    }

    pub fn next_left_brace(&mut self) -> Result<SToken> {
        next_t!(self, Token::LeftBrace, "`{`")
    }

    pub fn next_right_brace(&mut self) -> Result<SToken> {
        next_t!(self, Token::RightBrace, "`}`")
    }

    pub fn next_semicolon(&mut self) -> Result<SToken> {
        next_t!(self, Token::Semicolon, "`;`")
    }

    pub fn next_right_paren(&mut self) -> Result<SToken> {
        next_t!(self, Token::RightParen, "`)`")
    }

    pub fn next_colon(&mut self) -> Result<SToken> {
        next_t!(self, Token::Colon, "`:`")
    }

    pub fn next(&mut self) -> Result<SToken> {
        match try!(self.try_next()) {
            Some(t) => Ok(t),
            _ => self.unexpected_eof(),
        }
    }

    pub fn try_next(&mut self) -> Result<Option<SToken>> {
        if self.peek.len() > 0 {
            return Ok(self.peek.pop_left());
        }
        self.real_next()
    }

    fn real_next(&mut self) -> Result<Option<SToken>> {
        loop {
            self.skip_whitespace();
            if Some(b'/') == self.chars.peek_char(0) &&
                Some(b'/') == self.chars.peek_char(1)
            {
                self.skip_line();
            } else {
                break;
            }
        }


        let (cur_pos, cur) = match self.chars.peek(0) {
            Some(c) => c,
            _ => return Ok(None),
        };

        if cur == b'"' {
            return self.string(false);
        }

        macro_rules! ret {
            ($ty:ident) => {
                return Ok(Some(Spanned::new(Span::new(self.lo+cur_pos, self.pos()),
                                            $ty)));
            }
        }

        let mut tkn = None;

        macro_rules! one {
            ($ty:ident) => {
                tkn = Some((Token::$ty, false));
            }
        }

        match cur {
            b'[' => one!(LeftBracket),
            b']' => one!(RightBracket),
            b'{' => one!(LeftBrace),
            b'}' => one!(RightBrace),
            b'(' => one!(LeftParen),
            b')' => one!(RightParen),
            b';' => one!(Semicolon),
            b':' => one!(Colon),
            b',' => one!(Comma),
            b'*' => one!(Times),
            b'/' => one!(Div),
            b'?' => one!(Questionmark),
            b'@' => one!(At),
            b'%' => one!(Mod),
            _ => { },
        }

        if let Some((t, _)) = tkn {
            self.chars.next();
            ret!(t);
        }

        macro_rules! two {
            ($ty:ident) => {
                tkn = Some((Token::$ty, true));
            }
        }

        if let Some(c) = self.chars.peek_char(1) {
            match (cur, c) {
                (b'\\', b'\\') => two!(Overlay),
                (b'&', b'&') => two!(AndAnd),
                (b'|', b'|') => two!(OrOr),
                (b'.', b'.') => two!(DotDot),
                (b'.', _  ) => one!(Dot),
                (b'=', b'=') => two!(Equals),
                (b'=', _  ) => one!(Assign),
                (b'!', b'=') => two!(Unequal),
                (b'!', _  ) => one!(Not),
                (b'<', b'=') => two!(Le),
                (b'<', _  ) => one!(Lt),
                (b'>', b'=') => two!(Ge),
                (b'>', _  ) => one!(Gt),
                (b'+', b'+') => two!(Concat),
                (b'+', _  ) => one!(Plus),
                (b'-', b'>') => two!(Implies),
                (b'-', _  ) => one!(Minus),
                _ => { },
            }
        }

        if let Some((t, two)) = tkn {
            self.chars.next();
            if two { self.chars.next(); }
            ret!(t);
        }

        let mut tkn = None;

        macro_rules! is_ident_cont {
            ($c:expr) => {
                match $c {
                    b'a' ... b'z' | b'A' ... b'Z' | b'0' ... b'9' | b'_' => true,
                    _ => false,
                }
            }
        }

        macro_rules! keyword {
            ($txt:expr, $ty:ident) => {
                if self.chars.text().starts_with($txt) {
                    match self.chars.peek_char($txt.len()) {
                        Some(c) => if !is_ident_cont!(c) {
                            tkn = Some((Token::$ty, $txt.len()));
                        },
                        None => tkn = Some((Token::$ty, $txt.len())),
                    }
                }
            }
        }

        keyword!(b"true", True);
        keyword!(b"false", False);
        keyword!(b"null", Null);
        keyword!(b"rec", Rec);
        keyword!(b"let", Let);
        keyword!(b"in", In);
        keyword!(b"inherit", Inherit);
        keyword!(b"if", If);
        keyword!(b"then", Then);
        keyword!(b"else", Else);
        keyword!(b"or", Or);

        if let Some((t, skip)) = tkn {
            for _ in 0..skip { self.chars.next(); }
            ret!(t);
        }

        if let b'0' ... b'9' = cur {
            let (val, len) = i64::parse_bytes_init(self.chars.text().as_bytes()).unwrap();
            if let Some((next_pos, next)) = self.chars.peek(len) {
                if let b'0' ... b'9' = next {
                    self.diagnostic.error(Span::new(self.lo+cur_pos, self.lo+next_pos),
                                          Error::OverflowingLiteral);
                    return Err(error::InvalidSequence);
                }
            }
            for _ in 0..len { self.chars.next(); }
            return Ok(Some(Spanned::new(Span::new(self.lo+cur_pos, self.pos()),
                                        Token::Integer(val))));
        }

        macro_rules! is_ident_start {
            ($c:expr) => {
                match $c {
                    b'a' ... b'z' | b'A' ... b'Z' | b'_' => true,
                    _ => false,
                }
            }
        }

        if is_ident_start!(cur) {
            let mut i = 1;
            while let Some(c) = self.chars.peek_char(i) {
                if is_ident_cont!(c) {
                    i += 1;
                } else {
                    break;
                }
            }
            let ident = try!(self.chars.text()[..i].to_owned());
            let ident = self.interner.insert(ident);
            for _ in 0..i { self.chars.next(); }
            return Ok(Some(Spanned::new(Span::new(self.lo+cur_pos, self.pos()),
                                        Token::Ident(ident))));
        }

        self.diagnostic.error(
            Span::new(self.lo+cur_pos, self.lo+cur_pos+1),
            if cur & 0x80 != 0 {
                Error::FoundString("token", "non-ascii byte")
            } else {
                Error::FoundChar("token", cur as char)
            }
        );
        Err(error::InvalidSequence)
    }

    pub fn cont_string(&mut self) -> Result<SToken> {
        match try!(self.string(true)) {
            Some(t) => Ok(t),
            _ => self.unexpected_eof(),
        }
    }

    fn string(&mut self, is_cont: bool) -> Result<Option<SToken>> {
        let start = if is_cont {
            let b = try!(self.next_right_brace());
            self.chars.peek.clear();
            self.chars.pos = (b.span.hi - self.lo) as usize;
            self.peek.clear();
            self.pos()
        } else {
            let pos = self.pos();
            self.chars.next(); // eat the "
            pos
        };

        let mut res = Vec::new();
        while let Some((esc_pos, cur)) = self.chars.next() {
            if cur == b'"' {
                break;
            }
            if cur != b'\\' {
                res.try_push(cur).unwrap();
                continue;
            }
            let (pos, cur) = match self.chars.next() {
                Some(c) => c,
                _ => {
                    self.diagnostic.error(Span { lo: self.pos(), hi: self.pos(), },
                                          Error::FoundString("escape sequence",
                                                             "end-of-file"));
                    return Err(error::InvalidSequence);
                }
            };
            match cur {
                b'\\' => res.try_push(b'\\').unwrap(),
                b'"' => res.try_push(b'"').unwrap(),
                b'n' => res.try_push(b'\n').unwrap(),
                b't' => res.try_push(b'\t').unwrap(),
                b'u' => {
                    try!(self.next_left_brace());
                    let before = self.pos();
                    let (val, len) = try!(HexU32::parse_bytes_init(
                                                    self.chars.text().as_bytes()));
                    if len == 0 {
                        self.diagnostic.error(Span { lo: self.pos(), hi: self.pos()+1, },
                                              Error::FoundString("codepoint", "?"));
                        return Err(error::InvalidSequence);
                    }
                    for _ in 0..len { self.chars.next(); }
                    let after = self.pos();
                    try!(self.next_right_brace());
                    let chr = match char::from_u32(val.0) {
                        Some(c) => c,
                        _ => {
                            self.diagnostic.error(Span { lo: before, hi: after, },
                                                  Error::InvalidCodepoint);
                            return Err(error::InvalidSequence);
                        },
                    };
                    let bytes = chr.to_utf8();
                    res.push_all(&bytes[..chr.len()]);
                },
                b'd' => res.push_all(&self.dir).unwrap(),
                b'{' => {
                    let id = self.interner.insert(res);
                    let span = Span::new(start, esc_pos);
                    return Ok(Some(Spanned::new(span, Token::StringPart(id))));
                },
                _ => {
                    self.diagnostic.error(Span { lo: self.lo+pos, hi: self.pos() },
                                          Error::UnknownEscapeSequence(cur));
                    return Err(error::InvalidSequence);
                },
            }
        }
        let id = self.interner.insert(res);
        Ok(Some(Spanned::new(Span::new(start, self.pos()), Token::String(id))))
    }
}
