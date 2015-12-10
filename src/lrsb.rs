// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

use std::rc::{Rc, Arc};
use std::sync::{Mutex};
use std::share::{RefCell, RefCellStatus};
use std::fd::{STDERR, FdIo};
use std::string::{NoNullStr, CStr, AsNoNullStr, AsByteStr};
use std::{mem, error, tty};
use std::hashmap::{HashMap};

use {BuildStatus, Build, Obj};
use term::{self, Color, CharAttr};

use {lrsb_funcs};
use lrsb_lexer::{Lexer};
use lrsb_parser::{Parser};
use lrsb_eval::{Eval};

use lrsb_types::diagnostic::{Diagnostic, Error, Notice};
use lrsb_types::codemap::{Codemap};
use lrsb_types::interner::{Interner, Interned};
use lrsb_types::span::{Span, Spanned};
use lrsb_types::tree::{SExpr, Expr_, Expr, Selector, FnType};

pub fn parse(build_path: &NoNullStr, original: &NoNullStr, cfgs: &[&CStr],
             v: Vec<u8>) -> Result<Build> {
    let interner = try!(Rc::new()).set(Interner::new());
    let input = try!(Rc::new()).set(v);
    let map = {
        let mut path = try!(build_path.to_owned());
        try!(path.push_file("LRSBuild"));
        let mut map = Codemap::new();
        map.add_file(path, input.to());
        try!(Rc::new()).set(RefCell::new(map))
    };
    let diag = try!(Rc::new()).set(FdDiag::new(map.to(), interner.to(), STDERR));

    let tree = {
        let lexer = Lexer::new(0, input.to(), diag.to(), interner.to(),
                               try!(build_path.to_owned()));
        let mut parser = Parser::new(lexer, diag.to());
        try!(parser.parse())
    };

    let cfgs = {
        let mut ncfgs: Vec<_> = try!(Vec::with_capacity(cfgs.len()));
        for c in cfgs {
            ncfgs.push(interner.insert(try!(c.to_owned()).into()));
        }
        ncfgs
    };

    let path = {
        let mut p: Vec<_> = vec!();
        let mut path = original;
        while path != build_path {
            let (l, r) = path.split();
            p.push(interner.insert(try!(r.to_owned()).into()));
            path = l;
        }
        p.reverse();
        p
    };

    let eval = Rc::new().unwrap().set(Eval::new(diag.to(), interner.to()));

    let expr = try!(std_apl(tree, eval.to(), &interner, &cfgs, &path));

    flatten(&eval, expr, &diag, &interner)
}

fn flatten<D>(eval: &Eval<D>, expr: SExpr, diag: &D,
              interner: &Interner) -> Result<Build>
    where D: Diagnostic,
{
    let id_cfgs = interner.insert(try!(b"cfgs".to_owned()));
    let id_target = interner.insert(try!(b"target".to_owned()));

    let cfgs = {
        let cfgs = try!(eval.get_field(&expr, &Selector::Ident(id_cfgs), None));
        let cfgs = try!(eval.get_list(&cfgs));
        let mut rcfgs = try!(Vec::with_capacity(cfgs.len()));
        for cfg in &cfgs {
            let c = try!(eval.get_string(cfg));
            let c = interner.get(c);
            let c = match c.as_no_null_str() {
                Ok(c) => try!(c.to_owned()),
                Err(..) => {
                    diag.error(cfg.span, Error::InteriorNull);
                    eval.trace(cfg);
                    return Err(error::InvalidSequence);
                },
            };
            rcfgs.push(c);
        }
        rcfgs
    };

    let mut obj_map = try!(HashMap::new());

    let t = try!(eval.get_field(&expr, &Selector::Ident(id_target), None));
    let target = try!(get_target(eval, &t, diag, interner, &mut obj_map));

    Ok(Build {
        cfgs: cfgs,
        target: target,
    })
}

fn get_target<D>(eval: &Eval<D>, target: &SExpr, diag: &D, interner: &Interner,
                 map: &mut HashMap<usize, Arc<Mutex<Obj>>>) -> Result<Arc<Mutex<Obj>>>
    where D: Diagnostic,
{
    if target.val.val.status() != RefCellStatus::Free {
        diag.error(target.span, Error::InfiniteRecursion);
        eval.trace(target);
        return Err(error::InvalidSequence);
    };
    try!(eval.force(target));
    // let _borrow = target.val.val.borrow();

    macro_rules! get_string {
        ($f:expr) => {{
            let id = interner.insert(try!($f.to_owned()));
            let e = try!(eval.get_field(target, &Selector::Ident(id), None));
            let s = try!(eval.get_string(&e));
            match interner.get(s).as_no_null_str() {
                Ok(s) => try!(s.to_owned()),
                Err(..) => {
                    diag.error(e.span, Error::InteriorNull);
                    eval.trace(&e);
                    return Err(error::InvalidSequence);
                },
            }
        }}
    };

    let crate_name = get_string!(b"crate_name");
    let path = get_string!(b"path");
    let name = get_string!(b"name");

    let deps = {
        let id = interner.insert(try!(b"deps".to_owned()));
        let deps = try!(eval.get_field(target, &Selector::Ident(id), None));
        let deps = try!(eval.get_list(&deps));
        let mut rdeps = try!(Vec::with_capacity(deps.len()));
        for dep in &deps {
            let dep = try!(eval.resolve(dep));
            let addr = mem::addr::<RefCell<_>>(&dep.val.val);
            let obj = if let Some(res) = map.get(&addr).map(|d| d.to()) {
                res
            } else {
                let obj = try!(get_target(eval, &dep, diag, interner, map));
                map.set(addr, obj.to());
                obj
            };
            rdeps.push(obj);
        }
        rdeps
    };
    
    let obj = Obj {
        crate_name: crate_name,
        path: path,
        name: name,
        deps: deps,
        needs_rebuild: None,
        obj_modified: None,
        build_status: BuildStatus::Pending,
    };

    Ok(try!(Arc::new()).set(Mutex::new(obj)))
}

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
            write!(&self.out, "{}", src.as_byte_str());
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

macro_rules! bispan {
    ($e:expr) => { Spanned::new(Span::built_in(), $e) }
}

pub fn std<D>(eval: Rc<Eval<D>>, interner: &Interner, cfgs: &[Interned],
              path: &[Interned]) -> Result<SExpr>
    where D: Diagnostic + 'static,
{
    let mut fields = HashMap::new().unwrap();

    macro_rules! add_fn {
        ($func:ident) => {{
            let func = try!(lrsb_funcs::$func(eval.to()));
            let func = bispan!(Expr::new(Expr_::Fn(FnType::BuiltIn(func))));
            let func_ident = interner.insert(stringify!($func)
                                                     .to_owned().unwrap().into());
            fields.set(func_ident, (Span::built_in(), func));
        }}
    }

    add_fn!(to_list);
    add_fn!(assert);
    add_fn!(contains);
    add_fn!(filter);

    {
        let mut els = vec!();
        for c in cfgs {
            let e = bispan!(Expr::new(Expr_::String(*c)));
            els.push(e);
        }
        let list = bispan!(Expr::new(Expr_::List(Rc::new().unwrap().set(els))));
        let ident = interner.insert(b"cfgs".to_owned().unwrap());
        fields.set(ident, (Span::built_in(), list));
    }

    {
        let mut els = vec!();
        for c in path {
            let e = bispan!(Expr::new(Expr_::String(*c)));
            els.push(e);
        }
        let list = bispan!(Expr::new(Expr_::List(Rc::new().unwrap().set(els))));
        let ident = interner.insert(b"path".to_owned().unwrap());
        fields.set(ident, (Span::built_in(), list));
    }

    Ok(bispan!(Expr::new(Expr_::Set(Rc::new().unwrap().set(fields), false))))
}

pub fn std_apl<D>(expr: SExpr, eval: Rc<Eval<D>>, interner: &Interner, cfgs: &[Interned],
                  path: &[Interned]) -> Result<SExpr>
    where D: Diagnostic + 'static,
{
    Ok(bispan!(Expr::new(Expr_::Apl(expr, try!(std(eval, interner, cfgs, path))))))
}
