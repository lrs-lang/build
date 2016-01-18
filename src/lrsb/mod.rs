// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

use std::rc::{Rc, Arc};
use std::sync::{Mutex};
use std::share::{RefCell, RefCellStatus};
use std::fd::{STDERR, FdIo};
use std::string::{NoNullStr, CStr};
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
use lrsb_types::span::{Span};
use lrsb_types::tree::{SExpr, Expr_, Expr, Selector, FnType};

pub fn parse(build_path: &NoNullStr, original: &NoNullStr, cfgs: &[&CStr],
             v: Vec<u8>) -> Result<Build> {
    let interner = Rc::new()?.set(Interner::new()?);
    let input = Rc::new()?.set(v);
    let map = {
        let mut map = Codemap::new();
        map.add_file("LRSBuild".try_to()?, input.to());
        Rc::new()?.set(RefCell::new(map))
    };
    let diag = Rc::new()?.set(FdDiag::new(map.to(), interner.to(), STDERR));

    let tree = {
        let lexer = Lexer::new(0, input.to(), diag.to(), interner.to(),
                               build_path.try_to()?);
        let mut parser = Parser::new(lexer, diag.to());
        parser.parse()?
    };

    let cfgs = {
        let mut ncfgs: Vec<_> = Vec::with_capacity(cfgs.len())?;
        for c in cfgs {
            ncfgs.push(interner.insert((*c).try_to()?)?);
        }
        ncfgs
    };

    let path = {
        let mut p = vec!();
        let mut path = original;
        while path != build_path {
            let (l, r) = path.split_path();
            p.push(interner.insert(r.try_to()?)?);
            path = l;
        }
        p.reverse();
        p
    };

    let eval = Rc::new()?.set(Eval::new(diag.to(), interner.to()));

    let expr = std_apl(tree, eval.to(), interner.to(), diag.to(), build_path, map.to(),
                       &cfgs, &path)?;

    flatten(&eval, expr, &diag, &interner)
}

fn flatten<D>(eval: &Eval<D>, expr: SExpr, diag: &D,
              interner: &Interner) -> Result<Build>
    where D: Diagnostic,
{
    let id_cfgs = interner.insert("cfgs".try_to()?)?;
    let id_target = interner.insert("target".try_to()?)?;

    let cfgs = {
        let cfgs = eval.get_field(&expr, &Selector::Ident(id_cfgs), None)?;
        let cfgs = eval.get_list(&cfgs)?;
        let mut rcfgs = Vec::with_capacity(cfgs.len())?;
        for cfg in &cfgs {
            let c = eval.get_string(cfg)?;
            let c: Result<&NoNullStr> = interner.get(c).try_as_ref();
            let c = match c {
                Ok(c) => c.try_to()?,
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

    let mut obj_map = HashMap::new()?;

    let t = eval.get_field(&expr, &Selector::Ident(id_target), None)?;
    let target = get_target(eval, &t, diag, interner, &mut obj_map)?;

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
    eval.force(target)?;
    // let _borrow = target.val.val.borrow();

    macro_rules! get_string {
        ($f:expr) => {{
            let id = interner.insert($f.try_to()?)?;
            let e = eval.get_field(target, &Selector::Ident(id), None)?;
            let s = eval.get_string(&e)?;
            let s: Result<&NoNullStr> = interner.get(s).try_as_ref();
            match s {
                Ok(s) => s.try_to()?,
                Err(..) => {
                    diag.error(e.span, Error::InteriorNull);
                    eval.trace(&e);
                    return Err(error::InvalidSequence);
                },
            }
        }}
    };

    let crate_name = get_string!("crate_name");
    let path = get_string!("path");
    let name = get_string!("name");

    let deps = {
        let id = interner.insert("deps".try_to()?)?;
        let deps = eval.get_field(target, &Selector::Ident(id), None)?;
        let deps = eval.get_list(&deps)?;
        let mut rdeps = Vec::with_capacity(deps.len())?;
        for dep in &deps {
            let dep = eval.resolve(dep)?;
            let addr = mem::addr::<RefCell<_>>(&dep.val.val);
            let obj = if let Some(res) = map.get(&addr).map(|d| d.to()) {
                res
            } else {
                let obj = get_target(eval, &dep, diag, interner, map)?;
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

    Ok(Arc::new()?.set(Mutex::new(obj)))
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

macro_rules! bispan {
    ($e:expr) => { Expr::spanned(Span::built_in(), $e)? }
}

pub fn std<D>(eval: Rc<Eval<D>>, interner: Rc<Interner>, diag: Rc<D>,
              build_path: &NoNullStr, codemap: Rc<RefCell<Codemap>>, cfgs: &[Interned],
              path: &[Interned]) -> Result<SExpr>
    where D: Diagnostic + 'static,
{
    let mut fields = HashMap::new()?;

    macro_rules! add_fn {
        ($func:ident) => {{
            let func = lrsb_funcs::$func(eval.to())?;
            let func = bispan!(Expr_::Fn(FnType::BuiltIn(func)));
            let func_ident = interner.insert(stringify!($func).try_to()?)?;
            fields.set(func_ident, (Span::built_in(), func));
        }}
    }

    add_fn!(to_list);
    add_fn!(assert);
    add_fn!(contains);
    add_fn!(filter);

    {
        let func = lrsb_funcs::include(eval.to(), diag.to(), interner.to(),
                                       build_path.try_to()?, codemap.to()?);
        let func = bispan!(Expr_::Fn(FnType::BuiltIn(func)));
        let func_ident = interner.insert("include".try_to()?)?;
        fields.set(func_ident, (Span::built_in(), func));
    }

    {
        let mut els = vec!();
        for c in cfgs {
            let e = bispan!(Expr_::String(*c));
            els.push(e);
        }
        let list = bispan!(Expr_::List(Rc::new().unwrap().set(els)));
        let ident = interner.insert("cfgs".try_to().unwrap())?;
        fields.set(ident, (Span::built_in(), list));
    }

    {
        let mut els = vec!();
        for c in path {
            let e = bispan!(Expr_::String(*c));
            els.push(e);
        }
        let list = bispan!(Expr_::List(Rc::new().unwrap().set(els)));
        let ident = interner.insert("path".try_to().unwrap())?;
        fields.set(ident, (Span::built_in(), list));
    }

    Ok(bispan!(Expr_::Set(Rc::new()?.set(fields), false)))
}

pub fn std_apl<D>(expr: SExpr, eval: Rc<Eval<D>>, interner: Rc<Interner>, diag: Rc<D>,
                  build_path: &NoNullStr, codemap: Rc<RefCell<Codemap>>, cfgs: &[Interned],
                  path: &[Interned]) -> Result<SExpr>
    where D: Diagnostic + 'static,
{
    Ok(bispan!(Expr_::Apl(expr, std(eval, interner, diag, build_path, codemap, cfgs, path)?)))
}
