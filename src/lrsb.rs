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

use lrsb_lexer::{Lexer};
use lrsb_parser::{Parser};
use lrsb_eval::{Eval};

use lrsb_types::diagnostic::{Diagnostic};
use lrsb_types::codemap::{Codemap};
use lrsb_types::interner::{Interner, Interned};
use lrsb_types::span::{Span, Spanned};
use lrsb_types::tree::{SExpr, Expr_, Expr, Selector, BuiltInFn, FnType};

pub fn parse(build_path: &NoNullStr, original: &NoNullStr, cfgs: &[&CStr],
             v: Vec<u8>) -> Result<Build> {
    let interner = try!(Rc::new()).set(Interner::new());
    let input = try!(Rc::new()).set(v);
    let map = {
        let mut path = try!(build_path.to_owned());
        try!(path.push_file("LRSBuild"));
        let mut map = Codemap::new();
        map.add_file(path, input.clone());
        try!(Rc::new()).set(RefCell::new(map))
    };
    let diag = try!(Rc::new()).set(FdDiag::new(map.clone(), STDERR));

    let tree = {
        let lexer = Lexer::new(0, input.clone(), diag.clone(), interner.clone(),
                               try!(build_path.to_owned()));
        let mut parser = Parser::new(lexer, diag.clone());
        try!(parser.parse())
    };

    let cfgs = {
        let mut ncfgs: Vec<_> = try!(Vec::with_capacity(cfgs.len()));
        for c in cfgs {
            let c: &NoNullStr = c.as_ref();
            ncfgs.push(interner.insert(try!(c.to_owned()).unwrap()));
        }
        ncfgs
    };

    let path = {
        let mut p: Vec<_> = vec!();
        let mut path = original;
        while path != build_path {
            let (l, r) = path.split();
            p.push(interner.insert(try!(r.to_owned()).unwrap()));
            path = l;
        }
        p.reverse();
        p
    };

    let expr = std_apl(tree, &interner, &cfgs, &path);

    let eval = Eval::new(diag.clone(), interner.clone());

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
                    diag.error(cfg.span , |mut w| {
                        write!(w, "string contains an interior null: {:?}", c)
                    });
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
        diag.error(target.span , |mut w| {
            write!(w, "dependency list is recursive")
        });
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
                    diag.error(e.span , |mut w| {
                        write!(w, "string contains an interior null2")
                    });
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
            let obj = if let Some(res) = map.get(&addr).map(|d| d.clone()) {
                res
            } else {
                let obj = try!(get_target(eval, &dep, diag, interner, map));
                map.set(addr, obj.clone());
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
    out: FdIo,
    is_term: bool,
}

impl FdDiag {
    pub fn new(codemap: Rc<RefCell<Codemap>>, fd: FdIo) -> FdDiag {
        FdDiag {
            codemap: codemap,
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

    fn common<F>(&self, span: Span, f: F, color: Color, prefix: &str)
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
            for _ in lines.start_idx()..lines.last_idx()-1 {
                write!(&self.out, "~");
            }
            self.set_fg_color(Color::Default);
            self.set_char_attr(CharAttr::Bold, false);
            writeln!(&self.out, "");
        }
    }
}

impl Diagnostic for FdDiag {
    fn error<F>(&self, span: Span, f: F)
        where F: FnOnce(&mut Write) -> Result,
    {
        self.common(span, f, Color::Red, "error: ");
    }

    fn notice<F>(&self, span: Span, f: F)
        where F: FnOnce(&mut Write) -> Result,
    {
        self.common(span, f, Color::Cyan, "note: ");
    }
}

macro_rules! bispan {
    ($e:expr) => { Spanned::new(Span::built_in(), $e) }
}

pub fn std(interner: &Interner, cfgs: &[Interned], path: &[Interned]) -> SExpr {
    let mut fields = Vec::new();

    macro_rules! add_fn {
        ($func:expr, $name:expr) => {{
            let func = bispan!(Expr::new(Expr_::Fn(FnType::BuiltIn($func))));
            let func_ident = bispan!(interner.insert($name.to_owned().unwrap()));
            fields.push((func_ident, func));
        }}
    }

    add_fn!(BuiltInFn::ToList, b"to_list");
    add_fn!(BuiltInFn::GetField(None), b"get_field");
    add_fn!(BuiltInFn::TryGetField(None), b"try_get_field");
    add_fn!(BuiltInFn::Assert(None), b"assert");
    add_fn!(BuiltInFn::Contains(None), b"contains");
    add_fn!(BuiltInFn::Filter(None), b"filter");

    {
        let mut els = vec!();
        for c in cfgs {
            let e = bispan!(Expr::new(Expr_::String(*c)));
            els.push(e);
        }
        let list = bispan!(Expr::new(Expr_::List(Rc::new().unwrap().set(els))));
        let ident = bispan!(interner.insert(b"cfgs".to_owned().unwrap()));
        fields.push((ident, list))
    }

    {
        let mut els = vec!();
        for c in path {
            let e = bispan!(Expr::new(Expr_::String(*c)));
            els.push(e);
        }
        let list = bispan!(Expr::new(Expr_::List(Rc::new().unwrap().set(els))));
        let ident = bispan!(interner.insert(b"path".to_owned().unwrap()));
        fields.push((ident, list))
    }

    bispan!(Expr::new(Expr_::Set(Rc::new().unwrap().set(fields))))
}

pub fn std_apl(expr: SExpr, interner: &Interner, cfgs: &[Interned],
               path: &[Interned]) -> SExpr {
    bispan!(Expr::new(Expr_::Apl(expr, std(interner, cfgs, path))))
}
