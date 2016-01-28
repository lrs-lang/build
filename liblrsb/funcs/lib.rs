// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

#![crate_name = "lrsb_funcs"]
#![crate_type = "lib"]

extern crate lrsb_types as types;
extern crate lrsb_eval as eval;
extern crate lrsb_lexer as lexer;
extern crate lrsb_parser as parser;

use std::rc::{Rc};
use std::share::{RefCell};
use std::file::{File};
use std::string::{NoNullStr};
use std::{error};

use types::diagnostic::{Diagnostic, Error};
use types::tree::{BuiltInFn, SExpr, Expr_, FnType, Expr};
use types::span::{Span};
use types::codemap::{Codemap};
use types::interner::{Interner};
use lexer::{Lexer};
use parser::{Parser};
use eval::{Eval};

macro_rules! bi {
    ($f:expr) => {
        Expr_::Fn(FnType::BuiltIn(try!(Rc::new()).set($f)))
    }
}

pub fn to_list<D>(eval: Rc<Eval<D>>) -> Result<Rc<BuiltInFn>>
    where D: Diagnostic + 'static,
{
    let f = move |e: &SExpr| {
        let fields = try!(eval.get_fields(e));
        let mut list = try!(Vec::with_capacity(fields.size()));
        for (_, &(_, ref val)) in &fields {
            try!(list.push(val.to()));
        }
        Ok(Expr_::List(try!(Rc::new()).set(list)))
    };
    Ok(try!(Rc::new()).set(f))
}

pub fn assert<D>(eval: Rc<Eval<D>>) -> Result<Rc<BuiltInFn>>
    where D: Diagnostic + 'static,
{
    let f = move |cond: &SExpr| {
        let eval = eval.clone();
        let cond = cond.clone();
        let f = move |tail: &SExpr| {
            if try!(eval.get_bool(&cond)) {
                Ok(Expr_::Resolved(None, tail.to()))
            } else {
                eval.diag.error(cond.span, Error::AssertionFailed);
                eval.trace(&cond);
                Err(error::InvalidSequence)
            }
        };
        Ok(bi!(f))
    };
    Ok(try!(Rc::new()).set(f))
}

pub fn contains<D>(eval: Rc<Eval<D>>) -> Result<Rc<BuiltInFn>>
    where D: Diagnostic + 'static,
{
    let f = move |list: &SExpr| {
        let eval = eval.clone();
        let list = list.to();
        let f = move |val: &SExpr| {
            let list = try!(eval.get_list(&list));
            for el in &list {
                if try!(eval.equal_to(el, val)) {
                    return Ok(Expr_::Bool(true))
                }
            }
            Ok(Expr_::Bool(false))
        };
        Ok(bi!(f))
    };
    Ok(try!(Rc::new()).set(f))
}

pub fn filter<D>(eval: Rc<Eval<D>>) -> Result<Rc<BuiltInFn>>
    where D: Diagnostic + 'static,
{
    let f = move |list: &SExpr| {
        let eval = eval.clone();
        let olist = list.clone();
        let f = move |cond: &SExpr| {
            let list = try!(eval.get_list(&olist));
            let mut nlist = try!(Vec::with_capacity(list.len()));
            for el in &list {
                let span = Span::new(olist.span.lo, cond.span.hi);
                let expr = try!(Expr::spanned(span, Expr_::Apl(cond.to(), el.to())));
                if try!(eval.get_bool(&expr)) {
                    try!(nlist.push(el.to()));
                }
            }
            Ok(Expr_::List(try!(Rc::new()).set(nlist)))
        };
        Ok(bi!(f))
    };
    Ok(try!(Rc::new()).set(f))
}

pub fn include<D>(eval: Rc<Eval<D>>, diag: Rc<D>, interner: Rc<Interner>, cwd: Vec<u8>,
                  codemap: Rc<RefCell<Codemap>>) -> Result<Rc<BuiltInFn>>
    where D: Diagnostic + 'static,
{
    let f = move |expr: &SExpr| {
        let pathi = try!(eval.get_string(expr));
        let path: &NoNullStr = try!(interner.get(pathi).try_as_ref());
        if !path.starts_with("/") {
            let res = try!(eval.resolve(expr));
            diag.error(res.span, Error::AbsolutePath);
            eval.trace(expr);
            return Err(error::InvalidSequence);
        }
        let input = {
            let file = match File::open_read(path) {
                Ok(f) => f,
                _ => {
                    let res = try!(eval.resolve(expr));
                    diag.error(res.span, Error::CannotOpen(pathi));
                    eval.trace(expr);
                    return Err(error::InvalidSequence);
                },
            };
            let mut vec = vec!();
            let _ = try!(vec.read_to_eof(file));
            try!(Rc::new()).set(vec)
        };
        let rel_path = if path.starts_with(&cwd) {
            let p = &path[cwd.len()..];
            if p.starts_with("/") {
                &p[1..]
            } else {
                p
            }
        } else {
            path
        };
        let lo = codemap.borrow_mut().add_file(try!(rel_path.try_to()), input.to());
        let tree = {
            let lexer = Lexer::new(lo, input.to(), diag.to(), interner.to(),
                                   try!(path.dir().try_to()));
            let mut parser = Parser::new(lexer, diag.to());
            try!(parser.parse())
        };
        Ok(Expr_::Resolved(None, tree))
    };
    Ok(try!(Rc::new()).set(f))
}
