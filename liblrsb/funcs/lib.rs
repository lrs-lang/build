// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

#![crate_name = "lrsb_funcs"]
#![crate_type = "lib"]

extern crate lrsb_types as types;
extern crate lrsb_eval as eval;

use std::rc::{Rc};
use std::marker::{Leak};
use std::{error};

use types::diagnostic::{Diagnostic, Error};
use types::tree::{BuiltInFn, SExpr, Expr_, FnType, Expr};
use types::span::{Span};
use eval::{Eval};

struct Bi<C>(C);

impl<C> BuiltInFn for Bi<C>
    where C: for<'a> Fn(&'a SExpr)->Result<Expr_> + Leak
{
    fn apply<'a>(&self, expr: &'a SExpr) -> Result<Expr_> {
        self.0(expr)
    }
}

macro_rules! bi {
    ($f:expr) => {
        Expr_::Fn(FnType::BuiltIn(try!(Rc::new()).set(Bi($f))))
    }
}

pub fn to_list<D>(eval: Rc<Eval<D>>) -> Result<Rc<BuiltInFn>>
    where D: Diagnostic + 'static,
{
    let f = move |e: &SExpr| {
        let fields = try!(eval.get_fields(e));
        let mut list = try!(Vec::with_capacity(fields.size()));
        for (_, &(_, ref val)) in &fields {
            list.push(val.to());
        }
        Ok(Expr_::List(try!(Rc::new()).set(list)))
    };
    Ok(try!(Rc::new()).set(Bi(f)))
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
    Ok(try!(Rc::new()).set(Bi(f)))
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
    Ok(try!(Rc::new()).set(Bi(f)))
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
                    nlist.push(el.to());
                }
            }
            Ok(Expr_::List(try!(Rc::new()).set(nlist)))
        };
        Ok(bi!(f))
    };
    Ok(try!(Rc::new()).set(Bi(f)))
}
