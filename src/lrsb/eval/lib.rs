// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

#![crate_name = "lrsb_eval"]
#![crate_type = "lib"]

extern crate lrsb_types as types;

use std::rc::{Rc};
use std::share::{RefCellStatus};
use std::{error};
use std::clone::{MaybeClone};
use std::iter::{IteratorExt};

use types::diagnostic::{Diagnostic};
use types::interner::{Interner, Interned};
use types::span::{Spanned, Span};
use types::scope::{Scope, PseudoScope};
use types::tree::{SExpr, Expr, Expr_, FnArg, Selector, FnType, BuiltInFn};

pub struct Eval<D: Diagnostic> {
    diag: Rc<D>,
    interner: Rc<Interner>,
}

impl<D: Diagnostic> Eval<D> {
    pub fn new(diag: Rc<D>, interner: Rc<Interner>) -> Eval<D> {
        Eval {
            diag: diag,
            interner: interner,
        }
    }

    fn bi_to_list(&self, expr: &SExpr) -> Result<Expr_> {
        let fields = try!(self.get_fields(expr));
        let mut list = Vec::with_capacity(fields.len()).unwrap();
        for field in &fields {
            list.push(field.1.clone());
        }
        Ok(Expr_::List(Rc::new().unwrap().set(list)))
    }

    fn bi_get_field(&self, name: Option<SExpr>, expr: &SExpr) -> Result<Expr_> {
        if let Some(name) = name {
            let name = try!(self.get_string(&name));
            let field = try!(self.get_opt_field(expr, &Selector::Ident(name), None));
            if let Some(field) = field {
                try!(self.force(&field));
                Ok(Expr_::Resolved(None, field))
            } else {
                self.diag.error(expr.span , |mut w| {
                    write!(w, "set has no field `{}`", self.interner.get(name))
                });
                self.trace(expr);
                Err(error::InvalidSequence)
            }
        } else {
            Ok(Expr_::Fn(FnType::BuiltIn(BuiltInFn::GetField(Some(expr.clone())))))
        }
    }

    fn bi_try_get_field(&self, name: Option<SExpr>, expr: &SExpr) -> Result<Expr_> {
        if let Some(name) = name {
            let name = try!(self.get_string(&name));
            let field = try!(self.get_opt_field(expr, &Selector::Ident(name), None));
            if let Some(field) = field {
                try!(self.force(&field));
                Ok(Expr_::Resolved(None, field))
            } else {
                Ok(Expr_::Null)
            }
        } else {
            Ok(Expr_::Fn(FnType::BuiltIn(BuiltInFn::TryGetField(Some(expr.clone())))))
        }
    }

    fn bi_assert(&self, cond: Option<SExpr>, tail: &SExpr) -> Result<Expr_> {
        if let Some(cond) = cond {
            if try!(self.get_bool(&cond)) {
                Ok(Expr_::Resolved(None, tail.clone()))
            } else {
                self.diag.error(cond.span , |mut w| {
                    write!(w, "assertion failed")
                });
                self.trace(&cond);
                Err(error::InvalidSequence)
            }
        } else {
            Ok(Expr_::Fn(FnType::BuiltIn(BuiltInFn::Assert(Some(tail.clone())))))
        }
    }

    fn bi_contains(&self, list: Option<SExpr>, arg: &SExpr) -> Result<Expr_> {
        if let Some(list) = list {
            let list = try!(self.get_list(&list));
            for el in &list {
                if try!(self.equal_to(el, arg)) {
                    return Ok(Expr_::Bool(true))
                }
            }
            Ok(Expr_::Bool(false))
        } else {
            Ok(Expr_::Fn(FnType::BuiltIn(BuiltInFn::Contains(Some(arg.clone())))))
        }
    }

    fn bi_filter(&self, list: Option<SExpr>, arg: &SExpr) -> Result<Expr_> {
        if let Some(expr) = list {
            let list = try!(self.get_list(&expr));
            let mut nlist = try!(Vec::with_capacity(list.len()));
            for el in &list {
                let span = Span::new(expr.span.lo, arg.span.hi);
                let expr = Expr::new(Expr_::Apl(arg.clone(), el.clone()));
                if try!(self.get_bool(&Spanned::new(span, expr))) {
                    nlist.push(el.clone());
                }
            }
            Ok(Expr_::List(Rc::new().unwrap().set(nlist)))
        } else {
            Ok(Expr_::Fn(FnType::BuiltIn(BuiltInFn::Filter(Some(arg.clone())))))
        }
    }

    /// A helper function for `resolve_names` that recursively traverses the expression
    /// tree.
    ///
    /// [argument, span]
    /// The span of the expression.
    ///
    /// [argument, bound]
    /// The arguments bound by a `let` expression.
    ///
    /// [argument, unbound]
    /// The arguments bound by a function definition.
    fn resolve_names_(&self, expr: &SExpr, bound: &Scope,
                      unbound: &mut PseudoScope) -> Result {
        macro_rules! nary {
            ($a:expr) => {{
                self.resolve_names_($a, bound, unbound)
            }};
            ($a:expr, $b:expr) => {{
                try!(self.resolve_names_($a, bound, unbound));
                self.resolve_names_($b, bound, unbound)
            }};
            ($a:expr, $b:expr, $c:expr) => {{
                try!(self.resolve_names_($a, bound, unbound));
                try!(self.resolve_names_($b, bound, unbound));
                self.resolve_names_($c, bound, unbound)
            }}
        }

        let mut val = expr.val.val.borrow_mut();
        if let Expr_::Ident(id) = *val {
            if unbound.get(id).is_none() {
                let var = match bound.get(id) {
                    Some(e) => e,
                    _ => {
                        self.diag.error(expr.span , |mut w| {
                            write!(w, "undefined name: `{}`", self.interner.get(id))
                        });
                        return Err(error::InvalidSequence);
                    }
                };
                *val = Expr_::Resolved(Some(id), var);
            }
            return Ok(())
        }

        match *val {
            Expr_::Integer(..) | Expr_::Dummy => Ok(()),
            Expr_::Ident(..) | Expr_::Set(..) => abort!(),
            Expr_::PreSet(ref fields, rec) => {
                // If a set is recursive, the expressions inside the set can see the other
                // variables. But this excludes inherited fields.

                for field in fields {
                    if field.1.is_dummy() && unbound.get(field.0.val).is_none() {
                        match bound.get(field.0.val) {
                            Some(e) => {
                                *field.1.val.val.borrow_mut() =
                                                Expr_::Resolved(Some(field.0.val), e);
                            },
                            _ => {
                                self.diag.error(expr.span , |mut w| {
                                    write!(w, "undefined name: `{}`",
                                           self.interner.get(field.0.val))
                                });
                                return Err(error::InvalidSequence);
                            }
                        };
                    }
                }

                if rec {
                    for e in fields {
                        unbound.push(e.0.val, ());
                    }
                }
                for e in fields {
                    try!(self.resolve_names_(&e.1, bound, unbound));
                }
                if rec {
                    for e in fields {
                        unbound.pop(e.0.val);
                    }
                }
                Ok(())
            },
            Expr_::And(ref l, ref r) => nary!(l, r),
            Expr_::Or(ref l, ref r) => nary!(l, r),
            Expr_::Not(ref e) => nary!(e),
            Expr_::Stringify(ref e) => nary!(e),
            Expr_::Add(ref l, ref r) => nary!(l, r),
            Expr_::Min(ref l, ref r) => nary!(l, r),
            Expr_::Mul(ref l, ref r) => nary!(l, r),
            Expr_::Div(ref l, ref r) => nary!(l, r),
            Expr_::Mod(ref l, ref r) => nary!(l, r),
            Expr_::Gt(ref l, ref r) => nary!(l, r),
            Expr_::Lt(ref l, ref r) => nary!(l, r),
            Expr_::Ge(ref l, ref r) => nary!(l, r),
            Expr_::Le(ref l, ref r) => nary!(l, r),
            Expr_::Eq(ref l, ref r) => nary!(l, r),
            Expr_::Ne(ref l, ref r) => nary!(l, r),
            Expr_::Overlay(ref l, ref r) => nary!(l, r),
            Expr_::Concat(ref l, ref r) => nary!(l, r),
            Expr_::Apl(ref l, ref r) => nary!(l, r),
            Expr_::Neg(ref e) => nary!(e),
            Expr_::Cond(ref cond, ref then, ref el) => nary!(cond, then, el),
            Expr_::Bool(..) => Ok(()),
            Expr_::Null => Ok(()),
            Expr_::Test(ref e, ref path) => {
                try!(nary!(e));
                for seg in path {
                    if let Selector::Expr(ref e) = seg.val {
                        try!(nary!(e));
                    }
                }
                Ok(())
            },
            Expr_::Select(ref e, ref path, ref a) => {
                if let Some(ref a) = *a {
                    try!(nary!(a));
                }
                for seg in path {
                    if let Selector::Expr(ref e) = seg.val {
                        try!(nary!(e));
                    }
                }
                nary!(e)
            },
            Expr_::List(ref els) => {
                for el in els {
                    try!(nary!(el));
                }
                Ok(())
            },
            Expr_::Let(ref lets, ref e) => {
                // All expressions in a `let` header can see all other expressions.

                for x in lets {
                    unbound.push(x.0, ());
                }
                for x in lets {
                    try!(nary!(&x.1));
                }
                try!(nary!(e));
                for x in lets {
                    unbound.pop(x.0);
                }
                Ok(())
            },
            Expr_::Fn(FnType::BuiltIn(..)) => Ok(()),
            Expr_::Fn(FnType::Normal(ref arg, ref e)) => {
                // Variables in a function definition shadow outer variables in the body.

                match arg.val {
                    FnArg::Ident(id) => unbound.push(id, ()),
                    FnArg::Pat(id, ref args, _) => {
                        for arg in &**args {
                            if let Some(ref alt) = arg.1 {
                                try!(nary!(alt));
                            }
                        }
                        for arg in args { unbound.push(arg.0.val, ()) }
                        if let Some(id) = id { unbound.push(id.val, ()) }
                    },
                }
                try!(nary!(e));
                match arg.val {
                    FnArg::Ident(id) => unbound.pop(id),
                    FnArg::Pat(id, ref args, _) => {
                        for arg in args { unbound.pop(arg.0.val) }
                        if let Some(id) = id { unbound.pop(id.val) }
                    },
                }
                Ok(())
            },
            Expr_::Resolved(..) => Ok(()),
            Expr_::String(..) => Ok(()),
        }
    }

    /// Prints a trace of this expression to its origin.
    ///
    /// = Remarks
    ///
    /// If an expression is a reference to another expression, this function prints all
    /// stations until a datatype expression is encountered. Note that this expression
    /// should not contain a loop, otherwise the process is aborted.
    ///
    /// For example,
    ///
    /// ----
    /// let
    ///     a = 0;
    ///     b = a;
    /// in
    ///     b.x
    /// ----
    ///
    /// will print something like this:
    ///
    /// ----
    /// INPUT:5:4: 5:5 error: expected Set, found Integer
    ///     b.x
    ///     ^
    /// INPUT:3:8: 3:9 note: via
    ///     b = a;
    ///         ^
    /// INPUT:2:8: 2:9 note: via
    ///     a = 0;
    ///         ^
    /// ----
    ///
    /// where both `note` messages were printed by this function.
    pub fn trace(&self, expr: &SExpr) {
        if let Expr_::Resolved(_, ref e) = *expr.val.val.borrow() {
            self.diag.notice(e.span, |mut w| {
                write!(w, "via")
            });
            self.trace(e);
        }
    }

    fn equal_to(&self, left: &SExpr, right: &SExpr) -> Result<bool> {
        let l = try!(self.resolve(left));
        let l = l.val.val.borrow();
        let r = try!(self.resolve(right));
        let r = r.val.val.borrow();
        match (&*l, &*r) {
            (&Expr_::Integer(l), &Expr_::Integer(r)) => Ok(l == r),
            (&Expr_::String(l), &Expr_::String(r)) => Ok(l == r),
            (&Expr_::Null, &Expr_::Null) => Ok(true),
            _ => Ok(false),
        }
    }

    fn eval_bi_apl(&self, apl: &SExpr, func: BuiltInFn, arg: &SExpr) -> Result {
        let res = match func {
            BuiltInFn::ToList => self.bi_to_list(arg),
            BuiltInFn::GetField(name) => self.bi_get_field(name, arg),
            BuiltInFn::TryGetField(name) => self.bi_try_get_field(name, arg),
            BuiltInFn::Assert(cond) => self.bi_assert(cond, arg),
            BuiltInFn::Contains(list) => self.bi_contains(list, arg),
            BuiltInFn::Filter(list) => self.bi_filter(list, arg),
        };
        let res = try!(res);
        *apl.val.val.borrow_mut() = res;
        Ok(())
    }

    fn eval_apl(&self, expr: &SExpr) -> Result {
        let (func, arg) = match *expr.val.val.borrow() {
            Expr_::Apl(ref func, ref arg) => (func.clone(), arg.clone()),
            _ => abort!(),
        };

        let (pat, body) = match try!(self.get_func(&func)) {
            FnType::Normal(pat, body) => (pat, body),
            FnType::BuiltIn(f) => return self.eval_bi_apl(expr, f, &arg),
        };

        let mut bound = Scope::new();
        let mut unbound = PseudoScope::new();

        match pat.val {
            FnArg::Ident(i) => {
                bound.push(i, arg)
            },
            FnArg::Pat(at, fields, wild) => {
                let arg_fields = try!(self.get_fields(&arg));
                if !wild && fields.len() != arg_fields.len() {
                    self.diag.error(arg.span, |mut w| {
                        write!(w, "argument does not match function pattern")
                    });
                    self.trace(&arg);
                    return Err(error::InvalidSequence);
                }
                if let Some(at) = at {
                    bound.push(at.val, arg.clone());
                }
                'outer: for field in &*fields {
                    for arg_field in &arg_fields {
                        if field.0.val == arg_field.0.val {
                            bound.push(arg_field.0.val, arg_field.1.clone());
                            continue 'outer;
                        }
                    }
                    self.diag.error(arg.span, |mut w| {
                        write!(w, "argument misses field `{}`",
                               self.interner.get(field.0.val))
                    });
                    self.trace(&arg);
                    return Err(error::InvalidSequence);
                }
            },
        }

        *expr.val.val.borrow_mut() = match try!(self.subst_args(&body, &mut bound,
                                                                &mut unbound)) {
            Some(e) => {
                try!(self.force(&e));
                e.val.val.borrow().clone()
            },
            _ => {
                try!(self.force(&body));
                Expr_::Resolved(None, body)
            },
        };

        Ok(())
    }

    fn subst_args_path(&self, path: &[Spanned<Selector>], bound: &mut Scope,
                       unbound: &mut PseudoScope) -> Result<Option<Rc<Vec<Spanned<Selector>>>>>
    {
        let mut first = None;
        for (i, seg) in path.iter().enumerate() {
            if let Selector::Expr(ref e) = seg.val {
                if let Some(e) = try!(self.subst_args(e, bound, unbound)) {
                    first = Some((i, Spanned::new(seg.span, Selector::Expr(e))));
                    break;
                }
            }
        }
        if let Some((i, first)) = first {
            let mut npath = Vec::with_capacity(path.len()).unwrap();
            for seg in &path[0..i] {
                npath.push(seg.clone());
            }
            npath.push(first);
            for seg in &path[i+1..] {
                if let Selector::Expr(ref e) = seg.val {
                    if let Some(e) = try!(self.subst_args(e, bound, unbound)) {
                        npath.push(Spanned::new(seg.span, Selector::Expr(e)));
                        continue;
                    }
                }
                npath.push(seg.clone());
            }
            Ok(Some(Rc::new().unwrap().set(npath)))
        } else {
            Ok(None)
        }
    }

    fn subst_args(&self, body: &SExpr, bound: &mut Scope,
                  unbound: &mut PseudoScope) -> Result<Option<SExpr>> {
        macro_rules! nary {
            ($f:expr, $a:expr) => {{
                let na = try!(self.subst_args($a, bound, unbound));
                Some(($f, na))
            }};
            ($f:expr, $a:expr, $b:expr) => {{
                let na = try!(self.subst_args($a, bound, unbound));
                let nb = try!(self.subst_args($b, bound, unbound));
                Some(($f, na, nb, $a, $b))
            }};
        }

        let val = body.val.val.borrow();

        if let Expr_::Ident(id) = *val {
            return if unbound.get(id).is_none() {
                if let Some(e) = bound.get(id) {
                    let expr = Expr_::Resolved(Some(id), e);
                    Ok(Some(Spanned::new(body.span, Expr::new(expr))))
                } else {
                    self.diag.error(body.span , |mut w| {
                        write!(w, "undefined name: `{}`", self.interner.get(id))
                    });
                    Err(error::InvalidSequence)
                }
            } else { 
                Ok(None)
            };
        }

        match *val {
            Expr_::Integer(..) | Expr_::Bool(..) | Expr_::Null | Expr_::Resolved(..) |
                Expr_::String(..) | Expr_::Dummy =>
            {
                return Ok(None);
            }
            _ => { },
        }

        let un: Option<(fn(SExpr) -> Expr_, _)> = match *val {
            Expr_::Not(ref e) => nary!(Expr_::Not, e),
            Expr_::Neg(ref e) => nary!(Expr_::Neg, e),
            Expr_::Stringify(ref e) => nary!(Expr_::Stringify, e),
            _ => None,
        };

        if let Some((f, n)) = un {
            return match n {
                Some(n) => Ok(Some(Spanned::new(body.span, Expr::new(f(n))))),
                None => Ok(None),
            };
        }

        let bin: Option<(fn(SExpr, SExpr) -> Expr_, _, _, _, _)> = match *val {
            Expr_::And(ref l, ref r) => nary!(Expr_::And, l, r),
            Expr_::Or(ref l, ref r) => nary!(Expr_::Or, l, r),
            Expr_::Add(ref l, ref r) => nary!(Expr_::Add, l, r),
            Expr_::Min(ref l, ref r) => nary!(Expr_::Min, l, r),
            Expr_::Mul(ref l, ref r) => nary!(Expr_::Mul, l, r),
            Expr_::Div(ref l, ref r) => nary!(Expr_::Div, l, r),
            Expr_::Mod(ref l, ref r) => nary!(Expr_::Mod, l, r),
            Expr_::Gt(ref l, ref r) => nary!(Expr_::Gt, l, r),
            Expr_::Lt(ref l, ref r) => nary!(Expr_::Lt, l, r),
            Expr_::Ge(ref l, ref r) => nary!(Expr_::Ge, l, r),
            Expr_::Le(ref l, ref r) => nary!(Expr_::Le, l, r),
            Expr_::Eq(ref l, ref r) => nary!(Expr_::Eq, l, r),
            Expr_::Ne(ref l, ref r) => nary!(Expr_::Ne, l, r),
            Expr_::Overlay(ref l, ref r) => nary!(Expr_::Overlay, l, r),
            Expr_::Concat(ref l, ref r) => nary!(Expr_::Concat, l, r),
            Expr_::Apl(ref l, ref r) => nary!(Expr_::Apl, l, r),
            _ => None,
        };

        if let Some((f, nl, nr, l, r)) = bin {
            let (l, r) = match (nl, nr) {
                (Some(nl), Some(nr)) => (nl, nr),
                (Some(nl), None)     => (nl, r.clone()),
                (None,     Some(nr)) => (l.clone(), nr),
                (None,     None)     => return Ok(None),
            };
            return Ok(Some(Spanned::new(body.span, Expr::new(f(l, r)))));
        }

        match *val {
            Expr_::Cond(ref cond, ref then, ref el) => {
                let ncond = try!(self.subst_args(cond, bound, unbound));
                let nthen = try!(self.subst_args(then, bound, unbound));
                let nel   = try!(self.subst_args(el, bound, unbound));
                if ncond.is_none() && nthen.is_none() && nel.is_none() {
                    return Ok(None);
                }
                let cond = ncond.unwrap_or(cond.clone());
                let then = nthen.unwrap_or(then.clone());
                let el = nel.unwrap_or(el.clone());
                Ok(Some(Spanned::new(body.span, Expr::new(Expr_::Cond(cond, then, el)))))
            },
            Expr_::Test(ref e, ref path) => {
                let ne = try!(self.subst_args(e, bound, unbound));
                let npath = try!(self.subst_args_path(path, bound, unbound));

                if ne.is_none() && npath.is_none() {
                    Ok(None)
                } else {
                    let e = ne.unwrap_or_else(|| e.clone());
                    let path = npath.unwrap_or_else(|| path.clone());
                    let ex = Expr_::Test(e, path);
                    Ok(Some(Spanned::new(body.span, Expr::new(ex))))
                }
            },
            Expr_::Select(ref e, ref path, ref a) => {
                let na = match *a {
                    Some(ref a) => try!(self.subst_args(a, bound, unbound)).map(|v| Some(v)),
                    None => None,
                };
                let npath = try!(self.subst_args_path(path, bound, unbound));
                let ne = try!(self.subst_args(e, bound, unbound));

                if ne.is_none() && npath.is_none() && na.is_none() {
                    return Ok(None);
                }

                let e = ne.unwrap_or_else(|| e.clone());
                let a = na.unwrap_or_else(|| a.clone());
                let path = npath.unwrap_or_else(|| path.clone());

                Ok(Some(Spanned::new(body.span, Expr::new(Expr_::Select(e, path, a)))))
            },
            Expr_::Fn(FnType::BuiltIn(..)) => Ok(None),
            Expr_::Fn(FnType::Normal(ref arg, ref e)) => {
                let mut npat = None;

                match arg.val {
                    FnArg::Ident(id) => {
                        unbound.push(id, ())
                    },
                    FnArg::Pat(id, ref args, wild) => {
                        let mut first = None;

                        for (i, arg) in args.iter().enumerate() {
                            if let Some(ref alt) = arg.1 {
                                if let Some(r) = try!(self.subst_args(alt, bound,
                                                                      unbound)) {
                                    first = Some((i, r));
                                    break;
                                }
                            }
                        }

                        if let Some((i, r)) = first {
                            let mut nargs = Vec::with_capacity(args.len()).unwrap();
                            for arg in &args[0..i] {
                                nargs.push((arg.0, arg.1.clone()));
                            }
                            nargs.push((args[i].0, Some(r)));
                            for arg in &args[i+1..] {
                                if let Some(ref alt) = arg.1 {
                                    if let Some(r) = try!(self.subst_args(alt, bound,
                                                                          unbound)) {
                                        nargs.push((arg.0, Some(r)));
                                    } else {
                                        nargs.push((arg.0, Some(alt.clone())));
                                    }
                                } else {
                                    nargs.push((arg.0, None));
                                }
                            }
                            let pat = FnArg::Pat(id, Rc::new().unwrap().set(nargs), wild);
                            npat = Some(Spanned::new(arg.span, pat));
                        }

                        for arg in args { unbound.push(arg.0.val, ()) }
                        if let Some(id) = id { unbound.push(id.val, ()) }
                    },
                }

                let nbody = try!(self.subst_args(e, bound, unbound));

                match arg.val {
                    FnArg::Ident(id) => {
                        unbound.pop(id)
                    },
                    FnArg::Pat(id, ref args, _) => {
                        for arg in args { unbound.pop(arg.0.val) }
                        if let Some(id) = id { unbound.pop(id.val) }
                    },
                }

                let (arg, e) = match (npat, nbody) {
                    (Some(npat), Some(nbody)) => (npat,        nbody),
                    (Some(npat), None)        => (npat,        e.clone()),
                    (None,       Some(nbody)) => (arg.clone(), nbody),
                    (None,       None)        => return Ok(None),
                };

                let func = Expr::new(Expr_::Fn(FnType::Normal(arg, e)));
                Ok(Some(Spanned::new(body.span, func)))
            },
            Expr_::List(ref els) => {
                let mut first = None;

                for (i, el) in els.iter().enumerate() {
                    if let Some(el) = try!(self.subst_args(el, bound, unbound)) {
                        first = Some((i, el));
                        break;
                    }
                }

                if let Some((i, el)) = first {
                    let mut nels = Vec::with_capacity(els.len()).unwrap();
                    for el in &els[0..i] {
                        nels.push(el.clone());
                    }
                    nels.push(el);
                    for el in &els[i+1..] {
                        if let Some(el) = try!(self.subst_args(el, bound, unbound)) {
                            nels.push(el);
                        } else {
                            nels.push(el.clone());
                        }
                    }
                    let list = Expr_::List(Rc::new().unwrap().set(nels));
                    Ok(Some(Spanned::new(body.span, Expr::new(list))))
                } else {
                    Ok(None)
                }
            },
            Expr_::Let(ref lets, ref e) => {
                let mut nlets = None;
                let mut first = None;

                for x in lets {
                    unbound.push(x.0, ());
                }

                for (i, el) in lets.iter().enumerate() {
                    if let Some(el) = try!(self.subst_args(&el.1, bound, unbound)) {
                        first = Some((i, el));
                        break;
                    }
                }

                if let Some((i, el)) = first {
                    let mut tlets = Vec::with_capacity(lets.len()).unwrap();
                    for el in &lets[0..i] {
                        tlets.push(el.clone());
                    }
                    tlets.push((lets[i].0, el));
                    for el in &lets[i+1..] {
                        if let Some(nel) = try!(self.subst_args(&el.1, bound, unbound)) {
                            tlets.push((el.0, nel));
                        } else {
                            tlets.push(el.clone());
                        }
                    }
                    nlets = Some(Rc::new().unwrap().set(tlets));
                }

                let ne = try!(self.subst_args(e, bound, unbound));

                for x in lets {
                    unbound.pop(x.0);
                }

                let (lets, e) = match (nlets, ne) {
                    (Some(nlets), Some(ne)) => (nlets,        ne),
                    (Some(nlets), None)     => (nlets,        e.clone()),
                    (None,        Some(ne)) => (lets.clone(), ne),
                    (None,        None)     => return Ok(None),
                };

                Ok(Some(Spanned::new(body.span, Expr::new(Expr_::Let(lets, e)))))
            },
            Expr_::PreSet(ref exprs, rec) => {
                let mut first = None;

                if rec {
                    for e in exprs {
                        if !e.1.is_dummy() {
                            unbound.push(e.0.val, ());
                        }
                    }
                }

                for (i, ex) in exprs.iter().enumerate() {
                    if ex.1.is_dummy() {
                        if unbound.get(ex.0.val).is_none() {
                            first = Some((i, None));
                            break;
                        }
                    } else if let Some(nex) = try!(self.subst_args(&ex.1, bound,
                                                                   unbound)) {
                        first = Some((i, Some(nex)));
                        break;
                    }
                }

                let res = if let Some((mut i, ex)) = first {
                    let mut nexprs = Vec::with_capacity(exprs.len()).unwrap();
                    for ex in &exprs[0..i] {
                        nexprs.push(ex.clone());
                    }
                    match ex {
                        Some(ex) => nexprs.push((exprs[i].0, ex)),
                        _ => i -= 1,
                    }
                    for ex in &exprs[i+1..] {
                        let e = if ex.1.is_dummy() {
                            if unbound.get(ex.0.val).is_none() {
                                if let Some(e) = bound.get(ex.0.val) {
                                    let e = Expr_::Resolved(Some(ex.0.val), e);
                                    (ex.0, Spanned::new(ex.1.span, Expr::new(e)))
                                } else {
                                    self.diag.error(ex.1.span, |mut w| {
                                        write!(w, "undefined name: `{}`",
                                               self.interner.get(ex.0.val))
                                    });
                                    return Err(error::InvalidSequence);
                                }
                            } else {
                                ex.clone()
                            }
                        } else if let Some(nex) = try!(self.subst_args(&ex.1, bound,
                                                                       unbound)) {
                            (ex.0, nex)
                        } else {
                            ex.clone()
                        };
                        nexprs.push(e);
                    }
                    let set = Expr_::PreSet(Rc::new().unwrap().set(nexprs), rec);
                    Some(Spanned::new(body.span, Expr::new(set)))
                } else {
                    None
                };

                if rec {
                    for e in exprs {
                        if !e.1.is_dummy() {
                            unbound.pop(e.0.val);
                        }
                    }
                }

                Ok(res)
            },
            Expr_::Ident(..) | Expr_::Integer(..) | Expr_::Bool(..) | Expr_::Null |
                Expr_::Set(..) | Expr_::Resolved(..) | Expr_::Not(..) | Expr_::Neg(..) |
                Expr_::And(..) | Expr_::Or(..) | Expr_::Add(..) | Expr_::Min(..) |
                Expr_::Mul(..) | Expr_::Div(..) | Expr_::Mod(..) | Expr_::Gt(..) |
                Expr_::Lt(..) | Expr_::Ge(..) | Expr_::Le(..) | Expr_::Eq(..) |
                Expr_::Ne(..) | Expr_::Overlay(..) | Expr_::Concat(..) | Expr_::Apl(..) |
                Expr_::Stringify(..) | Expr_::String(..) | Expr_::Dummy =>
            {
                abort!();
            }
        }
    }

    /// Evaluates an expression that generates a new expression.
    ///
    /// = Remarks
    ///
    /// This is for (simple) functions which evaluate to new `Expr_` values.
    fn eval_new(&self, expr: &SExpr) -> Result {
        let mut val = expr.val.val.borrow_mut();

        let new = match *val {
            Expr_::And(ref l, ref r) => {
                Expr_::Bool(try!(self.get_bool(l)) && try!(self.get_bool(r)))
            }
            Expr_::Or(ref l, ref r) => {
                Expr_::Bool(try!(self.get_bool(l)) || try!(self.get_bool(r)))
            }
            Expr_::Not(ref e) => {
                Expr_::Bool(!try!(self.get_bool(e)))
            }
            Expr_::Add(ref l, ref r) => {
                Expr_::Integer(try!(self.get_int(l)).wrapping_add(try!(self.get_int(r))))
            }
            Expr_::Min(ref l, ref r) => {
                Expr_::Integer(try!(self.get_int(l)).wrapping_sub(try!(self.get_int(r))))
            }
            Expr_::Mul(ref l, ref r) => {
                Expr_::Integer(try!(self.get_int(l)).wrapping_mul(try!(self.get_int(r))))
            }
            Expr_::Div(ref l, ref r) => {
                let l = try!(self.get_int(l));
                let rn = try!(self.get_int(r));
                if rn == 0 {
                    self.diag.error(r.span , |mut w| {
                        write!(w, "attempted to divide by zero")
                    });
                    return Err(error::InvalidSequence);
                }
                Expr_::Integer(l / rn)
            }
            Expr_::Mod(ref l, ref r) => {
                let l = try!(self.get_int(l));
                let rn = try!(self.get_int(r));
                if rn == 0 {
                    self.diag.error(r.span , |mut w| {
                        write!(w, "attempted to divide by zero")
                    });
                    return Err(error::InvalidSequence);
                }
                Expr_::Integer(l % rn)
            }
            Expr_::Gt(ref l, ref r) => {
                Expr_::Bool(try!(self.get_int(l)) > try!(self.get_int(r)))
            },
            Expr_::Lt(ref l, ref r) => {
                Expr_::Bool(try!(self.get_int(l)) < try!(self.get_int(r)))
            },
            Expr_::Ge(ref l, ref r) => {
                Expr_::Bool(try!(self.get_int(l)) >= try!(self.get_int(r)))
            },
            Expr_::Le(ref l, ref r) => {
                Expr_::Bool(try!(self.get_int(l)) <=
                            try!(self.get_int(r)))
            },
            Expr_::Neg(ref e) => {
                Expr_::Integer(0i64.wrapping_sub(try!(self.get_int(e))))
            },
            Expr_::Concat(ref l, ref r) => {
                let l = try!(self.resolve(l));
                let left = l.val.val.borrow();
                match *left {
                    Expr_::String(left) => {
                        let right = try!(self.get_string(r));
                        let new = self.interner.concat(left, right);
                        Expr_::String(new)
                    },
                    Expr_::List(ref left) => {
                        let right = try!(self.get_list(r));
                        if right.len() == 0 {
                            Expr_::List(left.clone())
                        } else {
                            let mut left = left.maybe_clone().unwrap();
                            left.reserve(right.len());
                            for el in &right { left.push(el.clone()); }
                            Expr_::List(Rc::new().unwrap().set(left))
                        }
                    },
                    _ => {
                        self.diag.error(l.span , |mut w| {
                            write!(w, "expected String or List, found {:?}", l.val)
                        });
                        self.trace(&l);
                        return Err(error::InvalidSequence);
                    },
                }
            },
            Expr_::Test(ref s, ref path) => {
                let mut set = s.clone();
                let mut path = &path[..];
                let mut err_span = set.span;
                while path.len() > 0 {
                    set = match try!(self.get_opt_field(&set, &path[0].val, None)) {
                        Some(f) => f,
                        None => break,
                    };
                    err_span.hi = path[0].span.hi;
                    path = &path[1..];
                }
                Expr_::Bool(path.len() == 0)
            },
            Expr_::Eq(ref l, ref r) => {
                Expr_::Bool(try!(self.equal_to(r, l)))
            },
            Expr_::Ne(ref l, ref r) => {
                Expr_::Bool(!try!(self.equal_to(r, l)))
            },
            // Expr_::Overlay(ref bottom, ref top) => {
            //     let lset = try!(self.get_fields(bottom));
            //     let rset = try!(self.get_fields(top));
            //     if rset.len() == 0 {
            //         Expr_::Set(lset)
            //     } else {
            //         let mut lset = lset.maybe_clone().unwrap();
            //         'outer: for r in &rset {
            //             for l in &mut lset {
            //                 if l.0.val == r.0.val {
            //                     l.1 = r.1.clone();
            //                     continue 'outer;
            //                 }
            //             }
            //             lset.push((r.0, r.1.clone()));
            //         }
            //         Expr_::Set(Rc::new().unwrap().set(lset))
            //     }
            // },
            _ => abort!(),
        };

        *val = new;

        Ok(())
    }

    /// Evaluates a selection expression.
    ///
    /// = Remarks
    ///
    /// A selection expression is an expression of the form
    ///
    /// ----
    /// a.b or c
    /// ----
    ///
    /// where the `or c` part is optional and `b` can consist of multiple identifiers.
    ///
    /// If this is not a selection expression, the process is aborted.
    ///
    /// After this function returns successfully, the expression has been replaced by a
    /// reference to the selected expression. If the path does not exist and an
    /// alternative expression has been provided (`or c`), a reference to that expression
    /// is stored. If the path does not exist and no alternative has been provided, an
    /// error is printed and an error is returned.
    ///
    /// Note that all expressions which are selected on have to be sets or an error is
    /// printed, even if an alternative is provided.
    ///
    /// ----
    /// { a = 0 }.a               # evaluates to 0
    /// { a = { b = 0 } }.a.b     # evaluates to 0
    /// { }.a or 0                # evaluates to 0
    /// 1.a or 0                  # prints an error
    /// ----
    fn eval_select(&self, expr: &SExpr) -> Result {
        let res = {
            let val = expr.val.val.borrow();

            let (mut set, path, alt) = match *val {
                Expr_::Select(ref s, ref p, ref a) => (s.clone(), p, a.clone()),
                _ => abort!(),
            };

            // The span we will use to print error messages.
            let mut err_span = set.span;
            let mut path = &path[..];
            let mut bad_path = Selector::Integer(0);

            while path.len() > 0 {
                set = match try!(self.get_opt_field(&set, &path[0].val,
                                                    Some(&mut bad_path))) {
                    Some(f) => f,
                    _ => break,
                };
                err_span.hi = path[0].span.hi;
                path = &path[1..];
            }

            if path.len() > 0 {
                if let Some(alt) = alt {
                    try!(self.force(&alt));
                    alt
                } else {
                    self.diag.error(err_span , |mut w| {
                        match bad_path {
                            Selector::Ident(i) => {
                                write!(w, "set has no field `{}`", self.interner.get(i))
                            },
                            Selector::Integer(i) => {
                                write!(w, "list has no field `{}`", i)
                            },
                            _ => abort!(),
                        }
                    });
                    self.diag.notice(expr.span, |mut w| { write!(w, "via") });
                    self.trace(&expr);
                    return Err(error::InvalidSequence);
                }
            } else {
                try!(self.force(&set));
                set
            }
        };

        *expr.val.val.borrow_mut() = Expr_::Resolved(None, res);

        Ok(())
    }

    /// Evaluates an expression that generates a reference to another expression.
    ///
    /// = Remarks
    ///
    /// This is for (simple) functions which evaluate to `Resolved`. This function should
    /// only be called with the `Let` and `Cond` variants or the process will be aborted.
    fn eval_ref(&self, expr: &SExpr) -> Result {
        let mut val = expr.val.val.borrow_mut();

        let new = match *val {
            Expr_::Let(ref names, ref e) => {
                let mut bound = Scope::new();
                let mut unbound = PseudoScope::new();
                for n in names {
                    bound.push(n.0, n.1.clone());
                }
                for n in names {
                    try!(self.resolve_names_(&n.1, &bound, &mut unbound));
                }
                try!(self.resolve_names_(e, &bound, &mut unbound));
                try!(self.force(e));
                e.clone()
            },
            Expr_::Cond(ref cond, ref then, ref el) => {
                if try!(self.get_bool(cond)) {
                    try!(self.force(then));
                    then.clone()
                } else {
                    try!(self.force(el));
                    el.clone()
                }
            },
            _ => abort!(),
        };

        *val = Expr_::Resolved(None, new);

        Ok(())
    }

    fn eval_stringify(&self, expr: &SExpr) -> Result {
        let mut val = expr.val.val.borrow_mut();
        let e = match *val {
            Expr_::Stringify(ref e) => e.clone(),
            _ => abort!(),
        };
        let dst = try!(self.resolve(&e));
        let dst = dst.val.val.borrow();
        match *dst {
            Expr_::String(..) => *val = Expr_::Resolved(None, e),
            Expr_::Integer(v) => {
                let s = format!("{}", v).unwrap();
                let id = self.interner.insert(s);
                *val = Expr_::String(id);
            },
            _ => {
                self.diag.error(e.span , |mut w| {
                    write!(w, "cannot stringify this expression: {:?}", e.val)
                });
                self.trace(&e);
                return Err(error::InvalidSequence);
            },
        }
        Ok(())
    }

    fn eval_pre_set(&self, set: &SExpr) -> Result {
        let mut val = set.val.val.borrow_mut();
        let (fields, rec) = match *val {
            Expr_::PreSet(ref fields, rec) => (fields.clone(), rec),
            _ => abort!(),
        };

        let mut bound = Scope::new();
        let mut unbound = PseudoScope::new();

        for field in &fields {
            if field.1.is_dummy() {
                self.diag.error(field.0.span , |mut w| {
                    write!(w, "undefined name: `{}`", self.interner.get(field.0.val))
                });
                return Err(error::InvalidSequence);
            }
            if rec {
                bound.push(field.0.val, field.1.clone());
            }
        }

        for field in &fields {
            try!(self.resolve_names_(&field.1, &bound, &mut unbound))
        }
        *val = Expr_::Set(fields);
        Ok(())
    }

    /// Forces (evaluates) the expression.
    ///
    /// [argument, span]
    /// The span of the expression.
    ///
    /// = Remarks
    ///
    /// `val` should not be borrowed when this function is invoked. Otherwise this will be
    /// interpreted as illegal infinite recursion, an error will be printed, and an error
    /// will be returned.
    ///
    /// This function modifies existing expressions.
    ///
    /// After this function returns successfully, the expression has been replaced by a
    /// datatype expression or a reference to a datatype expression. If this expression is
    /// already a datatype expression, no operation is performed.
    ///
    /// Datatype expressions are:
    ///
    /// * Integer
    /// * Ident
    /// * Set
    /// * Bool
    /// * Null
    /// * List
    /// * Fn
    ///
    /// This evaluation occurs top-to-bottom and stops once we have created datatype
    /// expression. This means that the following expression will be evaluated to itself
    /// (is a no-op):
    ///
    /// ----
    /// rec { a = b, b = a }
    /// ----
    ///
    /// even though the fields themselves could not be evaluated. Similarly, the following
    /// will evaluate to `0`:
    ///
    /// ----
    /// rec { a = b, b = a, c = 0 }.c
    /// ----
    pub fn force(&self, expr: &SExpr) -> Result {
        // self.diag.notice(expr.span , |mut w| {
        //     write!(w, "forcing")
        // });

        let borrow = expr.val.val.borrow();

        match *borrow {
            Expr_::Integer(..) | Expr_::Ident(..) | Expr_::Bool(..) |
                Expr_::Null | Expr_::List(..) | Expr_::Fn(..) | Expr_::String(..) |
                Expr_::Set(..) | Expr_::Overlay(..) =>
            {
                return Ok(())
            },
            _ => { },
        }

        if expr.val.val.status() != RefCellStatus::Borrowed(0) {
            self.diag.error(expr.span , |mut w| {
                write!(w, "infinite recursion")
            });
            return Err(error::InvalidSequence);
        }

        // This has to be handled separately because `ref dst` borrows `borrow`. In the
        // cases below be have to drop `borrow` and this would not be possible with this
        // branch in it.
        if let Expr_::Resolved(_, ref dst) = *borrow {
            return self.force(dst);
        }

        match *borrow {
            Expr_::Integer(..) | Expr_::Ident(..) | Expr_::Bool(..) |
                Expr_::Null | Expr_::List(..) | Expr_::Fn(..) | Expr_::String(..) |
                Expr_::Set(..) | Expr_::Overlay(..) =>
            {
                abort!();
            },
            Expr_::And(..) | Expr_::Or(..)  | Expr_::Not(..) | Expr_::Add(..) |
                Expr_::Min(..) | Expr_::Mul(..) | Expr_::Div(..) | Expr_::Mod(..) |
                Expr_::Gt(..) | Expr_::Lt(..) | Expr_::Ge(..) | Expr_::Le(..) |
                Expr_::Neg(..) | Expr_::Test(..) | Expr_::Eq(..) |
                Expr_::Ne(..) | Expr_::Concat(..)  =>
            {
                drop(borrow);
                self.eval_new(expr)
            }
            Expr_::Select(..) => {
                drop(borrow);
                self.eval_select(expr)
            }
            Expr_::Apl(..) => {
                drop(borrow);
                self.eval_apl(expr)
            }
            Expr_::Let(..) | Expr_::Cond(..) => {
                drop(borrow);
                self.eval_ref(expr)
            }
            Expr_::Stringify(..) => {
                drop(borrow);
                self.eval_stringify(expr)
            }
            Expr_::PreSet(..) => {
                drop(borrow);
                self.eval_pre_set(expr)
            }
            Expr_::Resolved(..) | Expr_::Dummy => {
                // `Resolved` has already been handled above.
                abort!()
            },
        }
    }

    /// Forces the expression and returns the forced datatype expression.
    ///
    /// [argument, span]
    /// The span of the expression.
    ///
    /// = Remarks
    ///
    /// After forcing, this expression can be a reference to a datatype expression. This
    /// function recursively resolves these references and returns the datatype
    /// expression.
    pub fn resolve(&self, expr: &SExpr) -> Result<SExpr> {
        // We don't have to check for infinite recursion here since `force` already does
        // that.

        try!(self.force(expr));

        let mut expr = expr.clone();
        loop {
            let tmp = match *expr.val.val.borrow() {
                Expr_::Resolved(_, ref e) => e.clone(),
                _ => break,
            };
            expr = tmp;
        }
        Ok(expr)
    }

    /// Forces the expression and tries to interpret the created datatype expression as a
    /// boolean.
    ///
    /// [argument, span]
    /// The span of the expression.
    ///
    /// = Remarks
    ///
    /// If the datatype expression is not a boolean, an error message is printed and an
    /// error is returned.
    pub fn get_bool(&self, expr: &SExpr) -> Result<bool> {
        let res = try!(self.resolve(expr));
        let val = res.val.val.borrow();
        match *val {
            Expr_::Bool(b) => Ok(b),
            _ => {
                self.diag.error(expr.span , |mut w| {
                    write!(w, "expected Bool, found {:?}", res.val)
                });
                self.trace(expr);
                Err(error::InvalidSequence)
            },
        }
    }

    pub fn get_string(&self, expr: &SExpr) -> Result<Interned> {
        let res = try!(self.resolve(expr));
        let val = res.val.val.borrow();
        match *val {
            Expr_::String(s) => Ok(s),
            _ => {
                self.diag.error(expr.span , |mut w| {
                    write!(w, "expected String, found {:?}", res.val)
                });
                self.trace(expr);
                Err(error::InvalidSequence)
            },
        }
    }

    /// Forces the expression and tries to interpret the created datatype expression as an
    /// integer.
    ///
    /// [argument, span]
    /// The span of the expression.
    ///
    /// = Remarks
    ///
    /// If the datatype expression is not an integer, an error message is printed and an
    /// error is returned.
    pub fn get_int(&self, expr: &SExpr) -> Result<i64> {
        let res = try!(self.resolve(expr));
        let val = res.val.val.borrow();
        match *val {
            Expr_::Integer(i) => Ok(i),
            _ => {
                self.diag.error(expr.span , |mut w| {
                    write!(w, "expected Integer, found {:?}", res.val)
                });
                self.trace(expr);
                Err(error::InvalidSequence)
            },
        }
    }

    /// Forces the expression and tries to interpret the created datatype expression as a
    /// list.
    ///
    /// [argument, span]
    /// The span of the expression.
    ///
    /// = Remarks
    ///
    /// If the datatype expression is not a list, an error message is printed and an error
    /// is returned.
    pub fn get_list(&self, expr: &SExpr) -> Result<Rc<Vec<SExpr>>> {
        let res = try!(self.resolve(expr));
        let val = res.val.val.borrow();
        match *val {
            Expr_::List(ref l) => Ok(l.clone()),
            _ => {
                self.diag.error(expr.span , |mut w| {
                    write!(w, "expected List, found {:?}", res.val)
                });
                self.trace(expr);
                Err(error::InvalidSequence)
            },
        }
    }

    /// Forces the expression, tries to interpret the created datatype expression as a
    /// set, or overlay, and tries to retrieve a field.
    ///
    /// [argument, name]
    /// The name of the field to retrieve.
    ///
    /// [argument, span]
    /// The span of the expression.
    ///
    /// = Remarks
    ///
    /// If the datatype expression is not a set or an overlay, an error message is printed
    /// and an error is returned. If the set does not contain the field, `None` is
    /// returned but no error is printed.
    pub fn get_opt_field(&self, expr: &SExpr, selector: &Selector,
                         out: Option<&mut Selector>) -> Result<Option<SExpr>> {
        // Note: If we wanted to force the field in this function, we
        // would first have to drop the borrow since the field might refer
        // to this set. For example
        //
        // ----
        // let
        //     a = { x = a.y, y = 0 };
        // in
        //     a.x
        // ----

        let sel = match *selector {
            Selector::Expr(ref e) => {
                let res = try!(self.resolve(e));
                let res = res.val.val.borrow();
                match *res {
                    Expr_::String(s) => {
                        Selector::Ident(s)
                    },
                    Expr_::Integer(i) => {
                        if (isize::max() as i64) < i {
                            self.diag.error(e.span , |mut w| {
                                write!(w, "out of bounds")
                            });
                            self.trace(e);
                            return Err(error::InvalidSequence);
                        }
                        Selector::Integer(i as usize)
                    },
                    _ => {
                        self.diag.error(e.span , |mut w| {
                            write!(w, "expected Integer or String, found {:?}", &*res)
                        });
                        self.trace(e);
                        return Err(error::InvalidSequence);
                    },
                }
            },
            _ => selector.clone(),
        };

        self.get_field_(expr, &sel, out)
    }

    pub fn get_field(&self, expr: &SExpr, selector: &Selector,
                     out: Option<&mut Selector>) -> Result<SExpr> {
        let mut eval_sel = Selector::Integer(0);
        let field = try!(self.get_opt_field(expr, selector, Some(&mut eval_sel)));

        if let Some(f) = field {
            return Ok(f);
        }

        out.map(|o| *o = eval_sel.clone());

        self.diag.error(expr.span , |mut w| {
            match eval_sel {
                Selector::Ident(i) => {
                    write!(w, "set has no field `{}`", self.interner.get(i))
                },
                Selector::Integer(i) => {
                    write!(w, "list has no field `{}`", i)
                },
                _ => abort!(),
            }
        });
        self.diag.notice(expr.span, |mut w| { write!(w, "via") });
        self.trace(&expr);

        Err(error::InvalidSequence)
    }

    /// Like `get_field` but expects the selector to not be in expression form.
    fn get_field_(&self, expr: &SExpr, sel: &Selector,
                  out: Option<&mut Selector>) -> Result<Option<SExpr>> {
        let res = try!(self.resolve(expr));
        let val = res.val.val.borrow();

        match (&*val, sel) {
            (&Expr_::Set(ref fields), &Selector::Ident(name)) => {
                for f in fields {
                    if f.0.val == name {
                        return Ok(Some(f.1.clone()));
                    }
                }
                out.map(|o| *o = Selector::Ident(name));
                Ok(None)
            },
            (&Expr_::Overlay(ref bottom, ref top), &Selector::Ident(name)) => {
                try!(self.force(top));
                if let Some(s) = try!(self.get_field_(top, sel, None)) {
                    return Ok(Some(s));
                }
                try!(self.force(bottom));
                if let Some(s) = try!(self.get_field_(bottom, sel, None)) {
                    return Ok(Some(s));
                }
                out.map(|o| *o = Selector::Ident(name));
                Ok(None)
            },
            (&Expr_::List(ref fields), &Selector::Integer(i)) => {
                if fields.len() > i {
                    Ok(Some(fields[i].clone()))
                } else {
                    out.map(|o| *o = Selector::Integer(i));
                    Ok(None)
                }
            },
            _ => {
                self.diag.error(expr.span , |mut w| {
                    if let &Selector::Ident(..) = sel {
                        write!(w, "expected Set, found {:?}", expr.val)
                    } else {
                        write!(w, "expected List, found {:?}", expr.val)
                    }
                });
                self.trace(expr);
                Err(error::InvalidSequence)
            },
        }
    }

    /// Forces the expression, tries to interpret the created datatype expression as a
    /// set, and returns all of its fields.
    ///
    /// [argument, span]
    /// The span of the expression.
    ///
    /// = Remarks
    ///
    /// If the datatype expression is not a set, an error message is printed and an error
    /// is returned.
    pub fn get_fields(&self,
                      expr: &SExpr) -> Result<Rc<Vec<(Spanned<Interned>, SExpr)>>> {
        let res = try!(self.resolve(expr));
        let val = res.val.val.borrow();
        match *val {
            Expr_::Set(ref fields) => Ok(fields.clone()),
            _ => {
                self.diag.error(expr.span , |mut w| {
                    write!(w, "expected Set, found {:?}", res.val)
                });
                self.trace(expr);
                Err(error::InvalidSequence)
            },
        }
    }

    /// Forces the expression, tries to interpret the created datatype expression as a
    /// function, and returns its pattern and body.
    ///
    /// [argument, span]
    /// The span of the expression.
    ///
    /// = Remarks
    ///
    /// If the datatype expression is not a function, an error message is printed and an
    /// error is returned.
    pub fn get_func(&self, expr: &SExpr) -> Result<FnType> {
        let res = try!(self.resolve(expr));
        let val = res.val.val.borrow();
        match *val {
            Expr_::Fn(ref f) => Ok(f.clone()),
            _ => {
                self.diag.error(expr.span , |mut w| {
                    write!(w, "expected Function, found {:?}", res.val)
                });
                self.trace(expr);
                Err(error::InvalidSequence)
            },
        }
    }
}
