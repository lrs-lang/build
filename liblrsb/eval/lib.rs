// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

#![crate_name = "lrsb_eval"]
#![crate_type = "lib"]
#![feature(default_type_parameter_fallback)]

extern crate lrsb_types as types;

use std::rc::{Rc};
use std::hashmap::{HashMap};

use types::diagnostic::{Diagnostic, Notice};
use types::interner::{Interner};
use types::scope::{Scope};
use types::span::{Spanned};
use types::tree::{SExpr, Expr_, FnArg, Selector, FnType, Expr};

fn f(x: &SExpr) {
    let y = x.to();
    y.span;
}

fn g(x: &SExpr) {
    x.to();
}

fn h(x: &SExpr) {
    let y: SExpr = x.to();
    y.is_inherit();
}

// mod force;
// mod get;
// 
// pub struct Eval<D: Diagnostic> {
//     pub diag: Rc<D>,
//     pub interner: Rc<Interner>,
// }
// 
// impl<D: Diagnostic> Eval<D> {
//     pub fn new(diag: Rc<D>, interner: Rc<Interner>) -> Eval<D> {
//         Eval {
//             diag: diag,
//             interner: interner,
//         }
//     }
// 
//     /// Prints a trace of this expression to its origin.
//     ///
//     /// = Remarks
//     ///
//     /// If an expression is a reference to another expression, this function prints all
//     /// stations until a datatype expression is encountered. Note that this expression
//     /// should not contain a loop, otherwise the process is aborted.
//     ///
//     /// For example,
//     ///
//     /// ----
//     /// let
//     ///     a = 0;
//     ///     b = a;
//     /// in
//     ///     b.x
//     /// ----
//     ///
//     /// will print something like this:
//     ///
//     /// ----
//     /// INPUT:5:4: 5:5 error: expected Set, found Integer
//     ///     b.x
//     ///     ^
//     /// INPUT:3:8: 3:9 note: via
//     ///     b = a;
//     ///         ^
//     /// INPUT:2:8: 2:9 note: via
//     ///     a = 0;
//     ///         ^
//     /// ----
//     ///
//     /// where both `note` messages were printed by this function.
//     pub fn trace(&self, expr: &SExpr) {
//         if let Expr_::Resolved(_, ref e) = *expr.val.val.borrow() {
//             self.diag.notice(e.span, Notice::Via);
//             self.trace(e);
//         }
//     }
// 
//     pub fn equal_to(&self, left: &SExpr, right: &SExpr) -> Result<bool> {
//         let l = try!(self.resolve(left));
//         let l = l.val.val.borrow();
//         let r = try!(self.resolve(right));
//         let r = r.val.val.borrow();
//         match (&*l, &*r) {
//             (&Expr_::Integer(l), &Expr_::Integer(r)) => Ok(l == r),
//             (&Expr_::String(l), &Expr_::String(r)) => Ok(l == r),
//             (&Expr_::Null, &Expr_::Null) => Ok(true),
//             _ => Ok(false),
//         }
//     }
// 
//     /// Forces the expression and returns the forced datatype expression.
//     ///
//     /// [argument, span]
//     /// The span of the expression.
//     ///
//     /// = Remarks
//     ///
//     /// After forcing, this expression can be a reference to a datatype expression. This
//     /// function recursively resolves these references and returns the datatype
//     /// expression.
//     pub fn resolve(&self, expr: &SExpr) -> Result<SExpr> {
//         try!(self.force(expr));
// 
//         let mut expr = expr.to();
//         loop {
//             let tmp = match *expr.val.val.borrow() {
//                 Expr_::Resolved(_, ref e) => e.to(),
//                 _ => break,
//             };
//             expr = tmp;
//         }
//         Ok(expr)
//     }
// 
//     pub fn deep_copy(&self, target: &SExpr, scope: &mut Scope<SExpr>) -> Result<SExpr> {
//         macro_rules! copy {
//             ($e:expr) => {
//                 try!(self.deep_copy($e, scope))
//             }
//         }
//         macro_rules! new {
//             ($e:expr) => {
//                 Expr::spanned(target.span, $e)
//             }
//         }
// 
//         let val = target.val.val.borrow();
// 
//         // Unary operators
//         {
//             macro_rules! un {
//                 ($f:expr, $a:expr) => {
//                     Some(($f, $a))
//                 };
//             }
// 
//             let un: Option<(fn(_) -> _, _)> = match *val {
//                 Expr_::Not(ref e) => un!(Expr_::Not, e),
//                 Expr_::Neg(ref e) => un!(Expr_::Neg, e),
//                 Expr_::Stringify(ref e) => un!(Expr_::Stringify, e),
//                 _ => None,
//             };
// 
//             if let Some((f, n)) = un {
//                 return new!(f((copy!(n))));
//             }
//         }
// 
//         // Binary operators
//         {
//             macro_rules! bin {
//                 ($f:expr, $a:expr, $b:expr) => {
//                     Some(($f, $a, $b))
//                 };
//             }
// 
//             let bin: Option<(fn(_, _) -> _, _, _)> = match *val {
//                 Expr_::And(ref l, ref r) => bin!(Expr_::And, l, r),
//                 Expr_::Or(ref l, ref r) => bin!(Expr_::Or, l, r),
//                 Expr_::Add(ref l, ref r) => bin!(Expr_::Add, l, r),
//                 Expr_::Sub(ref l, ref r) => bin!(Expr_::Sub, l, r),
//                 Expr_::Mul(ref l, ref r) => bin!(Expr_::Mul, l, r),
//                 Expr_::Div(ref l, ref r) => bin!(Expr_::Div, l, r),
//                 Expr_::Mod(ref l, ref r) => bin!(Expr_::Mod, l, r),
//                 Expr_::Gt(ref l, ref r) => bin!(Expr_::Gt, l, r),
//                 Expr_::Lt(ref l, ref r) => bin!(Expr_::Lt, l, r),
//                 Expr_::Ge(ref l, ref r) => bin!(Expr_::Ge, l, r),
//                 Expr_::Le(ref l, ref r) => bin!(Expr_::Le, l, r),
//                 Expr_::Eq(ref l, ref r) => bin!(Expr_::Eq, l, r),
//                 Expr_::Ne(ref l, ref r) => bin!(Expr_::Ne, l, r),
//                 Expr_::Impl(ref l, ref r) => bin!(Expr_::Impl, l, r),
//                 Expr_::Overlay(ref l, ref r) => bin!(Expr_::Overlay, l, r),
//                 Expr_::Concat(ref l, ref r) => bin!(Expr_::Concat, l, r),
//                 Expr_::Apl(ref l, ref r) => bin!(Expr_::Apl, l, r),
//                 Expr_::Test(ref l, ref r) => bin!(Expr_::Test, l, r),
//                 _ => None,
//             };
// 
//             if let Some((f, l, r)) = bin {
//                 return new!(f(copy!(l), copy!(r)));
//             }
//         }
// 
//         // Ternary operators
//         {
//             macro_rules! tern {
//                 ($f:expr, $a:expr, $b:expr, $c:expr) => {
//                     Some(($f, $a, $b, $c))
//                 };
//             }
// 
//             let tern: Option<(fn(_, _, _) -> _, _, _, _)> = match *val {
//                 Expr_::Cond(ref a, ref b, ref c) => tern!(Expr_::Cond, a, b, c),
//                 _ => None,
//             };
// 
//             if let Some((f, a, b, c)) = tern {
//                 return new!(f(copy!(a), copy!(b), copy!(c)));
//             }
//         }
// 
//         // Everything else
//         let rv = match *val {
//             Expr_::Not(..) | Expr_::Neg(..) | Expr_::Stringify(..) | Expr_::And(..) |
//                 Expr_::Or(..) | Expr_::Add(..) | Expr_::Sub(..) | Expr_::Mul(..) |
//                 Expr_::Div(..) | Expr_::Mod(..) | Expr_::Gt(..) | Expr_::Lt(..) |
//                 Expr_::Ge(..) | Expr_::Le(..) | Expr_::Eq(..) | Expr_::Ne(..) |
//                 Expr_::Impl(..) | Expr_::Overlay(..) | Expr_::Concat(..) |
//                 Expr_::Apl(..) | Expr_::Test(..) | Expr_::Cond(..) =>
//             {
//                 // handled above
//                 abort!();
//             },
//             Expr_::Inherit => Expr_::Inherit,
//             Expr_::String(s) => Expr_::String(s),
//             Expr_::Integer(i) => Expr_::Integer(i),
//             Expr_::Bool(b) => Expr_::Bool(b),
//             Expr_::Null => Expr_::Null,
//             Expr_::Resolved(id, ref dst) => Expr_::Resolved(id, dst.to()),
//             Expr_::Fn(FnType::BuiltIn(ref f)) => Expr_::Fn(FnType::BuiltIn(f.to())),
//             Expr_::Ident(id) =>
//             {
//                 match scope.get(id) {
//                     Some(val) => Expr_::Resolved(Some(id), val),
//                     _ => Expr_::Ident(id),
//                 }
//             },
//             Expr_::List(ref els) =>
//             {
//                 let mut nels = try!(Vec::with_capacity(els.len()));
//                 for el in &*els {
//                     nels.push(copy!(el));
//                 }
//                 Expr_::List(try!(Rc::new()).set(nels))
//             },
//             Expr_::Set(ref fields, rec) =>
//             {
//                 let mut nfields = try!(HashMap::with_capacity(fields.size()));
// 
//                 if rec {
//                     for (&id, &(_, ref val)) in fields {
//                         if !val.is_inherit() {
//                             scope.hide(id);
//                         }
//                     }
//                 }
// 
//                 for (&id, &(span, ref val)) in fields {
//                     let nfield = copy!(val);
//                     if nfield.is_inherit() {
//                         if let Some(res) = scope.get(id) {
//                             *nfield.val.val.borrow_mut() = Expr_::Resolved(Some(id), res);
//                         }
//                     }
//                     nfields.set(id, (span, nfield));
//                 }
// 
//                 if rec {
//                     for (&id, &(_, ref val)) in fields {
//                         if !val.is_inherit() {
//                             scope.pop(id);
//                         }
//                     }
//                 }
// 
//                 Expr_::Set(try!(Rc::new()).set(nfields), rec)
//             }
//             Expr_::Let(ref fields, ref body) => {
//                 let mut nfields = try!(HashMap::with_capacity(fields.size()));
// 
//                 for (&id, _) in fields {
//                     scope.hide(id);
//                 }
// 
//                 for (&id, &(span, ref val)) in fields {
//                     nfields.set(id, (span, copy!(val)));
//                 }
//                 let nbody = copy!(body);
// 
//                 for (&id, _) in fields {
//                     scope.pop(id);
//                 }
// 
//                 Expr_::Let(try!(Rc::new()).set(nfields), nbody)
//             }
//             Expr_::Path(ref segs) =>
//             {
//                 let mut nsegs = try!(Vec::with_capacity(segs.len()));
// 
//                 for seg in &*segs {
//                     nsegs.push(copy!(seg));
//                 }
// 
//                 Expr_::Path(try!(Rc::new()).set(nsegs))
//             }
//             Expr_::Selector(ref s) =>
//             {
//                 if let Selector::Expr(ref e) = *s {
//                     Expr_::Selector(Selector::Expr(copy!(e)))
//                 } else {
//                     Expr_::Selector(s.to())
//                 }
//             }
//             Expr_::Select(ref target, ref segs, ref alt) =>
//             {
//                 let ntarget = copy!(target);
//                 let nsegs = copy!(segs);
//                 let nalt = match *alt {
//                     Some(ref alt) => Some(copy!(alt)),
//                     _ => None,
//                 };
//                 Expr_::Select(ntarget, nsegs, nalt)
//             }
//             Expr_::Fn(FnType::Normal(Spanned { span, val: FnArg::Ident(id) }, ref body)) =>
//             {
//                 scope.hide(id);
//                 let nbody = copy!(body);
//                 scope.pop(id);
// 
//                 Expr_::Fn(FnType::Normal(Spanned::new(span, FnArg::Ident(id)), nbody))
//             },
//             Expr_::Fn(FnType::Normal(Spanned{span, val: FnArg::Pat(id, ref fields, wild)},
//                                      ref body)) =>
//             {
//                 let mut nfields = try!(HashMap::with_capacity(fields.size()));
//                 for (&id, &(span, ref alt)) in &*fields {
//                     nfields.set(id, match *alt {
//                         Some(ref alt) => (span, Some(copy!(alt))),
//                         _ => (span, None),
//                     });
//                 }
// 
//                 for (&id, _) in &*fields {
//                     scope.hide(id);
//                 }
//                 if let Some(id) = id {
//                     scope.hide(id.val);
//                 }
//                 let nbody = copy!(body);
//                 for (&id, _) in &*fields {
//                     scope.pop(id);
//                 }
//                 if let Some(id) = id {
//                     scope.pop(id.val);
//                 }
// 
//                 let pat = FnArg::Pat(id, try!(Rc::new()).set(nfields), wild);
//                 Expr_::Fn(FnType::Normal(Spanned::new(span, pat), nbody))
//             }
//         };
// 
//         Expr::spanned(target.span, rv)
//     }
// }
