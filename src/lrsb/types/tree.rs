// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

use std::rc::{Rc};
use std::share::{RefCell};
use std::fmt::{Debug, Write};
use std::clone::{MaybeClone};

use interner::{Interned};
use span::{Spanned};

pub type SExpr = Spanned<Expr>;

/// A reference-counted expression with interior mutability.
///
/// = Remarks
///
/// After an expression has been parsed, you first have to resolve the identifiers before
/// the expression can be evaluated. To do this, call `resolve_names`.
#[derive(Clone)]
pub struct Expr {
    pub val: Rc<RefCell<Expr_>>,
}

impl Debug for Expr {
    fn fmt<W: Write>(&self, w: &mut W) -> Result {
        self.val.borrow().fmt(w)
    }
}

impl MaybeClone for Expr {
    fn maybe_clone(&self) -> Result<Expr> {
        Ok(Expr { val: self.val.clone() })
    }
}

impl Expr {
    /// Creates a new reference-counted expression from an expression.
    pub fn new(expr: Expr_) -> Expr {
        Expr {
            val: Rc::new().unwrap().set(RefCell::new(expr)),
        }
    }

    pub fn is_dummy(&self) -> bool {
        match *self.val.borrow() {
            Expr_::Dummy => true,
            _ => false,
        }
    }
}

#[derive(Clone)]
pub enum Expr_ {
    Dummy,
    String(Interned),
    Integer(i64),
    Ident(Interned),
    PreSet(Rc<Vec<(Spanned<Interned>, SExpr)>>, bool),
    Set(Rc<Vec<(Spanned<Interned>, SExpr)>>),
    And(SExpr, SExpr),
    Or(SExpr, SExpr),
    Not(SExpr),
    Add(SExpr, SExpr),
    Min(SExpr, SExpr),
    Mul(SExpr, SExpr),
    Div(SExpr, SExpr),
    Mod(SExpr, SExpr),
    Gt(SExpr, SExpr),
    Lt(SExpr, SExpr),
    Ge(SExpr, SExpr),
    Le(SExpr, SExpr),
    Eq(SExpr, SExpr),
    Ne(SExpr, SExpr),
    Overlay(SExpr, SExpr),
    Concat(SExpr, SExpr),
    Apl(SExpr, SExpr),
    Neg(SExpr),
    Cond(SExpr, SExpr, SExpr),
    Bool(bool),
    Null,
    Test(SExpr, Rc<Vec<Spanned<Selector>>>),
    Select(SExpr, Rc<Vec<Spanned<Selector>>>, Option<SExpr>),
    List(Rc<Vec<SExpr>>),
    Let(Rc<Vec<(Interned, SExpr)>>, SExpr),
    Fn(FnType),
    Resolved(Option<Interned>, SExpr),
    Stringify(SExpr),
}

impl Debug for Expr_ {
    fn fmt<W: Write>(&self, w: &mut W) -> Result {
        let s = match *self {
            Expr_::Dummy        => "Dummy",
            Expr_::String(..)   => "String",
            Expr_::Integer(..)  => "Integer",
            Expr_::Ident(..)    => "Ident",
            Expr_::PreSet(..)   => "PreSet",
            Expr_::Set(..)      => "Set",
            Expr_::And(..)      => "And",
            Expr_::Or(..)       => "Or",
            Expr_::Not(..)      => "Not",
            Expr_::Add(..)      => "Add",
            Expr_::Min(..)      => "Min",
            Expr_::Mul(..)      => "Mul",
            Expr_::Div(..)      => "Div",
            Expr_::Mod(..)      => "Mod",
            Expr_::Gt(..)       => "Gt",
            Expr_::Lt(..)       => "Lt",
            Expr_::Ge(..)       => "Ge",
            Expr_::Le(..)       => "Le",
            Expr_::Eq(..)       => "Eq",
            Expr_::Ne(..)       => "Ne",
            Expr_::Overlay(..)  => "Overlay",
            Expr_::Concat(..)   => "Concat",
            Expr_::Apl(..)      => "Apl",
            Expr_::Neg(..)      => "Neg",
            Expr_::Cond(..)     => "Cond",
            Expr_::Bool(..)     => "Bool",
            Expr_::Null         => "Null",
            Expr_::Test(..)     => "Test",
            Expr_::Select(..)   => "Select",
            Expr_::List(..)     => "List",
            Expr_::Let(..)      => "Let",
            Expr_::Fn(..)       => "Fn",
            Expr_::Resolved(_, ref e) => return e.val.fmt(w),
            Expr_::Stringify(..)       => "Stringify",
        };
        w.write_all(s.as_bytes()).ignore_ok()
    }
}

#[derive(Clone)]
pub enum FnArg {
    Ident(Interned),
    Pat(Option<Spanned<Interned>>, Rc<Vec<(Spanned<Interned>, Option<SExpr>)>>, bool),
}

#[derive(Clone)]
pub enum Selector {
    Ident(Interned),
    Integer(usize),
    Expr(SExpr),
}

#[derive(Clone)]
pub enum BuiltInFn {
    ToList,
    GetField(Option<SExpr>),
    TryGetField(Option<SExpr>),
    Assert(Option<SExpr>),
    Contains(Option<SExpr>),
    Filter(Option<SExpr>),
}

#[derive(Clone)]
pub enum FnType {
    BuiltIn(BuiltInFn),
    Normal(Spanned<FnArg>, SExpr),
}
