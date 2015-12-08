// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

use std::{mem};
use std::rc::{Rc};
use std::share::{RefCell};
use std::fmt::{Debug, Write};
use std::clone::{MaybeClone};
use std::marker::{Leak};
use std::hash::{Hash, Hasher};
use std::ops::{Eq};
use std::hashmap::{HashMap};

use interner::{Interned};
use span::{Spanned, Span};

/// An expression with an associated span.
pub type SExpr = Spanned<Expr>;

/// A reference-counted expression with interior mutability.
///
/// = Remarks
///
/// Instead of copying `Expr_`s every time they are used, we add references to them. This
/// way we only have to evaluate each `Expr_` once.
#[derive(Clone)]
pub struct Expr {
    pub val: Rc<RefCell<Expr_>>,
}

impl Hash for Expr {
    fn stateful_hash<H: Hasher>(&self, h: &mut H) {
        mem::addr::<RefCell<Expr_>>(&self.val).stateful_hash(h);
    }
}

impl Eq for Expr {
    fn eq(&self, other: &Expr) -> bool {
        mem::addr::<RefCell<Expr_>>(&self.val) ==
            mem::addr::<RefCell<Expr_>>(&other.val)
    }
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

    /// Returns whether this is a inherit expression.
    pub fn is_inherit(&self) -> bool {
        match *self.val.borrow() {
            Expr_::Inherit => true,
            _ => false,
        }
    }
}

/// An expression.
#[derive(Clone)]
pub enum Expr_ {
    /// A dummy expression.
    ///
    /// = Remarks
    ///
    /// This is used in sets when fields are created with the `inherit` operator. E.g.,
    ///
    /// ----
    /// {
    ///     a: 1,
    ///     inherit b,
    /// }
    /// ----
    ///
    /// Until the `b` field has its value resolved, it has a `Inherit` value on its
    /// right-hand-side. This way we don't have to distinguish between normal fields and
    /// `inherit` fields in most places.
    Inherit,

    /// A string.
    ///
    /// [field, 1]
    /// The interned string.
    ///
    /// = Remarks
    ///
    /// Note that
    ///
    /// ----
    /// "a\{e}b"
    /// ----
    ///
    /// is turned into
    ///
    /// ----
    /// "a" ++ Stringify(e) ++ "b"
    /// ----
    ///
    /// by the parser.
    String(Interned),

    /// An integer.
    ///
    /// [field, 1]
    /// The value of the integer.
    Integer(i64),

    /// An identifier.
    ///
    /// [field, 1]
    /// The interned identifier.
    Ident(Interned),

    /// A set.
    ///
    /// [field, 1]
    /// The fields of the set.
    Set(Rc<HashMap<Interned, (Span, SExpr)>>, bool),

    /// A logical and.
    ///
    /// [field, 1]
    /// The left-hand-side.
    ///
    /// [field, 2]
    /// The right-hand-side.
    ///
    /// = Remarks
    ///
    /// If the left-hand-side evaluates to `false`, the right-hand-side should not be
    /// evaluated.
    And(SExpr, SExpr),

    /// A logical or.
    ///
    /// [field, 1]
    /// The left-hand-side.
    ///
    /// [field, 2]
    /// The right-hand-side.
    ///
    /// = Remarks
    ///
    /// If the left-hand-side evaluates to `true`, the right-hand-side should not be
    /// evaluated.
    Or(SExpr, SExpr),

    /// A logical not.
    ///
    /// [field, 1]
    /// The expression to be inverted.
    Not(SExpr),

    /// A numeric plus.
    ///
    /// [field, 1]
    /// The left-hand-side.
    ///
    /// [field, 2]
    /// The right-hand-side.
    Add(SExpr, SExpr),

    /// A numeric minus.
    ///
    /// [field, 1]
    /// The left-hand-side.
    ///
    /// [field, 2]
    /// The right-hand-side.
    Sub(SExpr, SExpr),

    /// A numeric multiplication.
    ///
    /// [field, 1]
    /// The left-hand-side.
    ///
    /// [field, 2]
    /// The right-hand-side.
    Mul(SExpr, SExpr),

    /// A numeric division.
    ///
    /// [field, 1]
    /// The numerator.
    ///
    /// [field, 2]
    /// The denominator.
    Div(SExpr, SExpr),

    /// A numeric modulus operation.
    ///
    /// [field, 1]
    /// The numerator.
    ///
    /// [field, 2]
    /// The denominator.
    Mod(SExpr, SExpr),

    /// A numeric greater-than comparison.
    ///
    /// [field, 1]
    /// The left-hand-side.
    ///
    /// [field, 2]
    /// The right-hand-side.
    Gt(SExpr, SExpr),

    /// A numeric less-than comparison.
    ///
    /// [field, 1]
    /// The left-hand-side.
    ///
    /// [field, 2]
    /// The right-hand-side.
    Lt(SExpr, SExpr),

    /// A numeric greater-or-equal comparison.
    ///
    /// [field, 1]
    /// The left-hand-side.
    ///
    /// [field, 2]
    /// The right-hand-side.
    Ge(SExpr, SExpr),

    /// A numeric less-or-equal comparison.
    ///
    /// [field, 1]
    /// The left-hand-side.
    ///
    /// [field, 2]
    /// The right-hand-side.
    Le(SExpr, SExpr),

    /// An equal comparison.
    ///
    /// [field, 1]
    /// The left-hand-side.
    ///
    /// [field, 2]
    /// The right-hand-side.
    ///
    /// = Remarks
    ///
    /// The expressions for which this evaluates to true are
    ///
    /// * `String(s) == String(s)`,
    /// * `Integer(i) == Integer(i)`, and
    /// * `Null == Null`.
    ///
    /// For all other left-hand-sides and right-hand-sides, this evaluates to false.
    Eq(SExpr, SExpr),

    /// An not-equal comparison.
    ///
    /// [field, 1]
    /// The left-hand-side.
    ///
    /// [field, 2]
    /// The right-hand-side.
    ///
    /// = Remarks
    ///
    /// `l != e` is true iff `!(l == e)` is true.
    Ne(SExpr, SExpr),

    /// An implication.
    ///
    /// [field, 1]
    /// The antecedent.
    ///
    /// [field, 1]
    /// The consequent.
    ///
    /// = Remarks
    ///
    /// `l -> r` is true iff `!l || r` is true.
    Impl(SExpr, SExpr),

    /// An overlay operation.
    ///
    /// [field, 1]
    /// The bottom.
    ///
    /// [field, 2]
    /// The top.
    Overlay(SExpr, SExpr),
    Concat(SExpr, SExpr),
    Apl(SExpr, SExpr),
    Neg(SExpr),
    Cond(SExpr, SExpr, SExpr),
    Bool(bool),
    Null,
    Test(SExpr, SExpr),
    Select(SExpr, SExpr, Option<SExpr>),
    List(Rc<Vec<SExpr>>),
    Let(Rc<HashMap<Interned, (Span, SExpr)>>, SExpr),
    Fn(FnType),
    Resolved(Option<Interned>, SExpr),
    Stringify(SExpr),
    Path(Rc<Vec<SExpr>>),
    Selector(Selector),
}

impl Debug for Expr_ {
    fn fmt<W: Write>(&self, w: &mut W) -> Result {
        let s = match *self {
            Expr_::Inherit       => "Inherit",
            Expr_::String(..)    => "String",
            Expr_::Integer(..)   => "Integer",
            Expr_::Ident(..)     => "Ident",
            Expr_::Set(..)       => "Set",
            Expr_::And(..)       => "And",
            Expr_::Or(..)        => "Or",
            Expr_::Not(..)       => "Not",
            Expr_::Add(..)       => "Add",
            Expr_::Sub(..)       => "Sub",
            Expr_::Mul(..)       => "Mul",
            Expr_::Div(..)       => "Div",
            Expr_::Mod(..)       => "Mod",
            Expr_::Gt(..)        => "Gt",
            Expr_::Lt(..)        => "Lt",
            Expr_::Ge(..)        => "Ge",
            Expr_::Le(..)        => "Le",
            Expr_::Eq(..)        => "Eq",
            Expr_::Ne(..)        => "Ne",
            Expr_::Impl(..)      => "Imlp",
            Expr_::Overlay(..)   => "Overlay",
            Expr_::Concat(..)    => "Concat",
            Expr_::Apl(..)       => "Apl",
            Expr_::Neg(..)       => "Neg",
            Expr_::Cond(..)      => "Cond",
            Expr_::Bool(..)      => "Bool",
            Expr_::Null          => "Null",
            Expr_::Test(..)      => "Test",
            Expr_::Select(..)    => "Select",
            Expr_::List(..)      => "List",
            Expr_::Let(..)       => "Let",
            Expr_::Fn(..)        => "Fn",
            Expr_::Stringify(..) => "Stringify",
            Expr_::Path(..)      => "Path",
            Expr_::Selector(..)  => "Selector",
            Expr_::Resolved(_, ref e) => return e.val.fmt(w),
        };
        w.write_all(s.as_bytes()).ignore_ok()
    }
}

#[derive(Clone)]
pub enum FnArg {
    Ident(Interned),
    Pat(Option<Spanned<Interned>>, Rc<HashMap<Interned, (Span, Option<SExpr>)>>, bool),
}

#[derive(Clone)]
pub enum Selector {
    Ident(Interned),
    Integer(usize),
    Expr(SExpr),
}

pub trait BuiltInFn: Leak {
    fn apply<'a>(&self, expr: &'a SExpr) -> Result<Expr_>;
}

#[derive(Clone)]
pub enum FnType {
    BuiltIn(Rc<BuiltInFn>),
    Normal(Spanned<FnArg>, SExpr),
}
