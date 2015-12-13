// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

use std::rc::{Rc};
use std::share::{RefCellStatus};
use std::{error};

use types::diagnostic::{Diagnostic, Error};
use types::scope::{Scope};
use types::tree::{SExpr, Expr_, FnArg, Selector, FnType, BuiltInFn};

use {Eval};

impl<D: Diagnostic> Eval<D> {
    /// Forces (evaluates) the expression.
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
        let borrow = expr.val.val.borrow();

        if expr.val.val.status() != RefCellStatus::Borrowed(0) {
            self.diag.error(expr.span, Error::InfiniteRecursion);
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
                Expr_::Set(_, false) | Expr_::Inherit =>
            {
                // Nothing to do
                Ok(())
            },
            Expr_::Resolved(..) =>
            {
                // Handled above
                abort!()
            },
            Expr_::Add(..) | Expr_::Sub(..) | Expr_::Mul(..) | Expr_::Div(..) |
                Expr_::Mod(..) | Expr_::Neg(..) =>
            {
                drop(borrow);
                self.force_int(expr)
            },
            Expr_::And(..) | Expr_::Or(..)  | Expr_::Not(..) | Expr_::Gt(..) |
                Expr_::Lt(..) | Expr_::Ge(..) | Expr_::Le(..) | Expr_::Test(..) |
                Expr_::Eq(..) | Expr_::Ne(..) | Expr_::Impl(..) =>
            {
                drop(borrow);
                self.force_bool(expr)
            },
            Expr_::Concat(..)  =>
            {
                drop(borrow);
                self.force_concat(expr)
            },
            Expr_::Overlay(..)  =>
            {
                drop(borrow);
                self.force_overlay(expr)
            },
            Expr_::Select(..) =>
            {
                drop(borrow);
                self.force_select(expr)
            },
            Expr_::Apl(..) =>
            {
                drop(borrow);
                self.force_apl(expr)
            },
            Expr_::Let(..) | Expr_::Set(_, true) =>
            {
                drop(borrow);
                self.force_bind(expr, &mut try!(Scope::new()), false);
                self.force(expr)
            },
            Expr_::Cond(..) =>
            {
                drop(borrow);
                self.force_cond(expr)
            },
            Expr_::Stringify(..) =>
            {
                drop(borrow);
                self.force_stringify(expr)
            },
            Expr_::Path(..) | Expr_::Selector(..) =>
            {
                abort!();
            },
        }
    }

    fn force_int(&self, expr: &SExpr) -> Result {
        let mut val = expr.val.val.borrow_mut();

        let new = match *val {
            Expr_::Add(ref l, ref r) => {
                Expr_::Integer(try!(self.get_int(l)).wrapping_add(try!(self.get_int(r))))
            }
            Expr_::Sub(ref l, ref r) => {
                Expr_::Integer(try!(self.get_int(l)).wrapping_sub(try!(self.get_int(r))))
            }
            Expr_::Mul(ref l, ref r) => {
                Expr_::Integer(try!(self.get_int(l)).wrapping_mul(try!(self.get_int(r))))
            }
            Expr_::Div(ref l, ref r) => {
                let l = try!(self.get_int(l));
                let rn = try!(self.get_int(r));
                if rn == 0 {
                    self.diag.error(r.span , Error::DivideByZero);
                    return Err(error::InvalidSequence);
                }
                Expr_::Integer(l / rn)
            }
            Expr_::Mod(ref l, ref r) => {
                let l = try!(self.get_int(l));
                let rn = try!(self.get_int(r));
                if rn == 0 {
                    self.diag.error(r.span , Error::DivideByZero);
                    return Err(error::InvalidSequence);
                }
                Expr_::Integer(l % rn)
            }
            Expr_::Neg(ref e) => {
                Expr_::Integer(0i64.wrapping_sub(try!(self.get_int(e))))
            },
            _ => abort!(),
        };

        *val = new;

        Ok(())
    }

    fn force_bool(&self, expr: &SExpr) -> Result {
        let mut val = expr.val.val.borrow_mut();

        let new = match *val {
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
                Expr_::Bool(try!(self.get_int(l)) <= try!(self.get_int(r)))
            },
            Expr_::Eq(ref l, ref r) => {
                Expr_::Bool(try!(self.equal_to(r, l)))
            },
            Expr_::Ne(ref l, ref r) => {
                Expr_::Bool(!try!(self.equal_to(r, l)))
            },
            Expr_::Impl(ref l, ref r) => {
                Expr_::Bool(!try!(self.get_bool(l)) || try!(self.get_bool(r)))
            },
            Expr_::And(ref l, ref r) => {
                Expr_::Bool(try!(self.get_bool(l)) && try!(self.get_bool(r)))
            }
            Expr_::Or(ref l, ref r) => {
                Expr_::Bool(try!(self.get_bool(l)) || try!(self.get_bool(r)))
            }
            Expr_::Not(ref e) => {
                Expr_::Bool(!try!(self.get_bool(e)))
            }
            Expr_::Test(ref s, ref path) => {
                let mut set = s.clone();
                let mut path = &self.get_path(&path)[..];
                let mut err_span = set.span;
                while path.len() > 0 {
                    let selector = self.get_selector(&path[0]);
                    set = match try!(self.get_opt_field(&set, &selector, None)) {
                        Some(f) => f,
                        None => break,
                    };
                    err_span.hi = path[0].span.hi;
                    path = &path[1..];
                }
                Expr_::Bool(path.len() == 0)
            },
            _ => abort!(),
        };

        *val = new;

        Ok(())
    }

    fn force_overlay(&self, expr: &SExpr) -> Result {
        let mut val = expr.val.val.borrow_mut();

        let new = if let Expr_::Overlay(ref bottom, ref top) = *val {
            let bottom = try!(self.get_fields(bottom));
            let top = try!(self.get_fields(top));

            let mut new = try!((*bottom).try_clone());

            for (&id, &(span, ref val)) in &top {
                new.set(id, (span, val.to()));
            }

            try!(new.shrink_to_fit());

            Expr_::Set(try!(Rc::new()).set(new), false)
        } else {
            abort!();
        };

        *val = new;

        Ok(())
    }

    fn force_concat(&self, expr: &SExpr) -> Result {
        let mut val = expr.val.val.borrow_mut();

        let new = if let Expr_::Concat(ref l, ref r) = *val {
            let l = try!(self.resolve(l));
            let left = l.val.val.borrow();
            match *left {
                Expr_::String(left) => {
                    let right = try!(self.get_string(r));
                    let new = try!(self.interner.concat(left, right));
                    Expr_::String(new)
                },
                Expr_::List(ref left) => {
                    let right = try!(self.get_list(r));
                    if right.len() == 0 {
                        Expr_::List(left.to())
                    } else {
                        let mut left = try!((**left).try_clone());
                        left.reserve(right.len());
                        for el in &right { left.push(el.to()); }
                        Expr_::List(try!(Rc::new()).set(left))
                    }
                },
                _ => {
                    self.diag.error(l.span, Error::FoundExpr("String or List", l.to()));
                    self.trace(&l);
                    return Err(error::InvalidSequence);
                },
            }
        } else {
            abort!();
        };

        *val = new;

        Ok(())
    }

    fn force_bind(&self, expr: &SExpr, scope: &mut Scope<SExpr>, in_fn_body: bool) -> Result {
        macro_rules! resolve {
            ($a:expr) => {{
                try!(self.force_bind($a, scope, in_fn_body))
            }};
        }

        let mut val = expr.val.val.borrow_mut();

        // Identifiers
        {
            if let Expr_::Ident(id) = *val {
                if let Some(e) = scope.get(id) {
                    *val = Expr_::Resolved(Some(id), e);
                }
                return Ok(());
            }
        }

        // Lets
        {
            let mut new_val = None;
            if let Expr_::Let(ref fields, ref body) = *val {
                for (&id, &(_, ref val)) in fields {
                    if in_fn_body {
                        scope.hide(id);
                    } else {
                        scope.bind(id, val.to());
                    }
                }
                for (_, &(_, ref val)) in fields {
                    resolve!(val);
                }
                resolve!(body);
                for (&id, _) in fields {
                    scope.pop(id);
                }
                if !in_fn_body {
                    new_val = Some(Expr_::Resolved(None, body.to()));
                }
            }
            if let Expr_::Let(..) = *val {
                if let Some(new_val) = new_val {
                    *val = new_val;
                }
                return Ok(());
            }
        }

        // Recursive sets
        {
            let mut new_val = None;
            if let Expr_::Set(ref fields, true) = *val {
                for (&id, &(_, ref val)) in fields {
                    if !val.is_inherit() {
                        if in_fn_body {
                            scope.hide(id);
                        } else {
                            scope.bind(id, val.to());
                        }
                    }
                }
                for (&id, &(_, ref val)) in fields {
                    if val.is_inherit() {
                        if let Some(e) = scope.get(id) {
                            *val.val.val.borrow_mut() = Expr_::Resolved(Some(id), e);
                        }
                    } else {
                        resolve!(val);
                    }
                }
                for (&id, &(_, ref val)) in fields {
                    if !val.is_inherit() {
                        scope.pop(id);
                    }
                }
                if !in_fn_body {
                    new_val = Some(Expr_::Set(fields.to(), false));
                }
            }
            if let Expr_::Set(_, true) = *val {
                if let Some(new_val) = new_val {
                    *val = new_val;
                }
                return Ok(());
            }
        }

        // Functions
        {
            if let Expr_::Fn(FnType::Normal(ref pat, ref body)) = *val {
                match pat.val {
                    FnArg::Ident(id) => try!(scope.hide(id)),
                    FnArg::Pat(id, ref fields, _) => {
                        for (_, &(_, ref field_alt)) in &*fields {
                            if let Some(ref field_alt) = *field_alt {
                                resolve!(field_alt);
                            }
                        }
                        for (&field_name, _) in &*fields {
                            try!(scope.hide(field_name));
                        }
                        if let Some(id) = id {
                            try!(scope.hide(id.val));
                        }
                    },
                }
                self.force_bind(body, scope, true);
                match pat.val {
                    FnArg::Ident(id) => scope.pop(id),
                    FnArg::Pat(id, ref fields, _) => {
                        for (&field_name, _) in &*fields {
                            scope.pop(field_name);
                        }
                        if let Some(id) = id {
                            scope.pop(id.val);
                        }
                    },
                }
                return Ok(());
            }
        }

        // The rest
        match *val {
            Expr_::Ident(..) | Expr_::Let(..) | Expr_::Set(_, true) |
                Expr_::Fn(FnType::Normal(..)) =>
            {
                // handled above
                abort!();
            },
            Expr_::Inherit =>
            {
                abort!();
            },
            Expr_::Null | Expr_::String(..) | Expr_::Integer(..) | Expr_::Resolved(..) |
                Expr_::Fn(FnType::BuiltIn(..)) | Expr_::Bool(..) =>
            {
                // nothing to do
            },
            Expr_::Not(ref e) | Expr_::Neg(ref e) | Expr_::Stringify(ref e) =>
            {
                resolve!(e);
            },
            Expr_::And(ref l, ref r) | Expr_::Or(ref l, ref r) |
                Expr_::Add(ref l, ref r) | Expr_::Sub(ref l, ref r) |
                Expr_::Mul(ref l, ref r) | Expr_::Div(ref l, ref r) |
                Expr_::Mod(ref l, ref r) | Expr_::Gt(ref l, ref r) |
                Expr_::Lt(ref l, ref r) | Expr_::Ge(ref l, ref r) |
                Expr_::Le(ref l, ref r) | Expr_::Eq(ref l, ref r) |
                Expr_::Ne(ref l, ref r) | Expr_::Impl(ref l, ref r) |
                Expr_::Overlay(ref l, ref r) | Expr_::Concat(ref l, ref r) |
                Expr_::Apl(ref l, ref r) =>
            {
                resolve!(l);
                resolve!(r);
            },
            Expr_::Cond(ref cond, ref then, ref el) =>
            {
                resolve!(cond);
                resolve!(then);
                resolve!(el);
            },
            Expr_::Set(ref fields, false) =>
            {
                for (&name, &(_, ref val)) in fields {
                    if val.is_inherit() {
                        if let Some(e) = scope.get(name) {
                            *val.val.val.borrow_mut() =
                                    Expr_::Resolved(Some(name), e);
                        }
                    } else {
                        resolve!(val);
                    }
                }
            },
            Expr_::List(ref fields) =>
            {
                for field in fields {
                    resolve!(field);
                }
            },
            Expr_::Test(ref target, ref path) =>
            {
                resolve!(target);
                resolve!(path);
            },
            Expr_::Select(ref target, ref path, ref alt) =>
            {
                resolve!(target);
                resolve!(path);
                if let Some(ref alt) = *alt {
                    resolve!(alt);
                }
            },
            Expr_::Path(ref segs) =>
            {
                for seg in &*segs {
                    resolve!(seg);
                }
            },
            Expr_::Selector(ref ty) =>
            {
                if let Selector::Expr(ref e) = *ty {
                    resolve!(e);
                }
            },
        }
        Ok(())
    }

    fn force_cond(&self, expr: &SExpr) -> Result {
        let mut val = expr.val.val.borrow_mut();

        let new = if let Expr_::Cond(ref cond, ref then, ref el) = *val {
            if try!(self.get_bool(cond)) {
                try!(self.force(then));
                then.to()
            } else {
                try!(self.force(el));
                el.to()
            }
        } else {
            abort!()
        };

        *val = Expr_::Resolved(None, new);

        Ok(())
    }

    fn force_stringify(&self, expr: &SExpr) -> Result {
        let mut val = expr.val.val.borrow_mut();
        let e = match *val {
            Expr_::Stringify(ref e) => e.to(),
            _ => abort!(),
        };
        let dst = try!(self.resolve(&e));
        let dst = dst.val.val.borrow();
        match *dst {
            Expr_::String(..) => *val = Expr_::Resolved(None, e),
            Expr_::Integer(v) => {
                let s = try!(format!("{}", v));
                let id = try!(self.interner.insert(s));
                *val = Expr_::String(id);
            },
            _ => {
                self.diag.error(e.span, Error::CannotStringify(e.to()));
                self.trace(&e);
                return Err(error::InvalidSequence);
            },
        }
        Ok(())
    }

    fn force_apl(&self, apl: &SExpr) -> Result {
        let (func, arg) = match *apl.val.val.borrow() {
            Expr_::Apl(ref func, ref arg) => (func.to(), arg.to()),
            _ => abort!(),
        };

        let (pat, body) = match try!(self.get_func(&func)) {
            FnType::Normal(pat, body) => (pat, body),
            FnType::BuiltIn(func) => {
                let res = try!(func.apply(&arg));
                *apl.val.val.borrow_mut() = res;
                return Ok(());
            },
        };

        let mut scope = try!(Scope::new());

        match pat.val {
            FnArg::Ident(i) => {
                scope.bind(i, arg);
            },
            FnArg::Pat(at, fields, wild) => {
                let arg_fields = try!(self.get_fields(&arg));
                if !wild {
                    for (&id, _) in &*arg_fields {
                        if fields.get(&id).is_none() {
                            self.diag.error(arg.span, Error::FnPattern(id));
                            self.trace(&arg);
                            return Err(error::InvalidSequence);
                        }
                    }
                }
                for (&id, &(span, ref alt)) in &*fields {
                    if let Some(&(_, ref val)) = arg_fields.get(&id) {
                        scope.bind(id, val.to());
                    } else if let Some(ref alt) = *alt {
                        scope.bind(id, alt.to());
                    } else {
                        self.diag.error(span, Error::FnMissingField(id));
                        self.trace(&arg);
                        return Err(error::InvalidSequence);
                    }
                }
                if let Some(at) = at {
                    scope.bind(at.val, arg.to());
                }
            },
        }

        let new_body = try!(self.deep_copy(&body, &mut scope));
        try!(self.force(&new_body));

        *apl.val.val.borrow_mut() = Expr_::Resolved(None, new_body);

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
    fn force_select(&self, expr: &SExpr) -> Result {
        let res = {
            let val = expr.val.val.borrow();

            let (mut set, path, alt): (SExpr, _, _) = match *val {
                Expr_::Select(ref s, ref p, ref a) => (s.to(), p, a.to()),
                _ => abort!(),
            };

            // The span we will use to print error messages.
            let mut err_span = set.span;
            let mut path = &self.get_path(&path)[..];
            let mut bad_path = Selector::Integer(0);

            while path.len() > 0 {
                let selector = self.get_selector(&path[0]);
                set = match try!(self.get_opt_field(&set, &selector,
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
                    match bad_path {
                        Selector::Ident(i) => {
                            self.diag.error(err_span, Error::SetHasNoField(i));
                        },
                        Selector::Integer(i) => {
                            self.diag.error(err_span, Error::ListHasNoField(i));
                        },
                        _ => abort!(),
                    }
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
}
