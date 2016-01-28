// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

use std::rc::{Rc};
use std::{error};
use std::iter::{IteratorExt};
use std::hashmap::{HashMap};

use types::diagnostic::{Diagnostic, Error};
use types::interner::{Interned};
use types::span::{Span};
use types::tree::{SExpr, Expr_, Selector, FnType};

use {Eval};

impl<D: Diagnostic> Eval<D> {
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
                self.diag.error(expr.span, Error::FoundExpr("boolean", res.to()));
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
                self.diag.error(expr.span, Error::FoundExpr("string", res.to()));
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
                self.diag.error(expr.span, Error::FoundExpr("integer", res.to()));
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
            Expr_::List(ref l) => Ok(l.to()),
            Expr_::Null => Ok(try!(Rc::new()).set(vec!())),
            _ => {
                self.diag.error(expr.span, Error::FoundExpr("list", res.to()));
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
                let expr = try!(self.resolve(e));
                let res = expr.val.val.borrow();
                match *res {
                    Expr_::String(s) => {
                        Selector::Ident(s)
                    },
                    Expr_::Integer(i) => {
                        if (isize::max() as i64) < i {
                            self.diag.error(e.span, Error::OutOfBounds);
                            self.trace(e);
                            return Err(error::InvalidSequence);
                        }
                        Selector::Integer(i as usize)
                    },
                    _ => {
                        self.diag.error(e.span, Error::FoundExpr("integer or string",
                                                                 expr.to()));
                        self.trace(e);
                        return Err(error::InvalidSequence);
                    },
                }
            },
            _ => selector.to(),
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

        let _ = out.map(|o| *o = eval_sel.to());

        self.diag.error(expr.span,
            match eval_sel {
                Selector::Ident(i) => Error::SetHasNoField(i),
                Selector::Integer(i) => Error::ListHasNoField(i),
                _ => abort!(),
            }
        );
        self.trace(&expr);

        Err(error::InvalidSequence)
    }

    /// Like `get_field` but expects the selector to not be in expression form.
    fn get_field_(&self, expr: &SExpr, sel: &Selector,
                  out: Option<&mut Selector>) -> Result<Option<SExpr>> {
        let res = try!(self.resolve(expr));
        let val = res.val.val.borrow();

        match (&*val, sel) {
            (&Expr_::Set(ref fields, _), &Selector::Ident(name)) => {
                for (&id, &(_, ref val)) in fields {
                    if id == name {
                        return Ok(Some(val.to()));
                    }
                }
                let _ = out.map(|o| *o = Selector::Ident(name));
                Ok(None)
            },
            (&Expr_::List(ref fields), &Selector::Integer(i)) => {
                if fields.len() > i {
                    Ok(Some(fields[i].to()))
                } else {
                    let _ = out.map(|o| *o = Selector::Integer(i));
                    Ok(None)
                }
            },
            (&Expr_::Null, _) => {
                Ok(None)
            },
            _ => {
                self.diag.error(expr.span,
                    if let &Selector::Ident(..) = sel {
                        Error::FoundExpr("set", expr.to())
                    } else {
                        Error::FoundExpr("list", expr.to())
                    }
                );
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
                      expr: &SExpr) -> Result<Rc<HashMap<Interned, (Span, SExpr)>>> {
        let res = try!(self.resolve(expr));
        let val = res.val.val.borrow();
        match *val {
            Expr_::Set(ref fields, _) => Ok(fields.to()),
            Expr_::Null => Ok(try!(Rc::new()).set(try!(HashMap::new()))),
            _ => {
                self.diag.error(expr.span, Error::FoundExpr("set", res.to()));
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
            Expr_::Fn(ref f) => Ok(f.to()),
            _ => {
                self.diag.error(expr.span, Error::FoundExpr("function", res.to()));
                self.trace(expr);
                Err(error::InvalidSequence)
            },
        }
    }

    pub fn get_path(&self, expr: &SExpr) -> Rc<Vec<SExpr>> {
        let val = expr.val.val.borrow();
        match *val {
            Expr_::Path(ref f) => f.to(),
            _ => abort!(),
        }
    }

    pub fn get_selector(&self, expr: &SExpr) -> Selector {
        let val = expr.val.val.borrow();
        match *val {
            Expr_::Selector(ref s) => s.to(),
            _ => abort!(),
        }
    }
}
