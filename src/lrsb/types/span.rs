// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

use std::ops::{Deref, DerefMut};
use std::fmt::{Debug, Write};
use std::clone::{MaybeClone};

#[derive(Copy, Eq)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

impl Span {
    pub fn new(lo: u32, hi: u32) -> Span {
        Span {
            lo: lo,
            hi: hi,
        }
    }

    pub fn built_in() -> Span {
        Span::new(!0, !0)
    }
}

impl Debug for Span {
    fn fmt<W: Write>(&self, mut w: &mut W) -> Result {
        write!(w, "{}-{}", self.lo, self.hi)
    }
}

#[derive(Copy)]
pub struct Spanned<T> {
    pub span: Span,
    pub val: T,
}

impl<T: Clone> Clone for Spanned<T> {
    fn clone(&self) -> Spanned<T> {
        Spanned {
            span: self.span,
            val: self.val.clone(),
        }
    }
}

impl<T: MaybeClone> MaybeClone for Spanned<T> {
    fn maybe_clone(&self) -> Result<Spanned<T>> {
        Ok(Spanned {
            span: self.span,
            val: try!(self.val.maybe_clone()),
        })
    }
}

impl<T: Debug> Debug for Spanned<T> {
    fn fmt<W: Write>(&self, mut w: &mut W) -> Result {
        write!(w, "@{:?}: {:?}", self.span, self.val)
    }
}

impl<T> Spanned<T> {
    pub fn new(span: Span, val: T) -> Spanned<T> {
        Spanned {
            span: span,
            val: val,
        }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.val
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.val
    }
}
