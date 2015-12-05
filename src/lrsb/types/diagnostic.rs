// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

use std::io::{Write};
use std::marker::{Leak};

use span::{Span};

pub trait Diagnostic: Leak {
    fn error<F>(&self, span: Span, f: F)
        where F: FnOnce(&mut Write) -> Result;
    fn notice<F>(&self, span: Span, f: F)
        where F: FnOnce(&mut Write) -> Result;
}
