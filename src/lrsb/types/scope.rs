// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

use std::hashmap::{HashMap, Entry};
use std::fmt::{Debug, Write};

use tree::{SExpr};
use interner::{Interned};

pub type Scope = XScope<SExpr>;
pub type PseudoScope = XScope<()>;

pub struct XScope<T> {
    names: HashMap<Interned, Vec<T>>,
}

impl<T: Clone> XScope<T> {
    pub fn new() -> XScope<T> {
        XScope {
            names: HashMap::new().unwrap()
        }
    }

    pub fn push(&mut self, name: Interned, val: T) {
        match self.names.entry(&name).unwrap() {
            Entry::Occupied(mut o) => o.push(val),
            Entry::Vacant(v) => { v.set(name, vec!(val)); },
        }
    }

    pub fn pop(&mut self, name: Interned) {
        match self.names.entry(&name).unwrap() {
            Entry::Occupied(mut o) => {
                o.pop().unwrap();
                if o.len() == 0 {
                    o.remove();
                }
            },
            _ => abort!(),
        }
    }

    pub fn get(&self, name: Interned) -> Option<T> {
        match self.names.get(&name) {
            Some(v) => v.last().map(|v| v.clone()),
            _ => None,
        }
    }
}

impl<T: Debug> Debug for XScope<T> {
    fn fmt<W: Write>(&self, mut w: &mut W) -> Result {
        write!(w, "XScope {{ names: {:?} }}", self.names)
    }
}
