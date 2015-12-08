// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

use std::{mem};
use std::hashmap::{CompactMap, Entry};
use std::fmt::{Debug, Write};
use std::share::{Cell};
use std::string::{AsByteStr, ByteStr};
use std::hash::{Hash, Hasher};

/// A string interned in an interned.
#[derive(Copy, Eq)]
pub struct Interned(u32);

impl Debug for Interned {
    fn fmt<W: Write>(&self, mut w: &mut W) -> Result {
        write!(w, "Interned({})", self.0)
    }
}

impl Hash for Interned {
    fn stateful_hash<H: Hasher>(&self, h: &mut H) { self.0.stateful_hash(h) }
    fn hash<H: Hasher, S: Into<H::Seed>>(&self, seed: S) -> H::Digest {
        self.0.hash::<H,S>(seed)
    }
}

struct Inner {
    strings: Vec<Vec<u8>>,
    ids: CompactMap<&'static [u8], usize>,
}

/// A single-threaded string interner.
///
/// = Remarks
///
/// This interner takes ownership of strings and will them destroy only when it's getting
/// destroyed itself.
pub struct Interner {
    data: Cell<Inner>,
}

impl Interner {
    /// Creates a new interner.
    pub fn new() -> Interner {
        Interner {
            data: Cell::new(Inner {
                strings: Vec::new(),
                ids: CompactMap::new().unwrap(),
            }),
        }
    }

    fn inner(&self) -> &mut Inner {
        unsafe { &mut *self.data.ptr() }
    }

    /// Inserts a string into an interner.
    ///
    /// [argument, val]
    /// The string to insert.
    ///
    /// [return_value]
    /// Returns the interned string.
    pub fn insert(&self, val: Vec<u8>) -> Interned {
        let inner = self.inner();
        let pos = {
            let strr: &'static _ = unsafe { mem::cast(&*val) };
            match inner.ids.entry(&strr).unwrap() {
                Entry::Vacant(v) => {
                    let pos = inner.strings.len();
                    inner.strings.push(val);
                    v.set(strr, pos);
                    pos
                },
                Entry::Occupied(o) => *o,
            }
        };
        Interned(pos as u32)
    }

    /// Looks up the contents of an interned string.
    ///
    /// [argument, i]
    /// The interned string.
    pub fn get(&self, i: Interned) -> &ByteStr {
        let inner = self.inner();
        let i = i.0 as usize;
        assert!(i < inner.strings.len());
        inner.strings[i].as_byte_str()
    }

    /// Concatenates two interned strings.
    ///
    /// [argument, left]
    /// The left part.
    ///
    /// [argument, right]
    /// The right part.
    ///
    /// [return_value]
    /// Returns a new interned string whose content is the concatenation of the contents
    /// of the left and right parts.
    pub fn concat(&self, left: Interned, right: Interned) -> Interned {
        let tmp: &[u8] = self.get(left).as_ref();
        let mut l = tmp.to_owned().unwrap();
        let r = self.get(right).as_ref();
        l.push_all(r).unwrap();
        self.insert(l)
    }
}
