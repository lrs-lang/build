// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

use std::string::{CString, CStr, ByteStr};
use std::rc::{Rc};
use std::util::{memchr};

use span::{Span};

/// A codemap.
///
/// = Remarks
///
/// This type is used to map spans to source code lines.
pub struct Codemap {
    files: Vec<Filemap>,
}

impl Codemap {
    /// Creates a new, empty codemap.
    pub fn new() -> Codemap {
        Codemap { files: Vec::new() }
    }

    /// Adds a file to the codemap.
    ///
    /// [argument, name]
    /// The name of the file.
    ///
    /// [argument, src]
    /// The contents of the file.
    pub fn add_file(&mut self, name: CString, src: Rc<Vec<u8>>) -> u32 {
        let old_pos = match self.files.last() {
            Some(f) => *f.lines.last().unwrap(),
            _ => 0,
        };
        let mut cur_pos = old_pos;
        let mut lines = Vec::new();
        lines.push(cur_pos);
        {
            let mut src = &**src;
            while src.len() > 0 {
                let pos = match memchr(src, b'\n') {
                    Some(pos) => pos + 1,
                    _ => src.len(),
                };
                cur_pos += pos as u32;
                src = &src[pos..];
                lines.push(cur_pos);
            }
        }
        let map = Filemap {
            file: name,
            src: src,
            lines: lines,
        };
        self.files.push(map);
        old_pos
    }

    /// Retrieves the file associated with a span.
    pub fn file(&self, span: Span) -> &Filemap {
        for file in &self.files {
            if file.lines[file.lines.len()-1] > span.lo {
                return file;
            }
        }
        &self.files[self.files.len()-1]
    }
}

/// A file map.
///
/// = Remarks
///
/// This is where the real work happens.
pub struct Filemap {
    file: CString,
    src: Rc<Vec<u8>>,
    /// Contains at index `i` the byte that starts line `i+1`.
    lines: Vec<u32>,
}

impl Filemap {
    /// Returns the name of the file represented by this map.
    pub fn file(&self) -> &CStr {
        &self.file
    }

    /// Returns an iterator over the lines crossed by a span.
    pub fn lines<'a>(&'a self, span: Span) -> LineIter {
        let start = match self.lines.find_binary(|&l| l.cmp(&span.lo)) {
            (Some(l), _) => l,
            (_, l) => l - 1,
        };
        let end = self.lines.find_binary(|&l| l.cmp(&span.hi)).1;
        LineIter {
            src: self,
            start: start,
            end: end,
            start_idx: span.lo - self.lines[start],
            end_idx: span.hi - self.lines[end-1],
        }
    }
}

/// An iterator over file lines.
pub struct LineIter<'a> {
    src: &'a Filemap,
    start: usize,
    end: usize,
    start_idx: u32,
    end_idx: u32,
}

impl<'a> LineIter<'a> {
    /// The number of lines.
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    /// The first line.
    pub fn start(&self) -> usize {
        self.start + 1
    }

    /// The last line.
    pub fn last(&self) -> usize {
        self.end
    }

    /// The column of the first byte in the span.
    pub fn start_idx(&self) -> u32 {
        self.start_idx
    }

    /// The column of the last byte in the span.
    pub fn last_idx(&self) -> u32 {
        self.end_idx - 1
    }
}

impl<'a> Iterator for LineIter<'a> {
    type Item = (u32, &'a ByteStr);
    fn next(&mut self) -> Option<(u32, &'a ByteStr)> {
        if self.start == self.end {
            None
        } else {
            let base = self.src.lines[0];
            let lo = self.src.lines[self.start] - base;
            let hi = self.src.lines[self.start+1] - base;
            let line = &self.src.src[lo as usize..hi as usize];
            self.start += 1;
            Some((self.start as u32, line.as_ref()))
        }
    }
}

impl<'a> To for LineIter<'a> {
    fn to(&self) -> Self {
        LineIter {
            src:       self.src,
            start:     self.start,
            end:       self.end,
            start_idx: self.start_idx,
            end_idx:   self.end_idx,
        }
    }
}

impl<'a> TryTo for LineIter<'a> {
    fn try_to(&self) -> Result<Self> {
        Ok(self.to())
    }
}
