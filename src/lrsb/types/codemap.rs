// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

use std::string::{NoNullString, NoNullStr, AsByteStr, ByteStr};
use std::rc::{Rc};
use std::util::{memchr};

use span::{Span};

pub struct Codemap {
    files: Vec<Filemap>,
}

impl Codemap {
    pub fn new() -> Codemap {
        Codemap { files: Vec::new() }
    }

    pub fn add_file(&mut self, name: NoNullString, src: Rc<Vec<u8>>) {
        let mut cur_pos = match self.files.last() {
            Some(f) => *f.lines.last().unwrap(),
            _ => 0,
        };
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
    }

    pub fn file(&self, span: Span) -> &Filemap {
        for file in &self.files {
            if file.lines[file.lines.len()-1] > span.lo {
                return file;
            }
        }
        &self.files[self.files.len()-1]
    }
}

pub struct Filemap {
    file: NoNullString,
    src: Rc<Vec<u8>>,
    lines: Vec<u32>,
}

impl Filemap {
    pub fn file(&self) -> &NoNullStr {
        &self.file
    }

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

pub struct LineIter<'a> {
    src: &'a Filemap,
    start: usize,
    end: usize,
    start_idx: u32,
    end_idx: u32,
}

impl<'a> LineIter<'a> {
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn last(&self )-> usize {
        self.end - 1
    }

    pub fn start_idx(&self) -> u32 {
        self.start_idx
    }

    pub fn last_idx(&self) -> u32 {
        self.end_idx
    }
}

impl<'a> Iterator for LineIter<'a> {
    type Item = (u32, &'a ByteStr);
    fn next(&mut self) -> Option<(u32, &'a ByteStr)> {
        if self.start == self.end {
            None
        } else {
            let lo = self.src.lines[self.start];
            let hi = self.src.lines[self.start+1];
            let line = &self.src.src[lo as usize..hi as usize];
            self.start += 1;
            Some((self.start as u32, line.as_byte_str()))
        }
    }
}

impl<'a> Clone for LineIter<'a> {
    fn clone(&self) -> Self {
        LineIter {
            src:       self.src,
            start:     self.start,
            end:       self.end,
            start_idx: self.start_idx,
            end_idx:   self.end_idx,
        }
    }
}
