// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

pub struct ResolDb {
    data: Vec<u8>,
}

#[derive(Copy)]
pub struct Resol<'a>(&'a [u32]);

impl<'a> Resol<'a> {
    fn get(&self, idx: usize) -> Option<Resol<'a>> {
        if self.0[idx] != 0 {
            Some(Resol(&self.0[self.0[idx] as usize..]))
        } else {
            None
        }
    }

    pub fn binary_left(&self) -> Option<Resol<'a>> {
        self.get(0)
    }

    pub fn binary_right(&self) -> Option<Resol<'a>> {
        self.get(1)
    }

    pub fn cond_cond(&self) -> Option<Resol<'a>> {
        self.get(0)
    }

    pub fn cond_if(&self) -> Option<Resol<'a>> {
        self.get(1)
    }

    pub fn cond_then(&self) -> Option<Resol<'a>> {
        self.get(2)
    }

    pub fn set_field(&self, idx: usize) -> Option<Resol<'a>> {
        self.get(idx)
    }

    pub fn test_obj(&self) -> Option<Resol<'a>> {
        self.get(0)
    }

    pub fn test_query(&self) -> Option<Resol<'a>> {
        self.get(1)
    }

    pub fn select_obj(&self) -> Option<Resol<'a>> {
        self.get(0)
    }

    pub fn select_alt(&self) -> Option<Resol<'a>> {
        self.get(1)
    }

    pub fn select_query(&self) -> Option<Resol<'a>> {
        self.get(2)
    }

    pub fn query_idx(&self, idx: usize) -> Option<Resol<'a>> {
        self.get(idx)
    }

    pub fn list_el(&self, idx: usize) -> Option<Resol<'a>> {
        self.get(idx)
    }

    pub fn let_body(&self) -> Option<Resol<'a>> {
        self.get(0)
    }

    pub fn let_field(&self, idx: usize) -> Option<Resol<'a>> {
        self.get(idx + 1)
    }

    pub fn fn_body(&self) -> Option<Resol<'a>> {
        self.get(0)
    }

    pub fn fn_arg(&self, idx: usize) -> Option<Resol<'a>> {
        self.get(idx + 1)
    }
}

pub struct ResolBuilder {
}
