// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

use std::fd::{STDERR};

#[allow(dead_code)]
#[derive(Copy)]
pub enum Color {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    Default,
}

impl Color {
    fn as_u8(self) -> u8 {
        match self {
            Color::Black   => 0,
            Color::Red     => 1,
            Color::Green   => 2,
            Color::Yellow  => 3,
            Color::Blue    => 4,
            Color::Magenta => 5,
            Color::Cyan    => 6,
            Color::White   => 7,
            Color::Default => 9,
        }
    }
}

pub enum CharAttr {
    Bold,
}

pub fn set_char_attr(attr: CharAttr, val: bool) {
    write!(STDERR, "\x1B[{}m", attr as u8 + 1 + !val as u8 * 20);
}

pub fn set_fg_color(c: Color) {
    write!(STDERR, "\x1B[{}m", 30 + c.as_u8());
}
