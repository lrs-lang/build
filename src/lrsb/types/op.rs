// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

#[derive(Copy, Eq)]
pub enum Op {
    Impl,
    Or,
    And,
    Le,
    Ge,
    Lt,
    Gt,
    Eq,
    Ne,
    Not(u32),
    Overlay,
    Add,
    Min,
    Mul,
    Div,
    Mod,
    UnMin(u32),
    Concat,
    Test,
    Apl,
    Select,
}

impl Op {
    pub fn precedence(self) -> i8 {
        match self {
            Op::Impl      => 0,
            Op::Or        => 1,
            Op::And       => 2,

            Op::Le        => 3,
            Op::Ge        => 3,
            Op::Lt        => 3,
            Op::Gt        => 3,
            Op::Eq        => 3,
            Op::Ne        => 3,

            Op::Not(..)   => 4,
            Op::Overlay   => 5,

            Op::Add       => 6,
            Op::Min       => 6,

            Op::Mul       => 7,
            Op::Div       => 7,
            Op::Mod       => 7,

            Op::UnMin(..) => 8,
            Op::Concat    => 9,
            Op::Test      => 10,
            Op::Apl       => 11,
            Op::Select    => 12,
        }
    }

    pub fn left_assoc(self) -> bool {
        match self {
            Op::Impl      => false,
            Op::Or        => false,
            Op::And       => false,
            Op::Le        => false,
            Op::Ge        => false,
            Op::Lt        => false,
            Op::Gt        => false,
            Op::Eq        => false,
            Op::Ne        => false,
            Op::Not(..)   => false,
            Op::Overlay   => false,
            Op::Add       => true,
            Op::Min       => true,
            Op::Mul       => true,
            Op::Div       => true,
            Op::Mod       => true,
            Op::UnMin(..) => false,
            Op::Concat    => false,
            Op::Test      => false,
            Op::Apl       => true,
            Op::Select    => false,
        }
    }

    pub fn unary(self) -> bool {
        match self {
            Op::Not(..)   => true,
            Op::UnMin(..) => true,
            _ => false,
        }
    }
}
