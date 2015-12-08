// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

/// An operation on an expression.
#[derive(Copy, Eq)]
pub enum Op {
    /// Implication, `(e1 => e2) <=> (!e1 || e2)`
    Impl,
    Or,
    And,
    Le,
    Ge,
    Lt,
    Gt,
    Eq,
    Ne,
    /// `!`. The argument is the index of the `!` in the codemap.
    Not(u32),
    /// `\\`
    Overlay,
    Add,
    Sub,
    Mul,
    Div,
    /// `%`
    Mod,
    /// Unary minus. The argument is the index of the `!` in the codemap.
    UnMin(u32),
    /// `++`
    Concat,
    /// `?`
    Test,
    /// `e1 e2`
    Apl,
    /// `e1.e2`
    Select,
}

impl Op {
    /// Returns the precedence of an operator.
    ///
    /// = Remarks
    ///
    /// Higher precedence means that the operator binds tighter than operators with lower
    /// precedence.
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
            Op::Sub       => 6,

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

    /// Returns whether the operator is left-associative.
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
            Op::Sub       => true,
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

    /// Returns whether the operator is unary.
    pub fn unary(self) -> bool {
        match self {
            Op::Not(..)   => true,
            Op::UnMin(..) => true,
            _ => false,
        }
    }
}
