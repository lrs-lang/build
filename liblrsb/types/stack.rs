// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

use tree::{Expr, Expr_, SExpr};
use op::{Op};
use span::{Span, Spanned};

/// A stack for combining a stream of expressions and operators to a single expression
/// that respects the operator precedence.
///
/// = Remarks
///
/// E.g., `a + b * c + d == `(a + (b * c)) + d`. 
///
/// Uses the shunting-yard algorithm.
pub struct Stack {
    expr: Vec<SExpr>,
    op: Vec<Op>,
}

impl Stack {
    /// Creates a new stack.
    pub fn new() -> Stack {
        Stack {
            expr: Vec::new(),
            op: Vec::new(),
        }
    }

    /// Pushes an expression onto the stack.
    pub fn push_expr(&mut self, expr: SExpr) {
        self.expr.push(expr);
    }

    /// Pops an expression from the stack.
    ///
    /// = Remarks
    ///
    /// Panics if there are no expressions on the stack.
    pub fn pop_expr(&mut self) -> SExpr {
        self.expr.pop().unwrap()
    }

    /// Pushes an operator onto the stack.
    pub fn push_op(&mut self, nop: Op) {
        self.next_op(nop);
        self.op.push(nop);
    }

    /// Announces the next operator.
    ///
    /// = Remarks
    ///
    /// This causes expressions and operators that are already on the stack to be combined
    /// to a single expression if this is what would happen if the operator were pushed
    /// onto the stack. This new expression can then be popped off of the stack.
    ///
    /// This function is necessary because we want to handle certain operators inside the
    /// parser instead of the stack. See the comment in `combine_unary` below.
    pub fn next_op(&mut self, nop: Op) {
        let nprec = nop.precedence();
        while self.op.len() > 0 {
            let op = *self.op.last().unwrap();
            let prec = op.precedence();
            if prec > nprec || (prec >= nprec && op.left_assoc()) {
                self.combine();
            } else {
                break;
            }
        }
    }

    /// Combines all expressions and operators that are on the stack into a single
    /// expression.
    pub fn clear(&mut self) -> SExpr {
        while self.op.len() > 0 {
            self.combine();
        }
        assert!(self.expr.len() == 1);
        self.expr.pop().unwrap()
    }

    /// Pops an operator and one (in the case of a unary operator) or two (in the case of
    /// a binary operator) off of the stack to combine them.
    fn combine(&mut self) {
        let op = *self.op.last().unwrap();
        if op.unary() {
            self.combine_unary();
        } else {
            self.combine_binary();
        }
    }

    fn combine_binary(&mut self) {
        let op = self.op.pop().unwrap();
        let right = self.expr.pop().unwrap();
        let left = self.expr.pop().unwrap();
        let expr: fn(SExpr, SExpr) -> Expr_ = match op {
            Op::Impl => Expr_::Impl,
            Op::Or => Expr_::Or,
            Op::And => Expr_::And,
            Op::Le => Expr_::Le,
            Op::Ge => Expr_::Ge,
            Op::Lt => Expr_::Lt,
            Op::Gt => Expr_::Gt,
            Op::Eq => Expr_::Eq,
            Op::Ne => Expr_::Ne,
            Op::Overlay => Expr_::Overlay,
            Op::Add => Expr_::Add,
            Op::Sub => Expr_::Sub,
            Op::Mul => Expr_::Mul,
            Op::Div => Expr_::Div,
            Op::Mod => Expr_::Mod,
            Op::Concat => Expr_::Concat,
            Op::Apl => Expr_::Apl,

            // these are not handled via the stack but directly in the parser
            Op::Select | Op::Test => abort!(),

            // these are handled in combine_unary
            Op::Not(..) | Op::UnMin(..) => abort!(),
        };
        let span = Span::new(left.span.lo, right.span.hi);
        let expr = Spanned::new(span, Expr::new(expr(left, right)));
        self.expr.push(expr);
    }

    fn combine_unary(&mut self) {
        let op = self.op.pop().unwrap();
        let arg = self.expr.pop().unwrap();
        let (lo, expr): (_, fn(SExpr) -> Expr_) = match op {
            Op::Not(lo) => (lo, Expr_::Not),
            Op::UnMin(lo) => (lo, Expr_::Neg),

            // the rest is handled in combine_binary
            _ => abort!(),
        };
        let span = Span::new(lo, arg.span.hi);
        let expr = Spanned::new(span, Expr::new(expr(arg)));
        self.expr.push(expr);
    }
}
