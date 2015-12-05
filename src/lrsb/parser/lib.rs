// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

#![crate_name = "lrsb_parser"]
#![crate_type = "lib"]

extern crate lrsb_types as types;
extern crate lrsb_lexer as lexer;

use std::{error};
use std::rc::{Rc};

use lexer::{Lexer};
use types::diagnostic::{Diagnostic};
use types::interner::{Interned};
use types::tree::{Expr, Expr_, SExpr, FnArg, Selector, FnType};
use types::token::{Token};
use types::span::{Span, Spanned};
use types::op::{Op};
use types::stack::{Stack};

/// An expression parser.
pub struct Parser<D: Diagnostic> {
    lexer: Lexer<D>,
    diagnostic: Rc<D>,
}

impl<D: Diagnostic> Parser<D> {
    /// Creates a new parser.
    ///
    /// [argument, lexer]
    /// The lexer from which tokens will be read.
    ///
    /// [argument, diagnostic]
    /// The destination for diagnostic messages.
    pub fn new(lexer: Lexer<D>, diagnostic: Rc<D>) -> Parser<D> {
        Parser {
            lexer: lexer,
            diagnostic: diagnostic,
        }
    }

    /// Parses the content of the parser.
    ///
    /// = Remarks
    ///
    /// This succeeds if the whole content of the lexer is a single valid expression.
    pub fn parse(&mut self) -> Result<SExpr> {
        let expr = try!(self.parse_expr());
        if let Some(t) = try!(self.lexer.try_next()) {
            self.diagnostic.error(t.span , |mut w| {
                write!(w, "expected end-of-file, found `{:?}`", t.val)
            });
            return Err(error::InvalidSequence);
        }
        Ok(expr)
    }

    /// Parses an expression.
    fn parse_expr(&mut self) -> Result<SExpr> {
        let mut stack = Stack::new();

        'outer: loop {
            // Step 1: Check for unary operators.
            loop {
                let cur = try!(self.lexer.peek(0));
                if match cur.val {
                    Token::Not => { stack.push_op(Op::Not(cur.span.lo)); true },
                    Token::Minus => { stack.push_op(Op::UnMin(cur.span.lo)); true },
                    _ => false,
                } {
                    self.lexer.next();
                } else {
                    break;
                }
            }

            // Step 2: Read an atomic expression.
            stack.push_expr(try!(self.parse_atomic()));

            // Step 3: Read an operator.
            //
            // This is a loop because of the two special cases below which are effectively
            // unary postfix operators. Those are followed by an operator or end the
            // expression.
            loop {
                if try!(self.lexer.eof()) { break 'outer; }

                let next = try!(self.lexer.peek(0));
                let op = match next.val {
                    Token::Implies      => Op::Impl,
                    Token::OrOr         => Op::Or,
                    Token::AndAnd       => Op::And,
                    Token::Le           => Op::Le,
                    Token::Ge           => Op::Ge,
                    Token::Lt           => Op::Lt,
                    Token::Gt           => Op::Gt,
                    Token::Equals       => Op::Eq,
                    Token::Unequal      => Op::Ne,
                    Token::Overlay      => Op::Overlay,
                    Token::Plus         => Op::Add,
                    Token::Minus        => Op::Min,
                    Token::Times        => Op::Mul,
                    Token::Div          => Op::Div,
                    Token::Mod          => Op::Mod,
                    Token::Concat       => Op::Concat,
                    Token::Questionmark => Op::Test,
                    Token::Dot          => Op::Select,
                    t if t.starts_expr() => Op::Apl,
                    _ => break 'outer,
                };

                if op != Op::Apl {
                    // Apl is not a token.

                    self.lexer.next();
                }

                if op == Op::Test {
                    // Test has the form
                    //
                    // ----
                    // e ? i1.i2.i3
                    // ----

                    stack.next_op(op);
                    let expr = stack.pop_expr();
                    let path = try!(self.parse_attr_path());
                    let span = Span::new(expr.span.lo, path.last().unwrap().span.hi);
                    let expr = Spanned::new(span, Expr::new(Expr_::Test(expr, path)));
                    stack.push_expr(expr);
                } else if op == Op::Select {
                    // Select has the form
                    //
                    // ----
                    // e.i1.i2.i3
                    // ----
                    //
                    // or
                    //
                    // ----
                    // e1.i1.i2.i3 or e2
                    // ----

                    stack.next_op(op);
                    let expr = stack.pop_expr();
                    let path = try!(self.parse_attr_path());
                    let (hi, alt) = match try!(self.lexer.try_peek(0)) {
                        Some(Spanned { val: Token::Or, .. }) => {
                            self.lexer.next();
                            let alt = try!(self.parse_expr());
                            (alt.span.hi, Some(alt))
                        },
                        _ => (path.last().unwrap().span.hi, None),
                    };
                    let span = Span::new(expr.span.lo, hi);
                    let expr = Expr_::Select(expr, path, alt);
                    let expr = Spanned::new(span, Expr::new(expr));
                    stack.push_expr(expr);
                } else {
                    stack.push_op(op);
                    break;
                }
            }
        }

        Ok(stack.clear())
    }

    /// Parses an atomic expression.
    ///
    /// = Remarks
    ///
    /// An atomic expression is one that can be recognized with a statically known amount
    /// of lookahead. For example, an Integer can be recognized with 1 token lookahead.
    ///
    /// `e1 || e2` is not an atomic expression since `e1` can consist of arbitrarily many
    /// tokens. Neither are expressions starting with the unary `-` and `!` operators
    /// since they are subject to precedence considerations.
    ///
    /// == Atomic expressions
    ///
    /// * Function
    /// * Integer
    /// * Ident
    /// * Bool
    /// * Null
    /// * (recursive) Set
    /// * List
    /// * Let binding
    /// * Conditional
    /// * `(expr)`
    fn parse_atomic(&mut self) -> Result<SExpr> {
        let token = try!(self.lexer.peek(0));

        if !token.val.starts_expr() {
            self.diagnostic.error(token.span , |mut w| {
                write!(w, "expected expression, found `{:?}`", token.val)
            });
            return Err(error::InvalidSequence);
        }


        // Check if this is a function definition.
        match token.val {
            Token::Ident(..) => {
                match try!(self.lexer.try_peek(1)) {
                    // ident:
                    Some(Spanned { val: Token::Colon, .. }) => return self.parse_fn(),
                    // ident @
                    Some(Spanned { val: Token::At, .. }) => return self.parse_fn(),
                    _ => { }
                }
            },
            Token::LeftBrace => {
                if let Some(one) = try!(self.lexer.try_peek(1)) {
                    if one.val == Token::DotDot {
                        // { ..            }
                        return self.parse_fn();
                    } else if let Token::Ident(..) = one.val {
                        if let Some(two) = try!(self.lexer.try_peek(2)) {
                            if two.val == Token::Comma || two.val == Token::Questionmark {
                                // { ident,                }
                                // { ident ?               }
                                return self.parse_fn();
                            }
                        }
                    } else if one.val == Token::RightBrace {
                        if let Some(two) = try!(self.lexer.try_peek(2)) {
                            if two.val == Token::Colon {
                                // { }:
                                return self.parse_fn();
                            }
                        }
                    }
                }
            },
            _ => { },
        }

        match token.val {
            Token::Integer(..) | Token::Ident(..) | Token::True | Token::False |
                Token::Null | Token::String(..) => self.parse_simple(),
            Token::StringPart(..) => self.parse_string_part(),
            Token::Rec => self.parse_set(),
            Token::Let => self.parse_let(),
            Token::If => self.parse_conditional(),
            Token::LeftBracket => self.parse_list(),
            Token::LeftBrace => self.parse_set(),
            Token::LeftParen => {
                let opening = self.lexer.next().unwrap();
                let mut expr = try!(self.parse_expr());
                let closing = try!(self.lexer.next_right_paren());
                expr.span.lo = opening.span.lo;
                expr.span.hi = closing.span.hi;
                Ok(expr)
            },
            _ => abort!(),
        }
    }

    fn parse_string_part(&mut self) -> Result<SExpr> {
        let token = try!(self.lexer.next());
        let mut expr = match token.val {
            Token::StringPart(s) => Spanned::new(token.span,
                                                 Expr::new(Expr_::String(s))),
            _ => abort!(),
        };
        let mut end = false;
        while !end {
            let tmp = try!(self.parse_expr());
            let span = Span::new(expr.span.lo, tmp.span.hi);
            let sfy = Spanned::new(tmp.span, Expr::new(Expr_::Stringify(tmp)));
            expr = Spanned::new(span, Expr::new(Expr_::Concat(expr, sfy)));

            let tmp = try!(self.lexer.cont_string());
            let id = match tmp.val {
                Token::StringPart(id) => id,
                Token::String(id) => { end = true; id },
                _ => abort!(),
            };
            let tmp = Spanned::new(tmp.span, Expr::new(Expr_::String(id)));
            let span = Span::new(expr.span.lo, tmp.span.hi);
            expr = Spanned::new(span, Expr::new(Expr_::Concat(expr, tmp)));
        }
        Ok(expr)
    }

    fn parse_selector(&mut self) -> Result<Spanned<Selector>> {
        let next = try!(self.lexer.next());
        let sel = match next.val {
            Token::Ident(i) => Selector::Ident(i),
            Token::Integer(i) => {
                if (isize::max() as i64) < i {
                    self.diagnostic.error(next.span , |mut w| {
                        write!(w, "out of bounds")
                    });
                    return Err(error::InvalidSequence);
                }
                Selector::Integer(i as usize)
            }
            Token::LeftParen => {
                let expr = try!(self.parse_expr());
                try!(self.lexer.next_right_paren());
                Selector::Expr(expr)
            }
            _ => {
                self.diagnostic.error(next.span , |mut w| {
                    write!(w, "expected Integer, Identifier, or `(`, found {:?}",
                           next.val)
                });
                return Err(error::InvalidSequence);
            },
        };
        Ok(Spanned::new(next.span, sel))
    }

    /// Parses an attribute path.
    ///
    /// = Remarks
    ///
    /// An attribute path has the following form:
    ///
    /// ----
    /// i1.i2.i3
    /// ----
    fn parse_attr_path(&mut self) -> Result<Rc<Vec<Spanned<Selector>>>> {
        let mut path = Vec::new();
        path.push(try!(self.parse_selector()));
        loop {
            let next = match try!(self.lexer.try_peek(0)) {
                Some(n) => n,
                _ => break,
            };
            match next.val {
                Token::Dot => self.lexer.next(),
                _ => break,
            };
            path.push(try!(self.parse_selector()));
        }
        path.shrink_to_fit();
        Ok(Rc::new().unwrap().set(path))
    }

    /// Parses a function.
    ///
    /// = Remarks
    ///
    /// The lexer should be at the start of a function when this function is invoked.
    ///
    /// Functions have the following forms:
    ///
    /// ----
    /// i: e
    /// ----
    ///
    /// ----
    /// { i1 ? e1, i2 ? e2, }: e3
    /// ----
    ///
    /// Where the `? e` parts and the last comma are optional.
    ///
    /// ----
    /// i @ { }: e
    /// ----
    ///
    /// Where the body of the `{ }` is as above.
    fn parse_fn(&mut self) -> Result<SExpr> {
        let first = self.lexer.peek(0).unwrap();

        if let Token::Ident(id) = first.val {
            let second = self.lexer.peek(1).unwrap();

            // First case above.
            if second.val == Token::Colon {
                self.lexer.next();
                self.lexer.next();
                let body = try!(self.parse_expr());
                let arg = FnArg::Ident(id);
                let span = Span::new(first.span.lo, body.span.hi);
                let expr = Expr_::Fn(FnType::Normal(Spanned::new(first.span, arg), body));
                return Ok(Spanned::new(span, Expr::new(expr)));
            }

            // Third case above.
            if second.val == Token::At {
                self.lexer.next();
                self.lexer.next();
                let (pat_span, pat, wild) = try!(self.parse_fn_pat());
                try!(self.lexer.next_colon());
                let body = try!(self.parse_expr());
                let arg_span = Span::new(first.span.lo, pat_span.hi);
                let arg = FnArg::Pat(Some(Spanned::new(first.span, id)), pat, wild);
                let span = Span::new(first.span.lo, body.span.hi);
                let expr = Expr_::Fn(FnType::Normal(Spanned::new(arg_span, arg), body));
                return Ok(Spanned::new(span, Expr::new(expr)));
            }

            self.diagnostic.error(second.span , |mut w| {
                write!(w, "expected `:` or `@`, found `{:?}`", second.span)
            });
            return Err(error::InvalidSequence);
        }

        // Second case above.
        if first.val == Token::LeftBrace {
            let (pat_span, pat, wild) = try!(self.parse_fn_pat());
            try!(self.lexer.next_colon());
            let body = try!(self.parse_expr());
            let arg = FnArg::Pat(None, pat, wild);
            let span = Span::new(pat_span.lo, body.span.hi);
            let expr = Expr_::Fn(FnType::Normal(Spanned::new(pat_span, arg), body));
            return Ok(Spanned::new(span, Expr::new(expr)));
        }

        // Cannot happen
        abort!();
    }

    /// Parses a function argument pattern.
    ///
    /// [return_value]
    /// Returns (the span of the pattern, the bound variables and optional alternatives,
    /// whether the pattern has a wildcard).
    ///
    /// = Remarks
    ///
    /// Patterns have the following form.
    ///
    /// ----
    /// { i1 ? e1, i2 ? e2, .. }
    /// ----
    fn parse_fn_pat(&mut self) ->
        Result<(Span, Rc<Vec<(Spanned<Interned>, Option<SExpr>)>>, bool)>
    {
        let opening = self.lexer.next().unwrap();
        let mut vars = Vec::new();
        let mut wild = false;

        loop {
            let ident = try!(self.lexer.peek(0));
            let ident = match ident.val {
                Token::Ident(i) => {
                    self.lexer.next();
                    Spanned::new(ident.span, i)
                },
                Token::DotDot => {
                    self.lexer.next();
                    wild = true;
                    break;
                },
                _ => {
                    self.diagnostic.error(ident.span , |mut w| {
                        write!(w, "expected ident or `..`, found `{:?}`", ident.val)
                    });
                    return Err(error::InvalidSequence);
                },
            };

            let next = try!(self.lexer.peek(0));
            let alt = match next.val {
                Token::Questionmark => {
                    self.lexer.next();
                    Some(try!(self.parse_expr()))
                },
                _ => None,
            };

            vars.push((ident, alt));

            let next = try!(self.lexer.peek(0));
            match next.val {
                Token::Comma => self.lexer.next(),
                Token::RightBrace => break,
                _ => {
                    self.diagnostic.error(next.span , |mut w| {
                        write!(w, "expected `,` or `}}`, found `{:?}`", next.val)
                    });
                    return Err(error::InvalidSequence);
                },
            };
        }

        let closing = try!(self.lexer.next_right_brace());
        let span = Span::new(opening.span.lo, closing.span.hi);
        vars.shrink_to_fit();
        Ok((span, Rc::new().unwrap().set(vars), wild))
    }

    /// Parses a simple expression.
    ///
    /// = Remarks
    ///
    /// Simple expressions are those that consist of a single token. The lexer should
    /// currently be at one of those tokens or the process will be aborted.
    ///
    /// Simple expressions are
    ///
    /// * Integers
    /// * Identifiers
    /// * Booleans
    /// * Null
    fn parse_simple(&mut self) -> Result<SExpr> {
        let t = self.lexer.next().unwrap();
        let expr = match t.val {
            Token::Integer(i) => Expr_::Integer(i),
            Token::Ident(i) => Expr_::Ident(i),
            Token::True => Expr_::Bool(true),
            Token::False => Expr_::Bool(false),
            Token::Null => Expr_::Null,
            Token::String(s) => Expr_::String(s),
            _ => abort!(),
        };
        Ok(Spanned::new(t.span, Expr::new(expr)))
    }

    /// Parses a let binding.
    ///
    /// = Remarks
    ///
    /// When this function is invoked, the next token in the lexer should be `let`.
    ///
    /// A let binding has the following form:
    ///
    /// ----
    /// let
    ///     i1 = e1;
    ///     i2 = e2;
    /// in
    ///     e3
    /// ----
    fn parse_let(&mut self) -> Result<SExpr> {
        let let_ = self.lexer.next().unwrap();
        let mut bindings = Vec::new();
        loop {
            if try!(self.lexer.peek(0)).val == Token::In {
                self.lexer.next();
                break;
            }
            let (_, name) = try!(self.lexer.next_ident());
            try!(self.lexer.next_assign());
            let expr = try!(self.parse_expr());
            try!(self.lexer.next_semicolon());
            bindings.push((name, expr));
        }
        let expr = try!(self.parse_expr());
        let span = Span::new(let_.span.lo, expr.span.hi);
        bindings.shrink_to_fit();
        let expr = Expr_::Let(Rc::new().unwrap().set(bindings), expr);
        Ok(Spanned::new(span, Expr::new(expr)))
    }

    /// Parses a conditional.
    ///
    /// = Remarks
    ///
    /// When this function is invoked, the next token in the lexer should be `if`.
    ///
    /// A conditional has the following form:
    ///
    /// ----
    /// if
    ///     e1
    /// then
    ///     e2
    /// else
    ///     e3
    /// ----
    fn parse_conditional(&mut self) -> Result<SExpr> {
        let if_ = self.lexer.next().unwrap();
        let e1 = try!(self.parse_expr());
        try!(self.lexer.next_then());
        let e2 = try!(self.parse_expr());
        try!(self.lexer.next_else());
        let e3 = try!(self.parse_expr());
        let span = Span::new(if_.span.lo, e3.span.hi);
        Ok(Spanned::new(span, Expr::new(Expr_::Cond(e1, e2, e3))))
    }

    /// Parses a list.
    ///
    /// = Remarks
    ///
    /// When this function is invoked, the next token in the lexer should be `[`.
    ///
    /// A list has the following form:
    ///
    /// ----
    /// [
    ///     e1,
    ///     e2,
    ///     e3,
    /// ]
    /// ----
    ///
    /// The last comma is optional.
    fn parse_list(&mut self) -> Result<SExpr> {
        let start = self.lexer.next().unwrap();
        let mut els = Vec::new();
        loop {
            if try!(self.lexer.peek(0)).val == Token::RightBracket {
                break;
            }
            els.push(try!(self.parse_expr()));
            let next = try!(self.lexer.peek(0));
            match next.val {
                Token::Comma => self.lexer.next(),
                Token::RightBracket => break,
                _ => {
                    self.diagnostic.error(next.span , |mut w| {
                        write!(w, "expected `,` or `]`, found `{:?}`", next.val)
                    });
                    return Err(error::InvalidSequence);
                },
            };
        }
        let end = self.lexer.next().unwrap();
        let span = Span::new(start.span.lo, end.span.hi);
        els.shrink_to_fit();
        Ok(Spanned::new(span, Expr::new(Expr_::List(Rc::new().unwrap().set(els)))))
    }

    /// Parses a set.
    ///
    /// = Remarks
    ///
    /// When this function is invoked, the next token in the lexer should be `rec` or `{`.
    ///
    /// A set has the following form:
    ///
    /// ----
    /// rec # optional
    /// {
    ///     i1 = e1,
    ///     i2 = e2,
    /// }
    /// ----
    ///
    /// The last comma is optional.
    fn parse_set(&mut self) -> Result<SExpr> {
        let opening = self.lexer.next().unwrap();
        let rec = match opening.val {
            Token::Rec => {
                try!(self.lexer.next_left_brace());
                true
            },
            _ => false
        };
        let mut fields = Vec::new();
        loop {
            if try!(self.lexer.peek(0)).val == Token::RightBrace {
                break;
            }

            let next = try!(self.lexer.next());
            match next.val {
                Token::Ident(ident) => {
                    try!(self.lexer.next_assign());
                    let expr = try!(self.parse_expr());
                    fields.push((Spanned::new(next.span, ident), expr));
                },
                Token::Inherit => {
                    loop {
                        let next = try!(self.lexer.peek(0));
                        match next.val {
                            Token::Comma | Token::RightBrace => break,
                            Token::Ident(ident) => {
                                self.lexer.next();
                                let e = Spanned::new(next.span, Expr::new(Expr_::Dummy));
                                fields.push((Spanned::new(next.span, ident), e));
                            },
                            _ => {
                                self.diagnostic.error(next.span , |mut w| {
                                    write!(w, "expected Ident or `,` or `}}`, found `{:?}`",
                                           next.val)
                                });
                                return Err(error::InvalidSequence);
                            },
                        }
                    }
                },
                _ => {
                    self.diagnostic.error(next.span , |mut w| {
                        write!(w, "expected Ident or `inherit`, found `{:?}`", next.val)
                    });
                    return Err(error::InvalidSequence);
                },
            }

            let next = try!(self.lexer.peek(0));
            match next.val {
                Token::Comma => self.lexer.next(),
                Token::RightBrace => break,
                _ => {
                    self.diagnostic.error(next.span , |mut w| {
                        write!(w, "expected `,` or `}}`, found `{:?}`", next.val)
                    });
                    return Err(error::InvalidSequence);
                },
            };
        }
        let closing = try!(self.lexer.next_right_brace());
        let span = Span::new(opening.span.lo, closing.span.hi);
        fields.shrink_to_fit();
        Ok(Spanned::new(span, Expr::new(Expr_::PreSet(Rc::new().unwrap().set(fields), rec))))
    }
}
