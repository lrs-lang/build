// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

#![crate_name = "lrsbi"]
#![feature(default_type_parameter_fallback, type_ascription)]

extern crate lrsb_types;
extern crate lrsb_lexer;
extern crate lrsb_parser;
extern crate lrsb_eval;
extern crate lrsb_funcs;

use std::io::{BufReader};
use std::fd::{STDIN, STDERR};
use std::hashmap::{HashMap};
use std::rc::{Rc};
use std::share::{RefCell};

use lrsb_types::token::{Token};
use lrsb_types::span::{Span};
use lrsb_types::tree::{Expr_};
use lrsb_types::interner::{Interner};
use lrsb_types::codemap::{Codemap};
use lrsb_parser::{Parser};
use lrsb_lexer::{Lexer};
use lrsb_eval::{Eval};

use fddiag::{FdDiag};

mod fddiag;
mod term;

fn main() {
    if let Err(e) = main_() {
        errln!("lrsbi failed: {:?}", e);
    }
}

fn main_() -> Result {
    let mut input = vec!();
    let mut stdin = BufReader::new(STDIN, 1024)?;

    let interner = Rc::new()?.set(Interner::new()?);
    let map = Rc::new()?.set(RefCell::new(Codemap::new()));
    let diag = Rc::new()?.set(FdDiag::new(map.to(), interner.to(), STDERR));
    let eval = Rc::new()?.set(Eval::new(diag.to(), interner.to()));
    let mut ids = HashMap::new()?;
    let mut iteration = 1;

    loop {
        input.truncate(0);
        term::set_char_attr(term::CharAttr::Bold, true);
        term::set_fg_color(term::Color::Green);
        print!("{} $ ", iteration);
        iteration += 1;
        term::set_fg_color(term::Color::Default);
        term::set_char_attr(term::CharAttr::Bold, false);
        if stdin.copy_until(&mut input, b'\n')? == 0 {
            break;
        }
        if input.last() != Some(&b'\n') {
            input.push(b'\n');
        }
        let input = Rc::new()?.set(input.try_to()?:Vec<_>);
        let name = format!("<{}>", iteration - 1)?.try_to()?;
        let lo = map.borrow_mut().add_file(name, input.to());
        let mut lexer = Lexer::new(lo, input.to(), diag.to(), interner.to(), "".try_to()?);
        let (first, second) = (lexer.try_peek(0)?, lexer.try_peek(1)?);
        let mut id = None;
        if let (Some(first), Some(second)) = (first, second) {
            if let (Token::Ident(i), Token::Assign) = (first.val, second.val) {
                lexer.next();
                lexer.next();
                id = Some(i);
            }
        }
        let mut parser = Parser::new(lexer, diag.to());
        let tree = match parser.parse() {
            Ok(t) => t,
            _ => continue,
        };
        if let Some(id) = id {
            ids.set(id, tree.to());
        }
        if let Err(e) = eval.force_with(&tree, &ids) {
            continue;
        }
        let res = match eval.resolve(&tree) {
            Ok(r) => r,
            _ => continue,
        };
        if let Some(id) = id {
            print!("{} = ", interner.get(id));
        }
        match *res.val.val.borrow() {
            Expr_::Integer(i) => println!("{}", i),
            Expr_::Ident(i) => println!("{}", interner.get(i)),
            Expr_::Bool(b) => println!("{:?}", b),
            Expr_::Null => println!("null"),
            Expr_::List(..) => println!("[ ... ]"),
            Expr_::Fn(..) => println!("fn"), 
            Expr_::String(s) => println!("{:?}", interner.get(s)),
            Expr_::Set(..) => println!("{{ ... }}"),
            _ => abort!(),
        };
    }
    Ok(())
}
