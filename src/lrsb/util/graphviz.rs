// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

use std::string::{AsByteStr};
use std::hashmap::{HashMap};
use std::{mem};
use std::share::{RefCell};

use types::interner::{Interner};
use types::tree::{Expr, Expr_, FnArg, Selector, FnType};

pub fn print_tree<W: Write>(e: &Expr, int: &Interner, mut w: &mut W) -> Result {
    let mut map = try!(HashMap::new());
    let mut id = 0;
    try!(write!(w, "digraph {{ graph [ordering=\"out\"];"));
    try!(print_tree_(e, int, w, &mut id, &mut map));
    write!(w, "}}")
}

fn print_tree_<W: Write>(e: &Expr, int: &Interner, mut w: &mut W, id: &mut u32,
                         map: &mut HashMap<usize, u32>) -> Result {
    macro_rules! pt {
        ($ex:expr) => { print_tree_($ex, int, w, id, map) }
    }
    macro_rules! unary {
        ($label:expr, $ex:expr) => {{
            try!(write!(w, "{} [shape=\"box\", label=\"{}\"]; {} -> {};", id, $label,
                        id, *id+1));
            *id += 1;
            pt!($ex)
        }}
    }

    macro_rules! binary {
        ($label:expr, $left:expr, $right:expr) => {{
            let sid = *id;
            try!(write!(w, "{} [shape=\"box\", label=\"{}\"]; {} -> {};", id, $label,
                        id, sid+1));
            *id += 1;
            try!(pt!($left));
            *id += 1;
            try!(write!(w, "{} -> {};", sid, id));
            pt!($right)
        }}
    }

    map.set(mem::addr::<RefCell<_>>(&*e.val), *id);

    let val = e.val.borrow();

    match *val {
        Expr_::Dummy => {
            write!(w, "{} [shape=\"box\", label=\"dummy\"];", id)
        },
        Expr_::Integer(i) => {
            write!(w, "{} [label=\"{}\"];", id, i)
        },
        Expr_::Ident(i) => {
            write!(w, "{} [label=\"{}\"];", id, int.get(i))
        },
        Expr_::String(s) => {
            write!(w, "{} [label=\"\\\"{}\\\"\"];", id, int.get(s))
        },
        Expr_::Resolved(i, ref e) => {
            if let Some(i) = i {
                try!(write!(w, "{} [label=\"{}\"];", id, int.get(i)));
            } else {
                try!(write!(w, "{} [shape=\"box\", label=\"resolved\"];", id));
            }
            if let Some(val) = map.get(&mem::addr::<RefCell<_>>(&*e.val.val)).map(|v| *v) {
                write!(w, "{} -> {};", id, val)
            } else {
                try!(write!(w, "{} -> {};", id, *id+1));
                *id += 1;
                pt!(&e.val)
            }
        },
        Expr_::PreSet(ref els, rec) => {
            if rec {
                try!(write!(w, "{} [shape=\"box\", label=\"rec set\"];", id));
            } else {
                try!(write!(w, "{} [shape=\"box\", label=\"set\"];", id));
            }
            let sid = *id;
            for el in &**els {
                *id += 1;
                try!(write!(w, "{} [label=\"{}\"];{} -> {};", id, int.get(el.0.val), sid,
                            id));
                if !el.1.val.is_dummy() {
                    try!(write!(w, "{} -> {};", id, *id+1));
                    *id += 1;
                    try!(pt!(&el.1.val));
                }
            }
            Ok(())
        },
        Expr_::Set(ref els) => {
            try!(write!(w, "{} [shape=\"box\", label=\"set\"];", id));
            let sid = *id;
            for el in &**els {
                *id += 1;
                try!(write!(w, "{} [label=\"{}\"];{} -> {};", id, int.get(el.0.val), sid,
                            id));
                try!(write!(w, "{} -> {};", id, *id+1));
                *id += 1;
                try!(pt!(&el.1.val));
            }
            Ok(())
        },
        Expr_::And(ref l, ref r) => { binary!("&&", l, r) }
        Expr_::Or(ref l, ref r) => { binary!("||", l, r) }
        Expr_::Not(ref e) => { unary!("!", e) },
        Expr_::Add(ref l, ref r) => { binary!("+", l, r) },
        Expr_::Min(ref l, ref r) => { binary!("-", l, r) },
        Expr_::Mul(ref l, ref r) => { binary!("*", l, r) },
        Expr_::Div(ref l, ref r) => { binary!("/", l, r) },
        Expr_::Mod(ref l, ref r) => { binary!("%", l, r) },
        Expr_::Gt(ref l, ref r) => { binary!(">", l, r) },
        Expr_::Lt(ref l, ref r) => { binary!("<", l, r) },
        Expr_::Ge(ref l, ref r) => { binary!(">=", l, r) },
        Expr_::Le(ref l, ref r) => { binary!("<=", l, r) },
        Expr_::Eq(ref l, ref r) => { binary!("==", l, r) },
        Expr_::Ne(ref l, ref r) => { binary!("!=", l, r) },
        Expr_::Overlay(ref l, ref r) => { binary!("//", l, r) },
        Expr_::Concat(ref l, ref r) => { binary!("++", l, r) },
        Expr_::Apl(ref l, ref r) => { binary!("$", l, r) },
        Expr_::Neg(ref e) => { unary!("-", e) },
        Expr_::Cond(ref cond, ref then, ref el) => {
            let sid = *id;
            try!(write!(w, "{} [shape=\"box\", label=\"if\"];{} -> {};", id, id,
                        sid + 1));
            *id += 1;
            try!(pt!(cond));
            *id += 1;
            try!(write!(w, "{} -> {};", sid, id));
            try!(pt!(then));
            *id += 1;
            try!(write!(w, "{} -> {};", sid, id));
            pt!(el)
        },
        Expr_::Bool(b) => {
            write!(w, "{} [label=\"{}\"];", id, b)
        },
        Expr_::Null => {
            write!(w, "{} [label=\"null\"];", id)
        },
        Expr_::Test(ref e, ref path) => {
            let sid = *id;
            try!(write!(w, "{} [shape=\"box\", label=\"test\"];{} -> {};", id, id,
                        sid + 1));
            *id += 1;
            try!(pt!(e));
            *id += 1;
            try!(write!(w, "{} -> {};", sid, id));
            try!(write!(w, "{} [label=\"", id));
            for seg in &path[..path.len()-1] {
                match seg.val {
                    Selector::Ident(i) => try!(write!(w, "{}.", int.get(i))),
                    Selector::Integer(i) => try!(write!(w, "{}.", i)),
                    Selector::Expr(_) => try!(write!(w, "<expr>.")),
                }
            }
            match path[path.len()-1].val {
                Selector::Ident(i) => try!(write!(w, "{}", int.get(i))),
                Selector::Integer(i) => try!(write!(w, "{}", i)),
                Selector::Expr(_) => try!(write!(w, "<expr>")),
            }
            write!(w, "\"];")
        },
        Expr_::Select(ref e, ref path, ref alt) => {
            // XXX: This shares almost all code with the previous branch.
            let sid = *id;
            try!(write!(w, "{} [shape=\"box\", label=\"select\"];{} -> {};", id, id,
                        sid + 1));
            *id += 1;
            try!(pt!(e));
            *id += 1;
            try!(write!(w, "{} -> {};", sid, id));
            try!(write!(w, "{} [label=\"", id));
            for seg in &path[..path.len()-1] {
                match seg.val {
                    Selector::Ident(i) => try!(write!(w, "{}.", int.get(i))),
                    Selector::Integer(i) => try!(write!(w, "{}.", i)),
                    Selector::Expr(_) => try!(write!(w, "<expr>.")),
                }
            }
            match path[path.len()-1].val {
                Selector::Ident(i) => try!(write!(w, "{}", int.get(i))),
                Selector::Integer(i) => try!(write!(w, "{}", i)),
                Selector::Expr(_) => try!(write!(w, "<expr>")),
            }
            try!(write!(w, "\"];"));
            if let Some(ref alt) = *alt {
                *id += 1;
                try!(write!(w, "{} -> {};", sid, id));
                try!(pt!(alt));
            }
            Ok(())
        },
        Expr_::Stringify(ref e) => { unary!("stringify", e) },
        Expr_::List(ref els) => {
            try!(write!(w, "{} [shape=\"box\", label=\"list\"];", id));
            let sid = *id;
            for el in els {
                *id += 1;
                try!(write!(w, "{} -> {};", sid, id));
                try!(pt!(el));
            }
            Ok(())
        },
        Expr_::Let(ref lets, ref e) => {
            let sid = *id;
            *id += 1;
            let vid = *id;
            try!(write!(w, "{} [shape=\"box\", label=\"let\"]; {} -> {};", sid, sid,
                        vid));
            try!(write!(w, "{} [shape=\"box\", label=\"vars\"];", vid));
            for el in lets {
                *id += 1;
                try!(write!(w, "{} [label=\"{}\"];{} -> {}; {} -> {};", id,
                            int.get(el.0), vid, id, id, *id+1));
                *id += 1;
                try!(pt!(&el.1.val));
            }
            *id += 1;
            try!(write!(w, "{} -> {};", sid, id));
            pt!(e)
        },
        Expr_::Fn(FnType::Normal(ref arg, ref body)) => {
            let sid = *id;
            *id += 1;
            try!(write!(w, "{} [shape=\"box\", label=\"fn\"]; {} -> {};", sid, sid,
                        id));
            match arg.val {
                FnArg::Ident(i) => {
                    try!(write!(w, "{} [label=\"{}\"];", id, int.get(i)))
                },
                FnArg::Pat(i, ref args, wild) => {
                    let name = match i {
                        Some(i) => int.get(i.val),
                        _ => "".as_byte_str(),
                    };
                    try!(write!(w, "{} [shape=\"box\", label=\"{} @\"];", id, name));
                    let aid = *id;
                    for arg in args {
                        *id += 1;
                        try!(write!(w, "{} [label=\"{}\"];{} -> {};", id,
                                    int.get(arg.0.val), aid, id));
                    }
                    if wild {
                        *id += 1;
                        try!(write!(w, "{} [label=\"..\"];{} -> {};", id, aid, id));
                    }
                },
            }
            *id += 1;
            try!(write!(w, "{} -> {};", sid, id));
            pt!(body)
        },
        Expr_::Fn(FnType::BuiltIn(..)) => {
            write!(w, "{} [shape=\"box\", label=\"built-in fn\"];", *id)
        },
    }
}
