// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

#![crate_name = "lrsb_types"]
#![crate_type = "lib"]
#![feature(custom_derive)]
#![feature(default_type_parameter_fallback)]

pub mod codemap;
pub mod interner;
pub mod op;
pub mod scope;
pub mod span;
pub mod stack;
pub mod token;
pub mod tree;
pub mod diagnostic;
