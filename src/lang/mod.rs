//! The compiler fontend for `stylang`

mod errors;
pub use errors::*;

mod input;
pub use input::*;

mod color;
pub use color::*;

mod digits;
pub use digits::*;

mod unit;
pub use unit::*;

mod punct;
pub use punct::*;

mod delimiter;
pub use delimiter::*;

mod s;
pub use s::*;

mod func;
pub use func::*;

mod types;
pub use types::*;

mod attr;
pub use attr::*;

mod ident;
pub use ident::*;

mod call;
pub use call::*;

mod string;
pub use string::*;

mod lit;
pub use lit::*;

mod complex;
pub use complex::*;

mod mods;
pub use mods::*;

mod visibility;
pub use visibility::*;

mod path;
pub use path::*;

mod stmt;
pub use stmt::*;

mod parse;
pub use parse::*;

mod patt;
pub use patt::*;
