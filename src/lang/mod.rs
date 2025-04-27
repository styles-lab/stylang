//! The compiler fontend for `stylang`

mod errors;
pub use errors::*;

mod input;
pub use input::*;

mod token;
pub use token::*;

mod num;
pub use num::*;

mod color;
pub use color::*;

mod string;
pub use string::*;

mod lit;
pub use lit::*;

mod vs;
pub use vs::*;

mod ty;

mod punct;
pub use punct::*;

mod ident;
pub use ident::*;

mod expr;

mod meta;
pub use meta::*;

mod item;
pub use item::*;

mod patt;
