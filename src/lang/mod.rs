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
