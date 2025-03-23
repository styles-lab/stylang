//! The intermediate representation of the stylang.

mod attrs;
pub use attrs::*;

mod layout;
pub use layout::*;

mod value;
pub use value::*;

mod nav;
pub use nav::*;

mod views;
pub use views::*;
