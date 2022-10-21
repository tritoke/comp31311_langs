#![deny(missing_docs)]

//! Library relating to parsing and operating on ASTs for different languages
//! I come across in my "giving meaning to programs" class at university

extern crate core;

/// module implementing the untyped lambda calculus
pub mod lambda;

/// module implementing the simply typed lambda calculus
pub mod typed_lambda;
