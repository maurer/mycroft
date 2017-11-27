#![deny(missing_docs)]
#![recursion_limit = "256"]
//! `mycroft` is an in-memory, Datalog + callbacks library.
//!
//! Currently, all it can do is parse predicates.
//!
//! `mycroft` is in work-in-progress status, and is not even suitable for experimental use.
//!
#[macro_use]
extern crate combine;
#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate quote;
extern crate syn;

pub mod ast;
pub mod codegen;
pub mod ir;
pub mod parse;
