#![deny(missing_docs)]
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

pub mod ast;
pub mod ir;
pub mod parse;
