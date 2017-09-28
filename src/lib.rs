#![deny(missing_docs)]
//! `mycroft` is an in-memory, Datalog + callbacks library.
//!
//! Currently, all it can do is parse predicates.
//!
//! `mycroft` is in work-in-progress status, and is not even suitable for experimental use.
//!
#[macro_use]
extern crate combine;
pub mod ast;
pub mod parse;
