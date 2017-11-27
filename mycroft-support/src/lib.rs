#![deny(missing_docs)]
//! `mycroft-support` is a crate with code intended to be used for the runtime of programs
//! containing `mycroft` generated code. If you're not developing `mycroft`, you probably don't
//! want these docs. `mycroft` contains the compiler, `mycroft-macros` contains the plugin to run
//! the compiler during rust compilation, this crate just contains functions and structures used by
//! the generated code.
//!
//! If you are developing `mycroft`, or just don't care about an interface not made for you,
//! welcome, this crate essentially implements the datalog backend. This includes database-like
//! functionality and eventually rule execution management.
pub mod storage;
pub mod index;
pub mod join;
pub mod derivation;
