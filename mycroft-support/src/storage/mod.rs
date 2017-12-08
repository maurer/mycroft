//! `storage` provides utilities for keeping storing and retrieving data, notably `Data` and
//! `Tuples`
//! Both are in-memory stores at the moment, but this module is where future disk stores will live
//! as well.
pub mod tuple;
pub mod data;

pub use self::tuple::{MergeRef, Provenance, Tuples};
pub use self::data::Data;
