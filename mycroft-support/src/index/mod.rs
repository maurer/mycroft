//! `index` provides ways to index data contained in the structures from `storage`. At the moment,
//! there's only `HashIndex` here, but eventually it should include an MVCC tree.
pub mod hash;
pub use self::hash::HashIndex;
