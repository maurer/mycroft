//! `data` provides a facility to hold typed data in a deduped fashion, identified by a `usize`
//! key. The only requirements are that the data be `Hash` and `PartialEq`.
//! At the moment, this is done as an in-memory store.
use index::hash::HashIndex;
use std::hash::Hash;
use std::ops::Index;

/// A typed storage object which keeps deduped values of type T.
/// Keys produced by this object follow the relation:
///
/// `data[key] == data[key2]` <-> `key == key2`
pub struct Data<T> {
    inner: Vec<T>,
    index: HashIndex<T>,
}

impl<T: PartialEq + Hash> Data<T> {
    /// Constructs a fresh typed store
    pub fn new() -> Self {
        Self {
            inner: Vec::new(),
            index: HashIndex::new(),
        }
    }

    /// Returns the key for the provided data, relative to the store
    /// If the object is already in the store, it will be dropped, otherwise it will not.
    pub fn insert(&mut self, data: T) -> usize {
        match self.find(&data) {
            Some(key) => key,
            None => {
                let key = self.inner.len();
                self.index.insert(key, &data);
                self.inner.push(data);
                key
            }
        }
    }

    fn find(&self, data: &T) -> Option<usize> {
        self.index.find(data, &self.inner)
    }
}

impl<T> Index<usize> for Data<T> {
    type Output = T;
    fn index(&self, key: usize) -> &Self::Output {
        &self.inner[key]
    }
}
