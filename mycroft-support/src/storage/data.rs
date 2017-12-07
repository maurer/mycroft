//! `data` provides a facility to hold typed data in a deduped fashion, identified by a `usize`
//! key. The only requirements are that the data be `Hash` and `PartialEq`.
//! At the moment, this is done as an in-memory store.
use index::hash::HashIndex;
use std::hash::Hash;
use std::ops::Index;
use std::rc::Rc;
use std::cell::UnsafeCell;

/// A typed storage object which keeps deduped values of type T.
/// Keys produced by this object follow the relation:
///
/// `data[key] == data[key2]` <-> `key == key2`
pub struct Data<T> {
    raw: Rc<UnsafeCell<RawData<T>>>,
}

impl<T> Clone for Data<T> {
    fn clone(&self) -> Self {
        Self {
            raw: self.raw.clone(),
        }
    }
}

#[derive(Debug, Eq, Clone, PartialEq)]
enum Usage {
    Read(usize),
    Write,
}

struct RawData<T> {
    inner: Vec<T>,
    index: HashIndex<T>,
    //TODO: Once multithreaded, Usage needs to be locked
    usage: Usage,
}

impl<T: PartialEq + Hash> Data<T> {
    /// Constructs a fresh typed store
    pub fn new() -> Self {
        Self {
            raw: Rc::new(UnsafeCell::new(RawData::new())),
        }
    }

    /// Returns the key for the provided data, relative to the store
    /// If the object is already in the store, it will be dropped, otherwise it will not.
    pub fn insert(&self, data: T) -> usize {
        unsafe {
            let raw: &mut RawData<T> = &mut *self.raw.get();
            raw.insert(data)
        }
    }

    /// Releases read borrows in progress
    pub unsafe fn read_exit(&self, delta: usize) {
        (*self.raw.get()).read_exit(delta)
    }
}

impl<T: PartialEq + Hash> RawData<T> {
    fn new() -> Self {
        RawData {
            inner: Vec::new(),
            index: HashIndex::new(),
            usage: Usage::Read(0),
        }
    }

    fn insert(&mut self, data: T) -> usize {
        // Make sure no refs are out
        assert_eq!(self.usage, Usage::Read(0));
        self.usage = Usage::Write;
        let key = match self.find(&data) {
            Some(key) => key,
            None => {
                let key = self.inner.len();
                self.index.insert(key, &data);
                self.inner.push(data);
                key
            }
        };
        self.usage = Usage::Read(0);
        key
    }

    fn find(&self, data: &T) -> Option<usize> {
        self.index.find(data, &self.inner)
    }
}

impl<T> RawData<T> {
    fn read_exit(&mut self, delta: usize) {
        match self.usage {
            Usage::Read(n) => self.usage = Usage::Read(n - delta),
            Usage::Write => panic!("Tried to end read borrow while write was in progress"),
        }
    }
    fn read_enter(&mut self, delta: usize) {
        match self.usage {
            Usage::Read(n) => self.usage = Usage::Read(n + delta),
            Usage::Write => panic!("Tried to start reading while a write is in progress"),
        }
    }
}

impl<T> Index<usize> for Data<T> {
    type Output = T;
    fn index(&self, key: usize) -> &Self::Output {
        unsafe {
            let raw = &mut *self.raw.get();
            raw.read_enter(1);
            &raw.inner[key]
        }
    }
}
