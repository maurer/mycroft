use std::hash::{Hash, Hasher, BuildHasher};
use std::ops::Index;
use std::marker::PhantomData;
use std::collections::hash_map::RandomState;

const DEFAULT_SIZE: usize = 1024;

pub trait CheckIndex<T: ?Sized> {
    fn check_index(&self, key: usize, val: &T) -> bool;
}

impl<T: Eq, D: Index<usize, Output=T>> CheckIndex<T> for D {
    fn check_index(&self, key: usize, val: &T) -> bool {
        &self[key] == val
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
struct Entry {
    key: usize,
    // Technically, hashes are u64s, but:
    // 1.) usize will be <= u64 in size
    // 2.) If usize < u64, then we never need the upper bits of the hash anyways
    hash: usize,
}

const EMPTY_ENTRY: Entry = Entry {
    key: ::std::usize::MAX,
    hash: ::std::usize::MAX,
};

impl Entry {
    fn is_empty(&self) -> bool {
        self == &EMPTY_ENTRY
    }
}

pub struct HashIndex<T: ?Sized> {
    elems: usize,
    hash_key: RandomState,
    table: Vec<Entry>,
    marker: PhantomData<T>,
}

impl<T: Hash + Eq + ?Sized> HashIndex<T> {
    pub fn new() -> Self {
        Self {
            elems: 0,
            hash_key: RandomState::new(),
            table: vec![EMPTY_ENTRY; DEFAULT_SIZE],
            marker: PhantomData,
        }
    }


    fn probe_len(&self, index: usize) -> usize {
        debug_assert!(self.table[index] != EMPTY_ENTRY);
        let len = self.table.len();
        // TODO can probably make this expression better with wrapping arith
        (((self.table[index].hash % len) + len) - index) % len
    }

    fn robin_hood(&mut self, mut entry: Entry) {
        let len = self.table.len();
        let mut index = entry.hash % len;
        let mut probe_len = 0;
        loop {
            // If we're pointed at an empty entry, insert
            if self.table[index].is_empty() {
                self.table[index] = entry;
                return;
            }
            // If our probe len is greater than the probe len of the entry
            // we're pointed at, swap and continue
            let their_probe = self.probe_len(index);
            if probe_len > their_probe {
                ::std::mem::swap(&mut entry, &mut self.table[index]);
                probe_len = their_probe;
            }
            // Step the index and check the next entry
            index = (index + 1) % len;
            probe_len += 1;

            // Just in case our resize logic is wrong, check if we've manage to loop around to the
            // optimal - this would indicate a full hash table, and the hash table is supposed to
            // prevent itself from becoming overfull by resizing on insert if necessary.
            debug_assert!({
                let optimal_index = entry.hash % len;
                index != optimal_index
            })
        }
    }

    // Checks whether adding one more element would bring us over our max load
    // Currently max load is 0.9
    fn needs_resize(&self) -> bool {
        (self.elems + 1) * 10 > self.table.len() * 9
    }

    // Doubles table size
    fn resize(&mut self) {
        let mut table = vec![EMPTY_ENTRY; self.table.len() * 2];
        ::std::mem::swap(&mut table, &mut self.table);
        for entry in table.into_iter() {
            if !entry.is_empty() {
                self.robin_hood(entry)
            }
        }
    }

    /// Inserts the given key from the database, assuming that it is not already present.
    /// if you provide a value which would already return a key via `find()`, behavior
    /// is undefined.
    pub fn insert(&mut self, key: usize, val: &T) {
        if self.needs_resize() {
            self.resize()
        }
        let mut hasher = self.hash_key.build_hasher();
        val.hash(&mut hasher);
        let hash = hasher.finish();
        let entry = Entry {
            key: key,
            // See type decl for downcast justification
            hash: hash as usize,
        };
        self.robin_hood(entry);
        self.elems += 1;
    }

    pub fn find<D: CheckIndex<T>>(&self, val: &T, db: &D) -> Option<usize> {
        let mut hasher = self.hash_key.build_hasher();
        val.hash(&mut hasher);
        let hash = hasher.finish() as usize;
        let len = self.table.len();
        let mut index = hash % len;
        let mut probe_len = 0;
        loop {
            if self.table[index].is_empty() {
                return None;
            } else if self.table[index].hash == hash {
                if db.check_index(self.table[index].key, val) {
                    return Some(self.table[index].key);
                }
            } else if probe_len > self.probe_len(index) {
                // We would have swapped and inserted here, early termination
                return None;
            }
            index = (index + 1) % len;
            probe_len += 1;
            // Table load should prevent this from being possible, but like in insert, let's double
            // check
            debug_assert!({
                let optimal_index = hash % len;
                index != optimal_index
            });
        }
    }
}
