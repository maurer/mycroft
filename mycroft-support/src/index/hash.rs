//! `hash` provides a `HashIndex` over `storage` types - it only allows you to check presence or
//! not, and if present, gets you the key.
use std::hash::{Hash, Hasher, BuildHasher};
use std::ops::Index;
use std::marker::PhantomData;
use std::collections::hash_map::RandomState;

// Initial number of buckets in the hash table
const DEFAULT_SIZE: usize = 1024;

/// `CheckIndex` is a way to avoid needing to move or clone values from storage when checking for
/// equality.
///
/// Its primary purpose is to allow different storage mediums to be used as the `HashIndex`'s backing.
pub trait CheckIndex<T: ?Sized> {
    /// `x.check_index(key, val)` is equivalent to `&x[key] = val`, were such an indexing
    /// relationship to be definable.
    /// It's used when actually retrieving a value involves construction, such as when using
    /// columnar or disk based storage.
    fn check_index(&self, key: usize, val: &T) -> bool;
}

/// If `Index` can be implemented for a type, provide the definitional interpretation from
/// `CheckIndex`'s documentation.
impl<T: PartialEq, D: Index<usize, Output = T>> CheckIndex<T> for D {
    fn check_index(&self, key: usize, val: &T) -> bool {
        &self[key] == val
    }
}

// An individual bucket in the hash table. It needs both the hash and the key:
// * In the case of a lookup, the key is the value we return
// * To protect against hash collision, we need to check the store using the key when we think
//   we've found what we're looking for
// * If we store only the key, we will need to repeatedly re-hash potentially large values, and
//   incur potentially expensive access times if disk is in use.
// It keeps the whole hash because:
// * It avoids recomputing the hash on resize
// * Variable size buckets are a pain, and due to alignment may not even buy us anything
#[derive(Clone, Copy, Eq, PartialEq)]
struct Entry {
    key: usize,
    // Technically, hashes are u64s, but:
    // 1.) usize will be <= u64 in size
    // 2.) If usize < u64, then we never need the upper bits of the hash anyways because we can't
    //   have that many buckets
    hash: usize,
}

// An empty entry constructed via a max key and max hash. Regardless of whether you're on a 64-bit
// or 32-bit machine, `usize::MAX` is more of anything than you can put in memory, so this
// shouldn't ever collide with a real entry.
const EMPTY_ENTRY: Entry = Entry {
    key: ::std::usize::MAX,
    hash: ::std::usize::MAX,
};

impl Entry {
    // Convenience function in case the representation of emptiness changes
    fn is_empty(&self) -> bool {
        self == &EMPTY_ENTRY
    }
}

/// A `HashIndex` is an externally-referencing `HashSet`. This means it does not store its own
/// entries, or even pointers to its entries, but rather an abstract key used to reference those
/// entries which can be used with a provided store at operation time.
///
/// This enables a `HashIndex` to be used over data structures which may not have the entry fully
/// realized, or which do not have the structure in memory at all (e.g. column stores, lazy stores,
/// disk stores).
///
/// Due to my specific use case, it differs from a default `HashSet` in two ways in the interface:
///
/// * On membership, the `HashIndex` will return the key for use in deduping situations.
/// * There is no delete (thus why there are no tombstones or fancy shifting)
/// * `insert()` may not be used for membership testing - you must test via `find()`, then
///   `insert()` only if not present.
pub struct HashIndex<T: ?Sized> {
    elems: usize,
    hash_key: RandomState,
    table: Vec<Entry>,
    marker: PhantomData<T>,
}

impl<T: Hash + PartialEq + ?Sized> HashIndex<T> {
    /// Creates a fresh `HashIndex`
    pub fn new() -> Self {
        Self {
            elems: 0,
            hash_key: RandomState::new(),
            table: vec![EMPTY_ENTRY; DEFAULT_SIZE],
            marker: PhantomData,
        }
    }

    // Computes the probe length of a given index, e.g. on a lookup, how many steps would be
    // required to find it.
    fn probe_len(&self, index: usize) -> usize {
        debug_assert!(self.table[index] != EMPTY_ENTRY);
        let len = self.table.len();
        // TODO can probably make this expression better with wrapping arith
        (((self.table[index].hash % len) + len) - index) % len
    }

    // Performs a robin-hood insertion of the entry. This means it will walk the table until:
    // 1.) A free space is found, which it will place the entry in
    // 2.) An entry with a shorter probe length than how far we've gone is found, in which case we
    //   will steal its spot (from the rich) and give it to our current entry (the poor). In this
    //   case, it will take the now displaced entry, and continue the process looking for a spot
    //   for it.
    // Why this is fast is beyond the scope of these comments, but it is, go read the paper.
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

    /// Checks whether the `HashIndex` contains a provided value.
    /// If it does, the result will be `Some(key)`, where `&db[key] == val`.
    /// If it does not, the result will be `None`.
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
                // Would have swapped and inserted here, early termination
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
