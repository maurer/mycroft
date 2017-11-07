//! `tuples` contains structures related to an in-memory tuple-store for `usize` values.
//! It is intended to be used in conjunction with `storage::Data` to provid arbitrary-typed tuple
//! functionality.
use std::collections::{HashMap, BTreeSet};
use std::collections::btree_set;
use index::hash::{HashIndex, CheckIndex};
use join::SkipIterator;

type Tuple = Vec<usize>;

fn permute(perm: &[usize], tup: &[usize]) -> Tuple {
    let mut out = Vec::new();
    for col in perm {
        out.push(tup[*col]);
    }
    out
}

/// A projection is an ordered view of a tuple store, under a specific permutation.
pub struct Projection {
    perm: Vec<usize>,
    inner: BTreeSet<Tuple>,
}

impl Projection {
    fn new(perm: &[usize]) -> Self {
        Self {
            perm: perm.iter().cloned().collect(),
            inner: BTreeSet::new(),
        }
    }
    fn take(&mut self) -> Self {
        let mut out_inner = BTreeSet::new();
        ::std::mem::swap(&mut self.inner, &mut out_inner);
        Self {
            perm: self.perm.clone(),
            inner: out_inner,
        }
    }
    fn insert(&mut self, tup: &[usize]) {
        self.inner.insert(permute(&self.perm, &tup));
    }
    fn arity(&self) -> usize {
        self.perm.len()
    }
    /// Creates a `SkipIterator` for the `Projection`
    pub fn skip_iter<'a>(&'a self) -> ProjectionIter<'a> {
        ProjectionIter {
            proj: self,
            iter: self.inner.range(vec![]..),
        }
    }
}

/// Iterator over a projection implementing the `SkipIterator` interface
pub struct ProjectionIter<'a> {
    proj: &'a Projection,
    iter: btree_set::Range<'a, Tuple>,
}

impl<'a> SkipIterator for ProjectionIter<'a> {
    fn skip(&mut self, tup: Tuple) {
        self.iter = self.proj.inner.range(tup..);
    }
    fn next(&mut self) -> Option<Tuple> {
        self.iter.next().map(|x| x.clone())
    }
    fn arity(&self) -> usize {
        self.proj.arity()
    }
}

/// In-memory Tuple store.
///
/// * Stores exactly one copy of each tuple
/// * Allows for indexes on projections of the tuple (projections)
/// * Allows for differential indexes on projections of the tuple (mailboxes)
pub struct Tuples {
    inner: Vec<Tuple>,
    index: HashIndex<[usize]>,
    projections: HashMap<Vec<usize>, Projection>,
    mailboxes: Vec<Projection>,
}

struct Rows<'a> {
    inner: &'a Vec<Vec<usize>>,
}

impl<'a> CheckIndex<[usize]> for Rows<'a> {
    fn check_index(&self, index: usize, row: &[usize]) -> bool {
        assert!(row.len() == self.inner.len());
        for col in 0..row.len() {
            if self.inner[col][index] != row[col] {
                return false;
            }
        }
        return true;
    }
}

impl Tuples {
    // Basic integrity check - doesn't do much at the moment, just make sure the tuple store itself
    // is non-trivial and has equal-length columns.
    fn integrity(&self) -> bool {
        if self.arity() == 0 {
            return false;
        }
        for col in self.inner.iter() {
            if col.len() != self.inner[0].len() {
                return false;
            }
        }
        return true;
    }
    /// Acquires a **previously registered** projection for the permutation provided. If you did not
    /// register the projection, an assertion will trip.
    pub fn projection(&self, fields: &[usize]) -> &Projection {
        match self.projections.get(fields) {
            Some(ref p) => p,
            None => {
                // We can't just register the projection for them here, because if we do, we'll be
                // borrowed mutably, which prevents multiple indices from being accessed
                // simultaneously which is needed for self joins.
                // You really should want to create all projections at the beginning of the program
                // anyways, so this isn't that big of a hinderance.
                panic!("You must register the projection first");
            }
        }
    }
    /// Acquires the index at a previously registered mailbox, emptying it out and returning the
    /// projection that was there.
    pub fn mailbox(&mut self, mailbox: usize) -> Projection {
        self.mailboxes[mailbox].take()
    }
    /// Requests that a projection be made available for a given permutation. This is essentially
    /// analogous to asking a database to produce an index on a specific order or fields.
    /// Multiple requests for the same index ordering are idempotent.
    pub fn register_projection(&mut self, fields: &[usize]) {
        if self.projections.contains_key(fields) {
            return;
        }
        let mut projection = Projection::new(fields);
        for i in 0..self.len() {
            projection.insert(&self.get_unchecked(i))
        }
        self.projections.insert(
            fields.iter().cloned().collect(),
            projection,
        );
    }
    /// Requests that a mailbox be created for a given permutation, and returns the mailbox ID.
    /// A mailbox is basically an index for which only values added since you last checked it are
    /// present.
    /// Unlike `register_projection`, `register_mailbox` is **not** idempotent, since multiple
    /// clients may want to listen on the same order of fields. It will return a new mailbox ID
    /// every time.
    pub fn register_mailbox(&mut self, fields: &[usize]) -> usize {
        // TODO: dedup between this and register_projection
        let mut projection = Projection::new(fields);
        for i in 0..self.len() {
            projection.insert(&self.get_unchecked(i))
        }
        self.mailboxes.push(projection);
        self.mailboxes.len() - 1
    }
    /// Constructs a new `Tuples` tuplestore of the provided arity.
    pub fn new(arity: usize) -> Self {
        assert!(arity > 0);
        let mut inner = Vec::new();
        for _ in 0..arity {
            inner.push(Vec::new());
        }
        Tuples {
            inner: inner,
            index: HashIndex::new(),
            projections: HashMap::new(),
            mailboxes: Vec::new(),
        }
    }
    /// Returns the arity of the tuples stored
    pub fn arity(&self) -> usize {
        self.inner.len()
    }
    /// Returns the number of elements stored
    pub fn len(&self) -> usize {
        debug_assert!(self.integrity());
        self.inner[0].len()
    }
    /// Provides the ID for a given tuple if it is present
    /// The provided needle must have an arity equal to the `Tuples` object
    pub fn find(&self, needle: &[usize]) -> Option<usize> {
        assert_eq!(needle.len(), self.arity());
        debug_assert!(self.integrity());
        self.index.find(needle, &Rows { inner: &self.inner })
    }
    fn get_unchecked(&self, key: usize) -> Vec<usize> {
        let mut out = Vec::new();
        for i in 0..self.arity() {
            out.push(self.inner[i][key]);
        }
        out
    }
    /// Adds a new element to the tuple store.
    /// The arity of the provided value must equal the arity of the tuple store.
    /// The returned value is a pair of the key, and whether the value was new (true for new).
    pub fn insert(&mut self, val: &[usize]) -> (usize, bool) {
        match self.find(&val) {
            Some(id) => (id, false),
            None => {
                assert_eq!(val.len(), self.arity());
                debug_assert!(self.integrity());
                let key = self.len();
                self.index.insert(key, val);
                for (col, new_val) in self.inner.iter_mut().zip(val.into_iter()) {
                    col.push(*new_val)
                }
                for proj in self.projections.values_mut() {
                    proj.insert(val)
                }
                for mailbox in self.mailboxes.iter_mut() {
                    mailbox.insert(val)
                }
                (key, true)
            }
        }
    }
}
