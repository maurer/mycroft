//! `tuples` contains structures related to an in-memory tuple-store for `usize` values.
//! It is intended to be used in conjunction with `storage::Data` to provid arbitrary-typed tuple
//! functionality.
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::collections::btree_map;
use std::collections::hash_map;
use index::hash::{CheckIndex, HashIndex};
use join::SkipIterator;
use aggregator::Aggregator;

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
    inner: BTreeMap<Tuple, Vec<usize>>,
}

impl Projection {
    fn new(perm: &[usize]) -> Self {
        Self {
            perm: perm.iter().cloned().collect(),
            inner: BTreeMap::new(),
        }
    }
    fn take(&mut self) -> Self {
        let mut out_inner = BTreeMap::new();
        ::std::mem::swap(&mut self.inner, &mut out_inner);
        Self {
            perm: self.perm.clone(),
            inner: out_inner,
        }
    }
    fn insert(&mut self, tup: &[usize], fids: Vec<usize>) {
        self.inner.insert(permute(&self.perm, &tup), fids);
    }
    fn remove(&mut self, tup: &[usize]) {
        self.inner.remove(&permute(&self.perm, &tup));
    }
    fn arity(&self) -> usize {
        self.perm.len()
    }
    fn len(&self) -> usize {
        self.inner.len()
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
    iter: btree_map::Range<'a, Tuple, Vec<usize>>,
}

impl<'a> SkipIterator for ProjectionIter<'a> {
    fn skip(&mut self, tup: Tuple) {
        self.iter = self.proj.inner.range(tup..);
    }
    fn next(&mut self) -> Option<(Tuple, Vec<usize>)> {
        self.iter.next().map(|(t, f)| (t.clone(), f.clone()))
    }
    fn arity(&self) -> usize {
        self.proj.arity()
    }
    fn len(&self) -> usize {
        self.proj.len()
    }
}

/// How we know a fact to be true
#[derive(Ord, Eq, Debug, PartialOrd, PartialEq, Clone)]
pub enum Provenance {
    /// Part of the IDB, e.g. user defined
    Base,
    /// The named rule derived the fact using the provided premises
    Rule {
        /// Index of rule in alphabetical order
        rule_id: usize,
        /// Which facts were used - which tuplestore to look up from depends on the rule
        premises: Vec<Vec<usize>>,
    },
}

/// In-memory Tuple store.
///
/// * Stores exactly one copy of each tuple
/// * Allows for indexes on projections of the tuple (projections)
/// * Allows for differential indexes on projections of the tuple (mailboxes)
pub struct Tuples {
    inner: Vec<Tuple>,
    index: HashIndex<[usize]>,
    agg_map: HashMap<Vec<usize>, (Vec<usize>, Vec<usize>)>,
    agg_indices: Vec<usize>,
    key_indices: Vec<usize>,
    aggs: Vec<Box<Aggregator + 'static>>,
    projections: HashMap<Vec<usize>, Projection>,
    mailboxes: Vec<Projection>,
    provenance: Vec<BTreeSet<Provenance>>,
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
    // Basic integrity check
    fn integrity(&self) -> bool {
        // We have non-zero arity
        if self.arity() == 0 {
            return false;
        }
        // All our columns are the same length.
        for col in self.inner.iter() {
            if col.len() != self.inner[0].len() {
                return false;
            }
        }
        // We have provenance for every tuple
        // This makes integrity() slow, so it must only be used inside debug_assert!
        if self.inner[0].len() != self.provenance.len() {
            return false;
        }
        for p in self.provenance.iter() {
            if p.is_empty() {
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
            projection.insert(&self.get_unchecked(i), vec![i])
        }
        self.projections
            .insert(fields.iter().cloned().collect(), projection);
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
            projection.insert(&self.get_unchecked(i), vec![i])
        }
        self.mailboxes.push(projection);
        self.mailboxes.len() - 1
    }
    /// Constructs a new `Tuples` tuplestore of the provided arity.
    pub fn new(m_aggs: Vec<Option<Box<Aggregator + 'static>>>) -> Self {
        let arity = m_aggs.len();
        assert!(arity > 0);
        let mut inner = Vec::new();
        for _ in 0..arity {
            inner.push(Vec::new());
        }

        let mut agg_indices = Vec::new();
        let mut key_indices = Vec::new();
        let mut aggs = Vec::new();

        for (i, m_agg) in m_aggs.into_iter().enumerate() {
            match m_agg {
                Some(agg) => {
                    agg_indices.push(i);
                    aggs.push(agg);
                }
                None => key_indices.push(i),
            }
        }

        Tuples {
            inner: inner,
            index: HashIndex::new(),
            projections: HashMap::new(),
            mailboxes: Vec::new(),
            provenance: Vec::new(),
            aggs: aggs,
            agg_map: HashMap::new(),
            agg_indices: agg_indices,
            key_indices: key_indices,
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
    /// Get the fact referenced by the provided key
    pub fn get(&self, key: usize) -> Vec<usize> {
        self.get_unchecked(key)
    }
    /// Return the set of ways this tuple was derived
    pub fn get_provenance(&self, key: usize) -> &BTreeSet<Provenance> {
        &self.provenance[key]
    }
    /// Adds a new element to the tuple store.
    /// The arity of the provided value must equal the arity of the tuple store.
    /// The returned value is a pair of the key, and whether the value was new (true for new).
    pub fn insert(&mut self, val: &[usize], p: Provenance) -> (usize, bool) {
        match self.find(&val) {
            Some(id) => {
                self.provenance[id].insert(p);
                (id, false)
            }
            None => {
                assert_eq!(val.len(), self.arity());
                debug_assert!(self.integrity());
                let key = self.len();
                self.index.insert(key, val);
                let mut ps = BTreeSet::new();
                ps.insert(p);
                self.provenance.push(ps);
                for (col, new_val) in self.inner.iter_mut().zip(val.into_iter()) {
                    col.push(*new_val)
                }

                let key_tuple = permute(&self.key_indices, &val);
                let agg_tuple = permute(&self.agg_indices, &val);

                let (updated, agg_done, old) = match self.agg_map.entry(key_tuple) {
                    hash_map::Entry::Occupied(mut oe) => {
                        let old = oe.get().0.clone();
                        for (i, agg_elem) in agg_tuple.iter().enumerate() {
                            oe.get_mut().0[i] = self.aggs[i].aggregate(oe.get().0[i], *agg_elem)
                        }
                        oe.get_mut().1.push(key);
                        let new = oe.get().clone();
                        (old != new.0, new, Some(old))
                    }
                    hash_map::Entry::Vacant(ve) => {
                        let out = (agg_tuple.clone(), vec![key]);
                        ve.insert(out.clone());
                        (true, out, None)
                    }
                };

                if updated {
                    let mut done_tuple = val.to_vec();
                    for (i, agg_idx) in self.agg_indices.iter().enumerate() {
                        done_tuple[*agg_idx] = agg_done.0[i];
                    }

                    match old {
                        Some(old_agg) => {
                            let mut old_tup = val.to_vec();
                            for (i, agg_idx) in self.agg_indices.iter().enumerate() {
                                old_tup[*agg_idx] = old_agg[i]
                            }

                            for proj in self.projections.values_mut() {
                                proj.remove(&old_tup)
                            }
                            for mailbox in self.mailboxes.iter_mut() {
                                mailbox.remove(&old_tup)
                            }
                        }
                        None => (),
                    }

                    for proj in self.projections.values_mut() {
                        proj.insert(&done_tuple, agg_done.1.clone())
                    }
                    for mailbox in self.mailboxes.iter_mut() {
                        mailbox.insert(&done_tuple, agg_done.1.clone())
                    }
                }
                (key, true)
            }
        }
    }
}
