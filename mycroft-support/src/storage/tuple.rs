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

/// Key type for referencing a fact
pub type FactId = usize;
/// Key type for referencing a group of facts in an aggregate
pub type MetaId = usize;

/// Type for describing one of the two ways a merged result can be created - either as a simple
/// aggregate using the `FactIds` constructor, or as a circumscribed aggregate using a `MetaId`
#[derive(Ord, Eq, Debug, PartialOrd, PartialEq, Clone)]
pub enum MergeRef {
    /// Circumscribed fact grouping
    MetaId(MetaId),
    /// Aggregate fact grouping (singleton for non-aggregate)
    FactIds(Vec<FactId>),
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
        premises: Vec<MergeRef>,
    },
}

impl Provenance {
    fn uses_fid<F: Fn(usize, usize) -> usize>(&self, pred_id: usize, fid: FactId, f: F) -> bool {
        match *self {
            Provenance::Base => false,
            Provenance::Rule {
                rule_id,
                ref premises,
            } => {
                for (col, premise) in premises.iter().enumerate() {
                    if f(rule_id, col) == pred_id {
                        match *premise {
                            MergeRef::MetaId(_) => continue,
                            MergeRef::FactIds(ref fids) => if fids.contains(&fid) {
                                return true;
                            },
                        }
                    }
                }
                false
            }
        }
    }
    fn uses_mid<F: Fn(usize, usize) -> usize>(&self, pred_id: usize, mid: MetaId, f: F) -> bool {
        match *self {
            Provenance::Base => false,
            Provenance::Rule {
                rule_id,
                ref premises,
            } => {
                for (col, premise) in premises.iter().enumerate() {
                    if f(rule_id, col) == pred_id {
                        match *premise {
                            MergeRef::MetaId(mid2) => if mid == mid2 {
                                return true;
                            },
                            MergeRef::FactIds(_) => continue,
                        }
                    }
                }
                false
            }
        }
    }
}

#[derive(Default, Eq, Ord, Hash, Debug, PartialOrd, PartialEq, Clone)]
struct AggVal {
    fids: Vec<usize>,
    fids_loaded: usize,
    agg: Option<Vec<usize>>,
}

impl AggVal {
    fn new() -> Self {
        Self::default()
    }
    fn partial_load(&mut self, ts: &Tuples) {
        if self.agg.is_none() {
            self.agg = Some(permute(&ts.agg_indices, &ts.get(self.fids[0])));
            self.fids_loaded = 1;
        }
    }
    fn force(&mut self, ts: &Tuples) -> Vec<usize> {
        self.partial_load(ts);
        let mut new_agg = Vec::new();
        for (idx, agg_val) in self.agg.take().unwrap().iter().enumerate() {
            let mut input = vec![*agg_val];
            for fid in &self.fids[self.fids_loaded..] {
                input.push(ts.inner[ts.agg_indices[idx]][*fid]);
            }
            new_agg.push(ts.aggs[idx].aggregate(&input));
        }
        self.agg = Some(new_agg.clone());
        self.fids_loaded = self.fids.len();
        new_agg
    }
    fn current(&self) -> Option<Vec<usize>> {
        self.agg.clone()
    }
    fn purge_fid(&mut self, fid: usize) {
        let mut old_fids = Vec::new();
        ::std::mem::swap(&mut old_fids, &mut self.fids);
        self.fids = old_fids.into_iter().filter(|x| *x != fid).collect();
        self.fids_loaded = 0;
    }
    fn add_fid(&mut self, fid: usize) -> bool {
        if !self.fids.contains(&fid) {
            self.fids.push(fid);
            true
        } else {
            false
        }
    }
    fn is_empty(&self) -> bool {
        self.fids.is_empty()
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
    agg_map: HashMap<Vec<usize>, AggVal>,
    agg_indices: Vec<usize>,
    key_indices: Vec<usize>,
    aggs: Vec<Box<Aggregator + 'static>>,
    projections: HashMap<Vec<usize>, Projection>,
    mailboxes: Vec<Projection>,
    provenance: Vec<BTreeSet<Provenance>>,
    meta: Vec<Vec<FactId>>,
    inv_meta: HashMap<Vec<FactId>, MetaId>,
    delayed: BTreeSet<Vec<usize>>,
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

fn get_unchecked(inner: &Vec<Vec<usize>>, key: FactId) -> Vec<usize> {
    let mut out = Vec::new();
    for col in inner {
        out.push(col[key]);
    }
    out
}

impl Tuples {
    fn forced(&self) -> bool {
        self.delayed.is_empty()
    }
    /// Take all the delayed tuples and update the indices.
    /// Must be called prior to requesting a projection
    pub fn force(&mut self) {
        let mut all_delayed = BTreeSet::new();
        ::std::mem::swap(&mut all_delayed, &mut self.delayed);
        for delayed in all_delayed {
            let mut agg_val = self.agg_map.remove(&delayed).unwrap();
            let mut old_tup = delayed.clone();
            if let Some(cur) = agg_val.current() {
                old_tup.extend(cur);

                for proj in self.projections.values_mut() {
                    proj.remove(&old_tup)
                }
                for mailbox in self.mailboxes.iter_mut() {
                    mailbox.remove(&old_tup)
                }
            }

            if !agg_val.is_empty() {
                let mut new_tup = delayed.clone();
                new_tup.extend(agg_val.force(&self));

                let fids = agg_val.fids.clone();

                for proj in self.projections.values_mut() {
                    proj.insert(&new_tup, fids.clone())
                }
                for mailbox in self.mailboxes.iter_mut() {
                    mailbox.insert(&new_tup, fids.clone())
                }

                self.agg_map.insert(delayed, agg_val);
            }
        }
    }

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

        return true;
    }
    /// Acquires a **previously registered** projection for the permutation provided. If you did not
    /// register the projection, an assertion will trip.
    pub fn projection(&self, fields: &[usize]) -> &Projection {
        assert!(self.forced());
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
        self.force();
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
            projection.insert(&self.get(i), vec![i])
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
            projection.insert(&self.get(i), vec![i])
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
            meta: Vec::new(),
            inv_meta: HashMap::new(),
            aggs: aggs,
            agg_map: HashMap::new(),
            agg_indices: agg_indices,
            key_indices: key_indices,
            delayed: BTreeSet::new(),
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
    /// Get the fact referenced by the provided key
    pub fn get(&self, key: usize) -> Vec<usize> {
        get_unchecked(&self.inner, key)
    }
    /// Return the set of ways this tuple was derived
    pub fn get_provenance(&self, key: usize) -> &BTreeSet<Provenance> {
        &self.provenance[key]
    }
    /// Gets the list of fact IDs making up a particular meta-fact
    pub fn get_meta(&self, mid: MetaId) -> Vec<FactId> {
        self.meta[mid].clone()
    }
    /// Creates or references a `MetaId` for a circumscription over facts in this tuplestore
    pub fn make_meta(&mut self, fids: &[FactId]) -> MetaId {
        match self.inv_meta.entry(fids.to_vec()) {
            hash_map::Entry::Occupied(oe) => *oe.get(),
            hash_map::Entry::Vacant(ve) => {
                let key = self.meta.len();
                self.meta.push(fids.to_vec());
                ve.insert(key);
                key
            }
        }
    }
    /// Cleanses a fact's derivations of references to a MetaId
    pub fn purge_mid_prov<F: Fn(usize, usize) -> usize>(
        &mut self,
        fid: FactId,
        pred_id: usize,
        dep_mid: MetaId,
        f: F,
    ) -> (Option<FactId>, Option<MetaId>) {
        self.provenance[fid] = self.provenance[fid]
            .iter()
            .cloned()
            .filter(|p| !p.uses_mid(pred_id, dep_mid, &f))
            .collect();
        if self.provenance[fid].is_empty() {
            (Some(fid), self.purge(fid))
        } else {
            (None, None)
        }
    }
    /// Cleanses a fact's derivations of references to a FactId
    pub fn purge_fid_prov<F: Fn(usize, usize) -> usize>(
        &mut self,
        fid: FactId,
        pred_id: usize,
        dep_fid: FactId,
        f: F,
    ) -> (Option<FactId>, Option<MetaId>) {
        self.provenance[fid] = self.provenance[fid]
            .iter()
            .cloned()
            .filter(|p| !p.uses_fid(pred_id, dep_fid, &f))
            .collect();
        if self.provenance[fid].is_empty() {
            (Some(fid), self.purge(fid))
        } else {
            (None, None)
        }
    }
    /// Removes a tuple from all indices
    /// Does not actually remove it from the tuple store, it'll just stop showing up in
    /// projections. Returns a MetaId if it broke one
    fn purge(&mut self, fid: FactId) -> Option<MetaId> {
        let vals = self.get(fid);
        let key_tuple = permute(&self.key_indices, &vals);
        match self.agg_map.entry(key_tuple) {
            hash_map::Entry::Occupied(mut oe) => {
                assert!(oe.get().fids.contains(&fid));
                let old_fids = oe.get().fids.clone();

                self.delayed.insert(oe.key().clone());
                oe.get_mut().purge_fid(fid);
                self.inv_meta.get(&old_fids).map(|x| x.clone())
            }
            hash_map::Entry::Vacant(_) => panic!("Purged a fact not in the aggmap"),
        }
    }
    /// Adds a new element to the tuple store.
    /// The arity of the provided value must equal the arity of the tuple store.
    /// The returned value is a tuple of the key, whether the value was new (true for new),
    /// and any MetaId that got broken by the insert
    pub fn insert(&mut self, val: &[usize], p: Provenance) -> (FactId, bool, Option<MetaId>) {
        let key = match self.find(&val) {
            Some(id) => {
                self.provenance[id].insert(p);
                id
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
                key
            }
        };

        let key_tuple = permute(&self.key_indices, &val);

        match self.agg_map.entry(key_tuple) {
            hash_map::Entry::Occupied(mut oe) => {
                let mmid = self.inv_meta.get(&oe.get().fids).map(|x| *x);
                let fresh = oe.get_mut().add_fid(key);
                if fresh {
                    self.delayed.insert(oe.key().clone());
                }
                (key, fresh, mmid)
            }
            hash_map::Entry::Vacant(ve) => {
                let mut agg_val = AggVal::new();
                agg_val.add_fid(key);
                self.delayed.insert(ve.key().clone());
                ve.insert(agg_val.clone());
                (key, true, None)
            }
        }
    }
}
