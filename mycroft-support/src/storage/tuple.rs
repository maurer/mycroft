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

pub struct Projection {
    perm: Vec<usize>,
    inner: BTreeSet<Tuple>
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
    pub fn skip_iter<'a>(&'a self) -> ProjectionIter<'a> {
        ProjectionIter {
            proj: self,
            iter: self.inner.range(vec![]..),
        }
    }
}

pub struct ProjectionIter<'a> {
    proj: &'a Projection,
    iter: btree_set::Range<'a, Tuple>,
}

impl <'a> SkipIterator for ProjectionIter<'a> {
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

pub struct Tuples {
    inner: Vec<Vec<usize>>,
    index: HashIndex<[usize]>,
    projections: HashMap<Vec<usize>, Projection>,
    mailboxes: Vec<Projection>,
}

pub struct Rows<'a> {
    inner: &'a Vec<Vec<usize>>,
}

impl<'a> CheckIndex<[usize]> for Rows<'a> {
    fn check_index(&self, index: usize, row: &[usize]) -> bool {
        assert!(row.len() == self.inner.len());
        for col in 0..row.len() {
            if self.inner[col][index] != row[col] {
                return false
            }
        }
        return true
    }
}

impl Tuples {
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
    pub fn mailbox(&mut self, mailbox: usize) -> Projection {
        self.mailboxes[mailbox].take()
    }
    pub fn register_projection(&mut self, fields: &[usize]) {
        if self.projections.contains_key(fields) {
            return;
        }
        let mut projection = Projection::new(fields);
        for i in 0..self.len() {
            projection.insert(&self.get_unchecked(i))
        }
        self.projections.insert(fields.iter().cloned().collect(), projection);
    }
    pub fn register_mailbox(&mut self, fields: &[usize]) -> usize {
        // TODO: dedup between this and register_projection
        let mut projection = Projection::new(fields);
        for i in 0..self.len() {
            projection.insert(&self.get_unchecked(i))
        }
        self.mailboxes.push(projection);
        self.mailboxes.len() - 1
    }
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
    pub fn arity(&self) -> usize {
        self.inner.len()
    }
    pub fn len(&self) -> usize {
        debug_assert!(self.integrity());
        self.inner[0].len()
    }
    pub fn find(&self, needle: &[usize]) -> Option<usize> {
        assert_eq!(needle.len(), self.arity());
        debug_assert!(self.integrity());
        self.index.find(needle, &Rows {inner: &self.inner})
    }
    fn get_unchecked(&self, key: usize) -> Vec<usize> {
        let mut out = Vec::new();
        for i in 0..self.arity() {
            out.push(self.inner[i][key]);
        }
        out
    }
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
