//! `join` implements a simple indexed join with the assumption of a global attribute order. This
//! is likely not the optimal join, but it will get us started.
/// Shorthand for a variable length tuple
pub type Tuple = Vec<usize>;
/// A `SkipIterator` is like a normal iterator, but:
///
/// * The arity of all returned `Tuple`s must be identical, and equal to the value returned by
///   `arity()`
/// * It can be moved to the first value above or equal to a tuple, meaning the iterator must be
///   rewindable.
pub trait SkipIterator {
    /// Provides the next tuple in the iterator
    fn next(&mut self) -> Option<Tuple>;
    /// Sets the iterator position to return the minimum value which is greater than or equal to
    /// the provided min.
    fn skip(&mut self, min: Tuple);
    /// Returns the arity of the tuples that will be returned by `next()`
    fn arity(&self) -> usize;
}

/// A field specifies a particular value in the join problem
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Copy, Clone, Hash)]
pub struct Field {
    /// `clause` refers to which iterator this value comes from, based on the order they were
    /// provided to the `Join` constructor
    pub clause: usize,
    /// `field` indicates offset into the tuple referred to
    pub field: usize,
}

/// A `Restrict` describes a way a particular `Field` can be limited in the join
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Copy, Clone, Hash)]
pub enum Restrict {
    /// The value at `Field` must equal the provided value
    Const(usize),
    /// The value at `Field` must equal all other values which were restricted by being `Unify`'d
    /// to the same provided key
    Unify(usize),
}

impl Restrict {
    // Checks whether a restriction is met, updating the candidate if it has a new variable
    // definition
    fn check(&self, candidate: &mut Vec<usize>, val: usize) -> bool {
        match *self {
            Restrict::Const(v) => v == val,
            Restrict::Unify(var) => {
                if var < candidate.len() {
                    candidate[var] == val
                } else if var == candidate.len() {
                    candidate.push(val);
                    true
                } else {
                    panic!("Out of order variable numbering")
                }
            }
        }
    }
    // Provides the minimum legal value for the field, assuming the candidate we have partially
    // locked in.
    fn min(&self, candidate: &Vec<usize>) -> usize {
        match *self {
            Restrict::Const(v) => v,
            Restrict::Unify(var) => {
                if var < candidate.len() {
                    candidate[var]
                } else {
                    0
                }
            }
        }
    }
}

/// A join iterator, made of multiple `SkipIterators`, combined with the join condition.
pub struct Join<'a> {
    indices: Vec<&'a mut SkipIterator>,
    restricts: &'a Vec<Vec<Option<Restrict>>>,
    // Currently selected variable assignment
    candidate: Vec<usize>,
    // Stack of previous variable assignment lengths so we can rewind choices
    candidate_len: Vec<usize>,
}

fn min_possible(candidate: &Vec<usize>, restricts: &Vec<Option<Restrict>>) -> Vec<usize> {
    let mut out = Vec::new();
    for mr in restricts.iter() {
        match *mr {
            None => out.push(0),
            Some(ref r) => out.push(r.min(candidate)),
        }
    }
    out
}

impl<'a> Join<'a> {
    /// Creates a new join iterator:
    ///
    /// * `indices` will be walked in the order provided, so if you know you have a small one, try to
    ///   put it first
    /// * `restricts` must use a tight, ascending variable ordering, that is:
    ///
    ///   * If field0 < field1, and both map to `Unify(var0)` and `Unify(var1)` respectively, var0
    ///     < var1
    ///   * If `Unify(var)` is present in the map, and var0 < var, then `Unify(var0)` is present in
    ///     the map
    pub fn new(
        indices: Vec<&'a mut SkipIterator>,
        restricts: &'a Vec<Vec<Option<Restrict>>>,
    ) -> Self {
        let mut join = Join {
            indices: indices,
            restricts: restricts,
            candidate: Vec::new(),
            candidate_len: Vec::new(),
        };
        // We need to .right() once before starting to initialize the leftmost iterator properly
        join.right();
        join
    }

    // Moves one iterator left, e.g. because we have no more possibilities on this one
    fn left(&mut self) {
        self.candidate.truncate(self.candidate_len.pop().unwrap());
        self.candidate.truncate(self.candidate_len.pop().unwrap());
    }

    // Moves one iterator right, e.g. this one matched, and we need to finish filling the candidate
    // and checking restrictions
    fn right(&mut self) {
        let n = self.candidate_len.len();
        self.indices[n].skip(min_possible(&self.candidate, &self.restricts[n]));
    }
}
impl<'a> Iterator for Join<'a> {
    type Item = Tuple;
    fn next(&mut self) -> Option<Self::Item> {
        // Join invariants:
        // 1.) candidate_len.len() indicates the "current" index
        // 2.) All indices less than the current index are coherent, and their candidate values are in candidate
        // 3.) The current index is at least past the minimum possible advancement level (this needs to be set up in new())
        'states: loop {
            let n = self.candidate_len.len();
            self.candidate_len.push(self.candidate.len());
            match self.indices[n].next() {
                Some(tup) => {
                    for (f, v) in tup.into_iter().enumerate() {
                        let mut left_out = false;
                        match self.restricts[n][f] {
                            Some(r) => {
                                if !r.check(&mut self.candidate, v) {
                                    if n == 0 {
                                        self.candidate.truncate(self.candidate_len.pop().unwrap());
                                        return None;
                                    }
                                    left_out = true;
                                }
                            }
                            None => (),
                        }
                        if left_out {
                            self.left();
                            continue 'states;
                        }
                    }
                    if self.candidate_len.len() == self.indices.len() {
                        // We have a complete candidate
                        let out = self.candidate.clone();
                        self.candidate.truncate(self.candidate_len.pop().unwrap());
                        return Some(out);
                    }
                    // We're not done yet
                    self.right();
                }
                None => {
                    if n == 0 {
                        self.candidate.truncate(self.candidate_len.pop().unwrap());
                        return None;
                    }
                    self.left();
                }
            }
        }
    }
}


#[cfg(test)]
mod test {
    /// `TrivialIterator` provides a sort+dedup implementation of a `SkipIterator`.
    /// It will be slower than traditional indexes in essentailly all cases - it is _not_ the
    /// equivalent of a full tablescan, that is just an iterator with .skip() no-opped.
    /// It's provided to allow tests to separate from index implementation/initialization.
    pub struct TrivialIterator {
        payload: Vec<Vec<usize>>,
        loc: usize,
    }

    impl TrivialIterator {
        /// Consumes a list of tuples and produces a `SkipIterator` for them
        pub fn new(mut payload: Vec<Vec<usize>>) -> Self {
            payload.sort();
            payload.dedup();
            TrivialIterator {
                payload: payload,
                loc: 0,
            }
        }
    }

    impl SkipIterator for TrivialIterator {
        fn next(&mut self) -> Option<Tuple> {
            if self.loc < self.payload.len() {
                self.loc += 1;
                Some(self.payload[self.loc - 1].clone())
            } else {
                None
            }
        }
        fn skip(&mut self, min: Tuple) {
            self.loc = 0;
            while (self.payload.len() > self.loc) && (self.payload[self.loc] < min) {
                self.loc = self.loc + 1;
            }
        }
        fn arity(&self) -> usize {
            self.payload[0].len()
        }
    }
    use super::*;

    #[test]
    fn simple_join() {
        use self::Restrict::*;
        let mut p = TrivialIterator::new(vec![vec![0, 2, 3], vec![3, 2, 5], vec![4, 4, 4]]);
        let mut q = TrivialIterator::new(vec![vec![1, 3], vec![2, 7], vec![3, 4], vec![8, 2]]);
        let restricts = vec![
            vec![Some(Unify(0)), Some(Unify(1)), None],
            vec![Some(Unify(1)), Some(Const(7))],
        ];
        let its: Vec<&mut SkipIterator> = vec![&mut p, &mut q];
        let mut join = Join::new(its, &restricts);
        assert_eq!(join.next(), Some(vec![0, 2]));
        assert_eq!(join.next(), Some(vec![3, 2]));
        assert_eq!(join.next(), None);
        assert_eq!(join.next(), None);
    }
}
