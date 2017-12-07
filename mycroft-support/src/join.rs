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
    /// Provides the next tuple in the iterator, the fact ID it was derived from
    fn next(&mut self) -> Option<(Tuple, Vec<usize>)>;
    /// Sets the iterator position to return the minimum value which is greater than or equal to
    /// the provided min.
    fn skip(&mut self, min: Tuple);
    /// Returns the arity of the tuples that will be returned by `next()`
    fn arity(&self) -> usize;
    /// Returns the total length of the iterator, if it were fully rewound
    fn len(&self) -> usize;
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
    fn check(&self, candidate: &mut Vec<usize>, val: usize, order: &[usize]) -> bool {
        match *self {
            Restrict::Const(v) => v == val,
            Restrict::Unify(var) => if order[var] < candidate.len() {
                candidate[order[var]] == val
            } else if order[var] == candidate.len() {
                candidate.push(val);
                true
            } else {
                panic!("Out of order variable numbering")
            },
        }
    }
    // Provides the minimum legal value for the field, assuming the candidate we have partially
    // locked in.
    fn min(&self, candidate: &Vec<usize>, order: &[usize]) -> usize {
        match *self {
            Restrict::Const(v) => v,
            Restrict::Unify(var) => {
                let var_xlat = order[var];
                if var_xlat < candidate.len() {
                    candidate[var_xlat]
                } else {
                    0
                }
            }
        }
    }
}

/// A join iterator, made of multiple `SkipIterators`, combined with the join condition.
pub struct Join<'a> {
    // Order we're going to solve in
    order: Vec<usize>,
    // Based on the reorder of predicates, how to read the restricts as candidate idx
    var_old_to_new: Vec<usize>,
    indices: Vec<&'a mut SkipIterator>,
    restricts: &'a Vec<Vec<Option<Restrict>>>,
    // Currently selected variable assignment
    candidate: Vec<usize>,
    // Stack of previous variable assignment lengths so we can rewind choices
    candidate_len: Vec<usize>,
    // Stack of fact ids in use
    fids: Vec<Vec<usize>>,
}

fn min_possible(
    candidate: &Vec<usize>,
    restricts: &Vec<Option<Restrict>>,
    order: &[usize],
) -> Vec<usize> {
    let mut out = Vec::new();
    for mr in restricts.iter() {
        match *mr {
            None => out.push(0),
            Some(ref r) => out.push(r.min(candidate, order)),
        }
    }
    out
}

// Provide an ordering for walking the SkipIterators that will do the smallest index at the
// outside, and move in from there
fn reorder_indices(indices: &Vec<&mut SkipIterator>) -> Vec<usize> {
    let mut lens: Vec<(usize, usize)> = indices.iter().map(|x| x.len()).enumerate().collect();
    lens.sort_by_key(|x| x.1);
    lens.into_iter().map(|x| x.0).collect()
}

// Based on the ordering of iterators, renumber the variables
fn reorder_vars(order: &[usize], restricts: &[Vec<Option<Restrict>>]) -> Vec<usize> {
    let max_var_len: usize = restricts
        .iter()
        .flat_map(|pred| pred.iter().flat_map(|mr| mr.iter()))
        .filter_map(|r| if let Restrict::Unify(var) = *r {
            Some(var)
        } else {
            None
        })
        .max()
        // If we have a max_var, we need one more than it for space
        .map(|x| x + 1)
        // If we have no restricts, we have no variables - this could happen for a constant query
        .unwrap_or(0);
    let mut new_to_old = Vec::new();
    let mut old_to_new = vec![0; max_var_len];
    for idx in order {
        for mr in &restricts[*idx] {
            if let Some(Restrict::Unify(var)) = *mr {
                if !new_to_old.contains(&var) {
                    old_to_new[var] = new_to_old.len();
                    new_to_old.push(var);
                }
            }
        }
    }
    old_to_new
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
        let order = reorder_indices(&indices);
        let var_old_to_new = reorder_vars(&order, restricts);
        let mut join = Join {
            order: order,
            var_old_to_new: var_old_to_new,
            indices: indices,
            restricts: restricts,
            candidate: Vec::new(),
            candidate_len: Vec::new(),
            fids: Vec::new(),
        };
        // We need to .right() once before starting to initialize the leftmost iterator properly
        join.right();
        join
    }

    // Moves one iterator left, e.g. because we have no more possibilities on this one
    fn left(&mut self) {
        self.fids.pop();
        self.fids.pop();
        self.candidate.truncate(self.candidate_len.pop().unwrap());
        self.candidate.truncate(self.candidate_len.pop().unwrap());
    }

    // Moves one iterator right, e.g. this one matched, and we need to finish filling the candidate
    // and checking restrictions
    fn right(&mut self) {
        let n = self.order[self.candidate_len.len()];
        self.indices[n].skip(min_possible(
            &self.candidate,
            &self.restricts[n],
            &self.var_old_to_new,
        ));
    }
}
impl<'a> Iterator for Join<'a> {
    type Item = (Tuple, Vec<Vec<usize>>);
    fn next(&mut self) -> Option<Self::Item> {
        // Join invariants:
        // 1.) candidate_len.len() indicates the "current" index
        // 2.) All indices less than the current index are coherent, and their candidate values
        //     are in candidate
        // 3.) The current index is at least past the minimum possible advancement level
        //     (this needs to be set up in new())
        'states: loop {
            let n = self.order[self.candidate_len.len()];
            self.candidate_len.push(self.candidate.len());
            match self.indices[n].next() {
                Some((tup, fid)) => {
                    self.fids.push(fid);
                    for (f, v) in tup.iter().enumerate() {
                        if let Some(r) = self.restricts[n][f] {
                            if !r.check(&mut self.candidate, *v, &self.var_old_to_new) {
                                if n == self.order[0] {
                                    self.candidate.truncate(self.candidate_len.pop().unwrap());
                                    self.fids.pop();
                                    return None;
                                }
                                if f == 0 {
                                    self.left();
                                } else {
                                    let mut min = min_possible(
                                        &self.candidate,
                                        &self.restricts[n],
                                        &self.var_old_to_new,
                                    );
                                    if min[f] <= tup[f] {
                                        // We've passed the min, step the previous if possible
                                        if f != 0 {
                                            min[f - 1] += 1;
                                        } else {
                                            // If the first element has gone valid -> invalid,
                                            // it's time to go to the previous clause
                                            self.left();
                                            continue 'states;
                                        }
                                    }
                                    self.indices[n].skip(min);
                                    self.candidate.truncate(self.candidate_len.pop().unwrap());
                                    self.fids.pop();
                                }
                                continue 'states;
                            }
                        }
                    }

                    if self.candidate_len.len() == self.indices.len() {
                        // We have a complete candidate
                        let mut out = Vec::new();
                        for idx in &self.var_old_to_new {
                            out.push(self.candidate[*idx])
                        }
                        let mut fids = self.fids.clone();
                        for (new, old) in self.order.iter().enumerate() {
                            fids[*old] = self.fids[new].clone();
                        }
                        self.candidate.truncate(self.candidate_len.pop().unwrap());
                        self.fids.pop();
                        return Some((out, fids));
                    }
                    // We're not done yet
                    self.right();
                }
                None => {
                    if n == self.order[0] {
                        self.candidate.truncate(self.candidate_len.pop().unwrap());
                        return None;
                    }
                    // TODO: left() abstraction broken with new fids, this hacks around it
                    self.fids.push(vec![]);
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
        fn next(&mut self) -> Option<(Tuple, Vec<usize>)> {
            if self.loc < self.payload.len() {
                self.loc += 1;
                Some((self.payload[self.loc - 1].clone(), vec![self.loc - 1]))
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
        fn len(&self) -> usize {
            self.payload.len()
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
        assert_eq!(join.next().map(|x| x.0), Some(vec![0, 2]));
        assert_eq!(join.next().map(|x| x.0), Some(vec![3, 2]));
        assert_eq!(join.next().map(|x| x.0), None);
        assert_eq!(join.next().map(|x| x.0), None);
    }

    #[test]
    fn reorder_vars_flip() {
        use self::Restrict::*;
        let order = &[0, 1];
        let order_flip = &[1, 0];
        let restrict_flip = vec![
            vec![Some(Unify(0)), Some(Unify(1))],
            vec![Some(Unify(1)), Some(Unify(0))],
        ];
        assert_eq!(&reorder_vars(order, &restrict_flip), &[0, 1]);
        assert_eq!(&reorder_vars(order_flip, &restrict_flip), &[1, 0]);
    }

    // Derived from actual data causing a mistake in reordering, that's why the inputs are a bit
    // arcane
    #[test]
    fn reorder_real() {
        use self::Restrict::*;
        let mut p = TrivialIterator::new(vec![vec![16, 16], vec![18, 18], vec![26, 26]]);
        let mut q = TrivialIterator::new(vec![vec![16, 88], vec![18, 89]]);
        let restricts = vec![
            vec![Some(Unify(0)), Some(Unify(1))],
            vec![Some(Unify(1)), Some(Unify(2))],
        ];
        let its: Vec<&mut SkipIterator> = vec![&mut p, &mut q];
        let join = Join::new(its, &restricts);
        assert_eq!(join.collect::<Vec<_>>().len(), 2)
    }

    // Evidently the first one overminimized - it still found a bug, but there's more!
    #[test]
    fn reorder_real2() {
        use self::Restrict::*;
        let mut p = TrivialIterator::new(vec![vec![16, 16], vec![16, 88]]);

        let mut q = TrivialIterator::new(vec![vec![88, 15]]);
        let restricts = vec![
            vec![Some(Unify(0)), Some(Unify(1))],
            vec![Some(Unify(1)), Some(Unify(2))],
        ];
        let its: Vec<&mut SkipIterator> = vec![&mut p, &mut q];
        let join = Join::new(its, &restricts);
        assert_eq!(join.collect::<Vec<_>>().len(), 1)
    }
}
