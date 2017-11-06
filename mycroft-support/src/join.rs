use std::collections::HashMap;
type Tuple = Vec<usize>;
pub trait SkipIterator {
    fn next(&mut self) -> Option<Tuple>;
    fn skip(&mut self, min: Tuple);
    fn rewind(&mut self);
    fn arity(&self) -> usize;
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Copy, Clone, Hash)]
pub struct Field {
    clause: usize,
    field: usize,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Copy, Clone, Hash)]
pub enum Restrict {
    Const(usize),
    Unify(usize),
}

impl Restrict {
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


pub struct Join {
    indices: Vec<Box<SkipIterator>>,
    restricts: HashMap<Field, Restrict>,
    candidate: Vec<usize>,
    candidate_len: Vec<usize>,
}

fn min_possible(
    candidate: &Vec<usize>,
    restricts: &HashMap<Field, Restrict>,
    clause_index: usize,
    clause_arity: usize,
) -> Vec<usize> {
    let mut out = Vec::new();
    for field in 0..clause_arity {
        match restricts.get(&Field {
            clause: clause_index,
            field: field,
        }) {
            None => out.push(0),
            Some(r) => out.push(r.min(candidate)),
        }
    }
    out
}

impl Join {
    pub fn new(indices: Vec<Box<SkipIterator>>, restricts: HashMap<Field, Restrict>) -> Self {
        let mut join = Join {
            indices: indices,
            restricts: restricts,
            candidate: Vec::new(),
            candidate_len: Vec::new(),
        };
        join.right();
        join
    }
    fn left(&mut self) {
        self.candidate.truncate(self.candidate_len.pop().unwrap());
        let n = self.candidate_len.len();
        self.indices[n].rewind();
        self.candidate.truncate(self.candidate_len.pop().unwrap());
    }

    fn right(&mut self) {
        let n = self.candidate_len.len();
        let arity = self.indices[n].arity();
        self.indices[n].skip(min_possible(&self.candidate, &self.restricts, n, arity));
    }
}
impl Iterator for Join {
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
                        match self.restricts.get(&Field {
                            clause: n,
                            field: f,
                        }) {
                            Some(r) => {
                                if !r.check(&mut self.candidate, v) {
                                    if n == 0 {
                                        self.candidate.truncate(self.candidate_len.pop().unwrap());
                                        return None
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
                        return Some(out)
                    }
                    // We're not done yet
                    self.right();
                }
                None => {
                    if n == 0 {
                        self.candidate.truncate(self.candidate_len.pop().unwrap());
                        return None
                    }
                    self.left();
                }
            }
        }
    }
}

pub struct TrivialIterator {
    payload: Vec<Vec<usize>>,
    loc: usize,
}

impl TrivialIterator {
    pub fn new(mut payload: Vec<Vec<usize>>) -> Self {
        payload.sort();
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
        while (self.payload.len() > self.loc) && (self.payload[self.loc] < min) {
            self.loc = self.loc + 1;
        }
    }
    fn rewind(&mut self) {
        self.loc = 0;
    }
    fn arity(&self) -> usize {
        self.payload[0].len()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn simple_join() {
        let p = TrivialIterator::new(vec![vec![0, 2, 3], vec![3, 2, 5], vec![4, 4, 4]]);
        let q = TrivialIterator::new(vec![vec![1, 3], vec![2, 7], vec![3, 4], vec![8, 2]]);
        let its: Vec<Box<SkipIterator>> = vec![Box::new(p), Box::new(q)];
        let mut restricts = HashMap::new();
        restricts.insert(
            Field {
                clause: 0,
                field: 0,
            },
            Restrict::Unify(0),
        );
        restricts.insert(
            Field {
                clause: 0,
                field: 1,
            },
            Restrict::Unify(1),
        );
        restricts.insert(
            Field {
                clause: 1,
                field: 0,
            },
            Restrict::Unify(1),
        );
        restricts.insert(
            Field {
                clause: 1,
                field: 1,
            },
            Restrict::Const(7),
        );
        let mut join = Join::new(its, restricts);
        assert_eq!(join.next(), Some(vec![0, 2]));
        assert_eq!(join.next(), Some(vec![3, 2]));
        assert_eq!(join.next(), None);
        assert_eq!(join.next(), None);
    }
}
