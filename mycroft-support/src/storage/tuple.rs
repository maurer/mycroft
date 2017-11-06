use index::hash::{HashIndex, CheckIndex};

pub struct Tuples {
    inner: Vec<Vec<usize>>,
    index: HashIndex<[usize]>,
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
    pub fn projection(&self, fields: &[usize]) -> Vec<Vec<usize>> {
        let mut out = Vec::new();
        for i in 0..self.len() {
            let mut row = Vec::new();
            for field in fields.iter() {
                row.push(self.inner[*field][i]);
            }
            out.push(row);
        }
        out
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
                (key, true)
            }
        }
    }
}
