pub struct Tuples {
    /// Row,
    inner: Vec<Vec<usize>>,
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
    pub fn new(arity: usize) -> Self {
        assert!(arity > 0);
        let mut inner = Vec::new();
        for _ in 0..arity {
            inner.push(Vec::new());
        }
        Tuples { inner: inner }
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
        // TODO: Once I add indexes, place a default index in tuples to speed this up
        'rows: for row in 0..self.len() {
            for col in 0..needle.len() {
                if self.inner[col][row] != needle[col] {
                    continue 'rows;
                }
            }
            return Some(row);
        }
        None
    }
    pub fn insert(&mut self, val: &[usize]) -> (usize, bool) {
        match self.find(&val) {
            Some(id) => (id, false),
            None => {
                assert_eq!(val.len(), self.arity());
                debug_assert!(self.integrity());
                for (col, new_val) in self.inner.iter_mut().zip(val.into_iter()) {
                    col.push(*new_val)
                }
                (self.len() - 1, true)
            }
        }
    }
}
