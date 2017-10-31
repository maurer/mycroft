use index::hash::HashIndex;
use std::hash::Hash;

pub struct Data<T> {
    inner: Vec<T>,
    index: HashIndex<T>,
}

impl<T: Eq + Hash> Data<T> {
    pub fn new() -> Self {
        Self {
            inner: Vec::new(),
            index: HashIndex::new(),
        }
    }
    pub fn insert(&mut self, data: T) -> usize {
        match self.find(&data) {
            Some(key) => key,
            None => {
                let key = self.inner.len();
                self.index.insert(key, &data);
                self.inner.push(data);
                key
            }
        }
    }

    fn find(&self, data: &T) -> Option<usize> {
        self.index.find(data, &self.inner)
    }
    pub fn get(&self, key: usize) -> &T {
        &self.inner[key]
    }
}
