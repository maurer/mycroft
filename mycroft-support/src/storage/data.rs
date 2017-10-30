pub struct Data<T> {
    inner: Vec<T>,
}

impl<T: Eq> Data<T> {
    pub fn new() -> Self {
        Self { inner: Vec::new() }
    }
    pub fn insert(&mut self, data: T) -> usize {
        match self.find(&data) {
            Some(key) => key,
            None => {
                self.inner.push(data);
                self.inner.len() - 1
            }
        }
    }
    fn find(&self, data: &T) -> Option<usize> {
        // TODO, use hash or btree to accelerate this
        for i in 0..self.inner.len() {
            if &self.inner[i] == data {
                return Some(i);
            }
        }
        None
    }
    pub fn get(&self, key: usize) -> &T {
        &self.inner[key]
    }
}
