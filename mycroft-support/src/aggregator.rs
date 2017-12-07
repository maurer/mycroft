//! Defines types and utility wrappers for merging multiple pieces of data together
use std::hash::Hash;
use std::rc::Rc;
use std::marker::PhantomData;
use storage;

/// The base aggregation trait. It is intentionally not parameterized over type so that Tuple
/// operation remains fully generic.
pub trait Aggregator {
    /// This is an associative, commutative function over points in some dataspace.
    fn aggregate(&self, usize, usize) -> usize;
    /// Clone, jammed into the trait to avoid multitrait problems
    fn agg_clone(&self) -> Box<Aggregator>;
}

/// Func is an aggregator which doesn't need a data backing, for use with small types
#[derive(Clone)]
pub struct Func<F, T> {
    f: Rc<F>,
    phantom: PhantomData<T>,
}

impl<T, F: Fn(T, T) -> T> Func<F, T> {
    /// Create a new small-typed aggregator
    pub fn new(f: F) -> Self {
        Self {
            f: Rc::new(f),
            phantom: PhantomData,
        }
    }
}

macro_rules! castable_aggregator {
    ($ty:ty) => {
impl<F: Fn($ty, $ty) -> $ty + 'static> Aggregator for Func<F, $ty> {
    fn aggregate(&self, k0: usize, k1: usize) -> usize {
        (self.f)(k0 as $ty, k1 as $ty) as usize
    }
    fn agg_clone(&self) -> Box<Aggregator> {
        Box::new(Self { f: self.f.clone(), phantom: PhantomData})
    }
}
}
}

impl<F: Fn(bool, bool) -> bool + 'static> Aggregator for Func<F, bool> {
    fn aggregate(&self, k0: usize, k1: usize) -> usize {
        (self.f)(k0 == 1, k1 == 1) as usize
    }
    fn agg_clone(&self) -> Box<Aggregator> {
        Box::new(Self {
            f: self.f.clone(),
            phantom: PhantomData,
        })
    }
}

castable_aggregator!(u8);
castable_aggregator!(u16);
castable_aggregator!(u32);
castable_aggregator!(u64);
castable_aggregator!(usize);
castable_aggregator!(i8);
castable_aggregator!(i16);
castable_aggregator!(i32);
castable_aggregator!(i64);
castable_aggregator!(isize);


/// FuncData is a convenient way to box up a (T,T)->T style function as one that acts on
/// coordinates instead by wrapping it with a handle to the relevant Data.
#[derive(Clone)]
pub struct FuncData<T, F> {
    f: Rc<F>,
    data: storage::Data<T>,
}

impl<T, F: Fn(&T, &T) -> T> FuncData<T, F> {
    /// Creates a new `FuncData`, from the actual function you want to use plus the dataspace it
    /// operates on.
    pub fn new(f: F, data: storage::Data<T>) -> Self {
        Self {
            f: Rc::new(f),
            data: data,
        }
    }
}

impl<T: Hash + PartialEq + 'static, F: Fn(&T, &T) -> T + 'static> Aggregator for FuncData<T, F> {
    fn aggregate(&self, k0: usize, k1: usize) -> usize {
        let v = (self.f)(&self.data[k0], &self.data[k1]);
        unsafe {
            self.data.read_exit(2);
        }
        self.data.insert(v)
    }
    fn agg_clone(&self) -> Box<Aggregator> {
        Box::new(Self {
            f: self.f.clone(),
            data: self.data.clone(),
        })
    }
}
