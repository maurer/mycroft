#![feature(proc_macro)]
#![feature(test)]
extern crate mycroft;
extern crate mycroft_macros;
extern crate test;
extern crate rand;

use mycroft_macros::mycroft_program;
use test::Bencher;

mycroft_program!("p(usize)");

#[bench]
fn insert_1k(b: &mut Bencher) {
    use predicates::p::*;
    b.iter(|| {
        let mut p = Storage::new();
        for _ in 0..1_000 {
            p.insert(Fact { arg0: rand::random() });
        }
        test::black_box(p)
    })
}

#[bench]
fn insert_10k(b: &mut Bencher) {
    use predicates::p::*;
    b.iter(|| {
        let mut p = Storage::new();
        for _ in 0..10_000 {
            p.insert(Fact { arg0: rand::random() });
        }
        test::black_box(p)
    })
}
