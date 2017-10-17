#![feature(proc_macro)]
#![feature(test)]
extern crate mycroft;
extern crate mycroft_macros;
extern crate test;
extern crate rand;

use mycroft_macros::mycroft_program;
use test::Bencher;

mycroft_program!("p(usize)");

fn insert_n(n: usize, b: &mut Bencher) {
    use predicates::p::*;
    let mut rng = Vec::new();
    for _ in 0..n {
        rng.push(rand::random());
    }
    b.iter(|| {
        let mut p = Storage::new();
        for i in 0..n {
            p.insert(Fact { arg0: rng[i] });
        }
        test::black_box(p)
    })
}

#[bench]
fn insert_1k(b: &mut Bencher) {
    insert_n(1_000, b)
}

#[bench]
fn insert_10k(b: &mut Bencher) {
    insert_n(10_000, b)
}

#[bench]
fn insert_100k(b: &mut Bencher) {
    insert_n(100_000, b)
}
