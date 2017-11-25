#![feature(proc_macro)]
#![feature(test)]
extern crate mycroft;
extern crate mycroft_macros;
extern crate mycroft_support;
extern crate rand;
extern crate test;

use mycroft_macros::mycroft_program;
use test::Bencher;

mycroft_program!("P(usize)");

fn insert_n(n: usize, b: &mut Bencher) {
    use mycroft_program::{Database, P};
    let mut rng = Vec::new();
    for _ in 0..n {
        rng.push(rand::random());
    }
    b.iter(|| {
        let mut p = Database::new();
        for n in &rng {
            p.insert_p(P { arg0: *n });
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
